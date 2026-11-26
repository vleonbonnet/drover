;;; drover-handoff.el --- Handing an item of a session off to a session of its own  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A long session collects items: questions it opened, faults it found,
;; work it put off.  `drover-handoff', h on a session, asks its agent to
;; hand one of them off to a session of its own: the agent files an Org
;; heading for the item, whose body is a brief that reads alone, and
;; starts its task in a fresh session, or, with a prefix argument, forks
;; its own session for it, through the tools of `drover-anvil',
;; drover-task-start and drover-session-fork.  Drover writes no heading:
;; the agent knows the item.  Where its heading goes is the user's to
;; say, read as Org's refile reads a place (`drover-handoff-read-place'),
;; and given to the agent as Anvil's Org tools name it.  The prompt
;; stands alone: it names no skill, and tells the agent every step, so
;; that an agent learns of handing off only once the user asks for it.
;;
;; The item is the text of the region, where the user reads the session,
;; else it is read in the minibuffer.  The prompt
;; (`drover-handoff-prompt') goes through the queue of scheduled
;; prompts, as a wrap-up's does: out of every window, never over a
;; draft, and once the session's turn ended when one runs
;; (`drover-handoff-send').  A session of another host gets it through
;; its host, which queues it there.  The prompt telling a session where
;; the items of its list go goes the same way (see `drover-items').
;;
;; The session is not told handed its turn (`drover-note-handed-off'),
;; whose name is older than this file's: the user goes on reading it,
;; and its buffer stays.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'format-spec)
(require 'drover-core)
(require 'drover-schedule)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 32))
  (load "drover-core" nil t))

(declare-function drover-remote-schedule-later "drover-remote" (session when text argument by callback))
(declare-function drover-task-id-of "drover-task" (session))
(declare-function drover-task-org-marker "drover-task-org" (id &optional visited-only))
(declare-function drover-task-org-refused-p "drover-task-org" (file))
(declare-function drover-task-org-place-id "drover-task-org" (marker))
(declare-function org-refile-get-location "org-refile" (&optional prompt default-buffer new-nodes))
(declare-function org-agenda-files "org" (&optional unrestricted archives))
(declare-function anvil-org--find-allowed-file "ext:anvil-org" (filename))
(defvar org-refile-history)
(defvar org-refile-targets)
(defvar drover-dispatch-region)

;;;; Options

(defcustom drover-handoff-prompt
  "Hand this item of our work off to %m:

%i

1. Write an Org heading for the item with `org-add-todo`, %p.  The tool gives the heading its ID.
2. Write the body of the heading as a brief that reads alone, for a reader who saw nothing of this conversation.  The brief holds the goal, what is known with paths and links, the decisions already made, the limits, when the item is done, and a report paragraph.
3. Write the report paragraph for the session that takes the item: \"When the item is done or blocked, send one message to the session `%k` with `drover-session-send`.  Then set the state of this heading.\"
4. Start the item at once.  Do not ask me to review the brief.
5. For a fresh session, call `drover-task-start` with the ID of the heading, `repo` or `directory`, and `agent`.
6. For a fork of this session, call `drover-session-fork` with the ID of the heading as `task`, `agent`, and a `prompt` that names the item and links the heading.  A fork starts from this whole conversation, so only that prompt tells the fork which item is its own.  A refused fork ends the hand-off: report the refusal.
7. Choose `agent` yourself: `build` when the research is done and the brief says what to build, `plan` when design decisions remain.
8. Reply with the link of the heading, `[[id:ID][title]]`, and the link of the new session, the `link` that the tool returned.

Done when the new session runs and your reply holds both links."
  "The prompt asking a session to hand an item off to a session of its own.
A `format-spec' template: %i is the item, the text `drover-handoff' was
given; %m the mode, \"a fresh session\", or \"a fork of this session\"
when a fork is asked; %p where its heading goes, the place the user
chose, as Anvil's org-add-todo takes it (see
`drover-handoff-place-text'); %k the key of the session asked, as the
tools of its host name it, for the item's session to report to (see
`drover-handoff-session-key'); %% is a percent sign.  The default names
no skill and tells every step: the agent is told nothing of handing
off before."
  :type 'string
  :group 'drover)

(defconst drover-handoff-origin "hand-off"
  "How the prompts of hand-offs are named in the queue of scheduled prompts.")

(defvar drover-handoff--origins (list drover-handoff-origin)
  "The names of the prompts `drover-handoff-send' queued, forgotten once sent.")

(defvar drover-handoff-history nil
  "The items `drover-handoff' read in the minibuffer.")

(defvar drover-handoff-place-history nil
  "The places chosen for headings, newest first, as Org's refile names them.
The first is offered as the default of the next place read, for a
hand-off or for the items of a list (see `drover-handoff-read-place').
Add it to `savehist-additional-variables' to keep them from one Emacs
session to the next.")

(defconst drover-handoff-place-history-length 30
  "Places `drover-handoff-place-history' keeps at most.")

;;;; Places of headings

(defun drover-handoff--place-buffer (session)
  "Return an Org buffer for the refile targets naming no file, or nil.
`org-refile-targets' may name the current buffer, which must then be
an Org buffer: the buffer of the heading of SESSION's task, else the
current buffer when it is one, else that of the first agenda file."
  (or (when-let* ((id (and session (drover-task-id-of session)))
                  (marker (drover-task-org-marker id)))
        (prog1 (marker-buffer marker) (set-marker marker nil)))
      (and (derived-mode-p 'org-mode) (buffer-file-name) (current-buffer))
      (when-let* ((file (car (ignore-errors (org-agenda-files t))))
                  ((file-readable-p file)))
        (find-file-noselect file t))))

(defun drover-handoff--place-refusal (location by-id)
  "Say why LOCATION, as `org-refile-get-location' returns it, is no place, or nil.
A file tasks are never written in (see `drover-task-org-refused-p'), or
one Anvil's Org tools do not write (`anvil-org-allowed-files'), unless
BY-ID: the top of a file then, which has no ID."
  (pcase-let ((`(,_ ,file ,_ ,position) location))
    (cond
     ((or (not (stringp file)) (drover-task-org-refused-p file))
      (format "Headings are not written in %s" (if (stringp file) (abbreviate-file-name file) "that buffer")))
     ((and by-id (null position))
      "The top of a file has no ID, which another host's agent finds a place by")
     ((and (not by-id) (fboundp 'anvil-org--find-allowed-file)
           (not (ignore-errors (anvil-org--find-allowed-file (expand-file-name file)))))
      (format "Anvil's Org tools do not write %s (see `anvil-org-allowed-files')"
              (abbreviate-file-name file))))))

(defun drover-handoff--place-of (location)
  "Return the place LOCATION, as `org-refile-get-location' returns it, stands for.
See `drover-handoff-read-place'.  A heading without an ID is given one."
  (pcase-let* ((`(,name ,file ,_ ,position) location)
               (file (expand-file-name file))
               (name (string-remove-suffix "/" (substring-no-properties name))))
    (if (null position)
        (list :uri (concat "org-headline://" (string-replace "#" "%23" file))
              :name name :file file)
      (let* ((marker (if (markerp position)
                         position
                       (with-current-buffer (find-file-noselect file t) (copy-marker position))))
             (id (drover-task-org-place-id marker)))
        (list :uri (concat "org-id://" id) :name name :file file :id id)))))

(defun drover-handoff-read-place (prompt &optional session by-id)
  "Read where a heading goes, as Org's refile reads a place; return the place.
PROMPT is the question, without a colon.  The candidates are the
user's refile targets (`org-refile-targets'), with completion, as
`org-refile-get-location' offers them, the place chosen last offered
first (see `drover-handoff-place-history'); no new heading is made.
Targets of the current buffer are those of the heading of SESSION's
task, when it serves one (see `drover-handoff--place-buffer').

The place is a plist: :uri, where Anvil's org-add-todo writes a child
heading, its parent_uri: org-id://ID for a heading, given an ID when it
has none (see `drover-task-org-place-id'), org-headline://FILE for the
top of a file; :name, the place as completion names it; :file; :id,
the heading's.  A place in a file tasks are never written in, one Anvil
does not write, or, with BY-ID, for the agent of another host, which
finds a place by its ID, the top of a file, is refused, and asked again,
saying why; so is the place offered when it is no longer one."
  (require 'org-refile)
  (require 'drover-task)
  (let* ((buffer (drover-handoff--place-buffer session))
         (targets (if buffer
                      org-refile-targets
                    (or (seq-filter #'car org-refile-targets)
                        (user-error "No refile targets: `org-refile-targets' names the current buffer alone"))))
         (offered (car drover-handoff-place-history))
         (why nil)
         (place nil))
    (while (not place)
      (let* ((org-refile-targets targets)
             (org-refile-history (if offered
                                     (cons offered (remove offered org-refile-history))
                                   (copy-sequence org-refile-history)))
             (location (condition-case err
                           (org-refile-get-location (if why (format "%s.  %s" why prompt) prompt) buffer nil)
                         (user-error
                          (if (and offered (equal (cadr err) "Invalid target location"))
                              (progn
                                (setq why (format "%s is no longer a place" offered)
                                      drover-handoff-place-history (delete offered drover-handoff-place-history)
                                      offered nil)
                                nil)
                            (signal (car err) (cdr err)))))))
        (when location
          (if-let* ((refusal (drover-handoff--place-refusal location by-id)))
              (setq why refusal)
            (setq place (drover-handoff--place-of location))
            (setq drover-handoff-place-history
                  (seq-take (cons (car location) (delete (car location) drover-handoff-place-history))
                            drover-handoff-place-history-length))))))
    place))

(defun drover-handoff-place-text (place)
  "Return where a heading goes, PLACE, as the prompts tell it.
PLACE is what `drover-handoff-read-place' returns, or nil, for where
the work keeps its tasks, as the agent knows."
  (if (null place)
      "where this work keeps its tasks"
    (format "under the place I chose, %s: give `%s` as its `parent_uri`"
            (if (plist-get place :id)
                (format "the heading \"%s\"" (plist-get place :name))
              (format "the top of %s" (abbreviate-file-name (plist-get place :file))))
            (plist-get place :uri))))

;;;; The prompt

(defun drover-handoff-mode (fork)
  "Return how the item goes, as the prompt tells it: forked when FORK is non-nil."
  (if fork "a fork of this session" "a fresh session"))

(defun drover-handoff-session-key (session)
  "Return the key of SESSION as the tools of its own host name it, a string.
BACKEND/ID, what `drover-session-send' takes there: the prompts give it
to SESSION's agent, for the session an item goes to, which that agent
starts on its host, to report to SESSION.  A session of another host is
named by its owner's key, as that host's tools name it."
  (let ((id (drover-session-id session)))
    (if (and (drover-session-host session) (consp id))
        (format "%s/%s" (car id) (cdr id))
      (format "%s/%s" (drover-session-backend session) id))))

(defun drover-handoff-prompt-text (item session &optional fork place)
  "Return the prompt asking SESSION to hand ITEM off, to a fork of it with FORK.
Its heading goes to PLACE, as `drover-handoff-read-place' returns it;
nil leaves it to the agent."
  (format-spec drover-handoff-prompt
               `((?i . ,item)
                 (?m . ,(drover-handoff-mode fork))
                 (?p . ,(drover-handoff-place-text place))
                 (?k . ,(drover-handoff-session-key session)))))

;;;; Delivering

(defun drover-handoff--busy-p (session)
  "Return non-nil while SESSION runs a turn, or holds prompts not taken yet.
A question it asks is part of its turn."
  (or (memq (drover-session-status session) drover-active-statuses)
      (> (drover-pending-prompts session) 0)))

(defun drover-handoff--send-here (session text origin)
  "Queue TEXT for SESSION, of this host; return non-nil when it waits for a turn.
Through the queue of scheduled prompts, named ORIGIN, out of every
window: after its turn when one runs, else now, once its buffer takes a
prompt without touching a draft."
  ;; The queue tells what it scheduled, its whole prompt: the command tells the item.
  (let ((inhibit-message t))
    (if (drover-handoff--busy-p session)
        (progn
          (drover-schedule-for-session session "when turn ends" text
                                       (list :hidden t :origin origin))
          t)
      (drover-schedule-deliver (drover-schedule-session-target session) text
                               :hidden t :origin origin)
      nil)))

(defun drover-handoff--send-there (session text origin)
  "Have the host of SESSION, a peer's, give it TEXT; return non-nil when it waits.
Its host queues it, named ORIGIN: after its turn when one runs, else as
it queues a prompt sent, once the session takes one.  A refusal is
warned of once the host answers."
  (let ((title (drover-bound-line (drover-session-display-title session) 80))
        (host (drover-host-label (drover-session-host session))))
    (cl-flet ((refused (why)
                (drover-with-reporting "hand-off"
                  (drover-warn "Could not prompt %s: %s refused its prompt: %s" title host why))))
      (if (drover-handoff--busy-p session)
          (progn
            (unless (fboundp 'drover-remote-schedule-later)
              (user-error "%s runs on %s, and `drover-remote-mode' is off" title host))
            (drover-remote-schedule-later session "when turn ends" text nil origin
                                          (lambda (result)
                                            (when-let* ((why (plist-get result :failure)))
                                              (refused why))))
            t)
        (drover-backend-call session :submit text #'ignore
                             (lambda (&optional why &rest _) (refused (or why "no reason told"))))
        nil))))

(defun drover-handoff-send (session text &optional origin)
  "Give SESSION the prompt TEXT as a hand-off does; return non-nil when it waits.
Through the queue of scheduled prompts, under the name ORIGIN, a string,
`drover-handoff-origin' by default: out of every window, never over a
draft, and once SESSION's turn ended when one runs.  A session of
another host gets it through its host.  The prompt leaves the queue
once sent."
  (let ((origin (or origin drover-handoff-origin)))
    (cl-pushnew origin drover-handoff--origins :test #'equal)
    (if (drover-session-host session)
        (drover-handoff--send-there session text origin)
      (drover-handoff--send-here session text origin))))

(defun drover-handoff--schedule-changed ()
  "Forget the prompts of hand-offs that went.
On `drover-schedule-changed-hook': the list of scheduled prompts keeps
what waits, and what failed, which it sends again."
  (drover-with-reporting "hand-off"
    (dolist (job (seq-filter (lambda (job)
                               (and (member (plist-get job :origin) drover-handoff--origins)
                                    (eq (plist-get job :status) 'sent)))
                             (copy-sequence (drover-schedule--jobs))))
      (drover-schedule-forget (plist-get job :id)))))

(add-hook 'drover-schedule-changed-hook #'drover-handoff--schedule-changed)

;;;; The command

(defun drover-handoff--region ()
  "Return the text of the region telling an item here, or nil.
The active region of the current buffer, unless it lists sessions,
where a region selects sessions; else the region of the buffer the
dispatch menu was called from, when it acts on a list beside (see
`drover-dispatch-region')."
  (let ((text (if (and (not (drover-session-list-p)) (use-region-p))
                  (prog1 (buffer-substring-no-properties (region-beginning) (region-end))
                    (deactivate-mark))
                (bound-and-true-p drover-dispatch-region))))
    (and (stringp text) (not (string-blank-p text)) (string-trim text))))

(defun drover-handoff--read ()
  "Return the arguments of `drover-handoff', read where the user is."
  (let* ((sessions (drover-selected-sessions))
         (session (if (cdr sessions)
                      (user-error "A hand-off is of one session: %d are selected" (length sessions))
                    (car sessions)))
         (fork (and current-prefix-arg t))
         (title (drover-bound-line (drover-session-display-title session) 60))
         (item (or (drover-handoff--region)
                   (read-string (format "Hand off from %s, to %s: " title (drover-handoff-mode fork))
                                nil 'drover-handoff-history))))
    (when (string-blank-p item)
      (user-error "No item to hand off"))
    (when-let* (((and fork (not (drover-session-host session))))
                (why (drover-fork-refusal session)))
      (user-error "%s cannot be forked: %s" title why))
    (list session item fork
          (drover-handoff-read-place (format "Heading of %s goes under"
                                             (drover-bound-line (replace-regexp-in-string "[\n\r]+" " " item) 50))
                                     session (and (drover-session-host session) t)))))

;;;###autoload
(defun drover-handoff (session item &optional fork place)
  "Hand ITEM, one item of the work of SESSION, off to a session of its own.
SESSION's agent is asked to, by a prompt made from
`drover-handoff-prompt': it files an Org heading for ITEM under PLACE,
whose body is a brief that reads alone, starts the task of that
heading in a fresh session, and replies with the links of both.  With
FORK non-nil, it forks SESSION for the item instead; a session of this
host whose harness cannot fork is refused here, the harness named, and
nothing is handed off.  Drover writes no heading.  PLACE is what
`drover-handoff-read-place' returns; nil leaves the place to the agent.

The prompt goes as a wrap-up's does, through the queue of scheduled
prompts: out of every window, never over a draft, and once SESSION's
turn ended when one runs.  A session of another host gets it through
its host.

Interactively, SESSION is the session at point, in a list or in its own
buffer; ITEM the text of the region there, or, from the dispatch menu
acting on a list beside, of the buffer it was called from, else read in
the minibuffer: in a list, a region selects sessions.  A prefix
argument asks for a fork.  PLACE is read as Org's refile reads one,
the place chosen last offered first; a session of another host, whose
agent finds a heading by its ID, takes a heading."
  (interactive (drover-handoff--read))
  (drover-start)
  (let ((item (and (stringp item) (string-trim (substring-no-properties item))))
        (title (drover-bound-line (drover-session-display-title session) 80)))
    (when (or (null item) (string-empty-p item))
      (user-error "No item to hand off"))
    (when-let* (((and fork (not (drover-session-host session))))
                (why (drover-fork-refusal session)))
      (user-error "%s cannot be forked: %s" title why))
    (let ((waits (drover-handoff-send session (drover-handoff-prompt-text item session fork place))))
      (drover-message "%s hands off to %s%s: %s" title (drover-handoff-mode fork)
                      (if waits ", once its turn ends" "")
                      (drover-bound-line (replace-regexp-in-string "[\n\r]+" " " item) 120)))))

(defun drover-handoff-unload-function ()
  "Stop following the prompts of hand-offs; those queued stay in the queue."
  (remove-hook 'drover-schedule-changed-hook #'drover-handoff--schedule-changed)
  nil)

(provide 'drover-handoff)
;;; drover-handoff.el ends here
