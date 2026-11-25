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
;; the agent knows the item, and where the work is kept.
;;
;; The item is the text of the region, where the user reads the session,
;; else it is read in the minibuffer.  The prompt
;; (`drover-handoff-prompt') goes through the queue of scheduled
;; prompts, as a wrap-up's does: out of every window, never over a
;; draft, and once the session's turn ended when one runs.  A session of
;; another host gets it through its host, which queues it there.
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
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 31))
  (load "drover-core" nil t))

(declare-function drover-remote-schedule-later "drover-remote" (session when text argument by callback))
(defvar drover-dispatch-region)

;;;; Options

(defcustom drover-handoff-prompt
  "Hand this item of our work off to %m:

%i

Load your `handoff` skill if you have one, and follow it.  Without that skill, do these steps:

1. File an Org heading for the item, with an Org ID, where this work keeps its tasks.  Write its body as a brief that reads alone, for a reader who saw nothing of this conversation: the goal, what is known, the decisions made, the limits, when it is done, and links.
2. Start it at once, without asking for a review of the brief.  For a fresh session, call `drover-task-start` with the heading's ID.  For a fork of this session, call `drover-session-fork` with the heading's ID as `task`, and a `prompt` that names the item and links the heading.  A fork starts from this whole conversation, so only that prompt tells it which item is its own.  A refused fork ends the hand-off: report the refusal.
3. Reply with the heading's link, `[[id:ID][title]]`, and the new session's link, the `link` the tool returned.

Done when the new session runs and your reply holds both links."
  "The prompt asking a session to hand an item off to a session of its own.
A `format-spec' template: %i is the item, the text `drover-handoff' was
given; %m the mode, \"a fresh session\", or \"a fork of this session\"
when a fork is asked; %% is a percent sign."
  :type 'string
  :group 'drover)

(defconst drover-handoff-origin "hand-off"
  "How the prompts of hand-offs are named in the queue of scheduled prompts.")

(defvar drover-handoff-history nil
  "The items `drover-handoff' read in the minibuffer.")

;;;; The prompt

(defun drover-handoff-mode (fork)
  "Return how the item goes, as the prompt tells it: forked when FORK is non-nil."
  (if fork "a fork of this session" "a fresh session"))

(defun drover-handoff-prompt-text (item &optional fork)
  "Return the prompt handing ITEM off, to a fork of the session with FORK."
  (format-spec drover-handoff-prompt
               `((?i . ,item)
                 (?m . ,(drover-handoff-mode fork)))))

;;;; Delivering

(defun drover-handoff--busy-p (session)
  "Return non-nil while SESSION runs a turn, or holds prompts not taken yet.
A question it asks is part of its turn."
  (or (memq (drover-session-status session) drover-active-statuses)
      (> (drover-pending-prompts session) 0)))

(defun drover-handoff--send-here (session text)
  "Queue TEXT for SESSION, of this host; return non-nil when it waits for a turn.
Through the queue of scheduled prompts, out of every window: after its
turn when one runs, else now, once its buffer takes a prompt without
touching a draft."
  ;; The queue tells what it scheduled, its whole prompt: the command tells the item.
  (let ((inhibit-message t))
    (if (drover-handoff--busy-p session)
        (progn
          (drover-schedule-for-session session "when turn ends" text
                                       (list :hidden t :origin drover-handoff-origin))
          t)
      (drover-schedule-deliver (drover-schedule-session-target session) text
                               :hidden t :origin drover-handoff-origin)
      nil)))

(defun drover-handoff--send-there (session text)
  "Have the host of SESSION, a peer's, give it TEXT; return non-nil when it waits.
Its host queues it: after its turn when one runs, else as it queues a
prompt sent, once the session takes one.  A refusal is warned of once
the host answers."
  (let ((title (drover-bound-line (drover-session-display-title session) 80))
        (host (drover-host-label (drover-session-host session))))
    (cl-flet ((refused (why)
                (drover-with-reporting "hand-off"
                  (drover-warn "Could not hand off from %s: %s refused its prompt: %s" title host why))))
      (if (drover-handoff--busy-p session)
          (progn
            (unless (fboundp 'drover-remote-schedule-later)
              (user-error "%s runs on %s, and `drover-remote-mode' is off" title host))
            (drover-remote-schedule-later session "when turn ends" text nil drover-handoff-origin
                                          (lambda (result)
                                            (when-let* ((why (plist-get result :failure)))
                                              (refused why))))
            t)
        (drover-backend-call session :submit text #'ignore
                             (lambda (&optional why &rest _) (refused (or why "no reason told"))))
        nil))))

(defun drover-handoff--schedule-changed ()
  "Forget the prompts of hand-offs that went.
On `drover-schedule-changed-hook': the list of scheduled prompts keeps
what waits, and what failed, which it sends again."
  (drover-with-reporting "hand-off"
    (dolist (job (seq-filter (lambda (job)
                               (and (equal (plist-get job :origin) drover-handoff-origin)
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
         (fork (and current-prefix-arg t)))
    (list session
          (or (drover-handoff--region)
              (read-string (format "Hand off from %s, to %s: "
                                   (drover-bound-line (drover-session-display-title session) 60)
                                   (drover-handoff-mode fork))
                           nil 'drover-handoff-history))
          fork)))

;;;###autoload
(defun drover-handoff (session item &optional fork)
  "Hand ITEM, one item of the work of SESSION, off to a session of its own.
SESSION's agent is asked to, by a prompt made from
`drover-handoff-prompt': it files an Org heading for ITEM, whose body is
a brief that reads alone, starts the task of that heading in a fresh
session, and replies with the links of both.  With FORK non-nil, it
forks SESSION for the item instead; a session of this host whose
harness cannot fork is refused here, the harness named, and nothing is
handed off.  Drover writes no heading.

The prompt goes as a wrap-up's does, through the queue of scheduled
prompts: out of every window, never over a draft, and once SESSION's
turn ended when one runs.  A session of another host gets it through
its host.

Interactively, SESSION is the session at point, in a list or in its own
buffer; ITEM the text of the region there, or, from the dispatch menu
acting on a list beside, of the buffer it was called from, else read in
the minibuffer: in a list, a region selects sessions.  A prefix
argument asks for a fork."
  (interactive (drover-handoff--read))
  (drover-start)
  (let ((item (and (stringp item) (string-trim (substring-no-properties item))))
        (title (drover-bound-line (drover-session-display-title session) 80)))
    (when (or (null item) (string-empty-p item))
      (user-error "No item to hand off"))
    (when-let* (((and fork (not (drover-session-host session))))
                (why (drover-fork-refusal session)))
      (user-error "%s cannot be forked: %s" title why))
    (let ((waits (if (drover-session-host session)
                     (drover-handoff--send-there session (drover-handoff-prompt-text item fork))
                   (drover-handoff--send-here session (drover-handoff-prompt-text item fork)))))
      (drover-message "%s hands off to %s%s: %s" title (drover-handoff-mode fork)
                      (if waits ", once its turn ends" "")
                      (drover-bound-line (replace-regexp-in-string "[\n\r]+" " " item) 120)))))

(defun drover-handoff-unload-function ()
  "Stop following the prompts of hand-offs; those queued stay in the queue."
  (remove-hook 'drover-schedule-changed-hook #'drover-handoff--schedule-changed)
  nil)

(provide 'drover-handoff)
;;; drover-handoff.el ends here
