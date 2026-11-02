;;; drover-keys.el --- Commands, keys and menus acting on sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; Three sets of single keys, none sharing a key, so that all three fit
;; in one menu, `drover-dispatch':
;;
;; - Session keys act on the session at point.  `drover-session-map'
;;   binds them in every list of sessions: drover's views inherit it,
;;   and `drover-session-keys-mode' adds it to lists drover does not
;;   draw, those of other front ends.  In a prompt buffer they act on the
;;   buffer's session, through the menu; in a buffer without a session,
;;   on the session at point of drover's list shown beside it.
;; - Prompt keys perform the prompt actions of `drover-prompt-actions',
;;   through the menu.
;; - Global keys work everywhere, through the menu.
;;
;; While the menu acts on drover's list, here or beside, a fourth group
;; moves in that list, the menu staying open: C-n, C-p, M-n, M-p, M-<
;; and M->, keys of no other set; RET opens the session at point, but
;; in a buffer sending prompts, where it stays the prompt key Send.
;;
;; Bind `drover-dispatch' globally, for instance to C-'; drover binds no
;; global key.  Other packages add their own global commands to the menu
;; with `transient-append-suffix' and `transient-insert-suffix', placed
;; relative to drover's keys: more ways to pick a session or prompt
;; after "'", the prompt's last key (", n and '), before the scheduled
;; prompts of "#"; more lists of
;; sessions after "p", where `drover-opencode' appends its session
;; search; more prompt commands after "O".
;;
;; `drover-view', the View menu, chooses how drover's list sorts, groups
;; and filters sessions, which subagents it shows, whether it shows
;; older and archived sessions, and the fields it may show.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'drover-core)
(require 'drover-ui)
(require 'opencode-look)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

(autoload 'drover-next "drover-attention" nil t)
(autoload 'drover-select "drover-attention" nil t)
(autoload 'drover-list "drover-list" nil t)
(autoload 'drover-list-side "drover-list" nil t)
(autoload 'drover-list-next-session "drover-list" nil t)
(autoload 'drover-list-previous-session "drover-list" nil t)
(autoload 'drover-list-next-group "drover-list" nil t)
(autoload 'drover-list-previous-group "drover-list" nil t)
(autoload 'drover-list-first-session "drover-list" nil t)
(autoload 'drover-list-last-session "drover-list" nil t)
(autoload 'drover-prompt-dwim "drover-prompt" nil t)
(autoload 'drover-prompt-org-task "drover-prompt" nil t)
(autoload 'drover-prompt-toggle "drover-prompt" nil t)
(autoload 'drover-schedule-list "drover-schedule" nil t)

(defvar drover-prompt-mode)

(declare-function drover-read-session "drover-attention" (prompt sessions))

;;;; Opening sessions

(defun drover-open-at-point (&optional other-window)
  "Display the session at point; with OTHER-WINDOW, in another window.
Views bind it to RET.  The session shows where the view sends
sessions, see `drover-open'."
  (interactive "P")
  (drover-open (drover-session-at-point) other-window))

(defun drover-open-other-window (session)
  "Display SESSION in another window than the one RET would show it in.
From a list in a side window, that is another window than the one
where you work, split from it when there is none; from a list in an
ordinary window, another window than the list's.  Interactively,
SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (drover-open session t))

(defun drover-open-parent (session &optional other-window)
  "Display the parent session of SESSION, a subagent.
A parent the backend does not list, such as an old session, is opened
by its identifier, in the directory of SESSION.  Interactively, SESSION
is the session at point, and a prefix argument OTHER-WINDOW displays
the parent in another window."
  (interactive (list (drover-session-at-point) current-prefix-arg))
  (let ((parent (or (drover-session-parent session)
                    (user-error "%s is not a subagent" (drover-session-display-title session)))))
    (drover-open (or (drover-parent session)
                     (drover-session-create :backend (drover-session-backend session) :id parent
                                            :directory (drover-session-directory session)))
                 other-window)))

(defun drover-open-subagent (session &optional other-window)
  "Choose a subagent session of SESSION and display it.
With a single subagent, display it without asking.  Interactively,
SESSION is the session at point, and a prefix argument OTHER-WINDOW
displays the subagent in another window."
  (interactive (list (drover-session-at-point) current-prefix-arg))
  (let ((subagents (drover-subagents session)))
    (drover-open (pcase subagents
                   ('nil (user-error "%s has no subagent" (drover-session-display-title session)))
                   (`(,only) only)
                   (_ (require 'drover-attention)
                      (drover-read-session "Subagent: " subagents)))
                 other-window)))

;;;; Peeking

(defconst drover-peek-buffer "*drover peek*"
  "Name of the buffer showing a session's preview.")

(defun drover-peek (session)
  "Show where SESSION stands, without visiting it.
Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (drover-preview
   session
   (lambda (text)
     (with-current-buffer (get-buffer-create drover-peek-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (propertize (drover-session-display-title session) 'face 'drover-heading)
                 "\n\n"
                 (or text "Nothing to show."))
         (goto-char (point-min))
         (special-mode)))
     (display-buffer drover-peek-buffer
                     '((display-buffer-reuse-window display-buffer-below-selected)
                       (window-height . 0.3))))))

;;;; Following the session at point

(defvar-local drover--follow-position nil
  "Point and modification count of the buffer when follow mode last looked.")

(defvar-local drover--follow-key nil
  "Key of the session `drover-follow-mode' last showed.")

(defvar-local drover--follow-window nil
  "Window `drover-follow-mode' last showed a session in.")

(defun drover--follow-target ()
  "Return the window in which `drover-follow-mode' shows a session.
It is the one `drover-main-window-function' returns, else the one
used last, else another window, as `drover-other-window' chooses it."
  (let ((own (selected-window)))
    (or (and drover-main-window-function (funcall drover-main-window-function))
        (and (window-live-p drover--follow-window)
             (not (eq drover--follow-window own))
             (eq (window-frame drover--follow-window) (selected-frame))
             drover--follow-window)
        (drover-other-window own))))

(defun drover--follow (&optional force)
  "Show the session at point in another window if it changed since last time.
With FORCE, show it whatever was shown before."
  (let ((position (cons (point) (buffer-modified-tick))))
    (when (or force (not (equal position drover--follow-position)))
      (setq drover--follow-position position)
      (drover-with-reporting "follow"
        (when-let* ((session (drover-session-at-point t))
                    (key (drover-session-key session))
                    ((or force (not (equal key drover--follow-key))))
                    ((drover-supports-p session :open)))
          (setq drover--follow-key key)
          (let ((window (drover--follow-target)))
            (setq drover--follow-window window)
            ;; Displayed in that window, so that the list stays selected
            ;; even when the backend displays the session later.
            (with-selected-window window
              (drover-open session))))))))

(define-minor-mode drover-follow-mode
  "Show the session at point in another window as point moves.
The list stays selected.  The session shows in the window
`drover-main-window-function' returns, if set, else in another
window of the frame."
  :lighter " Follow"
  :group 'drover
  (if drover-follow-mode
      (progn
        (setq drover--follow-position nil
              drover--follow-key nil)
        (add-hook 'post-command-hook #'drover--follow nil t)
        (drover--follow t))
    (remove-hook 'post-command-hook #'drover--follow t)))

;;;; Lists

(defvar-local drover-toggle-subagents-function nil
  "Command showing or hiding subagents in the current list, or nil.
`drover-toggle-subagents' calls it in lists drover does not draw, those
of other front ends.")

(defun drover-toggle-subagents ()
  "Show every subagent, the active ones, or none, in turn.
In drover's views, this sets `drover-show-subagents' to the next of
`drover-subagent-modes' for every view: all, active, none, then all
again.  In another list, it calls `drover-toggle-subagents-function'."
  (interactive)
  (if drover-toggle-subagents-function
      (call-interactively drover-toggle-subagents-function)
    (drover-set-show-subagents (drover-next-subagent-mode))
    (message "%s" (drover-subagent-mode-description))))

(defun drover-subagent-mode-description (&optional mode)
  "Return a sentence telling which subagents MODE shows.
MODE defaults to the global value of `drover-show-subagents'."
  (pcase (or mode (default-value 'drover-show-subagents))
    ('all "Showing every subagent")
    ('none "Hiding subagents")
    (_ "Showing active subagents")))

(defun drover-refresh ()
  "Refresh what the current buffer shows, with `revert-buffer'.
In a list, or in the buffer of the session at point, that reverts the
current buffer.  In another buffer belonging to a session, such as an
Org buffer where prompts are written, that reverts the session's
buffer."
  (interactive)
  (let* ((session (and (not (drover-session-list-p)) (drover-session-at-point t)))
         (buffer (and session (drover-session-buffer session))))
    (cond ((or (null session) (eq buffer (current-buffer)))
           (call-interactively #'revert-buffer))
          (buffer (with-current-buffer buffer
                    (call-interactively #'revert-buffer)))
          (t (user-error "%s has no buffer to refresh" (drover-session-display-title session))))))

;;;; New sessions

(defun drover--read-backend (operation prompt)
  "Return a backend implementing OPERATION, asking with PROMPT among several."
  (let ((backends (seq-filter (lambda (backend) (functionp (drover-backend-property backend operation)))
                              (drover-backends))))
    (pcase backends
      ('nil (user-error "No backend supports %s" (substring (symbol-name operation) 1)))
      (`(,only) only)
      (_ (let ((labels (mapcar (lambda (backend) (cons (drover-backend-label backend) backend))
                               backends)))
           (cdr (assoc (completing-read prompt labels nil t) labels)))))))

(defun drover-new-session (directory backend)
  "Start a session of BACKEND in DIRECTORY.
Interactively, DIRECTORY is the directory of the session at point, or
`default-directory' without one, and BACKEND the backend of that
session, or the backend that starts sessions, chosen among several."
  (interactive
   (let ((session (drover-session-at-point t)))
     (list (or (and session (drover-session-directory session)) default-directory)
           (if (and session (drover-supports-p session :new))
               (drover-session-backend session)
             (drover--read-backend :new "Start a session of: ")))))
  (let ((start (drover-backend-property backend :new)))
    (unless (functionp start)
      (user-error "%s cannot start sessions" (drover-backend-label backend)))
    (funcall start directory)))

;;;; Projects

(defun drover--projects ()
  "Return the projects of the sessions lists show, sorted, without duplicates."
  (sort (delete-dups (delq nil (mapcar #'drover-session-project (drover-known-sessions))))
        #'string<))

(defun drover-select-project (project)
  "List the sessions of PROJECT, chosen with completion among known projects.
The list (see `drover-list') shows the sessions the filter keeps, and
the filter keeps PROJECT's; \\<drover-session-map>\\[drover-view] in the
list changes or clears it.  Known projects are those of the sessions
lists show."
  (interactive (progn
                 (drover-start)
                 (let ((projects (drover--projects)))
                   (unless projects
                     (user-error "No session belongs to a project"))
                   (list (completing-read "Project: " projects nil t)))))
  (drover-set-filter :project project)
  (drover-list))

;;;; Prompt actions

(defun drover-prompt-send ()
  "Send the prompt, through the prompt action `send'."
  (interactive)
  (drover-prompt-run 'send))

(defun drover-prompt-schedule ()
  "Schedule the prompt, through the prompt action `schedule'."
  (interactive)
  (drover-prompt-run 'schedule))

(defun drover-prompt-clear ()
  "Erase the prompt being written, through the prompt action `clear'."
  (interactive)
  (drover-prompt-run 'clear))

(defun drover-prompt-attach ()
  "Attach the region, or a file or buffer, through the prompt action `attach'."
  (interactive)
  (drover-prompt-run 'attach))

(defun drover-prompt-slash ()
  "Insert a slash command, through the prompt action `slash'."
  (interactive)
  (drover-prompt-run 'slash))

(defun drover-prompt-mention ()
  "Mention a subagent, through the prompt action `mention'."
  (interactive)
  (drover-prompt-run 'mention))

(defun drover-prompt-revert ()
  "Stage a revert of what follows point, through the prompt action `revert'."
  (interactive)
  (drover-prompt-run 'revert))

(defun drover-prompt-revert-cancel ()
  "Cancel the staged revert, through the prompt action `revert-cancel'."
  (interactive)
  (drover-prompt-run 'revert-cancel))

(defun drover-prompt-copy-block ()
  "Copy the code block at point, through the prompt action `copy-block'."
  (interactive)
  (drover-prompt-run 'copy-block))

(defun drover-prompt-agent ()
  "Choose the agent, through the prompt action `agent'."
  (interactive)
  (drover-prompt-run 'agent))

(defun drover-prompt-model ()
  "Choose the model, through the prompt action `model'."
  (interactive)
  (drover-prompt-run 'model))

(defun drover-prompt-effort ()
  "Choose the reasoning effort, through the prompt action `effort'."
  (interactive)
  (drover-prompt-run 'effort))

(defun drover-prompt-directory ()
  "Move the session to another directory, through the prompt action `directory'."
  (interactive)
  (drover-prompt-run 'directory))

(defun drover-prompt-last-answer ()
  "Show the start of the last answer, through the prompt action `last-answer'."
  (interactive)
  (drover-prompt-run 'last-answer))

(defun drover-prompt-open-link ()
  "Open the link at point, through the prompt action `open-link'."
  (interactive)
  (drover-prompt-run 'open-link))

;;;; Session keys

(defvar-keymap drover-session-map
  :doc "Keys acting on the session at point in lists of sessions.
Drover's views inherit it; `drover-session-keys-mode' binds it in
other lists."
  "o" #'drover-open-other-window
  "A" #'drover-respond
  "K" #'drover-interrupt
  "s" #'drover-mark-seen
  "u" #'drover-open-parent
  "c" #'drover-open-subagent
  "r" #'drover-rename
  "x" #'drover-delete
  "z" #'drover-archive
  "Z" #'drover-unarchive
  "g" #'drover-refresh
  "+" #'drover-new-session
  "v" #'drover-peek
  "U" #'drover-toggle-subagents
  "V" #'drover-view)

(define-minor-mode drover-session-keys-mode
  "Bind the keys of `drover-session-map' in a list of sessions.
For lists drover does not draw, those of other front ends, whose
buffers also set `drover-session-at-point-function'.

\\{drover-session-map}"
  :keymap drover-session-map
  :group 'drover)

;;;; The dispatch menu

(defun drover--shown-list-window ()
  "Return the window of the selected frame showing one of drover's views, or nil.
Among several, the one used most recently."
  (let (best)
    (dolist (window (window-list nil 'nomini))
      (when (and (buffer-local-value 'drover-view-p (window-buffer window))
                 (or (null best) (> (window-use-time window) (window-use-time best))))
        (setq best window)))
    best))

(defun drover--context ()
  "Return what the dispatch menu depends on in the current buffer, as a plist.
:session is the session at point or nil, :selection the sessions an
active region selects in a list, or nil (see
`drover-selected-sessions-function'), :list non-nil in a list of
sessions, :view non-nil in one of drover's views, :prompt non-nil in
drover's own prompt buffer (see `drover-prompt-mode'), :window the
window of a list the menu acts on in place of the current buffer, or
nil.

The menu acts on the current buffer when it is a list of sessions, or
when it tells a session at point, as a session buffer does.  Otherwise,
when one of drover's lists is shown in a window of the selected frame,
the most recently used if several, the menu acts on that list: :session,
:list and :view are the list's, its session at point the one at the
window's point, its selection the one of the window's region, and
:window is the window.  :prompt is always the current buffer's.  The
global commands keep to the current buffer."
  (let ((session (drover--context-session))
        (list (drover-session-list-p))
        (selection (drover--context-selection))
        (window nil))
    (unless (or list session)
      (when-let* ((shown (drover--shown-list-window)))
        (with-selected-window shown
          (setq window shown
                session (drover--context-session)
                list (drover-session-list-p)
                selection (drover--context-selection)))))
    (drover--make-context session list window (bound-and-true-p drover-prompt-mode) selection)))

(defun drover--make-context (session list window prompt &optional selection)
  "Return the context of the dispatch menu, see `drover--context'.
SESSION is the session at point, LIST non-nil in a list of sessions,
WINDOW the window of the list the menu acts on, or nil for the current
buffer, PROMPT non-nil in drover's prompt buffer, SELECTION the
sessions a region selects, or nil."
  (list :session session
        :selection selection
        :list (and list t)
        :view (and (if window
                       (buffer-local-value 'drover-view-p (window-buffer window))
                     drover-view-p)
                   t)
        :prompt (and prompt t)
        :window window))

(defun drover--context-again (scope)
  "Return the context of the dispatch menu, which acted on SCOPE, now.
When SCOPE is that of a list in another window, the menu keeps to that
list, whatever the current buffer became meanwhile, such as a session
follow mode showed: only the session at point is read again."
  (let ((window (plist-get scope :window)))
    (if (and (window-live-p window)
             (buffer-local-value 'drover-view-p (window-buffer window)))
        (with-selected-window window
          (drover--make-context (drover--context-session) (drover-session-list-p) window
                                (plist-get scope :prompt) (drover--context-selection)))
      (drover--context))))

(defun drover--context-session ()
  "Return the session at point, or nil, reporting a failing lookup."
  (drover-with-reporting "session at point"
    (drover-session-at-point t)))

(defun drover--context-selection ()
  "Return the sessions a region selects here, or nil, reporting a failing lookup."
  (drover-with-reporting "selected sessions"
    (and drover-selected-sessions-function
         (funcall drover-selected-sessions-function))))

(defun drover--scope ()
  "Return the context of the dispatch menu being set up, else of this buffer."
  (let ((prefix (transient-prefix-object)))
    (if (and prefix (eq (oref prefix command) 'drover-dispatch) (oref prefix scope))
        (oref prefix scope)
      (drover--context))))

(defun drover-dispatch-session (&optional operation predicate)
  "Return the session at point for the dispatch menu, or nil.
It is nil too when its backend lacks OPERATION, a keyword, or when
PREDICATE, a function of the session, returns nil for it."
  (when-let* ((session (plist-get (drover--scope) :session)))
    (and (or (null operation) (drover-supports-p session operation))
         (or (null predicate) (funcall predicate session))
         session)))

(defun drover-dispatch-selection (operation &optional predicate)
  "Return non-nil when the dispatch menu may perform OPERATION on sessions.
With several sessions selected (see `drover-selected-sessions'), when
the backend of one of them performs OPERATION, a keyword, unless it is
nil, and PREDICATE, a function of a session, if any, returns non-nil
for it; otherwise as `drover-dispatch-session' tells of the session at
point."
  (let ((selection (plist-get (drover--scope) :selection)))
    (if (cdr selection)
        (seq-some (lambda (session)
                    (and (or (null operation) (drover-supports-p session operation))
                         (or (null predicate) (funcall predicate session))))
                  selection)
      (drover-dispatch-session operation predicate))))

(defun drover-dispatch-list-p ()
  "Return non-nil when the dispatch menu acts on a list of sessions."
  (plist-get (drover--scope) :list))

(defun drover-dispatch-view-p ()
  "Return non-nil when the dispatch menu acts on one of drover's views."
  (plist-get (drover--scope) :view))

(defun drover-dispatch-in-prompt-p ()
  "Return non-nil when the dispatch menu acts on drover's own prompt buffer."
  (plist-get (drover--scope) :prompt))

(defun drover-dispatch-org-task-p ()
  "Return non-nil when the dispatch menu may start a prompt for an Org task.
That is in an Org buffer other than drover's prompt, which is one too:
`drover-prompt-org-task' finds the task point is under."
  (and (derived-mode-p 'org-mode) (not (drover-dispatch-in-prompt-p))))

(defun drover-dispatch-prompt-p (action)
  "Return non-nil when prompt ACTION applies from where the dispatch menu acts.
For `compact' and `fork', a list's session at point whose backend
operates on it counts.  Prompt actions run in the current buffer, so a
list shown in another window does not count."
  (let* ((scope (drover--scope))
         (here (not (plist-get scope :window)))
         (session (and here (plist-get scope :session)))
         (list (and here (plist-get scope :list))))
    (or (drover--prompt-target action session list t)
        (and list
             (when-let* ((operation (alist-get action drover--backend-prompt-actions)))
               (drover-dispatch-session operation))))))

(defun drover-dispatch-list-return-p ()
  "Return non-nil when RET in the dispatch menu opens the session at point.
It does so in the menu's List group, while the menu acts on one of
drover's lists, unless the current buffer sends prompts: there RET
stays Send, as losing a prompt to a slip is worse than reaching for o."
  (and (drover-dispatch-view-p)
       (not (drover-prompt-defines-p (current-buffer) 'send))))

(defun drover--dispatch-window ()
  "Return the window of the list the dispatch menu acts on, when not current.
It is nil when the menu acts on the current buffer, and when that
window no longer shows one of drover's views."
  (let ((window (plist-get (drover--scope) :window)))
    (and (window-live-p window)
         (buffer-local-value 'drover-view-p (window-buffer window))
         window)))

(defun drover--call-in-window (window function)
  "Call FUNCTION with no argument, WINDOW selected, and return its value.
Selection then returns to the window selected before, unless FUNCTION
selected another window or showed another buffer in WINDOW, such as a
session it opened: the user goes where it took them."
  (let ((origin (selected-window))
        (buffer (window-buffer window)))
    (select-window window 'norecord)
    (unwind-protect
        (funcall function)
      (when (and (window-live-p origin)
                 (eq (selected-window) window)
                 (eq (window-buffer window) buffer))
        (select-window origin 'norecord)))))

(defun drover--dispatch-in-list (function &rest arguments)
  "Apply FUNCTION to ARGUMENTS in the window of the list the menu acts on.
Without such a window, in the current buffer.  It is the advice of the
dispatch menu's session commands, for their interactive specification
and body alike, so that they read and act on the session at the point
of that window, as typed there."
  (if-let* ((window (drover--dispatch-window)))
      (drover--call-in-window window (lambda () (apply function arguments)))
    (apply function arguments)))

(defun drover--run-local-hook (hook)
  "Run the functions of HOOK local to the current buffer, not the global ones."
  (when (local-variable-p hook)
    (dolist (function (symbol-value hook))
      (unless (eq function t)
        (drover-with-reporting "list hook"
          (funcall function))))))

(defun drover--dispatch-navigate (function &rest arguments)
  "Apply FUNCTION, a command moving point in a list, to ARGUMENTS.
It is the advice of the menu's List group.  In a list shown in another
window, the command moves that window's point, between the list's own
`pre-command-hook' and `post-command-hook' functions, as if typed
there, so that follow mode and highlights follow it.  The menu, which
stays, then acts on the new session at point."
  (prog1 (if-let* ((window (drover--dispatch-window)))
             (drover--call-in-window window
                                     (lambda ()
                                       (drover--run-local-hook 'pre-command-hook)
                                       (prog1 (apply function arguments)
                                         (drover--run-local-hook 'post-command-hook))))
           (apply function arguments))
    (when-let* ((prefix (transient-prefix-object))
                ((eq (oref prefix command) 'drover-dispatch)))
      (oset prefix scope (drover--context-again (oref prefix scope))))))

(defun drover--dispatch-list-description ()
  "Return the title of the dispatch menu's List group, naming the session at point.
With several sessions selected, it tells how many instead."
  (let ((scope (drover--scope)))
    (cond ((cdr (plist-get scope :selection))
           (format "List: %d sessions selected" (length (plist-get scope :selection))))
          ((plist-get scope :session)
           (concat "List: " (truncate-string-to-width
                             (replace-regexp-in-string
                              "[\n\r\t]+" " "
                              (format "%s" (drover-session-display-title (plist-get scope :session))))
                             60 nil nil (truncate-string-ellipsis))))
          (t "List"))))

(defun drover--dispatch-grouped-p ()
  "Return non-nil when the list the dispatch menu acts on groups sessions."
  (and (drover-dispatch-view-p) drover-group-by))

(defun drover--can-start-p ()
  "Return non-nil when a backend starts sessions."
  (seq-some (lambda (backend) (functionp (drover-backend-property backend :new)))
            (drover-backends)))

(defun drover--has-subagents-p (session)
  "Return non-nil when SESSION has subagent sessions."
  (drover-subagents session))

;;;###autoload(autoload 'drover-dispatch "drover-keys" nil t)
(defcustom drover-menu-display-action
  '(display-buffer-at-bottom (dedicated . t) (inhibit-same-window . t))
  "How drover's menus are displayed, as an action of `display-buffer'.
The default opens a window of its own at the bottom of the frame, rather
than the bottom side window `transient-display-buffer-action' uses by
default: that window may show a prompt buffer, which the menu would
then hide from the commands it runs."
  :type 'sexp
  :group 'drover)

(transient-define-prefix drover-dispatch ()
  "Act on sessions of coding agents, on the session at point, or on the prompt.
Global commands work everywhere; session commands act on the session at
point, in lists and in prompt buffers; prompt commands perform the
prompt actions of the current buffer.  Commands that do not apply here
are shown, greyed out.

From a buffer telling no session, such as the prompt or a file, while
one of drover's lists shows in a window of the frame, session commands
act on that list, on the session at its window's point (see
`drover--context').  While the menu acts on one of drover's lists, its
List group moves that point, the menu staying open and following the
new session at point, and RET opens the session, but in a buffer that
sends prompts, where RET stays Send."
  :refresh-suffixes t
  ["Global"
   [("j" "Next" drover-next)
    ("b" "Select" drover-select)
    ("\"" "Prompt" drover-prompt-dwim :inapt-if drover-dispatch-in-prompt-p)
    ("n" "Task prompt" drover-prompt-org-task :inapt-if-not drover-dispatch-org-task-p)
    ("'" "Show prompt" drover-prompt-toggle)
    ("#" "Scheduled" drover-schedule-list)]
   [("l" "List" drover-list)
    ("L" "Side list" drover-list-side)
    ("p" "Project" drover-select-project)]
   [("a" "Agent" drover-prompt-agent :inapt-if-not (lambda () (drover-dispatch-prompt-p 'agent)))
    ("m" "Model" drover-prompt-model :inapt-if-not (lambda () (drover-dispatch-prompt-p 'model)))
    ("e" "Effort" drover-prompt-effort :inapt-if-not (lambda () (drover-dispatch-prompt-p 'effort)))
    ("d" "Directory" drover-prompt-directory
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'directory)))]
   [("C" "Compact" drover-compact :inapt-if-not (lambda () (drover-dispatch-prompt-p 'compact)))
    ("f" "Fork" drover-fork :inapt-if-not (lambda () (drover-dispatch-prompt-p 'fork)))
    ("." "Last answer" drover-prompt-last-answer
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'last-answer)))
    ("O" "Open link" drover-prompt-open-link
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'open-link)))]]
  ["Session"
   [:advice* drover--dispatch-in-list
    ("o" "Other window" drover-open-other-window
     :inapt-if-not (lambda () (drover-dispatch-session :open)))
    ("u" "Parent" drover-open-parent
     :inapt-if-not (lambda () (drover-dispatch-session :open #'drover-session-parent)))
    ("c" "Subagent" drover-open-subagent
     :inapt-if-not (lambda () (drover-dispatch-session :open #'drover--has-subagents-p)))
    ("v" "Peek" drover-peek
     :inapt-if-not (lambda () (and (drover-dispatch-list-p) (drover-dispatch-session :preview))))]
   [:advice* drover--dispatch-in-list
    ("A" "Answer" drover-respond
     :inapt-if-not (lambda () (drover-dispatch-session :respond #'drover-session-requests)))
    ("K" "Interrupt" drover-interrupt
     :inapt-if-not (lambda () (drover-dispatch-selection :interrupt #'drover--running-p)))
    ("s" "Mark seen" drover-mark-seen
     :inapt-if-not (lambda () (drover-dispatch-selection :mark-seen #'drover--ended-unseen-p)))
    ("+" "New session" drover-new-session
     :inapt-if-not (lambda () (and (or (drover-dispatch-list-p) (drover-dispatch-session))
                                   (drover--can-start-p))))
    ("g" "Refresh" drover-refresh
     :inapt-if-not (lambda () (or (drover-dispatch-list-p) (drover-dispatch-session))))]
   [:advice* drover--dispatch-in-list
    ("r" "Rename" drover-rename :inapt-if-not (lambda () (drover-dispatch-session :rename)))
    ("x" "Delete" drover-delete :inapt-if-not (lambda () (drover-dispatch-selection :delete)))
    ("z" "Archive" drover-archive
     :inapt-if-not (lambda () (drover-dispatch-selection nil #'drover-archivable-p)))
    ("Z" "Unarchive" drover-unarchive
     :inapt-if-not (lambda () (drover-dispatch-selection nil #'drover-unarchivable-p)))]
   [:advice* drover--dispatch-in-list
    ("U" "Subagents" drover-toggle-subagents :inapt-if-not drover-dispatch-list-p)
    ("V" "View" drover-view :inapt-if-not drover-dispatch-view-p)]]
  ["Prompt"
   [("RET" "Send" drover-prompt-send :inapt-if-not (lambda () (drover-dispatch-prompt-p 'send))
     :if-not drover-dispatch-list-return-p)
    ("S-<return>" "Schedule" drover-prompt-schedule
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'schedule)))
    ("i" "Attach" drover-prompt-attach :inapt-if-not (lambda () (drover-dispatch-prompt-p 'attach)))
    ("k" "Clear" drover-prompt-clear :inapt-if-not (lambda () (drover-dispatch-prompt-p 'clear)))]
   [("/" "Slash command" drover-prompt-slash :inapt-if-not (lambda () (drover-dispatch-prompt-p 'slash)))
    ("@" "Mention" drover-prompt-mention :inapt-if-not (lambda () (drover-dispatch-prompt-p 'mention)))
    ("y" "Copy block" drover-prompt-copy-block
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'copy-block)))]
   [("t" "Revert here" drover-prompt-revert :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert)))
    ("T" "Cancel revert" drover-prompt-revert-cancel
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert-cancel)))]]
  ;; Only while the menu acts on one of drover's lists, here or in
  ;; another window.  Moving stays in the menu, which follows the new
  ;; session at point.
  [:description drover--dispatch-list-description
   :if drover-dispatch-view-p
   [:advice drover--dispatch-navigate
    ("C-n" "Next session" drover-list-next-session :transient t)
    ("C-p" "Previous session" drover-list-previous-session :transient t)]
   [:advice drover--dispatch-navigate
    ("M-n" "Next group" drover-list-next-group :transient t
     :inapt-if-not drover--dispatch-grouped-p)
    ("M-p" "Previous group" drover-list-previous-group :transient t
     :inapt-if-not drover--dispatch-grouped-p)]
   [:advice drover--dispatch-navigate
    ("M-<" "First session" drover-list-first-session :transient t)
    ("M->" "Last session" drover-list-last-session :transient t)]
   [:advice* drover--dispatch-in-list
    ("RET" "Open" drover-open-at-point
     :if drover-dispatch-list-return-p
     :inapt-if-not (lambda () (drover-dispatch-session :open)))]]
  ;; The key opening the menu closes it, as users bind it globally.
  [:hide always
   ("C-'" "Close" transient-quit-one)]
  (interactive)
  (transient-setup 'drover-dispatch nil nil :scope (drover--context)
                   :display-action drover-menu-display-action))

;;;; Reading view settings

(defun drover--check-view ()
  "Signal a `user-error' unless the current buffer is one of drover's views."
  (unless drover-view-p
    (user-error "Only drover's session list sorts, groups and filters sessions")))

(defun drover--parse-since (string &optional now)
  "Return the float time STRING, read relative to NOW, designates, or nil.
STRING is \"today\", \"yesterday\", an age such as \"30m\", \"24h\",
\"3d\", \"7 days\" or \"2w\", or a date such as \"2026-10-01\"; an empty
STRING is nil.  NOW defaults to the current time.  Signal a
`user-error' for anything else."
  (let ((string (string-trim string))
        (now (or now (float-time)))
        (case-fold-search t))
    (cond
     ((string-empty-p string) nil)
     ((string-equal-ignore-case string "today") (drover--start-of-day now))
     ((string-equal-ignore-case string "yesterday") (drover--start-of-day (- now 86400)))
     ((string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)?\\) *\\(m\\|min\\|mins\\|minutes?\\|h\\|hours?\\|d\\|days?\\|w\\|weeks?\\)\\'"
                    string)
      (- now (* (string-to-number (match-string 1 string))
                (pcase (aref (match-string 2 string) 0)
                  ((or ?m ?M) 60)
                  ((or ?h ?H) 3600)
                  ((or ?d ?D) 86400)
                  (_ 604800)))))
     ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" string)
      (float-time (encode-time (list 0 0 0
                                     (string-to-number (match-string 3 string))
                                     (string-to-number (match-string 2 string))
                                     (string-to-number (match-string 1 string))
                                     nil -1 nil))))
     (t (user-error "Not a time: %s (try today, 24h, 7 days or 2026-10-01)" string)))))

(defun drover--read-choice (prompt labels current)
  "Read one of LABELS, an alist (VALUE . LABEL), with PROMPT; return its VALUE.
CURRENT is the value now, offered as the default."
  (let* ((names (mapcar #'cdr labels))
         (default (alist-get current labels))
         (name (completing-read (format-prompt prompt default) names nil t nil nil default)))
    (car (rassoc name labels))))

(defun drover--read-known (prompt field current)
  "Read one of the values of FIELD among known sessions, with PROMPT.
FIELD is a function of a session, such as `drover-session-agent';
CURRENT is the value now, or nil.  An empty answer returns nil."
  (let* ((values (sort (delete-dups (delq nil (mapcar field (drover-known-sessions)))) #'string<))
         (answer (completing-read (if current
                                      (format "%s (now %s, empty for any): " prompt current)
                                    (format "%s (empty for any): " prompt))
                                  values nil t)))
    (unless (string-empty-p answer) answer)))

(defun drover--read-filter-project (current)
  "Read the project to list, CURRENT being the one listed now, or nil."
  (drover--read-known "Project" #'drover-session-project current))

(defun drover--read-filter-agent (current)
  "Read the agent whose sessions to list, CURRENT being the one now, or nil."
  (drover--read-known "Agent" #'drover-session-agent current))

(defun drover--read-filter-model (current)
  "Read the model whose sessions to list, CURRENT being the one now, or nil."
  (drover--read-known "Model" #'drover-session-model current))

(defun drover--read-filter-provider (current)
  "Read the provider whose sessions to list, CURRENT being the one now, or nil."
  (drover--read-known "Provider" #'drover-session-provider-name current))

(defun drover--read-filter-statuses (current)
  "Read the statuses to list, CURRENT being those listed now."
  (let* ((labels (mapcar (lambda (status) (cons (opencode-session-status-label status) status))
                         drover-status-order))
         (chosen (completing-read-multiple
                  "Statuses (empty for any): " labels nil t
                  (and current (mapconcat #'opencode-session-status-label current ",")))))
    (delete-dups (delq nil (mapcar (lambda (label) (cdr (assoc label labels))) chosen)))))

(defun drover--read-filter-since (_current)
  "Read since when sessions must have been active to be listed."
  (drover--parse-since
   (completing-read "Updated since (today, 24h, 7 days, 3d…; empty for any): "
                    '("today" "24h" "7 days" "30 days"))))

(defun drover--read-filter-search (current)
  "Read the text sessions must contain to be listed, CURRENT being the one now."
  (let ((search (read-string "Search (empty for any): " current)))
    (unless (string-empty-p (string-trim search)) search)))

(defun drover--filter-label (key value)
  "Return VALUE of the filter member KEY as the View menu shows it."
  (pcase key
    (:statuses (drover-filter-statuses-label value))
    (:since (drover-filter-since-label value))
    (:search (format "\"%s\"" value))
    (_ (format "%s" value))))

(defun drover-filter-clear ()
  "List every session in every view."
  (interactive)
  (setq drover-filter nil)
  (drover-view-settings-changed))

;;;; The View menu

(defconst drover-sort-labels
  '((urgency . "urgency") (title . "title") (project . "project") (branch . "branch")
    (agent . "agent") (model . "model") (provider . "provider") (updated . "last activity")
    (created . "creation"))
  "Display name of each field of `drover-sort-fields'.")

(defconst drover-grouping-labels
  '((status . "status") (project . "project") (branch . "branch") (agent . "agent")
    (model . "model") (provider . "provider") (updated . "last activity") (nil . "nothing"))
  "Display name of each grouping of `drover-groupings'.")

(defclass drover--setting (transient-variable)
  ((getter :initarg :getter :initform nil
           :documentation "Function of no argument returning the value.")
   (setter :initarg :setter :initform nil
           :documentation "Function of a value setting it and redrawing views.")
   (chooser :initarg :chooser
            :documentation "Function of the current value returning the new one.")
   (options :initarg :options :initform nil
            :documentation "Alist (VALUE . LABEL) of every value, all shown, or nil.")
   (labeler :initarg :labeler :initform nil
            :documentation "Function of the value returning how it shows, or nil."))
  "An infix of `drover-view' showing one setting of drover's views and changing it.
With OPTIONS, it shows every value, the current one highlighted, as
Magit shows choices; otherwise the current value, as LABELER returns
it, or \"any\" for nil.")

(defclass drover--filter-setting (drover--setting)
  ((filter-key :initarg :filter-key :documentation "The member of `drover-filter' it sets."))
  "An infix of `drover-view' setting one member of the variable `drover-filter'.")

(cl-defgeneric drover--setting-value (obj)
  "Return the current value of the setting OBJ shows.")

(cl-defmethod drover--setting-value ((obj drover--setting))
  "Return the value the getter of OBJ returns."
  (funcall (oref obj getter)))

(cl-defmethod drover--setting-value ((obj drover--filter-setting))
  "Return the member of the variable `drover-filter' OBJ sets."
  (drover--filter-value drover-filter (oref obj filter-key)))

(cl-defgeneric drover--setting-set (obj value)
  "Set the setting OBJ shows to VALUE, redrawing views.")

(cl-defmethod drover--setting-set ((obj drover--setting) value)
  "Set the setting of OBJ to VALUE through its setter."
  (funcall (oref obj setter) value))

(cl-defmethod drover--setting-set ((obj drover--filter-setting) value)
  "Set the member of the variable `drover-filter' OBJ sets to VALUE."
  (drover-set-filter (oref obj filter-key) value))

(cl-defgeneric drover--setting-label (obj value)
  "Return how VALUE of the setting OBJ shows reads, or nil for none set.")

(cl-defmethod drover--setting-label ((obj drover--setting) value)
  "Return VALUE through the labeler of OBJ, or as is."
  (if-let* ((labeler (oref obj labeler)))
      (funcall labeler value)
    (and value (format "%s" value))))

(cl-defmethod drover--setting-label ((obj drover--filter-setting) value)
  "Return VALUE of the filter member OBJ sets as the menu shows it, or nil."
  (and value (drover--filter-label (oref obj filter-key) value)))

(cl-defmethod transient-init-value ((obj drover--setting))
  "Set the value of OBJ to the current value of its setting."
  (oset obj value (drover--setting-value obj)))

(cl-defmethod transient-infix-read ((obj drover--setting))
  "Return the new value of the setting of OBJ."
  (funcall (oref obj chooser) (drover--setting-value obj)))

(cl-defmethod transient-infix-set ((obj drover--setting) value)
  "Set the setting of OBJ to VALUE."
  (oset obj value value)
  (drover--setting-set obj value))

(cl-defmethod transient-format-value ((obj drover--setting))
  "Return the value of the setting of OBJ as the menu shows it."
  (let ((value (drover--setting-value obj)))
    (if-let* ((options (oref obj options)))
        (concat (propertize "[" 'face 'transient-delimiter)
                (mapconcat (pcase-lambda (`(,option . ,label))
                             (propertize label 'face (if (eq option value)
                                                         'transient-value
                                                       'transient-inactive-value)))
                           options
                           (propertize "|" 'face 'transient-delimiter))
                (propertize "]" 'face 'transient-delimiter))
      (if-let* ((label (drover--setting-label obj value)))
          (propertize label 'face 'transient-value)
        (propertize "any" 'face 'transient-inactive-value)))))

(transient-define-infix drover-view--sort-by ()
  "Sort the sessions of every view by a field."
  :class 'drover--setting
  :description "field"
  :format " %k %v"
  :getter (lambda () drover-sort-by)
  :setter (lambda (field) (drover-set-sort field drover-sort-descending))
  :chooser (lambda (current) (drover--read-choice "Sort by" drover-sort-labels current))
  :labeler (lambda (field) (alist-get field drover-sort-labels)))

(transient-define-infix drover-view--sort-order ()
  "Sort the sessions of every view in ascending or descending order."
  :class 'drover--setting
  :description "order"
  :format " %k %v"
  :getter (lambda () drover-sort-descending)
  :setter (lambda (descending) (drover-set-sort drover-sort-by descending))
  :chooser #'not
  :options '((nil . "ascending") (t . "descending")))

(transient-define-infix drover-view--group-by ()
  "Group the sessions of every view."
  :class 'drover--setting
  :description "grouping"
  :format " %k %v"
  :getter (lambda () drover-group-by)
  :setter #'drover-set-group-by
  :chooser (lambda (current) (drover--read-choice "Group by" drover-grouping-labels current))
  :labeler (lambda (grouping) (alist-get grouping drover-grouping-labels)))

(transient-define-infix drover-view--filter-project ()
  "List the sessions of one project in every view."
  :class 'drover--filter-setting
  :description "project"
  :filter-key :project
  :chooser #'drover--read-filter-project)

(transient-define-infix drover-view--filter-statuses ()
  "List the sessions of some statuses in every view."
  :class 'drover--filter-setting
  :description "status"
  :filter-key :statuses
  :chooser #'drover--read-filter-statuses)

(transient-define-infix drover-view--filter-agent ()
  "List the sessions of one agent in every view."
  :class 'drover--filter-setting
  :description "agent"
  :filter-key :agent
  :chooser #'drover--read-filter-agent)

(transient-define-infix drover-view--filter-model ()
  "List the sessions of one model in every view."
  :class 'drover--filter-setting
  :description "model"
  :filter-key :model
  :chooser #'drover--read-filter-model)

(transient-define-infix drover-view--filter-provider ()
  "List the sessions of one provider in every view."
  :class 'drover--filter-setting
  :description "provider"
  :filter-key :provider
  :chooser #'drover--read-filter-provider)

(transient-define-infix drover-view--filter-since ()
  "List the sessions active since a time in every view."
  :class 'drover--filter-setting
  :description "updated"
  :filter-key :since
  :chooser #'drover--read-filter-since)

(transient-define-infix drover-view--filter-search ()
  "List the sessions containing a text in every view."
  :class 'drover--filter-setting
  :description "text"
  :filter-key :search
  :chooser #'drover--read-filter-search)

(defun drover--view-window ()
  "Return the window of the list the View menu acts on.
It is the window selected when the menu opened, which shows the list,
even when the dispatch menu opened it from another window."
  (let ((prefix (transient-prefix-object)))
    (or (and prefix
             (eq (oref prefix command) 'drover-view)
             (window-live-p (oref prefix scope))
             (oref prefix scope))
        (selected-window))))

(transient-define-infix drover-view--follow ()
  "Show the session at point in another window as point moves, or stop."
  :class 'drover--setting
  :description "follow"
  :getter (lambda () (buffer-local-value 'drover-follow-mode (window-buffer (drover--view-window))))
  :setter (lambda (on)
            ;; Follow mode is the list's own, and reads its window's point.
            (with-selected-window (drover--view-window)
              (drover-follow-mode (if on 1 -1))))
  :chooser #'not
  :options '((nil . "off") (t . "on")))

(transient-define-infix drover-view--subagents ()
  "Show every subagent, the active ones, or none, in every view."
  :class 'drover--setting
  :description "subagents"
  :getter (lambda () (default-value 'drover-show-subagents))
  :setter #'drover-set-show-subagents
  :chooser #'drover-next-subagent-mode
  :options drover-subagent-mode-labels)

(transient-define-infix drover-view--history ()
  "Show or hide the older sessions of the backends in every view."
  :class 'drover--setting
  :description "older"
  :getter (lambda () drover-show-history)
  :setter #'drover-set-show-history
  :chooser #'not
  :options '((nil . "hidden") (t . "shown")))

(transient-define-infix drover-view--archived ()
  "Show or hide the archived sessions in every view."
  :class 'drover--setting
  :description "archived"
  :getter (lambda () drover-show-archived)
  :setter #'drover-set-show-archived
  :chooser #'not
  :options '((nil . "hidden") (t . "shown")))

;;;; Columns

(defvar drover-list-fields)
(defvar drover-list-hidden-fields)

(defconst drover-list-field-labels
  '((location . "location") (branch . "branch") (agent . "agent") (model . "model") (provider . "provider")
    (context . "context") (created . "created") (age . "age") (cache . "cache")
    (summary . "summary"))
  "Display name of each field of `drover-list-fields' the user may hide.")

(defun drover-list-field-enabled-p (field)
  "Return non-nil unless FIELD is one of `drover-list-hidden-fields'."
  (not (memq field (bound-and-true-p drover-list-hidden-fields))))

(defun drover-set-list-field (field enabled)
  "Let the list show FIELD when ENABLED is non-nil, else hide it, and redraw.
FIELD is a field of `drover-list-fields' other than the title.  This
changes `drover-list-hidden-fields' for this session; customize it to
keep the change."
  (when (eq field 'title)
    (error "The title always shows"))
  (setq drover-list-hidden-fields
        (if enabled
            (remq field drover-list-hidden-fields)
          (cons field (remq field drover-list-hidden-fields))))
  (drover-view-settings-changed))

(defclass drover--column-setting (drover--setting)
  ((field :initarg :field :documentation "The field of `drover-list-fields' it lets show."))
  "An infix of `drover-view' letting the list show one field, or not.
It shows the name of the field as Magit shows a switch: highlighted
while the list may show the field, dimmed otherwise.")

(cl-defmethod drover--setting-value ((obj drover--column-setting))
  "Return non-nil when the list may show the field of OBJ."
  (drover-list-field-enabled-p (oref obj field)))

(cl-defmethod drover--setting-set ((obj drover--column-setting) value)
  "Let the list show the field of OBJ when VALUE is non-nil."
  (drover-set-list-field (oref obj field) value))

(cl-defmethod transient-format-value ((obj drover--column-setting))
  "Return the name of the field of OBJ, highlighted when the list may show it."
  (propertize (alist-get (oref obj field) drover-list-field-labels)
              'face (if (drover--setting-value obj) 'transient-value 'transient-inactive-value)))

(defun drover--list-field-p (field)
  "Return non-nil when FIELD is one of `drover-list-fields'.
Before the list is loaded, any field is."
  (or (not (boundp 'drover-list-fields))
      (assq field drover-list-fields)))

(defmacro drover--define-column-switch (field)
  "Define the infix of the View menu letting the list show FIELD, or not."
  `(transient-define-infix ,(intern (format "drover-view--column-%s" field)) ()
     ,(format "Let the list show the field `%s', or not." field)
     :class 'drover--column-setting
     :field ',field
     :format " %k %v"
     :chooser #'not
     :if (lambda () (drover--list-field-p ',field))))

(drover--define-column-switch location)
(drover--define-column-switch branch)
(drover--define-column-switch agent)
(drover--define-column-switch model)
(drover--define-column-switch provider)
(drover--define-column-switch context)
(drover--define-column-switch created)
(drover--define-column-switch age)
(drover--define-column-switch cache)
(drover--define-column-switch summary)

(transient-define-prefix drover-view ()
  "Choose how drover's session list sorts, groups and filters sessions.
Each setting shows its value; changing one redraws the list at once,
and the menu stays open.  Sorting keeps subagents under their parent.
A filter left empty keeps every session.  The toggles turn follow mode
on in the list, cycle the subagents shown (all, active, none), show
older sessions, and show archived sessions.  The columns are the fields the list may show, as
room allows: each switch lets a field show, or hides it, for this
session (`drover-list-hidden-fields' keeps the choice)."
  :refresh-suffixes t
  [["Sort by"
    ("o" drover-view--sort-by)
    ("r" drover-view--sort-order)]
   ["Group by"
    ("g" drover-view--group-by)]
   ["Filter by"
    ("p" drover-view--filter-project)
    ("s" drover-view--filter-statuses)
    ("a" drover-view--filter-agent)
    ("m" drover-view--filter-model)
    ("v" drover-view--filter-provider)
    ("d" drover-view--filter-since)
    ("f" drover-view--filter-search)
    ("c" "clear" drover-filter-clear :transient t)]
   ["Toggles"
    ("F" drover-view--follow)
    ("U" drover-view--subagents)
    ("O" drover-view--history :inapt-if-not drover-history-available-p)
    ("A" drover-view--archived :inapt-if-not drover-archived-available-p)]]
  ["Columns"
   :class transient-row
   ("-l" drover-view--column-location)
   ("-b" drover-view--column-branch)
   ("-a" drover-view--column-agent)
   ("-m" drover-view--column-model)
   ("-r" drover-view--column-provider)
   ("-x" drover-view--column-context)
   ("-c" drover-view--column-created)
   ("-t" drover-view--column-age)
   ("-p" drover-view--column-cache)
   ("-s" drover-view--column-summary)]
  ;; The key opening the menu closes it.
  [:hide always
   ("V" "Close" transient-quit-one)]
  (interactive)
  (drover--check-view)
  ;; The list's window, from which the dispatch menu may have opened it.
  (transient-setup 'drover-view nil nil :scope (selected-window)
                   :display-action drover-menu-display-action))

(provide 'drover-keys)
;;; drover-keys.el ends here
