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

;; Four sets of keys, none sharing a key, so that all four fit in one
;; menu, `drover-dispatch':
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
;; - View keys change the settings drover's lists share, wherever one of
;;   them is in sight, through the menu: options on `=', switches on
;;   `-', as in Magit's menus.  The settings of a field share a row of
;;   the Field table (`drover--menu-table'): its filter, then its
;;   column, on the same letter, such as =m and -m for the model.
;;   `drover-view', the View menu, V in the lists, shows them alone,
;;   on the same keys, and the menu of the search those scoping it.
;;
;; Settings show their value beside their key, and changing one leaves
;; the menu open.  The menu has no fixed shape: its groups fill columns
;; of balanced height as wide as its window (`drover--menu-flow').  What
;; cannot apply where it opens is left out: the view settings while no
;; list is in sight, the prompt keys editing or reading a prompt buffer
;; elsewhere; what may, greyed out.
;;
;; While the menu acts on drover's list, here or beside, a List group
;; moves in that list, the menu staying open: C-n, C-p, M-n, M-p, M-<
;; and M->, keys of no other set; RET opens the session at point, but
;; in a buffer sending prompts, where it stays the prompt key Send, and
;; from another buffer while drover's prompt holds a prompt to send,
;; where RET sends it.  In the list itself, RET opens, and M-RET sends
;; the prompt: the menu never shows both RET.  The digits open the
;; sessions at the top of their trees by number, 0 the last, as in the
;; list: one entry shows them all.
;;
;; Bind `drover-dispatch' globally, for instance to C-'; drover binds no
;; global key.  Other packages add their own global commands to the menu
;; with `transient-append-suffix' and `transient-insert-suffix', placed
;; relative to drover's keys: more ways to pick a session or prompt
;; after "'", the Prompt group's last key (", n and '), before the
;; scheduled prompts of "#", or after "b"; more lists of sessions after
;; "p"; more prompt commands after "O".
;;
;; Any key opening the menu closes it, and q, as C-g, closes every menu
;; of drover's.  The echo area and hints name the key Emacs's help names
;; for the menu, or the name `drover-dispatch-key' gives it, such as
;; Caps Lock for a keyboard sending it as <f19>.

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
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 21))
  (load "drover-core" nil t))

(autoload 'drover-next "drover-attention" nil t)
(autoload 'drover-filter-read "drover-filter")
(autoload 'drover-select "drover-attention" nil t)
(autoload 'drover-list "drover-list" nil t)
(autoload 'drover-usage "drover-usage-report" nil t)
(autoload 'drover-list-side "drover-list" nil t)
(autoload 'drover-list-next-session "drover-list" nil t)
(autoload 'drover-list-previous-session "drover-list" nil t)
(autoload 'drover-list-next-group "drover-list" nil t)
(autoload 'drover-list-previous-group "drover-list" nil t)
(autoload 'drover-list-first-session "drover-list" nil t)
(autoload 'drover-list-last-session "drover-list" nil t)
(autoload 'drover-list-open-numbered "drover-list" nil t)
(autoload 'drover-prompt-dwim "drover-prompt" nil t)
(autoload 'drover-prompt-org-task "drover-prompt" nil t)
(autoload 'drover-prompt-send-to "drover-prompt")
(autoload 'drover-task-start "drover-task" nil t)
(autoload 'drover-task-finish "drover-task" nil t)
(autoload 'drover-task-link-session "drover-task" nil t)
(autoload 'drover-task-sweep "drover-task" nil t)
(autoload 'drover-prompt-toggle "drover-prompt" nil t)
(autoload 'drover-schedule-list "drover-schedule" nil t)
(autoload 'drover-goal "drover-goal" nil t)
(autoload 'drover-goal-list "drover-goal" nil t)
(autoload 'drover-fanout "drover-fanout" nil t)
(autoload 'drover-search "drover-search" nil t)
(autoload 'drover-review "drover-review" nil t)
(autoload 'drover-reviewable-p "drover-review")

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
                     ;; Of another host, the parent is its too.
                     (drover-session-create :backend (drover-session-backend session) :id parent
                                            :directory (drover-session-directory session)
                                            :host (drover-session-host session)
                                            :origin (and (drover-session-host session) (car-safe parent))))
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

;;;; Burying quiet sessions

(defun drover--own-buffer-p (buffer)
  "Return non-nil when BUFFER is one of drover's lists or its prompt."
  (with-current-buffer buffer
    (or drover-view-p (bound-and-true-p drover-prompt-mode))))

(defun drover--quiet-buffers ()
  "Return the buffers of the quiet sessions of this host, in the backends' order.
A quiet session is one that does not need the user, its status not one
of `drover-attention-statuses': idle, working or retrying.  Its buffer
is the one its backend shows it in (see `drover-session-buffer').  A
buffer showing a session that needs the user too is left out, and so
are the buffers of a conversation one backend tells needs the user,
whatever another backend showing it tells.  Sessions of other hosts
have no buffer here, and drover's lists and prompt are never among
them."
  (let ((busy (make-hash-table :test #'eq))
        (conversations (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'eq))
        (candidates nil)
        (quiet nil))
    (dolist (session (drover--live-sessions))
      (unless (drover-session-host session)
        (let ((buffer (drover-session-buffer session))
              (key (drover-session-engine-key session)))
          (cond ((drover-session-attention-p session)
                 (when buffer (puthash buffer t busy))
                 (when key (puthash key t conversations)))
                (buffer (push (cons buffer key) candidates))))))
    (pcase-dolist (`(,buffer . ,key) (nreverse candidates))
      (unless (or (gethash buffer busy)
                  (and key (gethash key conversations))
                  (gethash buffer seen)
                  (drover--own-buffer-p buffer))
        (puthash buffer t seen)
        (push buffer quiet)))
    (nreverse quiet)))

(defun drover--made-for-p (window buffer)
  "Return non-nil when the frame or tab of WINDOW was made to show BUFFER.
As the `quit-restore' parameter of WINDOW records it, from which
`quit-restore-window' deletes that frame or closes that tab."
  (let ((quit-restore (window-parameter window 'quit-restore)))
    (and (memq (nth 1 quit-restore) '(frame tab))
         (eq (nth 3 quit-restore) buffer))))

(defun drover--restores-p (window buffer)
  "Return non-nil when quitting WINDOW shows a buffer BUFFER replaced there.
That is when its `quit-restore-prev' or `quit-restore' parameter records
a buffer still live that WINDOW showed before BUFFER, which
`quit-restore-window' then shows again."
  (seq-some (lambda (parameter)
              (let ((value (window-parameter window parameter)))
                (and (consp (nth 1 value))
                     (buffer-live-p (car (nth 1 value)))
                     (eq (nth 3 value) buffer))))
            '(quit-restore-prev quit-restore)))

(defun drover--forget-shown-views (window buffer)
  "Keep WINDOW from showing again a view of drover's shown in another window.
Its `quit-restore-prev' and `quit-restore' parameters naming such a
view as the buffer BUFFER replaced are cleared: `quit-restore-window'
would show it again from them, whatever `switch-to-prev-buffer-skip'
says.  A view is a buffer whose `drover-view-p' is set."
  (dolist (parameter '(quit-restore-prev quit-restore))
    (let* ((value (window-parameter window parameter))
           (shown (and (consp (nth 1 value)) (car (nth 1 value)))))
      (when (and (eq (nth 3 value) buffer)
                 (buffer-live-p shown)
                 (drover--view-shown-elsewhere-p shown window))
        (set-window-parameter window parameter nil)))))

(defun drover--quit-window (window bury-or-kill)
  "Quit WINDOW as `quit-restore-window' does with BURY-OR-KILL, sparing frames.
The functions of `quit-window-hook' run first, in its buffer, as
`quit-window' runs them.  WINDOW then goes as its `quit-restore'
parameters tell, a frame made for its buffer deleted rather than hidden
as `frame-auto-hide-function' would, but never the last frame.  The
last window of a frame or tab not made for its buffer is never deleted,
nor that frame or tab: it shows the buffer it showed before, or
another, or stays as it is when there is none.  A view of drover's
shown in another window is not shown there again (see
`drover--forget-shown-views')."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (drover-run-hook-reporting 'quit-window-hook))
    (when (and (window-live-p window) (eq (window-buffer window) buffer))
      (drover--forget-shown-views window buffer)
      (if (or (not (frame-root-window-p window)) (drover--made-for-p window buffer))
          (let ((frame-auto-hide-function (lambda (frame)
                                            (drover-with-reporting "deleting a frame"
                                              (delete-frame frame)))))
            (quit-restore-window window bury-or-kill))
        ;; `quit-restore-window' deletes a dedicated window, or one with no
        ;; other buffer, closing its tab without asking
        ;; `window-deletable-functions': here it only restores.
        (let ((dedicated (window-dedicated-p window)))
          (unless (eq dedicated 'side)
            (set-window-dedicated-p window nil))
          (if (drover--restores-p window buffer)
              (let ((quit-restore-window-no-switch nil))
                (quit-restore-window window bury-or-kill))
            (unless (switch-to-prev-buffer window bury-or-kill)
              (set-window-dedicated-p window dedicated))))))))

(defun drover--bury-window (window quiet selected)
  "Quit WINDOW until it shows none of the buffers QUIET holds, or is gone.
QUIET is a hash table of buffers.  WINDOW is quit as `quit-window'
would (see `drover--quit-window'), and again when it then shows another
buffer of QUIET, at most once a buffer.  Unless SELECTED, WINDOW is not
the selected window, and the selected windows stay selected.  Return
non-nil when WINDOW went or shows another buffer."
  (let ((rounds (hash-table-count quiet))
        (changed nil))
    (while (and (> rounds 0) (window-live-p window) (gethash (window-buffer window) quiet))
      (let ((buffer (window-buffer window)))
        (if selected
            (drover--quit-window window 'bury)
          ;; As `quit-windows-on' quits them, the buffer buried apart.
          (save-selected-window
            (drover--quit-window window 'burying)))
        (if (and (window-live-p window) (eq (window-buffer window) buffer))
            (setq rounds 0)
          (setq changed t
                rounds (1- rounds)))))
    changed))

(defun drover-bury-quiet-sessions ()
  "Bury the buffers of the sessions of this host that do not need you.
Those are the sessions whose status is not one of
`drover-attention-statuses', idle, working or retrying, of every
backend (see `drover--quiet-buffers').  Each buffer goes to the end of
the buffer list, as `bury-buffer' puts it.  A window showing one is quit
as `quit-window' quits it, but burying the buffer, never killing it:
as its `quit-restore' parameters tell, a window or frame made for the
buffer is deleted, and another window shows what it showed before, its
previous buffer, or the one before when that is a quiet session's too.
The last window of a frame not made for the buffer is never deleted,
nor the last frame.  Drover's lists and prompt stay as they are, and
while a list shows, no window shows it a second time.  The selected
window is quit last, so that selection goes where quitting it sends it.
Tell how many sessions were buried and how many windows restored."
  (interactive)
  (let ((buffers (or (drover--quiet-buffers)
                     (user-error "No quiet session has a buffer")))
        (quiet (make-hash-table :test #'eq))
        (restored 0))
    (dolist (buffer buffers)
      (puthash buffer t quiet)
      (bury-buffer buffer))
    (let* ((selected (selected-window))
           (windows (seq-filter (lambda (window) (gethash (window-buffer window) quiet))
                                (window-list-1 nil 'nomini t)))
           (switch-to-prev-buffer-skip (drover--prev-buffer-skip)))
      (dolist (window (append (remq selected windows) (and (memq selected windows) (list selected))))
        (when (drover--bury-window window quiet (eq window selected))
          (cl-incf restored))))
    (drover-message "Buried %d session%s (%d window%s restored)"
                    (length buffers) (if (= (length buffers) 1) "" "s")
                    restored (if (= restored 1) "" "s"))))

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

(defvar drover-new-session-elsewhere-function nil
  "Function starting a session on a host chosen among this one and others, or nil.
`drover-remote-mode' sets it; it takes the session at point, or nil.")

(defun drover-new-session (directory backend &optional elsewhere)
  "Start a session of BACKEND in DIRECTORY.
Interactively, DIRECTORY is the directory of the session at point, or
`default-directory' without one or for a session of another host, and
BACKEND the backend of that session, or the backend that starts
sessions, chosen among several.  With a prefix argument, ELSEWHERE,
the host is chosen first, this one or another starting sessions for it
\=(see `drover-new-session-elsewhere-function'), the session at point's
first: on another, among the sessions it starts, in a directory of its."
  (interactive
   (if current-prefix-arg
       (list nil nil t)
     (let ((session (drover-session-at-point t)))
       (list (or (and session (drover-session-local-directory session)) default-directory)
             (if (and session (drover-supports-p session :new))
                 (drover-session-backend session)
               (drover--read-backend :new "Start a session of: "))))))
  (if elsewhere
      (if (functionp drover-new-session-elsewhere-function)
          (funcall drover-new-session-elsewhere-function (drover-session-at-point t))
        (user-error "No other host starts sessions: enable `drover-remote-mode'"))
    (let ((start (drover-backend-property backend :new)))
      (unless (functionp start)
        (user-error "%s cannot start sessions" (drover-backend-label backend)))
      (funcall start directory))))

;;;; Projects

(defun drover--projects ()
  "Return the projects of the sessions lists show, sorted, without duplicates."
  (sort (delete-dups (delq nil (mapcar #'drover-session-view-project (drover-known-sessions))))
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

;;;; Building

(defun drover-build-and-go-p (session)
  "Return non-nil when SESSION can switch to the build agent and take a prompt."
  (and (eq (drover-session-engine session) 'opencode)
       (drover-supports-p session :send)
       (drover-supports-p session :apply-options)
       (or (not (drover-supports-p session :settable))
           (memq :agent (drover-backend-call session :settable)))))

(defun drover-build-and-go (session)
  "Switch SESSION to the build agent and send the prompt \"Go ahead\".
Interactively, SESSION is the session at point.  Its backend applies
the agent before sending, as for options chosen in the prompt; any
draft stays as it is.  This applies to OpenCode sessions whose backend
can change their agent."
  (interactive (list (drover-session-at-point)))
  (unless (drover-build-and-go-p session)
    (user-error "%s cannot switch to the build agent"
                (drover-session-display-title session)))
  (drover-prompt-send-to session "Go ahead" '(:agent "build")))

;;;; Session keys

(defvar-keymap drover-session-map
  :doc "Keys acting on the session at point in lists of sessions.
Drover's views inherit it; `drover-session-keys-mode' binds it in
other lists."
  "o" #'drover-open-other-window
  "A" #'drover-respond
  "<tab>" #'drover-build-and-go
  "E" #'drover-edit-pending-prompt
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
  "R" #'drover-review
  "G" #'drover-goal
  "V" #'drover-view)

(define-minor-mode drover-session-keys-mode
  "Bind the keys of `drover-session-map' in a list of sessions.
For lists drover does not draw, those of other front ends, whose
buffers also set `drover-session-at-point-function'.

\\{drover-session-map}"
  :keymap drover-session-map
  :group 'drover)

;;;; Values in menus

(defconst drover--menu-value-width 28
  "Width at most of a value or title the menus show, in columns.")

(defconst drover--menu-title-width 24
  "Width at most of a session's title the menus show, in columns.")

(defconst drover--menu-directory-width 24
  "Width at most of a directory the menus show, in columns.")

(defun drover--menu-value (value &optional width tail)
  "Return VALUE as the menus show it: on one line, cut to fit.
It is cut to WIDTH columns, `drover--menu-value-width' by default,
keeping its start, or with TAIL its end, as for a directory."
  (let* ((width (or width drover--menu-value-width))
         (ellipsis (truncate-string-ellipsis))
         (room (- width (string-width ellipsis)))
         (string (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" value))))
    (cond ((<= (string-width string) width) string)
          ((not tail) (truncate-string-to-width string width nil nil ellipsis))
          (t (let ((start (max 0 (- (length string) room))))
               (while (> (string-width string start) room)
                 (cl-incf start))
               ;; Whole components of a file name, when one is left.
               (when-let* (((> start 0))
                           ((not (eq (aref string (1- start)) ?/)))
                           (slash (string-search "/" string start))
                           ((< slash (1- (length string)))))
                 (setq start slash))
               (concat ellipsis (substring string start)))))))

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

(defun drover--view-shown-p ()
  "Return non-nil when one of drover's views is current or in sight.
In sight is in a window of a visible frame.  The view settings, which
every view shares, apply from anywhere then."
  (or drover-view-p
      (seq-some (lambda (window) (buffer-local-value 'drover-view-p (window-buffer window)))
                (window-list-1 nil 'nomini 'visible))))

(defun drover--context ()
  "Return what the dispatch menu depends on in the current buffer, as a plist.
:session is the session at point or nil, :selection the sessions an
active region selects in a list, or nil (see
`drover-selected-sessions-function'), :list non-nil in a list of
sessions, :view non-nil in one of drover's views, :shown non-nil when
one of drover's views is current or shows on a visible frame (see
`drover--view-shown-p'), :prompt non-nil in drover's own prompt buffer
\(see `drover-prompt-mode'), :window the window of a list the menu acts
on in place of the current buffer, or nil, :quiet non-nil when a session
not needing the user has a buffer to bury (see `drover--quiet-buffers').

The menu acts on the current buffer when it is a list of sessions, or
when it tells a session at point, as a session buffer does.  Otherwise,
when one of drover's lists is shown in a window of the selected frame,
the most recently used if several, the menu acts on that list: :session,
:list and :view are the list's, its session at point the one at the
window's point, its selection the one of the window's region, and
:window is the window.  :prompt is always the current buffer's.  The
global commands keep to the current buffer, and the view settings
apply wherever :shown is non-nil, a session buffer included."
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
  (let ((view (and (if window
                       (buffer-local-value 'drover-view-p (window-buffer window))
                     drover-view-p)
                   t)))
    (list :session session
          :selection selection
          :list (and list t)
          :view view
          :shown (and (or view (drover--view-shown-p)) t)
          :prompt (and prompt t)
          :window window
          :quiet (and (drover-with-reporting "quiet sessions"
                        (drover--quiet-buffers))
                      t))))

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
With sessions selected (see `drover-selected-sessions'), when the
backend of one of them performs OPERATION, a keyword, unless it is nil,
and PREDICATE, a function of a session, if any, returns non-nil for it;
otherwise as `drover-dispatch-session' tells of the session at point.
A region may select a session alone that is not at point, point being
on a group's heading."
  (let ((selection (plist-get (drover--scope) :selection)))
    (if selection
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

(defun drover--menu-p (command)
  "Return non-nil while the menu of COMMAND, a prefix, is being set up or shown."
  (when-let* ((prefix (transient-prefix-object)))
    (eq (oref prefix command) command)))

(defun drover--view-menu-p ()
  "Return non-nil while the View menu, `drover-view', is being set up or shown."
  (drover--menu-p 'drover-view))

(defun drover--search-menu-p ()
  "Return non-nil while the menu of the search, `drover-search-menu', is up.
Its view settings scope the search, wherever the list is: it has the
filter's, without the sort, the grouping, follow mode and the columns."
  (drover--menu-p 'drover-search-menu))

(defun drover-dispatch-view-shown-p ()
  "Return non-nil when the menus may change the settings of drover's views.
That is while one of drover's views is current, or shows in a window of
a visible frame: the settings, shared by every view, then apply from any
buffer, a session buffer included.  The View menu opens in a view only,
and the menu of the search scopes the search with them."
  (or (drover--view-menu-p)
      (drover--search-menu-p)
      (plist-get (drover--scope) :shown)))

(defun drover--dispatch-layout-p ()
  "Return non-nil when the menus may change how the lists lay sessions out.
That is the sort, the grouping, follow mode and the fields shown,
wherever the view settings apply but in the menu of the search, which
they do not scope."
  (and (drover-dispatch-view-shown-p) (not (drover--search-menu-p))))

(defun drover--dispatch-foreign-subagents-p ()
  "Return non-nil when =u of the menu acts on a list of another front end.
That is a list drover does not draw, current, which shows or hides its
subagents itself (see `drover-toggle-subagents-function')."
  (unless (drover--view-menu-p)
    (let ((scope (drover--scope)))
      (and (plist-get scope :list)
           (not (plist-get scope :window))
           (not (plist-get scope :view))
           drover-toggle-subagents-function
           t))))

(defun drover--dispatch-subagents-p ()
  "Return non-nil when =u of the menu applies.
It does in a list of another front end showing its subagents itself,
and wherever the view settings apply."
  (or (drover--dispatch-foreign-subagents-p) (drover-dispatch-view-shown-p)))

(defun drover-dispatch-quiet-p ()
  "Return non-nil when the dispatch menu may bury the buffers of quiet sessions.
See `drover-bury-quiet-sessions'."
  (plist-get (drover--scope) :quiet))

(defun drover-dispatch-in-prompt-p ()
  "Return non-nil when the dispatch menu acts on drover's own prompt buffer."
  (plist-get (drover--scope) :prompt))

(defun drover-dispatch-org-task-p ()
  "Return non-nil when the dispatch menu may start a prompt for an Org task.
That is in an Org buffer other than drover's prompt, which is one too:
`drover-prompt-org-task' finds the task point is under."
  (and (derived-mode-p 'org-mode) (not (drover-dispatch-in-prompt-p))))

(defvar drover-session-task-function)

(defun drover-dispatch-task-p ()
  "Return non-nil when the dispatch menu may start, continue or finish a task.
That is on an Org heading, in a buffer other than drover's prompt, or
on a session serving a task (see `drover-session-task'), in this
buffer."
  (or (drover-dispatch-org-task-p)
      (let ((scope (drover--scope)))
        (and (not (plist-get scope :window))
             (when-let* ((session (plist-get scope :session)))
               (drover-session-task session))))))

(defun drover-dispatch-goal-p ()
  "Return non-nil when the dispatch menu may set a goal, or act on one.
On an Org heading, a task, or on a session at point, of this host:
another host runs its own sessions' goals."
  (or (drover-dispatch-org-task-p)
      (when-let* ((session (drover-dispatch-session)))
        (not (drover-session-host session)))))

(defun drover-dispatch-task-link-p ()
  "Return non-nil when the dispatch menu may link a session to a task.
On an Org heading, or on a session in this buffer, while
`drover-task-mode' tells the tasks of sessions."
  (and drover-session-task-function
       (or (drover-dispatch-org-task-p)
           (let ((scope (drover--scope)))
             (and (not (plist-get scope :window))
                  (plist-get scope :session))))))

(defun drover-dispatch-prompt-buffer-p ()
  "Return non-nil when the dispatch menu acts from a prompt buffer.
That is a buffer performing prompt actions (see `drover-prompt-define'):
the prompt keys editing or reading what such a buffer shows apply
there alone, and the menu leaves them out elsewhere."
  (and drover-prompt-actions t))

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
stays Send, as losing a prompt to a slip is worse than reaching for o.
Nor does it from another buffer, acting on a list in sight, while a
prompt is ready to send (see `drover-prepared-prompt-function'): RET
then sends it, as the user typing there means to.  In the list itself,
RET opens, as it does there, and M-RET sends the prompt."
  (and (drover-dispatch-view-p)
       (not (drover-prompt-defines-p (current-buffer) 'send))
       (not (and (plist-get (drover--scope) :window)
                 (drover--prompt-target 'send nil nil t)))))

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

(defun drover--dispatch-goal (function &rest arguments)
  "Apply FUNCTION to ARGUMENTS on an Org heading, else as a session command.
In Org, on the heading at point; elsewhere, on the session at point, of
the list the menu acts on (see `drover--dispatch-in-list')."
  (if (derived-mode-p 'org-mode)
      (apply function arguments)
    (apply #'drover--dispatch-in-list function arguments)))

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
  "Return the title of the dispatch menu's List group, naming the session acted on.
That is the session a region selects alone, else the session at point.
With several sessions selected, it tells how many instead."
  (let* ((scope (drover--scope))
         (selection (plist-get scope :selection))
         (session (if selection (car selection) (plist-get scope :session))))
    (cond ((cdr selection)
           (format "List: %d sessions selected" (length selection)))
          (session
           (concat "List: " (drover--menu-value
                             (drover-session-display-title session)
                             drover--menu-title-width)))
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

(defconst drover-filter-any "(any)"
  "The first candidate of every filter picker: choosing it clears the filter.")

(defun drover--filter-table (names)
  "Return a completion table of NAMES, strings, in their order.
`drover-filter-any' comes first.  Completion UIs keep the order rather
than sorting, so that RET on an empty input, which chooses the first
candidate in some, clears the filter as it does in the others."
  (let ((candidates (cons drover-filter-any names)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . identity) (cycle-sort-function . identity))
        (complete-with-action action candidates string predicate)))))

(defun drover--read-filter (prompt names current)
  "Read a categorical filter with PROMPT, offering NAMES and CURRENT.
NAMES is an alist (NAME . VALUE); CURRENT is the existing selection.
Values no longer known are still offered so they can be deselected."
  (dolist (value (drover-filter-selection-values current))
    (unless (rassoc value names)
      (setq names (append names (list (cons (if value (format "%s" value) "(unknown)") value))))))
  (drover-filter-read prompt names current))

(defun drover--read-known (prompt field current)
  "Read a selection of values of FIELD among known sessions, with PROMPT.
FIELD is a function of a session, such as `drover-session-agent';
CURRENT is the value now, or nil.  Return nil to clear the filter (see
`drover--read-filter')."
  (let* ((values (delete-dups (mapcar field (drover-known-sessions))))
         (unknown (memq nil values))
         (names (mapcar (lambda (value) (cons value value)) (sort (delq nil values) #'string<))))
    (drover--read-filter prompt
                         (if unknown (append names '(("(unknown)"))) names)
                         current)))

(defun drover--read-filter-project (current)
  "Read the projects to list, starting with CURRENT."
  (drover--read-known "Project" #'drover-session-view-project current))

(defun drover--read-filter-agent (current)
  "Read the agents whose sessions to list, starting with CURRENT."
  (drover--read-known "Agent" #'drover-session-agent current))

(defun drover--read-filter-model (current)
  "Read the models whose sessions to list, starting with CURRENT."
  (drover--read-known "Model" #'drover-session-model current))

(defun drover--read-filter-provider (current)
  "Read the providers whose sessions to list, starting with CURRENT."
  (drover--read-known "Provider" #'drover-session-provider-name current))

(defun drover--read-filter-harness (current)
  "Read the harnesses whose sessions to list, starting with CURRENT.
Harnesses are read by name, among those of the sessions known."
  (let ((harnesses (delete-dups (mapcar #'drover-session-harness (drover-known-sessions)))))
    (dolist (harness (drover-filter-selection-values current))
      (cl-pushnew harness harnesses))
    (drover--read-filter "Harness"
                         (sort (mapcar (lambda (harness) (cons (drover-harness-name harness) harness))
                                       harnesses)
                               :key (lambda (entry) (downcase (car entry))))
                         current)))

(defun drover--read-filter-statuses (current)
  "Read the statuses to list, CURRENT being those listed now."
  (drover--read-filter "Statuses"
                       (mapcar (lambda (status) (cons (opencode-session-status-label status) status))
                               drover-status-order)
                       current))

(defun drover--read-filter-since (current)
  "Read since when sessions must have been active to be listed.
CURRENT, the time now, is named in the prompt.  An empty answer, or
`drover-filter-any', offered first, clears the filter."
  (let ((answer (completing-read
                 (format "Updated since (%stoday, 24h, 7 days, 3d…; empty for any): "
                         (if current (format "now %s; " (drover-filter-since-label current)) ""))
                 (drover--filter-table '("today" "24h" "7 days" "30 days")))))
    (unless (equal answer drover-filter-any)
      (drover--parse-since answer))))

(defun drover--read-filter-search (current)
  "Read the text sessions must contain to be listed, CURRENT being the one now.
The input starts with CURRENT; an empty answer clears the filter."
  (let ((search (read-string "Search (empty for any): " current)))
    (unless (string-empty-p (string-trim search)) search)))

(defun drover--read-filter-host (current)
  "Read the hosts whose sessions to list, CURRENT being those listed now.
This host is `local'; All lists every host's sessions."
  (let ((names (cons (cons (format "%s (this host)" drover-host) 'local)
                     (mapcar (lambda (host) (cons host host)) (drover-hosts)))))
    (dolist (host (drover-filter-selection-values current))
      (unless (rassoc host names)
        (setq names (append names (list (cons (format "%s" host) host))))))
    (drover--read-filter "Hosts" names current)))

(defun drover--filter-label (key value)
  "Return VALUE of the filter member KEY as the View menu shows it."
  (pcase key
    (:since (drover-filter-since-label value))
    (:search (format "\"%s\"" value))
    (_ (drover-filter-selection-label key value))))

(defun drover-filter-clear ()
  "List every session in every view."
  (interactive)
  (setq drover-filter nil)
  (drover-view-settings-changed))

;;;; The View menu

(defconst drover-sort-labels
  '((urgency . "urgency") (title . "title") (project . "project") (branch . "branch")
    (agent . "agent") (model . "model") (provider . "provider") (harness . "harness")
    (updated . "last activity") (created . "creation"))
  "Display name of each field of `drover-sort-fields'.")

(defconst drover-grouping-labels
  '((status . "status") (project . "project") (task . "task") (branch . "branch") (agent . "agent")
    (model . "model") (provider . "provider") (harness . "harness") (host . "host")
    (updated . "last activity") (nil . "nothing"))
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
  ((format :initform " %k %v")
   (filter-key :initarg :filter-key :documentation "The member of `drover-filter' it sets."))
  "An infix of `drover-view' setting one member of the variable `drover-filter'.
It shows its key and value alone, in the row of a `drover--menu-table'
naming the field it filters.")

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
  "Return the value of the setting of OBJ as the menu shows it.
The value of an entry that does not apply is dimmed."
  (let ((value (drover--setting-value obj))
        (inapt (oref obj inapt)))
    (if-let* ((options (oref obj options)))
        (concat (propertize "[" 'face 'transient-delimiter)
                (mapconcat (pcase-lambda (`(,option . ,label))
                             (propertize label 'face (if (and (eq option value) (not inapt))
                                                         'transient-value
                                                       'transient-inactive-value)))
                           options
                           (propertize "|" 'face 'transient-delimiter))
                (propertize "]" 'face 'transient-delimiter))
      (if-let* ((label (drover--setting-label obj value)))
          (propertize (drover--menu-value label) 'face (if inapt 'transient-inactive-value 'transient-value))
        (propertize "any" 'face 'transient-inactive-value)))))

(transient-define-infix drover-view--sort-by ()
  "Sort the sessions of every view by a field."
  :class 'drover--setting
  :description "sort"
  :getter (lambda () drover-sort-by)
  :setter (lambda (field) (drover-set-sort field drover-sort-descending))
  :chooser (lambda (current) (drover--read-choice "Sort by" drover-sort-labels current))
  :labeler (lambda (field) (alist-get field drover-sort-labels)))

(transient-define-infix drover-view--sort-order ()
  "Sort the sessions of every view in ascending or descending order."
  :class 'drover--setting
  :description "order"
  :getter (lambda () drover-sort-descending)
  :setter (lambda (descending) (drover-set-sort drover-sort-by descending))
  :chooser #'not
  :options '((nil . "asc") (t . "desc")))

(transient-define-infix drover-view--group-by ()
  "Group the sessions of every view."
  :class 'drover--setting
  :description "group"
  :getter (lambda () drover-group-by)
  :setter #'drover-set-group-by
  :chooser (lambda (current) (drover--read-choice "Group by" drover-grouping-labels current))
  :labeler (lambda (grouping) (alist-get grouping drover-grouping-labels)))

(transient-define-infix drover-view--filter-project ()
  "List the sessions of selected projects in every view."
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
  "List the sessions of selected agents in every view."
  :class 'drover--filter-setting
  :description "agent"
  :filter-key :agent
  :chooser #'drover--read-filter-agent)

(transient-define-infix drover-view--filter-model ()
  "List the sessions of selected models in every view."
  :class 'drover--filter-setting
  :description "model"
  :filter-key :model
  :chooser #'drover--read-filter-model)

(transient-define-infix drover-view--filter-provider ()
  "List the sessions of selected providers in every view."
  :class 'drover--filter-setting
  :description "provider"
  :filter-key :provider
  :chooser #'drover--read-filter-provider)

(transient-define-infix drover-view--filter-harness ()
  "List the sessions of selected harnesses in every view."
  :class 'drover--filter-setting
  :description "harness"
  :filter-key :harness
  :chooser #'drover--read-filter-harness)

(transient-define-infix drover-view--filter-host ()
  "List the sessions of some hosts in every view."
  :class 'drover--filter-setting
  :description "host"
  :filter-key :host
  :chooser #'drover--read-filter-host
  :if #'drover-hosts)

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
  "Return the window of the list whose follow mode the menus set, or nil.
In the View menu, it is the window selected when the menu opened, which
shows the list.  In the dispatch menu, it is the window of the list the
menu acts on, here or beside (see `drover--context').  Otherwise, it is
the window of the current list, else of the list shown on the selected
frame, the one used last of several."
  (let ((prefix (transient-prefix-object)))
    (or (and prefix
             (eq (oref prefix command) 'drover-view)
             (window-live-p (oref prefix scope))
             (oref prefix scope))
        (and (not (drover--view-menu-p)) (drover--dispatch-window))
        (and drover-view-p
             (if (eq (window-buffer) (current-buffer))
                 (selected-window)
               (get-buffer-window (current-buffer))))
        (drover--shown-list-window))))

(transient-define-infix drover-view--follow ()
  "Show the session at point in another window as point moves, or stop."
  :class 'drover--setting
  :description "follow"
  :getter (lambda ()
            (when-let* ((window (drover--view-window)))
              (buffer-local-value 'drover-follow-mode (window-buffer window))))
  :setter (lambda (on)
            ;; Follow mode is the list's own, and reads its window's point.
            (with-selected-window (or (drover--view-window) (user-error "No list of sessions in sight"))
              (drover-follow-mode (if on 1 -1))))
  :chooser #'not
  :options '((nil . "off") (t . "on"))
  :inapt-if-not #'drover--view-window)

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

;;;; Tables of settings

(defclass drover--menu-table (transient-column)
  ((headings :initarg :headings :initform nil
             :documentation "The title of each column of cells, a string or nil.")
   (lines :initform nil
          :documentation "Its lines as last formatted, see `drover--menu-table-update'."))
  "A group of the settings of fields, a row each, aligned in columns.
Each child is a `drover--menu-table-row', whose description names its
field, and whose suffixes are its cells: the Nth suffix of each row
shows in the Nth column, so that a row lacking one, or one an :if
leaves out, leaves its cell empty, and a column whose cells are all
left out takes no room; the last cell of a row spans the columns after
it, which it leaves empty, so that a long value widens no column.  The
title names the column of the fields, as the description, then the
others, as HEADINGS, each above its column.
A row formats as its whole line, so that a table lays out wherever a
column of entries does.")

(defclass drover--menu-table-row (transient-row) ()
  "A row of a `drover--menu-table': the name of a field, then its settings.")

;;;; Columns

(defvar drover-list-fields)
(defvar drover-list-hidden-fields)

(defconst drover-list-field-labels
  '((goal . "goal") (attempt . "attempt") (host . "host") (harness . "harness") (location . "location") (branch . "branch") (agent . "agent")
    (model . "model") (provider . "provider") (context . "context") (created . "created") (cost . "cost")
    (age . "age") (cache . "cache") (summary . "summary"))
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
while the list may show the field, dimmed otherwise.  In the row of a
`drover--menu-table', which names the field, it shows \"shown\" or
\"hidden\" instead.")

(cl-defmethod drover--setting-value ((obj drover--column-setting))
  "Return non-nil when the list may show the field of OBJ."
  (drover-list-field-enabled-p (oref obj field)))

(cl-defmethod drover--setting-set ((obj drover--column-setting) value)
  "Let the list show the field of OBJ when VALUE is non-nil."
  (drover-set-list-field (oref obj field) value))

(cl-defmethod transient-format-value ((obj drover--column-setting))
  "Return the name of the field of OBJ, highlighted when the list may show it.
In a row of a table, return whether it is shown."
  (let ((shown (drover--setting-value obj)))
    (propertize (cond ((not (cl-typep (oref obj parent) 'drover--menu-table-row))
                       (alist-get (oref obj field) drover-list-field-labels))
                      (shown "shown")
                      (t "hidden"))
                'face (if (and shown (not (oref obj inapt)))
                          'transient-value
                        'transient-inactive-value))))

(defun drover--list-field-p (field)
  "Return non-nil when FIELD is one of `drover-list-fields'.
Before the list is loaded, any field is."
  (or (not (boundp 'drover-list-fields))
      (assq field drover-list-fields)))

(defmacro drover--define-column-switch (field)
  "Define the infix of the View menu letting the list show FIELD, or not.
It shows where the list has FIELD, but in the menu of the search."
  `(transient-define-infix ,(intern (format "drover-view--column-%s" field)) ()
     ,(format "Let the list show the field `%s', or not." field)
     :class 'drover--column-setting
     :field ',field
     :format " %k %v"
     :chooser #'not
     :if (lambda () (and (drover--list-field-p ',field) (not (drover--search-menu-p))))))

(drover--define-column-switch goal)
(drover--define-column-switch attempt)
(drover--define-column-switch host)
(drover--define-column-switch harness)
(drover--define-column-switch location)
(drover--define-column-switch branch)
(drover--define-column-switch agent)
(drover--define-column-switch model)
(drover--define-column-switch provider)
(drover--define-column-switch context)
(drover--define-column-switch created)
(drover--define-column-switch cost)
(drover--define-column-switch age)
(drover--define-column-switch cache)
(drover--define-column-switch summary)

;;;; Laying out the menus

(defclass drover--menu-flow (transient-columns) ()
  "A group of groups laid out in columns of balanced height, filling the width.
Each child is a column, titled by its description; a column without a
description continues the one before it, beside it or under it, as the
room asks.  Groups keep their order, filling each column of the menu
from the top, then the next one, as text fills newspaper columns; when
the width is too narrow for that to be lowest, in rows of such columns.
The menu so takes the fewest lines its window's width allows (see
`drover--menu-plan').  While the menu stays open, its plan stays as long
as it fits, so that entries keep their place as values change.")

(defconst drover--menu-gap 2
  "Columns of space between two columns of a menu, besides the entries' own.")

(defclass drover--menu-prefix (transient-prefix)
  ((plan :initform nil
         :documentation "The plan the menu was drawn after last, see `drover--menu-plan'."))
  "A menu of drover's, keeping the arrangement of its groups while open.")

(defun drover--menu-width ()
  "Return how many columns wide the menu being drawn may be.
It is the width of the menu's window, once shown; before, of the window
it will be shown in, at the bottom of the selected frame, beside its
side windows, such as the sidebar."
  (max 40 (if (window-live-p transient--window)
              (window-max-chars-per-line transient--window)
            (- (window-total-width (window-main-window)) 2))))

(defun drover--menu-column (column)
  "Return the lines of COLUMN, a group of the menu being drawn, or nil.
The first line is its title, the empty string for a column continuing
the one before it; the others are its entries shown.  Nil when it shows
no entry."
  (let* ((transient--pending-group column)
         ;; Before the entries: a table formats its rows with its title.
         (title (or (transient-format-description column) ""))
         (entries (delq nil (mapcar (lambda (suffix)
                                      (unless (and (cl-typep suffix 'transient-suffix)
                                                   (oref suffix inactive))
                                        (let ((line (transient-format suffix)))
                                          (and (not (equal line "")) line))))
                                    (oref column suffixes)))))
    (when entries
      (cons title entries))))

(defun drover--menu-lines-width (lines)
  "Return the width of the widest of LINES, in columns."
  (apply #'max 0 (mapcar #'string-width lines)))

(defun drover--menu-side-by-side (columns)
  "Return the lines of COLUMNS, lists of lines, side by side.
Each column starts `drover--menu-gap' columns past the widest line of
the one before; no line ends in padding."
  (let ((stops (let ((stop 0))
                 (mapcar (lambda (lines)
                           (prog1 stop
                             (setq stop (+ stop (drover--menu-lines-width lines) drover--menu-gap))))
                         columns))))
    (mapcar (lambda (index)
              (let ((line ""))
                (cl-mapc (lambda (lines stop)
                           (when-let* ((cell (nth index lines))
                                       ((not (equal cell ""))))
                             (setq line (concat line (make-string (max 0 (- stop (string-width line))) ?\s)
                                                cell))))
                         columns stops)
                line))
            (number-sequence 0 (1- (apply #'max (mapcar #'length columns)))))))

(defun drover--menu-layout (columns)
  "Return COLUMNS, lists of lines, side by side, as a layout.
A layout is (WIDTH HEIGHT . LINES), its width in columns and its height
in lines."
  (let ((lines (drover--menu-side-by-side columns)))
    (cl-list* (drover--menu-lines-width lines) (length lines) lines)))

(defun drover--menu-block-layouts (block)
  "Return the layouts BLOCK may show in, see `drover--menu-layout'.
BLOCK is a column and the columns continuing it, each a list of lines.
Each layout shows them in order, some side by side, the others under
the one before, as one column: from all side by side to all in one
column."
  (mapcar #'drover--menu-layout
          (named-let compose ((parts block))
            (if (null (cdr parts))
                (list parts)
              (let ((head (car parts))
                    (layouts nil))
                (dolist (rest (compose (cdr parts)))
                  ;; The next part beside, or under.
                  (push (cons head rest) layouts)
                  (push (cons (append head (cdr (car rest))) (cdr rest)) layouts))
                (nreverse layouts))))))

(defun drover--menu-stack (blocks height)
  "Return the narrowest way to show BLOCKS under each other in HEIGHT lines.
BLOCKS lists the layouts each block may show in; an empty line parts
two blocks.  The value is (WIDTH HEIGHT . INDEXES), the index of the
layout chosen for each block, or nil when they cannot fit."
  (let ((best nil))
    (named-let walk ((rest blocks) (width 0) (used -1) (chosen nil))
      (if rest
          (let ((index 0))
            (dolist (layout (car rest))
              (let ((total (+ used 1 (cadr layout))))
                (when (<= total height)
                  (walk (cdr rest) (max width (car layout)) total (cons index chosen))))
              (cl-incf index)))
        (when (or (null best)
                  (< width (car best))
                  (and (= width (car best)) (< used (cadr best))))
          (setq best (cl-list* width used (reverse chosen))))))
    best))

(defun drover--menu-arrange (blocks height)
  "Return the narrowest arrangement of BLOCKS in columns HEIGHT lines high.
BLOCKS lists the layouts each block may show in, in order; each column
holds the blocks that follow those of the column before.  The value is
\(WIDTH HEIGHT . COLUMNS): its width with `drover--menu-gap' between
columns, the height of its highest column, and its columns, each the
list of the indexes of the layouts its blocks show in.  Nil when a
block cannot fit HEIGHT lines."
  (let* ((blocks (vconcat blocks))
         (count (length blocks))
         (best (make-vector (1+ count) nil)))
    (aset best count (list (- drover--menu-gap) 0))
    (cl-loop for start from (1- count) downto 0 do
             (cl-loop for end from start below count
                      for stack = (drover--menu-stack (append (substring blocks start (1+ end)) nil)
                                                      height)
                      while stack
                      do (when-let* ((rest (aref best (1+ end))))
                           (let ((width (+ (car stack) drover--menu-gap (car rest))))
                             (when (or (null (aref best start)) (< width (car (aref best start))))
                               (aset best start (cl-list* width (max (cadr stack) (cadr rest))
                                                          (cddr stack) (cddr rest))))))))
    (aref best 0)))

(defun drover--menu-row (blocks width)
  "Return the lowest arrangement of BLOCKS in columns at most WIDTH wide, or nil.
BLOCKS lists the layouts each block may show in, in order.  Of the
arrangements as low, the narrowest.  The value is that of
`drover--menu-arrange'.  A block alone always has one, in one column,
though it be wider than WIDTH; several blocks none when even one column
is wider."
  (let* ((low (apply #'max 0 (mapcar (lambda (layouts) (apply #'min (mapcar #'cadr layouts))) blocks)))
         (high (+ (apply #'+ (mapcar (lambda (layouts) (apply #'max (mapcar #'cadr layouts))) blocks))
                  (max 0 (1- (length blocks))))))
    (while (< low high)
      (let* ((middle (/ (+ low high) 2))
             (arrangement (drover--menu-arrange blocks middle)))
        (if (and arrangement (<= (car arrangement) width))
            (setq high middle)
          (setq low (1+ middle)))))
    (let ((arrangement (drover--menu-arrange blocks low)))
      (and (or (null (cdr blocks)) (<= (car arrangement) width))
           arrangement))))

(defun drover--menu-plan (blocks width)
  "Return how to lay BLOCKS out in rows of columns at most WIDTH wide.
BLOCKS lists the layouts each block may show in, in order.  Each row
holds the blocks that follow those of the row before, in columns of
balanced height (see `drover--menu-row'), and an empty line parts two
rows.  The plan takes the fewest lines, then the fewest rows: one row
of columns, as newspapers fill theirs, as long as it is the lowest.
The value is the list of its rows, each the list of its columns, each
the list of the indexes of the layouts its blocks show in."
  (let* ((blocks (vconcat blocks))
         (count (length blocks))
         ;; The best plan of the blocks from an index on, as
         ;; (LINES ROWS . PLAN).
         (best (make-vector (1+ count) nil)))
    (aset best count (list -1 0))
    (cl-loop for start from (1- count) downto 0 do
             (cl-loop for end from start below count
                      for row = (drover--menu-row (append (substring blocks start (1+ end)) nil) width)
                      while row
                      do (pcase-let* ((`(,lines ,rows . ,plan) (aref best (1+ end)))
                                      (candidate (cl-list* (+ (cadr row) 1 lines) (1+ rows)
                                                           (cddr row) plan))
                                      (current (aref best start)))
                           (when (or (null current)
                                     (< (car candidate) (car current))
                                     (and (= (car candidate) (car current))
                                          (< (cadr candidate) (cadr current))))
                             (aset best start candidate)))))
    (cddr (aref best 0))))

(defun drover--menu-plan-width (blocks plan)
  "Return how wide the widest row of BLOCKS laid out after PLAN is, or nil.
Nil when PLAN does not lay out as many blocks, or names a layout one of
them lacks.  See `drover--menu-plan' for plans."
  (let ((indexes (flatten-tree plan)))
    (when (and (= (length blocks) (length indexes))
               (seq-every-p #'identity (cl-mapcar (lambda (layouts index) (< index (length layouts)))
                                                  blocks indexes)))
      (let ((rest blocks))
        (apply #'max (mapcar (lambda (row)
                               (+ (* drover--menu-gap (1- (length row)))
                                  (apply #'+ (mapcar (lambda (column)
                                                       (apply #'max (mapcar (lambda (index)
                                                                              (car (nth index (pop rest))))
                                                                            column)))
                                                     row))))
                             plan))))))

(defun drover--menu-insert (blocks plan)
  "Insert BLOCKS laid out after PLAN, see `drover--menu-plan'."
  (let ((rest blocks)
        (first t))
    (dolist (row plan)
      (unless first
        (insert ?\n))
      (setq first nil)
      (let ((columns (mapcar (lambda (column)
                               (let ((lines nil))
                                 (dolist (index column)
                                   (when lines
                                     (push "" lines))
                                   (dolist (line (cddr (nth index (pop rest))))
                                     (push line lines)))
                                 (nreverse lines)))
                             row)))
        (dolist (line (drover--menu-side-by-side columns))
          (insert line ?\n))))))

(defun drover--menu-table-cell (suffix)
  "Return the cell SUFFIX shows in the row of a table, or nil for none.
The space entries begin with is left out, the row beginning with one."
  (unless (and (cl-typep suffix 'transient-suffix) (oref suffix inactive))
    (let ((cell (transient-format suffix)))
      (cond ((equal cell "") nil)
            ((string-prefix-p " " cell) (substring cell 1))
            (t cell)))))

(defun drover--menu-table-line (cells stops)
  "Return CELLS, strings or nil for an empty cell, each at its column of STOPS.
No line ends in padding."
  (let ((line ""))
    (cl-mapc (lambda (cell stop)
               (when cell
                 (setq line (concat line (make-string (max 0 (- stop (string-width line))) ?\s) cell))))
             cells stops)
    line))

(defun drover--menu-table-update (table title)
  "Format the lines of TABLE, a `drover--menu-table' titled TITLE, and return them.
The value is (TITLE . ROWS): TITLE followed by the headings of the
columns shown, each above its column, and ROWS an alist (ROW . LINE)
of the rows showing a cell; it is kept in TABLE, for its rows to
format as their line.  The name of a field is dimmed when none of its
cells applies.  Columns are `drover--menu-gap' apart."
  (let* ((rows (delq nil (mapcar (lambda (row)
                                   (when (cl-typep row 'drover--menu-table-row)
                                     (let* ((transient--pending-group row)
                                            (cells (mapcar #'drover--menu-table-cell (oref row suffixes))))
                                       (when (seq-some #'identity cells)
                                         (list row (drover--menu-table-label row) cells)))))
                                 (oref table suffixes))))
         (headings (oref table headings))
         (count (apply #'max 0 (mapcar (lambda (row) (length (nth 2 row))) rows)))
         ;; The width of each column, 0 for one whose cells are all empty.
         ;; A row's last cell before the last column spans the columns
         ;; after it, which it leaves empty: it counts for none.
         (widths (mapcar (lambda (index)
                           (if (seq-some (lambda (row) (nth index (nth 2 row))) rows)
                               (apply #'max 1 (string-width (or (nth index headings) ""))
                                      (mapcar (lambda (row)
                                                (let ((cells (nth 2 row)))
                                                  (if (and (nth index cells)
                                                           (or (= index (1- count))
                                                               (seq-some #'identity (nthcdr (1+ index) cells))))
                                                      (string-width (nth index cells))
                                                    0)))
                                              rows))
                             0))
                         (number-sequence 0 (1- count))))
         (stop (max (1+ (apply #'max 0 (mapcar (lambda (row) (string-width (nth 1 row))) rows)))
                    (string-width title)))
         (stops (mapcar (lambda (width)
                          (prog1 (+ stop drover--menu-gap)
                            (when (> width 0)
                              (setq stop (+ stop drover--menu-gap width)))))
                        widths))
         (face (if (oref table inapt) 'transient-inapt-suffix 'transient-heading))
         (lines (cons (drover--menu-table-line
                       (cons title (cl-mapcar (lambda (width heading)
                                                (and (> width 0) heading (propertize heading 'face face)))
                                              widths
                                              (append headings (make-list count nil))))
                       (cons 0 stops))
                      (mapcar (pcase-lambda (`(,row ,label ,cells))
                                (cons row (drover--menu-table-line (cons label cells) (cons 1 stops))))
                              rows))))
    (oset table lines lines)
    lines))

(defun drover--menu-table-label (row)
  "Return the name of the field of ROW, a row of a table, as it shows.
It is dimmed when none of the cells of ROW shown applies."
  (let ((label (or (transient--get-description row) "")))
    (if (seq-every-p (lambda (suffix)
                       (or (not (cl-typep suffix 'transient-suffix))
                           (oref suffix inactive)
                           (oref suffix inapt)))
                     (oref row suffixes))
        (propertize label 'face 'transient-inapt-suffix)
      label)))

(cl-defmethod transient-format-description ((table drover--menu-table))
  "Return the title of TABLE, naming its columns, its lines formatted anew.
Its rows then format as their line, see `drover--menu-table-update'."
  (car (drover--menu-table-update table (or (cl-call-next-method) ""))))

(cl-defmethod transient-format ((row drover--menu-table-row))
  "Return the line of ROW in its table, or the empty string when it shows no cell.
The lines are those the title of the table formatted last, or anew."
  (let ((table (oref row parent)))
    (or (cdr (assq row (cdr (or (oref table lines)
                                (drover--menu-table-update table ""))))) "")))

(defun drover--menu-column-parts (column)
  "Return the parts COLUMN, a group of the menu being drawn, may show in.
Each part is a list of lines, as `drover--menu-column' returns, the
parts after the first continuing it: the menu shows them beside each
other, or under, as room asks.  A table's rows, aligned whatever the
part they fall in, show in two parts; another group in one.  Nil when
COLUMN shows no entry."
  (let ((lines (drover--menu-column column)))
    (if (and (cl-typep column 'drover--menu-table) (nthcdr 3 lines))
        (let ((half (/ (length (cdr lines)) 2)))
          (list (cons (car lines) (take (- (length (cdr lines)) half) (cdr lines)))
                (cons "" (last lines half))))
      (and lines (list lines)))))

(cl-defmethod transient--insert-group ((group drover--menu-flow))
  "Insert the groups of GROUP in columns of balanced height, filling the width.
Shown in one column, or aligned for a variable pitch font, they are
inserted as transient inserts columns."
  (if (or transient-force-single-column transient--docsp transient-align-variable-pitch
          (oref transient--prefix variable-pitch))
      (cl-call-next-method)
    (let ((blocks nil)
          (continued nil))
      (dolist (column (oref group suffixes))
        (let ((parts (transient-with-shadowed-buffer (drover--menu-column-parts column))))
          (cond ((null parts))
                ((and continued (equal (car (car parts)) ""))
                 (setcar blocks (append (car blocks) parts)))
                (t (push parts blocks)))
          (setq continued (and parts t))))
      (let* ((blocks (mapcar #'drover--menu-block-layouts (nreverse blocks)))
             (width (drover--menu-width))
             (keep (cl-typep transient--prefix 'drover--menu-prefix))
             (last (and keep (oref transient--prefix plan)))
             (plan (if-let* ((last)
                             (last-width (drover--menu-plan-width blocks last))
                             ((<= last-width width)))
                       last
                     (drover--menu-plan blocks width))))
        (when keep
          (oset transient--prefix plan plan))
        (drover--menu-insert blocks plan)))))

;;;; What prompts and sessions run

(declare-function drover-prompt-buffer "drover-prompt" (&optional create))
(declare-function drover-prompt-effective-options "drover-prompt" (&optional buffer))
(declare-function drover-model-name "drover-model" (model))
(declare-function drover-model-id "drover-model" (model))
(declare-function drover-model-short-name "drover-model" (name))
(declare-function drover-model-default-variant "drover-model" (model))

(defvar drover-dispatch-send-target-functions nil
  "Functions naming where Send of `drover-dispatch' sends the prompt.
Each is called with no argument in the buffer whose prompt Send sends,
and returns a short string naming the session or new session the
prompt goes to, or nil when it does not know; the first string shows
beside Send, as Magit shows a value.")

(defun drover--menu-model-name (model)
  "Return the name of MODEL, a model of `drover-model', as lists show it."
  (drover-model-short-name (or (drover-model-name model) (drover-model-id model))))

(defun drover--option-target (action)
  "Return what prompt ACTION of the dispatch menu changes, or nil.
ACTION is `agent', `model', `effort' or `directory'.  The value is
\(prompt . BUFFER) for drover's prompt BUFFER, or (session . SESSION)
for the session whose buffer ACTION applies to, as
`drover-prompt-target' finds them from where the menu acts.  The
default prompt buffer counts only when it is drover's prompt and
exists: finding it never creates a buffer."
  (let* ((scope (drover--scope))
         (here (not (plist-get scope :window)))
         (session (and here (plist-get scope :session)))
         (list (and here (plist-get scope :list)))
         (target (drover--prompt-target action session list t)))
    (cond ((eq target 'default)
           (when-let* (((eq drover-default-prompt-buffer-function 'drover-prompt-default-buffer))
                       ((fboundp 'drover-prompt-buffer))
                       (buffer (drover-prompt-buffer)))
             (cons 'prompt buffer)))
          ((not (buffer-live-p target)) nil)
          ((with-current-buffer target (bound-and-true-p drover-prompt-mode)) (cons 'prompt target))
          ((or list (eq target (current-buffer)))
           (and session (cons 'session session)))
          ((when-let* ((own (with-current-buffer target (drover--context-session))))
             (cons 'session own))))))

(defun drover--option (value chosen)
  "Return VALUE and CHOSEN as an option's value, (TEXT . CHOSEN), or nil.
TEXT is VALUE as a string; nil when VALUE is nil or empty."
  (when-let* ((text (and value (format "%s" value)))
              ((not (string-empty-p text))))
    (cons text (and chosen t))))

(defun drover--prompt-buffer-option (buffer action)
  "Return what prompt ACTION changes in drover's prompt BUFFER.
The value is that of `drover--option'."
  (when (fboundp 'drover-prompt-effective-options)
    (let ((options (drover-prompt-effective-options buffer)))
      (pcase action
        ('agent (drover--option (plist-get options :agent) (plist-get options :agent-explicit-p)))
        ('model (drover--option (and (plist-get options :model)
                                     (drover--menu-model-name (plist-get options :model)))
                                (plist-get options :model-explicit-p)))
        ('effort (drover--option (plist-get options :variant) (plist-get options :variant-explicit-p)))
        ('directory (drover--option (when-let* ((directory (plist-get options :directory)))
                                      (if-let* ((host (plist-get options :host-label)))
                                          (format "%s: %s" host (directory-file-name directory))
                                        (abbreviate-file-name (directory-file-name directory))))
                                    (plist-get options :directory-explicit-p)))))))

(defun drover--session-option (session action)
  "Return what prompt ACTION changes of SESSION, as `drover--option'.
The model and effort are those its backend tells it runs now, through
:options, when it can, else those of its record; an effort the model
runs by default is not chosen."
  (let* ((options (and (memq action '(model effort))
                       (drover-supports-p session :options)
                       (drover-backend-call session :options)))
         (model (plist-get options :model)))
    (pcase action
      ('agent (drover--option (drover-session-agent session) t))
      ('model (drover--option (if model (drover--menu-model-name model) (drover-session-model session)) t))
      ('effort (if model
                   (let ((variant (plist-get options :variant)))
                     (drover--option variant (not (eq variant (drover-model-default-variant model)))))
                 (drover--option (drover-session-effort session)
                                 (not (drover-session-effort-default session)))))
      ('directory (drover--option (and (drover-session-directory session)
                                       (abbreviate-file-name
                                        (directory-file-name (drover-session-directory session))))
                                  t)))))

(defun drover--option-value (action)
  "Return the value prompt ACTION of the dispatch menu changes, or nil.
The value is (TEXT . CHOSEN), CHOSEN nil for a value inherited from the
defaults, as the prompt's header dims it.  See `drover--option-target'."
  (drover-with-reporting "menu value"
    (pcase (drover--option-target action)
      (`(prompt . ,buffer) (drover--prompt-buffer-option buffer action))
      (`(session . ,session) (drover--session-option session action)))))

(defun drover--dispatch-options-description ()
  "Return the title of the dispatch menu's column choosing what runs.
It names whether the choices go to a session or to a prompt."
  (if (eq (car-safe (drover--option-target 'model)) 'session) "Session runs" "Prompt runs"))

(defcustom drover-dispatch-key nil
  "How drover's messages and hints name the key of `drover-dispatch'.
nil names the key Emacs's help names for the command, here
\\[drover-dispatch]; a string names it as written, such as \"Caps Lock\"
when the keyboard sends that key to Emacs as another one, such as
<f19>.  Either way, every key of the command closes the menu."
  :type '(choice (const :tag "The key Emacs's help names" nil)
                 (string :tag "Name"))
  :group 'drover)

(defun drover-dispatch-key-description (&optional fontify)
  "Return how drover names the key of `drover-dispatch', or nil.
It is `drover-dispatch-key' when set, else the key Emacs's help names
for the command, nil when the command has none.  With FONTIFY non-nil,
the name has the face of keys in help, as `substitute-command-keys'
gives them."
  (when-let* ((name (or drover-dispatch-key
                        (when-let* ((key (where-is-internal #'drover-dispatch nil t)))
                          (key-description key)))))
    (if fontify
        (propertize name 'font-lock-face 'help-key-binding 'face 'help-key-binding)
      name)))

(defun drover-dispatch-keys (command)
  "Return the keys running COMMAND through `drover-dispatch', or nil.
They are the menu's global key followed by COMMAND's key in it, such as
\"C-\\=' X\", the menu's key named as `drover-dispatch-key-description'
names it; nil when the menu has no global key or no entry for COMMAND."
  (when-let* ((menu (drover-dispatch-key-description))
              (key (plist-get (cdr (ignore-errors (transient-get-suffix 'drover-dispatch command)))
                              :key)))
    (concat menu " " key)))

(defun drover--dispatch-pending-description ()
  "Return the description of Edit pending prompt in the dispatch menu.
The count of the pending prompts of the session at point follows it,
when it has some."
  (let ((count (if-let* ((session (drover-dispatch-session :edit-pending)))
                   (drover-pending-prompts session)
                 0)))
    (if (> count 0)
        (concat "Edit pending " (propertize (format "%d" count) 'face 'transient-value))
      "Edit pending")))

(defun drover--dispatch-pending-p ()
  "Return non-nil when the session at point has pending prompts to edit."
  (when-let* ((session (drover-dispatch-session :edit-pending)))
    (> (drover-pending-prompts session) 0)))

(defun drover--dispatch-stop-description ()
  "Return the description of Stop actions in the dispatch menu.
How far the actions on several sessions running are follows it, as the
menu opened or last refreshed."
  (if-let* ((progress (drover-actions-progress)))
      (concat "Stop actions " (propertize (drover--menu-value progress) 'face 'transient-value))
    "Stop actions"))

(defun drover--dispatch-send-description ()
  "Return the description of Send in the dispatch menu.
It names where the prompt goes when `drover-dispatch-send-target-functions'
tells."
  (let ((target nil))
    (run-hook-wrapped 'drover-dispatch-send-target-functions
                      (lambda (function)
                        (setq target (drover-with-reporting "send target"
                                       (funcall function)))
                        (stringp target)))
    (if (stringp target)
        (concat "Send " (propertize (drover--menu-value target) 'face 'transient-value))
      "Send")))

(defclass drover--prompt-option (transient-variable)
  ((action :initarg :action
           :documentation "The prompt action of the entry: agent, model, effort or directory."))
  "An entry of `drover-dispatch' choosing what a prompt or a session runs.
Its command performs a prompt action; it shows the value that action
changes, as Magit shows an option's: highlighted when chosen, dimmed
when inherited from the defaults or when the entry does not apply.")

(cl-defmethod transient-init-value ((_obj drover--prompt-option))
  "Keep no value: the entry shows the value its action changes."
  nil)

(cl-defmethod transient-format-value ((obj drover--prompt-option))
  "Return the value the action of OBJ changes, as the menu shows it."
  (let ((action (oref obj action)))
    (pcase (drover--option-value action)
      (`(,text . ,chosen)
       (propertize (if (eq action 'directory)
                       (drover--menu-value text drover--menu-directory-width t)
                     (drover--menu-value text))
                   'face (if (and chosen (not (oref obj inapt))) 'transient-value 'transient-inactive-value)))
      (_ ""))))

;;;; The menus

(defun drover--dispatch-history-p ()
  "Return non-nil when the menus may show or hide older sessions."
  (and (drover-dispatch-view-shown-p) (drover-history-available-p)))

(defun drover--dispatch-archived-p ()
  "Return non-nil when the menus may show or hide archived sessions."
  (and (drover-dispatch-view-shown-p) (drover-archived-available-p)))

(transient-define-group drover--view-columns
  ["View"
   :if drover--dispatch-layout-p
   ("=o" drover-view--sort-by)
   ("=r" drover-view--sort-order)
   ("=g" drover-view--group-by)
   ("-f" drover-view--follow)]
  ;; A row a field: its filter, then its column, on the same letter.
  ;; The subagents, older and archived sessions shown come first, as
  ;; they keep sessions out as the filter does, on the filter's keys.
  [:class drover--menu-table
   :description "Field"
   :headings ("Filter" "Column")
   :if drover--dispatch-subagents-p
   ["subagents" :class drover--menu-table-row
    ("=u" "subagents" drover-toggle-subagents
     :class drover--setting
     :format " %k %v"
     :getter (lambda () (unless (drover--dispatch-foreign-subagents-p)
                          (default-value 'drover-show-subagents)))
     :setter drover-set-show-subagents
     :chooser drover-next-subagent-mode
     :options ,drover-subagent-mode-labels
     :inapt-if-not drover--dispatch-subagents-p)]
   ["older" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=O" drover-view--history :inapt-if-not drover--dispatch-history-p :format " %k %v")]
   ["archived" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=z" drover-view--archived :inapt-if-not drover--dispatch-archived-p :format " %k %v")]
   ["project" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=p" drover-view--filter-project) ("-p" drover-view--column-location)]
   ["status" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=s" drover-view--filter-statuses)]
   ["agent" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=a" drover-view--filter-agent) ("-a" drover-view--column-agent)]
   ["model" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=m" drover-view--filter-model) ("-m" drover-view--column-model)]
   ["provider" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=v" drover-view--filter-provider) ("-v" drover-view--column-provider)]
   ["harness" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=H" drover-view--filter-harness) ("-H" drover-view--column-harness)]
   ["host" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=h" drover-view--filter-host) ("-h" drover-view--column-host)]
   ["updated" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=d" drover-view--filter-since) ("-d" drover-view--column-age)]
   ["text" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=f" drover-view--filter-search)]
   ;; Under the filters, which it clears.
   ["" :class drover--menu-table-row :if drover-dispatch-view-shown-p
    ("=c" "clear" drover-filter-clear :transient t)]]
  ;; The fields no filter takes.
  ["Columns"
   :if drover--dispatch-layout-p
   ("-b" drover-view--column-branch)
   ("-x" drover-view--column-context)
   ("-c" drover-view--column-created)
   ("-k" drover-view--column-cache)]
  [:if drover--dispatch-layout-p
   ("-s" drover-view--column-summary)
   ("-$" drover-view--column-cost)
   ("-g" drover-view--column-goal)
   ("-#" drover-view--column-attempt)])

(defvar drover--dispatch-close-keys nil
  "The keys of `drover-dispatch' where it last opened, which close it.")

(defclass drover--menu-digits (transient-suffix) ()
  "The entry of the dispatch menu standing for the digits that open sessions.
Its key is 1, and it shows the keys 1 to 9 and 0 together: the other
nine are entries of a hidden group of the menu, running the same
command, so that ten keys take one line.")

(cl-defmethod transient-format-key ((obj drover--menu-digits))
  "Return the keys OBJ stands for, 1 to 9 and 0, in the face of its own."
  (let ((key (cl-call-next-method obj)))
    (concat key (propertize "…9 0" 'face (get-text-property 0 'face key)))))

(defun drover--dispatch-own-keys ()
  "Return the keys running `drover-dispatch' in the current buffer.
Those of the menu's own entries, which keep their command there, and
those of menus and remappings are left out."
  (seq-remove (lambda (key)
                (or (memq (aref key 0) '(menu-bar tool-bar tab-bar remap))
                    (ignore-errors (transient-get-suffix 'drover-dispatch (key-description key)))))
              (where-is-internal #'drover-dispatch)))

(defun drover--dispatch-close-entries (_children)
  "Return the entries of `drover-dispatch' closing it.
Each key of `drover--dispatch-close-keys' closes it, so that the key
that opened the menu, whichever it was, closes it again."
  (transient-parse-suffixes 'drover-dispatch
                            (mapcar (lambda (key)
                                      (list (key-description key) "Close" 'transient-quit-one))
                                    drover--dispatch-close-keys)))

;; Every menu of drover's closes on q, as on C-g: q quits drover's lists
;; and reports, from which the menus open.
(transient-define-group drover--menu-close
  [:hide always
   ("q" "Close" transient-quit-one)])

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
The menu lays its columns out in rows as wide as its window.  Commands
on sessions and prompts work everywhere; session commands act on the
session at point, in lists and in prompt buffers; prompt commands
perform the prompt actions of the current buffer; the view settings,
shared by drover's lists, apply while one shows.  Commands that do not
apply here are shown, greyed out, but those that cannot where the menu
opens, left out: the view settings while no list is in sight, the
prompt keys editing or reading a prompt buffer outside prompt buffers,
and Stop actions but while actions on several sessions run, their
progress beside it.  Settings show their value beside their key, as
Magit shows them, and changing one leaves the menu open; Edit pending
shows how many prompts of the session at point wait pending.  The
settings of a field share a row of the Field table: its filter, then
whether the list shows it, on the same letter, such as =m and -m.

From a buffer telling no session, such as the prompt or a file, while
one of drover's lists shows in a window of the frame, session commands
act on that list, on the session at its window's point (see
`drover--context').  While the menu acts on one of drover's lists, its
List group moves that point, the menu staying open and following the
new session at point, and RET opens the session, but in a buffer that
sends prompts, where RET stays Send, and from another buffer while
drover's prompt holds a prompt to send, which RET sends.  In the list
itself, RET opens the session and M-RET sends that prompt.  The digits
1 to 9 open the sessions at the top of their trees by number, 0 the
last, as in the list (see `drover-list-open-numbered').

Send sends drover's prompt from any buffer, as from the prompt, to the
session chosen for it, else to one asked for, unshown."
  :class 'drover--menu-prefix
  :refresh-suffixes t
  [:class drover--menu-flow
          ["Sessions"
           ("j" "Next" drover-next)
           ("B" "Bury quiet" drover-bury-quiet-sessions :inapt-if-not drover-dispatch-quiet-p)
           ("b" "Select" drover-select)
           ("l" "List" drover-list)
           ("L" "Side list" drover-list-side)
           ("p" "Project" drover-select-project)
           ("S" "Search" drover-search)
           ("$" "Usage" drover-usage)
           ;; Only while an action on several sessions runs.
           ("X" drover--dispatch-stop-description drover-stop-actions :if drover-actions-progress)]
          ["Prompt"
           ("\"" "Prompt" drover-prompt-dwim :inapt-if drover-dispatch-in-prompt-p)
           ("N" "Task prompt" drover-prompt-org-task :inapt-if-not drover-dispatch-org-task-p)
           ("'" "Show prompt" drover-prompt-toggle)
           ("#" "Scheduled" drover-schedule-list)]
          ["Task"
           ("n" "Start or continue" drover-task-start :inapt-if-not drover-dispatch-task-p)
           ("F" "Finish" drover-task-finish :inapt-if-not drover-dispatch-task-p)
           ("I" "Link session" drover-task-link-session :inapt-if-not drover-dispatch-task-link-p)
           ("&" "Attempts" drover-fanout :inapt-if-not drover-dispatch-task-p)
           ("W" "Worktrees" drover-task-sweep)]
          [:description drover--dispatch-options-description
                        ("a" "Agent" drover-prompt-agent :class drover--prompt-option :action agent
                         :inapt-if-not (lambda () (drover-dispatch-prompt-p 'agent)))
                        ("m" "Model" drover-prompt-model :class drover--prompt-option :action model
                         :inapt-if-not (lambda () (drover-dispatch-prompt-p 'model)))
                        ("e" "Effort" drover-prompt-effort :class drover--prompt-option :action effort
                         :inapt-if-not (lambda () (drover-dispatch-prompt-p 'effort)))
                        ("d" "Directory" drover-prompt-directory :class drover--prompt-option :action directory
                         :inapt-if-not (lambda () (drover-dispatch-prompt-p 'directory)))]
          ["Input"
           ("RET" drover--dispatch-send-description drover-prompt-send
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'send))
            :if-not drover-dispatch-list-return-p)
           ;; Where RET opens, Send has a key of its own.
           ("M-RET" drover--dispatch-send-description drover-prompt-send
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'send))
            :if drover-dispatch-list-return-p)
           ;; These edit or read what a prompt buffer shows: only there.
           ("S-<return>" "Schedule" drover-prompt-schedule :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'schedule)))
           ("i" "Attach" drover-prompt-attach :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'attach)))
           ("k" "Clear" drover-prompt-clear :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'clear)))]
          [:if drover-dispatch-prompt-buffer-p
               ("/" "Slash command" drover-prompt-slash :inapt-if-not (lambda () (drover-dispatch-prompt-p 'slash)))
               ("@" "Mention" drover-prompt-mention :inapt-if-not (lambda () (drover-dispatch-prompt-p 'mention)))
               ("y" "Copy block" drover-prompt-copy-block
                :inapt-if-not (lambda () (drover-dispatch-prompt-p 'copy-block)))]
          ["Conversation"
           ("t" "Revert here" drover-prompt-revert :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert)))
           ("T" "Cancel revert" drover-prompt-revert-cancel :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert-cancel)))
           ("C" "Compact" drover-compact :inapt-if-not (lambda () (drover-dispatch-prompt-p 'compact)))
           ("f" "Fork" drover-fork :inapt-if-not (lambda () (drover-dispatch-prompt-p 'fork)))
           ("H" "Clone" drover-clone
            :advice* drover--dispatch-in-list
            :inapt-if-not (lambda () (drover-dispatch-session nil #'drover-clone-p)))
           ("." "Last answer" drover-prompt-last-answer
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'last-answer)))
           ("O" "Open link" drover-prompt-open-link :if drover-dispatch-prompt-buffer-p
            :inapt-if-not (lambda () (drover-dispatch-prompt-p 'open-link)))]
          ["Session"
           :advice* drover--dispatch-in-list
           ("o" "Other window" drover-open-other-window
            :inapt-if-not (lambda () (drover-dispatch-session :open)))
           ("u" "Parent" drover-open-parent
            :inapt-if-not (lambda () (drover-dispatch-session :open #'drover-session-parent)))
           ("c" "Subagent" drover-open-subagent
            :inapt-if-not (lambda () (drover-dispatch-session :open #'drover--has-subagents-p)))
           ("v" "Peek" drover-peek
            :inapt-if-not (lambda () (and (drover-dispatch-list-p) (drover-dispatch-session :preview))))
           ("R" "Review" drover-review
            :inapt-if-not (lambda () (drover-dispatch-session nil #'drover-reviewable-p)))
           ("A" "Answer" drover-respond
            :inapt-if-not (lambda () (drover-dispatch-session nil #'drover-session-to-answer)))
           ("<tab>" "Build + go" drover-build-and-go
            :inapt-if-not (lambda () (drover-dispatch-session nil #'drover-build-and-go-p)))
           ("K" "Interrupt" drover-interrupt
            :inapt-if-not (lambda () (drover-dispatch-selection :interrupt #'drover--running-p)))
           ("s" "Mark seen" drover-mark-seen
            :inapt-if-not (lambda () (drover-dispatch-selection :mark-seen #'drover--ended-unseen-p)))]
          [:advice* drover--dispatch-in-list
                    ("E" drover--dispatch-pending-description drover-edit-pending-prompt
                     :inapt-if-not drover--dispatch-pending-p)
                    ("+" "New session" drover-new-session
                     :inapt-if-not (lambda () (and (or (drover-dispatch-list-p) (drover-dispatch-session))
                                                   (drover--can-start-p))))
                    ("g" "Refresh" drover-refresh
                     :inapt-if-not (lambda () (or (drover-dispatch-list-p) (drover-dispatch-session))))
                    ("r" "Rename" drover-rename :inapt-if-not (lambda () (drover-dispatch-session :rename)))
                    ("x" "Delete" drover-delete :inapt-if-not (lambda () (drover-dispatch-selection :delete)))
                    ("z" "Archive" drover-archive
                     :inapt-if-not (lambda () (drover-dispatch-selection nil #'drover-archivable-p)))
                    ("Z" "Unarchive" drover-unarchive
                     :inapt-if-not (lambda () (drover-dispatch-selection nil #'drover-unarchivable-p)))
                    ("G" "Goal" drover-goal :advice* drover--dispatch-goal :inapt-if-not drover-dispatch-goal-p)]
          drover--view-columns
          ;; Only while the menu acts on one of drover's lists, here or in
          ;; another window.  Moving stays in the menu, which follows the new
          ;; session at point.
          [:description drover--dispatch-list-description
                        :if drover-dispatch-view-p
                        ("C-n" "Next session" drover-list-next-session :transient t :advice drover--dispatch-navigate)
                        ("C-p" "Previous session" drover-list-previous-session :transient t :advice drover--dispatch-navigate)
                        ("M-n" "Next group" drover-list-next-group :transient t :advice drover--dispatch-navigate
                         :inapt-if-not drover--dispatch-grouped-p)
                        ("M-p" "Previous group" drover-list-previous-group :transient t :advice drover--dispatch-navigate
                         :inapt-if-not drover--dispatch-grouped-p)
                        ("M-<" "First session" drover-list-first-session :transient t :advice drover--dispatch-navigate)
                        ("M->" "Last session" drover-list-last-session :transient t :advice drover--dispatch-navigate)
                        ("1" "Open by number" drover-list-open-numbered :class drover--menu-digits
                         :advice* drover--dispatch-in-list)
                        ("RET" "Open" drover-open-at-point
                         :advice* drover--dispatch-in-list
                         :if drover-dispatch-list-return-p
                         :inapt-if-not (lambda () (drover-dispatch-session :open)))]]
  ;; The keys opening the menu close it, as users bind it globally.
  [:class transient-column
          :hide always
          :setup-children drover--dispatch-close-entries]
  ;; The other digits of the List group's entry, which shows them all.
  [:class transient-column
          :hide always
          :if drover-dispatch-view-p
          :advice* drover--dispatch-in-list
          ("2" "Open 2" drover-list-open-numbered)
          ("3" "Open 3" drover-list-open-numbered)
          ("4" "Open 4" drover-list-open-numbered)
          ("5" "Open 5" drover-list-open-numbered)
          ("6" "Open 6" drover-list-open-numbered)
          ("7" "Open 7" drover-list-open-numbered)
          ("8" "Open 8" drover-list-open-numbered)
          ("9" "Open 9" drover-list-open-numbered)
          ("0" "Open last" drover-list-open-numbered)]
  drover--menu-close
  (interactive)
  (setq drover--dispatch-close-keys (drover--dispatch-own-keys))
  (transient-setup 'drover-dispatch nil nil :scope (drover--context)
                   :display-action drover-menu-display-action))

(transient-define-prefix drover-view ()
  "Choose how drover's session lists sort, group and filter sessions.
Its entries are the view settings of `drover-dispatch', on the same
keys, for lists that bind it to V.  Each setting shows its value;
changing one redraws the lists at once, and the menu stays open.
Sorting keeps subagents under their parent.  =u cycles the subagents
shown (all, active, none), =O shows older sessions, =z archived ones,
-f turns follow mode on in the list.  The Field table has a row a
field: its filter, an option, then its column, a switch, on the same
letter.  A filter left empty keeps every session; its pickers clear it
on an empty answer or (any), and =c clears them all.  The columns are
the fields the list may show, as room allows: each switch lets a field
show, or hides it, for this session (`drover-list-hidden-fields' keeps
the choice); the Columns group holds those of the fields no filter
takes."
  :class 'drover--menu-prefix
  :refresh-suffixes t
  [:class drover--menu-flow
   drover--view-columns]
  ;; The key opening the menu closes it.
  [:hide always
   ("V" "Close" transient-quit-one)]
  drover--menu-close
  (interactive)
  (drover--check-view)
  ;; The list's window, from which the dispatch menu may have opened it.
  (transient-setup 'drover-view nil nil :scope (selected-window)
                   :display-action drover-menu-display-action))

(provide 'drover-keys)
;;; drover-keys.el ends here
