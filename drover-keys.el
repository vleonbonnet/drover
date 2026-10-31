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
;;   draw, such as opencode.el's.  In a prompt buffer they act on the
;;   buffer's session, through the menu.
;; - Prompt keys perform the prompt actions of `drover-prompt-actions',
;;   through the menu.
;; - Global keys work everywhere, through the menu.
;;
;; Bind `drover-dispatch' globally, for instance to C-'; drover binds no
;; global key.  Other packages add their own global commands to the menu
;; with `transient-append-suffix' and `transient-insert-suffix', placed
;; relative to drover's keys: `drover-opencode' inserts its session lists
;; before "3".
;;
;; `drover-group' and `drover-filter' choose how drover's views group
;; sessions and which they list.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'drover-core)
(require 'drover-ui)

(autoload 'drover-next "drover-attention" nil t)
(autoload 'drover-select "drover-attention" nil t)
(autoload 'drover-dashboard "drover-dashboard" nil t)
(autoload 'drover-table "drover-table" nil t)
(autoload 'drover-sidebar "drover-sidebar" nil t)

(declare-function drover-read-session "drover-attention" (prompt sessions))
(declare-function org-store-link "ol" (arg &optional interactive?))
(defvar org-stored-links)

;;;; Opening sessions

(defun drover-open-at-point (&optional other-window)
  "Display the session at point; with OTHER-WINDOW, in another window.
Views bind it to RET."
  (interactive "P")
  (drover-open (drover-session-at-point) other-window))

(defun drover-open-other-window (session)
  "Display SESSION in another window.
Interactively, SESSION is the session at point."
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
used last, else the most recently used other window that is not
dedicated, else a new window."
  (let ((own (selected-window)))
    (or (and drover-main-window-function (funcall drover-main-window-function))
        (and (window-live-p drover--follow-window)
             (not (eq drover--follow-window own))
             (eq (window-frame drover--follow-window) (selected-frame))
             drover--follow-window)
        (get-mru-window nil nil t)
        (split-window-sensibly own)
        (ignore-errors (split-window own))
        (user-error "No window to show sessions in"))))

(defun drover--follow (&optional force)
  "Show the session at point in another window if it changed since last time.
With FORCE, show it whatever was shown before."
  (let ((position (cons (point) (buffer-modified-tick))))
    (when (or force (not (equal position drover--follow-position)))
      (setq drover--follow-position position)
      (with-demoted-errors "Drover follow: %S"
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
`drover-toggle-subagents' calls it in lists drover does not draw, such
as opencode.el's.")

(defun drover-toggle-subagents ()
  "Show or hide the sessions of subagents.
In drover's views, this changes `drover-show-children' for every view;
in another list, it calls `drover-toggle-subagents-function'.  A
subagent that needs you always shows."
  (interactive)
  (if drover-toggle-subagents-function
      (call-interactively drover-toggle-subagents-function)
    (setq-default drover-show-children (not (default-value 'drover-show-children)))
    (drover-view-settings-changed)
    (message (if (default-value 'drover-show-children) "Showing subagents" "Hiding subagents"))))

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

;;;; Org links

(defvar drover-store-link-functions nil
  "Functions returning an Org link to a session, for `drover-store-link'.
Each is called with a `drover-session' and returns a list (LINK
DESCRIPTION), or nil to let the next one try.")

(defun drover-store-link (session)
  "Store an Org link to SESSION, for `org-insert-link'.
The link comes from the first function of `drover-store-link-functions'
that returns one; failing that, `org-store-link' runs in the session's
buffer.  Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (if-let* ((link (run-hook-with-args-until-success 'drover-store-link-functions session)))
      (progn
        (require 'ol)
        (push (list (car link) (cadr link)) org-stored-links)
        (message "Stored: %s" (or (cadr link) (car link))))
    (if-let* ((buffer (drover-session-buffer session)))
        (with-current-buffer buffer
          (org-store-link nil t))
      (user-error "No link to %s" (drover-session-display-title session)))))

;;;; Prompt actions

(defun drover-prompt-send ()
  "Send the prompt, through the prompt action `send'."
  (interactive)
  (drover-prompt-run 'send))

(defun drover-prompt-schedule ()
  "Schedule the prompt, through the prompt action `schedule'."
  (interactive)
  (drover-prompt-run 'schedule))

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
  "k" #'drover-interrupt
  "s" #'drover-mark-seen
  "u" #'drover-open-parent
  "c" #'drover-open-subagent
  "r" #'drover-rename
  "x" #'drover-delete
  "z" #'drover-archive
  "g" #'drover-refresh
  "l" #'drover-store-link
  "+" #'drover-new-session
  "v" #'drover-peek
  "V" #'drover-follow-mode
  "U" #'drover-toggle-subagents
  "G" #'drover-group
  "F" #'drover-filter)

(define-minor-mode drover-session-keys-mode
  "Bind the keys of `drover-session-map' in a list of sessions.
For lists drover does not draw, such as opencode.el's, whose buffers
also set `drover-session-at-point-function'.

\\{drover-session-map}"
  :keymap drover-session-map
  :group 'drover)

;;;; The dispatch menu

(defun drover--context ()
  "Return what the dispatch menu depends on in the current buffer, as a plist.
:session is the session at point or nil, :list non-nil in a list of
sessions, :view non-nil in one of drover's views."
  (list :session (with-demoted-errors "Drover session at point: %S"
                   (drover-session-at-point t))
        :list (and (drover-session-list-p) t)
        :view (and drover-view-p t)))

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

(defun drover-dispatch-list-p ()
  "Return non-nil when the dispatch menu acts on a list of sessions."
  (plist-get (drover--scope) :list))

(defun drover-dispatch-view-p ()
  "Return non-nil when the dispatch menu acts on one of drover's views."
  (plist-get (drover--scope) :view))

(defun drover-dispatch-prompt-p (action)
  "Return non-nil when prompt ACTION applies from where the dispatch menu acts.
For `compact' and `fork', a list's session at point whose backend
operates on it counts."
  (let ((scope (drover--scope)))
    (or (drover--prompt-target action (plist-get scope :session) (plist-get scope :list) t)
        (and (plist-get scope :list)
             (when-let* ((operation (alist-get action drover--backend-prompt-actions)))
               (drover-dispatch-session operation))))))

(defun drover--running-p (session)
  "Return non-nil when SESSION runs a turn, which can be interrupted."
  (memq (drover-session-status session) '(waiting retrying working)))

(defun drover--ended-unseen-p (session)
  "Return non-nil when SESSION ended a turn the user did not see."
  (memq (drover-session-status session) '(done failed)))

(defun drover--can-start-p ()
  "Return non-nil when a backend starts sessions."
  (seq-some (lambda (backend) (functionp (drover-backend-property backend :new)))
            (drover-backends)))

(defun drover--has-subagents-p (session)
  "Return non-nil when SESSION has subagent sessions."
  (drover-subagents session))

;;;###autoload(autoload 'drover-dispatch "drover-keys" nil t)
(transient-define-prefix drover-dispatch ()
  "Act on sessions of coding agents, on the session at point, or on the prompt.
Global commands work everywhere; session commands act on the session at
point, in lists and in prompt buffers; prompt commands perform the
prompt actions of the current buffer.  Commands that do not apply here
are shown, greyed out."
  ["Global"
   [("j" "Next" drover-next)
    ("b" "Select" drover-select)]
   [("3" "Dashboard" drover-dashboard)
    ("4" "Table" drover-table)
    ("5" "Sidebar" drover-sidebar)]
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
   [("o" "Other window" drover-open-other-window
     :inapt-if-not (lambda () (drover-dispatch-session :open)))
    ("u" "Parent" drover-open-parent
     :inapt-if-not (lambda () (drover-dispatch-session :open #'drover-session-parent)))
    ("c" "Subagent" drover-open-subagent
     :inapt-if-not (lambda () (drover-dispatch-session :open #'drover--has-subagents-p)))
    ("v" "Peek" drover-peek
     :inapt-if-not (lambda () (and (drover-dispatch-list-p) (drover-dispatch-session :preview))))
    ("V" "Follow" drover-follow-mode :inapt-if-not drover-dispatch-list-p)]
   [("A" "Answer" drover-respond
     :inapt-if-not (lambda () (drover-dispatch-session :respond #'drover-session-requests)))
    ("k" "Interrupt" drover-interrupt
     :inapt-if-not (lambda () (drover-dispatch-session :interrupt #'drover--running-p)))
    ("s" "Mark seen" drover-mark-seen
     :inapt-if-not (lambda () (drover-dispatch-session :mark-seen #'drover--ended-unseen-p)))
    ("+" "New session" drover-new-session
     :inapt-if-not (lambda () (and (or (drover-dispatch-list-p) (drover-dispatch-session))
                                   (drover--can-start-p))))
    ("g" "Refresh" drover-refresh
     :inapt-if-not (lambda () (or (drover-dispatch-list-p) (drover-dispatch-session))))]
   [("r" "Rename" drover-rename :inapt-if-not (lambda () (drover-dispatch-session :rename)))
    ("x" "Delete" drover-delete :inapt-if-not (lambda () (drover-dispatch-session :delete)))
    ("z" "Archive" drover-archive :inapt-if-not (lambda () (drover-dispatch-session :archive)))
    ("l" "Store link" drover-store-link :inapt-if-not (lambda () (drover-dispatch-session)))]
   [("U" "Subagents" drover-toggle-subagents :inapt-if-not drover-dispatch-list-p)
    ("G" "Group" drover-group :inapt-if-not drover-dispatch-view-p)
    ("F" "Filter" drover-filter :inapt-if-not drover-dispatch-view-p)]]
  ["Prompt"
   [("RET" "Send" drover-prompt-send :inapt-if-not (lambda () (drover-dispatch-prompt-p 'send)))
    ("S-<return>" "Schedule" drover-prompt-schedule
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'schedule)))
    ("i" "Attach" drover-prompt-attach :inapt-if-not (lambda () (drover-dispatch-prompt-p 'attach)))]
   [("/" "Slash command" drover-prompt-slash :inapt-if-not (lambda () (drover-dispatch-prompt-p 'slash)))
    ("@" "Mention" drover-prompt-mention :inapt-if-not (lambda () (drover-dispatch-prompt-p 'mention)))
    ("y" "Copy block" drover-prompt-copy-block
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'copy-block)))]
   [("t" "Revert here" drover-prompt-revert :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert)))
    ("T" "Cancel revert" drover-prompt-revert-cancel
     :inapt-if-not (lambda () (drover-dispatch-prompt-p 'revert-cancel)))]]
  (interactive)
  (transient-setup 'drover-dispatch nil nil :scope (drover--context)))

;;;; Grouping

(defun drover--check-view ()
  "Signal a `user-error' unless the current buffer is one of drover's views."
  (unless drover-view-p
    (user-error "Only drover's views group and filter sessions")))

(defclass drover--grouping (transient-variable)
  ((grouping :initarg :grouping :documentation "The value of `drover-group-by' it sets.")
   (format :initform " %k %d"))
  "An infix of `drover-group' choosing one value of `drover-group-by'.")

(cl-defmethod transient-init-value ((obj drover--grouping))
  "Set the value of OBJ to the current grouping."
  (oset obj value drover-group-by))

(cl-defmethod transient-infix-read ((obj drover--grouping))
  "Return the grouping OBJ chooses."
  (oref obj grouping))

(cl-defmethod transient-infix-set ((obj drover--grouping) value)
  "Group every view by VALUE, the grouping of OBJ."
  (oset obj value value)
  (drover-set-group-by value))

(cl-defmethod transient-format-description ((obj drover--grouping))
  "Return the description of OBJ, highlighted when it is the current grouping."
  (propertize (oref obj description)
              'face (if (eq drover-group-by (oref obj grouping))
                        'transient-value
                      'transient-inactive-value)))

(cl-defmethod transient-format-value ((_obj drover--grouping))
  "Return nothing: the description of a grouping shows whether it is chosen."
  "")

(transient-define-infix drover-group--status ()
  "Group sessions by status in every view."
  :class 'drover--grouping
  :grouping 'status
  :description "status")

(transient-define-infix drover-group--project ()
  "Group sessions by project in every view."
  :class 'drover--grouping
  :grouping 'project
  :description "project")

(transient-define-infix drover-group--none ()
  "List sessions without groups in every view."
  :class 'drover--grouping
  :grouping nil
  :description "nothing")

(transient-define-prefix drover-group ()
  "Choose how every drover view groups sessions.
The current grouping is highlighted; choosing another redraws the views
at once, and the menu stays open."
  ["Group sessions by"
   ("s" drover-group--status)
   ("p" drover-group--project)
   ("n" drover-group--none)]
  (interactive)
  (drover--check-view)
  (transient-setup 'drover-group))

;;;; Filtering

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

(defun drover--read-filter-project (current)
  "Read the project to list, CURRENT being the one listed now, or nil."
  (let* ((projects (sort (delete-dups (delq nil (mapcar #'drover-session-project (drover-sessions))))
                         #'string<))
         (project (completing-read (if current
                                       (format "Project (now %s, empty for any): " current)
                                     "Project (empty for any): ")
                                   projects nil t)))
    (unless (string-empty-p project) project)))

(defun drover--read-filter-statuses (current)
  "Read the statuses to list, CURRENT being those listed now."
  (let* ((labels (mapcar (lambda (status) (cons (drover-status-label status) status))
                         drover-status-order))
         (chosen (completing-read-multiple
                  "Statuses (empty for any): " labels nil t
                  (and current (mapconcat #'drover-status-label current ",")))))
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
  "Return VALUE of the filter member KEY as the filter menu shows it."
  (pcase key
    (:statuses (drover-filter-statuses-label value))
    (:since (drover-filter-since-label value))
    (:search (format "\"%s\"" value))
    (_ (format "%s" value))))

(defclass drover--filter-member (transient-variable)
  ((filter-key :initarg :filter-key :documentation "The member of `drover-filter' it sets.")
   (read-function :initarg :read-function
                  :documentation "Function of the current value reading a new one."))
  "An infix of `drover-filter' setting one member of the variable `drover-filter'.")

(cl-defmethod transient-init-value ((obj drover--filter-member))
  "Set the value of OBJ to its member of the variable `drover-filter'."
  (oset obj value (drover--filter-value drover-filter (oref obj filter-key))))

(cl-defmethod transient-infix-read ((obj drover--filter-member))
  "Read a new value for the member of the variable `drover-filter' OBJ sets."
  (funcall (oref obj read-function) (drover--filter-value drover-filter (oref obj filter-key))))

(cl-defmethod transient-infix-set ((obj drover--filter-member) value)
  "Set the member of the variable `drover-filter' OBJ sets to VALUE."
  (oset obj value value)
  (drover-set-filter (oref obj filter-key) value))

(cl-defmethod transient-format-value ((obj drover--filter-member))
  "Return the value of the member of the variable `drover-filter' OBJ sets."
  (let ((key (oref obj filter-key)))
    (if-let* ((value (drover--filter-value drover-filter key)))
        (propertize (drover--filter-label key value) 'face 'transient-value)
      (propertize "any" 'face 'transient-inactive-value))))

(transient-define-infix drover-filter--project ()
  "List the sessions of one project in every view."
  :class 'drover--filter-member
  :filter-key :project
  :read-function #'drover--read-filter-project
  :description "Project")

(transient-define-infix drover-filter--statuses ()
  "List the sessions of some statuses in every view."
  :class 'drover--filter-member
  :filter-key :statuses
  :read-function #'drover--read-filter-statuses
  :description "Status")

(transient-define-infix drover-filter--since ()
  "List the sessions active since a time in every view."
  :class 'drover--filter-member
  :filter-key :since
  :read-function #'drover--read-filter-since
  :description "Updated")

(transient-define-infix drover-filter--search ()
  "List the sessions containing a text in every view."
  :class 'drover--filter-member
  :filter-key :search
  :read-function #'drover--read-filter-search
  :description "Search")

(defun drover-filter-clear ()
  "List every session in every view."
  (interactive)
  (setq drover-filter nil)
  (drover-view-settings-changed))

(transient-define-prefix drover-filter ()
  "Choose which sessions every drover view lists.
Each filter shows its value, or \"any\"; setting one redraws the views
at once, and the menu stays open.  An empty answer removes a filter."
  :refresh-suffixes t
  ["Filter sessions"
   ("p" drover-filter--project)
   ("s" drover-filter--statuses)
   ("d" drover-filter--since)
   ("f" drover-filter--search)]
  [("c" "Clear all" drover-filter-clear :transient t)]
  (interactive)
  (drover--check-view)
  (transient-setup 'drover-filter))

(defun drover-filter-clear-keys (&optional keymap)
  "Return the keys clearing the filter from KEYMAP, such as \"F c\".
They are the key of `drover-filter' in KEYMAP, the current local map by
default, then the key of `drover-filter-clear' in that menu, in the face
`help-key-binding'."
  (let ((menu (where-is-internal #'drover-filter (or keymap (current-local-map)) t))
        (clear (plist-get (cdr (ignore-errors (transient-get-suffix 'drover-filter #'drover-filter-clear)))
                          :key)))
    (if (and menu clear)
        (propertize (concat (key-description menu) " " (key-description (kbd clear)))
                    'face 'help-key-binding 'font-lock-face 'help-key-binding)
      (substitute-command-keys "\\[drover-filter-clear]"))))

(defun drover-filter-clear-hint (&optional keymap)
  "Return the sentence telling how to clear the filter from KEYMAP.
KEYMAP defaults to the current local map; see `drover-filter-clear-keys'."
  (concat (drover-filter-clear-keys keymap) " clears the filter."))

(provide 'drover-keys)
;;; drover-keys.el ends here
