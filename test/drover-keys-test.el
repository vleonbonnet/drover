;;; drover-keys-test.el --- Tests of the commands, keys and menus  -*- lexical-binding: t; -*-

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

;; Menus are inspected rather than shown: `drover-keys-test-menu' sets
;; up the suffix objects of a transient prefix as showing it would, in
;; the current buffer, and reports their keys and whether they are
;; inapt.  It leaves out the commands other packages add, such as
;; opencode.el's, which `drover-opencode' adds when the tests load it.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-keys)

(defmacro drover-keys-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS and the keys' state isolated."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     ;; `dlet': the list, which declares it, may not be loaded.
     (dlet ((drover-list-hidden-fields nil))
       (let (;; Readable glyphs, whatever the defaults.
             (opencode-session-status-icons '((waiting . "⚑") (failed . "✗") (done . "✓")
                                     (retrying . "↻") (working . "◐") (idle . "·")))
             (drover-default-prompt-buffer-function nil)
             ;; No prompt to send but those a test prepares.
             (drover-prepared-prompt-function nil)
             (drover-keys-test-actions nil))
         ,@body))))

(defvar drover-keys-test-actions nil
  "Prompt actions performed, newest first, as (ACTION BUFFER).")

(defun drover-keys-test-action (action)
  "Return a command recording that it performed ACTION."
  (lambda ()
    (interactive)
    (push (list action (current-buffer)) drover-keys-test-actions)))

(defmacro drover-keys-test-in (kind id &rest body)
  "Run BODY in a buffer of KIND whose session at point is the fake session ID.
KIND is `view', one of drover's views; `list', a list drover does not
draw; or `prompt', a buffer with every prompt action.  ID may be nil."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (setq-local drover-session-at-point-function (lambda () (and ,id (drover-find 'fake ,id))))
     (pcase ,kind
       ('view (setq-local drover-view-p t))
       ('list (drover-session-keys-mode))
       ('prompt (dolist (action '(send schedule clear attach slash mention revert revert-cancel copy-block
                                       agent model effort directory compact fork last-answer open-link))
                  (drover-prompt-define action (drover-keys-test-action action)))))
     ,@body))

(defun drover-keys-test--own-p (suffix)
  "Return non-nil when SUFFIX runs one of drover's commands."
  (string-prefix-p "drover-" (symbol-name (oref suffix command))))

(defun drover-keys-test-menu (prefix &optional all)
  "Return the groups of transient PREFIX as set up in the current buffer.
Each group is (TITLE . COLUMNS), each column a list of (KEY . INAPT)
for its suffixes, in order.  Only groups with a title are returned, and
only drover's commands unless ALL is non-nil; groups without any of
drover's commands are left out.  An untitled group of titled columns,
as the menus' flow of columns, stands for its columns, and an untitled
column of it continues the group before it."
  (let* ((transient--prefix (transient--init-prefix
                             prefix (and (eq prefix 'drover-dispatch) (list :scope (drover--context)))))
         (title-of (lambda (group)
                     (let ((description (and (slot-boundp group 'description) (oref group description))))
                       (if (functionp description) (funcall description) description))))
         (groups-p (lambda (group)
                     (seq-every-p (lambda (child) (cl-typep child 'transient-group)) (oref group suffixes))))
         (layout (mapcan (lambda (group)
                           (if (and (cl-typep group 'transient-group)
                                    (not (stringp (funcall title-of group)))
                                    (funcall groups-p group))
                               (copy-sequence (oref group suffixes))
                             (list group)))
                         (transient--init-suffixes prefix)))
         (result nil)
         (last nil))
    (cl-labels ((entries (group)
                  (mapcan (lambda (suffix)
                            (and (or all (drover-keys-test--own-p suffix))
                                 ;; Those an :if leaves out are neither shown nor bound.
                                 (not (oref suffix inactive))
                                 (list (cons (key-description (kbd (oref suffix key))) (oref suffix inapt)))))
                          (oref group suffixes))))
      (dolist (group layout)
        (when (cl-typep group 'transient-group)
          (let ((title (funcall title-of group)))
            (cond ((stringp title)
                   (setq last
                         (and (seq-some #'drover-keys-test--own-p (transient--flatten-suffixes (list group)))
                              (cons title (if (funcall groups-p group)
                                              (mapcar #'entries (oref group suffixes))
                                            (list (entries group))))))
                   (when last
                     (push last result)))
                  ((and last (cl-typep (oref group parent) 'drover--menu-flow))
                   (setcdr last (append (cdr last) (list (entries group)))))))))
      (nreverse result))))

(defun drover-keys-test-keys (prefix &optional all)
  "Return the keys of the groups of PREFIX here, as (TITLE KEY...).
Only drover's commands count, unless ALL is non-nil."
  (mapcar (lambda (group) (cons (car group) (mapcar #'car (apply #'append (cdr group)))))
          (drover-keys-test-menu prefix all)))

(defun drover-keys-test-apt (prefix)
  "Return the keys of drover's commands in PREFIX that apply here, in order."
  (mapcan (lambda (group)
            (mapcan (lambda (column) (mapcar #'car (seq-remove #'cdr column)))
                    (cdr group)))
          (drover-keys-test-menu prefix)))

(defun drover-keys-test-suffixes (prefix)
  "Return the suffix objects of PREFIX set up here, and the prefix object.
The value is (PREFIX-OBJECT . SUFFIXES)."
  (let* ((object (transient--init-prefix
                  prefix (and (eq prefix 'drover-dispatch) (list :scope (drover--context)))))
         (transient--prefix object))
    (cons object (transient--flatten-suffixes (transient--init-suffixes prefix)))))

;;;; The session map

(ert-deftest drover-keys-test-session-map ()
  (let ((bindings nil))
    (map-keymap (lambda (event command) (push (cons (key-description (vector event)) command) bindings))
                drover-session-map)
    (should (equal (sort bindings (lambda (a b) (string< (car a) (car b))))
                   (sort (copy-sequence
                          '(("o" . drover-open-other-window) ("A" . drover-respond)
                            ("K" . drover-interrupt) ("s" . drover-mark-seen)
                            ("u" . drover-open-parent) ("c" . drover-open-subagent)
                            ("r" . drover-rename) ("x" . drover-delete) ("z" . drover-archive)
                            ("Z" . drover-unarchive) ("g" . drover-refresh) ("+" . drover-new-session) ("v" . drover-peek)
                            ("U" . drover-toggle-subagents) ("V" . drover-view)))
                         (lambda (a b) (string< (car a) (car b))))))
    (dolist (binding bindings)
      (should (commandp (cdr binding))))))

(ert-deftest drover-keys-test-session-keys-mode ()
  (with-temp-buffer
    (should-not (eq (key-binding (kbd "r")) #'drover-rename))
    (drover-session-keys-mode)
    (should (eq (key-binding (kbd "r")) #'drover-rename))
    (should (eq (key-binding (kbd "V")) #'drover-view))
    (should (eq (key-binding (kbd "K")) #'drover-interrupt))
    (drover-session-keys-mode -1)
    (should-not (eq (key-binding (kbd "r")) #'drover-rename))))

;;;; Opening sessions

(ert-deftest drover-keys-test-open ()
  "RET opens in the selected window, o and C-u RET in another window."
  (drover-keys-test-with (list (drover-test-session "s"))
    (save-window-excursion
      (delete-other-windows)
      (let ((origin (selected-window))
            (opened nil))
        (setf (alist-get 'fake drover--backends)
              (append (list :open (lambda (session other-window)
                                    (push (list (drover-session-id session) other-window (selected-window))
                                          opened)))
                      (alist-get 'fake drover--backends)))
        (drover-keys-test-in 'view "s"
          (let ((view (current-buffer)))
            (call-interactively #'drover-open-at-point)
            (should (equal (car opened) (list "s" nil origin)))
            (call-interactively #'drover-open-other-window)
            (let ((other (nth 2 (car opened))))
              (should-not (eq other origin))
              (should-not (nth 1 (car opened)))
              (select-window origin)
              (with-current-buffer view
                (let ((current-prefix-arg '(4)))
                  (call-interactively #'drover-open-at-point)))
              (should (equal (car opened) (list "s" nil other)))
              (should (= (length (window-list nil 'nomini)) 2)))))))))

(ert-deftest drover-keys-test-open-parent ()
  (drover-keys-test-with (list (drover-test-session "root")
                               (drover-test-session "child" :parent "root")
                               (drover-test-session "orphan" :parent "gone"))
    (drover-keys-test-in 'prompt "child"
      (call-interactively #'drover-open-parent)
      (should (equal drover-test-calls '((:open "root" nil)))))
    (drover-keys-test-in 'view "root"
      (should-error (call-interactively #'drover-open-parent) :type 'user-error))
    ;; A parent not listed opens by its identifier.
    (drover-keys-test-in 'view "orphan"
      (call-interactively #'drover-open-parent)
      (should (equal (car drover-test-calls) '(:open "gone" nil))))))

(ert-deftest drover-keys-test-open-subagent ()
  (drover-keys-test-with (list (drover-test-session "root")
                               (drover-test-session "only" :parent "root")
                               (drover-test-session "two")
                               (drover-test-session "idle" :parent "two" :status 'idle)
                               (drover-test-session "asking" :parent "two" :status 'waiting))
    (drover-keys-test-in 'view "root"
      (call-interactively #'drover-open-subagent)
      (should (equal drover-test-calls '((:open "only" nil)))))
    (setq drover-test-calls nil)
    (drover-keys-test-in 'view "two"
      (let ((offered nil))
        (cl-letf (((symbol-function 'drover-read-session)
                   (lambda (prompt sessions)
                     (should (equal prompt "Subagent: "))
                     (setq offered (mapcar #'drover-session-id sessions))
                     (cadr sessions))))
          (call-interactively #'drover-open-subagent))
        ;; Most urgent first.
        (should (equal offered '("asking" "idle")))
        (should (equal drover-test-calls '((:open "idle" nil))))))
    (drover-keys-test-in 'view "only"
      (should-error (call-interactively #'drover-open-subagent) :type 'user-error))))

(ert-deftest drover-keys-test-peek ()
  (drover-keys-test-with (list (drover-test-session "s"))
    (save-window-excursion
      (unwind-protect
          (drover-keys-test-in 'view "s"
            (call-interactively #'drover-peek)
            (should (equal drover-test-calls '((:preview "s"))))
            (with-current-buffer drover-peek-buffer
              (should (string-search "Session s" (buffer-string)))
              (should (string-search "Preview of s" (buffer-string)))))
        (when (get-buffer drover-peek-buffer)
          (kill-buffer drover-peek-buffer))))))

;;;; Lists

(ert-deftest drover-keys-test-toggle-subagents ()
  "U cycles every view through all, active and no subagents."
  (drover-keys-test-with nil
    (let ((runs 0)
          (said nil))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (cl-letf (((symbol-function 'current-message) (lambda () said))
                ((symbol-function 'message) (lambda (format &rest arguments)
                                              (setq said (apply #'format-message format arguments)))))
        (drover-keys-test-in 'view nil
          (should (eq drover-show-subagents 'active))
          (call-interactively #'drover-toggle-subagents)
          (should (eq drover-show-subagents 'none))
          (should (equal (current-message) "Hiding subagents"))
          (should (= runs 1))
          (with-temp-buffer
            ;; Every view follows.
            (should (eq drover-show-subagents 'none)))
          (call-interactively #'drover-toggle-subagents)
          (should (eq drover-show-subagents 'all))
          (should (equal (current-message) "Showing every subagent"))
          (call-interactively #'drover-toggle-subagents)
          (should (eq drover-show-subagents 'active))
          (should (equal (current-message) "Showing active subagents"))
          (should-not (local-variable-p 'drover-show-subagents))))
      (drover-keys-test-in 'list nil
        (let ((toggled 0))
          (setq-local drover-toggle-subagents-function (lambda () (interactive) (cl-incf toggled)))
          (call-interactively #'drover-toggle-subagents)
          (should (= toggled 1))
          (should (eq drover-show-subagents 'active))
          (should (= runs 3)))))))

(ert-deftest drover-keys-test-refresh ()
  (drover-keys-test-with (list (drover-test-session "s"))
    (let ((reverted nil)
          (session-buffer (generate-new-buffer " session")))
      (unwind-protect
          (progn
            (setf (alist-get 'fake drover--backends)
                  (append (list :buffer (lambda (_) session-buffer)) (alist-get 'fake drover--backends)))
            (with-current-buffer session-buffer
              (setq-local revert-buffer-function (lambda (&rest _) (push 'session reverted))))
            (drover-keys-test-in 'view "s"
              (setq-local revert-buffer-function (lambda (&rest _) (push 'list reverted)))
              (call-interactively #'drover-refresh))
            (drover-keys-test-in 'prompt "s"
              (setq-local revert-buffer-function (lambda (&rest _) (push 'prompt reverted)))
              (call-interactively #'drover-refresh))
            (with-current-buffer session-buffer
              (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s")))
              (call-interactively #'drover-refresh))
            ;; A prompt buffer refreshes its session's buffer.
            (should (equal (reverse reverted) '(list session session))))
        (kill-buffer session-buffer)))))

;;;; Burying quiet sessions

(defvar drover-keys-test-said nil
  "The messages shown, newest first.")

(defun drover-keys-test-buffer (id)
  "Return the buffer of the fake session ID, named after it between stars."
  (get-buffer (format "*%s*" id)))

(defmacro drover-keys-test-burying (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS, each with a buffer.
The buffer of a session is named after its identifier between stars
\(see `drover-keys-test-buffer'), and \"*work*\" and \"*other*\" are
two more buffers.  BODY runs in the one window of the frame, showing
\"*work*\", a frame large enough to split it a few times, its
messages going to `drover-keys-test-said'.  The buffers go afterwards,
and the windows and the size of the frame are restored."
  (declare (indent 1) (debug t))
  (let ((made (make-symbol "made"))
        (size (make-symbol "size")))
    `(drover-keys-test-with ,sessions
       (let ((,made nil)
             (,size (cons (frame-width) (frame-height)))
             (drover-keys-test-said nil)
             (split-height-threshold 0)
             (split-width-threshold nil))
         (unwind-protect
             (save-window-excursion
               (set-frame-size (selected-frame) 100 60)
               (dolist (name (append (mapcar (lambda (session) (format "*%s*" (drover-session-id session)))
                                             drover-test-sessions)
                                     '("*work*" "*other*")))
                 (push (get-buffer-create name) ,made))
               (setf (alist-get 'fake drover--backends)
                     (append (list :buffer (lambda (session) (drover-keys-test-buffer (drover-session-id session))))
                             (alist-get 'fake drover--backends)))
               (delete-other-windows)
               (switch-to-buffer "*work*")
               (cl-letf (((symbol-function 'message)
                          (lambda (format &rest arguments)
                            (push (apply #'format-message format arguments) drover-keys-test-said))))
                 ,@body))
           (set-frame-size (selected-frame) (car ,size) (cdr ,size))
           (dolist (buffer ,made)
             (when (buffer-live-p buffer)
               (kill-buffer buffer))))))))

(ert-deftest drover-keys-test-bury-hidden ()
  "Buffers no window shows go to the end of the buffer list, windows untouched.
Those of the sessions that need you stay where they are."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "working" :status 'working)
                                  (drover-test-session "retrying" :status 'retrying)
                                  (drover-test-session "waiting" :status 'waiting)
                                  (drover-test-session "failed" :status 'failed)
                                  (drover-test-session "done" :status 'done))
    (let ((attention (mapcar #'drover-keys-test-buffer '("waiting" "failed" "done")))
          (quiet (mapcar #'drover-keys-test-buffer '("idle" "working" "retrying")))
          (window (selected-window)))
      (dolist (buffer attention)
        (bury-buffer buffer))
      (should (equal (drover--quiet-buffers) quiet))
      (call-interactively #'drover-bury-quiet-sessions)
      (should (equal (last (buffer-list) 6) (append attention quiet)))
      (should (equal (window-list nil 'nomini) (list window)))
      (should (eq (window-buffer window) (get-buffer "*work*")))
      (should (equal drover-keys-test-said '("Buried 3 sessions (0 windows restored)"))))))

(ert-deftest drover-keys-test-bury-shown ()
  "A window showing a quiet session goes back to what it showed before.
A window made for it goes, one reused shows its previous buffer, and one
showing a session that needs you stays."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "working" :status 'working)
                                  (drover-test-session "waiting" :status 'waiting))
    (let* ((selected (selected-window))
           (made (display-buffer (drover-keys-test-buffer "idle") '(display-buffer-pop-up-window)))
           (reused (split-window selected))
           (asking (split-window selected)))
      (should (window-live-p made))
      (set-window-buffer asking (drover-keys-test-buffer "waiting"))
      (with-selected-window reused
        (switch-to-buffer "*other*")
        (display-buffer (drover-keys-test-buffer "working") '(display-buffer-same-window)))
      (should (eq (window-buffer reused) (drover-keys-test-buffer "working")))
      (call-interactively #'drover-bury-quiet-sessions)
      (should-not (window-live-p made))
      (should (eq (window-buffer reused) (get-buffer "*other*")))
      (should (eq (window-buffer asking) (drover-keys-test-buffer "waiting")))
      (should (eq (selected-window) selected))
      (should (eq (window-buffer selected) (get-buffer "*work*")))
      (should (equal drover-keys-test-said '("Buried 2 sessions (2 windows restored)"))))))

(ert-deftest drover-keys-test-bury-selected ()
  "The selected window is quit last, and stays selected.
A window quit while another is selected does not take the selection to
the window selected when it was made, nor does the selected window,
quit once that one is gone.  No window shows a quiet session after: one
that showed another before goes back further."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "working" :status 'working)
                                  (drover-test-session "retrying" :status 'retrying))
    (let* ((selected (selected-window))
           (other (split-window selected))
           (made nil))
      (with-selected-window other
        (switch-to-buffer "*other*")
        (display-buffer (drover-keys-test-buffer "working") '(display-buffer-same-window))
        (display-buffer (drover-keys-test-buffer "idle") '(display-buffer-same-window))
        (setq made (display-buffer (drover-keys-test-buffer "retrying") '(display-buffer-pop-up-window))))
      (should (eq (nth 2 (window-parameter made 'quit-restore)) other))
      (should (eq (window-buffer other) (drover-keys-test-buffer "idle")))
      ;; Shown in the selected window while the one made was selected.
      (with-selected-window made
        (display-buffer (drover-keys-test-buffer "working")
                        `(display-buffer-in-previous-window (previous-window . ,selected))))
      (should (eq (window-buffer selected) (drover-keys-test-buffer "working")))
      (should (memq made (mapcar (lambda (parameter) (nth 2 (window-parameter selected parameter)))
                                 '(quit-restore quit-restore-prev))))
      (call-interactively #'drover-bury-quiet-sessions)
      (should-not (window-live-p made))
      (should (eq (window-buffer other) (get-buffer "*other*")))
      (should (eq (selected-window) selected))
      (should (eq (window-buffer selected) (get-buffer "*work*")))
      (should (equal drover-keys-test-said '("Buried 3 sessions (3 windows restored)"))))))

(ert-deftest drover-keys-test-bury-spares-lists-and-prompt ()
  "Drover's lists and prompt stay, and a list shows no second time.
A backend telling the prompt shows one of its sessions does not get it
buried."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "prompted" :status 'idle))
    (let* ((list (selected-window))
           (prompt (split-window list))
           (session (split-window list))
           (list-buffer (get-buffer-create " drover list")))
      (unwind-protect
          (progn
            (with-current-buffer list-buffer
              (setq-local drover-view-p t))
            (with-current-buffer (drover-keys-test-buffer "prompted")
              (setq-local drover-prompt-mode t))
            (set-window-buffer list list-buffer)
            (set-window-buffer prompt (drover-keys-test-buffer "prompted"))
            ;; The session replaced the list there.
            (set-window-buffer session list-buffer)
            (set-window-prev-buffers session nil)
            (with-selected-window session
              (switch-to-buffer (drover-keys-test-buffer "idle")))
            (should (equal (drover--quiet-buffers) (list (drover-keys-test-buffer "idle"))))
            (bury-buffer (drover-keys-test-buffer "prompted"))
            (call-interactively #'drover-bury-quiet-sessions)
            (should (eq (window-buffer list) list-buffer))
            (should (eq (window-buffer prompt) (drover-keys-test-buffer "prompted")))
            (should (window-live-p session))
            (should-not (memq (window-buffer session) (list list-buffer (drover-keys-test-buffer "idle"))))
            (should (equal (last (buffer-list) 2)
                           (list (drover-keys-test-buffer "prompted") (drover-keys-test-buffer "idle"))))
            (should (equal drover-keys-test-said '("Buried 1 session (1 window restored)"))))
        (kill-buffer list-buffer)))))

(ert-deftest drover-keys-test-bury-last-window ()
  "The last window of a frame not made for the session stays, as the last frame.
It shows what it showed before, or another buffer, even dedicated; a
frame made for the session is deleted, but the last one."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle))
    (let ((window (selected-window))
          (idle (drover-keys-test-buffer "idle"))
          (deleted nil))
      ;; Shown in place of another buffer.
      (display-buffer idle '(display-buffer-same-window))
      (should (eq (window-buffer window) idle))
      (call-interactively #'drover-bury-quiet-sessions)
      (should (equal (window-list nil 'nomini) (list window)))
      (should (eq (window-buffer window) (get-buffer "*work*")))
      (cl-flet ((show ()
                  (set-window-dedicated-p window nil)
                  (set-window-buffer window idle)
                  (set-window-prev-buffers window nil)
                  (set-window-next-buffers window nil)))
        ;; Dedicated, without another buffer shown before, even when the
        ;; frame could go.
        (cl-letf (((symbol-function 'frame-deletable-p) (lambda (&optional _) t))
                  ((symbol-function 'delete-frame) (lambda (&optional frame _) (push frame deleted))))
          (show)
          (set-window-parameter window 'quit-restore nil)
          (set-window-dedicated-p window t)
          (call-interactively #'drover-bury-quiet-sessions)
          (should (equal (window-list nil 'nomini) (list window)))
          (should-not (eq (window-buffer window) idle))
          (should-not deleted)
          ;; A frame made for the session goes.
          (show)
          (set-window-parameter window 'quit-restore (list 'frame 'frame window idle))
          (call-interactively #'drover-bury-quiet-sessions)
          (should (equal deleted (list (selected-frame)))))
        ;; Not the last one.
        (show)
        (set-window-parameter window 'quit-restore (list 'frame 'frame window idle))
        (call-interactively #'drover-bury-quiet-sessions)
        (should (equal (window-list nil 'nomini) (list window)))
        (should-not (eq (window-buffer window) idle))))))

(ert-deftest drover-keys-test-bury-local-quiet-only ()
  "Only this host's sessions are buried, and none needing you.
Not a session of another host, nor a buffer a session needing you
shows too, nor one of a conversation another backend tells needs you.
B applies while a quiet session has a buffer."
  (drover-keys-test-burying (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "remote" :status 'idle :host "core")
                                  (drover-test-session "shared" :status 'idle)
                                  (drover-test-session "asking" :status 'waiting :parent "shared")
                                  (drover-test-session "terminal" :status 'idle :engine 'fake :engine-id "x"))
    (setf (alist-get 'fake drover--backends)
          (append (list :buffer (lambda (session)
                                  (drover-keys-test-buffer (if (equal (drover-session-id session) "asking")
                                                               "shared"
                                                             (drover-session-id session)))))
                  (alist-get 'fake drover--backends)))
    (drover-test-register-simple 'other (list (drover-session-create :backend 'other :id "o" :status 'waiting
                                                                     :engine 'fake :engine-id "x")))
    (should (equal (drover--quiet-buffers) (list (drover-keys-test-buffer "idle"))))
    (with-temp-buffer
      (should (member "B" (drover-keys-test-apt 'drover-dispatch))))
    (call-interactively #'drover-bury-quiet-sessions)
    (should (equal drover-keys-test-said '("Buried 1 session (0 windows restored)")))
    ;; Nothing quiet left with a buffer: B does not apply.
    (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "idle"))
                                           drover-test-sessions))
    (should-not (drover--quiet-buffers))
    (with-temp-buffer
      (should-not (member "B" (drover-keys-test-apt 'drover-dispatch))))
    (should-error (call-interactively #'drover-bury-quiet-sessions) :type 'user-error)))

(ert-deftest drover-keys-test-new-session ()
  (drover-keys-test-with (list (drover-test-session "s" :directory "/work/s/")
                               (drover-test-session "nowhere")
                               (drover-test-session "remote" :directory "/on/core/" :host "core"))
    (drover-keys-test-in 'view "s"
      (call-interactively #'drover-new-session))
    (drover-keys-test-in 'view "nowhere"
      (let ((default-directory "/work/list/"))
        (call-interactively #'drover-new-session)))
    (drover-keys-test-in 'view nil
      (let ((default-directory "/work/empty/"))
        (call-interactively #'drover-new-session)))
    ;; Another host's directory is not this host's to start in.
    (drover-keys-test-in 'view "remote"
      (let ((default-directory "/work/here/"))
        (call-interactively #'drover-new-session)))
    (should (equal (reverse drover-test-calls)
                   '((:new "/work/s/") (:new "/work/list/") (:new "/work/empty/") (:new "/work/here/"))))))

(ert-deftest drover-keys-test-new-session-backend-choice ()
  (drover-keys-test-with nil
    (drover-register-backend 'other :label "Other" :sessions #'ignore
                             :new (lambda (directory) (push (list :other directory) drover-test-calls)))
    (with-temp-buffer
      (let ((default-directory "/work/"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal (sort (mapcar #'car candidates) #'string<) '("Fake" "Other")))
                     "Other")))
          (call-interactively #'drover-new-session))))
    (should (equal drover-test-calls '((:other "/work/"))))
    (drover-unregister-backend 'other)
    (drover-unregister-backend 'fake)
    (with-temp-buffer
      (should-error (call-interactively #'drover-new-session) :type 'user-error))))

;;;; Following the session at point

(ert-deftest drover-keys-test-follow-mode ()
  (drover-keys-test-with (list (drover-test-session "a") (drover-test-session "b"))
    (save-window-excursion
      (delete-other-windows)
      (let ((opened nil))
        (setf (alist-get 'fake drover--backends)
              (append (list :open (lambda (session other-window)
                                    (push (list (drover-session-id session) other-window (selected-window))
                                          opened)))
                      (alist-get 'fake drover--backends)))
        (with-temp-buffer
          (insert "a\nb\n")
          (goto-char (point-min))
          (setq-local drover-view-p t
                      drover-session-at-point-function
                      (lambda () (drover-find 'fake (string (char-after (line-beginning-position))))))
          (switch-to-buffer (current-buffer))
          (let ((list-window (selected-window)))
            (drover-follow-mode)
            ;; Shown at once, in another window, the list staying selected.
            (should (= (length opened) 1))
            (pcase-let ((`(,id ,other-window ,window) (car opened)))
              (should (equal id "a"))
              (should-not other-window)
              (should-not (eq window list-window))
              (should (window-live-p window)))
            (should (eq (selected-window) list-window))
            ;; Moving within the same session shows nothing new.
            (run-hooks 'post-command-hook)
            (should (= (length opened) 1))
            (forward-line 1)
            (run-hooks 'post-command-hook)
            (should (equal (mapcar #'car opened) '("b" "a")))
            ;; The same window serves again.
            (should (eq (nth 2 (car opened)) (nth 2 (cadr opened))))
            (drover-follow-mode -1)
            (forward-line -1)
            (run-hooks 'post-command-hook)
            (should (= (length opened) 2))))))))

(ert-deftest drover-keys-test-follow-window-function ()
  (drover-keys-test-with (list (drover-test-session "a"))
    (save-window-excursion
      (delete-other-windows)
      (let* ((chosen (split-window))
             (opened nil))
        (setf (alist-get 'fake drover--backends)
              (append (list :open (lambda (_session _other-window) (push (selected-window) opened)))
                      (alist-get 'fake drover--backends)))
        (drover-keys-test-in 'view "a"
          (setq-local drover-main-window-function (lambda () chosen))
          (drover-follow-mode)
          (should (equal opened (list chosen)))
          (drover-follow-mode -1))))))

;;;; Prompt commands

(ert-deftest drover-keys-test-prompt-commands ()
  (drover-keys-test-with nil
    (drover-keys-test-in 'prompt nil
      (let ((commands '((drover-prompt-send . send) (drover-prompt-schedule . schedule)
                        (drover-prompt-clear . clear)
                        (drover-prompt-attach . attach) (drover-prompt-slash . slash)
                        (drover-prompt-mention . mention) (drover-prompt-revert . revert)
                        (drover-prompt-revert-cancel . revert-cancel)
                        (drover-prompt-copy-block . copy-block) (drover-prompt-agent . agent)
                        (drover-prompt-model . model) (drover-prompt-effort . effort)
                        (drover-prompt-directory . directory)
                        (drover-prompt-last-answer . last-answer)
                        (drover-prompt-open-link . open-link)
                        (drover-compact . compact) (drover-fork . fork))))
        (pcase-dolist (`(,command . ,_) commands)
          (call-interactively command))
        (should (equal (mapcar #'car (reverse drover-keys-test-actions)) (mapcar #'cdr commands)))
        (should (seq-every-p (lambda (entry) (eq (nth 1 entry) (current-buffer)))
                             drover-keys-test-actions))))
    (with-temp-buffer
      (should-error (call-interactively #'drover-prompt-send) :type 'user-error))))

;;;; The dispatch menu

(defconst drover-keys-test-dispatch-layout
  '(("Sessions" ("j" "B" "b" "l" "L" "p"))
    ("Prompt" ("\"" "n" "'" "#"))
    ("Prompt runs" ("a" "m" "e" "d"))
    ("Input" ("RET" "S-<return>" "i" "k") ("/" "@" "y"))
    ("Conversation" ("t" "T" "C" "f" "." "O"))
    ("Session" ("o" "u" "c" "v" "A" "K" "s") ("+" "g" "r" "x" "z" "Z"))
    ("View" ("= o" "= r" "= g" "U" "- o" "- z" "- f"))
    ("Filter" ("= p" "= s" "= a" "= m" "= v") ("= H" "= d" "= f" "= c"))
    ("Columns" ("- h" "- H" "- l" "- b") ("- a" "- m" "- r" "- x") ("- c" "- t" "- p" "- s")))
  "The groups of `drover-dispatch' outside lists, and the keys of their columns.")

(ert-deftest drover-keys-test-dispatch-layout ()
  "The menu has no Global group: each group is titled after what it does."
  (with-temp-buffer
    (let ((groups (drover-keys-test-menu 'drover-dispatch)))
      (should (equal (mapcar (lambda (group) (cons (car group) (mapcar (lambda (column) (mapcar #'car column))
                                                                       (cdr group))))
                             groups)
                     drover-keys-test-dispatch-layout))
      (should-not (assoc "Global" groups))
      ;; One key, one command.
      (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-dispatch))))
        (should (equal keys (delete-dups (copy-sequence keys))))))))

(defun drover-keys-test-layout-suffixes (layout)
  "Return the suffix specifications of LAYOUT, a transient layout.
The groups LAYOUT includes by name are searched too."
  (mapcan (lambda (child)
            (cond ((and child (symbolp child))
                   (drover-keys-test-layout-suffixes (transient--get-layout child)))
                  ((vectorp child) (drover-keys-test-layout-suffixes child))
                  ((consp child) (list child))))
          (aref layout 2)))

(defun drover-keys-test-defined-keys (prefix)
  "Return every entry PREFIX defines, as (KEY COMMAND . PROPERTIES).
Entries an :if may leave out are among them, and those other packages
added."
  (mapcar (lambda (spec)
            (let ((properties (cdr spec)))
              (cl-list* (key-description (kbd (plist-get properties :key)))
                        (plist-get properties :command)
                        properties)))
          (drover-keys-test-layout-suffixes (transient--get-layout prefix))))

(defun drover-keys-test-conflicts (prefix)
  "Return the keys of PREFIX that conflict, with their commands.
Two entries conflict when they share a key, but RET, Send or Open, never
both shown as their predicates are opposite, or when the key of one is
the prefix of the other's, which would hide it."
  (let* ((entries (drover-keys-test-defined-keys prefix))
         (conflicts nil))
    (dolist (key (delete-dups (mapcar #'car entries)))
      (let ((uses (seq-filter (lambda (entry) (equal (car entry) key)) entries)))
        (unless (or (null (cdr uses))
                    (and (equal key "RET")
                         (equal (mapcar #'cadr uses) '(drover-prompt-send drover-open-at-point))
                         (eq (plist-get (cddr (nth 0 uses)) :if-not) 'drover-dispatch-list-return-p)
                         (eq (plist-get (cddr (nth 1 uses)) :if) 'drover-dispatch-list-return-p)))
          (push (cons key (mapcar #'cadr uses)) conflicts))))
    (pcase-dolist (`(,key ,command . ,_) entries)
      (pcase-dolist (`(,other ,other-command . ,_) entries)
        (let ((a (kbd key)) (b (kbd other)))
          (when (and (< (length a) (length b)) (equal a (substring b 0 (length a))))
            (push (list key command other-command) conflicts)))))
    (nreverse conflicts)))

(ert-deftest drover-keys-test-dispatch-keys-unique ()
  "No key of the menu runs two commands, in any group, shown or not."
  (should-not (drover-keys-test-conflicts 'drover-dispatch))
  (should-not (drover-keys-test-conflicts 'drover-view))
  ;; The View menu's keys are the dispatch menu's, for the same commands.
  (let ((entries (drover-keys-test-defined-keys 'drover-dispatch)))
    (pcase-dolist (`(,key ,command . ,_) (drover-keys-test-defined-keys 'drover-view))
      (unless (eq command 'transient-quit-one)
        (should (equal (cadr (assoc key entries)) command)))))
  ;; A duplicate, or a key hiding another, is found.
  (unwind-protect
      (progn
        (transient-append-suffix 'drover-dispatch "p" '("=o" "Duplicate" ignore) t)
        (transient-append-suffix 'drover-dispatch "p" '("=" "Prefix" ignore-error) t)
        (let ((conflicts (drover-keys-test-conflicts 'drover-dispatch)))
          (should (member '("= o" ignore drover-view--sort-by) conflicts))
          (should (member '("=" ignore-error drover-view--sort-by) conflicts))))
    (transient-remove-suffix 'drover-dispatch 'ignore)
    (transient-remove-suffix 'drover-dispatch 'ignore-error)))

(ert-deftest drover-keys-test-dispatch-commands ()
  "Each key of the menu runs the command of the shared key tables."
  (dolist (entry '(("j" . drover-next) ("B" . drover-bury-quiet-sessions) ("b" . drover-select)
                   ("\"" . drover-prompt-dwim)
                   ("n" . drover-prompt-org-task) ("'" . drover-prompt-toggle) ("l" . drover-list)
                   ("L" . drover-list-side) ("p" . drover-select-project) ("a" . drover-prompt-agent)
                   ("m" . drover-prompt-model) ("e" . drover-prompt-effort)
                   ("d" . drover-prompt-directory) ("C" . drover-compact) ("f" . drover-fork)
                   ("." . drover-prompt-last-answer) ("O" . drover-prompt-open-link)
                   ("RET" . drover-prompt-send) ("S-<return>" . drover-prompt-schedule)
                   ("k" . drover-prompt-clear)
                   ("i" . drover-prompt-attach) ("/" . drover-prompt-slash)
                   ("@" . drover-prompt-mention) ("t" . drover-prompt-revert)
                   ("T" . drover-prompt-revert-cancel) ("y" . drover-prompt-copy-block)))
    (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch (car entry))) :command)
                (cdr entry))))
  ;; The view settings, the View menu's.
  (dolist (entry '(("=o" . drover-view--sort-by) ("=r" . drover-view--sort-order)
                   ("=g" . drover-view--group-by) ("U" . drover-toggle-subagents)
                   ("-o" . drover-view--history) ("-z" . drover-view--archived)
                   ("-f" . drover-view--follow) ("=p" . drover-view--filter-project)
                   ("=s" . drover-view--filter-statuses) ("=a" . drover-view--filter-agent)
                   ("=m" . drover-view--filter-model) ("=v" . drover-view--filter-provider)
                   ("=H" . drover-view--filter-harness) ("-H" . drover-view--column-harness)
                   ("=h" . drover-view--filter-host) ("=d" . drover-view--filter-since)
                   ("=f" . drover-view--filter-search) ("=c" . drover-filter-clear)
                   ("-l" . drover-view--column-location) ("-s" . drover-view--column-summary)))
    (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch (car entry))) :command)
                (cdr entry))))
  ;; Every session key runs the command `drover-session-map' binds it to,
  ;; but V, the View menu, whose settings the menu shows itself.
  (should-not (ignore-errors (transient-get-suffix 'drover-dispatch "V")))
  (map-keymap (lambda (event command)
                (unless (eq command 'drover-view)
                  (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch
                                                                   (key-description (vector event))))
                                         :command)
                              command))))
              drover-session-map))

(ert-deftest drover-keys-test-dispatch-elsewhere ()
  "Outside lists and prompt buffers, only the global commands apply.
With drover's prompt as the default prompt buffer, as by default, the
agent, model, effort and directory of the next prompt are among them."
  (drover-keys-test-with (list (drover-test-session "s"))
    (with-temp-buffer
      (should (equal (drover-keys-test-apt 'drover-dispatch) '("j" "b" "l" "L" "p" "\"" "'" "#")))
      (setq drover-default-prompt-buffer-function
            (eval (car (get 'drover-default-prompt-buffer-function 'standard-value)) t))
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "l" "L" "p" "\"" "'" "#" "a" "m" "e" "d"))))))

(ert-deftest drover-keys-test-dispatch-prompt-entries ()
  "The prompt's entries open it from anywhere, but from the prompt itself."
  (should (equal (cdr (assoc "Prompt" (drover-keys-test-keys 'drover-dispatch)))
                 '("\"" "n" "'" "#")))
  ;; Autoloaded: the menu opens without loading the prompt.
  (dolist (command '(drover-prompt-dwim drover-prompt-org-task drover-prompt-toggle))
    (should (commandp command))
    (should (or (featurep 'drover-prompt)
                (equal (cadr (symbol-function command)) "drover-prompt"))))
  (drover-keys-test-with nil
    (with-temp-buffer
      (setq-local drover-prompt-mode t)
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should-not (member "\"" apt))
        (should (member "'" apt))))))

(ert-deftest drover-keys-test-dispatch-task-prompt ()
  "n starts a task prompt from an Org buffer, but drover's prompt."
  (drover-keys-test-with nil
    (with-temp-buffer
      (should-not (member "n" (drover-keys-test-apt 'drover-dispatch)))
      (let ((org-mode-hook nil))
        (org-mode))
      (should (member "n" (drover-keys-test-apt 'drover-dispatch)))
      (setq-local drover-prompt-mode t)
      (should-not (member "n" (drover-keys-test-apt 'drover-dispatch))))))

(defconst drover-keys-test-view-keys
  '("= o" "= r" "= g" "U" "= p" "= s" "= a" "= m" "= v" "= H" "= d" "= f" "= c"
    "- h" "- H" "- l" "- b" "- a" "- m" "- r" "- x" "- c" "- t" "- p" "- s")
  "The keys of the view settings that apply in one of drover's views.
Older and archived sessions need a backend keeping them, and follow mode
a list in a window.")

(ert-deftest drover-keys-test-dispatch-in-view ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working :requests '((:kind question :summary "?"))
                                                    :parent "root")
                               (drover-test-session "root")
                               (drover-test-session "sub" :parent "s"))
    (drover-keys-test-in 'view "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     (append '("j" "b" "l" "L" "p" "\"" "'" "#" "C" "f"
                               "o" "u" "c" "v" "A" "K" "+" "g" "r" "x" "z")
                             drover-keys-test-view-keys
                             '("C-n" "C-p" "M-n" "M-p" "M-<" "M->" "RET")))))
    ;; Off a session, only what needs none.
    (drover-keys-test-in 'view nil
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     (append '("j" "b" "l" "L" "p" "\"" "'" "#" "+" "g")
                             drover-keys-test-view-keys
                             '("C-n" "C-p" "M-n" "M-p" "M-<" "M->"))))
      ;; Moving between groups needs groups.
      (setq drover-group-by nil)
      (should-not (member "M-n" (drover-keys-test-apt 'drover-dispatch))))))

(ert-deftest drover-keys-test-dispatch-session-state ()
  "Session commands apply when the session's state allows them."
  (drover-keys-test-with (list (drover-test-session "done" :status 'done)
                               (drover-test-session "idle" :status 'idle))
    (drover-keys-test-in 'view "done"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should (member "s" apt))
        (dolist (key '("A" "K" "u" "c"))
          (should-not (member key apt)))))
    (drover-keys-test-in 'view "idle"
      (should-not (member "s" (drover-keys-test-apt 'drover-dispatch))))
    ;; A backend lacking an operation leaves its key inapt.
    (setf (alist-get 'fake drover--backends)
          (append (list :rename nil :delete nil :fork nil :compact nil :preview nil :new nil)
                  (alist-get 'fake drover--backends)))
    (drover-keys-test-in 'view "done"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (dolist (key '("r" "x" "C" "f" "v" "+"))
          (should-not (member key apt)))
        (should (member "z" apt))))))

(ert-deftest drover-keys-test-dispatch-archive ()
  "z applies to a session not archived, by its backend or by drover; Z to one archived."
  (drover-keys-test-with (list (drover-test-session "s"))
    (drover-keys-test-in 'view "s"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should (member "z" apt))
        (should-not (member "Z" apt))))
    (drover-test-with-state
      (drover-test-register-simple 'shown (list (drover-session-create :backend 'shown :id "t"
                                                                       :engine 'claude-code :engine-id "c")
                                                (drover-session-create :backend 'shown :id "u")))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (setq-local drover-session-at-point-function (lambda () (drover-find 'shown "t")))
        ;; Its backend has no archive: drover archives it.
        (should (member "z" (drover-keys-test-apt 'drover-dispatch)))
        (drover-archive-session (drover-find 'shown "t"))
        (let ((apt (drover-keys-test-apt 'drover-dispatch)))
          (should-not (member "z" apt))
          (should (member "Z" apt)))
        ;; Of a conversation not known yet, it cannot.
        (setq-local drover-session-at-point-function (lambda () (drover-find 'shown "u")))
        (let ((apt (drover-keys-test-apt 'drover-dispatch)))
          (should-not (member "z" apt))
          (should-not (member "Z" apt)))))))

(ert-deftest drover-keys-test-dispatch-in-foreign-list ()
  "In a list drover does not draw, U toggles its subagents, if it does.
Drover's view settings apply only while one of drover's views shows."
  (drover-keys-test-with (list (drover-test-session "s"))
    (drover-keys-test-in 'list "s"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should (member "v" apt))
        (dolist (key '("U" "= o" "- l"))
          (should-not (member key apt))))
      (setq-local drover-toggle-subagents-function #'ignore)
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should (member "U" apt))
        (should-not (member "= o" apt)))
      ;; Its subagents are its own: drover's mode is not shown as its.
      (should-not (drover-keys-test-highlighted (drover-keys-test-value "U" 'drover-dispatch))))))

(ert-deftest drover-keys-test-dispatch-in-prompt ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working))
    (drover-keys-test-in 'prompt "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "l" "L" "p" "\"" "'" "#" "a" "m" "e" "d"
                       "RET" "S-<return>" "i" "k" "/" "@" "y" "t" "T" "C" "f" "." "O"
                       "o" "K" "+" "g" "r" "x" "z"))))))

(ert-deftest drover-keys-test-dispatch-prompt-from-list ()
  "In a list, the session's settings apply to its buffer, its prompt does not.
Agent, model, effort, directory and the last answer act on the buffer
of the session at point; the prompt keys work on what a prompt buffer
shows, so they apply only there."
  (drover-keys-test-with (list (drover-test-session "s"))
    (let ((session-buffer (generate-new-buffer " session")))
      (unwind-protect
          (progn
            (setf (alist-get 'fake drover--backends)
                  (append (list :buffer (lambda (_) session-buffer)) (alist-get 'fake drover--backends)))
            (with-current-buffer session-buffer
              (drover-prompt-define 'revert (drover-keys-test-action 'revert))
              (drover-prompt-define 'agent (drover-keys-test-action 'agent))
              (drover-prompt-define 'fork (drover-keys-test-action 'fork)))
            (drover-keys-test-in 'view "s"
              (let ((apt (drover-keys-test-apt 'drover-dispatch)))
                (dolist (key '("a" "f"))
                  (should (member key apt)))
                (dolist (key '("k" "t" "T" "m"))
                  (should-not (member key apt)))
                ;; RET opens the session at point: the list sends nothing.
                (should (equal (mapcar (lambda (suffix) (oref suffix command))
                                       (seq-filter (lambda (suffix) (and (equal (oref suffix key) "RET")
                                                                         (not (oref suffix inactive))))
                                                   (cdr (drover-keys-test-suffixes 'drover-dispatch))))
                               '(drover-open-at-point))))
              (should-error (call-interactively #'drover-prompt-revert) :type 'user-error)
              (should-not drover-keys-test-actions)
              (call-interactively #'drover-prompt-agent)
              (should (equal drover-keys-test-actions (list (list 'agent session-buffer))))
              ;; Forking in a list forks the whole session.
              (call-interactively #'drover-fork)
              (should (equal drover-test-calls '((:fork "s"))))))
        (kill-buffer session-buffer)))))

(ert-deftest drover-keys-test-dispatch-default-prompt-buffer ()
  (drover-keys-test-with nil
    (let ((default (generate-new-buffer " default")))
      (unwind-protect
          (progn
            (with-current-buffer default
              (dolist (action '(agent model effort directory send))
                (drover-prompt-define action (drover-keys-test-action action))))
            (setq drover-default-prompt-buffer-function (lambda () default))
            (with-temp-buffer
              (should (equal (drover-keys-test-apt 'drover-dispatch)
                             '("j" "b" "l" "L" "p" "\"" "'" "#" "a" "m" "e" "d")))
              (call-interactively #'drover-prompt-model)
              (should (equal drover-keys-test-actions (list (list 'model default)))))
            ;; Deciding what applies never calls the function, which may
            ;; create its buffer: opening the menu must not.
            (let ((calls 0))
              (setq drover-default-prompt-buffer-function (lambda () (cl-incf calls) default))
              (with-temp-buffer
                (should (member "a" (drover-keys-test-apt 'drover-dispatch)))
                (should (= calls 0)))))
        (kill-buffer default)))))

(ert-deftest drover-keys-test-menus-keep-other-windows ()
  "Menus open in a window of their own, closed by the key that opens them."
  (let (setups)
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (name &rest arguments) (push (cons name arguments) setups))))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (call-interactively #'drover-dispatch)
        (call-interactively #'drover-view)))
    (should (= (length setups) 2))
    (dolist (setup setups)
      (should (equal (plist-get (nthcdr 3 setup) :display-action) drover-menu-display-action))))
  (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch "C-'")) :command)
              'transient-quit-one))
  (should (eq (plist-get (cdr (transient-get-suffix 'drover-view "V")) :command)
              'transient-quit-one))
  ;; A bottom side window, such as the prompt's, stays shown with the menu.
  (save-window-excursion
    (delete-other-windows)
    (let* ((prompt (generate-new-buffer " prompt"))
           (menu (generate-new-buffer " menu")))
      (unwind-protect
          (progn
            (display-buffer prompt '((display-buffer-in-side-window) (side . bottom) (slot . 0)))
            (should (window-live-p (display-buffer menu drover-menu-display-action)))
            (should (get-buffer-window prompt)))
        (kill-buffer prompt)
        (kill-buffer menu)))))

(ert-deftest drover-keys-test-dispatch-extension-points ()
  "Others place their commands relative to drover's keys.
Ways to pick a session or prompt chain after ', the prompt's last,
before the scheduled prompts of #, or after b; other lists of sessions
go after p, more prompt commands after O."
  (unwind-protect
      (progn
        (transient-append-suffix 'drover-dispatch "'" '("%" "Percent" ignore))
        (transient-append-suffix 'drover-dispatch "b" '("&" "Pick" ignore))
        (transient-append-suffix 'drover-dispatch "p" '("!" "Lists" ignore))
        (transient-append-suffix 'drover-dispatch "O" '("P" "Other" ignore))
        (with-temp-buffer
          (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-dispatch t))))
            (cl-flet ((follows (before after)
                        (equal (cadr (member before keys)) after)))
              (should (follows "b" "&"))
              (should (follows "&" "l"))
              (should (follows "\"" "n"))
              (should (follows "n" "'"))
              (should (follows "'" "%"))
              (should (follows "%" "#"))
              (should (follows "p" "!"))
              (should (follows "O" "P"))))))
    (dolist (key '("%" "&" "!" "P"))
      (transient-remove-suffix 'drover-dispatch key))))

(ert-deftest drover-keys-test-dispatch-scope-computed-once ()
  "Setting up the menu asks for the session at point once."
  (drover-keys-test-with (list (drover-test-session "s"))
    (let ((asked 0))
      (with-temp-buffer
        (setq-local drover-view-p t
                    drover-session-at-point-function (lambda () (cl-incf asked) (drover-find 'fake "s")))
        (drover-keys-test-menu 'drover-dispatch)
        (should (= asked 1))))))

;;;; Laying out the menus

(defun drover-keys-test-block (&rest columns)
  "Return the layouts of a block of COLUMNS, each (WIDTH . ENTRIES).
Each column is titled \"T\" and holds ENTRIES lines WIDTH wide."
  (drover--menu-block-layouts
   (mapcar (lambda (column) (cons "T" (make-list (cdr column) (make-string (car column) ?x))))
           columns)))

(defun drover-keys-test-laid-out (blocks width)
  "Return the lines of BLOCKS laid out at most WIDTH wide, and their plan.
The value is (PLAN . LINES)."
  (let ((plan (drover--menu-plan blocks width)))
    (cons plan (with-temp-buffer
                 (drover--menu-insert blocks plan)
                 (split-string (string-remove-suffix "\n" (buffer-string)) "\n")))))

(ert-deftest drover-keys-test-menu-layout ()
  "Groups fill columns of balanced height, in as few lines as the width allows."
  (let ((small (drover-keys-test-block '(10 . 5)))
        (split (drover-keys-test-block '(10 . 5) '(10 . 5))))
    ;; Side by side when wide enough.
    (should (equal (car (drover-keys-test-laid-out (list small small small) 100)) '(((0) (0) (0)))))
    ;; Under each other, in one column, when not: as low as three rows,
    ;; and fewer.
    (should (equal (car (drover-keys-test-laid-out (list small small small) 10)) '(((0 0 0)))))
    ;; Two columns of balanced height: 13 lines.
    (pcase-let ((`(,plan . ,lines) (drover-keys-test-laid-out (list small small small) 22)))
      (should (= (length plan) 1))
      (should (= (length (car plan)) 2))
      (should (= (length lines) 13)))
    ;; A column continued shows beside its first, lower, or under it,
    ;; narrower, as the room asks.
    (should (equal (car (drover-keys-test-laid-out (list split) 100)) '(((0)))))
    (should (equal (car (drover-keys-test-laid-out (list split) 15)) '(((1)))))
    ;; Stacked where it makes no column higher.
    (let ((tall (drover-keys-test-block '(10 . 12))))
      (should (equal (car (drover-keys-test-laid-out (list tall split) 100)) '(((0) (1))))))
    ;; Wider than the window, a group still shows, alone in its row.
    (let ((wide (drover-keys-test-block '(60 . 2))))
      (pcase-let ((`(,plan . ,lines) (drover-keys-test-laid-out (list small wide small) 40)))
        (should (equal plan '(((0)) ((0)) ((0)))))
        ;; Each row its lines, an empty line between.
        (should (= (length lines) (+ 6 1 3 1 6)))))
    ;; Rows of columns, when one row would be higher: a wide group
    ;; leaves no room beside it for the columns after it.
    (let ((wide (drover-keys-test-block '(30 . 2))))
      (should (equal (car (drover-keys-test-laid-out (list small small wide small small) 30))
                     '(((0) (0)) ((0)) ((0) (0))))))
    (should-not (drover--menu-plan nil 80))))

(ert-deftest drover-keys-test-menu-insert ()
  "Columns are aligned, titles first, no line padded at its end."
  (let ((a (drover--menu-block-layouts '(("A" " a1" " a2"))))
        (b (drover--menu-block-layouts '(("B" " b1"))))
        (c (drover--menu-block-layouts '(("C" " c1") ("" " c2")))))
    (should (equal (cdr (drover-keys-test-laid-out (list a b) 100)) '("A    B" " a1   b1" " a2")))
    ;; An empty line parts groups under each other: B's last, A's a2.
    (should (equal (cdr (drover-keys-test-laid-out (list a b c) 9))
                   '("A    B" " a1   b1" " a2" "     C" "      c1" "      c2")))
    (should (equal (cdr (drover-keys-test-laid-out (list c) 100)) '("C" " c1   c2")))))

;;;; Drawing the menus

(defun drover-keys-test-render (prefix)
  "Return the lines of PREFIX's suffixes as its menu would draw them here."
  (let* ((setup (drover-keys-test-suffixes prefix))
         (transient--prefix (car setup))
         (transient--suffixes (cdr setup))
         (transient--predicate-map (transient--make-predicate-map)))
    (mapcar (lambda (suffix)
              (let ((transient--pending-group (oref suffix parent)))
                (substring-no-properties (transient-format suffix))))
            (seq-filter (lambda (suffix)
                          (and (drover-keys-test--own-p suffix) (not (oref suffix inactive))))
                        transient--suffixes))))

(ert-deftest drover-keys-test-menus-draw ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (drover-keys-test-with (list (drover-test-session "s" :project "drover"))
          (set-time-zone-rule "UTC")
          (drover-keys-test-in 'view "s"
            (let ((dispatch (drover-keys-test-render 'drover-dispatch)))
              (should (member " j Next" dispatch))
              (should (member " l List" dispatch))
              (should (member " =o sort urgency" dispatch))
              (should (member " k Clear" dispatch))
              ;; In a list, RET opens the session rather than send.
              (should (member " RET Open" dispatch))
              (should-not (member " RET Send" dispatch))
              (should (member " C-n Next session" dispatch))
              (should (member " S-<return> Schedule" dispatch)))
            ;; The order of the layout is checked elsewhere.
            (should (seq-set-equal-p (drover-keys-test-render 'drover-view)
                                     '(" =o sort urgency" " =r order [ascending|descending]" " =g group status"
                                       " =p project any" " =s status any" " =a agent any"
                                       " =m model any" " =v provider any" " =H harness any"
                                       " =d updated any"
                                       " =f text any" " =c clear"
                                       " -f follow [off|on]" " U subagents [all|active|none]"
                                       " -o older [hidden|shown]"
                                       " -z archived [hidden|shown]"
                                       " -h host" " -H harness" " -l location" " -b branch" " -a agent"
                                       " -m model" " -r provider"
                                       " -x context" " -c created"
                                       " -t age" " -p cache" " -s summary")))
            (setq drover-filter (list :project "drover" :statuses '(waiting done) :agent "build"
                                      :provider "Anthropic" :harness 'fake :since (- (float-time) 7200))
                  drover-sort-by 'updated
                  drover-sort-descending t
                  drover-group-by nil)
            (let ((view (drover-keys-test-render 'drover-view)))
              (dolist (line '(" =o sort last activity" " =g group nothing" " =p project drover"
                              " =s status needs you or done" " =a agent build" " =v provider Anthropic"
                              " =H harness Fake"
                              " =d updated since 2h"
                              " =f text any"))
                (should (member line view)))
              ;; The dispatch menu shows the same values on the same keys.
              (dolist (line view)
                (should (member line (drover-keys-test-render 'drover-dispatch)))))))
      (set-time-zone-rule zone))))

;;;; The View menu

(defun drover-keys-test-setting (key &optional menu)
  "Return the infix on KEY of MENU, `drover-view' by default, set up here.
The value is (PREFIX-OBJECT . SUFFIX), for `drover-keys-test-choose'."
  (let ((setup (drover-keys-test-suffixes (or menu 'drover-view))))
    (cons (car setup) (seq-find (lambda (suffix) (equal (oref suffix key) key)) (cdr setup)))))

(defun drover-keys-test-choose (key &optional menu)
  "Change the setting of KEY in MENU, `drover-view' by default, as pressing KEY would."
  (pcase-let* ((`(,prefix . ,infix) (drover-keys-test-setting key menu))
               (transient--prefix prefix))
    (cl-letf (((symbol-function 'transient--show) #'ignore))
      (transient-infix-set infix (transient-infix-read infix)))))

(defun drover-keys-test-value (key &optional menu)
  "Return the value the infix of KEY in MENU shows, with its faces.
MENU is `drover-view' by default."
  (pcase-let* ((`(,prefix . ,infix) (drover-keys-test-setting key menu))
               (transient--prefix prefix))
    (transient-format-value infix)))

(defun drover-keys-test-highlighted (string)
  "Return the parts of STRING in the face `transient-value'."
  (let ((parts nil)
        (position 0))
    (while (setq position (text-property-any position (length string) 'face 'transient-value string))
      (let ((end (or (next-single-property-change position 'face string) (length string))))
        (push (substring-no-properties string position end) parts)
        (setq position end)))
    (nreverse parts)))

(ert-deftest drover-keys-test-view-layout ()
  (drover-keys-test-with nil
    (with-temp-buffer
      (setq-local drover-view-p t)
      (should (equal (drover-keys-test-keys 'drover-view)
                     '(("View" "= o" "= r" "= g" "U" "- o" "- z" "- f")
                       ("Filter" "= p" "= s" "= a" "= m" "= v" "= H" "= d" "= f" "= c")
                       ("Columns" "- h" "- H" "- l" "- b" "- a" "- m" "- r" "- x" "- c" "- t" "- p" "- s"))))
      ;; Hosts are filtered once there are others.
      (let ((drover-hosts-function (lambda () '("core"))))
        (should (member "= h" (cdr (assoc "Filter" (drover-keys-test-keys 'drover-view))))))
      ;; One key, one setting.
      (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-view t))))
        (should (equal keys (delete-dups (copy-sequence keys)))))
      ;; Older sessions need a backend keeping them.
      (should-not (member "- o" (drover-keys-test-apt 'drover-view)))
      (setf (alist-get 'fake drover--backends)
            (append (list :history #'ignore) (alist-get 'fake drover--backends)))
      (should (member "- o" (drover-keys-test-apt 'drover-view)))
      ;; Archived sessions too, or drover's archive holding some.
      (should-not (member "- z" (drover-keys-test-apt 'drover-view)))
      (setf (alist-get 'fake drover--backends)
            (append (list :archived #'ignore) (alist-get 'fake drover--backends)))
      (should (member "- z" (drover-keys-test-apt 'drover-view))))))

(ert-deftest drover-keys-test-view-only-in-views ()
  (with-temp-buffer
    (should-error (drover-view) :type 'user-error)
    (drover-session-keys-mode)
    (should-error (drover-view) :type 'user-error)))

(ert-deftest drover-keys-test-view-defaults ()
  "The View menu shows the default view: no groups, last activity first."
  (drover-keys-test-with nil
    (let ((drover-group-by (eval (car (get 'drover-group-by 'standard-value)) t))
          (drover-sort-by (eval (car (get 'drover-sort-by 'standard-value)) t))
          (drover-sort-descending (eval (car (get 'drover-sort-descending 'standard-value)) t)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (let ((view (drover-keys-test-render 'drover-view)))
          (dolist (line '(" =o sort last activity" " =r order [ascending|descending]" " =g group nothing"))
            (should (member line view))))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=o")) '("last activity")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=r")) '("descending")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=g")) '("nothing")))))))

(ert-deftest drover-keys-test-view-sort-and-group ()
  (drover-keys-test-with nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=o")) '("urgency")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt candidates &rest _)
                     (should (equal prompt "Sort by (default urgency): "))
                     (should (equal candidates (mapcar #'cdr drover-sort-labels)))
                     "last activity")))
          (drover-keys-test-choose "=o"))
        (should (eq drover-sort-by 'updated))
        (should (= runs 1))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=o")) '("last activity")))
        ;; The order shows both values, the current one highlighted.
        (should (equal (substring-no-properties (drover-keys-test-value "=r")) "[ascending|descending]"))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=r")) '("ascending")))
        (drover-keys-test-choose "=r")
        (should drover-sort-descending)
        (should (eq drover-sort-by 'updated))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=r")) '("descending")))
        (drover-keys-test-choose "=r")
        (should-not drover-sort-descending)
        ;; Grouping by nothing is a grouping too.
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=g")) '("status")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (member "last activity" candidates))
                     "nothing")))
          (drover-keys-test-choose "=g"))
        (should-not drover-group-by)
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=g")) '("nothing")))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "branch")))
          (drover-keys-test-choose "=g"))
        (should (eq drover-group-by 'branch))
        (should (= runs 5))))))

(ert-deftest drover-keys-test-view-filters ()
  (drover-keys-test-with (list (drover-test-session "a" :project "drover" :agent "build" :model "opus")
                               (drover-test-session "b" :project "opencode" :agent "plan")
                               (drover-test-session "c"))
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (should (equal (drover-keys-test-value "=p") "any"))
        (should (eq (get-text-property 0 'face (drover-keys-test-value "=p")) 'transient-inactive-value))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     ;; The projects of the sessions.
                     (should (equal candidates '("drover" "opencode")))
                     "drover")))
          (drover-keys-test-choose "=p"))
        (should (equal drover-filter '(:project "drover")))
        (should (= runs 1))
        (should (equal (drover-keys-test-value "=p") "drover"))
        (should (eq (get-text-property 0 'face (drover-keys-test-value "=p")) 'transient-value))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt candidates &rest _)
                     (should (equal (mapcar #'cdr candidates) drover-status-order))
                     '("Needs you" "Done"))))
          (drover-keys-test-choose "=s"))
        (should (equal (plist-get drover-filter :statuses) '(waiting done)))
        (should (equal (drover-keys-test-value "=s") "needs you or done"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal candidates '("build" "plan")))
                     "plan")))
          (drover-keys-test-choose "=a"))
        (should (equal (plist-get drover-filter :agent) "plan"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal candidates '("opus")))
                     "opus")))
          (drover-keys-test-choose "=m"))
        (should (equal (drover-keys-test-value "=m") "opus"))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "3d")))
          (drover-keys-test-choose "=d"))
        (should (< (abs (- (plist-get drover-filter :since) (- (float-time) (* 3 86400)))) 60))
        (cl-letf (((symbol-function 'read-string) (lambda (_prompt initial &rest _)
                                                    (should-not initial)
                                                    "auth")))
          (drover-keys-test-choose "=f"))
        (should (equal (plist-get drover-filter :search) "auth"))
        (should (equal (drover-keys-test-value "=f") "\"auth\""))
        ;; An empty answer removes the filter.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
          (drover-keys-test-choose "=p"))
        (should-not (plist-member drover-filter :project))
        (should (equal (drover-keys-test-value "=p") "any"))
        (drover-filter-clear)
        (should-not drover-filter)
        (should (= runs 8))))))

(ert-deftest drover-keys-test-view-provider ()
  "The View menu sorts, groups and filters sessions by their provider's name."
  (drover-keys-test-with (list (drover-test-session "a" :provider "A" :provider-name "Anthropic")
                               (drover-test-session "b" :provider-name "OpenAI")
                               (drover-test-session "c" :provider "A" :provider-name "Anthropic")
                               (drover-test-session "d"))
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (member "provider" candidates))
                     "provider")))
          (drover-keys-test-choose "=o")
          (should (eq drover-sort-by 'provider))
          (setq drover-group-by nil)
          (should (equal (mapcar #'car (drover-test-tree-shape (cdar (drover-view-groups))))
                         '("a" "c" "b" "d")))
          (drover-keys-test-choose "=g"))
        (should (eq drover-group-by 'provider))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=o")) '("provider")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=g")) '("provider")))
        (should (equal (mapcar (lambda (group) (cons (cdar group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                               (drover-view-groups))
                       '(("Anthropic" "a" "c") ("OpenAI" "b") (nil "d"))))
        ;; The providers of the sessions, by name.
        (should (equal (drover-keys-test-value "=v") "any"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt candidates &rest _)
                     (should (equal prompt "Provider (empty for any): "))
                     (should (equal candidates '("Anthropic" "OpenAI")))
                     "OpenAI")))
          (drover-keys-test-choose "=v"))
        (should (equal drover-filter '(:provider "OpenAI")))
        (should (equal (drover-keys-test-value "=v") "OpenAI"))
        (should (equal (mapcar #'car (drover-test-tree-shape (drover-view-trees))) '("b")))
        (should (equal (drover-filter-description) "provider OpenAI"))
        (should (= runs 3))))))

(ert-deftest drover-keys-test-view-harness ()
  "The View menu sorts, groups and filters sessions by their harness.
A session of another host is of its owner's harness, the proxy of the
host standing for none."
  (drover-keys-test-with (list (drover-test-session "f"))
    (drover-test-register-simple 'claude-code-ide (list (drover-session-create :backend 'claude-code-ide :id "i")))
    (drover-test-register-simple 'agent-shell (list (drover-session-create :backend 'agent-shell :id "s")))
    (drover-test-register-simple 'remote:core (list (drover-session-create :backend 'remote:core :id "r"
                                                                           :host "core" :origin 'opencode)))
    (drover-changed-now)
    (with-temp-buffer
      (setq-local drover-view-p t)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (should (member "harness" candidates))
                   "harness")))
        (drover-keys-test-choose "=o")
        (should (eq drover-sort-by 'harness))
        (setq drover-group-by nil
              drover-sort-descending nil)
        ;; By name: agent-shell, Claude IDE, Fake, OpenCode.
        (should (equal (mapcar #'car (drover-test-tree-shape (cdar (drover-view-groups))))
                       '("s" "i" "f" "r")))
        (drover-keys-test-choose "=g"))
      (should (eq drover-group-by 'harness))
      (should (equal (drover-keys-test-highlighted (drover-keys-test-value "=g")) '("harness")))
      (should (equal (sort (mapcar (lambda (group) (cons (cdar group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                                   (drover-view-groups))
                           (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))
                     '((agent-shell "s") (claude-code-ide "i") (fake "f") (opencode "r"))))
      (should (equal (drover-group-label 'harness 'claude-code-ide) "Claude IDE"))
      ;; The harnesses of the sessions, by name.
      (should (equal (drover-keys-test-value "=H") "any"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt candidates &rest _)
                   (should (equal prompt "Harness (empty for any): "))
                   (should (equal candidates '("agent-shell" "Claude IDE" "Fake" "OpenCode")))
                   "OpenCode")))
        (drover-keys-test-choose "=H"))
      (should (equal drover-filter '(:harness opencode)))
      (should (equal (drover-keys-test-value "=H") "OpenCode"))
      (should (equal (mapcar #'car (drover-test-tree-shape (drover-view-trees))) '("r")))
      (should (equal (drover-filter-description) "harness OpenCode")))))

(ert-deftest drover-keys-test-view-toggles ()
  (drover-keys-test-with (list (drover-test-session "a"))
    (let ((fetched 0))
      (setf (alist-get 'fake drover--backends)
            (append (list :history (lambda (_) (cl-incf fetched))) (alist-get 'fake drover--backends)))
      (save-window-excursion
        (delete-other-windows)
        (drover-keys-test-in 'view "a"
          (switch-to-buffer (current-buffer))
          ;; Three values, the current one highlighted, cycled in turn.
          (should (equal (substring-no-properties (drover-keys-test-value "U")) "[all|active|none]"))
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "U")) '("active")))
          (drover-keys-test-choose "U")
          (should (eq (default-value 'drover-show-subagents) 'none))
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "U")) '("none")))
          (drover-keys-test-choose "U")
          (should (eq drover-show-subagents 'all))
          (drover-keys-test-choose "U")
          (should (eq drover-show-subagents 'active))
          (drover-keys-test-choose "-o")
          (should drover-show-history)
          (should (= fetched 1))
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "-o")) '("shown")))
          (drover-keys-test-choose "-o")
          (should-not drover-show-history)
          ;; Follow mode is the list's own.
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "-f")) '("off")))
          (drover-keys-test-choose "-f")
          (should drover-follow-mode)
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "-f")) '("on")))
          (should (equal (car drover-test-calls) '(:open "a" nil)))
          (drover-keys-test-choose "-f")
          (should-not drover-follow-mode))))))

(ert-deftest drover-keys-test-view-columns ()
  "Each field of the list but the title has a switch, applied at once."
  (drover-keys-test-with nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        ;; Shown as Magit shows a switch: the name, highlighted when on.
        (should (equal (drover-keys-test-value "-l") "location"))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "-l")) '("location")))
        (drover-keys-test-choose "-l")
        (should (equal drover-list-hidden-fields '(location)))
        (should (= runs 1))
        (should-not (drover-keys-test-highlighted (drover-keys-test-value "-l")))
        (should (eq (get-text-property 0 'face (drover-keys-test-value "-l")) 'transient-inactive-value))
        (drover-keys-test-choose "-p")
        (should (equal drover-list-hidden-fields '(cache location)))
        (drover-keys-test-choose "-l")
        (should (equal drover-list-hidden-fields '(cache)))
        (should (= runs 3))
        ;; The title has none.
        (should-error (drover-set-list-field 'title nil))
        ;; A field the list does not have has no switch.
        (dlet ((drover-list-fields '((title :priority 0) (agent :priority 1))))
          (should (equal (cdr (assoc "Columns" (drover-keys-test-keys 'drover-view))) '("- a"))))))))

(ert-deftest drover-keys-test-view-stays-open ()
  "Every setting of the View menu leaves it open, and its values redrawn."
  (let ((prefix (get 'drover-view 'transient--prefix)))
    (should (oref prefix refresh-suffixes)))
  (with-temp-buffer
    (setq-local drover-view-p t)
    (let ((suffixes (seq-filter #'drover-keys-test--own-p (cdr (drover-keys-test-suffixes 'drover-view)))))
      (should (= (length suffixes) 29))
      (dolist (suffix suffixes)
        (should (oref suffix transient))))))

(ert-deftest drover-keys-test-parse-since ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let ((now drover-test-now))
          (set-time-zone-rule "UTC")
          (should-not (drover--parse-since "" now))
          (should-not (drover--parse-since "  " now))
          ;; drover-test-now is 08:00 UTC.
          (should (= (drover--parse-since "today" now) (- now (* 8 3600))))
          (should (= (drover--parse-since "Yesterday" now) (- now (* 32 3600))))
          (should (= (drover--parse-since "24h" now) (- now 86400)))
          (should (= (drover--parse-since "7 days" now) (- now (* 7 86400))))
          (should (= (drover--parse-since "30 days" now) (- now (* 30 86400))))
          (should (= (drover--parse-since "3d" now) (- now (* 3 86400))))
          (should (= (drover--parse-since "45m" now) (- now (* 45 60))))
          (should (= (drover--parse-since "2 weeks" now) (- now (* 14 86400))))
          (should (= (drover--parse-since "1.5h" now) (- now 5400)))
          (should (= (drover--parse-since "2027-01-14" now) (- now (* 32 3600))))
          (should-error (drover--parse-since "soon" now) :type 'user-error)
          (should-error (drover--parse-since "3x" now) :type 'user-error))
      (set-time-zone-rule zone))))

(ert-deftest drover-keys-test-filter-description ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let ((now drover-test-now)
              (drover-filter nil))
          (set-time-zone-rule "UTC")
          (should-not (drover-filter-description nil now))
          (should (equal (drover-filter-description
                          (list :project "drover" :statuses '(waiting) :agent "build" :model "opus"
                                :since (- now (* 2 86400)) :search "auth")
                          now)
                         "project drover, needs you, agent build, model opus, since 2d, \"auth\""))
          (should (equal (drover-filter-description (list :statuses '(done failed)) now) "done or failed"))
          (should (equal (drover-filter-description (list :since (- now (* 8 3600))) now) "today"))
          (setq drover-filter '(:search "x"))
          (should (equal (drover-filter-description nil now) "\"x\"")))
      (set-time-zone-rule zone))))

;;;; Projects

(ert-deftest drover-keys-test-select-project ()
  (drover-keys-test-with (list (drover-test-session "a" :project "drover")
                               (drover-test-session "b" :project "opencode")
                               (drover-test-session "c" :project "drover"))
    (let ((listed 0))
      (cl-letf (((symbol-function 'drover-list) (lambda () (cl-incf listed)))
                ((symbol-function 'completing-read)
                 (lambda (prompt candidates &rest _)
                   (should (equal prompt "Project: "))
                   (should (equal candidates '("drover" "opencode")))
                   "opencode")))
        (call-interactively #'drover-select-project))
      (should (equal drover-filter '(:project "opencode")))
      (should (= listed 1))
      (should (member '(:start nil) drover-test-calls))
      (setq drover-test-sessions (list (drover-test-session "x")))
      (should-error (call-interactively #'drover-select-project) :type 'user-error))))

(provide 'drover-keys-test)
;;; drover-keys-test.el ends here
