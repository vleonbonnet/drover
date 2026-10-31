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
(require 'ol)

(defmacro drover-keys-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS and the keys' state isolated."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let (;; Readable glyphs, whatever the defaults.
           (drover-status-glyphs '((waiting . "⚑") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·")))
           (drover-default-prompt-buffer-function nil)
           (drover-store-link-functions nil)
           (drover-keys-test-actions nil))
       ,@body)))

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
       ('prompt (dolist (action '(send schedule attach slash mention revert revert-cancel copy-block
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
only drover's commands unless ALL is non-nil."
  (let* ((transient--prefix (transient--init-prefix
                             prefix (and (eq prefix 'drover-dispatch) (list :scope (drover--context)))))
         (layout (transient--init-suffixes prefix)))
    (cl-labels ((entries (group)
                  (mapcan (lambda (suffix)
                            (and (or all (drover-keys-test--own-p suffix))
                                 (list (cons (key-description (kbd (oref suffix key))) (oref suffix inapt)))))
                          (oref group suffixes))))
      (delq nil
            (mapcar (lambda (group)
                      (when-let* (((cl-typep group 'transient-group))
                                  (title (and (slot-boundp group 'description) (oref group description)))
                                  ((stringp title)))
                        (cons title
                              (if (seq-every-p (lambda (child) (cl-typep child 'transient-group))
                                               (oref group suffixes))
                                  (mapcar #'entries (oref group suffixes))
                                (list (entries group))))))
                    layout)))))

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
                            ("k" . drover-interrupt) ("s" . drover-mark-seen)
                            ("u" . drover-open-parent) ("c" . drover-open-subagent)
                            ("r" . drover-rename) ("x" . drover-delete) ("z" . drover-archive)
                            ("g" . drover-refresh) ("l" . drover-store-link)
                            ("+" . drover-new-session) ("v" . drover-peek)
                            ("V" . drover-follow-mode) ("U" . drover-toggle-subagents)
                            ("G" . drover-group) ("F" . drover-filter)))
                         (lambda (a b) (string< (car a) (car b))))))
    (dolist (binding bindings)
      (should (commandp (cdr binding))))))

(ert-deftest drover-keys-test-session-keys-mode ()
  (with-temp-buffer
    (should-not (eq (key-binding (kbd "r")) #'drover-rename))
    (drover-session-keys-mode)
    (should (eq (key-binding (kbd "r")) #'drover-rename))
    (should (eq (key-binding (kbd "F")) #'drover-filter))
    (drover-session-keys-mode -1)
    (should-not (eq (key-binding (kbd "r")) #'drover-rename))))

;;;; Opening sessions

(ert-deftest drover-keys-test-open ()
  (drover-keys-test-with (list (drover-test-session "s"))
    (drover-keys-test-in 'view "s"
      (call-interactively #'drover-open-other-window)
      (call-interactively #'drover-open-at-point)
      (let ((current-prefix-arg '(4)))
        (call-interactively #'drover-open-at-point))
      (should (equal (reverse drover-test-calls) '((:open "s" t) (:open "s" nil) (:open "s" (4))))))))

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
  (drover-keys-test-with nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-keys-test-in 'view nil
        (call-interactively #'drover-toggle-subagents)
        (should drover-show-children)
        (should (= runs 1))
        (with-temp-buffer
          ;; Every view follows.
          (should drover-show-children))
        (call-interactively #'drover-toggle-subagents)
        (should-not drover-show-children))
      (drover-keys-test-in 'list nil
        (let ((toggled 0))
          (setq-local drover-toggle-subagents-function (lambda () (interactive) (cl-incf toggled)))
          (call-interactively #'drover-toggle-subagents)
          (should (= toggled 1))
          (should-not drover-show-children)
          (should (= runs 2)))))))

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

(ert-deftest drover-keys-test-new-session ()
  (drover-keys-test-with (list (drover-test-session "s" :directory "/work/s/")
                               (drover-test-session "nowhere"))
    (drover-keys-test-in 'view "s"
      (call-interactively #'drover-new-session))
    (drover-keys-test-in 'view "nowhere"
      (let ((default-directory "/work/list/"))
        (call-interactively #'drover-new-session)))
    (drover-keys-test-in 'view nil
      (let ((default-directory "/work/empty/"))
        (call-interactively #'drover-new-session)))
    (should (equal (reverse drover-test-calls)
                   '((:new "/work/s/") (:new "/work/list/") (:new "/work/empty/"))))))

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

(ert-deftest drover-keys-test-store-link ()
  (drover-keys-test-with (list (drover-test-session "s"))
    (let ((org-stored-links nil))
      (add-hook 'drover-store-link-functions (lambda (_) nil))
      (add-hook 'drover-store-link-functions
                (lambda (session) (list (concat "drover:" (drover-session-id session)) "The session"))
                t)
      (drover-keys-test-in 'view "s"
        (call-interactively #'drover-store-link))
      (should (equal org-stored-links '(("drover:s" "The session")))))))

(ert-deftest drover-keys-test-store-link-from-buffer ()
  (drover-keys-test-with (list (drover-test-session "s"))
    (let ((session-buffer (generate-new-buffer " session"))
          (stored nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-store-link)
                     (lambda (arg interactive)
                       (should-not arg)
                       (should interactive)
                       (push (current-buffer) stored))))
            (drover-keys-test-in 'view "s"
              (should-error (call-interactively #'drover-store-link) :type 'user-error)
              (setf (alist-get 'fake drover--backends)
                    (append (list :buffer (lambda (_) session-buffer)) (alist-get 'fake drover--backends)))
              (call-interactively #'drover-store-link))
            (should (equal stored (list session-buffer))))
        (kill-buffer session-buffer)))))

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

(ert-deftest drover-keys-test-dispatch-layout ()
  (with-temp-buffer
    (let ((groups (drover-keys-test-menu 'drover-dispatch)))
      (should (equal (mapcar #'car groups) '("Global" "Session" "Prompt")))
      (should (equal (mapcar (lambda (group) (mapcar (lambda (column) (mapcar #'car column)) (cdr group)))
                             groups)
                     '((("j" "b") ("3" "4" "5") ("a" "m" "e" "d") ("C" "f" "." "O"))
                       (("o" "u" "c" "v" "V") ("A" "k" "s" "+" "g") ("r" "x" "z" "l") ("U" "G" "F"))
                       (("RET" "S-<return>" "i") ("/" "@" "y") ("t" "T")))))
      ;; One key, one command.
      (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-dispatch))))
        (should (equal keys (delete-dups (copy-sequence keys))))))))

(ert-deftest drover-keys-test-dispatch-commands ()
  "Each key of the menu runs the command of the shared key tables."
  (dolist (entry '(("j" . drover-next) ("b" . drover-select) ("3" . drover-dashboard)
                   ("4" . drover-table) ("5" . drover-sidebar) ("a" . drover-prompt-agent)
                   ("m" . drover-prompt-model) ("e" . drover-prompt-effort)
                   ("d" . drover-prompt-directory) ("C" . drover-compact) ("f" . drover-fork)
                   ("." . drover-prompt-last-answer) ("O" . drover-prompt-open-link)
                   ("RET" . drover-prompt-send) ("S-<return>" . drover-prompt-schedule)
                   ("i" . drover-prompt-attach) ("/" . drover-prompt-slash)
                   ("@" . drover-prompt-mention) ("t" . drover-prompt-revert)
                   ("T" . drover-prompt-revert-cancel) ("y" . drover-prompt-copy-block)))
    (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch (car entry))) :command)
                (cdr entry))))
  ;; Every session key runs the command `drover-session-map' binds it to.
  (map-keymap (lambda (event command)
                (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch
                                                                 (key-description (vector event))))
                                       :command)
                            command)))
              drover-session-map))

(ert-deftest drover-keys-test-dispatch-elsewhere ()
  "Outside lists and prompt buffers, only the global commands apply."
  (drover-keys-test-with (list (drover-test-session "s"))
    (with-temp-buffer
      (should (equal (drover-keys-test-apt 'drover-dispatch) '("j" "b" "3" "4" "5"))))))

(ert-deftest drover-keys-test-dispatch-in-view ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working :requests '((:kind question :summary "?"))
                                                    :parent "root")
                               (drover-test-session "root")
                               (drover-test-session "sub" :parent "s"))
    (drover-keys-test-in 'view "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "3" "4" "5" "C" "f"
                       "o" "u" "c" "v" "V" "A" "k" "+" "g" "r" "x" "z" "l" "U" "G" "F"))))
    ;; Off a session, only what needs none.
    (drover-keys-test-in 'view nil
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "3" "4" "5" "V" "+" "g" "U" "G" "F"))))))

(ert-deftest drover-keys-test-dispatch-session-state ()
  "Session commands apply when the session's state allows them."
  (drover-keys-test-with (list (drover-test-session "done" :status 'done)
                               (drover-test-session "idle" :status 'idle))
    (drover-keys-test-in 'view "done"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (should (member "s" apt))
        (dolist (key '("A" "k" "u" "c"))
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

(ert-deftest drover-keys-test-dispatch-in-foreign-list ()
  "In a list drover does not draw, grouping and filtering do not apply."
  (drover-keys-test-with (list (drover-test-session "s"))
    (drover-keys-test-in 'list "s"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (dolist (key '("v" "V" "U"))
          (should (member key apt)))
        (dolist (key '("G" "F"))
          (should-not (member key apt)))))))

(ert-deftest drover-keys-test-dispatch-in-prompt ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working))
    (drover-keys-test-in 'prompt "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "3" "4" "5" "a" "m" "e" "d" "C" "f" "." "O"
                       "o" "k" "+" "g" "r" "x" "z" "l"
                       "RET" "S-<return>" "i" "/" "@" "y" "t" "T"))))))

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
                (dolist (key '("RET" "t" "T" "m"))
                  (should-not (member key apt))))
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
                             '("j" "b" "3" "4" "5" "a" "m" "e" "d")))
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

(ert-deftest drover-keys-test-dispatch-extension-points ()
  "Others place their commands relative to drover's keys."
  (unwind-protect
      (progn
        (transient-append-suffix 'drover-dispatch "b" '("\"" "Prompt" ignore))
        (transient-insert-suffix 'drover-dispatch "3" '("!" "Lists" ignore))
        (transient-append-suffix 'drover-dispatch "5" '("6" "More" ignore))
        (transient-append-suffix 'drover-dispatch "O" '("P" "Other" ignore))
        (with-temp-buffer
          (let ((keys (cdr (assoc "Global" (drover-keys-test-keys 'drover-dispatch t)))))
            (cl-flet ((follows (before after)
                        (equal (cadr (member before keys)) after)))
              (should (follows "b" "\""))
              (should (follows "!" "3"))
              (should (follows "5" "6"))
              (should (follows "O" "P"))))))
    (dolist (key '("\"" "!" "6" "P"))
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
            (seq-filter #'drover-keys-test--own-p transient--suffixes))))

(ert-deftest drover-keys-test-menus-draw ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (drover-keys-test-with (list (drover-test-session "s" :project "drover"))
          (set-time-zone-rule "UTC")
          (drover-keys-test-in 'view "s"
            (let ((dispatch (drover-keys-test-render 'drover-dispatch)))
              (should (member " j Next" dispatch))
              (should (member " RET Send" dispatch))
              (should (member " S-<return> Schedule" dispatch)))
            ;; The order of the layout is checked elsewhere.
            (should (seq-set-equal-p (drover-keys-test-render 'drover-group)
                                     '(" s status" " p project" " n nothing")))
            (setq drover-filter (list :project "drover" :statuses '(waiting done)
                                      :since (- (float-time) 7200)))
            (should (seq-set-equal-p (drover-keys-test-render 'drover-filter)
                                     '(" p Project drover" " s Status needs you or done"
                                       " d Updated since 2h" " f Search any" " c Clear all")))))
      (set-time-zone-rule zone))))

;;;; Grouping

(ert-deftest drover-keys-test-group-menu ()
  (drover-keys-test-with nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (should (equal (drover-keys-test-keys 'drover-group) '(("Group sessions by" "s" "p" "n"))))
        (let* ((setup (drover-keys-test-suffixes 'drover-group))
               (transient--prefix (car setup))
               (suffixes (cdr setup))
               (by-key (lambda (key) (seq-find (lambda (suffix) (equal (oref suffix key) key)) suffixes)))
               (face (lambda (key) (get-text-property 0 'face (transient-format-description
                                                               (funcall by-key key)))))
               (choose (lambda (key)
                         (let ((infix (funcall by-key key)))
                           (cl-letf (((symbol-function 'transient--show) #'ignore))
                             (transient-infix-set infix (transient-infix-read infix)))))))
          ;; The current grouping stands out.
          (should (eq (funcall face "s") 'transient-value))
          (should (eq (funcall face "p") 'transient-inactive-value))
          (funcall choose "p")
          (should (eq drover-group-by 'project))
          (should (= runs 1))
          (should (eq (funcall face "p") 'transient-value))
          (should (eq (funcall face "s") 'transient-inactive-value))
          (funcall choose "n")
          (should-not drover-group-by)
          (should (eq (funcall face "n") 'transient-value)))))))

(ert-deftest drover-keys-test-group-and-filter-only-in-views ()
  (with-temp-buffer
    (should-error (drover-group) :type 'user-error)
    (should-error (drover-filter) :type 'user-error)
    (drover-session-keys-mode)
    (should-error (drover-group) :type 'user-error)
    (should-error (drover-filter) :type 'user-error)))

;;;; Filtering

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
                          (list :project "drover" :statuses '(waiting) :since (- now (* 2 86400))
                                :search "auth")
                          now)
                         "project drover, needs you, since 2d, \"auth\""))
          (should (equal (drover-filter-description (list :statuses '(done failed)) now) "done or failed"))
          (should (equal (drover-filter-description (list :since (- now (* 8 3600))) now) "today"))
          (setq drover-filter '(:search "x"))
          (should (equal (drover-filter-description nil now) "\"x\"")))
      (set-time-zone-rule zone))))

(ert-deftest drover-keys-test-filter-clear-hint ()
  "The hint names the keys bound to the filter menu, then its clear key."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map drover-session-map)
    (should (equal (substring-no-properties (drover-filter-clear-hint map)) "F c clears the filter."))
    (should (eq (get-text-property 0 'face (drover-filter-clear-keys map)) 'help-key-binding))
    (with-temp-buffer
      (use-local-map map)
      (should (equal (substring-no-properties (drover-filter-clear-keys)) "F c"))
      (keymap-set map "F" nil)
      (keymap-set map "C-c f" #'drover-filter)
      (should (equal (substring-no-properties (drover-filter-clear-keys)) "C-c f c")))))

(ert-deftest drover-keys-test-filter-menu ()
  (drover-keys-test-with (list (drover-test-session "a" :project "drover")
                               (drover-test-session "b" :project "opencode")
                               (drover-test-session "c"))
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (should (equal (drover-keys-test-keys 'drover-filter) '(("Filter sessions" "p" "s" "d" "f"))))
        (let* ((setup (drover-keys-test-suffixes 'drover-filter))
               (transient--prefix (car setup))
               (suffixes (cdr setup))
               (by-key (lambda (key) (seq-find (lambda (suffix) (equal (oref suffix key) key)) suffixes)))
               (value (lambda (key) (transient-format-value (funcall by-key key))))
               (set (lambda (key)
                      (let ((infix (funcall by-key key)))
                        (cl-letf (((symbol-function 'transient--show) #'ignore))
                          (transient-infix-set infix (transient-infix-read infix)))))))
          (should (equal (funcall value "p") "any"))
          (should (eq (get-text-property 0 'face (funcall value "p")) 'transient-inactive-value))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt candidates &rest _)
                       ;; The projects of the sessions.
                       (should (equal candidates '("drover" "opencode")))
                       "drover")))
            (funcall set "p"))
          (should (equal drover-filter '(:project "drover")))
          (should (= runs 1))
          (should (equal (funcall value "p") "drover"))
          (should (eq (get-text-property 0 'face (funcall value "p")) 'transient-value))
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (_prompt candidates &rest _)
                       (should (equal (mapcar #'cdr candidates) drover-status-order))
                       '("Needs you" "Done"))))
            (funcall set "s"))
          (should (equal (plist-get drover-filter :statuses) '(waiting done)))
          (should (equal (funcall value "s") "needs you or done"))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "3d")))
            (funcall set "d"))
          (should (< (abs (- (plist-get drover-filter :since) (- (float-time) (* 3 86400)))) 60))
          (cl-letf (((symbol-function 'read-string) (lambda (_prompt initial &rest _)
                                                      (should-not initial)
                                                      "auth")))
            (funcall set "f"))
          (should (equal (plist-get drover-filter :search) "auth"))
          (should (equal (funcall value "f") "\"auth\""))
          ;; An empty answer removes the filter.
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
            (funcall set "p"))
          (should-not (plist-member drover-filter :project))
          (should (equal (funcall value "p") "any"))
          (drover-filter-clear)
          (should-not drover-filter)
          (should (= runs 6)))))))

(provide 'drover-keys-test)
;;; drover-keys-test.el ends here
