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
as in a row of columns, stands for its columns."
  (let* ((transient--prefix (transient--init-prefix
                             prefix (and (eq prefix 'drover-dispatch) (list :scope (drover--context)))))
         (title-of (lambda (group)
                     (let ((description (and (slot-boundp group 'description) (oref group description))))
                       (if (functionp description) (funcall description) description))))
         (layout (mapcan (lambda (group)
                           (if (and (cl-typep group 'transient-group)
                                    (not (stringp (funcall title-of group)))
                                    (seq-every-p (lambda (child) (cl-typep child 'transient-group))
                                                 (oref group suffixes)))
                               (copy-sequence (oref group suffixes))
                             (list group)))
                         (transient--init-suffixes prefix))))
    (cl-labels ((entries (group)
                  (mapcan (lambda (suffix)
                            (and (or all (drover-keys-test--own-p suffix))
                                 ;; Those an :if leaves out are neither shown nor bound.
                                 (not (oref suffix inactive))
                                 (list (cons (key-description (kbd (oref suffix key))) (oref suffix inapt)))))
                          (oref group suffixes))))
      (delq nil
            (mapcar (lambda (group)
                      (when-let* (((cl-typep group 'transient-group))
                                  (title (funcall title-of group))
                                  ((stringp title))
                                  ;; Not transient's own, such as its common commands.
                                  ((seq-some #'drover-keys-test--own-p
                                             (transient--flatten-suffixes (list group)))))
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

(ert-deftest drover-keys-test-dispatch-layout ()
  (with-temp-buffer
    (let ((groups (drover-keys-test-menu 'drover-dispatch)))
      (should (equal (mapcar #'car groups) '("Global" "Session" "Prompt")))
      (should (equal (mapcar (lambda (group) (mapcar (lambda (column) (mapcar #'car column)) (cdr group)))
                             groups)
                     '((("j" "b" "\"" "n" "'" "#") ("l" "L" "p") ("a" "m" "e" "d") ("C" "f" "." "O"))
                       (("o" "u" "c" "v") ("A" "K" "s" "+" "g") ("r" "x" "z" "Z") ("U" "V"))
                       (("RET" "S-<return>" "i" "k") ("/" "@" "y") ("t" "T")))))
      ;; One key, one command.
      (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-dispatch))))
        (should (equal keys (delete-dups (copy-sequence keys))))))))

(ert-deftest drover-keys-test-dispatch-commands ()
  "Each key of the menu runs the command of the shared key tables."
  (dolist (entry '(("j" . drover-next) ("b" . drover-select) ("\"" . drover-prompt-dwim)
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
  ;; Every session key runs the command `drover-session-map' binds it to.
  (map-keymap (lambda (event command)
                (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch
                                                                 (key-description (vector event))))
                                       :command)
                            command)))
              drover-session-map))

(ert-deftest drover-keys-test-dispatch-elsewhere ()
  "Outside lists and prompt buffers, only the global commands apply.
With drover's prompt as the default prompt buffer, as by default, the
agent, model, effort and directory of the next prompt are among them."
  (drover-keys-test-with (list (drover-test-session "s"))
    (with-temp-buffer
      (should (equal (drover-keys-test-apt 'drover-dispatch) '("j" "b" "\"" "'" "#" "l" "L" "p")))
      (setq drover-default-prompt-buffer-function
            (eval (car (get 'drover-default-prompt-buffer-function 'standard-value)) t))
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "\"" "'" "#" "l" "L" "p" "a" "m" "e" "d"))))))

(ert-deftest drover-keys-test-dispatch-prompt-entries ()
  "The prompt's entries open it from anywhere, but from the prompt itself."
  (should (equal (cdr (assoc "Global" (drover-keys-test-keys 'drover-dispatch)))
                 '("j" "b" "\"" "n" "'" "#" "l" "L" "p" "a" "m" "e" "d" "C" "f" "." "O")))
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

(ert-deftest drover-keys-test-dispatch-in-view ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working :requests '((:kind question :summary "?"))
                                                    :parent "root")
                               (drover-test-session "root")
                               (drover-test-session "sub" :parent "s"))
    (drover-keys-test-in 'view "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "\"" "'" "#" "l" "L" "p" "C" "f"
                       "o" "u" "c" "v" "A" "K" "+" "g" "r" "x" "z" "U" "V"
                       "C-n" "C-p" "M-n" "M-p" "M-<" "M->" "RET"))))
    ;; Off a session, only what needs none.
    (drover-keys-test-in 'view nil
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "\"" "'" "#" "l" "L" "p" "+" "g" "U" "V"
                       "C-n" "C-p" "M-n" "M-p" "M-<" "M->")))
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
  "In a list drover does not draw, the View menu does not apply."
  (drover-keys-test-with (list (drover-test-session "s"))
    (drover-keys-test-in 'list "s"
      (let ((apt (drover-keys-test-apt 'drover-dispatch)))
        (dolist (key '("v" "U"))
          (should (member key apt)))
        (should-not (member "V" apt))))))

(ert-deftest drover-keys-test-dispatch-in-prompt ()
  (drover-keys-test-with (list (drover-test-session "s" :status 'working))
    (drover-keys-test-in 'prompt "s"
      (should (equal (drover-keys-test-apt 'drover-dispatch)
                     '("j" "b" "\"" "'" "#" "l" "L" "p" "a" "m" "e" "d" "C" "f" "." "O"
                       "o" "K" "+" "g" "r" "x" "z"
                       "RET" "S-<return>" "i" "k" "/" "@" "y" "t" "T"))))))

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
                             '("j" "b" "\"" "'" "#" "l" "L" "p" "a" "m" "e" "d")))
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
          (let ((keys (cdr (assoc "Global" (drover-keys-test-keys 'drover-dispatch t)))))
            (cl-flet ((follows (before after)
                        (equal (cadr (member before keys)) after)))
              (should (follows "b" "&"))
              (should (follows "&" "\""))
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
              (should (member " V View" dispatch))
              (should (member " k Clear" dispatch))
              ;; In a list, RET opens the session rather than send.
              (should (member " RET Open" dispatch))
              (should-not (member " RET Send" dispatch))
              (should (member " C-n Next session" dispatch))
              (should (member " S-<return> Schedule" dispatch)))
            ;; The order of the layout is checked elsewhere.
            (should (seq-set-equal-p (drover-keys-test-render 'drover-view)
                                     '(" o urgency" " r [ascending|descending]" " g status"
                                       " p project any" " s status any" " a agent any"
                                       " m model any" " v provider any" " d updated any"
                                       " f text any" " c clear"
                                       " F follow [off|on]" " U subagents [all|active|none]"
                                       " O older [hidden|shown]"
                                       " A archived [hidden|shown]"
                                       " -l location" " -b branch" " -a agent" " -m model" " -r provider"
                                       " -x context" " -c created"
                                       " -t age" " -p cache" " -s summary")))
            (setq drover-filter (list :project "drover" :statuses '(waiting done) :agent "build"
                                      :provider "Anthropic" :since (- (float-time) 7200))
                  drover-sort-by 'updated
                  drover-sort-descending t
                  drover-group-by nil)
            (let ((view (drover-keys-test-render 'drover-view)))
              (dolist (line '(" o last activity" " g nothing" " p project drover"
                              " s status needs you or done" " a agent build" " v provider Anthropic"
                              " d updated since 2h"
                              " f text any"))
                (should (member line view))))))
      (set-time-zone-rule zone))))

;;;; The View menu

(defun drover-keys-test-setting (key)
  "Return the infix of `drover-view' on KEY, set up in the current buffer.
The value is (PREFIX-OBJECT . SUFFIX), for `drover-keys-test-choose'."
  (let ((setup (drover-keys-test-suffixes 'drover-view)))
    (cons (car setup) (seq-find (lambda (suffix) (equal (oref suffix key) key)) (cdr setup)))))

(defun drover-keys-test-choose (key)
  "Change the setting of KEY in `drover-view' as pressing KEY would."
  (pcase-let* ((`(,prefix . ,infix) (drover-keys-test-setting key))
               (transient--prefix prefix))
    (cl-letf (((symbol-function 'transient--show) #'ignore))
      (transient-infix-set infix (transient-infix-read infix)))))

(defun drover-keys-test-value (key)
  "Return the value the infix of KEY in `drover-view' shows, with its faces."
  (pcase-let* ((`(,prefix . ,infix) (drover-keys-test-setting key))
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
                     '(("Sort by" "o" "r") ("Group by" "g")
                       ("Filter by" "p" "s" "a" "m" "v" "d" "f" "c")
                       ("Toggles" "F" "U" "O" "A")
                       ("Columns" "- l" "- b" "- a" "- m" "- r" "- x" "- c" "- t" "- p" "- s"))))
      ;; One key, one setting.
      (let ((keys (mapcan #'cdr (drover-keys-test-keys 'drover-view t))))
        (should (equal keys (delete-dups (copy-sequence keys)))))
      ;; Older sessions need a backend keeping them.
      (should-not (member "O" (drover-keys-test-apt 'drover-view)))
      (setf (alist-get 'fake drover--backends)
            (append (list :history #'ignore) (alist-get 'fake drover--backends)))
      (should (member "O" (drover-keys-test-apt 'drover-view)))
      ;; Archived sessions too, or drover's archive holding some.
      (should-not (member "A" (drover-keys-test-apt 'drover-view)))
      (setf (alist-get 'fake drover--backends)
            (append (list :archived #'ignore) (alist-get 'fake drover--backends)))
      (should (member "A" (drover-keys-test-apt 'drover-view))))))

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
          (dolist (line '(" o last activity" " r [ascending|descending]" " g nothing"))
            (should (member line view))))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "o")) '("last activity")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "r")) '("descending")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "g")) '("nothing")))))))

(ert-deftest drover-keys-test-view-sort-and-group ()
  (drover-keys-test-with nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (with-temp-buffer
        (setq-local drover-view-p t)
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "o")) '("urgency")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt candidates &rest _)
                     (should (equal prompt "Sort by (default urgency): "))
                     (should (equal candidates (mapcar #'cdr drover-sort-labels)))
                     "last activity")))
          (drover-keys-test-choose "o"))
        (should (eq drover-sort-by 'updated))
        (should (= runs 1))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "o")) '("last activity")))
        ;; The order shows both values, the current one highlighted.
        (should (equal (substring-no-properties (drover-keys-test-value "r")) "[ascending|descending]"))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "r")) '("ascending")))
        (drover-keys-test-choose "r")
        (should drover-sort-descending)
        (should (eq drover-sort-by 'updated))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "r")) '("descending")))
        (drover-keys-test-choose "r")
        (should-not drover-sort-descending)
        ;; Grouping by nothing is a grouping too.
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "g")) '("status")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (member "last activity" candidates))
                     "nothing")))
          (drover-keys-test-choose "g"))
        (should-not drover-group-by)
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "g")) '("nothing")))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "branch")))
          (drover-keys-test-choose "g"))
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
        (should (equal (drover-keys-test-value "p") "any"))
        (should (eq (get-text-property 0 'face (drover-keys-test-value "p")) 'transient-inactive-value))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     ;; The projects of the sessions.
                     (should (equal candidates '("drover" "opencode")))
                     "drover")))
          (drover-keys-test-choose "p"))
        (should (equal drover-filter '(:project "drover")))
        (should (= runs 1))
        (should (equal (drover-keys-test-value "p") "drover"))
        (should (eq (get-text-property 0 'face (drover-keys-test-value "p")) 'transient-value))
        (cl-letf (((symbol-function 'completing-read-multiple)
                   (lambda (_prompt candidates &rest _)
                     (should (equal (mapcar #'cdr candidates) drover-status-order))
                     '("Needs you" "Done"))))
          (drover-keys-test-choose "s"))
        (should (equal (plist-get drover-filter :statuses) '(waiting done)))
        (should (equal (drover-keys-test-value "s") "needs you or done"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal candidates '("build" "plan")))
                     "plan")))
          (drover-keys-test-choose "a"))
        (should (equal (plist-get drover-filter :agent) "plan"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (should (equal candidates '("opus")))
                     "opus")))
          (drover-keys-test-choose "m"))
        (should (equal (drover-keys-test-value "m") "opus"))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "3d")))
          (drover-keys-test-choose "d"))
        (should (< (abs (- (plist-get drover-filter :since) (- (float-time) (* 3 86400)))) 60))
        (cl-letf (((symbol-function 'read-string) (lambda (_prompt initial &rest _)
                                                    (should-not initial)
                                                    "auth")))
          (drover-keys-test-choose "f"))
        (should (equal (plist-get drover-filter :search) "auth"))
        (should (equal (drover-keys-test-value "f") "\"auth\""))
        ;; An empty answer removes the filter.
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
          (drover-keys-test-choose "p"))
        (should-not (plist-member drover-filter :project))
        (should (equal (drover-keys-test-value "p") "any"))
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
          (drover-keys-test-choose "o")
          (should (eq drover-sort-by 'provider))
          (setq drover-group-by nil)
          (should (equal (mapcar #'car (drover-test-tree-shape (cdar (drover-view-groups))))
                         '("a" "c" "b" "d")))
          (drover-keys-test-choose "g"))
        (should (eq drover-group-by 'provider))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "o")) '("provider")))
        (should (equal (drover-keys-test-highlighted (drover-keys-test-value "g")) '("provider")))
        (should (equal (mapcar (lambda (group) (cons (cdar group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                               (drover-view-groups))
                       '(("Anthropic" "a" "c") ("OpenAI" "b") (nil "d"))))
        ;; The providers of the sessions, by name.
        (should (equal (drover-keys-test-value "v") "any"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt candidates &rest _)
                     (should (equal prompt "Provider (empty for any): "))
                     (should (equal candidates '("Anthropic" "OpenAI")))
                     "OpenAI")))
          (drover-keys-test-choose "v"))
        (should (equal drover-filter '(:provider "OpenAI")))
        (should (equal (drover-keys-test-value "v") "OpenAI"))
        (should (equal (mapcar #'car (drover-test-tree-shape (drover-view-trees))) '("b")))
        (should (equal (drover-filter-description) "provider OpenAI"))
        (should (= runs 3))))))

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
          (drover-keys-test-choose "O")
          (should drover-show-history)
          (should (= fetched 1))
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "O")) '("shown")))
          (drover-keys-test-choose "O")
          (should-not drover-show-history)
          ;; Follow mode is the list's own.
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "F")) '("off")))
          (drover-keys-test-choose "F")
          (should drover-follow-mode)
          (should (equal (drover-keys-test-highlighted (drover-keys-test-value "F")) '("on")))
          (should (equal (car drover-test-calls) '(:open "a" nil)))
          (drover-keys-test-choose "F")
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
      (should (= (length suffixes) 25))
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
