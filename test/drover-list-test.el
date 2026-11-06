;;; drover-list-test.el --- Tests of the session list  -*- lexical-binding: t; -*-

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

;; The list is drawn in its buffer without displaying it, except where
;; a test is about windows: visibility, window point and start, the side
;; window, the main window.  Batch Emacs has one frame, where side
;; windows work but redisplay, and so the window change functions,
;; never runs: tests call those functions themselves.
;;
;; Tests run on a text terminal frame, where a pixel is a character
;; cell: `drover-list-test-displayed' turns each aligned or stretched
;; space into the spaces a terminal draws for it, so lines read as the
;; user sees them.  Layouts depending on the width, and alignment with
;; proportional widths, are tested with a fake font
;; (`drover-list-test-with-font').  The clock stands still at
;; `drover-list-test-clock', which tests move to see time pass.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-list)
;; Loaded before tests stub its functions, which loading would undo.
(require 'drover-attention)
(require 'text-property-search)

(defvar drover-list-test-clock drover-test-now
  "The current time, as the list reads it in tests.")

(defun drover-list-test--cleanup ()
  "Kill the buffers the tests made, leave one window, stop the timers."
  ;; A test failing within a menu leaves it open, holding the keys.
  (when (bound-and-true-p transient--prefix)
    (ignore-errors (execute-kbd-macro (kbd "C-g"))))
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (string-match-p "\\`\\*\\(drover \\|session \\|other\\)" (buffer-name buffer)))
      (kill-buffer buffer)))
  (let ((ignore-window-parameters t))
    (delete-other-windows (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                                    (window-list nil 'nomini))))
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer "*scratch*")
  (drover-list--stop-timer 'drover-list--age-timer)
  (drover-list--stop-timer 'drover-list--cache-timer)
  (drover-list--stop-timer 'drover-list--frame-timer)
  (drover-list--stop-timer 'drover-list--focus-timer)
  (drover-list--stop-timer 'drover-list--place-timer)
  (set-frame-parameter nil 'drover-list-sidebar-aside nil)
  (set-frame-parameter nil 'drover-list-sidebar-kept nil)
  (setq drover-list--age-due nil
        drover-list--frame-timer-pace nil
        drover-list--focused t))

(defmacro drover-list-test-with (sessions &rest body)
  "Run BODY in a fresh list drawn from SESSIONS of the fake backend.
The list buffer is `buffer', and current; it is not displayed.  The
settings of the list are their defaults, and the clock stands still at
`drover-list-test-clock'."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((drover-list-test-clock drover-test-now)
           (drover-list-age-interval nil)
           (drover-list-cache-interval nil)
           (drover-list-cache-bar-width 5)
           (drover-list-collapsed-groups '((status . idle)))
           (drover-list-fields (eval (car (get 'drover-list-fields 'standard-value)) t))
           (drover-list-side-window-side 'left)
           (drover-list-side-window-width 36)
           (drover-list-side-window-slot 0)
           (drover-status-order '(waiting failed done retrying working idle))
           ;; Readable glyphs, one column wide in every Emacs, whatever
           ;; the defaults, and still unless a test animates them.
           (opencode-session-status-icons '((waiting . "!") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·")))
           (opencode-subagent-icons '((waiting . "?") (working . "*")))
           (opencode-session-status-animations '((working "1" "2" "3") (retrying "a" "b")))
           (opencode-session-status-animation-interval nil)
           ;; The harness, the same for every fake session, shows in the
           ;; tests of it and of the sidebar, where its icon ends the
           ;; line (`drover-list-test-showing').
           (drover-list-hidden-fields '(harness))
           (drover-harness-default-icon "▣")
           (drover-host-icon "@")
           (drover-context-pie-icons '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"))
           (buffer nil))
       (cl-letf (((symbol-function 'drover-list--now) (lambda () drover-list-test-clock)))
         (unwind-protect
             (progn
               (setq buffer (drover-list--buffer))
               (with-current-buffer buffer
                 (revert-buffer)
                 ,@body))
           (drover-list-test--cleanup))))))

(defun drover-list-test-fake-width (string)
  "Return the width of STRING in a fake proportional font.
An i is 3 pixels wide, an m 12, a space 4, and any other character 7.
A space displayed as a stretch counts its own width."
  (let ((width 0))
    (dotimes (index (length string))
      (setq width (+ width
                     (pcase (get-text-property index 'display string)
                       (`(space :width (,pixels)) pixels)
                       ((and (pred stringp) display) (drover-list-test-fake-width display))
                       (_ (pcase (aref string index) (?i 3) (?m 12) (?\s 4) (_ 7)))))))
    width))

(defvar drover-list-test-width 1000
  "Width in pixels of the lines of the list, under `drover-list-test-with-font'.")

(defvar drover-list-test-ink nil
  "Characters of the fake font drawing ink past their advance.
An alist (CHAR LEFT . RIGHT): the ink of CHAR reaches LEFT pixels left
of where it is drawn and RIGHT pixels right of it.")

(defun drover-list-test-fake-extent (string)
  "Return how far STRING reaches drawn in the fake font, as (LEFT . RIGHT).
The ink of the characters of `drover-list-test-ink' reaches past their
advance, as `drover-list--extent' measures it; characters displayed
otherwise count by their advance."
  (let ((x 0) (left 0) (right 0))
    (dotimes (index (length string))
      (when-let* (((not (get-text-property index 'display string)))
                  (ink (alist-get (aref string index) drover-list-test-ink)))
        (setq left (max left (- (car ink) x))
              right (max right (+ x (cdr ink)))))
      (setq x (+ x (drover-list-test-fake-width (substring string index (1+ index))))))
    (cons left (max right x))))

(defmacro drover-list-test-with-font (&rest body)
  "Run BODY with text measured in `drover-list-test-fake-width'.
Ink reaches as `drover-list-test-fake-extent' says.  Lines may take
`drover-list-test-width' pixels."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'drover-list--pixel-width) #'drover-list-test-fake-width)
             ((symbol-function 'drover-list--extent) #'drover-list-test-fake-extent)
             ((symbol-function 'drover-list--line-width) (lambda () drover-list-test-width)))
     ,@body))

(defmacro drover-list-test-showing (&rest body)
  "Run BODY with every field let show, the harness's included."
  (declare (indent 0) (debug t))
  `(let ((drover-list-hidden-fields nil))
     ,@body))

(defun drover-list-test-ago (seconds)
  "Return the time SECONDS before the clock of the tests."
  (- drover-test-now seconds))

(defun drover-list-test-titled (id title &rest properties)
  "Return a fake session ID titled TITLE, with PROPERTIES."
  (let ((session (apply #'drover-test-session id properties)))
    (setf (drover-session-title session) title)
    session))

(defun drover-list-test-find (id)
  "Return the fake session ID the backend serves."
  (seq-find (lambda (session) (equal (drover-session-id session) id)) drover-test-sessions))

(defun drover-list-test-groups ()
  "Return the groups of the list as a list of (VALUE . IDS).
VALUE is the value of the group, without its grouping.  The sessions of
collapsed groups are drawn first, as for searching."
  (drover-list--draw-deferred)
  (mapcar (lambda (group)
            (cons (cdr (oref group value))
                  (mapcar (lambda (section) (cdr (oref section value)))
                          (oref group children))))
          (drover-list--groups)))

(defun drover-list-test-ungrouped ()
  "Return the identifiers of the sessions listed outside groups, in order."
  (drover-list--draw-deferred)
  (mapcar (lambda (section) (cdr (oref section value)))
          (drover-list--session-sections magit-root-section)))

(defun drover-list-test-order ()
  "Return the identifiers of the sessions listed, in order, groups or not.
The sessions of collapsed groups are drawn first."
  (drover-list--draw-deferred)
  (mapcan (lambda (container)
            (mapcar (lambda (section) (cdr (oref section value)))
                    (drover-list--session-sections container)))
          (drover-list--containers)))

(defmacro drover-list-test-shown (&rest body)
  "Run BODY with the list `buffer' alone in the selected window.
Point stays where it is.  The list redraws on settings changes only
while shown."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let ((position (point)))
       (delete-other-windows)
       (set-window-buffer (selected-window) buffer)
       (set-window-point (selected-window) position)
       ,@body)))

(defun drover-list-test-section (id &optional backend)
  "Return the section of the session ID of BACKEND, the fake one by default.
The sessions of collapsed groups are drawn first."
  (drover-list--draw-deferred)
  (drover-list--find-session (cons (or backend 'fake) id)))

(defun drover-list-test-goto (id)
  "Move point to the heading of the session ID of the fake backend."
  (goto-char (oref (or (drover-list-test-section id) (error "No session %s" id)) start)))

(defun drover-list-test-goto-group (value)
  "Move point to the heading of the group whose value is VALUE."
  (goto-char (oref (seq-find (lambda (group) (equal (cdr (oref group value)) value))
                             (drover-list--groups))
                   start)))

(defun drover-list-test-displayed (string)
  "Return STRING as a text terminal draws it.
Each space aligned to a pixel position becomes the spaces reaching it,
each stretched space as many spaces as it is wide, each character
displayed as a string that string, and invisible characters nothing:
on a terminal, a pixel is a character cell.  Aligned cells must not
overlap."
  (let ((column 0)
        (chunks nil))
    (dotimes (index (length string))
      (let ((char (aref string index))
            (display (get-text-property index 'display string)))
        (pcase (if (get-text-property index 'invisible string) 'invisible display)
          ('invisible nil)
          (`(space :align-to (,x))
           (should (>= x column))
           (push (make-string (- x column) ?\s) chunks)
           (setq column x))
          (`(space :width (,width))
           (push (make-string width ?\s) chunks)
           (setq column (+ column width)))
          ((pred stringp)
           (push display chunks)
           (setq column (+ column (string-width display))))
          (_
           (if (eq char ?\n)
               (progn (push "\n" chunks)
                      (setq column 0))
             (push (string char) chunks)
             (setq column (+ column (char-width char))))))))
    (apply #'concat (nreverse chunks))))

(defun drover-list-test-line ()
  "Return the line at point as a text terminal draws it."
  (drover-list-test-displayed (buffer-substring (line-beginning-position) (line-end-position))))

(defun drover-list-test-heading (id)
  "Return the first line of the heading of session ID as a terminal draws it."
  (save-excursion
    (drover-list-test-goto id)
    (drover-list-test-line)))

(defun drover-list-test-entry (id)
  "Return the whole heading of session ID, every line, as a terminal draws it."
  (let ((section (drover-list-test-section id)))
    (drover-list-test-displayed
     (buffer-substring (oref section start) (1- (or (oref section content) (oref section end)))))))

(defun drover-list-test-cells (id)
  "Return the cells of the first line of session ID, as a list of (X . TEXT).
X is the pixel position a cell is aligned to, TEXT what follows it up
to the next cell, with its properties."
  (save-excursion
    (drover-list-test-goto id)
    (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
          (cells nil))
      (dotimes (index (length line))
        (pcase (get-text-property index 'display line)
          ((guard (get-text-property index 'invisible line)))
          (`(space :align-to (,x)) (push (cons x "") cells))
          (_ (setcdr (car cells) (concat (cdar cells) (substring line index (1+ index)))))))
      (nreverse cells))))

(defun drover-list-test-cell-at (id text)
  "Return the pixel position of the cell of session ID holding TEXT."
  (car (seq-find (lambda (cell) (string-search text (cdr cell)))
                 (drover-list-test-cells id))))

(defun drover-list-test-title-x (id title)
  "Return the pixel position where TITLE begins on the first line of session ID."
  (let ((cell (seq-find (lambda (cell) (string-search title (cdr cell)))
                        (drover-list-test-cells id))))
    (+ (car cell) (drover-list--pixel-width (substring (cdr cell) 0 (string-search title (cdr cell)))))))

(defun drover-list-test-id-at-point ()
  "Return the identifier of the session whose section holds point, or nil."
  (when-let* ((section (drover-list--session-section (magit-section-at))))
    (cdr (oref section value))))

(defun drover-list-test-visible-text ()
  "Return the text of the list that is not invisible, as drawn."
  (let ((position (point-min))
        (chunks nil))
    (while (< position (point-max))
      (let ((next (next-single-char-property-change position 'invisible nil (point-max))))
        (unless (invisible-p position)
          (push (buffer-substring position next) chunks))
        (setq position next)))
    (drover-list-test-displayed (apply #'concat (nreverse chunks)))))

(defun drover-list-test-section-hidden-p (id)
  "Return non-nil when the section of session ID is collapsed."
  (oref (drover-list-test-section id) hidden))

(defun drover-list-test-group-hidden-p (value)
  "Return non-nil when the group whose value is VALUE is collapsed."
  (oref (seq-find (lambda (group) (equal (cdr (oref group value)) value))
                  (drover-list--groups))
        hidden))

(defun drover-list-test-heading-visible-p (id)
  "Return non-nil when the heading of session ID is visible."
  (drover-list--line-shown-p (oref (drover-list-test-section id) start)))

(defun drover-list-test-sample ()
  "Return a sample of sessions, one or more of each status."
  (list (drover-test-session "idle" :status 'idle :updated (drover-list-test-ago 10) :project "p")
        (drover-test-session "work" :status 'working :updated (drover-list-test-ago 20) :project "q")
        (drover-test-session "wait2" :status 'waiting :since (drover-list-test-ago 60) :project "p")
        (drover-test-session "done" :status 'done :since (drover-list-test-ago 30))
        (drover-test-session "wait1" :status 'waiting :since (drover-list-test-ago 600) :project "q")))

(defun drover-list-test-family ()
  "Return a parent with subagents two levels deep, and another tree.
The parent works, its subagent waits for the user, and the subagent's
own subagent is done; the other tree works and has a quiet subagent."
  (list (drover-test-session "root" :status 'working :updated (drover-list-test-ago 10))
        (drover-test-session "sub" :parent "root" :status 'waiting :since (drover-list-test-ago 60))
        (drover-test-session "deep" :parent "sub" :status 'done :since (drover-list-test-ago 30))
        (drover-test-session "other" :status 'working :updated (drover-list-test-ago 5))
        (drover-test-session "quiet" :parent "other" :status 'idle)))

(defun drover-list-test-run-relist ()
  "List the sessions anew, as a refresh does right after drawing."
  (when-let* ((timer drover-list--relist-timer))
    (cancel-timer timer)
    (funcall (timer--function timer))))

(defun drover-list-test-refresh ()
  "Draw the list anew from the sessions as they are now, reported or not.
The sessions are listed anew first, without the list following them in
place, as a refresh did before it drew from what was reported."
  (let ((drover-changed-hook nil))
    (drover-index-refresh))
  (revert-buffer)
  (drover-list-test-run-relist))

(defun drover-list-test-press (key)
  "Run the command KEY runs at point in the current buffer."
  (let ((command (key-binding (kbd key))))
    (should command)
    (call-interactively command)))

(defun drover-list-test-type (keys)
  "Type KEYS, a `kbd' string, through the command loop, as the user would.
The list `buffer' must be shown in the selected window; it is the
current buffer again afterwards."
  (should (eq (window-buffer) (current-buffer)))
  (let ((buffer (current-buffer)))
    (execute-kbd-macro (kbd keys))
    (should (eq (window-buffer) buffer))
    (set-buffer buffer)))

;;;; Grouping

(ert-deftest drover-list-test-groups-by-status ()
  (drover-list-test-with (drover-list-test-sample)
    (should (equal (drover-list-test-groups)
                   '((waiting "wait1" "wait2") (done "done") (working "work") (idle "idle"))))
    (should (seq-every-p (lambda (group) (equal (car (oref group value)) 'status))
                         (drover-list--groups)))
    (drover-list-test-goto-group 'waiting)
    (should (equal (drover-list-test-line) "Needs you (2)"))
    (drover-list-test-goto-group 'idle)
    (should (equal (drover-list-test-line) "Idle (1)"))))

(ert-deftest drover-list-test-empty-groups-omitted ()
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (should (equal (mapcar #'car (drover-list-test-groups)) '(working)))
    (let ((case-fold-search nil))
      (should-not (string-match-p "^\\(Needs you\\|Failed\\|Done\\|Retrying\\|Idle\\)" (buffer-string))))))

(ert-deftest drover-list-test-groups-by-project ()
  (drover-list-test-with (append (drover-list-test-sample)
                                 (list (drover-test-session "fail" :status 'failed
                                                            :since (drover-list-test-ago 5)
                                                            :project "p" :branch "main")))
    (drover-list-test-shown
      (drover-set-group-by 'project)
      (should (equal (drover-list-test-groups)
                     '(("q" "wait1" "work") ("p" "wait2" "fail" "idle") (nil "done"))))
      (drover-list-test-goto-group "p")
      (should (equal (drover-list-test-line) "p (3)  !1 ✗1"))
      (drover-list-test-goto-group nil)
      (should (equal (drover-list-test-line) "No project (1)  ✓1"))
      (should (string-prefix-p "  ! Session wait2" (drover-list-test-heading "wait2")))
      (should (string-prefix-p "  · Session idle" (drover-list-test-heading "idle")))
      ;; The heading names the project; main, the default branch, greyed out.
      (should (string-match-p "Session fail +main +5s\\'" (drover-list-test-heading "fail")))
      (drover-set-group-by 'status)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting failed done working idle))))))

(ert-deftest drover-list-test-groups-by-other-fields ()
  (drover-list-test-with
      (list (drover-test-session "a" :status 'working :agent "build" :model "opus" :branch "main"
                                 :updated (drover-list-test-ago 60))
            ;; Waiting, b's last activity is its question, three days old.
            (drover-test-session "b" :status 'waiting :agent "plan" :since (drover-list-test-ago (* 3 86400))
                                 :updated (drover-list-test-ago (* 3 86400)))
            (drover-test-session "c" :status 'idle :model "opus"))
    (let ((zone (getenv "TZ")))
      (unwind-protect
          (drover-list-test-shown
            (set-time-zone-rule "UTC")
            (drover-set-group-by 'agent)
            (should (equal (drover-list-test-groups) '(("plan" "b") ("build" "a") (nil "c"))))
            (drover-list-test-goto-group nil)
            (should (equal (drover-list-test-line) "No agent (1)"))
            (drover-list-test-goto-group "plan")
            (should (equal (drover-list-test-line) "plan (1)  !1"))
            (drover-set-group-by 'model)
            (should (equal (drover-list-test-groups) '((nil "b") ("opus" "a" "c"))))
            (drover-set-group-by 'branch)
            (should (equal (drover-list-test-groups) '((nil "b" "c") ("main" "a"))))
            (drover-list-test-goto-group nil)
            (should (equal (drover-list-test-line) "No branch (2)  !1"))
            (drover-set-group-by 'updated)
            (should (equal (drover-list-test-groups) '((today "a") (week "b") (nil "c"))))
            (should (equal (mapcar (lambda (value) (drover-list-test-goto-group value) (drover-list-test-line))
                                   '(today week nil))
                           '("Today (1)" "This week (1)  !1" "No activity (1)")))
            (should (equal (drover-list--mode-line-process) " by last activity")))
        (set-time-zone-rule zone)))))

(ert-deftest drover-list-test-group-by-provider ()
  "Grouped by provider, each group is named by the provider, after its icon."
  (drover-list-test-with (list (drover-test-session "a" :status 'working :provider "P" :provider-name "Prov")
                               (drover-test-session "b" :status 'working :provider-name "Acme")
                               (drover-test-session "c" :status 'working))
    (drover-list-test-shown
      (drover-set-group-by 'provider)
      (should (equal (drover-list-test-groups) '(("Prov" "a") ("Acme" "b") (nil "c"))))
      (should (equal (mapcar (lambda (value) (drover-list-test-goto-group value) (drover-list-test-line))
                             '("Prov" "Acme" nil))
                     '("P Prov (1)" "Acme (1)" "No provider (1)")))
      (drover-list-test-goto-group "Prov")
      (should (eq (get-text-property (point) 'font-lock-face) 'drover-heading))
      (should (equal (drover-list--mode-line-process) " by provider")))))

(ert-deftest drover-list-test-follows-group-by ()
  "A list drawn anew groups sessions as `drover-group-by' says."
  (drover-list-test-with (list (drover-test-session "s" :project "p"))
    (setq drover-group-by 'project)
    (revert-buffer)
    (should (equal (drover-list-test-groups) '(("p" "s"))))
    (setq drover-group-by nil)
    (revert-buffer)
    (should-not (drover-list-test-groups))
    (should (equal (drover-list-test-ungrouped) '("s")))))

(ert-deftest drover-list-test-ungrouped ()
  (drover-list-test-with (append (drover-list-test-family)
                                 (list (drover-test-session "idle" :status 'idle)))
    (setq drover-group-by nil)
    (revert-buffer)
    ;; Trees most urgent first, each subagent after its parent.
    ;; The subagent that ended and the quiet one are hidden; root tells
    ;; that a subagent of its waits for you.
    (should (equal (drover-list-test-ungrouped) '("root" "sub" "other" "idle")))
    (should (seq-every-p (lambda (section) (eq (oref section parent) magit-root-section))
                         (drover-list--session-sections magit-root-section)))
    (should (equal (drover-list-test-visible-text)
                   (concat "  ◐ ? Session root   10s\n"
                           "      ! Session sub   1m\n"
                           "  ◐ Session other     5s\n"
                           "  · Session idle\n")))
    ;; Sessions still show their details.
    (drover-list-test-goto "idle")
    (drover-list-test-press "TAB")
    (should (string-match-p "^    Prompt +Prompt of idle$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-ungrouped-refresh-takes-next-session ()
  (drover-list-test-with (drover-list-test-sample)
    (setq drover-group-by nil)
    (drover-list-test-refresh)
    (should (equal (drover-list-test-ungrouped) '("wait1" "wait2" "done" "work" "idle")))
    (drover-list-test-goto "wait2")
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait2"))
                                           drover-test-sessions))
    (drover-list-test-refresh)
    ;; The session that took its place in the list.
    (should (equal (drover-list-test-id-at-point) "done"))))

(ert-deftest drover-list-test-group-by-keeps-point ()
  "Changing the grouping of every view redraws the list, point kept."
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-goto "sub")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      (drover-list-test-shown
        (drover-set-group-by 'project)
        (should (equal (drover-list-test-groups) '((nil "root" "sub" "other"))))
        (should (equal (drover-list--mode-line-process) " by project"))
        (should (equal (drover-list-test-id-at-point) "sub"))
        (should (= (- (point) (line-beginning-position)) offset))
        (drover-set-group-by nil)
        (should-not (drover-list-test-groups))
        (should (equal (drover-list-test-ungrouped) '("root" "sub" "other")))
        (should (equal (drover-list--mode-line-process) " ungrouped"))
        (should (equal (drover-list-test-id-at-point) "sub"))
        (should (= (- (point) (line-beginning-position)) offset))
        (drover-set-group-by 'status)
        (should (equal (drover-list-test-groups) '((waiting "root" "sub") (working "other"))))
        (should (equal (drover-list--mode-line-process) " by status"))
        (should (equal (drover-list-test-id-at-point) "sub"))
        (should (= (- (point) (line-beginning-position)) offset))))))

(ert-deftest drover-list-test-sorted-within-groups ()
  "Sorting orders the trees of each group, subagents under their parent."
  (drover-list-test-with
      (list (drover-list-test-titled "w1" "Zebra" :status 'waiting :since (drover-list-test-ago 90))
            (drover-list-test-titled "w2" "Apple" :status 'waiting :since (drover-list-test-ago 60))
            (drover-list-test-titled "kid2" "beta" :parent "w2" :status 'idle)
            (drover-list-test-titled "kid1" "Alpha" :parent "w2" :status 'idle)
            (drover-list-test-titled "i" "Mango" :status 'idle))
    (drover-list-test-shown
      (setq drover-show-subagents 'all)
      (drover-set-sort 'title)
      (should (equal (drover-list-test-groups) '((waiting "w2" "kid1" "kid2" "w1") (idle "i"))))
      (should (equal (drover-list--mode-line-process) " by status, title ↑ +subagents"))
      (drover-set-sort 'title t)
      (should (equal (drover-list-test-groups) '((waiting "w1" "w2" "kid2" "kid1") (idle "i"))))
      (should (string-prefix-p "    · Session" (replace-regexp-in-string "beta" "Session"
                                                                         (drover-list-test-heading "kid2"))))
      (drover-set-sort 'urgency)
      (should (equal (drover-list-test-groups) '((waiting "w1" "w2" "kid2" "kid1") (idle "i")))))))

(ert-deftest drover-list-test-no-summary-line ()
  "The list begins with its sessions: no line counts them, nor what needs the user.
The mode line counts what needs the user, the group headings their
sessions; which subagents show, the View menu tells."
  (drover-list-test-with (append (drover-list-test-sample)
                                 (list (drover-test-session "fail" :status 'failed)
                                       (drover-test-session "sub" :parent "work")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Needs you (2)"))
    (forward-line 1)
    (should (equal (drover-list-test-id-at-point) "wait1"))
    (dolist (text '("needs you " "sessions" "subagent" "hidden" "Nothing needs you"))
      (should-not (string-search text (buffer-string))))
    ;; Ungrouped, the first line is the most urgent session's.
    (setq drover-group-by nil)
    (revert-buffer)
    (goto-char (point-min))
    (should (equal (drover-list-test-id-at-point) "wait1"))
    (should (string-prefix-p "  ! Session wait1" (drover-list-test-line)))
    ;; Without any session, the list says so, and nothing else.
    (setq drover-test-sessions nil)
    (drover-list-test-refresh)
    (should (equal (buffer-string) "No sessions.\n"))))

(ert-deftest drover-list-test-no-settings-or-keys ()
  "The list shows sessions and their counts; the View menu shows settings and keys."
  (drover-list-test-with (append (drover-list-test-family)
                                 (list (drover-test-session "x" :status 'waiting :project "p")))
    (dolist (mode drover-subagent-modes)
      (dolist (filter '(nil (:project "p") (:search "nowhere")))
        (setq drover-show-subagents mode
              drover-filter filter)
        (revert-buffer)
        (let ((text (buffer-string)))
          (should-not (text-property-any (point-min) (point-max) 'font-lock-face 'help-key-binding))
          (should-not (text-property-any (point-min) (point-max) 'face 'help-key-binding))
          (dolist (word '("Subagents:" "Filter" "clears" "hides" "shows" " U " "V c"))
            (should-not (string-search word text))))))))

;;;; Subagents

(ert-deftest drover-list-test-subagents-under-parent ()
  (drover-list-test-with (drover-list-test-family)
    ;; The tree of root goes with its waiting subagent; quiet, and deep,
    ;; whose turn ended, are hidden.
    (should (equal (drover-list-test-groups)
                   '((waiting "root" "sub") (working "other"))))
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (should (equal (drover-list-test-groups)
                   '((waiting "root" "sub" "deep") (working "other" "quiet"))))
    ;; Each subagent's glyph stands under its parent's title, past the
    ;; glyph telling its active subagents; ages line up past the
    ;; deepest title.
    (should (equal (drover-list-test-heading "root") "  ◐ ? Session root      10s"))
    (should (equal (drover-list-test-heading "sub") "      ! Session sub      1m"))
    (should (equal (drover-list-test-heading "deep") "        ✓ Session deep  30s"))
    (should (equal (drover-list-test-heading "other") "  ◐ Session other        5s"))
    (should (equal (drover-list-test-heading "quiet") "    · Session quiet"))
    (let ((margin (car (car (drover-list-test-cells "root")))))
      (should (= margin (* drover-list--margin (drover-list--char-width))))
      (should (= (car (car (drover-list-test-cells "sub"))) (drover-list-test-title-x "root" "Session")))
      (should (= (car (car (drover-list-test-cells "deep"))) (drover-list-test-title-x "sub" "Session")))
      (should (= (car (car (drover-list-test-cells "other"))) margin))
      (should (= (car (car (drover-list-test-cells "quiet"))) (drover-list-test-title-x "other" "Session"))))
    (should (eq (oref (drover-list-test-section "sub") parent)
                (oref (drover-list-test-section "root") parent)))))

(ert-deftest drover-list-test-nesting-depth-capped ()
  (drover-list-test-with
      (cons (drover-test-session "d0" :status 'working)
            (mapcar (lambda (depth)
                      (drover-test-session (format "d%d" depth) :status 'working
                                           :parent (format "d%d" (1- depth))))
                    (number-sequence 1 5)))
    (setq drover-show-children t)
    (revert-buffer)
    ;; Each glyph stands under its parent's title, past the parent's
    ;; glyph and that of its working subagents, "◐ * "; deeper than
    ;; three levels, subagents line up with the third.
    (should (equal (mapcar (lambda (depth) (car (car (drover-list-test-cells (format "d%d" depth)))))
                           (number-sequence 0 5))
                   '(2 6 10 14 14 14)))))

(ert-deftest drover-list-test-tree-in-group-of-urgent-subagent ()
  (drover-list-test-with
      (list (drover-test-session "parent" :status 'done :since (drover-list-test-ago 50))
            (drover-test-session "asking" :parent "parent" :status 'waiting
                                 :since (drover-list-test-ago 30))
            (drover-test-session "alone" :status 'waiting :since (drover-list-test-ago 40)))
    ;; Within the group, trees rank by their oldest wait.
    (should (equal (drover-list-test-groups) '((waiting "alone" "parent" "asking"))))
    (drover-list-test-goto-group 'waiting)
    (should (equal (drover-list-test-line) "Needs you (3)"))
    (should (string-prefix-p "  ✓ ? Session parent" (drover-list-test-heading "parent")))))

(ert-deftest drover-list-test-orphan-subagent ()
  (drover-list-test-with (list (drover-test-session "child" :parent "gone" :status 'working))
    (should (equal (drover-list-test-groups) '((working "child"))))
    ;; Its parent is not listed: it shows at the top level, marked.
    (should (string-prefix-p "  ◐ ↳ Session child" (drover-list-test-heading "child")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Working (1)"))
    (drover-list-test-goto "child")
    (magit-section-show (magit-current-section))
    ;; Under its title, the arrow included.
    (should (string-match-p "^    Prompt +Prompt of child$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-orphan-subagent-follows-mode ()
  "A subagent whose parent is not listed shows as `drover-show-subagents' says.
Its parent archived alone, it is a subagent still: active subagents
only, it leaves the list once its turn ended, and comes back with the
next, drawn in place as drawn anew; all of them, it shows idle too."
  (let ((root (drover-list-test-titled "root" "A title longer than the subagent's" :status 'working)))
    (drover-list-test-with (list root (drover-test-session "child" :parent "archived" :status 'working))
      (drover-list-test-with-font
        (drover-list-test-shown
          (revert-buffer)
          (should (equal (drover-list-test-groups) '((working "root" "child"))))
          (dolist (status '(idle working idle))
            (setq drover-test-sessions (list root (drover-test-session "child" :parent "archived"
                                                                       :status status)))
            (drover-backend-changed 'fake "child")
            (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
            (should (equal (drover-list-test-groups)
                           (if (eq status 'idle) '((working "root")) '((working "root" "child")))))
            (drover-list-test-is-fresh))
          (drover-set-show-subagents 'all)
          (should (equal (drover-list-test-groups) '((working "root") (idle "child"))))
          (drover-set-show-subagents 'none)
          (should (equal (drover-list-test-groups) '((working "root")))))))))

(ert-deftest drover-list-test-subagents-toggle ()
  "=u cycles every view through active, no and all subagents, redrawn at once.
The View menu of the list holds it, in its View group."
  (drover-list-test-with (list (drover-test-session "root" :status 'working)
                               (drover-test-session "child" :parent "root" :status 'working)
                               (drover-test-session "ended" :parent "root" :status 'done
                                                    :since (drover-list-test-ago 5))
                               (drover-test-session "asking" :parent "root" :status 'waiting))
    ;; Active subagents only: the one that ended is hidden, and does not
    ;; place its parent among the finished ones.
    (should (equal (drover-list-test-groups) '((waiting "root" "asking" "child"))))
    (drover-list-test-goto "root")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      (drover-list-test-shown
        (should-not (eq (key-binding (kbd "U")) #'drover-toggle-subagents))
        (drover-list-test-type "V = u C-g")
        ;; The setting is global, not the list's own.
        (should-not (local-variable-p 'drover-show-subagents))
        (should (eq drover-show-subagents 'none))
        (with-temp-buffer
          (should (eq drover-show-subagents 'none)))
        ;; No subagent: the parent ranks by itself.
        (should (equal (drover-list-test-groups) '((working "root"))))
        (should (equal (drover-list--mode-line-process) " by status -subagents"))
        (should (equal (drover-list-test-id-at-point) "root"))
        (should (= (- (point) (line-beginning-position)) offset))
        (save-excursion
          (goto-char (point-min))
          (should (equal (drover-list-test-line) "Working (1)")))
        (drover-list-test-type "V = u C-g")
        (should (eq drover-show-subagents 'all))
        (should (equal (drover-list-test-groups) '((waiting "root" "asking" "ended" "child"))))
        (should (string-prefix-p "        ◐ Session child" (drover-list-test-heading "child")))
        (should (equal (drover-list--mode-line-process) " by status +subagents"))
        (should (equal (drover-list-test-id-at-point) "root"))
        (save-excursion
          (goto-char (point-min))
          (should (equal (drover-list-test-line) "Needs you (4)")))
        (drover-list-test-type "V = u C-g")
        (should (eq drover-show-subagents 'active))
        (should (equal (drover-list-test-groups) '((waiting "root" "asking" "child"))))
        (should (equal (drover-list-test-id-at-point) "root"))
        (should (= (- (point) (line-beginning-position)) offset))))))

(ert-deftest drover-list-test-parent-details-keep-subagents ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (drover-list-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-list-test-section-hidden-p "root"))
    ;; The details come between the parent and its subagents.
    (should (string-match-p "  ◐ \\? Session root +10s\n      Prompt +Prompt of root\n\\(.*\n\\)*      ! Session sub"
                            (drover-list-test-visible-text)))
    (should (< (oref (drover-list-test-section "root") end)
               (1+ (oref (drover-list-test-section "sub") start))))
    (drover-list-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should (drover-list-test-section-hidden-p "root"))
    ;; Hiding them leaves the subagents in sight.
    (should (drover-list-test-heading-visible-p "sub"))
    (should (drover-list-test-heading-visible-p "deep"))
    (should-not (string-match-p "Prompt of root" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-nested-collapse-state-across-refresh ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (drover-list-test-refresh)
    (drover-list-test-goto "sub")
    (magit-section-show (magit-current-section))
    ;; The tree moves from Needs you to Failed.
    (setf (drover-session-status (drover-list-test-find "sub")) 'failed)
    (drover-list-test-refresh)
    (should (equal (drover-list-test-groups)
                   '((failed "root" "sub" "deep") (working "other" "quiet"))))
    (should-not (drover-list-test-section-hidden-p "sub"))
    (should (drover-list-test-section-hidden-p "root"))
    (should (drover-list-test-section-hidden-p "deep"))
    (should (string-match-p "^      Prompt +Prompt of sub$" (drover-list-test-visible-text)))
    ;; And when the parent's details open and close around it.
    (drover-list-test-goto "root")
    (magit-section-show (magit-current-section))
    (drover-list-test-goto "root")
    (magit-section-hide (magit-current-section))
    (drover-list-test-refresh)
    (should-not (drover-list-test-section-hidden-p "sub"))))

(ert-deftest drover-list-test-refresh-follows-nested-session ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (drover-list-test-refresh)
    (drover-list-test-goto "deep")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      ;; A new tree above, and deep's tree moves to Failed.
      (push (drover-test-session "new" :status 'waiting) drover-test-sessions)
      (setf (drover-session-status (drover-list-test-find "sub")) 'failed)
      (drover-list-test-refresh)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting failed working)))
      (should (equal (drover-list-test-id-at-point) "deep"))
      (should (= (- (point) (line-beginning-position)) offset)))))

(ert-deftest drover-list-test-project-grouping-nests-subagents ()
  (drover-list-test-with
      (list (drover-test-session "root" :project "p" :branch "main" :status 'working)
            (drover-test-session "elsewhere" :parent "root" :project "q" :branch "dev"
                                 :status 'waiting :since (drover-list-test-ago 20))
            (drover-test-session "here" :parent "root" :project "p" :branch "feat"
                                 :status 'waiting :since (drover-list-test-ago 10)))
    (setq drover-group-by 'project)
    (revert-buffer)
    ;; A subagent stays with its parent, whatever its own project.
    (should (equal (drover-list-test-groups) '(("p" "root" "elsewhere" "here"))))
    (drover-list-test-goto-group "p")
    (should (equal (drover-list-test-line) "p (3)  !2"))
    ;; The group names the project; main, the default branch, greyed out.
    (should (string-match-p "Session root +main\\'" (drover-list-test-heading "root")))
    (should (string-match-p "Session here +feat +10s\\'" (drover-list-test-heading "here")))
    ;; Another project than the group's is named.
    (should (string-match-p "Session elsewhere +q +dev +20s\\'" (drover-list-test-heading "elsewhere")))))

;;;; Lines

(ert-deftest drover-list-test-session-line ()
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 300)
                                 :project "drover" :branch "main"
                                 :requests '((:kind permission :summary "bash: make"))))
    ;; On the default branch, greyed out.
    (should (equal (drover-list-test-heading "s") "  ! Session s  drover   main     5m  permit: bash: make"))
    (drover-list-test-goto "s")
    ;; Past the invisible character beginning the line, and its aligned space.
    (let ((glyph (+ 2 (point))))
      (should (get-text-property (point) 'invisible))
      (should (equal (buffer-substring-no-properties glyph (1+ glyph)) "!"))
      (should (eq (get-text-property glyph 'font-lock-face) 'opencode-session-status-waiting)))
    (search-forward "drover")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (search-forward "5m")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (should (equal (get-text-property (match-beginning 0) 'help-echo)
                   "Session s\ndrover:main\npermit: bash: make"))))

(ert-deftest drover-list-test-idle-titles-dimmed ()
  (drover-list-test-with (list (drover-test-session "i" :status 'idle)
                               (drover-test-session "w" :status 'working))
    (setq drover-list-collapsed-groups nil)
    (revert-buffer)
    (drover-list-test-goto "i")
    (search-forward "Session i")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (drover-list-test-goto "w")
    (search-forward "Session w")
    (should-not (get-text-property (match-beginning 0) 'font-lock-face))))

(ert-deftest drover-list-test-wide-glyph-widens-every-cell ()
  "Every status glyph takes the room of the widest, so that titles line up."
  (drover-list-test-with (drover-list-test-family)
    (setq-local opencode-session-status-icons (cons '(waiting . "待") opencode-session-status-icons))
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (should (equal (plist-get (drover-list--metrics 0) :status-cell) '(0 . 2)))
    ;; The narrower glyphs are padded to its width, and a subagent's
    ;; glyph stands under its parent's title.
    (should (string-prefix-p "  ◐  ? Session root" (drover-list-test-heading "root")))
    (should (string-prefix-p "       待 Session sub" (drover-list-test-heading "sub")))
    (should (string-prefix-p "          ✓  Session deep" (drover-list-test-heading "deep")))
    (should (string-prefix-p "  ◐  Session other" (drover-list-test-heading "other")))))

(ert-deftest drover-list-test-one-space-after-glyph ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (dolist (id '("root" "sub" "deep" "other" "quiet"))
      (let ((status (cdr (car (drover-list-test-cells id)))))
        ;; The glyph, one plain space, then the title, or the glyph of
        ;; active subagents, one plain space, then the title.
        (should (string-match-p "\\`[!✓◐·] \\(?:[?*] \\)?Session" status))
        (should-not (get-text-property 1 'display status))
        (should-not (get-text-property (1- (string-search "Session" status)) 'display status))))
    (should (string-prefix-p "◐ ? Session" (cdr (car (drover-list-test-cells "root")))))))

(ert-deftest drover-list-test-columns-align ()
  (drover-list-test-with
      (list (drover-list-test-titled "a" "Short" :status 'working :project "p" :branch "b"
                                     :updated (drover-list-test-ago 5))
            (drover-list-test-titled "b" "A longer title" :status 'working :project "project"
                                     :updated (drover-list-test-ago 7200)
                                     :detail "Thinking"))
    (let ((a (drover-list-test-heading "a"))
          (b (drover-list-test-heading "b")))
      ;; The project, then the branch, in a column of its own.
      (should (string-prefix-p "  ◐ Short           p        b  " a))
      (should (= (string-search " p " a) (1- (string-search "project" b))))
      (should (= (string-search "5s" a) (string-search "2h" b)))
      (should (string-suffix-p "5s" a))
      (should (string-suffix-p "2h  Thinking" b)))))

(ert-deftest drover-list-test-pixel-alignment ()
  "Columns line up in pixels with characters of different widths."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "a" "mmmm" :status 'working :project "iii"
                                       :updated (drover-list-test-ago 5) :detail "Busy")
              (drover-list-test-titled "b" "iiiiiiii" :status 'working :project "mm" :branch "x"
                                       :updated (drover-list-test-ago 7200))
              (drover-list-test-titled "c" "Sub" :parent "b" :status 'waiting :project "p"
                                       :since (drover-list-test-ago 30) :detail "Asks"))
      (should (equal (drover-list-test-groups) '((waiting "b" "c") (working "a"))))
      (let* ((metrics (drover-list--metrics 0))
             (gap (plist-get metrics :gap))
             (margin (plist-get metrics :margin))
             (cells (mapcar (lambda (id) (cons id (drover-list-test-cells id))) '("a" "b" "c")))
             (title-ends (mapcar (lambda (entry)
                                   (let ((cell (cadr entry)))
                                     (+ (car cell) (drover-list-test-fake-width (cdr cell)))))
                                 cells))
             (location (+ (apply #'max title-ends) gap)))
        (should (= (drover-list-test-fake-width "x") (plist-get metrics :char)))
        ;; Status glyphs: roots at the margin, the subagent under its
        ;; parent's title.
        (should (= (car (nth 1 (assoc "a" cells))) margin))
        (should (= (car (nth 1 (assoc "b" cells))) margin))
        (should (= (car (nth 1 (assoc "c" cells))) (drover-list-test-title-x "b" "iiiiiiii")))
        ;; Locations begin at one position, past the widest title.
        (should (= (drover-list-test-cell-at "a" "iii") location))
        (should (= (drover-list-test-cell-at "b" "mm") location))
        (should (= (drover-list-test-cell-at "c" "p") location))
        ;; Ages end at one position.
        (let ((ends (mapcar (lambda (entry)
                              (let ((cell (seq-find (lambda (cell)
                                                      (string-match-p "\\`[0-9]+[smhd]\\'" (cdr cell)))
                                                    (cdr entry))))
                                (+ (car cell) (drover-list-test-fake-width (cdr cell)))))
                            cells)))
          (should (= (length (seq-uniq ends)) 1))
          ;; Summaries begin at one position, past the ages.
          (should (= (drover-list-test-cell-at "a" "Busy") (+ (car ends) gap)))
          (should (= (drover-list-test-cell-at "c" "Asks") (+ (car ends) gap))))))))

(ert-deftest drover-list-test-truncate-in-pixels ()
  (drover-list-test-with-font
    (let ((dim (drover-face "mmmmii" 'drover-dim)))
      (should (eq (drover-list--truncate dim 100) dim))
      ;; m m and the ellipsis: 12 + 12 + 7.
      (let ((cut (drover-list--truncate dim 33)))
        (should (equal cut "mm…"))
        (should (<= (drover-list-test-fake-width cut) 33))
        (should (eq (get-text-property 2 'face cut) 'drover-dim)))
      (should (equal (drover-list--truncate "ab cd" 22) "ab…"))
      (should (equal (drover-list--truncate "abc" 5) ""))
      (should (equal (drover-list--truncate "abc" -5) ""))
      (should (equal (drover-list--truncate "" 0) "")))))

(ert-deftest drover-list-test-cut-measures-little ()
  "A cut lays out few candidates, keeps none of them, and the one it returns."
  (with-temp-buffer
    (let ((drover-list--widths nil)
          (measured nil))
      (cl-letf (((symbol-function 'drover-list--measure)
                 (lambda (string) (push (substring-no-properties string) measured) (string-width string))))
        (let ((title (drover-face (make-string 40 ?m) 'drover-dim))
              (cut (concat (make-string 9 ?m) "…")))
          (should (equal (drover-list--truncate title 10) cut))
          ;; The text whole, then ten, eight and nine characters: neither
          ;; the empty prefix, which fits, nor the result is laid out again.
          (should (equal (reverse measured)
                         (list (make-string 40 ?m) (concat (make-string 10 ?m) "…")
                               (concat (make-string 8 ?m) "…") cut)))
          ;; No candidate is kept, but what the text is cut to is.
          (should-not (gethash (drover-face cut 'drover-dim) (nth 1 drover-list--widths)))
          (setq measured nil)
          (should (equal (drover-list--truncate title 10) cut))
          (should-not measured)
          ;; Not even the ellipsis fits: nothing, found last.
          (should (equal (drover-truncate-to-pixels "abc" 0 #'string-width) ""))
          (should (equal (drover-truncate-to-pixels "abc" 1 #'string-width) "…")))))))

(ert-deftest drover-list-test-lines-share-their-parts ()
  "A text on one line is used as it is, and aligned spaces are made once per position."
  (let ((text "Already one line"))
    (should (eq (drover-list--one-line text) text))
    (should (equal (drover-list--one-line " two\nlines ") "two lines"))
    (should (equal (drover-list--one-line nil) ""))
    (should (equal (drover-list--one-line 'symbol) "symbol")))
  (should (eq (drover-list--align 120) (drover-list--align 120)))
  (should (equal (get-text-property 0 'display (drover-list--align 120)) '(space :align-to (120)))))

(ert-deftest drover-list-test-titles-truncated-in-pixels ()
  (drover-list-test-with-font
    (drover-list-test-with (list (drover-list-test-titled "s" (make-string 60 ?m) :status 'working))
      (let* ((status (cdr (car (drover-list-test-cells "s"))))
             (title (substring status 2)))
        (should (string-suffix-p "…" title))
        ;; Forty characters as wide as x, at most.
        (should (<= (drover-list-test-fake-width title) (* 40 7)))
        (should (> (drover-list-test-fake-width (concat "m" title)) (* 40 7)))))))

(ert-deftest drover-list-test-detail-summary ()
  (drover-list-test-with (list (drover-test-session "s" :status 'failed :detail "Rate limited"))
    (should (string-match-p "Session s +Rate limited\\'" (drover-list-test-heading "s")))))

(ert-deftest drover-list-test-long-columns-truncated ()
  (drover-list-test-with
      (list (drover-list-test-titled "s" (make-string 60 ?t) :status 'waiting
                                     :project (make-string 50 ?p)))
    (should (equal (drover-list-test-heading "s")
                   (concat "  ! " (make-string 39 ?t) "…  " (make-string 29 ?p) "…")))))

(ert-deftest drover-list-test-summary-fits-window ()
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :project "p"
                                 :requests `((:kind question :summary ,(make-string 200 ?q)))))
    (let ((line (drover-list-test-heading "s")))
      ;; In batch, the selected window is on a text terminal.
      (should (= drover-list--width (window-max-chars-per-line)))
      ;; Magit's ellipsis takes the last column.
      (should (= (string-width line) (1- drover-list--width)))
      (should (string-match-p "  asks: q+…\\'" line)))))

(ert-deftest drover-list-test-one-line-headings ()
  (drover-list-test-with
      (list (drover-list-test-titled "s" "Two\nlines" :status 'waiting
                                     :requests '((:kind question :summary "First\n\nsecond"))))
    ;; A title is its first line, a summary all of its lines on one.
    (should (string-match-p "Two .*asks: First second\\'" (drover-list-test-heading "s")))))

;;;; Width

(defun drover-list-test-rich ()
  "Return two sessions with something to show in every field.
A works, its prompt cache four fifths full, on a model, effort and
provider, its context a quarter full; B asks a question."
  (list (drover-list-test-titled "a" "Alpha" :status 'working :project "drover" :branch "main"
                                 :agent "build" :model "opus" :effort "high"
                                 :provider "P" :provider-name "Prov"
                                 :context-tokens 50000 :context-limit 200000
                                 :created (drover-list-test-ago 86400)
                                 :updated (drover-list-test-ago 300) :detail "Busy"
                                 :cache-expires (+ drover-test-now 240) :cache-ttl 300)
        (drover-list-test-titled "b" "Bravo" :status 'waiting :since (drover-list-test-ago 60)
                                 :requests '((:kind question :summary "Which file?")))))

(defun drover-list-test-layout-at (width)
  "Return the layout of the list drawn WIDTH pixels wide, and its fields."
  (let ((drover-list-test-width width))
    (revert-buffer)
    (cons drover-list--layout drover-list--fields)))

(ert-deftest drover-list-test-fields-by-width ()
  "The wider the window, the more fields show, by priority.
With the fake font, an x is 7 pixels: the margin and the room before
titles take 25 pixels, and each field its :min characters, or the room
of its text when it is never cut, and a gap of 14 pixels, but the
title; Magit's ellipsis takes 7 more."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      ;; Title 109, age 144, cache 223 (its bar, a space and 59m, 65
      ;; pixels, past its 9 characters), summary 321, model 391, context
      ;; 468, provider 496, location 580, branch 636, agent 685, created
      ;; 720.
      (should (equal (drover-list-test-layout-at 730)
                     '(columns title location branch agent model provider context created age cache summary)))
      (should (equal (drover-list-test-layout-at 720)
                     '(columns title location branch agent model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 680)
                     '(columns title location branch model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 640)
                     '(columns title location model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 580)
                     '(columns title model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 500)
                     '(columns title model context age cache summary)))
      (should (equal (drover-list-test-layout-at 470)
                     '(columns title model age cache summary)))
      (should (equal (drover-list-test-layout-at 330)
                     '(columns title age cache summary)))
      ;; Windows of 75, 100 and 105 columns as wide as an x, as the
      ;; README says.
      (should (equal (drover-list-test-layout-at (* 75 7))
                     '(columns title model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at (* 100 7))
                     '(columns title location branch agent model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at (* 105 7))
                     '(columns title location branch agent model provider context created age cache summary)))
      ;; Too narrow for the summary in a column: two lines, where
      ;; neither the model nor its provider shows, the context as a
      ;; pie alone; columns need not show it.
      (should (equal (drover-list-test-layout-at 320) '(narrow title context age cache summary)))
      (should (equal (drover-list-test-layout-at 120) '(narrow title context age cache summary))))))

(ert-deftest drover-list-test-fields-by-width-with-the-harness ()
  "The harness's column, after the summary's, needs the room of its icon.
As by default, every field let show: windows of 80 columns as wide as
an x, of 100 and of 108 show what the README says."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-showing
      (drover-list-test-with-font
        (should (equal (drover-list-test-layout-at (* 75 7))
                       '(columns title harness model context age cache summary)))
        (should (equal (drover-list-test-layout-at (* 80 7))
                       '(columns title harness model provider context age cache summary)))
        (should (equal (drover-list-test-layout-at (* 100 7))
                       '(columns title harness location branch model provider context age cache summary)))
        (should (equal (drover-list-test-layout-at (* 108 7))
                       '(columns title harness location branch agent model provider context created age cache summary)))
        ;; Two lines: its icon on the title's line.
        (should (equal (drover-list-test-layout-at 320) '(narrow title harness context age cache summary)))))))

(ert-deftest drover-list-test-fields-grow-with-width ()
  "Every field shown at a width shows at any greater width, in columns."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (let ((previous nil)
            (columns-from nil))
        (dolist (width (number-sequence 100 1000 10))
          (pcase-let ((`(,layout . ,fields) (drover-list-test-layout-at width)))
            (when (eq layout 'columns)
              (setq columns-from (or columns-from width))
              (when previous
                (should (seq-every-p (lambda (field) (memq field fields)) previous)))
              (setq previous fields))
            ;; Narrow below a width, columns above it.
            (should (eq layout (if (and columns-from (>= width columns-from)) 'columns 'narrow)))))
        (should (= columns-from 330))))))

(ert-deftest drover-list-test-fields-without-content-take-no-room ()
  "A field no session has anything for is left out, leaving its room.
But the summary, which any session may come to need, keeps its column."
  (drover-list-test-with (list (drover-list-test-titled "a" "Alpha" :status 'working :model "opus"
                                                        :updated (drover-list-test-ago 300)))
    (drover-list-test-with-font
      ;; Neither cache, location nor agent: the summary comes after the
      ;; age, 109 + 35 + 98 = 242, then the model, 70 more, Magit's
      ;; ellipsis taking 7 pixels.
      (should (equal (drover-list-test-layout-at 330) '(columns title model age summary)))
      (should (equal (drover-list-test-layout-at 250) '(columns title age summary)))
      (should (equal (drover-list-test-layout-at 240) '(narrow title age summary))))))

(ert-deftest drover-list-test-fields-are-data ()
  "Priorities and minimum widths come from `drover-list-fields'."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (let ((drover-list-fields (append '((model :priority 1 :min 4))
                                        (assq-delete-all 'model (copy-alist drover-list-fields)))))
        ;; The model now comes second, and first in the line.  It needs
        ;; the room of an ellipsis and its effort, "… high", 35 pixels,
        ;; more than its :min: 109 + 49 + 35 + 79 + 98 = 370.
        (should (equal (drover-list-test-layout-at 380) '(columns model title age cache summary)))
        (should (equal (drover-list-test-layout-at 370) '(narrow title context age cache summary)))
        (drover-list-test-layout-at 380)
        (let ((cells (drover-list-test-cells "a")))
          (should (equal (car cells) (cons 14 "opus high")))
          ;; The title's column follows, past the widest model and a gap.
          (should (equal (nth 1 cells) (cons (+ 14 56 14) "◐ Alpha")))))
      ;; A field the narrow layout does not place never forces two lines.
      (let ((drover-list-fields (mapcar (lambda (entry)
                                          (if (eq (car entry) 'summary)
                                              (list 'summary :priority 3 :min 12)
                                            entry))
                                        drover-list-fields)))
        (should (equal (drover-list-test-layout-at 300) '(columns title age cache)))))))

(ert-deftest drover-list-test-narrow-layout ()
  "Narrow lines show the glyph, title and age, and what a session waits for below."
  (drover-list-test-with (append (drover-list-test-family)
                                 (list (drover-test-session "ask" :status 'waiting
                                                            :since (drover-list-test-ago 180)
                                                            :requests '((:kind question :summary "Which branch?")))
                                       (drover-test-session "busy" :status 'working
                                                            :updated (drover-list-test-ago 40)
                                                            :detail "Thinking hard")))
    (setq drover-show-subagents 'all)
    (drover-list-test-shown
      (save-window-excursion
        (let ((window (split-window-right -30)))
          (set-window-buffer window buffer)
          (with-selected-window window
            (drover-list--on-window-change window)
            (should (eq drover-list--layout 'narrow))
            ;; 30 columns: 28 of text, a column for the continuation
            ;; glyph, and Magit's ellipsis.
            (cl-flet ((flush (left right) (concat left (make-string (- 28 (length left) (length right)) ?\s)
                                                  right)))
              (should (equal (drover-list-test-entry "ask")
                             (concat (flush "  ! Session ask" "3m") "\n"
                                     "    asks: Which branch?")))
              (should (equal (drover-list-test-entry "root") (flush "  ◐ ? Session root" "10s")))
              (should (equal (drover-list-test-entry "sub") (flush "      ! Session sub" "1m")))
              ;; Only sessions that need you say why, and only on their
              ;; second line.
              (should (equal (drover-list-test-entry "deep") (flush "        ✓ Session deep" "30s")))
              (should (equal (drover-list-test-entry "busy") (flush "  ◐ Session busy" "40s"))))
            ;; The second line belongs to its session.
            (drover-list-test-goto "ask")
            (forward-line 1)
            (should (equal (drover-list-test-id-at-point) "ask"))
            (should (equal (drover-session-id (drover-session-at-point)) "ask"))
            (let ((summary (buffer-substring (line-beginning-position) (line-end-position))))
              (should (eq (get-text-property (1- (length summary)) 'font-lock-face summary) 'drover-dim)))
            ;; Moving goes from session to session, over second lines.
            (drover-list-test-goto "ask")
            (drover-list-test-press "n")
            (should (equal (drover-list-test-id-at-point) "root"))
            (drover-list-test-press "p")
            (should (equal (drover-list-test-id-at-point) "ask"))
            ;; Collapsing a session folds its details, not its second line.
            (drover-list-test-press "TAB")
            (should (string-match-p "Prompt of ask" (drover-list-test-visible-text)))
            (drover-list-test-press "TAB")
            (should (string-match-p "asks: Which branch" (drover-list-test-visible-text)))
            (should-not (string-match-p "Prompt of ask" (drover-list-test-visible-text)))))))))

(ert-deftest drover-list-test-narrow-truncation ()
  "Narrow lines cut titles before the age and summaries at the edge, in pixels."
  (let ((title "Session with a title much too wide for the window"))
    (drover-list-test-with
        (list (drover-session-create :backend 'fake :id "long" :title title
                                     :status 'done :since (drover-list-test-ago 7200))
              (drover-session-create :backend 'fake :id "kid" :parent "long"
                                     :title title :status 'waiting
                                     :requests '((:kind question
                                                  :summary "Which of the many files should I edit before the tests run?"))))
      (drover-list-test-with-font
        (let ((drover-list-test-width 240))
          (revert-buffer)
          (should (eq drover-list--layout 'narrow))
          ;; Lines end 233 pixels in, before Magit's ellipsis: too few for
          ;; the summary's column, past the title and the age, from the
          ;; glyph of a session alone, 25 + 84 + 35 + 98 = 242.
          (let* ((cells (drover-list-test-cells "long"))
                 (title-cell (car cells))
                 (age-cell (car (last cells)))
                 (shown (substring-no-properties (cdr title-cell) 4)))
            ;; The age, "2h", is 14 pixels: flush right.
            (should (equal (cdr age-cell) "2h"))
            (should (= (car age-cell) (- 233 14)))
            ;; The title ends in an ellipsis before it, with a space to
            ;; spare, and one more character would not fit.  It begins
            ;; past the glyph of the subagent waiting, 25 + 11 pixels in.
            (should (string-suffix-p "…" shown))
            (let ((room (- 233 14 4 36))
                  (kept (substring shown 0 -1)))
              (should (<= (drover-list-test-fake-width shown) room))
              (should (> (drover-list-test-fake-width (concat (substring title 0 (1+ (length kept))) "…"))
                         room))
              (should (string-prefix-p kept title))))
          ;; The subagent's glyph stands under its parent's title, 36
          ;; pixels in, and its summary under its own title, and fits.
          (should (= (car (car (drover-list-test-cells "kid"))) 36))
          (save-excursion
            (drover-list-test-goto "kid")
            (forward-line 1)
            (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                   (summary (substring line 2)))
              (should (get-text-property 0 'invisible line))
              (should (equal (get-text-property 1 'display line) '(space :align-to (47))))
              (should (string-suffix-p "…" summary))
              (should (<= (drover-list-test-fake-width summary) (- 233 47))))))))))

(ert-deftest drover-list-test-redraws-on-width-change ()
  (drover-list-test-with (list (drover-test-session "s" :status 'waiting
                                                    :requests '((:kind question :summary "Go?"))))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (drover-list--on-window-change (selected-window))
      (should (eq drover-list--layout 'columns))
      (let ((window (split-window-right -20)))
        (set-window-buffer window buffer)
        (select-window window)
        (drover-list--on-window-change window)
        (should (= drover-list--width (drover-list--line-width)))
        (should (eq drover-list--layout 'narrow))
        ;; Drawn for this width: nothing to do until it changes.
        (let ((tick (buffer-modified-tick)))
          (drover-list--on-window-change window)
          (should (= tick (buffer-modified-tick))))
        (delete-other-windows window)
        (drover-list--on-window-change window)
        (should (eq drover-list--layout 'columns))))))

;;;; What sessions run

(ert-deftest drover-list-test-model-and-effort ()
  "The model's column shows the model, then its effort, dimmed when the model's default."
  (drover-list-test-with
      (list (drover-list-test-titled "a" "Alpha" :status 'working :updated (drover-list-test-ago 60)
                                     :model "Opus 5.5" :effort "high")
            (drover-list-test-titled "b" "Bravo" :status 'working :updated (drover-list-test-ago 90)
                                     :model "GPT-5.5" :effort "medium" :effort-default t)
            (drover-list-test-titled "c" "Charlie" :status 'working :updated (drover-list-test-ago 120)
                                     :model "Big Pickle"))
    (should (memq 'model drover-list--fields))
    (let ((x (drover-list-test-cell-at "a" "Opus 5.5 high")))
      (should (= x (drover-list-test-cell-at "b" "GPT-5.5 medium")))
      (should (= x (drover-list-test-cell-at "c" "Big Pickle"))))
    (save-excursion
      (drover-list-test-goto "a")
      (search-forward "Opus 5.5 high")
      (let ((model (match-beginning 0))
            (effort (- (point) 4)))
        (should (eq (get-text-property model 'font-lock-face) 'drover-dim))
        (should (eq (get-text-property effort 'font-lock-face) 'opencode-setting-chosen))
        (should (equal (get-text-property effort 'help-echo) "Effort: high, chosen"))
        ;; The session's tooltip names them too.
        (should (equal (get-text-property model 'help-echo) "Alpha\nOpus 5.5 high"))))
    (save-excursion
      (drover-list-test-goto "b")
      (search-forward "GPT-5.5 medium")
      (let ((effort (- (point) 6)))
        (should (eq (get-text-property effort 'font-lock-face) 'opencode-setting-inherited))
        (should (equal (get-text-property effort 'help-echo) "Effort: medium, the model's default"))))
    ;; The line shows them: the details do not repeat them.
    (drover-list-test-goto "b")
    (magit-section-show (magit-current-section))
    (should-not (string-match-p "^ +Model" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-provider ()
  "The provider's column shows its icon in a cell as wide as its ink, else its name."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "a" "Alpha" :status 'working :updated (drover-list-test-ago 60)
                                       :model "opus" :provider "P" :provider-name "Prov")
              (drover-list-test-titled "b" "Bravo" :status 'working :updated (drover-list-test-ago 90)
                                       :model "opus" :provider-name "Acme"))
      ;; The icon advances 7 pixels, and draws 80 past its start: more
      ;; than its column's name and gap.
      (let ((drover-list-test-ink '((?P 0 . 80))))
        (revert-buffer)
        (should (memq 'provider drover-list--fields))
        (let ((x (drover-list-test-cell-at "a" "P")))
          (should (= x (drover-list-test-cell-at "b" "Acme")))
          (dolist (id '("a" "b"))
            (save-excursion
              (drover-list-test-goto id)
              (should (drover-list-test-ink-apart-p))))
          ;; The next column begins past the icon's ink and a gap.
          (should (= (drover-list-test-cell-at "a" "1m") (+ x 80 14 (- 21 19)))))
        (save-excursion
          (drover-list-test-goto "a")
          (let ((case-fold-search nil))
            (search-forward "P"))
          (should (equal (get-text-property (1- (point)) 'help-echo) "Provider: Prov"))
          (should (eq (get-text-property (1- (point)) 'font-lock-face) 'drover-dim)))
        (save-excursion
          (drover-list-test-goto "b")
          (search-forward "Acme")
          (should (equal (get-text-property (1- (point)) 'help-echo) "Provider: Acme")))
        (drover-list-test-goto "a")
        (magit-section-show (magit-current-section))
        (should-not (string-match-p "^ +Provider" (drover-list-test-visible-text)))))))

(defun drover-list-test-context-cell (id)
  "Return the context cell of session ID as a plist.
:x is where it begins, :full and :trough the widths of its bar's
parts, nil without a bar, :pad the width of the space standing for a
missing bar, :label its size, :label-face and :bar-face their faces,
and :help its tooltip."
  (let* ((cell (seq-find (lambda (cell)
                           (let ((case-fold-search nil))
                             (string-match-p "[0-9][kM]?\\'" (cdr cell))))
                         (drover-list-test-cells id)))
         (text (cdr cell))
         (widths (mapcar (lambda (index)
                           (pcase (get-text-property index 'display text)
                             (`(space :width (,width)) width)))
                         (number-sequence 0 (1- (length text)))))
         (bar (eq (get-text-property 1 'font-lock-face text) 'opencode-cache-trough)))
    (list :x (car cell)
          :full (and bar (nth 0 widths)) :trough (and bar (nth 1 widths))
          :pad (and (not bar) (nth 0 widths))
          :label (substring-no-properties text (if bar 3 2))
          :label-face (get-text-property (1- (length text)) 'font-lock-face text)
          :bar-face (and bar (get-text-property 0 'font-lock-face text))
          :help (get-text-property (1- (length text)) 'help-echo text))))

(ert-deftest drover-list-test-context ()
  "The context's column shows a short bar of how full it is and its size, warning near a tier."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "a" "Alpha" :status 'working :updated (drover-list-test-ago 60)
                                       :context-tokens 50000 :context-limit 200000)
              (drover-list-test-titled "n" "Near" :status 'working :updated (drover-list-test-ago 70)
                                       :context-tokens 186000 :context-limit 200000
                                       :context-tier '(near 200000 "2×"))
              (drover-list-test-titled "o" "Over" :status 'working :updated (drover-list-test-ago 80)
                                       :context-tokens 1200000 :context-limit 1000000
                                       :context-tier '(over 200000 "2×"))
              (drover-list-test-titled "u" "Unsized" :status 'working :updated (drover-list-test-ago 90)
                                       :context-tokens 8400)
              (drover-list-test-titled "z" "None" :status 'working :updated (drover-list-test-ago 95)))
      (should (memq 'context drover-list--fields))
      (let ((a (drover-list-test-context-cell "a"))
            (n (drover-list-test-context-cell "n"))
            (o (drover-list-test-context-cell "o"))
            (u (drover-list-test-context-cell "u")))
        ;; One column, bars of five characters, 35 pixels.
        (should (= (length (seq-uniq (mapcar (lambda (cell) (plist-get cell :x)) (list a n o u)))) 1))
        (should (equal (list (plist-get a :full) (plist-get a :trough)) '(9 26)))
        (should (equal (list (plist-get n :full) (plist-get n :trough)) '(33 2)))
        ;; Past the model's context, the bar is full.
        (should (equal (list (plist-get o :full) (plist-get o :trough)) '(35 0)))
        ;; Without the model's context size, no bar, but its room.
        (should (= (plist-get u :pad) 35))
        (should (equal (mapcar (lambda (cell) (plist-get cell :label)) (list a n o u))
                       '("50k" "186k" "1.2M" "8.4k")))
        (should (eq (plist-get a :label-face) 'drover-dim))
        (should (eq (plist-get a :bar-face) 'opencode-context-bar))
        ;; Near the tier, in a warning; past it, in an error; the bar
        ;; in inverse video, the face's colour as its background.
        (should (eq (plist-get n :label-face) 'opencode-context-near))
        (should (equal (plist-get n :bar-face) '((:inverse-video t) opencode-context-near)))
        (should (eq (plist-get o :label-face) 'opencode-context-over))
        (should (equal (plist-get o :bar-face) '((:inverse-video t) opencode-context-over)))
        (should (equal (plist-get a :help) "Context: 50k of 200k tokens (25%)"))
        (should (equal (plist-get n :help)
                       "Context: 186k of 200k tokens (93%)\nNear 200k tokens, past which requests cost 2×"))
        (should (equal (plist-get o :help)
                       "Context: 1.2M of 1M tokens (120%)\nPast 200k tokens: requests cost 2×"))
        (should (equal (plist-get u :help) "Context: 8.4k tokens")))
      ;; A session without a context shows nothing there.
      (should-not (seq-find (lambda (cell)
                              (let ((case-fold-search nil))
                                (string-match-p "[0-9][kM]\\'" (cdr cell))))
                            (drover-list-test-cells "z")))
      ;; The line shows it: the details do not repeat it.
      (drover-list-test-goto "n")
      (magit-section-show (magit-current-section))
      (should-not (string-match-p "^ +Context" (drover-list-test-visible-text))))))

(ert-deftest drover-list-test-context-estimated ()
  "A context the service estimated begins with a ~, which its column makes room for."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "e" "Estimated" :status 'working :updated (drover-list-test-ago 60)
                                       :context-tokens 50000 :context-limit 200000 :context-estimated t)
              (drover-list-test-titled "m" "Measured" :status 'working :updated (drover-list-test-ago 70)
                                       :context-tokens 50000 :context-limit 200000))
      (let ((e (drover-list-test-context-cell "e"))
            (m (drover-list-test-context-cell "m")))
        (should (equal (plist-get e :label) "~50k"))
        (should (equal (plist-get m :label) "50k"))
        (should (eq (plist-get e :label-face) 'drover-dim))
        (should (string-prefix-p "Context: ~50k of 200k tokens (25%)\nEstimated by the service"
                                 (plist-get e :help)))
        ;; The column holds the ~: the next one begins past it.
        (let* ((cells (drover-list-test-cells "e"))
               (context (seq-find (lambda (cell) (string-search "~50k" (cdr cell))) cells))
               (next (cadr (memq context cells))))
          (should next)
          (should (>= (- (car next) (car context))
                      (+ (drover-list-test-fake-width (cdr context))
                         (* drover-list--gap (drover-list--char-width))))))))))

(ert-deftest drover-list-test-context-pie-in-two-lines ()
  "Two lines show the context as a pie alone, at the end of the title's line.
Neither its bar nor its size: the cache already colours the line.  The
pie fills the eighth the context reaches into, in a cell after the age
that every line keeps, a line without a context size leaving it empty;
its tooltip tells the context's size and the model's.  As the context
grows, the pie is drawn in place as a drawing anew draws it.  With no
context to show, the cell goes."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (drover-list-test-layout-at 250)
      (should (eq drover-list--layout 'narrow))
      (should (memq 'context drover-list--fields))
      (should-not (string-search "50k" (drover-list-test-entry "a")))
      ;; 50k of 200k: the second eighth.  A's line, whose prompt cache
      ;; colours its background, ends in a space reaching the edge.
      (should (string-match-p "\\`.* Alpha +5m +▂ *\\'" (drover-list-test-heading "a")))
      (should (string-match-p "\\` +! Bravo +1m\\'" (drover-list-test-heading "b")))
      (should (equal (drover-list-test-tooltip-at '(fake . "a") "▂") "Context: 50k of 200k tokens (25%)"))
      ;; The ages end where the cell begins, whatever is in it.
      (let ((age-a (car (last (drover-list-test-key-cells '(fake . "a")) 2)))
            (age-b (car (last (drover-list-test-key-cells '(fake . "b"))))))
        (should (= (+ (car age-a) (drover-list-test-fake-width (cdr age-a)))
                   (+ (car age-b) (drover-list-test-fake-width (cdr age-b))))))
      (let ((session (copy-sequence (drover-list-test-find "a")))
            (drover-list-test-width 250))
        (drover-list-test-shown
          (setf (drover-session-context-tokens session) 190000)
          (setq drover-test-sessions (cons session (cdr drover-test-sessions)))
          (drover-backend-changed 'fake "a")
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (should (string-match-p " █ *\\'" (drover-list-test-heading "a")))
          (drover-list-test-is-fresh)))
      (let ((drover-list-hidden-fields '(harness context)))
        (drover-list-test-layout-at 250)
        (should-not (plist-get (drover-list--model-layout drover-list--model) :edges))
        (should (string-match-p "Alpha +5m *\\'" (drover-list-test-heading "a")))))))

;;;; The header line

(defun drover-list-test-header-cells ()
  "Return the names of the header line as a list of (X . NAME), X in pixels."
  (let ((header header-line-format)
        (cells nil))
    (dotimes (index (length header))
      (pcase (get-text-property index 'display header)
        (`(space :align-to (,x)) (push (cons x "") cells))
        (_ (setcdr (car cells) (concat (cdar cells) (substring header index (1+ index)))))))
    (nreverse cells)))

(ert-deftest drover-list-test-header-line ()
  "In columns, the header line names each column, aligned to it to the pixel."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (drover-list-test-layout-at 1000)
      (should (equal drover-list--fields
                     '(title location branch agent model provider context created age cache summary)))
      (let ((names (drover-list-test-header-cells))
            (right (lambda (text)
                     (let ((x (drover-list-test-cell-at "a" text)))
                       (+ x (drover-list-test-fake-width text))))))
        (should (equal (mapcar #'cdr names)
                       '("Session" "Project" "Branch" "Agent" "Model" "Provider" "Context" "Created"
                         "Age" "Cache" "Summary")))
        (cl-flet ((at (name) (car (rassoc name names)))
                  (end (name) (+ (car (rassoc name names)) (drover-list-test-fake-width name))))
          ;; Names begin where their columns do...
          (should (= (at "Session") (car (car (drover-list-test-cells "a")))))
          (should (= (at "Project") (drover-list-test-cell-at "a" "drover")))
          (should (= (at "Branch") (drover-list-test-cell-at "a" "main")))
          (should (= (at "Agent") (drover-list-test-cell-at "a" "@build")))
          (should (= (at "Model") (drover-list-test-cell-at "a" "opus high")))
          (should (= (at "Provider") (drover-list-test-cell-at "a" "P")))
          (should (= (at "Context") (plist-get (drover-list-test-context-cell "a") :x)))
          (should (= (at "Cache") (drover-list-test-cell-at "a" "4m")))
          (should (= (at "Summary") (drover-list-test-cell-at "a" "Busy")))
          ;; ... or end where they do, aligned right.
          (should (= (end "Created") (funcall right "1d")))
          (should (= (end "Age") (funcall right "5m"))))
        ;; A column is at least as wide as its name, such as "Created"
        ;; over "1d": names never overlap, a gap apart at least.
        (seq-mapn (lambda (name next)
                    (should (>= (car next) (+ (car name) (drover-list-test-fake-width (cdr name)) 14))))
                  names (cdr names))
        (let ((model (seq-find (lambda (name) (equal (cdr name) "Model")) names)))
          (should (eq (get-text-property 0 'face (cdr model)) 'drover-list-header))
          (should (equal (get-text-property 0 'help-echo (cdr model))
                         (alist-get 'model drover-list-field-descriptions)))))
      ;; Past line numbers, which the header line lacks.
      (let ((before (drover-list-test-header-cells)))
        (drover-list-test-shown
          (setq-local display-line-numbers t)
          (cl-letf (((symbol-function 'line-number-display-width) (lambda (&optional _) 21)))
            (drover-list-test-layout-at 1000))
          (should (equal (mapcar (lambda (name) (cons (- (car name) 21) (cdr name)))
                                 (drover-list-test-header-cells))
                         before))
          (setq-local display-line-numbers nil)))
      ;; Two lines name no column.
      (drover-list-test-layout-at 250)
      (should (eq drover-list--layout 'narrow))
      (should-not header-line-format)
      (drover-list-test-layout-at 1000)
      (should header-line-format))))

(ert-deftest drover-list-test-header-names-are-data ()
  "Names come from `drover-list-fields', cut to their column, and `%' shows as such."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (let ((drover-list-fields '((title :priority 0 :min 5 :label "100%")
                                  (agent :priority 1 :min 2 :max 3 :label "Agents")
                                  (age :priority 2 :min 3 :align right))))
        (drover-list-test-layout-at 1000)
        ;; Three characters of 7 pixels: "Ag" and the ellipsis.
        ;; Doubled, as the header line shows "%%" as "%".
        (should (equal (mapcar #'cdr (drover-list-test-header-cells)) '("100%%" "Ag…")))))))

;;;; Fetching what sessions lack

(ert-deftest drover-list-test-prefetch ()
  "The list asks backends for what the sessions in sight lack, once per drawing."
  (drover-list-test-with (drover-list-test-sample)
    (should (equal (drover-list-test-groups)
                   '((waiting "wait1" "wait2") (done "done") (working "work") (idle "idle"))))
    (should (eq drover-list--layout 'columns))
    ;; The idle group is collapsed: its session is not asked for.
    (should (equal drover-test-prefetched '(("wait1" "wait2" "done" "work"))))
    (revert-buffer)
    (should (equal drover-test-prefetched '(("wait1" "wait2" "done" "work") ("wait1" "wait2" "done" "work"))))
    ;; Expanding the group asks for its session after the command, once.
    (setq drover-test-prefetched nil)
    (should (memq #'drover-list--prefetch post-command-hook))
    (drover-list-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (run-hooks 'post-command-hook)
    (should (equal drover-test-prefetched '(("idle"))))
    (run-hooks 'post-command-hook)
    (should (equal drover-test-prefetched '(("idle"))))
    ;; A context the user hides needs nothing.
    (setq drover-test-prefetched nil
          drover-list-hidden-fields '(context))
    (revert-buffer)
    (should-not drover-test-prefetched)
    (run-hooks 'post-command-hook)
    (should-not drover-test-prefetched)
    ;; Two lines show it too, as a pie: asked for, whether a session
    ;; has a context yet or not, Idle expanded above.
    (setq drover-list-hidden-fields nil)
    (drover-list-test-with-font
      (drover-list-test-layout-at 100))
    (should (eq drover-list--layout 'narrow))
    (should (equal drover-test-prefetched '(("wait1" "wait2" "done" "work" "idle"))))
    ;; Unless the fields leave it out there.
    (setq drover-test-prefetched nil)
    (let ((drover-list-fields (cons '(context :priority 7 :min 9 :label "Context")
                                    (assq-delete-all 'context (copy-alist drover-list-fields)))))
      (drover-list-test-with-font
        (drover-list-test-layout-at 100))
      (should (eq drover-list--layout 'narrow))
      (should-not drover-test-prefetched))))

;;;; The prompt cache

(defun drover-list-test-cache-field (id)
  "Return the prompt cache field of session ID as (FULL TROUGH LABEL).
FULL and TROUGH are the widths of the bar's parts, from their display,
LABEL the text of the time left, or nil for those it lacks."
  (save-excursion
    (drover-list-test-goto id)
    (let ((end (line-end-position))
          (position (point))
          full trough label)
      (while (setq position (text-property-not-all position end 'drover-list-cache nil))
        (let ((width (pcase (get-text-property position 'display) (`(space :width (,width)) width))))
          (pcase (get-text-property position 'drover-list-cache)
            ('full (setq full width))
            ('trough (setq trough width))
            ('label (setq label (concat label (buffer-substring-no-properties position (1+ position)))))))
        (setq position (1+ position)))
      (list full trough label))))

(defun drover-list-test-help-at (position)
  "Return the tooltip of the list at POSITION, as a window showing it would."
  (let ((help (get-text-property position 'help-echo)))
    (if (functionp help) (funcall help (selected-window) (current-buffer) position) help)))

(defun drover-list-test-cached ()
  "Return a session whose prompt cache lasts five minutes, four left, and two others.
The cache holds 120k tokens of a prompt of 130k."
  (list (drover-list-test-titled "c" "Cached" :status 'working :updated (drover-list-test-ago 60)
                                 :cache-expires (+ drover-test-now 240) :cache-ttl 300
                                 :cache-tokens 120000 :cache-prompt-tokens 130000)
        (drover-list-test-titled "u" "Unknown" :status 'working :updated (drover-list-test-ago 90))
        (drover-list-test-titled "x" "Expired" :status 'working :updated (drover-list-test-ago 120)
                                 :cache-expires (- drover-test-now 10) :cache-ttl 300)))

(ert-deftest drover-list-test-cache-field ()
  "In columns, a live cache shows a short bar and the time left, as text."
  (drover-list-test-with (drover-list-test-cached)
    (should (eq drover-list--layout 'columns))
    ;; A bar of five characters, four fifths full, and four minutes.
    (should (equal (drover-list-test-cache-field "c") '(4 1 "4m")))
    (should (memq 'cache drover-list--fields))
    ;; The title, the age, a gap, the bar, a space and the time left.
    (should (equal (drover-list-test-heading "c") "  ◐ Cached    1m        4m"))
    (save-excursion
      (drover-list-test-goto "c")
      (let* ((full (text-property-any (point) (line-end-position) 'drover-list-cache 'full))
             (label (text-property-any (point) (line-end-position) 'drover-list-cache 'label)))
        ;; The bar fills in the colour of `opencode-cache-bar', in inverse
        ;; video, which highlights setting a background leave alone.
        (should (equal (get-text-property full 'face) '((:inverse-video t) opencode-cache-bar)))
        (should (equal (get-text-property full 'font-lock-face) '((:inverse-video t) opencode-cache-bar)))
        (should (eq (get-text-property (1+ full) 'face) 'opencode-cache-trough))
        (should (eq (get-text-property (1+ full) 'font-lock-face) 'opencode-cache-trough))
        ;; The time left is text, which copying the line keeps.
        (should (string-suffix-p "4m" (buffer-substring-no-properties (point) (line-end-position))))
        (should-not (get-text-property label 'display))
        (should (eq (get-text-property label 'font-lock-face) 'drover-dim))
        ;; Its tooltip, and the bar's, tell what it is.
        (dolist (position (list full (1+ full) label))
          (should (equal (drover-list-test-help-at position)
                         "Prompt cache: expires in 4m (5m window), 120k of 130k tokens cached")))
        ;; As the cache stands when the tooltip shows.
        (let ((drover-list-test-clock (+ drover-test-now 200)))
          (should (equal (drover-list-test-help-at label)
                         "Prompt cache: expires in 40s (5m window), 120k of 130k tokens cached")))
        (let ((drover-list-test-clock (+ drover-test-now 300)))
          (should (equal (drover-list-test-help-at label) "Prompt cache: expired")))
        ;; The rest of the line keeps the session's.
        (should (equal (drover-list-test-help-at (1+ (point))) "Cached"))))
    ;; Unknown or expired: nothing.
    (should (equal (drover-list-test-cache-field "u") '(nil nil nil)))
    (should (equal (drover-list-test-cache-field "x") '(nil nil nil)))
    (should (equal (drover-list-test-heading "u") "  ◐ Unknown   1m"))
    (should (= (length drover-list--caches) 1))
    (should (markerp (plist-get (car drover-list--caches) :start)))
    ;; No live cache, no field.
    (setq drover-test-sessions (cdr drover-test-sessions))
    (drover-list-test-refresh)
    (should-not (memq 'cache drover-list--fields))
    (should-not (text-property-not-all (point-min) (point-max) 'drover-list-cache nil))
    (should-not drover-list--caches)))

(ert-deftest drover-list-test-cache-field-shrinks ()
  "As time passes, only the bar and the time left change, until nothing shows."
  (drover-list-test-with (drover-list-test-cached)
    (let ((root magit-root-section)
          (text (buffer-substring-no-properties (point-min) (point-max)))
          (section (drover-list-test-section "c")))
      (setq drover-list-test-clock (+ drover-test-now 90))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(2 3 "2m")))
      ;; Only the time left changed, in place.
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (replace-regexp-in-string "4m\n" "2m\n" text)))
      (setq drover-list-test-clock (+ drover-test-now 200))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(1 4 "40s")))
      ;; Nothing was drawn anew: same sections, unmodified; the new time
      ;; left belongs to its session, and what follows it kept its place.
      (should (eq magit-root-section root))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (replace-regexp-in-string "4m\n" "40s\n" text)))
      (should-not (buffer-modified-p))
      (save-excursion
        (drover-list-test-goto "c")
        (let ((label (text-property-any (point) (line-end-position) 'drover-list-cache 'label)))
          (should (eq (magit-section-at label) section))
          (should (eq (magit-section-at (+ label 2)) section))
          (should (eq (get-text-property (+ label 2) 'font-lock-face) 'drover-dim))
          (should (equal (drover-list-test-help-at (+ label 2))
                         "Prompt cache: expires in 40s (5m window), 120k of 130k tokens cached"))))
      (should (equal (drover-list-test-heading "u") "  ◐ Unknown   1m"))
      (should (equal (drover-session-id (save-excursion (drover-list-test-goto "u") (drover-session-at-point)))
                     "u"))
      ;; Expired: nothing shows, and the cache is forgotten.
      (setq drover-list-test-clock (+ drover-test-now 240))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(0 0 nil)))
      (should (equal (string-trim-right (drover-list-test-heading "c")) "  ◐ Cached    1m"))
      (should-not drover-list--caches))))

(ert-deftest drover-list-test-cache-label-keeps-glyphs-in-place ()
  "A time left changing length leaves the animated glyphs after it animated."
  (drover-list-test-with (drover-list-test-cached)
    (setq opencode-session-status-animation-interval 0.5)
    (revert-buffer)
    (let ((glyphs (mapcar (lambda (entry) (marker-position (car entry))) drover-list--animated)))
      (should (= (length glyphs) 3))
      (should (seq-every-p (lambda (entry) (markerp (car entry))) drover-list--animated))
      ;; "4m" becomes "40s", one character longer, before the glyphs of
      ;; the sessions listed after it.
      (setq drover-list-test-clock (+ drover-test-now 200))
      (drover-list--draw-caches)
      (should (equal (mapcar (lambda (entry) (marker-position (car entry))) drover-list--animated)
                     (cons (car glyphs) (mapcar #'1+ (cdr glyphs)))))
      ;; The next frame shows on the glyphs, not beside them.
      (let* ((time (+ drover-test-now 200.5))
             (frame (nth (opencode-session-status-frame-index 3 time) '("1" "2" "3"))))
        (drover-list--draw-frames time)
        (dolist (entry drover-list--animated)
          (should (eq (get-text-property (car entry) 'drover-list-frame) 'working))
          (should (equal (substring-no-properties (get-text-property (car entry) 'display)) frame)))))))

(defun drover-list-test-bar ()
  "Return the bar of the first cache drawn as a background, as (START END).
START and END bound its overlay."
  (when-let* ((entry (car drover-list--caches))
              (overlay (plist-get entry :overlay))
              ((overlay-buffer overlay)))
    (list (overlay-start overlay) (overlay-end overlay))))

(ert-deftest drover-list-test-cache-background ()
  "In two lines, a live cache underlines its line from the left, shrinking."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 60)
                                 :requests '((:kind question :summary "Go?"))
                                 :cache-expires (+ drover-test-now 300) :cache-ttl 300))
    (drover-list-test-with-font
      (let ((drover-list-test-width 300))
        (revert-buffer)
        (should (eq drover-list--layout 'narrow))
        (drover-list-test-goto "s")
        ;; The line drawn begins past the invisible character.
        (let* ((start (1+ (point)))
               (end (line-end-position))
               (spec (plist-get (car drover-list--caches) :spec))
               (split (lambda (from)
                        (car (seq-find (lambda (item) (and (eq (nth 1 item) 'split) (= (nth 2 item) from)))
                                       spec))))
               (stretch (lambda (offset) (get-text-property (+ start offset) 'display))))
          ;; The line takes no more room: it reaches 293 pixels, the age,
          ;; 19 pixels, flush right as without a cache.
          (should (= (nth 3 (car (last spec))) 293))
          (should (equal (car (last (seq-filter (lambda (item) (eq (nth 1 item) 'text)) spec)))
                         (list (nth 0 (car (last spec 2))) 'text 274 19 (drover-face "1m" 'drover-dim))))
          ;; A line under the line, in the colour of `opencode-cache-bar'.
          (should (equal (overlay-get (plist-get (car drover-list--caches) :overlay) 'face)
                         `(:underline (:color ,(or (face-foreground 'opencode-cache-bar nil t)
                                                   'foreground-color)
                                       :position t))))
          ;; The line's tooltip tells of the cache too, as it stands when
          ;; shown, on both lines.
          (should (equal (drover-list-test-help-at start)
                         "Session s\nasks: Go?\nPrompt cache: expires in 5m (5m window)"))
          (should (equal (drover-list-test-help-at (1+ end))
                         "Session s\nasks: Go?\nPrompt cache: expires in 5m (5m window)"))
          (let ((drover-list-test-clock (+ drover-test-now 100)))
            (should (string-suffix-p "Prompt cache: expires in 3m (5m window)"
                                     (drover-list-test-help-at start))))
          ;; Full: the whole first line, up to its last 293 pixels.
          (should (equal (drover-list-test-bar) (list start end)))
          ;; Half, 146 pixels: within the space between the title, which
          ;; ends at 25 + 56 = 81, and the age, at 274.
          (setq drover-list-test-clock (+ drover-test-now 150))
          (drover-list--draw-caches)
          (let ((offset (funcall split 81)))
            (should (equal (funcall stretch offset) '(space :width (65))))
            (should (equal (drover-list-test-bar) (list start (+ start offset 1)))))
          ;; A quarter, 73 pixels: within the title, after "Session",
          ;; 45 pixels from 25; the space after it would reach 74.
          (setq drover-list-test-clock (+ drover-test-now 225))
          (drover-list--draw-caches)
          (should (equal (funcall stretch (funcall split 81)) '(space :width (0))))
          ;; The two spaces of the indentation, then the glyph and title.
          (should (equal (buffer-substring-no-properties start (cadr (drover-list-test-bar)))
                         "  ! Session"))
          ;; Expired: no bar, every space as it was.
          (setq drover-list-test-clock (+ drover-test-now 300))
          (drover-list--draw-caches)
          (should-not (drover-list-test-bar))
          (should-not drover-list--caches)
          (dolist (item spec)
            (when (eq (nth 1 item) 'split)
              (should (equal (funcall stretch (car item)) '(space :width (0)))))))))))

(ert-deftest drover-list-test-cache-timer ()
  "The cache timer runs while a shown list has a live cache, and only then."
  (drover-list-test-with (drover-list-test-cached)
    (let ((drover-list-cache-interval 5))
      ;; Hidden: no timer.
      (revert-buffer)
      (should drover-list--caches)
      (should-not drover-list--cache-timer)
      (drover-list-test-shown
        (revert-buffer)
        (should (timerp drover-list--cache-timer))
        (should (= (timer--repeat-delay drover-list--cache-timer) 5))
        ;; A tick draws the caches.
        (setq drover-list-test-clock (+ drover-test-now 90))
        (drover-list--cache-tick)
        (should (equal (drover-list-test-cache-field "c") '(2 3 "2m")))
        (should (timerp drover-list--cache-timer))
        ;; Once every cache is out, the timer stops.
        (setq drover-list-test-clock (+ drover-test-now 300))
        (drover-list--cache-tick)
        (should-not drover-list--caches)
        (should-not drover-list--cache-timer)
        (setq drover-list-test-clock drover-test-now)
        (revert-buffer)
        (should (timerp drover-list--cache-timer)))
      ;; Hidden again: the next tick stops it, drawing nothing.
      (setq drover-list-test-clock (+ drover-test-now 90))
      (drover-list--cache-tick)
      (should-not drover-list--cache-timer)
      (should (equal (drover-list-test-cache-field "c") '(4 1 "4m")))
      ;; Shown again, it starts again.
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (drover-list--on-window-change (selected-window))
        (should (timerp drover-list--cache-timer))))))

(ert-deftest drover-list-test-cache-timer-off ()
  (drover-list-test-with (drover-list-test-cached)
    (drover-list-test-shown
      (revert-buffer)
      (should drover-list--caches)
      ;; Never, as asked.
      (should-not drover-list--cache-timer))))

(ert-deftest drover-list-test-cache-in-details ()
  "The details tell the prompt cache when the line does not."
  (drover-list-test-with (drover-list-test-cached)
    (drover-list-test-goto "c")
    (magit-section-show (magit-current-section))
    (should-not (string-match-p "^ +Prompt cache" (drover-list-test-visible-text)))
    (setq drover-list-hidden-fields '(cache))
    (revert-buffer)
    (should-not (memq 'cache drover-list--fields))
    (should (string-match-p "^    Prompt cache +expires in 4m (5m window), 120k of 130k tokens cached$"
                            (drover-list-test-visible-text)))))

(defun drover-list-test-cached-family ()
  "Return a parent and its subagent, both working with a live prompt cache."
  (list (drover-list-test-titled "p" "Parent" :status 'working :updated (drover-list-test-ago 60)
                                 :cache-expires (+ drover-test-now 240) :cache-ttl 300)
        (drover-list-test-titled "k" "Kid" :parent "p" :status 'working
                                 :updated (drover-list-test-ago 90)
                                 :cache-expires (+ drover-test-now 120) :cache-ttl 300)))

(ert-deftest drover-list-test-cache-not-on-subagents ()
  "In columns, a subagent shows no prompt cache, its parent does."
  (drover-list-test-with (drover-list-test-cached-family)
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (should (eq drover-list--layout 'columns))
    (should (equal (drover-list-test-cache-field "p") '(4 1 "4m")))
    (should (equal (drover-list-test-cache-field "k") '(nil nil nil)))
    (should (equal (mapcar (lambda (entry) (drover-session-id (plist-get entry :session)))
                           drover-list--caches)
                   '("p")))
    ;; Nor its details, even where the line leaves the cache out.
    (setq drover-list-hidden-fields '(cache))
    (revert-buffer)
    (dolist (id '("p" "k"))
      (drover-list-test-goto id)
      (magit-section-show (magit-current-section)))
    (let ((text (drover-list-test-visible-text)))
      (should (= (with-temp-buffer
                   (insert text)
                   (count-matches "Prompt cache" (point-min) (point-max)))
                 1))
      (should (string-match-p "Prompt cache +expires in 4m" text)))
    ;; A subagent's cache alone takes no column, and no timer draws it.
    (setq drover-list-hidden-fields nil)
    (setf (drover-session-cache-expires (drover-list-test-find "p")) nil)
    (let ((drover-list-cache-interval 5))
      (drover-list-test-shown
        (drover-list-test-refresh)
        (should-not (memq 'cache drover-list--fields))
        (should-not drover-list--caches)
        (should-not (text-property-not-all (point-min) (point-max) 'drover-list-cache nil))
        (should-not drover-list--cache-timer)))))

(ert-deftest drover-list-test-cache-background-not-on-subagents ()
  "In two lines, a subagent's line has no cache under it, nor in its tooltip."
  (drover-list-test-with (drover-list-test-cached-family)
    (setq drover-show-subagents 'all)
    (drover-list-test-with-font
      (let ((drover-list-test-width 200))
        (revert-buffer)
        (should (eq drover-list--layout 'narrow))
        (should (equal (mapcar (lambda (entry) (list (drover-session-id (plist-get entry :session))
                                                     (plist-get entry :kind)))
                               drover-list--caches)
                       '(("p" narrow))))
        (drover-list-test-goto "k")
        (should (equal (drover-list-test-help-at (1+ (point))) "Kid"))
        (drover-list-test-goto "p")
        (should (string-suffix-p "Prompt cache: expires in 4m (5m window)"
                                 (drover-list-test-help-at (1+ (point)))))))))

(ert-deftest drover-list-test-cache-not-on-subagents-in-both-lists ()
  "Neither the sidebar nor the list shows a subagent's cache."
  (drover-list-test-two-lists
    (setq drover-test-sessions (drover-list-test-cached-family)
          drover-show-subagents 'all)
    (drover-view-settings-changed)
    (dolist (list (list buffer sidebar))
      (with-current-buffer list
        (drover-list-test-refresh)
        (should (equal (mapcar (lambda (entry) (drover-session-id (plist-get entry :session)))
                               drover-list--caches)
                       '("p")))
        (should (eq (plist-get (car drover-list--caches) :kind)
                    (if (eq list sidebar) 'narrow 'columns)))))))

;;;; Faces and measures

(ert-deftest drover-list-test-variable-pitch ()
  (drover-list-test-with (list (drover-test-session "s"))
    (should (eq (face-attribute 'drover-list :inherit) 'variable-pitch))
    (should (memq 'drover-list (flatten-tree (alist-get 'default face-remapping-alist))))
    (should (memq #'drover-list--on-face-change text-scale-mode-hook))))

(defvar drover-list-test--measured nil
  "Face remappings, windows and texts measured, newest first.")

(defmacro drover-list-test-measuring (&rest body)
  "Run BODY measuring text in a fake font, recording what was measured."
  (declare (indent 0) (debug t))
  `(let ((drover-list-test--measured nil))
     (cl-letf (((symbol-function 'buffer-text-pixel-size)
                (lambda (&optional _buffer window &rest _)
                  (push (list face-remapping-alist window (buffer-string)) drover-list-test--measured)
                  (cons (drover-list-test-fake-width (buffer-string)) 1))))
       ,@body)))

(ert-deftest drover-list-test-measures-in-list-faces ()
  (drover-list-test-with (list (drover-test-session "s"))
    (let ((remapping face-remapping-alist))
      (drover-list-test-measuring
        (setq drover-list--widths nil)
        (should (= (drover-list--pixel-width "abc") 21))
        ;; The measuring buffer has the list's remapping, and the
        ;; window the list is drawn for.
        (should (equal (car drover-list-test--measured) (list remapping (selected-window) "abc")))))))

(ert-deftest drover-list-test-measures-in-side-window ()
  (drover-list-test-with (drover-list-test-sample)
    ;; Alone: no other list measured these texts.
    (kill-buffer buffer)
    (let ((side (drover-list--show-side)))
      (with-current-buffer (window-buffer side)
        (drover-list-test-measuring
          (revert-buffer)
          (should drover-list-test--measured)
          (dolist (measure drover-list-test--measured)
            (should (eq (nth 1 measure) side))))))))

(ert-deftest drover-list-test-lists-share-widths ()
  "The sidebar measures only what the other list, measuring alike, did not."
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-showing
      (drover-list-test-shown
        (drover-list-test-measuring
          (revert-buffer)
          (should drover-list-test--measured)
          (let ((full-texts (mapcar #'caddr drover-list-test--measured)))
            (setq drover-list-test--measured nil)
            (with-current-buffer (window-buffer (drover-list--show-side))
              (should (eq drover-list--layout 'narrow))
              ;; What it measured, the other did not, properties included:
              ;; texts cut to its narrower room, say; the titles were
              ;; measured once.  The candidates of a cut, which neither
              ;; keeps, may be measured by both.
              (dolist (measure drover-list-test--measured)
                (unless (string-suffix-p (truncate-string-ellipsis) (nth 2 measure))
                  (should-not (seq-find (lambda (text) (equal-including-properties text (nth 2 measure)))
                                        full-texts))))
              (should-not (seq-find (lambda (measure) (equal (nth 2 measure) "Session wait1"))
                                    drover-list-test--measured))
              (should (seq-find (lambda (text) (string-search "Session wait1" text)) full-texts))
              ;; Measured otherwise, as in another font, it measures anew.
              (setq drover-list-test--measured nil)
              (face-remap-add-relative 'default :height 2.0)
              (drover-list--render)
              (should (seq-find (lambda (measure) (equal (nth 2 measure) "Session wait1"))
                                drover-list-test--measured)))))))))

(ert-deftest drover-list-test-reuses-widths ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-measuring
      (revert-buffer)
      (should drover-list-test--measured)
      (let ((lines (drover-list-test-visible-text)))
        ;; Nothing changed: the second drawing measures nothing.
        (setq drover-list-test--measured nil)
        (drover-list--render)
        (should-not drover-list-test--measured)
        (should (equal (drover-list-test-visible-text) lines))
        ;; Text of another size measures again, and so does a refresh.
        (face-remap-add-relative 'default :height 2.0)
        (drover-list--render)
        (should drover-list-test--measured)
        (setq drover-list-test--measured nil)
        (drover-list--render)
        (should-not drover-list-test--measured)
        (revert-buffer)
        (should drover-list-test--measured)))))

(ert-deftest drover-list-test-redraws-on-text-scale ()
  (drover-list-test-with (list (drover-test-session "s"))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (setq drover-list--width 1)
      (text-scale-set 1)
      (should (= drover-list--width (drover-list--line-width)))
      (setq drover-list--width 1)
      (text-scale-set 0)
      (should (= drover-list--width (drover-list--line-width))))
    ;; Hidden, it redraws once shown.
    (text-scale-set 1)
    (should drover-list--stale)
    (text-scale-set 0)))

;;;; Details

(ert-deftest drover-list-test-details-on-expand ()
  "Expanded, a session recaps its last turn and what its line leaves out."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 120)
                                 :directory (expand-file-name "~/src/drover") :branch "main"
                                 :agent "build" :model "sonnet" :detail "Blocked"
                                 :created (drover-list-test-ago (* 3 86400))
                                 :requests '((:kind permission :summary "bash: make")
                                             (:kind question :summary "Which branch?"))))
    (should (drover-list-test-section-hidden-p "s"))
    ;; Asked for once expanded, not before.
    (should-not drover-test-recap-calls)
    (should-not (string-match-p "Directory" (drover-list-test-visible-text)))
    (drover-list-test-goto "s")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-list-test-section-hidden-p "s"))
    (should (equal drover-test-recap-calls '("s")))
    (let ((text (drover-list-test-visible-text)))
      ;; Labels under the title, texts past the widest label: what it
      ;; waits for in full, the last turn, and the directory a session
      ;; without a project cannot tell otherwise.
      (dolist (line '("    Permission  bash: make"
                      "    Question    Which branch?"
                      "    Prompt      Prompt of s"
                      "    Reply       Reply of s"
                      "    Directory   ~/src/drover"))
        (should (string-match-p (concat "^" (regexp-quote line) "$") text)))
      ;; Not what the line shows already.
      ;; Its line shows the default branch, greyed out.
      (dolist (label '("Status" "Agent" "Model" "Created" "Backend" "Detail" "Branch"))
        (should-not (string-match-p (concat "^ +" label " ") text))))
    (search-forward "Permission")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (search-forward "bash: make")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'opencode-session-status-waiting))
    ;; Every line of the details begins with the invisible character.
    (should (get-text-property (line-beginning-position) 'invisible))
    ;; Point in the details still designates the session.
    (drover-list-test-goto "s")
    (forward-line 3)
    (should (equal (drover-list-test-id-at-point) "s"))
    (should (equal (drover-session-id (drover-session-at-point)) "s"))))

(ert-deftest drover-list-test-details-of-subagent ()
  (drover-list-test-with (list (drover-list-test-titled "root" "Parent work")
                               (drover-test-session "child" :parent "root" :status 'waiting))
    (drover-list-test-goto "child")
    (magit-section-show (magit-current-section))
    ;; Indented as deep as the subagent's title.
    (should (string-match-p "^        Prompt   Prompt of child$" (drover-list-test-visible-text)))
    ;; What runs it, which its line leaves out.
    (should (string-match-p "^        Harness  Fake$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-details-tell-what-the-line-leaves-out ()
  "What the session runs shows in its details when its line does not show it."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)
                                 :project "drover" :branch "main" :agent "build"
                                 :model "Opus" :effort "high" :provider "P" :provider-name "Acme"
                                 :context-tokens 50000 :context-limit 200000
                                 :created (drover-list-test-ago 7200)))
    (drover-list-test-goto "s")
    (magit-section-show (magit-current-section))
    ;; The line shows the project, the model, the provider and the
    ;; context, and keeps the summary's column, for when the session
    ;; needs you: the agent and the creation, left out, are told below.
    (let ((text (drover-list-test-visible-text)))
      (should-not (string-match-p "^ +\\(Model\\|Provider\\|Context\\|Project\\|Directory\\) " text))
      (should (string-match-p "^    Agent    @build$" text))
      (should (string-match-p "^    Created  2h ago$" text)))
    (setq drover-list-hidden-fields '(location branch agent model provider context created))
    (revert-buffer)
    (let ((text (drover-list-test-visible-text)))
      (dolist (line '("    Prompt    Prompt of s"
                      "    Project   drover"
                      "    Branch    main"
                      "    Agent     @build"
                      "    Model     Opus high"
                      "    Provider  Acme"
                      "    Context   50k of 200k tokens (25%)"
                      "    Created   2h ago"))
        (should (string-match-p (concat "^" (regexp-quote line) "$") text))))
    (search-forward "Opus high")
    (should (eq (get-text-property (- (point) 4) 'font-lock-face) 'opencode-setting-chosen))))

(ert-deftest drover-list-test-details-in-a-narrow-window ()
  "With little room past the labels, texts go under their labels."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)
                                 :model "Opus" :detail "Thinking hard about it"))
    (setq drover-test-recaps '(("s" :prompt "Fix the build" :reply "Fixed it")))
    (drover-list-test-shown
      (save-window-excursion
        (let ((window (split-window-right -30)))
          (set-window-buffer window buffer)
          (with-selected-window window
            (drover-list--on-window-change window)
            (should (eq drover-list--layout 'narrow))
            (drover-list-test-goto "s")
            (magit-section-show (magit-current-section))
            ;; How it stands, which its line, two lines for the
            ;; sessions that need you, leaves out.
            (should (string-match-p (concat "  ◐ Session s +1m\n"
                                            "    Status\n    Thinking hard about it\n"
                                            "    Prompt\n    Fix the build\n"
                                            "    Reply\n    Fixed it\n"
                                            "    Harness\n    Fake\n"
                                            "    Model\n    Opus\n")
                                    (drover-list-test-visible-text)))))))))

(ert-deftest drover-list-test-recap-wrapped ()
  "The prompt and the reply take a few lines each, wrapped in pixels, cut short."
  (drover-list-test-with-font
    (drover-list-test-with (list (drover-test-session "s" :status 'working
                                                      :updated (drover-list-test-ago 60)))
      (let* ((words (mapcar (lambda (n) (format "word%d" n)) (number-sequence 1 200)))
             (reply (concat "First paragraph.\n\n\n" (string-join words " "))))
        (setq drover-test-recaps `(("s" :prompt ,(string-join words " ") :reply ,reply)))
        (let ((drover-list-test-width 300))
          (revert-buffer)
          (drover-list-test-goto "s")
          (magit-section-show (magit-current-section))
          ;; Under the title, 25 pixels in, the texts past "Harness", the
          ;; widest label, 49 pixels, and a gap of 14: 88 pixels in, up
          ;; to 293.
          (let ((lines nil))
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp)) (equal (drover-list-test-id-at-point) "s"))
                (push (drover-list-test-cells-here) lines)
                (forward-line 1)))
            (setq lines (nreverse lines))
            (should (equal (mapcar #'car (car lines)) '(25 88)))
            (should (equal (cdr (nth 0 (car lines))) "Prompt"))
            (let ((texts (mapcar (lambda (line) (substring-no-properties (cdar (last line)))) lines)))
              (dolist (line lines)
                (should (= (car (car (last line))) 88))
                (should (<= (drover-list-test-fake-width (cdar (last line))) (- 293 88))))
              ;; Three lines of prompt, eight of reply, blank lines dropped,
              ;; then the harness, whose column is hidden.
              (should (= (length texts) 12))
              (should (equal (nth 11 texts) "Fake"))
              (should (string-suffix-p "…" (nth 2 texts)))
              (should (equal (nth 3 texts) "First paragraph."))
              (should (string-prefix-p "word1 word2 " (nth 4 texts)))
              (should (string-suffix-p "…" (nth 10 texts)))
              ;; Words follow one another from line to line.
              (should (string-prefix-p (string-join (mapcar (lambda (text) (string-remove-suffix "…" text))
                                                            (seq-subseq texts 4 10))
                                                    " ")
                                       (string-join words " "))))))))))

(defun drover-list-test-cells-here ()
  "Return the cells of the line at point, as `drover-list-test-cells' does."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
        (cells nil))
    (dotimes (index (length line))
      (pcase (get-text-property index 'display line)
        ((guard (get-text-property index 'invisible line)))
        (`(space :align-to (,x)) (push (cons x "") cells))
        (_ (setcdr (car cells) (concat (cdar cells) (substring line index (1+ index)))))))
    (nreverse cells)))

(ert-deftest drover-list-test-recap-changes-and-errors ()
  "A failed turn shows its error in full, and the files it changed.
An error the line shows whole already is not repeated."
  (drover-list-test-with
      (list (drover-test-session "f" :status 'failed :since (drover-list-test-ago 60)
                                 :directory "/work/" :project "p" :detail "Short")
            (drover-test-session "r" :status 'retrying :updated (drover-list-test-ago 30)
                                 :project "p" :detail "retry 2: overloaded")
            (drover-test-session "q" :status 'idle :project "p"))
    (setq drover-test-recaps
          '(("f" :prompt "Go" :error "The provider refused the request: quota exceeded"
             :changes (("/work/a.el" 10 2) ("/work/sub/b.el" 2 1) ("/elsewhere/c.el" nil nil)))
            ("r" :prompt "Go")
            ("q")))
    (drover-list-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (dolist (id '("f" "r" "q"))
      (drover-list-test-goto id)
      (magit-section-show (magit-current-section)))
    (let ((text (drover-list-test-visible-text)))
      (should (string-match-p "^    Error +The provider refused the request: quota exceeded$" text))
      ;; Without what the session changed in all its turns, its last
      ;; turn's files, a line each.
      (should (string-match-p (concat "^    Changes  \\+12 −3 · 3 files in the last turn\n"
                                      " +a\\.el  \\+10 −2\n"
                                      " +sub/b\\.el  \\+2 −1\n"
                                      " +/elsewhere/c\\.el\n")
                              text))
      ;; Its line shows the retry whole.
      (should-not (string-match-p "^    Retrying" text))
      ;; A recap with neither prompt nor reply says so.
      (should (string-match-p "^    Last turn +nothing to recap$" text)))
    (drover-list-test-goto "f")
    (search-forward "The provider")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'opencode-session-status-failed))
    (search-forward "+10")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-added))
    ;; Cut short, the retry shows in full under its line.
    (setq drover-list-hidden-fields '(summary harness))
    (revert-buffer)
    (should (string-match-p "^    Retrying +retry 2: overloaded$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-recap-fetched-once-per-change ()
  "A recap is fetched as its session expands, kept until the session changes."
  (drover-list-test-with (list (drover-test-session "a" :status 'working :updated (drover-list-test-ago 60))
                               (drover-test-session "b" :status 'working :updated (drover-list-test-ago 90)))
    (setq drover-test-recap-later t)
    (drover-list-test-shown
      (drover-list-test-goto "a")
      (drover-list-test-type "TAB")
      (should (equal drover-test-recap-calls '("a")))
      (should (string-match-p "^    Last turn +loading…$" (drover-list-test-visible-text)))
      ;; While it is fetched, a redraw asks nothing more.
      (drover-list--render)
      (should (equal drover-test-recap-calls '("a")))
      ;; Arriving, it is drawn.
      (funcall (cdr (pop drover-test-recap-pending)) '(:prompt "Fix it" :reply "Fixed"))
      (let ((text (drover-list-test-visible-text)))
        (should (string-match-p "^    Prompt +Fix it$" text))
        (should (string-match-p "^    Reply +Fixed$" text))
        (should-not (string-match-p "loading" text)))
      (drover-list--render)
      (should (equal drover-test-recap-calls '("a")))
      ;; The session changed: fetched again once some time passed, the
      ;; old recap shown meanwhile.
      (setf (drover-session-updated (drover-list-test-find "a")) (drover-list-test-ago 1))
      (drover-backend-changed 'fake "a")
      (drover-changed-now)
      (should (equal drover-test-recap-calls '("a")))
      (should (timerp drover-list--recap-timer))
      (setq drover-list-test-clock (+ drover-test-now drover-list--recap-interval))
      (drover-list--render)
      (should (equal drover-test-recap-calls '("a" "a")))
      (should (string-match-p "^    Prompt +Fix it$" (drover-list-test-visible-text)))
      (funcall (cdr (pop drover-test-recap-pending)) '(:failure "offline"))
      (should (string-match-p "^    Last turn +not available: offline$" (drover-list-test-visible-text)))
      ;; A refresh fetches again; an answer to the fetch it forgot is dropped.
      (drover-list-test-goto "b")
      (drover-list-test-type "TAB")
      (should (equal drover-test-recap-calls '("b" "a" "a")))
      (let ((forgotten (pop drover-test-recap-pending)))
        (revert-buffer)
        (should (equal (seq-take drover-test-recap-calls 2) '("b" "a")))
        (funcall (cdr forgotten) '(:prompt "Stale"))
        (should-not (string-match-p "Stale" (drover-list-test-visible-text))))
      (dolist (pending drover-test-recap-pending)
        (funcall (cdr pending) (list :prompt (concat "Fresh " (car pending)))))
      (should (string-match-p "^    Prompt +Fresh b$" (drover-list-test-visible-text)))
      ;; Collapsed, nothing is asked.
      (setq drover-test-recap-calls nil)
      (drover-list-test-goto "a")
      (drover-list-test-type "TAB")
      (setf (drover-session-updated (drover-list-test-find "a")) (drover-list-test-ago 0))
      (setq drover-list-test-clock (+ drover-test-now 60))
      (revert-buffer)
      (should (equal drover-test-recap-calls '("b"))))))

(ert-deftest drover-list-test-recap-without-backend-support ()
  "A backend telling no recap shows what the session tells, and asks nothing."
  (drover-list-test-with nil
    (drover-test-register-simple 'plain (list (drover-session-create :backend 'plain :id "p" :title "Plain"
                                                                     :status 'failed :detail "Boom\nAt startup")))
    (revert-buffer)
    (goto-char (oref (drover-list-test-section "p" 'plain) start))
    (magit-section-show (magit-current-section))
    (let ((text (drover-list-test-visible-text)))
      (should (string-match-p "^    Error +Boom$" text))
      (should (string-match-p "^ +At startup$" text))
      (should-not (string-match-p "Last turn" text)))))

(ert-deftest drover-list-test-changes-column-and-files ()
  "What a session changed in all its turns shows in a column, its files under it once expanded."
  (drover-list-test-with
      (list (drover-test-session "c" :status 'done :since (drover-list-test-ago 60) :directory "/work/"
                                 :changes '(12 3 150))
            (drover-test-session "n" :status 'done :since (drover-list-test-ago 90)))
    (setq drover-test-recaps
          '(("c" :prompt "Go" :changes (("/work/a.el" 1 0))
             :files (("/work/a.el" 10 2) ("/work/sub/b.el" 2 1)))
            ("n" :prompt "Go" :changes (("/work/c.el" 1 0)))))
    (revert-buffer)
    (should (memq 'changes drover-list--fields))
    (should (string-match-p "\\+12 −3 · 150 files +1m$" (drover-list-test-heading "c")))
    (should (seq-find (lambda (cell) (equal (cdr cell) "Changes")) (drover-list-test-header-cells)))
    (dolist (id '("c" "n"))
      (drover-list-test-goto id)
      (magit-section-show (magit-current-section)))
    (let ((text (drover-list-test-visible-text)))
      ;; Its line tells the counts: the files alone follow, a line each,
      ;; then how many more there are.
      (should (string-match-p (concat "^    Changes +a\\.el  \\+10 −2\n"
                                      " +sub/b\\.el  \\+2 −1\n"
                                      " +and 148 more files\n")
                              text))
      ;; Without what all its turns changed, its last turn's.
      (should (string-match-p "^    Changes +\\+1 −0 · 1 file in the last turn\n +/work/c\\.el  \\+1 −0\n" text)))
    ;; Without its column, the counts come first.
    (setq drover-list-hidden-fields '(harness changes))
    (revert-buffer)
    (should-not (memq 'changes drover-list--fields))
    (should (string-match-p (concat "^    Changes +\\+12 −3 · 150 files\n"
                                    " +a\\.el  \\+10 −2\n")
                            (drover-list-test-visible-text)))
    ;; The files take the lines their part is given, the last telling the others.
    (let ((drover-list-recap-lines '((changes . 2))))
      (revert-buffer)
      (should (string-match-p (concat "^    Changes +\\+12 −3 · 150 files\n"
                                      " +and 150 more files\n    Directory")
                              (drover-list-test-visible-text))))))

(ert-deftest drover-list-test-changes-asked-for-the-sessions-in-sight ()
  "The list asks what the sessions it shows changed, those it expands too."
  (drover-list-test-with (list (drover-test-session "a" :status 'done :since (drover-list-test-ago 60))
                               (drover-test-session "b" :status 'idle))
    (let* ((asked nil)
           (drover-changes-prefetch-function
            (lambda (sessions) (push (mapcar #'drover-session-id sessions) asked))))
      (revert-buffer)
      (should (memq 'changes drover-list--prefetch-kinds))
      ;; The collapsed group of idle sessions is not in sight.
      (should (equal asked '(("a"))))
      (drover-list-test-goto-group 'idle)
      (magit-section-show (magit-current-section))
      (drover-list--prefetch)
      (should (equal asked '(("b") ("a"))))
      ;; Its column hidden, a session expanded is asked for alone.
      (setq asked nil
            drover-list-hidden-fields '(harness changes))
      (revert-buffer)
      (should-not (memq 'changes drover-list--prefetch-kinds))
      (should-not asked)
      (drover-list-test-goto "a")
      (magit-section-show (magit-current-section))
      (should (equal asked '(("a")))))))

(ert-deftest drover-list-test-details-tell-what-the-line-cuts ()
  "What a line shows cut, or as an icon alone, its details tell whole; not what its group names."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)
                                 :title "A title far too long for its column, of sixty characters"
                                 :project "drover"))
    (setq drover-list-hidden-fields nil)
    (revert-buffer)
    (drover-list-test-goto "s")
    (magit-section-show (magit-current-section))
    (let ((text (drover-list-test-visible-text)))
      (should (string-match-p "^    Title +A title far too long for its column, of sixty characters$" text))
      ;; The project its column shows whole, not again.
      (should-not (string-match-p "^ +Project" text)))
    ;; Grouped by project, which its group names, not in its details either.
    (let ((drover-group-by 'project)
          (drover-list-hidden-fields '(location)))
      (revert-buffer)
      (drover-list-test-goto "s")
      (magit-section-show (magit-current-section))
      (should-not (string-match-p "^ +Project" (drover-list-test-visible-text))))))

(defun drover-list-test-x (position)
  "Return how far from its window's left edge POSITION is drawn, in pixels.
It is measured from the start of the screen line holding POSITION,
which the display finds, invisible text included."
  (car (window-text-pixel-size nil (save-excursion (goto-char position) (vertical-motion 0) (point))
                               position)))

(defun drover-list-test-text-x (id text)
  "Return where TEXT, searched from the heading of session ID, is drawn."
  (save-excursion
    (drover-list-test-goto id)
    (search-forward text)
    (drover-list-test-x (match-beginning 0))))

(ert-deftest drover-list-test-collapsed-bodies-take-no-room ()
  "Expanding and collapsing moves no line: headings keep their place, details under titles.
Emacs draws a `display' property where invisible text begins: a hidden
body beginning with an aligned space would push the next line right."
  (drover-list-test-with (list (drover-test-session "a" :status 'done :since (drover-list-test-ago 30))
                               (drover-test-session "b" :status 'done :since (drover-list-test-ago 40))
                               (drover-test-session "c" :status 'idle))
    (drover-list-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (drover-list-test-shown
      (let ((titles (mapcar (lambda (id) (drover-list-test-text-x id "Session")) '("a" "b" "c")))
            (group (save-excursion (drover-list-test-goto-group 'idle) (drover-list-test-x (point)))))
        (should (equal titles '(4 4 4)))
        (should (= group 0))
        (dolist (id '("b" "a" "b" "a"))
          (drover-list-test-goto id)
          (drover-list-test-type "TAB")
          (should (equal (mapcar (lambda (id) (drover-list-test-text-x id "Session")) '("a" "b" "c"))
                         titles))
          (unless (drover-list-test-section-hidden-p id)
            ;; The details begin under the title.
            (should (= (drover-list-test-text-x id "Prompt") (drover-list-test-text-x id "Session")))))
        ;; A collapsed group hides its sessions without moving the next.
        (drover-list-test-goto-group 'done)
        (drover-list-test-type "TAB")
        (should (drover-list-test-group-hidden-p 'done))
        (should (= (save-excursion (drover-list-test-goto-group 'idle) (drover-list-test-x (point))) group))
        (should (= (drover-list-test-text-x "c" "Session") 4))))))

;;;; Session at point

(ert-deftest drover-list-test-session-at-point-is-fresh ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (drover-list-test-goto "s")
    (let ((fresh (drover-test-session "s" :status 'done)))
      (setq drover-test-sessions (list fresh))
      (should (eq (drover-session-at-point) fresh)))
    (setq drover-test-sessions nil)
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-list-test-no-session-on-headings ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (goto-char (point-min))
    (should-not (funcall drover-session-at-point-function))
    (drover-list-test-goto-group 'working)
    (should-not (funcall drover-session-at-point-function))
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-list-test-subagent-at-point ()
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-goto "sub")
    (should (equal (drover-session-id (drover-session-at-point)) "sub"))
    (drover-list-test-goto "root")
    (should (equal (drover-session-id (drover-session-at-point)) "root"))))

;;;; Keys

(ert-deftest drover-list-test-keys-reach-backend ()
  (drover-list-test-with (list (drover-test-session "s" :status 'waiting))
    (save-window-excursion
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (drover-list-test-goto "s")
        ;; Opening selects a window, and its buffer: each key goes to the list.
        (dolist (key '("RET" "o" "A" "s" "K" "z" "x" "v"))
          (with-current-buffer buffer
            (drover-list-test-press key)))))
    (should (equal (reverse drover-test-calls)
                   '((:open "s" nil) (:open "s" nil) (:respond "s") (:mark-seen "s")
                     (:interrupt "s") (:archive "s") (:delete "s") (:preview "s"))))))

(ert-deftest drover-list-test-declined-confirmation ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (drover-list-test-goto "s")
      (dolist (key '("K" "x"))
        (should (equal (should-error (drover-list-test-press key) :type 'user-error)
                       '(user-error "Abort")))))
    (should-not drover-test-calls)))

(ert-deftest drover-list-test-archive-closes-after-list ()
  "z archives without asking; a buffer running a turn closes once it ends, the list gone."
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (let ((session-buffer (get-buffer-create "*session s*"))
          (messages nil))
      (setf (plist-get (alist-get 'fake drover--backends) :buffer)
            (lambda (session) (get-buffer (format "*session %s*" (drover-session-id session))))
            (plist-get (alist-get 'fake drover--backends) :close)
            (lambda (session)
              (drover-kill-buffer (get-buffer (format "*session %s*" (drover-session-id session))))
              nil)
            (plist-get (alist-get 'fake drover--backends) :runs-in-buffer) t
            (plist-get (alist-get 'fake drover--backends) :archive)
            (lambda (session)
              (setf (drover-session-archived (drover-list-test-find (drover-session-id session))) 1.0)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked")))
                ((symbol-function 'message)
                 (lambda (format &rest arguments)
                   (push (apply #'format-message format arguments) messages))))
        (drover-list-test-shown
          (drover-list-test-goto "s")
          (drover-list-test-press "z"))
        (should (equal messages '("Archived Session s")))
        (should (buffer-live-p session-buffer))
        (kill-buffer buffer)
        (setq messages nil)
        (setf (drover-session-status (drover-list-test-find "s")) 'done)
        (drover-backend-changed 'fake "s")
        (drover-changed-now)
        (should-not (buffer-live-p session-buffer))
        (should-not messages)))))

(ert-deftest drover-list-test-key-bindings ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (drover-list-test-goto "s")
    (should (eq (key-binding (kbd "RET")) #'drover-open-at-point))
    (should (eq (key-binding (kbd "C-o")) #'drover-list-display))
    (should (eq (key-binding (kbd "q")) #'drover-list-quit))
    (should (eq (key-binding (kbd "g")) #'drover-refresh))
    (should (eq (key-binding (kbd "c")) #'drover-open-subagent))
    (should (eq (key-binding (kbd "V")) #'drover-view))
    (should (eq (key-binding (kbd "K")) #'drover-interrupt))
    ;; The standard keys of Magit sections and special-mode stay.
    (should (eq (key-binding (kbd "TAB")) #'magit-section-toggle))
    (should (eq (key-binding (kbd "n")) #'magit-section-forward))
    (should (eq (key-binding (kbd "p")) #'magit-section-backward))
    (should (eq (key-binding (kbd "M-n")) #'magit-section-forward-sibling))
    (should (eq (key-binding (kbd "SPC")) #'scroll-up-command))
    (let ((parent (keymap-parent drover-list-mode-map)))
      (should (eq (nth 1 parent) drover-session-map))
      (should (eq (nth 2 parent) magit-section-mode-map)))
    ;; RET, C-o, q and the digits are the only keys of the list's own,
    ;; and copying copies sessions, wherever the user binds it.
    (let ((own nil))
      (map-keymap (lambda (event _) (push (key-description (vector event)) own))
                  (let ((map (copy-keymap drover-list-mode-map)))
                    (set-keymap-parent map nil)
                    map))
      (should (seq-set-equal-p own '("RET" "C-o" "q" "<remap>" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))))
    ;; The digits open sessions, rather than show Magit's levels.
    (dolist (key '("1" "4" "9" "0"))
      (should (eq (key-binding (kbd key)) #'drover-list-open-numbered)))
    (should (eq (key-binding (kbd "M-w")) #'drover-list-copy))
    (drover-list-test-goto-group 'working)
    (should (eq (key-binding (kbd "RET")) #'magit-section-toggle))))

(ert-deftest drover-list-test-session-keys-win ()
  "Every session key runs its command, whatever Magit and special-mode bind.
Of their keys, only g is a session key, and it refreshes either way."
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (drover-list-test-goto "s")
    (let ((keys nil))
      (map-keymap (lambda (event command)
                    (let* ((key (vector event))
                           (theirs (lookup-key magit-section-mode-map key)))
                      (push (key-description key) keys)
                      (should (eq (key-binding key) command))
                      (if (equal key [?g])
                          (should (eq theirs #'revert-buffer))
                        (should-not (commandp theirs)))))
                  drover-session-map)
      (should (seq-set-equal-p keys '("o" "A" "<tab>" "E" "K" "s" "u" "c" "r" "x" "z" "Z" "g" "+" "v" "R" "G" "V"))))))

(ert-deftest drover-list-test-session-keys-typed ()
  "Typed through the command loop, each session key does its work.
Redraws keep point on the session."
  (drover-list-test-with
      (list (drover-test-session "root" :status 'working :updated (drover-list-test-ago 10))
            (drover-test-session "s" :parent "root" :status 'waiting :since (drover-list-test-ago 60)
                                 :directory "/work/s/" :branch "feature" :provider-name "Anthropic"
                                 :requests '((:kind question :summary "Which?")))
            (drover-test-session "kid" :parent "s" :status 'idle)
            (drover-test-session "other" :status 'working :updated (drover-list-test-ago 5)
                                 :provider-name "OpenAI"))
    (unwind-protect
        (progn
          (drover-list-test-goto "s")
          (forward-char 3)
          (drover-list-test-shown
            (let ((offset (- (point) (line-beginning-position))))
              (cl-flet ((type (keys)
                          (setq drover-test-calls nil)
                          (drover-list-test-type keys)
                          (should (equal (drover-list-test-id-at-point) "s"))
                          (should (= (- (point) (line-beginning-position)) offset))
                          (reverse drover-test-calls)))
                (should (equal (type "RET") '((:open "s" nil))))
                ;; o opens in another window, which it selects.
                (let ((list-window (selected-window)))
                  (setq drover-test-calls nil)
                  (execute-kbd-macro (kbd "o"))
                  (should (equal drover-test-calls '((:open "s" nil))))
                  (should-not (eq (selected-window) list-window))
                  (should (eq (window-buffer list-window) buffer))
                  (delete-window (selected-window))
                  (select-window list-window)
                  (set-buffer buffer))
                (should (equal (type "A") '((:respond "s"))))
                (should (equal (type "K y") '((:interrupt "s"))))
                (should (equal (type "s") '((:mark-seen "s"))))
                (should (equal (type "u") '((:open "root" nil))))
                (should (equal (type "c") '((:open "kid" nil))))
                (should (equal (type "r Renamed RET") '((:rename "s" "Renamed"))))
                (should (equal (type "x yes RET") '((:delete "s"))))
                (should (equal (type "z y") '((:archive "s"))))
                (should (equal (type "+") '((:new "/work/s/"))))
                (should (equal (type "v") '((:preview "s"))))
                (should (get-buffer-window drover-peek-buffer))
                (push (drover-test-session "new" :status 'failed) drover-test-sessions)
                (should-not (type "g"))
                (should (equal (drover-list-test-groups)
                               '((waiting "root" "s") (failed "new") (working "other"))))
                ;; From active subagents to none: s hides, and its
                ;; parent leaves the group s placed it in.
                (drover-list-test-type "V = u C-g")
                (should (eq drover-show-subagents 'none))
                (should (equal (drover-list-test-groups) '((failed "new") (working "other" "root"))))
                ;; Then to all of them.
                (drover-list-test-type "V = u C-g")
                (should (eq drover-show-subagents 'all))
                (should (equal (drover-list-test-groups)
                               '((waiting "root" "s" "kid") (failed "new") (working "other"))))
                (drover-list-test-goto "s")
                (forward-char 3)
                ;; The View menu, its settings, and the filter it clears.
                (should (equal (type "V - f C-g") '((:open "s" nil))))
                (should drover-follow-mode)
                (should-not (type "V - f C-g"))
                (should-not drover-follow-mode)
                (should-not (type "V = g nothing RET C-g"))
                (should-not drover-group-by)
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "new" "other")))
                (should-not (type "V = o title RET = r C-g"))
                (should (eq drover-sort-by 'title))
                (should drover-sort-descending)
                ;; Roots by title, descending: "Session root", "other", "new".
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "other" "new")))
                (should-not (type "V = f feature RET C-g"))
                (should (equal drover-filter '(:search "feature")))
                (should (equal (drover-list-test-ungrouped) '("root" "s")))
                (should-not (type "V = c C-g"))
                (should-not drover-filter)
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "other" "new")))
                ;; By provider: s's tree, root above it.
                (should-not (type "V = v M-n Anthropic SPC RET C-g"))
                (should (equal drover-filter '(:provider (:include "Anthropic"))))
                (should (equal (drover-list-test-ungrouped) '("root" "s")))
                (should-not (type "V = c = g provider RET C-g"))
                (should (eq drover-group-by 'provider))
                (should (equal (drover-list-test-groups) '((nil "root" "s" "kid" "new") ("OpenAI" "other"))))
                (should-not (bound-and-true-p transient--prefix))))))
      (when (get-buffer drover-peek-buffer)
        (kill-buffer drover-peek-buffer)))))

(ert-deftest drover-list-test-is-a-view ()
  "The list is one of drover's views, where the View menu applies."
  (drover-list-test-with (list (drover-test-session "s"))
    (should (local-variable-p 'drover-view-p))
    (should drover-view-p)
    (should (drover-session-list-p))
    (should (plist-get (drover--context) :view))
    (should (eq drover-session-at-point-function #'drover-list--session-at-point))
    (should (eq drover-main-window-function #'drover-list--target-window))
    (should truncate-lines)
    (should buffer-read-only)
    (should (equal (buffer-name) "*drover sessions*"))
    (with-temp-buffer
      (should-not drover-view-p))))

(ert-deftest drover-list-test-ret-toggles-group ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (drover-list-test-goto-group 'working)
    (drover-list-test-press "RET")
    (should (drover-list-test-group-hidden-p 'working))
    (drover-list-test-press "RET")
    (should-not (drover-list-test-group-hidden-p 'working))
    (should-not drover-test-calls)))

(ert-deftest drover-list-test-moving-through-subagents ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (drover-list-test-goto "root")
    (drover-list-test-press "n")
    (should (equal (drover-list-test-id-at-point) "sub"))
    (drover-list-test-press "n")
    (should (equal (drover-list-test-id-at-point) "deep"))
    (drover-list-test-press "p")
    (should (equal (drover-list-test-id-at-point) "sub"))
    (drover-list-test-press "p")
    (should (equal (drover-list-test-id-at-point) "root"))))

(ert-deftest drover-list-test-g-refreshes ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
    ;; Drawn at once, from what was reported, then listed anew.
    (drover-list-test-shown
      (drover-list-test-press "g")
      (should (timerp drover-list--relist-timer))
      (drover-list-test-run-relist)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(done)))
      (setq drover-test-sessions (list (drover-test-session "s" :status 'idle)))
      (revert-buffer)
      (drover-list-test-run-relist)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(idle))))))

;;;; Older sessions

(ert-deftest drover-list-test-older-sessions ()
  "Older sessions show while asked for, fetched again on g."
  (drover-list-test-with (list (drover-test-session "live" :status 'working))
    (let ((fetches nil))
      (setf (alist-get 'fake drover--backends)
            (append (list :history (lambda (callback) (push callback fetches)))
                    (alist-get 'fake drover--backends)))
      (drover-list-test-shown
        (drover-set-show-history t)
        (should (= (length fetches) 1))
        (should (equal (drover-list--mode-line-process) " by status +older"))
        (funcall (car fetches) (list (drover-test-session "old" :status 'idle)
                                     (drover-test-session "live" :status 'idle)))
        (drover-changed-now)
        (setq drover-list-collapsed-groups nil)
        (revert-buffer)
        ;; g fetched again.
        (should (= (length fetches) 2))
        (should (equal (drover-list-test-groups) '((working "live") (idle "old"))))
        (drover-list-test-goto "old")
        (should (equal (drover-session-id (drover-session-at-point)) "old"))
        (drover-set-show-history nil)
        (should (equal (drover-list-test-groups) '((working "live"))))
        ;; Hidden, g fetches nothing.
        (revert-buffer)
        (should (= (length fetches) 2))))))

;;;; Archived sessions

(ert-deftest drover-list-test-archived-sessions ()
  "Archived sessions show while asked for, dimmed, marked, in a group of their own."
  (drover-list-test-with (list (drover-test-session "live" :status 'working)
                               (drover-test-session "seen" :status 'idle :archived t))
    (let ((fetches nil)
          (opencode-archived-icon "#"))
      (setf (alist-get 'fake drover--backends)
            (append (list :archived (lambda (callback) (push callback fetches))
                          :unarchive (drover-test--recorder :unarchive))
                    (alist-get 'fake drover--backends)))
      (drover-list-test-shown
        ;; A session the backend reports archived among its live ones
        ;; shows only with the others, once asked for.
        (should (equal (drover-list-test-groups) '((working "live"))))
        (drover-set-show-archived t)
        (should (= (length fetches) 1))
        (should (equal (drover-list--mode-line-process) " by status +archived"))
        (funcall (car fetches) (list (drover-test-session "old" :status 'done :archived (drover-list-test-ago 3600)
                                                          :updated (drover-list-test-ago 7200))))
        (drover-changed-now)
        (revert-buffer)
        ;; g fetched again.
        (should (= (length fetches) 2))
        (funcall (car fetches) (list (drover-test-session "old" :status 'done :archived (drover-list-test-ago 3600)
                                                          :updated (drover-list-test-ago 7200))))
        (drover-changed-now)
        (should (equal (drover-list-test-groups) '((working "live") (archived "old" "seen"))))
        (drover-list-test-goto-group 'archived)
        (should (string-match-p "\\`Archived (2)" (string-trim (drover-list-test-line))))
        ;; Archived, a session never needs the user: its group counts it.
        (goto-char (point-min))
        (should (equal (drover-list-test-line) "Working (1)"))
        (should (string-match-p "◐ +Session live" (drover-list-test-heading "live")))
        (should (string-match-p "✓ # Session old" (drover-list-test-heading "old")))
        (save-excursion
          (drover-list-test-goto "old")
          (search-forward "#")
          (should (equal (get-text-property (1- (point)) 'help-echo) "Archived 1h ago"))
          (search-forward "Session old")
          (should (memq 'drover-dim (ensure-list (get-text-property (1- (point)) 'font-lock-face)))))
        ;; Z restores it.
        (drover-list-test-goto "old")
        (drover-list-test-press "Z")
        (should (equal drover-test-calls '((:unarchive "old"))))
        (drover-set-show-archived nil)
        (should (equal (drover-list-test-groups) '((working "live"))))
        (should (equal (drover-list--mode-line-process) " by status"))
        ;; Hidden, g fetches nothing.
        (revert-buffer)
        (should (= (length fetches) 2))))))

(ert-deftest drover-list-test-archived-by-drover ()
  "z archives in drover's own archive a session of a backend without one; Z restores it."
  (drover-list-test-with nil
    (drover-test-with-state
      (let ((shown (list (drover-session-create :backend 'shown :id "t1" :title "Terminal" :status 'idle
                                                :engine 'claude-code :engine-id "conv-1"))))
        (drover-test-register-simple 'shown (lambda () shown))
        (drover-list-test-shown
          (revert-buffer)
          (should (equal (mapcar #'drover-session-key (drover-known-sessions)) '((shown . "t1"))))
          (goto-char (oref (drover-list-test-section "t1" 'shown) start))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (drover-list-test-press "z"))
          (drover-changed-now)
          (should-not (drover-list-test-section "t1" 'shown))
          (drover-set-show-archived t)
          (should (equal (mapcar #'car (drover-list-test-groups)) '(archived)))
          (goto-char (oref (drover-list-test-section "t1" 'shown) start))
          (should (drover-session-archived (drover-session-at-point)))
          (drover-list-test-press "Z")
          (drover-changed-now)
          (should (equal (mapcar #'car (drover-list-test-groups)) '(idle)))
          (drover-set-show-archived nil))))))

;;;; Refreshing

(ert-deftest drover-list-test-refresh-follows-session ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (forward-char 4)
    (let ((offset (- (point) (line-beginning-position))))
      ;; It moves from Working to the top of Needs you.
      (setf (drover-session-status (drover-list-test-find "work")) 'waiting)
      (setf (drover-session-since (drover-list-test-find "work")) (drover-list-test-ago 9999))
      (drover-list-test-refresh)
      (should (equal (drover-list-test-groups)
                     '((waiting "work" "wait1" "wait2") (done "done") (idle "idle"))))
      (should (equal (drover-list-test-id-at-point) "work"))
      (should (= (- (point) (line-beginning-position)) offset)))))

(ert-deftest drover-list-test-refresh-keeps-line-in-details ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "done")
    (magit-section-show (magit-current-section))
    (forward-line 2)
    (let ((line (drover-list-test-line)))
      ;; A new group above pushes every line down.
      (push (drover-test-session "new" :status 'failed) drover-test-sessions)
      (drover-list-test-refresh)
      (should (assq 'failed (drover-list-test-groups)))
      (should (equal (drover-list-test-id-at-point) "done"))
      (should (equal (drover-list-test-line) line)))))

(ert-deftest drover-list-test-refresh-same-group ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "wait1")
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait1"))
                                           drover-test-sessions))
    (drover-list-test-refresh)
    ;; The session that took its place in the group.
    (should (equal (drover-list-test-id-at-point) "wait2"))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait2"))
                                           drover-test-sessions))
    (drover-list-test-refresh)
    ;; The group is gone: point stays on the same line.
    (should (equal (drover-list-test-id-at-point) "done"))))

(ert-deftest drover-list-test-refresh-into-collapsed-group ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (setf (drover-session-status (drover-list-test-find "work")) 'idle)
    (drover-list-test-refresh)
    ;; Idle is collapsed: its heading stands for the session.
    (should (drover-list-test-group-hidden-p 'idle))
    (should (equal (drover-list-test-line) "Idle (2)"))
    (should-not (invisible-p (point)))))

(ert-deftest drover-list-test-refresh-keeps-group-heading ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto-group 'done)
    (setq drover-test-sessions (cons (drover-test-session "new" :status 'waiting) drover-test-sessions))
    (revert-buffer)
    (should (equal (drover-list-test-line) "Done (1)"))))

(ert-deftest drover-list-test-refresh-keeps-window-point-and-start ()
  (drover-list-test-with (cl-loop for i below 60
                                  collect (drover-test-session (format "s%02d" i) :status 'working
                                                               :updated (- 1000.0 i)))
    (save-window-excursion
      (let ((window (selected-window)))
        (set-window-buffer window buffer)
        (drover-list-test-goto "s30")
        (set-window-point window (point))
        (let ((start (save-excursion (forward-line -5) (point))))
          (set-window-start window start)
          (let ((start-line (line-number-at-pos start)))
            ;; s45 moves to the top: everything shifts down a line.
            (setf (drover-session-updated (nth 45 drover-test-sessions)) 2000.0)
            (revert-buffer)
            (should (equal (with-selected-window window (drover-list-test-id-at-point)) "s30"))
            (should (equal (save-excursion (goto-char (window-point window))
                                           (drover-list-test-id-at-point))
                           "s30"))
            (should (= (line-number-at-pos (window-start window)) start-line))))))))

;;;; Drawing in place

(defun drover-list-test-without-sections (string)
  "Return STRING without its sections, which belong to one drawing, as text."
  (let ((string (copy-sequence string)))
    (remove-list-of-text-properties 0 (length string) '(magit-section) string)
    string))

(defun drover-list-test-sections ()
  "Return the sections of the current list as data: type, value, positions, visibility."
  (let ((sections nil))
    (magit-map-sections
     (lambda (section)
       (push (list (oref section type) (oref section value)
                   (drover-list-test--position (oref section start))
                   (drover-list-test--position (oref section content))
                   (drover-list-test--position (oref section end))
                   (oref section hidden)
                   (and (oref section washer) t))
             sections)))
    (nreverse sections)))

(defun drover-list-test--position (position)
  "Return POSITION, a marker or a number, as a number."
  (if (markerp position) (marker-position position) position))

(defun drover-list-test-marks ()
  "Return what the current list follows in its text as data, by position.
That is the prompt caches drawn, as (KEY KIND START BAR), BAR the span
of the overlay of a background, and the animated glyphs, as (POSITION
. KIND), both in order: a drawing in place must leave them where a
drawing anew puts them."
  (list (sort (mapcar (lambda (entry)
                        (let ((overlay (plist-get entry :overlay)))
                          (list (drover-session-key (plist-get entry :session))
                                (plist-get entry :kind)
                                (marker-position (plist-get entry :start))
                                (and overlay (overlay-buffer overlay)
                                     (cons (overlay-start overlay) (overlay-end overlay))))))
                      drover-list--caches)
              (lambda (a b) (< (nth 2 a) (nth 2 b))))
        (sort (mapcar (lambda (entry) (cons (marker-position (car entry)) (cdr entry)))
                      drover-list--animated)
              (lambda (a b) (< (car a) (car b))))))

(defun drover-list-test-fresh (&optional list)
  "Return LIST drawn anew, from nothing, in a buffer of its own.
LIST is a list buffer, the current one by default.  The value is (TEXT
SECTIONS HEADER-LINE MARKS), TEXT without sections, MARKS as
`drover-list-test-marks' returns them.  The buffer has the
visibility of LIST, is drawn for its width, and is killed."
  (let* ((list (or list (current-buffer)))
         ;; The rows left for later past the first screens, drawn.
         (_ (with-current-buffer list
              (drover-list--draw-undrawn)))
         (visibility (with-current-buffer list (drover-list--remember-visibility)))
         (visibility (copy-hash-table (buffer-local-value 'drover-list--visibility list)))
         (recaps (buffer-local-value 'drover-list--recaps list))
         (width (buffer-local-value 'drover-list--width list))
         (drawn (with-current-buffer list
                  (mapcar #'drover-list--group-value
                          (seq-remove #'drover-list--deferred-p
                                      (drover-list--model-groups drover-list--model)))))
         (fresh (generate-new-buffer "*drover sessions fresh*")))
    (unwind-protect
        (with-current-buffer fresh
          ;; The sidebar's mode for the sidebar, which folds nothing.
          (funcall (buffer-local-value 'major-mode list))
          (setq drover-list--visibility visibility
                drover-list--recaps (and recaps (copy-hash-table recaps)))
          ;; Drawn from nothing: no row nor width of another list.
          (cl-letf (((symbol-function 'drover-list--line-width) (lambda () width))
                    ((symbol-function 'drover-list--buffers) (lambda () (list fresh))))
            (drover-list--render)
            (drover-list--draw-undrawn)
            ;; The collapsed groups the list drew, drawn too.
            (dolist (group (drover-list--model-groups drover-list--model))
              (when (member (drover-list--group-value group) drawn)
                (drover-list--draw-group group))))
          (list (drover-list-test-without-sections (buffer-string))
                (drover-list-test-sections)
                header-line-format
                (drover-list-test-marks)))
      (kill-buffer fresh))))

(defun drover-list-test-drawn (&optional list)
  "Return LIST, the current list by default, as `drover-list-test-fresh' returns a fresh one.
The rows left for later past the first screens are drawn first."
  (with-current-buffer (or list (current-buffer))
    (drover-list--draw-undrawn)
    (list (drover-list-test-without-sections (buffer-string))
          (drover-list-test-sections)
          header-line-format
          (drover-list-test-marks))))

(defun drover-list-test-random-sessions (count)
  "Return COUNT fake sessions of random statuses, places and times, a third subagents.
Some stand for sessions run elsewhere, their origin another harness.
Every third holds a prompt cache, of an hour's window, so that the
caches drawn move as the lines do; whether it does follows from its
place alone, which leaves the random sessions as they were."
  (let ((sessions nil))
    (dotimes (index count)
      (let* ((id (format "r%02d" index))
             (parent (and (> index 3) (zerop (random 3))
                          (drover-session-id (nth (random (length sessions)) sessions))))
             (status (seq-random-elt '(waiting failed done retrying working idle idle idle))))
        (push (drover-list-test-titled
               id (concat "Session " id (make-string (random 40) ?x))
               :parent parent :status status
               :since (drover-list-test-ago (+ 1 (random 7200)))
               :updated (drover-list-test-ago (+ 1 (random 7200)))
               :created (drover-list-test-ago (+ 7200 (random 7200)))
               :project (seq-random-elt '("alpha" "beta" nil))
               :model (seq-random-elt '("Opus 5.5" "Sonnet" nil))
               :effort (seq-random-elt '("high" nil))
               :context-tokens (seq-random-elt '(nil 12000 180000))
               :context-limit 200000
               :origin (seq-random-elt '(nil nil opencode agent-shell))
               :requests (and (eq status 'waiting) '((:kind question :summary "Which file?")))
               :cache-expires (and (zerop (% index 3)) (+ drover-test-now 600 (* 60 index)))
               :cache-ttl (and (zerop (% index 3)) 3600))
              sessions)))
    (nreverse sessions)))

(defun drover-list-test-mutate (sessions)
  "Change SESSIONS at random as a backend would, and return what it reports.
The value is (SESSIONS . IDS), IDS the sessions changed, came or went.
Some changes are the events of a turn: the last activity a little
later, within the tolerance or past it, or a turn starting."
  (let* ((index (random (length sessions)))
         (old (nth index sessions))
         (new (copy-sequence old))
         (id (drover-session-id old)))
    (pcase (random 13)
      (0 (setf (drover-session-status new) (seq-random-elt '(waiting failed done retrying working idle))))
      (1 (setf (drover-session-updated new) (drover-list-test-ago (random 7200))))
      (2 (setf (drover-session-context-tokens new) (+ 1000 (random 199000))))
      (3 (setf (drover-session-title new) (concat "Renamed " id (make-string (random 50) ?y))))
      (4 (setf (drover-session-since new) (drover-list-test-ago (random 7200))))
      (5 (setf (drover-session-project new) (seq-random-elt '("alpha" "beta" "gamma" nil))))
      (6 (setf (drover-session-effort new) (seq-random-elt '("high" "low" nil))))
      (7 (setq new nil))
      (8 (setq new (drover-list-test-titled (format "n%03d" (random 1000)) "New subagent"
                                            :parent id :status (seq-random-elt '(working idle done))
                                            :updated (drover-list-test-ago (random 60)))
               id (drover-session-id new)))
      ((or 9 10) (setf (drover-session-updated new)
                       (+ (or (drover-session-updated old) (drover-list-test-ago 7200)) (random 90))))
      (11 (setf (drover-session-status new) (seq-random-elt '(working retrying))
                (drover-session-since new) (drover-list-test-ago (random 120))))
      (12 (setf (drover-session-origin new) (seq-random-elt '(nil opencode claude-code-ide)))))
    (cond ((null new) (cons (remq old sessions) (list id)))
          ((equal (drover-session-id new) (drover-session-id old))
           (cons (append (seq-take sessions index) (list new) (nthcdr (1+ index) sessions)) (list id)))
          (t (cons (append sessions (list new)) (list id))))))

(defmacro drover-list-test-counting-redraws (&rest body)
  "Run BODY counting the drawings anew of the list, and return the count."
  (declare (indent 0) (debug t))
  `(let ((count 0)
         (render (symbol-function 'drover-list--render)))
     (cl-letf (((symbol-function 'drover-list--render)
                (lambda () (cl-incf count) (funcall render))))
       ,@body)
     count))

(defun drover-list-test-in-place-check (mode width grouping sort descending tolerance)
  "Check that the list drawn in place is the list drawn anew after random changes.
After each of many random changes, with `drover-show-subagents' MODE,
WIDTH, `drover-group-by' GROUPING, `drover-sort-by' SORT,
`drover-sort-descending' DESCENDING and
`drover-sort-activity-tolerance' TOLERANCE: its text and properties,
its sections and their places, its header line, some sessions
expanded, groups collapsed and expanded, their sessions drawn or not
yet.  Most changes are drawn in place.  The random changes follow
from the settings alone."
  (random (format "drover %s %s %s %s %s %s" mode width grouping sort descending tolerance))
  (drover-list-test-with (drover-list-test-random-sessions 40)
    (drover-list-test-with-font
      (let ((drover-list-test-width width)
            (drover-show-subagents mode)
            (drover-group-by grouping)
            (drover-sort-by sort)
            (drover-sort-descending descending)
            (drover-sort-activity-tolerance tolerance)
            (drover-list-collapsed-groups nil)
            ;; The harness too, some sessions of another.
            (drover-list-hidden-fields nil)
            (in-place 0))
        (drover-list-test-shown
          (revert-buffer)
          ;; Some sessions show their details.
          (dolist (section (seq-take (seq-filter (lambda (section) (oref section hidden))
                                                 (mapcan #'drover-list--session-sections
                                                         (drover-list--containers)))
                                     3))
            (magit-section-show section))
          (dotimes (step 30)
            ;; Now and then, a group is expanded or collapsed.
            (when (zerop (random 5))
              (when-let* ((groups (drover-list--groups)))
                (let ((group (seq-random-elt groups)))
                  (if (oref group hidden) (magit-section-show group) (magit-section-hide group)))))
            (pcase-let ((`(,sessions . ,ids) (drover-list-test-mutate drover-test-sessions)))
              (setq drover-test-sessions sessions)
              (dolist (id ids)
                (drover-backend-changed 'fake id))
              (when (zerop (drover-list-test-counting-redraws (drover-changed-now)))
                (cl-incf in-place))
              (let ((drawn (drover-list-test-drawn))
                    (fresh (drover-list-test-fresh)))
                (should (equal (list mode width grouping sort descending step (cdr drawn))
                               (list mode width grouping sort descending step (cdr fresh))))
                (should (equal-including-properties (car drawn) (car fresh))))))
          (should (> in-place 15)))))))

(defmacro drover-list-test-in-place-cases (&rest cases)
  "Define a test of `drover-list-test-in-place-check' for each of CASES, its arguments.
Each runs alone, so that the cases run in several Emacs at once."
  (declare (indent 0))
  `(progn
     ,@(seq-map-indexed
        (lambda (case index)
          `(ert-deftest ,(intern (format "drover-list-test-in-place-is-a-fresh-drawing-%d" (1+ index))) ()
             ,(format "Whatever sessions change, the list drawn in place is the list drawn anew.
Its case %S of `drover-list-test-in-place-check': every subagent
mode, columns and two lines, grouped by status, project or activity or
not at all, sorted by urgency, activity or title, either way, with the
tolerance of activity or without, over the cases."
                      case)
             (drover-list-test-in-place-check ,@(mapcar (lambda (value) `',value) case))))
        cases)))

(drover-list-test-in-place-cases
  (all 1000 status urgency nil 60) (active 300 status urgency nil 60)
  (none 1000 project updated nil 60) (all 1000 project urgency nil 60)
  (all 1000 updated title nil 60) (active 1000 nil urgency nil 60)
  (all 1000 nil updated t 60) (active 300 nil updated nil 60)
  (all 1000 updated updated t 60) (all 1000 nil updated t nil))

(defun drover-list-test-is-fresh ()
  "Check that the list is the list drawn anew, text, properties and sections."
  (let ((drawn (drover-list-test-drawn))
        (fresh (drover-list-test-fresh)))
    (should (equal (cdr drawn) (cdr fresh)))
    (should (equal-including-properties (car drawn) (car fresh)))))

(ert-deftest drover-list-test-closed-conversations-in-place ()
  "Front ends opening and closing recorded conversations are drawn in place, as drawn anew.
A conversation a front end follows is its session; closed, the record
of a backend listing what the engine recorded stands for it."
  (random "drover closed conversations")
  (let ((records (mapcar (lambda (index)
                           (drover-list-test-titled
                            (format "c%02d" index) (format "Closed conversation %d" index)
                            :updated (drover-list-test-ago (* 600 (1+ index)))
                            :engine 'fake :engine-id (format "c%02d" index)))
                         (number-sequence 0 19))))
    (dolist (record records)
      (setf (drover-session-backend record) 'records))
    (drover-list-test-with nil
      (drover-test-register-simple 'records (lambda () records) :recorded '(fake)
                                   :session (lambda (id)
                                              (seq-find (lambda (record) (equal (drover-session-id record) id))
                                                        records)))
      (let ((in-place 0))
        (drover-list-test-shown
          (revert-buffer)
          (dotimes (step 30)
            (let* ((open (mapcar #'drover-session-engine-id drover-test-sessions))
                   (id (format "c%02d" (random 20))))
              (if (member id open)
                  (setq drover-test-sessions
                        (seq-remove (lambda (session) (equal (drover-session-engine-id session) id))
                                    drover-test-sessions))
                (push (drover-list-test-titled (concat "t-" id) (concat "Terminal " id)
                                               :status (seq-random-elt '(working idle done))
                                               :updated (drover-list-test-ago (random 60))
                                               :engine 'fake :engine-id id)
                      drover-test-sessions))
              (drover-backend-changed 'fake (concat "t-" id))
              (when (zerop (drover-list-test-counting-redraws (drover-changed-now)))
                (cl-incf in-place))
              (should (equal (sort (mapcar (lambda (session) (drover-session-engine-id session))
                                           (drover-index-sessions))
                                   #'string<)
                             (mapcar (lambda (index) (format "c%02d" index)) (number-sequence 0 19))))
              (drover-list-test-is-fresh)))
          (should (> in-place 20)))))))

(ert-deftest drover-list-test-collapsed-groups-drawn-later ()
  "Collapsed groups draw their sessions once expanded, searched, copied, or idle."
  (drover-list-test-with (list (drover-list-test-titled "w" "Working away" :status 'working
                                                        :updated (drover-list-test-ago 5))
                               (drover-list-test-titled "i" "Quietly idle" :status 'idle
                                                        :updated (drover-list-test-ago 50))
                               (drover-list-test-titled "j" "Also idle" :status 'idle
                                                        :updated (drover-list-test-ago 60)))
    (drover-list-test-shown
      (cl-flet ((idle () (seq-find (lambda (group) (equal (oref group value) '(status . idle)))
                                   (drover-list--groups)))
                (drawn-p () (string-search "Quietly idle" (buffer-string))))
        ;; Drawn, the idle group, collapsed, holds no session yet.
        (should (oref (idle) hidden))
        (should-not (drawn-p))
        (should (timerp drover-list--deferred-timer))
        ;; Once idle, its sessions are drawn, and it stays collapsed.
        (let ((timer drover-list--deferred-timer))
          (cancel-timer timer)
          (apply (timer--function timer) (timer--args timer)))
        (should (drawn-p))
        (should (oref (idle) hidden))
        (should-not (string-match-p "Quietly" (drover-list-test-visible-text)))
        (should (equal (mapcar #'cdr (mapcar (lambda (section) (oref section value))
                                             (oref (idle) children)))
                       '("i" "j")))
        ;; Searching draws them, so that Isearch finds them as it finds
        ;; hidden text.
        (revert-buffer)
        (should-not (drawn-p))
        (should (memq #'drover-list--draw-deferred isearch-mode-hook))
        (run-hooks 'isearch-mode-hook)
        (should (drawn-p))
        (should (oref (idle) hidden))
        ;; Copying a region over the group copies its sessions' lines.
        (magit-section-hide (idle))
        (revert-buffer)
        (should-not (drawn-p))
        (let ((transient-mark-mode t)
              (kill-ring nil))
          (goto-char (oref (idle) start))
          (push-mark (point-max) t t)
          (drover-list-copy t)
          (should (string-match-p "Quietly idle" (car kill-ring))))
        ;; Expanded, a group's sessions are drawn, and drawn in place as
        ;; they change.
        (magit-section-hide (idle))
        (revert-buffer)
        (should-not (drawn-p))
        (setf (drover-session-title (drover-list-test-find "j")) "Also IDLE")
        (drover-backend-changed 'fake "j")
        (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
        (should-not (drawn-p))
        (magit-section-show (idle))
        (should (drawn-p))
        (should (string-match-p "Also IDLE" (drover-list-test-visible-text)))
        (drover-list-test-is-fresh)))))

(ert-deftest drover-list-test-hidden-list-draws-collapsed-groups-once-shown ()
  "A hidden list leaves the sessions of its collapsed groups undrawn until shown."
  (drover-list-test-with (list (drover-list-test-titled "w" "Working away" :status 'working)
                               (drover-list-test-titled "i" "Quietly idle" :status 'idle))
    (cl-flet ((drawn-p () (string-search "Quietly idle" (buffer-string)))
              (run-timer ()
                (let ((timer drover-list--deferred-timer))
                  (cancel-timer timer)
                  (apply (timer--function timer) (timer--args timer)))))
      (should-not (get-buffer-window buffer t))
      (should (timerp drover-list--deferred-timer))
      (run-timer)
      (should-not (drawn-p))
      (should-not drover-list--deferred-timer)
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (drover-list--on-window-change (selected-window))
        (should (timerp drover-list--deferred-timer))
        (run-timer)
        (should (drawn-p))))))

(ert-deftest drover-list-test-collapsed-groups-drawn-a-few-at-a-time ()
  "While idle, a collapsed group's sessions are drawn a few at a time, stopping for input.
Sessions change meanwhile, drawn or not; once all are drawn, the list
is the list drawn anew."
  (let ((sessions (cl-loop for index below 12
                           collect (drover-list-test-titled (format "i%02d" index) (format "Idle %02d" index)
                                                            :status 'idle
                                                            :updated (drover-list-test-ago (* 10 (1+ index))))))
        (drover-list--deferred-chunk 2))
    (drover-list-test-with (cons (drover-list-test-titled "w" "Working" :status 'working
                                                          :updated (drover-list-test-ago 1))
                                 sessions)
      (drover-list-test-shown
        (let ((idle (seq-find (lambda (group) (equal (oref group value) '(status . idle)))
                              (drover-list--groups)))
              (looks 0))
          (should-not (oref idle children))
          ;; Input comes after the second look.
          (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) (> (cl-incf looks) 2))))
            (let ((timer drover-list--deferred-timer))
              (cancel-timer timer)
              (apply (timer--function timer) (timer--args timer))))
          (should (equal (mapcar (lambda (section) (cdr (oref section value))) (oref idle children))
                         '("i00" "i01" "i02" "i03")))
          (should (oref idle hidden))
          (should (timerp drover-list--deferred-timer))
          ;; Sessions drawn and not change, come and go, in place.
          (dolist (id '("i01" "i09"))
            (setf (drover-session-title (drover-list-test-find id)) (format "IDLE %s" (substring id 1)))
            (drover-backend-changed 'fake id))
          (setf (drover-session-updated (drover-list-test-find "i10")) (drover-list-test-ago 25))
          (drover-backend-changed 'fake "i10")
          (setf (drover-session-updated (drover-list-test-find "i02")) (drover-list-test-ago 500))
          (drover-backend-changed 'fake "i02")
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (should (equal (mapcar (lambda (section) (cdr (oref section value))) (oref idle children))
                         '("i00" "i01" "i10" "i03")))
          ;; Idle again, the rest is drawn.
          (let ((timer drover-list--deferred-timer))
            (cancel-timer timer)
            (apply (timer--function timer) (timer--args timer)))
          (should-not (drover-list--deferred-p (drover-list--model-group idle)))
          (should (oref idle hidden))
          (should (equal (length (oref idle children)) 12))
          (drover-list-test-is-fresh))))))

(ert-deftest drover-list-test-in-place-changes ()
  "Sessions changing, moving, coming and going are drawn in place; a new layout anew."
  (drover-list-test-with (list (drover-list-test-titled "a" "Alpha" :status 'working
                                                        :updated (drover-list-test-ago 10)
                                                        :context-tokens 1000 :context-limit 200000)
                               (drover-list-test-titled "b" "Bravo" :status 'working
                                                        :updated (drover-list-test-ago 90)
                                                        :context-tokens 1000 :context-limit 200000)
                               (drover-list-test-titled "c" "Cee" :status 'done
                                                        :since (drover-list-test-ago 30)))
    (drover-list-test-shown
      (let ((root magit-root-section)
            (section (drover-list-test-section "a")))
        (cl-flet ((change (id function)
                    (when function
                      (funcall function (drover-list-test-find id)))
                    (drover-backend-changed 'fake id)
                    (prog1 (drover-list-test-counting-redraws (drover-changed-now))
                      (drover-list-test-is-fresh))))
          ;; As wide as it was, its context leaves the columns as they
          ;; are: its line alone is drawn again.
          (should (zerop (change "a" (lambda (session) (setf (drover-session-context-tokens session) 5000)))))
          (should (eq magit-root-section root))
          (should (eq (drover-list-test-section "a") section))
          (should (string-match-p "5k" (drover-list-test-heading "a")))
          ;; Another passing it, its new turn starting, moves a line, in
          ;; place.
          (should (zerop (change "b" (lambda (session)
                                       (setf (drover-session-since session) (drover-list-test-ago 0)
                                             (drover-session-updated session) (drover-list-test-ago 0))))))
          (should (equal (drover-list-test-order) '("c" "b" "a")))
          ;; So do a session going to another group, coming or going,
          ;; and groups coming and going with them.
          (should (zerop (change "a" (lambda (session) (setf (drover-session-status session) 'done)))))
          (should (equal (drover-list-test-groups) '((done "c" "a") (working "b"))))
          (setq drover-test-sessions (append drover-test-sessions
                                             (list (drover-list-test-titled "d" "Delta" :status 'idle))))
          (should (zerop (change "d" nil)))
          (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "c"))
                                                 drover-test-sessions))
          (should (zerop (change "c" nil)))
          (should (equal (drover-list-test-groups) '((done "a") (working "b") (idle "d"))))
          (should (zerop (change "b" (lambda (session) (setf (drover-session-status session) 'waiting)))))
          (should (equal (drover-list-test-groups) '((waiting "b") (done "a") (idle "d"))))
          (should (eq magit-root-section root))
          ;; Wider than any, a context widens its column: the list is
          ;; drawn anew.
          (should (= (change "a" (lambda (session) (setf (drover-session-context-tokens session) 50000))) 1))
          (should (string-match-p "50k" (drover-list-test-heading "a")))
          ;; A hidden subagent changing redraws what tells of it: its
          ;; parent's line, in place.
          (setq drover-test-sessions (append drover-test-sessions
                                             (list (drover-test-session "q" :parent "d" :status 'working))))
          (change "q" nil)
          (let ((drover-show-subagents 'none))
            (drover-list--render)
            (should (string-match-p "\\*" (drover-list-test-heading "d")))
            (should (zerop (change "q" (lambda (session) (setf (drover-session-title session) "Quebec")))))
            (save-excursion
              (drover-list-test-goto "d")
              (should (string-match-p "Quebec" (get-text-property (+ (point) 2) 'help-echo))))))))))

(ert-deftest drover-list-test-in-place-keeps-point ()
  "Lines drawn in place keep point, the mark and window starts where they were."
  (drover-list-test-with (cl-loop for i below 60
                                  collect (drover-list-test-titled (format "s%02d" i) (format "Session %02d" i)
                                                                   :status 'working
                                                                   :updated (- 1000.0 i)))
    (save-window-excursion
      (let ((window (selected-window))
            (transient-mark-mode t))
        (set-window-buffer window buffer)
        (drover-list-test-goto "s30")
        (forward-char 5)
        (set-window-point window (point))
        (let* ((start (save-excursion (forward-line -5) (point)))
               (start-line (line-number-at-pos start))
               (column (- (point) (line-beginning-position))))
          (set-window-start window start)
          (push-mark (save-excursion (drover-list-test-goto "s32") (point)) t t)
          ;; The line of point, and one above it, change in place.
          (dolist (id '("s30" "s28"))
            (setf (drover-session-title (drover-list-test-find id)) (concat "Sess " id))
            (drover-backend-changed 'fake id))
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (should (string-match-p "Sess s30" (drover-list-test-heading "s30")))
          (should (equal (drover-list-test-id-at-point) "s30"))
          (should (= (- (point) (line-beginning-position)) column))
          (should (= (window-point window) (point)))
          (should (= (line-number-at-pos (window-start window)) start-line))
          (should (region-active-p))
          (should (equal (save-excursion (goto-char (mark)) (drover-list-test-id-at-point)) "s32"))
          ;; A session moving above point, in place, keeps point on its
          ;; session and the window's start on its line, as a drawing
          ;; anew does.
          (setf (drover-session-updated (drover-list-test-find "s45")) 2000.0)
          (drover-backend-changed 'fake "s45")
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (should (equal (car (drover-list-test-order)) "s45"))
          (should (equal (drover-list-test-id-at-point) "s30"))
          (should (= (- (point) (line-beginning-position)) column))
          (should (= (window-point window) (point)))
          (should (= (line-number-at-pos (window-start window)) start-line))
          (should (region-active-p))
          (should (equal (save-excursion (goto-char (mark)) (drover-list-test-id-at-point)) "s32"))
          ;; Point on a session going is on the session taking its place.
          (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "s30"))
                                                 drover-test-sessions))
          (drover-backend-changed 'fake "s30")
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (should (equal (drover-list-test-id-at-point) "s31")))))))

;;;; Sessions running together

(defun drover-list-test-running ()
  "Return three sessions running turns, started a minute and more apart, and one idle."
  (list (drover-list-test-titled "a" "Alpha" :status 'working :since (drover-list-test-ago 300)
                                 :updated (drover-list-test-ago 5))
        (drover-list-test-titled "b" "Bravo" :status 'working :since (drover-list-test-ago 200)
                                 :updated (drover-list-test-ago 3))
        (drover-list-test-titled "c" "Charlie" :status 'working :since (drover-list-test-ago 100)
                                 :updated (drover-list-test-ago 1))
        (drover-list-test-titled "i" "Idle" :status 'idle :updated (drover-list-test-ago 30))))

(defvar drover-list-test-event-time drover-test-now
  "The time of the last event `drover-list-test-stream' made.")

(defun drover-list-test-stream (ids steps)
  "Have sessions IDS take STEPS events in turn, and return the orders listed.
Each event sets the session's last activity a second after the event
before, from `drover-list-test-event-time', as a turn streaming does;
the clock of the list stands still, so that ages read alike.  The list
follows each in place, as a drawing anew draws it."
  (let ((orders nil))
    (dotimes (step steps)
      (let ((id (nth (% step (length ids)) ids)))
        (setq drover-list-test-event-time (1+ drover-list-test-event-time))
        (setf (drover-session-updated (drover-list-test-find id)) drover-list-test-event-time)
        (drover-backend-changed 'fake id)
        (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
        (drover-list-test-is-fresh)
        (push (drover-list-test-order) orders)))
    (delete-dups (nreverse orders))))

(ert-deftest drover-list-test-default-view ()
  "By default, the list is ungrouped: sessions running first, newest turn first, then the most recently active."
  (drover-list-test-with (append (drover-list-test-running)
                                 (list (drover-list-test-titled "d" "Done" :status 'done
                                                                :since (drover-list-test-ago 10)
                                                                :updated (drover-list-test-ago 12))
                                       (drover-list-test-titled "w" "Waiting" :status 'waiting
                                                                :since (drover-list-test-ago 50)
                                                                :updated (drover-list-test-ago 600))))
    (cl-flet ((standard (symbol) (eval (car (get symbol 'standard-value)) t)))
      (let ((drover-group-by (standard 'drover-group-by))
            (drover-sort-by (standard 'drover-sort-by))
            (drover-sort-descending (standard 'drover-sort-descending)))
        (drover-list-test-shown
          (revert-buffer)
          (should-not (drover-list-test-groups))
          (should (equal (drover-list-test-ungrouped) '("c" "b" "a" "d" "i" "w")))
          (should (equal (drover-list--mode-line-process) " ungrouped, last activity ↓")))))))

(ert-deftest drover-list-test-running-sessions-keep-their-places ()
  "Sessions streaming the events of their turns never swap, however they alternate.
By last activity, newest first, they come before the others, newest turn
first; oldest first, after them, oldest turn first; by urgency, newest
turn first among the working ones."
  (pcase-dolist (`(,grouping ,sort ,descending ,order)
                 '((nil updated t ("c" "b" "a" "i"))
                   (nil updated nil ("i" "a" "b" "c"))
                   (updated updated t ("c" "b" "a" "i"))
                   (status urgency nil ("c" "b" "a" "i"))))
    (drover-list-test-with (drover-list-test-running)
      (let ((drover-list-test-event-time drover-test-now)
            (drover-group-by grouping)
            (drover-sort-by sort)
            (drover-sort-descending descending)
            (drover-list-collapsed-groups nil))
        (drover-list-test-shown
          (revert-buffer)
          (should (equal (drover-list-test-order) order))
          ;; Two, then three, alternating for minutes.
          (should (equal (drover-list-test-stream '("a" "b") 20) (list order)))
          (should (equal (drover-list-test-stream '("c" "a" "b") 200) (list order)))
          ;; A new turn starting moves its session, in place.
          (setf (drover-session-since (drover-list-test-find "a")) (cl-incf drover-list-test-event-time))
          (drover-backend-changed 'fake "a")
          (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
          (drover-list-test-is-fresh)
          (should (equal (drover-list-test-order)
                         (if (and (eq sort 'updated) (not descending))
                             '("i" "b" "c" "a")
                           '("a" "c" "b" "i")))))))))

(ert-deftest drover-list-test-activity-tolerance ()
  "Sessions whose activity moves within the tolerance keep their places; beyond it, they move.
Here running sessions whose backend does not tell when their turns
started, each placed at its last event: those moving by less than
`drover-sort-activity-tolerance' never pass one another; one moving
further does, in place.  Without a tolerance, every event moves them."
  (cl-flet ((sessions ()
              (list (drover-list-test-titled "a" "Alpha" :status 'working :updated (drover-list-test-ago 20))
                    (drover-list-test-titled "b" "Bravo" :status 'working :updated (drover-list-test-ago 10))
                    (drover-list-test-titled "c" "Charlie" :status 'working :updated (drover-list-test-ago 0)))))
    (pcase-dolist (`(,descending ,order ,moved) '((t ("c" "b" "a") ("a" "c" "b"))
                                                  (nil ("a" "b" "c") ("b" "c" "a"))))
      (drover-list-test-with (sessions)
        (let ((drover-list-test-event-time drover-test-now)
              (drover-group-by nil)
              (drover-sort-by 'updated)
              (drover-sort-descending descending))
          (drover-list-test-shown
            (revert-buffer)
            (should (equal (drover-list-test-order) order))
            ;; Within a minute of their places, a and b, streaming, do
            ;; not pass c, though newer.
            (should (equal (drover-list-test-stream '("a" "b") 40) (list order)))
            ;; a's events, more than a minute after its place, move it.
            (cl-incf drover-list-test-event-time 30)
            (should (equal (drover-list-test-stream '("a") 1) (list moved)))))))
    (drover-list-test-with (sessions)
      (let ((drover-list-test-event-time drover-test-now)
            (drover-group-by nil)
            (drover-sort-by 'updated)
            (drover-sort-descending t)
            (drover-sort-activity-tolerance nil))
        (drover-list-test-shown
          (revert-buffer)
          (should (equal (drover-list-test-stream '("a" "b") 4)
                         '(("a" "c" "b") ("b" "a" "c") ("a" "b" "c")))))))))

(ert-deftest drover-list-test-running-in-both-lists ()
  "The sidebar and the list keep sessions streaming in the same places."
  (drover-list-test-two-lists
    (drover-set-group-by nil)
    (drover-set-sort 'updated t)
    (setq drover-test-sessions (drover-list-test-running))
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (dotimes (step 30)
      (let ((drover-list-test-event-time (+ drover-test-now step))
            (id (nth (% step 3) '("a" "b" "c"))))
        (setf (drover-session-updated (drover-list-test-find id)) (cl-incf drover-list-test-event-time))
        (drover-backend-changed 'fake id)
        (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
        (dolist (list (list sidebar buffer))
          (with-current-buffer list
            (drover-list-test-is-fresh)
            (should (equal (drover-list-test-order) '("c" "b" "a" "i")))))))))

(ert-deftest drover-list-test-in-place-details ()
  "An expanded session changing has its details drawn again; a collapsed one draws them once expanded."
  (drover-list-test-with (list (drover-test-session "a" :status 'working :updated (drover-list-test-ago 10))
                               (drover-test-session "b" :status 'working :updated (drover-list-test-ago 20)))
    (drover-list-test-shown
      (drover-list-test-goto "a")
      (magit-section-show (drover-list-test-section "a"))
      (should (string-match-p "Prompt +Prompt of a" (drover-list-test-visible-text)))
      (setq drover-test-recaps '(("a" :prompt "Second prompt") ("b" :prompt "Prompt b2"))
            drover-list-test-clock (+ drover-list-test-clock drover-list--recap-interval))
      (dolist (id '("a" "b"))
        (cl-incf (drover-session-updated (drover-list-test-find id)) -1)
        (drover-backend-changed 'fake id))
      (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
      (should (string-match-p "Prompt +Second prompt" (drover-list-test-visible-text)))
      (should-not (string-match-p "Prompt b2" (drover-list-test-visible-text)))
      (magit-section-show (drover-list-test-section "b"))
      (should (string-match-p "Prompt +Prompt b2" (drover-list-test-visible-text))))))

(ert-deftest drover-list-test-in-place-follows-the-age-timer ()
  "As time passes, the age timer draws the rows whose ages read otherwise, in place."
  (drover-list-test-with (list (drover-test-session "a" :status 'working :updated (drover-list-test-ago 30))
                               (drover-test-session "b" :status 'working :updated (drover-list-test-ago 7200)))
    (drover-list-test-shown
      (should (string-match-p "30s" (drover-list-test-heading "a")))
      (setq drover-list-test-clock (+ drover-test-now 60))
      (should (zerop (drover-list-test-counting-redraws (drover-list--age-tick))))
      (should (string-match-p "1m" (drover-list-test-heading "a")))
      (should (string-match-p "2h" (drover-list-test-heading "b")))
      (should (equal-including-properties (car (drover-list-test-drawn)) (car (drover-list-test-fresh)))))))

(ert-deftest drover-list-test-collapse-state-preserved ()
  (drover-list-test-with (drover-list-test-sample)
    (should (drover-list-test-group-hidden-p 'idle))
    (should-not (drover-list-test-group-hidden-p 'waiting))
    (drover-list-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (drover-list-test-goto-group 'waiting)
    (magit-section-hide (magit-current-section))
    (drover-list-test-goto "work")
    (magit-section-show (magit-current-section))
    ;; Reorder: done becomes idle, work becomes waiting.
    (setf (drover-session-status (drover-list-test-find "done")) 'idle)
    (setf (drover-session-status (drover-list-test-find "work")) 'waiting)
    (drover-list-test-refresh)
    (should-not (drover-list-test-group-hidden-p 'idle))
    (should (drover-list-test-group-hidden-p 'waiting))
    ;; The session stays expanded in its new group.
    (should (member "work" (alist-get 'waiting (drover-list-test-groups))))
    (should-not (drover-list-test-section-hidden-p "work"))
    (should (drover-list-test-section-hidden-p "idle"))
    ;; And across changes of grouping and back.
    (drover-list-test-shown
      (drover-set-group-by 'project)
      (should-not (drover-list-test-section-hidden-p "work"))
      (drover-set-group-by nil)
      (should-not (drover-list-test-section-hidden-p "work"))
      (should (drover-list-test-section-hidden-p "idle"))
      (drover-set-group-by 'status)
      (should (drover-list-test-group-hidden-p 'waiting))
      (should-not (drover-list-test-group-hidden-p 'idle)))))

(ert-deftest drover-list-test-collapse-state-of-vanished-group ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto-group 'done)
    (magit-section-hide (magit-current-section))
    (let ((sessions drover-test-sessions))
      (setq drover-test-sessions (seq-remove (lambda (s) (eq (drover-session-status s) 'done)) sessions))
      (drover-list-test-refresh)
      (should-not (assq 'done (drover-list-test-groups)))
      (setq drover-test-sessions sessions)
      (drover-list-test-refresh)
      (should (drover-list-test-group-hidden-p 'done)))))

(ert-deftest drover-list-test-collapsed-groups-option ()
  (drover-list-test-with (drover-list-test-sample)
    (setq drover-list-collapsed-groups '((status . working) (project . "p")))
    ;; A new buffer, nothing remembered.
    (kill-buffer buffer)
    (setq buffer (drover-list--buffer))
    (set-buffer buffer)
    (revert-buffer)
    (should (drover-list-test-group-hidden-p 'working))
    (should-not (drover-list-test-group-hidden-p 'idle))
    (drover-list-test-shown
      (drover-set-group-by 'project)
      (should (drover-list-test-group-hidden-p "p"))
      (should-not (drover-list-test-group-hidden-p "q")))))

(ert-deftest drover-list-test-forgets-gone-sessions ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (magit-section-show (magit-current-section))
    (drover-list-test-refresh)
    (should (eq (gethash '(session fake . "work") drover-list--visibility) 'show))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "work"))
                                           drover-test-sessions))
    (drover-list-test-refresh)
    (should-not (gethash '(session fake . "work") drover-list--visibility))
    (should (gethash '(group status . idle) drover-list--visibility))
    ;; Magit's own cache, which never forgets, stays unused.
    (should-not magit-section-visibility-cache)))

(ert-deftest drover-list-test-redraws-when-visible ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
      (drover-backend-changed 'fake "s")
      (drover-changed-now)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(done)))
      (should-not drover-list--stale))))

(ert-deftest drover-list-test-follows-when-shown-again ()
  "A hidden list notes what changed, and draws it in place once shown."
  (drover-list-test-with (list (drover-test-session "s" :status 'working)
                               (drover-test-session "t" :status 'working))
    (should-not (get-buffer-window buffer t))
    (let ((text (buffer-string)))
      (setq drover-test-sessions (list (drover-test-session "s" :status 'done)
                                       (drover-test-session "t" :status 'working)))
      (drover-backend-changed 'fake "s")
      (drover-changed-now)
      ;; Nothing was drawn.
      (should (equal (buffer-string) text))
      (should-not drover-list--stale)
      (should (equal drover-list--pending '(((fake . "s")) nil nil))))
    (should (memq #'drover-list--on-window-change
                  (buffer-local-value 'window-buffer-change-functions buffer)))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (should (zerop (drover-list-test-counting-redraws
                       (drover-list--on-window-change (selected-window)))))
      (should-not drover-list--pending)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(done working)))
      (drover-list-test-is-fresh)
      ;; Up to date: showing it again draws nothing.
      (let ((tick (buffer-modified-tick)))
        (drover-list--on-window-change (selected-window))
        (should (= tick (buffer-modified-tick)))))
    ;; Settings changing while hidden draw it whole once shown.
    (drover-set-group-by 'project)
    (should drover-list--stale)
    (should-not drover-list--pending)
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (should (= (drover-list-test-counting-redraws (drover-list--on-window-change (selected-window))) 1))
      (should-not drover-list--stale))))

(ert-deftest drover-list-test-settings-redraw-views-mode-lines ()
  "A setting changed draws again the mode lines of views alone, which hint at it."
  (drover-list-test-with (drover-list-test-sample)
    (let ((forced nil)
          (other (get-buffer-create "*other code*")))
      (cl-letf (((symbol-function 'force-mode-line-update)
                 (lambda (&optional all) (push (cons (current-buffer) all) forced))))
        (drover-set-group-by 'project))
      (should (member (cons buffer nil) forced))
      (should-not (assq other forced))
      (should-not (rassq t forced)))))

(ert-deftest drover-list-test-mode-line ()
  (drover-list-test-with nil
    (should (equal (drover-list--mode-line-process) " by status"))
    (setq drover-group-by 'project
          drover-show-subagents 'all)
    (should (equal (drover-list--mode-line-process) " by project +subagents"))
    (setq drover-group-by nil
          drover-sort-by 'created
          drover-filter '(:search "x"))
    (should (equal (drover-list--mode-line-process) " ungrouped, creation ↑ +subagents filtered"))
    ;; The default, active subagents, goes without saying.
    (setq drover-show-subagents 'none)
    (should (equal (drover-list--mode-line-process) " ungrouped, creation ↑ -subagents filtered"))
    (setq drover-show-subagents 'active)
    (should (equal (drover-list--mode-line-process) " ungrouped, creation ↑ filtered"))))

;;;; Empty states

(ert-deftest drover-list-test-empty ()
  (drover-list-test-with nil
    (should (equal (buffer-string) "No sessions.\n"))
    (should-not (drover-list--groups))))

(ert-deftest drover-list-test-no-backend ()
  (drover-list-test-with nil
    (drover-unregister-backend 'fake)
    (revert-buffer)
    (should (string-prefix-p "No agent backend is registered." (buffer-string)))))

;;;; Filtering

(ert-deftest drover-list-test-filter-names-nothing ()
  "A filter leaves sessions out; the list names neither it nor how many it hides."
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (drover-list-test-shown
      (drover-set-filter :project "q")
      (should (equal (drover-list-test-groups) '((waiting "wait1") (working "work"))))
      (should (equal (drover-list-test-id-at-point) "work"))
      (should (equal (drover-list--mode-line-process) " by status filtered"))
      ;; The View menu names the filter; the list begins with its groups.
      (goto-char (point-min))
      (should (equal (drover-list-test-line) "Needs you (1)"))
      (should-not (string-search "Filter" (buffer-string)))
      (drover-set-filter :statuses '(working))
      (should (equal (drover-list-test-groups) '((working "work"))))
      (drover-set-filter :statuses nil)
      (drover-set-filter :project nil)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting done working idle)))
      (goto-char (point-min))
      (should (equal (drover-list-test-line) "Needs you (2)")))))

(ert-deftest drover-list-test-filter-shows-matching-subagent ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-filter '(:search "quiet"))
    (revert-buffer)
    ;; Hidden subagents show when they match, under their parent.
    (should (equal (drover-list-test-groups) '((working "other" "quiet"))))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Working (2)"))
    ;; And when no subagent shows.
    (drover-list-test-shown
      (drover-set-show-subagents 'none)
      (should (equal (drover-list-test-groups) '((working "other" "quiet")))))))

(ert-deftest drover-list-test-filter-matches-nothing ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-shown
      (drover-set-filter :search "nowhere")
      (should-not (drover-list--groups))
      ;; The View menu names the filter, and clears it.
      (should (equal (buffer-string) "No session matches the filter.\n"))
      (drover-filter-clear)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting done working idle))))))

(ert-deftest drover-list-test-filter-without-sessions ()
  "Without any session, the filter is not to blame."
  (drover-list-test-with nil
    (setq drover-filter '(:search "nowhere"))
    (revert-buffer)
    (should (equal (buffer-string) "No sessions.\n"))))

;;;; Entry point and life cycle

(ert-deftest drover-list-test-command ()
  (drover-list-test-with (list (drover-test-session "idle" :status 'idle)
                               (drover-test-session "wait" :status 'waiting))
    (kill-buffer buffer)
    (save-window-excursion
      (drover-list)
      (should (equal (buffer-name) "*drover sessions*"))
      (should (eq (window-buffer (selected-window)) (current-buffer)))
      (should (derived-mode-p 'drover-list-mode))
      (should (member '(:start nil) drover-test-calls))
      (should (memq #'drover-list--on-change drover-changed-hook))
      ;; Point starts on the most urgent session.
      (should (equal (drover-list-test-id-at-point) "wait"))
      (goto-char (point-min))
      (drover-list)
      (should (= (point) (point-min))))))

(ert-deftest drover-list-test-command-ungrouped ()
  (drover-list-test-with (list (drover-test-session "idle" :status 'idle)
                               (drover-test-session "wait" :status 'waiting))
    (kill-buffer buffer)
    (setq drover-group-by nil)
    (save-window-excursion
      (drover-list)
      ;; Point starts on the most urgent session, without groups too.
      (should (equal (drover-list-test-id-at-point) "wait")))))

(ert-deftest drover-list-test-kill-stops-watching ()
  (drover-list-test-with nil
    (let ((drover-list-age-interval 1000))
      (drover-list-test-shown
        (revert-buffer)
        (should (memq #'drover-list--on-change drover-changed-hook))
        (should (memq #'drover-list--windows-changed (default-value 'window-buffer-change-functions)))
        (should (timerp drover-list--age-timer))
        ;; Another list buffer keeps the watch.
        (with-temp-buffer
          (drover-list-mode))
        (should (memq #'drover-list--on-change drover-changed-hook))
        (kill-buffer buffer)
        (should-not (memq #'drover-list--on-change drover-changed-hook))
        (should-not (memq #'drover-list--windows-changed (default-value 'window-buffer-change-functions)))
        (should-not (memq #'drover-list--windows-changed
                          (default-value 'window-selection-change-functions)))
        (should-not drover-list--age-timer)))))

(ert-deftest drover-list-test-age-timer ()
  "Ages are redrawn on a timer that runs only while a list is shown."
  (drover-list-test-with (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)))
    (let ((drover-list-age-interval 60))
      (revert-buffer)
      (should-not drover-list--age-timer)
      (drover-list-test-shown
        (revert-buffer)
        (should (timerp drover-list--age-timer))
        (should (string-suffix-p "1m" (drover-list-test-heading "s")))
        (setq drover-list-test-clock (+ drover-test-now 120))
        (drover-list--age-tick)
        (should (string-suffix-p "3m" (drover-list-test-heading "s")))
        (should (timerp drover-list--age-timer)))
      ;; Hidden, a tick notes that time passed, and stops.
      (drover-list--age-tick)
      (should-not drover-list--stale)
      (should (nth 2 drover-list--pending))
      (should-not drover-list--age-timer)
      ;; Shown again, it draws the ages that changed, in place, and runs
      ;; again.
      (setq drover-list-test-clock (+ drover-test-now 300))
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (should (zerop (drover-list-test-counting-redraws
                         (drover-list--on-window-change (selected-window)))))
        (should-not drover-list--pending)
        (should (string-suffix-p "6m" (drover-list-test-heading "s")))
        (should (timerp drover-list--age-timer))))))

;;;; The side window

(defun drover-list-test--session-buffer-name (session)
  "Return the name of the buffer of SESSION."
  (format "*session %s*" (drover-session-id session)))

(defun drover-list-test--register-buffers ()
  "Register the fake backend opening sessions in buffers, as opencode.el does.
Each :open call records the selected window."
  (drover-register-backend
   'fake
   :label "Fake"
   :sessions (lambda () drover-test-sessions)
   :start (lambda () (push '(:start nil) drover-test-calls))
   :open (lambda (session other-window)
           (push (list :open (drover-session-id session) other-window (selected-window))
                 drover-test-calls)
           (let ((buffer (get-buffer-create (drover-list-test--session-buffer-name session))))
             (if other-window (pop-to-buffer buffer) (switch-to-buffer buffer))))
   :buffer (lambda (session)
             (get-buffer (drover-list-test--session-buffer-name session)))
   :respond (drover-test--recorder :respond)))

(defun drover-list-test--main ()
  "Split the selected window and return the two halves, the first selected.
Each shows its own buffer."
  (let* ((first (selected-window))
         (second (split-window first nil 'below)))
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (set-window-buffer second (get-buffer-create "*other 2*"))
    (select-window second)
    (select-window first)
    (list first second)))

(defun drover-list-test-side-sample ()
  "Return sessions of every status, one needing the user with a second line."
  (list (drover-test-session "idle" :status 'idle :updated (drover-list-test-ago 86400))
        (drover-test-session "work" :status 'working :updated (drover-list-test-ago 60)
                             :since (drover-list-test-ago 600))
        (drover-test-session "ask" :status 'waiting :since (drover-list-test-ago 180)
                             :requests '((:kind question :summary "Which branch?")))
        (drover-test-session "done" :status 'done :since (drover-list-test-ago 720))
        (drover-test-session "fail" :status 'failed :since (drover-list-test-ago 30)
                             :detail "Rate limited")))

(ert-deftest drover-list-test-side-toggle ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (kill-buffer buffer)
      (let ((origin (selected-window)))
        (drover-list-side)
        (let ((window (drover-list--side-window)))
          (should (window-live-p window))
          (should (eq (selected-window) origin))
          (should (equal (buffer-name (window-buffer window)) "*drover sidebar*"))
          (should (eq (window-parameter window 'window-side) 'left))
          (should (eql (window-parameter window 'window-slot) 0))
          (should (window-parameter window 'no-delete-other-windows))
          (should (window-parameter window 'no-other-window))
          (should (eq (window-dedicated-p window) t))
          (should (= (window-total-width window) 36))
          (should (window-preserved-size window t))
          (should (member '(:start nil) drover-test-calls))
          (with-current-buffer (window-buffer window)
            (should (eq major-mode 'drover-list-sidebar-mode))
            ;; Too narrow for columns: two lines where a session needs you.
            (should (eq drover-list--layout 'narrow))
            ;; Point starts on the first session.
            (goto-char (window-point window))
            (should (equal (drover-list-test-id-at-point) "ask"))
            (should-not drover-list--stale)))
        (drover-list-side)
        (should-not (drover-list--side-window))
        (should (get-buffer "*drover sidebar*"))
        (should (eq (selected-window) origin))))))

(ert-deftest drover-list-test-side-right ()
  (drover-list-test-with nil
    (let ((drover-list-side-window-side 'right)
          (drover-list-side-window-width 30))
      (drover-list-side)
      (should (eq (window-parameter (drover-list--side-window) 'window-side) 'right))
      (should (= (window-total-width (drover-list--side-window)) 30)))))

(ert-deftest drover-list-test-side-survives-delete-other-windows ()
  (drover-list-test-with nil
    (drover-list-test--main)
    (drover-list-side)
    (delete-other-windows)
    (should (window-live-p (drover-list--side-window)))
    (should (= (length (window-list nil 'nomini)) 2))))

(ert-deftest drover-list-test-side-not-replaced-by-switch-to-buffer ()
  (drover-list-test-with nil
    (let ((side (drover-list-side '(4))))
      (should (eq side (selected-window)))
      (switch-to-buffer (get-buffer-create "*other 1*"))
      (should (equal (buffer-name (window-buffer side)) "*drover sidebar*"))
      (should-not (eq (get-buffer-window "*other 1*") side)))))

(ert-deftest drover-list-test-side-select-back-and-forth ()
  (drover-list-test-with (drover-list-test-side-sample)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (drover-list-side '(4))
      (should (eq (selected-window) (drover-list--side-window)))
      (should (eq (current-buffer) (get-buffer "*drover sidebar*")))
      (drover-list-side '(4))
      (should (eq (selected-window) first))
      (drover-list-side '(4))
      (should (eq (selected-window) (drover-list--side-window)))
      ;; q hides it.
      (call-interactively (key-binding (kbd "q")))
      (should-not (drover-list--side-window))
      (should (eq (selected-window) first)))))

(ert-deftest drover-list-test-side-and-ordinary-window ()
  "The sidebar and the list in an ordinary window show at once, each for its width."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (pcase-let ((`(,first ,second) (drover-list-test--main)))
        (drover-list)
        (should (eq (window-buffer first) buffer))
        (should (eq drover-list--layout 'columns))
        ;; The sidebar is a list of its own: the other one stays.
        (let ((side (drover-list-side)))
          (should (eq side (drover-list--side-window)))
          (should (eq (window-buffer side) (drover-list--buffer t)))
          (should-not (eq (drover-list--buffer t) buffer))
          (should (eq (window-buffer first) buffer))
          (should (equal (buffer-name (window-buffer second)) "*other 2*"))
          (should (eq (buffer-local-value 'drover-list--layout (window-buffer side)) 'narrow))
          ;; Listing again in an ordinary window leaves the sidebar.
          (drover-list)
          (should (eq (drover-list--side-window) side))
          (should (eq (window-buffer (selected-window)) buffer))
          (should-not (window-parameter (selected-window) 'window-side))
          ;; q quits the ordinary window as usual; the sidebar stays.
          (call-interactively (key-binding (kbd "q")))
          (should-not (get-buffer-window buffer))
          (should (window-live-p side)))))))

(ert-deftest drover-list-test-side-narrow-lines ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (with-current-buffer (window-buffer (drover-list--show-side))
        ;; 36 columns: 34 of text, from the left edge, then the column of
        ;; the continuation glyph, as the window has no fringe; the
        ;; divider takes the last.  The sidebar leaves no room for
        ;; Magit's ellipsis, as it collapses nothing.  The harness's
        ;; icon ends the title's line, where the icons line up.
        (cl-flet ((flush (left right) (concat left (make-string (- 34 (length left) (length right)) ?\s)
                                              right)))
          (should (equal (drover-list-test-entry "ask")
                         (concat (flush "! Session ask" "3m ▣") "\n  asks: Which branch?")))
          (should (equal (drover-list-test-entry "fail")
                         (concat (flush "✗ Session fail" "30s ▣") "\n  Rate limited")))
          (should (equal (drover-list-test-entry "work") (flush "◐ Session work" "1m ▣"))))))))

;; Opening a session from the side window, with RET, o or C-o, must not
;; replace the list in its dedicated window: `drover-open' opens
;; sessions in the window `drover-main-window-function' returns, the
;; main window.

(ert-deftest drover-list-test-main-window ()
  (drover-list-test-with nil
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list-side '(4))
      (should (eq (drover-list--main-window) first))
      (select-window second)
      (drover-list-side '(4))
      (should (eq (drover-list--main-window) second))
      ;; Preferring a window not dedicated to its buffer.
      (set-window-dedicated-p second t)
      (should (eq (drover-list--main-window) first))
      (set-window-dedicated-p first t)
      (should (eq (drover-list--main-window) second))
      (set-window-dedicated-p first nil)
      (set-window-dedicated-p second nil))))

(ert-deftest drover-list-test-side-ret-opens-in-main-window ()
  "RET opens the session at point in the main window, and selects it."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (let ((side (drover-list-side '(4))))
        (drover-list-test-goto "done")
        (execute-kbd-macro (kbd "RET"))
        (should (equal (car drover-test-calls) (list :open "done" nil first)))
        (should (eq (selected-window) first))
        (should (equal (buffer-name (window-buffer first)) "*session done*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        (should (eq (window-buffer side) (drover-list--buffer t)))))))

(defun drover-list-test-posn (window position &optional x)
  "Return a mouse position at POSITION of WINDOW's text, X pixels from its left."
  (list window position (cons (or x 0) 0) 0 nil position '(0 . 0) nil '(0 . 0) '(1 . 1)))

(defun drover-list-test-mouse (window position &rest kinds)
  "Run the mouse events KINDS at POSITION of WINDOW through the command loop.
Each kind is a symbol such as `down-mouse-1' or `double-mouse-1'; a drag
is (drag-mouse-1 END), from POSITION to END.  The current buffer is the
one current before."
  (let ((buffer (current-buffer))
        (posn (drover-list-test-posn window position)))
    (execute-kbd-macro
     (vconcat (mapcar (lambda (kind)
                        (pcase kind
                          (`(drag-mouse-1 ,end) (list 'drag-mouse-1 posn (drover-list-test-posn window end 40)))
                          ((or 'double-down-mouse-1 'double-mouse-1) (list kind posn 2))
                          ((or 'triple-down-mouse-1 'triple-mouse-1) (list kind posn 3))
                          (_ (list kind posn))))
                      kinds)))
    (set-buffer buffer)))

(ert-deftest drover-list-test-side-click-opens-as-ret ()
  "A click on a session's row of the sidebar opens it as RET does, once.
From the window where you work, the sidebar not selected, the session
opens there, as RET in the sidebar opens it; a click on its second line
too.  A click on a group's heading, or below the rows, does nothing,
a double or triple click opens once, and the mouse drags no region.
The list of an ordinary window keeps the global click."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (drover-list-test-showing
      (pcase-let ((`(,first ,second) (drover-list-test--main)))
        (let* ((side (drover-list-side '(4)))
               (sidebar (window-buffer side))
               (calls (lambda () (seq-filter (lambda (call) (eq (car call) :open)) drover-test-calls))))
          (cl-flet ((row (id &optional line)
                      (with-current-buffer sidebar
                        (save-excursion
                          (drover-list-test-goto id)
                          (forward-line (or line 0))
                          (+ (point) 3)))))
            ;; RET, the reference.
            (with-current-buffer sidebar
              (drover-list-test-goto "done")
              (execute-kbd-macro (kbd "RET")))
            (let ((by-ret (car drover-test-calls)))
              (should (equal by-ret (list :open "done" nil first)))
              (setq drover-test-calls nil)
              ;; The same from a click, the sidebar unselected.
              (select-window first)
              (switch-to-buffer "*other 1*")
              (with-current-buffer sidebar
                (drover-list-test-goto "work"))
              (drover-list-test-mouse side (row "done") 'down-mouse-1 'mouse-1)
              (should (equal (funcall calls) (list by-ret)))
              (should (eq (selected-window) first))
              (should (equal (buffer-name (window-buffer first)) "*session done*"))
              (should (equal (buffer-name (window-buffer second)) "*other 2*"))
              (should (eq (window-buffer side) sidebar))
              (with-current-buffer sidebar
                (should (= (window-point side) (oref (drover-list-test-section "done") start)))))
            ;; Its second line.
            (setq drover-test-calls nil)
            (drover-list-test-mouse side (row "ask" 1) 'down-mouse-1 'mouse-1)
            (should (equal (funcall calls) (list (list :open "ask" nil first))))
            ;; A group's heading, or below the rows: nothing.
            (setq drover-test-calls nil)
            (select-window second)
            (let ((heading (with-current-buffer sidebar
                             (save-excursion (drover-list-test-goto-group 'idle) (+ (point) 2))))
                  (point (window-point side)))
              (drover-list-test-mouse side heading 'down-mouse-1 'mouse-1)
              (drover-list-test-mouse side heading 'down-mouse-1 'mouse-1 'double-down-mouse-1 'double-mouse-1)
              (drover-list-test-mouse side (with-current-buffer sidebar (point-max)) 'down-mouse-1 'mouse-1)
              (should-not (funcall calls))
              (should (eq (selected-window) second))
              (should (= (window-point side) point))
              (with-current-buffer sidebar
                (should-not (drover-list-test-group-hidden-p 'idle))))
            ;; A double click, a triple click: one session opened each.
            (drover-list-test-mouse side (row "fail") 'down-mouse-1 'mouse-1
                                    'double-down-mouse-1 'double-mouse-1)
            (should (equal (funcall calls) (list (list :open "fail" nil second))))
            (setq drover-test-calls nil)
            (drover-list-test-mouse side (row "work") 'down-mouse-1 'mouse-1
                                    'double-down-mouse-1 'double-mouse-1
                                    'triple-down-mouse-1 'triple-mouse-1)
            (should (equal (funcall calls) (list (list :open "work" nil second))))
            ;; A drag selects nothing and opens nothing.
            (setq drover-test-calls nil)
            (let ((transient-mark-mode t))
              (drover-list-test-mouse side (row "ask") 'down-mouse-1 `(drag-mouse-1 ,(row "done")))
              (should-not (funcall calls))
              (with-current-buffer sidebar
                (should-not (region-active-p))))
            (with-current-buffer sidebar
              (dolist (key '("<down-mouse-1>" "<drag-mouse-1>" "<double-mouse-1>" "<triple-mouse-1>"))
                (should (eq (keymap-lookup drover-list-sidebar-mode-map key) #'ignore))))
            ;; The list of an ordinary window clicks as ever: a double
            ;; click on a group's heading folds it.
            (should-not (keymap-lookup drover-list-mode-map "<mouse-1>"))
            (should-not (keymap-lookup drover-list-mode-map "<down-mouse-1>"))
            (set-window-buffer second buffer)
            (select-window second)
            (with-current-buffer buffer
              (revert-buffer)
              (should (drover-list-test-group-hidden-p 'idle))
              (let ((heading (save-excursion (drover-list-test-goto-group 'idle) (+ (point) 2))))
                (drover-list-test-mouse second heading 'down-mouse-1 'mouse-1
                                        'double-down-mouse-1 'double-mouse-1))
              (should-not (drover-list-test-group-hidden-p 'idle)))
            (should-not (funcall calls))))))))

(ert-deftest drover-list-test-side-o-opens-beside-main-window ()
  "From the side window, o opens a session in another window than the main window."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (let ((side (drover-list-side '(4))))
        (drover-list-test-goto "done")
        (execute-kbd-macro (kbd "o"))
        (should (equal (car drover-test-calls) (list :open "done" nil second)))
        (should (eq (selected-window) second))
        (should (equal (buffer-name (window-buffer second)) "*session done*"))
        (should (equal (buffer-name (window-buffer first)) "*other 1*"))
        (should (eq (window-buffer side) (drover-list--buffer t)))
        (should (= (length (window-list nil 'nomini)) 3))))))

(ert-deftest drover-list-test-side-o-splits-main-window ()
  "With the main window alone beside the list, o splits it, and RET keeps to it."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (let ((main (selected-window))
          (side nil))
      (switch-to-buffer (get-buffer-create "*other 1*"))
      (setq side (drover-list-side '(4)))
      (drover-list-test-goto "done")
      (execute-kbd-macro (kbd "o"))
      (let ((other (selected-window)))
        (should-not (memq other (list main side)))
        (should (equal (car drover-test-calls) (list :open "done" nil other)))
        (should (equal (buffer-name (window-buffer main)) "*other 1*"))
        (should (eq (window-buffer side) (drover-list--buffer t)))
        (should (= (length (window-list nil 'nomini)) 3))
        ;; RET opens in the main window, the one used last beside the list.
        (select-window main)
        (select-window side)
        (drover-list-test-goto "work")
        (execute-kbd-macro (kbd "RET"))
        (should (equal (car drover-test-calls) (list :open "work" nil main)))
        (should (equal (buffer-name (window-buffer other)) "*session done*"))))))

(ert-deftest drover-list-test-ordinary-window-opens-as-usual ()
  "From an ordinary window, RET opens in the same window and o in another."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list)
      (should-not (drover-list--target-window))
      (drover-list-test-goto "done")
      (execute-kbd-macro (kbd "o"))
      (should (equal (car drover-test-calls) (list :open "done" nil second)))
      (should (eq (window-buffer first) buffer))
      (should (equal (buffer-name (window-buffer second)) "*session done*"))
      (select-window first)
      (execute-kbd-macro (kbd "RET"))
      (should (equal (car drover-test-calls) (list :open "done" nil first)))
      (should (equal (buffer-name (window-buffer first)) "*session done*")))))

(ert-deftest drover-list-test-ordinary-window-alone-o-splits ()
  "A list alone in its frame splits its window for o, and stays."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (drover-list)
    (let ((window (selected-window)))
      (drover-list-test-goto "done")
      (execute-kbd-macro (kbd "o"))
      (should-not (eq (selected-window) window))
      (should (eq (window-buffer window) buffer))
      (should (equal (buffer-name (window-buffer (selected-window))) "*session done*")))))

(ert-deftest drover-list-test-display-keeps-list-selected ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (let ((side (drover-list-side '(4))))
        (drover-list-test-goto "work")
        (execute-kbd-macro (kbd "C-o"))
        (should (equal (car drover-test-calls) (list :open "work" nil first)))
        (should (eq (selected-window) side))
        (should (eq (current-buffer) (window-buffer side)))
        (should (equal (buffer-name (window-buffer first)) "*session work*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        ;; The title of the session it shows tells so at once.
        (should (eq (drover-list-test-title-face "work") 'opencode-session-active))))))

(ert-deftest drover-list-test-open-unsupported ()
  (drover-list-test-with nil
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "x"
                                                                               :status 'working))))
    (drover-list-test--main)
    (let ((side (drover-list-side '(4))))
      (goto-char (oref (drover-list-test-section "x" 'bare) start))
      (should-error (execute-kbd-macro (kbd "RET")) :type 'user-error)
      (should-error (execute-kbd-macro (kbd "C-o")) :type 'user-error)
      (should (eq (selected-window) side)))))

(ert-deftest drover-list-test-side-follow-mode ()
  "Follow mode shows the session at point in the main window as point moves."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (drover-list-test--register-buffers)
      (pcase-let ((`(,first ,second) (drover-list-test--main)))
        (let ((side (drover-list-side '(4)))
              (shown (lambda () (buffer-name (window-buffer first))))
              (opened (lambda () (length (seq-filter (lambda (call) (eq (car call) :open))
                                                     drover-test-calls)))))
          (drover-list-test-goto "ask")
          (execute-kbd-macro (kbd "n n"))
          (should (equal (funcall shown) "*other 1*"))
          (execute-kbd-macro (kbd "p p"))
          (should (equal (drover-list-test-id-at-point) "ask"))
          ;; Turning it on shows the session at point at once.
          (execute-kbd-macro (kbd "V - f C-g"))
          (should drover-follow-mode)
          (should (equal (car drover-test-calls) (list :open "ask" nil first)))
          (should (equal (funcall shown) "*session ask*"))
          ;; A group heading shows nothing.
          (execute-kbd-macro (kbd "n"))
          (should-not (drover-list-test-id-at-point))
          (should (= (funcall opened) 1))
          (execute-kbd-macro (kbd "n"))
          (should (equal (funcall shown) "*session fail*"))
          ;; The sidebar moves by rows: past the session's second line,
          ;; to the heading of the next group, which shows nothing.
          (execute-kbd-macro (kbd "C-n"))
          (should-not (drover-list-test-id-at-point))
          (should (= (funcall opened) 2))
          (execute-kbd-macro (kbd "C-n"))
          (should (equal (funcall shown) "*session done*"))
          (execute-kbd-macro (kbd "p p"))
          (should (equal (funcall shown) "*session fail*"))
          (should (eq (selected-window) side))
          (should (equal (buffer-name (window-buffer second)) "*other 2*"))
          ;; The main window stays the same, and its session is the active one.
          (should (eq (drover-list--main-window) first))
          (drover-list--windows-changed)
          (should (eq (drover-list-test-title-face "fail") 'opencode-session-active))
          (execute-kbd-macro (kbd "V - f C-g"))
          (should-not drover-follow-mode)
          (execute-kbd-macro (kbd "n"))
          (should (equal (funcall shown) "*session fail*")))))))

(defun drover-list-test-row-starts ()
  "Return the starts of the rows of the current list, sessions and headings, in order."
  (let ((starts nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((section (get-text-property (point) 'magit-section)))
          (when (and (not (eq section magit-root-section)) (= (point) (oref section start)))
            (push (point) starts)))
        (forward-line 1)))
    (nreverse starts)))

(ert-deftest drover-list-test-sidebar-moves-by-rows ()
  "The sidebar shows no cursor, and keeps point at the start of a row.
Lines move by rows, headings and sessions, over the second line of a
session needing the user, the mark too; any other command leaves point
at the start of its row, but while searching.  The list of an ordinary
window keeps its cursor and moves by lines."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (drover-list-test--main)
      (should (eq (key-binding (kbd "C-n")) #'next-line))
      (should cursor-type)
      (let ((side (drover-list-side '(4))))
        (should (eq (selected-window) side))
        (should-not cursor-type)
        (should-not cursor-in-non-selected-windows)
        (let ((starts (drover-list-test-row-starts)))
          ;; The heading of each of five groups and its session.
          (should (= (length starts) 10))
          (goto-char (car starts))
          (dolist (start (cdr starts))
            (execute-kbd-macro (kbd "C-n"))
            (should (= (point) start)))
          (should-error (drover-list-next-row) :type 'end-of-buffer)
          (should (= (point) (car (last starts))))
          (dolist (start (cdr (reverse starts)))
            (execute-kbd-macro (kbd "<up>"))
            (should (= (point) start)))
          (should-error (drover-list-previous-row) :type 'beginning-of-buffer)
          (execute-kbd-macro (kbd "C-u 3 C-n"))
          (should (= (point) (nth 3 starts)))
          ;; Anywhere in a row, any command takes point to its start:
          ;; within its first line, or on its second.
          (drover-list-test-goto "ask")
          (forward-line 1)
          (execute-kbd-macro (kbd "C-f"))
          (should (equal (drover-list-test-id-at-point) "ask"))
          (should (= (point) (oref (drover-list-test-section "ask") start)))
          (goto-char (+ (point) 4))
          (execute-kbd-macro (kbd "C-e"))
          (should (= (point) (oref (drover-list-test-section "ask") start)))
          ;; But not in the middle of a search.
          (goto-char (+ (point) 4))
          (let ((isearch-mode t))
            (drover-list--sidebar-settle))
          (should (= (point) (+ (oref (drover-list-test-section "ask") start) 4)))
          (drover-list--sidebar-settle)
          ;; A region goes from row to row, selecting the sessions of
          ;; both and those between, headings among them.
          (let ((transient-mark-mode t))
            (drover-list-test-goto "ask")
            (goto-char (+ (point) 3))
            (execute-kbd-macro (kbd "C-SPC C-n C-n C-n C-n"))
            (should (region-active-p))
            (should (= (mark) (oref (drover-list-test-section "ask") start)))
            (should (= (point) (oref (drover-list-test-section "done") start)))
            (should (equal (mapcar (lambda (section) (cdr (oref section value)))
                                   (drover-list--selected-sections))
                           '("ask" "fail" "done")))
            (deactivate-mark)))))))

(ert-deftest drover-list-test-sidebar-highlight ()
  "The row at point stands out more in the sidebar, as the theme says, and follows it.
Its background is that of `magit-section-highlight' taken further from
the default one, the face `drover-list-sidebar-highlight' over it; the
list in an ordinary window keeps Magit's, and both measure alike."
  (drover-list-test-with (drover-list-test-side-sample)
    (let ((default (face-background 'default nil t))
          (highlight (face-background 'magit-section-highlight nil t))
          (specs (lambda () (flatten-tree (cdr (assq 'magit-section-highlight face-remapping-alist)))))
          (background (lambda ()
                        (plist-get (cdr (memq 'drover-list-sidebar-highlight
                                              (flatten-tree (cdr (assq 'magit-section-highlight
                                                                       face-remapping-alist)))))
                                   :background))))
      (unwind-protect
          ;; The colours as given, not the nearest a terminal shows.
          (cl-letf (((symbol-function 'color-values) (lambda (color &optional _) (color-values-from-color-spec color))))
            (set-face-background 'default "#101010")
            (set-face-background 'magit-section-highlight "#203040")
            (with-current-buffer (window-buffer (drover-list--show-side))
              (should (memq 'drover-list-sidebar-highlight (funcall specs)))
              ;; Each colour 1.4 times as far from the default's.
              (should (equal (funcall background) "#263c53"))
              (let ((drover-list-sidebar-highlight-contrast 1))
                (drover-list--rehighlight)
                (should (equal (funcall background) "#203040")))
              (drover-list--rehighlight)
              ;; A theme enabled draws it anew.
              (set-face-background 'magit-section-highlight "#302010")
              (run-hook-with-args 'enable-theme-functions 'user)
              (should (equal (funcall background) "#3c2610"))
              (should (= 1 (seq-count (lambda (entry) (eq (car entry) 'magit-section-highlight))
                                      face-remapping-alist)))
              (should (equal (car (drover-list--measure-context))
                             (with-current-buffer buffer (car (drover-list--measure-context))))))
            (should-not (assq 'magit-section-highlight face-remapping-alist)))
        (set-face-background 'default default)
        (set-face-background 'magit-section-highlight highlight)))))

(ert-deftest drover-list-test-line-width-leaves-the-truncation-glyph ()
  "Lines leave the room of the glyph telling a line is cut, as Emacs keeps it.
Without fringes, Emacs keeps at the right edge the width of $, or of
the display table's glyph, in the default face as the buffer remaps it,
which may be wider than its average character; on a frame showing none,
an average character."
  (drover-list-test-with (drover-list-test-side-sample)
    (let* ((window (drover-list--show-side))
           (body (window-body-width window t)))
      (with-current-buffer (window-buffer window)
        (cl-letf (((symbol-function 'window-font-width) (lambda (&rest _) 7))
                  ((symbol-function 'drover-list--pixel-width)
                   (lambda (string)
                     (pcase (substring-no-properties string)
                       ("$" 9)
                       (">" (if (eq (get-text-property 0 'face string) 'bold) 11 5))
                       (_ 7)))))
          (should (= (drover-list--line-width) (- body 9)))
          (let ((buffer-display-table (make-display-table)))
            (set-display-table-slot buffer-display-table 'truncation (make-glyph-code ?> 'bold))
            (should (= (drover-list--line-width) (- body 11)))
            (set-display-table-slot buffer-display-table 'truncation ?>)
            (should (= (drover-list--line-width) (- body 7))))
          (let ((frame (window-frame window)))
            (unwind-protect
                (progn
                  (set-frame-parameter frame 'no-special-glyphs t)
                  (should (= (drover-list--line-width) (- body 7))))
              (set-frame-parameter frame 'no-special-glyphs nil))))))))

;;;; Where sessions are

(defun drover-list-test-title-face (id)
  "Return the face of the title of session ID, the same over the whole title."
  (save-excursion
    (drover-list-test-goto id)
    (let* ((start (text-property-not-all (point) (line-end-position) 'drover-list-title nil))
           (end (next-single-property-change start 'drover-list-title nil (line-end-position))))
      (should (equal (get-text-property start 'face) (get-text-property start 'font-lock-face)))
      (should (= (next-single-property-change start 'face nil end) end))
      (get-text-property start 'face))))

(defun drover-list-test-places (&rest ids)
  "Return the faces of the titles of the sessions IDS, as a list."
  (mapcar #'drover-list-test-title-face ids))

(defmacro drover-list-test-counting-refaces (&rest body)
  "Run BODY counting the titles given another face in place, and return the count."
  (declare (indent 0) (debug t))
  `(let ((count 0)
         (reface (symbol-function 'drover-list--reface)))
     (cl-letf (((symbol-function 'drover-list--reface)
                (lambda (&rest arguments)
                  (cl-incf count)
                  (apply reface arguments))))
       ,@body)
     count))

(ert-deftest drover-list-test-title-places ()
  "A title's colour tells where its session is, changing in place with windows."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (setq drover-list-collapsed-groups nil)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (set-window-buffer first (get-buffer-create "*session work*"))
      (set-window-buffer second (get-buffer-create "*session ask*"))
      (get-buffer-create "*session done*")
      (let ((side (drover-list--show-side)))
        (with-current-buffer (window-buffer side)
          ;; The selected window's, another window's, a buffer's no
          ;; window shows, and no buffer.
          (should (equal (drover-list-test-places "work" "ask" "done" "fail" "idle")
                         '(opencode-session-active opencode-session-shown opencode-session-open nil
                                                 drover-dim)))
          (let ((root magit-root-section))
            ;; Selecting another window changes two titles, in place.
            (select-window second)
            (set-buffer (window-buffer side))
            (should (= (drover-list-test-counting-refaces (drover-list--windows-changed)) 2))
            (should (eq magit-root-section root))
            (should (equal (drover-list-test-places "work" "ask")
                           '(opencode-session-shown opencode-session-active)))
            ;; Selecting the list keeps the window selected before.
            (select-window side)
            (should (= (drover-list-test-counting-refaces (drover-list--windows-changed)) 0))
            (should (equal (drover-list-test-places "work" "ask")
                           '(opencode-session-shown opencode-session-active)))
            ;; A buffer leaving its window stays open; one coming shows.
            (set-window-buffer first (get-buffer "*session done*"))
            (should (= (drover-list-test-counting-refaces (drover-list--windows-changed)) 2))
            (should (equal (drover-list-test-places "work" "done")
                           '(opencode-session-open opencode-session-shown)))
            ;; Killing a buffer no window shows, after the kill.
            (kill-buffer "*session work*")
            (should (timerp drover-list--restyle-timer))
            (funcall (timer--function drover-list--restyle-timer))
            (should-not (drover-list-test-title-face "work"))
            ;; Over the dimming of an idle session, which lends what it
            ;; lacks: the window used last beside the list shows it.
            (set-window-buffer second (get-buffer-create "*session idle*"))
            (drover-list--windows-changed)
            (should (equal (drover-list-test-places "ask" "idle")
                           '(opencode-session-open (opencode-session-active drover-dim))))
            (should (eq magit-root-section root))
            ;; A redraw finds the same places.
            (drover-list--render)
            (should (equal (drover-list-test-places "work" "ask" "done" "idle")
                           '(nil opencode-session-open opencode-session-shown
                                 (opencode-session-active drover-dim))))))))))

(ert-deftest drover-list-test-title-places-in-ordinary-window ()
  "In an ordinary window, the session of the window beside it is the active one."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list)
      (should (eq (window-buffer first) buffer))
      (set-window-buffer second (get-buffer-create "*session done*"))
      (drover-list--windows-changed)
      (should (eq (drover-list-test-title-face "done") 'opencode-session-active))
      ;; Selected, the session's window is.
      (select-window second)
      (set-buffer buffer)
      (drover-list--windows-changed)
      (should (eq (drover-list-test-title-face "done") 'opencode-session-active)))))

(ert-deftest drover-list-test-title-places-nested-and-cut ()
  "A subagent's title, and a title cut to fit, change face whole, ellipsis included."
  (drover-list-test-with
      (list (drover-test-session "parent" :status 'done :since (drover-list-test-ago 50))
            (drover-list-test-titled "asking" "A subagent asking with a long, long title"
                                     :parent "parent" :status 'waiting :since (drover-list-test-ago 30)
                                     :requests '((:kind question :summary "Which file?"))))
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (with-current-buffer (window-buffer (drover-list--show-side))
        (set-window-buffer first (get-buffer-create "*session asking*"))
        (drover-list--windows-changed)
        (should (equal (drover-list-test-places "parent" "asking") '(nil opencode-session-active)))
        (save-excursion
          (drover-list-test-goto "asking")
          (search-forward (truncate-string-ellipsis) (line-end-position))
          (should (eq (get-text-property (match-beginning 0) 'face) 'opencode-session-active)))
        (set-window-buffer first (get-buffer-create "*session parent*"))
        (drover-list--windows-changed)
        (should (equal (drover-list-test-places "parent" "asking")
                       '(opencode-session-active opencode-session-open)))))))

(ert-deftest drover-list-test-title-place-changing-width-redraws ()
  "A face changing the width of a title redraws the list, so that lines fit."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (with-current-buffer (window-buffer (drover-list--show-side))
        (let ((root magit-root-section)
              (width (symbol-function 'drover-list--pixel-width)))
          ;; A bold face for the active session, say: wider.
          (cl-letf (((symbol-function 'drover-list--pixel-width)
                     (lambda (string)
                       (+ (funcall width string)
                          (if (seq-some (lambda (index)
                                          (memq 'opencode-session-active
                                                (ensure-list (get-text-property index 'face string))))
                                        (number-sequence 0 (1- (length string))))
                              5 0)))))
            (set-window-buffer first (get-buffer-create "*session work*"))
            (drover-list--windows-changed)
            (should-not (eq magit-root-section root))
            (should (eq (drover-list-test-title-face "work") 'opencode-session-active))))))))

(ert-deftest drover-list-test-active-window ()
  "The window worked in is the selected one, else the one used last before."
  (drover-list-test-with (drover-list-test-side-sample)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (let ((side (drover-list--show-side)))
        (should (eq (drover-list--active-window) first))
        (select-window second)
        (should (eq (drover-list--active-window) second))
        (select-window side)
        (should (eq (drover-list--active-window) second))
        ;; From the minibuffer, the window selected before it.
        (cl-letf (((symbol-function 'selected-window) (lambda (&rest _) (minibuffer-window)))
                  ((symbol-function 'minibuffer-selected-window) (lambda () first)))
          (should (eq (drover-list--active-window) first)))
        (cl-letf (((symbol-function 'selected-window) (lambda (&rest _) (minibuffer-window)))
                  ((symbol-function 'minibuffer-selected-window) (lambda () side)))
          (should (eq (drover-list--active-window) second)))))))

(ert-deftest drover-list-test-no-place-without-buffers ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--main)
    (with-current-buffer (window-buffer (drover-list--show-side))
      (drover-list--windows-changed)
      (should-not (seq-some #'drover-list-test-title-face '("work" "ask" "done" "fail"))))))

(ert-deftest drover-list-test-side-refresh-keeps-point-on-session ()
  (drover-list-test-with (drover-list-test-side-sample)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (with-current-buffer (window-buffer first)
        (insert "some text\nmore text\n")
        (set-window-point first 5))
      (let ((side (drover-list--show-side)))
        (with-selected-window side
          (drover-list-test-goto "work"))
        ;; "work" changes group and a session joins above it.
        (setf (drover-session-status (drover-find 'fake "work")) 'done
              (drover-session-since (drover-find 'fake "work")) (drover-list-test-ago 10000))
        (push (drover-test-session "new" :status 'waiting :since (drover-list-test-ago 5)) drover-test-sessions)
        (drover-backend-changed 'fake)
        (drover-changed-now)
        (with-current-buffer (window-buffer side)
          (should (drover-list-test-section "new"))
          (goto-char (window-point side))
          (should (equal (drover-list-test-id-at-point) "work"))
          (should (member "work" (alist-get 'done (drover-list-test-groups)))))
        (should (= (window-point first) 5))
        (should (eq (selected-window) first))))))

(ert-deftest drover-list-test-side-follows-while-hidden ()
  "A hidden sidebar draws nothing as sessions change, and catches up in place once shown."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-side)
    (drover-list-side)
    (should-not (drover-list--side-window))
    (with-current-buffer (drover-list--buffer t)
      (let ((text (buffer-string)))
        (push (drover-test-session "new" :status 'working) drover-test-sessions)
        (drover-backend-changed 'fake)
        (drover-changed-now)
        ;; Nothing was drawn: the change is noted, to draw once shown.
        (should (equal (buffer-string) text))
        (should-not drover-list--stale)
        (should (equal (car drover-list--pending) '((fake . "new"))))
        (should-not (drover-list-test-section "new"))))
    (should (zerop (drover-list-test-counting-redraws (drover-list-side))))
    (with-current-buffer (drover-list--buffer t)
      (should-not drover-list--pending)
      (should (drover-list-test-section "new"))
      (drover-list-test-is-fresh))))

(ert-deftest drover-list-test-hidden-list-draws-whole-past-a-limit ()
  "Past `drover-list--pending-limit' sessions changed while hidden, a list is drawn whole once shown."
  (drover-list-test-with (drover-list-test-side-sample)
    (should-not (get-buffer-window buffer))
    (let ((drover-list--pending-limit 2))
      (dolist (id '("work" "done"))
        (setf (drover-session-title (drover-list-test-find id)) (concat "Renamed " id))
        (drover-backend-changed 'fake id)
        (drover-changed-now))
      (should-not drover-list--stale)
      (should (equal (car drover-list--pending) '((fake . "work") (fake . "done"))))
      (setf (drover-session-title (drover-list-test-find "fail")) "Renamed fail")
      (drover-backend-changed 'fake "fail")
      (drover-changed-now)
      (should drover-list--stale)
      (should-not drover-list--pending)
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (should (= (drover-list-test-counting-redraws (drover-list--on-window-change (selected-window))) 1))
        (should-not drover-list--stale)))))

(ert-deftest drover-list-test-shown-list-draws-whole-past-a-limit ()
  "A change of more than `drover-list--pending-limit' sessions draws a list shown whole."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-shown
      (let ((drover-list--pending-limit 2))
        (dolist (id '("work" "done" "fail"))
          (setf (drover-session-title (drover-list-test-find id)) (concat "Renamed " id))
          (drover-backend-changed 'fake id))
        (should (= (drover-list-test-counting-redraws (drover-changed-now)) 1))
        (setf (drover-session-title (drover-list-test-find "work")) "Renamed wark")
        (drover-backend-changed 'fake "work")
        (should (= (drover-list-test-counting-redraws (drover-changed-now)) 0))
        (drover-list-test-is-fresh)))))

(ert-deftest drover-list-test-side-shown-again-keeps-point ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-side)
    (with-current-buffer (drover-list--buffer t)
      (set-window-point (drover-list--side-window)
                        (save-excursion (drover-list-test-goto "work") (point)))
      (drover-list-side)
      (drover-list-side)
      (goto-char (window-point (drover-list--side-window)))
      (should (equal (drover-list-test-id-at-point) "work")))))

(ert-deftest drover-list-test-side-width-change ()
  (drover-list-test-with (list (drover-test-session "a" :status 'waiting :since (drover-list-test-ago 60)
                                                    :requests '((:kind question :summary "Go?"))))
    (drover-list-test-showing
      (let* ((side (drover-list--show-side))
             (before (window-max-chars-per-line side)))
        (with-current-buffer (window-buffer side)
          (should (= drover-list--width before))
          ;; Two lines: the summary does not fit in a column.
          (should (eq drover-list--layout 'narrow))
          (should-not header-line-format)
          (should (string-suffix-p "1m ▣" (drover-list-test-heading "a")))
          ;; Every column of the line: the sidebar leaves no room for
          ;; Magit's ellipsis, as it collapses nothing.
          (should (= (string-width (drover-list-test-heading "a")) before))
          (window-resize side 6 t nil t)
          (should (= (window-max-chars-per-line side) (+ before 6)))
          (drover-list--on-window-change side)
          (should (= drover-list--width (+ before 6)))
          ;; Now it does: one line, in columns, from the left edge; the
          ;; harness shows its name, room allowing, after the age, as in
          ;; two lines.
          (should (eq drover-list--layout 'columns))
          (should (equal (drover-list-test-entry "a") "! Session a   1m  ▣ Fake   asks: Go?"))
          ;; Named in the header line, which two lines lack.
          (should (equal (drover-list-test-displayed header-line-format) "Session      Age  Harness  Summary")))))))

;;;; The sidebar draws its first screens

(defun drover-list-test-drawn-ids ()
  "Return the identifiers of the sessions drawn, in order, without drawing others."
  (mapcan (lambda (container)
            (mapcar (lambda (section) (cdr (oref section value)))
                    (drover-list--session-sections container)))
          (drover-list--containers)))

(defun drover-list-test-idle-draw ()
  "Draw what the current list left for later, as idle timers do, slice by slice."
  (cl-letf (((symbol-function 'input-pending-p) #'ignore))
    (while (and drover-list--model
                (seq-some #'drover-list--deferred-p (drover-list--model-groups drover-list--model)))
      (drover-list--draw-idle (current-buffer)))))

(ert-deftest drover-list-test-sidebar-draws-first-screens ()
  "The sidebar draws the rows of its first screens at once, the others later, unfolded.
Grouped or not: each group's heading shows, its first trees drawn, the
others drawn while Emacs is idle, a few at a time, or as searching needs
them; no line is hidden, and none begins with an invisible character."
  (dolist (grouping '(status nil))
    (drover-list-test-with (drover-list-test-random-sessions 60)
      (drover-list-test-showing
        (let ((drover-group-by grouping)
              (drover-show-subagents 'all)
              (drover-list--deferred-chunk 7))
          (cl-letf (((symbol-function 'drover-list--first-rows) (lambda () 10)))
            (let ((side (drover-list--show-side)))
              (with-current-buffer (window-buffer side)
                (revert-buffer)
                (let ((drawn (drover-list-test-drawn-ids)))
                  ;; Trees whole: ten rows, a few more for a tree begun.
                  (should (<= 10 (length drawn) 20))
                  (should (seq-some #'drover-list--deferred-p (drover-list--model-groups drover-list--model)))
                  (should-not (text-property-any (point-min) (point-max) 'invisible t))
                  (when grouping
                    (should (= (length (drover-list--groups))
                               (length (drover-list--model-groups drover-list--model))))
                    (dolist (group (drover-list--groups))
                      (should-not (oref group hidden)))))
                ;; Drawn later, the list is the list drawn whole.
                (revert-buffer)
                (drover-list-test-idle-draw)
                (should (= (length (drover-list-test-drawn-ids)) 60))
                (should-not drover-list--deferred-timer)
                (drover-list-test-is-fresh)
                ;; Searching draws them at once.
                (revert-buffer)
                (should (< (length (drover-list-test-drawn-ids)) 60))
                (run-hooks 'isearch-mode-hook)
                (should (= (length (drover-list-test-drawn-ids)) 60))
                ;; Copying the whole of it too.
                (revert-buffer)
                (let ((transient-mark-mode t))
                  (save-excursion
                    (push-mark (point-min) t t)
                    (goto-char (point-max))
                    (drover-list-copy t)))
                (should (= (length (drover-list-test-drawn-ids)) 60))
                ;; The text of every session's lines, as drawn: in the
                ;; sidebar's 35 columns, the title of a subagent deep in
                ;; its tree may be cut before its identifier.
                (dolist (session drover-test-sessions)
                  (let ((section (drover-list-test-section (drover-session-id session))))
                    (should (string-search (buffer-substring-no-properties
                                            (oref section start)
                                            (or (oref section content) (oref section end)))
                                           (car kill-ring)))))
                (deactivate-mark)
                ;; And moving to the last session.
                (revert-buffer)
                (drover-list-last-session)
                (should (= (length (drover-list-test-drawn-ids)) 60))
                (should (equal (drover-list-test-id-at-point) (car (last (drover-list-test-order)))))))))))))

(ert-deftest drover-list-test-list-draws-first-screens ()
  "The list draws the rows of its first screens at once, the others later, as the sidebar does.
They are drawn while Emacs is idle, as a window nears them, or as
moving to the last session needs them, and the list is then the list
drawn whole."
  (dolist (grouping '(nil status))
    (drover-list-test-with (drover-list-test-random-sessions 60)
      (drover-list-test-showing
        (drover-list-test-shown
          (let ((drover-group-by grouping)
                (drover-show-subagents 'all)
                (drover-list--deferred-chunk 7))
            (cl-letf (((symbol-function 'drover-list--first-rows) (lambda () 10)))
              (revert-buffer)
              (let ((drawn (drover-list-test-drawn-ids)))
                (should (<= 10 (length drawn) 20))
                (should drover-list--undrawn))
              ;; Drawn while idle, the list is the list drawn whole.
              (drover-list-test-idle-draw)
              (should-not (seq-some (lambda (group) (and drover-list--undrawn (gethash group drover-list--undrawn)))
                                    (drover-list--model-groups drover-list--model)))
              (drover-list-test-is-fresh)
              ;; A window scrolled near the rows left draws them.
              (revert-buffer)
              (let ((before (length (drover-list-test-drawn-ids))))
                (set-window-start (selected-window) (point-min))
                (drover-list--draw-near-end)
                (should (> (length (drover-list-test-drawn-ids)) before)))
              (drover-list-test-is-fresh)
              ;; Moving to the last session draws them too.
              (revert-buffer)
              (drover-list-last-session)
              (should-not (seq-some (lambda (group) (and drover-list--undrawn (gethash group drover-list--undrawn)))
                                    (drover-list--model-groups drover-list--model)))
              (should (equal (drover-list-test-id-at-point)
                             (car (last (drover-list-test-drawn-ids))))))))))))

(ert-deftest drover-list-test-sidebar-in-place-while-drawing-later ()
  "Sessions changing while the sidebar left rows for later draw in place, as drawn anew.
Those changing among the rows left stay for later; once drawn, the
sidebar is the sidebar drawn whole."
  (pcase-dolist (`(,grouping ,sort ,descending) '((status urgency nil) (nil updated t)))
    (random (format "drover sidebar later %s" grouping))
    (drover-list-test-with (drover-list-test-random-sessions 50)
      (drover-list-test-showing
        (let ((drover-group-by grouping)
              (drover-sort-by sort)
              (drover-sort-descending descending)
              (in-place 0))
          (cl-letf (((symbol-function 'drover-list--first-rows) (lambda () 12)))
            (with-current-buffer (window-buffer (drover-list--show-side))
              (dotimes (round 5)
                (revert-buffer)
                (should (seq-some #'drover-list--deferred-p (drover-list--model-groups drover-list--model)))
                (dotimes (_ 8)
                  (pcase-let ((`(,sessions . ,ids) (drover-list-test-mutate drover-test-sessions)))
                    (setq drover-test-sessions sessions)
                    (dolist (id ids)
                      (drover-backend-changed 'fake id))
                    (when (zerop (drover-list-test-counting-redraws (drover-changed-now)))
                      (cl-incf in-place))))
                (ert-info ((format "%s, round %d" grouping round))
                  (drover-list-test-is-fresh)))
              (should (> in-place 20)))))))))

;;;; The sidebar beside the list

(defmacro drover-list-test-beside (&rest body)
  "Run BODY with the sidebar shown left of `first' and `second', ordinary windows.
The frame is 160 columns wide; the sidebar's window is `side', `first'
the selected window, above `second'.  Sidebars are placed as windows
changed, as the timer would."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-side-sample)
     (let ((size (cons (frame-width) (frame-height))))
       (unwind-protect
           (progn
             (set-frame-size (selected-frame) 160 50)
             (pcase-let ((`(,first ,second) (drover-list-test--main)))
               (let ((side (drover-list-side)))
                 (ignore first second side)
                 ,@body)))
         (set-frame-size (selected-frame) (car size) (cdr size))))))

(defun drover-list-test-place ()
  "Place the sidebars as windows changed, as the timer does."
  (drover-list--place-soon)
  (when-let* ((timer drover-list--place-timer))
    (cancel-timer timer)
    (funcall (timer--function timer))))

(ert-deftest drover-list-test-sidebar-steps-aside ()
  "The sidebar steps aside while the list shows beside it, and comes back where it was."
  (drover-list-test-beside
    (let ((width (window-total-width side)))
      ;; Its place as the user left it.
      (window-resize side 4 t nil t)
      (setq width (window-total-width side))
      (drover-list)
      (should (eq (window-buffer first) buffer))
      (should-not (drover-list--side-window))
      (should (equal (frame-parameter nil 'drover-list-sidebar-aside)
                     (list :side 'left :slot 0 :width width)))
      ;; The list took the room: it is drawn for it.
      (should (= drover-list--width (with-selected-window first (drover-list--line-width))))
      ;; Its buffer replaced: the sidebar comes back, as it was.
      (set-window-buffer first (get-buffer-create "*other 1*"))
      (drover-list-test-place)
      (let ((side (drover-list--side-window)))
        (should side)
        (should (eq (window-parameter side 'window-side) 'left))
        (should (= (window-total-width side) width))
        (should-not (frame-parameter nil 'drover-list-sidebar-aside))
        (should (eq (selected-window) first)))
      ;; Its window deleted.
      (drover-list)
      (should-not (drover-list--side-window))
      (delete-window first)
      (drover-list-test-place)
      (should (drover-list--side-window))
      ;; Buried.
      (select-window second)
      (drover-list)
      (should-not (drover-list--side-window))
      (bury-buffer)
      (should-not (eq (window-buffer second) buffer))
      (drover-list-test-place)
      (should (drover-list--side-window)))))

(ert-deftest drover-list-test-no-sideways-scrolling ()
  "Lists never scroll sideways by themselves, and windows scrolled so scroll back.
Lines fill the window from its left edge: point at the end of one must
not scroll the window, which then shows the lines cut on the left,
filling part of it, as the sidebar did once back beside the list."
  (drover-list-test-beside
    (with-current-buffer (window-buffer side)
      (should (local-variable-p 'auto-hscroll-mode))
      (should-not auto-hscroll-mode))
    (should (local-variable-p 'auto-hscroll-mode buffer))
    (should-not (buffer-local-value 'auto-hscroll-mode buffer))
    ;; Scrolled before, or by a command: brought up to date as it
    ;; shows, the sidebar scrolls back.
    (set-window-hscroll side 7)
    (should (= (window-hscroll side) 7))
    (drover-list--on-window-change side)
    (should (zerop (window-hscroll side)))
    ;; Drawn anew, too.
    (set-window-hscroll side 7)
    (with-current-buffer (window-buffer side)
      (drover-list--render))
    (should (zerop (window-hscroll side)))
    ;; Back from stepping aside, where it comes back to.
    (drover-list)
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (drover-list-test-place)
    (let ((side (drover-list--side-window)))
      (should (zerop (window-hscroll side)))
      (should (with-current-buffer (window-buffer side) (not auto-hscroll-mode))))))

(ert-deftest drover-list-test-sidebar-stays-unless-beside ()
  "The list in a window not beside the sidebar leaves it: both show."
  (drover-list-test-beside
    (let ((right (split-window first nil 'right)))
      (select-window right)
      (drover-list)
      (should (eq (window-buffer right) buffer))
      (drover-list-test-place)
      (should (eq (drover-list--side-window) side))
      (should-not (frame-parameter nil 'drover-list-sidebar-aside))
      ;; Moved beside it, by the window left of it going: it steps aside.
      (delete-window first)
      (drover-list-test-place)
      (should-not (drover-list--side-window))
      ;; It comes back where it was, whatever the options say now.
      (let ((drover-list-side-window-side 'right))
        (set-window-buffer right (get-buffer-create "*other 1*"))
        (drover-list-test-place)
        (should (eq (window-parameter (drover-list--side-window) 'window-side) 'left))))))

(ert-deftest drover-list-test-sidebar-as-the-user-asks ()
  "Shown beside the list by \\[drover-list-side], the sidebar stays; hidden, it does not come back."
  (drover-list-test-beside
    (drover-list)
    (should-not (drover-list--side-window))
    ;; Asked for, it shows beside the list, where it was, and stays.
    (drover-list-side)
    (should (drover-list--side-window))
    (should (frame-parameter nil 'drover-list-sidebar-kept))
    (should-not (frame-parameter nil 'drover-list-sidebar-aside))
    (drover-list-test-place)
    (should (drover-list--side-window))
    ;; Until the list leaves: beside it again, it steps aside.
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (drover-list-test-place)
    (should (drover-list--side-window))
    (should-not (frame-parameter nil 'drover-list-sidebar-kept))
    (select-window first)
    (drover-list)
    (should-not (drover-list--side-window))
    ;; Asked for, then hidden as asked: it does not come back.
    (drover-list-side)
    (drover-list-side)
    (should-not (drover-list--side-window))
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (drover-list-test-place)
    (should-not (drover-list--side-window))
    (should-not (frame-parameter nil 'drover-list-sidebar-aside))
    ;; Nor once q hid it.
    (drover-list-side)
    (select-window first)
    (drover-list)
    (drover-list-side)
    (with-selected-window (drover-list--side-window)
      (call-interactively (key-binding (kbd "q"))))
    (set-window-buffer first (get-buffer-create "*other 1*"))
    (drover-list-test-place)
    (should-not (drover-list--side-window))))

(ert-deftest drover-list-test-sidebar-placed-as-windows-change ()
  "Windows changing place the sidebars once they settled, through a timer."
  (drover-list-test-beside
    (should (memq #'drover-list--place-soon (default-value 'window-buffer-change-functions)))
    (should (memq #'drover-list--place-soon (default-value 'window-configuration-change-hook)))
    (set-window-buffer first (drover-list--buffer))
    (drover-list--place-soon)
    (drover-list--place-soon)
    (should (timerp drover-list--place-timer))
    (should (drover-list--side-window))
    (drover-list-test-place)
    (should-not drover-list--place-timer)
    (should-not (drover-list--side-window))))

;;;; Where the list shows

(defmacro drover-list-test-alone-beside (&rest body)
  "Run BODY with one ordinary window, `first', and the sidebar left of it.
The frame is 160 columns wide.  `first' shows `work', a buffer of its
own, as a session's buffer would, and is selected; the sidebar's
window is `side'.  The list's buffer is `buffer', shown nowhere yet."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-side-sample)
     (let ((size (cons (frame-width) (frame-height))))
       (unwind-protect
           (progn
             (set-frame-size (selected-frame) 160 50)
             (let ((first (selected-window))
                   (work (get-buffer-create "*other work*")))
               (switch-to-buffer work)
               (let ((side (drover-list-side)))
                 (should (eq (selected-window) first))
                 (ignore side)
                 ,@body)))
         (set-frame-size (selected-frame) (car size) (cdr size))))))

(defun drover-list-test-ordinary-windows ()
  "Return the windows of the selected frame outside side windows, in order.
That is from the top left one, whichever is selected."
  (seq-remove (lambda (window) (window-parameter window 'window-side))
              (window-list nil 'nomini (frame-first-window))))

(defun drover-list-test-quit ()
  "Type q in the selected window, the list's, and place the sidebars."
  (execute-kbd-macro (kbd "q"))
  (drover-list-test-place))

(ert-deftest drover-list-test-takes-the-selected-window ()
  "The list takes the selected window rather than splitting it, and q gives it back.
In a frame of one ordinary window beside the sidebar, the list shows in
that window, the sidebar stepping aside, then coming back once q brings
back the buffer the list replaced."
  (drover-list-test-alone-beside
    (drover-list)
    (should (equal (drover-list-test-ordinary-windows) (list first)))
    (should (eq (selected-window) first))
    (should (eq (window-buffer first) buffer))
    (should-not (drover-list--side-window))
    (should (frame-parameter nil 'drover-list-sidebar-aside))
    (drover-list-test-quit)
    (should (equal (drover-list-test-ordinary-windows) (list first)))
    (should (eq (window-buffer first) work))
    (should (eq (selected-window) first))
    (should (drover-list--side-window))
    (should-not (frame-parameter nil 'drover-list-sidebar-aside))
    ;; Again, the list buried rather than killed.
    (drover-list)
    (should (equal (drover-list-test-ordinary-windows) (list first)))
    (should (eq (window-buffer first) buffer))))

(ert-deftest drover-list-test-from-the-sidebar ()
  "From the sidebar, the list takes the window where you work, as sessions do.
It never splits a window, nor shows in the sidebar's; the sidebar steps
aside, and q brings back the buffer it replaced, and the sidebar."
  (drover-list-test-alone-beside
    (select-window side)
    (drover-list)
    (should (equal (drover-list-test-ordinary-windows) (list first)))
    (should (eq (selected-window) first))
    (should (eq (window-buffer first) buffer))
    (should-not (drover-list--side-window))
    (drover-list-test-quit)
    (should (equal (drover-list-test-ordinary-windows) (list first)))
    (should (eq (window-buffer first) work))
    (should (drover-list--side-window))
    ;; A window dedicated to its buffer is not where you work: the list
    ;; takes the other one, used last.
    (let ((second (split-window first nil 'below)))
      (set-window-buffer second (get-buffer-create "*other 2*"))
      (select-window second)
      (select-window first)
      (set-window-dedicated-p first t)
      (drover-list)
      (should (equal (drover-list-test-ordinary-windows) (list first second)))
      (should (eq (window-buffer first) work))
      (should (eq (window-buffer second) buffer))
      (should (eq (selected-window) second)))))

(ert-deftest drover-list-test-where-it-shows ()
  "The list shows in the selected one of two windows, or where it already shows.
Already shown, the command selects its window, which keeps it."
  (drover-list-test-alone-beside
    (let ((second (split-window first nil 'right))
          (other (get-buffer-create "*other 2*")))
      (set-window-buffer second other)
      (select-window second)
      (drover-list)
      (should (equal (drover-list-test-ordinary-windows) (list first second)))
      (should (eq (window-buffer second) buffer))
      (should (eq (window-buffer first) work))
      (should (eq (selected-window) second))
      ;; Not beside the sidebar: it stays.
      (drover-list-test-place)
      (should (drover-list--side-window))
      ;; Shown, from the other window: selected where it is.
      (select-window first)
      (drover-list)
      (should (equal (drover-list-test-ordinary-windows) (list first second)))
      (should (eq (window-buffer first) work))
      (should (eq (window-buffer second) buffer))
      (should (eq (selected-window) second))
      ;; From the sidebar too.
      (select-window side)
      (drover-list)
      (should (eq (window-buffer first) work))
      (should (eq (selected-window) second))
      (drover-list-test-quit)
      (should (eq (window-buffer second) other)))))

(ert-deftest drover-list-test-other-window ()
  "With a prefix argument, the list shows in another window.
That is the one used last, else one split from the selected window, or
from the window where you work when the sidebar is selected; q deletes
a window split for it."
  (drover-list-test-alone-beside
    (drover-list t)
    (let ((windows (drover-list-test-ordinary-windows)))
      (should (= (length windows) 2))
      (should (eq (window-buffer first) work))
      (should-not (eq (selected-window) first))
      (should (eq (window-buffer (selected-window)) buffer))
      (drover-list-test-quit)
      (should (equal (drover-list-test-ordinary-windows) (list first)))
      (should (eq (window-buffer first) work)))
    ;; From the sidebar, back from stepping aside.
    (select-window (or (drover-list--side-window) (error "No sidebar")))
    (drover-list t)
    (should (= (length (drover-list-test-ordinary-windows)) 2))
    (should (eq (window-buffer first) work))
    (should (eq (window-buffer (selected-window)) buffer))
    (drover-list-test-quit)
    ;; Of two windows, in the other one.
    (let ((second (split-window first nil 'right))
          (other (get-buffer-create "*other 2*")))
      (set-window-buffer second other)
      (select-window first)
      (drover-list t)
      (should (equal (drover-list-test-ordinary-windows) (list first second)))
      (should (eq (window-buffer first) work))
      (should (eq (window-buffer second) buffer))
      (should (eq (selected-window) second))
      ;; Already shown there, from there: in another window.
      (drover-list t)
      (should (equal (drover-list-test-ordinary-windows) (list first second)))
      (should (eq (window-buffer first) buffer))
      (should (eq (selected-window) first)))))

;;;; The sidebar and the list at once

(defmacro drover-list-test-two-lists (&rest body)
  "Run BODY with the sidebar and the list in an ordinary window, both shown.
The frame is 200 columns wide.  The side sample is served, sessions
open in buffers; `side' is the sidebar's window and `sidebar' its
buffer, `full' the window of the list in an ordinary window, `buffer',
right of another one, so that it is not beside the sidebar, which would
step aside, and `second' another ordinary window, below them.  The
list's window is selected, the list current."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-side-sample)
     (drover-list-test--register-buffers)
     (let ((size (cons (frame-width) (frame-height)))
           (drover-list-hidden-fields nil))
       (unwind-protect
           (progn
             (set-frame-size (selected-frame) 200 60)
             (pcase-let ((`(,first ,second) (drover-list-test--main)))
               (let* ((side (drover-list-side))
                      (sidebar (window-buffer side))
                      (full (split-window first nil 'right)))
                 (select-window full)
                 (drover-list)
                 (should (eq (selected-window) full))
                 (ignore first second sidebar)
                 ,@body)))
         (set-frame-size (selected-frame) (car size) (cdr size))))))

(ert-deftest drover-list-test-two-lists-at-once ()
  "Both lists show at once, each laid out for its width, with its own point.
The list folds its sections, the sidebar none.  A change draws both, in
place, each as a drawing anew draws it."
  (drover-list-test-two-lists
    (should (eq (window-buffer full) buffer))
    (should-not (eq sidebar buffer))
    (with-current-buffer buffer
      (should (eq drover-list--layout 'columns))
      (should header-line-format)
      (should (= drover-list--width (with-selected-window full (drover-list--line-width)))))
    (with-current-buffer sidebar
      (should (eq drover-list--layout 'narrow))
      (should-not header-line-format)
      (should (= drover-list--width (with-selected-window side (drover-list--line-width))))
      (should (< drover-list--width (buffer-local-value 'drover-list--width buffer))))
    ;; Each list its point; the list its sessions expanded and its
    ;; groups collapsed, the sidebar none, Idle shown whatever
    ;; `drover-list-collapsed-groups' says.  Its error, longer than its
    ;; line, gives the failed session details.
    (setf (drover-session-detail (drover-list-test-find "fail"))
          "Rate limited\nThe provider asks to wait a minute before the next request")
    (drover-backend-changed 'fake "fail")
    (drover-changed-now)
    (with-selected-window side
      (drover-list-test-goto "work")
      (magit-section-show (drover-list-test-section "ask")))
    (with-selected-window full
      (drover-list-test-goto "fail")
      (magit-section-show (drover-list-test-section "fail"))
      (magit-section-hide (seq-find (lambda (group) (equal (oref group value) '(status . done)))
                                    (drover-list--groups))))
    (cl-flet ((expanded-p (list id)
                (with-current-buffer list
                  (let ((section (drover-list-test-section id)))
                    (and (not (oref section hidden)) (magit-section-content-p section)))))
              (done-shown-p (list)
                (with-current-buffer list (not (drover-list-test-group-hidden-p 'done))))
              (idle-shown-p (list)
                (with-current-buffer list (not (drover-list-test-group-hidden-p 'idle)))))
      (should (expanded-p buffer "fail"))
      (should-not (expanded-p sidebar "ask"))
      (should-not (expanded-p sidebar "fail"))
      (should-not (done-shown-p buffer))
      (should (done-shown-p sidebar))
      (should-not (idle-shown-p buffer))
      (should (idle-shown-p sidebar))
      ;; A change: both lists draw it, in place.
      (setf (drover-session-title (drover-list-test-find "work")) "Renamed work"
            (drover-session-title (drover-list-test-find "ask")) "Renamed ask")
      (dolist (id '("work" "ask"))
        (drover-backend-changed 'fake id))
      (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
      (dolist (list (list sidebar buffer))
        (with-current-buffer list
          (should (string-match-p "Renamed work" (drover-list-test-heading "work")))
          (drover-list-test-is-fresh)))
      ;; Each kept its point and its sections.
      (should (equal (drover-list-test-at-side side) "work"))
      (should (equal (drover-list-test-at-side full) "fail"))
      (should (expanded-p buffer "fail"))
      (should-not (expanded-p sidebar "fail"))
      (should-not (done-shown-p buffer))
      (should (done-shown-p sidebar))
      ;; Commands act in the list they are typed in: TAB expands in the
      ;; list, and does nothing in the sidebar.
      (with-selected-window side
        (drover-list-test-goto "ask")
        (should (eq (key-binding (kbd "TAB")) 'ignore))
        (drover-list-test-press "TAB"))
      (should-not (expanded-p sidebar "ask"))
      (should-not (expanded-p buffer "ask"))
      (with-selected-window full
        (drover-list-test-goto "ask")
        (drover-list-test-press "TAB"))
      (should (expanded-p buffer "ask"))
      (should (equal (drover-list-test-at-side full) "ask")))))

(ert-deftest drover-list-test-two-lists-in-place-is-a-fresh-drawing ()
  "Whatever sessions change, both lists drawn in place are the lists drawn anew.
Grouped by status, by urgency, and as by default, ungrouped, last
activity first."
  (pcase-dolist (`(,grouping ,sort ,descending) '((status urgency nil) (nil updated t)))
    (random (format "drover two lists %s" grouping))
    (drover-list-test-two-lists
      (setq drover-test-sessions (drover-list-test-random-sessions 30))
      (let ((drover-list-collapsed-groups nil)
            (drover-group-by grouping)
            (drover-sort-by sort)
            (drover-sort-descending descending)
            (in-place 0))
        (drover-changed-now)
        (dolist (list (list sidebar buffer))
          (with-current-buffer list
            (revert-buffer)))
        ;; Some sessions of the list show their details; the sidebar
        ;; has none.
        (with-current-buffer buffer
          (dolist (section (seq-take (seq-filter (lambda (section) (oref section hidden))
                                                 (mapcan #'drover-list--session-sections
                                                         (drover-list--containers)))
                                     4))
            (magit-section-show section)))
        (dotimes (step 25)
          ;; Groups collapse and expand in the list; the sidebar folds
          ;; nothing.
          (when (zerop (random 4))
            (with-current-buffer buffer
              (when-let* ((groups (drover-list--groups)))
                (let ((group (seq-random-elt groups)))
                  (if (oref group hidden) (magit-section-show group) (magit-section-hide group))))))
          (pcase-let ((`(,sessions . ,ids) (drover-list-test-mutate drover-test-sessions)))
            (setq drover-test-sessions sessions)
            (dolist (id ids)
              (drover-backend-changed 'fake id))
            (when (zerop (drover-list-test-counting-redraws (drover-changed-now)))
              (cl-incf in-place))
            (dolist (list (list sidebar buffer))
              (with-current-buffer list
                (let ((drawn (drover-list-test-drawn))
                      (fresh (drover-list-test-fresh)))
                  (should (equal (list grouping step (buffer-name) (cdr drawn))
                                 (list grouping step (buffer-name) (cdr fresh))))
                  (should (equal-including-properties (car drawn) (car fresh))))))))
        (should (> in-place 10))))))

;;;; The order of the sessions needing the user

(defun drover-list-test-attention-order ()
  "Return the identifiers of the sessions needing the user in the order to visit them.
At the clock of the tests, as a pair (CACHED . OTHERS): those visited
for a live prompt cache, theirs or the top of their tree's (see
`drover-attention-cache'), and the others, both in order."
  (let ((order (drover-attention-order (drover-attention-sessions) drover-list-test-clock)))
    (cons (mapcar #'drover-session-id
                  (seq-filter (lambda (session)
                                (drover-attention-cache session drover-list-test-clock))
                              order))
          (mapcar #'drover-session-id
                  (seq-remove (lambda (session)
                                (drover-attention-cache session drover-list-test-clock))
                              order)))))

(ert-deftest drover-list-test-attention-order-follows-both-lists ()
  "Sessions with a live cache come first, then the others as both lists draw them.
The caches to expire soonest first, a subagent's counting as none; the
others in the order of the sidebar and of the list in an ordinary
window, which share the view settings, under several of them."
  (pcase-dolist (`(,mode ,grouping ,sort ,descending)
                 '((active status urgency nil) (all nil updated t) (active project title nil)
                   (all updated created t) (active model updated nil) (all status updated t)
                   (active nil urgency t) (all harness title t)))
    (random (format "drover attention order %s %s %s %s" mode grouping sort descending))
    (drover-list-test-two-lists
      (setq drover-test-sessions (drover-list-test-random-sessions 40))
      (drover-backend-changed 'fake)
      (drover-set-show-subagents mode)
      (drover-set-group-by grouping)
      (drover-set-sort sort descending)
      (pcase-let* ((`(,cached . ,others) (drover-list-test-attention-order))
                   (records (mapcar (lambda (id) (drover-list-test-find id)) cached))
                   (expiries (mapcar (lambda (record)
                                       (drover-attention-cache record drover-list-test-clock))
                                     records)))
        (should cached)
        (should others)
        (should (equal expiries (sort (copy-sequence expiries) #'<)))
        ;; A subagent ranks by the cache of the top of its tree, when it
        ;; waits, never by its own.
        (dolist (record records)
          (let ((target (drover-attention-target record)))
            (should-not (drover-session-parent target))
            (should (equal (drover-session-cache-expires target)
                           (drover-attention-cache record drover-list-test-clock)))
            (unless (eq target record)
              (should (eq (drover-session-status record) 'waiting)))))
        (dolist (list (list sidebar buffer))
          (with-current-buffer list
            (should (equal (list mode grouping sort descending
                                 (seq-filter (lambda (id) (member id others)) (drover-list-test-order)))
                           (list mode grouping sort descending others)))))))))

(ert-deftest drover-list-test-attention-order-filter ()
  "The sessions the filter leaves out come after those both lists draw.
Those in the order the lists would draw them without the filter."
  (random "drover attention order filter")
  (drover-list-test-two-lists
    (setq drover-test-sessions (mapcar (lambda (session)
                                         (setf (drover-session-cache-expires session) nil)
                                         session)
                                       (drover-list-test-random-sessions 40)))
    (drover-backend-changed 'fake)
    (drover-set-show-subagents 'all)
    (drover-set-group-by nil)
    (drover-set-sort 'updated t)
    (let ((whole (cdr (drover-list-test-attention-order))))
      (drover-set-filter :project "alpha")
      (let* ((order (cdr (drover-list-test-attention-order)))
             (drawn (with-current-buffer sidebar
                      (seq-filter (lambda (id) (member id order)) (drover-list-test-order))))
             (left (seq-remove (lambda (id) (member id drawn)) order)))
        (should drawn)
        (should left)
        (should (equal (seq-take order (length drawn)) drawn))
        (should (equal (nthcdr (length drawn) order)
                       (seq-filter (lambda (id) (member id left)) whole)))
        (with-current-buffer buffer
          (should (equal (seq-filter (lambda (id) (member id order)) (drover-list-test-order))
                         drawn)))))))

(ert-deftest drover-list-test-two-lists-share-rows ()
  "A row made for one list, the other copies: its texts are made and measured once."
  (drover-list-test-two-lists
    (let ((made nil))
      (cl-letf* ((title-text (symbol-function 'drover-list--title-text))
                 ((symbol-function 'drover-list--title-text)
                  (lambda (session &rest arguments)
                    (push (drover-session-id session) made)
                    (apply title-text session arguments))))
        (setf (drover-session-title (drover-list-test-find "work")) "Renamed work")
        (drover-backend-changed 'fake "work")
        (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
        (should (equal made '("work")))
        (let ((rows (mapcar (lambda (list)
                              (with-current-buffer list
                                (gethash '(fake . "work") (drover-list--model-rows drover-list--model))))
                            (list sidebar buffer))))
          (should-not (eq (car rows) (cadr rows)))
          (should (eq (drover-list--row-texts (car rows)) (drover-list--row-texts (cadr rows))))
          (should (eq (drover-list--row-measures (car rows)) (drover-list--row-measures (cadr rows)))))
        ;; A sidebar drawn from nothing beside the list makes no row.
        (setq made nil)
        (with-current-buffer sidebar
          (revert-buffer)
          (should-not made)
          (should (string-match-p "Renamed work" (drover-list-test-heading "work")))
          (drover-list-test-is-fresh))
        ;; Measured otherwise, it makes its own.
        (setq made nil)
        (with-current-buffer sidebar
          (face-remap-add-relative 'default :height 2.0)
          (drover-list--render))
        (should (member "work" made))))))

(ert-deftest drover-list-test-two-lists-make-trees-once ()
  "A change makes the trees it touches once, for both lists."
  (drover-list-test-two-lists
    (let ((made 0))
      (cl-letf* ((tree-sessions (symbol-function 'drover-index-tree-sessions))
                 ((symbol-function 'drover-index-tree-sessions)
                  (lambda (root) (cl-incf made) (funcall tree-sessions root))))
        (setf (drover-session-title (drover-list-test-find "work")) "Renamed work")
        (drover-backend-changed 'fake "work")
        (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
        (should (= made 1))
        (dolist (list (list sidebar buffer))
          (with-current-buffer list
            (should (string-match-p "Renamed work" (drover-list-test-heading "work")))))))
    ;; A setting changed draws both whole, from trees made once.
    (let ((made 0))
      (cl-letf* ((tree (symbol-function 'drover-tree))
                 ((symbol-function 'drover-tree)
                  (lambda (sessions) (cl-incf made) (funcall tree sessions))))
        (should (= (drover-list-test-counting-redraws (drover-set-group-by 'project)) 2))
        (should (= made 1))
        (dolist (list (list sidebar buffer))
          (with-current-buffer list
            (should (equal (mapcar #'car (drover-list-test-groups)) '(nil)))
            (drover-list-test-is-fresh)))))))

(ert-deftest drover-list-test-two-lists-hidden-one-waits ()
  "With the list hidden, a change draws the sidebar alone; the list catches up once shown."
  (drover-list-test-two-lists
    (set-window-buffer full (get-buffer-create "*other 1*"))
    (let ((text (with-current-buffer buffer (buffer-string))))
      (setf (drover-session-title (drover-list-test-find "work")) "Renamed work")
      (drover-backend-changed 'fake "work")
      (should (zerop (drover-list-test-counting-redraws (drover-changed-now))))
      (with-current-buffer sidebar
        (should (string-match-p "Renamed work" (drover-list-test-heading "work"))))
      (with-current-buffer buffer
        (should (equal (buffer-string) text))
        (should (equal (car drover-list--pending) '((fake . "work"))))))
    (set-window-buffer full buffer)
    (should (zerop (drover-list-test-counting-redraws (drover-list--on-window-change full))))
    (with-current-buffer buffer
      (should (string-match-p "Renamed work" (drover-list-test-heading "work")))
      (drover-list-test-is-fresh))))

(ert-deftest drover-list-test-two-lists-title-faces ()
  "Both lists colour a title by where its session is."
  (drover-list-test-two-lists
    (select-window side)
    (drover-list-test-goto "done")
    (execute-kbd-macro (kbd "RET"))
    ;; Opened in the window where the user works, beside both lists.
    (should (equal (buffer-name (window-buffer first)) "*session done*"))
    (should (eq (selected-window) first))
    (should (eq (window-buffer full) buffer))
    (drover-list--windows-changed)
    (dolist (list (list sidebar buffer))
      (with-current-buffer list
        (should (eq (drover-list-test-title-face "done") 'opencode-session-active))))
    ;; Another session shown, the user working in a list: the window
    ;; selected before stays the one worked in.
    (select-window side)
    (drover-list-test-goto "fail")
    (execute-kbd-macro (kbd "C-o"))
    (drover-list--windows-changed)
    (dolist (list (list sidebar buffer))
      (with-current-buffer list
        (should (eq (drover-list-test-title-face "fail") 'opencode-session-active))
        (should (eq (drover-list-test-title-face "done") 'opencode-session-open))))))

(ert-deftest drover-list-test-two-lists-menu-targets ()
  "The menu acts on the list it opens in, else on the list used last."
  (drover-list-test-two-lists
    (with-selected-window side (drover-list-test-goto "work"))
    (with-selected-window full (drover-list-test-goto "fail"))
    ;; In a list, on that list.
    (with-selected-window side
      (let ((context (drover--context)))
        (should-not (plist-get context :window))
        (should (equal (drover-session-id (plist-get context :session)) "work"))))
    (with-selected-window full
      (should (equal (drover-session-id (plist-get (drover--context) :session)) "fail")))
    ;; Elsewhere, on the list used last.
    (let ((code (drover-list-test-code-buffer)))
      (set-window-buffer second code)
      (select-window side)
      (select-window second)
      (with-current-buffer code
        (should (eq (plist-get (drover--context) :window) side))
        (should (equal (drover-session-id (plist-get (drover--context) :session)) "work")))
      (select-window full)
      (select-window second)
      (with-current-buffer code
        (should (eq (plist-get (drover--context) :window) full))
        (should (equal (drover-session-id (plist-get (drover--context) :session)) "fail"))))))

(ert-deftest drover-list-test-two-lists-recaps ()
  "Only the list fetches the last turn of a session expanded: the sidebar has no details."
  (drover-list-test-two-lists
    (setf (alist-get 'fake drover--backends)
          (append (list :recap #'drover-test--recap) (alist-get 'fake drover--backends)))
    (with-current-buffer sidebar
      (magit-section-show (drover-list-test-section "ask"))
      (should-not (string-match-p "Prompt" (drover-list-test-visible-text))))
    (should-not drover-test-recap-calls)
    (with-current-buffer buffer
      (magit-section-show (drover-list-test-section "ask"))
      (should (string-match-p "Prompt +Prompt of ask" (drover-list-test-visible-text))))
    (should (equal drover-test-recap-calls '("ask")))
    ;; The session changed: the list fetches it again, the sidebar not.
    (setq drover-list-test-clock (+ drover-list-test-clock drover-list--recap-interval))
    (cl-incf (drover-session-since (drover-list-test-find "ask")) 1)
    (setf (drover-session-updated (drover-list-test-find "ask")) drover-list-test-clock)
    (drover-backend-changed 'fake "ask")
    (drover-changed-now)
    (should (equal drover-test-recap-calls '("ask" "ask")))
    (with-current-buffer sidebar
      (should-not drover-list--recaps))))

;;;; Active subagents

(defun drover-list-test-busy-family ()
  "Return an idle parent whose subagents wait, work, wait to retry and ended.
The one waiting to retry is a subagent of the working one."
  (list (drover-test-session "p" :status 'idle :updated (drover-list-test-ago 600))
        (drover-test-session "a" :parent "p" :status 'waiting :since (drover-list-test-ago 60))
        (drover-test-session "w" :parent "p" :status 'working :updated (drover-list-test-ago 30))
        (drover-test-session "r" :parent "w" :status 'retrying :updated (drover-list-test-ago 20))
        (drover-test-session "d" :parent "p" :status 'done :since (drover-list-test-ago 10))))

(ert-deftest drover-list-test-active-subagents-indicator ()
  "A parent tells its subagents at work, shown or not: waiting, then working."
  (drover-list-test-with (drover-list-test-busy-family)
    (setq drover-show-subagents 'none)
    (drover-list-test-refresh)
    ;; No subagent shows, but the parent says one waits for you and two
    ;; work, the one waiting to retry among them; the one that ended
    ;; does not count.
    (should (equal (drover-list-test-groups) '((idle "p"))))
    (drover-list-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (should (string-prefix-p "  · ? * Session p" (drover-list-test-heading "p")))
    (drover-list-test-goto "p")
    (let* ((start (point))
           (waiting (save-excursion (search-forward "?") (match-beginning 0)))
           (working (save-excursion (search-forward "*") (match-beginning 0)))
           (help (concat "1 subagent waiting for you: Session a\n"
                         "2 subagents working: Session r, Session w")))
      (should (eq (get-text-property waiting 'font-lock-face) 'opencode-session-status-waiting))
      (should (eq (get-text-property working 'font-lock-face) 'opencode-session-status-working))
      ;; Its tooltip names them, most urgent first, and counts them.
      (should (equal (get-text-property waiting 'help-echo) help))
      (should (equal (get-text-property working 'help-echo) help))
      ;; The rest of the line names them too, after the session.
      (should (equal (get-text-property (+ start 2) 'help-echo) (concat "Session p\n" help))))
    ;; Shown, they tell the same, the subagents under the parent's
    ;; title; a subagent tells its own.
    (setq drover-show-subagents 'all)
    (drover-list-test-refresh)
    (should (equal (drover-list-test-groups) '((waiting "p" "a" "d" "w" "r"))))
    (should (string-prefix-p "  · ? * Session p" (drover-list-test-heading "p")))
    (should (string-prefix-p "        ◐ * Session w" (drover-list-test-heading "w")))
    (should (string-prefix-p "        ! Session a" (drover-list-test-heading "a")))
    (should (string-prefix-p "            ↻ Session r" (drover-list-test-heading "r")))
    ;; Once they are done, nothing.
    (dolist (id '("a" "w" "r"))
      (setf (drover-session-status (drover-list-test-find id)) 'done))
    (drover-list-test-refresh)
    (should (string-prefix-p "  · Session p" (drover-list-test-heading "p")))))

(ert-deftest drover-list-test-active-subagents-not-counted ()
  "The glyphs of active subagents tell no number: their tooltip does."
  (drover-list-test-with
      (list (drover-test-session "p" :status 'idle)
            (drover-test-session "a1" :parent "p" :status 'waiting)
            (drover-test-session "a2" :parent "p" :status 'waiting)
            (drover-test-session "w" :parent "p" :status 'working))
    (setq drover-show-subagents 'none
          drover-list-collapsed-groups nil)
    (revert-buffer)
    (should (string-prefix-p "  · ? * Session p" (drover-list-test-heading "p")))
    (drover-list-test-goto "p")
    (search-forward "?")
    (should (equal (get-text-property (match-beginning 0) 'help-echo)
                   (concat "2 subagents waiting for you: Session a1, Session a2\n"
                           "1 subagent working: Session w")))))

(ert-deftest drover-list-test-active-subagents-aligned ()
  "With the indicator, one space still parts each glyph, and columns align."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "p" "mm" :status 'working :project "x"
                                       :updated (drover-list-test-ago 5))
              (drover-list-test-titled "a" "Ask" :parent "p" :status 'waiting :project "y"
                                       :since (drover-list-test-ago 7200))
              (drover-list-test-titled "q" "Quiet" :status 'working :project "zz"
                                       :updated (drover-list-test-ago 60)))
      (should (eq drover-list--layout 'columns))
      (let* ((metrics (drover-list--metrics 0))
             (margin (plist-get metrics :margin))
             (space (plist-get metrics :space))
             (glyph (car (drover-list-test-cells "p"))))
        ;; The glyph, a space, the indicator, a space, the title, all in
        ;; one cell: "◐ ? mm" is 7 + 4 + 7 + 4 then the title.
        (should (= (car glyph) margin))
        (should (equal (substring-no-properties (cdr glyph)) "◐ ? mm"))
        (should (= (drover-list-test-fake-width (cdr glyph)) (+ 7 space 7 space 24)))
        ;; Locations begin at one position whatever the glyphs before.
        (let ((location (drover-list-test-cell-at "p" "x")))
          (should (= (drover-list-test-cell-at "a" "y") location))
          (should (= (drover-list-test-cell-at "q" "zz") location)))
        ;; Ages end at one position.
        (let ((ends (mapcar (lambda (id)
                              (let ((cell (seq-find (lambda (cell)
                                                      (string-match-p "\\`[0-9]+[smhd]\\'" (cdr cell)))
                                                    (drover-list-test-cells id))))
                                (+ (car cell) (drover-list-test-fake-width (cdr cell)))))
                            '("p" "a" "q"))))
          (should (= (length (seq-uniq ends)) 1)))))))

(defun drover-list-test-ink-spans ()
  "Return what the visible characters of the line at point draw, in the fake font.
The value is a list of (START . END) in pixels, one per character
drawn: its advance, or the ink `drover-list-test-ink' gives it, when it
reaches further.  Spaces draw nothing, and a character displayed as a
string draws that string."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
        (x 0)
        (spans nil))
    (dotimes (index (length line))
      (let* ((char (substring line index (1+ index)))
             (display (get-text-property index 'display line))
             (shown (if (stringp display) display char)))
        (pcase display
          (`(space :align-to (,to)) (setq x to))
          (_
           (unless (or (consp display) (string-blank-p shown))
             (let ((ink (alist-get (aref shown 0) drover-list-test-ink))
                   (width (drover-list-test-fake-width shown)))
               (push (cons (- x (or (car ink) 0)) (+ x (max width (or (cdr ink) 0)))) spans)))
           (setq x (+ x (drover-list-test-fake-width char)))))))
    (nreverse spans)))

(defun drover-list-test-ink-apart-p ()
  "Return non-nil when nothing the line at point draws overlaps, in the fake font."
  (let ((spans (drover-list-test-ink-spans)))
    (seq-every-p (lambda (pair) (<= (cdr (car pair)) (car (cdr pair))))
                 (seq-mapn #'cons spans (cdr spans)))))

(ert-deftest drover-list-test-glyphs-leave-room-for-ink ()
  "Text after a glyph begins past its ink, which may reach past its advance.
Icon fonts draw icons wider than the advance they give them; titles
still line up, and subagents under their parent's title."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "p" "Parent" :status 'working :project "x"
                                       :updated (drover-list-test-ago 5))
              (drover-list-test-titled "a" "Ask" :parent "p" :status 'waiting :project "y"
                                       :since (drover-list-test-ago 60)
                                       :requests '((:kind question :summary "Go on")))
              (drover-list-test-titled "w" "Work" :parent "p" :status 'working :project "z"
                                       :updated (drover-list-test-ago 7))
              (drover-list-test-titled "q" "Quiet" :status 'idle :project "x"))
      ;; Glyphs advance 7 pixels: the ink of the waiting subagents' reaches
      ;; 13 pixels right, that of the waiting status and the idle one 12,
      ;; that of the working subagents' 3 pixels left.
      (let ((drover-list-test-ink '((?? 0 . 13) (?! 0 . 12) (?· 0 . 12) (?* 3 . 7)))
            (drover-show-subagents 'all)
            (drover-list-collapsed-groups nil))
        (dolist (width '(1000 250))
          (let ((drover-list-test-width width))
            (revert-buffer)
            (should (eq drover-list--layout (if (= width 1000) 'columns 'narrow)))
            (dolist (id '("p" "a" "w" "q"))
              (save-excursion
                (drover-list-test-goto id)
                (should (drover-list-test-ink-apart-p))))
            (should (equal (plist-get (drover-list--metrics 0) :status-cell) '(0 . 12)))
            (should (equal (plist-get (drover-list--metrics 0) :subagent-cell) '(3 . 13)))
            ;; "◐" and its pad, a space, the subagents' glyphs in their
            ;; cells of 16 pixels and a space each: 12 + 4 + 2 * 20.
            (should (= (drover-list-test-title-x "p" "Parent") (+ 14 12 4 20 20)))
            (should (= (drover-list-test-title-x "q" "Quiet") (+ 14 12 4)))
            (should (= (car (car (drover-list-test-cells "a"))) (drover-list-test-title-x "p" "Parent")))
            (should (= (car (car (drover-list-test-cells "w"))) (drover-list-test-title-x "p" "Parent")))))
        ;; A group heading's glyph and count.
        (setq drover-group-by 'project)
        (revert-buffer)
        (drover-list-test-goto-group "x")
        (should (string-match-p "\\`x (4)  ! +1\\'" (drover-list-test-line)))
        (should (drover-list-test-ink-apart-p))))))

(ert-deftest drover-list-test-active-subagents-narrow ()
  "In two lines too, the indicator stands between the status and the title.
The subagent's glyph stands under its parent's title."
  (drover-list-test-with (list (drover-test-session "p" :status 'idle :updated (drover-list-test-ago 60))
                               (drover-test-session "a" :parent "p" :status 'waiting
                                                    :since (drover-list-test-ago 120)
                                                    :requests '((:kind question :summary "Go?"))))
    (drover-list-test-showing
      (with-current-buffer (window-buffer (drover-list--show-side))
        (should (eq drover-list--layout 'narrow))
        ;; The sidebar's lines begin with their glyphs.
        (should (string-match-p "\\`· \\? Session p +1m ▣\\'" (drover-list-test-entry "p")))
        (should (string-match-p "\\`    ! Session a +2m ▣\n      asks: Go\\?\\'" (drover-list-test-entry "a")))))))

;;;; Open sessions

(ert-deftest drover-list-test-open-sessions ()
  "The title of a session open in a buffer is in its face, over its other faces."
  (drover-list-test-with (list (drover-test-session "work" :status 'working)
                               (drover-list-test-titled "idle" "Idle and open for a long while"
                                                        :status 'idle)
                               (drover-test-session "shut" :status 'working))
    (drover-list-test--register-buffers)
    (setq drover-list-collapsed-groups nil)
    (get-buffer-create "*session work*")
    (get-buffer-create "*session idle*")
    (revert-buffer)
    (cl-flet ((title-face (id title)
                (save-excursion
                  (drover-list-test-goto id)
                  (search-forward title)
                  (let ((start (match-beginning 0)))
                    (should (equal (get-text-property start 'face)
                                   (get-text-property start 'font-lock-face)))
                    ;; One face over the whole title.
                    (should (= (next-single-property-change start 'face nil (point)) (point)))
                    (get-text-property start 'face)))))
      (should (eq (title-face "work" "Session work") 'opencode-session-open))
      ;; Over the dimming of idle sessions, which lends what it lacks.
      (should (equal (title-face "idle" "Idle and open") '(opencode-session-open drover-dim)))
      (should-not (title-face "shut" "Session shut"))
      ;; Only the title: the glyph keeps the face of its status.
      (save-excursion
        (drover-list-test-goto "work")
        (search-forward "◐")
        (should (eq (get-text-property (match-beginning 0) 'face) 'opencode-session-status-working)))
      ;; Closing a buffer is a change its backend reports: the list,
      ;; shown, redraws.
      (drover-list-test-shown
        (kill-buffer "*session work*")
        (drover-backend-changed 'fake "work")
        (drover-changed-now)
        (should-not (title-face "work" "Session work"))
        (should (equal (title-face "idle" "Idle and open") '(opencode-session-open drover-dim)))))
    ;; A title cut to fit keeps the face up to its ellipsis.
    (drover-list-test-with-font
      (let ((drover-list-test-width 150))
        (revert-buffer)
        (save-excursion
          (drover-list-test-goto "idle")
          (search-forward (truncate-string-ellipsis) (line-end-position))
          (should (equal (get-text-property (match-beginning 0) 'face)
                         '(opencode-session-open drover-dim))))))))

;;;; Places

(ert-deftest drover-list-test-current-face-obsolete ()
  "The face of the former highlight of the main window's session stands for the active title."
  (should (eq (get 'drover-list-current 'face-alias) 'opencode-session-active))
  (should (get 'drover-list-current 'obsolete-face)))

(ert-deftest drover-list-test-title-faces-are-themes ()
  "The faces of places inherit sibling faces of font lock, none of a status."
  (should (equal (face-attribute 'opencode-session-open :inherit nil t) 'font-lock-keyword-face))
  (should (equal (face-attribute 'opencode-session-shown :inherit nil t) 'font-lock-type-face))
  (should (equal (face-attribute 'opencode-session-active :inherit nil t) 'font-lock-function-name-face))
  (dolist (status drover-status-order)
    (should-not (memq (face-attribute (opencode-session-status-face status) :inherit nil t)
                      '(font-lock-type-face font-lock-function-name-face)))))

(ert-deftest drover-list-test-cache-line-follows-theme ()
  "The line of a prompt cache takes the colour of `opencode-cache-bar', anew with a theme."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 60)
                                 :requests '((:kind question :summary "Go?"))
                                 :cache-expires (+ drover-test-now 300) :cache-ttl 300))
    (unwind-protect
        (drover-list-test-with-font
          (let ((drover-list-test-width 300))
            (drover-list-test-shown
              (set-face-attribute 'opencode-cache-bar nil :foreground "red")
              (revert-buffer)
              (should (eq drover-list--layout 'narrow))
              (cl-flet ((color ()
                          (plist-get (plist-get (overlay-get (plist-get (car drover-list--caches) :overlay)
                                                             'face)
                                                :underline)
                                     :color)))
                (should (equal (color) "red"))
                (set-face-attribute 'opencode-cache-bar nil :foreground "blue")
                ;; A theme enabled redraws the list in the new colour.
                (should (memq #'drover-list--theme-changed enable-theme-functions))
                (run-hook-with-args 'enable-theme-functions 'some-theme)
                (should (equal (color) "blue"))))))
      (set-face-attribute 'opencode-cache-bar nil :foreground 'unspecified))))

;;;; Animated glyphs

(ert-deftest drover-list-test-animated-glyphs ()
  "Working glyphs show their frames in turn, each as wide as the widest."
  (drover-list-test-with-font
    (drover-list-test-with
        (list (drover-list-test-titled "w" "Work" :status 'working :project "p"
                                       :updated (drover-list-test-ago 5))
              (drover-list-test-titled "r" "Retry" :status 'retrying :project "q"
                                       :updated (drover-list-test-ago 7))
              (drover-list-test-titled "i" "Idle" :status 'idle :project "p"))
      (let ((opencode-session-status-animation-interval 1)
            (opencode-session-status-animations '((working "i" "m" "x") (retrying "ii" "m")))
            (drover-list-collapsed-groups nil))
        ;; 1800000000 seconds: frame 0 of three, 0 of two.
        (revert-buffer)
        (drover-list--draw-deferred)
        (let ((root magit-root-section)
              (text (buffer-substring-no-properties (point-min) (point-max)))
              (title (drover-list-test-cell-at "w" "Work"))
              (location (drover-list-test-cell-at "w" "p")))
          (cl-flet ((frame (id)
                      (save-excursion
                        (drover-list-test-goto id)
                        (let ((cell (text-property-not-all (point) (line-end-position)
                                                           'drover-list-frame nil)))
                          (list (get-text-property cell 'display)
                                (get-text-property (1+ cell) 'display)
                                (get-text-property cell 'drover-list-frame)))))
                    (width (id) (drover-list-test-fake-width (cdr (car (drover-list-test-cells id))))))
            ;; The frame, then a stretch up to the widest frame, 12 pixels.
            (should (equal (frame "w") '("i" (space :width (9)) working)))
            (should (eq (get-text-property 0 'face (car (frame "w"))) 'opencode-session-status-working))
            (should (equal (frame "r") '("ii" (space :width (6)) retrying)))
            (should (eq (get-text-property 0 'face (car (frame "r"))) 'opencode-session-status-retrying))
            ;; Still glyphs stay still.
            (should-not (save-excursion (drover-list-test-goto "i")
                                        (text-property-not-all (point) (line-end-position)
                                                               'drover-list-frame nil)))
            (let ((glyph-and-title (width "w")))
              (dolist (step '((1 "m" 0 "m" 0) (2 "x" 5 "ii" 6) (3 "i" 9 "m" 0)))
                (pcase-let ((`(,seconds ,working ,pad ,retrying ,retry-pad) step))
                  (setq drover-list-test-clock (+ drover-test-now seconds))
                  (drover-list--draw-frames)
                  (should (equal (frame "w") (list working `(space :width (,pad)) 'working)))
                  (should (equal (frame "r") (list retrying `(space :width (,retry-pad)) 'retrying)))
                  ;; Nothing after the glyph moves.
                  (should (= (width "w") glyph-and-title))
                  (should (= (drover-list-test-cell-at "w" "Work") title))
                  (should (= (drover-list-test-cell-at "w" "p") location)))))
            ;; Nothing was drawn anew.
            (should (eq magit-root-section root))
            (should (equal (buffer-substring-no-properties (point-min) (point-max)) text))
            (should-not (buffer-modified-p))
            ;; A redraw keeps the pace: the frame of the time.
            (setq drover-list-test-clock (+ drover-test-now 4))
            (revert-buffer)
            (should (equal (frame "w") '("m" (space :width (0)) working)))))))))

(ert-deftest drover-list-test-still-indicator ()
  "The glyphs of active subagents stay still while status glyphs move."
  (drover-list-test-with (list (drover-test-session "p" :status 'working)
                               (drover-test-session "a" :parent "p" :status 'waiting)
                               (drover-test-session "w" :parent "p" :status 'working))
    ;; Frames for subagents, as the option once took, animate nothing.
    (let ((opencode-session-status-animation-interval 1)
          (opencode-session-status-animations '((working "1" "2" "3") (subagents "+" "*")))
          (drover-show-subagents 'none))
      (revert-buffer)
      (should (string-prefix-p "  1 ? * Session p" (drover-list-test-heading "p")))
      ;; The status glyph alone is animated.
      (should (equal (mapcar #'cdr drover-list--animated) '(working)))
      (setq drover-list-test-clock (+ drover-test-now 1))
      (drover-list--draw-frames)
      (should (string-prefix-p "  2 ? * Session p" (drover-list-test-heading "p"))))))

(ert-deftest drover-list-test-default-animations ()
  "By default, only status glyphs move, each frame close to the one before."
  (let ((animations (eval (car (get 'opencode-session-status-animations 'standard-value)) t)))
    (should (equal (mapcar #'car animations) '(working retrying)))
    ;; The phases of the moon in order; the hourglass blinked.
    (should (equal (alist-get 'working animations)
                   '("\U000F0F64" "\U000F0F67" "\U000F0F61" "\U000F0F68"
                     "\U000F0F62" "\U000F0F66" "\U000F0F63" "\U000F0F65")))
    (should (= (eval (car (get 'opencode-session-status-animation-interval 'standard-value)) t)
               0.15))
    ;; Four filled clocks, the hand alone turning.
    (should (equal (alist-get 'retrying animations)
                   '("\U000F144A" "\U000F1441" "\U000F1444" "\U000F1447")))))

(ert-deftest drover-list-test-animated-glyph-under-cache-bar ()
  "In two lines, a cache bar may end within an animated glyph, or past it."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)
                                 :detail "Busy"
                                 :cache-expires (+ drover-test-now 300) :cache-ttl 300))
    (drover-list-test-with-font
      (let ((drover-list-test-width 300)
            (opencode-session-status-animation-interval 1)
            (opencode-session-status-animations '((working "i" "m"))))
        (revert-buffer)
        (should (eq drover-list--layout 'narrow))
        (let* ((glyph (car (car drover-list--animated)))
               (overlay (plist-get (car drover-list--caches) :overlay))
               (segment (seq-find (lambda (item) (and (eq (nth 1 item) 'text) (= (nth 2 item) 14)))
                                  (plist-get (car drover-list--caches) :spec))))
          ;; The glyph's segment: 12 pixels of frame and pad, a space of 4.
          (should (= (nth 3 segment) 16))
          ;; 49 pixels of 293: in the title, past the glyph and its space.
          (setq drover-list-test-clock (+ drover-test-now 250))
          (drover-list--draw-frames)
          (drover-list--draw-caches)
          (should (> (overlay-end overlay) (+ glyph 2)))
          ;; 20 pixels, 6 into the glyph: the frame "i", 3 pixels, fits,
          ;; not its pad.
          (setq drover-list-test-clock (+ drover-test-now 280))
          (drover-list--draw-frames)
          (should (equal (get-text-property glyph 'display) "i"))
          (drover-list--draw-caches)
          (should (= (overlay-end overlay) (1+ glyph)))
          ;; "m", 12 pixels, does not.
          (setq drover-list-test-clock (+ drover-test-now 279.6))
          (drover-list--draw-frames)
          (should (equal (get-text-property glyph 'display) "m"))
          (drover-list--draw-caches)
          (should (= (overlay-end overlay) glyph)))))))

(ert-deftest drover-list-test-animation-off ()
  "Without an interval, glyphs stay still and no timer runs."
  (drover-list-test-with (list (drover-test-session "w" :status 'working)
                               (drover-test-session "s" :parent "w" :status 'working))
    (drover-list-test-shown
      (revert-buffer)
      (should-not drover-list--animated)
      (should-not (text-property-not-all (point-min) (point-max) 'drover-list-frame nil))
      (should (string-prefix-p "  ◐ * Session w" (drover-list-test-heading "w")))
      (should-not drover-list--frame-timer))))

(ert-deftest drover-list-test-animation-skips-hidden-glyphs ()
  "Glyphs out of sight keep their frame until they show."
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (let ((opencode-session-status-animation-interval 1))
      (revert-buffer)
      (drover-list-test-goto-group 'working)
      (magit-section-hide (magit-current-section))
      (let ((position (car (car drover-list--animated))))
        (should (equal (get-text-property position 'display) "1"))
        (setq drover-list-test-clock (+ drover-test-now 1))
        (drover-list--draw-frames)
        (should (equal (get-text-property position 'display) "1"))
        (magit-section-show (magit-current-section))
        (drover-list--draw-frames)
        (should (equal (get-text-property position 'display) "2"))))))

(ert-deftest drover-list-test-animation-timer ()
  "The frame timer runs while a shown list animates a glyph, and only then."
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (let ((opencode-session-status-animation-interval 0.5))
      ;; Hidden: no timer.
      (drover-list-test-refresh)
      (should drover-list--animated)
      (should-not drover-list--frame-timer)
      (drover-list-test-shown
        (drover-list-test-refresh)
        (should (timerp drover-list--frame-timer))
        (should (= (timer--repeat-delay drover-list--frame-timer) 0.5))
        ;; It ticks mid-frame, so that a late tick shows no frame twice.
        (should (< (abs (- (opencode-session-status-frame-delay 10.1) 0.65)) 1e-9))
        (should (< (abs (- (opencode-session-status-frame-delay 10.0) 0.75)) 1e-9))
        (should (< (abs (- (opencode-session-status-frame-delay 10.49) 0.26)) 1e-9))
        ;; A tick shows the next frame.
        (setq drover-list-test-clock (+ drover-test-now 0.5))
        (drover-list--frame-tick)
        (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))
        (should (timerp drover-list--frame-timer))
        ;; Nothing animates: the timer stops.
        (setf (drover-session-status (drover-list-test-find "w")) 'done)
        (drover-list-test-refresh)
        (should-not drover-list--animated)
        (should-not drover-list--frame-timer)
        (setf (drover-session-status (drover-list-test-find "w")) 'working)
        (drover-list-test-refresh)
        (should (timerp drover-list--frame-timer)))
      ;; Hidden again: the next tick stops it, drawing nothing.
      (let ((display (get-text-property (car (car drover-list--animated)) 'display)))
        (setq drover-list-test-clock (+ drover-test-now 2))
        (drover-list--frame-tick)
        (should-not drover-list--frame-timer)
        (should (eq (get-text-property (car (car drover-list--animated)) 'display) display)))
      ;; Shown again, it starts again; killing the list stops it.
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (drover-list--on-window-change (selected-window))
        (should (timerp drover-list--frame-timer))
        (kill-buffer buffer)
        (should-not drover-list--frame-timer)))))

(ert-deftest drover-list-test-buffers-tracked ()
  "Lists are known as they come and go: timers and window hooks scan no buffer list."
  (drover-list-test-with (list (drover-test-session "w" :status 'working
                                                    :cache-expires (+ drover-test-now 240) :cache-ttl 300))
    (let ((opencode-session-status-animation-interval 0.5)
          (drover-list-cache-interval 5)
          (other (generate-new-buffer "*drover sessions other*")))
      (should (equal (drover-list--buffers) (list buffer)))
      (with-current-buffer other
        (drover-list-mode))
      (should (equal (drover-list--buffers) (list buffer other)))
      ;; Of another mode, or killed, a buffer is no list.
      (with-current-buffer other
        (fundamental-mode))
      (should (equal (drover-list--buffers) (list buffer)))
      (with-current-buffer other
        (drover-list-mode))
      (kill-buffer other)
      (should (equal (drover-list--buffers) (list buffer)))
      (drover-list-test-shown
        (revert-buffer)
        (should (timerp drover-list--frame-timer))
        (should (timerp drover-list--cache-timer))
        (let* ((scans 0)
               (count (lambda (&rest _) (cl-incf scans))))
          (advice-add 'match-buffers :before count)
          (advice-add 'buffer-list :before count)
          (unwind-protect
              (progn
                (setq drover-list-test-clock (+ drover-test-now 0.5))
                (drover-list--frame-tick)
                (drover-list--cache-tick)
                (drover-list--update-timers)
                (drover-list--windows-changed))
            (advice-remove 'match-buffers count)
            (advice-remove 'buffer-list count))
          (should (zerop scans)))
        (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))))))

(ert-deftest drover-list-test-timers-follow-the-focus ()
  "Losing focus, glyphs move slower and the other timers keep their pace.
Regaining it, the frame of the time shows at once, at the full pace."
  (drover-list-test-with (list (drover-test-session "w" :status 'working
                                                    :cache-expires (+ drover-test-now 240) :cache-ttl 300))
    (let ((opencode-session-status-animation-interval 0.5)
          (drover-list-animation-unfocused-interval 2)
          (drover-list-cache-interval 5)
          (drover-list-age-interval 60)
          (focused t))
      (cl-letf (((symbol-function 'frame-initial-p) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda (&optional _frame) focused)))
        (drover-list-test-shown
          (revert-buffer)
          (should (timerp drover-list--frame-timer))
          (should (= (timer--repeat-delay drover-list--frame-timer) 0.5))
          (should (timerp drover-list--cache-timer))
          (should (timerp drover-list--age-timer))
          (let ((cache-timer drover-list--cache-timer)
                (age-timer drover-list--age-timer))
            ;; Focus moves out: looked at once the events settled.
            (setq focused nil)
            (drover-list--focus-changed)
            (should (timerp drover-list--focus-timer))
            (drover-list--refocus)
            (should-not drover-list--focus-timer)
            (should-not drover-list--focused)
            ;; The glyphs tick every 3.5 s, seven frames, the next frame
            ;; of three and of two from 2 s up, in the middle of a frame;
            ;; the caches and ages keep their timers.
            (should (timerp drover-list--frame-timer))
            (should (= (timer--repeat-delay drover-list--frame-timer) 3.5))
            (should (<= (float-time (time-subtract (timer--time drover-list--frame-timer) nil)) 0.75))
            (should (eq drover-list--cache-timer cache-timer))
            (should (eq drover-list--age-timer age-timer))
            ;; A tick shows the frame of the time, as at the full pace:
            ;; 4.5 frames of 0.5 s later, the second of three.
            (setq drover-list-test-clock (+ drover-test-now 2.25))
            (drover-list--frame-tick)
            (should (= (opencode-session-status-frame-index 3 drover-list-test-clock) 1))
            (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))
            (setq drover-list-test-clock (+ drover-test-now 5.75))
            (drover-list--frame-tick)
            (should (string-prefix-p "  3 Session w" (drover-list-test-heading "w")))
            (should (= (timer--repeat-delay drover-list--frame-timer) 3.5))
            ;; The caches keep moving.
            (setq drover-list-test-clock (+ drover-test-now 90.5))
            (drover-list--cache-tick)
            (should (equal (drover-list-test-cache-field "w") '(2 3 "2m")))
            (should (eq drover-list--cache-timer cache-timer))
            ;; A change of windows meanwhile keeps the slow pace.
            (let ((timer drover-list--frame-timer))
              (drover-list--windows-changed)
              (should (eq drover-list--frame-timer timer)))
            ;; Focus back: the frame of the time shows at once, and the
            ;; glyphs move at the full pace.
            (setq focused t
                  drover-list-test-clock (+ drover-test-now 100.75))
            (drover-list--refocus)
            (should drover-list--focused)
            (should (string-prefix-p "  1 Session w" (drover-list-test-heading "w")))
            (should (timerp drover-list--frame-timer))
            (should (= (timer--repeat-delay drover-list--frame-timer) 0.5))
            (should (<= (float-time (time-subtract (timer--time drover-list--frame-timer) nil)) 0.75))
            (should (eq drover-list--cache-timer cache-timer))
            (should (eq drover-list--age-timer age-timer))))))))

(ert-deftest drover-list-test-unfocused-pace ()
  "The pace while Emacs has no focus is a whole number of frames, never quicker.
With the moon's eight phases and the clock's four hours, nine frames:
seven, a second, showed them turning backwards."
  (let ((opencode-session-status-animation-interval 0.15)
        (opencode-session-status-animations (eval (car (get 'opencode-session-status-animations
                                                             'standard-value))
                                                  t))
        (drover-list-animation-unfocused-interval
         (eval (car (get 'opencode-session-status-animation-unfocused-interval 'standard-value)) t))
        (drover-list--focused nil))
    (should (equal (mapcar (lambda (entry) (length (cdr entry))) opencode-session-status-animations) '(8 4)))
    ;; The default, nine frames, ticks fall mid-frame.
    (should (= drover-list-animation-unfocused-interval 1.35))
    (should (< (abs (- (drover-list--frame-pace) 1.35)) 1e-9))
    ;; A second, 6.67 frames, rounds up to nine too, not to seven.
    (let ((drover-list-animation-unfocused-interval 1.0))
      (should (< (abs (- (drover-list--frame-pace) 1.35)) 1e-9)))
    ;; Past nine frames, seventeen.
    (let ((drover-list-animation-unfocused-interval 1.4))
      (should (< (abs (- (drover-list--frame-pace) 2.55)) 1e-9)))
    ;; Quicker than a frame, a frame.
    (let ((drover-list-animation-unfocused-interval 0.05))
      (should (= (drover-list--frame-pace) 0.15)))
    ;; Without animations, the frames asked, rounded up.
    (let ((opencode-session-status-animations nil)
          (drover-list-animation-unfocused-interval 1.0))
      (should (< (abs (- (drover-list--frame-pace) 1.05)) 1e-9)))
    (let ((drover-list-animation-unfocused-interval nil))
      (should-not (drover-list--frame-pace)))
    (let ((opencode-session-status-animation-interval nil))
      (should-not (drover-list--frame-pace)))
    (let ((drover-list--focused t))
      (should (= (drover-list--frame-pace) 0.15)))))

(defun drover-list-test-steps (pace counts &optional ticks)
  "Return how each animation of COUNTS frames moves at each of TICKS ticks of PACE.
The ticks fall in the middle of frames, from an instant the clock picks;
each step is the frames an animation moved at a tick, as shown by the
frame of the time, modulo its frames: 1 its next frame."
  (let ((time (+ drover-test-now (opencode-session-status-frame-delay drover-test-now)))
        (steps nil))
    (dotimes (_ (or ticks 20))
      (let ((next (+ time pace)))
        (dolist (count counts)
          (push (mod (- (opencode-session-status-frame-index count next)
                        (opencode-session-status-frame-index count time))
                     count)
                steps))
        (setq time next)))
    (delete-dups steps)))

(ert-deftest drover-list-test-unfocused-steps-forward ()
  "Without focus, each tick shows every animation its next frame.
The frame shown is the clock's: a tick of seven frames of eight showed
the moon its phase before, turning backwards."
  (let ((opencode-session-status-animation-interval 0.15)
        (drover-list--focused nil))
    (pcase-dolist (`(,counts ,seconds ,frames)
                   '(((8 4) 1.35 9) ((8 4) 1.0 9) ((8 4) 0.5 9) ((8 4) 2 17) ((8) 1.0 9)
                     ((4) 1.0 9) ((3 2) 1.0 7) ((3) 1.0 7) ((5) 1.0 11) ((6 4) 1.0 13)
                     ((8 4) 0.15 1) ((8 4) 0.1 1) ((2) 1.0 7) (nil 1.0 7)))
      (ert-info ((format "Animations of %s frames, %s s asked" counts seconds))
        (let ((opencode-session-status-animations
               (seq-map-indexed (lambda (count index)
                                  (cons (nth index '(working retrying))
                                        (mapcar #'number-to-string (number-sequence 1 count))))
                                counts))
              (drover-list-animation-unfocused-interval seconds))
          (should (< (abs (- (drover-list--frame-pace) (* frames 0.15))) 1e-9))
          (should (>= (+ (drover-list--frame-pace) 1e-9) (max seconds 0.15)))
          (when counts
            (should (equal (drover-list-test-steps (drover-list--frame-pace) counts)
                           (if (seq-every-p (lambda (count) (= count 1)) counts) '(0) '(1))))))))
    ;; Frames of five, seven and nine show their next frames together
    ;; every 315 frames, past three times a second, 20 frames.  Then the
    ;; least spanning showing their next frame to the animations of the
    ;; most frames together: from 7 to 20 frames, 10, the nine-frame
    ;; one's; from 20 to 60, 46, the nine and five-frame ones', 14
    ;; frames, rather than 36, seven and five, 12.
    (should (= (opencode-session-status-forward-frames (/ 1.0 0.15) '(5 7 9)) 10))
    (should (= (opencode-session-status-forward-frames 20 '(5 7 9)) 46))
    ;; Within reach, the least of all.
    (should (= (opencode-session-status-forward-frames 200 '(5 7 9)) 316))
    ;; Eight and three: their next frames together every 25 frames, past
    ;; three times 6.67; nine shows the moon its next phase, seven would
    ;; show the three-frame one its next, the moon its phase before.
    (should (= (opencode-session-status-forward-frames (/ 1.0 0.15) '(8 3)) 9))
    (should (= (opencode-session-status-forward-frames 9 '(8 4)) 9))
    (should (= (opencode-session-status-forward-frames 9.000000000000002 '(8 4)) 9))
    (should (= (opencode-session-status-forward-frames 0.3 '(8 4)) 1))))

(ert-deftest drover-list-test-unfocused-glyphs-step-forward ()
  "A list without focus shows each animated glyph its next frame at each tick."
  (drover-list-test-with (list (drover-test-session "w" :status 'working)
                               (drover-test-session "r" :status 'retrying))
    (let ((opencode-session-status-animations '((working "1" "2" "3" "4" "5" "6" "7" "8")
                                                (retrying "a" "b" "c" "d")))
          (opencode-session-status-animation-interval 0.15)
          (drover-list-animation-unfocused-interval 1.0)
          (drover-list-collapsed-groups nil)
          (focused nil))
      (cl-letf (((symbol-function 'frame-initial-p) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda (&optional _frame) focused)))
        (drover-list-test-shown
          (revert-buffer)
          (drover-list--refocus)
          (should-not drover-list--focused)
          (should (< (abs (- (timer--repeat-delay drover-list--frame-timer) 1.35)) 1e-9))
          (cl-flet ((glyph (id) (substring (drover-list-test-heading id) 2 3)))
            (let ((moon (glyph "w"))
                  (clock (glyph "r")))
              (dotimes (tick 12)
                (setq drover-list-test-clock (+ drover-test-now 0.075 (* (1+ tick) 1.35)))
                (drover-list--frame-tick)
                (should (equal (glyph "w") (number-to-string (1+ (mod (string-to-number moon) 8)))))
                (should (equal (glyph "r") (string (+ ?a (mod (1+ (- (aref clock 0) ?a)) 4)))))
                (setq moon (glyph "w")
                      clock (glyph "r"))))))))))

(ert-deftest drover-list-test-unfocused-animation-off ()
  "Without an unfocused pace, glyphs stand still while Emacs has no focus.
The caches and ages keep their timers; focus back, the frame of the
time shows at once."
  (drover-list-test-with (list (drover-test-session "w" :status 'working
                                                    :cache-expires (+ drover-test-now 240) :cache-ttl 300))
    (let ((opencode-session-status-animation-interval 0.5)
          (drover-list-animation-unfocused-interval nil)
          (drover-list-cache-interval 5)
          (drover-list-age-interval 60)
          (focused t))
      (cl-letf (((symbol-function 'frame-initial-p) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda (&optional _frame) focused)))
        (drover-list-test-shown
          (revert-buffer)
          (should (timerp drover-list--frame-timer))
          (setq focused nil)
          (drover-list--refocus)
          (should-not drover-list--frame-timer)
          (should (timerp drover-list--cache-timer))
          (should (timerp drover-list--age-timer))
          ;; A tick already due draws nothing, and changes nothing.
          (setq drover-list-test-clock (+ drover-test-now 0.5))
          (let ((tick (buffer-modified-tick)))
            (drover-list--frame-tick)
            (should (= tick (buffer-modified-tick))))
          (should (string-prefix-p "  1 Session w" (drover-list-test-heading "w")))
          ;; A change of windows meanwhile starts nothing.
          (drover-list--windows-changed)
          (should-not drover-list--frame-timer)
          (setq focused t
                drover-list-test-clock (+ drover-test-now 90.5))
          (drover-list--refocus)
          (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))
          (should (timerp drover-list--frame-timer))
          (should (= (timer--repeat-delay drover-list--frame-timer) 0.5)))))))

(ert-deftest drover-list-test-other-frame-focus ()
  "Another frame of Emacs having focus, the list's glyphs keep the full pace.
Emacs may tell the list's frame lost focus before telling the other has
it: the focus is looked at once those events settled."
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (let ((opencode-session-status-animation-interval 0.5)
          (drover-list-animation-unfocused-interval 2)
          (focused (selected-frame)))
      ;; Frames listed last: redefining a primitive may compile its
      ;; trampoline, which lists frames.
      (cl-letf (((symbol-function 'frame-initial-p) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda (&optional frame) (eq frame focused))))
        (cl-letf (((symbol-function 'frame-list) (lambda () (list (selected-frame) 'other))))
          (drover-list-test-shown
            (revert-buffer)
            (let ((timer drover-list--frame-timer))
              (should (timerp timer))
              ;; The list's frame loses focus, then the other frame has it.
              (setq focused nil)
              (drover-list--focus-changed)
              (setq focused 'other)
              (drover-list--focus-changed)
              (drover-list--refocus)
              (should drover-list--focused)
              (should (eq drover-list--frame-timer timer))
              (should (= (timer--repeat-delay drover-list--frame-timer) 0.5))
              ;; Then Emacs loses focus: the slow pace.
              (setq focused nil)
              (drover-list--refocus)
              (should-not drover-list--focused)
              (should (= (timer--repeat-delay drover-list--frame-timer) 3.5)))))))))

(ert-deftest drover-list-test-unfocused-hidden-frames ()
  "A list on an iconified frame animates nothing, Emacs having focus or not."
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (let ((opencode-session-status-animation-interval 0.5)
          (drover-list-animation-unfocused-interval 2)
          (focused nil))
      (cl-letf (((symbol-function 'frame-initial-p) #'ignore)
                ((symbol-function 'frame-focus-state) (lambda (&optional _frame) focused)))
        (drover-list-test-shown
          (revert-buffer)
          (drover-list--refocus)
          (should-not drover-list--focused)
          (should (= (timer--repeat-delay drover-list--frame-timer) 3.5))
          (cl-letf (((symbol-function 'frame-visible-p) (lambda (_frame) 'icon)))
            (setq drover-list-test-clock (+ drover-test-now 2.25))
            (let ((tick (buffer-modified-tick)))
              (drover-list--frame-tick)
              (should (= tick (buffer-modified-tick))))
            (should-not drover-list--frame-timer)
            (setq focused t)
            (drover-list--refocus)
            (should-not drover-list--frame-timer)
            (should (string-prefix-p "  1 Session w" (drover-list-test-heading "w")))))))))

(ert-deftest drover-list-test-timers-stop-on-hidden-frames ()
  "A list on an iconified frame animates nothing; shown again, it does."
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (let ((opencode-session-status-animation-interval 0.5))
      (drover-list-test-shown
        (revert-buffer)
        (should (timerp drover-list--frame-timer))
        (cl-letf (((symbol-function 'frame-visible-p) (lambda (_frame) 'icon)))
          (setq drover-list-test-clock (+ drover-test-now 0.5))
          (let ((tick (buffer-modified-tick)))
            (drover-list--frame-tick)
            (should (= tick (buffer-modified-tick))))
          (should-not drover-list--frame-timer)
          (should (string-prefix-p "  1 Session w" (drover-list-test-heading "w"))))
        ;; Visible again, as windows change, it runs again.
        (drover-list--windows-changed)
        (should (timerp drover-list--frame-timer))
        (drover-list--frame-tick)
        (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))))))

(ert-deftest drover-list-test-glyphs-out-of-view ()
  "Glyphs and caches no window shows are left as they are, and so is the buffer."
  (drover-list-test-with (list (drover-test-session "w" :status 'working
                                                    :cache-expires (+ drover-test-now 240) :cache-ttl 300))
    (let ((opencode-session-status-animation-interval 0.5))
      (revert-buffer)
      (let ((elsewhere (list (cons (1+ (point-max)) (+ 2 (point-max)))))
            (here (list (cons (point-min) (point-max))))
            (tick (buffer-modified-tick)))
        (setq drover-list-test-clock (+ drover-test-now 90.5))
        (drover-list--draw-frames nil elsewhere)
        (drover-list--draw-caches nil elsewhere)
        (should (= tick (buffer-modified-tick)))
        (should (string-prefix-p "  1 Session w" (drover-list-test-heading "w")))
        (should (equal (drover-list-test-cache-field "w") '(4 1 "4m")))
        (drover-list--draw-frames nil here)
        (drover-list--draw-caches nil here)
        (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))
        (should (equal (drover-list-test-cache-field "w") '(2 3 "2m")))
        ;; Drawn again as they stand, nothing changes.
        (setq tick (buffer-modified-tick))
        (drover-list--draw-frames)
        (drover-list--draw-caches)
        (should (= tick (buffer-modified-tick)))
        ;; A cache gone out is forgotten wherever it is.
        (setq drover-list-test-clock (+ drover-test-now 300))
        (drover-list--draw-caches nil elsewhere)
        (should-not drover-list--caches)))))

(ert-deftest drover-list-test-cache-line-drawn-once ()
  "In two lines, a cache line drawn again as it stands changes nothing."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'working :updated (drover-list-test-ago 60)
                                 :detail "Busy" :cache-expires (+ drover-test-now 300) :cache-ttl 300))
    (drover-list-test-with-font
      (let ((drover-list-test-width 300))
        (revert-buffer)
        (should (eq drover-list--layout 'narrow))
        (let ((overlay (plist-get (car drover-list--caches) :overlay)))
          (setq drover-list-test-clock (+ drover-test-now 30))
          (drover-list--draw-caches)
          (let ((tick (buffer-modified-tick))
                (end (overlay-end overlay)))
            (drover-list--draw-caches)
            (should (= tick (buffer-modified-tick)))
            (should (= end (overlay-end overlay))))
          (should (equal-including-properties (car (drover-list-test-drawn))
                                              (car (drover-list-test-fresh)))))))))

;;;; A prompt cache across turns

(defun drover-list-test-turn-sessions ()
  "Return a session at work with an hour's prompt cache, 50 minutes left, and two idle ones.
Their ages read as wide all along, and so do the summaries of the turn
of the second, so that the columns keep their widths."
  (list (drover-list-test-titled "p" "Parent" :status 'working :since (drover-list-test-ago 600)
                                 :updated (drover-list-test-ago 5) :detail "Step 0"
                                 :cache-expires (+ drover-test-now 3000) :cache-ttl 3600)
        (drover-list-test-titled "b" "Bravo" :status 'idle :updated (drover-list-test-ago 1800))
        (drover-list-test-titled "c" "Charlie" :status 'idle :updated (drover-list-test-ago 3000))))

(defun drover-list-test-change (id function)
  "Change the session ID with FUNCTION, a function of its record, and report it.
Return how many times the lists were drawn anew."
  (funcall function (drover-list-test-find id))
  (drover-backend-changed 'fake id)
  (drover-list-test-counting-redraws (drover-changed-now)))

(defun drover-list-test-cache-shown (id)
  "Return how the current list shows the prompt cache of session ID, or nil for none.
In columns, the field of its line, as `drover-list-test-cache-field'
returns it; in two lines, the text its underline spans, in its
properties, the widths of its spaces included, which must begin where
the session's line does."
  (when-let* ((entry (seq-find (lambda (entry) (equal (drover-session-id (plist-get entry :session)) id))
                               drover-list--caches)))
    (if (eq (plist-get entry :kind) 'columns)
        (drover-list-test-cache-field id)
      (let ((overlay (plist-get entry :overlay))
            (start (save-excursion
                     (drover-list-test-goto id)
                     (+ (point) (length (drover-list--line-prefix
                                         (drover-list--model-layout drover-list--model)))))))
        (should (eq (overlay-buffer overlay) (current-buffer)))
        (should (= (overlay-start overlay) start))
        (buffer-substring start (overlay-end overlay))))))

(defun drover-list-test-cache-across-turns (layout)
  "Check that the current list shows the cache of a session gone idle until it expires.
The list, in LAYOUT, is shown, its sessions those of
`drover-list-test-turn-sessions', newest turn first: the session's line
goes on showing its cache once idle, as another session's turn begins
before it and goes on, and as its cache runs out, and none once out.
Each change is drawn in place, as a drawing anew draws it."
  (should (eq drover-list--layout layout))
  (let ((narrow (eq layout 'narrow)))
    ;; At work.
    (should (equal (drover-list-test-order) '("p" "b" "c")))
    (let ((shown (drover-list-test-cache-shown "p")))
      (should (if narrow (string-match-p "Parent" shown) (equal (nth 2 shown) "50m"))))
    ;; Idle, its cache live: it goes on showing.
    (drover-list-test-change "p" (lambda (session)
                                   (setf (drover-session-status session) 'idle
                                         (drover-session-since session) nil
                                         (drover-session-updated session) drover-test-now)))
    (drover-list-test-is-fresh)
    (should (equal (drover-list-test-order) '("p" "b" "c")))
    (let ((shown (drover-list-test-cache-shown "p")))
      (should (if narrow (string-match-p "Parent" shown) (equal (nth 2 shown) "50m"))))
    ;; Another session's turn: its line comes before, in place.
    (should (zerop (drover-list-test-change "b" (lambda (session)
                                                  (setf (drover-session-status session) 'working
                                                        (drover-session-since session) drover-test-now
                                                        (drover-session-updated session) drover-test-now
                                                        (drover-session-detail session) "Step 0")))))
    (drover-list-test-is-fresh)
    (should (equal (drover-list-test-order) '("b" "p" "c")))
    (let ((shown (drover-list-test-cache-shown "p")))
      (should (if narrow (string-match-p "Parent" shown) (equal (nth 2 shown) "50m"))))
    (should-not (drover-list-test-cache-shown "b"))
    ;; Its line drawn again and again as its turn goes on.
    (dolist (step '(1 2 3))
      (should (zerop (drover-list-test-change "b" (lambda (session)
                                                    (setf (drover-session-detail session)
                                                          (format "Step %s" step)))))))
    (drover-list-test-is-fresh)
    (let ((shown (drover-list-test-cache-shown "p")))
      (should (if narrow (string-match-p "Parent" shown) (equal (nth 2 shown) "50m"))))
    ;; The cache running out: 30 minutes left, then 10.
    (let ((before (drover-list-test-cache-shown "p")))
      (dolist (left '("30m" "10m"))
        (setq drover-list-test-clock (+ drover-test-now (- 3000 (* 60 (string-to-number left)))))
        (drover-list--draw-caches)
        (let ((shown (drover-list-test-cache-shown "p")))
          (if narrow
              ;; Shorter: its spaces narrower, or its end sooner.
              (progn (should-not (equal-including-properties shown before))
                     (should (string-match-p (concat "expires in " left)
                                             (save-excursion
                                               (drover-list-test-goto "p")
                                               (drover-list-test-help-at (point))))))
            (should (equal (nth 2 shown) left)))
          (setq before shown))))
    ;; Out: none.
    (setq drover-list-test-clock (+ drover-test-now 3000))
    (drover-list--draw-caches)
    (should-not (drover-list-test-cache-shown "p"))
    (should-not drover-list--caches)
    (unless narrow
      (should (equal (drover-list-test-cache-field "p") '(0 0 nil))))))

(defmacro drover-list-test-with-turns (&rest body)
  "Run BODY in a list of `drover-list-test-turn-sessions', ungrouped, newest turn first."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-turn-sessions)
     (cl-flet ((standard (symbol) (eval (car (get symbol 'standard-value)) t)))
       (let ((drover-group-by nil)
             (drover-sort-by (standard 'drover-sort-by))
             (drover-sort-descending (standard 'drover-sort-descending)))
         ,@body))))

(ert-deftest drover-list-test-cache-line-across-turns ()
  "In the sidebar's two lines, a session gone idle keeps its cache line until it expires.
Another session's line drawn before its own, as that session's turn
begins, left the marks of its cache on that line, which the next
drawing of that line took away: the line of the idle session lost its
cache, whose expiry was an hour away."
  (drover-list-test-with-turns
    (kill-buffer buffer)
    (with-current-buffer (window-buffer (drover-list-side))
      (drover-list-test-cache-across-turns 'narrow))))

(ert-deftest drover-list-test-cache-column-across-turns ()
  "In a sidebar wide enough for columns, a session gone idle keeps its cache field until it expires."
  ;; The frame's size back once the sidebar is gone: shrunk under its
  ;; side window, the frame would leave its other window that wide.
  (let ((size (cons (frame-width) (frame-height))))
    (unwind-protect
        (drover-list-test-with-turns
          (let ((drover-list-side-window-width 150))
            (set-frame-size (selected-frame) 200 60)
            (kill-buffer buffer)
            (with-current-buffer (window-buffer (drover-list-side))
              (drover-list-test-cache-across-turns 'columns))))
      (set-frame-size (selected-frame) (car size) (cdr size)))))

(ert-deftest drover-list-test-cache-across-turns-in-the-list ()
  "In the list in an ordinary window, a session gone idle keeps its cache field until it expires."
  (drover-list-test-with-turns
    (drover-list-test-shown
      (revert-buffer)
      (drover-list-test-cache-across-turns 'columns))))

(ert-deftest drover-list-test-cache-line-not-kept-by-mode-again ()
  "Applying the sidebar's mode again leaves no overlay of a cache drawn before.
The mode forgot the caches drawn, not their overlays: each application
left one per cache, empty at the start of the buffer once erased."
  (drover-list-test-with-turns
    (kill-buffer buffer)
    (with-current-buffer (window-buffer (drover-list-side))
      (cl-flet ((overlays ()
                  (seq-filter (lambda (overlay) (eql (overlay-get overlay 'priority) -50))
                              (overlays-in (point-min) (point-max)))))
        (should (equal (overlays) (list (plist-get (car drover-list--caches) :overlay))))
        (dotimes (_ 2)
          (drover-list-sidebar-mode)
          (revert-buffer)
          (should (equal (overlays) (list (plist-get (car drover-list--caches) :overlay)))))))))

(ert-deftest drover-list-test-glyph-keeps-animating-under-a-line-drawn-before ()
  "In the sidebar, a glyph beginning its line goes on animating as a line is drawn before it.
Its mark stayed where the line drawn before begins, at that line's own
glyph, which its next drawing took away with it: the glyph stood still."
  (drover-list-test-with (list (drover-list-test-titled "p" "Parent" :status 'working
                                                        :since (drover-list-test-ago 600)
                                                        :updated (drover-list-test-ago 5))
                               (drover-list-test-titled "b" "Bravo" :status 'idle
                                                        :updated (drover-list-test-ago 1800)))
    (let ((drover-group-by nil)
          (opencode-session-status-animation-interval 1)
          (opencode-session-status-animations '((working "1" "2" "3"))))
      (kill-buffer buffer)
      (with-current-buffer (window-buffer (drover-list-side))
        (cl-flet ((glyphs ()
                    (sort (mapcar (lambda (entry)
                                    (should (eq (get-text-property (car entry) 'drover-list-frame) 'working))
                                    (marker-position (car entry)))
                                  drover-list--animated)
                          #'<))
                  (glyph-of (id)
                    (save-excursion
                      (drover-list-test-goto id)
                      (text-property-not-all (point) (line-end-position) 'drover-list-frame nil))))
          (should (equal (glyphs) (list (glyph-of "p"))))
          (should (zerop (drover-list-test-change "b" (lambda (session)
                                                        (setf (drover-session-status session) 'working
                                                              (drover-session-since session) drover-test-now
                                                              (drover-session-updated session) drover-test-now)))))
          (should (equal (drover-list-test-order) '("b" "p")))
          (drover-list-test-is-fresh)
          (should (equal (glyphs) (list (glyph-of "b") (glyph-of "p"))))
          (should (zerop (drover-list-test-change "b" (lambda (session)
                                                        (setf (drover-session-detail session) "Step")))))
          (should (equal (glyphs) (list (glyph-of "b") (glyph-of "p"))))
          (let ((frames nil))
            (dolist (step '(1 2 3))
              (setq drover-list-test-clock (+ drover-test-now step))
              (drover-list--draw-frames)
              (let ((frame (nth (opencode-session-status-frame-index 3 drover-list-test-clock) '("1" "2" "3"))))
                (push frame frames)
                (dolist (id '("b" "p"))
                  (should (equal (get-text-property (glyph-of id) 'display) frame)))))
            ;; Each a frame of its own: the glyphs moved.
            (should (= (length (delete-dups frames)) 3))))))))

(ert-deftest drover-list-test-reordering-only-right-to-left ()
  "A list's lines are reordered for display only while it holds right-to-left text."
  (drover-list-test-with (list (drover-list-test-titled "a" "Plain title" :status 'working))
    (drover-list--reorder)
    (should-not bidi-display-reordering)
    (setf (drover-session-title (drover-list-test-find "a")) "שלום עולם")
    (drover-list-test-refresh)
    (drover-list--reorder)
    (should bidi-display-reordering)
    ;; The display of characters changing, it is not looked at again.
    (setq bidi-display-reordering nil)
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (put-text-property (point-min) (1+ (point-min)) 'display "x")))
    (drover-list--reorder)
    (should-not bidi-display-reordering)
    (setf (drover-session-title (drover-list-test-find "a")) "Back to \u202eplain")
    (drover-list-test-refresh)
    (drover-list--reorder)
    (should bidi-display-reordering)
    (setf (drover-session-title (drover-list-test-find "a")) "Plain again")
    (drover-list-test-refresh)
    (drover-list--reorder)
    (should-not bidi-display-reordering)))

;;;; The menu on a list shown elsewhere

(defun drover-list-test-code-buffer (&optional name)
  "Return a buffer telling no session, NAME or \"*other code*\", C-' opening the menu."
  (with-current-buffer (get-buffer-create (or name "*other code*"))
    (use-local-map (let ((map (make-sparse-keymap)))
                     (keymap-set map "C-'" #'drover-dispatch)
                     map))
    (current-buffer)))

(defmacro drover-list-test-elsewhere (&rest body)
  "Run BODY in a code buffer, the sidebar in a side window beside it.
The side sample is served, sessions open in buffers; `first' is the
window of the code buffer, selected, `second' another one, `side' the
sidebar's, its point on \"ask\", `buffer' the sidebar, `code' the code
buffer."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-side-sample)
     (drover-list-test--register-buffers)
     (drover-list-test-showing
       (pcase-let ((`(,first ,second) (drover-list-test--main)))
         (let* ((side (drover-list--show-side))
                (buffer (window-buffer side))
                (code (drover-list-test-code-buffer)))
           (ignore first second side code buffer)
           (with-selected-window side
             (drover-list-test-goto "ask"))
           (set-window-buffer first code)
           (select-window first)
           (set-buffer code)
           (unwind-protect
               (progn ,@body)
             (when (bound-and-true-p transient--prefix)
               (execute-kbd-macro (kbd "C-g")))))))))

(defun drover-list-test-at-side (side)
  "Return the identifier of the session at the point of the list window SIDE."
  (with-selected-window side
    (drover-list-test-id-at-point)))

(defun drover-list-test-menu-session ()
  "Return the identifier of the session the open dispatch menu acts on."
  (when-let* ((session (plist-get (oref transient--prefix scope) :session)))
    (drover-session-id session)))

(ert-deftest drover-list-test-dispatch-context-elsewhere ()
  "From a buffer telling no session, the menu acts on a list shown beside it."
  (drover-list-test-elsewhere
    (let ((context (drover--context)))
      (should (eq (plist-get context :window) side))
      (should (equal (drover-session-id (plist-get context :session)) "ask"))
      (should (plist-get context :list))
      (should (plist-get context :view))
      (should-not (plist-get context :prompt)))
    ;; The session at the window's point, wherever the buffer's is.
    (with-current-buffer buffer
      (goto-char (point-min)))
    (should (equal (drover-session-id (plist-get (drover--context) :session)) "ask"))
    ;; The list itself acts on itself.
    (with-selected-window side
      (should-not (plist-get (drover--context) :window)))
    ;; Without a list shown, on nothing.
    (delete-window side)
    (should-not (plist-get (drover--context) :list))
    (should-not (plist-get (drover--context) :session))))

(ert-deftest drover-list-test-dispatch-session-keys-elsewhere ()
  "Typed from a code buffer, session keys act on the list's session at point."
  (drover-list-test-elsewhere
    (progn
      (execute-kbd-macro (kbd "C-' A"))
      (should (equal (car drover-test-calls) '(:respond "ask")))
      ;; The user stays where they work.
      (should (eq (selected-window) first))
      (should (eq (window-buffer first) code))
      ;; o opens in another window than the one worked in, and goes there.
      (execute-kbd-macro (kbd "C-' o"))
      (should (equal (car drover-test-calls) (list :open "ask" nil second)))
      (should (equal (buffer-name (window-buffer second)) "*session ask*"))
      (should (eq (window-buffer first) code))
      (should (eq (selected-window) second)))))

(ert-deftest drover-list-test-dispatch-own-session-wins ()
  "A buffer telling its own session acts on it, not on the list beside it."
  (drover-list-test-elsewhere
    (with-current-buffer code
      (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "ask"))))
    ;; The list's session at point asks nothing: A answers the buffer's.
    (with-selected-window side
      (drover-list-test-goto "fail"))
    (should-not (plist-get (drover--context) :window))
    (execute-kbd-macro (kbd "C-' A"))
    (should (equal (car drover-test-calls) '(:respond "ask")))
    ;; The List group opens the list's sessions by number alone: C-n is
    ;; not the menu's.
    (execute-kbd-macro (kbd "C-'"))
    (should-not (seq-find (lambda (suffix) (and (eq (oref suffix command) 'drover-list-next-session)
                                                (not (oref suffix inactive))))
                          transient--suffixes))
    (execute-kbd-macro (kbd "C-g"))
    (should (equal (drover-list-test-at-side side) "fail"))))

(ert-deftest drover-list-test-dispatch-closed-by-its-keys ()
  "Any key opening the menu closes it, the one that opened it or another."
  (drover-list-test-elsewhere
    (keymap-set (current-local-map) "<f19>" #'drover-dispatch)
    (unwind-protect
        (dolist (keys '(("C-'" "C-'") ("<f19>" "<f19>") ("C-'" "<f19>") ("<f19>" "C-'")))
          (execute-kbd-macro (kbd (car keys)))
          (should (eq (oref transient--prefix command) 'drover-dispatch))
          (execute-kbd-macro (kbd (cadr keys)))
          (should-not transient--prefix)
          (should (eq (selected-window) first)))
      (keymap-unset (current-local-map) "<f19>" t))))

(ert-deftest drover-list-test-menus-closed-by-q ()
  "q closes the menus opened over the list, as C-g does, and the list stays.
In the list, q quits it: with a menu open, q is the menu's."
  (drover-list-test-elsewhere
    (let ((global (current-global-map))
          (map (make-sparse-keymap)))
      ;; Bound globally, as users bind it, so that it opens in the list.
      (keymap-set map "C-'" #'drover-dispatch)
      (set-keymap-parent map global)
      (use-global-map map)
      (unwind-protect
          (progn
            (select-window side)
            (dolist (keys '("C-'" "V"))
              (ert-info (keys)
                (execute-kbd-macro (kbd keys))
                (should transient--prefix)
                (execute-kbd-macro (kbd "q"))
                (should-not transient--prefix)
                (should (window-live-p side))
                (should (eq (window-buffer side) buffer))
                (should (eq (selected-window) side))
                (should (equal (drover-list-test-at-side side) "ask"))))
            ;; From the code buffer too.
            (select-window first)
            (execute-kbd-macro (kbd "C-'"))
            (should transient--prefix)
            (execute-kbd-macro (kbd "q"))
            (should-not transient--prefix)
            (should (eq (selected-window) first))
            (should (eq (window-buffer side) buffer)))
        (use-global-map global)))))

(ert-deftest drover-list-test-dispatch-view-elsewhere ()
  "The dispatch menu opened from a code buffer sets the list's follow mode."
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-' - f"))
    (should (eq (oref transient--prefix command) 'drover-dispatch))
    (should (buffer-local-value 'drover-follow-mode buffer))
    (should-not (buffer-local-value 'drover-follow-mode code))
    ;; Following shows the session at the list's point in the main window.
    (should (equal (car drover-test-calls) (list :open "ask" nil first)))
    (execute-kbd-macro (kbd "- f"))
    (should-not (buffer-local-value 'drover-follow-mode buffer))
    (execute-kbd-macro (kbd "C-g"))
    (should-not transient--prefix)
    ;; The user is back where they work.
    (should (eq (selected-window) first))))

(ert-deftest drover-list-test-dispatch-view-beside-session ()
  "From a session buffer, the view settings change the list shown beside it.
The session keys keep to the buffer's own session, and the follow mode
set is that of the list in sight."
  (drover-list-test-elsewhere
    (with-current-buffer code
      (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "fail"))))
    (let ((scope (drover--context)))
      (should-not (plist-get scope :window))
      (should-not (plist-get scope :view))
      (should (plist-get scope :shown)))
    (execute-kbd-macro (kbd "C-'"))
    (cl-flet ((apt (key) (not (oref (seq-find (lambda (suffix) (equal (oref suffix key) key))
                                              transient--suffixes)
                                    inapt))))
      (dolist (key '("=u" "=o" "=r" "=g" "=p" "=c" "-f" "-p"))
        (should (apt key)))
      ;; Peeking needs a list: it would peek at the buffer's own session.
      (should-not (apt "v")))
    ;; The menu acts on the session buffer: the List group opens the
    ;; list's sessions by number alone, moving in it is not the menu's.
    (should-not (seq-find (lambda (suffix) (and (eq (oref suffix command) 'drover-list-next-session)
                                                (not (oref suffix inactive))))
                          transient--suffixes))
    (should (seq-find (lambda (suffix) (and (eq (oref suffix command) 'drover-list-open-numbered)
                                            (not (oref suffix inactive))))
                      transient--suffixes))
    (execute-kbd-macro (kbd "- f"))
    (should (buffer-local-value 'drover-follow-mode buffer))
    (execute-kbd-macro (kbd "- f"))
    (should-not (buffer-local-value 'drover-follow-mode buffer))
    (execute-kbd-macro (kbd "= u"))
    (should (eq (default-value 'drover-show-subagents) 'none))
    (should (eq (oref transient--prefix command) 'drover-dispatch))
    (execute-kbd-macro (kbd "C-g"))
    (should (eq (selected-window) first))
    ;; Hidden, the list leaves the view settings to nothing, and the
    ;; menu leaves them out.  Following showed its session where the
    ;; user works: back to the code.
    (delete-window side)
    (set-window-buffer first code)
    (set-buffer code)
    (execute-kbd-macro (kbd "C-'"))
    (dolist (key '("=o" "=p" "-p" "-b" "=u"))
      (should-not (seq-find (lambda (suffix) (equal (oref suffix key) key)) transient--suffixes)))
    (execute-kbd-macro (kbd "C-g"))))

(defun drover-list-test-menu-text ()
  "Return the text of the open menu, without properties."
  (with-current-buffer " *transient*"
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest drover-list-test-dispatch-fills-width ()
  "The menu lays its groups out in rows as wide as its window, in few lines.
The old menu took 24 lines at any width, its View menu apart; before
the settings of a field shared a row, 14 at 240 columns, 18 at 160, 28
at 100 and 44 at 60.  The column of the changes and the digits opening
sessions by number took one more each at 60."
  (drover-list-test-elsewhere
    ;; The table of fields shows in two halves side by side, or one under
    ;; the other, as room asks; the view settings show, the list being in
    ;; sight, the prompt keys editing a prompt buffer do not.
    (pcase-dolist (`(,width . ,bound) '((240 . 10) (160 . 16) (100 . 26) (60 . 41)))
      (cl-letf (((symbol-function 'drover--menu-width) (lambda () width)))
        (execute-kbd-macro (kbd "C-'"))
        (let ((lines (split-string (string-trim-right (drover-list-test-menu-text)) "\n")))
          (should (<= (length lines) bound))
          ;; Narrow, a column wider than the window takes a row alone.
          (should (<= (apply #'max (mapcar #'string-width lines)) (max width 40)))
          (should-not (seq-find (lambda (line) (string-match-p "Global" line)) lines))
          ;; Titles start their rows.
          (should (string-prefix-p "Sessions" (car lines))))
        (execute-kbd-macro (kbd "C-g"))))
    ;; The window's width, before the menu shows, beside the sidebar.
    (should (= (drover--menu-width) (max 40 (- (window-total-width (window-main-window)) 2))))))

(ert-deftest drover-list-test-dispatch-layout-stays ()
  "While the menu stays open, its groups keep their place as long as they fit."
  (drover-list-test-elsewhere
    (let ((width 400))
      (cl-letf (((symbol-function 'drover--menu-width) (lambda () width)))
        (execute-kbd-macro (kbd "C-'"))
        (let* ((count (length (flatten-tree (oref transient--prefix plan))))
               ;; Every group in a column of its own, on one row.
               (wide (list (make-list count (list 0)))))
          (oset transient--prefix plan wide)
          (execute-kbd-macro (kbd "= u"))
          (should (equal (oref transient--prefix plan) wide))
          ;; As high as the highest group: a title and nine entries.
          (should (= (length (split-string (string-trim-right (drover-list-test-menu-text)) "\n")) 10))
          ;; Narrower than it, the menu is laid out anew.
          (setq width 160)
          (execute-kbd-macro (kbd "= u"))
          (should-not (equal (oref transient--prefix plan) wide))
          (should (<= (apply #'max (mapcar #'string-width (split-string (drover-list-test-menu-text) "\n")))
                      160)))
        (execute-kbd-macro (kbd "C-g"))))))

(ert-deftest drover-list-test-dispatch-values-update ()
  "The menu shows each view setting's value, and the new one once changed."
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-'"))
    (should (string-search "=g group status" (drover-list-test-menu-text)))
    (should (string-match-p "subagents +=u \\[all|active|none\\]" (drover-list-test-menu-text)))
    (cl-flet ((face-of (text)
                (with-current-buffer " *transient*"
                  (goto-char (point-min))
                  (search-forward text)
                  (get-text-property (match-beginning 0) 'face)))
              (value-face (key)
                (with-current-buffer " *transient*"
                  (goto-char (point-min))
                  (search-forward (concat key " "))
                  (get-text-property (point) 'face))))
      (should (eq (face-of "active") 'transient-value))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "nothing")))
        (execute-kbd-macro (kbd "= g")))
      (should (eq (oref transient--prefix command) 'drover-dispatch))
      (should-not drover-group-by)
      (should (string-search "=g group nothing" (drover-list-test-menu-text)))
      (execute-kbd-macro (kbd "= u"))
      (should (eq (face-of "none") 'transient-value))
      (should (eq (face-of "active") 'transient-inactive-value))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "redirect")))
        (execute-kbd-macro (kbd "= f")))
      ;; In the row of the text, the key and value alone.
      (should (string-match-p "text +=f \"redirect\"" (drover-list-test-menu-text)))
      (execute-kbd-macro (kbd "= c"))
      (should (string-match-p "text +=f any" (drover-list-test-menu-text)))
      (should (string-match-p "project +=p any +-p shown" (drover-list-test-menu-text)))
      (should (eq (value-face "-p") 'transient-value))
      (execute-kbd-macro (kbd "- p"))
      (should (string-match-p "project +=p any +-p hidden" (drover-list-test-menu-text)))
      (should (eq (value-face "-p") 'transient-inactive-value))
      (should (equal drover-list-hidden-fields '(location))))
    (execute-kbd-macro (kbd "C-g"))))

(ert-deftest drover-list-test-dispatch-in-prompt-buffer ()
  "In a buffer sending prompts, RET sends, and o acts on the list."
  (drover-list-test-elsewhere
    (let ((sent 0))
      (with-current-buffer code
        (drover-prompt-define 'send (lambda () (interactive) (cl-incf sent))))
      ;; One RET in the menu, Send's, though the List group shows.
      (execute-kbd-macro (kbd "C-'"))
      (should (equal (mapcar (lambda (suffix) (oref suffix command))
                             (seq-filter (lambda (suffix) (and (equal (oref suffix key) "RET")
                                                               (not (oref suffix inactive))))
                                         transient--suffixes))
                     '(drover-prompt-send)))
      (should (string-search "List: Session ask" (with-current-buffer " *transient*" (buffer-string))))
      (execute-kbd-macro (kbd "RET"))
      (should (= sent 1))
      (should-not (eq (car (car drover-test-calls)) :open))
      (execute-kbd-macro (kbd "C-' o"))
      (should (equal (car drover-test-calls) (list :open "ask" nil second))))))

;;;; Moving in the list from the menu

(ert-deftest drover-list-test-dispatch-moves-list-elsewhere ()
  "The List group moves the point of the list's window, the menu staying open."
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-'"))
    (should (equal (drover-list-test-menu-session) "ask"))
    (should (string-search "List: Session ask" (with-current-buffer " *transient*" (buffer-string))))
    (execute-kbd-macro (kbd "C-n"))
    (should (eq (oref transient--prefix command) 'drover-dispatch))
    (should (equal (drover-list-test-at-side side) "fail"))
    (should (eq (selected-window) first))
    ;; The menu follows the new session at point.
    (should (equal (drover-list-test-menu-session) "fail"))
    (should (string-search "List: Session fail" (with-current-buffer " *transient*" (buffer-string))))
    (cl-flet ((inapt (key) (oref (seq-find (lambda (suffix) (equal (oref suffix key) key))
                                           transient--suffixes)
                                 inapt)))
      ;; "fail" asks nothing; "ask" did.
      (should (inapt "A"))
      (execute-kbd-macro (kbd "C-p"))
      (should (equal (drover-list-test-at-side side) "ask"))
      (should-not (inapt "A")))
    ;; Groups, first and last.
    (execute-kbd-macro (kbd "M-n"))
    (should (equal (drover-list-test-at-side side) "fail"))
    (execute-kbd-macro (kbd "M-n M-n"))
    (should (equal (drover-list-test-at-side side) "work"))
    (execute-kbd-macro (kbd "M-p"))
    (should (equal (drover-list-test-at-side side) "done"))
    (execute-kbd-macro (kbd "M->"))
    ;; The sidebar collapses no group: idle is the last session.
    (should (equal (drover-list-test-at-side side) "idle"))
    (execute-kbd-macro (kbd "M-<"))
    (should (equal (drover-list-test-at-side side) "ask"))
    (execute-kbd-macro (kbd "C-n C-n"))
    (should (equal (drover-list-test-at-side side) "done"))
    ;; The list highlights the section at its point, as Magit does.
    (with-current-buffer buffer
      (should (equal (mapcar #'overlay-start magit-section-highlight-overlays)
                     (list (marker-position (oref (drover-list-test-section "done") start))))))
    ;; RET opens it, in the main window, and closes the menu.
    (execute-kbd-macro (kbd "RET"))
    (should-not transient--prefix)
    (should (equal (car drover-test-calls) (list :open "done" nil first)))
    (should (eq (selected-window) first))
    (should (equal (buffer-name (window-buffer first)) "*session done*"))))

(ert-deftest drover-list-test-dispatch-moves-with-follow ()
  "Moving from the menu shows each session, as moving in the list would."
  (drover-list-test-elsewhere
    ;; Session buffers that tell their session, as an OpenCode buffer does.
    (dolist (session drover-test-sessions)
      (with-current-buffer (get-buffer-create (drover-list-test--session-buffer-name session))
        (let ((id (drover-session-id session)))
          (setq-local drover-session-at-point-function (lambda () (drover-find 'fake id))))))
    (with-selected-window side
      (drover-follow-mode))
    (should (equal (car drover-test-calls) (list :open "ask" nil first)))
    (set-window-buffer first code)
    (execute-kbd-macro (kbd "C-' C-n"))
    (should (equal (car drover-test-calls) (list :open "fail" nil first)))
    (should (equal (buffer-name (window-buffer first)) "*session fail*"))
    ;; The menu still acts on the list, whatever the main window shows.
    (should (equal (drover-list-test-menu-session) "fail"))
    ;; Though the window where the menu opened now shows a session.
    (execute-kbd-macro (kbd "C-n"))
    (should (equal (drover-list-test-at-side side) "done"))
    (should (equal (car drover-test-calls) (list :open "done" nil first)))
    (should (equal (drover-list-test-menu-session) "done"))))

(ert-deftest drover-list-test-move-between-sessions ()
  "Sessions and groups are stepped through in order, stopping at both ends."
  (drover-list-test-with (drover-list-test-side-sample)
    (cl-flet ((at () (or (drover-list-test-id-at-point)
                         (cdr (oref (drover-list--group-section (magit-current-section)) value)))))
      ;; From the header, the first session either way.
      (goto-char (point-min))
      (drover-list-next-session)
      (should (equal (at) "ask"))
      (goto-char (point-min))
      (drover-list-previous-session)
      (should (equal (at) "ask"))
      (drover-list-next-session 2)
      (should (equal (at) "done"))
      ;; From within a session's details, the next one.
      (magit-section-show (magit-current-section))
      (forward-line 2)
      (should (equal (at) "done"))
      (drover-list-next-session)
      (should (equal (at) "work"))
      ;; The collapsed idle group is passed over: work is the last.
      (drover-list-next-session)
      (should (equal (at) "work"))
      (drover-list-previous-session 9)
      (should (equal (at) "ask"))
      (drover-list-last-session)
      (should (equal (at) "work"))
      (drover-list-first-session)
      (should (equal (at) "ask"))
      ;; Groups: their first session, the heading of a collapsed one.
      (drover-list-next-group 3)
      (should (equal (at) "work"))
      (drover-list-next-group)
      (should (equal (at) 'idle))
      (should (oref (magit-current-section) hidden))
      (drover-list-next-group)
      (should (equal (at) 'idle))
      (drover-list-previous-group)
      (should (equal (at) "work"))
      (drover-list-previous-group 9)
      (should (equal (at) "ask"))
      ;; Without groups, nothing to move between.
      (setq drover-group-by nil)
      (revert-buffer)
      (should-error (drover-list-next-group) :type 'user-error)
      (with-temp-buffer
        (should-error (drover-list-next-session) :type 'user-error)))))

(ert-deftest drover-list-test-dispatch-moves-in-list ()
  "In the list itself, the List group moves its point and RET opens there."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list)
      (should (eq (window-buffer first) buffer))
      (drover-list-test-goto "ask")
      (use-local-map (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map drover-list-mode-map)
                       (keymap-set map "C-'" #'drover-dispatch)
                       map))
      (unwind-protect
          (progn
            (execute-kbd-macro (kbd "C-' C-n"))
            (should (equal (drover-list-test-id-at-point) "fail"))
            (should (equal (drover-list-test-menu-session) "fail"))
            ;; C-u stays the prefix argument: RET opens in another window.
            (execute-kbd-macro (kbd "C-u RET"))
            (should-not transient--prefix)
            (should (equal (car drover-test-calls) (list :open "fail" nil second)))
            (should (eq (window-buffer first) buffer))
            (should (equal (buffer-name (window-buffer second)) "*session fail*")))
        (use-local-map drover-list-mode-map)))))

(ert-deftest drover-list-test-dispatch-list-keys ()
  "The List group's keys are its own: transient binds none of them."
  (dolist (key '("C-n" "C-p" "M-n" "M-p" "M-<" "M->" "1" "9" "0"))
    (dolist (map (list transient-map transient-base-map transient-popup-navigation-map
                       transient-predicate-map))
      (should-not (commandp (keymap-lookup map key)))))
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-'"))
    (pcase-dolist (`(,key . ,command) '(("C-n" . drover-list-next-session)
                                        ("C-p" . drover-list-previous-session)
                                        ("M-n" . drover-list-next-group)
                                        ("M-p" . drover-list-previous-group)
                                        ("M-<" . drover-list-first-session)
                                        ("M->" . drover-list-last-session)
                                        ("RET" . drover-open-at-point)
                                        ("1" . drover-list-open-numbered)
                                        ("5" . drover-list-open-numbered)
                                        ("0" . drover-list-open-numbered)
                                        ("C-u" . universal-argument)))
      (should (eq (keymap-lookup transient--transient-map key) command)))
    ;; One key, one entry of the menu.
    (let ((keys (mapcar (lambda (suffix) (oref suffix key))
                        (seq-remove (lambda (suffix) (oref suffix inactive)) transient--suffixes))))
      (should (equal keys (delete-dups (copy-sequence keys)))))))

(ert-deftest drover-list-test-dispatch-opens-numbered-elsewhere ()
  "From a buffer beside the sidebar, the menu's digits open the sidebar's sessions by number."
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-' 3"))
    (should-not transient--prefix)
    ;; In the window where you work, as RET at the sidebar's row opens it.
    (should (equal (car drover-test-calls) (list :open "done" nil first)))
    (should (eq (selected-window) first))
    (should (equal (buffer-name (window-buffer first)) "*session done*"))
    (should (equal (drover-list-test-at-side side) "done"))
    ;; The sidebar collapses no group: idle is the last.
    (set-window-buffer first code)
    (set-buffer code)
    (execute-kbd-macro (kbd "C-' 0"))
    (should (equal (car drover-test-calls) (list :open "idle" nil first)))
    (should (equal (drover-list-test-at-side side) "idle"))
    ;; From a session's own buffer too, though the menu acts on its session
    ;; there and shows no List group: the digits open the sidebar's.
    (set-window-buffer first code)
    (set-buffer code)
    (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "work")))
    (should-not (plist-get (drover--context) :window))
    (execute-kbd-macro (kbd "C-'"))
    (should (string-search "1…9 0 Open by number" (drover-list-test-menu-text)))
    (execute-kbd-macro (kbd "2"))
    (should-not transient--prefix)
    (should (equal (car drover-test-calls) (list :open "fail" nil first)))
    (should (eq (selected-window) first))
    (should (equal (drover-list-test-at-side side) "fail"))
    ;; Without a list in sight, the digits are not the menu's.
    (set-window-buffer first code)
    (set-buffer code)
    (delete-window side)
    (execute-kbd-macro (kbd "C-'"))
    (should-not (seq-find (lambda (suffix) (and (eq (oref suffix command) 'drover-list-open-numbered)
                                                (not (oref suffix inactive))))
                          transient--suffixes))
    (execute-kbd-macro (kbd "C-g"))))

(ert-deftest drover-list-test-open-numbered ()
  "The digits open the sessions at the top of their trees by number, 0 the last.
Subagents are not numbered, nor the sessions of collapsed groups; point
goes to the session opened."
  (drover-list-test-with (cons (drover-test-session "sub" :status 'working :parent "fail"
                                                    :since (drover-list-test-ago 20))
                               (drover-list-test-side-sample))
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (ignore second)
      (drover-list)
      (should (eq (window-buffer first) buffer))
      ;; The subagent shows, under its parent.
      (should (drover-list-test-section "sub"))
      (cl-flet ((press (key)
                  (setq drover-test-calls nil)
                  (set-window-buffer first buffer)
                  (select-window first)
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (execute-kbd-macro (kbd key)))
                  (cadr (car drover-test-calls))))
        (pcase-dolist (`(,key . ,id) '(("1" . "ask") ("2" . "fail") ("3" . "done") ("4" . "work")
                                       ;; The idle group is collapsed: work is the last.
                                       ("0" . "work")))
          (ert-info (key)
            (should (equal (press key) id))
            ;; Opened as RET opens it, in the list's own window.
            (should (equal (car drover-test-calls) (list :open id nil first)))
            (should (equal (buffer-name (window-buffer first)) (format "*session %s*" id)))
            (with-current-buffer buffer
              (should (equal (drover-list-test-id-at-point) id)))))
        (with-current-buffer buffer
          (setq drover-test-calls nil)
          (should-error (drover-list-open-numbered 5) :type 'user-error)
          (should-not drover-test-calls)
          ;; Expanded, the idle group numbers its session too.
          (drover-list-test-goto-group 'idle)
          (magit-section-show (magit-current-section)))
        (should (equal (press "5") "idle"))
        (should (equal (press "0") "idle"))
        ;; Run otherwise than by a digit, the number is read.
        (setq drover-test-calls nil)
        (set-window-buffer first buffer)
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 2)))
            (let ((last-command-event 'f5))
              (call-interactively #'drover-list-open-numbered))))
        (should (equal (cadr (car drover-test-calls)) "fail"))
        ;; Elsewhere than in drover's list, nothing to number.
        (with-temp-buffer
          (should-error (drover-list-open-numbered 1) :type 'user-error))))))

(ert-deftest drover-list-test-open-numbered-draws-later-rows ()
  "A number among the rows the list left for later draws them, and opens its session."
  (dolist (grouping '(nil status))
    (drover-list-test-with (drover-list-test-random-sessions 60)
      (drover-list-test-shown
        (let ((drover-group-by grouping)
              (drover-show-subagents 'all)
              (drover-list-collapsed-groups nil))
          (cl-letf (((symbol-function 'drover-list--first-rows) (lambda () 10)))
            (let ((tops (progn (revert-buffer)
                               (drover-list--draw-undrawn)
                               (seq-filter (lambda (section)
                                             (zerop (drover-list--row-depth
                                                     (gethash (oref section value)
                                                              (drover-list--model-rows drover-list--model)))))
                                           (drover-list--reachable-sessions)))))
              (should (> (length tops) 9))
              (revert-buffer)
              (should drover-list--undrawn)
              (dolist (number '(9 0))
                (ert-info ((format "%s %d" grouping number))
                  (let ((section (drover-list--numbered-section number)))
                    (should section)
                    (should (equal (oref section value)
                                   (oref (if (zerop number) (car (last tops)) (nth (1- number) tops))
                                         value)))))))))))))

;;;; Columns

(ert-deftest drover-list-test-hidden-fields ()
  "The width chooses among the fields the user lets show; the title always shows."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (setq drover-list-hidden-fields '(location branch model harness))
      (should (equal (drover-list-test-layout-at 600)
                     '(columns title agent provider context created age cache summary)))
      ;; Room freed for the fields of lower rank: context 396, provider
      ;; 424, agent 473, created 508.
      (should (equal (drover-list-test-layout-at 520)
                     '(columns title agent provider context created age cache summary)))
      (should (equal (drover-list-test-layout-at 510)
                     '(columns title agent provider context age cache summary)))
      ;; In two lines, a hidden summary leaves no second line.
      (setq drover-list-hidden-fields '(summary harness))
      (should (equal (drover-list-test-layout-at 320) '(columns title model age cache)))
      (setq drover-list-hidden-fields '(cache title harness))
      (should (equal (drover-list-test-layout-at 120) '(narrow title context age summary)))
      (should-not drover-list--caches)
      (should (string-match-p "\\` +! Bravo +1m\n +asks: Which f" (drover-list-test-entry "b")))
      (setq drover-list-hidden-fields '(summary age harness))
      (drover-list-test-layout-at 120)
      (should (string-match-p "\\` +! Bravo\\'" (drover-list-test-entry "b"))))))

(ert-deftest drover-list-test-columns-from-view-menu ()
  "The View menu's switches hide a field at once, the menu staying open."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-shown
      (revert-buffer)
      (pcase-dolist (`(,key ,field ,text) '(("m" model "opus high") ("v" provider "P")
                                            ("x" context "50k")))
        (should (memq field drover-list--fields))
        (should (string-search text (drover-list-test-heading "a")))
        (drover-list-test-type (concat "V - " key))
        (should (eq (oref transient--prefix command) 'drover-view))
        (should (equal drover-list-hidden-fields (list field 'harness)))
        (should-not (memq field drover-list--fields))
        (should-not (string-search text (drover-list-test-heading "a")))
        ;; Its column is no longer named.
        (should-not (string-search (plist-get (drover-list--field-spec field) :label)
                                   header-line-format))
        (drover-list-test-type (concat "- " key " C-g"))
        (should (equal drover-list-hidden-fields '(harness)))
        (should (memq field drover-list--fields))
        (should-not transient--prefix)))))

;;;; Short columns

(defmacro drover-list-test-messages (&rest body)
  "Run BODY and return the messages it showed in the echo area, in order."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  ;; The command loop clears the echo area, with "".
                  (when (and format-string (not (equal format-string "")))
                    (push (apply #'format-message format-string arguments) messages)))))
       ,@body)
     (nreverse messages)))

(defun drover-list-test-ages-sample ()
  "Return the sessions of a wide row whose ages were cut while it had room.
One works, aged 11m, created 25m ago, on a model and effort, its prompt
cache live; the other asks a long question, which the summary shows."
  (list (drover-list-test-titled "a" "drover list: provider, expansion" :status 'working
                                 :project ".emacs.d" :branch "main" :agent "general"
                                 :model "Opus 5.5" :effort "high"
                                 :created (drover-list-test-ago 1500)
                                 :updated (drover-list-test-ago 700)
                                 :cache-expires (+ drover-test-now 240) :cache-ttl 300)
        (drover-list-test-titled "b" "Bravo" :status 'waiting :since (drover-list-test-ago 60)
                                 :created (drover-list-test-ago 1500)
                                 :requests '((:kind question :summary "Which of the many files should I edit before the tests run, and in which order would you like them changed?")))))

(ert-deftest drover-list-test-short-columns-never-cut ()
  "Ages are never cut while the summary or the title has room to give.
The summary of b wants more than the line has: before, every column
without a :max gave up room in proportion, the ages too, and at least a
pixel each, so \"11m\", 26 pixels, was cut in a column of 25 or of its
:min, 21, though the line had room."
  (drover-list-test-with (drover-list-test-ages-sample)
    (drover-list-test-with-font
      (dolist (width '(1100 1000 900 800 700 600 560))
        (drover-list-test-layout-at width)
        (should (memq 'age drover-list--fields))
        (let ((cells (mapcar #'cdr (drover-list-test-cells "a"))))
          (should (member "11m" cells))
          (when (memq 'created drover-list--fields)
            (should (member "25m" cells)))
          ;; The model may give way, its effort never.
          (should (seq-find (lambda (cell) (string-suffix-p " high" cell)) cells))
          ;; The title gives way before it.
          (when (>= width 700)
            (should (member "Opus 5.5 high" cells)))
          (should-not (seq-find (lambda (cell) (string-match-p "\\`[0-9]+…\\'" cell)) cells)))
        ;; The summary, last, ends within the line.
        (let ((last (car (last (drover-list-test-cells "b")))))
          (should (<= (+ (car last) (drover-list-test-fake-width (cdr last)))
                      (- width 7)))))
      ;; Narrower, the title gives way, the ages and effort stay whole.
      (drover-list-test-layout-at 520)
      (let ((cells (mapcar #'cdr (drover-list-test-cells "a"))))
        (should (member "11m" cells))
        (should (seq-find (lambda (cell) (string-match-p "provider…\\|exp…\\|…\\'" cell)) cells))))))

(ert-deftest drover-list-test-columns-give-exactly ()
  "Columns give up the room missing, no more: no column is narrowed while the line has room."
  (drover-list-test-with (drover-list-test-ages-sample)
    (drover-list-test-with-font
      (dolist (width (number-sequence 500 1200 37))
        (let* ((drover-list-test-width width)
               (metrics (drover-list--metrics drover-test-now))
               (rows (mapcan (lambda (group) (drover-list--tree-rows (cdr group) (car group) metrics))
                             (drover-view-groups (drover-known-sessions) drover-test-now)))
               (layout (drover-list--layout rows metrics))
               (columns (plist-get layout :columns)))
          (when (eq (plist-get layout :layout) 'columns)
            (pcase-let ((`(,_ ,x ,column ,_) (car (last columns))))
              (let ((wanted (apply #'+ (mapcar (lambda (entry)
                                                 (drover-list--natural (car entry)
                                                                       (drover-list--summarize rows)
                                                                       metrics))
                                               columns))))
                ;; Either every column has what it wants, or the line is full.
                (should (or (<= (+ wanted (* 14 (1- (length columns))) 14) (plist-get metrics :width))
                            (= (+ x column) (plist-get metrics :width))))))))))))

(ert-deftest drover-list-test-give-never-takes-more ()
  "Room given up is shared in proportion, its rounding never taking a pixel too many."
  (let ((widths (vector 26 1000 70))
        (floors (vector 21 84 70)))
    (should (= (drover-list--give widths floors '(0 1 2) 10) 0))
    (should (= (apply #'+ (append widths nil)) (- 1096 10)))
    ;; The age gives 10 × 5 / 921 pixels, which rounds to none.
    (should (= (aref widths 0) 26))
    (should (equal (append floors nil) '(21 84 70))))
  (let ((widths (vector 30 30 30))
        (floors (vector 0 0 0)))
    ;; Two pixels among three equal columns: two give one each.
    (should (= (drover-list--give widths floors '(0 1 2) 2) 0))
    (should (equal (sort (append widths nil) #'<) '(29 29 30))))
  (let ((widths (vector 30 30))
        (floors (vector 25 28)))
    ;; More missing than they can give: they give all, the rest remains.
    (should (= (drover-list--give widths floors '(0 1) 10) 3))
    (should (equal (append widths nil) '(25 28)))))

(ert-deftest drover-list-test-model-keeps-its-effort ()
  "A model's column too narrow cuts the model's name and keeps its effort."
  (drover-list-test-with-font
    (let* ((text (drover-list--model-text
                  (drover-session-create :model "Claude Opus 5.5" :effort "high")))
           (cut (drover-list--truncate text 70)))
      (should (string-suffix-p "… high" cut))
      (should (<= (drover-list-test-fake-width cut) 70))
      (should (eq (get-text-property (1- (length cut)) 'font-lock-face cut) 'opencode-setting-chosen))
      ;; Too narrow even for that: cut as any text.
      (should (equal (drover-list--truncate text 21) "Cl…"))
      ;; An effort alone is cut as any text.
      (should (equal (drover-list--truncate (drover-list--model-text
                                             (drover-session-create :effort "medium"))
                                            30)
                     "me…")))))

;;;; The branch column

(defun drover-list-test-face-in-heading (id text)
  "Return the face of TEXT in the heading of session ID, or `none' without it."
  (save-excursion
    (drover-list-test-goto id)
    (if (search-forward text (line-end-position) t)
        (get-text-property (match-beginning 0) 'font-lock-face)
      'none)))

(ert-deftest drover-list-test-branch-column ()
  "The branch has a column of its own, the default one greyed out."
  (drover-list-test-with
      (list (drover-test-session "m" :status 'working :project "p" :branch "main"
                                 :updated (drover-list-test-ago 10))
            (drover-test-session "f" :status 'working :project "p" :branch "feature/x"
                                 :updated (drover-list-test-ago 20))
            (drover-test-session "o" :status 'working :project "q" :branch "master"
                                 :updated (drover-list-test-ago 30))
            (drover-test-session "n" :status 'working :project "q"
                                 :updated (drover-list-test-ago 40)))
    (should (memq 'branch drover-list--fields))
    (should (string-match-p "Session m +p +main +10s\\'" (drover-list-test-heading "m")))
    (should (string-match-p "Session f +p +feature/x +20s\\'" (drover-list-test-heading "f")))
    (should (string-match-p "Session o +q +master +30s\\'" (drover-list-test-heading "o")))
    ;; Without a branch, the cell is empty.
    (should (string-match-p "Session n +q +40s\\'" (drover-list-test-heading "n")))
    (should (= (drover-list-test-cell-at "f" "feature/x")
               (car (rassoc "Branch" (drover-list-test-header-cells)))))
    ;; The default branch reads as a default, as the model's default
    ;; effort does; another plain.
    (should (eq (drover-list-test-face-in-heading "m" "main") 'opencode-setting-inherited))
    (should (eq (drover-list-test-face-in-heading "o" "master") 'opencode-setting-inherited))
    (should-not (drover-list-test-face-in-heading "f" "feature/x"))
    ;; On default branches alone, the column still tells them.
    (setf (drover-session-branch (drover-list-test-find "f")) "main")
    (drover-list-test-refresh)
    (should (memq 'branch drover-list--fields))
    (should (string-search "Branch" header-line-format))
    ;; Grouped by branch, the group names it.
    (setf (drover-session-branch (drover-list-test-find "f")) "feature/x")
    (drover-list-test-shown
      (drover-set-group-by 'branch)
      (should (string-match-p "Session f +p +20s\\'" (drover-list-test-heading "f"))))))

(ert-deftest drover-list-test-branch-default-from-the-repository ()
  "A repository naming its default branch greys that one out, not main."
  (let* ((root (make-temp-file "drover-repo" t))
         (remote (expand-file-name ".git/refs/remotes/origin" root)))
    (unwind-protect
        (progn
          (make-directory remote t)
          (with-temp-file (expand-file-name ".git/HEAD" root) (insert "ref: refs/heads/main\n"))
          (with-temp-file (expand-file-name "HEAD" remote) (insert "ref: refs/remotes/origin/develop\n"))
          (let ((drover--default-branches (make-hash-table :test #'equal)))
            (drover-list-test-with
                (list (drover-test-session "m" :status 'working :project "p" :branch "main"
                                           :directory root :updated (drover-list-test-ago 10))
                      (drover-test-session "d" :status 'working :project "p" :branch "develop"
                                           :directory root :updated (drover-list-test-ago 20)))
              (should (string-match-p "Session m +p +main +10s\\'" (drover-list-test-heading "m")))
              (should (string-match-p "Session d +p +develop +20s\\'" (drover-list-test-heading "d")))
              (should-not (drover-list-test-face-in-heading "m" "main"))
              (should (eq (drover-list-test-face-in-heading "d" "develop") 'opencode-setting-inherited)))))
      (delete-directory root t))))

(ert-deftest drover-list-test-branch-default-of-another-host ()
  "Another host's session greys out the branch its owner read as the default; this host reads nothing."
  (let* ((root (make-temp-file "drover-repo" t))
         (remote (expand-file-name ".git/refs/remotes/origin" root))
         (looked nil))
    (unwind-protect
        (progn
          (make-directory remote t)
          (with-temp-file (expand-file-name ".git/HEAD" root) (insert "ref: refs/heads/main\n"))
          ;; Here, the same directory is a repository of another default.
          (with-temp-file (expand-file-name "HEAD" remote) (insert "ref: refs/remotes/origin/develop\n"))
          (advice-add 'drover--read-git-default-branch :before (lambda (directory) (push directory looked))
                      '((name . drover-list-test-looked)))
          (let ((drover--default-branches (make-hash-table :test #'equal)))
            (drover-list-test-with
                (list (drover-test-session "m" :status 'working :project "p" :branch "main" :host "core"
                                           :default-branch "main" :directory root
                                           :updated (drover-list-test-ago 10))
                      (drover-test-session "d" :status 'working :project "p" :branch "develop" :host "core"
                                           :default-branch "main" :directory root
                                           :updated (drover-list-test-ago 20))
                      (drover-test-session "u" :status 'working :project "p" :branch "master" :host "core"
                                           :directory root :updated (drover-list-test-ago 30)))
              (should (eq (drover-list-test-face-in-heading "m" "main") 'opencode-setting-inherited))
              (should-not (drover-list-test-face-in-heading "d" "develop"))
              ;; The owner knowing none, main and master stand for it.
              (should (eq (drover-list-test-face-in-heading "u" "master") 'opencode-setting-inherited))
              (should-not looked))))
      (advice-remove 'drover--read-git-default-branch 'drover-list-test-looked)
      (delete-directory root t))))

(ert-deftest drover-list-test-directory-of-another-host ()
  "Details name another host's directory as it does, and its changed files as it sent them."
  (let* ((home (expand-file-name "~/src/x/"))
         (layout (list :fields nil :now drover-test-now))
         (fact (lambda (session)
                 (nth 1 (assoc "Directory" (drover-list--hidden-facts session layout nil))))))
    (should (equal (funcall fact (drover-session-create :directory home)) "~/src/x/"))
    (should (equal (funcall fact (drover-session-create :directory home :host "core")) home))
    (should (equal (substring-no-properties (drover-list--file-line (list (concat home "a.el") 1 2) home))
                   "a.el  +1 −2"))
    (should (equal (substring-no-properties
                    (drover-list--file-line (list (concat home "a.el") 1 2)
                                            (drover-session-local-directory
                                             (drover-session-create :directory home :host "core"))))
                   (concat home "a.el  +1 −2")))))

(ert-deftest drover-list-test-branch-in-view-menu ()
  "The View menu's Columns switch the branch's column."
  (drover-list-test-with
      (list (drover-test-session "f" :status 'working :project "p" :branch "feature"
                                 :updated (drover-list-test-ago 20)))
    (drover-list-test-shown
      (revert-buffer)
      (should (memq 'branch drover-list--fields))
      (drover-list-test-type "V - b C-g")
      (should (equal drover-list-hidden-fields '(branch harness)))
      (should-not (memq 'branch drover-list--fields))
      (should-not (string-search "feature" (drover-list-test-heading "f"))))))

;;;; The agent column

(ert-deftest drover-list-test-agent-column ()
  "The agent shows as @name, bold, in its face, the @ too; a backend may draw it itself."
  (drover-list-test-with
      (list (drover-test-session "b" :status 'working :agent "build" :updated (drover-list-test-ago 10))
            (drover-test-session "c" :status 'working :agent "Coder" :updated (drover-list-test-ago 20)
                                 :agent-display (propertize "Code" 'face '(bold (:foreground "#ff8800"))
                                                            'help-echo "Agent: Code")))
    (let ((opencode-agent-faces '(font-lock-keyword-face font-lock-type-face)))
      (revert-buffer)
      (should (string-match-p "Session b +@build +10s\\'" (drover-list-test-heading "b")))
      (should (string-match-p "Session c +@Code +20s\\'" (drover-list-test-heading "c")))
      (save-excursion
        (drover-list-test-goto "b")
        (search-forward "@build")
        (let ((face (get-text-property (match-beginning 0) 'font-lock-face)))
          (should (equal face (list 'bold (drover-agent-face "build"))))
          (should (memq (cadr face) opencode-agent-faces))
          ;; The @ and the name read as one word.
          (should (equal (get-text-property (1+ (match-beginning 0)) 'font-lock-face) face))
          (should (equal (get-text-property (match-beginning 0) 'help-echo) "Agent: build"))))
      (save-excursion
        (drover-list-test-goto "c")
        (search-forward "@Code")
        (should (equal (get-text-property (match-beginning 0) 'font-lock-face)
                       '(bold (:foreground "#ff8800"))))
        (should (equal (get-text-property (1- (point)) 'help-echo) "Agent: Code")))
      ;; Grouped and filtered by the agent's name, not its display.
      (should (equal (drover-session-agent (drover-list-test-find "c")) "Coder")))))

(ert-deftest drover-list-test-agent-width-counts-its-face ()
  "The agent's column is as wide as @name drawn bold."
  (drover-list-test-with
      (list (drover-test-session "b" :status 'working :agent "build" :updated (drover-list-test-ago 10)
                                 :detail "Busy"))
    ;; A font whose bold characters take one pixel more.
    (cl-letf (((symbol-function 'drover-list--pixel-width)
               (lambda (string)
                 (let ((width (drover-list-test-fake-width string)))
                   (dotimes (index (length string) width)
                     (let ((face (get-text-property index 'face string)))
                       (when (or (eq face 'bold) (memq 'bold (ensure-list face)))
                         (setq width (1+ width))))))))
              ((symbol-function 'drover-list--extent)
               (lambda (string) (cons 0 (drover-list--pixel-width string))))
              ((symbol-function 'drover-list--line-width) (lambda () 1000)))
      (revert-buffer)
      (let ((agent (seq-find (lambda (column) (eq (car column) 'agent))
                             (plist-get (drover-list--layout
                                         (drover-list--tree-rows
                                          (cdar (drover-view-groups (drover-known-sessions) drover-test-now))
                                          nil (drover-list--metrics drover-test-now))
                                         (drover-list--metrics drover-test-now))
                                        :columns))))
        ;; "@build", 38 pixels, and 6 more drawn bold: wider than "Agent".
        (should (= (nth 2 agent) 44))))))

;;;; Selection

(defun drover-list-test-selection-sample ()
  "Return sessions in one group and another: done a, b and c, working w."
  (list (drover-test-session "a" :status 'done :since (drover-list-test-ago 30))
        (drover-test-session "b" :status 'done :since (drover-list-test-ago 20))
        (drover-test-session "c" :status 'done :since (drover-list-test-ago 10))
        (drover-test-session "w" :status 'working :updated (drover-list-test-ago 5))))

(defun drover-list-test-selected ()
  "Return the identifiers of the sessions selected in the list, in order."
  (mapcar #'drover-session-id (drover-selected-sessions t)))

(defun drover-list-test-highlighted ()
  "Return the values of the sections Magit highlights as selected, in order.
A session by its identifier, a group by its value."
  (mapcar (lambda (overlay)
            (let ((section (magit-section-at (overlay-start overlay))))
              (if (object-of-class-p section 'drover-list-session-section)
                  (cdr (oref section value))
                (cdr (oref section value)))))
          (sort (copy-sequence magit-section-selection-overlays)
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defmacro drover-list-test-selecting (sessions &rest body)
  "Run BODY in the list of SESSIONS shown, Transient Mark mode on.
Confirmations answer yes, their prompts pushed to `asked', newest first."
  (declare (indent 1) (debug t))
  `(drover-list-test-with ,sessions
     (let ((transient-mark-mode t)
           (asked nil))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t))
                 ((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) t)))
         (drover-list-test-shown
           (ignore asked)
           ,@body)))))

(ert-deftest drover-list-test-region-selects-sessions ()
  "A region selects the rows of sessions it touches, those Magit highlights."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    ;; Set within the line of a, point a line down: two sessions,
    ;; though the text of the region lies on a's line but for a few
    ;; characters.
    (drover-list-test-goto "a")
    (forward-char 3)
    (drover-list-test-type "C-SPC C-n")
    (should (region-active-p))
    (should (equal (drover-list-test-id-at-point) "b"))
    (should (equal (drover-list-test-selected) '("a" "b")))
    (should (equal (drover-list-test-highlighted) '("a" "b")))
    (drover-list-test-type "C-n")
    (should (equal (drover-list-test-selected) '("a" "b" "c")))
    (should (equal (drover-list-test-highlighted) '("a" "b" "c")))
    ;; Point back on a's line: a alone.
    (drover-list-test-type "C-p C-p")
    (should (equal (drover-list-test-selected) '("a")))
    (should (equal (drover-list-test-highlighted) '("a")))
    ;; Some text of a's line is text: nothing is selected, and the
    ;; session at point stands alone.
    (drover-list-test-type "C-f C-f")
    (should (region-active-p))
    (should-not (drover-list--selected-sessions))
    (should-not (drover-list-test-highlighted))
    (should (equal (drover-list-test-selected) '("a")))
    ;; Into another group: the rows of both it touches.
    (deactivate-mark)
    (drover-list-test-goto "c")
    (drover-list-test-type "C-SPC")
    (goto-char (oref (drover-list-test-section "w") start))
    (drover-list-test-type "C-e")
    (should (region-active-p))
    (should (equal (drover-list-test-highlighted) '("c" "w")))
    (should (equal (drover-list-test-selected) '("c" "w")))
    ;; A group's heading selects nothing, nor does the session at point
    ;; stand for it there.
    (deactivate-mark)
    (drover-list-test-goto-group 'done)
    (drover-list-test-type "C-SPC")
    (should-not (drover-list-test-highlighted))
    (should-not (drover-list--selected-sessions))
    (should-not (drover-list-test-selected))
    (should-error (drover-selected-sessions) :type 'user-error)
    ;; Without a region, the session at point.
    (deactivate-mark)
    (drover-list-test-goto "b")
    (should (equal (drover-list-test-selected) '("b")))))

(ert-deftest drover-list-test-region-selects-rows-only ()
  "A region over a group's heading selects the rows it touches that show, never the group's."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (cl-flet ((calls ()
                (prog1 (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) :archive))
                                                  (reverse drover-test-calls)))
                  (setq drover-test-calls nil)))
              (region (from to)
                (deactivate-mark)
                (goto-char from)
                (drover-list-test-type "C-SPC")
                (goto-char to)
                ;; Moved through the command loop, so that Magit highlights.
                (drover-list-test-type "C-e")))
      ;; From Done's heading to b's row: a and b, not the group.
      (region (save-excursion (drover-list-test-goto-group 'done) (point))
              (oref (drover-list-test-section "b") start))
      (should (equal (drover-list-test-selected) '("a" "b")))
      (should (equal (drover-list-test-highlighted) '("a" "b")))
      ;; From c's row to Working's heading: c alone, the heading
      ;; adding nothing; the menu names it though point is on no session.
      (region (oref (drover-list-test-section "c") start)
              (save-excursion (drover-list-test-goto-group 'working) (point)))
      (should-not (drover-session-at-point t))
      (should (equal (drover-list-test-selected) '("c")))
      (should (equal (drover-list-test-highlighted) '("c")))
      (let ((scope (drover--context)))
        (cl-letf (((symbol-function 'drover--scope) (lambda () scope)))
          (should (equal (drover--dispatch-list-description) "List: Session c"))
          (should (drover-dispatch-selection nil #'drover-archivable-p))
          (should-not (drover-dispatch-selection :interrupt #'drover--running-p))))
      ;; Done collapsed, from its heading to w's row: w alone, its
      ;; sessions hidden; archiving acts on w, without asking.
      (deactivate-mark)
      (magit-section-hide (drover-list--group-section
                           (drover-list-test-section "a")))
      (should (drover-list-test-group-hidden-p 'done))
      (region (save-excursion (drover-list-test-goto-group 'done) (point))
              (oref (drover-list-test-section "w") start))
      (should (drover-list-test-group-hidden-p 'done))
      (should (equal (drover-list-test-selected) '("w")))
      (should (equal (drover-list-test-highlighted) '("w")))
      (let ((scope (drover--context)))
        (cl-letf (((symbol-function 'drover--scope) (lambda () scope)))
          (should (equal (drover--dispatch-list-description) "List: Session w"))))
      (setq drover-test-calls nil)
      (should (equal (drover-list-test-messages (drover-list-test-type "z"))
                     '("Archived Session w")))
      (should-not asked)
      (should (equal (calls) '("w")))
      ;; Over headings alone, the collapsed group's between: nothing,
      ;; and nothing is archived.
      (region (save-excursion (drover-list-test-goto-group 'done) (point))
              (save-excursion (drover-list-test-goto-group 'working) (point)))
      (should (region-active-p))
      (should-not (drover-list--selected-sessions))
      (should-not (drover-list-test-highlighted))
      (should-error (call-interactively #'drover-archive) :type 'user-error)
      (should-not (calls)))))

(ert-deftest drover-list-test-region-survives-redraws ()
  "A redraw keeps the region and the sessions it selects."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (drover-list-test-goto "a")
    (forward-char 2)
    (drover-list-test-type "C-SPC C-n")
    (setq deactivate-mark nil)
    (setf (drover-session-since (drover-list-test-find "c")) (drover-list-test-ago 40))
    ;; Moved in place, then drawn anew.
    (drover-backend-changed 'fake "c")
    (drover-changed-now)
    (dotimes (_ 2)
      (should-not deactivate-mark)
      (should (region-active-p))
      (should (equal (mapcar #'cdr (drover-list-test-groups)) '(("c" "a" "b") ("w"))))
      (should (equal (drover-list-test-selected) '("a" "b")))
      (should (equal (drover-list-test-highlighted) '("a" "b")))
      (drover-list--render))
    (drover-list--draw-caches)
    (should-not deactivate-mark)))

(ert-deftest drover-list-test-actions-on-selection ()
  "Marking seen, interrupting, archiving and deleting act on every session selected."
  (drover-list-test-selecting
      (list (drover-test-session "a" :status 'failed :since (drover-list-test-ago 30))
            (drover-test-session "b" :status 'done :since (drover-list-test-ago 20))
            (drover-test-session "c" :status 'working :since (drover-list-test-ago 10))
            (drover-test-session "d" :status 'idle))
    (setq drover-group-by nil)
    (revert-buffer)
    (cl-flet ((select-all ()
                (drover-list-test-goto "a")
                (forward-char 1)
                (drover-list-test-type "C-SPC")
                (goto-char (oref (drover-list-test-section "d") start))
                (drover-list-test-type "C-f")
                (should (equal (drover-list-test-selected) '("a" "b" "c" "d"))))
              (calls (operation)
                (prog1 (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) operation))
                                                  (reverse drover-test-calls)))
                  (setq drover-test-calls nil))))
      (should (equal (drover-list-test-order) '("a" "b" "c" "d")))
      ;; Only the turns that ended unseen are marked seen.
      (select-all)
      (should (equal (drover-list-test-messages (drover-list-test-type "s"))
                     '("Marked 2 sessions seen")))
      (should (equal (calls :mark-seen) '("a" "b")))
      ;; Acting ends the selection.
      (should-not (region-active-p))
      ;; Only the running turn is interrupted, after asking.
      (select-all)
      (drover-list-test-type "K")
      (should (equal asked '("Interrupt Session c? ")))
      (should (equal (calls :interrupt) '("c")))
      ;; Archiving asks nothing, and tells once.
      (setq asked nil)
      (select-all)
      (should (equal (drover-list-test-messages (drover-list-test-type "z"))
                     '("Archived 4 sessions")))
      (should-not asked)
      (should (equal (calls :archive) '("a" "b" "c" "d")))
      ;; One confirmation names them all.
      (select-all)
      (drover-list-test-type "x")
      (should (equal asked '("Delete 4 sessions for good: Session a, Session b, Session c, Session d? ")))
      (should (equal (calls :delete) '("a" "b" "c" "d")))
      ;; Declined, nothing happens, and the selection stays.
      (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
        (select-all)
        (should-error (call-interactively #'drover-delete) :type 'user-error)
        (should-not (calls :delete))
        (should (region-active-p)))
      ;; Nothing selected applies: said so, nothing done.
      (deactivate-mark)
      (drover-list-test-goto "c")
      (drover-list-test-type "C-SPC C-n")
      (should (equal (drover-list-test-selected) '("c" "d")))
      (should (equal (cadr (should-error (call-interactively #'drover-mark-seen) :type 'user-error))
                     "None of the 2 selected sessions ended a turn unseen")))))

(ert-deftest drover-list-test-selection-failures-reported-once ()
  "A session failing does not stop the others, and the outcome is told once."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (setf (plist-get (alist-get 'fake drover--backends) :archive)
          (lambda (session)
            (if (equal (drover-session-id session) "b")
                (error "Gone already")
              (push (list :archive (drover-session-id session)) drover-test-calls))))
    (drover-list-test-goto "a")
    (drover-list-test-type "C-SPC C-n C-n")
    (should (equal (drover-list-test-selected) '("a" "b" "c")))
    (should (equal (drover-list-test-messages (drover-list-test-type "z"))
                   '("Archived 2 sessions; failed for Session b (Gone already)")))
    (should (equal (reverse drover-test-calls) '((:archive "a") (:archive "c"))))))

(ert-deftest drover-list-test-selection-from-menu ()
  "The menu, in the list, acts on the selection, and tells how many it holds."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (use-local-map (let ((map (make-sparse-keymap)))
                     (set-keymap-parent map drover-list-mode-map)
                     (keymap-set map "C-'" #'drover-dispatch)
                     map))
    (unwind-protect
        (progn
          (drover-list-test-goto "a")
          (drover-list-test-type "C-SPC C-n")
          (execute-kbd-macro (kbd "C-'"))
          (should (equal (mapcar #'drover-session-id (plist-get (oref transient--prefix scope) :selection))
                         '("a" "b")))
          (should (equal (drover--dispatch-list-description) "List: 2 sessions selected"))
          ;; Moving extends the selection, which the menu follows.
          (execute-kbd-macro (kbd "C-n"))
          (should (equal (length (plist-get (oref transient--prefix scope) :selection)) 3))
          (execute-kbd-macro (kbd "z"))
          (should-not transient--prefix)
          ;; Archiving asks nothing.
          (should-not asked)
          (should (equal (reverse drover-test-calls) '((:archive "a") (:archive "b") (:archive "c")))))
      (when transient--prefix
        (execute-kbd-macro (kbd "C-g")))
      (use-local-map drover-list-mode-map))))

(ert-deftest drover-list-test-selection-menu-apt ()
  "An entry applies to a selection when it applies to one of its sessions."
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (drover-list-test-goto "a")
    (drover-list-test-type "C-SPC C-n")
    (let ((scope (drover--context)))
      (cl-letf (((symbol-function 'drover--scope) (lambda () scope)))
        (should (drover-dispatch-selection :mark-seen #'drover--ended-unseen-p))
        (should (drover-dispatch-selection :archive))
        ;; None of a and b runs a turn.
        (should-not (drover-dispatch-selection :interrupt #'drover--running-p))))))

;;;; Copying sessions

(defmacro drover-list-test-with-links (&rest body)
  "Run BODY with an Org link type storing the session at point, as test-session:ID."
  (declare (indent 0) (debug t))
  `(progn
     (require 'ol)
     (let ((org-link-parameters (copy-tree org-link-parameters)))
       (org-link-set-parameters
        "test-session"
        :store (lambda (&optional _interactive)
                 (when-let* ((session (drover-session-at-point t)))
                   (org-link-store-props :type "test-session"
                                         :link (concat "test-session:" (drover-session-id session))
                                         :description "Not the title"))))
       ,@body)))

(ert-deftest drover-list-test-copy-links ()
  "M-w copies the links of the sessions selected, else of the session at point."
  (drover-list-test-with-links
    (drover-list-test-selecting (drover-list-test-selection-sample)
      (let ((kill-ring nil))
        (drover-list-test-goto "a")
        (drover-list-test-type "C-SPC C-n")
        (should (equal (drover-list-test-messages (drover-list-test-type "M-w"))
                       '("Copied links to 2 sessions")))
        (should (equal (substring-no-properties (car kill-ring))
                       "[[test-session:a][Session a]]\n[[test-session:b][Session b]]"))
        (should-not (region-active-p))
        ;; Without a region, the session at point.
        (drover-list-test-goto "c")
        (should (equal (drover-list-test-messages (drover-list-test-type "M-w"))
                       '("Copied a link to Session c")))
        (should (equal (car kill-ring) "[[test-session:c][Session c]]"))
        ;; A region from one group into another copies the links of
        ;; the rows it touches.
        (drover-list-test-goto "c")
        (drover-list-test-type "C-SPC")
        (goto-char (oref (drover-list-test-section "w") start))
        (drover-list-test-type "C-e M-w")
        (should (equal (car kill-ring) "[[test-session:c][Session c]]\n[[test-session:w][Session w]]"))
        ;; A region selecting no session copies its text: some of a
        ;; session's line, or headings alone.
        (drover-list-test-goto "c")
        (drover-list-test-type "C-SPC C-e M-w")
        (should (string-match-p "Session c 10s\\'" (car kill-ring)))
        (drover-list-test-goto-group 'done)
        (drover-list-test-type "C-SPC C-e M-w")
        (should (equal (substring-no-properties (car kill-ring)) "Done (3)"))))))

(ert-deftest drover-list-test-copy-without-link-type ()
  "Without a link type storing sessions, their titles are copied."
  ;; Copying loads Org's links: bound before, the variable would be lexical.
  (require 'ol)
  (drover-list-test-selecting (drover-list-test-selection-sample)
    (let ((kill-ring nil)
          (org-link-parameters nil))
      (drover-list-test-goto "a")
      (drover-list-test-type "C-SPC C-n")
      (should (equal (drover-list-test-messages (drover-list-test-type "M-w"))
                     '("Copied links to 2 sessions, 2 by title: no Org link type stores them")))
      (should (equal (car kill-ring) "Session a\nSession b")))))

;;;; Quoting sessions in the prompt

(defmacro drover-list-test-quoting (sessions &rest body)
  "Run BODY in the list of SESSIONS, selecting, with a prompt of its own.
The prompt shows nowhere; `prompt' returns its text."
  (declare (indent 1) (debug t))
  `(drover-list-test-with-links
     (require 'drover-prompt)
     (let ((drover-prompt-buffer-name "*drover quote test*")
           (drover-prompt-require-features nil)
           (org-mode-hook nil))
       (unwind-protect
           (cl-letf (((symbol-function 'drover-prompt--display) #'ignore)
                     ((symbol-function 'drover-prompt--schedule-refresh) #'ignore)
                     ((symbol-function 'drover-prompt--fetch-defaults) #'ignore))
             (cl-flet ((prompt () (and (get-buffer drover-prompt-buffer-name)
                                       (with-current-buffer drover-prompt-buffer-name
                                         (buffer-substring-no-properties (point-min) (point-max))))))
               (drover-list-test-selecting ,sessions
                 ,@body)))
         (when-let* ((prompt (get-buffer drover-prompt-buffer-name)))
           (kill-buffer prompt))))))

(ert-deftest drover-list-test-quote-selection ()
  "The prompt quotes the sessions selected: link, status and last reply."
  (drover-list-test-quoting
      (list (drover-test-session "a" :status 'failed :since (drover-list-test-ago 30) :detail "Rate limited")
            (drover-test-session "b" :status 'failed :since (drover-list-test-ago 20))
            (drover-test-session "c" :status 'done :since (drover-list-test-ago 10)))
    (setq drover-test-recaps
          `(("a" :prompt "Fix it" :reply ,(concat "Fixed.\n* Not a heading\n#+end_src\n"
                                                  (mapconcat (lambda (n) (format "Line %d" n))
                                                             (number-sequence 1 25) "\n")))
            ("b" :failure "Gone")))
    (drover-list-test-goto "a")
    (drover-list-test-type "C-SPC C-n")
    (let ((messages (drover-list-test-messages (drover-prompt-dwim))))
      (should (equal (car (last messages)) "Quoted 2 sessions")))
    (should (equal (prompt)
                   (concat "Agent sessions:\n"
                           "- [[test-session:a][Session a]] — failed: Rate limited\n"
                           "  Last reply:\n"
                           "  #+begin_src markdown\n"
                           "  Fixed.\n"
                           "  ,* Not a heading\n"
                           "  ,#+end_src\n"
                           (mapconcat (lambda (n) (format "  Line %d\n" n)) (number-sequence 1 17) "")
                           "  #+end_src\n"
                           "  (The reply goes on for 8 more lines.)\n"
                           ;; Its recap failed: no reply.
                           "- [[test-session:b][Session b]] — failed\n"
                           "\n")))
    ;; Sent, the reply is a Markdown fence, escapes undone.
    (should (string-match-p "^  ```markdown\n  Fixed\\.\n  \\* Not a heading\n  #\\+end_src\n"
                            (drover-prompt-to-markdown (prompt))))
    (should (equal (sort (copy-sequence drover-test-recap-calls) #'string<) '("a" "b")))))

(ert-deftest drover-list-test-quote-at-point ()
  "Without a region, the prompt quotes the session at point; on a heading, the line."
  (drover-list-test-quoting (drover-list-test-selection-sample)
    (drover-list-test-goto "b")
    (drover-prompt-dwim)
    (should (equal (prompt) (concat "Agent session:\n"
                                    "- [[test-session:b][Session b]] — done\n"
                                    "  Last reply:\n"
                                    "  #+begin_src markdown\n"
                                    "  Reply of b\n"
                                    "  #+end_src\n\n")))
    (with-current-buffer drover-prompt-buffer-name (erase-buffer))
    (drover-list-test-goto-group 'done)
    (drover-prompt-dwim)
    (should (string-prefix-p "From buffer *drover sessions* line" (prompt)))))

(ert-deftest drover-list-test-quote-waits-for-replies ()
  "Replies arriving later are quoted once all arrived, or once the wait is over."
  (drover-list-test-quoting (drover-list-test-selection-sample)
    (setq drover-test-recap-later t)
    (let ((timers nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (time _repeat function &rest arguments)
                   (let ((timer (timer-create)))
                     (push (list time function arguments timer) timers)
                     timer))))
        (drover-list-test-goto "a")
        (drover-list-test-type "C-SPC C-n C-n")
        (should (equal (drover-list-test-messages (drover-prompt-dwim))
                       '("Fetching the last replies of 3 sessions… 0/3")))
        (should-not (prompt))
        (should (= (caar timers) drover-prompt-quote-timeout))
        (pcase-dolist (`(,id . ,callback) (reverse drover-test-recap-pending))
          (unless (equal id "c")
            (should (equal (drover-list-test-messages
                            (funcall callback (list :reply (concat "Late reply of " id))))
                           (list (format "Fetching the last replies of 3 sessions… %d/3"
                                         (if (equal id "a") 1 2)))))))
        (should-not (prompt))
        ;; The wait is over: c is quoted without a reply.
        (should (equal (drover-list-test-messages
                        (apply (nth 1 (car timers)) (nth 2 (car timers))))
                       '("Quoted 3 sessions; 1 reply did not arrive in time")))
        (let ((text (prompt)))
          (should (string-match-p "Late reply of a" text))
          (should (string-match-p "Late reply of b" text))
          (should (string-match-p "- \\[\\[test-session:c\\]\\[Session c\\]\\] — done\n\n\\'" text))
          ;; c's reply, arriving later still, adds nothing.
          (funcall (cdr (assoc "c" drover-test-recap-pending)) (list :reply "Too late"))
          (should (equal (prompt) text)))))))

(ert-deftest drover-list-test-prints-in-proportion ()
  "What the lists keep prints in proportion to the sessions they show.
Emacs prints a value whole in an error's data, or as `format' does,
here the closure drawing a group, as a profile showed it: each row,
tree and group once led back to every section of the buffer, every
tree of its group and every row, printed again for each, and a group
of 3500 sessions ran to gigabytes.  The lists, here both, with older
and archived sessions, every session expanded and its recap kept, and
the mode line's count, print within a bound a session, and a row or a
tree within a bound whatever the sessions."
  (let ((sessions (cl-loop for i below 200
                           collect (drover-test-session
                                    (format "s%03d" i) :title (format "Session %d" i)
                                    :status (nth (% i 4) '(working idle waiting done))
                                    :requests (and (= (% i 4) 2) (list (list :kind 'question :summary "Which?")))
                                    :parent (and (zerop (% i 5)) (> i 0) (format "s%03d" (1- i))))))
        (older (cl-loop for i below 100
                        collect (drover-test-session (format "old%03d" i) :status 'idle)))
        (archived (cl-loop for i below 100
                           collect (drover-test-session (format "gone%03d" i) :status 'done :archived 1.0))))
    (dolist (grouping '(nil status))
      (drover-list-test-with sessions
        (setf (alist-get 'fake drover--backends)
              (append (list :history (lambda (callback) (funcall callback older))
                            :archived (lambda (callback) (funcall callback archived)))
                      (alist-get 'fake drover--backends)))
        (let ((drover-group-by grouping)
              (drover-list-collapsed-groups nil)
              (global-mode-string nil))
          (drover-list-test-shown
            (drover-set-show-history t)
            (drover-set-show-archived t)
            (drover-changed-now)
            (revert-buffer)
            ;; Every row, those left for later past the first screens too.
            (drover-list--draw-undrawn)
            (let ((expanded 0))
              (magit-map-sections (lambda (section)
                                    (when (drover-list-session-section-p section)
                                      (cl-incf expanded)
                                      (magit-section-show section))))
              (should (> expanded 200))
              (should (= (hash-table-count drover-list--recaps) expanded)))
            (drover-list-side)
            (drover-attention-mode 1)
            (unwind-protect
                (let* ((model drover-list--model)
                       (group (car (drover-list--model-groups model)))
                       (tree (aref (drover-list--group-trees group) 0))
                       (row (car (drover-list--tree-shown tree)))
                       (drawing (lambda () group)))
                  (should (drover-list--row-section row))
                  (should (drover-list--group-container group))
                  ;; Drover's own report of an error carrying one is a line.
                  (should (< (length (drover-error-string (list 'wrong-type-argument 'drover-list--tree group)))
                             drover-error-length))
                  (ert-info ((format "Grouped by %s" grouping))
                    (should (drover-test-printed-length drawing (* 10000 400)))
                    (drover-test-prints-in-proportion
                     400 (list (cons "a row" row) (cons "a tree" tree)
                               (cons "a record" (drover-list--row-session row))))))
              (drover-attention-mode -1))))))))

;;;; Sessions of other hosts

(defun drover-list-test-remote-line (key)
  "Return the first line of the session KEY, of any backend, as a terminal draws it."
  (drover-list--draw-deferred)
  (save-excursion
    (goto-char (oref (drover-list--find-session key) start))
    (drover-list-test-line)))

(defun drover-list-test-remote-animated-p (key)
  "Return non-nil when the glyph of the session KEY, of any backend, is animated."
  (let ((section (drover-list--find-session key)))
    (text-property-any (oref section start) (oref section end) 'drover-list-frame 'working)))

(ert-deftest drover-list-test-host-column ()
  "The host shows only once a session of another host is listed, with its age while not connected."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (should-not (memq 'host (cdr (drover-list-test-layout-at 1000))))
      (let* ((remote (drover-session-create :backend 'remote:core :id '(fake . "r1") :host "core"
                                            :origin 'fake :title "Remote" :status 'working
                                            :since (drover-list-test-ago 60)
                                            :updated (drover-list-test-ago 60)))
             (records (list remote))
             (key '(remote:core fake . "r1"))
             (opencode-session-status-animation-interval 0.15))
        (drover-test-register-simple 'remote:core (lambda () records) :remote "core")
        (should (equal (cdr (drover-list-test-layout-at 1000))
                       '(title host location branch agent model provider context created age cache summary)))
        (should (string-match-p "Remote +core " (drover-list-test-remote-line key)))
        ;; Animated while its host is connected.
        (should (drover-list-test-remote-animated-p key))
        ;; Not connected: dimmed, still, its host telling since when.
        (setq remote (copy-sequence remote))
        (setf (drover-session-stale remote) (drover-list-test-ago 300))
        (setq records (list remote))
        (drover-backend-changed 'remote:core)
        (drover-changed-now)
        (revert-buffer)
        (should (string-match-p "Remote +core 5m " (drover-list-test-remote-line key)))
        (should-not (drover-list-test-remote-animated-p key))
        (goto-char (point-min))
        (let ((title (text-property-search-forward 'drover-list-title key t)))
          (should (memq 'drover-dim (ensure-list (get-text-property (prop-match-beginning title)
                                                                     'font-lock-face)))))
        ;; The narrow layout places it on the title's line, and its
        ;; icon at the end, its tooltip naming the host and telling
        ;; since when it is not connected, as its face does.
        (drover-list-test-layout-at 300)
        (should (string-match-p "\\`.*Remote.*core 5m.* @\\'" (drover-list-test-remote-line key)))
        (should (equal (drover-list-test-tooltip-at key "@") "core — disconnected for 5m"))
        (should (equal (drover-list-test-tooltip-at key "core") "Host: core"))))))

(defun drover-list-test-tooltip-at (key text)
  "Return the tooltip of TEXT on the first line of the session KEY, of any backend.
A function of `help-echo' is called as Emacs calls it."
  (save-excursion
    (goto-char (oref (drover-list--find-session key) start))
    (search-forward text (line-end-position))
    (let ((help (get-text-property (match-beginning 0) 'help-echo)))
      (if (functionp help) (funcall help nil (current-buffer) (match-beginning 0)) help))))

(ert-deftest drover-list-test-group-by-host ()
  "Grouped by host, this host comes first, then the others in order, each saying how it stands."
  (drover-list-test-with (list (drover-test-session "a"))
    (let ((drover-group-by 'host)
          (drover-hosts-function (lambda () '("zed" "core")))
          (drover-host-state-function (lambda (host) (and (equal host "core") "reconnecting")))
          (drover-host "here"))
      (drover-test-register-simple
       'remote:core (list (drover-session-create :backend 'remote:core :id '(fake . "c") :host "core"
                                                 :title "From core" :stale 1.0))
       :remote "core")
      (drover-test-register-simple
       'remote:zed (list (drover-session-create :backend 'remote:zed :id '(fake . "z") :host "zed"
                                                :title "From zed"))
       :remote "zed")
      (revert-buffer)
      (should (equal (drover-list-test-groups) '((nil "a") ("zed" (fake . "z")) ("core" (fake . "c")))))
      (should (string-match-p "\\`here" (drover-list-test-visible-text)))
      (should (string-match-p "core (not connected)" (drover-list-test-visible-text)))
      (should-not (string-match-p "zed (not connected)" (drover-list-test-visible-text))))))

;;;; The harness

(defconst drover-list-test-harnesses
  '((opencode :name "OpenCode" :icon "O" :description "OpenCode's service")
    (claude-code-ide :name "Claude IDE" :icon "C" :description "Claude Code in a terminal")
    (agent-shell :name "agent-shell" :icon "A" :description "a shell over ACP")
    (claude-code :name "Claude Code" :icon "T" :description "Claude Code's transcript")
    (codex :name "Codex" :icon "X" :description "Codex's rollout"))
  "Harnesses of readable icons, a character each.")

(defconst drover-list-test-harness-keys
  '((opencode . "o") (claude-code-ide . "i") (agent-shell . "s") (claude-code . "t") (codex . "x")
    (remote:core claude-code-ide . "r"))
  "The keys of the sessions of `drover-list-test-register-harnesses'.")

(defun drover-list-test-register-harnesses ()
  "Register a backend of each harness serving a session, and another host's.
The session of another host ran in a terminal of claude-code-ide there.
Their keys are those of `drover-list-test-harness-keys'."
  (cl-flet ((session (backend id title engine &rest properties)
              (apply #'drover-session-create :backend backend :id id :title title :status 'idle
                     :engine engine :engine-id id
                     :updated (drover-list-test-ago (* 60 (length title)))
                     properties)))
    (drover-test-register-simple 'opencode (list (session 'opencode "o" "Open" 'opencode)))
    (drover-test-register-simple 'claude-code-ide (list (session 'claude-code-ide "i" "Terminal" 'claude-code)))
    (drover-test-register-simple 'agent-shell (list (session 'agent-shell "s" "Shell" 'codex)))
    (drover-test-register-simple 'claude-code (list (session 'claude-code "t" "Transcript" 'claude-code)))
    (drover-test-register-simple 'codex (list (session 'codex "x" "Rollout" 'codex)))
    (drover-test-register-simple 'remote:core (list (session 'remote:core '(claude-code-ide . "r") "Remote"
                                                             'claude-code :host "core"
                                                             :origin 'claude-code-ide))
                                 :remote "core"))
  (drover-changed-now))

(defun drover-list-test-harness-help (key)
  "Return the tooltip of the harness of session KEY, of any backend."
  (save-excursion
    (goto-char (oref (drover-list--find-session key) start))
    (let ((match (text-property-search-forward 'help-echo nil
                                               (lambda (_ value)
                                                 (and (stringp value) (string-prefix-p "Harness: " value)))
                                               nil)))
      (prop-match-value match))))

(ert-deftest drover-list-test-harness-column ()
  "The harness's column tells what runs each session: its icon and name.
A session of another host is of its owner's harness, its host in the
host's column; the tooltip tells the engine of a harness running
several."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses drover-list-test-harnesses)
            (drover-sort-by 'title))
        (drover-list-test-register-harnesses)
        (drover-list-test-with-font
          (should (equal (seq-take (cdr (drover-list-test-layout-at 1000)) 3) '(title host harness)))
          (pcase-dolist (`(,key . ,text) '(((opencode . "o") . "Open +O OpenCode ")
                                           ((claude-code-ide . "i") . "Terminal +C Claude IDE ")
                                           ((agent-shell . "s") . "Shell +A agent-shell ")
                                           ((claude-code . "t") . "Transcript +T Claude Code ")
                                           ((codex . "x") . "Rollout +X Codex ")
                                           ((remote:core claude-code-ide . "r") . "Remote +core +C Claude IDE ")))
            (should (string-match-p text (drover-list-test-remote-line key))))
          (should (string-search "Harness" (drover-list-test-displayed header-line-format)))
          (should (equal (drover-list-test-harness-help '(agent-shell . "s"))
                         "Harness: agent-shell: a shell over ACP, running Codex"))
          (should (equal (drover-list-test-harness-help '(claude-code-ide . "i"))
                         "Harness: Claude IDE: Claude Code in a terminal"))
          (should (equal (drover-list-test-harness-help '(remote:core claude-code-ide . "r"))
                         "Harness: Claude IDE: Claude Code in a terminal, on core"))
          ;; A harness without an entry: its backend's label, after the
          ;; default icon.
          (let ((drover-harnesses (assq-delete-all 'codex (copy-sequence drover-list-test-harnesses))))
            (revert-buffer)
            (should (string-match-p "Rollout +▣ Codex " (drover-list-test-remote-line '(codex . "x"))))))))))

(ert-deftest drover-list-test-harness-provider-icon ()
  "A harness drawn with its vendor's logo follows `opencode-provider-icons', drawn anew."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses '((codex :name "Codex" :provider "openai")))
            (opencode-provider-icons '(("\\`openai\\'" . "Ω"))))
        (drover-list-test-register-harnesses)
        (revert-buffer)
        (should (string-match-p "Rollout +Ω Codex " (drover-list-test-remote-line '(codex . "x"))))
        (setq opencode-provider-icons '(("\\`openai\\'" . "Ø")))
        (revert-buffer)
        (should (string-match-p "Rollout +Ø Codex " (drover-list-test-remote-line '(codex . "x"))))))))

(ert-deftest drover-list-test-harness-icon-where-room-is-short ()
  "A column too narrow for its name shows the harness's icon alone, never a name cut.
The column is then as wide as the icon; in two lines, the icon alone
ends the title's line.  A session asks a question: the summary's
column, which keeps its room, gives none."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses drover-list-test-harnesses))
        (drover-list-test-register-harnesses)
        (drover-test-register-simple 'fake (list (drover-test-session "ask" :status 'waiting
                                                                      :requests '((:kind question :summary "Go?")))))
        (drover-list-test-with-font
          (let ((seen nil))
            (cl-loop for width from 1000 downto 150 by 10
                     do (pcase-let ((`(,layout . ,fields) (drover-list-test-layout-at width)))
                          (when (and (eq layout 'columns) (memq 'harness fields))
                            (let* ((column (assq 'harness (plist-get (drover-list--model-layout drover-list--model)
                                                                     :columns)))
                                   (shown (mapcar
                                           (lambda (key)
                                             (let* ((entry (alist-get (car key) drover-harnesses))
                                                    (icon (plist-get entry :icon))
                                                    (name (plist-get entry :name))
                                                    (line (drover-list-test-remote-line key)))
                                               (cond ((string-search (concat icon " " name) line) 'name)
                                                     ;; Never a name cut.
                                                     ((string-search (concat icon " " (substring name 0 2)) line)
                                                      'cut)
                                                     (t 'icon))))
                                           (butlast drover-list-test-harness-keys))))
                              (should-not (memq 'cut shown))
                              ;; All names or none.
                              (should (= (length (delete-dups (copy-sequence shown))) 1))
                              (push (car shown) seen)
                              (when (eq (car shown) 'icon)
                                ;; As wide as the icon, at least its :min of two characters.
                                (should (= (nth 2 column) 14)))))))
            (should (memq 'name seen))
            (should (memq 'icon seen)))
          ;; In two lines.
          (should (eq (car (drover-list-test-layout-at 150)) 'narrow))
          (should (string-match-p "\\` +· Open +4m +O\\'" (drover-list-test-remote-line '(opencode . "o")))))))))

(ert-deftest drover-list-test-harness-in-view-menu ()
  "The View menu's Columns switch the harness's column, -H."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses drover-list-test-harnesses))
        (drover-list-test-register-harnesses)
        (drover-list-test-shown
          (revert-buffer)
          (should (memq 'harness drover-list--fields))
          (should (string-search "O OpenCode" (drover-list-test-remote-line '(opencode . "o"))))
          (drover-list-test-type "V - H")
          (should (eq (oref transient--prefix command) 'drover-view))
          (should (equal drover-list-hidden-fields '(harness)))
          (should-not (memq 'harness drover-list--fields))
          (should-not (string-search "OpenCode" (drover-list-test-remote-line '(opencode . "o"))))
          (should-not (string-search "Harness" header-line-format))
          (drover-list-test-type "- H C-g")
          (should-not drover-list-hidden-fields)
          (should (memq 'harness drover-list--fields)))))))

(ert-deftest drover-list-test-group-by-harness ()
  "Grouped by harness, each group is named after its harness, behind its icon.
Its sessions do not repeat it; the filter keeps those of one harness."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses drover-list-test-harnesses)
            (drover-group-by 'harness))
        (drover-list-test-register-harnesses)
        (revert-buffer)
        (let ((groups (drover-list-test-groups)))
          (should (equal (sort (mapcar #'car groups) :key #'symbol-name)
                         '(agent-shell claude-code claude-code-ide codex opencode)))
          (should (equal (sort (alist-get 'claude-code-ide groups) :key #'prin1-to-string)
                         '("i" (claude-code-ide . "r")))))
        (let ((text (drover-list-test-visible-text)))
          (should (string-match-p "^C Claude IDE (2)$" text))
          (should (string-match-p "^O OpenCode (1)$" text))
          ;; Once, in the heading.
          (should (= (how-many "Claude IDE" (point-min) (point-max)) 1)))
        (drover-set-filter :harness 'codex)
        (revert-buffer)
        (should (equal (drover-list-test-order) '("x")))))))

;;;; The sidebar

(defun drover-list-test-key-cells (key)
  "Return the cells of the first line of the session KEY, of any backend.
As `drover-list-test-cells' returns them, a list of (X . TEXT), the
first at 0 when the line begins with text, as in the sidebar.  The
spaces stretched for a prompt cache's background, of no width, end no
cell."
  (drover-list--draw-deferred)
  (save-excursion
    (goto-char (oref (drover-list--find-session key) start))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
          (cells (list (cons 0 ""))))
      (dotimes (index (length line))
        (pcase (get-text-property index 'display line)
          ((guard (get-text-property index 'invisible line)))
          (`(space :align-to (,x)) (push (cons x "") cells))
          ('(space :width (0)))
          (_ (setcdr (car cells) (concat (cdar cells) (substring line index (1+ index)))))))
      (seq-remove (lambda (cell) (string-empty-p (cdr cell))) (nreverse cells)))))

(ert-deftest drover-list-test-sidebar-harness-ends-the-line ()
  "In the sidebar, the harness's icon ends each line, where the icons line up.
In two lines, at the end of the title's line, after the age, whatever
its width, in a cell as wide as the widest icon, the host's icon and the
context's pie before it, each in a cell of its own, which a line without
one leaves empty: the host's only on the line of a session of another
host, the pie only on that of a session whose context fills a known
part.  In columns, the harness in the last column, after the age's and
the prompt cache's, which a session without a live cache leaves empty;
the host's icon before the harness's, empty but for a session of
another host, whose host's name neither layout shows.  The positions
are the pixels lines align their parts to."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let ((drover-harnesses drover-list-test-harnesses))
        (drover-list-test-register-harnesses)
        ;; OpenCode's session has a live cache: its backend reports caches.
        (drover-test-register-simple
         'opencode (list (drover-session-create :backend 'opencode :id "o" :title "Open" :status 'idle
                                                :engine 'opencode :engine-id "o"
                                                :updated (drover-list-test-ago 240)
                                                :context-tokens 100000 :context-limit 200000
                                                :cache-expires (+ drover-test-now 200) :cache-ttl 300))
         :caches t)
        (let ((side (drover-list--show-side))
              (keys drover-list-test-harness-keys))
          (with-current-buffer (window-buffer side)
            (should (eq drover-list--layout 'narrow))
            (let* ((layout (drover-list--model-layout drover-list--model))
                   (edges (plist-get layout :edges))
                   (space (plist-get layout :space))
                   (harness-x (- (plist-get layout :width) (alist-get 'harness edges)))
                   (host-x (- harness-x space (alist-get 'host edges)))
                   (context-x (- host-x space (alist-get 'context edges))))
              (should (equal (mapcar #'car edges) '(harness host context)))
              ;; Ages of two and three characters: 4m, 10m.
              (should (string-match-p "\\`· Open +4m ▄ +O\\'" (drover-list-test-remote-line '(opencode . "o"))))
              (should (string-match-p "\\`· Transcript +10m +T\\'"
                                      (drover-list-test-remote-line '(claude-code . "t"))))
              (should (string-match-p "\\`· Remote +6m +@ C\\'"
                                      (drover-list-test-remote-line '(remote:core claude-code-ide . "r"))))
              (dolist (key keys)
                (let* ((cells (drover-list-test-key-cells key))
                       (text (lambda (x) (and (assoc x cells) (substring-no-properties (cdr (assoc x cells))))))
                       (age (car (last (seq-take-while (lambda (cell) (< (car cell) context-x)) cells)))))
                  (ert-info ((format "%S" key))
                    ;; Every icon begins at the pixel of its cell, past the age.
                    (should (equal (funcall text harness-x)
                                   (plist-get (alist-get (if (eq (car key) 'remote:core) 'claude-code-ide (car key))
                                                         drover-harnesses)
                                              :icon)))
                    (should (equal (funcall text host-x) (and (eq (car key) 'remote:core) "@")))
                    (should (equal (funcall text context-x) (and (equal key '(opencode . "o")) "▄")))
                    (should (equal (car (last cells)) (assoc harness-x cells)))
                    (should (<= (+ (car age) (string-pixel-width (cdr age))) (- context-x space)))))))
            (drover-list-test-is-fresh)
            ;; Wider, in columns: the host's and the harness's columns
            ;; follow the age's and the cache's, the summary's after them.
            (window-resize side 24 t nil t)
            (drover-list--on-window-change side)
            (should (eq drover-list--layout 'columns))
            (should (equal drover-list--fields '(title age cache host harness summary)))
            (should (string-match-p "\\`· Remote +6m +@ +C Claude IDE\\'"
                                    (drover-list-test-remote-line '(remote:core claude-code-ide . "r"))))
            (let* ((columns (plist-get (drover-list--model-layout drover-list--model) :columns))
                   (cache (assq 'cache columns))
                   (host (assq 'host columns))
                   (harness (assq 'harness columns)))
              (dolist (key keys)
                (let ((cells (drover-list-test-key-cells key)))
                  ;; Each icon in the harness's column, its own last cell.
                  (should (= (car (car (last cells))) (nth 1 harness)))
                  ;; The host only on the line of a session of another host.
                  (should (eq (and (assoc (nth 1 host) cells) t) (eq (car key) 'remote:core)))
                  ;; The cache only on the line of the session that has one.
                  (should (eq (and (assoc (nth 1 cache) cells) t) (equal key '(opencode . "o")))))))
            (drover-list-test-is-fresh)))))))

(defun drover-list-test-icon-face-p (key face)
  "Return non-nil when the host's icon on the line of session KEY is in FACE."
  (save-excursion
    (goto-char (oref (drover-list--find-session key) start))
    (search-forward "@" (line-end-position))
    (memq face (ensure-list (get-text-property (match-beginning 0) 'font-lock-face)))))

(ert-deftest drover-list-test-sidebar-host-icon-alone ()
  "The sidebar shows a session's host by its icon alone, which tells how the host stands.
In two lines and in columns, neither the host's name nor since when it
is not connected: the icon's tooltip tells both, and its face whether
the host is connected.  A host connecting and another leaving change
no field, cell nor column, drawn in place as anew; the list in an
ordinary window keeps the host's name and age."
  (drover-list-test-with nil
    (drover-list-test-showing
      (let* ((drover-harnesses drover-list-test-harnesses)
             (zed (list (drover-session-create :backend 'remote:zed :id '(claude-code-ide . "z") :title "Far"
                                               :status 'idle :engine 'claude-code :engine-id "z"
                                               :updated (drover-list-test-ago 180) :host "zed"
                                               :origin 'claude-code-ide :stale (drover-list-test-ago 300))))
             (core (list (drover-session-create :backend 'remote:core :id '(claude-code-ide . "r")
                                                :title "Remote" :status 'idle :engine 'claude-code
                                                :engine-id '(claude-code-ide . "r")
                                                :updated (drover-list-test-ago 360) :host "core"
                                                :origin 'claude-code-ide)))
             (far '(remote:zed claude-code-ide . "z"))
             (near '(remote:core claude-code-ide . "r"))
             (drover-host-state-function (lambda (host)
                                           (and (drover-session-stale (car (if (equal host "zed") zed core)))
                                                "reconnecting"))))
        (drover-list-test-register-harnesses)
        (drover-test-register-simple 'remote:core (lambda () core) :remote "core")
        (drover-test-register-simple 'remote:zed (lambda () zed) :remote "zed")
        (drover-changed-now)
        (let* ((side (drover-list--show-side))
               (sidebar (window-buffer side))
               (shape (lambda ()
                        (let ((layout (drover-list--model-layout drover-list--model)))
                          (list drover-list--layout drover-list--fields
                                (plist-get layout :edges) (plist-get layout :columns)))))
               (check
                (lambda (layout)
                  (should (eq drover-list--layout layout))
                  (let* ((model (drover-list--model-layout drover-list--model))
                         (x (if (eq layout 'narrow)
                                (let ((edges (plist-get model :edges)))
                                  (- (plist-get model :width) (alist-get 'harness edges)
                                     (plist-get model :space) (alist-get 'host edges)))
                              (nth 1 (assq 'host (plist-get model :columns))))))
                    (dolist (key (list near far))
                      (let ((line (drover-list-test-remote-line key))
                            (cells (drover-list-test-key-cells key)))
                        (ert-info ((format "%S %s" layout line))
                          (should-not (string-match-p "core\\|zed\\|5m" line))
                          ;; The icon in its cell, where every line places it.
                          (should (equal (substring-no-properties (cdr (assoc x cells))) "@")))))
                    (should-not (and header-line-format
                                     (string-search "Host" (drover-list-test-displayed header-line-format)))))))
               (swap (lambda ()
                       ;; The host not connected connects again, the other leaves.
                       (setq zed (list (let ((record (copy-sequence (car zed))))
                                         (setf (drover-session-stale record)
                                               (and (null (drover-session-stale record))
                                                    (drover-list-test-ago 300)))
                                         record))
                             core (list (let ((record (copy-sequence (car core))))
                                          (setf (drover-session-stale record)
                                                (and (null (drover-session-stale record))
                                                     (drover-list-test-ago 120)))
                                          record)))
                       (drover-backend-changed 'remote:zed)
                       (drover-backend-changed 'remote:core)
                       (drover-changed-now))))
          (with-current-buffer sidebar
            (dolist (width '(nil 24))
              (when width
                (window-resize side width t nil t)
                (drover-list--on-window-change side))
              (let ((layout (if width 'columns 'narrow)))
                (funcall check layout)
                (should (drover-list-test-icon-face-p near 'drover-dim))
                (should (drover-list-test-icon-face-p far 'drover-host-disconnected))
                (should (equal (drover-list-test-tooltip-at near "@") "core — connected"))
                (should (equal (drover-list-test-tooltip-at far "@") "zed — disconnected for 5m\nreconnecting"))
                (let ((before (funcall shape)))
                  (funcall swap)
                  (should (equal (funcall shape) before))
                  (funcall check layout)
                  (should (drover-list-test-icon-face-p near 'drover-host-disconnected))
                  (should (drover-list-test-icon-face-p far 'drover-dim))
                  (should (equal (drover-list-test-tooltip-at near "@") "core — disconnected for 2m\nreconnecting"))
                  (should (equal (drover-list-test-tooltip-at far "@") "zed — connected"))
                  (drover-list-test-is-fresh)
                  (funcall swap)
                  (should (equal (funcall shape) before))
                  (drover-list-test-is-fresh))))))
        ;; The list keeps the host's name, and its age while not connected.
        (drover-list-test-with-font
          (should (equal (seq-take (cdr (drover-list-test-layout-at 1000)) 2) '(title host)))
          (should (string-match-p "Far +zed 5m " (drover-list-test-remote-line far)))
          (should (string-match-p "Remote +core " (drover-list-test-remote-line near))))))))

(ert-deftest drover-list-test-send-keeps-the-fields ()
  "Sending a prompt changes no field either list shows, nor where lines place them.
The session asked a question, its prompt cache expired meanwhile; the
prompt answers it.  On its way, the options of the prompt change the
agent and the model, whose effort is not known until the catalogs come,
the session goes to work, without anything to tell until it answers,
the prompt arrives, its first answer makes a cache, and a subagent
starts.  The sidebar is wide enough for the title, the harness, the age
and the model, too narrow for the prompt cache besides them: when no
session needed you and no cache was live, as between the prompt sent
and its first answer, the model's column showed for a moment.  Each
step is drawn in place as a drawing anew draws it."
  (drover-list-test-with
      (list (drover-list-test-titled "s" "Sent to" :status 'waiting :since (drover-list-test-ago 900)
                                     :updated (drover-list-test-ago 900)
                                     :agent "build" :model "Opus" :effort "high"
                                     :requests '((:kind question :summary "Which branch?")))
            (drover-list-test-titled "q" "Quiet" :status 'idle :updated (drover-list-test-ago 3600)
                                     :agent "build" :model "Opus" :effort "high"))
    (drover-list-test-showing
      (drover-list-test-with-font
        ;; The fake backend reports prompt caches, as OpenCode's does.
        (apply #'drover-register-backend 'fake :caches t (alist-get 'fake drover--backends))
        (cl-letf (((symbol-function 'drover-list--line-width)
                   (lambda () (if drover-list--folds 600 240))))
          (drover-list-test-shown
            (revert-buffer)
            (let* ((side (window-buffer (drover-list--show-side)))
                   (lists (list buffer side))
                   ;; The sidebar's lines, in two, place the same fields
                   ;; alike; the list's columns follow the widths of
                   ;; their texts, as the model's name changes.
                   (shape (lambda (list)
                            (with-current-buffer list
                              (let ((layout (drover-list--model-layout drover-list--model)))
                                (list drover-list--layout drover-list--fields
                                      (and (eq list side) (plist-get layout :edges)))))))
                   (shapes (mapcar shape lists))
                   (step (lambda (change)
                           (let ((session (copy-sequence (drover-list-test-find "s"))))
                             (funcall change session)
                             (setq drover-test-sessions
                                   (cons session (seq-remove (lambda (old) (equal (drover-session-id old) "s"))
                                                             drover-test-sessions)))
                             (drover-backend-changed 'fake "s")
                             (drover-changed-now)
                             (dolist (list lists)
                               (should (equal (funcall shape list) (nth (seq-position lists list) shapes)))
                               (with-current-buffer list
                                 (drover-list-test-is-fresh)))))))
              (with-current-buffer side
                (should (eq drover-list--layout 'narrow))
                (should-not (memq 'model drover-list--fields)))
              (with-current-buffer buffer
                (should (eq drover-list--layout 'columns)))
              ;; The options of the prompt.
              (funcall step (lambda (session)
                              (setf (drover-session-agent session) "plan"
                                    (drover-session-model session) "Sonnet"
                                    (drover-session-effort session) nil)))
              ;; At work: the question answered, nothing to tell yet.
              (funcall step (lambda (session)
                              (setf (drover-session-status session) 'working
                                    (drover-session-requests session) nil
                                    (drover-session-since session) drover-test-now)))
              ;; The prompt arrives.
              (funcall step (lambda (session) (setf (drover-session-updated session) drover-test-now)))
              ;; The catalogs: the effort the model runs by default.
              (funcall step (lambda (session) (setf (drover-session-effort session) "medium")))
              ;; Its first answer: a cache, five minutes long.
              (funcall step (lambda (session)
                              (setf (drover-session-cache-expires session) (+ drover-test-now 300)
                                    (drover-session-cache-ttl session) 300)))
              ;; A subagent starts, under it.
              (push (drover-test-session "k" :parent "s" :status 'working :updated drover-test-now)
                    drover-test-sessions)
              (drover-backend-changed 'fake "k")
              (funcall step #'ignore)
              ;; The turn ends; the subagent's too.
              (setq drover-test-sessions (seq-remove (lambda (old) (equal (drover-session-id old) "k"))
                                                     drover-test-sessions))
              (drover-backend-changed 'fake "k")
              (funcall step (lambda (session)
                              (setf (drover-session-status session) 'done
                                    (drover-session-since session) drover-test-now))))))))))

(ert-deftest drover-list-test-sidebar-folds-nothing ()
  "The sidebar expands no session and collapses no group: no key, click nor indicator does.
The list beside it folds its own."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test-showing
      (let* ((drover-list-collapsed-groups '((status . idle)))
             (side (drover-list--show-side))
             (sidebar (window-buffer side)))
        (with-current-buffer sidebar
          (should (eq major-mode 'drover-list-sidebar-mode))
          (should-not drover-list--folds)
          ;; The idle group, collapsed by default elsewhere, shows.
          (should-not (drover-list-test-group-hidden-p 'idle))
          (should (string-match-p "Session idle" (drover-list-test-visible-text)))
          ;; Sessions without details to expand.
          (dolist (section (mapcan #'drover-list--session-sections (drover-list--containers)))
            (should-not (magit-section-content-p section))
            (should-not (oref section washer))
            (should-not (oref section hidden)))
          ;; No folding command does anything, on a session or a group.
          (with-selected-window side
            (drover-list-test-goto "ask")
            (dolist (key '("TAB" "C-<tab>" "M-<tab>" "<backtab>" "M-1" "M-4"))
              (should (eq (key-binding (kbd key)) 'ignore)))
            ;; The digits open sessions by number, as in the list.
            (dolist (key '("1" "4" "0"))
              (should (eq (key-binding (kbd key)) 'drover-list-open-numbered)))
            (drover-list-test-press "TAB")
            (should-not (string-match-p "Prompt" (drover-list-test-visible-text)))
            (drover-list-test-goto-group 'idle)
            (should (eq (key-binding (kbd "RET")) 'ignore))
            (should (eq (key-binding (kbd "<double-mouse-1>")) 'ignore))
            (drover-list-test-press "TAB")
            (drover-list-test-press "RET")
            (should-not (drover-list-test-group-hidden-p 'idle)))
          ;; Nor tells it could.
          (should-not magit-section-visibility-indicators)
          (should-not (seq-some (lambda (overlay) (overlay-get overlay 'magit-vis-indicator))
                                (overlays-in (point-min) (point-max))))
          ;; Drawn anew, the same.
          (revert-buffer)
          (should-not (drover-list-test-group-hidden-p 'idle))
          (drover-list-test-is-fresh))
        ;; The list folds as ever.
        (with-current-buffer buffer
          (revert-buffer)
          (should drover-list--folds)
          (should (drover-list-test-group-hidden-p 'idle))
          (should (eq (key-binding (kbd "TAB")) 'magit-section-toggle))
          (should (magit-section-content-p (drover-list-test-section "ask"))))))))

(ert-deftest drover-list-test-sidebar-lines-begin-with-glyphs ()
  "The sidebar's lines begin with their glyphs, without the invisible character of the list.
That character keeps the lines of a list that folds aligned as sections
fold; the sidebar folds nothing.  Nothing comes before the glyph of a
session at the top of its tree: neither a margin nor an aligned space."
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-showing
      (let ((drover-show-subagents 'all)
            (sidebar (window-buffer (drover-list--show-side))))
        (drover-view-settings-changed)
        (with-current-buffer sidebar
          (should-not (text-property-any (point-min) (point-max) 'invisible t))
          (dolist (section (mapcan #'drover-list--session-sections (drover-list--containers)))
            (let* ((start (oref section start))
                   (key (oref section value))
                   (subagent (drover-session-parent (drover-index-session key))))
              (should-not (get-text-property start 'invisible))
              (if subagent
                  ;; A subagent's glyph stands under its parent's title.
                  (should (eq (car-safe (get-text-property start 'display)) 'space))
                (should-not (get-text-property start 'display))
                (should (string-match-p "\\`[!✗✓↻◐·]" (drover-list-test-remote-line key)))))))
        (with-current-buffer buffer
          (revert-buffer)
          (dolist (section (mapcan #'drover-list--session-sections (drover-list--containers)))
            (should (get-text-property (oref section start) 'invisible))))))))

(ert-deftest drover-list-test-sidebar-window-without-fringes ()
  "The sidebar's side window has no fringes; the other windows keep theirs.
The sidebar shown in an ordinary window keeps its fringes too: they are
set on the side window alone, which a window showing another buffer
sets anew from it."
  (let ((fringes (make-hash-table :test #'eq))
        (set nil))
    (cl-letf (((symbol-function 'window-fringes)
               (lambda (&optional window)
                 (gethash (window-normalize-window window) fringes '(8 8 nil nil))))
              ((symbol-function 'set-window-fringes)
               (lambda (window left right &rest _)
                 (push (window-normalize-window window) set)
                 (puthash (window-normalize-window window) (list left right nil nil) fringes))))
      (drover-list-test-with (drover-list-test-side-sample)
        (pcase-let ((`(,first ,second) (drover-list-test--main)))
          (let ((side (drover-list-side)))
            (should (equal set (list side)))
            (should (equal (window-fringes side) '(0 0 nil nil)))
            (dolist (window (list first second))
              (should (equal (window-fringes window) '(8 8 nil nil))))
            ;; Shown again in its side window, as by a window change.
            (remhash side fringes)
            (drover-list--sidebar-shown side)
            (should (equal (window-fringes side) '(0 0 nil nil)))
            ;; Fringes off already: nothing is set again.
            (setq set nil)
            (drover-list--sidebar-shown side)
            (should-not set)
            ;; In an ordinary window, they stay.
            (set-window-buffer second (window-buffer side))
            (drover-list--sidebar-shown second)
            (should-not set)
            (should (equal (window-fringes second) '(8 8 nil nil)))
            ;; Nor does the list's window lose its own.
            (drover-list)
            (should-not set)))))))

(ert-deftest drover-list-test-sidebar-window-set-up-as-drawn ()
  "Drawing the sidebar gives its side window the sidebar's settings, if it lacks them.
A side window shown by an older drover, with fringes, gets them as the
sidebar draws, wholly or in place, or is brought up to date; the
sidebar's buffer in an ordinary window, never."
  (let ((fringes (make-hash-table :test #'eq))
        (set nil))
    (cl-letf (((symbol-function 'window-fringes)
               (lambda (&optional window)
                 (gethash (window-normalize-window window) fringes '(8 8 nil nil))))
              ((symbol-function 'set-window-fringes)
               (lambda (window left right &rest rest)
                 (push (cons (window-normalize-window window) rest) set)
                 (puthash (window-normalize-window window) (list left right nil nil) fringes))))
      (drover-list-test-with (drover-list-test-side-sample)
        (pcase-let ((`(,_first ,second) (drover-list-test--main)))
          (let* ((side (drover-list-side))
                 (sidebar (window-buffer side)))
            (cl-flet ((older ()
                        ;; As an older drover left it.
                        (puthash side '(8 8 nil nil) fringes)
                        (set-window-dedicated-p side nil)
                        (set-window-parameter side 'no-other-window nil)
                        (window-preserve-size side t nil)
                        (setq set nil))
                      (set-up-p ()
                        (and (equal (window-fringes side) '(0 0 nil nil))
                             (eq (window-dedicated-p side) t)
                             (eq (window-parameter side 'no-other-window) t)
                             (eq (window-parameter side 'no-delete-other-windows) t)
                             (window-preserved-size side t)
                             ;; On the window alone, not persistently.
                             (equal set (list (list side))))))
              (should (set-up-p))
              ;; Drawn anew.
              (older)
              (with-current-buffer sidebar
                (revert-buffer))
              (should (set-up-p))
              ;; Drawn in place.
              (older)
              (setf (drover-session-title (drover-list-test-find "done")) "Renamed")
              (drover-backend-changed 'fake "done")
              (drover-changed-now)
              (with-current-buffer sidebar
                (should (string-search "Renamed" (buffer-string))))
              (should (set-up-p))
              ;; Brought up to date, nothing having changed.
              (older)
              (with-current-buffer sidebar
                (drover-list--refresh-if-needed))
              (should (set-up-p))
              ;; Set up already: nothing is set again.
              (setq set nil)
              (with-current-buffer sidebar
                (revert-buffer))
              (should-not set)
              ;; The sidebar's buffer in an ordinary window keeps its
              ;; fringes as it draws, and the window its settings.
              (set-window-buffer second sidebar)
              (with-current-buffer sidebar
                (revert-buffer))
              (should-not set)
              (should (equal (window-fringes second) '(8 8 nil nil)))
              (should-not (window-dedicated-p second))
              (should-not (window-parameter second 'no-other-window)))))))))

(provide 'drover-list-test)
;;; drover-list-test.el ends here
