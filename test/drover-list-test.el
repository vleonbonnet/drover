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
  (drover-list--stop-timer 'drover-list--cache-timer))

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
           ;; the defaults.
           (drover-status-glyphs '((waiting . "!") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·")))
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

(defmacro drover-list-test-with-font (&rest body)
  "Run BODY with text measured in `drover-list-test-fake-width'.
Lines may take `drover-list-test-width' pixels."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'drover-list--pixel-width) #'drover-list-test-fake-width)
             ((symbol-function 'drover-list--line-width) (lambda () drover-list-test-width)))
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
VALUE is the value of the group, without its grouping."
  (mapcar (lambda (group)
            (cons (cdr (oref group value))
                  (mapcar (lambda (section) (cdr (oref section value)))
                          (oref group children))))
          (drover-list--groups)))

(defun drover-list-test-ungrouped ()
  "Return the identifiers of the sessions listed outside groups, in order."
  (mapcar (lambda (section) (cdr (oref section value)))
          (drover-list--session-sections magit-root-section)))

(defun drover-list-test-order ()
  "Return the identifiers of the sessions listed, in order, groups or not."
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

(defun drover-list-test-section (id)
  "Return the section of the session ID of the fake backend."
  (drover-list--find-session (cons 'fake id)))

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
each stretched space as many spaces as it is wide, and each character
displayed as a string that string: on a terminal, a pixel is a
character cell.  Aligned cells must not overlap."
  (let ((column 0)
        (chunks nil))
    (dotimes (index (length string))
      (let ((char (aref string index))
            (display (get-text-property index 'display string)))
        (pcase display
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
          (`(space :align-to (,x)) (push (cons x "") cells))
          (_ (setcdr (car cells) (concat (cdar cells) (substring line index (1+ index)))))))
      (nreverse cells))))

(defun drover-list-test-cell-at (id text)
  "Return the pixel position of the cell of session ID holding TEXT."
  (car (seq-find (lambda (cell) (string-search text (cdr cell)))
                 (drover-list-test-cells id))))

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
  (not (invisible-p (oref (drover-list-test-section id) start))))

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
      ;; The heading names the project: lines keep only the branch.
      (should (string-match-p "Session fail +main +5s" (drover-list-test-heading "fail")))
      (should-not (string-match-p "p:main" (drover-list-test-heading "fail")))
      (drover-set-group-by 'status)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting failed done working idle))))))

(ert-deftest drover-list-test-groups-by-other-fields ()
  (drover-list-test-with
      (list (drover-test-session "a" :status 'working :agent "build" :model "opus" :branch "main"
                                 :updated (drover-list-test-ago 60))
            (drover-test-session "b" :status 'waiting :agent "plan" :since (drover-list-test-ago 30)
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
    (should (equal (drover-list-test-ungrouped) '("root" "sub" "deep" "other" "idle")))
    (should (seq-every-p (lambda (section) (eq (oref section parent) magit-root-section))
                         (drover-list--session-sections magit-root-section)))
    (should (equal (drover-list-test-visible-text)
                   (concat "! 1 needs you   ✓ 1 done   5 sessions, 1 subagent hidden\n"
                           "\n"
                           "  ◐ Session root      10s\n"
                           "    ! Session sub      1m\n"
                           "      ✓ Session deep  30s\n"
                           "  ◐ Session other      5s\n"
                           "  · Session idle\n")))
    ;; Sessions still show their details.
    (drover-list-test-goto "idle")
    (drover-list-test-press "TAB")
    (should (string-match-p "^    Status +Idle$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-ungrouped-refresh-takes-next-session ()
  (drover-list-test-with (drover-list-test-sample)
    (setq drover-group-by nil)
    (revert-buffer)
    (should (equal (drover-list-test-ungrouped) '("wait1" "wait2" "done" "work" "idle")))
    (drover-list-test-goto "wait2")
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait2"))
                                           drover-test-sessions))
    (revert-buffer)
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
        (should (equal (drover-list-test-groups) '((nil "root" "sub" "deep" "other"))))
        (should (equal (drover-list--mode-line-process) " by project"))
        (should (equal (drover-list-test-id-at-point) "sub"))
        (should (= (- (point) (line-beginning-position)) offset))
        (drover-set-group-by nil)
        (should-not (drover-list-test-groups))
        (should (equal (drover-list-test-ungrouped) '("root" "sub" "deep" "other")))
        (should (equal (drover-list--mode-line-process) " ungrouped"))
        (should (equal (drover-list-test-id-at-point) "sub"))
        (should (= (- (point) (line-beginning-position)) offset))
        (drover-set-group-by 'status)
        (should (equal (drover-list-test-groups) '((waiting "root" "sub" "deep") (working "other"))))
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
      (setq drover-show-children t)
      (drover-set-sort 'title)
      (should (equal (drover-list-test-groups) '((waiting "w2" "kid1" "kid2" "w1") (idle "i"))))
      (should (equal (drover-list--mode-line-process) " by status, title ↑ +subagents"))
      (drover-set-sort 'title t)
      (should (equal (drover-list-test-groups) '((waiting "w1" "w2" "kid2" "kid1") (idle "i"))))
      (should (string-prefix-p "    · Session" (replace-regexp-in-string "beta" "Session"
                                                                         (drover-list-test-heading "kid2"))))
      (drover-set-sort 'urgency)
      (should (equal (drover-list-test-groups) '((waiting "w1" "w2" "kid2" "kid1") (idle "i")))))))

(ert-deftest drover-list-test-header ()
  (drover-list-test-with (append (drover-list-test-sample)
                                 (list (drover-test-session "fail" :status 'failed)
                                       (drover-test-session "sub" :parent "work")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line)
                   "! 2 needs you   ✗ 1 failed   ✓ 1 done   6 sessions, 1 subagent hidden"))
    (should (null (drover-list-test-id-at-point)))))

(ert-deftest drover-list-test-header-nothing-needs-you ()
  (drover-list-test-with (list (drover-test-session "w" :status 'working))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Nothing needs you   1 session"))))

(ert-deftest drover-list-test-header-flows-in-narrow-windows ()
  "In a narrow window, the parts of the header that do not fit go below."
  (drover-list-test-with (append (drover-list-test-sample)
                                 (list (drover-test-session "fail" :status 'failed)
                                       (drover-test-session "sub" :parent "work")))
    (drover-list-test-with-font
      ;; 200 pixels after the ellipsis: two counts, 154 pixels, fit but
      ;; not the third, 216; the session count alone, 183, does.
      (let ((drover-list-test-width 207))
        (revert-buffer)
        (goto-char (point-min))
        (let ((lines nil))
          (while (not (string-empty-p (drover-list-test-line)))
            (push (string-trim (drover-list-test-line)) lines)
            (forward-line 1))
          (should (equal (nreverse lines)
                         '("! 2 needs you   ✗ 1 failed" "✓ 1 done"
                           "6 sessions, 1 subagent hidden"))))))))

;;;; Subagents

(ert-deftest drover-list-test-subagents-under-parent ()
  (drover-list-test-with (drover-list-test-family)
    ;; The tree of root goes with its waiting subagent; quiet is hidden.
    (should (equal (drover-list-test-groups)
                   '((waiting "root" "sub" "deep") (working "other"))))
    ;; Each level is indented one step deeper, each with its own glyph;
    ;; ages line up past the deepest title.
    (should (equal (drover-list-test-heading "root") "  ◐ Session root      10s"))
    (should (equal (drover-list-test-heading "sub") "    ! Session sub      1m"))
    (should (equal (drover-list-test-heading "deep") "      ✓ Session deep  30s"))
    (should (equal (drover-list-test-heading "other") "  ◐ Session other      5s"))
    (let ((step (plist-get (drover-list--metrics 0) :step))
          (margin (car (car (drover-list-test-cells "root")))))
      (should (= margin (* drover-list--margin (drover-list--char-width))))
      (should (= (car (car (drover-list-test-cells "sub"))) (+ margin step)))
      (should (= (car (car (drover-list-test-cells "deep"))) (+ margin step step)))
      (should (= (car (car (drover-list-test-cells "other"))) margin)))
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
    ;; Deeper than three levels, subagents line up with the third.
    (should (equal (mapcar (lambda (depth) (car (car (drover-list-test-cells (format "d%d" depth)))))
                           (number-sequence 0 5))
                   '(2 4 6 8 8 8)))))

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
    ;; The header counts sessions, not trees.
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "! 2 needs you   ✓ 1 done   3 sessions"))
    (should (string-prefix-p "  ✓ Session parent" (drover-list-test-heading "parent")))))

(ert-deftest drover-list-test-orphan-subagent ()
  (drover-list-test-with (list (drover-test-session "child" :parent "gone" :status 'working))
    (should (equal (drover-list-test-groups) '((working "child"))))
    ;; Its parent is not listed: it shows at the top level, marked.
    (should (string-prefix-p "  ◐ ↳ Session child" (drover-list-test-heading "child")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Nothing needs you   1 session"))
    (drover-list-test-goto "child")
    (magit-section-show (magit-current-section))
    (should (string-match-p "^ +Subagent of +gone$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-subagents-toggle ()
  "U shows or hides subagents in every view, the list redrawn at once."
  (drover-list-test-with (list (drover-test-session "root" :status 'working)
                               (drover-test-session "child" :parent "root" :status 'working)
                               (drover-test-session "asking" :parent "root" :status 'waiting))
    (should (equal (drover-list-test-groups) '((waiting "root" "asking"))))
    (goto-char (point-min))
    (should (string-suffix-p "2 sessions, 1 subagent hidden" (drover-list-test-line)))
    (drover-list-test-goto "asking")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      (drover-list-test-shown
        (should (eq (key-binding (kbd "U")) #'drover-toggle-subagents))
        (drover-list-test-press "U")
        ;; The setting is global, not the list's own.
        (should-not (local-variable-p 'drover-show-children))
        (should drover-show-children)
        (with-temp-buffer
          (should drover-show-children))
        (should (equal (drover-list-test-groups) '((waiting "root" "asking" "child"))))
        (should (string-prefix-p "    ◐ Session child" (drover-list-test-heading "child")))
        (should (equal (drover-list--mode-line-process) " by status +subagents"))
        (should (equal (drover-list-test-id-at-point) "asking"))
        (should (= (- (point) (line-beginning-position)) offset))
        (save-excursion
          (goto-char (point-min))
          (should (string-suffix-p "   3 sessions" (drover-list-test-line))))
        (drover-list-test-press "U")
        (should-not drover-show-children)
        (should (equal (drover-list-test-groups) '((waiting "root" "asking"))))
        (should (equal (drover-list-test-id-at-point) "asking"))
        (should (= (- (point) (line-beginning-position)) offset))))))

(ert-deftest drover-list-test-parent-details-keep-subagents ()
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-list-test-section-hidden-p "root"))
    ;; The details come between the parent and its subagents.
    (should (string-match-p "  ◐ Session root +10s\n    Status +Working for 10s\n\\(.*\n\\)*    ! Session sub"
                            (drover-list-test-visible-text)))
    (should (< (oref (drover-list-test-section "root") end)
               (1+ (oref (drover-list-test-section "sub") start))))
    (drover-list-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should (drover-list-test-section-hidden-p "root"))
    ;; Hiding them leaves the subagents in sight.
    (should (drover-list-test-heading-visible-p "sub"))
    (should (drover-list-test-heading-visible-p "deep"))
    (should-not (string-match-p "Status" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-nested-collapse-state-across-refresh ()
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-goto "sub")
    (magit-section-show (magit-current-section))
    ;; The tree moves from Needs you to Failed.
    (setf (drover-session-status (drover-list-test-find "sub")) 'failed)
    (revert-buffer)
    (should (equal (drover-list-test-groups)
                   '((failed "root" "sub" "deep") (working "other"))))
    (should-not (drover-list-test-section-hidden-p "sub"))
    (should (drover-list-test-section-hidden-p "root"))
    (should (drover-list-test-section-hidden-p "deep"))
    (should (string-match-p "^      Status +Failed for 1m$" (drover-list-test-visible-text)))
    ;; And when the parent's details open and close around it.
    (drover-list-test-goto "root")
    (magit-section-show (magit-current-section))
    (drover-list-test-goto "root")
    (magit-section-hide (magit-current-section))
    (revert-buffer)
    (should-not (drover-list-test-section-hidden-p "sub"))))

(ert-deftest drover-list-test-refresh-follows-nested-session ()
  (drover-list-test-with (drover-list-test-family)
    (drover-list-test-goto "deep")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      ;; A new tree above, and deep's tree moves to Failed.
      (push (drover-test-session "new" :status 'waiting) drover-test-sessions)
      (setf (drover-session-status (drover-list-test-find "sub")) 'failed)
      (revert-buffer)
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
    (should (string-match-p "Session root +main\\'" (drover-list-test-heading "root")))
    (should (string-match-p "Session here +feat +10s\\'" (drover-list-test-heading "here")))
    ;; Another project than the group's is named.
    (should (string-match-p "Session elsewhere +q:dev +20s\\'" (drover-list-test-heading "elsewhere")))))

;;;; Lines

(ert-deftest drover-list-test-session-line ()
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 300)
                                 :project "drover" :branch "main"
                                 :requests '((:kind permission :summary "bash: make"))))
    (should (equal (drover-list-test-heading "s") "  ! Session s  drover:main  5m  permit: bash: make"))
    (drover-list-test-goto "s")
    (let ((glyph (1+ (point))))
      (should (equal (buffer-substring-no-properties glyph (1+ glyph)) "!"))
      (should (eq (get-text-property glyph 'font-lock-face) 'drover-waiting)))
    (search-forward "drover:main")
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

(ert-deftest drover-list-test-wide-glyph-deepens-step ()
  "A step of indentation is as wide as the widest glyph and a space."
  (drover-list-test-with (drover-list-test-family)
    (setq-local drover-status-glyphs (cons '(waiting . "待") drover-status-glyphs))
    (revert-buffer)
    (should (= (plist-get (drover-list--metrics 0) :step) 3))
    ;; A subagent's glyph never starts left of its parent's title.
    (should (string-prefix-p "  ◐ Session root" (drover-list-test-heading "root")))
    (should (string-prefix-p "     待 Session sub" (drover-list-test-heading "sub")))
    (should (string-prefix-p "        ✓ Session deep" (drover-list-test-heading "deep")))))

(ert-deftest drover-list-test-one-space-after-glyph ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-children t)
    (revert-buffer)
    (dolist (id '("root" "sub" "deep" "other" "quiet"))
      (let ((status (cdr (car (drover-list-test-cells id)))))
        ;; The glyph, one plain space, then the title.
        (should (string-match-p "\\`[!✓◐·] Session" status))
        (should-not (get-text-property 1 'display status))))))

(ert-deftest drover-list-test-columns-align ()
  (drover-list-test-with
      (list (drover-list-test-titled "a" "Short" :status 'working :project "p" :branch "b"
                                     :updated (drover-list-test-ago 5))
            (drover-list-test-titled "b" "A longer title" :status 'working :project "project"
                                     :updated (drover-list-test-ago 7200)
                                     :detail "Thinking"))
    (let ((a (drover-list-test-heading "a"))
          (b (drover-list-test-heading "b")))
      (should (string-prefix-p "  ◐ Short           p:b" a))
      (should (= (string-search "p:b" a) (string-search "project" b)))
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
        ;; Status glyphs: roots at the margin, the subagent one step in.
        (should (= (car (nth 1 (assoc "a" cells))) margin))
        (should (= (car (nth 1 (assoc "b" cells))) margin))
        (should (= (car (nth 1 (assoc "c" cells))) (+ margin (plist-get metrics :step))))
        ;; Locations begin at one position, past the widest title.
        (should (= (drover-list-test-cell-at "a" "iii") location))
        (should (= (drover-list-test-cell-at "b" "mm:x") location))
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
    (should (string-match-p "Two lines.*asks: First second\\'" (drover-list-test-heading "s")))))

;;;; Width

(defun drover-list-test-rich ()
  "Return two sessions with something to show in every field.
A works, its prompt cache four fifths full; B asks a question."
  (list (drover-list-test-titled "a" "Alpha" :status 'working :project "drover" :branch "main"
                                 :agent "build" :model "opus" :created (drover-list-test-ago 86400)
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
titles take 25 pixels, and each field its :min characters and a gap of
14 pixels, but the title; Magit's ellipsis takes 7 more."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      ;; Title 109, age 144, cache 221, summary 319, location 403,
      ;; agent 452, model 522, created 557.
      (should (equal (drover-list-test-layout-at 600)
                     '(columns title location agent model created age cache summary)))
      (should (equal (drover-list-test-layout-at 540)
                     '(columns title location agent model age cache summary)))
      (should (equal (drover-list-test-layout-at 470)
                     '(columns title location agent age cache summary)))
      (should (equal (drover-list-test-layout-at 420)
                     '(columns title location age cache summary)))
      (should (equal (drover-list-test-layout-at 330)
                     '(columns title age cache summary)))
      ;; Too narrow for the summary in a column: two lines.
      (should (equal (drover-list-test-layout-at 320) '(narrow title age cache summary)))
      (should (equal (drover-list-test-layout-at 120) '(narrow title age cache summary))))))

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
  "A field no session has anything for is left out, leaving its room."
  (drover-list-test-with (list (drover-list-test-titled "a" "Alpha" :status 'working :model "opus"
                                                        :updated (drover-list-test-ago 300)))
    (drover-list-test-with-font
      ;; Neither cache, summary, location nor agent: the model comes
      ;; after the age, 109 + 35 + 70.
      (should (equal (drover-list-test-layout-at 230) '(columns title model age)))
      (should (equal (drover-list-test-layout-at 200) '(columns title age))))))

(ert-deftest drover-list-test-fields-are-data ()
  "Priorities and minimum widths come from `drover-list-fields'."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (let ((drover-list-fields (append '((model :priority 1 :min 4))
                                        (assq-delete-all 'model (copy-alist drover-list-fields)))))
        ;; The model now comes second, and first in the line.
        ;; 109 + 42 + 35 + 77 + 98 = 361.
        (should (equal (drover-list-test-layout-at 370) '(columns model title age cache summary)))
        (should (equal (drover-list-test-layout-at 360) '(narrow title age cache summary)))
        (drover-list-test-layout-at 370)
        (let ((cells (drover-list-test-cells "a")))
          (should (equal (car cells) (cons 14 "opus")))
          ;; The title's column follows, past the widest model and a gap.
          (should (equal (nth 1 cells) (cons (+ 14 28 14) "◐ Alpha")))))
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
                                                            :requests '((:kind question :summary "Which branch?")))))
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
              (should (equal (drover-list-test-entry "root") (flush "  ◐ Session root" "10s")))
              (should (equal (drover-list-test-entry "sub") (flush "    ! Session sub" "1m")))
              ;; Only sessions that need you say why, and only on their
              ;; second line.
              (should (equal (drover-list-test-entry "deep") (flush "      ✓ Session deep" "30s"))))
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
            (should (string-match-p "Status" (drover-list-test-visible-text)))
            (drover-list-test-press "TAB")
            (should (string-match-p "asks: Which branch" (drover-list-test-visible-text)))
            (should-not (string-match-p "Status" (drover-list-test-visible-text)))))))))

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
        (let ((drover-list-test-width 250))
          (revert-buffer)
          (should (eq drover-list--layout 'narrow))
          ;; Lines end 243 pixels in, before Magit's ellipsis.
          (let* ((cells (drover-list-test-cells "long"))
                 (title-cell (car cells))
                 (age-cell (car (last cells)))
                 (shown (substring-no-properties (cdr title-cell) 2)))
            ;; The age, "2h", is 14 pixels: flush right.
            (should (equal (cdr age-cell) "2h"))
            (should (= (car age-cell) (- 243 14)))
            ;; The title ends in an ellipsis before it, with a space to
            ;; spare, and one more character would not fit.
            (should (string-suffix-p "…" shown))
            (let ((room (- 243 14 4 25))
                  (kept (substring shown 0 -1)))
              (should (<= (drover-list-test-fake-width shown) room))
              (should (> (drover-list-test-fake-width (concat (substring title 0 (1+ (length kept))) "…"))
                         room))
              (should (string-prefix-p kept title))))
          ;; The subagent's summary starts under its title, and fits.
          (save-excursion
            (drover-list-test-goto "kid")
            (forward-line 1)
            (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                   (summary (substring line 1)))
              (should (equal (get-text-property 0 'display line) '(space :align-to (36))))
              (should (string-suffix-p "…" summary))
              (should (<= (drover-list-test-fake-width summary) (- 243 36))))))))))

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

;;;; The prompt cache

(defun drover-list-test-cache-parts (id)
  "Return the parts of the prompt cache field of session ID, as an alist.
Each entry is (PART . DISPLAY), PART being `full', `trough', `space' or
`label'."
  (save-excursion
    (drover-list-test-goto id)
    (let ((end (line-end-position))
          (parts nil))
      (while (setq parts (let ((position (text-property-not-all (point) end 'drover-list-cache nil)))
                           (if (not position)
                               (throw 'done parts)
                             (goto-char (1+ position))
                             (cons (cons (get-text-property position 'drover-list-cache)
                                         (get-text-property position 'display))
                                   parts)))))
      parts)))

(defun drover-list-test-cache-field (id)
  "Return the prompt cache field of session ID as (FULL TROUGH LABEL)."
  (let ((parts (catch 'done (drover-list-test-cache-parts id))))
    (list (pcase (alist-get 'full parts) (`(space :width (,width)) width))
          (pcase (alist-get 'trough parts) (`(space :width (,width)) width))
          (alist-get 'label parts))))

(defun drover-list-test-cached ()
  "Return a session whose prompt cache lasts five minutes, four left, and two others."
  (list (drover-list-test-titled "c" "Cached" :status 'working :updated (drover-list-test-ago 60)
                                 :cache-expires (+ drover-test-now 240) :cache-ttl 300)
        (drover-list-test-titled "u" "Unknown" :status 'working :updated (drover-list-test-ago 90))
        (drover-list-test-titled "x" "Expired" :status 'working :updated (drover-list-test-ago 120)
                                 :cache-expires (- drover-test-now 10) :cache-ttl 300)))

(ert-deftest drover-list-test-cache-field ()
  "In columns, a live cache shows a short bar and the time left."
  (drover-list-test-with (drover-list-test-cached)
    (should (eq drover-list--layout 'columns))
    ;; A bar of five characters, four fifths full, and four minutes.
    (should (equal (drover-list-test-cache-field "c") '(4 1 "4m")))
    (should (memq 'cache drover-list--fields))
    ;; The title, the age, a gap, the bar, a space and the time left.
    (should (equal (drover-list-test-heading "c") "  ◐ Cached   1m        4m"))
    (save-excursion
      (drover-list-test-goto "c")
      (let ((full (text-property-any (point) (line-end-position) 'drover-list-cache 'full)))
        (should (eq (get-text-property full 'face) 'drover-cache-bar))
        (should (eq (get-text-property (1+ full) 'face) 'drover-cache-trough))))
    ;; Unknown or expired: nothing.
    (should (equal (drover-list-test-cache-field "u") '(nil nil nil)))
    (should (equal (drover-list-test-cache-field "x") '(nil nil nil)))
    (should (equal (drover-list-test-heading "u") "  ◐ Unknown  1m"))
    (should (= (length drover-list--caches) 1))
    ;; No live cache, no field.
    (setq drover-test-sessions (cdr drover-test-sessions))
    (revert-buffer)
    (should-not (memq 'cache drover-list--fields))
    (should-not (text-property-not-all (point-min) (point-max) 'drover-list-cache nil))
    (should-not drover-list--caches)))

(ert-deftest drover-list-test-cache-field-shrinks ()
  "As time passes, only the bar and the time left change, until nothing shows."
  (drover-list-test-with (drover-list-test-cached)
    (let ((root magit-root-section)
          (text (buffer-substring-no-properties (point-min) (point-max))))
      (setq drover-list-test-clock (+ drover-test-now 90))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(2 3 "2m")))
      (setq drover-list-test-clock (+ drover-test-now 200))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(1 4 "40s")))
      ;; Nothing was drawn anew: same sections, same text, unmodified.
      (should (eq magit-root-section root))
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) text))
      (should-not (buffer-modified-p))
      ;; Expired: nothing shows, and the cache is forgotten.
      (setq drover-list-test-clock (+ drover-test-now 240))
      (drover-list--draw-caches)
      (should (equal (drover-list-test-cache-field "c") '(0 0 "")))
      (should (equal (string-trim-right (drover-list-test-heading "c")) "  ◐ Cached   1m"))
      (should-not drover-list--caches))))

(defun drover-list-test-bar ()
  "Return the bar of the first cache drawn as a background, as (START END).
START and END bound its overlay."
  (when-let* ((entry (car drover-list--caches))
              (overlay (plist-get entry :overlay))
              ((overlay-buffer overlay)))
    (list (overlay-start overlay) (overlay-end overlay))))

(ert-deftest drover-list-test-cache-background ()
  "In two lines, a live cache colours its line from the left, shrinking."
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 60)
                                 :requests '((:kind question :summary "Go?"))
                                 :cache-expires (+ drover-test-now 300) :cache-ttl 300))
    (drover-list-test-with-font
      (let ((drover-list-test-width 300))
        (revert-buffer)
        (should (eq drover-list--layout 'narrow))
        (drover-list-test-goto "s")
        (let* ((start (point))
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
          (should (eq (overlay-get (plist-get (car drover-list--caches) :overlay) 'face) 'drover-cache-bar))
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
  (drover-list-test-with (drover-list-test-cached)
    (drover-list-test-goto "c")
    (magit-section-show (magit-current-section))
    (should (string-match-p "^    Prompt cache +4m left$" (drover-list-test-visible-text)))))

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
    (let ((side (drover-list--show-side)))
      (with-current-buffer (window-buffer side)
        (drover-list-test-measuring
          (revert-buffer)
          (should drover-list-test--measured)
          (dolist (measure drover-list-test--measured)
            (should (eq (nth 1 measure) side))))))))

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
  (drover-list-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-list-test-ago 120)
                                 :directory (expand-file-name "~/src/drover") :branch "main"
                                 :agent "build" :model "sonnet" :detail "Blocked"
                                 :created (drover-list-test-ago (* 3 86400))
                                 :requests '((:kind permission :summary "bash: make")
                                             (:kind question :summary "Which branch?"))))
    (should (drover-list-test-section-hidden-p "s"))
    (should-not (string-match-p "Directory" (drover-list-test-visible-text)))
    (drover-list-test-goto "s")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-list-test-section-hidden-p "s"))
    (let ((text (drover-list-test-visible-text)))
      ;; Labels under the title, values past the widest label.
      (dolist (line '("    Status      Needs you for 2m"
                      "    Directory   ~/src/drover"
                      "    Branch      main"
                      "    Agent       build"
                      "    Model       sonnet"
                      "    Created     3d ago"
                      "    Backend     Fake"
                      "    Detail      Blocked"
                      "    Permission  bash: make"
                      "    Question    Which branch?"))
        (should (string-match-p (concat "^" (regexp-quote line) "$") text))))
    (search-forward "Permission")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (search-forward "bash: make")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-waiting))
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
    (should (string-match-p "^      Subagent of  Parent work$" (drover-list-test-visible-text)))))

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
        (dolist (key '("RET" "o" "A" "s" "K" "z" "x" "v"))
          (drover-list-test-press key))))
    (should (equal (reverse drover-test-calls)
                   '((:open "s" nil) (:open "s" t) (:respond "s") (:mark-seen "s")
                     (:interrupt "s") (:archive "s") (:delete "s") (:preview "s"))))))

(ert-deftest drover-list-test-declined-confirmation ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (drover-list-test-goto "s")
      (dolist (key '("K" "z" "x"))
        (should (equal (should-error (drover-list-test-press key) :type 'user-error)
                       '(user-error "Abort")))))
    (should-not drover-test-calls)))

(ert-deftest drover-list-test-key-bindings ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (drover-list-test-goto "s")
    (should (eq (key-binding (kbd "RET")) #'drover-open-at-point))
    (should (eq (key-binding (kbd "C-o")) #'drover-list-display))
    (should (eq (key-binding (kbd "q")) #'drover-list-quit))
    (should (eq (key-binding (kbd "g")) #'drover-refresh))
    (should (eq (key-binding (kbd "c")) #'drover-open-subagent))
    (should (eq (key-binding (kbd "U")) #'drover-toggle-subagents))
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
    ;; RET, C-o and q are the only keys of the list's own.
    (let ((own nil))
      (map-keymap (lambda (event _) (push (key-description (vector event)) own))
                  (let ((map (copy-keymap drover-list-mode-map)))
                    (set-keymap-parent map nil)
                    map))
      (should (seq-set-equal-p own '("RET" "C-o" "q"))))
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
      (should (seq-set-equal-p keys '("o" "A" "K" "s" "u" "c" "r" "x" "z" "g" "+" "v" "U" "V"))))))

(ert-deftest drover-list-test-session-keys-typed ()
  "Typed through the command loop, each session key does its work.
Redraws keep point on the session."
  (drover-list-test-with
      (list (drover-test-session "root" :status 'working :updated (drover-list-test-ago 10))
            (drover-test-session "s" :parent "root" :status 'waiting :since (drover-list-test-ago 60)
                                 :directory "/work/s/" :branch "feature"
                                 :requests '((:kind question :summary "Which?")))
            (drover-test-session "kid" :parent "s" :status 'idle)
            (drover-test-session "other" :status 'working :updated (drover-list-test-ago 5)))
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
                (should (equal (type "o") '((:open "s" t))))
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
                (should-not (type "U"))
                (should drover-show-children)
                (should (equal (drover-list-test-groups)
                               '((waiting "root" "s" "kid") (failed "new") (working "other"))))
                ;; The View menu, its settings, and the filter it clears.
                (should (equal (type "V F C-g") '((:open "s" nil))))
                (should drover-follow-mode)
                (should-not (type "V F C-g"))
                (should-not drover-follow-mode)
                (should-not (type "V g nothing RET C-g"))
                (should-not drover-group-by)
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "new" "other")))
                (should-not (type "V o title RET r C-g"))
                (should (eq drover-sort-by 'title))
                (should drover-sort-descending)
                ;; Roots by title, descending: "Session root", "other", "new".
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "other" "new")))
                (should-not (type "V f feature RET C-g"))
                (should (equal drover-filter '(:search "feature")))
                (should (equal (drover-list-test-ungrouped) '("root" "s")))
                (save-excursion
                  (goto-char (point-min))
                  (forward-line 1)
                  (should (equal (drover-list-test-line) "Filter: \"feature\"   V c clears it")))
                (should-not (type "V c C-g"))
                (should-not drover-filter)
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "other" "new")))
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
    (drover-list-test-press "g")
    (should (equal (mapcar #'car (drover-list-test-groups)) '(done)))
    (setq drover-test-sessions (list (drover-test-session "s" :status 'idle)))
    (revert-buffer)
    (should (equal (mapcar #'car (drover-list-test-groups)) '(idle)))))

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

;;;; Refreshing

(ert-deftest drover-list-test-refresh-follows-session ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (forward-char 4)
    (let ((offset (- (point) (line-beginning-position))))
      ;; It moves from Working to the top of Needs you.
      (setf (drover-session-status (drover-list-test-find "work")) 'waiting)
      (setf (drover-session-since (drover-list-test-find "work")) (drover-list-test-ago 9999))
      (revert-buffer)
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
      (revert-buffer)
      (should (assq 'failed (drover-list-test-groups)))
      (should (equal (drover-list-test-id-at-point) "done"))
      (should (equal (drover-list-test-line) line)))))

(ert-deftest drover-list-test-refresh-same-group ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "wait1")
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait1"))
                                           drover-test-sessions))
    (revert-buffer)
    ;; The session that took its place in the group.
    (should (equal (drover-list-test-id-at-point) "wait2"))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait2"))
                                           drover-test-sessions))
    (revert-buffer)
    ;; The group is gone: point stays on the same line.
    (should (equal (drover-list-test-id-at-point) "done"))))

(ert-deftest drover-list-test-refresh-into-collapsed-group ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (setf (drover-session-status (drover-list-test-find "work")) 'idle)
    (revert-buffer)
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
    (revert-buffer)
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
      (revert-buffer)
      (should-not (assq 'done (drover-list-test-groups)))
      (setq drover-test-sessions sessions)
      (revert-buffer)
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
    (revert-buffer)
    (should (eq (gethash '(session fake . "work") drover-list--visibility) 'show))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "work"))
                                           drover-test-sessions))
    (revert-buffer)
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

(ert-deftest drover-list-test-stale-when-hidden ()
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (should-not (get-buffer-window buffer t))
    (let ((text (buffer-string)))
      (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
      (drover-backend-changed 'fake "s")
      (drover-changed-now)
      ;; Nothing was drawn.
      (should (equal (buffer-string) text))
      (should drover-list--stale))
    (should (memq #'drover-list--on-window-change
                  (buffer-local-value 'window-buffer-change-functions buffer)))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (drover-list--on-window-change (selected-window))
      (should-not drover-list--stale)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(done)))
      ;; Up to date: showing it again draws nothing.
      (let ((tick (buffer-modified-tick)))
        (drover-list--on-window-change (selected-window))
        (should (= tick (buffer-modified-tick)))))))

(ert-deftest drover-list-test-mode-line ()
  (drover-list-test-with nil
    (should (equal (drover-list--mode-line-process) " by status"))
    (setq drover-group-by 'project
          drover-show-children t)
    (should (equal (drover-list--mode-line-process) " by project +subagents"))
    (setq drover-group-by nil
          drover-sort-by 'created
          drover-filter '(:search "x"))
    (should (equal (drover-list--mode-line-process) " ungrouped, creation ↑ +subagents filtered"))))

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

(ert-deftest drover-list-test-filter-header ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (drover-list-test-shown
      (drover-set-filter :project "q")
      (should (equal (drover-list-test-groups) '((waiting "wait1") (working "work"))))
      (should (equal (drover-list-test-id-at-point) "work"))
      (should (equal (drover-list--mode-line-process) " by status filtered"))
      (goto-char (point-min))
      ;; The header counts what is listed, and names the filter.
      (should (equal (drover-list-test-line) "! 1 needs you   2 sessions"))
      (forward-line 1)
      (should (equal (drover-list-test-line) "Filter: project q   V c clears it"))
      (search-forward "V c")
      (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'help-key-binding))
      (should (null (drover-list-test-id-at-point)))
      (drover-set-filter :statuses '(working))
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (drover-list-test-line) "Filter: project q, working   V c clears it"))
      (drover-set-filter :statuses nil)
      (drover-set-filter :project nil)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting done working idle)))
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (drover-list-test-line) "")))))

(ert-deftest drover-list-test-filter-shows-matching-subagent ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-filter '(:search "quiet"))
    (revert-buffer)
    ;; Hidden subagents show when they match, under their parent.
    (should (equal (drover-list-test-groups) '((working "other" "quiet"))))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Nothing needs you   2 sessions"))))

(ert-deftest drover-list-test-filter-matches-nothing ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-shown
      (drover-set-filter :search "nowhere")
      (should-not (drover-list--groups))
      ;; The filter is named, with the keys clearing it.
      (should (equal (buffer-string)
                     (concat "No session matches the filter: \"nowhere\".\n"
                             "V c clears the filter.\n")))
      (drover-filter-clear)
      (should (equal (mapcar #'car (drover-list-test-groups)) '(waiting done working idle))))))

(ert-deftest drover-list-test-filter-hint-follows-keymap ()
  "The keys the list names are those the user bound."
  (drover-list-test-with (list (drover-test-session "s"))
    (unwind-protect
        (progn
          (keymap-unset drover-session-map "V" t)
          (keymap-set drover-session-map "C-c v" #'drover-view)
          (setq drover-filter '(:search "nowhere"))
          (revert-buffer)
          (should (equal (buffer-string)
                         (concat "No session matches the filter: \"nowhere\".\n"
                                 "C-c v c clears the filter.\n"))))
      (keymap-unset drover-session-map "C-c v" t)
      (keymap-unset drover-session-map "C-c" t)
      (keymap-set drover-session-map "V" #'drover-view))
    (should (eq (keymap-lookup drover-session-map "V") #'drover-view))
    (should-not (keymap-lookup drover-session-map "C-c"))))

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
      ;; Hidden, a tick marks it stale and stops.
      (drover-list--age-tick)
      (should drover-list--stale)
      (should-not drover-list--age-timer)
      ;; Shown again, it redraws and runs again.
      (save-window-excursion
        (set-window-buffer (selected-window) buffer)
        (drover-list--on-window-change (selected-window))
        (should-not drover-list--stale)
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
  "Return sessions of every status, the clock-relative ones of the sidebar."
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
    (kill-buffer buffer)
    (let ((origin (selected-window)))
      (drover-list-side)
      (let ((window (drover-list--side-window)))
        (should (window-live-p window))
        (should (eq (selected-window) origin))
        (should (equal (buffer-name (window-buffer window)) "*drover sessions*"))
        (should (eq (window-parameter window 'window-side) 'left))
        (should (eql (window-parameter window 'window-slot) 0))
        (should (window-parameter window 'no-delete-other-windows))
        (should (window-parameter window 'no-other-window))
        (should (eq (window-dedicated-p window) t))
        (should (= (window-total-width window) 36))
        (should (window-preserved-size window t))
        (should (member '(:start nil) drover-test-calls))
        (with-current-buffer (window-buffer window)
          (should (eq major-mode 'drover-list-mode))
          ;; Too narrow for columns: two lines where a session needs you.
          (should (eq drover-list--layout 'narrow))
          ;; Point starts on the first session.
          (goto-char (window-point window))
          (should (equal (drover-list-test-id-at-point) "ask"))
          (should-not drover-list--stale)))
      (drover-list-side)
      (should-not (drover-list--side-window))
      (should (get-buffer "*drover sessions*"))
      (should (eq (selected-window) origin)))))

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
      (should (equal (buffer-name (window-buffer side)) "*drover sessions*"))
      (should-not (eq (get-buffer-window "*other 1*") side)))))

(ert-deftest drover-list-test-side-select-back-and-forth ()
  (drover-list-test-with (drover-list-test-side-sample)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (drover-list-side '(4))
      (should (eq (selected-window) (drover-list--side-window)))
      (should (eq (current-buffer) (get-buffer "*drover sessions*")))
      (drover-list-side '(4))
      (should (eq (selected-window) first))
      (drover-list-side '(4))
      (should (eq (selected-window) (drover-list--side-window)))
      ;; q hides it.
      (call-interactively (key-binding (kbd "q")))
      (should-not (drover-list--side-window))
      (should (eq (selected-window) first)))))

(ert-deftest drover-list-test-side-and-ordinary-window ()
  "The list moves between an ordinary window and the side window."
  (drover-list-test-with (drover-list-test-side-sample)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list)
      (should (eq (window-buffer first) buffer))
      (should (eq drover-list--layout 'columns))
      ;; Shown on the side, it leaves the ordinary window.
      (drover-list-side)
      (should (drover-list--side-window))
      (should (equal (get-buffer-window-list buffer) (list (drover-list--side-window))))
      (with-current-buffer buffer
        (should (eq drover-list--layout 'narrow)))
      (should (equal (buffer-name (window-buffer second)) "*other 2*"))
      ;; Listed in an ordinary window again, it leaves the side.
      (drover-list)
      (should-not (drover-list--side-window))
      (should (eq (window-buffer (selected-window)) buffer))
      (should-not (window-parameter (selected-window) 'window-side))
      (should (eq drover-list--layout 'columns))
      ;; q quits the ordinary window as usual.
      (call-interactively (key-binding (kbd "q")))
      (should-not (get-buffer-window buffer)))))

(ert-deftest drover-list-test-side-narrow-lines ()
  (drover-list-test-with (drover-list-test-side-sample)
    (with-current-buffer (window-buffer (drover-list--show-side))
      ;; 36 columns: 33 of text, then the column of the continuation
      ;; glyph and Magit's ellipsis; the divider takes the last.
      (cl-flet ((flush (left right) (concat left (make-string (- 33 (length left) (length right)) ?\s)
                                            right)))
        (should (equal (drover-list-test-entry "ask")
                       (concat (flush "  ! Session ask" "3m") "\n    asks: Which branch?")))
        (should (equal (drover-list-test-entry "fail")
                       (concat (flush "  ✗ Session fail" "30s") "\n    Rate limited")))
        (should (equal (drover-list-test-entry "work") (flush "  ◐ Session work" "1m")))))))

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
        (should (eq (window-buffer side) buffer))))))

(ert-deftest drover-list-test-side-o-opens-in-main-window ()
  "From the side window, the other window o opens a session in is the main window."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (let ((side (drover-list-side '(4))))
        (drover-list-test-goto "done")
        (execute-kbd-macro (kbd "o"))
        (should (equal (car drover-test-calls) (list :open "done" nil first)))
        (should (eq (selected-window) first))
        (should (equal (buffer-name (window-buffer first)) "*session done*"))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        (should (eq (window-buffer side) buffer))
        (should (= (length (window-list nil 'nomini)) 3))))))

(ert-deftest drover-list-test-ordinary-window-opens-as-usual ()
  "From an ordinary window, RET opens in the same window and o in another."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (drover-list)
      (should-not (drover-list--target-window))
      (drover-list-test-goto "done")
      (execute-kbd-macro (kbd "RET"))
      (should (equal (car drover-test-calls) (list :open "done" nil first)))
      (should (equal (buffer-name (window-buffer first)) "*session done*")))))

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
        ;; The session it shows is highlighted at once.
        (should (equal (drover-list-test-current) '("work")))))))

(ert-deftest drover-list-test-open-unsupported ()
  (drover-list-test-with nil
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "x"
                                                                               :status 'working))))
    (drover-list-test--main)
    (let ((side (drover-list-side '(4))))
      (goto-char (oref (drover-list--find-session '(bare . "x")) start))
      (should-error (execute-kbd-macro (kbd "RET")) :type 'user-error)
      (should-error (execute-kbd-macro (kbd "C-o")) :type 'user-error)
      (should (eq (selected-window) side)))))

(ert-deftest drover-list-test-side-follow-mode ()
  "Follow mode shows the session at point in the main window as point moves."
  (drover-list-test-with (drover-list-test-side-sample)
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
        (execute-kbd-macro (kbd "V F C-g"))
        (should drover-follow-mode)
        (should (equal (car drover-test-calls) (list :open "ask" nil first)))
        (should (equal (funcall shown) "*session ask*"))
        ;; A group heading shows nothing.
        (execute-kbd-macro (kbd "n"))
        (should-not (drover-list-test-id-at-point))
        (should (= (funcall opened) 1))
        (execute-kbd-macro (kbd "n"))
        (should (equal (funcall shown) "*session fail*"))
        ;; Within a session, on its second line, nothing is shown again.
        (execute-kbd-macro (kbd "C-n"))
        (should (equal (drover-list-test-id-at-point) "fail"))
        (should (= (funcall opened) 2))
        (execute-kbd-macro (kbd "n n"))
        (should (equal (funcall shown) "*session done*"))
        (execute-kbd-macro (kbd "p p"))
        (should (equal (funcall shown) "*session fail*"))
        (should (eq (selected-window) side))
        (should (equal (buffer-name (window-buffer second)) "*other 2*"))
        ;; The main window stays the same, and its session is highlighted.
        (should (eq (drover-list--main-window) first))
        (drover-list--windows-changed)
        (should (equal (drover-list-test-current) '("fail")))
        (execute-kbd-macro (kbd "V F C-g"))
        (should-not drover-follow-mode)
        (execute-kbd-macro (kbd "n"))
        (should (equal (funcall shown) "*session fail*"))))))

;;;; The session in the main window

(defun drover-list-test-current ()
  "Return the sessions highlighted as shown in the main window."
  (mapcar (lambda (overlay)
            (save-excursion
              (goto-char (overlay-start overlay))
              (drover-list-test-id-at-point)))
          drover-list--current-overlays))

(ert-deftest drover-list-test-highlights-current-session ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (let ((side (drover-list--show-side)))
        (with-current-buffer (window-buffer side)
          (should-not drover-list--current-overlays)
          (set-window-buffer first (get-buffer-create "*session work*"))
          (drover-list--windows-changed)
          (let ((overlay (car drover-list--current-overlays))
                (section (drover-list-test-section "work")))
            (should (= (length drover-list--current-overlays) 1))
            (should (eq (overlay-get overlay 'face) 'drover-list-current))
            (should (eq (overlay-get overlay 'window) side))
            (should (equal (drover-list-test-current) '("work")))
            ;; Its heading, one line.
            (should (= (overlay-start overlay) (oref section start)))
            (should (= (overlay-end overlay) (oref section content))))
          ;; A session with a second line is highlighted whole.
          (set-window-buffer first (get-buffer-create "*session ask*"))
          (drover-list--windows-changed)
          (let ((overlay (car drover-list--current-overlays)))
            (should (equal (drover-list-test-current) '("ask")))
            (should (= (overlay-end overlay)
                       (save-excursion (goto-char (overlay-start overlay)) (forward-line 2) (point)))))
          ;; The highlight survives redraws.
          (drover-list--render)
          (should (equal (drover-list-test-current) '("ask")))
          (set-window-buffer first (get-buffer "*other 1*"))
          (drover-list--windows-changed)
          (should-not drover-list--current-overlays))))))

(ert-deftest drover-list-test-highlights-in-ordinary-window ()
  "In an ordinary window, the session of the window beside it is highlighted."
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,second) (drover-list-test--main)))
      (drover-list)
      (should (eq (window-buffer first) buffer))
      (set-window-buffer second (get-buffer-create "*session done*"))
      (drover-list--windows-changed)
      (should (equal (drover-list-test-current) '("done"))))))

(ert-deftest drover-list-test-highlights-nested-session ()
  (drover-list-test-with
      (list (drover-test-session "busy" :status 'working)
            (drover-test-session "parent" :status 'done :since (drover-list-test-ago 50))
            (drover-test-session "asking" :parent "parent" :status 'waiting
                                 :since (drover-list-test-ago 30)
                                 :requests '((:kind question :summary "Which file?"))))
    (drover-list-test--register-buffers)
    (pcase-let ((`(,first ,_second) (drover-list-test--main)))
      (with-current-buffer (window-buffer (drover-list--show-side))
        ;; The parent alone, not the subagent below it.
        (set-window-buffer first (get-buffer-create "*session parent*"))
        (drover-list--windows-changed)
        (let ((overlay (car drover-list--current-overlays)))
          (should (= (overlay-start overlay) (oref (drover-list-test-section "parent") start)))
          (should (= (overlay-end overlay) (oref (drover-list-test-section "asking") start))))
        ;; The subagent with its second line.
        (set-window-buffer first (get-buffer-create "*session asking*"))
        (drover-list--windows-changed)
        (let ((overlay (car drover-list--current-overlays)))
          (should (= (overlay-start overlay) (oref (drover-list-test-section "asking") start)))
          (should (= (overlay-end overlay)
                     (save-excursion (goto-char (overlay-start overlay)) (forward-line 2) (point)))))))))

(ert-deftest drover-list-test-no-highlight-without-buffers ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-test--main)
    (with-current-buffer (window-buffer (drover-list--show-side))
      (drover-list--windows-changed)
      (should-not drover-list--current-overlays))))

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

(ert-deftest drover-list-test-side-stale-while-hidden ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-side)
    (drover-list-side)
    (should-not (drover-list--side-window))
    (push (drover-test-session "new" :status 'working) drover-test-sessions)
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (should drover-list--stale)
    (should-not (drover-list-test-section "new"))
    (drover-list-side)
    (with-current-buffer buffer
      (should-not drover-list--stale)
      (should (drover-list-test-section "new")))))

(ert-deftest drover-list-test-side-shown-again-keeps-point ()
  (drover-list-test-with (drover-list-test-side-sample)
    (drover-list-side)
    (set-window-point (drover-list--side-window)
                      (save-excursion (drover-list-test-goto "work") (point)))
    (drover-list-side)
    (drover-list-side)
    (goto-char (window-point (drover-list--side-window)))
    (should (equal (drover-list-test-id-at-point) "work"))))

(ert-deftest drover-list-test-side-width-change ()
  (drover-list-test-with (list (drover-test-session "a" :status 'waiting :since (drover-list-test-ago 60)
                                                    :requests '((:kind question :summary "Go?"))))
    (let* ((side (drover-list--show-side))
           (before (window-max-chars-per-line side)))
      (should (= drover-list--width before))
      ;; Two lines: the summary does not fit in a column.
      (should (eq drover-list--layout 'narrow))
      (should (string-suffix-p "1m" (drover-list-test-heading "a")))
      (should (= (string-width (drover-list-test-heading "a")) (1- before)))
      (window-resize side 6 t nil t)
      (should (= (window-max-chars-per-line side) (+ before 6)))
      (drover-list--on-window-change side)
      (should (= drover-list--width (+ before 6)))
      ;; Now it does: one line, in columns.
      (should (eq drover-list--layout 'columns))
      (should (equal (drover-list-test-entry "a") "  ! Session a  1m  asks: Go?")))))

(provide 'drover-list-test)
;;; drover-list-test.el ends here
