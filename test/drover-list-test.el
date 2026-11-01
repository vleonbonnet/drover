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
  (drover-list--stop-timer 'drover-list--frame-timer))

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
           (drover-status-glyphs '((waiting . "!") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·")))
           (drover-subagent-glyphs '((waiting . "?") (working . "*")))
           (drover-animations '((working "1" "2" "3") (retrying "a" "b")))
           (drover-list-animation-interval nil)
           (drover-list-hidden-fields nil)
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
                   (concat "! 1 needs you   4 sessions, 2 subagents hidden\n"
                           "\n"
                           "  ◐ ? Session root   10s\n"
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

(ert-deftest drover-list-test-header ()
  (drover-list-test-with (append (drover-list-test-sample)
                                 (list (drover-test-session "fail" :status 'failed)
                                       (drover-test-session "sub" :parent "work")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line)
                   "! 2 needs you   ✗ 1 failed   ✓ 1 done   6 sessions, 1 subagent hidden"))
    (should (null (drover-list-test-id-at-point)))
    ;; Which subagents show is the View menu's to tell, not the list's.
    (forward-line 1)
    (should (equal (drover-list-test-line) ""))))

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
    ;; The header counts sessions, not trees.
    (goto-char (point-min))
    (should (equal (drover-list-test-line)
                   "! 2 needs you   ✓ 1 done   3 sessions"))
    (should (string-prefix-p "  ✓ ? Session parent" (drover-list-test-heading "parent")))))

(ert-deftest drover-list-test-orphan-subagent ()
  (drover-list-test-with (list (drover-test-session "child" :parent "gone" :status 'working))
    (should (equal (drover-list-test-groups) '((working "child"))))
    ;; Its parent is not listed: it shows at the top level, marked.
    (should (string-prefix-p "  ◐ ↳ Session child" (drover-list-test-heading "child")))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Nothing needs you   1 session"))
    (drover-list-test-goto "child")
    (magit-section-show (magit-current-section))
    ;; Under its title, the arrow included.
    (should (string-match-p "^    Prompt +Prompt of child$" (drover-list-test-visible-text)))))

(ert-deftest drover-list-test-subagents-toggle ()
  "U cycles every view through active, no and all subagents, redrawn at once."
  (drover-list-test-with (list (drover-test-session "root" :status 'working)
                               (drover-test-session "child" :parent "root" :status 'working)
                               (drover-test-session "ended" :parent "root" :status 'done
                                                    :since (drover-list-test-ago 5))
                               (drover-test-session "asking" :parent "root" :status 'waiting))
    ;; Active subagents only: the one that ended is hidden, and does not
    ;; place its parent among the finished ones.
    (should (equal (drover-list-test-groups) '((waiting "root" "asking" "child"))))
    (goto-char (point-min))
    (should (string-suffix-p "   3 sessions, 1 subagent hidden" (drover-list-test-line)))
    (drover-list-test-goto "root")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      (drover-list-test-shown
        (should (eq (key-binding (kbd "U")) #'drover-toggle-subagents))
        (drover-list-test-press "U")
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
          (should (equal (drover-list-test-line) "Nothing needs you   1 session, 3 subagents hidden"))
          (forward-line 1)
          (should (equal (drover-list-test-line) "")))
        (drover-list-test-press "U")
        (should (eq drover-show-subagents 'all))
        (should (equal (drover-list-test-groups) '((waiting "root" "asking" "ended" "child"))))
        (should (string-prefix-p "        ◐ Session child" (drover-list-test-heading "child")))
        (should (equal (drover-list--mode-line-process) " by status +subagents"))
        (should (equal (drover-list-test-id-at-point) "root"))
        (save-excursion
          (goto-char (point-min))
          (should (string-suffix-p "   4 sessions" (drover-list-test-line))))
        (drover-list-test-press "U")
        (should (eq drover-show-subagents 'active))
        (should (equal (drover-list-test-groups) '((waiting "root" "asking" "child"))))
        (should (equal (drover-list-test-id-at-point) "root"))
        (should (= (- (point) (line-beginning-position)) offset))))))

(ert-deftest drover-list-test-no-subagent-no-hint ()
  "Without subagents, the header says nothing of them."
  (drover-list-test-with (list (drover-test-session "s" :status 'working))
    (goto-char (point-min))
    (should (equal (drover-list-test-line) "Nothing needs you   1 session"))
    (forward-line 1)
    (should (equal (drover-list-test-line) ""))))

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
    (revert-buffer)
    (drover-list-test-goto "sub")
    (magit-section-show (magit-current-section))
    ;; The tree moves from Needs you to Failed.
    (setf (drover-session-status (drover-list-test-find "sub")) 'failed)
    (revert-buffer)
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
    (revert-buffer)
    (should-not (drover-list-test-section-hidden-p "sub"))))

(ert-deftest drover-list-test-refresh-follows-nested-session ()
  (drover-list-test-with (drover-list-test-family)
    (setq drover-show-subagents 'all)
    (revert-buffer)
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
    (should (equal (drover-list-test-heading "s") "  ! Session s  drover:main   5m  permit: bash: make"))
    (drover-list-test-goto "s")
    ;; Past the invisible character beginning the line, and its aligned space.
    (let ((glyph (+ 2 (point))))
      (should (get-text-property (point) 'invisible))
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

(ert-deftest drover-list-test-wide-glyph-widens-every-cell ()
  "Every status glyph takes the room of the widest, so that titles line up."
  (drover-list-test-with (drover-list-test-family)
    (setq-local drover-status-glyphs (cons '(waiting . "待") drover-status-glyphs))
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
        ;; Status glyphs: roots at the margin, the subagent under its
        ;; parent's title.
        (should (= (car (nth 1 (assoc "a" cells))) margin))
        (should (= (car (nth 1 (assoc "b" cells))) margin))
        (should (= (car (nth 1 (assoc "c" cells))) (drover-list-test-title-x "b" "iiiiiiii")))
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
titles take 25 pixels, and each field its :min characters and a gap of
14 pixels, but the title; Magit's ellipsis takes 7 more."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      ;; Title 109, age 144, cache 221, summary 319, model 389, context
      ;; 466, provider 494, location 578, agent 627, created 662.
      (should (equal (drover-list-test-layout-at 670)
                     '(columns title location agent model provider context created age cache summary)))
      (should (equal (drover-list-test-layout-at 660)
                     '(columns title location agent model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 600)
                     '(columns title location model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 580)
                     '(columns title model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at 500)
                     '(columns title model context age cache summary)))
      (should (equal (drover-list-test-layout-at 470)
                     '(columns title model age cache summary)))
      (should (equal (drover-list-test-layout-at 330)
                     '(columns title age cache summary)))
      ;; Windows of 75 and 100 columns as wide as an x, as the README says.
      (should (equal (drover-list-test-layout-at (* 75 7))
                     '(columns title model provider context age cache summary)))
      (should (equal (drover-list-test-layout-at (* 100 7))
                     '(columns title location agent model provider context created age cache summary)))
      ;; Too narrow for the summary in a column: two lines, where
      ;; neither the model, its provider nor the context shows.
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
        (let ((drover-list-test-width 250))
          (revert-buffer)
          (should (eq drover-list--layout 'narrow))
          ;; Lines end 243 pixels in, before Magit's ellipsis.
          (let* ((cells (drover-list-test-cells "long"))
                 (title-cell (car cells))
                 (age-cell (car (last cells)))
                 (shown (substring-no-properties (cdr title-cell) 4)))
            ;; The age, "2h", is 14 pixels: flush right.
            (should (equal (cdr age-cell) "2h"))
            (should (= (car age-cell) (- 243 14)))
            ;; The title ends in an ellipsis before it, with a space to
            ;; spare, and one more character would not fit.  It begins
            ;; past the glyph of the subagent waiting, 25 + 11 pixels in.
            (should (string-suffix-p "…" shown))
            (let ((room (- 243 14 4 36))
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
              (should (<= (drover-list-test-fake-width summary) (- 243 47))))))))))

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
        (should (eq (get-text-property effort 'font-lock-face) 'drover-prompt-picked))
        (should (equal (get-text-property effort 'help-echo) "Effort: high, chosen"))
        ;; The session's tooltip names them too.
        (should (equal (get-text-property model 'help-echo) "Alpha\nOpus 5.5 high"))))
    (save-excursion
      (drover-list-test-goto "b")
      (search-forward "GPT-5.5 medium")
      (let ((effort (- (point) 6)))
        (should (eq (get-text-property effort 'font-lock-face) 'drover-prompt-inherited))
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
         (bar (eq (get-text-property 1 'font-lock-face text) 'drover-cache-trough)))
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
        (should (eq (plist-get a :bar-face) 'drover-context-bar))
        ;; Near the tier, in a warning; past it, in an error; the bar
        ;; in inverse video, the face's colour as its background.
        (should (eq (plist-get n :label-face) 'drover-context-near))
        (should (equal (plist-get n :bar-face) '((:inverse-video t) drover-context-near)))
        (should (eq (plist-get o :label-face) 'drover-context-over))
        (should (equal (plist-get o :bar-face) '((:inverse-video t) drover-context-over)))
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

(ert-deftest drover-list-test-context-not-in-two-lines ()
  "Two lines leave the context out: the cache already colours the line."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (drover-list-test-layout-at 250)
      (should (eq drover-list--layout 'narrow))
      (should-not (memq 'context drover-list--fields))
      (should-not (string-search "50k" (drover-list-test-entry "a"))))))

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
                     '(title location agent model provider context created age cache summary)))
      (let ((names (drover-list-test-header-cells))
            (right (lambda (text)
                     (let ((x (drover-list-test-cell-at "a" text)))
                       (+ x (drover-list-test-fake-width text))))))
        (should (equal (mapcar #'cdr names)
                       '("Session" "Project" "Agent" "Model" "Provider" "Context" "Created" "Age"
                         "Cache" "Summary")))
        (cl-flet ((at (name) (car (rassoc name names)))
                  (end (name) (+ (car (rassoc name names)) (drover-list-test-fake-width name))))
          ;; Names begin where their columns do...
          (should (= (at "Session") (car (car (drover-list-test-cells "a")))))
          (should (= (at "Project") (drover-list-test-cell-at "a" "drover:main")))
          (should (= (at "Agent") (drover-list-test-cell-at "a" "build")))
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
    ;; Nor do two lines, which leave it out.
    (setq drover-list-hidden-fields nil)
    (drover-list-test-with-font
      (drover-list-test-layout-at 100))
    (should (eq drover-list--layout 'narrow))
    (should-not drover-test-prefetched)))

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
        (should (eq (get-text-property full 'face) 'drover-cache-bar))
        (should (eq (get-text-property (1+ full) 'face) 'drover-cache-trough))
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
    (revert-buffer)
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
    (setq drover-list-animation-interval 0.5)
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
             (frame (nth (drover-list--frame-index 3 time) '("1" "2" "3"))))
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
          (should (eq (overlay-get (plist-get (car drover-list--caches) :overlay) 'face) 'drover-cache-bar))
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
      (dolist (label '("Status" "Branch" "Agent" "Model" "Created" "Backend" "Detail"))
        (should-not (string-match-p (concat "^ +" label " ") text))))
    (search-forward "Permission")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (search-forward "bash: make")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-waiting))
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
    (should (string-match-p "^        Prompt  Prompt of child$" (drover-list-test-visible-text)))))

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
    (should-not (string-match-p "^ +\\(Model\\|Provider\\|Context\\|Project\\|Agent\\|Created\\|Directory\\) "
                                (drover-list-test-visible-text)))
    (setq drover-list-hidden-fields '(location agent model provider context created))
    (revert-buffer)
    (let ((text (drover-list-test-visible-text)))
      (dolist (line '("    Prompt    Prompt of s"
                      "    Project   drover:main"
                      "    Agent     build"
                      "    Model     Opus high"
                      "    Provider  Acme"
                      "    Context   50k of 200k tokens (25%)"
                      "    Created   2h ago"))
        (should (string-match-p (concat "^" (regexp-quote line) "$") text))))
    (search-forward "Opus high")
    (should (eq (get-text-property (- (point) 4) 'font-lock-face) 'drover-prompt-picked))))

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
            (should (string-match-p (concat "  ◐ Session s +1m\n"
                                            "    Prompt\n    Fix the build\n"
                                            "    Reply\n    Fixed it\n"
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
          ;; Under the title, 25 pixels in, the texts past "Prompt", 47
          ;; pixels, and a gap of 14: 86 pixels in, up to 293.
          (let ((lines nil))
            (save-excursion
              (forward-line 1)
              (while (and (not (eobp)) (equal (drover-list-test-id-at-point) "s"))
                (push (drover-list-test-cells-here) lines)
                (forward-line 1)))
            (setq lines (nreverse lines))
            (should (equal (mapcar #'car (car lines)) '(25 86)))
            (should (equal (cdr (nth 0 (car lines))) "Prompt"))
            (let ((texts (mapcar (lambda (line) (substring-no-properties (cdar (last line)))) lines)))
              (dolist (line lines)
                (should (= (car (car (last line))) 86))
                (should (<= (drover-list-test-fake-width (cdar (last line))) (- 293 86))))
              ;; Three lines of prompt, eight of reply, blank lines dropped.
              (should (= (length texts) 11))
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
  "A failed turn shows its error in full, and the files it changed."
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
      (should (string-match-p "^    Changes +\\+12 −3 in 3 files: a\\.el, sub/b\\.el, /elsewhere/c\\.el$" text))
      (should (string-match-p "^    Retrying +retry 2: overloaded$" text))
      ;; A recap with neither prompt nor reply says so.
      (should (string-match-p "^    Last turn +nothing to recap$" text)))
    (drover-list-test-goto "f")
    (search-forward "The provider")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-failed))))

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
      (drover-list--render)
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
                                                                     :status 'failed :detail "Boom")))
    (revert-buffer)
    (goto-char (oref (drover-list--find-session '(plain . "p")) start))
    (magit-section-show (magit-current-section))
    (let ((text (drover-list-test-visible-text)))
      (should (string-match-p "^    Error +Boom$" text))
      (should-not (string-match-p "Last turn" text)))))

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
                ;; From active subagents to none: s hides, and its
                ;; parent leaves the group s placed it in.
                (drover-list-test-type "U")
                (should (eq drover-show-subagents 'none))
                (should (equal (drover-list-test-groups) '((failed "new") (working "other" "root"))))
                ;; Then to all of them.
                (drover-list-test-type "U")
                (should (eq drover-show-subagents 'all))
                (should (equal (drover-list-test-groups)
                               '((waiting "root" "s" "kid") (failed "new") (working "other"))))
                (drover-list-test-goto "s")
                (forward-char 3)
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
                (should-not (type "V c C-g"))
                (should-not drover-filter)
                (should (equal (drover-list-test-ungrouped) '("root" "s" "kid" "other" "new")))
                ;; By provider: s's tree, root above it.
                (should-not (type "V v Anthropic RET C-g"))
                (should (equal drover-filter '(:provider "Anthropic")))
                (should (equal (drover-list-test-ungrouped) '("root" "s")))
                (should-not (type "V c g provider RET C-g"))
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

(ert-deftest drover-list-test-filter-header ()
  (drover-list-test-with (drover-list-test-sample)
    (drover-list-test-goto "work")
    (drover-list-test-shown
      (drover-set-filter :project "q")
      (should (equal (drover-list-test-groups) '((waiting "wait1") (working "work"))))
      (should (equal (drover-list-test-id-at-point) "work"))
      (should (equal (drover-list--mode-line-process) " by status filtered"))
      (goto-char (point-min))
      ;; The header counts what is listed; the View menu names the filter.
      (should (equal (drover-list-test-line) "! 1 needs you   2 sessions"))
      (forward-line 1)
      (should (equal (drover-list-test-line) ""))
      (should-not (string-search "Filter" (buffer-string)))
      (drover-set-filter :statuses '(working))
      (should (equal (drover-list-test-groups) '((working "work"))))
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
    (should (equal (drover-list-test-line) "Nothing needs you   2 sessions"))
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
      (should-not header-line-format)
      (should (string-suffix-p "1m" (drover-list-test-heading "a")))
      (should (= (string-width (drover-list-test-heading "a")) (1- before)))
      (window-resize side 6 t nil t)
      (should (= (window-max-chars-per-line side) (+ before 6)))
      (drover-list--on-window-change side)
      (should (= drover-list--width (+ before 6)))
      ;; Now it does: one line, in columns.
      (should (eq drover-list--layout 'columns))
      (should (equal (drover-list-test-entry "a") "  ! Session a   1m  asks: Go?"))
      ;; Named in the header line, which two lines lack.
      (should (equal (drover-list-test-displayed header-line-format) "  Session      Age  Summary")))))

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
    (revert-buffer)
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
      (should (eq (get-text-property waiting 'font-lock-face) 'drover-waiting))
      (should (eq (get-text-property working 'font-lock-face) 'drover-working))
      ;; Its tooltip names them, most urgent first, and counts them.
      (should (equal (get-text-property waiting 'help-echo) help))
      (should (equal (get-text-property working 'help-echo) help))
      ;; The rest of the line names them too, after the session.
      (should (equal (get-text-property (+ start 2) 'help-echo) (concat "Session p\n" help))))
    ;; Shown, they tell the same, the subagents under the parent's
    ;; title; a subagent tells its own.
    (setq drover-show-subagents 'all)
    (revert-buffer)
    (should (equal (drover-list-test-groups) '((waiting "p" "a" "d" "w" "r"))))
    (should (string-prefix-p "  · ? * Session p" (drover-list-test-heading "p")))
    (should (string-prefix-p "        ◐ * Session w" (drover-list-test-heading "w")))
    (should (string-prefix-p "        ! Session a" (drover-list-test-heading "a")))
    (should (string-prefix-p "            ↻ Session r" (drover-list-test-heading "r")))
    ;; Once they are done, nothing.
    (dolist (id '("a" "w" "r"))
      (setf (drover-session-status (drover-list-test-find id)) 'done))
    (revert-buffer)
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
            (should (= (car (car (drover-list-test-cells "w"))) (drover-list-test-title-x "p" "Parent")))
            ;; The header's glyph and count.
            (goto-char (point-min))
            (should (looking-at-p "!"))
            (should (drover-list-test-ink-apart-p))))
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
    (with-current-buffer (window-buffer (drover-list--show-side))
      (should (eq drover-list--layout 'narrow))
      (should (string-match-p "\\`  · \\? Session p +1m\\'" (drover-list-test-entry "p")))
      (should (string-match-p "\\`      ! Session a +2m\n        asks: Go\\?\\'" (drover-list-test-entry "a"))))))

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
      (should (eq (title-face "work" "Session work") 'drover-open-session))
      ;; Over the dimming of idle sessions, which lends what it lacks.
      (should (equal (title-face "idle" "Idle and open") '(drover-open-session drover-dim)))
      (should-not (title-face "shut" "Session shut"))
      ;; Only the title: the glyph keeps the face of its status.
      (save-excursion
        (drover-list-test-goto "work")
        (search-forward "◐")
        (should (eq (get-text-property (match-beginning 0) 'face) 'drover-working)))
      ;; Closing a buffer is a change its backend reports: the list,
      ;; shown, redraws.
      (drover-list-test-shown
        (kill-buffer "*session work*")
        (drover-backend-changed 'fake "work")
        (drover-changed-now)
        (should-not (title-face "work" "Session work"))
        (should (equal (title-face "idle" "Idle and open") '(drover-open-session drover-dim)))))
    ;; A title cut to fit keeps the face up to its ellipsis.
    (drover-list-test-with-font
      (let ((drover-list-test-width 150))
        (revert-buffer)
        (save-excursion
          (drover-list-test-goto "idle")
          (search-forward (truncate-string-ellipsis) (line-end-position))
          (should (equal (get-text-property (match-beginning 0) 'face)
                         '(drover-open-session drover-dim))))))))

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
      (let ((drover-list-animation-interval 1)
            (drover-animations '((working "i" "m" "x") (retrying "ii" "m")))
            (drover-list-collapsed-groups nil))
        ;; 1800000000 seconds: frame 0 of three, 0 of two.
        (revert-buffer)
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
            (should (eq (get-text-property 0 'face (car (frame "w"))) 'drover-working))
            (should (equal (frame "r") '("ii" (space :width (6)) retrying)))
            (should (eq (get-text-property 0 'face (car (frame "r"))) 'drover-retrying))
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
    (let ((drover-list-animation-interval 1)
          (drover-animations '((working "1" "2" "3") (subagents "+" "*")))
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
  (let ((animations (eval (car (get 'drover-animations 'standard-value)) t)))
    (should (equal (mapcar #'car animations) '(working retrying)))
    ;; The phases of the moon in order; the hourglass blinked.
    (should (equal (alist-get 'working animations)
                   '("\U000F0F64" "\U000F0F67" "\U000F0F61" "\U000F0F68"
                     "\U000F0F62" "\U000F0F66" "\U000F0F63" "\U000F0F65")))
    (should (= (eval (car (get 'drover-list-animation-interval 'standard-value)) t)
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
            (drover-list-animation-interval 1)
            (drover-animations '((working "i" "m"))))
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
    (let ((drover-list-animation-interval 1))
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
    (let ((drover-list-animation-interval 0.5))
      ;; Hidden: no timer.
      (revert-buffer)
      (should drover-list--animated)
      (should-not drover-list--frame-timer)
      (drover-list-test-shown
        (revert-buffer)
        (should (timerp drover-list--frame-timer))
        (should (= (timer--repeat-delay drover-list--frame-timer) 0.5))
        ;; It ticks mid-frame, so that a late tick shows no frame twice.
        (should (< (abs (- (drover-list--frame-delay 10.1) 0.65)) 1e-9))
        (should (< (abs (- (drover-list--frame-delay 10.0) 0.75)) 1e-9))
        (should (< (abs (- (drover-list--frame-delay 10.49) 0.26)) 1e-9))
        ;; A tick shows the next frame.
        (setq drover-list-test-clock (+ drover-test-now 0.5))
        (drover-list--frame-tick)
        (should (string-prefix-p "  2 Session w" (drover-list-test-heading "w")))
        (should (timerp drover-list--frame-timer))
        ;; Nothing animates: the timer stops.
        (setf (drover-session-status (drover-list-test-find "w")) 'done)
        (revert-buffer)
        (should-not drover-list--animated)
        (should-not drover-list--frame-timer)
        (setf (drover-session-status (drover-list-test-find "w")) 'working)
        (revert-buffer)
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

;;;; The menu on a list shown elsewhere

(defun drover-list-test-code-buffer (&optional name)
  "Return a buffer telling no session, NAME or \"*other code*\", C-' opening the menu."
  (with-current-buffer (get-buffer-create (or name "*other code*"))
    (use-local-map (let ((map (make-sparse-keymap)))
                     (keymap-set map "C-'" #'drover-dispatch)
                     map))
    (current-buffer)))

(defmacro drover-list-test-elsewhere (&rest body)
  "Run BODY in a code buffer, the list in a side window beside it.
The side sample is served, sessions open in buffers; `first' is the
window of the code buffer, selected, `side' the list's, its point on
\"ask\", `code' the code buffer."
  (declare (indent 0) (debug t))
  `(drover-list-test-with (drover-list-test-side-sample)
     (drover-list-test--register-buffers)
     (pcase-let ((`(,first ,_second) (drover-list-test--main)))
       (let ((side (drover-list--show-side))
             (code (drover-list-test-code-buffer)))
         (ignore first side code)
         (with-selected-window side
           (drover-list-test-goto "ask"))
         (set-window-buffer first code)
         (select-window first)
         (set-buffer code)
         (unwind-protect
             (progn ,@body)
           (when (bound-and-true-p transient--prefix)
             (execute-kbd-macro (kbd "C-g"))))))))

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
      (execute-kbd-macro (kbd "C-' o"))
      (should (equal (car drover-test-calls) (list :open "ask" nil first)))
      (should (equal (buffer-name (window-buffer first)) "*session ask*"))
      (should (eq (selected-window) first)))))

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
    ;; No List group: C-n is not the menu's.
    (execute-kbd-macro (kbd "C-'"))
    (should-not (seq-find (lambda (suffix) (eq (oref suffix command) 'drover-list-next-session))
                          transient--suffixes))
    (execute-kbd-macro (kbd "C-g"))
    (should (equal (drover-list-test-at-side side) "fail"))))

(ert-deftest drover-list-test-dispatch-view-elsewhere ()
  "The View menu opened from a code buffer sets the list's follow mode."
  (drover-list-test-elsewhere
    (execute-kbd-macro (kbd "C-' V F"))
    (should (eq (oref transient--prefix command) 'drover-view))
    (should (buffer-local-value 'drover-follow-mode buffer))
    (should-not (buffer-local-value 'drover-follow-mode code))
    ;; Following shows the session at the list's point in the main window.
    (should (equal (car drover-test-calls) (list :open "ask" nil first)))
    (execute-kbd-macro (kbd "F"))
    (should-not (buffer-local-value 'drover-follow-mode buffer))
    (execute-kbd-macro (kbd "C-g C-g"))
    (should-not transient--prefix)
    ;; The user is back where they work.
    (should (eq (selected-window) first))))

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
      (should (equal (car drover-test-calls) (list :open "ask" nil first))))))

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
    ;; The idle group is collapsed: work is the last session in sight.
    (should (equal (drover-list-test-at-side side) "work"))
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
            (should (equal (seq-take (car drover-test-calls) 3) '(:open "fail" (4))))
            (should (eq (window-buffer first) buffer))
            (should (equal (buffer-name (window-buffer second)) "*session fail*")))
        (use-local-map drover-list-mode-map)))))

(ert-deftest drover-list-test-dispatch-list-keys ()
  "The List group's keys are its own: transient binds none of them."
  (dolist (key '("C-n" "C-p" "M-n" "M-p" "M-<" "M->"))
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
                                        ("C-u" . universal-argument)))
      (should (eq (keymap-lookup transient--transient-map key) command)))
    ;; One key, one entry of the menu.
    (let ((keys (mapcar (lambda (suffix) (oref suffix key))
                        (seq-remove (lambda (suffix) (oref suffix inactive)) transient--suffixes))))
      (should (equal keys (delete-dups (copy-sequence keys)))))))

;;;; Columns

(ert-deftest drover-list-test-hidden-fields ()
  "The width chooses among the fields the user lets show; the title always shows."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-with-font
      (setq drover-list-hidden-fields '(location model))
      (should (equal (drover-list-test-layout-at 600)
                     '(columns title agent provider context created age cache summary)))
      ;; Room freed for the fields of lower rank: context 396, provider
      ;; 424, agent 473, created 508.
      (should (equal (drover-list-test-layout-at 520)
                     '(columns title agent provider context created age cache summary)))
      (should (equal (drover-list-test-layout-at 510)
                     '(columns title agent provider context age cache summary)))
      ;; In two lines, a hidden summary leaves no second line.
      (setq drover-list-hidden-fields '(summary))
      (should (equal (drover-list-test-layout-at 320) '(columns title model age cache)))
      (setq drover-list-hidden-fields '(cache title))
      (should (equal (drover-list-test-layout-at 120) '(narrow title age summary)))
      (should-not drover-list--caches)
      (should (string-match-p "\\` +! Bravo +1m\n +asks: Which f" (drover-list-test-entry "b")))
      (setq drover-list-hidden-fields '(summary age))
      (drover-list-test-layout-at 120)
      (should (string-match-p "\\` +! Bravo\\'" (drover-list-test-entry "b"))))))

(ert-deftest drover-list-test-columns-from-view-menu ()
  "The View menu's switches hide a field at once, the menu staying open."
  (drover-list-test-with (drover-list-test-rich)
    (drover-list-test-shown
      (revert-buffer)
      (pcase-dolist (`(,key ,field ,text) '(("m" model "opus high") ("r" provider "P")
                                            ("x" context "50k")))
        (should (memq field drover-list--fields))
        (should (string-search text (drover-list-test-heading "a")))
        (drover-list-test-type (concat "V - " key))
        (should (eq (oref transient--prefix command) 'drover-view))
        (should (equal drover-list-hidden-fields (list field)))
        (should-not (memq field drover-list--fields))
        (should-not (string-search text (drover-list-test-heading "a")))
        ;; Its column is no longer named.
        (should-not (string-search (plist-get (drover-list--field-spec field) :label)
                                   header-line-format))
        (drover-list-test-type (concat "- " key " C-g"))
        (should-not drover-list-hidden-fields)
        (should (memq field drover-list--fields))
        (should-not transient--prefix)))))

(provide 'drover-list-test)
;;; drover-list-test.el ends here
