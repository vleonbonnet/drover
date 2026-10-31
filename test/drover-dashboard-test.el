;;; drover-dashboard-test.el --- Tests of the sectioned dashboard  -*- lexical-binding: t; -*-

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

;; The dashboard is drawn in its buffer without displaying it, except
;; where a test is about windows: visibility, window point and start.
;;
;; Tests run in batch, on a text terminal frame, where a pixel is a
;; character cell: `drover-dashboard-test-displayed' turns each aligned
;; space into the spaces a terminal draws for it, so lines read as the
;; user sees them.  Alignment with proportional widths is tested with a
;; fake measure, `drover-dashboard-test-with-proportional-font'.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-dashboard)

(defmacro drover-dashboard-test-with (sessions &rest body)
  "Run BODY in a fresh dashboard drawn from SESSIONS of the fake backend."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((drover-dashboard-age-refresh-interval nil)
           (drover-dashboard-buffer-name " *drover-test*")
           ;; Readable glyphs, one column wide in every Emacs, whatever
           ;; the defaults.
           (drover-status-glyphs '((waiting . "!") (failed . "✗") (done . "✓")
                                   (retrying . "↻") (working . "◐") (idle . "·")))
           (buffer nil))
       (unwind-protect
           (progn
             (setq buffer (drover-dashboard--buffer))
             (with-current-buffer buffer
               (drover-dashboard-refresh)
               ,@body))
         (when (buffer-live-p buffer)
           (kill-buffer buffer))))))

(defun drover-dashboard-test-fake-width (string)
  "Return the width of STRING in a fake proportional font.
An i is 3 pixels wide, an m 12, a space 4, and any other character 7."
  (apply #'+ (mapcar (lambda (char)
                       (pcase char (?i 3) (?m 12) (?\s 4) (_ 7)))
                     string)))

(defmacro drover-dashboard-test-with-proportional-font (&rest body)
  "Run BODY with text measured in `drover-dashboard-test-fake-width'.
Lines may take 1000 pixels."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'drover-dashboard--pixel-width) #'drover-dashboard-test-fake-width)
             ((symbol-function 'drover-dashboard--line-width) (lambda () 1000)))
     ,@body))

(defun drover-dashboard-test-ago (seconds)
  "Return the float time SECONDS ago."
  (- (float-time) seconds))

(defun drover-dashboard-test-titled (id title &rest properties)
  "Return a fake session ID titled TITLE, with PROPERTIES."
  (let ((session (apply #'drover-test-session id properties)))
    (setf (drover-session-title session) title)
    session))

(defun drover-dashboard-test-find (id)
  "Return the fake session ID the backend serves."
  (seq-find (lambda (session) (equal (drover-session-id session) id)) drover-test-sessions))

(defun drover-dashboard-test-groups ()
  "Return the groups of the dashboard as a list of (VALUE . IDS)."
  (mapcar (lambda (group)
            (cons (oref group value)
                  (mapcar (lambda (section) (cdr (oref section value)))
                          (oref group children))))
          (drover-dashboard--groups)))

(defun drover-dashboard-test-section (id)
  "Return the section of the session ID of the fake backend."
  (drover-dashboard--find-session (cons 'fake id)))

(defun drover-dashboard-test-goto (id)
  "Move point to the heading of the session ID of the fake backend."
  (goto-char (oref (drover-dashboard-test-section id) start)))

(defun drover-dashboard-test-goto-group (value)
  "Move point to the heading of the group whose value is VALUE."
  (goto-char (oref (seq-find (lambda (group) (equal (oref group value) value))
                             (drover-dashboard--groups))
                   start)))

(defun drover-dashboard-test-align-to (position &optional object)
  "Return the pixel position the space at POSITION in OBJECT aligns to, or nil."
  (pcase (get-text-property position 'display object)
    (`(space :align-to (,x)) x)))

(defun drover-dashboard-test-displayed (string)
  "Return STRING as a text terminal draws it.
Each space aligned to a pixel position becomes the spaces reaching it:
on a terminal, a pixel is a character cell.  Aligned cells must not
overlap."
  (let ((column 0)
        (chunks nil))
    (dotimes (index (length string))
      (let ((char (aref string index))
            (x (drover-dashboard-test-align-to index string)))
        (cond (x
               (should (>= x column))
               (push (make-string (- x column) ?\s) chunks)
               (setq column x))
              ((eq char ?\n)
               (push "\n" chunks)
               (setq column 0))
              (t
               (push (string char) chunks)
               (setq column (+ column (char-width char)))))))
    (apply #'concat (nreverse chunks))))

(defun drover-dashboard-test-line ()
  "Return the line at point as a text terminal draws it."
  (drover-dashboard-test-displayed (buffer-substring (line-beginning-position) (line-end-position))))

(defun drover-dashboard-test-heading (id)
  "Return the heading line of session ID as a text terminal draws it."
  (save-excursion
    (drover-dashboard-test-goto id)
    (drover-dashboard-test-line)))

(defun drover-dashboard-test-cells (id)
  "Return the cells of the heading of session ID, as a list of (X . TEXT).
X is the pixel position a cell is aligned to, TEXT what follows it up
to the next cell, with its properties."
  (save-excursion
    (drover-dashboard-test-goto id)
    (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
          (cells nil))
      (should (drover-dashboard-test-align-to 0 line))
      (dotimes (index (length line))
        (if-let* ((x (drover-dashboard-test-align-to index line)))
            (push (cons x "") cells)
          (setcdr (car cells) (concat (cdar cells) (substring line index (1+ index))))))
      (nreverse cells))))

(defun drover-dashboard-test-cell-at (id text)
  "Return the pixel position of the cell of session ID holding TEXT."
  (car (seq-find (lambda (cell) (string-search text (cdr cell)))
                 (drover-dashboard-test-cells id))))

(defun drover-dashboard-test-id-at-point ()
  "Return the identifier of the session whose section holds point, or nil."
  (when-let* ((section (drover-dashboard--session-section (magit-section-at))))
    (cdr (oref section value))))

(defun drover-dashboard-test-visible-text ()
  "Return the text of the dashboard that is not invisible, as drawn."
  (let ((position (point-min))
        (chunks nil))
    (while (< position (point-max))
      (let ((next (next-single-char-property-change position 'invisible nil (point-max))))
        (unless (invisible-p position)
          (push (buffer-substring position next) chunks))
        (setq position next)))
    (drover-dashboard-test-displayed (apply #'concat (nreverse chunks)))))

(defun drover-dashboard-test-section-hidden-p (id)
  "Return non-nil when the section of session ID is collapsed."
  (oref (drover-dashboard-test-section id) hidden))

(defun drover-dashboard-test-group-hidden-p (value)
  "Return non-nil when the group whose value is VALUE is collapsed."
  (oref (seq-find (lambda (group) (equal (oref group value) value))
                  (drover-dashboard--groups))
        hidden))

(defun drover-dashboard-test-heading-visible-p (id)
  "Return non-nil when the heading of session ID is visible."
  (not (invisible-p (oref (drover-dashboard-test-section id) start))))

(defun drover-dashboard-test-sample ()
  "Return a sample of sessions, one or more of each status."
  (list (drover-test-session "idle" :status 'idle :updated (drover-dashboard-test-ago 10) :project "p")
        (drover-test-session "work" :status 'working :updated (drover-dashboard-test-ago 20) :project "q")
        (drover-test-session "wait2" :status 'waiting :since (drover-dashboard-test-ago 60) :project "p")
        (drover-test-session "done" :status 'done :since (drover-dashboard-test-ago 30))
        (drover-test-session "wait1" :status 'waiting :since (drover-dashboard-test-ago 600) :project "q")))

(defun drover-dashboard-test-family ()
  "Return a parent with subagents two levels deep, and another tree.
The parent works, its subagent waits for the user, and the subagent's
own subagent is done; the other tree works and has a quiet subagent."
  (list (drover-test-session "root" :status 'working :updated (drover-dashboard-test-ago 10))
        (drover-test-session "sub" :parent "root" :status 'waiting :since (drover-dashboard-test-ago 60))
        (drover-test-session "deep" :parent "sub" :status 'done :since (drover-dashboard-test-ago 30))
        (drover-test-session "other" :status 'working :updated (drover-dashboard-test-ago 5))
        (drover-test-session "quiet" :parent "other" :status 'idle)))

;;;; Grouping

(ert-deftest drover-dashboard-test-groups-by-status ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (should (equal (drover-dashboard-test-groups)
                   '((waiting "wait1" "wait2") (done "done") (working "work") (idle "idle"))))
    (should (seq-every-p (lambda (group) (object-of-class-p group 'drover-dashboard-status-section))
                         (drover-dashboard--groups)))
    (drover-dashboard-test-goto-group 'waiting)
    (should (equal (drover-dashboard-test-line) "Needs you (2)"))
    (drover-dashboard-test-goto-group 'idle)
    (should (equal (drover-dashboard-test-line) "Idle (1)"))))

(ert-deftest drover-dashboard-test-empty-groups-omitted ()
  (drover-dashboard-test-with (list (drover-test-session "w" :status 'working))
    (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(working)))
    (let ((case-fold-search nil))
      (should-not (string-match-p "^\\(Needs you\\|Failed\\|Done\\|Retrying\\|Idle\\)" (buffer-string))))))

(ert-deftest drover-dashboard-test-groups-by-project ()
  (drover-dashboard-test-with (append (drover-dashboard-test-sample)
                                      (list (drover-test-session "fail" :status 'failed
                                                                 :since (drover-dashboard-test-ago 5)
                                                                 :project "p" :branch "main")))
    (drover-dashboard-toggle-grouping)
    (should (eq drover-dashboard--grouping 'project))
    (should (equal (drover-dashboard-test-groups)
                   '(("q" "wait1" "work") ("p" "wait2" "fail" "idle") (nil "done"))))
    (should (seq-every-p (lambda (group) (object-of-class-p group 'drover-dashboard-project-section))
                         (drover-dashboard--groups)))
    (drover-dashboard-test-goto-group "p")
    (should (equal (drover-dashboard-test-line) "p (3)  !1 ✗1"))
    (drover-dashboard-test-goto-group nil)
    (should (equal (drover-dashboard-test-line) "No project (1)  ✓1"))
    (should (string-prefix-p "  ! Session wait2" (drover-dashboard-test-heading "wait2")))
    (should (string-prefix-p "  · Session idle" (drover-dashboard-test-heading "idle")))
    ;; The heading names the project: lines keep only the branch.
    (should (string-match-p "Session fail +main +5s" (drover-dashboard-test-heading "fail")))
    (should-not (string-match-p "p:main" (drover-dashboard-test-heading "fail")))
    (drover-dashboard-toggle-grouping)
    (should (eq drover-dashboard--grouping 'status))
    (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(waiting failed done working idle)))))

(ert-deftest drover-dashboard-test-default-grouping ()
  (let ((drover-dashboard-grouping 'project))
    (drover-dashboard-test-with (list (drover-test-session "s" :project "p"))
      (should (equal (drover-dashboard-test-groups) '(("p" "s")))))))

(ert-deftest drover-dashboard-test-header ()
  (drover-dashboard-test-with (append (drover-dashboard-test-sample)
                                      (list (drover-test-session "fail" :status 'failed)
                                            (drover-test-session "sub" :parent "work")))
    (goto-char (point-min))
    (should (equal (drover-dashboard-test-line)
                   "! 2 needs you   ✗ 1 failed   ✓ 1 done   6 sessions, 1 subagent hidden"))
    (should (null (drover-dashboard-test-id-at-point)))))

(ert-deftest drover-dashboard-test-header-nothing-needs-you ()
  (drover-dashboard-test-with (list (drover-test-session "w" :status 'working))
    (goto-char (point-min))
    (should (equal (drover-dashboard-test-line) "Nothing needs you   1 session"))))

;;;; Subagents

(ert-deftest drover-dashboard-test-subagents-under-parent ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    ;; The tree of root goes with its waiting subagent; quiet is hidden.
    (should (equal (drover-dashboard-test-groups)
                   '((waiting "root" "sub" "deep") (working "other"))))
    ;; Each level is indented one step deeper, each with its own glyph;
    ;; ages line up past the deepest title.
    (should (equal (drover-dashboard-test-heading "root") "  ◐ Session root      10s"))
    (should (equal (drover-dashboard-test-heading "sub") "    ! Session sub      1m"))
    (should (equal (drover-dashboard-test-heading "deep") "      ✓ Session deep  30s"))
    (should (equal (drover-dashboard-test-heading "other") "  ◐ Session other      5s"))
    (let ((step (plist-get (drover-dashboard--metrics 0) :step))
          (margin (car (car (drover-dashboard-test-cells "root")))))
      (should (= margin (* drover-dashboard--margin (drover-dashboard--char-width))))
      (should (= (car (car (drover-dashboard-test-cells "sub"))) (+ margin step)))
      (should (= (car (car (drover-dashboard-test-cells "deep"))) (+ margin step step)))
      (should (= (car (car (drover-dashboard-test-cells "other"))) margin)))
    (should (eq (oref (drover-dashboard-test-section "sub") parent)
                (oref (drover-dashboard-test-section "root") parent)))))

(ert-deftest drover-dashboard-test-tree-in-group-of-urgent-subagent ()
  (drover-dashboard-test-with
      (list (drover-test-session "parent" :status 'done :since (drover-dashboard-test-ago 50))
            (drover-test-session "asking" :parent "parent" :status 'waiting
                                 :since (drover-dashboard-test-ago 30))
            (drover-test-session "alone" :status 'waiting :since (drover-dashboard-test-ago 40)))
    ;; Within the group, trees rank by their oldest wait.
    (should (equal (drover-dashboard-test-groups) '((waiting "alone" "parent" "asking"))))
    (drover-dashboard-test-goto-group 'waiting)
    (should (equal (drover-dashboard-test-line) "Needs you (3)"))
    ;; The header counts sessions, not trees.
    (goto-char (point-min))
    (should (equal (drover-dashboard-test-line) "! 2 needs you   ✓ 1 done   3 sessions"))
    (should (string-prefix-p "  ✓ Session parent" (drover-dashboard-test-heading "parent")))))

(ert-deftest drover-dashboard-test-orphan-subagent ()
  (drover-dashboard-test-with (list (drover-test-session "child" :parent "gone" :status 'working))
    (should (equal (drover-dashboard-test-groups) '((working "child"))))
    (should (string-prefix-p "  ◐ Session child" (drover-dashboard-test-heading "child")))
    (goto-char (point-min))
    (should (equal (drover-dashboard-test-line) "Nothing needs you   1 session"))
    (drover-dashboard-test-goto "child")
    (magit-section-show (magit-current-section))
    (should (string-match-p "^ +Subagent of +gone$" (drover-dashboard-test-visible-text)))))

(ert-deftest drover-dashboard-test-children-toggle ()
  (drover-dashboard-test-with (list (drover-test-session "root" :status 'working)
                                    (drover-test-session "child" :parent "root" :status 'working)
                                    (drover-test-session "asking" :parent "root" :status 'waiting))
    (should (equal (drover-dashboard-test-groups) '((waiting "root" "asking"))))
    (goto-char (point-min))
    (should (string-suffix-p "2 sessions, 1 subagent hidden" (drover-dashboard-test-line)))
    (drover-dashboard-test-press "c")
    (should (local-variable-p 'drover-show-children))
    (should drover-show-children)
    (should (equal (drover-dashboard-test-groups) '((waiting "root" "asking" "child"))))
    (goto-char (point-min))
    (should (string-suffix-p "   3 sessions" (drover-dashboard-test-line)))
    (should (string-prefix-p "    ◐ Session child" (drover-dashboard-test-heading "child")))
    (with-temp-buffer
      (should-not drover-show-children))
    (drover-dashboard-test-press "c")
    (should-not drover-show-children)
    (should (equal (drover-dashboard-test-groups) '((waiting "root" "asking"))))))

(ert-deftest drover-dashboard-test-parent-details-keep-subagents ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-dashboard-test-section-hidden-p "root"))
    ;; The details come between the parent and its subagents.
    (should (string-match-p "  ◐ Session root +10s\n    Status +Working for 10s\n\\(.*\n\\)*    ! Session sub"
                            (drover-dashboard-test-visible-text)))
    (should (< (oref (drover-dashboard-test-section "root") end)
               (1+ (oref (drover-dashboard-test-section "sub") start))))
    (drover-dashboard-test-goto "root")
    (magit-section-toggle (magit-current-section))
    (should (drover-dashboard-test-section-hidden-p "root"))
    ;; Hiding them leaves the subagents in sight.
    (should (drover-dashboard-test-heading-visible-p "sub"))
    (should (drover-dashboard-test-heading-visible-p "deep"))
    (should-not (string-match-p "Status" (drover-dashboard-test-visible-text)))))

(ert-deftest drover-dashboard-test-nested-collapse-state-across-refresh ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-goto "sub")
    (magit-section-show (magit-current-section))
    ;; The tree moves from Needs you to Failed.
    (setf (drover-session-status (drover-dashboard-test-find "sub")) 'failed)
    (drover-dashboard-refresh)
    (should (equal (drover-dashboard-test-groups)
                   '((failed "root" "sub" "deep") (working "other"))))
    (should-not (drover-dashboard-test-section-hidden-p "sub"))
    (should (drover-dashboard-test-section-hidden-p "root"))
    (should (drover-dashboard-test-section-hidden-p "deep"))
    (should (string-match-p "^      Status +Failed for 1m$" (drover-dashboard-test-visible-text)))
    ;; And when the parent's details open and close around it.
    (drover-dashboard-test-goto "root")
    (magit-section-show (magit-current-section))
    (drover-dashboard-test-goto "root")
    (magit-section-hide (magit-current-section))
    (drover-dashboard-refresh)
    (should-not (drover-dashboard-test-section-hidden-p "sub"))))

(ert-deftest drover-dashboard-test-refresh-follows-nested-session ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-goto "deep")
    (forward-char 3)
    (let ((offset (- (point) (line-beginning-position))))
      ;; A new tree above, and deep's tree moves to Failed.
      (push (drover-test-session "new" :status 'waiting) drover-test-sessions)
      (setf (drover-session-status (drover-dashboard-test-find "sub")) 'failed)
      (drover-dashboard-refresh)
      (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(waiting failed working)))
      (should (equal (drover-dashboard-test-id-at-point) "deep"))
      (should (= (- (point) (line-beginning-position)) offset)))))

(ert-deftest drover-dashboard-test-project-grouping-nests-subagents ()
  (drover-dashboard-test-with
      (list (drover-test-session "root" :project "p" :branch "main" :status 'working)
            (drover-test-session "elsewhere" :parent "root" :project "q" :branch "dev"
                                 :status 'waiting :since (drover-dashboard-test-ago 20))
            (drover-test-session "here" :parent "root" :project "p" :branch "feat"
                                 :status 'waiting :since (drover-dashboard-test-ago 10)))
    (drover-dashboard-toggle-grouping)
    ;; A subagent stays with its parent, whatever its own project.
    (should (equal (drover-dashboard-test-groups) '(("p" "root" "elsewhere" "here"))))
    (drover-dashboard-test-goto-group "p")
    (should (equal (drover-dashboard-test-line) "p (3)  !2"))
    (should (string-match-p "Session root +main\\'" (drover-dashboard-test-heading "root")))
    (should (string-match-p "Session here +feat +10s\\'" (drover-dashboard-test-heading "here")))
    ;; Another project than the group's is named.
    (should (string-match-p "Session elsewhere +q:dev +20s\\'" (drover-dashboard-test-heading "elsewhere")))))

;;;; Lines

(ert-deftest drover-dashboard-test-session-line ()
  (drover-dashboard-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-dashboard-test-ago 300)
                                 :project "drover" :branch "main"
                                 :requests '((:kind permission :summary "bash: make"))))
    (should (equal (drover-dashboard-test-heading "s") "  ! Session s  drover:main  5m  permit: bash: make"))
    (drover-dashboard-test-goto "s")
    (let ((glyph (1+ (point))))
      (should (equal (buffer-substring-no-properties glyph (1+ glyph)) "!"))
      (should (eq (get-text-property glyph 'font-lock-face) 'drover-waiting)))
    (search-forward "drover:main")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))
    (search-forward "5m")
    (should (eq (get-text-property (match-beginning 0) 'font-lock-face) 'drover-dim))))

(ert-deftest drover-dashboard-test-wide-glyph-deepens-step ()
  "A step of indentation is as wide as the widest glyph and a space."
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (setq-local drover-status-glyphs (cons '(waiting . "待") drover-status-glyphs))
    (drover-dashboard-refresh)
    (should (= (plist-get (drover-dashboard--metrics 0) :step) 3))
    ;; A subagent's glyph never starts left of its parent's title.
    (should (string-prefix-p "  ◐ Session root" (drover-dashboard-test-heading "root")))
    (should (string-prefix-p "     待 Session sub" (drover-dashboard-test-heading "sub")))
    (should (string-prefix-p "        ✓ Session deep" (drover-dashboard-test-heading "deep")))))

(ert-deftest drover-dashboard-test-one-space-after-glyph ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-press "c")
    (dolist (id '("root" "sub" "deep" "other" "quiet"))
      (let ((status (cdr (car (drover-dashboard-test-cells id)))))
        ;; The glyph, one plain space, then the title.
        (should (string-match-p "\\`[!✓◐·] Session" status))
        (should-not (get-text-property 1 'display status))))))

(ert-deftest drover-dashboard-test-columns-align ()
  (drover-dashboard-test-with
      (list (drover-dashboard-test-titled "a" "Short" :status 'working :project "p" :branch "b"
                                          :updated (drover-dashboard-test-ago 5))
            (drover-dashboard-test-titled "b" "A longer title" :status 'working :project "project"
                                          :updated (drover-dashboard-test-ago 7200)
                                          :detail "Thinking"))
    (let ((a (drover-dashboard-test-heading "a"))
          (b (drover-dashboard-test-heading "b")))
      (should (string-prefix-p "  ◐ Short           p:b" a))
      (should (= (string-search "p:b" a) (string-search "project" b)))
      (should (= (string-search "5s" a) (string-search "2h" b)))
      (should (string-suffix-p "5s" a))
      (should (string-suffix-p "2h  Thinking" b)))))

(ert-deftest drover-dashboard-test-pixel-alignment ()
  "Columns line up in pixels with characters of different widths."
  (drover-dashboard-test-with-proportional-font
    (drover-dashboard-test-with
        (list (drover-dashboard-test-titled "a" "mmmm" :status 'working :project "iii"
                                            :updated (drover-dashboard-test-ago 5) :detail "Busy")
              (drover-dashboard-test-titled "b" "iiiiiiii" :status 'working :project "mm" :branch "x"
                                            :updated (drover-dashboard-test-ago 7200))
              (drover-dashboard-test-titled "c" "Sub" :parent "b" :status 'waiting :project "p"
                                            :since (drover-dashboard-test-ago 30) :detail "Asks"))
      (should (equal (drover-dashboard-test-groups) '((waiting "b" "c") (working "a"))))
      (let* ((metrics (drover-dashboard--metrics 0))
             (gap (plist-get metrics :gap))
             (margin (plist-get metrics :margin))
             (cells (mapcar (lambda (id) (cons id (drover-dashboard-test-cells id))) '("a" "b" "c")))
             (title-ends (mapcar (lambda (entry)
                                   (let ((cell (cadr entry)))
                                     (+ (car cell) (drover-dashboard-test-fake-width (cdr cell)))))
                                 cells))
             (location (+ (apply #'max title-ends) gap)))
        (should (= (drover-dashboard-test-fake-width "x") (plist-get metrics :char)))
        ;; Status glyphs: roots at the margin, the subagent one step in.
        (should (= (car (nth 1 (assoc "a" cells))) margin))
        (should (= (car (nth 1 (assoc "b" cells))) margin))
        (should (= (car (nth 1 (assoc "c" cells))) (+ margin (plist-get metrics :step))))
        ;; Locations begin at one position, past the widest title.
        (should (= (drover-dashboard-test-cell-at "a" "iii") location))
        (should (= (drover-dashboard-test-cell-at "b" "mm:x") location))
        (should (= (drover-dashboard-test-cell-at "c" "p") location))
        ;; Ages end at one position.
        (let ((ends (mapcar (lambda (entry)
                              (let ((cell (seq-find (lambda (cell)
                                                      (string-match-p "\\`[0-9]+[smhd]\\'" (cdr cell)))
                                                    (cdr entry))))
                                (+ (car cell) (drover-dashboard-test-fake-width (cdr cell)))))
                            cells)))
          (should (= (length (seq-uniq ends)) 1))
          ;; Summaries begin at one position, past the ages.
          (should (= (drover-dashboard-test-cell-at "a" "Busy") (+ (car ends) gap)))
          (should (= (drover-dashboard-test-cell-at "c" "Asks") (+ (car ends) gap))))))))

(ert-deftest drover-dashboard-test-truncate-in-pixels ()
  (drover-dashboard-test-with-proportional-font
    (let ((dim (drover-face "mmmmii" 'drover-dim)))
      (should (eq (drover-dashboard--truncate dim 100) dim))
      ;; m m and the ellipsis: 12 + 12 + 7.
      (let ((cut (drover-dashboard--truncate dim 33)))
        (should (equal cut "mm…"))
        (should (<= (drover-dashboard-test-fake-width cut) 33))
        (should (eq (get-text-property 2 'face cut) 'drover-dim)))
      (should (equal (drover-dashboard--truncate "ab cd" 22) "ab…"))
      (should (equal (drover-dashboard--truncate "abc" 5) ""))
      (should (equal (drover-dashboard--truncate "" 0) "")))))

(ert-deftest drover-dashboard-test-titles-truncated-in-pixels ()
  (drover-dashboard-test-with-proportional-font
    (let ((drover-dashboard-title-width 10))
      (drover-dashboard-test-with (list (drover-dashboard-test-titled "s" (make-string 30 ?m)
                                                                      :status 'working))
        (let ((status (cdr (car (drover-dashboard-test-cells "s")))))
          (should (string-suffix-p "…" status))
          ;; Ten characters as wide as x, after the glyph and space.
          (should (<= (drover-dashboard-test-fake-width (substring status 2)) 70)))))))

(ert-deftest drover-dashboard-test-detail-summary ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'failed :detail "Rate limited"))
    (should (string-match-p "Session s +Rate limited\\'" (drover-dashboard-test-heading "s")))))

(ert-deftest drover-dashboard-test-long-columns-truncated ()
  (drover-dashboard-test-with
      (list (drover-dashboard-test-titled "s" (make-string 60 ?t) :status 'waiting
                                          :project (make-string 50 ?p)))
    (should (equal (drover-dashboard-test-heading "s")
                   (concat "  ! " (make-string 39 ?t) "…  " (make-string 29 ?p) "…")))))

(ert-deftest drover-dashboard-test-summary-fits-window ()
  (drover-dashboard-test-with
      (list (drover-test-session "s" :status 'waiting :project "p"
                                 :requests `((:kind question :summary ,(make-string 200 ?q)))))
    (let ((line (drover-dashboard-test-heading "s")))
      ;; In batch, the selected window is on a text terminal.
      (should (= drover-dashboard--width (window-max-chars-per-line)))
      ;; Magit's ellipsis takes the last column.
      (should (= (string-width line) (1- drover-dashboard--width)))
      (should (string-match-p "  asks: q+…\\'" line)))))

(ert-deftest drover-dashboard-test-narrow-window-drops-summary ()
  (drover-dashboard-test-with
      (list (drover-test-session "s" :status 'waiting :project (make-string 30 ?p)
                                 :requests '((:kind question :summary "Which branch?"))))
    (should (string-match-p "asks" (drover-dashboard-test-heading "s")))
    (save-window-excursion
      (let ((window (split-window-right 40)))
        (set-window-buffer window buffer)
        (with-selected-window window
          (drover-dashboard--on-window-change window)
          (should (= drover-dashboard--width (window-max-chars-per-line window)))
          (should-not (string-match-p "asks" (drover-dashboard-test-heading "s"))))))))

(ert-deftest drover-dashboard-test-one-line-headings ()
  (drover-dashboard-test-with
      (list (drover-dashboard-test-titled "s" "Two\nlines" :status 'waiting
                                          :requests '((:kind question :summary "First\n\nsecond"))))
    (should (string-match-p "Two lines.*asks: First second\\'" (drover-dashboard-test-heading "s")))))

;;;; Faces

(ert-deftest drover-dashboard-test-variable-pitch ()
  (drover-dashboard-test-with (list (drover-test-session "s"))
    (should (eq (face-attribute 'drover-dashboard :inherit) 'variable-pitch))
    (should (memq 'drover-dashboard (flatten-tree (alist-get 'default face-remapping-alist))))))

(ert-deftest drover-dashboard-test-measures-in-dashboard-faces ()
  (drover-dashboard-test-with (list (drover-test-session "s"))
    (let ((remapping face-remapping-alist)
          (measured nil))
      (cl-letf (((symbol-function 'buffer-text-pixel-size)
                 (lambda (&optional _buffer window &rest _)
                   (setq measured (list face-remapping-alist window (buffer-string)))
                   '(42 . 1))))
        (should (= (drover-dashboard--pixel-width "abc") 42)))
      ;; The measuring buffer has the dashboard's remapping, and the
      ;; window the dashboard is drawn for.
      (should (equal measured (list remapping (selected-window) "abc"))))))

(ert-deftest drover-dashboard-test-redraws-on-text-scale ()
  (drover-dashboard-test-with (list (drover-test-session "s"))
    (setq drover-dashboard--width 1)
    (text-scale-set 1)
    (should (= drover-dashboard--width (drover-dashboard--line-width)))
    (setq drover-dashboard--width 1)
    (text-scale-set 0)
    (should (= drover-dashboard--width (drover-dashboard--line-width)))))

;;;; Details

(ert-deftest drover-dashboard-test-details-on-expand ()
  (drover-dashboard-test-with
      (list (drover-test-session "s" :status 'waiting :since (drover-dashboard-test-ago 120)
                                 :directory (expand-file-name "~/src/drover") :branch "main"
                                 :agent "build" :model "sonnet" :detail "Blocked"
                                 :requests '((:kind permission :summary "bash: make")
                                             (:kind question :summary "Which branch?"))))
    (should (drover-dashboard-test-section-hidden-p "s"))
    (should-not (string-match-p "Directory" (drover-dashboard-test-visible-text)))
    (drover-dashboard-test-goto "s")
    (magit-section-toggle (magit-current-section))
    (should-not (drover-dashboard-test-section-hidden-p "s"))
    (let ((text (drover-dashboard-test-visible-text)))
      ;; Labels under the title, values past the widest label.
      (dolist (line '("    Status      Needs you for 2m"
                      "    Directory   ~/src/drover"
                      "    Branch      main"
                      "    Agent       build"
                      "    Model       sonnet"
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
    (drover-dashboard-test-goto "s")
    (forward-line 3)
    (should (equal (drover-dashboard-test-id-at-point) "s"))
    (should (equal (drover-session-id (drover-session-at-point)) "s"))))

(ert-deftest drover-dashboard-test-details-of-subagent ()
  (drover-dashboard-test-with (list (drover-dashboard-test-titled "root" "Parent work")
                                    (drover-test-session "child" :parent "root" :status 'waiting))
    (drover-dashboard-test-goto "child")
    (magit-section-show (magit-current-section))
    ;; Indented as deep as the subagent's title.
    (should (string-match-p "^      Subagent of  Parent work$" (drover-dashboard-test-visible-text)))))

;;;; Session at point

(ert-deftest drover-dashboard-test-session-at-point-is-fresh ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (drover-dashboard-test-goto "s")
    (let ((fresh (drover-test-session "s" :status 'done)))
      (setq drover-test-sessions (list fresh))
      (should (eq (drover-session-at-point) fresh)))
    (setq drover-test-sessions nil)
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-dashboard-test-no-session-on-headings ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (goto-char (point-min))
    (should-not (funcall drover-session-at-point-function))
    (drover-dashboard-test-goto-group 'working)
    (should-not (funcall drover-session-at-point-function))
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-dashboard-test-subagent-at-point ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-goto "sub")
    (should (equal (drover-session-id (drover-session-at-point)) "sub"))
    (drover-dashboard-test-goto "root")
    (should (equal (drover-session-id (drover-session-at-point)) "root"))))

;;;; Keys

(defun drover-dashboard-test-press (key)
  "Run the command KEY runs at point in the current buffer."
  (let ((command (key-binding (kbd key))))
    (should command)
    (call-interactively command)))

(ert-deftest drover-dashboard-test-keys-reach-backend ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'waiting))
    (save-window-excursion
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (drover-dashboard-test-goto "s")
        (dolist (key '("RET" "o" "a" "C-c C-q" "s" "k" "A" "SPC"))
          (drover-dashboard-test-press key)))
      (when (get-buffer drover-peek-buffer)
        (kill-buffer drover-peek-buffer)))
    (should (equal (reverse drover-test-calls)
                   '((:open "s" nil) (:open "s" t) (:respond "s") (:respond "s") (:mark-seen "s")
                     (:interrupt "s") (:archive "s") (:preview "s"))))))

(ert-deftest drover-dashboard-test-declined-confirmation ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (drover-dashboard-test-goto "s")
      (drover-dashboard-test-press "k")
      (drover-dashboard-test-press "A"))
    (should-not drover-test-calls)))

(ert-deftest drover-dashboard-test-key-bindings ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (drover-dashboard-test-goto "s")
    (should (eq (key-binding (kbd "g")) #'drover-dashboard-refresh))
    (should (eq (key-binding (kbd "q")) #'quit-window))
    (should (eq (key-binding (kbd "G")) #'drover-dashboard-toggle-grouping))
    (should (eq (key-binding (kbd "c")) #'drover-dashboard-toggle-children))
    (should (eq (key-binding (kbd "TAB")) #'magit-section-toggle))
    (should (eq (key-binding (kbd "n")) #'magit-section-forward))
    (should (eq (key-binding (kbd "p")) #'magit-section-backward))
    (should (eq (key-binding (kbd "RET")) #'drover-open-at-point))
    ;; Peeking wins over the scrolling of special-mode.
    (should (eq (key-binding (kbd "SPC")) #'drover-peek-at-point))
    (let ((parent (keymap-parent drover-dashboard-mode-map)))
      (should (eq (nth 1 parent) drover-session-map))
      (should (eq (nth 2 parent) magit-section-mode-map)))
    (drover-dashboard-test-goto-group 'working)
    (should (eq (key-binding (kbd "RET")) #'magit-section-toggle))))

(ert-deftest drover-dashboard-test-ret-toggles-group ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (drover-dashboard-test-goto-group 'working)
    (drover-dashboard-test-press "RET")
    (should (drover-dashboard-test-group-hidden-p 'working))
    (drover-dashboard-test-press "RET")
    (should-not (drover-dashboard-test-group-hidden-p 'working))
    (should-not drover-test-calls)))

(ert-deftest drover-dashboard-test-moving-through-subagents ()
  (drover-dashboard-test-with (drover-dashboard-test-family)
    (drover-dashboard-test-goto "root")
    (drover-dashboard-test-press "n")
    (should (equal (drover-dashboard-test-id-at-point) "sub"))
    (drover-dashboard-test-press "n")
    (should (equal (drover-dashboard-test-id-at-point) "deep"))
    (drover-dashboard-test-press "p")
    (should (equal (drover-dashboard-test-id-at-point) "sub"))
    (drover-dashboard-test-press "p")
    (should (equal (drover-dashboard-test-id-at-point) "root"))))

(ert-deftest drover-dashboard-test-g-refreshes ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
    (drover-dashboard-test-press "g")
    (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(done)))
    (setq drover-test-sessions (list (drover-test-session "s" :status 'idle)))
    (revert-buffer)
    (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(idle)))))

;;;; Refreshing

(ert-deftest drover-dashboard-test-refresh-follows-session ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto "work")
    (forward-char 4)
    (let ((offset (- (point) (line-beginning-position))))
      ;; It moves from Working to the top of Needs you.
      (setf (drover-session-status (drover-dashboard-test-find "work")) 'waiting)
      (setf (drover-session-since (drover-dashboard-test-find "work")) (drover-dashboard-test-ago 9999))
      (drover-dashboard-refresh)
      (should (equal (drover-dashboard-test-groups)
                     '((waiting "work" "wait1" "wait2") (done "done") (idle "idle"))))
      (should (equal (drover-dashboard-test-id-at-point) "work"))
      (should (= (- (point) (line-beginning-position)) offset)))))

(ert-deftest drover-dashboard-test-refresh-keeps-line-in-details ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto "done")
    (magit-section-show (magit-current-section))
    (forward-line 2)
    (let ((line (drover-dashboard-test-line)))
      ;; A new group above pushes every line down.
      (push (drover-test-session "new" :status 'failed) drover-test-sessions)
      (drover-dashboard-refresh)
      (should (assq 'failed (drover-dashboard-test-groups)))
      (should (equal (drover-dashboard-test-id-at-point) "done"))
      (should (equal (drover-dashboard-test-line) line)))))

(ert-deftest drover-dashboard-test-refresh-same-group ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto "wait1")
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait1"))
                                           drover-test-sessions))
    (drover-dashboard-refresh)
    ;; The session that took its place in the group.
    (should (equal (drover-dashboard-test-id-at-point) "wait2"))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "wait2"))
                                           drover-test-sessions))
    (drover-dashboard-refresh)
    ;; The group is gone: point stays on the same line.
    (should (equal (drover-dashboard-test-id-at-point) "done"))))

(ert-deftest drover-dashboard-test-refresh-into-collapsed-group ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto "work")
    (setf (drover-session-status (drover-dashboard-test-find "work")) 'idle)
    (drover-dashboard-refresh)
    ;; Idle is collapsed: its heading stands for the session.
    (should (drover-dashboard-test-group-hidden-p 'idle))
    (should (equal (drover-dashboard-test-line) "Idle (2)"))
    (should-not (invisible-p (point)))))

(ert-deftest drover-dashboard-test-refresh-keeps-group-heading ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto-group 'done)
    (setq drover-test-sessions (cons (drover-test-session "new" :status 'waiting) drover-test-sessions))
    (drover-dashboard-refresh)
    (should (equal (drover-dashboard-test-line) "Done (1)"))))

(ert-deftest drover-dashboard-test-refresh-keeps-window-point-and-start ()
  (drover-dashboard-test-with (cl-loop for i below 60
                                       collect (drover-test-session (format "s%02d" i) :status 'working
                                                                    :updated (- 1000.0 i)))
    (save-window-excursion
      (let ((window (selected-window)))
        (set-window-buffer window buffer)
        (drover-dashboard-test-goto "s30")
        (set-window-point window (point))
        (let ((start (save-excursion (forward-line -5) (point))))
          (set-window-start window start)
          (let ((start-line (line-number-at-pos start)))
            ;; s45 moves to the top: everything shifts down a line.
            (setf (drover-session-updated (nth 45 drover-test-sessions)) 2000.0)
            (drover-dashboard-refresh)
            (should (equal (with-selected-window window (drover-dashboard-test-id-at-point)) "s30"))
            (should (equal (save-excursion (goto-char (window-point window))
                                           (drover-dashboard-test-id-at-point))
                           "s30"))
            (should (= (line-number-at-pos (window-start window)) start-line))))))))

(ert-deftest drover-dashboard-test-collapse-state-preserved ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (should (drover-dashboard-test-group-hidden-p 'idle))
    (should-not (drover-dashboard-test-group-hidden-p 'waiting))
    (drover-dashboard-test-goto-group 'idle)
    (magit-section-show (magit-current-section))
    (drover-dashboard-test-goto-group 'waiting)
    (magit-section-hide (magit-current-section))
    (drover-dashboard-test-goto "work")
    (magit-section-show (magit-current-section))
    ;; Reorder: done becomes idle, work becomes waiting.
    (setf (drover-session-status (drover-dashboard-test-find "done")) 'idle)
    (setf (drover-session-status (drover-dashboard-test-find "work")) 'waiting)
    (drover-dashboard-refresh)
    (should-not (drover-dashboard-test-group-hidden-p 'idle))
    (should (drover-dashboard-test-group-hidden-p 'waiting))
    ;; The session stays expanded in its new group.
    (should (member "work" (alist-get 'waiting (drover-dashboard-test-groups))))
    (should-not (drover-dashboard-test-section-hidden-p "work"))
    (should (drover-dashboard-test-section-hidden-p "idle"))
    ;; And across a change of grouping and back.
    (drover-dashboard-toggle-grouping)
    (should-not (drover-dashboard-test-section-hidden-p "work"))
    (drover-dashboard-toggle-grouping)
    (should (drover-dashboard-test-group-hidden-p 'waiting))
    (should-not (drover-dashboard-test-group-hidden-p 'idle))))

(ert-deftest drover-dashboard-test-collapse-state-of-vanished-group ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto-group 'done)
    (magit-section-hide (magit-current-section))
    (let ((sessions drover-test-sessions))
      (setq drover-test-sessions (seq-remove (lambda (s) (eq (drover-session-status s) 'done)) sessions))
      (drover-dashboard-refresh)
      (should-not (assq 'done (drover-dashboard-test-groups)))
      (setq drover-test-sessions sessions)
      (drover-dashboard-refresh)
      (should (drover-dashboard-test-group-hidden-p 'done)))))

(ert-deftest drover-dashboard-test-collapsed-statuses-option ()
  (let ((drover-dashboard-collapsed-statuses '(working)))
    (drover-dashboard-test-with (drover-dashboard-test-sample)
      (should (drover-dashboard-test-group-hidden-p 'working))
      (should-not (drover-dashboard-test-group-hidden-p 'idle)))))

(ert-deftest drover-dashboard-test-forgets-gone-sessions ()
  (drover-dashboard-test-with (drover-dashboard-test-sample)
    (drover-dashboard-test-goto "work")
    (magit-section-show (magit-current-section))
    (drover-dashboard-refresh)
    (should (eq (gethash '(session fake . "work") drover-dashboard--visibility) 'show))
    (setq drover-test-sessions (seq-remove (lambda (s) (equal (drover-session-id s) "work"))
                                           drover-test-sessions))
    (drover-dashboard-refresh)
    (should-not (gethash '(session fake . "work") drover-dashboard--visibility))
    (should (gethash '(drover-dashboard-status-section . idle) drover-dashboard--visibility))
    ;; Magit's own cache, which never forgets, stays unused.
    (should-not magit-section-visibility-cache)))

(ert-deftest drover-dashboard-test-redraws-when-visible ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
      (drover-backend-changed 'fake "s")
      (drover-changed-now)
      (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(done)))
      (should-not drover-dashboard--stale))))

(ert-deftest drover-dashboard-test-stale-when-hidden ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (should-not (get-buffer-window buffer t))
    (let ((text (buffer-string)))
      (setq drover-test-sessions (list (drover-test-session "s" :status 'done)))
      (drover-backend-changed 'fake "s")
      (drover-changed-now)
      ;; Nothing was drawn.
      (should (equal (buffer-string) text))
      (should drover-dashboard--stale))
    (should (memq #'drover-dashboard--on-window-change
                  (buffer-local-value 'window-buffer-change-functions buffer)))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (drover-dashboard--on-window-change (selected-window))
      (should-not drover-dashboard--stale)
      (should (equal (mapcar #'car (drover-dashboard-test-groups)) '(done)))
      ;; Up to date: showing it again draws nothing.
      (let ((tick (buffer-modified-tick)))
        (drover-dashboard--on-window-change (selected-window))
        (should (= tick (buffer-modified-tick)))))))

(ert-deftest drover-dashboard-test-redraws-on-width-change ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (setq drover-dashboard--width 20)
      (drover-dashboard--on-window-change (selected-window))
      (should (= drover-dashboard--width (drover-dashboard--line-width))))))

(ert-deftest drover-dashboard-test-mode-line ()
  (drover-dashboard-test-with nil
    (should (equal (drover-dashboard--mode-line-process) " by status"))
    (drover-dashboard-toggle-grouping)
    (drover-dashboard-toggle-children)
    (should (equal (drover-dashboard--mode-line-process) " by project +subagents"))))

;;;; Empty states

(ert-deftest drover-dashboard-test-empty ()
  (drover-dashboard-test-with nil
    (should (equal (buffer-string) "No sessions.\n"))
    (should-not (drover-dashboard--groups))))

(ert-deftest drover-dashboard-test-no-backend ()
  (drover-dashboard-test-with nil
    (drover-unregister-backend 'fake)
    (drover-dashboard-refresh)
    (should (string-prefix-p "No agent backend is registered." (buffer-string)))))

;;;; Entry point and life cycle

(ert-deftest drover-dashboard-test-command ()
  (drover-test-with-backend (list (drover-test-session "idle" :status 'idle)
                                  (drover-test-session "wait" :status 'waiting))
    (let ((drover-dashboard-age-refresh-interval nil)
          (drover-dashboard-buffer-name " *drover-test*"))
      (save-window-excursion
        (unwind-protect
            (progn
              (drover-dashboard)
              (should (equal (buffer-name) " *drover-test*"))
              (should (eq (window-buffer (selected-window)) (current-buffer)))
              (should (derived-mode-p 'drover-dashboard-mode))
              (should (member '(:start nil) drover-test-calls))
              (should (memq #'drover-dashboard--on-change drover-changed-hook))
              ;; Point starts on the most urgent session.
              (should (equal (drover-dashboard-test-id-at-point) "wait"))
              (goto-char (point-min))
              (drover-dashboard)
              (should (= (point) (point-min))))
          (when (get-buffer " *drover-test*")
            (kill-buffer " *drover-test*")))))))

(ert-deftest drover-dashboard-test-kill-stops-watching ()
  (drover-test-with-backend nil
    (let ((drover-dashboard-age-refresh-interval 1000)
          (drover-dashboard--timer nil)
          (drover-dashboard-buffer-name " *drover-test*"))
      (let ((buffer (drover-dashboard--buffer)))
        (should (memq #'drover-dashboard--on-change drover-changed-hook))
        (should (timerp drover-dashboard--timer))
        (kill-buffer buffer)
        (should-not (memq #'drover-dashboard--on-change drover-changed-hook))
        (should-not drover-dashboard--timer)))))

(ert-deftest drover-dashboard-test-tick ()
  (drover-dashboard-test-with (list (drover-test-session "s" :status 'working))
    (drover-dashboard--tick)
    (should drover-dashboard--stale)
    (save-window-excursion
      (set-window-buffer (selected-window) buffer)
      (drover-dashboard--tick)
      (should-not drover-dashboard--stale))))

(ert-deftest drover-dashboard-test-commands-need-dashboard ()
  (with-temp-buffer
    (should-error (drover-dashboard-refresh) :type 'user-error)
    (should-error (drover-dashboard-toggle-grouping) :type 'user-error)
    (should-error (drover-dashboard-toggle-children) :type 'user-error)))

(provide 'drover-dashboard-test)
;;; drover-dashboard-test.el ends here
