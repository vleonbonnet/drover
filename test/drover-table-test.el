;;; drover-table-test.el --- Tests of the session table  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'drover-test-helpers)
(require 'drover-table)

(defmacro drover-table-test-with-table (sessions &rest body)
  "Run BODY in a drawn table buffer, the fake backend serving SESSIONS.
The table is not shown in a window unless BODY shows it; windows are
restored afterwards."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(drover-test-with-backend ,sessions
       (let ((drover-table-age-refresh-interval nil)
             (,buffer (generate-new-buffer "*drover table test*")))
         (unwind-protect
             (save-window-excursion
               (with-current-buffer ,buffer
                 (drover-table-mode)
                 (drover-table-refresh)
                 ,@body))
           (kill-buffer ,buffer)
           (when (get-buffer drover-peek-buffer)
             (kill-buffer drover-peek-buffer)))))))

(defun drover-table-test--titled (id title &rest properties)
  "Return a fake session ID titled TITLE, with PROPERTIES.
`drover-test-session' gives its default title precedence."
  (apply #'drover-session-create :backend 'fake :id id :title title properties))

(defun drover-table-test--cells ()
  "Return the texts of the cells on this line."
  (save-excursion
    (let ((end (line-end-position))
          cells)
      (goto-char (line-beginning-position))
      (while (< (point) end)
        (let ((column (get-text-property (point) 'vtable-column))
              (next (next-single-property-change (point) 'vtable-column nil end)))
          (when column
            (push (string-trim (buffer-substring-no-properties (point) next)) cells))
          (goto-char next)))
      (nreverse cells))))

(defun drover-table-test--rows ()
  "Return the rows of the table, as lists (ID . CELLS)."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when-let* ((session (get-text-property (point) 'vtable-object)))
          (push (cons (drover-session-id session) (drover-table-test--cells)) rows))
        (forward-line 1))
      (nreverse rows))))

(defun drover-table-test--ids ()
  "Return the identifiers of the sessions in the table, in order."
  (mapcar #'car (drover-table-test--rows)))

(defun drover-table-test--cell (id column)
  "Return the text of COLUMN, a header, in the row of session ID."
  (nth (seq-position '("Status" "Title" "Project" "Branch" "Age" "Summary") column)
       (cdr (assoc id (drover-table-test--rows)))))

(defun drover-table-test--goto (id &optional column)
  "Move to the row of session ID, on COLUMN, an index, when non-nil."
  (goto-char (point-min))
  (while (not (equal (drover-table-test--id-at-point) id))
    (when (eobp)
      (error "No row for %s" id))
    (forward-line 1))
  (when column
    (vtable-goto-column column)))

(defun drover-table-test--id-at-point ()
  "Return the identifier of the session on this line, or nil."
  (when-let* ((session (vtable-current-object)))
    (drover-session-id session)))

(defun drover-table-test--faces (id column)
  "Return the faces at the start of COLUMN, an index, in the row of ID."
  (save-excursion
    (drover-table-test--goto id column)
    (ensure-list (get-text-property (point) 'face))))

(defun drover-table-test--binding (key)
  "Return the command KEY runs at point."
  (key-binding (kbd key) t nil (point)))

(defun drover-table-test--press (key)
  "Run the command KEY runs at point."
  (call-interactively (drover-table-test--binding key)))

(defun drover-table-test--total-width ()
  "Return the width in pixels of the columns and separators last drawn."
  (+ (apply #'+ (append drover-table--widths nil))
     (* (1- (length drover-table--widths))
        drover-table--separator-width
        (drover-table--char-width))))

(defun drover-table-test--set-sessions (&rest sessions)
  "Make the fake backend serve SESSIONS, and report the change at once."
  (setq drover-test-sessions sessions)
  (drover-backend-changed 'fake)
  (drover-changed-now))

;;;; Rows and cells

(ert-deftest drover-table-test-rows-in-urgency-order ()
  (let ((now (float-time)))
    (drover-table-test-with-table
        (list (drover-test-session "idle" :status 'idle :updated (- now 50))
              (drover-test-session "done" :status 'done :since (- now 30))
              (drover-test-session "working" :status 'working :updated (- now 10))
              (drover-test-session "waiting" :status 'waiting :since (- now 40)))
      (should (equal (drover-table-test--ids) '("waiting" "done" "working" "idle"))))))

(ert-deftest drover-table-test-cells ()
  (let ((now (float-time)))
    (drover-table-test-with-table
        (list (drover-test-session "asking" :status 'waiting :since (- now 330) :updated (- now 5)
                                   :project "drover" :branch "main"
                                   :requests '((:kind question :summary "Which branch?")))
              (drover-test-session "broken" :status 'failed :since (- now 7300) :detail "Rate limited")
              (drover-test-session "quiet" :status 'idle :updated (- now 90000) :since (- now 5))
              (drover-test-session "child" :status 'done :parent "quiet" :project "drover")
              (drover-session-create :backend 'fake :id "untitled" :status 'working))
      (should (equal (cdr (assoc "asking" (drover-table-test--rows)))
                     '("⚑ Needs you" "Session asking" "drover" "main" "5m" "asks: Which branch?")))
      (should (equal (cdr (assoc "broken" (drover-table-test--rows)))
                     '("✗ Failed" "Session broken" "" "" "2h" "Rate limited")))
      (should (equal (cdr (assoc "quiet" (drover-table-test--rows)))
                     '("· Idle" "Session quiet" "" "" "1d" "")))
      ;; A subagent that needs the user is shown, marked as such; its age is unknown.
      (should (equal (cdr (assoc "child" (drover-table-test--rows)))
                     '("✓ Done" "↳ Session child" "drover" "" "" "")))
      (should (equal (drover-table-test--cell "untitled" "Title") "untitled"))
      (should (equal (drover-table-test--cell "untitled" "Status") "◐ Working")))))

(ert-deftest drover-table-test-column-headers ()
  (drover-table-test-with-table (list (drover-test-session "s"))
    (goto-char (point-min))
    (should (equal (drover-table-test--cells)
                   '("Status" "Title" "Project" "Branch" "Age" "Summary")))
    (should (memq 'header-line (ensure-list (get-text-property (point) 'face))))))

(ert-deftest drover-table-test-cells-on-one-line ()
  (drover-table-test-with-table
      (list (drover-table-test--titled "s" "Two\nlines" :status 'failed :detail "Error:\n  boom\tnow"))
    (should (equal (drover-table-test--cell "s" "Title") "Two lines"))
    (should (equal (drover-table-test--cell "s" "Summary") "Error: boom now"))
    (should (= (count-lines (point-min) (point-max)) 2))))

(ert-deftest drover-table-test-unknown-status ()
  (drover-table-test-with-table
      (list (drover-test-session "odd" :status 'mystery)
            (drover-test-session "idle" :status 'idle))
    (should (equal (drover-table-test--ids) '("idle" "odd")))
    (should (equal (drover-table-test--cell "odd" "Status") "? Mystery"))))

(ert-deftest drover-table-test-status-faces ()
  (drover-table-test-with-table
      (mapcar (lambda (status)
                (drover-test-session (symbol-name status) :status status :branch "main"))
              drover-status-order)
    (dolist (status drover-status-order)
      (let ((faces (drover-table-test--faces (symbol-name status) 0)))
        (should (memq (drover-status-face status) faces))
        (should (memq 'drover-table faces))))
    (should (memq 'drover-dim (drover-table-test--faces "idle" 3)))
    (should (memq 'drover-table (drover-table-test--faces "idle" 1)))))

(ert-deftest drover-table-test-age-of-since-or-updated ()
  (let ((now (float-time)))
    (drover-table-test-with-table
        (list (drover-test-session "done" :status 'done :since (- now 7300) :updated (- now 5))
              (drover-test-session "working" :status 'working :since (- now 5) :updated (- now 7300))
              (drover-test-session "waiting" :status 'waiting :updated (- now 5)))
      (should (equal (drover-table-test--cell "done" "Age") "2h"))
      (should (equal (drover-table-test--cell "working" "Age") "2h"))
      (should (equal (drover-table-test--cell "waiting" "Age") "")))))

;;;; Widths

(ert-deftest drover-table-test-share ()
  (should (equal (drover-table--share '(10 50 10 100) '(3 1 1 3) 100 6)
                 '(10 20 10 60)))
  ;; Room for every column: each gets what it needs.
  (should (equal (drover-table--share '(10 20 30) '(1 1 1) 100 6) '(10 20 30)))
  ;; What rounding down leaves goes to the last column sharing room.
  (should (equal (drover-table--share '(50 50 50) '(1 1 1) 100 6) '(33 33 34)))
  ;; No room: a column keeps its minimum, or less when it needs less.
  (should (equal (drover-table--share '(40 3) '(1 1) -10 6) '(6 3))))

(ert-deftest drover-table-test-columns-fill-default-width ()
  (drover-table-test-with-table
      (list (drover-test-session "s" :status 'failed :detail (make-string 200 ?x)))
    (should (= (drover-table-test--total-width)
               (* (1- drover-table--default-columns) (drover-table--char-width))))
    (should (string-suffix-p "…" (drover-table-test--cell "s" "Summary")))))

(ert-deftest drover-table-test-short-columns-keep-their-width ()
  (drover-table-test-with-table (list (drover-test-session "s" :project "p"))
    (should (< (drover-table-test--total-width)
               (* drover-table--default-columns (drover-table--char-width))))
    (should (equal (drover-table-test--cell "s" "Title") "Session s"))
    ;; The header of an empty column keeps room for its sort indicator.
    (should (>= (aref drover-table--widths 5)
                (+ (string-pixel-width "Summary") (string-pixel-width "▼"))))))

(ert-deftest drover-table-test-sized-to-window ()
  (drover-table-test-with-table
      (list (drover-test-session "s" :status 'failed :detail (make-string 200 ?x)))
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-refresh)
    (let ((full (window-body-width nil t)))
      (should (eql drover-table--window-width full))
      (should (= (drover-table-test--total-width) (- full (drover-table--char-width))))
      (let ((total (drover-table-test--total-width))
            (narrow (split-window-right)))
        (should (< (window-body-width nil t) full))
        ;; A width change, as `window-size-change-functions' reports it.
        (drover-table--window-changed (selected-window))
        (should (< (drover-table-test--total-width) total))
        ;; Both windows show the table, which fits the narrower.
        (let ((narrowest (min (window-body-width nil t) (window-body-width narrow t)))
              (drawn drover-table--drawn-at))
          (should (eql drover-table--window-width narrowest))
          (drover-table--window-changed narrow)
          (should (eql drover-table--drawn-at drawn)))
        (delete-window narrow)
        (drover-table--window-changed (selected-window))
        (should (= (drover-table-test--total-width) total))))))

(ert-deftest drover-table-test-window-to-size-for ()
  (drover-table-test-with-table (list (drover-test-session "s"))
    (should-not (drover-table--window (current-buffer)))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((top (selected-window))
          (below (split-window-below)))
      ;; As wide as each other: the selected window.
      (should (eq (drover-table--window (current-buffer)) top))
      (select-window below)
      (should (eq (drover-table--window (current-buffer)) below))
      (let* ((right (split-window-right))
             (narrowest (if (< (window-body-width right t) (window-body-width below t))
                            right
                          below)))
        (should (< (window-body-width narrowest t) (window-body-width top t)))
        (select-window top)
        (should (eq (drover-table--window (current-buffer)) narrowest))))))

(ert-deftest drover-table-test-narrow-and-widen-hold ()
  (drover-table-test-with-table
      (list (drover-table-test--titled "s" (make-string 40 ?t) :status 'failed
                                       :detail (make-string 200 ?x)))
    (drover-table-test--goto "s" 1)
    (let ((char-width (drover-table--char-width))
          (title (aref drover-table--widths 1)))
      (should (eq (drover-table-test--binding "{") #'drover-table-narrow-column))
      (should (eq (drover-table-test--binding "}") #'drover-table-widen-column))
      (drover-table-narrow-column 5)
      (should (= (aref drover-table--widths 1) (- title (* 5 char-width))))
      (should (string-suffix-p "…" (drover-table-test--cell "s" "Title")))
      (should (equal (drover-table-test--id-at-point) "s"))
      (should (eql (vtable-current-column) 1))
      ;; The summary takes the room given up.
      (should (= (drover-table-test--total-width)
                 (* (1- drover-table--default-columns) char-width)))
      (drover-table-refresh)
      (should (= (aref drover-table--widths 1) (- title (* 5 char-width))))
      (drover-table-widen-column 2)
      (should (= (aref drover-table--widths 1) (- title (* 3 char-width))))
      (drover-table-reset)
      (should (= (aref drover-table--widths 1) title)))))

;;;; The session at point

(ert-deftest drover-table-test-session-at-point-is-fresh ()
  (drover-table-test-with-table (list (drover-test-session "a") (drover-test-session "b"))
    (drover-table-test--goto "b")
    (let ((fresh (drover-table-test--titled "b" "Renamed")))
      (setq drover-test-sessions (list (drover-test-session "a") fresh))
      (should (eq (drover-session-at-point) fresh))
      (should (equal (drover-session-title (drover-session-at-point)) "Renamed")))
    (setq drover-test-sessions (list (drover-test-session "a")))
    (should-error (drover-session-at-point) :type 'user-error)
    (goto-char (point-min))
    (should-error (drover-session-at-point) :type 'user-error)
    (goto-char (point-max))
    (should-error (drover-session-at-point) :type 'user-error)))

;;;; Keys

(ert-deftest drover-table-test-keys-reach-backend ()
  (drover-table-test-with-table (list (drover-test-session "a") (drover-test-session "b"))
    (drover-table-test--goto "b" 2)
    (setq drover-test-calls nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (dolist (key '("RET" "o" "a" "C-c C-q" "k" "A" "s" "SPC"))
        (drover-table-test--press key)))
    (should (equal (reverse drover-test-calls)
                   '((:open "b" nil) (:open "b" t) (:respond "b") (:respond "b")
                     (:interrupt "b") (:archive "b") (:mark-seen "b") (:preview "b"))))
    (should (string-match-p "Preview of b" (with-current-buffer drover-peek-buffer
                                             (buffer-string))))))

(ert-deftest drover-table-test-declined-actions ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (drover-table-test--goto "a")
    (setq drover-test-calls nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (drover-table-test--press "k")
      (drover-table-test--press "A"))
    (should-not drover-test-calls)))

(ert-deftest drover-table-test-keys-through-command-loop ()
  (drover-table-test-with-table (list (drover-test-session "a") (drover-test-session "b"))
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-test--goto "b")
    (set-window-point (selected-window) (point))
    (setq drover-test-calls nil)
    (execute-kbd-macro (kbd "a"))
    (should (equal drover-test-calls '((:respond "b"))))))

(ert-deftest drover-table-test-key-conflicts ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (should (eq (keymap-parent drover-table-mode-map) drover-session-map))
    ;; On a row, vtable's keymap is active as well; on the header, its
    ;; header keymap.  Both must leave the shared keys alone.
    (dolist (where (list (lambda () (drover-table-test--goto "a" 1))
                         (lambda () (goto-char (point-min)))))
      (funcall where)
      (should (eq (drover-table-test--binding "g") #'drover-table-refresh))
      (should (eq (drover-table-test--binding "q") #'quit-window))
      (should (eq (drover-table-test--binding "c") #'drover-table-toggle-children))
      (should (eq (drover-table-test--binding "=") #'drover-table-reset))
      (should (eq (drover-table-test--binding "S") #'vtable-sort-by-current-column))
      (should (eq (drover-table-test--binding "SPC") #'drover-peek-at-point))
      (should (eq (drover-table-test--binding "RET") #'drover-open-at-point))
      (should (eq (drover-table-test--binding "a") #'drover-respond-at-point))
      (should (eq (drover-table-test--binding "s") #'drover-mark-seen-at-point))
      (should (eq (drover-table-test--binding "DEL") #'scroll-down-command))
      (should (eq (drover-table-test--binding "x") #'undefined))
      (should (eq (drover-table-test--binding "h") #'describe-mode)))))

(ert-deftest drover-table-test-revert-buffer ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (setq drover-test-sessions (list (drover-test-session "a") (drover-test-session "b")))
    (revert-buffer)
    (should (equal (drover-table-test--ids) '("a" "b")))))

(ert-deftest drover-table-test-commands-outside-table ()
  (with-temp-buffer
    (should-error (drover-table-refresh) :type 'user-error)
    (should-error (drover-table-toggle-children) :type 'user-error)))

;;;; Refreshing

(ert-deftest drover-table-test-refresh-keeps-session ()
  (drover-table-test-with-table
      (list (drover-test-session "a" :status 'waiting :since 1.0)
            (drover-test-session "b" :status 'working :updated 2.0)
            (drover-test-session "c" :status 'idle :updated 3.0))
    (drover-table-test--goto "b" 1)
    (setq drover-test-sessions
          (list (drover-test-session "a" :status 'idle :updated 1.0)
                (drover-test-session "b" :status 'idle :updated 2.0)
                (drover-test-session "c" :status 'waiting :since 3.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("c" "b" "a")))
    (should (equal (drover-table-test--id-at-point) "b"))
    (should (eql (vtable-current-column) 1))))

(ert-deftest drover-table-test-refresh-near-vanished-session ()
  (drover-table-test-with-table
      (list (drover-test-session "a" :updated 3.0)
            (drover-test-session "b" :updated 2.0)
            (drover-test-session "c" :updated 1.0))
    (drover-table-test--goto "b")
    (setq drover-test-sessions (list (drover-test-session "a" :updated 3.0)
                                     (drover-test-session "c" :updated 1.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--id-at-point) "c"))
    (setq drover-test-sessions (list (drover-test-session "a" :updated 3.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--id-at-point) "a"))))

(ert-deftest drover-table-test-refresh-keeps-window-points ()
  (drover-table-test-with-table
      (list (drover-test-session "a" :updated 3.0)
            (drover-test-session "b" :updated 2.0)
            (drover-test-session "c" :updated 1.0))
    (set-window-buffer (selected-window) (current-buffer))
    (let ((other (split-window-below)))
      (set-window-buffer other (current-buffer))
      (drover-table-test--goto "a")
      (set-window-point (selected-window) (point))
      (set-window-point other (save-excursion (drover-table-test--goto "c") (point)))
      (setq drover-test-sessions (list (drover-test-session "a" :updated 1.0)
                                       (drover-test-session "b" :updated 2.0)
                                       (drover-test-session "c" :updated 3.0)))
      (drover-table-refresh)
      (should (equal (drover-table-test--ids) '("c" "b" "a")))
      (should (equal (drover-table-test--id-at-point) "a"))
      (should (equal (save-excursion (goto-char (window-point other))
                                     (drover-table-test--id-at-point))
                     "c")))))

(ert-deftest drover-table-test-first-draw-on-first-row ()
  (drover-table-test-with-table nil
    (drover-table-test--set-sessions (drover-test-session "a") (drover-test-session "b"))
    (drover-table-refresh)
    (should (equal (drover-table-test--id-at-point) "a"))))

(ert-deftest drover-table-test-user-sort-holds ()
  (drover-table-test-with-table
      (list (drover-test-session "b" :status 'waiting :since 1.0)
            (drover-test-session "c" :status 'idle)
            (drover-test-session "a" :status 'done :since 1.0))
    (should (equal (drover-table-test--ids) '("b" "a" "c")))
    (drover-table-test--goto "b" 1)
    (drover-table-test--press "S")
    (should (equal (drover-table-test--ids) '("a" "b" "c")))
    (setq drover-test-sessions (cons (drover-test-session "0" :status 'idle) drover-test-sessions))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("0" "a" "b" "c")))
    (should (equal (drover-table-test--id-at-point) "b"))
    (drover-table-test--press "S")
    (should (equal (drover-table-test--ids) '("c" "b" "a" "0")))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("c" "b" "a" "0")))
    (drover-table-reset)
    (should (equal (drover-table-test--ids) '("b" "a" "0" "c")))
    (should (equal (drover-table-test--id-at-point) "b"))))

(ert-deftest drover-table-test-user-sort-survives-empty-table ()
  (drover-table-test-with-table (list (drover-test-session "b") (drover-test-session "a"))
    (drover-table-test--goto "b" 1)
    (drover-table-test--press "S")
    (drover-table-test--set-sessions)
    (drover-table-refresh)
    (setq drover-test-sessions (list (drover-test-session "b") (drover-test-session "a")))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("a" "b")))))

(ert-deftest drover-table-test-sort-status-and-age-numerically ()
  (let ((now (float-time)))
    (drover-table-test-with-table
        (list (drover-test-session "hour" :status 'idle :updated (- now 3700))
              (drover-test-session "seconds" :status 'working :updated (- now 50))
              (drover-test-session "minutes" :status 'waiting :since (- now 330))
              (drover-test-session "unknown" :status 'done))
      (drover-table-test--goto "hour" 4)
      (drover-table-test--press "S")
      ;; Youngest first; a string sort would put "1h" first.
      (should (equal (drover-table-test--ids) '("seconds" "minutes" "hour" "unknown")))
      (drover-table-test--goto "hour" 0)
      (drover-table-test--press "S")
      (drover-table-test--press "S")
      (should (equal (drover-table-test--ids) '("hour" "seconds" "unknown" "minutes"))))))

;;;; Subagents

(ert-deftest drover-table-test-toggle-children ()
  (drover-table-test-with-table
      (list (drover-test-session "root" :updated 2.0)
            (drover-test-session "child" :parent "root" :updated 1.0))
    (should (equal (drover-table-test--ids) '("root")))
    (should (string-match-p "1 subagent hidden" header-line-format))
    (drover-table-test--press "c")
    (should (equal (drover-table-test--ids) '("root" "child")))
    (should (equal (drover-table-test--cell "child" "Title") "↳ Session child"))
    (should (local-variable-p 'drover-show-children))
    (should-not (default-value 'drover-show-children))
    (should-not (string-match-p "hidden" header-line-format))
    (drover-table-toggle-children)
    (should (equal (drover-table-test--ids) '("root")))))

;;;; Following the backends

(ert-deftest drover-table-test-shown-table-follows-changes ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-test--set-sessions (drover-test-session "a") (drover-test-session "b"))
    (should (equal (drover-table-test--ids) '("a" "b")))
    (should-not drover-table--stale)))

(ert-deftest drover-table-test-hidden-table-goes-stale ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (should-not (get-buffer-window (current-buffer) t))
    (let ((drawn drover-table--drawn-at))
      (drover-table-test--set-sessions (drover-test-session "a") (drover-test-session "b"))
      (should drover-table--stale)
      (should (equal (drover-table-test--ids) '("a")))
      (should (eql drover-table--drawn-at drawn)))
    ;; Shown again, as `window-buffer-change-functions' reports it.
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table--window-changed (selected-window))
    (should-not drover-table--stale)
    (should (equal (drover-table-test--ids) '("a" "b")))
    (let ((drawn drover-table--drawn-at))
      (drover-table--window-changed (selected-window))
      (should (eql drover-table--drawn-at drawn)))))

(ert-deftest drover-table-test-old-table-outdated ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-refresh)
    (should-not (drover-table--outdated-p (selected-window)))
    (let ((drover-table-age-refresh-interval 15))
      (should-not (drover-table--outdated-p (selected-window)))
      (setq drover-table--drawn-at (- (float-time) 20))
      (should (drover-table--outdated-p (selected-window))))))

(ert-deftest drover-table-test-age-timer ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (let ((drover-table-age-refresh-interval 15)
          (drover-table--age-timer nil))
      (unwind-protect
          (progn
            (drover-table-refresh)
            (should-not drover-table--age-timer)
            (set-window-buffer (selected-window) (current-buffer))
            (drover-table-refresh)
            (should (timerp drover-table--age-timer))
            (setq drover-test-sessions (list (drover-test-session "a") (drover-test-session "b")))
            (drover-table--tick)
            (should (equal (drover-table-test--ids) '("a" "b")))
            (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
            (drover-table--tick)
            (should-not drover-table--age-timer))
        (drover-table--stop-age-timer)))))

(ert-deftest drover-table-test-last-table-unhooks ()
  (drover-test-with-backend nil
    (let ((drover-table-age-refresh-interval nil)
          (first (generate-new-buffer "*drover table test*"))
          (second (generate-new-buffer "*drover table test*")))
      (with-current-buffer first (drover-table-mode))
      (with-current-buffer second (drover-table-mode))
      (should (memq #'drover-table--changed drover-changed-hook))
      (kill-buffer first)
      (should (memq #'drover-table--changed drover-changed-hook))
      (kill-buffer second)
      (should-not (memq #'drover-table--changed drover-changed-hook)))))

;;;; Empty state and header

(ert-deftest drover-table-test-empty-state ()
  (drover-table-test-with-table nil
    (should (equal (buffer-string) "No agent sessions.\n"))
    (should-not drover-table--table)
    (should-error (drover-session-at-point) :type 'user-error)
    (should (string-match-p "Nothing needs you" header-line-format))
    (should (string-match-p "0 sessions" header-line-format))
    (drover-table-test--set-sessions (drover-test-session "root")
                                     (drover-test-session "child" :parent "root"))
    (drover-table-test--set-sessions (drover-test-session "child" :parent "root"))
    (drover-table-refresh)
    (should (equal (buffer-string)
                   "No agent sessions; 1 subagent session is hidden, c shows them.\n"))
    (drover-unregister-backend 'fake)
    (drover-table-refresh)
    (should (equal (buffer-string) "No agent backend is loaded.\n"))))

(ert-deftest drover-table-test-header-counts ()
  (drover-table-test-with-table
      (list (drover-test-session "w1" :status 'waiting :since 1.0)
            (drover-test-session "w2" :status 'waiting :since 2.0)
            (drover-test-session "f" :status 'failed :since 1.0)
            (drover-test-session "r" :status 'working)
            (drover-test-session "hidden" :status 'working :parent "r"))
    (should (equal (substring-no-properties header-line-format)
                   " ⚑ 2 Needs you   ✗ 1 Failed   4 sessions, 1 subagent hidden"))
    (should (eq (get-text-property 1 'face header-line-format) 'drover-waiting))
    (should (eq (get-text-property (string-search "1 Failed" header-line-format) 'face
                                   header-line-format)
                'drover-failed))))

(ert-deftest drover-table-test-header-escapes-percent ()
  (let ((drover-status-labels '((waiting . "100% you"))))
    (drover-table-test-with-table (list (drover-test-session "w" :status 'waiting))
      (should (string-match-p "1 100%% you" header-line-format)))))

;;;; The command

(ert-deftest drover-table-test-command ()
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b"))
    (let ((drover-table-age-refresh-interval nil))
      (save-window-excursion
        (unwind-protect
            (progn
              (drover-table)
              (should (equal (buffer-name) drover-table-buffer-name))
              (should (derived-mode-p 'drover-table-mode))
              (should (eq (window-buffer (selected-window)) (current-buffer)))
              (should (member '(:start nil) drover-test-calls))
              (should (equal (drover-table-test--ids) '("a" "b")))
              (should (equal (drover-table-test--id-at-point) "a"))
              (should (eql drover-table--window-width (window-body-width nil t)))
              (drover-table-test--goto "b")
              (drover-table)
              (should (equal (drover-table-test--id-at-point) "b")))
          (when (get-buffer drover-table-buffer-name)
            (kill-buffer drover-table-buffer-name)))))))

(provide 'drover-table-test)
;;; drover-table-test.el ends here
