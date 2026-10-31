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
(require 'ol)

(defmacro drover-table-test-with-table (sessions &rest body)
  "Run BODY in a drawn table buffer, the fake backend serving SESSIONS.
The table is not shown in a window unless BODY shows it; windows are
restored afterwards.  Status glyphs are readable ones, whatever the
defaults: in batch, \"⚑\" is two columns wide and the others one."
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(drover-test-with-backend ,sessions
       (let ((drover-table-age-refresh-interval nil)
             (drover-status-glyphs '((waiting . "⚑") (failed . "✗") (done . "✓")
                                     (retrying . "↻") (working . "◐") (idle . "·")))
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
  "Return the rows of the sessions of the table, as lists (ID . CELLS)."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when-let* ((session (get-text-property (point) 'vtable-object))
                    ((drover-session-p session)))
          (push (cons (drover-session-id session) (drover-table-test--cells)) rows))
        (forward-line 1))
      (nreverse rows))))

(defun drover-table-test--lines ()
  "Return the rows of the table in order, sessions and group headings.
A session is its identifier, a group heading the list (group TITLE),
TITLE the text of its first cell."
  (save-excursion
    (goto-char (point-min))
    (let (lines)
      (while (not (eobp))
        (when-let* ((object (get-text-property (point) 'vtable-object)))
          (push (if (drover-session-p object)
                    (drover-session-id object)
                  (list 'group (car (drover-table-test--cells))))
                lines))
        (forward-line 1))
      (nreverse lines))))

(defun drover-table-test--ids ()
  "Return the identifiers of the sessions in the table, in order."
  (mapcar #'car (drover-table-test--rows)))

(defun drover-table-test--cell (id column)
  "Return the text of COLUMN, a header, in the row of session ID."
  (nth (seq-position '("Title" "Project" "Branch" "Age" "Summary") column)
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
  (when-let* ((session (vtable-current-object))
              ((drover-session-p session)))
    (drover-session-id session)))

(defun drover-table-test--goto-group (title)
  "Move to the heading of the group whose first cell reads TITLE."
  (goto-char (point-min))
  (while (not (and (drover-table--group-p (vtable-current-object))
                   (equal (car (drover-table-test--cells)) title)))
    (when (eobp)
      (error "No group %s" title))
    (forward-line 1)))

(defun drover-table-test--show ()
  "Show the table in the selected window, its window point at point.
Keys run through the command loop act there, and view settings redraw
only shown tables."
  (let ((position (point)))
    (set-window-buffer (selected-window) (current-buffer))
    (set-window-point (selected-window) position)
    (goto-char position)))

(defun drover-table-test--group-by (grouping)
  "Group the sessions by GROUPING, and draw the table again."
  (drover-set-group-by grouping)
  (drover-table-refresh))

(defun drover-table-test--faces (id column)
  "Return the faces at the start of COLUMN, an index, in the row of ID."
  (save-excursion
    (drover-table-test--goto id column)
    (ensure-list (get-text-property (point) 'face))))

(defun drover-table-test--title (id)
  "Return the title cell of session ID as drawn, without vtable's padding."
  (save-excursion
    (drover-table-test--goto id 0)
    (buffer-substring (point) (1- (next-single-property-change
                                   (point) 'vtable-column nil (line-end-position))))))

(defun drover-table-test--width-at (string index)
  "Return the width of the character at INDEX in STRING.
That is the pixel width of its display space, or nil when it has none."
  (pcase (get-text-property index 'display string)
    (`(space :width (,pixels)) pixels)))

(defun drover-table-test--indent (id)
  "Return the pixel width the title of session ID is indented by."
  (let ((title (drover-table-test--title id)))
    (or (and (equal (substring-no-properties title 0 1) " ")
             (drover-table-test--width-at title 0))
        0)))

(defun drover-table-test--binding (key)
  "Return the command KEY runs at point."
  (key-binding (kbd key) t nil (point)))

(defun drover-table-test--press (key)
  "Run the command KEY runs at point, then `post-command-hook'.
The hook runs as the command loop would run it."
  (call-interactively (drover-table-test--binding key))
  (run-hooks 'post-command-hook))

(defun drover-table-test--sort (column)
  "Sort the table by COLUMN, an index, from its header, as a click does."
  (goto-char (point-min))
  (vtable-goto-column column)
  (drover-table-test--press "S"))

(defun drover-table-test--header ()
  "Return the texts of the column headers."
  (save-excursion
    (goto-char (point-min))
    (drover-table-test--cells)))

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
                     '("⚑ Session asking" "drover" "main" "5m" "asks: Which branch?")))
      (should (equal (cdr (assoc "broken" (drover-table-test--rows)))
                     '("✗ Session broken" "" "" "2h" "Rate limited")))
      (should (equal (cdr (assoc "quiet" (drover-table-test--rows)))
                     '("· Session quiet" "" "" "1d" "")))
      ;; A subagent that needs the user is shown; its age is unknown.
      (should (equal (cdr (assoc "child" (drover-table-test--rows)))
                     '("✓ Session child" "drover" "" "" "")))
      (should (equal (drover-table-test--cell "untitled" "Title") "◐ untitled")))))

(ert-deftest drover-table-test-column-headers ()
  (drover-table-test-with-table (list (drover-test-session "s"))
    (should (equal (drover-table-test--header) '("Title" "Project" "Branch" "Age" "Summary")))
    (goto-char (point-min))
    (should (memq 'header-line (ensure-list (get-text-property (point) 'face))))))

(ert-deftest drover-table-test-cells-on-one-line ()
  (drover-table-test-with-table
      (list (drover-table-test--titled "s" "Two\nlines" :status 'failed :detail "Error:\n  boom\tnow"))
    (should (equal (drover-table-test--cell "s" "Title") "✗ Two lines"))
    (should (equal (drover-table-test--cell "s" "Summary") "Error: boom now"))
    ;; The column header, the heading of the group, and the row.
    (should (= (count-lines (point-min) (point-max)) 3))))

(ert-deftest drover-table-test-unknown-status ()
  (drover-table-test-with-table
      (list (drover-test-session "odd" :status 'mystery)
            (drover-test-session "idle" :status 'idle))
    (should (equal (drover-table-test--ids) '("idle" "odd")))
    ;; A status `drover-status-order' lacks has a group of its own, last.
    (should (equal (drover-table-test--lines)
                   '((group "Idle (1)") "idle" (group "Mystery (1)") "odd")))
    (should (equal (drover-table-test--cell "odd" "Title") "? Session odd"))))

(ert-deftest drover-table-test-title-cell ()
  "The title cell is the glyph, exactly one space, then the title."
  (drover-table-test-with-table
      (list (drover-test-session "wide" :status 'waiting :since 1.0)
            (drover-test-session "narrow" :status 'done :since 2.0)
            (drover-test-session "child" :status 'failed :since 3.0 :parent "narrow")
            (drover-test-session "grandchild" :status 'done :since 4.0 :parent "child"))
    (should (equal (drover-table-test--ids) '("wide" "narrow" "child" "grandchild")))
    (pcase-dolist (`(,id ,text) '(("wide" "⚑ Session wide") ("narrow" "✓ Session narrow")))
      (should (equal (substring-no-properties (drover-table-test--title id)) text)))
    (let ((wide (drover-table-test--title "wide"))
          (narrow (drover-table-test--title "narrow")))
      ;; The widest glyph is followed by a plain space; a narrower one by
      ;; a space as much wider, so that the titles line up.
      (should-not (get-text-property 1 'display wide))
      (should (= (drover-table-test--width-at narrow 1)
                 (+ (string-pixel-width " ")
                    (- (string-pixel-width "⚑") (string-pixel-width "✓"))))))
    ;; A subagent is indented by a glyph and a space for each level, so
    ;; that its glyph stands under the title of its parent.
    (let ((step (+ (string-pixel-width "⚑") (string-pixel-width " "))))
      (pcase-dolist (`(,id ,depth ,text) '(("wide" 0 "⚑ Session wide")
                                           ("child" 1 " ✗ Session child")
                                           ("grandchild" 2 " ✓ Session grandchild")))
        (should (= (drover-table-test--indent id) (* depth step)))
        (should (equal (substring-no-properties (drover-table-test--title id)) text))))))

(ert-deftest drover-table-test-status-tooltip ()
  (drover-table-test-with-table
      (list (drover-test-session "asking" :status 'waiting)
            (drover-test-session "odd" :status 'mystery)
            (drover-test-session "child" :status 'done :parent "odd"))
    (pcase-dolist (`(,id ,offset ,label) '(("asking" 0 "Needs you") ("odd" 0 "Mystery")
                                           ("child" 1 "Done")))
      (drover-table-test--goto id 0)
      (forward-char offset)
      (should (equal (get-text-property (point) 'help-echo) label))
      ;; The title itself has no tooltip.
      (forward-char 2)
      (should-not (get-text-property (point) 'help-echo)))))

(ert-deftest drover-table-test-status-faces ()
  (drover-table-test-with-table
      (mapcar (lambda (status)
                (drover-test-session (symbol-name status) :status status
                                     :project "drover" :branch "main"))
              drover-status-order)
    (dolist (status drover-status-order)
      (let ((faces (drover-table-test--faces (symbol-name status) 0)))
        (should (memq (drover-status-face status) faces))
        (should (memq 'drover-table faces)))
      ;; The title after the glyph is in the face of the table alone.
      (save-excursion
        (drover-table-test--goto (symbol-name status) 0)
        (forward-char 2)
        (should (equal (ensure-list (get-text-property (point) 'face)) '(drover-table)))))
    (should (memq 'drover-dim (drover-table-test--faces "idle" 2)))
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

;;;; Trees

(ert-deftest drover-table-test-subagents-under-parent ()
  (drover-table-test-with-table
      (list (drover-test-session "grandchild" :parent "quiet" :status 'idle :updated 1.0)
            (drover-test-session "other" :status 'idle :updated 9.0)
            (drover-test-session "quiet" :parent "root" :status 'idle :updated 4.0)
            (drover-test-session "root" :status 'working :updated 5.0)
            (drover-test-session "busy" :parent "root" :status 'working :updated 6.0))
    (setq drover-show-children t)
    (drover-table-refresh)
    ;; Siblings come most urgent first, each before its own subagents.
    (should (equal (drover-table-test--ids) '("root" "busy" "quiet" "grandchild" "other")))
    (let ((step (+ (string-pixel-width "◐") (string-pixel-width " "))))
      (should (equal (mapcar #'drover-table-test--indent (drover-table-test--ids))
                     (list 0 step step (* 2 step) 0))))))

(ert-deftest drover-table-test-rolled-up-status ()
  "A parent whose subagent waits for the user sorts with the waiting sessions."
  (drover-table-test-with-table
      (list (drover-test-session "busy" :status 'working :updated 100.0)
            (drover-test-session "parent" :status 'idle :updated 1.0)
            (drover-test-session "quiet" :parent "parent" :status 'idle)
            (drover-test-session "asking" :parent "parent" :status 'waiting :since 30.0)
            (drover-test-session "waiting" :status 'waiting :since 40.0))
    (should (equal (drover-table-test--ids) '("parent" "asking" "waiting" "busy")))
    ;; The parent shows its own status, and the header counts sessions.
    (should (equal (drover-table-test--cell "parent" "Title") "· Session parent"))
    (should (equal (substring-no-properties header-line-format)
                   " ⚑ 2 Needs you   4 sessions, 1 subagent hidden"))))

(ert-deftest drover-table-test-orphan-is-own-tree ()
  (drover-table-test-with-table
      (list (drover-test-session "orphan" :parent "gone" :status 'idle :updated 2.0)
            (drover-test-session "root" :status 'working :updated 1.0)
            (drover-test-session "child" :parent "orphan" :status 'idle))
    ;; Shown although subagents are hidden, as a root, without indentation.
    (should (equal (drover-table-test--ids) '("root" "orphan")))
    (should (= (drover-table-test--indent "orphan") 0))
    (should (string-match-p "2 sessions, 1 subagent hidden" header-line-format))
    (drover-table-test--show)
    (execute-kbd-macro (kbd "U"))
    (should (equal (drover-table-test--ids) '("root" "orphan" "child")))
    (should (> (drover-table-test--indent "child") 0))))

(ert-deftest drover-table-test-toggle-subagents ()
  "\\`U' shows subagents in every view, and the shown table follows at once."
  (drover-table-test-with-table
      (list (drover-test-session "root" :updated 2.0)
            (drover-test-session "child" :parent "root" :updated 1.0)
            (drover-test-session "asking" :parent "root" :status 'waiting :since 1.0)
            (drover-test-session "other" :updated 3.0))
    (should (equal (drover-table-test--ids) '("root" "asking" "other")))
    (should (string-match-p "3 sessions, 1 subagent hidden" header-line-format))
    (drover-table-test--goto "other" 1)
    (drover-table-test--show)
    (should (eq (drover-table-test--binding "U") #'drover-toggle-subagents))
    (execute-kbd-macro (kbd "U"))
    (should (equal (drover-table-test--ids) '("root" "asking" "child" "other")))
    (should (equal (drover-table-test--cell "child" "Title") "· Session child"))
    (should (= (drover-table-test--indent "child") (drover-table-test--indent "asking")))
    ;; Point stays on its session and column.
    (should (equal (drover-table-test--id-at-point) "other"))
    (should (eql (vtable-current-column) 1))
    ;; The setting is every view's, not the table's.
    (should-not (local-variable-p 'drover-show-children))
    (should drover-show-children)
    (should-not (string-match-p "hidden" header-line-format))
    (execute-kbd-macro (kbd "U"))
    (should-not drover-show-children)
    (should (equal (drover-table-test--ids) '("root" "asking" "other")))
    (should (equal (drover-table-test--id-at-point) "other"))))

(ert-deftest drover-table-test-refresh-keeps-nested-session ()
  (drover-table-test-with-table
      (list (drover-test-session "first" :status 'working :updated 5.0)
            (drover-test-session "root" :status 'idle :updated 2.0)
            (drover-test-session "child" :parent "root" :status 'idle :updated 1.0))
    ;; Sorting crosses status groups.
    (setq drover-show-children t)
    (drover-table-test--group-by nil)
    (should (equal (drover-table-test--ids) '("first" "root" "child")))
    (drover-table-test--goto "child" 2)
    ;; The child's tree moves up and gains a sibling before the child.
    (setq drover-test-sessions
          (list (drover-test-session "first" :status 'working :updated 5.0)
                (drover-test-session "root" :status 'idle :updated 2.0)
                (drover-test-session "child" :parent "root" :status 'idle :updated 1.0)
                (drover-test-session "asking" :parent "root" :status 'waiting :since 1.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("root" "asking" "child" "first")))
    (should (equal (drover-table-test--id-at-point) "child"))
    (should (eql (vtable-current-column) 2))
    (drover-table-test--goto "first")
    (drover-table-test--sort 0)
    (should (equal (drover-table-test--ids) '("first" "root" "asking" "child")))
    ;; Sorted again, now descending, from the child's own row.
    (drover-table-test--goto "child" 0)
    (drover-table-test--press "S")
    (should (equal (drover-table-test--ids) '("root" "child" "asking" "first")))
    (should (equal (drover-table-test--id-at-point) "child"))
    (should (eql (vtable-current-column) 0))))

;;;; Groups

(ert-deftest drover-table-test-group-by-status ()
  (drover-table-test-with-table
      (list (drover-test-session "i1" :status 'idle :updated 2.0)
            (drover-test-session "w1" :status 'waiting :since 2.0)
            (drover-test-session "d" :status 'done :since 1.0)
            (drover-test-session "i2" :status 'idle :updated 1.0)
            (drover-test-session "parent" :status 'idle :updated 3.0)
            (drover-test-session "kid" :parent "parent" :status 'waiting :since 1.0))
    (should (eq drover-group-by 'status))
    ;; A tree goes with its most urgent status; a group counts its rows.
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (3)") "parent" "kid" "w1"
                     (group "Done (1)") "d"
                     (group "Idle (2)") "i1" "i2")))
    ;; Headings are not sessions, and the header counts sessions only.
    (should (string-match-p "  6 sessions\\'" (substring-no-properties header-line-format)))
    (drover-table-test--goto-group "Needs you (3)")
    (let ((faces (ensure-list (get-text-property (point) 'face))))
      (should (memq 'drover-heading faces))
      (should (memq 'drover-waiting faces))
      (should (memq 'drover-table faces)))
    (search-forward "(3)")
    (should (memq 'drover-dim (ensure-list (get-text-property (1- (point)) 'face))))
    (should-not (drover-session-at-point t))
    (should-error (drover-table-test--press "RET") :type 'user-error)
    (should-error (drover-table-test--press "A") :type 'user-error)))

(ert-deftest drover-table-test-group-by-project ()
  "Grouping follows `drover-set-group-by' at once, point staying on its session."
  (drover-table-test-with-table
      (list (drover-test-session "a1" :project "alpha" :status 'idle :updated 1.0)
            (drover-test-session "b1" :project "beta" :status 'waiting :since 1.0)
            (drover-test-session "n1" :status 'done :since 1.0)
            (drover-test-session "a2" :project "alpha" :status 'working :updated 2.0))
    (drover-table-test--goto "a1" 2)
    (drover-table-test--show)
    (drover-set-group-by 'project)
    ;; A project comes where its most urgent tree would.
    (should (equal (drover-table-test--lines)
                   '((group "beta (1)") "b1"
                     (group "No project (1)") "n1"
                     (group "alpha (2)") "a2" "a1")))
    (should (equal (drover-table-test--id-at-point) "a1"))
    (should (eql (vtable-current-column) 2))
    (save-excursion
      (drover-table-test--goto-group "No project (1)")
      (should (memq 'italic (ensure-list (get-text-property (point) 'face)))))
    (drover-set-group-by nil)
    (should (equal (drover-table-test--lines) '("b1" "n1" "a2" "a1")))
    (should (equal (drover-table-test--id-at-point) "a1"))
    (should (eql (vtable-current-column) 2))
    (drover-set-group-by 'status)
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (1)") "b1" (group "Done (1)") "n1"
                     (group "Working (1)") "a2" (group "Idle (1)") "a1")))
    (should (equal (drover-table-test--id-at-point) "a1"))))

(ert-deftest drover-table-test-sort-within-groups ()
  "A sort orders the trees of each group; the groups keep their order."
  (drover-table-test-with-table
      (list (drover-table-test--titled "w-b" "Bravo" :status 'waiting :since 1.0)
            (drover-table-test--titled "w-a" "Alpha" :status 'waiting :since 2.0)
            (drover-table-test--titled "i-z" "Zulu" :status 'idle :updated 2.0)
            (drover-table-test--titled "i-c" "Charlie" :status 'idle :updated 1.0)
            (drover-table-test--titled "i-c1" "Able" :parent "i-c" :status 'idle))
    (setq drover-show-children t)
    (drover-table-refresh)
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (2)") "w-b" "w-a" (group "Idle (3)") "i-z" "i-c" "i-c1")))
    (drover-table-test--sort 0)
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (2)") "w-a" "w-b" (group "Idle (3)") "i-c" "i-c1" "i-z")))
    (drover-table-test--sort 0)
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (2)") "w-b" "w-a" (group "Idle (3)") "i-z" "i-c" "i-c1")))
    ;; Sorted from a heading, by its column.
    (drover-table-test--goto-group "Idle (3)")
    (vtable-goto-column 0)
    (drover-table-test--press "S")
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (2)") "w-a" "w-b" (group "Idle (3)") "i-c" "i-c1" "i-z")))
    (should (equal (car (drover-table-test--cells)) "Idle (3)"))
    (drover-table-test--press "=")
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (2)") "w-b" "w-a" (group "Idle (3)") "i-z" "i-c" "i-c1")))))

(ert-deftest drover-table-test-point-near-headings ()
  (drover-table-test-with-table
      (list (drover-test-session "w1" :status 'waiting :since 1.0)
            (drover-test-session "w2" :status 'waiting :since 2.0)
            (drover-test-session "d" :status 'done :since 1.0)
            (drover-test-session "i" :status 'idle :updated 1.0))
    ;; A heading keeps point across redraws.
    (drover-table-test--goto-group "Done (1)")
    (drover-table-refresh)
    (should (equal (car (drover-table-test--cells)) "Done (1)"))
    ;; A session leaves, and a heading takes its line: point goes to the
    ;; first session of that group, not to the heading.
    (drover-table-test--goto "w2")
    (setq drover-test-sessions (list (drover-test-session "w1" :status 'waiting :since 1.0)
                                     (drover-test-session "d" :status 'done :since 1.0)
                                     (drover-test-session "i" :status 'idle :updated 1.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--lines)
                   '((group "Needs you (1)") "w1" (group "Done (1)") "d" (group "Idle (1)") "i")))
    (should (equal (drover-table-test--id-at-point) "d"))
    ;; A heading leaves, the last line with it: point goes to the last row.
    (drover-table-test--goto-group "Idle (1)")
    (setq drover-test-sessions (list (drover-test-session "w1" :status 'waiting :since 1.0)
                                     (drover-test-session "d" :status 'done :since 1.0)))
    (drover-table-refresh)
    (should (equal (drover-table-test--id-at-point) "d"))))

;;;; Filters

(ert-deftest drover-table-test-filter ()
  "The table lists what `drover-filter' keeps, and says which filter is set."
  (drover-table-test-with-table
      (list (drover-test-session "a" :project "drover" :status 'working :updated 3.0)
            (drover-test-session "b" :project "other" :status 'idle :updated 2.0)
            (drover-table-test--titled "kid" "Needle" :parent "b" :status 'idle)
            (drover-test-session "kid2" :parent "b" :status 'idle))
    (should-not (string-match-p "Filter" header-line-format))
    (drover-table-test--goto "b" 1)
    (drover-table-test--show)
    ;; A matching subagent shows under its parent, subagents hidden.
    (drover-set-filter :search "needle")
    (should (equal (drover-table-test--lines) '((group "Idle (2)") "b" "kid")))
    (should (equal (drover-table-test--id-at-point) "b"))
    (should (eql (vtable-current-column) 1))
    (should (equal (substring-no-properties header-line-format)
                   " Nothing needs you   2 sessions   Filter: \"needle\""))
    (drover-set-filter :search nil)
    (drover-set-filter :project "drover")
    (should (equal (drover-table-test--ids) '("a")))
    (should (string-suffix-p "Filter: project drover" header-line-format))
    ;; Nothing matches: the table says which filter, and how to clear it.
    (drover-set-filter :search "50%")
    (should (equal (buffer-string)
                   (concat "No session matches the filter: project drover, \"50%\".\n"
                           "F c clears the filter.\n")))
    (should (string-search "0 sessions   Filter: project drover, \"50%%\"" header-line-format))
    (should-error (drover-session-at-point) :type 'user-error)
    (goto-char (point-min))
    (should (memq 'drover-dim (ensure-list (get-text-property (point) 'face))))
    (search-forward "F c")
    (should (memq 'help-key-binding (ensure-list (get-text-property (- (point) 3) 'face))))
    (drover-filter-clear)
    (should (equal (drover-table-test--ids) '("a" "b")))
    (should (equal (drover-table-test--id-at-point) "a"))
    (should-not (string-match-p "Filter" header-line-format))))

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
    (should (equal (drover-table-test--cell "s" "Title") "· Session s"))
    ;; The header of an empty column keeps room for its sort indicator.
    (should (>= (aref drover-table--widths 4)
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
    (drover-table-test--goto "s" 0)
    (let ((char-width (drover-table--char-width))
          (title (aref drover-table--widths 0)))
      (should (eq (drover-table-test--binding "{") #'drover-table-narrow-column))
      (should (eq (drover-table-test--binding "}") #'drover-table-widen-column))
      (drover-table-narrow-column 5)
      (should (= (aref drover-table--widths 0) (- title (* 5 char-width))))
      (should (string-suffix-p "…" (drover-table-test--cell "s" "Title")))
      (should (equal (drover-table-test--id-at-point) "s"))
      (should (eql (vtable-current-column) 0))
      ;; The summary takes the room given up.
      (should (= (drover-table-test--total-width)
                 (* (1- drover-table--default-columns) char-width)))
      (drover-table-refresh)
      (should (= (aref drover-table--widths 0) (- title (* 5 char-width))))
      (drover-table-widen-column 2)
      (should (= (aref drover-table--widths 0) (- title (* 3 char-width))))
      (drover-table-reset)
      (should (= (aref drover-table--widths 0) title)))))

;;;; The session at point

(ert-deftest drover-table-test-session-at-point-is-fresh ()
  (drover-table-test-with-table (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
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

(defun drover-table-test--session-keys ()
  "Return the bindings of `drover-session-map', as a list (KEY . COMMAND)."
  (let ((bindings nil))
    (map-keymap (lambda (event command)
                  (push (cons (key-description (vector event)) command) bindings))
                drover-session-map)
    bindings))

(ert-deftest drover-table-test-view ()
  "The table is one of drover's views, where \\`G' and \\`F' apply."
  (drover-table-test-with-table (list (drover-test-session "a"))
    (should drover-view-p)
    (should (drover-session-list-p))
    (should (plist-get (drover--context) :view))
    (drover-table-test--goto "a")
    (should (equal (drover-session-id (drover-session-at-point)) "a"))))

(ert-deftest drover-table-test-keys-reach-backend ()
  (drover-table-test-with-table (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
    (drover-table-test--goto "b" 2)
    (setq drover-test-calls nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (dolist (key '("RET" "o" "A" "k" "s" "v"))
        (drover-table-test--press key)))
    (should (equal (reverse drover-test-calls)
                   '((:open "b" nil) (:open "b" t) (:respond "b") (:interrupt "b")
                     (:mark-seen "b") (:preview "b"))))
    (should (string-match-p "Preview of b" (with-current-buffer drover-peek-buffer
                                             (buffer-string))))))

(ert-deftest drover-table-test-declined-actions ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (drover-table-test--goto "a")
    (setq drover-test-calls nil)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (dolist (key '("k" "x" "z"))
        (should (equal (should-error (drover-table-test--press key) :type 'user-error)
                       '(user-error "Abort")))))
    (should-not drover-test-calls)))

(ert-deftest drover-table-test-keys-through-command-loop ()
  (drover-table-test-with-table (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
    (drover-table-test--goto "b" 1)
    (drover-table-test--show)
    (setq drover-test-calls nil)
    (execute-kbd-macro (kbd "RET"))
    (execute-kbd-macro (kbd "A"))
    (should (equal (reverse drover-test-calls) '((:open "b" nil) (:respond "b"))))))

(ert-deftest drover-table-test-session-keys-through-command-loop ()
  "Every key of `drover-session-map' runs its command on a row of the table.
Each runs through the command loop, under vtable's keymap of the row."
  (drover-table-test-with-table
      (list (drover-test-session "p" :status 'working :updated 2.0 :directory "/work/p/")
            (drover-test-session "kid" :parent "p" :status 'waiting :since 1.0))
    (let ((commands nil)
          (menus nil)
          (org-stored-links nil)
          (drover-store-link-functions
           (list (lambda (session) (list (concat "fake:" (drover-session-id session)) "Fake")))))
      (add-hook 'post-command-hook (lambda () (push this-command commands)) nil t)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'read-string) (lambda (&rest _) "Renamed"))
                ((symbol-function 'transient-setup) (lambda (name &rest _) (push name menus))))
        (drover-table-test--goto "p" 1)
        (drover-table-test--show)
        (setq drover-test-calls nil)
        (let ((keys '("o" "A" "k" "s" "c" "r" "x" "z" "l" "+" "v" "G" "F" "U" "g" "V")))
          ;; The keys pressed here, and u below, are every key of the map.
          (should (equal (sort (cons "u" (copy-sequence keys)) #'string<)
                         (sort (mapcar #'car (drover-table-test--session-keys)) #'string<)))
          (dolist (key keys)
            ;; A session only a refresh can show.
            (when (equal key "g")
              (setq drover-test-sessions (append drover-test-sessions
                                                 (list (drover-test-session "new" :updated 0.5))))
              (should-not (member "new" (drover-table-test--ids))))
            (execute-kbd-macro (kbd key))
            (should (eq (car commands) (keymap-lookup drover-session-map key)))
            ;; Every command leaves point on its row.
            (should (equal (drover-table-test--id-at-point) "p")))
          (should (equal (reverse drover-test-calls)
                         '((:open "p" t) (:respond "p") (:interrupt "p") (:mark-seen "p")
                           (:open "kid" nil) (:rename "p" "Renamed") (:delete "p") (:archive "p")
                           (:new "/work/p/") (:preview "p")
                           ;; Follow mode shows the session at point at once.
                           (:open "p" nil))))
          (should (equal (car org-stored-links) '("fake:p" "Fake")))
          (should (equal menus '(drover-filter drover-group)))
          (should drover-show-children)
          ;; g refreshed the sessions, not only the rows drawn.
          (should (member "new" (drover-table-test--ids)))
          (should drover-follow-mode)
          (execute-kbd-macro (kbd "V"))
          (should-not drover-follow-mode))
        ;; u, on a subagent, opens its parent.
        (drover-table-test--goto "kid" 1)
        (set-window-point (selected-window) (point))
        (setq drover-test-calls nil)
        (execute-kbd-macro (kbd "u"))
        (should (eq (car commands) #'drover-open-parent))
        (should (equal drover-test-calls '((:open "p" nil))))))))

(ert-deftest drover-table-test-key-conflicts ()
  "Vtable's keymaps of rows and of the column header shadow no shared key."
  (drover-table-test-with-table
      (list (drover-test-session "a" :status 'waiting) (drover-test-session "b"))
    (should (eq (keymap-lookup drover-table-mode-map "F") #'drover-filter))
    (dolist (where (list (lambda () (drover-table-test--goto "a" 1))
                         (lambda () (drover-table-test--goto-group "Idle (1)"))
                         (lambda () (goto-char (point-min)))))
      (funcall where)
      ;; vtable's map is there, with its own commands.
      (should (get-text-property (point) 'keymap))
      (should (eq (drover-table-test--binding "S") #'vtable-sort-by-current-column))
      (should (eq (drover-table-test--binding "{") #'drover-table-narrow-column))
      (should (eq (drover-table-test--binding "}") #'drover-table-widen-column))
      (should (eq (drover-table-test--binding "M-<right>") #'vtable-next-column))
      (pcase-dolist (`(,key . ,command) (drover-table-test--session-keys))
        (should (eq (drover-table-test--binding key) command)))
      (should (eq (drover-table-test--binding "v") #'drover-peek))
      (should (eq (drover-table-test--binding "g") #'drover-refresh))
      (should (eq (drover-table-test--binding "RET") #'drover-open-at-point))
      (should (eq (drover-table-test--binding "=") #'drover-table-reset))
      (should (eq (drover-table-test--binding "n") #'next-line))
      (should (eq (drover-table-test--binding "p") #'previous-line))
      (should (eq (drover-table-test--binding "q") #'quit-window))
      (should (eq (drover-table-test--binding "SPC") #'scroll-up-command))
      (should (eq (drover-table-test--binding "DEL") #'scroll-down-command))
      (should (eq (drover-table-test--binding "h") #'describe-mode))
      ;; No key of another set of `drover-dispatch'.
      (dolist (key '("w" "i" "t" "y" "j" "b" "a" "m" "e" "d" "C" "f" "O"))
        (should (eq (drover-table-test--binding key) #'undefined))))))

(ert-deftest drover-table-test-revert-buffer ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (setq drover-test-sessions (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0)))
    (revert-buffer)
    (should (equal (drover-table-test--ids) '("a" "b")))))

(ert-deftest drover-table-test-commands-outside-table ()
  (with-temp-buffer
    (should-error (drover-table-refresh) :type 'user-error)
    (should-error (drover-table-reset) :type 'user-error)
    (should-error (drover-table-narrow-column) :type 'user-error)))

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
    (drover-table-test--set-sessions (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
    (drover-table-refresh)
    (should (equal (drover-table-test--id-at-point) "a"))))

;;;; Sorting

(ert-deftest drover-table-test-user-sort-holds ()
  (drover-table-test-with-table
      (list (drover-test-session "b" :status 'waiting :since 1.0)
            (drover-test-session "c" :status 'idle :updated 1.0)
            (drover-test-session "a" :status 'done :since 1.0))
    (drover-table-test--group-by nil)
    (should (equal (drover-table-test--ids) '("b" "a" "c")))
    (drover-table-test--goto "b" 0)
    (drover-table-test--press "S")
    (should (equal (drover-table-test--ids) '("a" "b" "c")))
    (should (equal drover-table--sort-by '((0 . ascend))))
    (setq drover-test-sessions (cons (drover-test-session "0" :status 'idle :updated 2.0)
                                     drover-test-sessions))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("0" "a" "b" "c")))
    (should (equal (drover-table-test--id-at-point) "b"))
    (drover-table-test--press "S")
    (should (equal (drover-table-test--ids) '("c" "b" "a" "0")))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("c" "b" "a" "0")))
    (drover-table-reset)
    (should (equal (drover-table-test--ids) '("b" "a" "0" "c")))
    (should-not drover-table--sort-by)
    (should (equal (drover-table-test--id-at-point) "b"))))

(ert-deftest drover-table-test-user-sort-survives-empty-table ()
  (drover-table-test-with-table (list (drover-test-session "b") (drover-test-session "a"))
    (drover-table-test--goto "b" 0)
    (drover-table-test--press "S")
    (drover-table-test--set-sessions)
    (drover-table-refresh)
    (setq drover-test-sessions (list (drover-test-session "b") (drover-test-session "a")))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("a" "b")))))

(ert-deftest drover-table-test-sort-keeps-trees-together ()
  (drover-table-test-with-table
      (list (drover-table-test--titled "a" "Beta" :status 'idle :updated 3.0)
            (drover-table-test--titled "a1" "Zulu" :parent "a" :status 'idle)
            (drover-table-test--titled "a2" "alpha" :parent "a" :status 'waiting :since 1.0)
            (drover-table-test--titled "b" "Alpha" :status 'working :updated 2.0)
            (drover-table-test--titled "b1" "Mike" :parent "b" :status 'idle))
    (setq drover-show-children t)
    (drover-table-test--group-by nil)
    (should (equal (drover-table-test--ids) '("a" "a2" "a1" "b" "b1")))
    ;; The trees go by the title of their root, the siblings by their
    ;; own, ignoring case and the glyphs; a plain sort of the rows would
    ;; give a2 b a b1 a1.
    (drover-table-test--sort 0)
    (should (equal (drover-table-test--ids) '("b" "b1" "a" "a2" "a1")))
    (should (equal (car (drover-table-test--header)) "Title ▲"))
    (drover-table-test--sort 0)
    (should (equal (drover-table-test--ids) '("a" "a1" "a2" "b" "b1")))
    (should (equal (car (drover-table-test--header)) "Title ▼"))
    (should (equal (drover-table-test--indent "a1") (drover-table-test--indent "b1")))
    (should (= (drover-table-test--indent "b") 0))
    ;; A redraw keeps the order, and the subagents nested.
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("a" "a1" "a2" "b" "b1")))
    (drover-table-test--press "=")
    (should (equal (drover-table-test--ids) '("a" "a2" "a1" "b" "b1")))
    (should (equal (car (drover-table-test--header)) "Title"))))

(ert-deftest drover-table-test-sort-ties-and-earlier-keys ()
  (drover-table-test-with-table
      (list (drover-test-session "x" :project "p" :status 'idle :updated 1.0)
            (drover-test-session "y" :project "p" :status 'waiting :since 5.0)
            (drover-test-session "z" :project "a" :status 'idle :updated 2.0))
    (drover-table-test--group-by nil)
    (should (equal (drover-table-test--ids) '("y" "z" "x")))
    ;; Equal projects keep the order of urgency.
    (drover-table-test--sort 1)
    (should (equal (drover-table-test--ids) '("z" "y" "x")))
    ;; Sorted by title, then project: equal projects go by title.
    (drover-table-reset)
    (drover-table-test--sort 0)
    (drover-table-test--sort 1)
    (should (equal drover-table--sort-by '((0 . ascend) (1 . ascend))))
    (should (equal (drover-table-test--ids) '("z" "x" "y")))))

(ert-deftest drover-table-test-sort-age-numerically ()
  (let ((now (float-time)))
    (drover-table-test-with-table
        (list (drover-test-session "hour" :status 'idle :updated (- now 3700))
              (drover-test-session "seconds" :status 'working :updated (- now 50))
              (drover-test-session "minutes" :status 'waiting :since (- now 330))
              (drover-test-session "unknown" :status 'done))
      (drover-table-test--group-by nil)
      (drover-table-test--goto "hour" 3)
      (drover-table-test--press "S")
      ;; Youngest first; a string sort would put "1h" first.
      (should (equal (drover-table-test--ids) '("seconds" "minutes" "hour" "unknown")))
      (drover-table-test--press "S")
      (should (equal (drover-table-test--ids) '("unknown" "hour" "minutes" "seconds")))
      (drover-table-test--press "=")
      (should (equal (drover-table-test--ids) '("minutes" "unknown" "seconds" "hour"))))))

(ert-deftest drover-table-test-sort-through-command-loop ()
  (drover-table-test-with-table
      (list (drover-table-test--titled "a" "Beta" :status 'waiting :since 1.0)
            (drover-table-test--titled "a1" "Alpha" :parent "a" :status 'done :since 2.0)
            (drover-table-test--titled "b" "Gamma" :status 'idle))
    (drover-table-test--group-by nil)
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-test--goto "a1" 0)
    (set-window-point (selected-window) (point))
    (execute-kbd-macro (kbd "S"))
    (should (equal (drover-table-test--ids) '("a" "a1" "b")))
    (should (equal drover-table--sort-by '((0 . ascend))))
    (should (equal (drover-table-test--id-at-point) "a1"))
    (execute-kbd-macro (kbd "S"))
    (should (equal (drover-table-test--ids) '("b" "a" "a1")))))

(ert-deftest drover-table-test-sort-from-click-in-other-window ()
  "A click on the header of a table in a window that is not selected."
  (drover-table-test-with-table
      (list (drover-table-test--titled "a" "Beta" :status 'waiting :since 1.0)
            (drover-table-test--titled "a1" "Alpha" :parent "a" :status 'done :since 2.0)
            (drover-table-test--titled "b" "Alpha" :status 'idle))
    (drover-table-test--group-by nil)
    (let ((table (current-buffer))
          (other (get-buffer-create "*drover table test other*")))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) table)
            (let ((window (selected-window)))
              (select-window (split-window-below))
              (set-window-buffer (selected-window) other)
              (set-buffer other)
              (let* ((header (with-current-buffer table
                               (save-excursion (goto-char (point-min))
                                               (vtable-goto-column 0)
                                               (point))))
                     ;; Batch Emacs does not lay windows out, which
                     ;; `posn-at-point' needs.
                     (event (list 'mouse-2 (list window header '(0 . 0) 0 nil header
                                                 '(0 . 0) nil '(0 . 0) '(1 . 1)))))
                (let ((last-command-event event))
                  (vtable-header-line-sort event)
                  (run-hooks 'post-command-hook))))
            (with-current-buffer table
              (should (equal drover-table--sort-by '((0 . ascend))))
              (should (equal (drover-table-test--ids) '("b" "a" "a1")))))
        (kill-buffer other)))))

;;;; Following the backends

(ert-deftest drover-table-test-shown-table-follows-changes ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (set-window-buffer (selected-window) (current-buffer))
    (drover-table-test--set-sessions (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
    (should (equal (drover-table-test--ids) '("a" "b")))
    (should-not drover-table--stale)))

(ert-deftest drover-table-test-hidden-table-goes-stale ()
  (drover-table-test-with-table (list (drover-test-session "a"))
    (should-not (get-buffer-window (current-buffer) t))
    (let ((drawn drover-table--drawn-at))
      (drover-table-test--set-sessions (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
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
            (setq drover-test-sessions (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0)))
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
      (should (memq #'drover-table--follow-sort (default-value 'post-command-hook)))
      (kill-buffer first)
      (should (memq #'drover-table--changed drover-changed-hook))
      (should (memq #'drover-table--follow-sort (default-value 'post-command-hook)))
      (kill-buffer second)
      (should-not (memq #'drover-table--changed drover-changed-hook))
      (should-not (memq #'drover-table--follow-sort (default-value 'post-command-hook))))))

;;;; Empty state and header

(ert-deftest drover-table-test-empty-state ()
  (drover-table-test-with-table nil
    (should (equal (buffer-string) "No agent sessions.\n"))
    (should-not drover-table--table)
    (should-error (drover-session-at-point) :type 'user-error)
    (should (string-match-p "Nothing needs you" header-line-format))
    (should (string-match-p "0 sessions" header-line-format))
    ;; A subagent whose parent is gone is a tree of its own, so shown.
    (drover-table-test--set-sessions (drover-test-session "child" :parent "root"))
    (drover-table-refresh)
    (should (equal (drover-table-test--ids) '("child")))
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
  (drover-test-with-backend (list (drover-test-session "a" :updated 2.0) (drover-test-session "b" :updated 1.0))
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
