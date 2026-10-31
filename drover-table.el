;;; drover-table.el --- Dashboard of agent sessions as one table  -*- lexical-binding: t; -*-

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

;; `drover-table' lists every session in one `vtable', with its title,
;; project, branch, age and why it has its status.  The title cell
;; starts with the glyph of the session's status.  The header line
;; counts the sessions that need you.
;;
;; Sessions come as trees, most urgent first: each subagent follows its
;; parent, its title indented by its depth, and a tree ranks by the most
;; urgent status within it, so a parent whose subagent waits for you
;; comes with the sessions that wait for you.  Subagents are hidden
;; unless `drover-show-children' is non-nil or they need you; c toggles
;; it in the table.
;;
;; The table follows the backends: it is drawn again whenever
;; `drover-changed-hook' runs while it is shown, and when it is next
;; shown otherwise, so a buried table costs nothing.  Its columns are
;; sized to its window, and drawn again when the window's width changes.
;; Point stays on its session across redraws.
;;
;; Vtable's own commands sort the table by a column: S on a column, or a
;; click on its header.  The sort orders the trees by the value of their
;; root, and the subagents of a session among themselves, never parting
;; a subagent from its parent; ties keep the order of urgency.  The
;; Title column sorts by the title alone, not the status before it.  The
;; sort holds across redraws, as do widths changed with { and }; =
;; restores the order of urgency and the default widths.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'vtable)
(require 'drover)

(defgroup drover-table nil
  "Dashboard of agent sessions as one table."
  :group 'drover
  :prefix "drover-table-")

(defface drover-table '((t :inherit vtable))
  "Face of the session table.
It inherits `vtable', which is variable pitch; inherit `fixed-pitch'
instead for a monospaced table."
  :group 'drover-faces)

(defcustom drover-table-age-refresh-interval 15
  "Seconds between redraws of a shown table, so that its ages stay true.
Nil redraws only when sessions change."
  :type '(choice (number :tag "Seconds") (const :tag "Never" nil))
  :group 'drover-table)

(defconst drover-table-buffer-name "*drover table*"
  "Name of the buffer of `drover-table'.")

(defconst drover-table--separator-width 2
  "Characters between two columns.")

(defconst drover-table--default-columns 100
  "Width, in characters, of a table drawn while no window shows it.")

(defconst drover-table--minimum-columns 6
  "Fewest characters a shrunk column keeps.")

(defconst drover-table--unknown-age 1.0e+INF
  "Age of a session whose time is unknown, so that it sorts as the newest.")

;;;; State

(defvar-local drover-table--table nil
  "The vtable of this buffer, or nil when no session is shown.")

(defvar-local drover-table--sort-by nil
  "Sort keys of the table, as vtable's `sort-by', kept across redraws.
A list of (INDEX . DIRECTION), the last key sorting first.  While a
table is shown, it holds the keys the table was drawn in the order of.")

(defvar-local drover-table--width-overrides nil
  "Widths set by the user, as an alist (COLUMN-NAME . CHARACTERS).")

(defvar-local drover-table--widths nil
  "Vector of the pixel widths of the columns as last drawn.")

(defvar-local drover-table--window-width nil
  "Body width in pixels of the window the table was last drawn for, or nil.")

(defvar-local drover-table--drawn-at nil
  "Time, as a float, of the last redraw, or nil.")

(defvar-local drover-table--stale nil
  "Non-nil when sessions changed while the table was not shown.")

(defvar drover-table--age-timer nil
  "Timer redrawing the shown tables, so that their ages stay true.")

;;;; Cells

(defun drover-table--one-line (text &optional face)
  "Return TEXT on one line, with FACE before its own faces.
Return \"\" for a nil TEXT."
  (let ((text (string-trim (replace-regexp-in-string
                            "[ \t\n\r]+" " " (format "%s" (or text "")) t t))))
    (when face
      (add-face-text-property 0 (length text) face nil text))
    text))

(defun drover-table--styled (text)
  "Return a copy of TEXT in the face of the table, after its own faces."
  (let ((text (copy-sequence text)))
    (add-face-text-property 0 (length text) 'drover-table t text)
    text))

(defun drover-table--text-width (text)
  "Return the width in pixels of TEXT drawn in the table."
  (drover-text-pixel-width (drover-table--styled text) face-remapping-alist
                           (get-buffer-window nil 0)))

(defun drover-table--char-width ()
  "Return the width in pixels of a character of the table."
  (drover-table--text-width "x"))

;; A subagent is indented, not marked: each level of depth is a stretch
;; of space exactly as wide as a glyph and the space after it, so that
;; its glyph stands under the title of its parent, whatever the font.
;; A marker such as "↳ " would add a second symbol before the glyph,
;; and its width, like that of spaces, varies with a variable pitch font.

(defun drover-table--space (pixels)
  "Return one space drawn PIXELS wide."
  (propertize " " 'display `(space :width (,pixels))))

(defun drover-table--status-text (status)
  "Return the glyph of STATUS in its face, its tooltip naming it."
  (propertize (drover-status-glyph status) 'help-echo (drover-status-label status)))

(defun drover-table--glyph-widths (sessions)
  "Return the pixel widths of the glyphs of SESSIONS, as an alist.
Its entries are (STATUS . WIDTH), one for each status of SESSIONS."
  (mapcar (lambda (status)
            (cons status (drover-table--text-width (drover-table--status-text status))))
          (seq-uniq (mapcar #'drover-session-status sessions))))

(defun drover-table--title-text (session depth glyph-widths)
  "Return the glyph of SESSION's status, one space and its title.
The text is indented by DEPTH, the depth of SESSION in its tree.
GLYPH-WIDTHS is an alist (STATUS . WIDTH) of the glyphs shown, from
`drover-table--glyph-widths'.  The space after a glyph narrower than
the widest is as much wider, so that the titles at one depth line up."
  (let* ((status (drover-session-status session))
         (widest (apply #'max (mapcar #'cdr glyph-widths)))
         (space (drover-table--text-width " "))
         (pad (- widest (alist-get status glyph-widths))))
    (concat (when (> depth 0)
              (drover-table--space (* depth (+ widest space))))
            (drover-table--status-text status)
            (if (> pad 0) (drover-table--space (+ space pad)) " ")
            (drover-table--one-line (drover-session-display-title session)))))

(defun drover-table--age (session now)
  "Return the age in seconds of SESSION at time NOW.
A session that needs the user is as old as its status, another as its
last activity.  Return `drover-table--unknown-age' when that time is
unknown."
  (if-let* ((time (if (drover-session-attention-p session)
                      (drover-session-since session)
                    (drover-session-updated session))))
      (max 0.0 (- now time))
    drover-table--unknown-age))

(defun drover-table--age-text (age now)
  "Return AGE, in seconds at time NOW, as `drover-format-age' does."
  (if (= age drover-table--unknown-age)
      ""
    (propertize (drover-format-age (- now age) now) 'face 'drover-dim)))

(defun drover-table--text-column (name weight field &optional face)
  "Return a column NAME of WEIGHT showing FIELD of a session, in FACE.
FIELD is a function of a session returning a string or nil.  The
column sorts by that string, ignoring case."
  (list :name name :align 'left :weight weight
        :key (lambda (session) (downcase (drover-table--one-line (funcall field session))))
        :text (lambda (session _depth) (drover-table--one-line (funcall field session) face))))

(defun drover-table--columns (now glyph-widths)
  "Return the columns of a table of sessions at time NOW.
GLYPH-WIDTHS is an alist (STATUS . WIDTH) of the glyphs of the statuses
shown.  The Title column shows the glyph of a session's status before
its title, but sorts by the title alone.  Each column is a plist of:

 :name    Its header.
 :key     Function of a session returning the value it sorts by, a
          string or a number.
 :text    Function of a session and its depth in its tree returning
          the text shown.
 :align   `left' or `right'.
 :weight  Share of the room left that the column may take, or nil for
          a column as wide as its content."
  (list
   (list :name "Title" :align 'left :weight 3
         :key (lambda (session)
                (downcase (drover-table--one-line (drover-session-display-title session))))
         :text (lambda (session depth) (drover-table--title-text session depth glyph-widths)))
   (drover-table--text-column "Project" 1 #'drover-session-project)
   (drover-table--text-column "Branch" 1 #'drover-session-branch 'drover-dim)
   (list :name "Age" :align 'right
         :key (lambda (session) (drover-table--age session now))
         :text (lambda (session _depth) (drover-table--age-text (drover-table--age session now) now)))
   (drover-table--text-column "Summary" 3 #'drover-session-summary)))

;;;; Order

(defun drover-table--key-less-p (a b)
  "Return non-nil when the sort key A comes before B.
Keys are numbers or strings."
  (if (numberp a) (< a b) (string< a b)))

(defun drover-table--predicate (sort-by keys)
  "Return a predicate ordering sessions by the columns of SORT-BY, or nil.
SORT-BY is a list of (INDEX . DIRECTION) as vtable keeps it, its last
key sorting first and the others breaking ties.  KEYS is a hash table
mapping a session to the vector of its sort keys, one per column.
Return nil for an empty SORT-BY."
  (when sort-by
    (let ((sort-by (reverse sort-by)))
      (lambda (a b)
        (let ((keys-a (gethash a keys))
              (keys-b (gethash b keys)))
          (catch 'order
            (pcase-dolist (`(,index . ,direction) sort-by)
              (let ((key-a (aref keys-a index))
                    (key-b (aref keys-b index)))
                (unless (equal key-a key-b)
                  (throw 'order (if (eq direction 'descend)
                                    (drover-table--key-less-p key-b key-a)
                                  (drover-table--key-less-p key-a key-b))))))
            nil))))))

(defun drover-table--rows (nodes predicate &optional depth)
  "Return the rows of the trees NODES, each session before its subagents.
A row is a cons (SESSION . DEPTH), DEPTH being 0 for the roots of NODES,
or DEPTH when non-nil.  NODES and their children come most urgent
first; PREDICATE, a function of two sessions, orders each level instead
when non-nil, ties keeping the order of urgency."
  (let ((depth (or depth 0)))
    (mapcan (lambda (node)
              (cons (cons (drover-node-session node) depth)
                    (drover-table--rows (drover-node-children node) predicate (1+ depth))))
            (if predicate
                ;; `sort' is stable, which keeps ties in order of urgency.
                (sort (copy-sequence nodes)
                      (lambda (a b)
                        (funcall predicate (drover-node-session a) (drover-node-session b))))
              nodes))))

;;;; Widths

(defun drover-table--natural-width (column texts char-width)
  "Return the pixel width COLUMN needs to show its cell TEXTS in full.
The header needs room for vtable's sort indicator, and CHAR-WIDTH
more."
  (apply #'max
         (+ (drover-table--text-width (plist-get column :name))
            (drover-table--text-width "▼")
            char-width)
         (mapcar #'drover-table--text-width texts)))

(defun drover-table--share (naturals weights room minimum)
  "Return widths sharing ROOM between columns, by WEIGHTS.
NATURALS are the widths the columns need, WEIGHTS their shares.  A
column needing less than its share gets what it needs, and the rest
goes to the others; a column needing more gets its share, but no less
than MINIMUM or what it needs, whichever is smaller."
  (let* ((count (length naturals))
         (widths (make-vector count nil))
         (open (number-sequence 0 (1- count)))
         (settled t))
    (while (and open settled)
      (let ((total (apply #'+ (mapcar (lambda (index) (nth index weights)) open))))
        (setq settled (seq-filter (lambda (index)
                                    (<= (nth index naturals)
                                        (/ (* room (nth index weights)) (float total))))
                                  open))
        (dolist (index settled)
          (aset widths index (nth index naturals))
          (setq room (- room (nth index naturals))))
        (setq open (seq-difference open settled))))
    (when open
      (let ((total (apply #'+ (mapcar (lambda (index) (nth index weights)) open))))
        (dolist (index open)
          (aset widths index
                (max (min (nth index naturals) minimum)
                     (floor (* (max room 0) (nth index weights)) total))))
        ;; What rounding down left goes to the last column.
        (let ((left (- room (apply #'+ (mapcar (lambda (index) (aref widths index)) open))))
              (last (car (last open))))
          (when (> left 0)
            (aset widths last (+ (aref widths last) left))))))
    (append widths nil)))

(defun drover-table--column-widths (columns cells width)
  "Return the pixel widths of COLUMNS showing CELLS in WIDTH pixels.
CELLS holds the texts of each row, a list with one per column.  Columns
without a weight are as wide as their content; the user's widths hold;
the weighted columns share the room left."
  (let* ((char-width (drover-table--char-width))
         (naturals (seq-map-indexed
                    (lambda (column index)
                      (drover-table--natural-width
                       column (mapcar (lambda (texts) (nth index texts)) cells) char-width))
                    columns))
         (overrides (mapcar (lambda (column)
                              (when-let* ((characters (alist-get (plist-get column :name)
                                                                 drover-table--width-overrides
                                                                 nil nil #'equal)))
                                (* characters char-width)))
                            columns))
         (flexible (seq-filter (lambda (index)
                                 (and (plist-get (nth index columns) :weight)
                                      (null (nth index overrides))))
                               (number-sequence 0 (1- (length columns)))))
         (fixed (cl-loop for index from 0 below (length columns)
                         unless (memq index flexible)
                         sum (or (nth index overrides) (nth index naturals))))
         ;; One character stays free, so that a full line is not truncated.
         (room (- width char-width fixed
                  (* (1- (length columns)) drover-table--separator-width char-width)))
         (shared (drover-table--share
                  (mapcar (lambda (index) (nth index naturals)) flexible)
                  (mapcar (lambda (index) (plist-get (nth index columns) :weight)) flexible)
                  room
                  (* drover-table--minimum-columns char-width))))
    (cl-loop for index from 0 below (length columns)
             collect (if (memq index flexible)
                         (nth (seq-position flexible index) shared)
                       (or (nth index overrides) (nth index naturals))))))

(defun drover-table--width-spec (pixels)
  "Return PIXELS as a vtable width.
Vtable reads pixel widths as tenths of characters on text terminals,
where a character is one pixel wide."
  (if (display-graphic-p) (format "%dpx" pixels) pixels))

;;;; Drawing

;; Vtable sorts the rows itself, by the values of a column, whenever it
;; draws: it would part subagents from their parents.  So every cell's
;; value is the rank of its row in the order the table computed, and a
;; formatter shows the text: sorting by the ranks, in the direction of
;; the last sort key, keeps that order.  Vtable's sort commands still
;; change the keys, and `drover-table--follow-sort' draws the table again
;; in the order they name.

(defun drover-table--keys (columns sessions)
  "Return the sort keys of SESSIONS in COLUMNS, as a hash table.
It maps each session to a vector of its keys, one per column."
  (let ((keys (make-hash-table :test #'eq)))
    (dolist (session sessions)
      (puthash session
               (vconcat (mapcar (lambda (column) (funcall (plist-get column :key) session))
                                columns))
               keys))
    keys))

(defun drover-table--insert (nodes sort-by width)
  "Insert a table of the trees NODES, sorted by SORT-BY, WIDTH pixels wide.
NODES come most urgent first.  SORT-BY is a list of vtable sort keys,
or nil for the order of urgency.  Return the vtable."
  (let* ((now (float-time))
         (sessions (drover-tree-sessions nodes))
         (columns (drover-table--columns now (drover-table--glyph-widths sessions)))
         ;; Keys left by a former version of the table, with more columns.
         (sort-by (seq-filter (lambda (key) (< (car key) (length columns))) sort-by))
         (rows (drover-table--rows
                nodes (drover-table--predicate sort-by (drover-table--keys columns sessions))))
         (cells (mapcar (lambda (row)
                          (mapcar (lambda (column)
                                    (funcall (plist-get column :text) (car row) (cdr row)))
                                  columns))
                        rows))
         (widths (drover-table--column-widths columns cells width))
         (count (length rows))
         (descending (eq (cdar (last sort-by)) 'descend))
         (ranks (make-hash-table :test #'eq))
         (texts (make-vector count nil)))
    (cl-loop for row in rows
             for row-cells in cells
             for position from 0
             for rank = (if descending (- count position 1) position)
             do (puthash (car row) rank ranks)
             do (aset texts rank row-cells))
    (setq drover-table--widths (vconcat widths))
    (make-vtable
     :columns (cl-loop for column in columns
                       for width in widths
                       for index from 0
                       ;; `cl-loop' steps INDEX in place, which every
                       ;; formatter would share.
                       collect (let ((index index))
                                 (list :name (plist-get column :name)
                                       :width (drover-table--width-spec width)
                                       :align (plist-get column :align)
                                       :getter (lambda (session _table) (gethash session ranks))
                                       ;; Vtable styles the text of its values, not
                                       ;; formatted text.
                                       :formatter (lambda (rank)
                                                    (drover-table--styled
                                                     (nth index (aref texts rank)))))))
     :objects (mapcar #'car rows)
     :face 'drover-table
     :use-header-line nil
     :separator-width drover-table--separator-width
     :sort-by (copy-tree sort-by))))

(defun drover-table--empty-text ()
  "Return the line shown when no session is listed."
  (if (drover-backends) "No agent sessions." "No agent backend is loaded."))

(defun drover-table--header-line (sessions hidden)
  "Return the header line counting SESSIONS that need the user.
HIDDEN is the number of subagent sessions left out."
  (let ((counts
         (delq nil
               (mapcar (lambda (status)
                         (let ((count (seq-count (lambda (session)
                                                   (eq (drover-session-status session) status))
                                                 sessions)))
                           (when (> count 0)
                             (concat (drover-status-glyph status) " "
                                     (propertize (format "%d %s" count (drover-status-label status))
                                                 'face (drover-status-face status))))))
                       (seq-sort-by #'drover-status-rank #'< drover-attention-statuses)))))
    (string-replace
     "%" "%%"
     (concat " "
             (if counts
                 (string-join counts "   ")
               (propertize "Nothing needs you" 'face 'drover-dim))
             (propertize (concat (format "   %d session%s" (length sessions)
                                         (if (= (length sessions) 1) "" "s"))
                                 (when (> hidden 0)
                                   (format ", %d subagent%s hidden" hidden
                                           (if (= hidden 1) "" "s"))))
                         'face 'drover-dim)))))

(defun drover-table--position (position)
  "Return where POSITION stands, as a list (KEY LINE COLUMN).
KEY is the key of the session on its line, or nil; COLUMN the index of
its column, or nil."
  (save-excursion
    (goto-char position)
    (list (when-let* ((session (vtable-current-object)))
            (drover-session-key session))
          (line-number-at-pos)
          (vtable-current-column))))

(defun drover-table--first-row ()
  "Move to the first row of the table, if any."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'vtable-object nil (lambda (_ session) session))))
    (goto-char (prop-match-beginning match))))

(defun drover-table--goto-session (key)
  "Move to the row of the session whose key is KEY; return nil if none."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'vtable-object key
                      (lambda (key session)
                        (and session (equal key (drover-session-key session)))))))
    (goto-char (prop-match-beginning match))))

(defun drover-table--locate (position)
  "Return the buffer position matching POSITION, from `drover-table--position'.
That is the row of the same session, else the same line, else the
last row.  A nil POSITION is the first row."
  (save-excursion
    (pcase-let ((`(,key ,line ,column) position))
      (cond ((null position) (drover-table--first-row))
            ((and key (drover-table--goto-session key)))
            (t (goto-char (point-min))
               (forward-line (1- line))
               (when (and (eobp) (not (bobp)))
                 (forward-line -1))))
      (when (and column (vtable-current-object))
        (vtable-goto-column column))
      (point))))

(defun drover-table--draw (window)
  "Draw the sessions in this buffer, sized for WINDOW, or a default width.
WINDOW, when non-nil, shows this buffer and is selected."
  (let* ((width (if window
                    (window-body-width window t)
                  (* drover-table--default-columns (drover-table--char-width))))
         (here (and drover-table--table (drover-table--position (point))))
         (views (mapcar (lambda (view)
                          (list view
                                (and drover-table--table
                                     (drover-table--position (window-point view)))
                                (line-number-at-pos (window-start view))))
                        (get-buffer-window-list nil nil t)))
         (trees (drover-tree (drover-sessions)))
         (nodes (drover-tree-visible trees))
         (sessions (drover-tree-sessions nodes))
         (inhibit-read-only t))
    ;; A vtable sort command may have changed the keys since the last draw.
    (when drover-table--table
      (setq drover-table--sort-by (vtable-sort-by drover-table--table)))
    (erase-buffer)
    (setq drover-table--table (and nodes (drover-table--insert nodes drover-table--sort-by width)))
    (if drover-table--table
        (setq drover-table--sort-by (copy-tree (vtable-sort-by drover-table--table)))
      (insert (propertize (drover-table--empty-text) 'face 'drover-dim) "\n"))
    (setq header-line-format
          (drover-table--header-line sessions (drover-tree-hidden-count trees))
          drover-table--window-width (and window width)
          drover-table--drawn-at (float-time)
          drover-table--stale nil)
    (goto-char (drover-table--locate here))
    (pcase-dolist (`(,view ,position ,start) views)
      (set-window-point view (drover-table--locate position))
      (set-window-start view (save-excursion
                               (goto-char (point-min))
                               (forward-line (1- start))
                               (point))
                        t))))

(defun drover-table--window (buffer)
  "Return the window to size the table of BUFFER for, or nil.
That is the narrowest window showing BUFFER, so that the table fits in
all of them; the selected window when it is as narrow as any."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (when (memq (selected-window) windows)
      (setq windows (cons (selected-window) (delq (selected-window) windows))))
    (seq-reduce (lambda (narrowest window)
                  (if (< (window-body-width window t) (window-body-width narrowest t))
                      window
                    narrowest))
                (cdr windows) (car windows))))

(defun drover-table--redraw (buffer)
  "Draw the table of BUFFER again, sized for the windows showing it."
  (let ((window (drover-table--window buffer)))
    (if window
        (with-selected-window window
          (drover-table--draw window))
      (with-current-buffer buffer
        (drover-table--draw nil)))
    (when window
      (drover-table--start-age-timer))))

(defun drover-table--buffers ()
  "Return the live table buffers."
  (seq-filter (lambda (buffer)
                (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                         'drover-table-mode))
              (buffer-list)))

(defun drover-table--changed ()
  "Draw the shown tables again, and mark the others stale."
  (dolist (buffer (drover-table--buffers))
    (if (get-buffer-window buffer t)
        (with-demoted-errors "Drover table: %S"
          (drover-table--redraw buffer))
      (with-current-buffer buffer
        (setq drover-table--stale t)))))

(defun drover-table--outdated-p (window)
  "Return non-nil when the table of this buffer is out of date for WINDOW.
WINDOW is the window it is sized for, or nil when none shows it."
  (and window
       (or drover-table--stale
           (not (eql drover-table--window-width (window-body-width window t)))
           (and drover-table-age-refresh-interval
                drover-table--drawn-at
                (>= (- (float-time) drover-table--drawn-at)
                    drover-table-age-refresh-interval)))))

(defun drover-table--window-changed (window)
  "Draw the table shown in WINDOW again when it is out of date.
Run when WINDOW starts showing the table or changes size."
  (let ((buffer (window-buffer window)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'drover-table-mode)
                 (drover-table--outdated-p (drover-table--window buffer)))
        (with-demoted-errors "Drover table: %S"
          (drover-table--redraw buffer))))))

(defun drover-table--event-buffer ()
  "Return the buffer of the window the last command's mouse event is in.
Return nil when that command was not run by a mouse event in a window."
  (when-let* (((mouse-event-p last-command-event))
              (window (posn-window (event-start last-command-event)))
              ((window-live-p window)))
    (window-buffer window)))

(defun drover-table--follow-sort ()
  "Draw again a table that the last command sorted.
Vtable's sort commands, S on a column or a click on its header, change
the sort keys of a table and sort its rows by themselves, which would
part subagents from their parents; the table is drawn again in its own
order for the new keys.  The table is in the current buffer, or in the
window of a mouse click, which need not be selected."
  (dolist (buffer (seq-uniq (delq nil (list (current-buffer) (drover-table--event-buffer)))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'drover-table-mode)
                 drover-table--table
                 (not (equal (vtable-sort-by drover-table--table) drover-table--sort-by)))
        (with-demoted-errors "Drover table: %S"
          (drover-table--redraw buffer))))))

(defun drover-table--tick ()
  "Draw the shown tables again; stop when none is shown."
  (let ((shown nil))
    (dolist (buffer (drover-table--buffers))
      (when (get-buffer-window buffer t)
        (setq shown t)
        (with-demoted-errors "Drover table: %S"
          (drover-table--redraw buffer))))
    (unless shown
      (drover-table--stop-age-timer))))

(defun drover-table--start-age-timer ()
  "Start redrawing the shown tables every `drover-table-age-refresh-interval'."
  (when (and drover-table-age-refresh-interval
             (not (timerp drover-table--age-timer)))
    (setq drover-table--age-timer
          (run-at-time drover-table-age-refresh-interval drover-table-age-refresh-interval
                       #'drover-table--tick))))

(defun drover-table--stop-age-timer ()
  "Stop redrawing the tables for their ages."
  (when (timerp drover-table--age-timer)
    (cancel-timer drover-table--age-timer))
  (setq drover-table--age-timer nil))

(defun drover-table--killed ()
  "Stop following the backends when the last table goes."
  (unless (delq (current-buffer) (drover-table--buffers))
    (remove-hook 'drover-changed-hook #'drover-table--changed)
    (remove-hook 'post-command-hook #'drover-table--follow-sort)
    (drover-table--stop-age-timer)))

(defun drover-table--session-at-point ()
  "Return the current record of the session on this line, or nil."
  (when-let* ((session (vtable-current-object)))
    (drover-find (drover-session-backend session) (drover-session-id session))))

;;;; Commands

(defun drover-table--check-mode ()
  "Signal a `user-error' unless this buffer is a session table."
  (unless (derived-mode-p 'drover-table-mode)
    (user-error "Not in a drover table")))

(defun drover-table-refresh ()
  "Draw the sessions again, keeping point on its session."
  (interactive)
  (drover-table--check-mode)
  (drover-table--redraw (current-buffer)))

(defun drover-table--revert (&rest _)
  "Draw the sessions again, for `revert-buffer'."
  (drover-table-refresh))

(defun drover-table-toggle-children ()
  "Show or hide the sessions of subagents in this table.
Subagents that need the user, and the sessions above them, show either
way."
  (interactive)
  (drover-table--check-mode)
  (setq-local drover-show-children (not drover-show-children))
  (drover-table-refresh)
  (message (if drover-show-children "Showing subagent sessions" "Hiding subagent sessions")))

(defun drover-table-reset ()
  "Sort the sessions by urgency again, and restore the column widths."
  (interactive)
  (drover-table--check-mode)
  (when drover-table--table
    (setf (vtable-sort-by drover-table--table) nil))
  (setq drover-table--sort-by nil
        drover-table--width-overrides nil)
  (drover-table-refresh)
  (message "Sessions sorted by urgency"))

(defun drover-table-narrow-column (&optional n)
  "Narrow the column at point by N characters, 1 by default.
The width holds across redraws, until `drover-table-reset'."
  (interactive "p")
  (drover-table--check-mode)
  (let ((index (vtable-current-column)))
    (unless (and index drover-table--table)
      (user-error "No column at point"))
    (let ((characters (round (aref drover-table--widths index) (drover-table--char-width))))
      (setf (alist-get (vtable-column drover-table--table index)
                       drover-table--width-overrides nil nil #'equal)
            (max 2 (- characters (or n 1))))
      (drover-table-refresh))))

(defun drover-table-widen-column (&optional n)
  "Widen the column at point by N characters, 1 by default.
The width holds across redraws, until `drover-table-reset'."
  (interactive "p")
  (drover-table-narrow-column (- (or n 1))))

(defvar-keymap drover-table-mode-map
  :doc "Keymap of `drover-table-mode'.
Its parent is `drover-session-map'.  It binds the keys of
`special-mode-map' itself, but for \\`SPC', which peeks at a session.
Rows and the column header also carry vtable's keymap, whose commands
are remapped here when the table must handle them itself."
  :parent drover-session-map
  "g" #'drover-table-refresh
  "q" #'quit-window
  "c" #'drover-table-toggle-children
  "=" #'drover-table-reset
  "n" #'next-line
  "p" #'previous-line
  "<" #'beginning-of-buffer
  ">" #'end-of-buffer
  "h" #'describe-mode
  "?" #'describe-mode
  "DEL" #'scroll-down-command
  "S-SPC" #'scroll-down-command
  "-" #'negative-argument
  "0" #'digit-argument "1" #'digit-argument "2" #'digit-argument
  "3" #'digit-argument "4" #'digit-argument "5" #'digit-argument
  "6" #'digit-argument "7" #'digit-argument "8" #'digit-argument
  "9" #'digit-argument
  "<remap> <self-insert-command>" #'undefined
  "<remap> <vtable-revert-command>" #'drover-table-refresh
  "<remap> <vtable-narrow-current-column>" #'drover-table-narrow-column
  "<remap> <vtable-widen-current-column>" #'drover-table-widen-column)

(define-derived-mode drover-table-mode special-mode "Drover Table"
  "Major mode listing the sessions of every agent in one table.
\\<drover-table-mode-map>Sessions come as trees, each subagent under its
parent, the tree with the most urgent session first.  \\`S' on a column
sorts the trees by the value of their root, and the subagents of each
session among themselves; the Title column sorts by title.  \\`{' and
\\`}' narrow and widen a column, and \\[drover-table-reset] restores the
order of urgency and the default widths.
\\[drover-table-toggle-children] shows or hides the subagents that do
not need you.

\\{drover-table-mode-map}"
  :group 'drover-table
  (setq-local drover-session-at-point-function #'drover-table--session-at-point)
  (setq-local revert-buffer-function #'drover-table--revert)
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)
  (add-hook 'window-buffer-change-functions #'drover-table--window-changed nil t)
  (add-hook 'window-size-change-functions #'drover-table--window-changed nil t)
  (add-hook 'kill-buffer-hook #'drover-table--killed nil t)
  (add-hook 'drover-changed-hook #'drover-table--changed)
  (add-hook 'post-command-hook #'drover-table--follow-sort))

;;;###autoload
(defun drover-table ()
  "Show the sessions of every agent in a table."
  (interactive)
  (drover-start)
  (let ((buffer (get-buffer-create drover-table-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'drover-table-mode)
        (drover-table-mode)))
    (pop-to-buffer-same-window buffer)
    (drover-table--redraw buffer)))

(provide 'drover-table)
;;; drover-table.el ends here
