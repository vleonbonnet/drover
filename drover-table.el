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

;; `drover-table' lists every session in one `vtable', most urgent
;; first, with its status, title, project, branch, age and why it has
;; its status.  The header line counts the sessions that need you.
;;
;; The table follows the backends: it is drawn again whenever
;; `drover-changed-hook' runs while it is shown, and when it is next
;; shown otherwise, so a buried table costs nothing.  Its columns are
;; sized to its window, and drawn again when the window's width changes.
;; Point stays on its session across redraws.
;;
;; Sorting a column with vtable's own commands (S on a column, or a
;; click on its header) holds across redraws, as do widths changed with
;; { and }; = restores the order of urgency and the default widths.

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
  "Sort keys of the table, as vtable's `sort-by', kept across redraws.")

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
  (string-pixel-width (drover-table--styled text) (current-buffer)))

(defun drover-table--char-width ()
  "Return the width in pixels of a character of the table."
  (drover-table--text-width "x"))

(defun drover-table--status-text (status)
  "Return the glyph and label of STATUS, in its face."
  (concat (drover-status-glyph status) " "
          (propertize (drover-status-label status) 'face (drover-status-face status))))

(defun drover-table--title (session)
  "Return the title of SESSION, marked when it is a subagent."
  (let ((title (drover-table--one-line (drover-session-display-title session))))
    (if (drover-session-parent session)
        (concat (propertize "↳ " 'face 'drover-dim) title)
      title)))

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

(defun drover-table--columns (statuses now)
  "Return the columns of a table of sessions at time NOW.
STATUSES is a vector of every status shown, most urgent first; the
Status column holds the index of a session's status in it, so that it
sorts by urgency.  Each column is a plist of:

 :name     Its header.
 :value    Function of a session returning the value it sorts by.
 :text     Function of a value returning the text shown, when the
           value is not that text.
 :align    `left' or `right'.
 :weight   Share of the room left that the column may take, or nil for
           a column as wide as its content.
 :samples  Texts the column fits even when no row shows them."
  (list
   (list :name "Status" :align 'left
         :value (lambda (session) (seq-position statuses (drover-session-status session)))
         :text (lambda (index) (drover-table--status-text (aref statuses index)))
         :samples (mapcar #'drover-table--status-text statuses))
   (list :name "Title" :align 'left :weight 3 :value #'drover-table--title)
   (list :name "Project" :align 'left :weight 1
         :value (lambda (session) (drover-table--one-line (drover-session-project session))))
   (list :name "Branch" :align 'left :weight 1
         :value (lambda (session)
                  (drover-table--one-line (drover-session-branch session) 'drover-dim)))
   (list :name "Age" :align 'right
         :value (lambda (session) (drover-table--age session now))
         :text (lambda (age) (drover-table--age-text age now)))
   (list :name "Summary" :align 'left :weight 3
         :value (lambda (session) (drover-table--one-line (drover-session-summary session))))))

(defun drover-table--cell-text (column session)
  "Return the text COLUMN shows for SESSION."
  (let ((value (funcall (plist-get column :value) session)))
    (if-let* ((text (plist-get column :text)))
        (funcall text value)
      value)))

;;;; Widths

(defun drover-table--natural-width (column sessions char-width)
  "Return the pixel width COLUMN needs to show SESSIONS in full.
The header needs room for vtable's sort indicator, and CHAR-WIDTH
more."
  (apply #'max
         (+ (drover-table--text-width (plist-get column :name))
            (drover-table--text-width "▼")
            char-width)
         (mapcar #'drover-table--text-width
                 (append (plist-get column :samples)
                         (mapcar (lambda (session) (drover-table--cell-text column session))
                                 sessions)))))

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

(defun drover-table--column-widths (columns sessions width)
  "Return the pixel widths of COLUMNS showing SESSIONS in WIDTH pixels.
Columns without a weight are as wide as their content; the user's
widths hold; the weighted columns share the room left."
  (let* ((char-width (drover-table--char-width))
         (naturals (mapcar (lambda (column)
                             (drover-table--natural-width column sessions char-width))
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

(defun drover-table--insert (sessions width)
  "Insert a table of SESSIONS, in this order, WIDTH pixels wide.
Return the vtable."
  (let* ((now (float-time))
         (statuses (vconcat drover-status-order
                            (seq-difference (seq-uniq (mapcar #'drover-session-status sessions))
                                            drover-status-order)))
         (columns (drover-table--columns statuses now))
         (widths (drover-table--column-widths columns sessions width)))
    (setq drover-table--widths (vconcat widths))
    (make-vtable
     :columns (cl-mapcar
               (lambda (column width)
                 (let ((value (plist-get column :value))
                       (text (plist-get column :text)))
                   (list :name (plist-get column :name)
                         :width (drover-table--width-spec width)
                         :align (plist-get column :align)
                         :getter (lambda (session _table) (funcall value session))
                         ;; Vtable styles the text of its values, not formatted text.
                         :formatter (and text (lambda (value)
                                                (drover-table--styled (funcall text value)))))))
               columns widths)
     :objects sessions
     :face 'drover-table
     :use-header-line nil
     :separator-width drover-table--separator-width
     :sort-by (copy-tree drover-table--sort-by))))

(defun drover-table--empty-text (sessions)
  "Return the line shown when no session is listed, SESSIONS being hidden."
  (substitute-command-keys
   (cond
    ((null (drover-backends)) "No agent backend is loaded.")
    ((null sessions) "No agent sessions.")
    (t (format "No agent sessions; %d subagent %s hidden, \
\\<drover-table-mode-map>\\[drover-table-toggle-children] shows them."
               (length sessions) (if (cdr sessions) "sessions are" "session is"))))))

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
         (all (drover-sessions))
         (sessions (drover-sort (and all (drover-visible-sessions all))))
         (inhibit-read-only t))
    (when drover-table--table
      (setq drover-table--sort-by (vtable-sort-by drover-table--table)))
    (erase-buffer)
    (setq drover-table--table (and sessions (drover-table--insert sessions width)))
    (unless sessions
      (insert (propertize (drover-table--empty-text all) 'face 'drover-dim) "\n"))
    (setq header-line-format
          (drover-table--header-line sessions (- (length all) (length sessions)))
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
  "Show or hide the sessions of subagents in this table."
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
\\<drover-table-mode-map>Sessions come most urgent first.  \\`S' on a
column sorts by it, \\`{' and \\`}' narrow and widen it, and
\\[drover-table-reset] restores the order of urgency and the default
widths.

\\{drover-table-mode-map}"
  :group 'drover-table
  (setq-local drover-session-at-point-function #'drover-table--session-at-point)
  (setq-local revert-buffer-function #'drover-table--revert)
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)
  (add-hook 'window-buffer-change-functions #'drover-table--window-changed nil t)
  (add-hook 'window-size-change-functions #'drover-table--window-changed nil t)
  (add-hook 'kill-buffer-hook #'drover-table--killed nil t)
  (add-hook 'drover-changed-hook #'drover-table--changed))

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
