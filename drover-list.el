;;; drover-list.el --- The list of agent sessions  -*- lexical-binding: t; -*-

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

;; `drover-list' lists every agent session in a Magit-like buffer, in an
;; ordinary window; `drover-list-side' shows the same buffer in a side
;; window beside your work.  It lists the sessions `drover-filter'
;; keeps, grouped as `drover-group-by' says and sorted as
;; `drover-sort-by' says; the View menu, \\`V', changes all three.  Each
;; group is a section that \\`TAB' collapses, and each session a section
;; whose body holds its details.  The keys of `drover-session-map' act
;; on the session at point.
;;
;; A subagent is listed under its parent, indented one step deeper, and
;; a parent and its subagents stay together: they go in the group of the
;; most urgent session among them.  Subagents are sections of the group,
;; like their parent, rather than sections inside their parent's: Magit
;; hides the whole body of a section, and showing a parent's details
;; must neither hide nor reveal its subagents.
;;
;; What a line shows depends on the width of the window, measured in
;; pixels in the face `drover-list', variable pitch by default.
;; `drover-list-fields' names the fields a line may show, by priority,
;; with the room each needs: the widest windows show every field as
;; aligned columns, narrower ones drop the fields of least priority,
;; and the narrowest, such as a side window, show two lines per session:
;; status, title and age, and what the session waits for under them.
;; The layout is computed anew whenever the width changes.
;;
;; A session whose provider still holds its prompt cache shows how much
;; of the cache's lifetime is left: as a short bar and the time left in
;; columns, as the background of its line, filled from the left, in two
;; lines.  Only that display is redrawn as the cache runs out.
;;
;; The buffer redraws when backends report changes, but only while it
;; is displayed: a hidden list is marked stale and redraws when it is
;; next shown.  A redraw keeps point on the same session, even when the
;; session moved to another group, and keeps every section expanded or
;; collapsed as it was.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'drover)

(defgroup drover-list nil
  "The list of agent sessions."
  :group 'drover
  :prefix "drover-list-")

(defface drover-list '((t :inherit variable-pitch))
  "Face of the text of the session list.
It inherits `variable-pitch', which fits more in a line; inherit
`fixed-pitch' instead for a monospaced list."
  :group 'drover-faces)

(defface drover-list-current '((t :inherit highlight :extend t))
  "Face of the session shown in the main window."
  :group 'drover-faces)

(defconst drover-list-buffer-name "*drover sessions*"
  "Name of the session list buffer.")

(defcustom drover-list-display-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Action by which `drover-list' displays its buffer in an ordinary window.
See `display-buffer' for the form of an action."
  :type 'sexp
  :group 'drover-list)

(defcustom drover-list-side-window-side 'left
  "Side of the frame on which `drover-list-side' shows the list."
  :type '(choice (const left) (const right))
  :group 'drover-list)

(defcustom drover-list-side-window-width 36
  "Total width of the side window of `drover-list-side', in columns."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-side-window-slot 0
  "Slot of the list among the side windows of its side.
See `display-buffer-in-side-window'."
  :type 'integer
  :group 'drover-list)

(defcustom drover-list-collapsed-groups '((status . idle))
  "Groups collapsed until expanded, as a list of (GROUPING . VALUE).
GROUPING is a value of `drover-group-by', VALUE that of the group, such
as (status . idle) for the idle sessions when grouping by status.  Once
a group is expanded or collapsed, the list remembers it."
  :type '(repeat (cons symbol sexp))
  :group 'drover-list)

(defcustom drover-list-age-interval 60
  "Seconds between redraws of a displayed list, keeping ages current.
A value of nil disables these redraws: the list then redraws only when
sessions change.  The timer runs only while a list is displayed."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-list)

(defcustom drover-list-cache-interval 5
  "Seconds between updates of the prompt cache bars of a displayed list.
Only the bars are redrawn.  A value of nil leaves them as last drawn.
The timer runs only while a displayed list shows a live cache."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-list)

(defcustom drover-list-cache-bar-width 5
  "Width of the short bar of the prompt cache field, in characters.
A character counts as wide as the letter x in the face `drover-list'."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-fields
  '((title :priority 0 :min 12 :max 40 :narrow line)
    (location :priority 4 :min 10 :max 30)
    (agent :priority 5 :min 5 :max 12)
    (model :priority 6 :min 8 :max 24)
    (created :priority 7 :min 3 :align right)
    (age :priority 1 :min 3 :align right :narrow line)
    (cache :priority 2 :min 9 :narrow background)
    (summary :priority 3 :min 12 :narrow below))
  "Fields a line of the session list may show, in the order of columns.
Each entry is (FIELD . PROPERTIES).  FIELD is one of:

 title     The status icon and the title; it is always shown.
 location  The project and branch, as \"project:branch\".
 agent     The agent.
 model     The model.
 created   How long ago the session was created.
 age       How long ago it last did something, or since when it needs
           you.
 cache     How much of the lifetime of its prompt cache is left.
 summary   What it waits for, or one line on its status.

PROPERTIES is a plist of:

 :priority  Rank of the field, 0 first; fields of a lower rank show
            only when every field of a higher one does.
 :min       Fewest characters the field needs to be shown.
 :max       Most characters it takes before being cut; nil lets it
            take what is left of the line.
 :align     `right' to align the field on the right of its column.
 :narrow    Where the field goes when the list is too narrow for
            columns: `line', on the line of the title, flush right;
            `below', on a second line, under the title, for the
            sessions that need you; `background', as the background
            of the line; nil, not shown.

Characters count as wide as the letter x in the face `drover-list'.
The list shows the fields with something to show, by priority, as long
as their :min fit in the window; it uses columns when those include
every field the narrow layout places, else the narrow layout, which
shows all of those."
  :type '(repeat (cons symbol plist))
  :group 'drover-list)

(defconst drover-list--gap 2
  "Characters between two columns.")

(defconst drover-list--margin 2
  "Characters before the sessions at the top of a tree.")

(defconst drover-list--max-depth 3
  "Deepest nesting level indented further than the one above it.
Deeper subagents line up with that level, so that titles keep room in a
narrow window.")

;;;; Sections

(defvar-keymap drover-list-group-section-map
  :doc "Keys on the heading of a group of sessions."
  "RET" #'magit-section-toggle)

(defclass drover-list-group-section (magit-section)
  ((keymap :initform 'drover-list-group-section-map))
  "A group of sessions; the value is (GROUPING . VALUE).")

(defclass drover-list-session-section (magit-section) ()
  "One session; the value is its `drover-session-key'.
Its subagents follow it in its group, as sections of their own.")

(defun drover-list--session-section (section)
  "Return SECTION or its ancestor showing a session, or nil."
  (while (and section (not (object-of-class-p section 'drover-list-session-section)))
    (setq section (oref section parent)))
  section)

(defun drover-list--group-section (section)
  "Return SECTION or its ancestor showing a group of sessions, or nil."
  (while (and section (not (object-of-class-p section 'drover-list-group-section)))
    (setq section (oref section parent)))
  section)

(defun drover-list--groups ()
  "Return the group sections of the current list."
  (and magit-root-section
       (seq-filter (lambda (section) (object-of-class-p section 'drover-list-group-section))
                   (oref magit-root-section children))))

(defun drover-list--containers ()
  "Return the sections holding the sessions of the current list.
They are its groups, or its root section when sessions are not grouped."
  (and magit-root-section
       (or (drover-list--groups) (list magit-root-section))))

(defun drover-list--session-sections (container)
  "Return the session sections CONTAINER holds, a group or the root section."
  (seq-filter (lambda (section) (object-of-class-p section 'drover-list-session-section))
              (oref container children)))

(defun drover-list--container-id (container)
  "Return what identifies CONTAINER, a group or the root section, in a redraw.
It is `root' for the root section, else the value of the group."
  (if (eq container magit-root-section)
      'root
    (cons 'group (oref container value))))

(defun drover-list--find-container (id)
  "Return the section holding sessions identified by ID, or nil.
ID is a value of `drover-list--container-id'.  The root section holds
sessions only while they are not grouped."
  (if (eq id 'root)
      (and magit-root-section
           (null (drover-list--groups))
           (drover-list--session-sections magit-root-section)
           magit-root-section)
    (seq-find (lambda (section) (equal (oref section value) (cdr id)))
              (drover-list--groups))))

(defun drover-list--find-session (key)
  "Return the section showing the session whose key is KEY, or nil."
  (seq-some (lambda (container)
              (seq-find (lambda (section) (equal (oref section value) key))
                        (drover-list--session-sections container)))
            (drover-list--containers)))

(defun drover-list--session-at-point ()
  "Return the current record of the session at point, or nil.
The record comes from the backend, not from the last redraw, so actions
see the session as it stands now."
  (when-let* ((section (drover-list--session-section (magit-current-section)))
              (key (oref section value)))
    (drover-find (car key) (cdr key))))

;;;; Visibility

(defvar-local drover-list--visibility nil
  "Hash table of the visibility, `show' or `hide', of past sections.
Groups are keyed by (group GROUPING . VALUE), sessions by a cons of
`session' and their key: a session keeps its visibility when it moves
to another group.")

(defun drover-list--visibility-key (section)
  "Return the key of SECTION in `drover-list--visibility', or nil."
  (cond ((object-of-class-p section 'drover-list-session-section)
         (cons 'session (oref section value)))
        ((object-of-class-p section 'drover-list-group-section)
         (cons 'group (oref section value)))))

(defun drover-list--remember-visibility ()
  "Record the visibility of every section of the current list."
  (when magit-root-section
    (magit-map-sections
     (lambda (section)
       (when-let* ((key (drover-list--visibility-key section)))
         (puthash key (if (oref section hidden) 'hide 'show) drover-list--visibility))))))

(defun drover-list--forget-visibility (sessions)
  "Forget the visibility of sessions other than SESSIONS.
Sessions come and go, groups are few: only sessions are forgotten."
  (let ((keys (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (drover-session-key session) t keys))
    (maphash (lambda (key _)
               (when (and (eq (car key) 'session) (not (gethash (cdr key) keys)))
                 (remhash key drover-list--visibility)))
             drover-list--visibility)))

(defun drover-list--cached-visibility (section)
  "Return the recorded visibility of SECTION, or nil.
For `magit-section-set-visibility-hook'."
  (when-let* ((key (drover-list--visibility-key section)))
    (gethash key drover-list--visibility)))

;;;; Measuring

(define-hash-table-test 'drover-list--text
  #'equal-including-properties #'sxhash-equal-including-properties)

(defvar-local drover-list--widths nil
  "Widths of texts measured, as (CONTEXT TABLE KNOWN).
CONTEXT is what widths depend on besides the text: a copy of the face
remapping of the buffer, which `face-remap-add-relative' changes in
place, and the frame measuring it.  TABLE maps texts, with their
properties, to their widths in pixels for the current drawing; KNOWN
is the table of the previous drawing in the same context, or nil.")

(defun drover-list--window ()
  "Return the window the current list is drawn for.
It is the selected window when it shows the list, else another window
showing it, else the selected window."
  (if (eq (window-buffer) (current-buffer))
      (selected-window)
    (or (get-buffer-window nil 0) (selected-window))))

(defun drover-list--measure-begin ()
  "Start a drawing: widths measured for the last one stay known.
Laying text out costs more than the rest of a redraw, so widths are
reused while the faces and fonts stay the same."
  (let ((context (list (copy-tree face-remapping-alist) (window-frame (drover-list--window)))))
    (setq drover-list--widths
          (list context
                (make-hash-table :test 'drover-list--text)
                (and (equal (car drover-list--widths) context) (nth 1 drover-list--widths))))))

(defun drover-list--measure (string)
  "Return the width in pixels of STRING, laid out as the list draws it."
  (drover-text-pixel-width string face-remapping-alist (drover-list--window)))

(defun drover-list--pixel-width (string)
  "Return the width in pixels of STRING drawn in the current buffer.
The width is that of the fonts of `drover-list--window' with the face
remapping of the buffer, which draws its text in the face `drover-list'.
On a text terminal, a pixel is a character cell."
  (if (zerop (length string))
      0
    (unless drover-list--widths
      (drover-list--measure-begin))
    (pcase-let ((`(,_ ,table ,known) drover-list--widths))
      (or (gethash string table)
          (puthash string
                   (or (and known (gethash string known)) (drover-list--measure string))
                   table)))))

(defun drover-list--char-width ()
  "Return the width in pixels of an average character, the letter x."
  (drover-list--pixel-width "x"))

(defun drover-list--line-width ()
  "Return the width in pixels that lines of the list may take.
Like `window-max-chars-per-line', leave out line numbers, and the
column of the continuation glyph when the window has no fringe for it."
  (let* ((window (drover-list--window))
         (fringes (window-fringes window))
         (width (window-body-width window t)))
    (when (buffer-local-value 'display-line-numbers (window-buffer window))
      (setq width (- width (with-selected-window window (line-number-display-width t)))))
    (if (and (display-graphic-p (window-frame window))
             overflow-newline-into-fringe
             (not (eq (car fringes) 0))
             (not (eq (cadr fringes) 0)))
        width
      (- width (window-font-width window)))))

(defun drover-list--truncate (string pixels)
  "Return STRING cut to at most PIXELS wide, ending in an ellipsis if cut.
The ellipsis takes the properties of the text before it.  Return \"\"
when not even the ellipsis fits."
  (drover-truncate-to-pixels string (max 0 pixels) #'drover-list--pixel-width))

(defun drover-list--align (x)
  "Return a space reaching X pixels from the left of the text area."
  (propertize " " 'display `(space :align-to (,x))))

(defun drover-list--now ()
  "Return the current time, as a float.
The list reads the clock through it, so that tests may stop the clock."
  (float-time))

;;;; Fields

(defun drover-list--one-line (string)
  "Return STRING, or \"\" for nil, on one line."
  (string-trim (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" (or string "")))))

(defun drover-list--age-time (session)
  "Return the time from which the age of SESSION counts, or nil.
A session needing the user ages from when its status began, the others
from their last activity, matching the order of `drover-sort'."
  (if (drover-session-attention-p session)
      (or (drover-session-since session) (drover-session-updated session))
    (or (drover-session-updated session) (drover-session-since session))))

(defun drover-list--field-spec (field)
  "Return the properties of FIELD in `drover-list-fields', or nil."
  (alist-get field drover-list-fields))

(defun drover-list--title-text (session depth)
  "Return the title of SESSION, at DEPTH in its tree, as its line shows it.
A subagent at the top of its tree, its parent not listed, starts with
an arrow; an idle session is dimmed."
  (let ((title (drover-list--one-line (drover-session-display-title session))))
    (concat (if (and (zerop depth) (drover-session-parent session))
                (drover-face "↳ " 'drover-dim)
              "")
            (if (eq (drover-session-status session) 'idle) (drover-face title 'drover-dim) title))))

(defun drover-list--text (field session location now)
  "Return the text of FIELD for SESSION, or \"\" when it has none.
LOCATION is a function of a session returning where it works, as its
group shows it; ages count up to NOW.  The prompt cache has no text:
it is drawn apart."
  (pcase field
    ('location (drover-face (drover-list--one-line (funcall location session)) 'drover-dim))
    ('agent (drover-face (drover-list--one-line (drover-session-agent session)) 'drover-dim))
    ('model (drover-face (drover-list--one-line (drover-session-model session)) 'drover-dim))
    ('created (drover-face (drover-format-age (drover-session-created session) now) 'drover-dim))
    ('age (drover-face (drover-format-age (drover-list--age-time session) now) 'drover-dim))
    ('summary (drover-list--one-line (drover-session-summary session)))
    (_ "")))

(cl-defstruct (drover-list--row (:constructor drover-list--row-create)
                                (:copier nil))
  "A session as its heading shows it, before the layout."
  (session nil :documentation "The `drover-session' shown.")
  (depth nil :documentation "Depth of the session in its tree.")
  (indent nil :documentation "Position of the status glyph, in pixels.")
  (title-start nil :documentation "Position of the title, in pixels.")
  (title nil :documentation "The title, without the glyph.")
  (texts nil :documentation "Alist (FIELD . TEXT) of the other fields.")
  (cache nil :documentation "Fraction of the prompt cache's lifetime left, or nil."))

(defun drover-list--metrics (now)
  "Return the measures lines of the list are drawn with at time NOW.
The value is a plist of :now, :char, the width of an average character,
:gap, the room between two columns, :space, the width of a space,
:margin, where the sessions at the top of a tree begin, :step, how much
deeper each level of subagents begins, and :width, the width lines may
take.  A step is as wide as the widest status glyph and a space, so that
a subagent's glyph stands under its parent's title.  Widths are in
pixels; lines leave room for the ellipsis Magit adds to collapsed
headings."
  (let ((char (drover-list--char-width))
        (statuses (seq-uniq (append drover-status-order (mapcar #'car drover-status-glyphs)))))
    (list :now now
          :char char
          :gap (* drover-list--gap char)
          :space (drover-list--pixel-width " ")
          :margin (* drover-list--margin char)
          :step (apply #'max 0 (mapcar (lambda (status)
                                         (drover-list--pixel-width
                                          (concat (drover-status-glyph status) " ")))
                                       statuses))
          :width (- (drover-list--line-width)
                    (drover-list--pixel-width (truncate-string-ellipsis))))))

(defun drover-list--row (session depth location metrics)
  "Return the row of SESSION at DEPTH in its tree.
LOCATION is a function of a session returning where it works, as its
group shows it; METRICS are the measures of `drover-list--metrics'."
  (let* ((now (plist-get metrics :now))
         (indent (+ (plist-get metrics :margin)
                    (* (min depth drover-list--max-depth) (plist-get metrics :step)))))
    (drover-list--row-create
     :session session :depth depth :indent indent
     :title-start (+ indent (drover-list--pixel-width
                             (concat (drover-status-glyph (drover-session-status session)) " ")))
     :title (drover-list--title-text session depth)
     :texts (mapcar (lambda (field) (cons field (drover-list--text field session location now)))
                    '(location agent model created age summary))
     :cache (drover-session-cache-fraction session now))))

(defun drover-list--tree-rows (nodes location metrics &optional depth)
  "Return the rows of the trees NODES at DEPTH, subagents after their parent.
LOCATION is a function of a session returning where it works, as the
group shows it; METRICS are the measures of `drover-list--metrics'."
  (let ((depth (or depth 0)))
    (mapcan (lambda (node)
              (let ((session (drover-node-session node)))
                (cons (drover-list--row session depth location metrics)
                      (drover-list--tree-rows (drover-node-children node) location metrics
                                              (1+ depth)))))
            nodes)))

(defun drover-list--has-p (field rows)
  "Return non-nil when one of ROWS has something to show for FIELD."
  (pcase field
    ('title t)
    ('cache (seq-some #'drover-list--row-cache rows))
    (_ (seq-some (lambda (row)
                   (not (string-empty-p (or (alist-get field (drover-list--row-texts row)) ""))))
                 rows))))

;;;; Layout

(defun drover-list--cache-cell-width (metrics)
  "Return the width of the prompt cache field, in pixels, per METRICS.
It holds the short bar, a space and the longest time left it shows."
  (+ (* drover-list-cache-bar-width (plist-get metrics :char))
     (plist-get metrics :space)
     (drover-list--pixel-width (drover-face "59m" 'drover-dim))))

(defun drover-list--natural (field rows metrics)
  "Return the width in pixels FIELD takes to show ROWS in full, per METRICS.
That of the title counts from the margin, indentation and glyph
included."
  (pcase field
    ('title (- (apply #'max 0 (mapcar (lambda (row)
                                        (+ (drover-list--row-title-start row)
                                           (drover-list--pixel-width (drover-list--row-title row))))
                                      rows))
               (plist-get metrics :margin)))
    ('cache (drover-list--cache-cell-width metrics))
    (_ (apply #'max 0 (mapcar (lambda (row)
                                (drover-list--pixel-width (alist-get field (drover-list--row-texts row))))
                              rows)))))

(defun drover-list--title-prefix (rows metrics)
  "Return the room, in pixels, before titles in ROWS, from the margin, per METRICS."
  (- (apply #'max 0 (mapcar #'drover-list--row-title-start rows))
     (plist-get metrics :margin)))

(defun drover-list--choose (rows metrics)
  "Return the layout of ROWS in lines of the width of METRICS.
The value is (LAYOUT . FIELDS): LAYOUT is `columns' or `narrow', and
FIELDS the entries of `drover-list-fields' shown, in their order.  The
fields with something to show for ROWS are taken by priority, as long as
their minimum widths fit, and the title always.  Columns are used when
they include every field the narrow layout places; else the narrow
layout shows those fields, which need no room of their own but on the
title's line, where they give way to the title."
  (let* ((char (plist-get metrics :char))
         (gap (plist-get metrics :gap))
         (present (seq-filter (lambda (entry) (drover-list--has-p (car entry) rows))
                              drover-list-fields))
         (used (+ (plist-get metrics :margin) (drover-list--title-prefix rows metrics)))
         (chosen nil))
    (catch 'full
      (dolist (entry (seq-sort-by (lambda (entry) (or (plist-get (cdr entry) :priority) 99)) #'<
                                  present))
        (let* ((title (eq (car entry) 'title))
               (need (+ (* char (or (plist-get (cdr entry) :min) 0)) (if title 0 gap))))
          (if (or title (<= (+ used need) (plist-get metrics :width)))
              (setq used (+ used need)
                    chosen (cons entry chosen))
            (throw 'full nil)))))
    (let ((narrow (seq-filter (lambda (entry)
                                (or (eq (car entry) 'title) (plist-get (cdr entry) :narrow)))
                              present)))
      (if (seq-every-p (lambda (entry) (memq entry chosen)) narrow)
          (cons 'columns (seq-filter (lambda (entry) (memq entry chosen)) present))
        (cons 'narrow narrow)))))

(defun drover-list--column-widths (fields rows metrics)
  "Return the widths in pixels of the columns of FIELDS showing ROWS.
FIELDS are entries of `drover-list-fields'; METRICS give the width of
lines.  A column is as wide as what it shows, up to its :max; when the
line is too narrow for that, the fields without a :max give up room
first, down to their :min, then the others in proportion to what they
would lose, down to theirs."
  (let* ((char (plist-get metrics :char))
         (prefix (drover-list--title-prefix rows metrics))
         (room (- (plist-get metrics :width) (plist-get metrics :margin)
                  (* (plist-get metrics :gap) (max 0 (1- (length fields))))))
         (bounds (mapcar (lambda (entry)
                           (let* ((field (car entry))
                                  (natural (drover-list--natural field rows metrics))
                                  (extra (if (eq field 'title) prefix 0))
                                  (max (plist-get (cdr entry) :max))
                                  (want (if max (min natural (+ extra (* char max))) natural))
                                  (floor (min want (+ extra (* char (or (plist-get (cdr entry) :min) 0))))))
                             (list want (if (eq field 'cache) want floor) (null max))))
                         fields))
         (widths (mapcar #'car bounds))
         (deficit (- (apply #'+ widths) room)))
    (when (> deficit 0)
      ;; Flexible fields first, then the others in proportion.
      (dolist (pass '(flexible fixed))
        (when (> deficit 0)
          (let* ((indices (seq-filter (lambda (index)
                                        (eq (nth 2 (nth index bounds)) (eq pass 'flexible)))
                                      (number-sequence 0 (1- (length bounds)))))
                 (slack (apply #'+ (mapcar (lambda (index)
                                             (- (nth index widths) (nth 1 (nth index bounds))))
                                           indices))))
            (when (> slack 0)
              (let ((cut (min deficit slack)))
                (dolist (index indices)
                  (let ((give (- (nth index widths) (nth 1 (nth index bounds)))))
                    (setf (nth index widths)
                          (- (nth index widths) (ceiling (* cut give) slack)))))
                (setq deficit (- deficit cut))))))))
    (cl-mapcar (lambda (width bound) (max width (nth 1 bound) 0)) widths bounds)))

(defun drover-list--layout (rows metrics)
  "Return METRICS with the layout of ROWS added.
Add :layout, `columns' or `narrow', and :fields, the fields shown; in
columns, :columns, a list of (FIELD X WIDTH ALIGN) giving where each
field begins and how wide it is, in pixels."
  (pcase-let* ((`(,layout . ,fields) (drover-list--choose rows metrics))
               (columns nil))
    (when (eq layout 'columns)
      (let ((x (plist-get metrics :margin)))
        (cl-mapc (lambda (entry width)
                   (push (list (car entry) x width (plist-get (cdr entry) :align)) columns)
                   (setq x (+ x width (plist-get metrics :gap))))
                 fields (drover-list--column-widths fields rows metrics))))
    (append (list :layout layout :fields (mapcar #'car fields) :columns (nreverse columns))
            metrics)))

;;;; Lines

(defun drover-list--assemble (segments &optional split)
  "Return the string of SEGMENTS, and where they lie, as (STRING . SPEC).
Each segment is (align X), a space reaching X pixels, or (text STRING
WIDTH X), STRING, WIDTH pixels wide, beginning X pixels in.  With SPLIT,
each space is two, the first, of no width, ready to stretch: SPEC then
lists each segment as (OFFSET split FROM TO), where FROM and TO bound
the space, or (OFFSET text X WIDTH STRING), OFFSET being where it
begins in STRING.  A last space reaching SPLIT, a number, closes the
line, so that a bar may fill all of it."
  (let ((x 0) (parts nil) (spec nil) (offset 0))
    (dolist (segment (if (numberp split) (append segments (list (list 'align split))) segments))
      (pcase segment
        (`(align ,to)
         (if split
             (progn
               (push (list offset 'split x (max x to)) spec)
               (push (propertize " " 'display '(space :width (0))) parts)
               (push (drover-list--align to) parts)
               (setq offset (+ offset 2)))
           (push (drover-list--align to) parts)
           (setq offset (1+ offset)))
         (setq x (max x to)))
        (`(text ,string ,width ,start)
         (push (list offset 'text start width string) spec)
         (push string parts)
         (setq offset (+ offset (length string))
               x (+ start width)))))
    (cons (apply #'concat (nreverse parts)) (nreverse spec))))

(defun drover-list--text-segment (string x)
  "Return the segment of STRING beginning X pixels in, for `drover-list--assemble'."
  (list 'text string (drover-list--pixel-width string) x))

(defun drover-list--title-segments (row room &optional shift)
  "Return the segments of the glyph and title of ROW, the title ROOM pixels wide.
They begin SHIFT pixels further right than the row's indentation, when
the title's column does not come first."
  (let* ((shift (or shift 0))
         (glyph (concat (drover-status-glyph (drover-session-status (drover-list--row-session row))) " "))
         (indent (+ shift (drover-list--row-indent row)))
         (title (drover-list--truncate (drover-list--row-title row) room)))
    (list (list 'align indent)
          (drover-list--text-segment glyph indent)
          (drover-list--text-segment title (+ shift (drover-list--row-title-start row))))))

(defun drover-list--cache-cell (fraction remaining layout)
  "Return the prompt cache field: a bar FRACTION full, REMAINING seconds left.
The value is a string of four characters, displayed as the full part of
the bar, its empty part, a space and the time left; LAYOUT gives the
measures.  Each character tells its part in the property
`drover-list-cache', so that only they change as time passes."
  (let* ((bar (* drover-list-cache-bar-width (plist-get layout :char)))
         (full (round (* bar fraction))))
    (concat (propertize " " 'display `(space :width (,full)) 'face 'drover-cache-bar
                        'drover-list-cache 'full)
            (propertize " " 'display `(space :width (,(- bar full))) 'face 'drover-cache-trough
                        'drover-list-cache 'trough)
            (propertize " " 'drover-list-cache 'space)
            (propertize " " 'display (drover-face (drover-format-duration remaining) 'drover-dim)
                        'drover-list-cache 'label))))

(defun drover-list--column-line (row layout)
  "Return the heading line of ROW in the columns of LAYOUT, and its bar.
The value is (LINE . CACHE): CACHE is non-nil when the line shows a
prompt cache field.  Each cell begins with a space aligned to its
column, so that columns line up whatever the widths of the characters
before them."
  (let ((segments nil)
        (cache nil)
        (session (drover-list--row-session row)))
    (pcase-dolist (`(,field ,x ,width ,align) (plist-get layout :columns))
      (pcase field
        ('title
         (let ((shift (- x (plist-get layout :margin))))
           (setq segments (append segments
                                  (drover-list--title-segments
                                   row (- (+ x width) shift (drover-list--row-title-start row))
                                   shift)))))
        ('cache
         (when-let* ((fraction (drover-list--row-cache row)))
           (let ((cell (drover-list--cache-cell
                        fraction (drover-session-cache-remaining session (plist-get layout :now))
                        layout)))
             (setq cache t
                   segments (append segments (list (list 'align x)
                                                   (list 'text cell (drover-list--cache-cell-width layout)
                                                         x)))))))
        (_
         (let ((text (drover-list--truncate (alist-get field (drover-list--row-texts row)) width)))
           (unless (string-empty-p text)
             (let* ((text-width (drover-list--pixel-width text))
                    (start (if (eq align 'right) (- (+ x width) text-width) x)))
               (setq segments (append segments (list (list 'align start)
                                                     (list 'text text text-width start))))))))))
    (cons (car (drover-list--assemble segments)) cache)))

(defun drover-list--narrow-line (row layout)
  "Return the heading of ROW in the narrow layout of LAYOUT, and its bar.
The value is (HEADING . SPEC).  HEADING is one or two lines: the glyph,
the title and the fields placed on its line, flush right, then, for a
session that needs the user, the fields placed below, under the title.
SPEC tells where the first line's segments lie, from
`drover-list--assemble', when a prompt cache colours its background,
else nil."
  (let* ((session (drover-list--row-session row))
         (width (plist-get layout :width))
         (space (plist-get layout :space))
         (texts (drover-list--row-texts row))
         (narrow (lambda (place)
                   (seq-filter (lambda (field)
                                 (eq (plist-get (drover-list--field-spec field) :narrow) place))
                               (remq 'title (plist-get layout :fields)))))
         (right nil)
         (end width))
    ;; The fields on the title's line, from the right edge leftwards.
    (dolist (field (reverse (funcall narrow 'line)))
      (let ((text (or (alist-get field texts) "")))
        (unless (string-empty-p text)
          (let ((start (- end (drover-list--pixel-width text))))
            (when (> start (+ (drover-list--row-title-start row) space))
              (setq right (append (list (list 'align start) (drover-list--text-segment text start))
                                  right)
                    end (- start space)))))))
    (let* ((cache (and (memq 'cache (funcall narrow 'background)) (drover-list--row-cache row)))
           (line (drover-list--assemble
                  (append (drover-list--title-segments
                           row (- (if right end width) (drover-list--row-title-start row)))
                          right)
                  (and cache width)))
           (below (and (drover-session-attention-p session)
                       (string-join (seq-remove #'string-empty-p
                                                (mapcar (lambda (field) (or (alist-get field texts) ""))
                                                        (funcall narrow 'below)))
                                    "  "))))
      (cons (concat (car line)
                    (when (and below (not (string-empty-p below)))
                      (let ((start (drover-list--row-title-start row)))
                        (concat "\n" (drover-list--align start)
                                (drover-list--truncate (drover-face below 'drover-dim)
                                                       (- width start))))))
            (and cache (cdr line))))))

;;;; The prompt cache

(defvar-local drover-list--caches nil
  "Prompt caches drawn, live when last drawn, as plists.
Each has :session, the record drawn, :start, where its heading begins,
:kind, `columns' for a field or `narrow' for a background, :spec, the
segments of a background line from `drover-list--assemble', :width, the
width a background fills, :bar, the width of a field's bar, in pixels,
and :overlay, the overlay of a background.")

(defun drover-list--fitting (string pixels)
  "Return how many characters of STRING, from its start, fit in PIXELS."
  (let ((low 0)
        (high (length string)))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (<= (drover-list--pixel-width (substring string 0 middle)) pixels)
            (setq low middle)
          (setq high (1- middle)))))
    low))

(defun drover-list--draw-background (entry fraction)
  "Colour the line of ENTRY, from the left, over FRACTION of its width.
FRACTION nil colours nothing.  The bar ends at a space, stretched to
the pixel, or between two characters of text."
  (let* ((start (plist-get entry :start))
         (spec (plist-get entry :spec))
         (x (and fraction (round (* fraction (plist-get entry :width)))))
         (end start))
    (cl-flet ((stretch (offset width)
                (put-text-property (+ start offset) (+ start offset 1)
                                   'display `(space :width (,(max 0 width))))))
      (dolist (item spec)
        (when (eq (nth 1 item) 'split)
          (stretch (car item) 0)))
      (when x
        (catch 'done
          (dolist (item spec)
            (pcase item
              (`(,offset split ,from ,to)
               (cond ((>= x to)
                      (stretch offset (- to from))
                      (setq end (+ start offset 2)))
                     ((> x from)
                      (stretch offset (- x from))
                      (setq end (+ start offset 1))
                      (throw 'done nil))
                     (t (throw 'done nil))))
              (`(,offset text ,from ,width ,string)
               (if (>= x (+ from width))
                   (setq end (+ start offset (length string)))
                 (setq end (+ start offset (drover-list--fitting string (- x from))))
                 (throw 'done nil))))))))
    (move-overlay (plist-get entry :overlay) start end)))

(defun drover-list--draw-field (entry fraction remaining)
  "Draw the prompt cache field of ENTRY, FRACTION full, REMAINING seconds left.
FRACTION nil draws nothing in its place."
  (let* ((start (plist-get entry :start))
         (bar (plist-get entry :bar))
         (full (if fraction (round (* bar fraction)) 0))
         (end (save-excursion (goto-char start) (line-end-position))))
    (let ((position start))
      (while (setq position (text-property-not-all position end 'drover-list-cache nil))
        (put-text-property
         position (1+ position) 'display
         (pcase (get-text-property position 'drover-list-cache)
           ('full `(space :width (,full)))
           ('trough `(space :width (,(if fraction (- bar full) 0))))
           ('space (if fraction nil '(space :width (0))))
           ('label (if fraction (drover-face (drover-format-duration remaining) 'drover-dim) ""))))
        (setq position (1+ position))))))

(defun drover-list--draw-caches (&optional now)
  "Redraw the prompt caches of the current list as they stand at NOW.
NOW defaults to the current time.  Only the caches change, not the
text: a cache gone out shows nothing, and is forgotten."
  (let ((now (or now (drover-list--now)))
        (inhibit-read-only t))
    (with-silent-modifications
      (setq drover-list--caches
            (seq-filter
             (lambda (entry)
               (let* ((session (plist-get entry :session))
                      (fraction (drover-session-cache-fraction session now)))
                 (if (eq (plist-get entry :kind) 'narrow)
                     (drover-list--draw-background entry fraction)
                   (drover-list--draw-field entry fraction
                                            (drover-session-cache-remaining session now)))
                 (unless fraction
                   (when-let* ((overlay (plist-get entry :overlay)))
                     (delete-overlay overlay)))
                 fraction))
             drover-list--caches)))))

;;;; Drawing

(defvar drover-list-mode-map)

(defvar-local drover-list--stale nil
  "Non-nil when sessions changed since the hidden list was drawn.")

(defvar-local drover-list--width nil
  "Width, in pixels, of the lines the list was last drawn for.")

(defvar-local drover-list--layout nil
  "Layout of the last drawing, `columns' or `narrow'.")

(defvar-local drover-list--fields nil
  "Fields of `drover-list-fields' the last drawing shows, in their order.")

(defvar-local drover-list--drawn nil
  "Hash table of the sessions drawn, by key, as records of the last drawing.")

(defun drover-list--help (session)
  "Return the tooltip of SESSION: its full title and details."
  (string-join
   (delq nil (list (drover-list--one-line (drover-session-display-title session))
                   (drover-session-location session)
                   (when-let* ((agent (drover-session-agent session)))
                     (if-let* ((model (drover-session-model session)))
                         (format "%s (%s)" agent model)
                       agent))
                   (when-let* ((summary (drover-session-summary session)))
                     (drover-list--one-line summary))))
   "\n"))

(defun drover-list--details (session now)
  "Return the details of SESSION as a list of (LABEL VALUE FACE).
Ages are relative to NOW; details without a value are left out."
  (let* ((status (drover-session-status session))
         (time (drover-list--age-time session))
         (parent-id (drover-session-parent session))
         (created (drover-session-created session))
         (cache (drover-session-cache-remaining session now))
         (details
          `(("Status" ,(concat (drover-status-label status)
                               (and time (format " for %s" (drover-format-age time now))))
             ,(drover-status-face status))
            ("Directory" ,(and (drover-session-directory session)
                               (abbreviate-file-name (drover-session-directory session))))
            ("Branch" ,(drover-session-branch session))
            ("Agent" ,(drover-session-agent session))
            ("Model" ,(drover-session-model session))
            ("Created" ,(and created (concat (drover-format-age created now) " ago")))
            ("Prompt cache" ,(and cache (concat (drover-format-duration cache) " left")))
            ("Subagent of" ,(and parent-id
                                 (if-let* ((parent (drover-find (drover-session-backend session)
                                                                parent-id)))
                                     (drover-session-display-title parent)
                                   parent-id)))
            ("Backend" ,(drover-backend-label session))
            ("Detail" ,(drover-session-detail session) ,(drover-status-face status))
            ,@(mapcar (lambda (request)
                        (list (capitalize (format "%s" (or (plist-get request :kind) "request")))
                              (plist-get request :summary)
                              'drover-waiting))
                      (drover-session-requests session)))))
    (delq nil
          (mapcar (lambda (detail)
                    (let ((value (drover-list--one-line (nth 1 detail))))
                      (unless (string-empty-p value)
                        (list (car detail) value (nth 2 detail)))))
                  details))))

(defun drover-list--insert-details (session x layout)
  "Insert the details of SESSION, their labels X pixels from the left.
LAYOUT gives the measures and the time ages count up to."
  (let* ((details (mapcar (lambda (detail)
                            (cons (drover-face (car detail) 'drover-dim) (cdr detail)))
                          (drover-list--details session (plist-get layout :now))))
         (values-x (+ x (plist-get layout :gap)
                      (apply #'max 0 (mapcar (lambda (detail)
                                               (drover-list--pixel-width (car detail)))
                                             details)))))
    (pcase-dolist (`(,label ,value ,face) details)
      (insert (drover-list--align x) label
              (drover-list--align values-x)
              (drover-list--truncate (if face (drover-face value face) value)
                                     (- (plist-get layout :width) values-x))
              "\n"))))

(defun drover-list--insert-session (row layout)
  "Insert the section of the session of ROW, in LAYOUT.
Its details begin under its title.  A prompt cache it shows is noted
in `drover-list--caches'."
  (let* ((session (drover-list--row-session row))
         (start (point))
         (drawn (if (eq (plist-get layout :layout) 'columns)
                    (let ((line (drover-list--column-line row layout)))
                      (list (car line) (and (cdr line) 'columns) nil))
                  (let ((heading (drover-list--narrow-line row layout)))
                    (list (car heading) (and (cdr heading) 'narrow) (cdr heading)))))
         (x (+ (drover-list--row-indent row) (plist-get layout :step))))
    (magit-insert-section (drover-list-session-section (drover-session-key session) t)
      (magit-insert-heading (propertize (car drawn) 'help-echo (drover-list--help session)))
      (magit-insert-section-body
        (drover-list--insert-details session x layout)))
    (puthash (drover-session-key session) session drover-list--drawn)
    (when (nth 1 drawn)
      (push (list :session session :start start :kind (nth 1 drawn) :spec (nth 2 drawn)
                  :width (plist-get layout :width)
                  :bar (* drover-list-cache-bar-width (plist-get layout :char))
                  :overlay (and (eq (nth 1 drawn) 'narrow)
                                (let ((overlay (make-overlay start start)))
                                  (overlay-put overlay 'face 'drover-cache-bar)
                                  ;; Below the highlights of the current section and session.
                                  (overlay-put overlay 'priority -50)
                                  overlay)))
            drover-list--caches))))

(defun drover-list--count (rows)
  "Return the number of ROWS, dimmed and in parentheses."
  (drover-face (format " (%d)" (length rows)) 'drover-dim))

(defun drover-list--status-counts (sessions)
  "Return how many of SESSIONS need the user, per status, as one string.
Return nil when none does."
  (when-let* ((parts (delq nil
                           (mapcar (lambda (status)
                                     (let ((count (seq-count (lambda (session)
                                                               (eq (drover-session-status session) status))
                                                             sessions)))
                                       (when (> count 0)
                                         (concat (drover-status-glyph status)
                                                 (drover-face (format "%d" count)
                                                              (drover-status-face status))))))
                                   drover-attention-statuses))))
    (string-join parts " ")))

(defun drover-list--location-function (group)
  "Return the function giving where a session works, as GROUP shows it.
GROUP is (GROUPING . VALUE), or nil.  A project group names its
project, so its sessions show only their branch."
  (pcase group
    (`(project . ,project)
     (lambda (session)
       (if (equal (drover-session-project session) project)
           (drover-session-branch session)
         (drover-session-location session))))
    (_ #'drover-session-location)))

(defun drover-list--group-heading (group rows)
  "Return the heading of GROUP, (GROUPING . VALUE), listing ROWS."
  (pcase-let ((`(,grouping . ,value) group))
    (concat (drover-face (drover-group-label grouping value)
                         (cond ((eq grouping 'status) (list (drover-status-face value) 'drover-heading))
                               ((and (null value) (not (eq grouping 'updated))) '(italic drover-heading))
                               (t 'drover-heading)))
            (drover-list--count rows)
            (unless (eq grouping 'status)
              (when-let* ((counts (drover-list--status-counts
                                   (mapcar #'drover-list--row-session rows))))
                (concat "  " counts))))))

(defun drover-list--insert-group (group rows layout)
  "Insert the section of GROUP holding ROWS, in LAYOUT.
GROUP is (GROUPING . VALUE); when it is nil, insert the rows alone, in
the current section."
  (if (null group)
      (dolist (row rows)
        (drover-list--insert-session row layout))
    (magit-insert-section (drover-list-group-section group (and (member group drover-list-collapsed-groups) t))
      (magit-insert-heading (drover-list--truncate (drover-list--group-heading group rows)
                                                   (plist-get layout :width)))
      (dolist (row rows)
        (drover-list--insert-session row layout))
      (insert "\n"))))

(defun drover-list--flow (parts width)
  "Return PARTS, strings, laid out in lines WIDTH pixels wide, as one string.
Parts are separated by three spaces, and one that does not fit on a
line starts the next; a part wider than a line is cut."
  (let ((separator "   ")
        (lines nil)
        (line nil))
    (dolist (part parts)
      (let ((joined (if line (concat line separator part) part)))
        (if (or (null line) (<= (drover-list--pixel-width joined) width))
            (setq line joined)
          (push line lines)
          (setq line part))))
    (when line
      (push line lines))
    (mapconcat (lambda (line) (concat (drover-list--truncate line width) "\n"))
               (nreverse lines) "")))

(defun drover-list--insert-header (sessions hidden layout)
  "Insert the summary of SESSIONS, of which HIDDEN subagents are not shown.
When `drover-filter' is set, the filter follows.  Lines fit the width of
LAYOUT."
  (magit-insert-section (drover-list-header)
    (let* ((counts (seq-filter (lambda (count) (> (cdr count) 0))
                               (mapcar (lambda (status)
                                         (cons status (seq-count (lambda (session)
                                                                   (eq (drover-session-status session) status))
                                                                 sessions)))
                                       drover-attention-statuses)))
           (width (plist-get layout :width))
           (parts (append
                   (if counts
                       (mapcar (lambda (count)
                                 (concat (drover-status-glyph (car count))
                                         " "
                                         (drover-face
                                          (format "%d %s" (cdr count)
                                                  (downcase (drover-status-label (car count))))
                                          (drover-status-face (car count)))))
                               counts)
                     (list (drover-face "Nothing needs you" 'drover-dim)))
                   (list (drover-face
                          (concat (format "%d session%s" (length sessions)
                                          (if (= (length sessions) 1) "" "s"))
                                  (and (> hidden 0)
                                       (format ", %d subagent%s hidden" hidden (if (= hidden 1) "" "s"))))
                          'drover-dim)))))
      (insert (drover-list--flow parts width))
      (when-let* ((filter (drover-filter-description nil (plist-get layout :now))))
        (insert (drover-list--flow
                 (list (concat (drover-face "Filter: " 'drover-dim) filter)
                       (concat (drover-filter-clear-keys drover-list-mode-map)
                               (drover-face " clears it" 'drover-dim)))
                 width)))
      (insert "\n"))))

(defun drover-list--insert-empty (sessions layout)
  "Insert why the list shows none of SESSIONS, every session known.
When there are SESSIONS, `drover-filter' left them all out: name the
filter and how to clear it.  Lines fit the width of LAYOUT."
  (magit-insert-section (drover-list-empty)
    (let ((width (plist-get layout :width)))
      (cond
       ((null (drover-backends))
        (insert (drover-list--flow '("No agent backend is registered.") width)
                (drover-list--flow '("Load an agent front end, such as opencode.el, to see its sessions.")
                                   width)))
       ((and sessions (drover-filter-active-p))
        (insert (drover-list--flow (list (concat "No session matches the filter: "
                                                 (drover-filter-description nil (plist-get layout :now))
                                                 "."))
                                   width)
                (drover-list--flow (list (drover-filter-clear-hint drover-list-mode-map)) width)))
       (t (insert "No sessions.\n"))))))

(defun drover-list--insert ()
  "Insert every section of the list at point."
  (let* ((now (drover-list--now))
         (all (drover-known-sessions))
         (groups (and all (drover-view-groups all now)))
         (metrics (drover-list--metrics now))
         (grouped (mapcar (pcase-lambda (`(,group . ,nodes))
                            (cons group (drover-list--tree-rows
                                         nodes (drover-list--location-function group) metrics)))
                          groups))
         (rows (mapcan (lambda (group) (copy-sequence (cdr group))) grouped))
         ;; Columns line up across groups.
         (layout (drover-list--layout rows metrics)))
    (setq drover-list--layout (plist-get layout :layout)
          drover-list--fields (plist-get layout :fields))
    (drover-list--forget-visibility all)
    (magit-insert-section (drover-list)
      ;; Every tree the filter keeps has its root shown, so no row means
      ;; no session at all, or none the filter keeps.
      (if (null rows)
          (drover-list--insert-empty all layout)
        (drover-list--insert-header (mapcar #'drover-list--row-session rows)
                                    (drover-view-hidden-count all) layout)
        (pcase-dolist (`(,group . ,group-rows) grouped)
          (drover-list--insert-group group group-rows layout))))))

;;;; Keeping point

(defun drover-list--anchor (position)
  "Return what is at POSITION, to find it again after a redraw.
The value is a plist of :key, the key of the session there, :offset,
the line within its section, :container, the identifier of the group
there or of the root section holding the session (see
`drover-list--container-id'), :index, the position of the session
among the sessions of its container, :line, the line number, and
:char, the position within the line.  Positions in a line count
characters, not columns: with variable pitch and aligned cells, columns
say little."
  (save-excursion
    (goto-char position)
    (let* ((section (magit-section-at))
           (session (drover-list--session-section section))
           (container (if session
                          (oref session parent)
                        (drover-list--group-section section))))
      (list :key (and session (oref session value))
            :offset (and session (- (line-number-at-pos) (line-number-at-pos (oref session start))))
            :container (and container (drover-list--container-id container))
            :index (and session
                        (seq-position (drover-list--session-sections container) session))
            :line (line-number-at-pos)
            :char (- (point) (line-beginning-position))))))

(defun drover-list--move-in-line (char)
  "Move point CHAR characters into its line, or to its end."
  (goto-char (min (+ (line-beginning-position) char) (line-end-position))))

(defun drover-list--visible-start (section)
  "Return the start of SECTION, or of its outermost collapsed ancestor."
  (let ((visible section))
    (while-let ((parent (oref section parent)))
      (when (oref parent hidden)
        (setq visible parent))
      (setq section parent))
    (oref visible start)))

(defun drover-list--resolve (anchor)
  "Return the position of ANCHOR, a value of `drover-list--anchor'.
Prefer the same session, wherever it moved, then the session that took
its place in its group, or in the list when sessions are not grouped,
then the same line."
  (save-excursion
    (let* ((key (plist-get anchor :key))
           (session (and key (drover-list--find-session key)))
           (container-id (plist-get anchor :container))
           (container (and (not session) container-id
                           (drover-list--find-container container-id))))
      (cond
       (session
        (let ((start (drover-list--visible-start session)))
          (goto-char start)
          ;; In a collapsed group, the group heading stands for the session.
          (when (= start (oref session start))
            (forward-line (plist-get anchor :offset))
            (unless (and (< (point) (oref session end))
                         (not (invisible-p (point)))
                         (eq (drover-list--session-section (magit-section-at)) session))
              (goto-char start))
            (drover-list--move-in-line (plist-get anchor :char)))))
       (container
        (let ((sessions (drover-list--session-sections container))
              (index (plist-get anchor :index)))
          (goto-char (drover-list--visible-start
                      (if (and index sessions)
                          (nth (min index (1- (length sessions))) sessions)
                        container)))))
       (t
        (goto-char (point-min))
        (forward-line (1- (plist-get anchor :line)))
        (when (and (eobp) (not (bobp)))
          (forward-line -1))
        (if-let* ((section (magit-section-at))
                  (_ (invisible-p (point))))
            (goto-char (drover-list--visible-start section))
          (drover-list--move-in-line (plist-get anchor :char)))))
      (point))))

;;;; Refreshing

(defun drover-list--render ()
  "Redraw the current list from what backends report now.
Point, in the buffer and in every window showing it, stays on the
same session, and window starts on the same line."
  (let* ((windows (get-buffer-window-list nil nil t))
         (anchor (drover-list--anchor (point)))
         (placements (mapcar (lambda (window)
                               (list window
                                     (drover-list--anchor (window-point window))
                                     (line-number-at-pos (window-start window))))
                             windows))
         (inhibit-read-only t))
    (drover-list--remember-visibility)
    (deactivate-mark)
    (mapc (lambda (entry) (when-let* ((overlay (plist-get entry :overlay))) (delete-overlay overlay)))
          drover-list--caches)
    (setq magit-section-pre-command-section nil
          magit-section-highlight-overlays nil
          magit-section-selection-overlays nil
          magit-section-highlighted-sections nil
          magit-section-focused-sections nil
          drover-list--caches nil
          drover-list--drawn (make-hash-table :test #'equal)
          drover-list--width (drover-list--line-width)
          drover-list--stale nil)
    (drover-list--measure-begin)
    (erase-buffer)
    (save-excursion
      (drover-list--insert))
    ;; Sections exist now; showing the root applies their visibility.
    (magit-section-show magit-root-section)
    (goto-char (drover-list--resolve anchor))
    (pcase-dolist (`(,window ,window-anchor ,start-line) placements)
      (set-window-point window (drover-list--resolve window-anchor))
      (set-window-start window
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point))
                        t))
    (when (eq (window-buffer) (current-buffer))
      (goto-char (window-point)))
    (drover-list--draw-caches)
    (drover-list--update-current)
    (magit-section-update-highlight t)
    (set-buffer-modified-p nil)
    (push (current-buffer) magit-section--refreshed-buffers)
    (drover-list--update-timers)))

(defun drover-list--visible-p (buffer)
  "Return non-nil when a window of a visible or iconified frame shows BUFFER."
  (get-buffer-window buffer 0))

(defun drover-list--buffers ()
  "Return the live list buffers."
  (match-buffers '(derived-mode . drover-list-mode)))

(defun drover-list--shown-buffers ()
  "Return the live list buffers a window shows."
  (seq-filter #'drover-list--visible-p (drover-list--buffers)))

(defun drover-list--update (buffer)
  "Redraw list BUFFER if it is displayed, else mark it stale."
  (with-current-buffer buffer
    (if (drover-list--visible-p buffer)
        (drover-list--render)
      (setq drover-list--stale t))))

(defun drover-list--on-change ()
  "Bring every list up to date with the backends.
For `drover-changed-hook'."
  (mapc #'drover-list--update (drover-list--buffers)))

(defun drover-list--refresh-if-needed ()
  "Redraw the current list if it is stale or was drawn for another width.
Otherwise bring the highlight of the main window's session and the
timers up to date."
  (if (or drover-list--stale (not (eql drover-list--width (drover-list--line-width))))
      (drover-list--render)
    (drover-list--update-current)
    (drover-list--update-timers)))

(defun drover-list--on-window-change (window)
  "Bring the list WINDOW shows up to date, now that WINDOW shows it.
For `window-buffer-change-functions' and `window-size-change-functions',
which run in the list's buffer: the layout follows the width."
  (let ((buffer (window-buffer window)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'drover-list-mode)
          (drover-list--refresh-if-needed))))))

(defun drover-list--on-face-change ()
  "Redraw the list, whose text changed size, or mark it stale if hidden.
For `text-scale-mode-hook' and `buffer-face-mode-hook': lines are laid
out in pixels, measured in the faces of the buffer."
  (when (derived-mode-p 'drover-list-mode)
    (if (drover-list--visible-p (current-buffer))
        (drover-list--render)
      (setq drover-list--stale t))))

(defun drover-list--revert (&rest _)
  "Fetch older sessions again if shown, measure text anew, and redraw.
For `revert-buffer-function'."
  (when drover-show-history
    (drover-history-fetch))
  (setq drover-list--widths nil)
  (drover-list--render))

;;;; Timers

(defvar drover-list--age-timer nil
  "Timer redrawing displayed lists, so that ages stay current.")

(defvar drover-list--cache-timer nil
  "Timer redrawing the prompt caches of displayed lists.")

(defun drover-list--age-tick ()
  "Redraw displayed lists, mark hidden ones stale; stop when none is shown."
  (mapc #'drover-list--update (drover-list--buffers))
  (drover-list--update-timers))

(defun drover-list--cache-tick ()
  "Redraw the prompt caches of displayed lists; stop when none is live."
  (dolist (buffer (drover-list--shown-buffers))
    (with-current-buffer buffer
      (drover-list--draw-caches)))
  (drover-list--update-timers))

(defun drover-list--stop-timer (symbol)
  "Cancel the timer in the variable SYMBOL, and forget it."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun drover-list--update-timers ()
  "Run the timers a displayed list needs, and only those.
Ages need redraws while a list is displayed; prompt caches while a
displayed list shows a live one."
  (let ((shown (drover-list--shown-buffers)))
    (if (and shown drover-list-age-interval)
        (unless (timerp drover-list--age-timer)
          (setq drover-list--age-timer
                (run-at-time drover-list-age-interval drover-list-age-interval
                             #'drover-list--age-tick)))
      (drover-list--stop-timer 'drover-list--age-timer))
    (if (and drover-list-cache-interval
             (seq-some (lambda (buffer) (buffer-local-value 'drover-list--caches buffer)) shown))
        (unless (timerp drover-list--cache-timer)
          (setq drover-list--cache-timer
                (run-at-time drover-list-cache-interval drover-list-cache-interval
                             #'drover-list--cache-tick)))
      (drover-list--stop-timer 'drover-list--cache-timer))))

;;;; Windows

(defun drover-list--side-window (&optional frame)
  "Return the side window showing the list on FRAME, or nil.
FRAME defaults to the selected frame."
  (when-let* ((buffer (get-buffer drover-list-buffer-name)))
    (seq-find (lambda (window) (window-parameter window 'window-side))
              (get-buffer-window-list buffer nil (or frame (selected-frame))))))

(defun drover-list--main-window (&optional frame)
  "Return the window of FRAME where the user works beside the list, or nil.
It is the most recently used window outside side windows not showing
the list, preferring one not dedicated to its buffer.  FRAME defaults
to the selected frame."
  (let (best best-dedicated)
    (dolist (window (window-list (or frame (selected-frame)) 'nomini))
      (unless (or (window-parameter window 'window-side)
                  (with-current-buffer (window-buffer window)
                    (derived-mode-p 'drover-list-mode)))
        (if (window-dedicated-p window)
            (when (or (null best-dedicated)
                      (> (window-use-time window) (window-use-time best-dedicated)))
              (setq best-dedicated window))
          (when (or (null best) (> (window-use-time window) (window-use-time best)))
            (setq best window)))))
    (or best best-dedicated)))

(defun drover-list--target-window ()
  "Return the main window when the current list shows in a side window, else nil.
It is the list's `drover-main-window-function': from a side window,
sessions open in the main window; from an ordinary one, as usual."
  (let ((window (if (eq (window-buffer) (current-buffer))
                    (selected-window)
                  (get-buffer-window nil))))
    (and window (window-parameter window 'window-side)
         (drover-list--main-window (window-frame window)))))

(defvar-local drover-list--current-overlays nil
  "Overlays highlighting the session shown in the main window.")

(defun drover-list--session-of-buffer (buffer)
  "Return the section of the session BUFFER shows, or nil."
  (when (and buffer drover-list--drawn)
    (seq-some (lambda (container)
                (seq-find (lambda (section)
                            (when-let* ((session (gethash (oref section value) drover-list--drawn)))
                              (eq (drover-session-buffer session) buffer)))
                          (drover-list--session-sections container)))
              (drover-list--containers))))

(defun drover-list--update-current ()
  "Highlight, in each window showing the list, the main window's session.
The main window is that of `drover-list--main-window' on the window's
frame; the heading of the session its buffer shows is highlighted."
  (mapc #'delete-overlay drover-list--current-overlays)
  (setq drover-list--current-overlays nil)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when-let* ((main (drover-list--main-window (window-frame window)))
                (section (drover-list--session-of-buffer (window-buffer main))))
      (let ((overlay (make-overlay (oref section start)
                                   (or (oref section content) (oref section end)))))
        (overlay-put overlay 'face 'drover-list-current)
        (overlay-put overlay 'window window)
        (push overlay drover-list--current-overlays)))))

(defun drover-list--windows-changed (&optional _frame)
  "Update the highlight of the main window's session in displayed lists.
For `window-buffer-change-functions' and
`window-selection-change-functions'."
  (dolist (buffer (drover-list--shown-buffers))
    (with-current-buffer buffer
      (drover-list--update-current))))

(defun drover-list--watch ()
  "Follow sessions and windows while a list buffer lives."
  (add-hook 'drover-changed-hook #'drover-list--on-change)
  (add-hook 'window-buffer-change-functions #'drover-list--windows-changed)
  (add-hook 'window-selection-change-functions #'drover-list--windows-changed))

(defun drover-list--on-kill ()
  "Stop following sessions and windows once the last list is killed.
For `kill-buffer-hook'."
  (unless (seq-some (lambda (buffer) (not (eq buffer (current-buffer))))
                    (drover-list--buffers))
    (remove-hook 'drover-changed-hook #'drover-list--on-change)
    (remove-hook 'window-buffer-change-functions #'drover-list--windows-changed)
    (remove-hook 'window-selection-change-functions #'drover-list--windows-changed)
    (drover-list--stop-timer 'drover-list--age-timer)
    (drover-list--stop-timer 'drover-list--cache-timer)))

;;;; Commands

(defun drover-list--goto-first-session ()
  "Move point to the first session shown, if any."
  (when-let* ((container (car (drover-list--containers)))
              (target (or (car (drover-list--session-sections container))
                          (and (not (eq container magit-root-section)) container))))
    (goto-char (drover-list--visible-start target))))

(defun drover-list--buffer ()
  "Return the list buffer, creating it if needed."
  (or (get-buffer drover-list-buffer-name)
      (with-current-buffer (get-buffer-create drover-list-buffer-name)
        (drover-list-mode)
        (current-buffer))))

(defun drover-list--show (window)
  "Bring the list WINDOW shows up to date; on a first showing, go to a session."
  (with-current-buffer (window-buffer window)
    (let ((new (null drover-list--width)))
      (with-selected-window window
        (drover-list--refresh-if-needed)
        (when new
          (drover-list--goto-first-session))))))

;;;###autoload
(defun drover-list ()
  "Show the list of every agent session in an ordinary window, and select it.
It lists the sessions `drover-filter' keeps, grouped as
`drover-group-by' says and sorted as `drover-sort-by' says; see
`drover-list-mode' for the keys.  A list shown in a side window moves
here."
  (interactive)
  (drover-start)
  (let ((buffer (drover-list--buffer)))
    (when-let* ((side (drover-list--side-window)))
      (delete-window side))
    (pop-to-buffer buffer drover-list-display-action)
    (with-current-buffer buffer
      ;; Asked for, so drawn anew whatever its state.
      (setq drover-list--stale t))
    (drover-list--show (selected-window))))

(defun drover-list--display-alist ()
  "Return the action alist displaying the list in a side window."
  `((side . ,drover-list-side-window-side)
    (slot . ,drover-list-side-window-slot)
    (window-width . ,drover-list-side-window-width)
    (preserve-size . (t . nil))
    ;; Strongly dedicated, so that `switch-to-buffer' never replaces it.
    (dedicated . t)
    (window-parameters . ((no-delete-other-windows . t)
                          (no-other-window . t)))))

(defun drover-list--show-side ()
  "Show the list in a side window of the selected frame and return the window.
An ordinary window of the frame showing the list stops showing it."
  (drover-start)
  (let ((buffer (drover-list--buffer)))
    (dolist (window (get-buffer-window-list buffer nil (selected-frame)))
      (unless (window-parameter window 'window-side)
        (quit-window nil window)))
    (let ((window (or (drover-list--side-window)
                      (display-buffer-in-side-window buffer (drover-list--display-alist))
                      (user-error "No room for the drover list"))))
      (drover-list--show window)
      window)))

;;;###autoload
(defun drover-list-side (&optional select)
  "Toggle the list of agent sessions in a side window.
Show it, without selecting it, when the selected frame lacks it, and
hide it otherwise.  With prefix argument SELECT, select it instead,
showing it first if needed; in the side window, select the window where
you work instead, so that the same key moves back and forth.  From the
side window, sessions open in the window where you work."
  (interactive "P")
  (let ((window (drover-list--side-window)))
    (cond (select
           (if (and window (eq window (selected-window)))
               (when-let* ((main (drover-list--main-window)))
                 (select-window main))
             (select-window (drover-list--show-side))))
          (window (delete-window window))
          (t (drover-list--show-side)))))

(defun drover-list-quit ()
  "Hide the list: delete its side window, or quit its ordinary window."
  (interactive nil drover-list-mode)
  (if (window-parameter nil 'window-side)
      (delete-window)
    (quit-window)))

(defun drover-list-display ()
  "Show the session at point in another window, the list staying selected.
From a side window, that is the window where you work."
  (interactive nil drover-list-mode)
  (let ((session (drover-session-at-point)))
    (if-let* ((main (drover-list--target-window)))
        (with-selected-window main
          (drover-open session))
      (save-selected-window
        (drover-open session t)))
    (drover-list--update-current)))

;;;; The mode

(defun drover-list--mode-line-process ()
  "Return how the list lists sessions, for the mode line.
It tells the grouping, the sort when not by urgency, whether subagents
and older sessions show, and whether a filter leaves sessions out."
  (concat (if drover-group-by
              (concat " by " (alist-get drover-group-by drover-grouping-labels))
            " ungrouped")
          (unless (and (eq drover-sort-by 'urgency) (not drover-sort-descending))
            (concat ", " (alist-get drover-sort-by drover-sort-labels)
                    (if drover-sort-descending " ↓" " ↑")))
          (and drover-show-children " +subagents")
          (and drover-show-history " +older")
          (and (drover-filter-active-p) " filtered")))

;; The keys of `drover-session-map' come first, so they win over those
;; of `magit-section-mode-map' and its parent `special-mode-map'.  The
;; only one they share is g, which refreshes either way: `drover-refresh'
;; reverts the buffer, as `revert-buffer' would.  RET on a group heading
;; toggles it, through the group sections' own keymap.
(defvar-keymap drover-list-mode-map
  :doc "Keymap of `drover-list-mode'."
  :parent (make-composed-keymap (list drover-session-map magit-section-mode-map))
  "RET" #'drover-open-at-point
  "C-o" #'drover-list-display
  "q" #'drover-list-quit)

(define-derived-mode drover-list-mode magit-section-mode "Drover"
  "Major mode of the list of agent sessions.
\\<drover-list-mode-map>
The list shows the sessions the filter keeps, each subagent under its
parent, grouped and sorted as the View menu, \\[drover-view], says.
\\[magit-section-toggle] collapses a group or shows the details of a
session, \\[drover-open-at-point] opens the session at point and
\\[drover-list-display] shows it, the list staying selected.

What a line shows depends on the window's width: see
`drover-list-fields'.  In a side window (`drover-list-side'), sessions
open in the window where you work, and the session it shows is
highlighted.  The keys of `drover-session-map' act on the session at
point; \\[drover-toggle-subagents] shows or hides subagents and
\\[drover-refresh] redraws the list.  Text is drawn in the face
`drover-list'.

\\{drover-list-mode-map}"
  :group 'drover-list
  :interactive nil
  (setq-local drover-view-p t)
  (setq-local drover-list--visibility (make-hash-table :test #'equal))
  (setq-local drover-session-at-point-function #'drover-list--session-at-point)
  (setq-local drover-main-window-function #'drover-list--target-window)
  (setq-local revert-buffer-function #'drover-list--revert)
  (setq-local mode-line-process '(:eval (drover-list--mode-line-process)))
  (setq-local cursor-in-non-selected-windows nil)
  ;; Magit's cache keys a session by its group too, and never forgets;
  ;; `drover-list--visibility' replaces it.
  (setq-local magit-section-cache-visibility nil)
  (drover-remap-default-face 'drover-list)
  (add-hook 'magit-section-set-visibility-hook #'drover-list--cached-visibility nil t)
  (add-hook 'window-buffer-change-functions #'drover-list--on-window-change nil t)
  (add-hook 'window-size-change-functions #'drover-list--on-window-change nil t)
  (add-hook 'text-scale-mode-hook #'drover-list--on-face-change nil t)
  (add-hook 'buffer-face-mode-hook #'drover-list--on-face-change nil t)
  (add-hook 'kill-buffer-hook #'drover-list--on-kill nil t)
  (drover-list--watch))

(provide 'drover-list)
;;; drover-list.el ends here
