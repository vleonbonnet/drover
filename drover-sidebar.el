;;; drover-sidebar.el --- Sessions in a narrow side window  -*- lexical-binding: t; -*-

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

;; `drover-sidebar' lists every session in a narrow side window, beside
;; the user's work: one line per session, the sessions needing the user
;; first, with what they wait for on a second line.  Like every drover
;; view, it lists the trees of `drover-view-trees', filtered by
;; `drover-filter' and grouped as `drover-group-by' says: under status
;; headings, the idle group last, under project headings, or without
;; headings.  Headings fold.  `drover-sidebar-select' moves there and
;; back.
;;
;; Subagents follow their parent, indented one level deeper, so that
;; each status glyph sits under the title above it.  Grouped by status,
;; a parent goes in the group of the most urgent status in its tree: a
;; parent whose subagent asks something shows under "Needs you", the
;; subagent below it.  A subagent whose parent is not listed starts
;; with an arrow.
;;
;; The text uses the face `drover-sidebar', variable pitch by default,
;; for listings more compact than in a fixed-width font.  Lines are
;; measured in pixels: titles and summaries end in an ellipsis where
;; they would reach the age, aligned on the right edge by display.
;;
;; The sidebar acts on the "main window": the most recently used window
;; of its frame outside side windows, which is where the user worked
;; last.  It is the sidebar's `drover-main-window-function', so
;; `drover-follow-mode' shows each session there as point reaches it;
;; C-o shows the session at point there once, the sidebar staying
;; selected.  Every other key acting on sessions is drover's own, from
;; `drover-session-map'.
;;
;; The sidebar redraws on `drover-changed-hook' while a window shows
;; it, and once shown again after changes it missed.  When a backend
;; tells which buffer shows a session (`drover-session-buffer'), the
;; session shown in the main window is highlighted.

;;; Code:

(require 'cl-lib)
(require 'mule-util)
(require 'seq)
(require 'subr-x)
(require 'drover)

(defgroup drover-sidebar nil
  "Sessions of coding agents in a side window."
  :group 'drover
  :prefix "drover-sidebar-")

(defcustom drover-sidebar-side 'left
  "Side of the frame showing the sidebar."
  :type '(choice (const left) (const right))
  :group 'drover-sidebar)

(defcustom drover-sidebar-width 36
  "Total width of the sidebar window, in columns."
  :type 'natnum
  :group 'drover-sidebar)

(defcustom drover-sidebar-slot 0
  "Slot of the sidebar among the side windows of `drover-sidebar-side'.
See `display-buffer-in-side-window'."
  :type 'integer
  :group 'drover-sidebar)

(defcustom drover-sidebar-folded-statuses nil
  "Statuses whose group is folded when the sidebar is created.
They apply while `drover-group-by' groups sessions by status."
  :type '(repeat symbol)
  :group 'drover-sidebar)

(defcustom drover-sidebar-age-interval 60
  "Seconds between redraws keeping ages current, or nil for never.
Only a visible sidebar redraws; a hidden one redraws once shown."
  :type '(choice (const :tag "Never" nil) number)
  :group 'drover-sidebar)

(defface drover-sidebar '((t :inherit variable-pitch))
  "Face of the text of the sidebar.
The sidebar remaps the default face to it.  Make it inherit `default'
or `fixed-pitch' instead to list sessions in a fixed-width font."
  :group 'drover-faces)

(defface drover-sidebar-current '((t :inherit highlight :extend t))
  "Face of the session shown in the main window."
  :group 'drover-faces)

(defconst drover-sidebar-buffer-name "*drover sidebar*"
  "Name of the sidebar buffer.")

(defconst drover-sidebar--fold-glyphs '("▾ " . "▸ ")
  "Prefixes of unfolded and folded group headings.")

(defconst drover-sidebar--max-depth 3
  "Deepest nesting level indented further than the one above it.
Deeper subagents line up with that level, so that titles keep room in a
narrow window.")

(defvar-keymap drover-sidebar-heading-map
  :doc "Keys on the group headings of the sidebar.
They take precedence over `drover-sidebar-mode-map' there."
  "RET" #'drover-sidebar-toggle-group)

(defvar drover-sidebar-mode-map)

(defvar-local drover-sidebar--folded nil
  "Groups that are folded, each as (GROUPING . NAME).
GROUPING is the value of `drover-group-by' making the group, and NAME
its status, or its project name or nil.")

(defvar-local drover-sidebar--stale t
  "Non-nil when sessions changed since the sidebar was last drawn.")

(defvar-local drover-sidebar--width nil
  "Width, in pixels, the sidebar was last drawn for.")

(defvar-local drover-sidebar--widths nil
  "Widths of the text of the last drawing, as (CONTEXT . TABLE).
CONTEXT is what widths depend on besides the text: a copy of the face
remapping of the sidebar, which `face-remap-add-relative' changes in
place, and the frame measuring it.  TABLE maps texts, with their
properties, to their widths in pixels.")

(defvar-local drover-sidebar--current-overlays nil
  "Overlays highlighting the session shown in the main window.")

(defvar drover-sidebar--timer nil
  "Timer redrawing the sidebar to keep ages current.")

;;;; Measuring

(define-hash-table-test 'drover-sidebar--text
  #'equal-including-properties #'sxhash-equal-including-properties)

(cl-defstruct (drover-sidebar--layout (:constructor drover-sidebar--layout-create)
                                      (:copier nil))
  "Measures of one drawing of the sidebar, in pixels."
  (window nil :documentation "Window whose fonts measure text, or nil for the selected one.")
  (remapping nil :documentation "Face remapping of the sidebar buffer.")
  (width nil :documentation "Width lines fill without being truncated.")
  (space nil :documentation "Width of a space.")
  (margin nil :documentation "Indentation of the top level, putting glyphs under group labels.")
  (margin-columns nil :documentation "Spaces of text the top level is indented by.")
  (widths nil :documentation "Table of the widths of texts measured for this drawing.")
  (known nil :documentation "Table of the widths of the previous drawing, or nil.")
  (glyphs nil :documentation "Widths of the status glyphs, as an alist (STATUS . WIDTH)."))

(defun drover-sidebar--pixels (layout string)
  "Return the width of STRING in pixels, as the sidebar of LAYOUT shows it.
The faces of STRING count, and so do the face remapping of the sidebar
buffer, such as its variable pitch, and the fonts of the frame of the
window of LAYOUT.  Widths measured for the previous drawing are reused:
laying text out costs more than the rest of a redraw."
  (let ((widths (drover-sidebar--layout-widths layout))
        (known (drover-sidebar--layout-known layout)))
    (or (gethash string widths)
        (puthash string
                 (or (and known (gethash string known))
                     (drover-sidebar--measure layout string))
                 widths))))

(defun drover-sidebar--measure (layout string)
  "Return the width of STRING in pixels, laid out as LAYOUT says."
  (drover-text-pixel-width string (drover-sidebar--layout-remapping layout)
                           (drover-sidebar--layout-window layout)))

(defun drover-sidebar--truncate (layout string pixels)
  "Return STRING cut to fit PIXELS wide, as LAYOUT measures it.
A cut string ends in an ellipsis, in the faces of the text before it;
when not even the ellipsis fits, the result is empty."
  (drover-truncate-to-pixels string pixels
                             (lambda (text) (drover-sidebar--pixels layout text))))

(defun drover-sidebar--window-pixels (window)
  "Return the width, in pixels, lines of WINDOW fill without truncation.
Without WINDOW, return the width of a new sidebar in the selected frame."
  (if (not window)
      (* (max 1 (1- drover-sidebar-width)) (frame-char-width))
    (let ((pixels (window-body-width window t))
          (column (window-font-width window)))
      ;; As `window-max-chars-per-line' says, the last column holds the
      ;; truncation glyph unless the fringes do.
      (if (< (window-max-chars-per-line window) (/ pixels column))
          (- pixels column)
        pixels))))

(defun drover-sidebar--layout (width window &optional flat)
  "Return the measures of the current buffer drawn WIDTH pixels wide.
Text is measured with the fonts of WINDOW, or of the selected window
when WINDOW is nil.  FLAT non-nil means no group headings: the top
level is not indented.  The widths measured become those the next
layout of the buffer may reuse."
  (let* ((context (list (copy-tree face-remapping-alist) (window-frame window)))
         (widths (make-hash-table :test 'drover-sidebar--text))
         (layout (drover-sidebar--layout-create
                  :window window
                  :remapping face-remapping-alist
                  :width width
                  :widths widths
                  :known (and (equal (car drover-sidebar--widths) context)
                              (cdr drover-sidebar--widths))))
         (space (drover-sidebar--pixels layout " "))
         (statuses (seq-uniq (append drover-status-order (mapcar #'car drover-status-glyphs))))
         (glyphs (mapcar (lambda (status)
                           (cons status
                                 (drover-sidebar--pixels layout (drover-status-glyph status))))
                         statuses))
         (fold (drover-face (car drover-sidebar--fold-glyphs) 'drover-heading)))
    (setf (drover-sidebar--layout-space layout) space
          (drover-sidebar--layout-glyphs layout) glyphs
          (drover-sidebar--layout-margin layout) (if flat 0 (drover-sidebar--pixels layout fold))
          (drover-sidebar--layout-margin-columns layout) (if flat 0 2))
    (setq drover-sidebar--widths (cons context widths))
    layout))

(defun drover-sidebar--glyph-width (layout status)
  "Return the width of the glyph of STATUS, in pixels, per LAYOUT."
  (or (alist-get status (drover-sidebar--layout-glyphs layout))
      (drover-sidebar--pixels layout (drover-status-glyph status))))

(defun drover-sidebar--blank (columns pixels)
  "Return COLUMNS spaces shown as one blank PIXELS wide.
The spaces keep the buffer text readable, and the display gives the
width, since spaces of a proportional font are narrow."
  (propertize (make-string columns ?\s) 'display `(space :width (,pixels))))

(defun drover-sidebar--indent-columns (layout depth)
  "Return the spaces of text indenting sessions at DEPTH in trees, per LAYOUT."
  (+ (drover-sidebar--layout-margin-columns layout)
     (* 2 (min depth drover-sidebar--max-depth))))

;;;; Drawing

(defun drover-sidebar--groups (nodes)
  "Return trees NODES grouped as `drover-group-by' says.
The value is a list of (GROUP . NODES).  GROUP is (GROUPING . NAME),
GROUPING being the value of `drover-group-by' and NAME a status, the
idle group last, or a project name or nil, in the order of
`drover-group-trees-by-project'.  Without grouping, GROUP is nil and
holds every tree; without trees, the value is nil."
  (pcase drover-group-by
    ('status
     (let* ((groups (drover-group-trees-by-status nodes))
            (idle (assq 'idle groups)))
       (mapcar (lambda (group) (cons (cons 'status (car group)) (cdr group)))
               (if idle (append (remq idle groups) (list idle)) groups))))
    ('project
     (mapcar (lambda (group) (cons (cons 'project (car group)) (cdr group)))
             (drover-group-trees-by-project nodes)))
    (_ (and nodes (list (cons nil nodes))))))

(defun drover-sidebar--group-label (group)
  "Return the display name of GROUP, a (GROUPING . NAME) pair."
  (pcase group
    (`(status . ,status) (drover-status-label status))
    (`(project . ,project) (if project (drover-sidebar--one-line project) "No project"))))

(defun drover-sidebar--one-line (string)
  "Return STRING with its line breaks and tabs replaced by spaces."
  (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" string)))

(defun drover-sidebar--age-time (session)
  "Return the time from which the age of SESSION counts.
A session needing the user ages from when its status began, the others
from their last activity, matching the order of `drover-sort'."
  (if (drover-session-attention-p session)
      (or (drover-session-since session) (drover-session-updated session))
    (or (drover-session-updated session) (drover-session-since session))))

(defun drover-sidebar--help (session)
  "Return the tooltip of SESSION: its full title and details."
  (string-join
   (delq nil (list (drover-sidebar--one-line (drover-session-display-title session))
                   (drover-session-location session)
                   (when-let* ((agent (drover-session-agent session)))
                     (if-let* ((model (drover-session-model session)))
                         (format "%s (%s)" agent model)
                       agent))
                   (when-let* ((summary (drover-session-summary session)))
                     (drover-sidebar--one-line summary))))
   "\n"))

(defun drover-sidebar--heading-line (group count folded layout)
  "Return the heading of GROUP, of COUNT trees.
GROUP is (GROUPING . NAME), as `drover-sidebar--groups' makes it.
FOLDED non-nil marks the group folded; the line fits the width of
LAYOUT."
  (drover-sidebar--truncate
   layout
   (concat (drover-face (concat (if folded
                                    (cdr drover-sidebar--fold-glyphs)
                                  (car drover-sidebar--fold-glyphs))
                                (drover-sidebar--group-label group))
                        'drover-heading)
           " "
           (drover-face (number-to-string count) 'drover-dim))
   (drover-sidebar--layout-width layout)))

(defun drover-sidebar--title-start (layout session indent)
  "Return where the title of SESSION starts, in pixels, per LAYOUT.
INDENT is the indentation of the line of SESSION, in pixels."
  (+ indent
     (drover-sidebar--glyph-width layout (drover-session-status session))
     (drover-sidebar--layout-space layout)))

(defun drover-sidebar--session-line (session depth indent layout now)
  "Return the line of SESSION, at DEPTH in its tree, with ages as of NOW.
The line holds INDENT pixels of indentation, the status glyph, one
space, the title, truncated, and the age, flush right; LAYOUT gives the
measures.  A subagent at the top level, its parent not listed, has an
arrow before its title."
  (let* ((status (drover-session-status session))
         (width (drover-sidebar--layout-width layout))
         (age (drover-face (drover-format-age (drover-sidebar--age-time session) now) 'drover-dim))
         (age-start (if (string-empty-p age)
                        width
                      (- width (drover-sidebar--pixels layout age))))
         (title (drover-sidebar--one-line (drover-session-display-title session)))
         (title (concat (if (and (zerop depth) (drover-session-parent session))
                            (drover-face "↳ " 'drover-dim)
                          "")
                        (if (eq status 'idle) (drover-face title 'drover-dim) title))))
    (concat (drover-sidebar--blank (drover-sidebar--indent-columns layout depth) indent)
            (drover-status-glyph status)
            " "
            (drover-sidebar--truncate
             layout title
             (- age-start
                (drover-sidebar--title-start layout session indent)
                (if (string-empty-p age) 0 (drover-sidebar--layout-space layout))))
            (unless (string-empty-p age)
              ;; Aligned by display, in pixels, whatever the font draws
              ;; before it.
              (concat (propertize " " 'display `(space :align-to (,age-start)))
                      age)))))

(defun drover-sidebar--summary-line (session depth indent layout)
  "Return the second line of SESSION, at DEPTH in its tree, or nil.
Only a session needing the user and saying why has one.  It starts
under the title of the session's line, indented INDENT pixels, and fits
the width of LAYOUT."
  (when-let* (((drover-session-attention-p session))
              (summary (drover-session-summary session)))
    (let ((start (drover-sidebar--title-start layout session indent)))
      (concat (drover-sidebar--blank (+ 2 (drover-sidebar--indent-columns layout depth)) start)
              (drover-sidebar--truncate
               layout (drover-face (drover-sidebar--one-line summary) 'drover-dim)
               (- (drover-sidebar--layout-width layout) start))))))

(defun drover-sidebar--insert-line (string &rest properties)
  "Insert STRING and a newline, both with text PROPERTIES."
  (let ((start (point)))
    (insert string "\n")
    (add-text-properties start (point) properties)))

(defun drover-sidebar--insert-session (session depth indent path layout now)
  "Insert the lines of SESSION, at DEPTH in its tree, with ages as of NOW.
Its line is indented INDENT pixels.  PATH lists the keys of SESSION and
of the sessions above it, nearest first.  LAYOUT gives the measures."
  (let ((start (point)))
    (drover-sidebar--insert-line (drover-sidebar--session-line session depth indent layout now)
                                 'drover-sidebar-line 'session
                                 'mouse-face 'highlight
                                 'follow-link t
                                 'help-echo (drover-sidebar--help session))
    ;; The newline would stretch the mouse highlight over the window's width.
    (remove-text-properties (1- (point)) (point) '(mouse-face nil))
    (when-let* ((summary (drover-sidebar--summary-line session depth indent layout)))
      (drover-sidebar--insert-line summary 'drover-sidebar-line 'summary))
    ;; One key object for every line of the session, so that
    ;; `next-single-property-change' finds where the session ends.
    (add-text-properties start (point) (list 'drover-sidebar-key (car path)
                                             'drover-sidebar-path path
                                             'drover-sidebar-session session))))

(defun drover-sidebar--insert-tree (node depth indent path layout now)
  "Insert the lines of tree NODE, at DEPTH, with ages as of NOW.
The line of its session is indented INDENT pixels.  PATH lists the keys
of the sessions above NODE, nearest first.  LAYOUT gives the measures.
Subagents start where the title of their parent does, so that each
glyph sits under the title above it."
  (let* ((session (drover-node-session node))
         (path (cons (drover-session-key session) path))
         (children-indent (if (< depth drover-sidebar--max-depth)
                              (drover-sidebar--title-start layout session indent)
                            indent)))
    (drover-sidebar--insert-session session depth indent path layout now)
    (dolist (child (drover-node-children node))
      (drover-sidebar--insert-tree child (1+ depth) children-indent path layout now))))

(defun drover-sidebar--insert-note (text type layout)
  "Insert a line of TYPE saying TEXT, dimmed, fitting the width of LAYOUT."
  (drover-sidebar--insert-line
   (drover-sidebar--truncate layout (drover-face text 'drover-dim)
                             (drover-sidebar--layout-width layout))
   'drover-sidebar-line type))

(defun drover-sidebar--insert-filter (description layout)
  "Insert the line telling the filter in force, DESCRIPTION, per LAYOUT."
  (drover-sidebar--insert-line
   (drover-sidebar--truncate layout
                             (concat (drover-face "Filter: " 'drover-dim) description)
                             (drover-sidebar--layout-width layout))
   'drover-sidebar-line 'filter
   'help-echo (format "Filter: %s\n%s" description (drover-filter-clear-hint drover-sidebar-mode-map))))

(defun drover-sidebar--heading-map-properties ()
  "Return the text properties giving a heading line its keys.
They make the keymap hold from the start of the line to its end, and
not past it."
  (list 'keymap drover-sidebar-heading-map
        'front-sticky '(keymap)
        'rear-nonsticky '(keymap)))

(defun drover-sidebar--insert-group (group members layout now)
  "Insert GROUP of trees MEMBERS, with ages as of NOW, per LAYOUT.
GROUP is (GROUPING . NAME) under a heading, or nil for trees without
one.  Every line of the group tells GROUP."
  (let ((start (point))
        (folded (and group (member group drover-sidebar--folded))))
    (when group
      (apply #'drover-sidebar--insert-line
             (drover-sidebar--heading-line group (length members) folded layout)
             'drover-sidebar-line 'heading
             'mouse-face 'highlight
             'follow-link t
             'help-echo (if folded "Show this group" "Fold this group")
             (drover-sidebar--heading-map-properties))
      (remove-text-properties (1- (point)) (point) '(mouse-face nil)))
    (unless folded
      (dolist (node members)
        (drover-sidebar--insert-tree node 0 (drover-sidebar--layout-margin layout)
                                     nil layout now)))
    (when group
      (put-text-property start (point) 'drover-sidebar-group group))))

(defun drover-sidebar--render (width &optional now window)
  "Draw the sessions of `drover-view-trees' in the current buffer.
Lines are WIDTH pixels wide; ages count up to NOW, a float time
defaulting to the current time.  Text is measured with the fonts of
WINDOW, or of the selected window.  Point lands at the start of the
buffer."
  (let* ((inhibit-read-only t)
         (now (or now (float-time)))
         (sessions (drover-sessions))
         (trees (drover-view-trees sessions))
         (hidden (drover-view-hidden-count sessions))
         (groups (drover-sidebar--groups trees))
         (filter (drover-filter-description nil now))
         (headings (and (car (car groups)) t))
         (layout (drover-sidebar--layout width window (not headings))))
    (erase-buffer)
    (setq drover-sidebar--width width)
    (when filter
      (drover-sidebar--insert-filter filter layout))
    (cond
     (groups
      (pcase-dolist (`(,group . ,members) groups)
        (drover-sidebar--insert-group group members layout now)))
     ((and filter sessions)
      (drover-sidebar--insert-note "No session matches." 'empty layout)
      (drover-sidebar--insert-note (drover-filter-clear-hint drover-sidebar-mode-map)
                                   'hint layout))
     (t (drover-sidebar--insert-note "No sessions." 'empty layout)))
    ;; Every tree shows its root, so hidden subagents never leave the
    ;; list empty: the hint at them closes the list instead.
    (when (> hidden 0)
      (drover-sidebar--insert-note
       (substitute-command-keys
        (format "\\<drover-sidebar-mode-map>\\[drover-toggle-subagents] shows %d subagent%s."
                hidden (if (= hidden 1) "" "s"))
        t)
       'hint layout))
    (goto-char (point-min))
    (drover-sidebar--update-current)))

;;;; Lines

(defun drover-sidebar--line-type (&optional position)
  "Return the kind of the line at POSITION or point.
It is `filter', `heading', `session', `summary', `empty', `hint', or nil
past the end."
  (get-text-property (save-excursion
                       (when position (goto-char position))
                       (line-beginning-position))
                     'drover-sidebar-line))

(defun drover-sidebar--search (count types)
  "Return the start of the COUNTth line of TYPES after the line at point.
A negative COUNT searches before.  When fewer lines match, return the
last that does, or nil when none does."
  (save-excursion
    (let ((step (if (< count 0) -1 1))
          (left (abs count))
          found)
      (beginning-of-line)
      (while (and (> left 0) (zerop (forward-line step)) (not (eobp)))
        (when (memq (drover-sidebar--line-type) types)
          (setq left (1- left)
                found (point))))
      found)))

(defun drover-sidebar--find-line (type predicate)
  "Return the start of the first line of TYPE satisfying PREDICATE, or nil.
PREDICATE is called with the start of the line."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (while (not (eobp))
        (when (and (eq (drover-sidebar--line-type) type)
                   (funcall predicate (point)))
          (throw 'found (point)))
        (forward-line 1))
      nil)))

(defun drover-sidebar--entry-start (position)
  "Return the start of the session line of the entry at POSITION, or nil."
  (save-excursion
    (goto-char position)
    (beginning-of-line)
    (while (and (eq (drover-sidebar--line-type) 'summary) (zerop (forward-line -1))))
    (and (eq (drover-sidebar--line-type) 'session) (point))))

(defun drover-sidebar--anchor (position)
  "Return what the line at POSITION shows, to find it again after a redraw.
The value is (TYPE ID LINE): TYPE is `session', with ID the keys of the
session and of the sessions above it in its tree, nearest first, or
`heading', with ID its group, or nil; LINE is the line number."
  (when (> (buffer-size) 0)
    (save-excursion
      (goto-char position)
      (let ((line (line-number-at-pos nil t))
            (path (get-text-property (line-beginning-position) 'drover-sidebar-path)))
        (cond
         (path (list 'session path line))
         ((eq (drover-sidebar--line-type) 'heading)
          (list 'heading (get-text-property (line-beginning-position) 'drover-sidebar-group) line))
         (t (list nil nil line)))))))

(defun drover-sidebar--first-session ()
  "Return the start of the first session line, or nil."
  (drover-sidebar--find-line 'session #'always))

(defun drover-sidebar--line-position (line)
  "Return the start of LINE, clamped to the last line of text.
On the hint closing the list, return the last session or heading."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (when (and (eobp) (not (bobp)))
      (forward-line -1))
    (or (drover-sidebar--entry-start (point))
        (and (eq (drover-sidebar--line-type) 'hint)
             (drover-sidebar--search -1 '(session heading)))
        (point))))

(defun drover-sidebar--anchor-position (anchor)
  "Return the position of what ANCHOR, from `drover-sidebar--anchor', shows.
A session no longer shown falls back to the nearest session above it in
its tree, such as the parent of a subagent now hidden; a session without
one, or a heading, to the same line.  No ANCHOR falls back to the first
session."
  (pcase-let ((`(,type ,id ,line) anchor))
    (or (pcase type
          ('session (seq-some
                     (lambda (key)
                       (drover-sidebar--find-line
                        'session (lambda (start)
                                   (equal (get-text-property start 'drover-sidebar-key) key))))
                     id))
          ('heading (drover-sidebar--find-line
                     'heading (lambda (start)
                                (equal (get-text-property start 'drover-sidebar-group) id)))))
        (and line (drover-sidebar--line-position line))
        (drover-sidebar--first-session)
        (point-min))))

;;;; Windows

(defun drover-sidebar--window (&optional frame)
  "Return the window showing the sidebar on FRAME, or nil.
FRAME defaults to the selected frame."
  (when-let* ((buffer (get-buffer drover-sidebar-buffer-name)))
    (get-buffer-window buffer (or frame (selected-frame)))))

(defun drover-sidebar--main-window (&optional frame)
  "Return the window of FRAME the sidebar shows sessions in, or nil.
It is the most recently used window outside side windows, preferring
one not dedicated to its buffer.  FRAME defaults to the selected frame."
  (let (best best-dedicated)
    (dolist (window (window-list (or frame (selected-frame)) 'nomini))
      (unless (or (window-parameter window 'window-side)
                  (equal (buffer-name (window-buffer window)) drover-sidebar-buffer-name))
        (if (window-dedicated-p window)
            (when (or (null best-dedicated)
                      (> (window-use-time window) (window-use-time best-dedicated)))
              (setq best-dedicated window))
          (when (or (null best) (> (window-use-time window) (window-use-time best)))
            (setq best window)))))
    (or best best-dedicated)))

(defun drover-sidebar--display-alist ()
  "Return the action alist displaying the sidebar."
  `((side . ,drover-sidebar-side)
    (slot . ,drover-sidebar-slot)
    (window-width . ,drover-sidebar-width)
    (preserve-size . (t . nil))
    ;; Strongly dedicated, so that `switch-to-buffer' never replaces it.
    (dedicated . t)
    (window-parameters . ((no-delete-other-windows . t)
                          (no-other-window . t)))))

(defun drover-sidebar--drawing-window (windows)
  "Return the window of WINDOWS, showing the sidebar, to draw it for, or nil.
The window on the selected frame wins."
  (or (seq-find (lambda (window) (eq (window-frame window) (selected-frame)))
                windows)
      (car windows)))

(defun drover-sidebar--entry-of-buffer (buffer)
  "Return the bounds (START . END) of the session shown by BUFFER, or nil."
  (when-let* ((start (drover-sidebar--find-line
                      'session (lambda (start)
                                 (eq (drover-session-buffer
                                      (get-text-property start 'drover-sidebar-session))
                                     buffer)))))
    (cons start (next-single-property-change start 'drover-sidebar-key nil (point-max)))))

(defun drover-sidebar--update-current ()
  "Highlight, in each window showing the sidebar, the main window's session."
  (mapc #'delete-overlay drover-sidebar--current-overlays)
  (setq drover-sidebar--current-overlays nil)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when-let* ((main (drover-sidebar--main-window (window-frame window)))
                (bounds (drover-sidebar--entry-of-buffer (window-buffer main))))
      (let ((overlay (make-overlay (car bounds) (cdr bounds))))
        (overlay-put overlay 'face 'drover-sidebar-current)
        (overlay-put overlay 'window window)
        (push overlay drover-sidebar--current-overlays)))))

;;;; Redrawing

(defun drover-sidebar--redraw (&optional anchor)
  "Redraw the sidebar in the current buffer, keeping every view of it.
Point and each window's point stay on their session, or on ANCHOR,
and window starts stay on the same line."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (here (or anchor (drover-sidebar--anchor (point))))
         (views (mapcar (lambda (window)
                          (list window
                                (or anchor (drover-sidebar--anchor (window-point window)))
                                (line-number-at-pos (window-start window) t)))
                        windows))
         (window (drover-sidebar--drawing-window windows)))
    (drover-sidebar--render (drover-sidebar--window-pixels window) nil window)
    (goto-char (drover-sidebar--anchor-position here))
    (pcase-dolist (`(,window ,window-anchor ,start-line) views)
      (set-window-point window (drover-sidebar--anchor-position window-anchor))
      ;; Not forced, so that redisplay scrolls if point left the view.
      (set-window-start window
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (line-beginning-position))
                        t))
    (setq drover-sidebar--stale nil)))

(defun drover-sidebar--changed ()
  "Redraw the sidebar if a window shows it, else mark it stale."
  (when-let* ((buffer (get-buffer drover-sidebar-buffer-name)))
    (with-current-buffer buffer
      (if (get-buffer-window buffer t)
          (drover-sidebar--redraw)
        (setq drover-sidebar--stale t)))))

(defun drover-sidebar--size-changed (window)
  "Redraw the sidebar when WINDOW, showing it, changed width."
  (when (and (window-live-p window)
             (not (eql (drover-sidebar--window-pixels window) drover-sidebar--width)))
    (with-current-buffer (window-buffer window)
      (drover-sidebar--redraw))))

(defun drover-sidebar--text-scaled ()
  "Redraw the sidebar, whose text changed size, or mark it stale if hidden."
  (if (get-buffer-window (current-buffer) t)
      (drover-sidebar--redraw)
    (setq drover-sidebar--stale t)))

(defun drover-sidebar--windows-changed (&optional _frame)
  "Update the highlight of the main window's session."
  (when-let* ((buffer (get-buffer drover-sidebar-buffer-name))
              ((get-buffer-window buffer t)))
    (with-current-buffer buffer
      (drover-sidebar--update-current))))

(defun drover-sidebar--watch ()
  "Follow session and window changes while the sidebar buffer lives."
  (add-hook 'drover-changed-hook #'drover-sidebar--changed)
  (add-hook 'window-buffer-change-functions #'drover-sidebar--windows-changed)
  (add-hook 'window-selection-change-functions #'drover-sidebar--windows-changed)
  (when (and drover-sidebar-age-interval (not (timerp drover-sidebar--timer)))
    (setq drover-sidebar--timer (run-at-time drover-sidebar-age-interval
                                             drover-sidebar-age-interval
                                             #'drover-sidebar--changed))))

(defun drover-sidebar--unwatch ()
  "Stop following changes, as the sidebar buffer is killed."
  (when (equal (buffer-name) drover-sidebar-buffer-name)
    (remove-hook 'drover-changed-hook #'drover-sidebar--changed)
    (remove-hook 'window-buffer-change-functions #'drover-sidebar--windows-changed)
    (remove-hook 'window-selection-change-functions #'drover-sidebar--windows-changed)
    (when (timerp drover-sidebar--timer)
      (cancel-timer drover-sidebar--timer))
    (setq drover-sidebar--timer nil)))

;;;; Showing and hiding

(defun drover-sidebar--buffer ()
  "Return the sidebar buffer, creating it if needed."
  (or (get-buffer drover-sidebar-buffer-name)
      (with-current-buffer (get-buffer-create drover-sidebar-buffer-name)
        (drover-sidebar-mode)
        (current-buffer))))

(defun drover-sidebar--show ()
  "Show the sidebar on the selected frame and return its window.
Prepare the backends, and redraw if sessions changed while hidden."
  (drover-start)
  (let* ((buffer (drover-sidebar--buffer))
         (window (or (drover-sidebar--window)
                     (display-buffer-in-side-window buffer (drover-sidebar--display-alist))
                     (user-error "No room for the drover sidebar"))))
    (with-current-buffer buffer
      (drover-sidebar--watch)
      (if drover-sidebar--stale
          (drover-sidebar--redraw)
        (drover-sidebar--update-current)))
    window))

(defun drover-sidebar--hide ()
  "Hide the sidebar on the selected frame."
  (when-let* ((window (drover-sidebar--window)))
    (if (window-parameter window 'window-side)
        (delete-window window)
      (quit-window nil window))))

;;;###autoload
(defun drover-sidebar (&optional select)
  "Toggle the sidebar listing the sessions of coding agents.
Show it, without selecting it, when the selected frame lacks it, and
hide it otherwise.  With prefix argument SELECT, select it instead,
showing it first if needed, as `drover-sidebar-select' does."
  (interactive "P")
  (cond (select (drover-sidebar-select))
        ((drover-sidebar--window) (drover-sidebar--hide))
        (t (drover-sidebar--show))))

;;;###autoload
(defun drover-sidebar-select ()
  "Select the sidebar, showing it first if needed.
In the sidebar, select the main window instead, so that one key moves
back and forth."
  (interactive)
  (let ((window (drover-sidebar--window)))
    (if (and window (eq window (selected-window)))
        (when-let* ((main (drover-sidebar--main-window)))
          (select-window main))
      (select-window (drover-sidebar--show)))))

(defun drover-sidebar-hide ()
  "Hide the sidebar."
  (interactive)
  (drover-sidebar--hide))

(defun drover-sidebar-refresh ()
  "Redraw the sidebar from what the backends report.
Text is measured anew, as after changing the face `drover-sidebar'."
  (interactive)
  (when-let* ((buffer (get-buffer drover-sidebar-buffer-name)))
    (with-current-buffer buffer
      (setq drover-sidebar--widths nil)
      (drover-sidebar--redraw))))

;;;; Commands

(defun drover-sidebar--session-at-point ()
  "Return the current record of the session at point, or nil."
  (when-let* ((key (get-text-property (line-beginning-position) 'drover-sidebar-key)))
    (drover-find (car key) (cdr key))))

(defun drover-sidebar--display (session)
  "Show SESSION in the main window, keeping the selected window."
  (if-let* ((main (drover-sidebar--main-window)))
      (with-selected-window main
        (drover-open session))
    (save-selected-window
      (drover-open session t)))
  (drover-sidebar--update-current))

(defun drover-sidebar--move (count types)
  "Move to the COUNTth line of TYPES after point, before when negative.
Return non-nil when point moved."
  (if-let* ((position (drover-sidebar--search count types)))
      (progn (goto-char position) t)
    (message "No %s %s" (if (< count 0) "previous" "next")
             (if (memq 'heading types) "group" "session"))
    nil))

(defun drover-sidebar-next (&optional count)
  "Move to the next session, or COUNT sessions forward."
  (interactive "p" drover-sidebar-mode)
  (drover-sidebar--move (or count 1) '(session)))

(defun drover-sidebar-previous (&optional count)
  "Move to the previous session, or COUNT sessions back."
  (interactive "p" drover-sidebar-mode)
  (drover-sidebar--move (- (or count 1)) '(session)))

(defun drover-sidebar-next-group (&optional count)
  "Move to the next group heading, or COUNT headings forward."
  (interactive "p" drover-sidebar-mode)
  (drover-sidebar--move (or count 1) '(heading)))

(defun drover-sidebar-previous-group (&optional count)
  "Move to the previous group heading, or COUNT headings back."
  (interactive "p" drover-sidebar-mode)
  (drover-sidebar--move (- (or count 1)) '(heading)))

(defun drover-sidebar-toggle-group ()
  "Fold or unfold the group at point.
Point moves to the group's heading.  The fold lasts while sessions are
grouped the same way, and comes back with that grouping."
  (interactive nil drover-sidebar-mode)
  (let ((group (get-text-property (line-beginning-position) 'drover-sidebar-group)))
    (unless group
      (user-error "No group at point"))
    (setq drover-sidebar--folded (if (member group drover-sidebar--folded)
                                     (remove group drover-sidebar--folded)
                                   (cons group drover-sidebar--folded)))
    (drover-sidebar--redraw (list 'heading group nil))))

(defun drover-sidebar-toggle-all-groups ()
  "Fold every group shown, or unfold them all when all are folded."
  (interactive nil drover-sidebar-mode)
  (let* ((shown (mapcar (lambda (start) (get-text-property start 'drover-sidebar-group))
                        (drover-sidebar--heading-starts)))
         (all-folded (seq-every-p (lambda (group) (member group drover-sidebar--folded)) shown)))
    (unless shown
      (user-error "No groups to fold"))
    (setq drover-sidebar--folded (if all-folded
                                     (seq-difference drover-sidebar--folded shown)
                                   (seq-union drover-sidebar--folded shown)))
    (drover-sidebar--redraw)))

(defun drover-sidebar--heading-starts ()
  "Return the starts of the heading lines."
  (save-excursion
    (goto-char (point-min))
    (let (starts)
      (while (not (eobp))
        (when (eq (drover-sidebar--line-type) 'heading)
          (push (point) starts))
        (forward-line 1))
      (nreverse starts))))

(defun drover-sidebar-display ()
  "Show the session at point in the main window, staying in the sidebar."
  (interactive nil drover-sidebar-mode)
  (drover-sidebar--display (drover-session-at-point)))

(defun drover-sidebar-mouse-open (event)
  "Open the session clicked in EVENT, or fold or unfold the group clicked.
A session opens as \\<drover-sidebar-mode-map>\\[drover-open-at-point] opens it."
  (interactive "e" drover-sidebar-mode)
  (let ((position (event-end event)))
    (select-window (posn-window position))
    (goto-char (posn-point position))
    (if (eq (drover-sidebar--line-type) 'heading)
        (drover-sidebar-toggle-group)
      (call-interactively #'drover-open-at-point))))

;;;; Mode

(defun drover-sidebar--mode-line-flags ()
  "Return the mode line flags of the settings the sidebar follows."
  (concat (and drover-show-children " +sub")
          (and drover-follow-mode " Follow")))

(defvar-keymap drover-sidebar-mode-map
  :doc "Keymap of `drover-sidebar-mode'.
Its parent combines `drover-session-map', whose keys act on the session
at point, and `special-mode-map'.  On a group heading,
`drover-sidebar-heading-map' takes precedence."
  :parent (make-composed-keymap drover-session-map special-mode-map)
  "RET" #'drover-open-at-point
  "C-o" #'drover-sidebar-display
  "n" #'drover-sidebar-next
  "p" #'drover-sidebar-previous
  "M-n" #'drover-sidebar-next-group
  "M-p" #'drover-sidebar-previous-group
  "TAB" #'drover-sidebar-toggle-group
  "<backtab>" #'drover-sidebar-toggle-all-groups
  "q" #'drover-sidebar-hide
  "<mouse-2>" #'drover-sidebar-mouse-open)

(define-derived-mode drover-sidebar-mode special-mode "Drover"
  "Major mode of the drover sidebar, listing sessions of coding agents.
Like every drover view, it lists the sessions `drover-filter' keeps,
grouped as `drover-group-by' says.\\<drover-sidebar-mode-map>
\\[drover-group] groups and \\[drover-filter] filters sessions in every
view, and \\[drover-toggle-subagents] shows or hides subagents.
\\[drover-sidebar-toggle-group] folds the group at point, as \\`RET'
does on its heading.

The keys of `drover-session-map' act on the session at point.  The
sidebar shows sessions in the main window, where the user works:
\\[drover-sidebar-display] shows the session at point there, the
sidebar staying selected, and \\[drover-follow-mode] shows each session
there as point reaches it.

\\{drover-sidebar-mode-map}"
  :interactive nil
  (setq-local drover-view-p t
              drover-session-at-point-function #'drover-sidebar--session-at-point
              drover-main-window-function #'drover-sidebar--main-window
              revert-buffer-function (lambda (&rest _)
                                       (setq drover-sidebar--widths nil)
                                       (drover-sidebar--redraw))
              drover-sidebar--folded (mapcar (lambda (status) (cons 'status status))
                                             drover-sidebar-folded-statuses)
              truncate-lines t
              buffer-undo-list t
              cursor-in-non-selected-windows nil
              mode-line-format '(" " (:propertize "Drover" face mode-line-buffer-id)
                                 (:eval (drover-sidebar--mode-line-flags))))
  (add-hook 'window-size-change-functions #'drover-sidebar--size-changed nil t)
  (add-hook 'text-scale-mode-hook #'drover-sidebar--text-scaled nil t)
  (add-hook 'kill-buffer-hook #'drover-sidebar--unwatch nil t)
  (drover-remap-default-face 'drover-sidebar))

(provide 'drover-sidebar)
;;; drover-sidebar.el ends here
