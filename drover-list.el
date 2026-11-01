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
;; whose body recaps its last turn: what it waits for, the error that
;; ended it, the prompt, the agent's reply and the files it changed, as
;; its backend tells them (`drover-recap'), fetched as it expands, and
;; what its line leaves out.  The keys of `drover-session-map' act on
;; the session at point.  The buffer shows sessions and their counts;
;; settings and keys are the View menu's to show.
;;
;; A subagent is listed under its parent, its glyph under its parent's
;; title, and a parent and its subagents stay together: they go in the
;; group of the most urgent session among them.  Subagents are sections
;; of the group, like their parent, rather than sections inside their
;; parent's: Magit hides the whole body of a section, and showing a
;; parent's details must neither hide nor reveal its subagents.  Which
;; subagents show, all, the active ones or none, `drover-show-subagents'
;; says; whether shown or not, a session whose subagents work or wait
;; for you tells so beside its status (`drover-subagent-glyphs').  The
;; title of a session open in a buffer is in `drover-open-session'.
;;
;; Working glyphs are animated (`drover-animations'): a timer changes
;; the display of their characters alone, every frame as wide as the
;; widest, while a displayed list shows one.  Every glyph takes the room
;; its font draws it in, which an icon font may draw wider than the
;; advance it gives it, so that no text is drawn on an icon.
;;
;; What a line shows depends on the width of the window, measured in
;; pixels in the face `drover-list', variable pitch by default.
;; `drover-list-fields' names the fields a line may show, by priority,
;; with the room each needs: the widest windows show every field as
;; aligned columns, narrower ones drop the fields of least priority,
;; and the narrowest, such as a side window, show two lines per session:
;; status, title and age, and what the session waits for under them.
;; The layout is computed anew whenever the width changes, among the
;; fields the user did not hide (`drover-list-hidden-fields').  Columns
;; are named in the header line, aligned to them.
;;
;; Besides the session's own fields, a line may show what it runs: its
;; model and effort, its provider, and its context, as a short bar of
;; how much of the model's context it fills and its size.  The context
;; costs a request per session to some backends: the list asks for it
;; (`drover-prefetch') only for the sessions in sight.
;;
;; A session whose provider still holds its prompt cache shows how much
;; of the cache's lifetime is left: as a short bar and the time left in
;; columns, as the background of its line, filled from the left, in two
;; lines.  Only the bar and the time left are redrawn as the cache runs
;; out.
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

(defface drover-list-header '((t :inherit drover-list))
  "Face of the names of the columns, in the header line of the session list.
It goes over the face `header-line'; inheriting `drover-list', it draws
the names in the font of the columns they name."
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

(defcustom drover-list-context-bar-width 5
  "Width of the short bar of the context field, in characters.
A character counts as wide as the letter x in the face `drover-list'."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-animation-interval 0.15
  "Seconds between two frames of the animated glyphs of a displayed list.
Working sessions and those waiting to retry show the frames of
`drover-animations' in turn.  A value of nil shows still glyphs, those
of `drover-status-glyphs'.

A frame changes only the display of the animated characters, as the
prompt cache bars do, so it costs a redisplay of their lines in the
windows showing the list.  The default turns the eight phases of the
moon in 1.2 seconds, quick enough to read as busy rather than as a slow
blink; a slower pace costs fewer redisplays.  The timer runs only while
a displayed list shows an animated glyph."
  :type '(choice (const :tag "Still glyphs" nil) (number :tag "Seconds"))
  :group 'drover-list)

(defcustom drover-list-fields
  '((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
    (location :priority 7 :min 10 :max 30 :label "Project")
    (agent :priority 8 :min 5 :max 12 :label "Agent")
    (model :priority 4 :min 8 :max 22 :label "Model")
    (provider :priority 6 :min 2 :max 12 :label "Provider")
    (context :priority 5 :min 9 :label "Context")
    (created :priority 9 :min 3 :align right :label "Created")
    (age :priority 1 :min 3 :align right :narrow line :label "Age")
    (cache :priority 2 :min 9 :narrow background :label "Cache")
    (summary :priority 3 :min 12 :narrow below :label "Summary"))
  "Fields a line of the session list may show, in the order of columns.
Each entry is (FIELD . PROPERTIES).  FIELD is one of:

 title     The status icon and the title; it is always shown.
 location  The project and branch, as \"project:branch\".
 agent     The agent.
 model     The model, compact, and the effort it runs, dimmed when it
           is the model's default.
 provider  The provider serving the model, often as an icon.
 context   The size of the context, and a short bar of how much of the
           model's context it fills, in a warning face near a
           long-context price tier.
 created   How long ago the session was created.
 age       How long ago it last did something, or since when it needs
           you.
 cache     How much of the lifetime of its prompt cache is left: a
           short bar and the time left.
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
 :label     The name of the field's column in the header line.

Characters count as wide as the letter x in the face `drover-list'.
The list shows the fields with something to show, by priority, as long
as their :min fit in the window, but those of
`drover-list-hidden-fields'; it uses columns when those include every
field the narrow layout places, else the narrow layout, which shows all
of those.  Columns are named in the header line, and are at least as
wide as their name, room allowing."
  :type '(repeat (cons symbol plist))
  :group 'drover-list)

(defcustom drover-list-hidden-fields nil
  "Fields of `drover-list-fields' the list never shows.
The list chooses among the other fields by the room they take; the
title always shows.  The View menu, \\<drover-session-map>\\[drover-view] in the list,
changes it for the session, with one switch per field."
  :type '(set (const :tag "Project and branch" location)
              (const :tag "Agent" agent)
              (const :tag "Model and effort" model)
              (const :tag "Provider" provider)
              (const :tag "Context" context)
              (const :tag "Creation" created)
              (const :tag "Age" age)
              (const :tag "Prompt cache" cache)
              (const :tag "Summary" summary))
  :group 'drover-list)

(defconst drover-list-field-descriptions
  '((title . "The status of the session, the subagents at work under it, and its title")
    (location . "The project and branch the session works in")
    (agent . "The agent the session runs")
    (model . "The model the session runs, and its effort: dimmed when the model's default")
    (provider . "The provider serving the model")
    (context . "The size of the session's context, and how much of the model's it fills")
    (created . "How long ago the session was created")
    (age . "How long ago the session last did something, or since when it needs you")
    (cache . "How long the provider keeps the conversation cached: the next turn sent before then costs less")
    (summary . "What the session waits for, or how its turn stands"))
  "What each field of `drover-list-fields' shows.
The header line's tooltips tell it.")

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
properties, to their widths in pixels for the current drawing, and
conses (extent . TEXT) to their extents (see `drover-list--extent');
KNOWN is the table of the previous drawing in the same context, or nil.")

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

(defun drover-list--extent (string)
  "Return how far STRING reaches drawn in the current buffer, as (LEFT . RIGHT).
See `drover-text-ink-extent': RIGHT is the further of its advance and
its ink, LEFT how far left of its start its ink reaches, in the fonts
of `drover-list--window' with the face remapping of the buffer.
Extents are kept as widths are, in `drover-list--widths'."
  (if (zerop (length string))
      (cons 0 0)
    (unless drover-list--widths
      (drover-list--measure-begin))
    (pcase-let ((`(,_ ,table ,known) drover-list--widths)
                (key (cons 'extent string)))
      (or (gethash key table)
          (puthash key
                   (or (and known (gethash key known))
                       (drover-text-ink-extent string face-remapping-alist (drover-list--window)))
                   table)))))

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

(defconst drover-list--line-start (propertize " " 'invisible t)
  "The character each line of a session's section begins with.
Magit hides the body of a collapsed section under an invisible overlay,
yet Emacs still draws a `display' property found where invisible text
begins: a body beginning with an aligned space, as every line of the
list but group headings would, drew that space on the next visible
line, which then began where the body would have.  An invisible
character without a display property begins every line of a session
instead, so that a hidden body, or a hidden group of sessions, draws
nothing.")

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

;;;; Glyphs

;; A glyph takes the room its font draws it in, its cell, which may be
;; wider than its advance: icon fonts draw icons past their advance
;; (see `drover-text-ink-extent'), and text after a glyph must not be
;; drawn under its ink.

(defun drover-list--pad (pixels)
  "Return a space PIXELS wide, or \"\" when PIXELS is not positive."
  (if (> pixels 0)
      (propertize " " 'display `(space :width (,pixels)))
    ""))

(defun drover-list--cell (strings)
  "Return the cell in which each of STRINGS may be drawn, as (LEFT . RIGHT).
A string drawn in the cell begins LEFT pixels into it, and the cell ends
RIGHT pixels past that: the ink of none of STRINGS leaves it, as
`drover-list--extent' measures it."
  (let ((left 0)
        (right 0))
    (dolist (string strings)
      (pcase-let ((`(,ink-left . ,ink-right) (drover-list--extent string)))
        (setq left (max left ink-left)
              right (max right ink-right))))
    (cons left right)))

(defun drover-list--in-cell (string cell)
  "Return STRING drawn in CELL, a value of `drover-list--cell'.
Spaces as wide as needed come before and after it."
  (concat (drover-list--pad (car cell))
          string
          (drover-list--pad (- (cdr cell) (drover-list--pixel-width string)))))

(defun drover-list--icon (string)
  "Return STRING in a cell of its own, so that no text is drawn on its ink."
  (drover-list--in-cell string (drover-list--cell (list string))))

(defun drover-list--frame-index (count now)
  "Return which of COUNT frames of an animation shows at NOW, a float time.
Every animated glyph shows its frame of the same instant, so that a
redraw keeps the pace."
  (mod (floor now drover-list-animation-interval) count))

(defun drover-list--frames (status)
  "Return the frames of the glyph of STATUS, measured, or nil.
The value is (STRINGS WIDTHS): the frames of `drover-animations' in the
face of STATUS, and their widths in pixels.  It is nil when the list
does not animate, `drover-list-animation-interval' being nil, and when
STATUS has no frame."
  (when-let* ((drover-list-animation-interval)
              (frames (seq-remove (lambda (frame) (or (not (stringp frame)) (string-empty-p frame)))
                                  (alist-get status drover-animations))))
    (let ((strings (mapcar (lambda (frame) (drover-face frame (drover-status-face status))) frames)))
      (list strings (mapcar #'drover-list--pixel-width strings)))))

(defun drover-list--status-glyph (status metrics)
  "Return the glyph of STATUS in the status cell of METRICS, animated as they say.
Every status glyph takes the room of the one drawn widest, so that
titles line up whatever the status (see `drover-list--metrics').

When METRICS hold frames for STATUS, the glyph is animated: one
character displays the frame of the time of METRICS, the next one a
space reaching the end of the cell, so that what follows never moves.
Their properties `drover-list-frame' and `drover-list-frame-pad' name
STATUS: from frame to frame, only their display changes."
  (let ((glyph (drover-status-glyph status))
        (cell (plist-get metrics :status-cell)))
    (if-let* ((frames (alist-get status (plist-get metrics :frames))))
        (pcase-let* ((`(,strings ,widths ,width) frames)
                     (index (drover-list--frame-index (length strings) (plist-get metrics :now))))
          (concat (drover-list--pad (car cell))
                  (propertize (if (string-empty-p glyph) (drover-face " " (drover-status-face status))
                                (substring glyph 0 1))
                              'display (nth index strings) 'drover-list-frame status)
                  (propertize " " 'display `(space :width (,(- width (nth index widths))))
                              'drover-list-frame-pad status)))
      (drover-list--in-cell glyph cell))))

(defun drover-list--subagent-glyph (kind)
  "Return the glyph of subagents of KIND, `waiting' or `working', in its face.
See `drover-subagent-glyphs'."
  (drover-face (or (alist-get kind drover-subagent-glyphs) "?")
               (if (eq kind 'waiting) 'drover-waiting 'drover-working)))

(defun drover-list--subagents-help (subagents)
  "Return the tooltip naming SUBAGENTS, active sessions, by what they do.
It tells how many do each, which their glyphs do not."
  (let* ((waiting (seq-filter (lambda (session) (eq (drover-session-status session) 'waiting))
                              subagents))
         (working (seq-difference subagents waiting #'eq))
         (line (lambda (sessions doing)
                 (when sessions
                   (format "%d subagent%s %s: %s" (length sessions) (if (cdr sessions) "s" "") doing
                           (mapconcat (lambda (session)
                                        (drover-list--one-line (drover-session-display-title session)))
                                      sessions ", "))))))
    (string-join (delq nil (list (funcall line waiting "waiting for you")
                                 (funcall line working "working")))
                 "\n")))

(defun drover-list--indicator (subagents metrics)
  "Return the glyphs telling that SUBAGENTS are in the middle of a turn, or nil.
SUBAGENTS are the active subagents of a session, at any depth (see
`drover-tree-active-subagents').  The glyph of `drover-subagent-glyphs'
for those waiting for the user comes first, then that for those working
or waiting to retry, a space between, each in the subagent cell of
METRICS.  They stay still and tell no number: their tooltip names the
subagents and tells how many."
  (when subagents
    (let* ((waiting (seq-some (lambda (session) (eq (drover-session-status session) 'waiting))
                              subagents))
           (working (seq-some (lambda (session) (not (eq (drover-session-status session) 'waiting)))
                              subagents))
           (cell (plist-get metrics :subagent-cell)))
      (propertize (mapconcat (lambda (kind) (drover-list--in-cell (drover-list--subagent-glyph kind) cell))
                             (append (and waiting '(waiting)) (and working '(working)))
                             " ")
                  'help-echo (drover-list--subagents-help subagents)))))

(defun drover-list--icons (session metrics)
  "Return what comes before the title of SESSION: its glyphs and a space.
The glyph of its status comes first, then, when subagents of SESSION are
in the middle of a turn, the glyphs telling so; one space separates
each.  METRICS give the cells of glyphs, the frames of animated ones and
the active subagents of each session."
  (let ((indicator (drover-list--indicator
                    (and (plist-get metrics :active)
                         (gethash (drover-session-key session) (plist-get metrics :active)))
                    metrics)))
    (concat (drover-list--status-glyph (drover-session-status session) metrics)
            " "
            (and indicator (concat indicator " ")))))

(defun drover-list--title-text (session depth)
  "Return the title of SESSION, at DEPTH in its tree, as its line shows it.
A subagent at the top of its tree, its parent not listed, starts with
an arrow; an idle session is dimmed; the title of a session open in a
buffer is in the face `drover-open-session', over the dimming."
  (let* ((title (drover-list--one-line (drover-session-display-title session)))
         (faces (delq nil (list (and (drover-session-buffer session) 'drover-open-session)
                                (and (eq (drover-session-status session) 'idle) 'drover-dim)))))
    (concat (if (and (zerop depth) (drover-session-parent session))
                (drover-face "↳ " 'drover-dim)
              "")
            (if faces (drover-face title (if (cdr faces) faces (car faces))) title))))

(defun drover-list--bar (fraction pixels face)
  "Return a short bar PIXELS wide, FRACTION of it full in FACE.
It is two stretched spaces, the full part and the trough, the trough
in `drover-cache-trough'."
  (let ((full (round (* pixels (max 0 (min 1 fraction))))))
    (concat (drover-face (propertize " " 'display `(space :width (,full))) face)
            (drover-face (propertize " " 'display `(space :width (,(- pixels full))))
                         'drover-cache-trough))))

(defun drover-list--model-text (session)
  "Return the model of SESSION and the effort it runs, as its column shows them.
The effort is in `drover-prompt-picked' when chosen, in
`drover-prompt-inherited' when it is the model's default; its tooltip
says which."
  (let* ((model (drover-session-model session))
         (effort (drover-session-effort session))
         (default (drover-session-effort-default session)))
    (string-join
     (delq nil
           (list (and model (drover-face (drover-list--one-line model) 'drover-dim))
                 (and effort
                      (propertize (drover-face (drover-list--one-line effort)
                                               (if default 'drover-prompt-inherited 'drover-prompt-picked))
                                  'help-echo (format "Effort: %s, %s" effort
                                                     (if default "the model's default" "chosen"))))))
     " ")))

(defun drover-list--provider-text (session)
  "Return the provider of SESSION as its column shows it, or nil.
That is its compact display, often an icon, in a cell of its own, else
its name; the tooltip names it."
  (when-let* ((text (or (drover-session-provider session) (drover-session-provider-name session))))
    (propertize (drover-list--icon (drover-face (drover-list--one-line text) 'drover-dim))
                'help-echo (concat "Provider: " (or (drover-session-provider-name session) text)))))

(defun drover-list--context-text (session char)
  "Return the context of SESSION as its column shows it, or nil when unknown.
A short bar of how much of the model's context it fills comes first,
`drover-list-context-bar-width' characters CHAR pixels wide, then the
size.  Near a long-context price tier, the size is in
`drover-context-near' and the bar's full part in its inverse video,
past it in `drover-context-over'.  Without the model's context size,
an empty space takes the bar's place, so that sizes line up."
  (when-let* ((tokens (drover-session-context-tokens session)))
    (let* ((fraction (drover-session-context-fraction session))
           (pixels (* drover-list-context-bar-width char))
           (face (pcase (car (drover-session-context-tier session))
                   ('over 'drover-context-over)
                   ('near 'drover-context-near))))
      (propertize
       (concat (if fraction
                   (drover-list--bar fraction pixels (if face (list '(:inverse-video t) face) 'drover-context-bar))
                 (drover-list--pad pixels))
               " "
               (drover-face (drover-format-tokens tokens) (or face 'drover-dim)))
       'help-echo (concat "Context: " (drover-session-context-description session))))))

(defun drover-list--text (field session location metrics)
  "Return the text of FIELD for SESSION, or \"\" when it has none.
LOCATION is a function of a session returning where it works, as its
group shows it; METRICS give the time ages count up to and the width
of a character.  The prompt cache has no text: it is drawn apart."
  (let ((now (plist-get metrics :now)))
    (or (pcase field
          ('location (drover-face (drover-list--one-line (funcall location session)) 'drover-dim))
          ('agent (drover-face (drover-list--one-line (drover-session-agent session)) 'drover-dim))
          ('model (drover-list--model-text session))
          ('provider (drover-list--provider-text session))
          ('context (drover-list--context-text session (plist-get metrics :char)))
          ('created (drover-face (drover-format-age (drover-session-created session) now) 'drover-dim))
          ('age (drover-face (drover-format-age (drover-list--age-time session) now) 'drover-dim))
          ('summary (drover-list--one-line (drover-session-summary session))))
        "")))

(cl-defstruct (drover-list--row (:constructor drover-list--row-create)
                                (:copier nil))
  "A session as its heading shows it, before the layout."
  (session nil :documentation "The `drover-session' shown.")
  (depth nil :documentation "Depth of the session in its tree.")
  (indent nil :documentation "Position of the status glyph, in pixels.")
  (icons nil :documentation "The glyphs before the title, and a space.")
  (title-start nil :documentation "Position of the title, in pixels.")
  (title nil :documentation "The title, without the glyph.")
  (texts nil :documentation "Alist (FIELD . TEXT) of the other fields.")
  (cache nil :documentation "Fraction of the prompt cache's lifetime left, or nil."))

(defun drover-list--metrics (now &optional active)
  "Return the measures lines of the list are drawn with at time NOW.
The value is a plist of :now, :char, the width of an average character,
:gap, the room between two columns, :space, the width of a space,
:margin, where the sessions at the top of a tree begin, :width, the
width lines may take, :status-cell and :subagent-cell, the cells of the
status glyphs and of the glyphs of active subagents (see
`drover-list--cell'), :frames, an alist (STATUS STRINGS WIDTHS WIDTH)
of the animated status glyphs, their frames and widths (see
`drover-list--frames') and the right of the status cell, and :active,
ACTIVE, the table of the active subagents of each session (see
`drover-tree-active-subagents'), or nil.  The status cell holds every
status glyph and frame, so that titles line up whatever the status; the
subagent cell both glyphs of `drover-subagent-glyphs'.  Widths are in
pixels; lines leave room for the ellipsis Magit adds to collapsed
headings."
  (let* ((char (drover-list--char-width))
         (statuses (seq-uniq (append drover-status-order (mapcar #'car drover-status-glyphs))))
         (frames (delq nil (mapcar (lambda (status)
                                     (when-let* ((frames (drover-list--frames status)))
                                       (cons status frames)))
                                   '(working retrying))))
         (status-cell (drover-list--cell
                       (append (mapcar #'drover-status-glyph statuses)
                               (mapcan (lambda (entry) (copy-sequence (nth 1 entry))) frames)))))
    (list :now now
          :char char
          :gap (* drover-list--gap char)
          :space (drover-list--pixel-width " ")
          :margin (* drover-list--margin char)
          :width (- (drover-list--line-width)
                    (drover-list--pixel-width (truncate-string-ellipsis)))
          :status-cell status-cell
          :subagent-cell (drover-list--cell (mapcar #'drover-list--subagent-glyph '(waiting working)))
          :frames (mapcar (lambda (entry) (append entry (list (cdr status-cell)))) frames)
          :active active)))

(defun drover-list--row (session depth indent location metrics)
  "Return the row of SESSION at DEPTH in its tree, its glyph INDENT pixels in.
LOCATION is a function of a session returning where it works, as its
group shows it; METRICS are the measures of `drover-list--metrics'."
  (let ((icons (drover-list--icons session metrics)))
    (drover-list--row-create
     :session session :depth depth :indent indent :icons icons
     :title-start (+ indent (drover-list--pixel-width icons))
     :title (drover-list--title-text session depth)
     :texts (mapcar (lambda (field) (cons field (drover-list--text field session location metrics)))
                    '(location agent model provider context created age summary))
     :cache (drover-session-cache-fraction session (plist-get metrics :now)))))

(defun drover-list--tree-rows (nodes location metrics &optional depth indent)
  "Return the rows of the trees NODES at DEPTH, subagents after their parent.
Their glyphs begin INDENT pixels in, by default the margin.  The glyph
of a subagent stands under its parent's title, whatever glyphs come
before the title, down to `drover-list--max-depth'; deeper, under its
parent's glyph.  LOCATION is a function of a session returning where it
works, as the group shows it; METRICS are the measures of
`drover-list--metrics'."
  (let ((depth (or depth 0))
        (indent (or indent (plist-get metrics :margin))))
    (mapcan (lambda (node)
              (let ((row (drover-list--row (drover-node-session node) depth indent location metrics)))
                (cons row
                      (drover-list--tree-rows (drover-node-children node) location metrics (1+ depth)
                                              (if (< depth drover-list--max-depth)
                                                  (drover-list--row-title-start row)
                                                indent)))))
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

(defun drover-list--label-width (field)
  "Return the width in pixels of the name of FIELD's column, 0 without one.
It is measured over the face `header-line', in which the header line
draws it."
  (if-let* ((label (plist-get (drover-list--field-spec field) :label)))
      (drover-list--pixel-width (propertize label 'face '(drover-list-header header-line)))
    0))

(defun drover-list--whole-p (field)
  "Return non-nil when FIELD is drawn whole or not at all, never cut.
Those are the fields of a bar and a number: cut, the number would
mislead."
  (memq field '(cache context)))

(defun drover-list--content-width (field rows metrics)
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

(defun drover-list--natural (field rows metrics)
  "Return the width in pixels the column of FIELD wants to show ROWS, per METRICS.
It is as wide as what it shows, see `drover-list--content-width', and
at least as wide as its name in the header line."
  (max (drover-list--label-width field) (drover-list--content-width field rows metrics)))

(defun drover-list--title-prefix (rows metrics)
  "Return the room, in pixels, before titles in ROWS, from the margin, per METRICS."
  (- (apply #'max 0 (mapcar #'drover-list--row-title-start rows))
     (plist-get metrics :margin)))

(defun drover-list--choose (rows metrics)
  "Return the layout of ROWS in lines of the width of METRICS.
The value is (LAYOUT . FIELDS): LAYOUT is `columns' or `narrow', and
FIELDS the entries of `drover-list-fields' shown, in their order.  The
fields with something to show for ROWS are taken by priority, as long as
their minimum widths fit, and the title always; the fields of
`drover-list-hidden-fields' never.  Columns are used when they include
every field the narrow layout places; else the narrow layout shows
those fields, which need no room of their own but on the title's line,
where they give way to the title."
  (let* ((char (plist-get metrics :char))
         (gap (plist-get metrics :gap))
         (present (seq-filter (lambda (entry)
                                (and (drover-list--has-p (car entry) rows)
                                     (or (eq (car entry) 'title)
                                         (not (memq (car entry) drover-list-hidden-fields)))))
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
lines.  A column is as wide as what it shows and its name, up to its
:max; when the line is too narrow for that, the fields without a :max
give up room first, down to their :min, then the others in proportion
to what they would lose, down to theirs.  A field drawn whole (see
`drover-list--whole-p') gives up the room of its name alone."
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
                                  (floor (if (drover-list--whole-p field)
                                             (min want (drover-list--content-width field rows metrics))
                                           (min want (+ extra (* char (or (plist-get (cdr entry) :min) 0)))))))
                             (list want floor (null max))))
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
  "Return the segments of the glyphs and title of ROW, the title ROOM pixels wide.
They begin SHIFT pixels further right than the row's indentation, when
the title's column does not come first."
  (let* ((shift (or shift 0))
         (indent (+ shift (drover-list--row-indent row)))
         (title (drover-list--truncate (drover-list--row-title row) room)))
    (list (list 'align indent)
          (drover-list--text-segment (drover-list--row-icons row) indent)
          (drover-list--text-segment title (+ shift (drover-list--row-title-start row))))))

(defun drover-list--cache-label (remaining)
  "Return the time left of a prompt cache, REMAINING seconds, as a field shows it.
It is text, as copyable as the rest of the line, which the cache timer
replaces as time passes: its characters tell their part in the
property `drover-list-cache', as `label'."
  (propertize (drover-face (drover-format-duration remaining) 'drover-dim)
              'drover-list-cache 'label 'help-echo #'drover-list--cache-help-echo))

(defun drover-list--cache-cell (fraction remaining layout)
  "Return the prompt cache field: a bar FRACTION full, REMAINING seconds left.
The value is a string displayed as the full part of the bar, its empty
part, a space and the time left, in text (see
`drover-list--cache-label'); LAYOUT gives the measures.  Each character
tells its part in the property `drover-list-cache', so that only they
change as time passes.  The tooltip tells how the cache stands when
shown, see `drover-list--cache-help-echo'."
  (let* ((bar (* drover-list-cache-bar-width (plist-get layout :char)))
         (full (round (* bar fraction))))
    (concat (propertize " " 'display `(space :width (,full)) 'face 'drover-cache-bar
                        'drover-list-cache 'full 'help-echo #'drover-list--cache-help-echo)
            (propertize " " 'display `(space :width (,(- bar full))) 'face 'drover-cache-trough
                        'drover-list-cache 'trough 'help-echo #'drover-list--cache-help-echo)
            (propertize " " 'drover-list-cache 'space 'help-echo #'drover-list--cache-help-echo)
            (drover-list--cache-label remaining))))

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
                        (concat "\n" drover-list--line-start (drover-list--align start)
                                (drover-list--truncate (drover-face below 'drover-dim)
                                                       (- width start))))))
            (and cache (cdr line))))))

;;;; The prompt cache

(defvar-local drover-list--drawn nil
  "Hash table of the sessions drawn, by key, as records of the last drawing.")

(defvar-local drover-list--caches nil
  "Prompt caches drawn, live when last drawn, as plists.
Each has :session, the record drawn, :start, a marker where its heading
begins, :kind, `columns' for a field or `narrow' for a background,
:spec, the segments of a background line from `drover-list--assemble',
:width, the width a background fills, :bar, the width of a field's bar,
in pixels, and :overlay, the overlay of a background.  The time left a
field shows is text, which changes length as time passes: positions
after it are kept by markers.")

(defun drover-list--cache-help (session now)
  "Return the line telling how the prompt cache of SESSION stands at NOW."
  (concat "Prompt cache: " (or (drover-session-cache-description session now) "expired")))

(defun drover-list--cache-help-echo (window object position)
  "Return the tooltip of the prompt cache drawn at POSITION in OBJECT.
OBJECT is the list's buffer, else WINDOW shows it.  The tooltip tells
how the cache stands when it shows, see
`drover-session-cache-description', after the text of the property
`drover-list-help' there, if any: in two lines, where the cache is the
background of the line, that of the session.  For `help-echo'."
  (with-current-buffer (cond ((bufferp object) object)
                             ((window-live-p window) (window-buffer window))
                             (t (current-buffer)))
    (when-let* ((section (drover-list--session-section (magit-section-at position)))
                (session (and drover-list--drawn (gethash (oref section value) drover-list--drawn))))
      (string-join (delq nil (list (get-text-property position 'drover-list-help)
                                   (drover-list--cache-help session (drover-list--now))))
                   "\n"))))

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
                 ;; The text as shown now: an animated glyph may show
                 ;; another frame than when the line was drawn.
                 (setq end (+ start offset
                              (drover-list--fitting
                               (buffer-substring (+ start offset) (+ start offset (length string)))
                               (- x from))))
                 (throw 'done nil))))))))
    (move-overlay (plist-get entry :overlay) start end)))

(defun drover-list--draw-field (entry fraction remaining)
  "Draw the prompt cache field of ENTRY, FRACTION full, REMAINING seconds left.
FRACTION nil draws nothing in its place.  The bar changes its display
alone; the time left, text, is replaced when it reads otherwise, in the
properties it had, the section it belongs to included."
  (let* ((start (plist-get entry :start))
         (bar (plist-get entry :bar))
         (full (if fraction (round (* bar fraction)) 0))
         (end (save-excursion (goto-char start) (line-end-position)))
         (label nil))
    (let ((position start))
      (while (setq position (text-property-not-all position end 'drover-list-cache nil))
        (pcase (get-text-property position 'drover-list-cache)
          ('label (setq label (or label position)))
          (part (put-text-property
                 position (1+ position) 'display
                 (pcase part
                   ('full `(space :width (,full)))
                   ('trough `(space :width (,(if fraction (- bar full) 0))))
                   ('space (if fraction nil '(space :width (0))))))))
        (setq position (1+ position))))
    (when label
      (let* ((label-end (next-single-property-change label 'drover-list-cache nil end))
             (text (if fraction (drover-format-duration remaining) "")))
        (unless (equal (buffer-substring-no-properties label label-end) text)
          (let ((properties (text-properties-at label)))
            (save-excursion
              ;; Inserted before the old text goes, so that a marker
              ;; right after the label stays after the new one.
              (goto-char label)
              (insert (apply #'propertize text properties))
              (delete-region (point) (+ (point) (- label-end label))))))))))

(defun drover-list--draw-caches (&optional now)
  "Redraw the prompt caches of the current list as they stand at NOW.
NOW defaults to the current time.  Only the caches change: the display
of their bars, and the text of the time left.  A cache gone out shows
nothing, and is forgotten."
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
                     (delete-overlay overlay))
                   (set-marker (plist-get entry :start) nil))
                 fraction))
             drover-list--caches)))))

;;;; Animated glyphs

(defvar-local drover-list--frames nil
  "Frames of the animations of the last drawing, from `drover-list--metrics'.
An alist (STATUS STRINGS WIDTHS WIDTH): the frames of the glyph of
STATUS, their widths and the right of the cell they are drawn in.")

(defvar-local drover-list--animated nil
  "Animated glyphs drawn, as a list of (POSITION . KIND).
POSITION is a marker at the character displaying the frame, the next
one its pad: the time left of a prompt cache before it may change
length.  KIND is their animation, of `drover-animations'.")

(defun drover-list--collect-animated ()
  "Return the animated glyphs of the current list.
The value is a list of (POSITION . KIND), see `drover-list--animated'."
  (let ((position (point-min))
        (found nil))
    (while (setq position (text-property-not-all position (point-max) 'drover-list-frame nil))
      (push (cons (copy-marker position) (get-text-property position 'drover-list-frame)) found)
      (setq position (1+ position)))
    (nreverse found)))

(defun drover-list--draw-frames (&optional now)
  "Show the frames of the animated glyphs of the current list at NOW.
NOW defaults to the current time.  Only the display of their two
characters changes, and only where the frame did: neither the text nor
the sections.  Glyphs out of sight, in a collapsed group, are left as
they are until they show."
  (when drover-list-animation-interval
    (let ((now (or now (drover-list--now)))
          (inhibit-read-only t))
      (with-silent-modifications
        (pcase-dolist (`(,position . ,kind) drover-list--animated)
          (pcase-let ((`(,strings ,widths ,width) (alist-get kind drover-list--frames)))
            (when strings
              (let* ((index (drover-list--frame-index (length strings) now))
                     (frame (nth index strings)))
                (unless (or (invisible-p position)
                            (eq (get-text-property position 'display) frame))
                  (put-text-property position (1+ position) 'display frame)
                  (put-text-property (1+ position) (+ position 2) 'display
                                     `(space :width (,(- width (nth index widths))))))))))))))

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

(defun drover-list--model-label (session)
  "Return the model of SESSION and its effort as one plain string, or nil."
  (let ((label (string-join (delq nil (list (drover-session-model session) (drover-session-effort session)))
                            " ")))
    (unless (string-empty-p label) label)))

(defun drover-list--help (session &optional subagents)
  "Return the tooltip of SESSION: its full title and details.
SUBAGENTS, its active subagents, are named after them."
  (string-join
   (delq nil (list (drover-list--one-line (drover-session-display-title session))
                   (drover-session-location session)
                   (let ((agent (drover-session-agent session))
                         (model (drover-list--model-label session)))
                     (cond ((and agent model) (format "%s (%s)" agent model))
                           (t (or agent model))))
                   (when-let* ((summary (drover-session-summary session)))
                     (drover-list--one-line summary))
                   (and subagents (drover-list--subagents-help subagents))))
   "\n"))

(defun drover-list--with-help (string help)
  "Return STRING with HELP as the tooltip of the characters without one.
The glyphs of active subagents keep their own."
  (let ((string (copy-sequence string))
        (position 0))
    (while (< position (length string))
      (let ((next (next-single-property-change position 'help-echo string (length string))))
        (unless (get-text-property position 'help-echo string)
          (put-text-property position next 'help-echo help string))
        (setq position next)))
    string))

(defun drover-list--wrap (text pixels lines)
  "Return TEXT as at most LINES lines at most PIXELS wide, as a list.
Lines break between words, and within a word wider than a line; blank
lines are dropped, and spaces within a line count as one.  When TEXT
takes more lines, the last one ends in an ellipsis."
  (let ((result nil)
        (count 0)
        (more nil)
        (pixels (max pixels (drover-list--char-width))))
    (cl-flet ((add (line)
                (if (>= count lines)
                    (throw 'full (setq more t))
                  (push line result)
                  (cl-incf count))))
      (catch 'full
        (dolist (paragraph (split-string (or text "") "\n"))
          (let ((words (split-string paragraph "[ \t\r]+" t))
                (line nil))
            (while words
              (let ((candidate (if line (concat line " " (car words)) (car words))))
                (cond ((<= (drover-list--pixel-width candidate) pixels)
                       (setq line candidate
                             words (cdr words)))
                      (line
                       (add line)
                       (setq line nil))
                      (t
                       ;; A word wider than a line: what fits, then the rest.
                       (let ((fit (max 1 (drover-list--fitting (car words) pixels))))
                         (add (substring (car words) 0 fit))
                         (setcar words (substring (car words) fit)))))))
            (when line
              (add line))))))
    (when (and more result)
      (setcar result (drover-list--truncate (concat (car result) (truncate-string-ellipsis)) pixels)))
    (nreverse result)))

(defvar-local drover-list--recaps nil
  "Recaps of the last turns of sessions, as a hash table by session key.
Each value is a plist: :updated, the last activity of the session the
recap was asked at; :recap, the recap of `drover-recap', when one
arrived; :pending, a token while a fetch runs, an answer counting only
when its token is still there; :fetched, when the last fetch began.")

(defvar-local drover-list--recap-timer nil
  "Timer drawing the list again, once a recap asked too soon may be fetched.")

(defvar drover-list--recap-asking nil
  "Non-nil while the list asks for a recap.
A recap arriving then is drawn by the asking, not by a redraw.")

(defconst drover-list--recap-interval 2
  "Seconds at least between two fetches of the recap of one session.
A session at work changes many times a second, and each change made
its recap stale: an expanded session fetches its recap again at most
this often while it changes, and once more after.")

(defun drover-list--recap-entry (session)
  "Return what the list knows of the recap of SESSION, fetching it when stale.
The value is a plist of :recap, the recap last known, or nil, :known,
non-nil once a recap arrived, and :loading, non-nil while a fetch
runs.  A recap is fresh while the session's last activity is the one
it was asked at; a stale one is fetched again in the background, at
most every `drover-list--recap-interval' seconds, and the list drawn
again once it arrives.  Nil when the backend of SESSION tells no recap."
  (when (drover-supports-p session :recap)
    (unless drover-list--recaps
      (setq drover-list--recaps (make-hash-table :test #'equal)))
    (let* ((key (drover-session-key session))
           (entry (gethash key drover-list--recaps))
           (fresh (and (plist-member entry :recap)
                       (equal (plist-get entry :updated) (drover-session-updated session)))))
      (unless (or fresh (plist-get entry :pending))
        (let ((wait (and (plist-get entry :fetched)
                         (- (+ (plist-get entry :fetched) drover-list--recap-interval)
                            (drover-list--now)))))
          (if (and wait (> wait 0))
              (drover-list--recap-later wait)
            (drover-list--fetch-recap session)
            (setq entry (gethash key drover-list--recaps)))))
      (list :recap (plist-get entry :recap)
            :known (and (plist-member entry :recap) t)
            :loading (and (plist-get entry :pending) t)))))

(defun drover-list--recap-later (delay)
  "Draw the current list again in DELAY seconds, unless a redraw is due already."
  (unless (timerp drover-list--recap-timer)
    (let ((buffer (current-buffer)))
      (setq drover-list--recap-timer
            (run-at-time delay nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq drover-list--recap-timer nil))
                             (drover-list--update buffer))))))))

(defun drover-list--fetch-recap (session)
  "Ask the backend of SESSION for its recap, for the current list.
The recap is kept for the last activity SESSION has now.  Arriving
later than the asking, it draws the list again if SESSION's details
show; an answer to a fetch the list forgot, as a refresh does, is
dropped."
  (let* ((buffer (current-buffer))
         (key (drover-session-key session))
         (updated (drover-session-updated session))
         (token (list 'recap))
         (fetched (drover-list--now))
         (entry (gethash key drover-list--recaps)))
    (puthash key (append (list :pending token :fetched fetched)
                         (and (plist-member entry :recap)
                              (list :updated (plist-get entry :updated) :recap (plist-get entry :recap))))
             drover-list--recaps)
    (let ((drover-list--recap-asking t))
      (drover-recap session
                    (lambda (recap)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when (and drover-list--recaps
                                     (eq (plist-get (gethash key drover-list--recaps) :pending) token))
                            (puthash key (list :updated updated :recap recap :fetched fetched)
                                     drover-list--recaps)
                            (unless drover-list--recap-asking
                              (drover-list--recap-arrived key))))))))))

(defun drover-list--recap-arrived (key)
  "Draw the current list again if the details of the session KEY show."
  (when-let* ((section (drover-list--find-session key))
              ((not (oref section hidden)))
              ((drover-list--line-shown-p (oref section start))))
    (drover-list--update (current-buffer))))

(defun drover-list--forget-recaps (sessions)
  "Forget the recaps of sessions other than SESSIONS."
  (when drover-list--recaps
    (let ((keys (make-hash-table :test #'equal)))
      (dolist (session sessions)
        (puthash (drover-session-key session) t keys))
      (maphash (lambda (key _)
                 (unless (gethash key keys)
                   (remhash key drover-list--recaps)))
               drover-list--recaps))))

(defcustom drover-list-recap-lines
  '((request . 6) (error . 6) (prompt . 3) (reply . 8) (changes . 2))
  "Lines each part of the details of an expanded session takes at most.
The details recap the session's last turn: what it waits for, the
error that ended it, the user's prompt, the agent's reply and the files
it changed.  A part cut short ends in an ellipsis; the whole reply
shows in the session itself, or in a preview.  An alist (PART . LINES)."
  :type '(alist :key-type (choice (const :tag "What the session waits for" request)
                                  (const :tag "Error" error)
                                  (const :tag "Prompt" prompt)
                                  (const :tag "Reply" reply)
                                  (const :tag "Files changed" changes))
                :value-type natnum)
  :group 'drover-list)

(defun drover-list--recap-lines (part)
  "Return the lines PART of the details takes at most."
  (or (alist-get part drover-list-recap-lines) 3))

(defun drover-list--changes-text (changes directory)
  "Return CHANGES, a recap's files changed, as one line of text.
Such as \"+120 −14 in 3 files: a.el, b.el, c.el\"; the counts are left
out when unknown.  Files under DIRECTORY are named relative to it."
  (let* ((additions (delq nil (mapcar #'cadr changes)))
         (deletions (delq nil (mapcar #'caddr changes)))
         (names (mapcar (lambda (change)
                          (let ((file (car change)))
                            ;; Compared as names: the files may be gone.
                            (if (and directory (file-name-absolute-p file)
                                     (string-prefix-p (file-name-as-directory (expand-file-name directory))
                                                      (expand-file-name file)))
                                (file-relative-name file directory)
                              file)))
                        changes)))
    (concat (and (or additions deletions)
                 (format "+%d −%d in " (apply #'+ additions) (apply #'+ deletions)))
            (format "%d file%s: " (length changes) (if (cdr changes) "s" ""))
            (string-join names ", "))))

(defun drover-list--hidden-facts (session layout)
  "Return what the line of SESSION does not show of it, as (LABEL VALUE FACE).
Those are the fields of `drover-list-fields' LAYOUT leaves out, such
as the model in two lines, and the directory of a session without a
project, which its line cannot tell."
  (let ((shown (plist-get layout :fields))
        (now (plist-get layout :now))
        (facts nil))
    (cl-flet ((fact (field label value &optional face)
                (when (and value (not (memq field shown))
                           (not (string-empty-p (drover-list--one-line value))))
                  (push (list label value face) facts))))
      (unless (drover-session-project session)
        (fact 'directory "Directory" (and (drover-session-directory session)
                                          (abbreviate-file-name (drover-session-directory session)))))
      (fact 'location "Project" (drover-session-location session))
      (fact 'agent "Agent" (drover-session-agent session))
      (fact 'model "Model" (let ((text (drover-list--model-text session)))
                             (unless (string-empty-p text) text)))
      (fact 'provider "Provider" (drover-session-provider-name session))
      (fact 'context "Context" (when-let* ((context (drover-session-context-description session)))
                                 (string-replace "\n" "; " context))
            (pcase (car (drover-session-context-tier session))
              ('over 'drover-context-over)
              ('near 'drover-context-near)))
      (fact 'created "Created" (when-let* ((created (drover-session-created session)))
                                 (concat (drover-format-age created now) " ago")))
      (fact 'cache "Prompt cache" (drover-session-cache-description session now)))
    (nreverse facts)))

(defun drover-list--details (session layout)
  "Return the details of SESSION, as a list of (LABEL TEXT LINES FACE).
They recap its last turn, from what the session tells and the recap
its backend fetches (see `drover-list--recap-entry'): what it waits
for, the error that ended or holds it, the user's last prompt, the
agent's reply and the files it changed, then what its line leaves out
in LAYOUT.  TEXT takes at most LINES lines, wrapped; LINES nil keeps it
to one, cut.  FACE, if any, is that of TEXT."
  (let* ((status (drover-session-status session))
         (entry (drover-list--recap-entry session))
         (recap (plist-get entry :recap))
         (failure (plist-get recap :failure))
         (problem (and (memq status '(failed retrying))
                       (or (and (eq status 'failed) (plist-get recap :error))
                           (drover-session-detail session))))
         (details nil))
    (cl-flet ((add (label text part &optional face)
                (when (and text (not (string-blank-p text)))
                  (push (list label text (and part (drover-list--recap-lines part)) face) details))))
      (dolist (request (drover-session-requests session))
        (add (if (eq (plist-get request :kind) 'question) "Question" "Permission")
             (plist-get request :summary) 'request 'drover-waiting))
      (add (if (eq status 'retrying) "Retrying" "Error") problem 'error (drover-status-face status))
      (add "Prompt" (plist-get recap :prompt) 'prompt)
      (add "Reply" (plist-get recap :reply) 'reply)
      (when-let* ((changes (plist-get recap :changes)))
        (add "Changes" (drover-list--changes-text changes (drover-session-directory session)) 'changes))
      (cond ((and (plist-get entry :loading) (not (plist-get entry :known)))
             (add "Last turn" "loading…" nil 'drover-dim))
            (failure (add "Last turn" (concat "not available: " failure) nil 'drover-dim))
            ((and (plist-get entry :known)
                  (not (or (plist-get recap :prompt) (plist-get recap :reply) (plist-get recap :changes))))
             (add "Last turn" "nothing to recap" nil 'drover-dim))))
    (append (nreverse details)
            (mapcar (pcase-lambda (`(,label ,value ,face)) (list label value nil face))
                    (drover-list--hidden-facts session layout)))))

(defun drover-list--body-x (row layout)
  "Return where the details of ROW begin in LAYOUT, under its title, in pixels."
  (+ (drover-list--row-title-start row)
     (if-let* ((column (assq 'title (plist-get layout :columns))))
         (- (nth 1 column) (plist-get layout :margin))
       0)))

(defun drover-list--insert-details (row layout)
  "Insert the details of the session of ROW, under its title, in LAYOUT.
Each label is followed by its text, wrapped to the window and aligned
after the widest label; when that leaves little room, as in a side
window, the texts go under their labels instead.  Every line begins
with `drover-list--line-start'."
  (let* ((session (drover-list--row-session row))
         (x (drover-list--body-x row layout))
         (width (plist-get layout :width))
         (details (mapcar (pcase-lambda (`(,label . ,rest))
                            (cons (drover-face label 'drover-dim) rest))
                          (drover-list--details session layout)))
         (values-x (+ x (plist-get layout :gap)
                      (apply #'max 0 (mapcar (lambda (detail) (drover-list--pixel-width (car detail)))
                                             details))))
         (stacked (< (- width values-x) (* 20 (plist-get layout :char))))
         (text-x (if stacked x values-x)))
    (cl-flet ((line (&rest parts)
                (apply #'insert drover-list--line-start parts)
                (insert "\n")))
      (pcase-dolist (`(,label ,text ,lines ,face) details)
        (let* ((room (- width text-x))
               (texts (if lines
                          (drover-list--wrap text room lines)
                        (list (drover-list--truncate (drover-list--one-line text) room))))
               (texts (mapcar (lambda (text) (if face (drover-face text face) text)) texts)))
          (if stacked
              (line (drover-list--align x) label)
            (line (drover-list--align x) label (drover-list--align values-x) (or (car texts) ""))
            (setq texts (cdr texts)))
          (dolist (text texts)
            (line (drover-list--align text-x) text)))))))

(defun drover-list--insert-session (row layout)
  "Insert the section of the session of ROW, in LAYOUT.
Its heading is its line; its body, drawn once expanded, its details,
as `drover-list--insert-details' draws them.  A prompt cache it shows
is noted in `drover-list--caches'."
  (let* ((session (drover-list--row-session row))
         (drawn (if (eq (plist-get layout :layout) 'columns)
                    (let ((line (drover-list--column-line row layout)))
                      (list (car line) (and (cdr line) 'columns) nil))
                  (let ((heading (drover-list--narrow-line row layout)))
                    (list (car heading) (and (cdr heading) 'narrow) (cdr heading)))))
         (help (drover-list--help session (and (plist-get layout :active)
                                               (gethash (drover-session-key session)
                                                        (plist-get layout :active)))))
         start)
    (magit-insert-section (drover-list-session-section (drover-session-key session) t)
      ;; Where the line drawn begins: caches find their parts from there.
      (setq start (+ (point) (length drover-list--line-start)))
      (magit-insert-heading
        drover-list--line-start
        (if (eq (nth 1 drawn) 'narrow)
            ;; The cache colours the line: its tooltip tells of it too,
            ;; as it stands when shown.
            (drover-list--with-help (propertize (car drawn) 'drover-list-help help)
                                    #'drover-list--cache-help-echo)
          (drover-list--with-help (car drawn) help)))
      (magit-insert-section-body
        (drover-list--insert-details row layout)))
    (puthash (drover-session-key session) session drover-list--drawn)
    (when (nth 1 drawn)
      (push (list :session session :start (copy-marker start) :kind (nth 1 drawn) :spec (nth 2 drawn)
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
                                         (concat (drover-list--icon (drover-status-glyph status))
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

(defun drover-list--group-icon (group rows)
  "Return the icon of GROUP, (GROUPING . VALUE), listing ROWS, and a space, or nil.
A group of a provider has the icon its sessions show for it, when they
show one rather than its name."
  (pcase group
    (`(provider . ,(and name (pred stringp)))
     (when-let* ((icon (seq-some (lambda (row)
                                   (let ((session (drover-list--row-session row)))
                                     (and (equal (drover-session-provider-name session) name)
                                          (drover-session-provider session))))
                                 rows))
                 ((not (equal icon name))))
       (concat (drover-list--icon (drover-face (drover-list--one-line icon) 'drover-heading)) " ")))))

(defun drover-list--group-heading (group rows)
  "Return the heading of GROUP, (GROUPING . VALUE), listing ROWS."
  (pcase-let ((`(,grouping . ,value) group))
    (concat (drover-list--group-icon group rows)
            (drover-face (drover-group-label grouping value)
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
  "Insert the summary of SESSIONS: how many need the user, how many are listed.
HIDDEN is how many subagents of the trees listed are hidden, told after
the count of sessions when there are some.  Settings and keys are the
View menu's to show, not the list's.  Lines fit the width of LAYOUT."
  (magit-insert-section (drover-list-header)
    (let ((counts (seq-filter (lambda (count) (> (cdr count) 0))
                              (mapcar (lambda (status)
                                        (cons status (seq-count (lambda (session)
                                                                  (eq (drover-session-status session) status))
                                                                sessions)))
                                      drover-attention-statuses))))
      (insert (drover-list--flow
               (append
                (if counts
                    (mapcar (lambda (count)
                              (concat (drover-list--icon (drover-status-glyph (car count)))
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
                       'drover-dim)))
               (plist-get layout :width))
              "\n"))))

(defun drover-list--insert-empty (sessions layout)
  "Insert why the list shows none of SESSIONS, every session known.
When there are SESSIONS, `drover-filter' left them all out; the View
menu tells which filter and clears it.  Lines fit the width of LAYOUT."
  (magit-insert-section (drover-list-empty)
    (let ((width (plist-get layout :width)))
      (cond
       ((null (drover-backends))
        (insert (drover-list--flow '("No agent backend is registered.") width)
                (drover-list--flow '("Load an agent front end, such as opencode.el, to see its sessions.")
                                   width)))
       ((and sessions (drover-filter-active-p))
        (insert (drover-list--flow '("No session matches the filter.") width)))
       (t (insert "No sessions.\n"))))))

(defun drover-list--line-numbers-width ()
  "Return the width in pixels of the line numbers of the list's window, or 0.
Positions aligned to in the text count from past the line numbers,
those of the header line from the text area's edge."
  (let ((window (drover-list--window)))
    (if (buffer-local-value 'display-line-numbers (window-buffer window))
        (with-selected-window window (line-number-display-width t))
      0)))

(defun drover-list--header-line (layout)
  "Return the header line naming the columns of LAYOUT, or nil in two lines.
Each name, the :label of its field in `drover-list-fields', begins with
a space aligned to its column as the cells of lines do, past the line
numbers, which the header line lacks; the name of a column aligned
right is aligned right.  A name wider than its column is cut.  The
tooltip of a name tells what its column shows, from
`drover-list-field-descriptions'."
  (when (eq (plist-get layout :layout) 'columns)
    (let ((shift (drover-list--line-numbers-width))
          (segments nil))
      (pcase-dolist (`(,field ,x ,width ,align) (plist-get layout :columns))
        (when-let* ((label (plist-get (drover-list--field-spec field) :label))
                    ;; Measured over the face of the header line, which draws it.
                    (cut (drover-list--truncate (propertize label 'face '(drover-list-header header-line))
                                                width))
                    ((not (string-empty-p cut))))
          (let* ((label-width (drover-list--pixel-width cut))
                 (start (+ shift (if (eq align 'right) (- (+ x width) label-width) x)))
                 (text (propertize (string-replace "%" "%%" (substring-no-properties cut))
                                   'face 'drover-list-header
                                   'help-echo (alist-get field drover-list-field-descriptions))))
            (setq segments (append segments (list (list 'align start)
                                                  (list 'text text label-width start)))))))
      (and segments (car (drover-list--assemble segments))))))

(defvar-local drover-list--prefetched nil
  "Keys of the sessions the list asked their backends to complete, or nil.
A hash table, made anew by each drawing that may show what backends
fetch on demand, nil when the drawing shows none of it.  See
`drover-list--prefetch'.")

(defun drover-list--prefetch ()
  "Ask backends to fetch what the sessions in sight lack, once per drawing.
Those are the sessions of the expanded groups, not yet asked for since
the list was drawn: a group expanded after the drawing has its sessions
asked for then, as `post-command-hook' runs this.  See
`drover-prefetch'."
  (when drover-list--prefetched
    (let ((sessions nil))
      (dolist (container (drover-list--containers))
        (unless (and (not (eq container magit-root-section)) (oref container hidden))
          (dolist (section (drover-list--session-sections container))
            (let ((key (oref section value)))
              (unless (gethash key drover-list--prefetched)
                (puthash key t drover-list--prefetched)
                (when-let* ((session (gethash key drover-list--drawn)))
                  (push session sessions)))))))
      (when sessions
        (drover-prefetch (nreverse sessions))))))

(defun drover-list--prefetch-wanted-p ()
  "Return non-nil when the list may show what backends fetch on demand.
That is the context, which costs a request per session: in columns,
unless the user hides it.  In two lines, it does not show."
  (and (eq drover-list--layout 'columns)
       (assq 'context drover-list-fields)
       (not (memq 'context drover-list-hidden-fields))))

(defun drover-list--insert ()
  "Insert every section of the list at point."
  (let* ((now (drover-list--now))
         (all (drover-known-sessions))
         (groups (and all (drover-view-groups all now)))
         ;; From every session, so that a parent tells its subagents
         ;; at work whether or not they are shown.
         (metrics (drover-list--metrics now (drover-tree-active-subagents (drover-tree all))))
         (grouped (mapcar (pcase-lambda (`(,group . ,nodes))
                            (cons group (drover-list--tree-rows
                                         nodes (drover-list--location-function group) metrics)))
                          groups))
         (rows (mapcan (lambda (group) (copy-sequence (cdr group))) grouped))
         ;; Columns line up across groups.
         (layout (drover-list--layout rows metrics)))
    (setq drover-list--layout (plist-get layout :layout)
          drover-list--fields (plist-get layout :fields)
          drover-list--frames (plist-get layout :frames)
          header-line-format (and rows (drover-list--header-line layout)))
    (drover-list--forget-visibility all)
    (drover-list--forget-recaps all)
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

(defun drover-list--line-shown-p (position)
  "Return non-nil when the line of POSITION shows, in a section not collapsed.
Its first character tells nothing: every line of a session begins with
an invisible one (see `drover-list--line-start').  Its newline belongs
to the body of the section collapsed around it, if any."
  (not (invisible-p (save-excursion (goto-char position) (line-end-position)))))

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
                         (drover-list--line-shown-p (point))
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
                  (_ (not (drover-list--line-shown-p (point)))))
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
    (mapc (lambda (entry)
            (when-let* ((overlay (plist-get entry :overlay))) (delete-overlay overlay))
            (set-marker (plist-get entry :start) nil))
          drover-list--caches)
    (pcase-dolist (`(,marker . ,_) drover-list--animated)
      (set-marker marker nil))
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
    (setq drover-list--animated (drover-list--collect-animated)
          drover-list--prefetched (and (drover-list--prefetch-wanted-p)
                                       (make-hash-table :test #'equal)))
    (drover-list--prefetch)
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
The recaps of expanded sessions are fetched anew too.
For `revert-buffer-function'."
  (when drover-show-history
    (drover-history-fetch))
  (setq drover-list--widths nil
        drover-list--recaps nil)
  (drover-list--render))

;;;; Timers

(defvar drover-list--age-timer nil
  "Timer redrawing displayed lists, so that ages stay current.")

(defvar drover-list--cache-timer nil
  "Timer redrawing the prompt caches of displayed lists.")

(defvar drover-list--frame-timer nil
  "Timer showing the next frames of the animated glyphs of displayed lists.")

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

(defun drover-list--frame-tick ()
  "Show the next frames of displayed lists; stop when none animates."
  (dolist (buffer (drover-list--shown-buffers))
    (with-current-buffer buffer
      (drover-list--draw-frames)))
  (drover-list--update-timers))

(defun drover-list--stop-timer (symbol)
  "Cancel the timer in the variable SYMBOL, and forget it."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun drover-list--keep-timer (symbol interval needed function &optional first)
  "Keep the timer in SYMBOL running FUNCTION every INTERVAL seconds while NEEDED.
Start it if needed and not running, FIRST seconds from now, INTERVAL by
default; stop it when INTERVAL or NEEDED is nil."
  (if (and interval needed)
      (unless (timerp (symbol-value symbol))
        (set symbol (run-at-time (or first interval) interval function)))
    (drover-list--stop-timer symbol)))

(defun drover-list--update-timers ()
  "Run the timers a displayed list needs, and only those.
Ages need redraws while a list is displayed; prompt caches while a
displayed list shows a live one; animated glyphs while a displayed list
shows one.  Caches and glyphs change only a few characters: their
display, and the text of a cache's time left, never the sections."
  (let ((shown (drover-list--shown-buffers)))
    (drover-list--keep-timer 'drover-list--age-timer drover-list-age-interval shown
                             #'drover-list--age-tick)
    (drover-list--keep-timer 'drover-list--cache-timer drover-list-cache-interval
                             (seq-some (lambda (buffer) (buffer-local-value 'drover-list--caches buffer))
                                       shown)
                             #'drover-list--cache-tick)
    (drover-list--keep-timer 'drover-list--frame-timer drover-list-animation-interval
                             (seq-some (lambda (buffer) (buffer-local-value 'drover-list--animated buffer))
                                       shown)
                             #'drover-list--frame-tick
                             (and drover-list-animation-interval
                                  (drover-list--frame-delay (drover-list--now))))))

(defun drover-list--frame-delay (now)
  "Return the seconds from NOW to the middle of the next frame.
Ticking mid-frame, the timer never shows a frame twice or skips one
when it runs a little late or early."
  (let ((interval drover-list-animation-interval))
    (- (* (+ (floor now interval) 1.5) interval) now)))

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
The list killed draws no recap later.  For `kill-buffer-hook'."
  (when (timerp drover-list--recap-timer)
    (cancel-timer drover-list--recap-timer))
  (unless (seq-some (lambda (buffer) (not (eq buffer (current-buffer))))
                    (drover-list--buffers))
    (remove-hook 'drover-changed-hook #'drover-list--on-change)
    (remove-hook 'window-buffer-change-functions #'drover-list--windows-changed)
    (remove-hook 'window-selection-change-functions #'drover-list--windows-changed)
    (drover-list--stop-timer 'drover-list--age-timer)
    (drover-list--stop-timer 'drover-list--cache-timer)
    (drover-list--stop-timer 'drover-list--frame-timer)))

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

;;;; Moving between sessions

;; Commands of the dispatch menu's List group, which moves point in a
;; list from wherever the menu opened (see `drover--dispatch-navigate').
;; The list binds none of them: Magit's n, p, M-n and M-p move there.

(defun drover-list--check ()
  "Signal a `user-error' unless the current buffer is drover's list."
  (unless (derived-mode-p 'drover-list-mode)
    (user-error "Not in drover's list of sessions")))

(defun drover-list--reachable-sessions ()
  "Return the session sections of the current list point may reach, in order.
Those of collapsed groups are left out: their heading stands for them."
  (mapcan (lambda (container)
            (unless (and (not (eq container magit-root-section)) (oref container hidden))
              (drover-list--session-sections container)))
          (drover-list--containers)))

(defun drover-list--step (sections current count)
  "Return the section COUNT steps away among SECTIONS, and whether it stops.
SECTIONS are in the order of the buffer, CURRENT the one of them point
is in, or nil.  A step goes to the next section, or back to the
previous one when COUNT is negative; from outside SECTIONS, the first
step goes to the first section after point, or the last before it.  The
value is (SECTION . CLAMPED), or nil without SECTIONS; CLAMPED is
non-nil when the steps would have left SECTIONS, SECTION being then the
first or the last."
  (when sections
    (let* ((point (point))
           (here (seq-position sections current #'eq))
           (index (cond (here (+ here count))
                        ((> count 0)
                         (+ (or (cl-position-if (lambda (section) (> (oref section start) point)) sections)
                                (length sections))
                            count -1))
                        (t
                         (+ (or (cl-position-if (lambda (section) (< (oref section start) point))
                                                sections :from-end t)
                                -1)
                            count 1))))
           (clamped (max 0 (min index (1- (length sections))))))
      (cons (nth clamped sections) (/= clamped index)))))

(defun drover-list-next-session (&optional count)
  "Move to the next session of the list, over group headings.
With COUNT, move that many sessions, backwards when negative.  Sessions
in collapsed groups are passed over."
  (interactive "p" drover-list-mode)
  (drover-list--check)
  (let ((count (or count 1)))
    (pcase (drover-list--step (drover-list--reachable-sessions)
                              (drover-list--session-section (magit-current-section))
                              count)
      ('nil (message "No session"))
      (`(,section . ,clamped)
       (goto-char (oref section start))
       (when clamped
         (message (if (< count 0) "First session" "Last session")))))))

(defun drover-list-previous-session (&optional count)
  "Move to the previous session of the list, over group headings.
With COUNT, move that many sessions, forwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list-next-session (- (or count 1))))

(defun drover-list-next-group (&optional count)
  "Move to the first session of the next group of the list.
The heading of a collapsed group stands for its sessions.  With COUNT,
move that many groups, backwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list--check)
  (let ((groups (drover-list--groups))
        (count (or count 1)))
    (unless groups
      (user-error "Sessions are not grouped"))
    (pcase-let ((`(,group . ,clamped)
                 (drover-list--step groups (drover-list--group-section (magit-current-section)) count)))
      (goto-char (oref (or (and (not (oref group hidden)) (car (drover-list--session-sections group)))
                           group)
                       start))
      (when clamped
        (message (if (< count 0) "First group" "Last group"))))))

(defun drover-list-previous-group (&optional count)
  "Move to the first session of the previous group of the list.
With COUNT, move that many groups, forwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list-next-group (- (or count 1))))

(defun drover-list-first-session ()
  "Move to the first session of the list."
  (interactive nil drover-list-mode)
  (drover-list--check)
  (if-let* ((section (car (drover-list--reachable-sessions))))
      (goto-char (oref section start))
    (message "No session")))

(defun drover-list-last-session ()
  "Move to the last session of the list.
Sessions in collapsed groups are passed over."
  (interactive nil drover-list-mode)
  (drover-list--check)
  (if-let* ((section (car (last (drover-list--reachable-sessions)))))
      (goto-char (oref section start))
    (message "No session")))

;;;; The mode

(defun drover-list--mode-line-process ()
  "Return how the list lists sessions, for the mode line.
It tells the grouping, the sort when not by urgency, which subagents
show when not the active ones, whether older sessions show, and whether
a filter leaves sessions out."
  (concat (if drover-group-by
              (concat " by " (alist-get drover-group-by drover-grouping-labels))
            " ungrouped")
          (unless (and (eq drover-sort-by 'urgency) (not drover-sort-descending))
            (concat ", " (alist-get drover-sort-by drover-sort-labels)
                    (if drover-sort-descending " ↓" " ↑")))
          (pcase drover-show-subagents
            ('all " +subagents")
            ('none " -subagents"))
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
\\[magit-section-toggle] collapses a group or shows the recap of a
session's last turn, \\[drover-open-at-point] opens the session at point and
\\[drover-list-display] shows it, the list staying selected.

What a line shows depends on the window's width: see
`drover-list-fields'.  In columns, the header line names them; hover
a name to read what its column shows.  In a side window, opened by
`drover-list-side', sessions open in the window where you work, and
the session it shows is highlighted.  The keys of `drover-session-map'
act on the session at point; \\[drover-toggle-subagents] shows every subagent, the active ones or
none, and \\[drover-refresh] redraws the list.  Text is drawn in the face
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
  (add-hook 'post-command-hook #'drover-list--prefetch nil t)
  (drover-list--watch))

(provide 'drover-list)
;;; drover-list.el ends here
