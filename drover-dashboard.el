;;; drover-dashboard.el --- Dashboard of agent sessions in collapsible sections  -*- lexical-binding: t; -*-

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

;; `drover-dashboard' lists every agent session in a Magit-like buffer.
;; Sessions are grouped by status, most urgent first, or by project
;; (\\`G'); each group is a section that \\`TAB' collapses, and each
;; session a section whose body holds its details.  The keys of
;; `drover-session-map' act on the session at point.
;;
;; A subagent is listed under its parent, indented one step deeper, and
;; a parent and its subagents stay together: they go in the group of the
;; most urgent session among them, so a parent whose subagent waits for
;; the user is listed with the sessions that need the user.  Subagents
;; are sections of the group, like their parent, rather than sections
;; inside their parent's: Magit hides the whole body of a section, and
;; showing a parent's details must neither hide nor reveal its
;; subagents.  \\`c' lists every subagent or only those needing the user.
;;
;; Text is drawn in the face `drover-dashboard', variable pitch by
;; default.  Columns are aligned in pixels, measured in that face and
;; in the fonts of the window showing the dashboard; on a text terminal
;; a pixel is a character cell, so the same layout holds there.
;;
;; The buffer redraws when backends report changes, but only while it
;; is displayed: a hidden dashboard is marked stale and redraws when it
;; is next shown.  A redraw keeps point on the same session, even when
;; the session moved to another group, and keeps every section expanded
;; or collapsed as it was.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'drover)

(defgroup drover-dashboard nil
  "Dashboard of agent sessions in collapsible sections."
  :group 'drover
  :prefix "drover-dashboard-")

(defface drover-dashboard '((t :inherit variable-pitch))
  "Face of the text of the dashboard.
It inherits `variable-pitch', which fits more in a line; inherit
`fixed-pitch' instead for a monospaced dashboard."
  :group 'drover-faces)

(defcustom drover-dashboard-buffer-name "*drover*"
  "Name of the dashboard buffer."
  :type 'string
  :group 'drover-dashboard)

(defcustom drover-dashboard-display-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Action by which `drover-dashboard' displays its buffer.
See `display-buffer' for the form of an action."
  :type 'sexp
  :group 'drover-dashboard)

(defcustom drover-dashboard-grouping 'status
  "How a new dashboard groups sessions: by `status' or by `project'.
\\<drover-dashboard-mode-map>\\[drover-dashboard-toggle-grouping] switches the grouping of one dashboard."
  :type '(choice (const :tag "By status" status)
                 (const :tag "By project" project))
  :group 'drover-dashboard)

(defcustom drover-dashboard-collapsed-statuses '(idle)
  "Statuses whose group is collapsed until expanded.
Once a group is expanded or collapsed, the dashboard remembers it."
  :type '(repeat symbol)
  :group 'drover-dashboard)

(defcustom drover-dashboard-title-width 40
  "Most characters a session title takes before it is truncated.
A character counts as wide as the letter x in the face
`drover-dashboard'."
  :type 'natnum
  :group 'drover-dashboard)

(defcustom drover-dashboard-location-width 30
  "Most characters a session's project and branch take before truncation.
A character counts as wide as the letter x in the face
`drover-dashboard'."
  :type 'natnum
  :group 'drover-dashboard)

(defcustom drover-dashboard-age-refresh-interval 60
  "Seconds between redraws of a displayed dashboard, keeping ages current.
A value of nil disables these redraws: the dashboard then redraws only
when sessions change."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-dashboard)

(defconst drover-dashboard--summary-min-width 8
  "Fewest characters worth showing a truncated summary in.")

(defconst drover-dashboard--gap 2
  "Characters between two columns.")

(defconst drover-dashboard--margin 2
  "Characters before the sessions at the top of a tree.")

;;;; Sections

(defvar-keymap drover-dashboard-group-section-map
  :doc "Keys on the heading of a group of sessions."
  "RET" #'magit-section-toggle)

(defclass drover-dashboard-group-section (magit-section)
  ((keymap :initform 'drover-dashboard-group-section-map))
  "A group of sessions.")

(defclass drover-dashboard-status-section (drover-dashboard-group-section) ()
  "The sessions of one status; the value is the status.")

(defclass drover-dashboard-project-section (drover-dashboard-group-section) ()
  "The sessions of one project; the value is the project name, or nil.")

(defclass drover-dashboard-session-section (magit-section) ()
  "One session; the value is its `drover-session-key'.
Its subagents follow it in its group, as sections of their own.")

(defun drover-dashboard--session-section (section)
  "Return SECTION or its ancestor showing a session, or nil."
  (while (and section (not (object-of-class-p section 'drover-dashboard-session-section)))
    (setq section (oref section parent)))
  section)

(defun drover-dashboard--group-section (section)
  "Return SECTION or its ancestor showing a group of sessions, or nil."
  (while (and section (not (object-of-class-p section 'drover-dashboard-group-section)))
    (setq section (oref section parent)))
  section)

(defun drover-dashboard--groups ()
  "Return the group sections of the current dashboard."
  (and magit-root-section
       (seq-filter (lambda (section) (object-of-class-p section 'drover-dashboard-group-section))
                   (oref magit-root-section children))))

(defun drover-dashboard--find-session (key)
  "Return the section showing the session whose key is KEY, or nil."
  (seq-some (lambda (group)
              (seq-find (lambda (section) (equal (oref section value) key))
                        (oref group children)))
            (drover-dashboard--groups)))

(defun drover-dashboard--find-group (class value)
  "Return the group section of CLASS whose value is VALUE, or nil."
  (seq-find (lambda (section)
              (and (eq (eieio-object-class section) class)
                   (equal (oref section value) value)))
            (drover-dashboard--groups)))

(defun drover-dashboard--session-at-point ()
  "Return the current record of the session at point, or nil.
The record comes from the backend, not from the last redraw, so actions
see the session as it stands now."
  (when-let* ((section (drover-dashboard--session-section (magit-current-section)))
              (key (oref section value)))
    (drover-find (car key) (cdr key))))

;;;; Visibility

(defvar-local drover-dashboard--visibility nil
  "Hash table of the visibility, `show' or `hide', of past sections.
Groups are keyed by (CLASS . VALUE), sessions by (session . KEY): a
session keeps its visibility when it moves to another group.")

(defun drover-dashboard--visibility-key (section)
  "Return the key of SECTION in `drover-dashboard--visibility', or nil."
  (cond ((object-of-class-p section 'drover-dashboard-session-section)
         (cons 'session (oref section value)))
        ((object-of-class-p section 'drover-dashboard-group-section)
         (cons (eieio-object-class section) (oref section value)))))

(defun drover-dashboard--remember-visibility ()
  "Record the visibility of every section of the current dashboard."
  (when magit-root-section
    (magit-map-sections
     (lambda (section)
       (when-let* ((key (drover-dashboard--visibility-key section)))
         (puthash key (if (oref section hidden) 'hide 'show) drover-dashboard--visibility))))))

(defun drover-dashboard--forget-visibility (sessions)
  "Forget the visibility of sessions other than SESSIONS.
Sessions come and go, groups are few: only sessions are forgotten."
  (let ((keys (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (drover-session-key session) t keys))
    (maphash (lambda (key _)
               (when (and (eq (car key) 'session) (not (gethash (cdr key) keys)))
                 (remhash key drover-dashboard--visibility)))
             drover-dashboard--visibility)))

(defun drover-dashboard--cached-visibility (section)
  "Return the recorded visibility of SECTION, or nil.
For `magit-section-set-visibility-hook'."
  (when-let* ((key (drover-dashboard--visibility-key section)))
    (gethash key drover-dashboard--visibility)))

;;;; Measuring

(defun drover-dashboard--window ()
  "Return the window the current dashboard is drawn for.
It is the selected window when it shows the dashboard, else another
window showing it, else the selected window."
  (if (eq (window-buffer) (current-buffer))
      (selected-window)
    (or (get-buffer-window nil 0) (selected-window))))

(defun drover-dashboard--pixel-width (string)
  "Return the width in pixels of STRING drawn in the current buffer.
The width is that of the fonts of `drover-dashboard--window' with the
face remapping of the buffer, which draws its text in the face
`drover-dashboard'.  On a text terminal, a pixel is a character cell."
  (drover-text-pixel-width string face-remapping-alist (drover-dashboard--window)))

(defun drover-dashboard--char-width ()
  "Return the width in pixels of an average character of the dashboard.
It is the width of the letter x."
  (drover-dashboard--pixel-width "x"))

(defun drover-dashboard--line-width ()
  "Return the width in pixels that lines of the dashboard may take.
Like `window-max-chars-per-line', leave out line numbers, and the
column of the continuation glyph when the window has no fringe for it."
  (let* ((window (drover-dashboard--window))
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

(defun drover-dashboard--truncate (string pixels)
  "Return STRING cut to at most PIXELS wide, ending in an ellipsis if cut.
The ellipsis takes the properties of the text before it.  Return \"\"
when not even the ellipsis fits."
  (drover-truncate-to-pixels string pixels #'drover-dashboard--pixel-width))

(defun drover-dashboard--align (x)
  "Return a space reaching X pixels from the left of the text area."
  (propertize " " 'display `(space :align-to (,x))))

;;;; Drawing

(defvar-local drover-dashboard--grouping nil
  "How the dashboard groups sessions: `status' or `project'.")

(defvar-local drover-dashboard--stale nil
  "Non-nil when sessions changed since the hidden dashboard was drawn.")

(defvar-local drover-dashboard--width nil
  "Width, in pixels, of the lines the dashboard was last drawn for.")

(defun drover-dashboard--one-line (string)
  "Return STRING, or \"\" for nil, on one line."
  (string-trim (replace-regexp-in-string "[\n\r\t]+" " " (or string ""))))

(defun drover-dashboard--fit (string pixels &optional face)
  "Return STRING in FACE, truncated to PIXELS wide."
  (drover-dashboard--truncate (if face (drover-face string face) string) pixels))

(defun drover-dashboard--time (session)
  "Return the time from which SESSION's age counts, or nil."
  (or (drover-session-since session) (drover-session-updated session)))

(defun drover-dashboard--metrics (now)
  "Return the measures lines of the dashboard are drawn with at time NOW.
The value is a plist of :now, :char, the width of an average character,
:gap, the room between two columns, :margin, where the sessions at the
top of a tree begin, and :step, how much deeper each level of subagents
begins.  A step is as wide as the widest status glyph and a space, so
that a subagent's glyph stands under its parent's title.  Widths are
in pixels."
  (let ((char (drover-dashboard--char-width))
        (statuses (seq-uniq (append drover-status-order (mapcar #'car drover-status-glyphs)))))
    (list :now now
          :char char
          :gap (* drover-dashboard--gap char)
          :margin (* drover-dashboard--margin char)
          :step (apply #'max 0 (mapcar (lambda (status)
                                         (drover-dashboard--pixel-width
                                          (concat (drover-status-glyph status) " ")))
                                       statuses)))))

(cl-defstruct (drover-dashboard--row (:constructor drover-dashboard--row-create)
                                     (:copier nil))
  "The heading line of a session, its cells cut to their columns."
  (session nil :documentation "The `drover-session' shown.")
  (indent nil :documentation "Position of the status glyph, in pixels.")
  (start nil :documentation "Position of the cell of the status and title.")
  (status nil :documentation "Status glyph, one space and title.")
  (end nil :documentation "Position where the title ends.")
  (location nil :documentation "Where the session works, in its face.")
  (location-width nil :documentation "Width of LOCATION, in pixels.")
  (age nil :documentation "Age of the session, in its face.")
  (age-width nil :documentation "Width of AGE, in pixels.")
  (summary nil :documentation "Why the session has its status, on one line."))

(defun drover-dashboard--row (session depth location metrics)
  "Return the row of SESSION at DEPTH in its tree, working at LOCATION.
METRICS are the measures of `drover-dashboard--metrics'."
  (let* ((char (plist-get metrics :char))
         (indent (+ (plist-get metrics :margin) (* depth (plist-get metrics :step))))
         (status (concat (drover-status-glyph (drover-session-status session))
                         " "
                         (drover-dashboard--fit
                          (drover-dashboard--one-line
                           (format "%s" (drover-session-display-title session)))
                          (* drover-dashboard-title-width char))))
         (location (drover-dashboard--fit (drover-dashboard--one-line location)
                                          (* drover-dashboard-location-width char)
                                          'drover-dim))
         (age (drover-face (drover-format-age (drover-dashboard--time session)
                                              (plist-get metrics :now))
                           'drover-dim)))
    (drover-dashboard--row-create
     :session session :indent indent :status status
     :end (+ indent (drover-dashboard--pixel-width status))
     :location location :location-width (drover-dashboard--pixel-width location)
     :age age :age-width (drover-dashboard--pixel-width age)
     :summary (drover-dashboard--one-line (drover-session-summary session)))))

(defun drover-dashboard--tree-rows (nodes location metrics &optional depth)
  "Return the rows of the trees NODES at DEPTH, subagents after their parent.
LOCATION is a function of a session returning where it works, as the
group shows it; METRICS are the measures of `drover-dashboard--metrics'."
  (let ((depth (or depth 0)))
    (mapcan (lambda (node)
              (let ((session (drover-node-session node)))
                (cons (drover-dashboard--row session depth (funcall location session) metrics)
                      (drover-dashboard--tree-rows (drover-node-children node) location metrics
                                                   (1+ depth)))))
            nodes)))

(defun drover-dashboard--layout (rows metrics width)
  "Return METRICS with the columns of ROWS in lines WIDTH pixels wide.
Add :location, where locations begin, or nil when no row has one;
:age-end, where ages end, aligned right, or nil when no row has one;
:summary, where summaries begin; and :summary-width, the room left for
them, or nil when too little is left."
  (let* ((gap (plist-get metrics :gap))
         (x (+ gap (apply #'max 0 (mapcar #'drover-dashboard--row-end rows))))
         (location-width (apply #'max 0 (mapcar #'drover-dashboard--row-location-width rows)))
         (age-width (apply #'max 0 (mapcar #'drover-dashboard--row-age-width rows)))
         (location nil)
         (age-end nil))
    (when (> location-width 0)
      (setq location x
            x (+ x location-width gap)))
    (when (> age-width 0)
      (setq age-end (+ x age-width)
            x (+ age-end gap)))
    ;; Magit appends an ellipsis to a collapsed heading.
    (let ((room (- width x (drover-dashboard--pixel-width (truncate-string-ellipsis)))))
      (append (list :location location :age-end age-end :summary x
                    :summary-width (and (>= room (* drover-dashboard--summary-min-width
                                                    (plist-get metrics :char)))
                                        room))
              metrics))))

(defun drover-dashboard--line (row layout)
  "Return the heading line of ROW, in the columns of LAYOUT.
Each cell begins with a space aligned to its column, so that columns
line up whatever the widths of the characters before them."
  (let ((location (plist-get layout :location))
        (age-end (plist-get layout :age-end))
        (summary (drover-dashboard--row-summary row))
        (summary-width (plist-get layout :summary-width)))
    (concat (drover-dashboard--align (drover-dashboard--row-indent row))
            (drover-dashboard--row-status row)
            (when (and location (> (drover-dashboard--row-location-width row) 0))
              (concat (drover-dashboard--align location) (drover-dashboard--row-location row)))
            (when (and age-end (> (drover-dashboard--row-age-width row) 0))
              (concat (drover-dashboard--align (- age-end (drover-dashboard--row-age-width row)))
                      (drover-dashboard--row-age row)))
            (when (and summary-width (not (string-empty-p summary)))
              (concat (drover-dashboard--align (plist-get layout :summary))
                      (drover-dashboard--truncate summary summary-width))))))

(defun drover-dashboard--details (session now)
  "Return the details of SESSION as a list of (LABEL VALUE FACE).
Ages are relative to NOW; details without a value are left out."
  (let* ((status (drover-session-status session))
         (time (drover-dashboard--time session))
         (parent-id (drover-session-parent session))
         (details
          `(("Status" ,(concat (drover-status-label status)
                               (and time (format " for %s" (drover-format-age time now))))
             ,(drover-status-face status))
            ("Directory" ,(and (drover-session-directory session)
                               (abbreviate-file-name (drover-session-directory session))))
            ("Branch" ,(drover-session-branch session))
            ("Agent" ,(drover-session-agent session))
            ("Model" ,(drover-session-model session))
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
                    (let ((value (drover-dashboard--one-line
                                  (and (nth 1 detail) (format "%s" (nth 1 detail))))))
                      (unless (string-empty-p value)
                        (list (car detail) value (nth 2 detail)))))
                  details))))

(defun drover-dashboard--insert-details (session x gap now)
  "Insert the details of SESSION, their labels X pixels from the left.
GAP is the room between labels and values, in pixels; ages are relative
to NOW."
  (let* ((details (mapcar (lambda (detail)
                            (cons (drover-face (car detail) 'drover-dim) (cdr detail)))
                          (drover-dashboard--details session now)))
         (values-x (+ x gap (apply #'max 0 (mapcar (lambda (detail)
                                                     (drover-dashboard--pixel-width (car detail)))
                                                   details)))))
    (pcase-dolist (`(,label ,value ,face) details)
      (insert (drover-dashboard--align x) label
              (drover-dashboard--align values-x) (if face (drover-face value face) value)
              "\n"))))

(defun drover-dashboard--insert-session (row layout)
  "Insert the section of the session of ROW, in the columns of LAYOUT.
Its details begin under its title."
  (let ((session (drover-dashboard--row-session row))
        (x (+ (drover-dashboard--row-indent row) (plist-get layout :step)))
        (gap (plist-get layout :gap))
        (now (plist-get layout :now)))
    (magit-insert-section (drover-dashboard-session-section (drover-session-key session) t)
      (magit-insert-heading (drover-dashboard--line row layout))
      (magit-insert-section-body
        (drover-dashboard--insert-details session x gap now)))))

(defun drover-dashboard--count (rows)
  "Return the number of ROWS, dimmed and in parentheses."
  (drover-face (format " (%d)" (length rows)) 'drover-dim))

(defun drover-dashboard--status-counts (sessions)
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

(defun drover-dashboard--groups-by-status (nodes metrics)
  "Return the groups of trees NODES by status, measured with METRICS.
Each group is a list (CLASS VALUE HEADING ROWS HIDE), as
`drover-dashboard--insert-group' takes."
  (mapcar (pcase-lambda (`(,status . ,group))
            (let ((rows (drover-dashboard--tree-rows group #'drover-session-location metrics)))
              (list 'drover-dashboard-status-section status
                    (concat (drover-face (drover-status-label status)
                                         (list (drover-status-face status) 'drover-heading))
                            (drover-dashboard--count rows))
                    rows
                    (memq status drover-dashboard-collapsed-statuses))))
          (drover-group-trees-by-status nodes)))

(defun drover-dashboard--groups-by-project (nodes metrics)
  "Return the groups of trees NODES by project, measured with METRICS.
Each group is a list (CLASS VALUE HEADING ROWS HIDE), as
`drover-dashboard--insert-group' takes.  The heading names the
project, so a session of that project shows only its branch."
  (mapcar (pcase-lambda (`(,project . ,group))
            (let* ((rows (drover-dashboard--tree-rows
                          group
                          (lambda (session)
                            (if (equal (drover-session-project session) project)
                                (drover-session-branch session)
                              (drover-session-location session)))
                          metrics))
                   (counts (drover-dashboard--status-counts
                            (mapcar #'drover-dashboard--row-session rows))))
              (list 'drover-dashboard-project-section project
                    (concat (drover-face (or project "No project")
                                         (if project 'drover-heading '(italic drover-heading)))
                            (drover-dashboard--count rows)
                            (and counts (concat "  " counts)))
                    rows
                    nil)))
          (drover-group-trees-by-project nodes)))

(defun drover-dashboard--insert-group (class value heading rows layout hide)
  "Insert a group section of CLASS and VALUE holding ROWS.
HEADING is its heading line and LAYOUT the columns of its rows.
When HIDE is non-nil, the group starts collapsed."
  (magit-insert-section ((eval class) value hide)
    (magit-insert-heading heading)
    (dolist (row rows)
      (drover-dashboard--insert-session row layout))
    (insert "\n")))

(defun drover-dashboard--insert-header (sessions hidden)
  "Insert the summary of SESSIONS, of which HIDDEN subagents are not shown."
  (magit-insert-section (drover-dashboard-header)
    (let ((counts (mapcar (lambda (status)
                            (cons status (seq-count (lambda (session)
                                                      (eq (drover-session-status session) status))
                                                    sessions)))
                          drover-attention-statuses)))
      (insert
       (if (seq-some (lambda (count) (> (cdr count) 0)) counts)
           (mapconcat (lambda (count)
                        (concat (drover-status-glyph (car count))
                                " "
                                (drover-face
                                 (format "%d %s" (cdr count) (downcase (drover-status-label (car count))))
                                 (drover-status-face (car count)))))
                      (seq-filter (lambda (count) (> (cdr count) 0)) counts)
                      "   ")
         (drover-face "Nothing needs you" 'drover-dim))
       "   "
       (drover-face
        (concat (format "%d session%s" (length sessions) (if (= (length sessions) 1) "" "s"))
                (and (> hidden 0)
                     (format ", %d subagent%s hidden" hidden (if (= hidden 1) "" "s"))))
        'drover-dim)
       "\n\n"))))

(defun drover-dashboard--insert-empty ()
  "Insert why the dashboard lists no session."
  (magit-insert-section (drover-dashboard-empty)
    (insert
     (if (null (drover-backends))
         "No agent backend is registered.\nLoad an agent front end, such as opencode.el, to see its sessions.\n"
       "No sessions.\n"))))

(defun drover-dashboard--insert ()
  "Insert every section of the dashboard at point."
  (let* ((all (drover-sessions))
         (trees (drover-tree all))
         (nodes (drover-tree-visible trees))
         (sessions (drover-tree-sessions nodes))
         (metrics (drover-dashboard--metrics (float-time)))
         (groups (if (eq drover-dashboard--grouping 'project)
                     (drover-dashboard--groups-by-project nodes metrics)
                   (drover-dashboard--groups-by-status nodes metrics)))
         ;; Columns line up across groups.
         (layout (drover-dashboard--layout (mapcan (lambda (group) (copy-sequence (nth 3 group)))
                                                   groups)
                                           metrics drover-dashboard--width)))
    (drover-dashboard--forget-visibility all)
    (magit-insert-section (drover-dashboard)
      ;; Every tree has a root shown, so no session shown means none at all.
      (if (null sessions)
          (drover-dashboard--insert-empty)
        (drover-dashboard--insert-header sessions (drover-tree-hidden-count trees))
        (pcase-dolist (`(,class ,value ,heading ,rows ,hide) groups)
          (drover-dashboard--insert-group class value heading rows layout hide))))))

;;;; Keeping point

(defun drover-dashboard--anchor (position)
  "Return what is at POSITION, to find it again after a redraw.
The value is a plist of :key, the key of the session there, :offset,
the line within its section, :group, the class and value of the group
there, :index, the position of the session in the group, :line, the
line number, and :char, the position within the line.  Positions in a
line count characters, not columns: with variable pitch and aligned
cells, columns say little."
  (save-excursion
    (goto-char position)
    (let* ((section (magit-section-at))
           (session (drover-dashboard--session-section section))
           (group (drover-dashboard--group-section section)))
      (list :key (and session (oref session value))
            :offset (and session (- (line-number-at-pos) (line-number-at-pos (oref session start))))
            :group (and group (cons (eieio-object-class group) (oref group value)))
            :index (and session group (seq-position (oref group children) session))
            :line (line-number-at-pos)
            :char (- (point) (line-beginning-position))))))

(defun drover-dashboard--move-in-line (char)
  "Move point CHAR characters into its line, or to its end."
  (goto-char (min (+ (line-beginning-position) char) (line-end-position))))

(defun drover-dashboard--visible-start (section)
  "Return the start of SECTION, or of its outermost collapsed ancestor."
  (let ((visible section))
    (while-let ((parent (oref section parent)))
      (when (oref parent hidden)
        (setq visible parent))
      (setq section parent))
    (oref visible start)))

(defun drover-dashboard--resolve (anchor)
  "Return the position of ANCHOR, a value of `drover-dashboard--anchor'.
Prefer the same session, wherever it moved, then the same group, then
the same line."
  (save-excursion
    (let* ((key (plist-get anchor :key))
           (session (and key (drover-dashboard--find-session key)))
           (group-id (plist-get anchor :group))
           (group (and (not session) group-id
                       (drover-dashboard--find-group (car group-id) (cdr group-id)))))
      (cond
       (session
        (let ((start (drover-dashboard--visible-start session)))
          (goto-char start)
          ;; In a collapsed group, the group heading stands for the session.
          (when (= start (oref session start))
            (unless (oref session hidden)
              (forward-line (plist-get anchor :offset))
              (unless (and (< (point) (oref session end))
                           (eq (drover-dashboard--session-section (magit-section-at)) session))
                (goto-char start)))
            (drover-dashboard--move-in-line (plist-get anchor :char)))))
       (group
        (let ((children (oref group children))
              (index (plist-get anchor :index)))
          (goto-char (drover-dashboard--visible-start
                      (if (and index children)
                          (nth (min index (1- (length children))) children)
                        group)))))
       (t
        (goto-char (point-min))
        (forward-line (1- (plist-get anchor :line)))
        (when (and (eobp) (not (bobp)))
          (forward-line -1))
        (if-let* ((section (magit-section-at))
                  (_ (invisible-p (point))))
            (goto-char (drover-dashboard--visible-start section))
          (drover-dashboard--move-in-line (plist-get anchor :char)))))
      (point))))

;;;; Refreshing

(defun drover-dashboard--render ()
  "Redraw the current dashboard from what backends report now.
Point, in the buffer and in every window showing it, stays on the
same session, and window starts on the same line."
  (let* ((windows (get-buffer-window-list nil nil t))
         (anchor (drover-dashboard--anchor (point)))
         (placements (mapcar (lambda (window)
                               (list window
                                     (drover-dashboard--anchor (window-point window))
                                     (line-number-at-pos (window-start window))))
                             windows))
         (inhibit-read-only t))
    (drover-dashboard--remember-visibility)
    (deactivate-mark)
    (setq magit-section-pre-command-section nil
          magit-section-highlight-overlays nil
          magit-section-selection-overlays nil
          magit-section-highlighted-sections nil
          magit-section-focused-sections nil
          drover-dashboard--width (drover-dashboard--line-width)
          drover-dashboard--stale nil)
    (erase-buffer)
    (save-excursion
      (drover-dashboard--insert))
    ;; Sections exist now; showing the root applies their visibility.
    (magit-section-show magit-root-section)
    (goto-char (drover-dashboard--resolve anchor))
    (pcase-dolist (`(,window ,window-anchor ,start-line) placements)
      (set-window-point window (drover-dashboard--resolve window-anchor))
      (set-window-start window
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point))
                        t))
    (when (eq (window-buffer) (current-buffer))
      (goto-char (window-point)))
    (magit-section-update-highlight t)
    (set-buffer-modified-p nil)
    (push (current-buffer) magit-section--refreshed-buffers)))

(defun drover-dashboard--visible-p (buffer)
  "Return non-nil when a window of a visible or iconified frame shows BUFFER."
  (get-buffer-window buffer 0))

(defun drover-dashboard--buffers ()
  "Return the live dashboard buffers."
  (match-buffers '(derived-mode . drover-dashboard-mode)))

(defun drover-dashboard--update (buffer)
  "Redraw dashboard BUFFER if it is displayed, else mark it stale."
  (with-current-buffer buffer
    (if (drover-dashboard--visible-p buffer)
        (drover-dashboard--render)
      (setq drover-dashboard--stale t))))

(defun drover-dashboard--on-change ()
  "Bring every dashboard up to date with the backends.
For `drover-changed-hook'."
  (mapc #'drover-dashboard--update (drover-dashboard--buffers)))

(defun drover-dashboard--on-window-change (window)
  "Redraw the dashboard WINDOW shows if it is stale or drawn for another width.
For `window-buffer-change-functions' and `window-size-change-functions'."
  (let ((buffer (window-buffer window)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'drover-dashboard-mode)
                   (or drover-dashboard--stale
                       (not (eql drover-dashboard--width (drover-dashboard--line-width)))))
          (drover-dashboard--render))))))

(defun drover-dashboard--on-face-change ()
  "Redraw the dashboard, whose text changed size.
For `text-scale-mode-hook' and `buffer-face-mode-hook': columns are
aligned in pixels, measured in the faces of the buffer."
  (when (derived-mode-p 'drover-dashboard-mode)
    (drover-dashboard--render)))

(defvar drover-dashboard--timer nil
  "Timer redrawing displayed dashboards, so that ages stay current.")

(defun drover-dashboard--tick ()
  "Redraw displayed dashboards; stop when none is left."
  (if-let* ((buffers (drover-dashboard--buffers)))
      (mapc #'drover-dashboard--update buffers)
    (drover-dashboard--stop-timer)))

(defun drover-dashboard--start-timer ()
  "Start the timer of `drover-dashboard-age-refresh-interval', unless it runs."
  (when (and drover-dashboard-age-refresh-interval (not (timerp drover-dashboard--timer)))
    (setq drover-dashboard--timer
          (run-at-time drover-dashboard-age-refresh-interval drover-dashboard-age-refresh-interval
                       #'drover-dashboard--tick))))

(defun drover-dashboard--stop-timer ()
  "Stop the timer redrawing dashboards."
  (when (timerp drover-dashboard--timer)
    (cancel-timer drover-dashboard--timer))
  (setq drover-dashboard--timer nil))

(defun drover-dashboard--on-kill ()
  "Stop watching backends once the last dashboard is killed.
For `kill-buffer-hook'."
  (unless (seq-some (lambda (buffer) (not (eq buffer (current-buffer))))
                    (drover-dashboard--buffers))
    (remove-hook 'drover-changed-hook #'drover-dashboard--on-change)
    (drover-dashboard--stop-timer)))

(defun drover-dashboard--revert (&rest _)
  "Redraw the dashboard, for `revert-buffer-function'."
  (drover-dashboard--render))

;;;; Commands

(defun drover-dashboard--check-mode ()
  "Signal a `user-error' unless the current buffer is a dashboard."
  (unless (derived-mode-p 'drover-dashboard-mode)
    (user-error "Not in a drover dashboard")))

(defun drover-dashboard-refresh ()
  "Redraw the dashboard from what backends report now."
  (interactive nil drover-dashboard-mode)
  (drover-dashboard--check-mode)
  (drover-dashboard--render))

(defun drover-dashboard-toggle-grouping ()
  "Group the sessions of the dashboard by project, or back by status."
  (interactive nil drover-dashboard-mode)
  (drover-dashboard--check-mode)
  (setq drover-dashboard--grouping (if (eq drover-dashboard--grouping 'project) 'status 'project))
  (drover-dashboard--render)
  (message "Sessions grouped by %s" drover-dashboard--grouping))

(defun drover-dashboard-toggle-children ()
  "Show or hide the sessions of subagents in the dashboard.
A subagent that needs the user, or whose own subagents do, is always
shown."
  (interactive nil drover-dashboard-mode)
  (drover-dashboard--check-mode)
  (setq-local drover-show-children (not drover-show-children))
  (drover-dashboard--render)
  (message (if drover-show-children "Showing subagents" "Hiding subagents")))

(defun drover-dashboard--goto-first-session ()
  "Move point to the first session shown, if any."
  (when-let* ((group (car (drover-dashboard--groups))))
    (goto-char (drover-dashboard--visible-start (or (car (oref group children)) group)))))

(defun drover-dashboard--mode-line-process ()
  "Return the grouping and subagent display of the dashboard, for the mode line."
  (concat " by " (symbol-name drover-dashboard--grouping)
          (and drover-show-children " +subagents")))

;; Precedence, highest first: these bindings, `drover-session-map', then
;; `magit-section-mode-map' and its parent `special-mode-map'.  The only
;; key two of them bind differently is SPC, which peeks at the session
;; (drover) rather than scrolling (special-mode): acting on sessions is
;; what the dashboard is for, and C-v still scrolls.  `g' and `q' are
;; bound here so they do not depend on special-mode.  RET on a group
;; heading toggles it, through the group sections' own keymap, so RET
;; stays the shared command on sessions even when users rebind it.
(defvar-keymap drover-dashboard-mode-map
  :doc "Keymap of `drover-dashboard-mode'."
  :parent (make-composed-keymap (list drover-session-map magit-section-mode-map))
  "g" #'drover-dashboard-refresh
  "q" #'quit-window
  "G" #'drover-dashboard-toggle-grouping
  "c" #'drover-dashboard-toggle-children)

(define-derived-mode drover-dashboard-mode magit-section-mode "Drover"
  "Major mode of the dashboard of agent sessions.
Sessions are grouped by status or by project, each subagent under its
parent; \\[magit-section-toggle] collapses a group or shows the details
of a session.  The keys of `drover-session-map' act on the session at
point.  Text is drawn in the face `drover-dashboard'.

\\{drover-dashboard-mode-map}"
  :group 'drover-dashboard
  :interactive nil
  (setq-local drover-dashboard--grouping drover-dashboard-grouping)
  (setq-local drover-dashboard--visibility (make-hash-table :test #'equal))
  (setq-local drover-session-at-point-function #'drover-dashboard--session-at-point)
  (setq-local revert-buffer-function #'drover-dashboard--revert)
  (setq-local mode-line-process '(:eval (drover-dashboard--mode-line-process)))
  ;; Magit's cache keys a session by its group too, and never forgets;
  ;; `drover-dashboard--visibility' replaces it.
  (setq-local magit-section-cache-visibility nil)
  (drover-remap-default-face 'drover-dashboard)
  (add-hook 'magit-section-set-visibility-hook #'drover-dashboard--cached-visibility nil t)
  (add-hook 'window-buffer-change-functions #'drover-dashboard--on-window-change nil t)
  (add-hook 'window-size-change-functions #'drover-dashboard--on-window-change nil t)
  (add-hook 'text-scale-mode-hook #'drover-dashboard--on-face-change nil t)
  (add-hook 'buffer-face-mode-hook #'drover-dashboard--on-face-change nil t)
  (add-hook 'kill-buffer-hook #'drover-dashboard--on-kill nil t)
  (add-hook 'drover-changed-hook #'drover-dashboard--on-change)
  (drover-dashboard--start-timer))

(defun drover-dashboard--buffer ()
  "Return the dashboard buffer, creating it if needed."
  (let ((buffer (get-buffer-create drover-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'drover-dashboard-mode)
        (drover-dashboard-mode)))
    buffer))

;;;###autoload
(defun drover-dashboard ()
  "Show the dashboard of every agent session and select it.
Sessions are grouped by status, those needing you first, or by
project; see `drover-dashboard-mode' for the keys."
  (interactive)
  (drover-start)
  (let* ((buffer (drover-dashboard--buffer))
         (new (null (buffer-local-value 'drover-dashboard--width buffer))))
    (pop-to-buffer buffer drover-dashboard-display-action)
    (with-current-buffer buffer
      (drover-dashboard--render)
      (when new
        (drover-dashboard--goto-first-session)))))

(provide 'drover-dashboard)
;;; drover-dashboard.el ends here
