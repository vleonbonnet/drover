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
;; The buffer redraws when backends report changes, but only while it
;; is displayed: a hidden dashboard is marked stale and redraws when it
;; is next shown.  A redraw keeps point on the same session, even when
;; the session moved to another group, and keeps every section expanded
;; or collapsed as it was.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'drover)

(defgroup drover-dashboard nil
  "Dashboard of agent sessions in collapsible sections."
  :group 'drover
  :prefix "drover-dashboard-")

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
  "Most columns a session title takes before it is truncated."
  :type 'natnum
  :group 'drover-dashboard)

(defcustom drover-dashboard-location-width 30
  "Most columns a session's project and branch take before they are truncated."
  :type 'natnum
  :group 'drover-dashboard)

(defcustom drover-dashboard-age-refresh-interval 60
  "Seconds between redraws of a displayed dashboard, keeping ages current.
A value of nil disables these redraws: the dashboard then redraws only
when sessions change."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-dashboard)

(defconst drover-dashboard--summary-min-width 8
  "Fewest columns worth showing a truncated summary in.")

(defconst drover-dashboard--detail-label-width 12
  "Columns of the labels in a session's details.")

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
  "One session; the value is its `drover-session-key'.")

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

;;;; Drawing

(defvar-local drover-dashboard--grouping nil
  "How the dashboard groups sessions: `status' or `project'.")

(defvar-local drover-dashboard--stale nil
  "Non-nil when sessions changed since the hidden dashboard was drawn.")

(defvar-local drover-dashboard--width nil
  "Width, in columns, the dashboard was last drawn for.")

(defun drover-dashboard--font-lock (string)
  "Return STRING with its `face' properties turned into `font-lock-face'.
Font lock clears `face' properties in `magit-section-mode' buffers."
  (let ((string (copy-sequence string))
        (position 0))
    (while (< position (length string))
      (let ((next (next-single-property-change position 'face string (length string))))
        (when-let* ((face (get-text-property position 'face string)))
          (put-text-property position next 'font-lock-face face string))
        (setq position next)))
    (remove-text-properties 0 (length string) '(face nil) string)
    string))

(defun drover-dashboard--face (string face)
  "Return STRING shown in FACE."
  (propertize string 'font-lock-face face))

(defun drover-dashboard--one-line (string)
  "Return STRING, or \"\" for nil, on one line."
  (string-trim (replace-regexp-in-string "[\n\r\t]+" " " (or string ""))))

(defun drover-dashboard--fit (string width &optional pad)
  "Return STRING truncated to WIDTH columns with an ellipsis.
With PAD, pad it with spaces to WIDTH."
  (truncate-string-to-width string width nil (and pad ?\s) "…"))

(defun drover-dashboard--display-width ()
  "Return the number of columns lines of the dashboard may take.
Use the selected window when it shows the dashboard, else another
window showing it, else the selected window."
  (let ((window (if (eq (window-buffer) (current-buffer))
                    (selected-window)
                  (or (get-buffer-window nil 0) (selected-window)))))
    (window-max-chars-per-line window)))

(defun drover-dashboard--title (session)
  "Return the title of SESSION as the dashboard shows it."
  (concat (and (drover-session-parent session) "↳ ")
          (drover-dashboard--one-line (format "%s" (drover-session-display-title session)))))

(defun drover-dashboard--location (session grouping)
  "Return where SESSION works under GROUPING, or \"\".
Grouped by project, the heading names the project, so only the
branch is left."
  (drover-dashboard--one-line
   (if (eq grouping 'project)
       (drover-session-branch session)
     (drover-session-location session))))

(defun drover-dashboard--time (session)
  "Return the time from which SESSION's age counts, or nil."
  (or (drover-session-since session) (drover-session-updated session)))

(defun drover-dashboard--layout (sessions grouping width now)
  "Return the column widths of SESSIONS under GROUPING.
WIDTH is the width of the lines and NOW the current time; both are
kept in the returned plist."
  (cl-flet ((widest (function limit)
              (min limit (apply #'max 0 (mapcar (lambda (session)
                                                  (string-width (funcall function session)))
                                                sessions)))))
    (list :grouping grouping :width width :now now
          :title (widest #'drover-dashboard--title drover-dashboard-title-width)
          :location (widest (lambda (session) (drover-dashboard--location session grouping))
                            drover-dashboard-location-width)
          :age (widest (lambda (session)
                         (drover-format-age (drover-dashboard--time session) now))
                       most-positive-fixnum))))

(defun drover-dashboard--session-line (session layout)
  "Return the heading line of SESSION, in columns of LAYOUT."
  (let* ((location-width (plist-get layout :location))
         (age-width (plist-get layout :age))
         (line (concat
                "  "
                (drover-dashboard--font-lock (drover-status-glyph (drover-session-status session)))
                " "
                (drover-dashboard--fit (drover-dashboard--title session) (plist-get layout :title) t)
                (when (> location-width 0)
                  (concat "  " (drover-dashboard--face
                                (drover-dashboard--fit
                                 (drover-dashboard--location session (plist-get layout :grouping))
                                 location-width t)
                                'drover-dim)))
                (when (> age-width 0)
                  (concat "  " (drover-dashboard--face
                                (string-pad (drover-format-age (drover-dashboard--time session)
                                                               (plist-get layout :now))
                                            age-width nil t)
                                'drover-dim)))))
         (summary (drover-dashboard--one-line (drover-session-summary session)))
         ;; Two columns separate the summary, and one is left for the
         ;; ellipsis `magit-section' appends to a collapsed heading.
         (room (- (plist-get layout :width) (string-width line) 3)))
    (string-trim-right
     (if (and (not (string-empty-p summary)) (>= room drover-dashboard--summary-min-width))
         (concat line "  " (drover-dashboard--fit summary room))
       line))))

(defun drover-dashboard--insert-detail (label value &optional face)
  "Insert a detail line of a session labelled LABEL showing VALUE in FACE.
Insert nothing when VALUE is nil or empty."
  (let ((value (drover-dashboard--one-line (and value (format "%s" value)))))
    (unless (string-empty-p value)
      (insert "      "
              (drover-dashboard--face (string-pad label drover-dashboard--detail-label-width) 'drover-dim)
              (if face (drover-dashboard--face value face) value)
              "\n"))))

(defun drover-dashboard--insert-details (session now)
  "Insert the details of SESSION, with ages relative to NOW."
  (let ((status (drover-session-status session))
        (time (drover-dashboard--time session)))
    (drover-dashboard--insert-detail
     "Status"
     (concat (drover-status-label status)
             (and time (format " for %s" (drover-format-age time now))))
     (drover-status-face status))
    (drover-dashboard--insert-detail
     "Directory" (and (drover-session-directory session)
                      (abbreviate-file-name (drover-session-directory session))))
    (drover-dashboard--insert-detail "Branch" (drover-session-branch session))
    (drover-dashboard--insert-detail "Agent" (drover-session-agent session))
    (drover-dashboard--insert-detail "Model" (drover-session-model session))
    (when-let* ((parent-id (drover-session-parent session)))
      (drover-dashboard--insert-detail
       "Subagent of"
       (if-let* ((parent (drover-find (drover-session-backend session) parent-id)))
           (drover-session-display-title parent)
         parent-id)))
    (drover-dashboard--insert-detail "Backend" (drover-backend-label session))
    (drover-dashboard--insert-detail "Detail" (drover-session-detail session)
                                     (drover-status-face status))
    (dolist (request (drover-session-requests session))
      (drover-dashboard--insert-detail
       (capitalize (format "%s" (or (plist-get request :kind) "request")))
       (plist-get request :summary)
       'drover-waiting))))

(defun drover-dashboard--insert-session (session layout)
  "Insert the section of SESSION, in columns of LAYOUT."
  (magit-insert-section (drover-dashboard-session-section (drover-session-key session) t)
    (magit-insert-heading (drover-dashboard--session-line session layout))
    (magit-insert-section-body
      (drover-dashboard--insert-details session (plist-get layout :now)))))

(defun drover-dashboard--insert-group (class value heading sessions layout hide)
  "Insert a group section of CLASS and VALUE holding SESSIONS.
HEADING is its heading line and LAYOUT the columns of its sessions.
When HIDE is non-nil, the group starts collapsed."
  (magit-insert-section ((eval class) value hide)
    (magit-insert-heading heading)
    (dolist (session sessions)
      (drover-dashboard--insert-session session layout))
    (insert "\n")))

(defun drover-dashboard--count (sessions)
  "Return the number of SESSIONS, dimmed and in parentheses."
  (drover-dashboard--face (format " (%d)" (length sessions)) 'drover-dim))

(defun drover-dashboard--status-counts (sessions)
  "Return how many of SESSIONS need the user, per status, as one string.
Return nil when none does."
  (when-let* ((parts (delq nil
                           (mapcar (lambda (status)
                                     (let ((count (seq-count (lambda (session)
                                                               (eq (drover-session-status session) status))
                                                             sessions)))
                                       (when (> count 0)
                                         (concat (drover-dashboard--font-lock (drover-status-glyph status))
                                                 (drover-dashboard--face (format "%d" count)
                                                                         (drover-status-face status))))))
                                   drover-attention-statuses))))
    (string-join parts " ")))

(defun drover-dashboard--insert-by-status (sessions layout)
  "Insert SESSIONS in one group per status, in columns of LAYOUT."
  (pcase-dolist (`(,status . ,group) (drover-group-by-status sessions))
    (drover-dashboard--insert-group
     'drover-dashboard-status-section status
     (concat (drover-dashboard--face (drover-status-label status)
                                     (list (drover-status-face status) 'drover-heading))
             (drover-dashboard--count group))
     group layout (memq status drover-dashboard-collapsed-statuses))))

(defun drover-dashboard--insert-by-project (sessions layout)
  "Insert SESSIONS in one group per project, in columns of LAYOUT."
  (pcase-dolist (`(,project . ,group) (drover-group-by-project sessions))
    (let ((counts (drover-dashboard--status-counts group)))
      (drover-dashboard--insert-group
       'drover-dashboard-project-section project
       (concat (drover-dashboard--face (or project "No project")
                                       (if project 'drover-heading '(italic drover-heading)))
               (drover-dashboard--count group)
               (and counts (concat "  " counts)))
       group layout nil))))

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
                        (concat (drover-dashboard--font-lock (drover-status-glyph (car count)))
                                " "
                                (drover-dashboard--face
                                 (format "%d %s" (cdr count) (downcase (drover-status-label (car count))))
                                 (drover-status-face (car count)))))
                      (seq-filter (lambda (count) (> (cdr count) 0)) counts)
                      "   ")
         (drover-dashboard--face "Nothing needs you" 'drover-dim))
       "   "
       (drover-dashboard--face
        (concat (format "%d session%s" (length sessions) (if (= (length sessions) 1) "" "s"))
                (and (> hidden 0)
                     (format ", %d subagent%s hidden" hidden (if (= hidden 1) "" "s"))))
        'drover-dim)
       "\n\n"))))

(defun drover-dashboard--insert-empty (hidden)
  "Insert why the dashboard lists no session; HIDDEN subagents are not shown."
  (magit-insert-section (drover-dashboard-empty)
    (insert
     (cond ((null (drover-backends))
            "No agent backend is registered.\nLoad an agent front end, such as opencode.el, to see its sessions.\n")
           ((> hidden 0)
            (substitute-command-keys
             (format "No sessions to show; %d subagent%s hidden.\nPress \\<drover-dashboard-mode-map>\\[drover-dashboard-toggle-children] to show subagents.\n"
                     hidden (if (= hidden 1) "" "s"))))
           (t "No sessions.\n")))))

(defun drover-dashboard--insert ()
  "Insert every section of the dashboard at point."
  (let* ((all (drover-sessions))
         (sessions (drover-visible-sessions all))
         (hidden (- (length all) (length sessions)))
         (layout (drover-dashboard--layout sessions drover-dashboard--grouping
                                           drover-dashboard--width (float-time))))
    (drover-dashboard--forget-visibility all)
    (magit-insert-section (drover-dashboard)
      (if (null sessions)
          (drover-dashboard--insert-empty hidden)
        (drover-dashboard--insert-header sessions hidden)
        (if (eq drover-dashboard--grouping 'project)
            (drover-dashboard--insert-by-project sessions layout)
          (drover-dashboard--insert-by-status sessions layout))))))

;;;; Keeping point

(defun drover-dashboard--anchor (position)
  "Return what is at POSITION, to find it again after a redraw.
The value is a plist of :key, the key of the session there, :offset,
the line within its section, :group, the class and value of the group
there, :index, the position of the session in the group, :line, the
line number, and :column."
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
            :column (current-column)))))

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
            (move-to-column (plist-get anchor :column)))))
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
          (move-to-column (plist-get anchor :column)))))
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
          drover-dashboard--width (drover-dashboard--display-width)
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
                       (not (eql drover-dashboard--width (drover-dashboard--display-width)))))
          (drover-dashboard--render))))))

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
A subagent that needs the user is always shown."
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
Sessions are grouped by status or by project; \\[magit-section-toggle]
collapses a group or shows the details of a session.  The keys of
`drover-session-map' act on the session at point.

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
  (add-hook 'magit-section-set-visibility-hook #'drover-dashboard--cached-visibility nil t)
  (add-hook 'window-buffer-change-functions #'drover-dashboard--on-window-change nil t)
  (add-hook 'window-size-change-functions #'drover-dashboard--on-window-change nil t)
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
