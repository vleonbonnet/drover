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
;; the user's work: one line per session under status headings, the
;; sessions needing the user first, with what they wait for on a second
;; line.  `drover-sidebar-select' moves there and back.
;;
;; The sidebar acts on the "main window": the most recently used window
;; of its frame outside side windows, which is where the user worked
;; last.  RET opens the session at point there and selects it, C-o
;; shows it there and stays in the sidebar, and o shows it in yet
;; another window.  With `drover-sidebar-preview', moving between
;; sessions shows each in the main window as point reaches it.
;;
;; The sidebar redraws on `drover-changed-hook' while a window shows
;; it, and once shown again after changes it missed.  When a backend
;; tells which buffer shows a session (`drover-session-buffer'), the
;; session shown in the main window is highlighted.

;;; Code:

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

(defcustom drover-sidebar-preview nil
  "Whether moving to a session shows it in the main window.
The sidebar stays selected.  \\<drover-sidebar-mode-map>\
\\[drover-sidebar-toggle-preview] toggles it in the sidebar."
  :type 'boolean
  :group 'drover-sidebar)

(defcustom drover-sidebar-folded-statuses nil
  "Statuses whose group is folded when the sidebar is created."
  :type '(repeat symbol)
  :group 'drover-sidebar)

(defcustom drover-sidebar-age-interval 60
  "Seconds between redraws keeping ages current, or nil for never.
Only a visible sidebar redraws; a hidden one redraws once shown."
  :type '(choice (const :tag "Never" nil) number)
  :group 'drover-sidebar)

(defface drover-sidebar-current '((t :inherit highlight :extend t))
  "Face of the session shown in the main window."
  :group 'drover-faces)

(defconst drover-sidebar-buffer-name "*drover sidebar*"
  "Name of the sidebar buffer.")

(defconst drover-sidebar--fold-glyphs '("▾ " . "▸ ")
  "Prefixes of unfolded and folded group headings.")

(defvar-local drover-sidebar--folded nil
  "Statuses whose group is folded.")

(defvar-local drover-sidebar--stale t
  "Non-nil when sessions changed since the sidebar was last drawn.")

(defvar-local drover-sidebar--width nil
  "Width, in columns, the sidebar was last drawn for.")

(defvar-local drover-sidebar--current-overlays nil
  "Overlays highlighting the session shown in the main window.")

(defvar drover-sidebar--timer nil
  "Timer redrawing the sidebar to keep ages current.")

;;;; Drawing

(defun drover-sidebar--groups (sessions)
  "Return SESSIONS grouped by status, with the idle group last."
  (let* ((groups (drover-group-by-status sessions))
         (idle (assq 'idle groups)))
    (if idle
        (append (remq idle groups) (list idle))
      groups)))

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

(defun drover-sidebar--heading-line (status count folded width)
  "Return the heading of the group of COUNT sessions of STATUS.
FOLDED non-nil marks the group folded; the line fits in WIDTH columns."
  (truncate-string-to-width
   (concat (propertize (concat (if folded
                                   (cdr drover-sidebar--fold-glyphs)
                                 (car drover-sidebar--fold-glyphs))
                               (drover-status-label status))
                       'face 'drover-heading)
           " "
           (propertize (number-to-string count) 'face 'drover-dim))
   width nil nil t))

(defun drover-sidebar--session-line (session width now)
  "Return the line of SESSION, WIDTH columns wide, with ages as of NOW.
The line holds the status glyph, the title, truncated, and the age,
flush right."
  (let* ((status (drover-session-status session))
         (glyph (drover-status-glyph status))
         (age (drover-format-age (drover-sidebar--age-time session) now))
         (age-width (string-width age))
         (title (concat (if (drover-session-parent session)
                            (propertize "↳ " 'face 'drover-dim)
                          "")
                        (propertize (drover-sidebar--one-line
                                     (drover-session-display-title session))
                                    'face (and (eq status 'idle) 'drover-dim))))
         (room (max 1 (- width 2 (string-width glyph) 1
                         (if (zerop age-width) 0 (1+ age-width))))))
    (concat "  " glyph " "
            (truncate-string-to-width title room nil nil t)
            (unless (zerop age-width)
              ;; Aligned by display rather than by padding, since a glyph
              ;; may be drawn narrower or wider than `string-width' says.
              (concat (propertize " " 'display `(space :align-to ,(- width age-width)))
                      (propertize age 'face 'drover-dim))))))

(defun drover-sidebar--summary-line (session width)
  "Return the second line of SESSION, WIDTH columns wide, or nil.
Only a session needing the user and saying why has one."
  (when-let* (((drover-session-attention-p session))
              (summary (drover-session-summary session)))
    (concat "    "
            (propertize (truncate-string-to-width
                         (drover-sidebar--one-line summary) (max 1 (- width 4)) nil nil t)
                        'face 'drover-dim))))

(defun drover-sidebar--insert-line (string &rest properties)
  "Insert STRING and a newline, both with text PROPERTIES."
  (let ((start (point)))
    (insert string "\n")
    (add-text-properties start (point) properties)))

(defun drover-sidebar--insert-session (session width now)
  "Insert the lines of SESSION, WIDTH columns wide, with ages as of NOW."
  (let ((key (drover-session-key session))
        (status (drover-session-status session))
        (start (point)))
    (drover-sidebar--insert-line (drover-sidebar--session-line session width now)
                                 'drover-sidebar-line 'session
                                 'mouse-face 'highlight
                                 'follow-link t
                                 'help-echo (drover-sidebar--help session))
    ;; The newline would stretch the mouse highlight over the window's width.
    (remove-text-properties (1- (point)) (point) '(mouse-face nil))
    (when-let* ((summary (drover-sidebar--summary-line session width)))
      (drover-sidebar--insert-line summary 'drover-sidebar-line 'summary))
    ;; One key object for every line of the session, so that
    ;; `next-single-property-change' finds where the session ends.
    (add-text-properties start (point) (list 'drover-sidebar-key key
                                             'drover-sidebar-session session
                                             'drover-sidebar-status status))))

(defun drover-sidebar--insert-empty (sessions width)
  "Insert the line saying no session shows, WIDTH columns wide.
SESSIONS are every session, so as to hint at hidden subagents."
  (drover-sidebar--insert-line (propertize (truncate-string-to-width "No sessions." width nil nil t)
                                           'face 'drover-dim)
                               'drover-sidebar-line 'empty)
  (when sessions
    (drover-sidebar--insert-line
     (propertize (truncate-string-to-width
                  (substitute-command-keys
                   (format "\\<drover-sidebar-mode-map>\\[drover-sidebar-toggle-children] shows %d subagent%s."
                           (length sessions) (if (cdr sessions) "s" "")))
                  width nil nil t)
                 'face 'drover-dim)
     'drover-sidebar-line 'empty)))

(defun drover-sidebar--render (width &optional now)
  "Draw every session in the current buffer, WIDTH columns wide.
Ages count up to NOW, a float time defaulting to the current time.
Point lands at the start of the buffer."
  (let* ((inhibit-read-only t)
         (now (or now (float-time)))
         (sessions (drover-sessions))
         (visible (and sessions (drover-visible-sessions sessions)))
         (groups (drover-sidebar--groups visible)))
    (erase-buffer)
    (setq drover-sidebar--width width)
    (if (null groups)
        (drover-sidebar--insert-empty sessions width)
      (pcase-dolist (`(,status . ,members) groups)
        (let ((folded (memq status drover-sidebar--folded)))
          (drover-sidebar--insert-line
           (drover-sidebar--heading-line status (length members) folded width)
           'drover-sidebar-line 'heading
           'drover-sidebar-status status
           'mouse-face 'highlight
           'follow-link t
           'help-echo (if folded "Show this group" "Fold this group"))
          (remove-text-properties (1- (point)) (point) '(mouse-face nil))
          (unless folded
            (dolist (session members)
              (drover-sidebar--insert-session session width now))))))
    (goto-char (point-min))
    (drover-sidebar--update-current)))

;;;; Lines

(defun drover-sidebar--line-type (&optional position)
  "Return the kind of the line at POSITION or point.
It is `heading', `session', `summary', `empty', or nil past the end."
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
The value is (TYPE ID LINE): TYPE is `session', with ID its key, or
`heading', with ID its status, or nil; LINE is the line number."
  (when (> (buffer-size) 0)
    (save-excursion
      (goto-char position)
      (let ((line (line-number-at-pos nil t))
            (key (get-text-property (line-beginning-position) 'drover-sidebar-key)))
        (cond
         (key (list 'session key line))
         ((eq (drover-sidebar--line-type) 'heading)
          (list 'heading (get-text-property (line-beginning-position) 'drover-sidebar-status) line))
         (t (list nil nil line)))))))

(defun drover-sidebar--first-session ()
  "Return the start of the first session line, or nil."
  (drover-sidebar--find-line 'session #'always))

(defun drover-sidebar--line-position (line)
  "Return the start of LINE, clamped to the last line of text."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (when (and (eobp) (not (bobp)))
      (forward-line -1))
    (or (drover-sidebar--entry-start (point)) (point))))

(defun drover-sidebar--anchor-position (anchor)
  "Return the position of what ANCHOR, from `drover-sidebar--anchor', shows.
A session or heading no longer shown falls back to the same line; no
ANCHOR falls back to the first session."
  (pcase-let ((`(,type ,id ,line) anchor))
    (or (pcase type
          ('session (drover-sidebar--find-line
                     'session (lambda (start)
                                (equal (get-text-property start 'drover-sidebar-key) id))))
          ('heading (drover-sidebar--find-line
                     'heading (lambda (start)
                                (eq (get-text-property start 'drover-sidebar-status) id)))))
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

(defun drover-sidebar--window-width (windows)
  "Return the width to draw the sidebar for, in WINDOWS showing it.
The window on the selected frame wins; without a window, the width
follows `drover-sidebar-width'."
  (let ((window (or (seq-find (lambda (window) (eq (window-frame window) (selected-frame)))
                              windows)
                    (car windows))))
    (if window
        (window-max-chars-per-line window)
      (max 1 (1- drover-sidebar-width)))))

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
                        windows)))
    (drover-sidebar--render (drover-sidebar--window-width windows))
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
             (not (eql (window-max-chars-per-line window) drover-sidebar--width)))
    (with-current-buffer (window-buffer window)
      (drover-sidebar--redraw))))

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
  "Redraw the sidebar from what the backends report."
  (interactive)
  (when-let* ((buffer (get-buffer drover-sidebar-buffer-name)))
    (with-current-buffer buffer
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

(defun drover-sidebar--preview ()
  "Show the session at point in the main window, if previews are on."
  (when-let* ((drover-sidebar-preview)
              (session (drover-sidebar--session-at-point))
              ((drover-supports-p session :open)))
    (with-demoted-errors "Drover preview: %S"
      (drover-sidebar--display session))))

(defun drover-sidebar--move (count types)
  "Move to the COUNTth line of TYPES after point, before when negative.
Return non-nil when point moved."
  (if-let* ((position (drover-sidebar--search count types)))
      (progn (goto-char position) t)
    (message "No %s %s" (if (< count 0) "previous" "next")
             (if (memq 'heading types) "group" "session"))
    nil))

(defun drover-sidebar-next (&optional count)
  "Move to the next session, or COUNT sessions forward.
Show it in the main window when `drover-sidebar-preview' is non-nil."
  (interactive "p" drover-sidebar-mode)
  (when (drover-sidebar--move (or count 1) '(session))
    (drover-sidebar--preview)))

(defun drover-sidebar-previous (&optional count)
  "Move to the previous session, or COUNT sessions back.
Show it in the main window when `drover-sidebar-preview' is non-nil."
  (interactive "p" drover-sidebar-mode)
  (when (drover-sidebar--move (- (or count 1)) '(session))
    (drover-sidebar--preview)))

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
Point moves to the group's heading."
  (interactive nil drover-sidebar-mode)
  (let ((status (get-text-property (line-beginning-position) 'drover-sidebar-status)))
    (unless status
      (user-error "No group at point"))
    (setq drover-sidebar--folded (if (memq status drover-sidebar--folded)
                                     (delq status drover-sidebar--folded)
                                   (cons status drover-sidebar--folded)))
    (drover-sidebar--redraw (list 'heading status nil))))

(defun drover-sidebar-toggle-all-groups ()
  "Fold every group, or unfold them all when all are folded."
  (interactive nil drover-sidebar-mode)
  (let* ((shown (delq nil (mapcar (lambda (start) (get-text-property start 'drover-sidebar-status))
                                  (drover-sidebar--heading-starts))))
         (all-folded (seq-every-p (lambda (status) (memq status drover-sidebar--folded)) shown)))
    (setq drover-sidebar--folded (if all-folded nil (copy-sequence drover-status-order)))
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

(defun drover-sidebar-open ()
  "Open the session at point in the main window and select it.
On a group heading, fold or unfold the group."
  (interactive nil drover-sidebar-mode)
  (if (eq (drover-sidebar--line-type) 'heading)
      (drover-sidebar-toggle-group)
    (let ((session (drover-session-at-point))
          (main (drover-sidebar--main-window)))
      (unless (drover-supports-p session :open)
        (user-error "%s sessions cannot be opened" (drover-backend-label session)))
      (if main
          (progn (select-window main)
                 (drover-open session))
        (drover-open session t)))))

(defun drover-sidebar-open-other-window ()
  "Open the session at point in a window other than the main window.
The sidebar stays selected."
  (interactive nil drover-sidebar-mode)
  (let ((session (drover-session-at-point)))
    (if-let* ((main (drover-sidebar--main-window)))
        (with-selected-window main
          (drover-open session t))
      (save-selected-window
        (drover-open session t)))
    (drover-sidebar--update-current)))

(defun drover-sidebar-display ()
  "Show the session at point in the main window, staying in the sidebar."
  (interactive nil drover-sidebar-mode)
  (drover-sidebar--display (drover-session-at-point)))

(defun drover-sidebar-mouse-open (event)
  "Open the session clicked in EVENT, or fold or unfold the group clicked."
  (interactive "e" drover-sidebar-mode)
  (let ((position (event-end event)))
    (select-window (posn-window position))
    (goto-char (posn-point position))
    (drover-sidebar-open)))

(defun drover-sidebar-toggle-preview ()
  "Toggle showing sessions in the main window while moving between them.
The change is local to the sidebar."
  (interactive nil drover-sidebar-mode)
  (setq-local drover-sidebar-preview (not drover-sidebar-preview))
  (force-mode-line-update)
  (drover-sidebar--preview)
  (message "Sidebar previews %s" (if drover-sidebar-preview "on" "off")))

(defun drover-sidebar-toggle-children ()
  "Toggle listing the sessions of subagents in the sidebar.
A subagent needing the user always shows."
  (interactive nil drover-sidebar-mode)
  (setq-local drover-show-children (not drover-show-children))
  (force-mode-line-update)
  (drover-sidebar--redraw)
  (message "%s subagent sessions" (if drover-show-children "Showing" "Hiding")))

;;;; Mode

(defun drover-sidebar--mode-line-flags ()
  "Return the mode line flags of the local sidebar settings."
  (concat (and drover-show-children " +sub")
          (and drover-sidebar-preview " preview")))

(defvar-keymap drover-sidebar-mode-map
  :doc "Keymap of `drover-sidebar-mode'."
  :parent drover-session-map
  "n" #'drover-sidebar-next
  "p" #'drover-sidebar-previous
  "M-n" #'drover-sidebar-next-group
  "M-p" #'drover-sidebar-previous-group
  "TAB" #'drover-sidebar-toggle-group
  "<backtab>" #'drover-sidebar-toggle-all-groups
  "RET" #'drover-sidebar-open
  "o" #'drover-sidebar-open-other-window
  "C-o" #'drover-sidebar-display
  "C-c C-f" #'drover-sidebar-toggle-preview
  "c" #'drover-sidebar-toggle-children
  "g" #'drover-sidebar-refresh
  "q" #'drover-sidebar-hide
  "?" #'describe-mode
  "h" #'describe-mode
  "<" #'beginning-of-buffer
  ">" #'end-of-buffer
  "<mouse-2>" #'drover-sidebar-mouse-open)

(define-derived-mode drover-sidebar-mode special-mode "Drover"
  "Major mode of the drover sidebar, listing sessions by status.

\\{drover-sidebar-mode-map}"
  :interactive nil
  (setq-local drover-session-at-point-function #'drover-sidebar--session-at-point
              revert-buffer-function (lambda (&rest _) (drover-sidebar--redraw))
              drover-sidebar--folded (copy-sequence drover-sidebar-folded-statuses)
              truncate-lines t
              buffer-undo-list t
              cursor-in-non-selected-windows nil
              mode-line-format '(" " (:propertize "Drover" face mode-line-buffer-id)
                                 (:eval (drover-sidebar--mode-line-flags))))
  (add-hook 'window-size-change-functions #'drover-sidebar--size-changed nil t)
  (add-hook 'kill-buffer-hook #'drover-sidebar--unwatch nil t))

(provide 'drover-sidebar)
;;; drover-sidebar.el ends here
