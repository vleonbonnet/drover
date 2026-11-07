;;; drover-ui.el --- Faces, labels and commands shared by session views  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: internal

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

;; Every view of sessions (the list, completion) draws them
;; with the same glyphs, faces and labels, measures text the same way,
;; and describes the filter in force the same way.  The glyphs, faces,
;; animations and formats are opencode.el's (opencode-look.el), which
;; its own mode line and pickers draw with: drover keeps no look of its
;; own, for any backend, and its former options and faces are obsolete
;; aliases of those.  The commands and keys every view shares are in
;; `drover-keys'.

;;; Code:

(require 'cl-lib)
(require 'drover-core)
(require 'face-remap)
(require 'opencode-look)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 21))
  (load "drover-core" nil t))

(defgroup drover-faces nil
  "Faces of drover."
  :group 'drover
  :group 'faces)

;; Those of sessions are opencode.el's, which drover draws with.
(custom-add-to-group 'drover-faces 'opencode-faces 'custom-group)

;; Drover's own: the headings of groups and the secondary details of
;; lists, which opencode.el has no list to show.

(defface drover-heading '((t :inherit bold))
  "Face of group headings."
  :group 'drover-faces)

(defface drover-dim '((t :inherit shadow))
  "Face of secondary details, such as directories and ages."
  :group 'drover-faces)

(defface drover-harness '((t :inherit drover-dim))
  "Face of the harness of a session, its icon and name (see `drover-harnesses')."
  :group 'drover-faces)

(defface drover-host-local '((t :inherit success :weight normal))
  "Face of this host's name and icon in the session list."
  :group 'drover-faces)

(defface drover-host-disconnected '((t :inherit warning :weight normal))
  "Face of the icon of a host not connected (see `drover-host-icon').
Of the weight of the icon of a host connected, dimmed, so that the
icon keeps its width as the host comes and goes."
  :group 'drover-faces)

(defface drover-added '((t :inherit success :weight normal))
  "Face of the lines a session added, as its changes count them."
  :group 'drover-faces)

(defface drover-removed '((t :inherit error :weight normal))
  "Face of the lines a session removed, as its changes count them."
  :group 'drover-faces)

(defcustom drover-attention-glyph "\U0001F916" ; robot face
  "Glyph before the count of sessions needing you, in the mode line.
`drover-attention-mode' shows it, a space and the count, in the face of
the most urgent status counted.  The robot emoji by default, a plain
Unicode symbol as other indicators of the mode line use, such as mail
counts.  A Nerd Font icon suits a mode line of Nerd Font icons, such as
\"\\U000F16A4\" (nf-md-robot_excited_outline): an icon whose ink
reaches past its advance further than a space is given room to keep it
off the count."
  :type 'string
  :group 'drover)

(defcustom drover-host-icon "\U000F048D" ; nf-md-server_network
  "Icon of the host a session runs on, before its name.
Lists show it in the host's field, beside the host's name in two lines,
alone in the sidebar, where room is short.  This host's icon is in
`drover-host-local'.  Its tooltip names the host and tells whether it
is local or connected; another host's is dimmed while connected, else
in the face `drover-host-disconnected'.
A Nerd Font icon by default; any short string will do, such as \"⇄\"."
  :type 'string
  :group 'drover)

(defcustom drover-project-icon "\U000F09DE" ; nf-md-circle_medium
  "Icon of the project of a session's task, in the project's colour.
Lists show it before the project's name, alone in two lines and in the
sidebar, in a cell lining it up (see `drover-list-fields'), for the
sessions of a task (see `drover-session-task'), whose project has a
colour of its own.  A Nerd Font icon by default; any short string will
do, such as \"●\"."
  :type 'string
  :group 'drover)

(defcustom drover-context-pie-icons
  (mapcar (lambda (slice) (string (+ #xF0A9E slice))) (number-sequence 0 7))
  "Icons of a pie filling by eighths, as a session fills its context.
The first fills an eighth, the last the whole: a context fills the
eighth it reaches into, as opencode.el's mode line shows it.  In two
lines, lists show it at the end of the title's line (see
`drover-list-fields').  Nerd Font icons by default, nf-md-circle_slice_1
to 8."
  :type '(repeat string)
  :group 'drover)

(defun drover-context-pie-glyph (fraction)
  "Return the icon of `drover-context-pie-icons' for a context FRACTION full.
FRACTION may exceed 1, past the model's context; nil without icons."
  (when drover-context-pie-icons
    (let ((count (length drover-context-pie-icons)))
      (nth (max 0 (min (1- count) (1- (ceiling (* fraction count)))))
           drover-context-pie-icons))))

;;;; The look of opencode.el

;; Drover's former options and faces name opencode.el's now.  A value
;; one of them got before this file loaded, as a configuration setting
;; them first does, or an older drover reloaded over left, would be
;; lost to the alias: a customized one goes to opencode.el's first.

(drover--adopt-option 'drover-status-glyphs 'opencode-session-status-icons)
(define-obsolete-variable-alias 'drover-status-glyphs 'opencode-session-status-icons "0.2.0")
(drover--adopt-option 'drover-animations 'opencode-session-status-animations)
(define-obsolete-variable-alias 'drover-animations 'opencode-session-status-animations "0.2.0")
(drover--adopt-option 'drover-subagent-glyphs 'opencode-subagent-icons)
(define-obsolete-variable-alias 'drover-subagent-glyphs 'opencode-subagent-icons "0.2.0")
(drover--adopt-option 'drover-archived-glyph 'opencode-archived-icon)
(define-obsolete-variable-alias 'drover-archived-glyph 'opencode-archived-icon "0.2.0")
;; Its standard value, nil, meant opencode.el's faces, the only ones now.
(drover--adopt-option 'drover-agent-faces 'opencode-agent-faces)
(define-obsolete-variable-alias 'drover-agent-faces 'opencode-agent-faces "0.2.0")
(when (and (boundp 'drover-status-labels)
           (not (eq (indirect-variable 'drover-status-labels) 'opencode-session-status-labels)))
  (makunbound 'drover-status-labels))
(define-obsolete-variable-alias 'drover-status-labels 'opencode-session-status-labels "0.2.0")

(define-obsolete-face-alias 'drover-waiting 'opencode-session-status-waiting "0.2.0")
(define-obsolete-face-alias 'drover-failed 'opencode-session-status-failed "0.2.0")
(define-obsolete-face-alias 'drover-done 'opencode-session-status-done "0.2.0")
(define-obsolete-face-alias 'drover-retrying 'opencode-session-status-retrying "0.2.0")
(define-obsolete-face-alias 'drover-working 'opencode-session-status-working "0.2.0")
(define-obsolete-face-alias 'drover-idle 'opencode-session-status-idle "0.2.0")
(define-obsolete-face-alias 'drover-open-session 'opencode-session-open "0.2.0")
(define-obsolete-face-alias 'drover-shown-session 'opencode-session-shown "0.2.0")
(define-obsolete-face-alias 'drover-active-session 'opencode-session-active "0.2.0")
(define-obsolete-face-alias 'drover-cache-bar 'opencode-cache-bar "0.2.0")
(define-obsolete-face-alias 'drover-cache-trough 'opencode-cache-trough "0.2.0")
(define-obsolete-face-alias 'drover-context-bar 'opencode-context-bar "0.2.0")
(define-obsolete-face-alias 'drover-context-near 'opencode-context-near "0.2.0")
(define-obsolete-face-alias 'drover-context-over 'opencode-context-over "0.2.0")
(define-obsolete-face-alias 'drover-prompt-picked 'opencode-setting-chosen "0.2.0")
(define-obsolete-face-alias 'drover-prompt-inherited 'opencode-setting-inherited "0.2.0")

(define-obsolete-function-alias 'drover-status-face #'opencode-session-status-face "0.2.0")
(define-obsolete-function-alias 'drover-status-label #'opencode-session-status-label "0.2.0")
(define-obsolete-function-alias 'drover-status-glyph #'opencode-session-status-glyph "0.2.0")
(define-obsolete-function-alias 'drover-format-duration #'opencode-format-duration "0.2.0")
(define-obsolete-function-alias 'drover-format-age #'opencode-format-age "0.2.0")
(define-obsolete-function-alias 'drover-format-tokens #'opencode-format-tokens "0.2.0")

(defconst drover-subagent-mode-labels
  '((all . "all") (active . "active") (none . "none"))
  "Display name of each mode of `drover-subagent-modes'.")

(defalias 'drover-face #'opencode-face-string
  "Return STRING shown in FACE, whether or not font lock runs in its buffer.
Font lock replaces `face' properties with its own in buffers where it
runs, such as `magit-section-mode' ones, and keeps `font-lock-face'.")

(defun drover-session-context-description (session)
  "Return what the context of SESSION fills, for tooltips and details, or nil.
Such as \"182k of 200k tokens (91%)\", then, on lines of their own,
whether the size is an estimate and the price tier it nears or passes,
as `opencode-context-description' tells them for opencode.el's mode
line.  A size the service estimated after a compaction, which the next
step measures, begins with \"~\".  Nil when the size is unknown."
  (when-let* ((tokens (drover-session-context-tokens session)))
    (opencode-context-description tokens (drover-session-context-limit session)
                                  (drover-session-context-tier session)
                                  (drover-session-context-estimated session))))

(defun drover-session-cache-description (session &optional now)
  "Return how the prompt cache of SESSION stands at NOW, or nil when not live.
Such as \"expires in 4m (1h window), 120k of 130k tokens cached\".  NOW
defaults to the current time."
  (when-let* ((remaining (drover-session-cache-remaining session now)))
    (let ((ttl (drover-session-cache-ttl session))
          (tokens (drover-session-cache-tokens session))
          (prompt (drover-session-cache-prompt-tokens session)))
      (concat "expires in " (opencode-format-duration remaining)
              (and (numberp ttl) (format " (%s window)" (opencode-format-duration ttl)))
              (and (numberp tokens)
                   (concat ", " (opencode-format-tokens tokens)
                           (and (numberp prompt) (concat " of " (opencode-format-tokens prompt)))
                           " tokens cached"))))))

(defconst drover-activity-labels
  '((today . "Today") (yesterday . "Yesterday") (week . "This week") (older . "Older"))
  "Display name of each bucket of last activity, from `drover-activity-buckets'.")

(defun drover-group-label (grouping value)
  "Return the display name of the group of VALUE when grouping by GROUPING.
GROUPING is one of `drover-groupings'; groups without a value, nil, are
named after what they lack, but this host's, grouping by host.
Grouping by status, the group `archived' holds the archived sessions."
  (pcase grouping
    ('status (if (eq value 'archived) "Archived" (opencode-session-status-label value)))
    ('updated (or (alist-get value drover-activity-labels) "No activity"))
    ('harness (replace-regexp-in-string "[\n\r\t]+" " " (drover-harness-name value)))
    ('task (if value
               (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" (or (cdr-safe value) value)))
             "No task"))
    ('host (replace-regexp-in-string
            "[\n\r\t]+" " "
            (concat (drover-host-label value)
                    (and value (drover-host-state value) " (not connected)"))))
    (_ (if value
           (replace-regexp-in-string "[\n\r\t]+" " " (format "%s" value))
         (format "No %s" grouping)))))

(defun drover-session-location (session)
  "Return the project and branch of SESSION as one string, or nil."
  (let ((project (drover-session-project session))
        (branch (drover-session-branch session)))
    (cond ((and project branch) (format "%s:%s" project branch))
          (project project)
          (branch branch))))

;;;; Harnesses

(defun drover-harness-glyph (harness)
  "Return the icon of HARNESS, a backend name, in the face `drover-harness'.
See `drover-harnesses'."
  (drover-face (drover-harness-icon harness) 'drover-harness))

;;;; Agents

(defun drover-agent-face (name)
  "Return the face of the agent NAME, among `opencode-agent-faces', or nil.
It is chosen by a digest of NAME, so that an agent keeps its face from
one session, one list and one Emacs to the next.  OpenCode's own
sessions show their agent as opencode.el's mode line does, its face
chosen by its place among the agents of its location."
  (when opencode-agent-faces
    (nth (mod (string-to-number (substring (md5 name) 0 6) 16) (length opencode-agent-faces))
         opencode-agent-faces)))

(defun drover--lock-faces (string)
  "Return STRING with its `face' properties also as `font-lock-face'.
Font lock, which runs in lists, replaces the first and keeps the
second (see `drover-face')."
  (let ((string (copy-sequence string))
        (position 0))
    (while (< position (length string))
      (let ((next (next-single-property-change position 'face string (length string))))
        (when-let* ((face (get-text-property position 'face string)))
          (put-text-property position next 'font-lock-face face string))
        (setq position next)))
    string))

(defun drover-agent-label (session)
  "Return the agent of SESSION as lists show it, \"@\" and its name, or nil.
The name is the backend's display of it (`drover-session-agent-display'),
else the agent in bold in its face of `drover-agent-face'.  The \"@\"
takes the face of the name, so that both read as one word, as an agent
mentioned in a prompt is spelt."
  (when-let* ((agent (drover-session-agent session))
              ((not (string-empty-p agent))))
    (let ((name (or (drover-session-agent-display session)
                    (propertize agent 'face (list 'bold (drover-agent-face agent))
                                'help-echo (concat "Agent: " agent)))))
      (drover--lock-faces
       (concat (propertize "@" 'face (get-text-property 0 'face name)
                           'help-echo (get-text-property 0 'help-echo name))
               name)))))

;;;; Links

(defvar org-store-link-plist)
(declare-function org-store-link-functions "ol" ())
(declare-function org-link-make-string "ol" (link &optional description))

(defun drover-session-link (session)
  "Return an Org link to SESSION, described by its title, or nil.
It is the link `org-store-link' would store with SESSION at point in a
list of sessions: the link comes from the first type whose store
function knows the session at point (see `drover-session-at-point'),
drover's agent-session (see `drover-links') or one of your own.  Nil
when no type does."
  (require 'ol)
  (let ((drover-session-at-point-function (lambda () session))
        (drover-selected-sessions-function nil)
        (org-store-link-plist nil)
        (stored nil))
    (dolist (function (org-store-link-functions))
      (unless stored
        (when (drover-with-reporting "session link"
                (condition-case nil
                    (funcall function nil)
                  ;; Store functions of Org before 9.7 take no argument.
                  (wrong-number-of-arguments (funcall function))))
          (setq stored (copy-sequence org-store-link-plist)))))
    (when-let* ((link (plist-get stored :link)))
      (org-link-make-string link (replace-regexp-in-string
                                  "[\n\r\t]+" " " (format "%s" (drover-session-display-title session)))))))

;;;; Measuring text

(defun drover-remap-default-face (face)
  "Draw the text of the current buffer in FACE, over the default face.
The remapping stays the buffer's own: `face-remap-add-relative' changes
the list it finds in place, and a buffer-local value made from a global
`face-remapping-alist' shares that list, so the buffer gets a copy
first.  Without it, every buffer would take FACE."
  (setq-local face-remapping-alist (copy-tree face-remapping-alist))
  (face-remap-add-relative 'default face))

(defconst drover--measure-buffer " *drover measure*"
  "Name of the buffer in which text is laid out to be measured.")

(defconst drover-measure-limit 1000
  "Characters of a text measured in pixels at most.
Laying a line out costs as much as it is long, and a line of hundreds
of kilobytes took minutes: a longer text is measured by its first
characters, its width estimated from theirs, which no window is wide
enough to show anyway.")

(defun drover-text-pixel-width (string &optional remapping window)
  "Return the width in pixels of STRING as a session view draws it.
REMAPPING is the face remapping of the view's buffer, such as its
variable pitch, and WINDOW the window whose frame's fonts draw it, the
selected one by default.  The faces of STRING count.  On a text
terminal, a pixel is a character cell."
  (cond
   ((zerop (length string)) 0)
   ((> (length string) drover-measure-limit)
    (let ((head (drover-text-pixel-width (substring string 0 drover-measure-limit) remapping window)))
      (ceiling (* head (length string)) drover-measure-limit)))
   (t
    ;; `string-pixel-width' ignores face remapping before Emacs 31.
    (with-current-buffer (get-buffer-create drover--measure-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local face-remapping-alist remapping
                    buffer-undo-list t
                    truncate-lines t
                    line-prefix nil
                    wrap-prefix nil
                    display-line-numbers nil)
        (insert string)
        (car (buffer-text-pixel-size nil (and (window-live-p window) window) t)))))))

(defun drover--face-over-default (face)
  "Return FACE, a value of the `face' property, merged over the face `default'."
  (append (if (and (proper-list-p face) (not (keywordp (car face))))
              face
            (list face))
          '(default)))

(defun drover-text-ink-extent (string &optional remapping window)
  "Return how far the drawing of STRING reaches, as (LEFT . RIGHT), in pixels.
RIGHT is how far from the start of STRING its ink or its advance
reaches, whichever is further, and LEFT how far left of its start its
ink reaches, 0 when it does not.  REMAPPING and WINDOW are as for
`drover-text-pixel-width', which measures only advances: text drawn
after STRING begins at its advance, and may be drawn under its ink.
Icon fonts draw glyphs wider than their advance: the Nerd Fonts but
their Mono variants give an icon the advance of one cell, and up to
half a cell of ink beyond it.

The font drawing each character is the one the window's frame would
pick, through its fontset when the face's font lacks the character.  A
character displayed as something else, through a `display' property,
counts by its advance alone, and so does every character on a text
terminal.  Each character measures the text before it: ink is looked
for in the first 64 characters only, glyphs and short labels."
  (let ((width (drover-text-pixel-width string remapping window))
        (string (if (> (length string) 64) (substring string 0 64) string))
        (window (if (window-live-p window) window (selected-window))))
    (if (not (display-graphic-p (window-frame window)))
        (cons 0 width)
      (let ((left 0)
            (right width)
            ;; `font-at' merges the faces of a string over the frame's
            ;; default face, not over the buffer's remapping of it; named,
            ;; `default' takes the remapping, as in the view's buffer.
            (faced (copy-sequence string)))
        (dotimes (index (length string))
          (put-text-property index (1+ index) 'face
                             (drover--face-over-default (get-text-property index 'face string))
                             faced))
        (with-current-buffer (get-buffer-create drover--measure-buffer)
          (setq-local face-remapping-alist remapping)
          (dotimes (index (length string))
            (unless (get-text-property index 'display string)
              (when-let* ((font (font-at index window faced))
                          (glyph (aref (font-get-glyphs font index (1+ index) faced) 0)))
                (let ((x (drover-text-pixel-width (substring string 0 index) remapping window)))
                  ;; Bearings run from the glyph's origin to its ink's edges.
                  (setq left (max left (- (+ x (aref glyph 5))))
                        right (max right (+ x (aref glyph 6)))))))))
        (cons left right)))))

(defun drover-truncate-to-pixels (string pixels measure)
  "Return STRING cut to fit PIXELS wide, as MEASURE measures it.
MEASURE is a function of a string returning its width in pixels.  A
cut string loses its trailing blanks and ends in an ellipsis in the
properties of the text before it; when not even the ellipsis fits, the
result is empty.  A text longer than `drover-measure-limit' characters
is cut to those first: no window shows more."
  (let* ((string (if (> (length string) drover-measure-limit)
                     (concat (substring string 0 drover-measure-limit) (truncate-string-ellipsis))
                   string))
         (width (if (zerop (length string)) 0 (funcall measure string))))
    (if (<= width pixels)
        string
      (let* ((ellipsis (truncate-string-ellipsis))
             (ellipses nil)
             (cut (lambda (length)
                    (let ((properties (text-properties-at (max 0 (1- length)) string)))
                      (concat (string-trim-right (substring string 0 length))
                              ;; One ellipsis per properties: titles have one run.
                              (or (cdr (assq properties ellipses))
                                  (cdar (push (cons properties (apply #'propertize ellipsis properties))
                                              ellipses)))))))
             ;; The candidate that fits, of the longest length tried.
             (best nil)
             (fits (lambda (length)
                     (let ((candidate (funcall cut length)))
                       (when (<= (funcall measure candidate) pixels)
                         (when (or (null best) (> length (car best)))
                           (setq best (cons length candidate)))
                         t))))
             (low 0)
             (high (1- (length string))))
        ;; Characters differ in width, so the longest prefix that fits is
        ;; found by laying out candidates.  Each costs a layout: the
        ;; search starts where the average width of a character puts the
        ;; cut, and gallops away from it, before bisecting.  The empty
        ;; prefix, assumed to fit, is checked last, when nothing longer
        ;; does.
        (let ((guess (max 0 (min high (floor (* (length string) pixels) (max 1 width)))))
              (step 1))
          (if (funcall fits guess)
              (progn
                (setq low guess)
                (while (and (<= (+ low step) high) (funcall fits (+ low step)))
                  (setq low (+ low step)
                        step (* 2 step)))
                (setq high (min high (+ low step -1))))
            (setq high (1- guess))
            (while (and (> (- high step) low) (not (funcall fits (- high step))))
              (setq high (- high step 1)
                    step (* 2 step)))
            (when (> (- high step) low)
              (setq low (- high step)))))
        (while (< low high)
          (let ((middle (/ (+ low high 1) 2)))
            (if (funcall fits middle)
                (setq low middle)
              (setq high (1- middle)))))
        (cond ((and best (= (car best) low)) (cdr best))
              ((funcall fits low) (cdr best))
              (t ""))))))

;;;; Filters

(defun drover-filter-since-label (since &optional now)
  "Return how a filter on activity since SINCE, a float time, reads.
It is \"today\" for the start of the day of NOW, the current time by
default, and otherwise the age of SINCE, such as \"2d\"."
  (if (= since (drover--start-of-day now))
      "today"
    (concat "since " (opencode-format-age since now))))

(defun drover-filter-statuses-label (statuses)
  "Return the display names of STATUSES as one string, such as \"done or failed\"."
  (mapconcat (lambda (status) (downcase (opencode-session-status-label status)))
             statuses " or "))

(defun drover-filter-selection-label (key selection)
  "Return a readable label for categorical filter SELECTION of KEY."
  (if (equal selection '(:include))
      "none"
    (concat
     (and (eq (car-safe selection) :exclude) "except ")
     (mapconcat
      (lambda (value)
        (cond
         ((null value) "(unknown)")
         ((eq key :statuses) (downcase (opencode-session-status-label value)))
         ((eq key :harness) (drover-harness-name value))
         ((eq key :host) (drover-host-label (unless (eq value 'local) value)))
         (t (format "%s" value))))
      (drover-filter-selection-values selection) " or "))))

(defun drover-filter-hosts-label (hosts)
  "Return HOSTS, a value of the filter member :host, as one string.
Such as \"on reaver or core\": `local' names this host."
  (concat "on " (drover-filter-selection-label :host hosts)))

(defun drover-filter-description (&optional filter now)
  "Return FILTER, `drover-filter' by default, described for headers, or nil.
The description lists each member set, such as \"project drover,
needs you, agent build, provider Anthropic, harness Claude IDE, since
2d, \"auth\", on core\".  Ages count up to NOW, the current time by
default.  Return nil when FILTER lists every session."
  (let ((filter (or filter drover-filter)))
    (when (drover-filter-active-p filter)
      (let ((project (drover--filter-value filter :project))
            (statuses (drover--filter-value filter :statuses))
            (agent (drover--filter-value filter :agent))
            (model (drover--filter-value filter :model))
            (provider (drover--filter-value filter :provider))
            (harness (drover--filter-value filter :harness))
            (since (drover--filter-value filter :since))
            (search (drover--filter-value filter :search))
            (hosts (drover--filter-value filter :host)))
        (mapconcat #'identity
                   (delq nil (list (and project (concat "project " (drover-filter-selection-label :project project)))
                                   (and statuses (drover-filter-selection-label :statuses statuses))
                                   (and agent (concat "agent " (drover-filter-selection-label :agent agent)))
                                   (and model (concat "model " (drover-filter-selection-label :model model)))
                                   (and provider (concat "provider " (drover-filter-selection-label :provider provider)))
                                   (and harness (concat "harness " (drover-filter-selection-label :harness harness)))
                                   (and since (drover-filter-since-label since now))
                                   (and search (format "\"%s\"" search))
                                   (and hosts (drover-filter-hosts-label hosts))))
                   ", ")))))

(provide 'drover-ui)
;;; drover-ui.el ends here
