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
;; and describes the filter in force the same way.  The commands and
;; keys every view shares are in `drover-keys'.

;;; Code:

(require 'cl-lib)
(require 'drover-core)
(require 'face-remap)

(defgroup drover-faces nil
  "Faces of drover."
  :group 'drover
  :group 'faces)

(defface drover-waiting '((t :inherit warning :weight bold))
  "Face of sessions waiting for the user."
  :group 'drover-faces)

(defface drover-failed '((t :inherit error))
  "Face of sessions whose turn failed."
  :group 'drover-faces)

(defface drover-done '((t :inherit success))
  "Face of sessions whose turn ended unseen."
  :group 'drover-faces)

(defface drover-retrying '((t :inherit warning))
  "Face of sessions waiting to retry."
  :group 'drover-faces)

(defface drover-working '((t :inherit font-lock-keyword-face))
  "Face of running sessions."
  :group 'drover-faces)

(defface drover-idle '((t :inherit shadow))
  "Face of idle sessions."
  :group 'drover-faces)

(defface drover-heading '((t :inherit bold))
  "Face of group headings."
  :group 'drover-faces)

(defface drover-dim '((t :inherit shadow))
  "Face of secondary details, such as directories and ages."
  :group 'drover-faces)

;; The title of a session tells where it is in Emacs: in no buffer, in a
;; buffer no window shows, in a window, or in the window the user works
;; in.  The three faces inherit sibling faces of font lock, which themes
;; tell apart from one another and from the default foreground, and
;; which no status takes but `drover-working', a glyph rather than a
;; title: `success', `warning' and `error' would read as a status.

(defface drover-open-session '((t :inherit font-lock-keyword-face))
  "Face of the titles of sessions open in a buffer no window shows.
It goes over the other faces of a title, such as the dimming of idle
sessions, which keep the attributes it does not set.  See also
`drover-shown-session' and `drover-active-session'."
  :group 'drover-faces)

(defface drover-shown-session '((t :inherit font-lock-type-face))
  "Face of the titles of sessions shown in a window of a visible frame.
As `drover-open-session', it goes over the other faces of a title."
  :group 'drover-faces)

(defface drover-active-session '((t :inherit font-lock-function-name-face))
  "Face of the title of the session of the window the user works in.
That is the selected window, or, while a list of sessions or the
minibuffer is selected, the window selected before.  It inherits the
face `which-function-mode' names the definition point is in with: where
the user is.  As `drover-open-session', it goes over the other faces of
a title."
  :group 'drover-faces)

(defface drover-cache-bar '((t :inherit success))
  "Face of the part of a prompt cache's lifetime still left.
Its foreground draws the bar: short bars fill with it, in inverse video,
and the line of a session in two lines is underlined in it, so that the
bar shows over the highlight of the current line and the region, which
change only the background.  It inherits `success': the cache is still
there to save the next turn's cost."
  :group 'drover-faces)

(defface drover-cache-trough '((t :inherit drover-dim :underline t))
  "Face of the part of a prompt cache's lifetime already gone, in short bars.
It shows how long the bar would be, without drawing attention."
  :group 'drover-faces)

(defface drover-context-bar
  '((((class color) (min-colors 88) (background light)) :background "#ebe3f2")
    (((class color) (min-colors 88) (background dark)) :background "#382d45")
    (((class color)) :background "magenta")
    (t :underline t))
  "Face of the part of a model's context a session's context fills, in short bars.
Only a background, subtle beside the prompt cache's bar, which takes
the colour of `drover-cache-bar', so that the two bars of a line are
told apart."
  :group 'drover-faces)

(defface drover-context-near '((t :inherit warning))
  "Face of a context size nearing a long-context price tier.
Bars draw it in inverse video: its foreground as their background."
  :group 'drover-faces)

(defface drover-context-over '((t :inherit error))
  "Face of a context size past a long-context price tier, whose requests cost more.
Bars draw it in inverse video: its foreground as their background."
  :group 'drover-faces)

(defface drover-prompt-picked '((t :inherit success))
  "Face of a setting the user chose, such as the model or the effort.
The prompt's header and pickers show settings in it or in
`drover-prompt-inherited', and session lists the effort a session runs."
  :group 'drover-faces)

(defface drover-prompt-inherited '((t :inherit shadow))
  "Face of a setting inherited rather than chosen, such as a default effort.
The prompt shows in it what a new session would inherit, and session
lists the effort a session runs by default."
  :group 'drover-faces)

(defcustom drover-status-glyphs
  '((waiting . "\U000F02D7")            ; nf-md-help_circle
    (failed . "\uf057")                 ; nf-fa-times_circle
    (done . "\U000F05E0")               ; nf-md-check_circle
    (retrying . "\uf021")               ; nf-fa-refresh
    (working . "\U000F051F")            ; nf-md-timer_sand
    (idle . "\uf10c"))                  ; nf-fa-circle_o
  "Glyph of each session status, Nerd Font icons by default.
Session lists animate some of them; see `drover-animations'."
  :type '(alist :key-type symbol :value-type string)
  :group 'drover)

(defcustom drover-subagent-glyphs
  '((waiting . "\U000F169F")            ; nf-md-robot_confused
    (working . "\U000F06A9"))           ; nf-md-robot
  "Glyphs telling that subagents of a session are in the middle of a turn.
Session lists show them beside the status of the session, shown or
not: `waiting' when subagents wait for you, `working' when subagents
work or wait to retry.  They stay still and tell no number, which their
tooltip tells.  Nerd Font icons by default."
  :type '(alist :key-type (choice (const waiting) (const working)) :value-type string)
  :group 'drover)

(defcustom drover-archived-glyph "\U000F003C" ; nf-md-archive
  "Glyph telling that a session is archived.
Session lists show archived sessions, while they show them at all (see
`drover-show-archived'), with it after their status, their title
dimmed.  A Nerd Font icon by default."
  :type 'string
  :group 'drover)

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

(defcustom drover-animations
  ;; nf-md-moon_new, waxing_crescent, first_quarter, waxing_gibbous,
  ;; full, waning_gibbous, last_quarter, waning_crescent
  '((working "\U000F0F64" "\U000F0F67" "\U000F0F61" "\U000F0F68"
             "\U000F0F62" "\U000F0F66" "\U000F0F63" "\U000F0F65")
    (retrying "\U000F144A" "\U000F1441" "\U000F1444" "\U000F1447")) ; nf-md-clock_time_twelve, three, six, nine
  "Frames of the status glyphs session lists animate, shown in turn.
Each entry is (KIND FRAME...), each FRAME a string.  KIND is `working'
or `retrying': sessions of that status show the frames in place of
their glyph of `drover-status-glyphs'.  A kind without frames stays
still.  Each frame takes the room of the widest, so that nothing after
it moves.  Nerd Font icons by default: the phases of the moon, and a
clock whose hand turns.  Each frame differs little from the one before,
so the cycle reads as motion; a lone frame much darker or lighter than
the others, such as a full hourglass among outlined ones, reads as a
blink.
`drover-list-animation-interval' sets the pace, or stops every
animation."
  :type '(alist :key-type (choice (const working) (const retrying))
                :value-type (repeat string))
  :group 'drover)

(defconst drover-subagent-mode-labels
  '((all . "all") (active . "active") (none . "none"))
  "Display name of each mode of `drover-subagent-modes'.")

(defconst drover-status-labels
  '((waiting . "Needs you") (failed . "Failed") (done . "Done") (retrying . "Retrying")
    (working . "Working") (idle . "Idle"))
  "Display name of each session status.")

(defun drover-status-face (status)
  "Return the face of STATUS."
  (pcase status
    ('waiting 'drover-waiting) ('failed 'drover-failed) ('done 'drover-done)
    ('retrying 'drover-retrying) ('working 'drover-working) (_ 'drover-idle)))

(defun drover-status-label (status)
  "Return the display name of STATUS."
  (or (alist-get status drover-status-labels) (capitalize (symbol-name status))))

(defun drover-face (string face)
  "Return STRING shown in FACE, whether or not font lock runs in its buffer.
Font lock replaces `face' properties with its own in buffers where it
runs, such as `magit-section-mode' ones, and keeps `font-lock-face'."
  (propertize string 'face face 'font-lock-face face))

(defun drover-status-glyph (status)
  "Return the glyph of STATUS in its face."
  (drover-face (or (alist-get status drover-status-glyphs) "?")
               (drover-status-face status)))

(defun drover-format-duration (seconds)
  "Return SECONDS as \"42s\", \"5m\", \"3h\" or \"2d\", rounded down.
A negative duration counts as none."
  (let ((seconds (max 0 seconds)))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(defun drover-format-age (time &optional now)
  "Return the age of TIME, a float time, as \"42s\", \"5m\", \"3h\" or \"2d\".
NOW defaults to the current time.  Return \"\" for a nil TIME."
  (if (null time)
      ""
    (drover-format-duration (- (or now (float-time)) time))))

(defun drover-format-tokens (tokens)
  "Return a count of TOKENS as \"950\", \"8.4k\", \"182k\" or \"1.2M\".
Counts of thousands and millions keep one decimal below ten, none
from ten on; a decimal of zero is dropped."
  (cl-flet ((scaled (value unit)
              (if (< value 9.95)
                  (concat (string-remove-suffix ".0" (format "%.1f" value)) unit)
                (format "%d%s" (round value) unit))))
    (cond ((< tokens 1000) (format "%d" tokens))
          ((< tokens 999500) (scaled (/ tokens 1000.0) "k"))
          (t (scaled (/ tokens 1000000.0) "M")))))

(defun drover-session-context-description (session)
  "Return what the context of SESSION fills, for tooltips and details, or nil.
Such as \"182k of 200k tokens (91%)\", then, on lines of their own,
whether the size is an estimate and the price tier it nears or passes.
A size the service estimated after a compaction, which the next step
measures, begins with \"~\".  Nil when the size is unknown."
  (when-let* ((tokens (drover-session-context-tokens session)))
    (let ((limit (drover-session-context-limit session))
          (fraction (drover-session-context-fraction session))
          (estimated (drover-session-context-estimated session)))
      (concat (and estimated "~")
              (drover-format-tokens tokens)
              (and limit (concat " of " (drover-format-tokens limit)))
              " tokens"
              (and fraction (format " (%d%%)" (round (* 100 fraction))))
              (and estimated "\nEstimated by the service after compaction; the next step measures it")
              (pcase (drover-session-context-tier session)
                (`(over ,size ,label)
                 (format "\n%s %s tokens: requests cost %s"
                         (if estimated "Estimated past" "Past") (drover-format-tokens size) label))
                (`(near ,size ,label)
                 (format "\n%s %s tokens, past which requests cost %s"
                         (if estimated "Estimated near" "Near") (drover-format-tokens size) label)))))))

(defun drover-session-cache-description (session &optional now)
  "Return how the prompt cache of SESSION stands at NOW, or nil when not live.
Such as \"expires in 4m (1h window), 120k of 130k tokens cached\".  NOW
defaults to the current time."
  (when-let* ((remaining (drover-session-cache-remaining session now)))
    (let ((ttl (drover-session-cache-ttl session))
          (tokens (drover-session-cache-tokens session))
          (prompt (drover-session-cache-prompt-tokens session)))
      (concat "expires in " (drover-format-duration remaining)
              (and (numberp ttl) (format " (%s window)" (drover-format-duration ttl)))
              (and (numberp tokens)
                   (concat ", " (drover-format-tokens tokens)
                           (and (numberp prompt) (concat " of " (drover-format-tokens prompt)))
                           " tokens cached"))))))

(defconst drover-activity-labels
  '((today . "Today") (yesterday . "Yesterday") (week . "This week") (older . "Older"))
  "Display name of each bucket of last activity, from `drover-activity-buckets'.")

(defun drover-group-label (grouping value)
  "Return the display name of the group of VALUE when grouping by GROUPING.
GROUPING is one of `drover-groupings'; groups without a value, nil, are
named after what they lack.  Grouping by status, the group `archived'
holds the archived sessions."
  (pcase grouping
    ('status (if (eq value 'archived) "Archived" (drover-status-label value)))
    ('updated (or (alist-get value drover-activity-labels) "No activity"))
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

;;;; Agents

(defconst drover--agent-faces
  '(font-lock-keyword-face font-lock-type-face font-lock-constant-face font-lock-string-face)
  "Faces agents take without `drover-agent-faces' nor opencode.el.
The defaults of `opencode-mode-line-agent-faces'.")

(defcustom drover-agent-faces nil
  "Faces in which lists show agents whose backend gives no display of its own.
An agent takes one by its name, always the same.  Nil takes those of
opencode.el's mode line, `opencode-mode-line-agent-faces', once it is
loaded, so that agents of every backend draw from one set of faces;
OpenCode's own sessions show their agent as that mode line does."
  :type '(choice (const :tag "Those of opencode.el's mode line" nil) (repeat face))
  :group 'drover-faces)

(defun drover-agent-face (name)
  "Return the face of the agent NAME, among `drover-agent-faces'.
It is chosen by a digest of NAME, so that an agent keeps its face from
one session, one list and one Emacs to the next."
  (let ((faces (or drover-agent-faces
                   (bound-and-true-p opencode-mode-line-agent-faces)
                   drover--agent-faces)))
    (nth (mod (string-to-number (substring (md5 name) 0 6) 16) (length faces)) faces)))

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
list of sessions: drover defines no link type of its own, so the link
comes from the first type whose store function knows the session at
point (see `drover-session-at-point'), such as one of your own.  Nil
when no type does."
  (require 'ol)
  (let ((drover-session-at-point-function (lambda () session))
        (drover-selected-sessions-function nil)
        (org-store-link-plist nil)
        (stored nil))
    (dolist (function (org-store-link-functions))
      (unless stored
        (when (with-demoted-errors "Drover session link: %S"
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

(defun drover-text-pixel-width (string &optional remapping window)
  "Return the width in pixels of STRING as a session view draws it.
REMAPPING is the face remapping of the view's buffer, such as its
variable pitch, and WINDOW the window whose frame's fonts draw it, the
selected one by default.  The faces of STRING count.  On a text
terminal, a pixel is a character cell."
  (if (zerop (length string))
      0
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
        (car (buffer-text-pixel-size nil (and (window-live-p window) window) t))))))

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
terminal."
  (let ((width (drover-text-pixel-width string remapping window))
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
result is empty."
  (if (or (zerop (length string)) (<= (funcall measure string) pixels))
      string
    (let* ((ellipsis (truncate-string-ellipsis))
           (cut (lambda (length)
                  (concat (string-trim-right (substring string 0 length))
                          (apply #'propertize ellipsis
                                 (text-properties-at (max 0 (1- length)) string)))))
           (fits (lambda (length) (<= (funcall measure (funcall cut length)) pixels)))
           (low 0)
           (high (1- (length string))))
      (if (not (funcall fits 0))
          ""
        ;; Characters differ in width, so the longest prefix that fits is
        ;; found by bisection, laying out each candidate.
        (while (< low high)
          (let ((middle (/ (+ low high 1) 2)))
            (if (funcall fits middle)
                (setq low middle)
              (setq high (1- middle)))))
        (funcall cut low)))))

;;;; Filters

(defun drover-filter-since-label (since &optional now)
  "Return how a filter on activity since SINCE, a float time, reads.
It is \"today\" for the start of the day of NOW, the current time by
default, and otherwise the age of SINCE, such as \"2d\"."
  (if (= since (drover--start-of-day now))
      "today"
    (concat "since " (drover-format-age since now))))

(defun drover-filter-statuses-label (statuses)
  "Return the display names of STATUSES as one string, such as \"done or failed\"."
  (mapconcat (lambda (status) (downcase (drover-status-label status)))
             statuses " or "))

(defun drover-filter-description (&optional filter now)
  "Return FILTER, `drover-filter' by default, described for headers, or nil.
The description lists each member set, such as \"project drover,
needs you, agent build, provider Anthropic, since 2d, \"auth\"\".  Ages
count up to NOW, the current time by default.  Return nil when FILTER
lists every session."
  (let ((filter (or filter drover-filter)))
    (when (drover-filter-active-p filter)
      (let ((project (drover--filter-value filter :project))
            (statuses (drover--filter-value filter :statuses))
            (agent (drover--filter-value filter :agent))
            (model (drover--filter-value filter :model))
            (provider (drover--filter-value filter :provider))
            (since (drover--filter-value filter :since))
            (search (drover--filter-value filter :search)))
        (mapconcat #'identity
                   (delq nil (list (and project (concat "project " project))
                                   (and statuses (drover-filter-statuses-label statuses))
                                   (and agent (concat "agent " agent))
                                   (and model (concat "model " model))
                                   (and provider (concat "provider " provider))
                                   (and since (drover-filter-since-label since now))
                                   (and search (format "\"%s\"" search))))
                   ", ")))))

(provide 'drover-ui)
;;; drover-ui.el ends here
