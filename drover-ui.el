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

;; Every view of sessions (dashboards, sidebar, completion) draws them
;; with the same glyphs, faces and labels, measures text the same way,
;; and describes the filter in force the same way.  The commands and
;; keys every view shares are in `drover-keys'.

;;; Code:

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

(defcustom drover-status-glyphs
  '((waiting . "\U000F02D7")            ; nf-md-help_circle
    (failed . "\uf057")                 ; nf-fa-times_circle
    (done . "\U000F05E0")               ; nf-md-check_circle
    (retrying . "\uf021")               ; nf-fa-refresh
    (working . "\U000F051F")            ; nf-md-timer_sand
    (idle . "\uf10c"))                  ; nf-fa-circle_o
  "Glyph of each session status, Nerd Font icons by default."
  :type '(alist :key-type symbol :value-type string)
  :group 'drover)

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

(defun drover-format-age (time &optional now)
  "Return the age of TIME, a float time, as \"42s\", \"5m\", \"3h\" or \"2d\".
NOW defaults to the current time.  Return \"\" for a nil TIME."
  (if (null time)
      ""
    (let ((seconds (max 0 (- (or now (float-time)) time))))
      (cond ((< seconds 60) (format "%ds" seconds))
            ((< seconds 3600) (format "%dm" (/ seconds 60)))
            ((< seconds 86400) (format "%dh" (/ seconds 3600)))
            (t (format "%dd" (/ seconds 86400)))))))

(defun drover-session-location (session)
  "Return the project and branch of SESSION as one string, or nil."
  (let ((project (drover-session-project session))
        (branch (drover-session-branch session)))
    (cond ((and project branch) (format "%s:%s" project branch))
          (project project)
          (branch branch))))

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

(defun drover--start-of-day (&optional time)
  "Return the float time at which the day of TIME began, today by default."
  (pcase-let ((`(,_ ,_ ,_ ,day ,month ,year) (decode-time time)))
    ;; Midnight may fall on the other side of a daylight saving change.
    (float-time (encode-time (list 0 0 0 day month year nil -1 nil)))))

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
The description lists each member set, such as
\"project drover, needs you, since 2d, \"auth\"\".  Ages count up to
NOW, the current time by default.  Return nil when FILTER lists every
session."
  (let ((filter (or filter drover-filter)))
    (when (drover-filter-active-p filter)
      (let ((project (drover--filter-value filter :project))
            (statuses (drover--filter-value filter :statuses))
            (since (drover--filter-value filter :since))
            (search (drover--filter-value filter :search)))
        (mapconcat #'identity
                   (delq nil (list (and project (concat "project " project))
                                   (and statuses (drover-filter-statuses-label statuses))
                                   (and since (drover-filter-since-label since now))
                                   (and search (format "\"%s\"" search))))
                   ", ")))))

(provide 'drover-ui)
;;; drover-ui.el ends here
