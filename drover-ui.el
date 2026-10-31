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
;; with the same glyphs, faces and labels, and acts on them with the same
;; commands and keys.  A view sets `drover-session-at-point-function' in
;; its buffer and uses `drover-session-map' as the parent of its keymap.

;;; Code:

(require 'drover-core)

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

(defun drover-session-display-title (session)
  "Return the title of SESSION, or its identifier."
  (or (drover-session-title session) (drover-session-id session)))

(defun drover-session-summary (session)
  "Return one line on why SESSION has its status, or nil.
A waiting session shows its oldest request, another its detail."
  (if-let* ((request (car (drover-session-requests session))))
      (concat (if (eq (plist-get request :kind) 'question) "asks: " "permit: ")
              (plist-get request :summary))
    (drover-session-detail session)))

(defun drover-session-location (session)
  "Return the project and branch of SESSION as one string, or nil."
  (let ((project (drover-session-project session))
        (branch (drover-session-branch session)))
    (cond ((and project branch) (format "%s:%s" project branch))
          (project project)
          (branch branch))))

;;;; Measuring text

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

;;;; The session at point

(defvar-local drover-session-at-point-function nil
  "Function of no argument returning the session at point, or nil.
Every view of sessions sets it in its buffer.")

(defun drover-session-at-point ()
  "Return the session at point, or signal a `user-error'."
  (or (and drover-session-at-point-function
           (funcall drover-session-at-point-function))
      (user-error "No session at point")))

(defun drover-open-at-point (&optional other-window)
  "Display the session at point; with OTHER-WINDOW, in another window."
  (interactive "P")
  (drover-open (drover-session-at-point) other-window))

(defun drover-open-other-window-at-point ()
  "Display the session at point in another window."
  (interactive)
  (drover-open (drover-session-at-point) t))

(defun drover-respond-at-point ()
  "Answer the oldest request of the session at point."
  (interactive)
  (drover-respond (drover-session-at-point)))

(defun drover-interrupt-at-point ()
  "Stop the running turn of the session at point."
  (interactive)
  (let ((session (drover-session-at-point)))
    (when (y-or-n-p (format "Interrupt %s? " (drover-session-display-title session)))
      (drover-interrupt session))))

(defun drover-archive-at-point ()
  "Archive the session at point."
  (interactive)
  (let ((session (drover-session-at-point)))
    (when (y-or-n-p (format "Archive %s? " (drover-session-display-title session)))
      (drover-archive session))))

(defun drover-mark-seen-at-point ()
  "Mark the ended turn of the session at point seen."
  (interactive)
  (drover-mark-seen (drover-session-at-point)))

(defconst drover-peek-buffer "*drover peek*"
  "Name of the buffer showing a session's preview.")

(defun drover-peek-at-point ()
  "Show where the session at point stands, without visiting it."
  (interactive)
  (let ((session (drover-session-at-point)))
    (drover-preview
     session
     (lambda (text)
       (with-current-buffer (get-buffer-create drover-peek-buffer)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize (drover-session-display-title session) 'face 'drover-heading)
                   "\n\n"
                   (or text "Nothing to show."))
           (goto-char (point-min))
           (special-mode)))
       (display-buffer drover-peek-buffer
                       '((display-buffer-reuse-window display-buffer-below-selected)
                         (window-height . 0.3)))))))

(defvar-keymap drover-session-map
  :doc "Keys acting on the session at point, shared by every view."
  "RET" #'drover-open-at-point
  "o" #'drover-open-other-window-at-point
  "a" #'drover-respond-at-point
  "C-c C-q" #'drover-respond-at-point
  "k" #'drover-interrupt-at-point
  "A" #'drover-archive-at-point
  "s" #'drover-mark-seen-at-point
  "SPC" #'drover-peek-at-point)

(provide 'drover-ui)
;;; drover-ui.el ends here
