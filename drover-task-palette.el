;;; drover-task-palette.el --- Colours of the projects of tasks, from the theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: faces, convenience

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

;; Each project of a task (see `drover-task') has a colour of its own,
;; taken from the theme in force: the palette is made of the colours
;; the theme gives its standard faces, Font Lock's and ANSI's first,
;; then every other colour the enabled themes name, those readable over
;; the default background and distinct enough from one another, as
;; OKLab measures them; when projects outnumber them, more are made in
;; the palette's own lightness and chroma, of the hues furthest from
;; those it has.  A project keeps its place in the palette, its slot,
;; once given, kept in `drover-state-directory': a theme changing
;; recolours every project alike, never reshuffling them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'color)
(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 20))
  (load "drover-core" nil t))

(defcustom drover-task-palette-faces
  '(font-lock-keyword-face font-lock-function-name-face font-lock-string-face
    font-lock-type-face font-lock-constant-face font-lock-variable-name-face
    font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face
    font-lock-doc-face font-lock-regexp-grouping-construct
    ansi-color-red ansi-color-green ansi-color-yellow ansi-color-blue
    ansi-color-magenta ansi-color-cyan ansi-color-bright-red ansi-color-bright-green
    ansi-color-bright-yellow ansi-color-bright-blue ansi-color-bright-magenta
    ansi-color-bright-cyan)
  "Faces whose foregrounds come first in the palette of projects, in order.
The theme in force gives them their colours; the other colours its
settings name come after them."
  :type '(repeat face)
  :group 'drover)

(defcustom drover-task-palette-distance 0.09
  "How far apart two colours of the palette of projects are at least, in OKLab.
A colour of the theme closer than this to one taken before is left
out, as one made for more projects is."
  :type 'number
  :group 'drover)

(defcustom drover-task-palette-chroma 0.05
  "Chroma a colour of the theme needs, in OKLCH, to colour a project.
Greys and near greys, which tell nothing apart, are left out."
  :type 'number
  :group 'drover)

(defcustom drover-task-palette-contrast 0.22
  "Lightness a colour differs from the default background by at least, in OKLab.
Colours of backgrounds, which would not read as text, are left out."
  :type 'number
  :group 'drover)

(defconst drover-task-palette--version 1
  "Version of the format of the file of the slots of projects.")

(defconst drover-task-palette-limit 500
  "Projects whose slots the file keeps, those seen last.")

;;;; Colours

(defun drover-task-palette--rgb (color)
  "Return COLOR, a name or \"#RRGGBB\", as three floats in [0, 1], or nil."
  (when (stringp color)
    (cond
     ((string-match "\\`#\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'" color)
      (mapcar (lambda (group) (/ (string-to-number (match-string group color) 16) 255.0)) '(1 2 3)))
     ((string-match "\\`#\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\'" color)
      (mapcar (lambda (group) (/ (string-to-number (match-string group color) 16) 15.0)) '(1 2 3)))
     ((string-match-p "\\`#\\|\\`unspecified" color) nil)
     (t (ignore-errors (color-name-to-rgb color))))))

(defun drover-task-palette--linear (channel)
  "Return the sRGB CHANNEL, in [0, 1], as linear light."
  (if (<= channel 0.04045)
      (/ channel 12.92)
    (expt (/ (+ channel 0.055) 1.055) 2.4)))

(defun drover-task-palette--gamma (channel)
  "Return the linear CHANNEL as sRGB, unclamped."
  (if (<= channel 0.0031308)
      (* 12.92 channel)
    (- (* 1.055 (expt (max channel 0.0) (/ 1 2.4))) 0.055)))

(defun drover-task-palette-oklab (rgb)
  "Return RGB, sRGB in [0, 1], as OKLab, a list (L A B)."
  (pcase-let* ((`(,r ,g ,b) (mapcar #'drover-task-palette--linear rgb))
               (l (expt (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)) (/ 1.0 3)))
               (m (expt (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)) (/ 1.0 3)))
               (s (expt (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)) (/ 1.0 3))))
    (list (+ (* 0.2104542553 l) (* 0.7936177850 m) (* -0.0040720468 s))
          (+ (* 1.9779984951 l) (* -2.4285922050 m) (* 0.4505937099 s))
          (+ (* 0.0259040371 l) (* 0.7827717662 m) (* -0.8086757660 s)))))

(defun drover-task-palette--from-oklab (lab)
  "Return LAB, OKLab, as linear sRGB, unclamped."
  (pcase-let* ((`(,l* ,a ,b) lab)
               (l (expt (+ l* (* 0.3963377774 a) (* 0.2158037573 b)) 3))
               (m (expt (+ l* (* -0.1055613458 a) (* -0.0638541728 b)) 3))
               (s (expt (+ l* (* -0.0894841775 a) (* -1.2914855480 b)) 3)))
    (list (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s))
          (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s))
          (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s)))))

(defun drover-task-palette--oklch (l c h)
  "Return the colour of OKLCH L, C and H, hue in degrees, as \"#RRGGBB\".
A colour out of the sRGB gamut loses chroma until it is in."
  (let ((chroma c)
        (rgb nil))
    (while (progn
             (setq rgb (drover-task-palette--from-oklab
                        (list l (* chroma (cos (degrees-to-radians h))) (* chroma (sin (degrees-to-radians h))))))
             (and (> chroma 0.001)
                  (seq-some (lambda (channel) (or (< channel -0.0001) (> channel 1.0001))) rgb)))
      (setq chroma (* chroma 0.95)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (channel)
                     (round (* 255 (min 1.0 (max 0.0 (drover-task-palette--gamma channel))))))
                   rgb))))

(defun drover-task-palette-delta (a b)
  "Return how far apart OKLab colours A and B are."
  (sqrt (apply #'+ (cl-mapcar (lambda (x y) (expt (- x y) 2)) a b))))

(defun drover-task-palette--chroma (lab)
  "Return the chroma of LAB, OKLab."
  (sqrt (+ (expt (nth 1 lab) 2) (expt (nth 2 lab) 2))))

(defun drover-task-palette--hue (lab)
  "Return the hue of LAB, OKLab, in degrees."
  (mod (radians-to-degrees (atan (nth 2 lab) (nth 1 lab))) 360))

;;;; The palette of the theme

(defun drover-task-palette--background ()
  "Return the default background, as OKLab."
  (drover-task-palette-oklab
   (or (drover-task-palette--rgb (face-background 'default nil t))
       (if (eq (frame-parameter nil 'background-mode) 'dark) '(0.0 0.0 0.0) '(1.0 1.0 1.0)))))

(defun drover-task-palette--theme-colors (value)
  "Return the colours VALUE, a theme's settings, names, in order."
  (let ((colors nil)
        (seen 0))
    (named-let walk ((tree value))
      (cond ((> (cl-incf seen) 100000))
            ((stringp tree)
             (when (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" tree)
               (push (downcase tree) colors)))
            ((consp tree)
             (when (and (memq (car tree) '(:foreground :color :distant-foreground))
                        (stringp (car-safe (cdr tree))))
               (push (cadr tree) colors))
             (walk (car tree))
             (walk (cdr tree)))
            ((vectorp tree) (mapc #'walk tree))))
    (nreverse colors)))

(defun drover-task-palette--candidates ()
  "Return the colours the theme in force offers projects, as names, in order."
  (append (delq nil (mapcar (lambda (face)
                              (and (facep face) (face-foreground face nil t)))
                            drover-task-palette-faces))
          (mapcan (lambda (theme)
                    (drover-task-palette--theme-colors (get theme 'theme-settings)))
                  (reverse custom-enabled-themes))))

(defun drover-task-palette-make (candidates background)
  "Return the palette of CANDIDATES, colour names, over BACKGROUND, OKLab.
A vector of (NAME . LAB): the candidates of enough chroma, far enough
in lightness from BACKGROUND, each far enough from those before (see
`drover-task-palette-chroma', `drover-task-palette-contrast' and
`drover-task-palette-distance')."
  (let ((kept nil))
    (dolist (name candidates)
      (when-let* ((rgb (drover-task-palette--rgb name))
                  (lab (drover-task-palette-oklab rgb)))
        (when (and (>= (drover-task-palette--chroma lab) drover-task-palette-chroma)
                   (>= (abs (- (car lab) (car background))) drover-task-palette-contrast)
                   (not (seq-find (lambda (entry)
                                    (< (drover-task-palette-delta lab (cdr entry))
                                       drover-task-palette-distance))
                                  kept)))
          (push (cons (apply #'format "#%02x%02x%02x" (mapcar (lambda (c) (round (* 255 c))) rgb)) lab)
                kept))))
    (vconcat (nreverse kept))))

(defun drover-task-palette--median (numbers)
  "Return the median of NUMBERS, a non-empty list."
  (let ((sorted (sort (copy-sequence numbers) #'<)))
    (nth (/ (length sorted) 2) sorted)))

(defun drover-task-palette-extend (palette count background)
  "Return PALETTE, a vector of (NAME . LAB), extended to COUNT colours at least.
The colours made have the median lightness and chroma of PALETTE, of
the default foreground's lightness over BACKGROUND for an empty one,
and the hues of the golden angle from its first, each far enough from
every colour before it; past a full turn of tries, the distance asked
shrinks."
  (if (>= (length palette) count)
      palette
    (let* ((labs (mapcar #'cdr palette))
           (l (if labs
                  (drover-task-palette--median (mapcar #'car labs))
                (if (> (car background) 0.5) 0.45 0.78)))
           (c (max 0.09 (if labs (drover-task-palette--median (mapcar #'drover-task-palette--chroma labs)) 0.12)))
           (hue (if labs (drover-task-palette--hue (car labs)) 30.0))
           (distance drover-task-palette-distance)
           (made (append palette nil))
           (tries 0))
      (while (< (length made) count)
        (setq hue (mod (+ hue 137.50776) 360))
        (let* ((name (drover-task-palette--oklch l c hue))
               (lab (drover-task-palette-oklab (drover-task-palette--rgb name))))
          (if (seq-find (lambda (entry) (< (drover-task-palette-delta lab (cdr entry)) distance)) made)
              (when (> (cl-incf tries) 360)
                (setq tries 0
                      distance (* distance 0.8)))
            (setq made (append made (list (cons name lab)))
                  tries 0))))
      (vconcat made))))

(defvar drover-task-palette--current nil
  "The palette of the theme in force, as (SIGNATURE . PALETTE), or nil.
SIGNATURE tells the theme it was made for; see `drover-task-palette'.")

(defun drover-task-palette--signature ()
  "Return what the palette depends on: themes, the default face's colours."
  (list custom-enabled-themes
        (face-background 'default nil t)
        (face-foreground 'default nil t)
        drover-task-palette-faces drover-task-palette-distance
        drover-task-palette-chroma drover-task-palette-contrast))

(defun drover-task-palette (&optional count)
  "Return the palette of projects of the theme in force, COUNT colours at least.
A vector of (NAME . LAB); made once per theme (see
`drover-task-palette-forget')."
  (let ((signature (drover-task-palette--signature)))
    (unless (equal (car drover-task-palette--current) signature)
      (let ((background (drover-task-palette--background)))
        (setq drover-task-palette--current
              (cons signature (drover-task-palette-make (drover-task-palette--candidates) background)))))
    (when (and count (< (length (cdr drover-task-palette--current)) count))
      (setcdr drover-task-palette--current
              (drover-task-palette-extend (cdr drover-task-palette--current) count
                                          (drover-task-palette--background))))
    (cdr drover-task-palette--current)))

(defun drover-task-palette-forget ()
  "Forget the palette, to be made anew from the theme in force."
  (setq drover-task-palette--current nil))

(defun drover-task-palette-color (slot)
  "Return the colour of the palette's SLOT, an index from 0, as \"#RRGGBB\"."
  (car (aref (drover-task-palette (1+ slot)) slot)))

;;;; Slots of projects

(defvar drover-task-palette--slots nil
  "The slot of each project, a hash table PROJECT to (SLOT . TIME), nil unread.
TIME is when the project was last seen, for the bound of the file.")

(defvar drover-task-palette--warned nil
  "Non-nil once a problem with the file of slots was told.")

(defun drover-task-palette-file ()
  "Return the file of the slots of projects, in `drover-state-directory'."
  (expand-file-name "task-projects.eld" drover-state-directory))

(defun drover-task-palette--slots ()
  "Return the table of slots, read from its file the first time."
  (or drover-task-palette--slots
      (let ((table (make-hash-table :test #'equal)))
        (condition-case err
            (dolist (entry (drover-read-data (drover-task-palette-file) drover-task-palette--version :projects))
              (pcase entry
                (`(,(and project (pred stringp)) ,(and slot (pred natnump)) ,(and time (pred numberp)))
                 (puthash project (cons slot time) table))))
          (error
           (unless drover-task-palette--warned
             (setq drover-task-palette--warned t)
             (drover-warn "Could not read the colours of projects: %s" (drover-error-message err)))))
        (setq drover-task-palette--slots table))))

(defun drover-task-palette--save ()
  "Write the slots of the projects seen last, `drover-task-palette-limit'."
  (let ((entries nil))
    (maphash (lambda (project entry) (push (list project (car entry) (cdr entry)) entries))
             (drover-task-palette--slots))
    (setq entries (seq-take (sort entries :key #'caddr :reverse t) drover-task-palette-limit))
    (condition-case err
        (drover-write-data (drover-task-palette-file) "Slots of the colours of projects"
                           drover-task-palette--version :projects
                           (sort entries :key #'car))
      (error
       (unless drover-task-palette--warned
         (setq drover-task-palette--warned t)
         (drover-warn "Could not keep the colours of projects: %s" (drover-error-message err)))))))

(defun drover-task-palette-slot (project)
  "Return the slot of PROJECT, a string, giving it the first free one if new.
A new slot is kept at once in `drover-task-palette-file'.  The time it
was last seen is noted once a day, so that the file keeps the projects
of the last months."
  (let* ((table (drover-task-palette--slots))
         (entry (gethash project table))
         (now (float-time)))
    (cond
     (entry
      (when (> (- now (cdr entry)) 86400)
        (setcdr entry now)
        (drover-task-palette--save))
      (car entry))
     (t
      (let ((taken (make-hash-table)))
        (maphash (lambda (_ entry) (puthash (car entry) t taken)) table)
        (let ((slot 0))
          (while (gethash slot taken) (cl-incf slot))
          (puthash project (cons slot now) table)
          (when (> (hash-table-count table) drover-task-palette-limit)
            (let ((oldest nil))
              (maphash (lambda (name entry)
                         (when (and (not (equal name project))
                                    (or (null oldest) (> (cddr oldest) (cdr entry))))
                           (setq oldest (cons name entry))))
                       table)
              (when oldest (remhash (car oldest) table))))
          (drover-task-palette--save)
          slot))))))

(defun drover-task-project-color (project)
  "Return the colour of PROJECT in the theme in force, as \"#RRGGBB\"."
  (drover-task-palette-color (drover-task-palette-slot project)))

(provide 'drover-task-palette)
;;; drover-task-palette.el ends here
