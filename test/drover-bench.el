;;; drover-bench.el --- Time the session list over thousands of sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

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

;; `make bench' times the list over a fake backend of 2510 sessions,
;; 967 at the top of their trees and 1543 subagents, the shape of a
;; registry holding every unarchived OpenCode session, in each mode of
;; subagents: drawing the list whole (a refresh, a redraw, another
;; width, the first drawing, every group expanded) and following the
;; changes a running turn makes (a step of a working session, two
;; taking turns at the top, one going idle and back, one coming), with
;; the garbage collections each costs; then with the sidebar beside the
;; list: its first drawing, the list's rows at hand, two sessions
;; taking turns drawn in both, and the list catching up once shown
;; again after changes while hidden.  It checks that what was drawn in
;; place is what a drawing anew draws.
;;
;; It runs in batch, or in a graphical Emacs, where measuring text and
;; collecting garbage cost what they cost the user:
;;
;;   make bench
;;   make bench EMACS_GUI=/Applications/Emacs.app/Contents/MacOS/Emacs
;;
;; The graphical run uses the font of DROVER_BENCH_FONT, if set, and
;; the theme DROVER_BENCH_THEME from DROVER_BENCH_THEME_DIRECTORY.  Its
;; output goes to DROVER_BENCH_OUTPUT, which make prints.  Times are
;; averages per run, garbage collections included, which the next
;; columns tell apart.  A time over its target is flagged with a !:
;; 150 ms to draw the list whole, 10 ms to follow a change.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-list)
(require 'drover-attention)

(defvar drover-bench-output (getenv "DROVER_BENCH_OUTPUT")
  "File the results go to, or nil for the messages.")

(defun drover-bench-log (format-string &rest arguments)
  "Print FORMAT-STRING with ARGUMENTS as a line of the results."
  (let ((line (apply #'format format-string arguments)))
    (if drover-bench-output
        (with-temp-buffer
          (insert line "\n")
          (append-to-file (point-min) (point-max) drover-bench-output))
      (message "%s" line))))

(defconst drover-bench-top 967
  "Sessions at the top of their trees.")

(defconst drover-bench-subagents 1543
  "Subagent sessions.")

(defvar drover-bench-now (float-time)
  "The time the clock stands still at, as the list reads it.")

(defun drover-bench-id (kind index)
  "Return the identifier of session INDEX of KIND, `top' or `sub'."
  (format "ses_%s%08dAbCdEfGhIjKlMn" (if (eq kind 'top) "t" "s") index))

(defun drover-bench-session (kind index)
  "Return session INDEX of KIND, `top' or `sub', as a backend would report it."
  (let* ((sub (eq kind 'sub))
         (age (* (if sub (1+ (* index 2)) (* index 3)) 61))
         (updated (- drover-bench-now age)))
    (drover-session-create
     :backend 'fake :id (drover-bench-id kind index)
     :title (format "%s %d: investigate the registry paging and the drover list redraw"
                    (if sub "Subagent task" "Session") index)
     :directory (format "/tmp/drover-bench/project-%02d" (% index 17))
     :project (format "project-%02d" (% index 17))
     :branch (format "feature-branch-%d" (% index 17))
     :parent (and sub (if (zerop (% index 10))
                          (drover-bench-id 'sub (/ index 2))
                        (drover-bench-id 'top (% index drover-bench-top))))
     :status (cond ((zerop (% index 97)) 'failed) ((zerop (% index 31)) 'done) (t 'idle))
     :since updated :updated updated :created (- updated 600)
     :agent (if sub "general" (if (zerop (% index 4)) "plan" "build"))
     :model "Opus 5.5" :effort "high" :provider "GitHub Copilot" :provider-name "GitHub Copilot"
     :context-tokens 35321 :context-limit 200000 :engine 'fake :engine-id nil)))

(defun drover-bench-sessions ()
  "Return the sessions of the fake backend, newest first, as a registry lists them."
  (sort (append (mapcar (lambda (index) (drover-bench-session 'top index))
                        (number-sequence 0 (1- drover-bench-top)))
                (mapcar (lambda (index) (drover-bench-session 'sub index))
                        (number-sequence 0 (1- drover-bench-subagents))))
        :key #'drover-session-updated :reverse t))

(defun drover-bench-measure (function runs &optional setup)
  "Return (MS GCS GC-MS) of FUNCTION over RUNS runs, averages per run.
The time includes the garbage collections.  Each run of a measure pays
for its own garbage, not for that of the measure before.  SETUP, a
function, runs before each run, untimed."
  (let ((times nil)
        (gcs 0)
        (gc-time 0.0))
    (garbage-collect)
    (dotimes (_ runs)
      (when setup
        (funcall setup))
      (let ((done gcs-done)
            (elapsed gc-elapsed)
            (start (float-time)))
        (funcall function)
        (push (* 1000 (- (float-time) start)) times)
        (cl-incf gcs (- gcs-done done))
        (cl-incf gc-time (* 1000 (- gc-elapsed elapsed)))))
    (list (/ (apply #'+ times) runs) (/ gcs (float runs)) (/ gc-time runs))))

(defun drover-bench-change (id function)
  "Change session ID with FUNCTION of its record, report it, and follow it now."
  (setq drover-test-sessions
        (mapcar (lambda (session)
                  (if (equal (drover-session-id session) id)
                      (let ((copy (copy-sequence session)))
                        (funcall function copy)
                        copy)
                    session))
                drover-test-sessions))
  (drover-backend-changed 'fake id)
  (drover-changed-now))

(defvar drover-bench-tick 0
  "Steps taken by working sessions.")

(defun drover-bench-step (id)
  "Take a step of the turn session ID runs: its activity and context grow."
  (cl-incf drover-bench-tick)
  (drover-bench-change id (lambda (session)
                            (setf (drover-session-status session) 'working
                                  (drover-session-updated session) (+ drover-bench-now drover-bench-tick)
                                  (drover-session-context-tokens session)
                                  (+ 40000 (* 1000 (% drover-bench-tick 50)))))))

(defun drover-bench-text ()
  "Return the text of the list, without its sections, which belong to one drawing.
Nor the `face' properties font lock removes from what it displayed, as
`font-lock-face' gives the same, nor its marks of what it displayed."
  (let ((text (buffer-string)))
    (remove-list-of-text-properties 0 (length text) '(magit-section face fontified) text)
    text))

(defun drover-bench-row (label result target)
  "Return RESULT, of `drover-bench-measure', as a cell LABEL, flagged over TARGET ms."
  (pcase-let ((`(,time ,gcs ,gc-time) result))
    (format "%s %7.2f ms%s %4.1f GCs %5.1f ms" label time (if (> time target) "!" " ") gcs gc-time)))

(defun drover-bench-run ()
  "Time the list in each mode of subagents, and print the results."
  ;; Batch Emacs collects garbage once the heap doubled; an interactive
  ;; one once it grew a tenth, which a user's drawing pays.
  (setq gc-cons-percentage 0.1)
  (ignore-errors (set-frame-size (selected-frame) 200 60))
  (when (display-graphic-p)
    (when-let* ((font (getenv "DROVER_BENCH_FONT")))
      (set-face-attribute 'default nil :family font :height 130))
    (when-let* ((theme (getenv "DROVER_BENCH_THEME")))
      (when-let* ((directory (getenv "DROVER_BENCH_THEME_DIRECTORY")))
        (add-to-list 'custom-theme-load-path directory))
      (load-theme (intern theme) t))
    (set-frame-size (selected-frame) 200 60)
    (redisplay t))
  (drover-test-with-backend (drover-bench-sessions)
    ;; A backend makes its records anew whenever it lists them.
    (drover-test-register-simple 'fake (lambda () (mapcar #'copy-sequence drover-test-sessions))
                                 :session (lambda (id)
                                            (when-let* ((session (seq-find (lambda (session)
                                                                             (equal (drover-session-id session) id))
                                                                           drover-test-sessions)))
                                              (copy-sequence session))))
    (let ((drover-list-age-interval nil)
          (drover-list-cache-interval nil)
          (drover-list-animation-interval nil)
          (drover-list-deferred-delay nil)
          (buffer nil))
      (cl-letf (((symbol-function 'drover-list--now) (lambda () drover-bench-now)))
        (drover-start)
        (drover-attention-mode 1)
        (setq buffer (drover-list--buffer))
        (delete-other-windows)
        (set-window-buffer (selected-window) buffer)
        (when (display-graphic-p)
          (redisplay t))
        (drover-bench-log "drover bench: %d sessions, %d at the top; %s, %d columns, gc-cons-threshold %d, gc-cons-percentage %s"
                          (length drover-test-sessions) drover-bench-top
                          (if (display-graphic-p) (format "%s %s" window-system (face-font 'default)) "batch")
                          (window-body-width) gc-cons-threshold gc-cons-percentage)
        (with-current-buffer buffer
          (let ((top (drover-bench-id 'top 0))
                (second (drover-bench-id 'top 1))
                (sub (drover-bench-id 'sub 0))
                (idle (drover-bench-id 'top 500)))
            (dolist (id (list top second sub))
              (drover-bench-step id))
            (dolist (mode '(all active none))
              (setq drover-show-subagents mode)
              (drover-list--revert t)
              (let* ((rows (hash-table-count (drover-list--model-rows drover-list--model)))
                     (first (drover-bench-measure (lambda ()
                                                    (setq drover-list--widths nil
                                                          drover-list--model nil)
                                                    (drover-list--render))
                                                  3))
                     (revert (drover-bench-measure (lambda () (drover-list--revert t)) 5))
                     (relist (drover-bench-measure #'drover-index-refresh 5))
                     (render (drover-bench-measure #'drover-list--render 5))
                     (narrow nil)
                     (width (drover-bench-measure
                             (lambda ()
                               (setq narrow (not narrow))
                               (cl-letf (((symbol-function 'drover-list--line-width)
                                          (let ((width (drover-list--line-width)))
                                            (lambda () (if narrow (- width 40) width)))))
                                 (drover-list--render)))
                             4))
                     (deferred (drover-bench-measure (lambda () (drover-list--render) (drover-list--draw-deferred))
                                                     3))
                     (chunk (progn
                              (drover-list--render)
                              (let ((group (seq-find #'drover-list--deferred-p
                                                     (drover-list--model-groups drover-list--model))))
                                (drover-bench-measure (lambda () (drover-list--draw-some group)) 5))))
                     (expanded (progn
                                 (dolist (group (drover-list--groups))
                                   (magit-section-show group))
                                 (prog1 (drover-bench-measure #'drover-list--render 3)
                                   (dolist (group (drover-list--groups))
                                     (when (equal (oref group value) '(status . idle))
                                       (magit-section-hide group)))
                                   (drover-list--render))))
                     (step (progn (drover-bench-step top)
                                  (drover-bench-measure (lambda () (drover-bench-step top)) 21)))
                     (step-sub (drover-bench-measure (lambda () (drover-bench-step sub)) 21))
                     (shown (drover-bench-measure (lambda () (drover-bench-step top) (redisplay t)) 21))
                     (unchanged (drover-bench-measure (lambda () (force-window-update) (redisplay t)) 21))

                     (rename (drover-bench-measure
                              (lambda ()
                                (drover-bench-change idle (lambda (session)
                                                            (setf (drover-session-title session)
                                                                  (format "Renamed %06d" (random 1000000))))))
                              21))
                     (turns (drover-bench-measure
                             (lambda () (drover-bench-step (if (cl-evenp drover-bench-tick) top second)))
                             21))
                     (back (drover-bench-measure
                            (lambda ()
                              (drover-bench-change second (lambda (session)
                                                            (setf (drover-session-status session) 'idle)))
                              (drover-bench-step second))
                            11))
                     (coming (drover-bench-measure
                              (lambda ()
                                (let ((session (drover-bench-session 'top (+ 100000 (cl-incf drover-bench-tick)))))
                                  (setf (drover-session-updated session) (+ drover-bench-now drover-bench-tick))
                                  (setq drover-test-sessions (cons session drover-test-sessions))
                                  (drover-backend-changed 'fake (drover-session-id session))
                                  (drover-changed-now)))
                              11))
                     (redraws 0)
                     (in-place (let ((count (lambda () (cl-incf redraws))))
                                 (advice-add 'drover-list--render :before count)
                                 (unwind-protect
                                     (dotimes (_ 10)
                                       (drover-bench-step (if (cl-evenp drover-bench-tick) top second)))
                                   (advice-remove 'drover-list--render count))
                                 (- 10 redraws)))
                     (same (let ((text (drover-bench-text)))
                             (drover-list--render)
                             (equal-including-properties text (drover-bench-text)))))
                (drover-bench-log "%-6s %4d rows" mode rows)
                (drover-bench-log "  drawn whole: %s | %s | %s"
                                  (drover-bench-row "refresh" revert 150)
                                  (drover-bench-row "redraw" render 150)
                                  (drover-bench-row "another width" width 150))
                (drover-bench-log "               %s | %s | %s"
                                  (drover-bench-row "first, measuring" first 1000)
                                  (drover-bench-row "every group expanded" expanded 1000)
                                  (drover-bench-row "and collapsed ones' sessions" deferred 1000))
                (drover-bench-log "               %s, right after a refresh | %s, while idle"
                                  (drover-bench-row "listing anew" relist 150)
                                  (drover-bench-row (format "%d collapsed trees drawn" drover-list--deferred-chunk)
                                                    chunk 50))
                (drover-bench-log "  changes:     %s | %s | %s"
                                  (drover-bench-row "a step" step 10)
                                  (drover-bench-row "a subagent's step" step-sub 10)
                                  (drover-bench-row "an idle one renamed" rename 10))
                (when (display-graphic-p)
                  ;; Emacs redisplays the list's window after a change,
                  ;; once per redisplay however many changes came.
                  (drover-bench-log "               %s | %s"
                                    (drover-bench-row "a step, then redisplay" shown 1000)
                                    (drover-bench-row "redisplay of the window alone" unchanged 1000)))
                (drover-bench-log "               %s | %s | %s"
                                  (drover-bench-row "two taking turns" turns 10)
                                  (drover-bench-row "to idle and back" back 10)
                                  (drover-bench-row "one coming" coming 10))
                (drover-bench-log "               %d of 10 turns drawn in place; the same as drawn anew: %s"
                                  in-place (if same "yes" "NO"))
                (drover-bench-two-lists top second)))))
        (drover-attention-mode -1)))))

(defun drover-bench-two-lists (top second)
  "Time the sidebar beside the list, sessions TOP and SECOND taking turns.
The list is the current buffer, alone in the selected window."
  (let* ((main (selected-window))
         (list (current-buffer))
         (side (drover-list--show-side))
         (sidebar (window-buffer side))
         (hidden (get-buffer-create " *drover bench other*")))
    (unwind-protect
        (progn
          ;; Narrower beside the sidebar: drawn for its width.
          (drover-list--on-window-change main)
          (let* ((first (drover-bench-measure
                         (lambda ()
                           (with-current-buffer sidebar
                             (setq drover-list--widths nil
                                   drover-list--model nil)
                             (drover-list--render)))
                         3))
                 (both (drover-bench-measure
                        (lambda () (drover-bench-step (if (cl-evenp drover-bench-tick) top second)))
                        21))
                 (catch-up (drover-bench-measure
                            (lambda ()
                              (set-window-buffer main list)
                              (drover-list--on-window-change main))
                            5
                            (lambda ()
                              (set-window-buffer main hidden)
                              (dotimes (_ 10)
                                (drover-bench-step (if (cl-evenp drover-bench-tick) top second))))))
                 ;; As many sessions as a hidden list follows in place,
                 ;; idle ones starting turns, then going idle again.
                 (many (let ((ids (mapcar (lambda (index) (drover-bench-id 'top index))
                                          (number-sequence 300 (+ 299 drover-list--pending-limit))))
                             (working nil))
                         (drover-bench-measure
                          (lambda ()
                            (set-window-buffer main list)
                            (drover-list--on-window-change main))
                          4
                          (lambda ()
                            (set-window-buffer main hidden)
                            (setq working (not working))
                            (dolist (id ids)
                              (drover-bench-change
                               id (lambda (session)
                                    (setf (drover-session-status session) (if working 'working 'idle)
                                          (drover-session-updated session)
                                          (+ drover-bench-now (cl-incf drover-bench-tick))))))))))
                 (whole (drover-bench-measure
                         (lambda ()
                           (set-window-buffer main list)
                           (drover-list--on-window-change main))
                         4
                         (lambda ()
                           (set-window-buffer main hidden)
                           (with-current-buffer list
                             (setq drover-list--stale t)))))
                 (same (seq-every-p (lambda (buffer)
                                      (with-current-buffer buffer
                                        (let ((text (drover-bench-text)))
                                          (drover-list--render)
                                          (equal-including-properties text (drover-bench-text)))))
                                    (list sidebar list))))
            (drover-bench-log "  with the sidebar, %d columns, beside the list, %d:"
                              (window-body-width side) (window-body-width main))
            (drover-bench-log "               %s | %s | %s"
                              (drover-bench-row "the sidebar's first drawing" first 1000)
                              (drover-bench-row "two taking turns, both drawn" both 10)
                              (drover-bench-row "the list shown after 10 hidden" catch-up 150))
            (drover-bench-log "               %s | %s"
                              (drover-bench-row (format "the list shown after %d sessions changed hidden"
                                                        drover-list--pending-limit)
                                                many 150)
                              (drover-bench-row "after more, drawn whole" whole 150))
            (drover-bench-log "               both the same as drawn anew: %s" (if same "yes" "NO"))))
      (delete-window side)
      (kill-buffer sidebar)
      (set-window-buffer main list)
      (kill-buffer hidden)
      (with-current-buffer list
        (drover-list--on-window-change main)))))

(if (not drover-bench-output)
    (drover-bench-run)
  ;; A graphical Emacs ends with the bench, whatever happens.
  (unwind-protect
      (condition-case err
          (drover-bench-run)
        (error (drover-bench-log "drover bench failed: %S" err)))
    (kill-emacs 0)))

(provide 'drover-bench)
;;; drover-bench.el ends here
