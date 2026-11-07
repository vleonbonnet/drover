;;; drover-toggle-bench.el --- Time the sidebar shown and hidden, and the menu opened  -*- lexical-binding: t; -*-

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

;; `make bench-toggle' times what the user waits for as the sidebar
;; shows and hides and as the dispatch menu opens, over the 2510 fake
;; sessions of test/drover-bench.el, 30 of them with a buffer, every
;; row of both lists drawn, as an idle Emacs has drawn them:
;;
;; - the main window showing the list, the sidebar beside it stepping
;;   aside, and showing another buffer again, the sidebar coming back;
;;   the same after changes, and after the minute's refresh, while the
;;   list was hidden;
;; - the sidebar toggled with `drover-list-side', beside another buffer
;;   and beside the list;
;; - the dispatch menu opened from the list, the sidebar, a buffer
;;   while a list shows, a buffer without a list in sight and the prompt
;;   buffer; moving to the next session from it, which sets it up again.
;;
;; Each measure tells the time, the garbage collections and their time,
;; the megabytes allocated and the lists drawn whole, averages per run.
;; A batch Emacs redisplays nothing, so it runs no window change
;; function: `drover-toggle-bench--redisplay' runs them as redisplay
;; does (see `run_window_change_functions' in window.c), then drover's
;; timers of no delay, and again until nothing changes.  The menu is set
;; up and drawn into a buffer as transient does, its window not shown.
;;
;; DROVER_BENCH_PROFILE set, it profiles each measure too, and prints
;; the heaviest of the profiler's entries.  It runs in a graphical Emacs
;; too (EMACS_GUI), where redisplay runs for real besides.

;;; Code:

(require 'drover-bench)
(require 'drover-keys)
(require 'drover-prompt)
(require 'profiler)

;;;; Redisplay

(defvar drover-toggle-bench--windows nil
  "The windows of the selected frame, as window change functions last saw them.
An alist (WINDOW BUFFER PIXEL-WIDTH PIXEL-HEIGHT BODY-WIDTH BODY-HEIGHT).")

(defvar drover-toggle-bench--selected nil
  "The selected window as window change functions last saw it.")

(defun drover-toggle-bench--state (window)
  "Return the state of WINDOW, as `drover-toggle-bench--windows' holds it."
  (list window (window-buffer window) (window-pixel-width window) (window-pixel-height window)
        (window-body-width window t) (window-body-height window t)))

(defun drover-toggle-bench--record ()
  "Record the windows of the selected frame, as window change functions see them."
  (setq drover-toggle-bench--windows (mapcar #'drover-toggle-bench--state (window-list nil 'nomini))
        drover-toggle-bench--selected (selected-window)))

(defun drover-toggle-bench--run-local (hook buffer window)
  "Run the functions local to BUFFER of HOOK with WINDOW, BUFFER current."
  (when (and (buffer-live-p buffer) (local-variable-p hook buffer))
    (with-current-buffer buffer
      (dolist (function (buffer-local-value hook buffer))
        (unless (eq function t)
          (funcall function window))))))

(defun drover-toggle-bench--run-global (hook &rest arguments)
  "Run the global functions of HOOK with ARGUMENTS."
  (dolist (function (default-value hook))
    (unless (eq function t)
      (apply function arguments))))

(defun drover-toggle-bench--change-functions ()
  "Run the window change functions of the selected frame, as redisplay does.
Return non-nil when a window changed since they last ran."
  (let* ((frame (selected-frame))
         (old drover-toggle-bench--windows)
         (windows (window-list frame 'nomini))
         (deleted (seq-some (lambda (entry) (not (memq (car entry) windows))) old))
         (selected (not (eq (selected-window) drover-toggle-bench--selected)))
         (buffer-change nil)
         (size-change nil))
    (dolist (window windows)
      (let* ((entry (assq window old))
             (buffer (window-buffer window))
             (new-buffer (or (null entry) (not (eq (nth 1 entry) buffer))))
             (new-size (or new-buffer
                           (not (equal (cddr entry) (cddr (drover-toggle-bench--state window)))))))
        (setq buffer-change (or buffer-change new-buffer)
              size-change (or size-change new-size))
        (when new-buffer
          (when entry
            (drover-toggle-bench--run-local 'window-buffer-change-functions (nth 1 entry) window))
          (drover-toggle-bench--run-local 'window-buffer-change-functions buffer window))
        (when (and new-size (window-live-p window))
          (drover-toggle-bench--run-local 'window-size-change-functions (window-buffer window) window))
        (when (and selected (window-live-p window)
                   (or (eq window (selected-window)) (eq window drover-toggle-bench--selected)))
          (drover-toggle-bench--run-local 'window-selection-change-functions (window-buffer window) window))))
    (when (or buffer-change deleted)
      (drover-toggle-bench--run-global 'window-buffer-change-functions frame))
    (when size-change
      (drover-toggle-bench--run-global 'window-size-change-functions frame))
    (when selected
      (drover-toggle-bench--run-global 'window-selection-change-functions frame))
    (when (or size-change deleted)
      (dolist (window windows)
        (when (window-live-p window)
          (let ((buffer (window-buffer window)))
            (when (local-variable-p 'window-configuration-change-hook buffer)
              (with-selected-window window
                (dolist (function (buffer-local-value 'window-configuration-change-hook buffer))
                  (unless (eq function t)
                    (funcall function))))))))
      (drover-toggle-bench--run-global 'window-configuration-change-hook))
    (drover-toggle-bench--record)
    (or buffer-change size-change deleted selected)))

(defconst drover-toggle-bench--timers
  '(drover-list--place-timer drover-list--restyle-timer drover-list--focus-timer)
  "Variables of drover's timers of no delay that windows changing arm.")

(defun drover-toggle-bench--run-timers ()
  "Run drover's timers of no delay that are armed; return non-nil if any ran."
  (let ((ran nil))
    (dolist (symbol drover-toggle-bench--timers)
      (let ((timer (symbol-value symbol)))
        (when (timerp timer)
          (cancel-timer timer)
          (setq ran t)
          (apply (timer--function timer) (timer--args timer)))))
    ran))

(defun drover-toggle-bench--redisplay ()
  "Run what redisplay and the timers following it run, until nothing changes.
In a graphical Emacs, redisplay itself, which runs the window change
functions; in batch, those functions as redisplay runs them."
  (dotimes (_ 6)
    (if (display-graphic-p)
        (redisplay t)
      (drover-toggle-bench--change-functions))
    (drover-toggle-bench--run-timers))
  (when (display-graphic-p)
    (redisplay t)))

;;;; Measuring

(defvar drover-toggle-bench--renders 0
  "Lists drawn whole since the measure began.")

(defun drover-toggle-bench--count-render (&rest _)
  "Count a list drawn whole."
  (cl-incf drover-toggle-bench--renders))

(defun drover-toggle-bench--allocated ()
  "Return about how many bytes Emacs allocated so far."
  (pcase-let ((`(,conses ,floats ,vector-cells ,symbols ,string-chars ,intervals ,strings)
               (memory-use-counts)))
    (+ (* 16 conses) (* 16 floats) (* 8 vector-cells) (* 48 symbols) string-chars
       (* 56 intervals) (* 32 strings))))

(defvar drover-toggle-bench-profile (getenv "DROVER_BENCH_PROFILE")
  "Non-nil to profile each measure, and print its heaviest entries.")

(defvar drover-toggle-bench-only (getenv "DROVER_BENCH_ONLY")
  "Regexp matching the labels of the measures to take, or nil for all.")

(defun drover-toggle-bench--profile-report (label)
  "Print the heaviest entries of the profile of the measure LABEL."
  (dolist (type '(cpu memory))
    (let ((profile (if (eq type 'cpu) (profiler-cpu-profile) (profiler-memory-profile))))
      (when (and profile (hash-table-p (profiler-profile-log profile)))
        (let ((totals (make-hash-table :test #'eq))
              (all 0))
          (maphash (lambda (backtrace count)
                     (cl-incf all count)
                     (let ((seen nil))
                       (seq-doseq (frame backtrace)
                         (when (and frame (symbolp frame) (not (special-form-p frame))
                                    (not (memq frame seen)))
                           (push frame seen)
                           (cl-incf (gethash frame totals 0) count)))))
                   (profiler-profile-log profile))
          (let ((entries nil))
            (maphash (lambda (function count) (push (cons function count) entries)) totals)
            (setq entries (seq-take (sort entries (lambda (a b) (> (cdr a) (cdr b)))) 40))
            (drover-bench-log "    %s profile of %s, %d samples, inclusive:" type label all)
            (dolist (entry entries)
              (drover-bench-log "      %5.1f%%  %s" (/ (* 100.0 (cdr entry)) (max all 1)) (car entry)))))))))

(defun drover-toggle-bench-measure (label function runs &optional setup target)
  "Time FUNCTION over RUNS runs and log it as LABEL, flagged over TARGET ms.
SETUP, a function, runs before each run, untimed.  Log the averages per
run of the time, the garbage collections and their time, the megabytes
allocated and the lists drawn whole.  Return the average time.  A
measure whose LABEL `drover-toggle-bench-only' does not match is not
taken."
  (when (or (not drover-toggle-bench-only) (string-match-p drover-toggle-bench-only label))
    (drover-toggle-bench--measure label function runs setup target)))

(defun drover-toggle-bench--measure (label function runs setup target)
  "Time FUNCTION over RUNS runs, SETUP before each, and log it as LABEL.
See `drover-toggle-bench-measure', TARGET included."
  (let ((times nil)
        (gcs 0)
        (gc-time 0.0)
        (bytes 0)
        (renders 0))
    (advice-add 'drover-list--render :before #'drover-toggle-bench--count-render)
    (unwind-protect
        (progn
          (garbage-collect)
          (when drover-toggle-bench-profile
            (let ((profiler-sampling-interval 100000))
              (profiler-start 'cpu+mem)))
          (dotimes (_ runs)
            (when setup
              (when drover-toggle-bench-profile
                (profiler-cpu-stop)
                (profiler-memory-stop))
              (funcall setup)
              (when drover-toggle-bench-profile
                (profiler-cpu-start 100000)
                (profiler-memory-start)))
            (setq drover-toggle-bench--renders 0)
            (let ((done gcs-done)
                  (elapsed gc-elapsed)
                  (allocated (drover-toggle-bench--allocated))
                  (start (float-time)))
              (funcall function)
              (push (* 1000 (- (float-time) start)) times)
              (cl-incf gcs (- gcs-done done))
              (cl-incf gc-time (* 1000 (- gc-elapsed elapsed)))
              (cl-incf bytes (- (drover-toggle-bench--allocated) allocated))
              (cl-incf renders drover-toggle-bench--renders))))
      (advice-remove 'drover-list--render #'drover-toggle-bench--count-render)
      (when drover-toggle-bench-profile
        (profiler-stop)))
    (let ((time (/ (apply #'+ times) runs)))
      (drover-bench-log "  %-58s %7.2f ms%s %4.1f GCs %5.1f ms %6.2f MB %4.1f drawn whole"
                        label time (if (and target (> time target)) "!" " ")
                        (/ gcs (float runs)) (/ gc-time runs) (/ bytes runs 1e6) (/ renders (float runs)))
      (when drover-toggle-bench-profile
        (drover-toggle-bench--profile-report label)
        (profiler-reset))
      time)))

;;;; The fake backend

(defvar drover-toggle-bench--by-id nil
  "Hash table of the fake sessions by identifier.")

(defvar drover-toggle-bench--buffers nil
  "Hash table of the buffers of the fake sessions that have one, by identifier.")

(defun drover-toggle-bench--index ()
  "Index the fake sessions by identifier, as a backend holds them."
  (setq drover-toggle-bench--by-id (make-hash-table :test #'equal :size (length drover-test-sessions)))
  (dolist (session drover-test-sessions)
    (puthash (drover-session-id session) session drover-toggle-bench--by-id)))

(defun drover-toggle-bench--change (id function)
  "Change session ID with FUNCTION of its record, report it, and follow it now."
  (let ((copy (copy-sequence (gethash id drover-toggle-bench--by-id))))
    (funcall function copy)
    (setq drover-test-sessions (mapcar (lambda (session)
                                         (if (equal (drover-session-id session) id) copy session))
                                       drover-test-sessions))
    (puthash id copy drover-toggle-bench--by-id)
    (drover-backend-changed 'fake id)
    (drover-changed-now)))

(defun drover-toggle-bench--step (id)
  "Take a step of the turn session ID runs: its activity and context grow."
  (cl-incf drover-bench-tick)
  (drover-toggle-bench--change id (lambda (session)
                                    (setf (drover-session-status session) 'working
                                          (drover-session-updated session) (+ drover-bench-now drover-bench-tick)
                                          (drover-session-context-tokens session)
                                          (+ 40000 (* 1000 (% drover-bench-tick 50)))))))

(defun drover-toggle-bench--steps ()
  "Take 10 steps, two sessions at the top taking turns."
  (dotimes (_ 10)
    (drover-toggle-bench--step (drover-bench-id 'top (% drover-bench-tick 2)))))

(defun drover-toggle-bench--sessions ()
  "Return the fake sessions, made anew, as a backend lists them."
  (mapcar #'copy-sequence drover-test-sessions))

(defun drover-toggle-bench--session (id)
  "Return the fake session ID, made anew, or nil."
  (when-let* ((session (gethash id drover-toggle-bench--by-id)))
    (copy-sequence session)))

(defun drover-toggle-bench--buffer (session)
  "Return the buffer of the fake SESSION, or nil."
  (gethash (drover-session-id session) drover-toggle-bench--buffers))

(defun drover-toggle-bench--preview (_session callback)
  "Call CALLBACK with the preview of a fake session."
  (funcall callback "Preview"))

(defun drover-toggle-bench--register ()
  "Register the fake backend, whose sessions open, and 30 of which have a buffer.
Its functions are compiled, as a backend's are: this file is loaded as
source."
  (drover-toggle-bench--index)
  (setq drover-toggle-bench--buffers (make-hash-table :test #'equal))
  (dotimes (index 30)
    (let ((id (drover-bench-id 'top (* index 7))))
      (puthash id (get-buffer-create (format " *drover bench session %d*" index))
               drover-toggle-bench--buffers)))
  (dolist (function '(drover-toggle-bench--sessions drover-toggle-bench--session
                      drover-toggle-bench--buffer drover-toggle-bench--preview))
    (unless (compiled-function-p (symbol-function function))
      (byte-compile function)))
  (drover-test-register-simple
   'fake #'drover-toggle-bench--sessions
   :session #'drover-toggle-bench--session
   :buffer #'drover-toggle-bench--buffer
   :open #'ignore :rename #'ignore :interrupt #'ignore :archive #'ignore :mark-seen #'ignore
   :delete #'ignore :respond #'ignore :new #'ignore
   :preview #'drover-toggle-bench--preview))

;;;; The menu

(defun drover-toggle-bench--open-menu ()
  "Set up `drover-dispatch' here and draw it into a buffer, as transient does."
  (setq drover--dispatch-close-keys (drover--dispatch-own-keys))
  (transient--init-transient 'drover-dispatch nil
                             (list :scope (drover--context)
                                   :display-action drover-menu-display-action))
  (drover-toggle-bench--draw-menu))

(defun drover-toggle-bench--draw-menu ()
  "Draw the menu set up into its buffer, as `transient--show' does.
The buffer is made once and erased each time, as transient's is."
  (let ((transient--shadowed-buffer (current-buffer))
        (setup (not (get-buffer " *drover bench menu*"))))
    (with-current-buffer (get-buffer-create " *drover bench menu*")
      (erase-buffer)
      (transient--insert-menu setup))))

(defun drover-toggle-bench--close-menu ()
  "Forget the menu set up."
  (setq transient--prefix nil
        transient--layout nil
        transient--suffixes nil))

(defun drover-toggle-bench--menu-step ()
  "Move to the next session from the menu set up, then set it up and draw it.
As `transient--refresh-transient' does after a command of the menu that
keeps it, the menu having `:refresh-suffixes'."
  (drover--dispatch-navigate #'drover-list-next-session)
  (transient--init-transient)
  (drover-toggle-bench--draw-menu))

;;;; The scenarios

(defun drover-toggle-bench--show (window buffer)
  "Show BUFFER in WINDOW, then redisplay."
  (set-window-buffer window buffer)
  (drover-toggle-bench--redisplay))

(defun drover-toggle-bench--draw-all ()
  "Draw every row the lists left for later, as they draw them while Emacs is idle."
  (dolist (buffer (drover-list--buffers))
    (with-current-buffer buffer
      (drover-list--draw-undrawn))))

(defun drover-toggle-bench--goto-session (window)
  "Move the point of WINDOW to a session row, the fourth one."
  (with-selected-window window
    (goto-char (point-min))
    (drover-list-next-session 4)))

(defun drover-toggle-bench--sidebar (main list scratch)
  "Time the sidebar stepping aside and coming back.
MAIN is the window where you work, LIST the buffer of `drover-list',
SCRATCH another buffer."
  (progn
    (drover-bench-log "The sidebar, the list in the main window and out of it:")
    (drover-toggle-bench-measure
     "to the list, the sidebar stepping aside" (lambda () (drover-toggle-bench--show main list)) 6
     (lambda () (drover-toggle-bench--show main scratch)) 5)
    (drover-toggle-bench-measure
     "to another buffer, the sidebar coming back" (lambda () (drover-toggle-bench--show main scratch)) 6
     (lambda () (drover-toggle-bench--show main list)) 3)
    (drover-toggle-bench-measure
     "to the list after 10 changes hidden" (lambda () (drover-toggle-bench--show main list)) 6
     (lambda ()
       (drover-toggle-bench--show main scratch)
       (drover-toggle-bench--steps))
     5)
    (drover-toggle-bench-measure
     "to another buffer after 10 changes, the sidebar shown" (lambda () (drover-toggle-bench--show main scratch)) 6
     (lambda ()
       (drover-toggle-bench--show main list)
       (drover-toggle-bench--steps))
     3)
    (drover-toggle-bench-measure
     "to the list after a minute's refresh hidden" (lambda () (drover-toggle-bench--show main list)) 6
     (lambda ()
       (drover-toggle-bench--show main scratch)
       (setq drover-bench-now (+ drover-bench-now 61))
       (drover-list--age-tick))
     5)
    (drover-toggle-bench-measure
     "to another buffer after a minute's refresh" (lambda () (drover-toggle-bench--show main scratch)) 6
     (lambda ()
       (drover-toggle-bench--show main list)
       (setq drover-bench-now (+ drover-bench-now 61))
       (drover-list--age-tick))
     3)
    (drover-toggle-bench--show main scratch)
    (drover-toggle-bench-measure
     "the sidebar hidden with drover-list-side" (lambda () (drover-list-side) (drover-toggle-bench--redisplay)) 6
     (lambda () (unless (drover-list--side-window) (drover-list-side) (drover-toggle-bench--redisplay)))
     3)
    (drover-toggle-bench-measure
     "the sidebar shown with drover-list-side" (lambda () (drover-list-side) (drover-toggle-bench--redisplay)) 6
     (lambda () (when (drover-list--side-window) (drover-list-side) (drover-toggle-bench--redisplay)))
     3)
    (drover-toggle-bench--show main list)
    (drover-toggle-bench-measure
     "the sidebar shown beside the list, which narrows"
     (lambda () (drover-list-side) (drover-toggle-bench--redisplay)) 4
     (lambda () (when (drover-list--side-window) (drover-list-side) (drover-toggle-bench--redisplay)))
     150)
    (drover-toggle-bench-measure
     "the sidebar hidden beside the list, which widens"
     (lambda () (drover-list-side) (drover-toggle-bench--redisplay)) 4
     (lambda () (unless (drover-list--side-window) (drover-list-side) (drover-toggle-bench--redisplay)))
     150)
    (when (drover-list--side-window)
      (drover-list-side)
      (drover-toggle-bench--redisplay))
    (drover-toggle-bench--show main scratch)
    (unless (drover-list--side-window)
      (drover-list-side)
      (drover-toggle-bench--redisplay))))

(defun drover-toggle-bench--menu (main list scratch)
  "Time the dispatch menu opened from where it opens.
MAIN is the window where you work, LIST the buffer of `drover-list',
SCRATCH another buffer; the sidebar shows.  The menu opens once before,
untimed, as Emacs loads what its entries need the first time."
  (drover-bench-log "The dispatch menu, set up and drawn:")
  (with-selected-window main
    (drover-toggle-bench--open-menu)
    (drover-toggle-bench--close-menu))
  (let ((side (drover-list--side-window)))
    (drover-toggle-bench--goto-session side)
    (with-selected-window side
      (drover-toggle-bench-measure "opened in the sidebar, on a session" #'drover-toggle-bench--open-menu 10
                                   #'drover-toggle-bench--close-menu 10))
    (with-selected-window main
      (drover-toggle-bench-measure "opened in a buffer, the sidebar in sight" #'drover-toggle-bench--open-menu 10
                                   #'drover-toggle-bench--close-menu 10)))
  (drover-toggle-bench--show main list)
  (drover-toggle-bench--goto-session main)
  (with-selected-window main
    (drover-toggle-bench-measure "opened in the list, on a session" #'drover-toggle-bench--open-menu 10
                                 #'drover-toggle-bench--close-menu 10)
    (drover-toggle-bench--open-menu)
    (drover-toggle-bench-measure "the next session, from the menu" #'drover-toggle-bench--menu-step 10
                                 nil 5)
    (drover-toggle-bench--close-menu))
  (drover-toggle-bench--show main scratch)
  (when-let* ((side (drover-list--side-window)))
    (drover-list--hide-side side)
    (drover-toggle-bench--redisplay))
  (with-selected-window main
    (drover-toggle-bench-measure "opened in a buffer, no list in sight" #'drover-toggle-bench--open-menu 10
                                 #'drover-toggle-bench--close-menu 10))
  (let ((prompt (drover-prompt-buffer t)))
    (drover-toggle-bench--show main prompt)
    (with-selected-window main
      (drover-toggle-bench-measure "opened in the prompt buffer" #'drover-toggle-bench--open-menu 10
                                   #'drover-toggle-bench--close-menu 10))
    (drover-toggle-bench--show main scratch)
    (kill-buffer prompt))
  (drover-list-side)
  (drover-toggle-bench--redisplay))

(defun drover-toggle-bench--shared (main list scratch)
  "Time the sidebar stepping aside while two backends list one conversation.
MAIN is the window where you work, LIST the buffer of `drover-list',
SCRATCH another buffer."
  (let ((id (drover-bench-id 'top 2)))
    (drover-toggle-bench--change id (lambda (session)
                                      (setf (drover-session-engine-id session) "shared-conversation")))
    (drover-test-register-simple
     'mirror (list (drover-session-create :backend 'mirror :id "mirror-1" :title "The same conversation"
                                          :directory "/tmp/drover-bench/project-02" :status 'idle
                                          :since drover-bench-now :updated drover-bench-now
                                          :created drover-bench-now :engine 'fake
                                          :engine-id "shared-conversation")))
    (drover-changed-now)
    (drover-toggle-bench--redisplay)
    (drover-toggle-bench--draw-all)
    (drover-bench-log "One conversation listed by two backends (%d shared):"
                      (drover--index-shared (drover-index)))
    (drover-toggle-bench-measure
     "to the list after 10 changes hidden" (lambda () (drover-toggle-bench--show main list)) 4
     (lambda ()
       (drover-toggle-bench--show main scratch)
       (drover-toggle-bench--steps))
     5)
    (drover-toggle-bench-measure
     "to another buffer after 10 changes, the sidebar shown" (lambda () (drover-toggle-bench--show main scratch)) 4
     (lambda ()
       (drover-toggle-bench--show main list)
       (drover-toggle-bench--steps))
     3)
    (drover-unregister-backend 'mirror)
    (drover-changed-now)
    (drover-toggle-bench--redisplay)))

(defun drover-toggle-bench-run (&optional scenarios)
  "Time the sidebar shown and hidden and the menu opened, and print the results.
SCENARIOS, a function, is called instead of the measures with the
window where you work, the buffer of the list and another buffer, the
sidebar shown and both lists drawn."
  (setq gc-cons-percentage 0.1)
  (ignore-errors (set-frame-size (selected-frame) 200 60))
  (when (display-graphic-p)
    (set-frame-size (selected-frame) 200 60)
    (redisplay t))
  (drover-test-with-backend (drover-bench-sessions)
    (drover-toggle-bench--register)
    (let ((drover-list-age-interval nil)
          (drover-list-cache-interval nil)
          (opencode-session-status-animation-interval nil)
          (drover-list-deferred-delay nil)
          (scratch (get-buffer-create " *drover bench scratch*"))
          (main nil)
          (list nil))
      (cl-letf (((symbol-function 'drover-list--now) (lambda () drover-bench-now)))
        (unwind-protect
            (progn
              (drover-start)
              (drover-attention-mode 1)
              (delete-other-windows)
              (setq main (selected-window))
              (set-window-buffer main scratch)
              (drover-toggle-bench--record)
              (setq list (drover-list--buffer))
              (drover-list-side)
              (drover-toggle-bench--redisplay)
              ;; Both lists shown once and drawn whole, as an idle Emacs has.
              (drover-toggle-bench--show main list)
              (drover-toggle-bench--draw-all)
              (drover-toggle-bench--show main scratch)
              (drover-toggle-bench--draw-all)
              (drover-bench-log "drover toggle bench: %d sessions, %d with a buffer; %s, frame %dx%d, sidebar %d columns, gc-cons-percentage %s"
                                (length drover-test-sessions) (hash-table-count drover-toggle-bench--buffers)
                                (if (display-graphic-p) (format "%s %s" window-system (face-font 'default)) "batch")
                                (frame-width) (frame-height)
                                (window-body-width (drover-list--side-window)) gc-cons-percentage)
              (if scenarios
                  (funcall scenarios main list scratch)
                (drover-toggle-bench--sidebar main list scratch)
                (drover-toggle-bench--menu main list scratch)
                (drover-toggle-bench--shared main list scratch)))
          (drover-attention-mode -1)
          (dolist (buffer (drover-list--buffers))
            (kill-buffer buffer))
          (kill-buffer scratch)
          (when-let* ((menu (get-buffer " *drover bench menu*")))
            (kill-buffer menu))
          (maphash (lambda (_ buffer) (kill-buffer buffer)) drover-toggle-bench--buffers))))))

(defun drover-toggle-bench-main ()
  "Run the bench, and print the results; a graphical Emacs then exits."
  (if (not drover-bench-output)
      (drover-toggle-bench-run)
    (unwind-protect
        (condition-case err
            (drover-toggle-bench-run)
          (error (drover-bench-log "drover toggle bench failed: %s" (drover-error-string err))))
      (kill-emacs 0))))

(provide 'drover-toggle-bench)
;;; drover-toggle-bench.el ends here
