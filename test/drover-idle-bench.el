;;; drover-idle-bench.el --- Time what drover costs while nothing happens  -*- lexical-binding: t; -*-

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

;; `make bench-idle EMACS_GUI=...' runs drover in a graphical Emacs, as
;; the user runs it, and measures what it costs in the background: the
;; OpenCode backend over a registry of 2519 sessions in 53 Git
;; worktrees, the list and the sidebar shown, the mode-line count on,
;; one session and one of its subagents working, their glyphs animated,
;; the user working in a window below the list.
;; Nothing reaches an OpenCode service: the registry and the catalogs
;; are filled here, and a request signals.
;;
;; The command loop runs between phases, as it runs for the user, with
;; its timers and redisplays:
;;
;; - idle: nothing changes but the clock, for `drover-idle-bench-seconds';
;; - streaming: the working session takes a step every 100 ms;
;; - unfocused: Emacs lost focus, as a focus-out event tells, the glyphs
;;   standing still (`opencode-session-status-animation-unfocused-interval' nil),
;;   then moving at the unfocused pace, by default;
;; - then, synchronously, a frame of the animation drawn and redisplayed,
;;   its lines reordered for display or not, against a redisplay
;;   changing nothing; a change followed, of a session and of the
;;   registry as a whole; and the minute's refresh.
;;
;; Each phase tells the processor time it took per second (the share of
;; one core), what it allocated per second, the garbage collections, the
;; frame ticks and the redisplays that did something.  The streaming
;; phase is then profiled, processor and memory, and so are changes and
;; the refresh, the reports written beside the results.  Focus is simulated, as `handle-focus-out' and
;; `handle-focus-in' set it, so that the user's clicking elsewhere while
;; it runs changes nothing.  Above 2 GB, Emacs exits.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-opencode-fixtures)
(require 'drover-list)
(require 'drover-attention)
(require 'profiler)

(defvar drover-idle-bench-output (getenv "DROVER_BENCH_OUTPUT")
  "File the results go to, or nil for the messages.")

(defvar drover-idle-bench-seconds
  (string-to-number (or (getenv "DROVER_IDLE_BENCH_SECONDS") "30"))
  "Seconds each live phase lasts.")

(defconst drover-idle-bench-top 967
  "Sessions at the top of their trees.")

(defconst drover-idle-bench-subagents 1552
  "Subagent sessions.")

(defconst drover-idle-bench-directories 53
  "Git worktrees the sessions run in.")

(defvar drover-idle-bench-root nil
  "Directory holding the worktrees.")

(defvar drover-idle-bench-focused t
  "Whether Emacs has focus, as the bench pretends.")

(defvar drover-idle-bench-frame-ticks 0
  "Frame ticks so far.")

(defvar drover-idle-bench-redisplays 0
  "Redisplays so far that looked at a window.")

(defvar drover-idle-bench-steps 0
  "Steps the working session took.")

(defun drover-idle-bench-log (format-string &rest arguments)
  "Print FORMAT-STRING with ARGUMENTS as a line of the results."
  (let ((line (apply #'format format-string arguments)))
    (if drover-idle-bench-output
        (with-temp-buffer
          (insert line "\n")
          (append-to-file (point-min) (point-max) drover-idle-bench-output))
      (message "%s" line))))

;;;; The registry

(defun drover-idle-bench-directory (index)
  "Return the worktree of session INDEX, a directory name."
  (file-name-as-directory
   (expand-file-name (format "repo-%02d" (% index drover-idle-bench-directories))
                     drover-idle-bench-root)))

(defun drover-idle-bench-make-worktrees ()
  "Make the Git worktrees, each on a branch of its own."
  (setq drover-idle-bench-root (file-name-as-directory (make-temp-file "drover-idle" t)))
  (dotimes (index drover-idle-bench-directories)
    (let ((git (expand-file-name ".git" (drover-idle-bench-directory index))))
      (make-directory git t)
      (with-temp-file (expand-file-name "HEAD" git)
        (insert (format "ref: refs/heads/feature-%02d\n" index))))))

(defun drover-idle-bench-id (kind index)
  "Return the identifier of session INDEX of KIND, `top' or `sub'."
  (format "ses_%s%08dAbCdEfGhIjKlMn" (if (eq kind 'top) "t" "s") index))

(defconst drover-idle-bench-models
  '(((id . "claude-opus-5-5") (providerID . "anthropic") (variant . "high"))
    ((id . "gpt-5.5") (providerID . "openai"))
    ((id . "claude-opus-5-5") (providerID . "github-copilot")))
  "Models the sessions run.")

(defun drover-idle-bench-entry (kind index now)
  "Return the registry entry of session INDEX of KIND at NOW."
  (let* ((sub (eq kind 'sub))
         (age (* (if sub (1+ (* index 2)) (* index 3)) 61))
         (updated (- now age 120))
         (top-index (% index drover-idle-bench-top)))
    (opencode-state--make-entry
     :id (drover-idle-bench-id kind index)
     :title (format "%s %d: investigate the registry paging and the drover list redraw"
                    (if sub "Subagent task" "Session") index)
     :directory (drover-idle-bench-directory (if sub top-index index))
     :project-id (format "prj_%02d" (% (if sub top-index index) drover-idle-bench-directories))
     :parent-id (and sub (if (zerop (% index 10))
                             (drover-idle-bench-id 'sub (/ index 2))
                           (drover-idle-bench-id 'top top-index)))
     :agent (if sub "general" (if (zerop (% index 4)) "plan" "build"))
     :model (nth (% index 3) drover-idle-bench-models)
     :context (+ 20000 (* 37 index))
     :running nil
     :outcome (if (zerop (% index 97)) 'failed 'succeeded)
     :idle updated
     :viewed (if (or (zerop (% index 97)) (zerop (% index 31))) (- updated 10) updated)
     :created (- updated 600)
     :updated updated)))

(defun drover-idle-bench-fill (now)
  "Fill the registry and the catalogs, as the service would have, at NOW.
Session 0 and its first subagent run turns."
  (clrhash opencode-state--sessions)
  (clrhash opencode-state--children)
  (dotimes (index drover-idle-bench-top)
    (let ((entry (drover-idle-bench-entry 'top index now)))
      (puthash (plist-get entry :id) entry opencode-state--sessions)))
  (dotimes (index drover-idle-bench-subagents)
    (let ((entry (drover-idle-bench-entry 'sub index now)))
      (puthash (plist-get entry :id) entry opencode-state--sessions)
      (push (plist-get entry :id) (gethash (plist-get entry :parent-id) opencode-state--children))))
  (dolist (id (list (drover-idle-bench-id 'top 0) (drover-idle-bench-id 'sub 0)))
    (let ((entry (copy-sequence (gethash id opencode-state--sessions))))
      (setq entry (plist-put entry :running t))
      (setq entry (plist-put entry :started (- now 30)))
      (setq entry (plist-put entry :updated now))
      (puthash id entry opencode-state--sessions)))
  ;; Its prompt cache lives, so that the cache timer runs.
  (puthash (drover-idle-bench-id 'top 0)
           (list :expires (+ now 3600) :ttl 3600 :tokens 90000 :prompt-tokens 100000)
           opencode-state--caches)
  (dotimes (index drover-idle-bench-directories)
    (let ((directory (opencode-normalize-directory (drover-idle-bench-directory index))))
      (puthash (cons directory 'model) drover-opencode-fixtures-models opencode--catalogs)
      (puthash (cons directory 'provider) drover-opencode-fixtures-providers opencode--catalogs)
      (puthash (cons directory 'agent) drover-opencode-fixtures-agents opencode--catalogs))))

(defun drover-idle-bench-projects ()
  "Return the service's projects, one per worktree."
  (mapcar (lambda (index)
            `((id . ,(format "prj_%02d" index)) (name . ,(format "project-%02d" index))
              (canonical . ,(directory-file-name (drover-idle-bench-directory index)))))
          (number-sequence 0 (1- drover-idle-bench-directories))))

(defun drover-idle-bench-step ()
  "Take a step of the working session's turn, as an event would."
  (cl-incf drover-idle-bench-steps)
  (let* ((id (drover-idle-bench-id 'top 0))
         (entry (copy-sequence (gethash id opencode-state--sessions))))
    (setq entry (plist-put entry :updated (float-time)))
    (setq entry (plist-put entry :context (+ 40000 (* 100 drover-idle-bench-steps))))
    (opencode-state--store id entry)))

;;;; Isolation

(defun drover-idle-bench-isolate ()
  "Keep everything from reaching an OpenCode service, or the user's state."
  (advice-add 'opencode-request :override
              (lambda (&rest _) (error "The bench reaches no OpenCode service")))
  (advice-add 'opencode-events-register :override #'ignore)
  (advice-add 'opencode-state-ensure :override (lambda (&optional then) (when then (funcall then))))
  (advice-add 'opencode-state-context :override #'ignore)
  (advice-add 'opencode-fetch-projects :override
              (lambda (callback &optional _else) (funcall callback (drover-idle-bench-projects))))
  (advice-add 'frame-focus-state :override
              (lambda (&optional _frame) drover-idle-bench-focused)))

(defun drover-idle-bench-set-focus (focused)
  "Pretend Emacs has focus when FOCUSED is non-nil, else that it lost it."
  (setq drover-idle-bench-focused focused)
  (funcall after-focus-change-function))

;;;; Measures

(defun drover-idle-bench-allocated ()
  "Return the bytes allocated so far, about."
  (+ (* 16 cons-cells-consed) (* 16 floats-consed) (* 8 vector-cells-consed)
     (* 48 symbols-consed) string-chars-consed (* 56 intervals-consed) (* 32 strings-consed)))

(defun drover-idle-bench-cpu ()
  "Return the processor time Emacs used so far, in seconds."
  (float-time (get-internal-run-time)))

(defun drover-idle-bench-snapshot ()
  "Return the counters a phase compares, as a plist."
  (list :wall (float-time) :cpu (drover-idle-bench-cpu) :bytes (drover-idle-bench-allocated)
        :gcs gcs-done :gc-time gc-elapsed :ticks drover-idle-bench-frame-ticks
        :redisplays drover-idle-bench-redisplays :steps drover-idle-bench-steps))

(defun drover-idle-bench-report (label before after)
  "Log phase LABEL from the snapshots BEFORE and AFTER."
  (cl-flet ((delta (key) (- (plist-get after key) (plist-get before key))))
    (let ((wall (delta :wall)))
      (drover-idle-bench-log
       "%-36s %5.1f s: CPU %5.2f%% | %6.3f MB/s | %3d GCs %6.1f ms | %4d frame ticks | %5d redisplays | %4d steps"
       label wall (/ (* 100 (delta :cpu)) wall) (/ (delta :bytes) wall 1e6)
       (delta :gcs) (* 1000 (delta :gc-time)) (delta :ticks) (delta :redisplays) (delta :steps)))))

(defun drover-idle-bench-guard ()
  "Exit Emacs once it holds more than 2 GB."
  (let ((rss (alist-get 'rss (process-attributes (emacs-pid)))))
    (when (and rss (> rss (* 2 1024 1024)))
      (drover-idle-bench-log "drover idle bench: %d MB, over 2 GB: stopped" (/ rss 1024))
      (kill-emacs 2))))

(defun drover-idle-bench-time (function runs)
  "Return the milliseconds FUNCTION takes, averaged over RUNS runs."
  (garbage-collect)
  (let ((start (float-time)))
    (dotimes (_ runs)
      (funcall function))
    (/ (* 1000 (- (float-time) start)) runs)))

(defun drover-idle-bench-tick-cost (reordering ticks clock-cell)
  "Return the milliseconds a frame tick and its redisplay take, TICKS times.
The lists' lines are reordered for display as REORDERING says, as
`bidi-display-reordering', or as they are when it is `as-is';
CLOCK-CELL is a cons whose car is the clock the lists read."
  (let ((buffers (list (drover-list--buffer) (drover-list--buffer t)))
        (saved nil))
    (unless (eq reordering 'as-is)
      (dolist (buffer buffers)
        (push (cons buffer (buffer-local-value 'bidi-display-reordering buffer)) saved)
        (with-current-buffer buffer
          (setq bidi-display-reordering reordering))))
    (redisplay t)
    (unwind-protect
        (drover-idle-bench-time
         (lambda ()
           (setcar clock-cell (+ (car clock-cell) opencode-session-status-animation-interval))
           (drover-list--frame-tick)
           (redisplay t))
         ticks)
      (pcase-dolist (`(,buffer . ,value) saved)
        (with-current-buffer buffer
          (setq bidi-display-reordering value))))))

(defun drover-idle-bench-redisplay-costs ()
  "Log what a frame of the animation costs to draw and to redisplay."
  (let* ((clock (list (float-time)))
         (ticks 40))
    (cl-letf (((symbol-function 'drover-list--now) (lambda () (car clock))))
      (let* ((quiet (drover-idle-bench-time (lambda () (redisplay t)) ticks))
             (draw (drover-idle-bench-time
                    (lambda ()
                      (setcar clock (+ (car clock) opencode-session-status-animation-interval))
                      (drover-list--frame-tick))
                    ticks))
             (as-is (drover-idle-bench-tick-cost 'as-is ticks clock))
             (on (drover-idle-bench-tick-cost t ticks clock))
             (off (drover-idle-bench-tick-cost nil ticks clock)))
        (drover-idle-bench-log
         "a frame of the animation: drawn %.2f ms; drawn and redisplayed %.2f ms (lines reordered: %s), %.2f ms (reordered), %.2f ms (not reordered); a redisplay changing nothing %.2f ms"
         draw as-is (buffer-local-value 'bidi-display-reordering (drover-list--buffer)) on off quiet)))))

(defun drover-idle-bench-change-costs ()
  "Log what following a change costs, a session's or the registry's."
  (let* ((step (drover-idle-bench-time (lambda () (drover-idle-bench-step) (drover-changed-now)) 50))
         (whole (drover-idle-bench-time (lambda ()
                                          (opencode-state--run-hook 'opencode-state-updated-functions nil)
                                          (drover-changed-now))
                                        10)))
    (drover-idle-bench-log "a change followed, redisplay apart: a step of a session %.2f ms; the registry, no session told, %.1f ms"
                           step whole)
    (profiler-start 'cpu+mem)
    (dotimes (_ 100)
      (drover-idle-bench-step)
      (drover-changed-now))
    (dotimes (_ 10)
      (opencode-state--run-hook 'opencode-state-updated-functions nil)
      (drover-changed-now))
    (profiler-stop)
    (drover-idle-bench-write-profile "changes")))

(defun drover-idle-bench-refresh-costs ()
  "Log what the minute's refresh costs, a branch changing or none."
  (let* ((unchanged (drover-idle-bench-time #'drover-list--age-tick 5))
         (head (expand-file-name "repo-03/.git/HEAD" drover-idle-bench-root))
         (count 0)
         (changed (drover-idle-bench-time
                   (lambda ()
                     (with-temp-file head
                       (insert (format "ref: refs/heads/moved-%d\n" (cl-incf count))))
                     (drover-list--age-tick)
                     (drover-changed-now))
                   5)))
    (drover-idle-bench-log "the minute's refresh: %.1f ms, nothing changed; %.1f ms, a worktree's branch changed"
                           unchanged changed)
    (profiler-start 'cpu)
    (dotimes (_ 5) (drover-list--age-tick))
    (profiler-stop)
    (drover-idle-bench-write-profile "refresh")))

(defun drover-idle-bench-summary (log)
  "Return the functions of profiler LOG by inclusive and self counts, as text."
  (let ((inclusive (make-hash-table :test #'equal))
        (self (make-hash-table :test #'equal))
        (names (make-hash-table :test #'eq))
        (total 0))
    (maphash (lambda (backtrace count)
               (cl-incf total count)
               (let ((seen nil)
                     (first t))
                 (seq-doseq (frame backtrace)
                   (when frame
                     (let ((name (if (symbolp frame)
                                     frame
                                   (with-memoization (gethash frame names)
                                     ;; Bounded: a closure may hold the sessions.
                                     (let ((print-length 3) (print-level 2))
                                       (truncate-string-to-width (prin1-to-string frame) 60))))))
                       (when first
                         (cl-incf (gethash name self 0) count)
                         (setq first nil))
                       (unless (member name seen)
                         (push name seen)
                         (cl-incf (gethash name inclusive 0) count)))))))
             log)
    (cl-flet ((top (table count)
                (let ((entries nil))
                  (maphash (lambda (name n) (push (cons name n) entries)) table)
                  (mapconcat (lambda (entry)
                               (format "  %5.1f%%  %s" (/ (* 100.0 (cdr entry)) (max total 1)) (car entry)))
                             (seq-take (sort entries :key #'cdr :reverse t) count)
                             "\n"))))
      (format "total %d\n-- inclusive\n%s\n-- self\n%s\n" total (top inclusive 70) (top self 40)))))

(defun drover-idle-bench-write-profile (name)
  "Write the profile just taken beside the results, as NAME."
  (when drover-idle-bench-output
    (with-temp-file (concat (file-name-sans-extension drover-idle-bench-output) "-" name ".txt")
      (when profiler-cpu-log
        (insert (format "==== %s cpu\n" name) (drover-idle-bench-summary profiler-cpu-log)))
      (when profiler-memory-log
        (insert (format "==== %s memory\n" name) (drover-idle-bench-summary profiler-memory-log))))
    (setq profiler-cpu-log nil
          profiler-memory-log nil)))

;;;; Phases

(defvar drover-idle-bench-stepper nil
  "Timer taking the working session's steps.")

(defun drover-idle-bench-phases ()
  "Return the live phases, as (LABEL SETUP TEARDOWN PROFILE)."
  (list (list "idle, focused, two glyphs animated" nil nil nil)
        (list "streaming, a step every 100 ms"
              (lambda () (setq drover-idle-bench-stepper
                               (run-at-time 0.1 0.1 #'drover-idle-bench-step)))
              (lambda () (cancel-timer drover-idle-bench-stepper))
              nil)
        (list "streaming, profiled"
              (lambda ()
                (setq drover-idle-bench-stepper (run-at-time 0.1 0.1 #'drover-idle-bench-step))
                (profiler-start 'cpu+mem))
              (lambda ()
                (profiler-stop)
                (cancel-timer drover-idle-bench-stepper)
                (drover-idle-bench-write-profile "streaming"))
              t)
        (list "unfocused, idle, glyphs still"
              (lambda ()
                (setq opencode-session-status-animation-unfocused-interval nil)
                (drover-idle-bench-set-focus nil))
              (lambda ()
                (drover-idle-bench-set-focus t)
                (custom-reevaluate-setting 'opencode-session-status-animation-unfocused-interval))
              nil)
        (list (format "unfocused, idle, glyphs every %s s" opencode-session-status-animation-unfocused-interval)
              (lambda () (drover-idle-bench-set-focus nil))
              (lambda () (drover-idle-bench-set-focus t))
              nil)
        (list "focused again, idle" nil nil nil)))

(defun drover-idle-bench-run-phases (phases)
  "Run PHASES one by one in the command loop, then the synchronous measures."
  (if (null phases)
      (progn
        (drover-idle-bench-redisplay-costs)
        (drover-idle-bench-change-costs)
        (drover-idle-bench-refresh-costs)
        (drover-idle-bench-log "drover idle bench: done, %d MB"
                               (/ (alist-get 'rss (process-attributes (emacs-pid))) 1024))
        (kill-emacs 0))
    (pcase-let ((`(,label ,setup ,teardown ,_) (car phases)))
      (when setup (funcall setup))
      ;; A second to settle, then the phase.
      (run-at-time
       1 nil
       (lambda ()
         (let ((before (drover-idle-bench-snapshot)))
           (run-at-time
            drover-idle-bench-seconds nil
            (lambda ()
              (drover-idle-bench-report label before (drover-idle-bench-snapshot))
              (when teardown (funcall teardown))
              ;; The focus is looked at once its events settled: the
              ;; next phase's moving it again would go unseen.
              (run-at-time 1 nil #'drover-idle-bench-run-phases (cdr phases))))))))))

(defun drover-idle-bench-main ()
  "Set the bench up, and run its phases from the command loop."
  ;; macOS's own arguments follow, which are no Emacs options.
  (setq command-line-args-left nil)
  (run-at-time 1 1 #'drover-idle-bench-guard)
  (run-at-time 600 nil (lambda () (drover-idle-bench-log "drover idle bench: timed out") (kill-emacs 3)))
  (condition-case err
      (progn
        (when (fboundp 'ns-block-system-sleep)
          ;; No App Nap: timers run as they run for the user.
          (ns-block-system-sleep "drover idle bench" t))
        (setq gc-cons-percentage 0.1)
        (when-let* ((font (getenv "DROVER_BENCH_FONT")))
          (set-face-attribute 'default nil :family font :height 130))
        (when-let* ((theme (getenv "DROVER_BENCH_THEME")))
          (when-let* ((directory (getenv "DROVER_BENCH_THEME_DIRECTORY")))
            (add-to-list 'custom-theme-load-path directory))
          (load-theme (intern theme) t))
        ;; As the configuration this was written for has them: no
        ;; scroll bars, tool bar, menu bar, nor blinking cursor.
        (scroll-bar-mode -1)
        (tool-bar-mode -1)
        (menu-bar-mode -1)
        (blink-cursor-mode -1)
        (set-frame-size (selected-frame) 240 60)
        (drover-idle-bench-isolate)
        (drover-idle-bench-make-worktrees)
        (drover-idle-bench-fill (float-time))
        (advice-add 'drover-list--frame-tick :before
                    (lambda (&rest _) (cl-incf drover-idle-bench-frame-ticks)))
        (add-function :before pre-redisplay-function
                      (lambda (&rest _) (cl-incf drover-idle-bench-redisplays)))
        (setq drover-idle-bench-focused t)
        (drover-start)
        (drover-attention-mode 1)
        (delete-other-windows)
        (switch-to-buffer (drover-list--buffer))
        (drover-list--show-side)
        ;; The user works in another window, below the list.
        (select-window (split-window (selected-window) -15 'below))
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (redisplay t)
        (drover-idle-bench-log "drover idle bench: %d sessions in %d worktrees; %s, frame %dx%d, %s"
                               (hash-table-count opencode-state--sessions) drover-idle-bench-directories
                               (if (display-graphic-p) (format "%s %s" window-system (frame-parameter nil 'font)) "batch")
                               (frame-width) (frame-height) emacs-version)
        (dolist (buffer (list (drover-list--buffer) (drover-list--buffer t)))
          (with-current-buffer buffer
            (drover-idle-bench-log "  %s: %d characters, %d overlays, %d lines shown in a window %d columns wide"
                                   (buffer-name) (buffer-size)
                                   (length (overlays-in (point-min) (point-max)))
                                   (window-body-height (get-buffer-window buffer))
                                   (window-body-width (get-buffer-window buffer)))))
        (run-at-time 5 nil #'drover-idle-bench-run-phases (drover-idle-bench-phases)))
    (error (drover-idle-bench-log "drover idle bench failed: %s" (drover-error-string err))
           (kill-emacs 1))))

(provide 'drover-idle-bench)
;;; drover-idle-bench.el ends here
