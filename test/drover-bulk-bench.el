;;; drover-bulk-bench.el --- Time actions on thousands of sessions at once  -*- lexical-binding: t; -*-

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

;; Times an action on every one of the 2510 sessions of `drover-bench'
;; at once, the list showing them, as when the user archives a
;; selection of thousands: how long the command keeps Emacs, the
;; longest Emacs then goes without running a timer (a timer ticks every
;; hundredth of a second), how long until the last session settled and
;; the list showed it, how many requests were in flight at most, and
;; what the list cost.
;;
;; The fake backend's actions are requests: each costs the main thread
;; what opencode.el's sending one costs, about 0.3 ms, and its archive
;; is answered 5 to 50 ms later, a few of them refused.  A tenth of the
;; sessions have buffers, which archiving closes.  Three actions are
;; timed: the backend's archive, confirmed; drover's own archive, for a
;; backend without one, which drover keeps in a file; marking seen, a
;; request no answer is awaited of.
;;
;;   make bench-bulk
;;   make bench-bulk EMACS_GUI=/Applications/Emacs.app/Contents/MacOS/Emacs
;;
;; `make bench' runs it too.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-bench)
(require 'drover-list)

(defconst drover-bulk-bench-request-cost 0.0003
  "Seconds of the main thread one request costs, as opencode.el sends it.")

(defconst drover-bulk-bench-failing '("ses_t00000007AbCdEfGhIjKlMn" "ses_t00000300AbCdEfGhIjKlMn"
                                      "ses_s00000999AbCdEfGhIjKlMn")
  "Sessions whose archive the fake service refuses.")

(defvar drover-bulk-bench--rate nil
  "Turns of an empty loop a second, as `drover-bulk-bench-busy' measured them.")

(defun drover-bulk-bench-busy (seconds)
  "Keep the main thread busy for SECONDS, running nothing else.
It allocates nothing, as reading the clock in a loop would: the garbage
collections of the bench are drover's."
  (unless drover-bulk-bench--rate
    (let ((start (float-time)))
      (dotimes (_ 10000000))
      (setq drover-bulk-bench--rate (/ 10000000 (- (float-time) start)))))
  (dotimes (_ (round (* seconds drover-bulk-bench--rate)))))

(defvar drover-bulk-bench--state nil
  "The state of the fake service of the bench run, a plist.")

(defmacro drover-bulk-bench--slot (key)
  "Return the place of KEY in the bench's state."
  `(plist-get drover-bulk-bench--state ,key))

(defun drover-bulk-bench--register (name own)
  "Register the fake backend NAME over `drover-test-sessions'.
With OWN non-nil, it archives nothing itself: drover archives its
sessions, which tell their conversations."
  (let ((archived (drover-bulk-bench--slot :archived))
        (by-id (make-hash-table :test #'equal))
        (buffers (drover-bulk-bench--slot :buffers)))
    (dolist (session drover-test-sessions)
      (puthash (drover-session-id session) session by-id))
    (apply
     #'drover-test-register-simple name
     (lambda ()
       (delq nil (mapcar (lambda (session)
                           (unless (gethash (drover-session-id session) archived)
                             (copy-sequence session)))
                         drover-test-sessions)))
     :session (lambda (id)
                (when-let* ((session (gethash id by-id))
                            ((not (gethash id archived))))
                  (copy-sequence session)))
     :buffer (lambda (session)
               (let ((buffer (gethash (drover-session-id session) buffers)))
                 (and (buffer-live-p buffer) buffer)))
     :close (lambda (session)
              (when-let* ((buffer (gethash (drover-session-id session) buffers))
                          ((buffer-live-p buffer)))
                (prog1 (drover-keep-unsent (list (with-current-buffer buffer (buffer-string))))
                  (drover-kill-buffer buffer))))
     :mark-seen (lambda (_session)
                  (drover-bulk-bench-busy drover-bulk-bench-request-cost)
                  (cl-incf (drover-bulk-bench--slot :asked))
                  (cl-incf (drover-bulk-bench--slot :answered)))
     (unless own
       (list
        :archive-confirms t
        :archive
        (lambda (session then else)
          (let ((id (drover-session-id session)))
            (drover-bulk-bench-busy drover-bulk-bench-request-cost)
            (cl-incf (drover-bulk-bench--slot :asked))
            (setf (drover-bulk-bench--slot :flight) (1+ (drover-bulk-bench--slot :flight)))
            (setf (drover-bulk-bench--slot :most)
                  (max (drover-bulk-bench--slot :most) (drover-bulk-bench--slot :flight)))
            (run-at-time (/ (+ 5 (random 46)) 1000.0) nil
                         (lambda ()
                           (cl-decf (drover-bulk-bench--slot :flight))
                           (cl-incf (drover-bulk-bench--slot :answered))
                           (drover-bulk-bench-busy (/ drover-bulk-bench-request-cost 3))
                           (if (member id drover-bulk-bench-failing)
                               (funcall else "PATCH /api/session failed (HTTP 500): busy")
                             (puthash id t archived)
                             (drover-backend-changed name id)
                             (funcall then)))))))))))

(defun drover-bulk-bench--sessions (own)
  "Return the sessions of the bench; with OWN, telling their conversations."
  (mapcar (lambda (session)
            (when own
              (setf (drover-session-engine session) 'claude-code
                    (drover-session-engine-id session) (drover-session-id session)))
            (setf (drover-session-status session) 'done)
            session)
          (drover-bench-sessions)))

(defun drover-bulk-bench--busy-p ()
  "Return non-nil while drover still works on an action of several sessions."
  (and (boundp 'drover--bulk-jobs) (symbol-value 'drover--bulk-jobs)))

(defun drover-bulk-bench-one (label action own)
  "Time ACTION, a function of the sessions, on every session; log it as LABEL.
With OWN, drover archives the sessions itself."
  (setq drover-bulk-bench--state
        (list :archived (make-hash-table :test #'equal) :buffers (make-hash-table :test #'equal)
              :asked 0 :answered 0 :flight 0 :most 0))
  (drover-test-with-backend (drover-bulk-bench--sessions own)
    (drover-test-with-state
      (let ((drover-group-by nil)
            (drover-sort-by 'updated)
            (drover-sort-descending t)
            (drover-list-age-interval nil)
            (drover-list-cache-interval nil)
            (opencode-session-status-animation-interval nil)
            (drover-list-deferred-delay nil)
            (kill-ring nil)
            (buffers (drover-bulk-bench--slot :buffers))
            (index 0))
        (setq drover--backends nil)
        (drover-bulk-bench--register 'fake own)
        (dolist (session drover-test-sessions)
          (when (zerop (% (cl-incf index) 10))
            (let ((buffer (generate-new-buffer (format " *bulk %s*" (drover-session-id session)))))
              (when (zerop (% index 100))
                (with-current-buffer buffer (insert "unsent")))
              (puthash (drover-session-id session) buffer buffers))))
        (let ((list (drover-list--buffer))
              (updates 0) (update-most 0.0) (update-keys 0)
              (renders 0) (render-most 0.0)
              (slices 0) (slice-most 0.0) (slice-total 0.0)
              (report nil) (warnings nil))
          (cl-letf (((symbol-function 'drover-list--now) (lambda () drover-bench-now)))
            (drover-start)
            (delete-other-windows)
            (set-window-buffer (selected-window) list)
            (with-current-buffer list
              (drover-list--revert t)
              (drover-changed-now)
              (redisplay t)
              (let* ((rows (hash-table-count (drover-list--model-rows drover-list--model)))
                     (sessions (drover-index-sessions))
                     (timed-update (lambda (run &rest arguments)
                                     (let ((start (float-time))
                                           (keys (if (listp drover--changed-keys) (length drover--changed-keys) 0)))
                                       (prog1 (apply run arguments)
                                         (let ((took (- (float-time) start)))
                                           (cl-incf updates)
                                           (when (> took update-most)
                                             (setq update-most took update-keys keys)))))))
                     (timed-render (lambda (run &rest arguments)
                                     (let ((start (float-time)))
                                       (prog1 (apply run arguments)
                                         (cl-incf renders)
                                         (setq render-most (max render-most (- (float-time) start)))))))
                     (timed-slice (lambda (run &rest arguments)
                                    (let ((start (float-time)))
                                      (prog1 (apply run arguments)
                                        (let ((took (- (float-time) start)))
                                          (cl-incf slices)
                                          (cl-incf slice-total took)
                                          (setq slice-most (max slice-most took)))))))
                     (said (lambda (format-string &rest arguments)
                             (when format-string
                               (setq report (apply #'format-message format-string arguments)))))
                     (warned (lambda (&rest arguments)
                               (push (apply #'format-message arguments) warnings)))
                     (last (float-time))
                     (lag 0.0)
                     (long 0)
                     (ticker (run-at-time 0 0.01 (lambda ()
                                                   (let* ((now (float-time))
                                                          (gap (- now last)))
                                                     (when (> gap 0.05) (cl-incf long))
                                                     (setq lag (max lag gap)
                                                           last now)))))
                     start command total gcs gc-time consed)
                (advice-add 'drover--run-changed :around timed-update)
                (advice-add 'drover-list--render :around timed-render)
                (when (fboundp 'drover--bulk-run)
                  (advice-add 'drover--bulk-run :around timed-slice))
                (advice-add 'message :before said)
                (advice-add 'drover-warn :before warned)
                (unwind-protect
                    (progn
                      (sit-for 0.05)
                      (setq lag 0.0 long 0 last (float-time) start (float-time)
                            gcs gcs-done gc-time gc-elapsed
                            consed (apply #'+ (memory-use-counts)))
                      (funcall action sessions)
                      (setq command (- (float-time) start))
                      (let ((deadline (+ start 300)))
                        (while (and (< (float-time) deadline)
                                    (or (drover-bulk-bench--busy-p)
                                        (< (drover-bulk-bench--slot :answered)
                                           (drover-bulk-bench--slot :asked))
                                        (timerp drover--change-timer)))
                          ;; Timers run meanwhile, the ticker's too; a
                          ;; shorter wait would redisplay more often than
                          ;; Emacs does while the user waits.
                          (sit-for 0.05)))
                      (setq total (- (float-time) start)
                            gcs (- gcs-done gcs) gc-time (- gc-elapsed gc-time)
                            consed (- (apply #'+ (memory-use-counts)) consed))
                      (setq lag (max lag (- (float-time) last))))
                  (cancel-timer ticker)
                  (advice-remove 'drover--run-changed timed-update)
                  (advice-remove 'drover-list--render timed-render)
                  (when (fboundp 'drover--bulk-run)
                    (advice-remove 'drover--bulk-run timed-slice))
                  (advice-remove 'message said)
                  (advice-remove 'drover-warn warned))
                (let ((left (hash-table-count (drover-list--model-rows drover-list--model)))
                      (open (seq-count #'buffer-live-p (hash-table-values buffers)))
                      (same (drover-bench-same-p)))
                  (drover-bench-log "%s: %d sessions, %d rows, %d buffers, %d kill-ring entries"
                                    label (length sessions) rows (hash-table-count buffers) (length kill-ring))
                  (drover-bench-log "  command %7.1f ms | longest block %7.1f ms%s, %d over 50 ms | done after %7.1f ms | %d requests, %d in flight at most"
                                    (* 1000 command) (* 1000 lag) (if (> lag 0.05) "!" " ") long (* 1000 total)
                                    (drover-bulk-bench--slot :asked) (drover-bulk-bench--slot :most))
                  (drover-bench-log "  %d GCs, %.1f ms; %.1f M objects allocated, %.0f a session"
                                    gcs (* 1000 gc-time) (/ consed 1e6) (/ consed (float (length sessions))))
                  (when (> slices 0)
                    (drover-bench-log "  %d slices, the longest %.1f ms, %.1f ms in all"
                                      slices (* 1000 slice-most) (* 1000 slice-total)))
                  (drover-bench-log "  %d updates, the longest %.1f ms (%d keys); %d whole drawings, the longest %.1f ms; %d rows, %d buffers left; the same as drawn anew: %s"
                                    updates (* 1000 update-most) update-keys renders (* 1000 render-most)
                                    left open (if same "yes" "NO"))
                  (drover-bench-log "  report: %s" (drover-bound-line (or (car warnings) report "") 240))))))
          (kill-buffer list))
        (dolist (buffer (hash-table-values buffers))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

;; Compiled, as a backend is: interpreted, the fake service would
;; allocate as it runs, each turn of a loop.
(mapc #'byte-compile '(drover-bulk-bench-busy drover-bulk-bench--register))

(defun drover-bulk-bench-run ()
  "Time actions on every session at once."
  (setq gc-cons-percentage 0.1)
  (ignore-errors (set-frame-size (selected-frame) 200 60))
  (drover-bulk-bench-busy 0)
  (drover-bench-log "drover bulk bench: %s"
                    (if (display-graphic-p) (format "%s %s" window-system (face-font 'default)) "batch"))
  (drover-bulk-bench-one "archived by the backend, each confirmed" #'drover-archive nil)
  (drover-bulk-bench-one "archived by drover, into its file" #'drover-archive t)
  (drover-bulk-bench-one "marked seen, unconfirmed requests" #'drover-mark-seen nil))

(defun drover-bulk-bench-main ()
  "Run the bulk bench, and print the results; a graphical Emacs then exits."
  (if (not drover-bench-output)
      (drover-bulk-bench-run)
    (unwind-protect
        (condition-case err
            (drover-bulk-bench-run)
          (error (drover-bench-log "drover bulk bench failed: %s" (drover-error-string err))))
      (kill-emacs 0))))

(provide 'drover-bulk-bench)
;;; drover-bulk-bench.el ends here
