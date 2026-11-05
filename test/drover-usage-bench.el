;;; drover-usage-bench.el --- Times the totals of usage  -*- lexical-binding: t; -*-

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

;; `make bench-usage' writes, in a throwaway home, OpenCode's database
;; of 2500 sessions, 1000 at the top of their trees and 1500 subagents,
;; 30 steps each, and 120 Claude Code transcripts, 20 of 2 MB, with
;; subagents, and times: reading it all the first time, from timers in
;; slices, and the longest Emacs went without running a timer
;; meanwhile; the facts made of it; each session's totals; the report
;; by each grouping; the list drawn whole with the column of costs and
;; without; a turn appended and steps of OpenCode added, read; and an
;; Emacs starting from what was kept.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-list)
(require 'drover-usage)

(defconst drover-usage-bench-roots 1000 "OpenCode sessions at the top of their trees.")
(defconst drover-usage-bench-subagents 1500 "OpenCode subagent sessions.")
(defconst drover-usage-bench-steps 30 "Steps of each OpenCode session.")
(defconst drover-usage-bench-transcripts 120 "Claude Code conversations.")
(defconst drover-usage-bench-large 20 "Claude Code conversations of 2 MB.")

(defun drover-usage-bench-log (format-string &rest arguments)
  "Print FORMAT-STRING with ARGUMENTS as a line of the results."
  (message "%s" (apply #'format format-string arguments)))

(defun drover-usage-bench-ms (function)
  "Return how long FUNCTION takes, in milliseconds, and its collections, as (MS . GCS)."
  (garbage-collect)
  (let ((start (float-time))
        (gcs gcs-done))
    (funcall function)
    (cons (* 1000 (- (float-time) start)) (- gcs-done gcs))))

(defun drover-usage-bench-id (kind index)
  "Return the identifier of OpenCode session INDEX of KIND, `root' or `sub'."
  (format "ses_%s%08dAbCdEfGh" (if (eq kind 'root) "r" "s") index))

(defun drover-usage-bench-opencode (home now)
  "Write OpenCode's database of the bench under HOME, its steps before NOW."
  (let ((db (sqlite-open drover-opencode-database))
        (sessions (append (mapcar (lambda (i) (list (drover-usage-bench-id 'root i) nil))
                                  (number-sequence 0 (1- drover-usage-bench-roots)))
                          (mapcar (lambda (i) (list (drover-usage-bench-id 'sub i)
                                                    (drover-usage-bench-id 'root (% i drover-usage-bench-roots))))
                                  (number-sequence 0 (1- drover-usage-bench-subagents)))))
        (serial 0)
        ;; The text of a step, as long as the content OpenCode keeps of one.
        (content (make-string 8000 ?c)))
    (sqlite-execute db "CREATE TABLE session_v2 (id text PRIMARY KEY, parent_id text, directory text, title text, time_updated integer)")
    (sqlite-execute db "CREATE TABLE session_message (id text PRIMARY KEY, session_id text NOT NULL, type text NOT NULL, time_created integer NOT NULL, data text NOT NULL)")
    (sqlite-execute db "CREATE INDEX session_message_time_created_idx ON session_message (time_created)")
    (sqlite-transaction db)
    (pcase-dolist (`(,id ,parent) sessions)
      (sqlite-execute db "INSERT INTO session_v2 VALUES (?, ?, ?, ?, ?)"
                      (list id parent (format "%swork/project-%02d/" home (% (sxhash id) 17))
                            (format "Investigate %s" id) (round (* 1000 now))))
      (dotimes (step drover-usage-bench-steps)
        (let ((time (round (* 1000 (- now (* 60 (cl-incf serial)))))))
          (sqlite-execute
           db "INSERT INTO session_message VALUES (?, ?, 'assistant', ?, ?)"
           (list (format "msg_%010d" serial) id time
                 (json-serialize
                  `((id . ,(format "msg_%010d" serial)) (type . "assistant") (content . ,content)
                    (model . ((id . "claude-opus-5-5")
                              (providerID . ,(if (cl-evenp step) "github-copilot" "ai-gateway-anthropic"))))
                    (time . ((created . ,time) (completed . ,(+ time 1000))))
                    (tokens . ((input . 3) (output . 400) (reasoning . 50)
                               (cache . ((read . 80000) (write . 2000)))))
                    (cost . 0.0731))))))))
    (sqlite-commit db)
    (sqlite-close db)))

(defun drover-usage-bench-claude (home now)
  "Write the Claude Code transcripts of the bench under HOME, before NOW."
  (dotimes (index drover-usage-bench-transcripts)
    (let* ((id (format "%08x-0000-4000-8000-%012x" index index))
           (directory (format "%swork/project-%02d/" home (% index 17)))
           (file (drover-fixture-claude id directory :start (- now (* index 3600))
                                        :subagents (and (zerop (% index 10))
                                                        '(("agent-a1" "Explore" "Explore")))))
           (replies (if (< index drover-usage-bench-large) 400 40))
           (lines nil))
      (dotimes (reply replies)
        (let ((time (- now (* index 3600) (- replies reply))))
          (push (drover-fixture-claude-assistant
                 id directory time (make-string 4000 ?r) :message (format "msg_%d_%d" index reply)
                 :usage (drover-fixture-claude-usage :input 3 :minutes 500 :read 60000 :output 300))
                lines)))
      (apply #'drover-fixture-claude-append file (nreverse lines)))))

(defun drover-usage-bench-run-timers (predicate)
  "Run timers until PREDICATE returns nil; return the longest gap between ticks, in ms.
A tick every 10 ms tells how long Emacs went without running timers."
  (let* ((last (float-time))
         (lag 0)
         (tick (run-at-time 0 0.01 (lambda ()
                                     (let ((now (float-time)))
                                       (setq lag (max lag (- now last))
                                             last now))))))
    (unwind-protect
        (while (funcall predicate)
          (accept-process-output nil 0.005))
      (cancel-timer tick))
    (* 1000 lag)))

(defun drover-usage-bench-main ()
  "Time the totals of usage, as the commentary tells."
  (drover-test-with-homes
    (drover-test-with-backend nil
      (let* ((now (float-time))
             (drover-usage-prices '(("opus" :input 5 :output 25 :cache-read 0.5 :cache-write 6.25)))
             (drover-list-usage-function #'drover-usage-session-text)
             (records nil))
        (make-directory (file-name-directory drover-opencode-database) t)
        (let ((written (drover-usage-bench-ms (lambda ()
                                                (drover-usage-bench-opencode home now)
                                                (drover-usage-bench-claude home now)))))
          (drover-usage-bench-log "Wrote %d OpenCode steps and %d transcripts in %.1f s, %.0f MB"
                                  (* drover-usage-bench-steps (+ drover-usage-bench-roots drover-usage-bench-subagents))
                                  drover-usage-bench-transcripts (/ (car written) 1000)
                                  (/ (+ (file-attribute-size (file-attributes drover-opencode-database))
                                        (string-to-number
                                         (shell-command-to-string
                                          (format "du -sk %s | cut -f1"
                                                  (shell-quote-argument (drover-claude-code-projects-directory))))))
                                     1e6)))
        ;; The first read, from timers, as in the user's Emacs.
        (let* ((start (float-time))
               (lag nil))
          (drover-usage-start)
          (setq lag (drover-usage-bench-run-timers (lambda ()
                                                     (or (timerp drover-usage--starting)
                                                         (drover-usage-reading-p)
                                                         (< (- (float-time) start) 0.2)))))
          (drover-usage-bench-log "First read, in slices: %.1f s, the longest without timers %.0f ms"
                                  (- (float-time) start) lag))
        (let ((facts (drover-usage-bench-ms #'drover-usage--current-facts)))
          (drover-usage-bench-log "Facts made: %.1f ms (%d collections), %d facts" (car facts) (cdr facts)
                                  (length (drover-usage--facts-facts drover-usage--facts))))
        (dotimes (index drover-usage-bench-roots)
          (let ((id (drover-usage-bench-id 'root index)))
            (push (drover-session-create :backend 'fake :id id :title id :status 'idle :updated now
                                         :engine 'opencode :engine-id id)
                  records)))
        (dotimes (index drover-usage-bench-subagents)
          (let ((id (drover-usage-bench-id 'sub index)))
            (push (drover-session-create :backend 'fake :id id :title id :status 'idle :updated now
                                         :parent (drover-usage-bench-id 'root (% index drover-usage-bench-roots))
                                         :engine 'opencode :engine-id id)
                  records)))
        (let ((table (drover-usage-bench-ms
                      (lambda () (dolist (record records) (drover-usage-session-totals record))))))
          (drover-usage-bench-log "Totals of %d sessions, their table made: %.1f ms" (length records) (car table)))
        (let ((lookups (drover-usage-bench-ms
                        (lambda () (dolist (record records) (drover-usage-session-text record 'column))))))
          (drover-usage-bench-log "Their costs, as the column shows them: %.1f ms" (car lookups)))
        (dolist (grouping '(session project harness provider model host day))
          (dolist (period '(today all))
            (let ((report (drover-usage-bench-ms (lambda () (drover-usage-report period grouping 'model)))))
              (drover-usage-bench-log "Report of %s by %s: %.1f ms" period grouping (car report)))))
        ;; The list of those sessions drawn whole, with and without costs.
        (setq drover-test-sessions records)
        (let ((drover-group-by nil))
          (save-window-excursion
            (drover-list)
            (dolist (hidden '((cost) nil))
              (let ((drover-list-hidden-fields hidden))
                (let ((drawn (drover-usage-bench-ms (lambda () (with-current-buffer (drover-list--buffer) (drover-list--render) (drover-list--draw-deferred))))))
                  (drover-usage-bench-log "The list drawn whole %s: %.1f ms"
                                          (if hidden "without costs" "with costs") (car drawn)))))
            (kill-buffer (drover-list--buffer))))
        ;; A turn going on: a reply appended, steps added.
        (let ((file (drover-fixture-claude-file (format "%08x-0000-4000-8000-%012x" 3 3)
                                                (format "%swork/project-%02d/" home 3)))
              (db (sqlite-open drover-opencode-database)))
          (drover-fixture-claude-append
           file (drover-fixture-claude-assistant (format "%08x-0000-4000-8000-%012x" 3 3)
                                                 (format "%swork/project-%02d/" home 3) now "More"
                                                 :message "msg_new"
                                                 :usage (drover-fixture-claude-usage :output 10)))
          (dotimes (index 10)
            (sqlite-execute db "INSERT INTO session_message VALUES (?, ?, 'assistant', ?, ?)"
                            (list (format "msg_new_%d" index) (drover-usage-bench-id 'root 1)
                                  (round (* 1000 now))
                                  (json-serialize `((model . ((id . "claude-opus-5-5") (providerID . "x")))
                                                    (time . ((created . 1) (completed . 2)))
                                                    (tokens . ((input . 1) (output . 1) (reasoning . 0)
                                                               (cache . ((read . 0) (write . 0)))))
                                                    (cost . 0.01))))))
          (sqlite-close db)
          (let ((incremental (drover-usage-bench-ms #'drover-usage-scan))
                (facts (drover-usage-bench-ms #'drover-usage--current-facts)))
            (drover-usage-bench-log "A reply and 10 steps read: %.1f ms, the facts made again %.1f ms"
                                    (car incremental) (car facts))))
        (drover-usage-stop)
        (dolist (file (directory-files drover-state-directory t "usage"))
          (drover-usage-bench-log "Kept: %s, %.0f kB" (file-name-nondirectory file)
                                  (/ (file-attribute-size (file-attributes file)) 1000.0)))
        (setq drover-usage--facts nil)
        (let ((again (drover-usage-bench-ms (lambda () (drover-usage-scan) (drover-usage--current-facts)))))
          (drover-usage-bench-log "An Emacs starting from what was kept: %.1f ms" (car again)))
        (drover-usage-stop)
        (drover-usage-bench-log "Resident memory: %d MB"
                                (/ (or (cdr (assq 'rss (process-attributes (emacs-pid)))) 0) 1024))))))

(provide 'drover-usage-bench)
;;; drover-usage-bench.el ends here
