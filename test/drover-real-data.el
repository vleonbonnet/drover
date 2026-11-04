;;; drover-real-data.el --- Index the user's own transcripts and rollouts, read only  -*- lexical-binding: t; -*-

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

;; `make real-data' runs the `claude-code' and `codex' backends over
;; the transcripts and rollouts of the user running it, ~/.claude and
;; ~/.codex, or DROVER_CLAUDE_HOME and DROVER_CODEX_HOME, and reports
;; what they list and how long it takes: the first look, reading every
;; file, and the next Emacs's, reading what was kept; the longest texts
;; listed; what could not be read; then the list, with the 2510
;; sessions of `make bench' besides, drawn whole and following changes,
;; in the default view and grouped by status.
;;
;; It reads those directories and writes nothing there: what drover
;; keeps goes to a directory of its own, deleted after; it runs neither
;; claude nor codex.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-list)
(require 'drover-claude-code-transcripts)
(require 'drover-codex-rollouts)

(require 'drover-bench)

(defvar drover-real-data-messages nil
  "The messages shown while indexing, newest first.")

(defun drover-real-data-log (format-string &rest arguments)
  "Print FORMAT-STRING with ARGUMENTS as a line of the report."
  (princ (concat (apply #'format format-string arguments) "\n") #'external-debugging-output))

(defun drover-real-data-rss ()
  "Return the resident memory of this Emacs, in MB."
  (/ (string-to-number (shell-command-to-string (format "ps -o rss= -p %d" (emacs-pid)))) 1024.0))

(defun drover-real-data-time (function)
  "Return the seconds FUNCTION takes, and its value, as (SECONDS . VALUE)."
  (let* ((start (float-time))
         (value (funcall function)))
    (cons (- (float-time) start) value)))

(defun drover-real-data-longest (records reader)
  "Return the longest of what READER gives of RECORDS, as (LENGTH . RECORD)."
  (let ((best (cons 0 nil)))
    (dolist (record records)
      (let ((text (funcall reader record)))
        (when (and (stringp text) (> (length text) (car best)))
          (setq best (cons (length text) record)))))
    best))

(defun drover-real-data-run ()
  "Index the user's transcripts and rollouts, list them, and report."
  (setq gc-cons-percentage 0.1)
  (let* ((state (make-temp-file "drover-real-data" t))
         (drover-state-directory (file-name-as-directory state))
         (drover-claude-code-home (or (getenv "DROVER_CLAUDE_HOME") (expand-file-name "~/.claude")))
         (drover-codex-home (or (getenv "DROVER_CODEX_HOME") (expand-file-name "~/.codex")))
         (drover-claude-code-agents-interval nil)
         (drover-codex-program "drover-real-data-no-codex")
         (bytes 0))
    (unwind-protect
        (drover-test-with-backend nil
          (let ((drover-claude-code-transcripts--users nil)
                (drover-claude-code-transcripts--index nil)
                (drover-claude-code-transcripts--agents (make-hash-table :test #'equal))
                (drover-codex-rollouts--users nil)
                (drover-codex-rollouts--index nil)
                (drover-codex-rollouts--names nil)
                (drover-list-age-interval nil)
                (drover-list-cache-interval nil)
                (opencode-session-status-animation-interval nil)
                (drover-list-deferred-delay nil))
            (setq drover--backends nil)
            (drover-claude-code-transcripts-install 'real-data)
            (drover-codex-rollouts-install 'real-data)
            (advice-add 'message :before
                        (lambda (format &rest arguments)
                          (when format
                            (push (apply #'format-message format arguments) drover-real-data-messages))))
            (drover-real-data-log "drover real data: %s and %s, read only; drover's state in %s"
                                  drover-claude-code-home drover-codex-home state)
            (let* ((claude drover-claude-code-transcripts--index)
                   (codex drover-codex-rollouts--index)
                   (rss-before (drover-real-data-rss))
                   (cold (cl-letf* ((insert (symbol-function 'drover-transcript-insert))
                                    ((symbol-function 'drover-transcript-insert)
                                     (lambda (file beg end)
                                       (cl-incf bytes (- end beg))
                                       (funcall insert file beg end))))
                           (drover-real-data-time (lambda ()
                                                    (drover-transcript-scan claude)
                                                    (drover-transcript-scan codex)))))
                   (files (+ (hash-table-count (drover-transcript-index-entries claude))
                             (hash-table-count (drover-transcript-index-entries codex))))
                   (unreadable (let ((count 0))
                                 (dolist (index (list claude codex))
                                   (maphash (lambda (_ entry) (unless (drover-transcript-data entry) (cl-incf count)))
                                            (drover-transcript-index-entries index)))
                                 count))
                   (again (drover-real-data-time (lambda ()
                                                   (drover-transcript-scan claude)
                                                   (drover-transcript-scan codex))))
                   (kept (drover-real-data-time (lambda ()
                                                  (drover-transcript-save claude t)
                                                  (drover-transcript-save codex t))))
                   (warm (drover-real-data-time
                          (lambda ()
                            (let ((claude (drover-claude-code-transcripts--make-index))
                                  (codex (drover-codex-rollouts--make-index))
                                  (read 0))
                              (cl-letf* ((insert (symbol-function 'drover-transcript-insert))
                                         ((symbol-function 'drover-transcript-insert)
                                          (lambda (file beg end)
                                            (cl-incf read (- end beg))
                                            (funcall insert file beg end))))
                                ;; The next Emacs, its changes told to nobody.
                                (cl-letf (((symbol-function 'drover-backend-changed) #'ignore))
                                  (drover-transcript-scan claude)
                                  (drover-transcript-scan codex)))
                              read))))
                   (claude-records (drover-claude-code-transcripts--sessions))
                   (codex-records (drover-codex-rollouts--sessions))
                   (codex-archived (let (archived)
                                     (drover-codex-rollouts--archived (lambda (records) (setq archived records)))
                                     archived))
                   (records (append claude-records codex-records codex-archived))
                   (conversations (seq-remove #'drover-session-parent claude-records))
                   (titles (drover-real-data-longest records #'drover-session-title))
                   (details (drover-real-data-longest records #'drover-session-detail))
                   (untitled (seq-count (lambda (record) (null (drover-session-title record))) records))
                   (odd (seq-filter (lambda (record)
                                      (let ((title (or (drover-session-title record) "")))
                                        (or (string-match-p "\\`[[{<\"]" title)
                                            (string-match-p "[\x3fff80-\x3fffff]" title))))
                                    records))
                   (listed-claude 0)
                   (unlisted 0))
              (drover-transcript-map (lambda (_ data)
                                       (if (drover-claude-code-transcripts--listed-p data)
                                           (cl-incf listed-claude)
                                         (cl-incf unlisted)))
                                     claude)
              (drover-changed-now)
              (drover-real-data-log "  files: %d indexed, %d unreadable; Claude Code: %d conversations listed, %d subagents, %d transcripts left out (no message or title); Codex: %d listed, %d archived"
                                    files unreadable (length conversations)
                                    (- (length claude-records) (length conversations))
                                    unlisted (length codex-records) (length codex-archived))
              (drover-real-data-log "  first look, cold: %.0f ms, %.1f MB read | again, nothing changed: %.0f ms | kept: %.0f ms, %d kB | next Emacs, warm: %.0f ms, %.1f MB read"
                                    (* 1000 (car cold)) (/ bytes 1048576.0) (* 1000 (car again))
                                    (* 1000 (car kept))
                                    (/ (+ (file-attribute-size (file-attributes (drover-transcript-cache-file claude)))
                                          (or (file-attribute-size (file-attributes (drover-transcript-cache-file codex))) 0))
                                       1024)
                                    (* 1000 (car warm)) (/ (cdr warm) 1048576.0))
              (drover-real-data-log "  longest title: %d characters (%s) | longest detail: %d | untitled: %d | odd titles: %d %S"
                                    (car titles)
                                    (and (cdr titles) (drover-session-id (cdr titles)))
                                    (car details) untitled (length odd)
                                    (mapcar (lambda (record)
                                              (cons (drover-session-id record)
                                                    (substring (drover-session-title record) 0
                                                               (min 60 (length (drover-session-title record))))))
                                            odd))
              (drover-real-data-log "  memory: %.0f MB before indexing, %.0f MB after" rss-before (drover-real-data-rss))
              (drover-real-data-states conversations codex-records)
              (drover-real-data-list (length records)))))
      (advice-mapc (lambda (advice _) (advice-remove 'message advice)) 'message)
      (let ((long (seq-filter (lambda (message) (> (length message) 300)) drover-real-data-messages)))
        (drover-real-data-log "  messages: %d, %d longer than 300 characters%s"
                              (length drover-real-data-messages) (length long)
                              (if drover-real-data-messages
                                  (format "; the last: %S" (substring (car drover-real-data-messages) 0
                                                                      (min 200 (length (car drover-real-data-messages)))))
                                "")))
      (delete-directory state t))))

(defun drover-real-data-counts (records function)
  "Return how many of RECORDS each value of FUNCTION has, as a sorted alist."
  (let ((counts nil))
    (dolist (record records)
      (cl-incf (alist-get (funcall function record) counts 0 nil #'equal)))
    (sort counts :key (lambda (entry) (format "%s" (car entry))))))

(defun drover-real-data-states (claude codex)
  "Report the statuses, contexts and caches of the CLAUDE and CODEX conversations.
Their statuses as listed, drover reading them for the first time, every
reply before counting as seen; then as they would be had drover first
read them before any, every turn's end unseen."
  (let ((now (float-time)))
    (cl-flet ((report (label records)
                (drover-real-data-log "  %s: %d, statuses %S" label (length records)
                                      (drover-real-data-counts records #'drover-session-status))))
      (report "Claude Code, first sight" claude)
      (report "Codex, first sight" codex)
      (let ((table (drover-transcript--seen-table)))
        (puthash (list drover-claude-code-transcripts--name) 0.0 table)
        (puthash (list drover-codex-rollouts--name) 0.0 table))
      (let ((claude (seq-remove #'drover-session-parent (drover-claude-code-transcripts--sessions)))
            (codex (drover-codex-rollouts--sessions)))
        (report "Claude Code, every turn's end unseen" claude)
        (report "Codex, every turn's end unseen" codex)
        (drover-real-data-log "  Claude Code, the failed: %S"
                              (mapcar (lambda (record)
                                        (let ((detail (or (drover-session-detail record) "")))
                                          (substring detail 0 (min 60 (length detail)))))
                                      (seq-filter (lambda (record) (eq (drover-session-status record) 'failed))
                                                  claude)))
        (dolist (entry (list (cons "Claude Code" claude) (cons "Codex" codex)))
          (let ((records (cdr entry)))
            (drover-real-data-log
             "  %s context: %S; limits %S; providers %S"
             (car entry)
             (drover-real-data-counts
              records (lambda (record)
                        (if-let* ((fraction (drover-session-context-fraction record)))
                            (cond ((< fraction 0.1) "<10%") ((< fraction 0.25) "10-25%")
                                  ((< fraction 0.5) "25-50%") ((< fraction 0.75) "50-75%")
                                  (t ">=75%"))
                          "unknown")))
             (drover-real-data-counts records #'drover-session-context-limit)
             (drover-real-data-counts records #'drover-session-provider-name))
            (drover-real-data-log
             "  %s caches: %d told (TTL %S), %d live now%s"
             (car entry)
             (seq-count #'drover-session-cache-expires records)
             (drover-real-data-counts (seq-filter #'drover-session-cache-expires records)
                                      #'drover-session-cache-ttl)
             (seq-count (lambda (record) (drover-session-cache-fraction record now)) records)
             (mapconcat (lambda (record)
                          (format ", %s %.0f s left" (drover-session-id record)
                                  (drover-session-cache-remaining record now)))
                        (seq-filter (lambda (record) (drover-session-cache-fraction record now)) records)
                        ""))))))))

(defun drover-real-data-list (recorded)
  "Time the list over the RECORDED conversations and the 2510 sessions of the bench."
  (setq drover-test-sessions (drover-bench-sessions))
  (drover-test-register-simple 'fake (lambda () (mapcar #'copy-sequence drover-test-sessions))
                               :session (lambda (id)
                                          (when-let* ((session (seq-find (lambda (session)
                                                                           (equal (drover-session-id session) id))
                                                                         drover-test-sessions)))
                                            (copy-sequence session))))
  (let ((buffer (drover-list--buffer))
        (top (drover-bench-id 'top 0))
        (second (drover-bench-id 'top 1)))
    (cl-letf (((symbol-function 'drover-list--now) (lambda () drover-bench-now)))
      (drover-start)
      (delete-other-windows)
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (dolist (view '((nil updated t) (status urgency nil)))
          (pcase-let ((`(,group ,sort ,descending) view))
            (setq drover-group-by group
                  drover-sort-by sort
                  drover-sort-descending descending
                  drover-show-subagents 'active)
            (drover-list--revert t)
            (let* ((rows (hash-table-count (drover-list--model-rows drover-list--model)))
                   (first (drover-bench-measure (lambda ()
                                                  (setq drover-list--widths nil
                                                        drover-list--model nil)
                                                  (drover-list--render))
                                                2))
                   (render (drover-bench-measure #'drover-list--render 5))
                   (relist (drover-bench-measure #'drover-index-refresh 3))
                   (step (progn (drover-bench-step top)
                                (drover-bench-measure (lambda () (drover-bench-step top)) 21)))
                   (turns (drover-bench-measure
                           (lambda () (drover-bench-step (if (cl-evenp drover-bench-tick) top second)))
                           21))
                   (same (let ((text (drover-bench-text)))
                           (drover-list--render)
                           (equal-including-properties text (drover-bench-text)))))
              (drover-real-data-log "  the list %s, %d recorded and %d of the bench, %d rows drawn:"
                                    (if group (format "by %s" group) "ungrouped, last activity first")
                                    recorded (length drover-test-sessions) rows)
              (drover-real-data-log "               %s | %s | %s"
                                    (drover-bench-row "first, measuring" first 1000)
                                    (drover-bench-row "drawn whole" render 150)
                                    (drover-bench-row "listing anew" relist 150))
              (drover-real-data-log "               %s | %s; the same as drawn anew: %s"
                                    (drover-bench-row "a step" step 10)
                                    (drover-bench-row "two taking turns" turns 10)
                                    (if same "yes" "NO"))))))
      (kill-buffer buffer))))

(let ((drover-bench-output nil))
  (drover-real-data-run))

(provide 'drover-real-data)
;;; drover-real-data.el ends here
