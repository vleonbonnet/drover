;;; drover-search-bench.el --- Time the search of thousands of sessions  -*- lexical-binding: t; -*-

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

;; Times `drover-search' over the 2510 sessions of `drover-bench', their
;; titles varied: making its candidates as it starts, then each key
;; typed, as a completion UI asks (the completion style, then the
;; annotations of a page of candidates), with and without lazy
;; highlighting; then ripgrep searching 120 transcripts of fixtures,
;; some of megabytes, the real one: the longest any function of the
;; search runs on the main thread, garbage collections apart, the
;; longest a timer ticking every millisecond waits, and how long until
;; every match came.  The target
;; of a key typed and of a function of the search is 5 ms, flagged "!"
;; past it.
;;
;;   make bench-search
;;
;; `make bench' runs it too.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-bench)
(require 'drover-search)

(defconst drover-search-bench-words
  ["registry" "paging" "sidebar" "redraw" "transcript" "ripgrep" "consult" "minibuffer" "export"
   "login" "redirect" "archive" "subagent" "prompt" "cache" "model" "effort" "peer" "spyglass"
   "index" "schedule" "fork" "compact" "worktree" "branch" "filter" "search" "snippet" "agent"]
  "Words the titles of the bench's sessions are made of.")

(defun drover-search-bench-title (index)
  "Return a title for session INDEX, of words of `drover-search-bench-words'."
  (let ((words drover-search-bench-words))
    (format "%s the %s of the %s (%d)"
            (capitalize (aref words (% index (length words))))
            (aref words (% (* index 7) (length words)))
            (aref words (% (* index 13) (length words)))
            index)))

(defun drover-search-bench-key (state query)
  "Return what typing the last key of QUERY costs in STATE, as a completion UI asks.
The candidates through the completion style, then the annotations of
the first 20."
  (let ((completion-styles '(drover-search))
        (completion-category-overrides '((drover-session (styles drover-search))))
        (drover-search--current state))
    (let* ((all (completion-all-completions query #'drover-search--table nil (length query)))
           (names (and (consp all) (seq-take all 20))))
      (when (consp all)
        (setcdr (last all) nil))
      (drover-search--affixation (mapcar (lambda (name) (if (stringp name) name "")) names)))))

(defun drover-search-bench-keys ()
  "Time the search's candidates as it starts and as keys are typed."
  (let ((index 0))
    (drover-test-with-backend (mapcar (lambda (session)
                                        (setf (drover-session-title session) (drover-search-bench-title index))
                                        (when (zerop (% index 5))
                                          (setf (drover-session-detail session) "Finished: wrote the summary"))
                                        (cl-incf index)
                                        session)
                                      (drover-bench-sessions))
      (let ((drover-group-by nil)
            (drover-sort-by 'updated)
            (drover-sort-descending t)
            (drover-search-delay 3600))
        (drover-start)
        (drover-changed-now)
        (let ((state nil))
          (drover-bench-log "search: %d sessions" (length drover-test-sessions))
          (drover-bench-log "%s"
                            (drover-bench-row "  candidates made, as it starts      "
                                              (drover-bench-measure (lambda () (setq state (drover-search--build))) 5)
                                              50))
          (dolist (lazy '(t nil))
            (let ((completion-lazy-hilit lazy)
                  (completion-lazy-hilit-fn nil))
              (dolist (query '("d" "dr" "dro" "drov" "registry" "registry pag" "project-03" "feature-branch-5"
                               "summary" "zzzz"))
                (setf (drover-search--state-query state) nil)
                (let ((count (length (drover-search-entries state query))))
                  (drover-bench-log "%s"
                                    (drover-bench-row (format "  key %-18s %-5s %4d matches"
                                                              (format "%S" query) (if lazy "lazy" "eager") count)
                                                      (drover-bench-measure
                                                       (lambda ()
                                                         (setf (drover-search--state-query state) nil)
                                                         (drover-search-bench-key state query))
                                                       20)
                                                      5))))))
          (drover-search--finish state))))))

(defun drover-search-bench-text ()
  "Time ripgrep searching 120 transcripts of fixtures, and what it costs Emacs."
  (if (not (executable-find "rg"))
      (drover-bench-log "search: no rg, text not timed")
    (drover-test-with-homes
      (drover-test-with-backend nil
        (let ((drover-claude-code-transcripts--users nil)
              (drover-claude-code-transcripts--index nil)
              (drover-claude-code-transcripts--agents (make-hash-table :test #'equal))
              (drover-claude-code-agents-interval nil)
              (drover-claude-code-history-days nil)
              (drover-group-by nil)
              (drover-search--programmatic nil)
              (directory (file-name-as-directory (expand-file-name "work" home)))
              (start (- (float-time) 7200)))
          (setq drover--backends nil)
          (drover-claude-code-transcripts-install 'bench)
          (unwind-protect
              (progn
                (dotimes (n 120)
                  (drover-fixture-claude (format "44444444-4444-4444-8444-%012d" n) directory
                                         :start (+ start n)
                                         :prompt (format "Task %d about the capybara" n)
                                         :reply (format "Reply %d: %s" n (if (zerop (% n 10))
                                                                             (make-string 300000 ?r)
                                                                           "the capybara swims"))
                                         :filler (if (zerop (% n 7)) 400000 0)))
                (drover-transcript-scan drover-claude-code-transcripts--index)
                (drover-changed-now)
                (dolist (query '("capybara" "swims" "zzzz"))
                  (let* ((longest 0.0)
                         (functions '(drover-search--start drover-search--receive
                                                           drover-transcript--search-filter
                                                           drover-transcript--search-sentinel))
                         ;; Garbage collections apart: they are the whole Emacs's.
                         (collected 0.0)
                         (timing (lambda (original &rest arguments)
                                   (let ((begin (float-time))
                                         (collecting gc-elapsed))
                                     (prog1 (apply original arguments)
                                       (let ((collection (- gc-elapsed collecting)))
                                         (setq collected (max collected collection)
                                               longest (max longest (- (float-time) begin collection))))))))
                         (result nil)
                         (last (float-time))
                         (gap 0.0)
                         (timer (run-at-time 0 0.001 (lambda ()
                                                       (let ((now (float-time)))
                                                         (setq gap (max gap (- now last))
                                                               last now)))))
                         (begin (float-time)))
                    (dolist (function functions)
                      (advice-add function :around timing))
                    (unwind-protect
                        (progn
                          (drover-search-text query (lambda (found) (setq result found)))
                          (while (and (not result) (< (- (float-time) begin) 10))
                            (accept-process-output nil 0.001)))
                      (cancel-timer timer)
                      (dolist (function functions)
                        (advice-remove function timing)))
                    (drover-bench-log "  text %-10s %3d sessions in %6.1f ms; longest function %5.2f ms%s (a collection in it %4.1f ms), longest timer gap %5.1f ms"
                                      (format "%S" query) (length (plist-get result :sessions))
                                      (* 1000 (- (float-time) begin)) (* 1000 longest)
                                      (if (> longest 0.005) "!" " ") (* 1000 collected) (* 1000 gap)))))
            (drover-claude-code-transcripts-uninstall 'bench)))))))

(defun drover-search-bench-run ()
  "Time the search, and print the results."
  (drover-search-bench-keys)
  (drover-search-bench-text))

(provide 'drover-search-bench)
;;; drover-search-bench.el ends here
