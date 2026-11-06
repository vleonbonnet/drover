;;; drover-summary-test.el --- Tests of the summaries a model writes  -*- lexical-binding: t; -*-

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

;; A fake model, `drover-summary-test--generate', writes summaries for
;; the fake backend's sessions: at once, later on command, or failing.
;; The other host's sessions are in drover-remote-test.el, which starts
;; a second batch Emacs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-summary)
(require 'drover-list)
(require 'drover-brief)
(require 'drover-opencode)
(require 'opencode-http)

(defvar drover-summary-test-prompts nil
  "The prompts the fake model was given, newest first.")

(defconst drover-summary--test-default-answer
  '(:text "Done: wrote the feature, tests pass\nAdded the parser.\nTests pass.")
  "What the fake model answers unless a test says otherwise.")

(defvar drover-summary-test-answer drover-summary--test-default-answer
  "What the fake model answers: a result, a function of the prompt returning one, or `later'.")

(defvar drover-summary-test-pending nil
  "Callbacks the fake model holds, newest first, while its answer is `later'.")

(defvar drover-summary-test-cancelled 0
  "How many requests held were cancelled.")

(defvar drover-summary-test-clock 1800000000.0
  "The time of the last activity the tests give sessions.")

(defun drover-summary-test--generate (prompt callback)
  "Answer PROMPT through CALLBACK as `drover-summary-test-answer' says."
  (push prompt drover-summary-test-prompts)
  (pcase drover-summary-test-answer
    ('later (push callback drover-summary-test-pending)
            (lambda () (cl-incf drover-summary-test-cancelled)))
    ((pred functionp) (funcall callback (funcall drover-summary-test-answer prompt)))
    (answer (funcall callback answer))))

(defun drover-summary-test-session (id &rest properties)
  "Return the fake session ID with PROPERTIES, a long reply recapped for it."
  (apply #'drover-test-session id
         (append properties (list :engine 'fake :engine-id id :updated drover-summary-test-clock))))

(defconst drover-summary-test-reply
  (concat "I added the parser and its tests.\n" (make-string 200 ?x) "\nAll 48 tests pass.")
  "A reply long enough to need the model.")

(defmacro drover-summary-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS, summaries on, state fresh."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (drover-test-with-state
       (let ((drover-summary-delay 0)
             (drover-summary-catch-up nil)
             (drover-summary-concurrency 2)
             (drover-summary-daily-limit 200)
             (drover-summary-timeout 60)
             (drover-summary-harnesses t)
             (drover-summary-generate-function #'drover-summary-test--generate)
             (drover-summary-test-prompts nil)
             (drover-summary-test-answer drover-summary-test-answer)
             (drover-summary-test-pending nil)
             (drover-summary-test-clock drover-summary-test-clock)
             (drover-summary--store nil)
             (drover-summary--usage nil)
             (drover-summary--dirty nil)
             (drover-summary--save-timer nil)
             (drover-summary--warned nil)
             (drover-summary--failing nil)
             (drover-summary--failed-at nil)
             (drover-cheap--former-path nil)
             (drover-summary--capped nil)
             (drover-summary--off nil)
             (drover-summary--timer nil)
             (drover-summary--asked nil)
             (drover-summary--seen (make-hash-table :test #'equal))
             (drover-summary--queue (make-hash-table :test #'equal))
             (drover-summary--jobs (make-hash-table :test #'eql))
             (drover-summary--copies (make-hash-table :test #'eq :weakness 'key))
             (drover-brief-function nil)
             (kill-emacs-hook nil))
         (setq drover-test-recaps
               (mapcar (lambda (session)
                         (let ((id (drover-session-id session)))
                           (cons id (list :prompt (concat "Write the parser of " id)
                                          :reply drover-summary-test-reply
                                          :changes '(("parser.el" 40 2))))))
                       drover-test-sessions))
         (unwind-protect
             (progn
               (drover-summary-mode 1)
               (drover-summary-test-settle)
               ,@body)
           (drover-summary-mode -1)
           (when (timerp drover-summary--save-timer)
             (cancel-timer drover-summary--save-timer)))))))

(defun drover-summary-test-settle (&optional seconds)
  "Run changes and timers until summaries settle, SECONDS at most, 5 by default."
  (let ((end (+ (float-time) (or seconds 5))))
    (drover-changed-now)
    (while (and (< (float-time) end)
                (or (timerp drover--change-timer)
                    (and (> (hash-table-count drover-summary--queue) 0)
                         (< (hash-table-count drover-summary--jobs) drover-summary-concurrency))
                    (and (> (hash-table-count drover-summary--jobs) 0)
                         (not (eq drover-summary-test-answer 'later)))))
      (accept-process-output nil 0.01)
      (drover-changed-now))
    (drover-changed-now)))

(defun drover-summary-test-turn (id &rest slots)
  "Change the fake session ID as a turn would: SLOTS set, its activity later.
SLOTS is a plist of slot names as keywords and values."
  (setq drover-test-sessions
        (mapcar (lambda (session)
                  (if (not (equal (drover-session-id session) id))
                      session
                    (let ((copy (copy-sequence session)))
                      (cl-loop for (slot value) on slots by #'cddr
                               do (setf (cl-struct-slot-value 'drover-session
                                                              (intern (substring (symbol-name slot) 1))
                                                              copy)
                                        value))
                      (setf (drover-session-updated copy) (cl-incf drover-summary-test-clock))
                      copy)))
                drover-test-sessions))
  (drover-backend-changed 'fake id))

(defun drover-summary-test-of (id)
  "Return the record of the fake session ID the index holds."
  (drover-index-session (cons 'fake id)))

(defmacro drover-summary-test-messages (&rest body)
  "Run BODY and return the messages it showed, oldest first."
  (declare (indent 0) (debug t))
  `(let ((messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest arguments)
                  (when format
                    (push (apply #'format-message format arguments) messages)))))
       ,@body)
     (nreverse messages)))

;;;; Writing summaries

(ert-deftest drover-summary-test-turn-ended ()
  "A turn ended is summarized once, the line in lists and the recap in details."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working)
                                  (drover-summary-test-session "s2" :status 'idle))
    (should-not drover-summary-test-prompts)
    ;; A session at work is never summarized, however often it changes.
    (dotimes (_ 3)
      (drover-summary-test-turn "s1" :status 'working)
      (drover-summary-test-settle))
    (should-not drover-summary-test-prompts)
    (should-not drover-test-recap-calls)
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 1))
    (should (string-search "Write the parser of s1" (car drover-summary-test-prompts)))
    (should (string-search "parser.el (+40 -2)" (car drover-summary-test-prompts)))
    (let ((session (drover-summary-test-of "s1")))
      (should (equal (drover-session-brief session) "Done: wrote the feature, tests pass"))
      (should (equal (drover-session-brief-recap session) "Added the parser.\nTests pass."))
      (should (equal (drover-session-summary session) "Done: wrote the feature, tests pass"))
      ;; Expanded, it shows the recap above the turn as it was said.
      (let ((details (drover-list--details session (list :fields '(title summary)))))
        (should (equal (car (car details)) "Summary"))
        (should (equal (nth 1 (car details)) "Added the parser.\nTests pass."))
        (should (assoc "Reply" details)))
      ;; Without the summary's column, the line goes there too.
      (should (string-prefix-p "Done: wrote the feature"
                               (nth 1 (car (drover-list--details session (list :fields '(title))))))))
    ;; The other session, unchanged, was not summarized.
    (should-not (drover-session-brief (drover-summary-test-of "s2")))
    ;; Counted, for usage totals.
    (should (equal (mapcar (lambda (entry) (nth 1 entry)) (drover-summary-usage)) '(1)))))

(ert-deftest drover-summary-test-debounced ()
  "Changes coming fast ask for one summary, once the session settled."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'idle))
    (let ((drover-summary-delay 0.3))
      (drover-summary-test-turn "s1" :status 'done)
      (drover-changed-now)
      (accept-process-output nil 0.1)
      (drover-summary-test-turn "s1" :status 'done :title "Renamed")
      (drover-changed-now)
      (accept-process-output nil 0.1)
      (should-not drover-summary-test-prompts)
      (drover-summary-test-settle)
      (should (= (length drover-summary-test-prompts) 1)))))

(ert-deftest drover-summary-test-cached ()
  "A turn summarized is not summarized again, in this Emacs or the next."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'idle))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 1))
    ;; Renamed, then seen: the same turn, its recap read again.
    (drover-summary-test-turn "s1" :status 'done :title "Renamed")
    (drover-summary-test-settle)
    (drover-summary-test-turn "s1" :status 'idle)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 1))
    (should (equal (drover-session-brief (drover-summary-test-of "s1")) "Done: wrote the feature, tests pass"))
    ;; Kept as Emacs exits, read by the next, which neither recaps nor asks.
    (let ((updated (drover-session-updated (drover-summary-test-of "s1"))))
      (drover-summary-mode -1)
      (should-not (drover-session-brief (progn (drover-summary-test-settle) (drover-summary-test-of "s1"))))
      (should (file-exists-p (drover-summary-file)))
      (setq drover-summary--store nil
            drover-test-recap-calls nil)
      (drover-summary-mode 1)
      (drover-summary-test-settle)
      (let ((session (drover-summary-test-of "s1")))
        (should (equal (drover-session-updated session) updated))
        (should (equal (drover-session-brief session) "Done: wrote the feature, tests pass")))
      (should-not drover-test-recap-calls)
      (should (= (length drover-summary-test-prompts) 1)))
    ;; Asked anew, it is written again.
    (drover-summarize (list (drover-summary-test-of "s1")) t)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 2))))

(ert-deftest drover-summary-test-session-story ()
  "The model writes of the whole session too, from its title, first prompt and summary so far."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'idle :title "Search for the app"))
    (setq drover-test-recaps (list (cons "s1" (list :prompt "Index the notes" :reply drover-summary-test-reply
                                                    :first-prompt "Add search to the app"))))
    (setq drover-summary-test-answer
          '(:text "Done: wrote the index, tests pass\nAdded the index.\n---\nAdding search to the app: the index is done, ranking next."))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (let ((prompt (car drover-summary-test-prompts)))
      (should (string-search "<session>\nTitle: Search for the app\n\nFirst prompt:\nAdd search to the app\n</session>"
                             prompt))
      (should (string-search "<turn>\n" prompt))
      (should-not (string-search "Summary so far" prompt)))
    (let ((session (drover-summary-test-of "s1")))
      ;; Lists show the turn's line, its buffer the session's.
      (should (equal (drover-session-brief session) "Done: wrote the index, tests pass"))
      (should (equal (drover-session-brief-session session)
                     "Adding search to the app: the index is done, ranking next."))
      (drover-summary-test-with-buffers '("s1")
        (should (equal (drover-brief-of-buffer #'ignore (car buffers))
                       "Adding search to the app: the index is done, ranking next."))))
    ;; Renamed, the same turn is not summarized again.
    (drover-summary-test-turn "s1" :status 'done :title "Renamed")
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 1))
    ;; The next turn builds on what was written of the session.
    (drover-summary-test-turn "s1" :status 'working)
    (drover-summary-test-settle)
    (setq drover-test-recaps (list (cons "s1" (list :prompt "Rank the results"
                                                    :reply (concat drover-summary-test-reply " Ranked.")
                                                    :first-prompt "Add search to the app")))
          drover-summary-test-answer
          '(:text "Done: ranked the results\n---\nAdding search to the app: the index and ranking are done."))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 2))
    (should (string-search "Summary so far:\nAdding search to the app: the index is done, ranking next."
                           (car drover-summary-test-prompts)))
    (should (equal (drover-session-brief-session (drover-summary-test-of "s1"))
                   "Adding search to the app: the index and ranking are done."))
    ;; A turn needing no model keeps what was written of the session.
    (drover-summary-test-turn "s1" :status 'working)
    (drover-summary-test-settle)
    (setq drover-test-recaps (list (cons "s1" (list :prompt "Thanks" :reply "You are welcome."))))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 2))
    (should (equal (drover-session-brief (drover-summary-test-of "s1")) "You are welcome."))
    (should (equal (drover-session-brief-session (drover-summary-test-of "s1"))
                   "Adding search to the app: the index and ranking are done."))
    ;; Kept in the file, read by the next Emacs.
    (drover-summary-mode -1)
    (setq drover-summary--store nil)
    (drover-summary-mode 1)
    (drover-summary-test-settle)
    (should (equal (drover-session-brief-session (drover-summary-test-of "s1"))
                   "Adding search to the app: the index and ranking are done."))))

(ert-deftest drover-summary-test-first-prompt ()
  "The first prompt is its backend's, else what the engines recording the conversation tell first."
  (drover-summary-test-with nil
    (let ((session (drover-summary-test-session "s1")))
      (should (equal (drover-summary--first-prompt session '(:first-prompt "Told by the backend")) "Told by the backend"))
      (cl-letf (((symbol-function 'drover--recording-backends) (lambda (_) '(recorder)))
                ((symbol-function 'drover-backend-property)
                 (lambda (backend property)
                   (and (eq backend 'recorder) (eq property :search-text)
                        (lambda (_) (list "The first prompt" "The last prompt"))))))
        (should (equal (drover-summary--first-prompt session nil) "The first prompt")))
      (should-not (drover-summary--first-prompt session nil)))))

(ert-deftest drover-summary-test-works-again ()
  "A summary goes as its session works, and the next turn gets its own."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'idle))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (drover-session-brief (drover-summary-test-of "s1")))
    (drover-summary-test-turn "s1" :status 'working)
    (drover-summary-test-settle)
    (should-not (drover-session-brief (drover-summary-test-of "s1")))
    ;; Its turn ends with another reply: the summary of the turn before
    ;; does not show meanwhile.
    (setq drover-summary-test-answer 'later)
    (setf (alist-get "s1" drover-test-recaps nil nil #'equal)
          (list :prompt "Now fix the bug" :reply (concat "Fixed it.\n" drover-summary-test-reply)))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-pending) 1))
    (should-not (drover-session-brief (drover-summary-test-of "s1")))
    (funcall (pop drover-summary-test-pending) '(:text "Done: fixed the bug"))
    (drover-summary-test-settle)
    (should (equal (drover-session-brief (drover-summary-test-of "s1")) "Done: fixed the bug"))
    (should-not (drover-session-brief-recap (drover-summary-test-of "s1")))))

(ert-deftest drover-summary-test-waiting ()
  "A session waiting is summarized for what it asks, anew as that changes."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working))
    (setq drover-summary-test-answer
          (lambda (prompt)
            (list :text (if (string-search "git push" prompt)
                            "Waiting for approval to push the fix"
                          "Waiting for approval to run the tests"))))
    (drover-summary-test-turn "s1" :status 'waiting
                              :requests (list (list :kind 'permission :summary "git push origin main")))
    (drover-summary-test-settle)
    (should (string-search "permission: git push origin main" (car drover-summary-test-prompts)))
    (should (equal (drover-session-summary (drover-summary-test-of "s1"))
                   "Waiting for approval to push the fix"))
    ;; Another request: the summary goes until written anew.
    (setq drover-summary-test-answer
          (let ((answer drover-summary-test-answer))
            (lambda (prompt) (funcall answer prompt))))
    (let ((drover-summary-delay 10))
      (drover-summary-test-turn "s1" :status 'waiting
                                :requests (list (list :kind 'permission :summary "make test")))
      (drover-changed-now)
      (should (equal (drover-session-summary (drover-summary-test-of "s1")) "permit: make test")))
    (drover-summarize (list (drover-summary-test-of "s1")))
    (drover-summary-test-settle)
    (should (equal (drover-session-summary (drover-summary-test-of "s1"))
                   "Waiting for approval to run the tests"))))

(ert-deftest drover-summary-test-trivial ()
  "A turn of one short reply needs no model, a turn of nothing no summary."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working)
                                  (drover-summary-test-session "s2" :status 'working))
    (setq drover-test-recaps (list (cons "s1" (list :prompt "Which branch?" :reply "We are on main."))
                                   (cons "s2" (list :prompt nil :reply nil))))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-turn "s2" :status 'done)
    (drover-summary-test-settle)
    (should-not drover-summary-test-prompts)
    (should (equal (drover-session-brief (drover-summary-test-of "s1")) "We are on main."))
    (should-not (drover-session-brief (drover-summary-test-of "s2")))))

(ert-deftest drover-summary-test-concurrency ()
  "Two summaries are asked for at once, at most; the others wait their turn."
  (drover-summary-test-with (mapcar (lambda (i) (drover-summary-test-session (format "s%d" i) :status 'working))
                                    (number-sequence 1 5))
    (setq drover-summary-test-answer 'later)
    (dotimes (i 5)
      (drover-summary-test-turn (format "s%d" (1+ i)) :status 'done))
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-pending) 2))
    (should (= (hash-table-count drover-summary--jobs) 2))
    (let ((done 0))
      (while (and drover-summary-test-pending (< done 10))
        (funcall (pop drover-summary-test-pending) '(:text "Done: one more"))
        (cl-incf done)
        (drover-summary-test-settle)
        (should (<= (hash-table-count drover-summary--jobs) 2)))
      (should (= done 5)))
    (should (seq-every-p (lambda (i) (drover-session-brief (drover-summary-test-of (format "s%d" i))))
                         (number-sequence 1 5)))))

(ert-deftest drover-summary-test-daily-limit ()
  "Past the daily limit, sessions show as before, said once that day."
  (drover-summary-test-with (mapcar (lambda (i) (drover-summary-test-session (format "s%d" i) :status 'working))
                                    (number-sequence 1 3))
    (let* ((drover-summary-daily-limit 1)
           (messages (drover-summary-test-messages
                       (dolist (id '("s1" "s2" "s3"))
                         (drover-summary-test-turn id :status 'failed :detail (concat "boom " id)))
                       (drover-summary-test-settle))))
      (should (= (length drover-summary-test-prompts) 1))
      (should (= (seq-count (lambda (message) (string-search "drover-summary-daily-limit" message)) messages)
                 1))
      (should (= (seq-count (lambda (id) (drover-session-brief (drover-summary-test-of id))) '("s1" "s2" "s3"))
                 1))
      (should (seq-some (lambda (id) (equal (drover-session-summary (drover-summary-test-of id))
                                            (concat "boom " id)))
                        '("s1" "s2" "s3"))))))

(ert-deftest drover-summary-test-failure-falls-back ()
  "A model failing leaves sessions as they showed, said once until it works again."
  (drover-summary-test-with (mapcar (lambda (i) (drover-summary-test-session (format "s%d" i) :status 'working))
                                    (number-sequence 1 3))
    (setq drover-summary-test-answer '(:failure "HTTP 503"))
    (let ((messages (drover-summary-test-messages
                      (drover-summary-test-turn "s1" :status 'waiting
                                                :requests (list (list :kind 'question :summary "Which one?")))
                      (drover-summary-test-turn "s2" :status 'done)
                      (drover-summary-test-settle))))
      ;; The first failed: the second is not asked for.
      (should (= (length drover-summary-test-prompts) 1))
      (should (equal (seq-filter (lambda (message) (string-search "could not summarize" message)) messages)
                     '("Drover could not summarize a session, shown as before: HTTP 503")))
      (should (equal (drover-session-summary (drover-summary-test-of "s1")) "asks: Which one?"))
      (should-not (drover-session-brief (drover-summary-test-of "s2"))))
    ;; Meanwhile, nothing is asked of a model that failed.
    (drover-summary-test-turn "s2" :status 'working)
    (drover-summary-test-turn "s2" :status 'done)
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 1))
    ;; Asked for, it is.
    (drover-summarize (list (drover-summary-test-of "s2")))
    (drover-summary-test-settle)
    (should (= (length drover-summary-test-prompts) 2))
    ;; A model never answering is given up, as a failure, once each model
    ;; it might ask was given its time, and its request cancelled.
    (let ((drover-summary-timeout 0.1)
          (drover-cheap-attempts 2)
          (drover-summary-test-cancelled 0))
      (setq drover-summary-test-answer 'later)
      (let ((messages (drover-summary-test-messages
                        (setq drover-summary--failing nil
                              drover-summary--failed-at nil)
                        (drover-summary-test-turn "s3" :status 'done)
                        (drover-summary-test-settle)
                        (should (= (hash-table-count drover-summary--jobs) 1))
                        (accept-process-output nil 0.4))))
        (should (seq-some (lambda (message) (string-search "no summary after 0.2 s" message)) messages))
        (should (= drover-summary-test-cancelled 1))
        (should (= (hash-table-count drover-summary--jobs) 0))))))

(ert-deftest drover-summary-test-no-model ()
  "Without a model, sessions show as they did, said once, until one resolves."
  (drover-summary-test-with (mapcar (lambda (i) (drover-summary-test-session (format "s%d" i) :status 'working))
                                    (number-sequence 1 3))
    (setq drover-summary-test-answer
          '(:failure "no cheap model, no provider of subscription offers one" :none t))
    (let* ((cheap nil)
           (messages
            (cl-letf (((symbol-function 'drover-model-cheap) (lambda (&rest _) cheap)))
              (drover-summary-test-messages
                (drover-summary-test-turn "s1" :status 'done)
                (drover-summary-test-settle)
                ;; Nothing more is asked of the model, nor said, but a turn
                ;; needing no model is still summarized.
                (setf (alist-get "s3" drover-test-recaps nil nil #'equal) (list :reply "Done: nothing to do"))
                (drover-summary-test-turn "s2" :status 'done)
                (drover-summary-test-turn "s3" :status 'done)
                (drover-summary-test-settle)
                (should (= (length drover-summary-test-prompts) 1))
                (should (equal (drover-session-brief (drover-summary-test-of "s3")) "Done: nothing to do"))
                (should-not (drover-session-brief (drover-summary-test-of "s2")))
                (should-error (funcall-interactively #'drover-summarize (list (drover-summary-test-of "s2")))
                              :type 'user-error)
                ;; Not resting as after a failure: once a model resolves, the next turn has its summary.
                (should-not drover-summary--failed-at)
                (setq cheap '(:model "openai/gpt-6-luna")
                      drover-summary-test-answer drover-summary--test-default-answer)
                (drover-summary-test-turn "s2" :status 'working)
                (drover-summary-test-turn "s2" :status 'done)
                (drover-summary-test-settle)))))
      (should (equal (seq-filter (lambda (message) (string-search "needing a model" message)) messages)
                     '("Drover summarizes no session needing a model: no cheap model, no provider of subscription offers one")))
      (should (= (length drover-summary-test-prompts) 2))
      (should-not drover-summary--off)
      (should (equal (drover-session-brief (drover-summary-test-of "s2")) "Done: wrote the feature, tests pass")))))

(ert-deftest drover-summary-test-redacted ()
  "Obvious secrets never reach the model, and what it reads stays small."
  (let ((text (concat "export GITHUB_TOKEN=ghp_" (make-string 36 ?a) "\n"
                      "aws AKIAABCDEFGHIJKLMNOP and sk-ant-api03-" (make-string 30 ?b) "\n"
                      "curl -H 'Authorization: Bearer Zyxw9876543210vutsrq' https://user:hunter22@example.com/x\n"
                      "password = \"correct horse\" and api_key: s3cr3tValue\n"
                      "slack xoxb-1234567890-abcdefghij and a key AbCdEfGh1234567890IjKlMnOpQrStUvWx\n"
                      "-----BEGIN OPENSSH PRIVATE KEY-----\nb3BlbnNzaC1rZXktdjEAAAAA\n-----END OPENSSH PRIVATE KEY-----\n"
                      "commit 0123456789abcdef0123456789abcdef01234567 stays")))
    (let ((redacted (drover-summary-redact text)))
      (dolist (secret (list (make-string 36 ?a) "AKIAABCDEFGHIJKLMNOP" (make-string 30 ?b)
                            "Zyxw9876543210vutsrq" "hunter22" "correct horse" "s3cr3tValue"
                            "xoxb-1234567890" "AbCdEfGh1234567890IjKlMnOpQrStUvWx" "b3BlbnNzaC1rZXktdjEAAAAA"))
        (ert-info (secret)
          (should-not (string-search secret redacted))))
      (should (string-search "0123456789abcdef0123456789abcdef01234567 stays" redacted))
      (should (string-search "https://user:[redacted]@example.com" redacted))
      (should (string-search "Authorization: Bearer [redacted]" redacted)))
    (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working))
      (setq drover-test-recaps (list (cons "s1" (list :prompt (concat text (make-string 500000 ?y))
                                                      :reply (concat text (make-string 500000 ?z))))))
      (drover-summary-test-turn "s1" :status 'done)
      (drover-summary-test-settle)
      (let ((prompt (car drover-summary-test-prompts)))
        (should prompt)
        (should-not (string-search "AKIAABCDEFGHIJKLMNOP" prompt))
        ;; The turn, and the session: its title, first prompt and summary so far.
        (should (<= (length prompt) (+ drover-summary-input-limit (/ drover-summary-input-limit 2) 200
                                       (length drover-summary-instructions) 120)))))))

(ert-deftest drover-summary-test-bounded ()
  "What the model writes is bounded, its marks of lists and emphasis dropped."
  (should (equal (drover-summary-parse "\n- **Done: added FTS search**\n\n* first\n2. second\n")
                 '("Done: added FTS search" "first\nsecond" nil)))
  (should-not (drover-summary-parse "  \n \n"))
  (let ((parsed (drover-summary-parse (concat (make-string 500000 ?a) "\n" (make-string 500000 ?b)
                                              "\n---\n" (make-string 500000 ?c)))))
    (should (<= (length (nth 0 parsed)) 101))
    (should (<= (length (nth 1 parsed)) 201))
    (should (<= (length (nth 2 parsed)) (1+ drover-brief-session-limit)))))

(ert-deftest drover-summary-test-parse-session ()
  "After a line of dashes, the model writes of the whole session: one paragraph."
  (should (equal (drover-summary-parse
                  "Done: added search\nThe index is built.\n\n---\nSummary: Adding search to the app.\nThe index is done;\n\nnext, ranking.\n")
                 '("Done: added search" "The index is built."
                   "Adding search to the app. The index is done; next, ranking.")))
  ;; Without a recap, and dashes the model spaced.
  (should (equal (drover-summary-parse "Done: added search\n  ----  \nAdding search.")
                 '("Done: added search" nil "Adding search.")))
  ;; Nothing after them is no summary of the session.
  (should (equal (drover-summary-parse "Done: added search\n---\n  \n")
                 '("Done: added search" nil nil))))

(ert-deftest drover-summary-test-opted-out ()
  "Sessions of a harness left out are neither sent nor briefed."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (should (drover-session-brief (drover-summary-test-of "s1")))
    (let ((drover-summary-harnesses '(other)))
      (drover-backend-changed 'fake "s1")
      (drover-summary-test-settle)
      (should-not (drover-session-brief (drover-summary-test-of "s1")))
      (drover-summary-test-turn "s1" :status 'working)
      (drover-summary-test-turn "s1" :status 'done)
      (drover-summary-test-settle)
      (should (= (length drover-summary-test-prompts) 1))
      (should-not (drover-session-brief (drover-summary-test-of "s1"))))
    (let ((drover-summary-harnesses nil))
      (should (zerop (drover-summarize (list (drover-summary-test-of "s1"))))))))

(ert-deftest drover-summary-test-catch-up ()
  "Sessions found settled are summarized only when recently active."
  (let ((now (float-time)))
    (drover-summary-test-with (list (drover-summary-test-session "recent" :status 'done :updated (- now 60))
                                    (drover-summary-test-session "old" :status 'done :updated (- now 86400)))
      (should-not drover-summary-test-prompts)
      (let ((drover-summary-catch-up 900))
        (drover-summary-mode -1)
        (drover-summary-mode 1)
        (drover-summary-test-settle))
      (should (= (length drover-summary-test-prompts) 1))
      (should (drover-session-brief (drover-summary-test-of "recent")))
      (should-not (drover-session-brief (drover-summary-test-of "old"))))))

(ert-deftest drover-summary-test-records-compare-alike ()
  "Records listed again with the same brief are the same record, compared once."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (let* ((listed (seq-find (lambda (session) (equal (drover-session-id session) "s1")) drover-test-sessions))
           (first (drover--bound-record listed)))
      (should (drover-session-brief first))
      (should (eq first (drover--bound-record listed)))
      (should-not (drover-session-brief listed)))
    ;; Listed anew, nothing changed.
    (let ((ran nil))
      (let ((drover-changed-hook (list (lambda () (push drover-changed-sessions ran)))))
        (drover-index-refresh t))
      (should-not ran))))

(ert-deftest drover-summary-test-opencode-request ()
  "OpenCode's service is asked a stateless generation, never started for it."
  (let ((requests nil)
        (results nil)
        (drover-summary-model "github-copilot/claude-haiku-5.5"))
    (cl-letf (((symbol-function 'opencode-service-read-registration)
               (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
              ((symbol-function 'opencode-request)
               (lambda (method path &rest arguments)
                 (push (append (list method path) arguments) requests)
                 (funcall (plist-get arguments :then) '((data (text . "Done: it works")))))))
      (let ((drover-summary-variant "low"))
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))))
    (should (equal results '((:text "Done: it works" :model "github-copilot/claude-haiku-5.5"))))
    (let ((request (car requests)))
      (should (equal (seq-take request 2) '(post "/api/experimental/generate/text")))
      (should (equal (plist-get (cddr request) :body)
                     '((prompt . "the prompt")
                       (model . ((providerID . "github-copilot") (id . "claude-haiku-5.5") (variant . "low"))))))
      (should (plist-member (cddr request) :directory))
      (should-not (plist-get (cddr request) :directory)))
    ;; A service knowing generation by its former path alone is asked there,
    ;; from then on; one not knowing the model says so.
    (setq requests nil results nil)
    (cl-letf (((symbol-function 'opencode-service-read-registration)
               (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
              ((symbol-function 'opencode-request)
               (lambda (method path &rest arguments)
                 (push path requests)
                 (if (equal path "/api/experimental/generate/text")
                     (funcall (plist-get arguments :else)
                              (opencode-error--make :status 404 :method method :path path :message "Not Found"))
                   (funcall (plist-get arguments :then) '((data (text . "Done: it works"))))))))
      (let ((drover-cheap--former-path nil))
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))))
    (should (equal (reverse requests) '("/api/experimental/generate/text" "/api/experimental/generate"
                                        "/api/experimental/generate")))
    (should (equal results '((:text "Done: it works" :model "github-copilot/claude-haiku-5.5")
                             (:text "Done: it works" :model "github-copilot/claude-haiku-5.5"))))
    (setq requests nil results nil)
    (cl-letf (((symbol-function 'opencode-service-read-registration)
               (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
              ((symbol-function 'opencode-request)
               (lambda (method path &rest arguments)
                 (push path requests)
                 (funcall (plist-get arguments :else)
                          (opencode-error--make :status 404 :tag "ModelNotFoundError" :method method :path path
                                                :message "Model not found: github-copilot/nope")))))
      (let ((drover-cheap--former-path nil))
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))))
    (should (equal requests '("/api/experimental/generate/text")))
    (should (string-search "Model not found" (plist-get (car results) :failure)))
    ;; Its errors are told; a service not running is not started.
    (setq requests nil results nil)
    (cl-letf (((symbol-function 'opencode-service-read-registration) (lambda () nil))
              ((symbol-function 'opencode-request)
               (lambda (&rest arguments) (push arguments requests))))
      (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results))))
    (should-not requests)
    (should (equal results '((:failure "OpenCode's service does not run"))))))

(ert-deftest drover-summary-test-opencode-cheap-model ()
  "Without a model named, this host's cheap model writes, at the variant when it offers it."
  (let ((requests nil)
        (results nil)
        (cheap nil)
        (drover-summary-model nil)
        (drover-summary-variant "low"))
    (cl-letf (((symbol-function 'opencode-service-read-registration)
               (lambda () (list :url "http://127.0.0.1:1" :password "x" :pid (emacs-pid))))
              ((symbol-function 'drover-model-cheap-fetch)
               (lambda (callback) (funcall callback cheap)))
              ((symbol-function 'opencode-request)
               (lambda (method path &rest arguments)
                 (push (list method path (plist-get arguments :body)) requests)
                 (funcall (plist-get arguments :then) '((data (text . "Done: it works")))))))
      (dolist (case '(((:model "openai/gpt-6-luna" :provider-id "openai" :id "gpt-6-luna"
                                :variants ("none" "low" "high"))
                       ((providerID . "openai") (id . "gpt-6-luna") (variant . "low")))
                      ((:model "github-copilot/claude-haiku-4.5" :provider-id "github-copilot"
                               :id "claude-haiku-4.5" :variants ("high" "max"))
                       ((providerID . "github-copilot") (id . "claude-haiku-4.5")))))
        (setq cheap (car case) requests nil results nil)
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))
        (should (equal results `((:text "Done: it works" :model ,(plist-get (car case) :model)))))
        (should (equal (car requests)
                       `(post "/api/experimental/generate/text"
                              ((prompt . "the prompt") (model . ,(cadr case)))))))
      ;; None resolving asks nothing, and says there is none; a failure tells it.
      (dolist (case '(((:none "no provider offers one")
                       (:failure "no cheap model, no provider offers one" :none t))
                      ((:failure "Boom") (:failure "Boom"))))
        (setq cheap (car case) requests nil results nil)
        (drover-summary-opencode-generate "the prompt" (lambda (result) (push result results)))
        (should-not requests)
        (should (equal results (cdr case)))))))

;;;; Briefs in the buffers of sessions

(defvar drover-summary-test-refreshed nil
  "Buffers whose brief was shown anew, newest first.")

(defun drover-summary-test-refresh ()
  "Note that the current buffer showed its brief anew."
  (push (current-buffer) drover-summary-test-refreshed))

(defmacro drover-summary-test-with-buffers (ids &rest body)
  "Run BODY with a buffer for each fake session of IDS, bound to BUFFERS.
Each tells its session as the one at point, as a front end's buffer does."
  (declare (indent 1) (debug t))
  `(let ((drover-brief--buffers (make-hash-table :test #'eq :weakness 'key))
         (drover-summary-test-refreshed nil)
         (buffers (mapcar (lambda (id)
                            (with-current-buffer (generate-new-buffer (concat "session " id))
                              (setq-local drover-session-at-point-function
                                          (lambda () (drover-summary-test-of id)))
                              (current-buffer)))
                          ,ids)))
     (add-hook 'drover-changed-hook #'drover-brief--changed)
     (unwind-protect (progn ,@body)
       (mapc #'kill-buffer buffers))))

(ert-deftest drover-summary-test-brief-of-buffer ()
  "A buffer gets its session's brief, shown anew as that session changes, and only then."
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working)
                                  (drover-summary-test-session "s2" :status 'working))
    (drover-summary-test-with-buffers '("s1" "s2")
      (let ((one (car buffers))
            (two (cadr buffers)))
        ;; At work: no brief, but followed.
        (should-not (drover-brief-of-buffer #'drover-summary-test-refresh one))
        (should-not (drover-brief-of-buffer #'drover-summary-test-refresh two))
        (drover-summary-test-turn "s1" :status 'done)
        (drover-summary-test-settle)
        ;; The turn ended, then its brief was written: shown anew, the other not.
        (should (memq one drover-summary-test-refreshed))
        (should-not (memq two drover-summary-test-refreshed))
        (should (equal (drover-brief-of-buffer #'drover-summary-test-refresh one)
                       '("Done: wrote the feature, tests pass" . "Added the parser.\nTests pass.")))
        ;; Working again, the brief goes, which the buffer is told.
        (setq drover-summary-test-refreshed nil)
        (drover-summary-test-turn "s1" :status 'working)
        (drover-summary-test-settle)
        (should (equal drover-summary-test-refreshed (list one)))
        (should-not (drover-brief-of-buffer #'drover-summary-test-refresh one))
        ;; Turned off, buffers show none.
        (drover-summary-test-turn "s1" :status 'done)
        (drover-summary-test-settle)
        (should (drover-brief-of-buffer #'drover-summary-test-refresh one))
        (let ((drover-brief-in-buffers nil))
          (should-not (drover-brief-of-buffer #'drover-summary-test-refresh one)))
        ;; A buffer killed is forgotten.
        (kill-buffer two)
        (drover-brief-refresh t)
        (should-not (gethash two drover-brief--buffers))
        (should (gethash one drover-brief--buffers))))))

(ert-deftest drover-summary-test-brief-of-other-host ()
  "A session of another host shows the brief its owner wrote, from its record."
  (drover-summary-test-with nil
    (let* ((record (drover-session-create :backend 'remote:core :id '(opencode . "ses_1") :host "core"
                                          :status 'idle :brief "Done: on Core" :brief-recap "Its recap"))
           (drover-brief--buffers (make-hash-table :test #'eq :weakness 'key)))
      (with-temp-buffer
        (setq-local drover-session-at-point-function (lambda () record))
        (should (equal (drover-brief-of-buffer #'ignore) '("Done: on Core" . "Its recap")))
        (should (equal (car (gethash (current-buffer) drover-brief--buffers))
                       '(remote:core opencode . "ses_1")))))))

(ert-deftest drover-summary-test-brief-opencode-function ()
  "Drover tells OpenCode's buffers their briefs, unless the user chose a function of their own."
  (let ((opencode-session-summary-function nil))
    (cl-letf (((symbol-function 'drover-opencode--buffers) (lambda (_) nil)))
      (drover-opencode--install)
      (should (eq opencode-session-summary-function #'drover-opencode-brief))
      (drover-opencode--uninstall)
      (should-not opencode-session-summary-function)
      (let ((opencode-session-summary-function #'ignore))
        (drover-opencode--install)
        (should (eq opencode-session-summary-function #'ignore))
        (drover-opencode--uninstall)
        (should (eq opencode-session-summary-function #'ignore)))
      (drover-opencode--install)))
  (drover-summary-test-with (list (drover-summary-test-session "s1" :status 'working))
    (drover-summary-test-turn "s1" :status 'done)
    (drover-summary-test-settle)
    (drover-summary-test-with-buffers '("s1")
      (with-current-buffer (car buffers)
        (should (equal (car (drover-opencode-brief)) "Done: wrote the feature, tests pass"))
        (should (eq (cdr (gethash (current-buffer) drover-brief--buffers))
                    #'opencode-session-summary-update))))))

(ert-deftest drover-summary-test-prints-in-proportion ()
  "What summaries keep prints in proportion to the sessions."
  (let ((sessions (mapcar (lambda (i) (drover-summary-test-session (format "s%d" i) :status 'working))
                          (number-sequence 1 50))))
    (drover-summary-test-with sessions
      (setq drover-summary-test-answer
            (lambda (_) (list :text (concat "Done: " (make-string 300 ?l) "\n" (make-string 3000 ?r)))))
      (dolist (session sessions)
        (drover-summary-test-turn (drover-session-id session) :status 'done))
      (drover-summary-test-settle 20)
      (should (= (length drover-summary-test-prompts) 50))
      ;; Some asked for, not answered yet.
      (setq drover-summary-test-answer 'later)
      (drover-summarize (list (drover-summary-test-of "s1") (drover-summary-test-of "s2")) t)
      (drover-summary-test-settle)
      (drover-test-prints-in-proportion
       50 (list (cons "a record briefed" (drover-summary-test-of "s3"))
                (cons "a summary kept" (drover-summary--entry (drover-summary-test-of "s3"))))))))

(provide 'drover-summary-test)
;;; drover-summary-test.el ends here
