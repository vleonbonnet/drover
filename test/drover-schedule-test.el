;;; drover-schedule-test.el --- Tests of prompts scheduled for later  -*- lexical-binding: t; -*-

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

;; The queue lives in a throwaway state directory; nothing is shown:
;; the session of a prompt is delivered in no window, and the list is
;; drawn only by the tests of it.  Prompts go to the fake backend, whose
;; buffers `drover-schedule-test-buffer' makes, with a process, and
;; whose :blocker and :submit read and record `drover-schedule-test-*'.
;; The time grammar is checked in a fixed US Eastern zone, across both
;; changes of daylight saving time.

;;; Code:

(require 'drover-test-helpers)
(require 'opencode)
(require 'drover-opencode)
(require 'drover-schedule)

(defvar drover-schedule-test-blocker nil
  "What the fake backend's :blocker says, nil when its buffers take a prompt.")

(defvar drover-schedule-test-submitted nil
  "Prompts the fake backend's :submit took, newest first, as (ID TEXT THEN ELSE).")

(defvar drover-schedule-test-buffers nil
  "Buffers of fake sessions, by session identifier, as an alist (ID . BUFFER).")

(defun drover-schedule-test--submit (session text then else)
  "Record TEXT submitted to SESSION, with THEN and ELSE, as :submit does."
  (push (list (drover-session-id session) text then else) drover-schedule-test-submitted))

(defun drover-schedule-test--register ()
  "Register the fake backend with buffers, :blocker and :submit."
  (drover-test-register)
  (setf (alist-get 'fake drover--backends)
        (append (list :buffer (lambda (session)
                                (let ((buffer (alist-get (drover-session-id session) drover-schedule-test-buffers
                                                         nil nil #'equal)))
                                  (and (buffer-live-p buffer) buffer)))
                      :blocker (lambda (_session) drover-schedule-test-blocker)
                      :submit #'drover-schedule-test--submit)
                (alist-get 'fake drover--backends))))

(defun drover-schedule-test--stop ()
  "Cancel the timers of the queue and of its deliveries."
  (when (drover-schedule--queue-p drover-schedule--queue)
    (maphash (lambda (_ timer) (cancel-timer timer)) (drover-schedule--queue-timers drover-schedule--queue))
    (maphash (lambda (_ run)
               (when-let* ((timer (plist-get run :timer)))
                 (cancel-timer timer)))
             (drover-schedule--queue-active drover-schedule--queue))))

(defmacro drover-schedule-test-state (&rest body)
  "Run BODY with an empty queue, read, in a fresh state directory.
The fake backend serves `drover-test-sessions' with buffers, no event
is defined, no former file read, nothing displayed, nor the list
drawn.  Buffers of sessions BODY makes are killed."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (let ((drover-schedule--queue (drover-schedule--queue-make :loaded t))
             (drover-schedule--triggers nil)
             (drover-schedule-former-files nil)
             (drover-schedule-display-function #'ignore)
             (drover-schedule-test-blocker nil)
             (drover-schedule-test-submitted nil)
             (drover-schedule-test-buffers nil))
         (drover-schedule-test--register)
         (unwind-protect
             (cl-letf (((symbol-function 'drover-schedule--refresh) #'ignore))
               ,@body)
           (drover-schedule-test--stop)
           (dolist (entry drover-schedule-test-buffers)
             (when (buffer-live-p (cdr entry))
               (kill-buffer (cdr entry)))))))))

(defun drover-schedule-test-reload ()
  "Forget the queue, then read it again from its file and arm its timers."
  (drover-schedule-test--stop)
  (setq drover-schedule--queue (drover-schedule--queue-make))
  (drover-schedule--start))

(defun drover-schedule-test-target (&optional engine-id)
  "Return a target in the fake backend, of conversation ENGINE-ID."
  (list :link (concat "agent-session:claude-code/" (or engine-id "fixture-session"))
        :backend 'fake :directory temporary-file-directory :title "Scheduler fixture"))

(defun drover-schedule-test-job ()
  "Schedule a prompt an hour ahead for the fixture's target."
  (drover-schedule-add (drover-schedule-test-target) (time-add (current-time) 3600) "check"))

(defun drover-schedule-test-session (&rest properties)
  "Serve the fixture's session, of `claude-code', with PROPERTIES, and return it."
  (let ((session (apply #'drover-test-session "fixture"
                        (append properties (list :engine 'claude-code :engine-id "fixture-session")))))
    (setq drover-test-sessions (cons session (seq-remove (lambda (other) (equal (drover-session-id other) "fixture"))
                                                         drover-test-sessions)))
    session))

(defun drover-schedule-test-buffer (&optional id)
  "Return a buffer of fake session ID, \"fixture\" by default, with a process."
  (let* ((id (or id "fixture"))
         (buffer (generate-new-buffer (concat " *schedule " id "*"))))
    (setf (alist-get id drover-schedule-test-buffers nil nil #'equal) buffer)
    (with-current-buffer buffer
      (make-pipe-process :name (concat "schedule-" id) :buffer buffer :noquery t)
      (setq-local drover-session-at-point-function (lambda () (drover-find 'fake id))))
    buffer))

;;;; The time grammar

(defconst drover-schedule-test-zone "EST5EDT,M3.2.0,M11.1.0"
  "US Eastern rules, so the time tests do not depend on the machine's zone.")

(defun drover-schedule-test-now (&optional hour day month year)
  "Return HOUR:00 on DAY MONTH YEAR in the test zone, Saturday 31 October 2026 14:00 by default."
  (encode-time (list 0 0 (or hour 14) (or day 31) (or month 10) (or year 2026) nil -1
                     drover-schedule-test-zone)))

(defun drover-schedule-test-parse (text &optional now)
  "Return TEXT parsed at NOW as \"YYYY-MM-DD HH:MM\" in the test zone, or the event or nil."
  (let ((when (drover-schedule-parse-time text (or now (drover-schedule-test-now))
                                          drover-schedule-test-zone)))
    (if (and when (not (symbolp when)))
        (format-time-string "%F %R" when drover-schedule-test-zone)
      when)))

(defmacro drover-schedule-test-with-restarts (&rest body)
  "Run BODY with the events of restarts defined, as a configuration does."
  (declare (indent 0) (debug t))
  `(let ((drover-schedule--triggers nil))
     (drover-schedule-define-trigger 'opencode-restart "OpenCode restart")
     (drover-schedule-define-trigger 'emacs-restart "Emacs restart")
     ,@body))

(ert-deftest drover-schedule-test-parse-delays ()
  "Minutes and hours elapse; days and weeks keep the clock time across the DST change of 1 November."
  (dolist (case '(("in 90m" "2026-10-31 15:30") ("in 55m" "2026-10-31 14:55")
                  ("in 2 hours" "2026-10-31 16:00") ("in 8h" "2026-10-31 22:00")
                  ("in 24h" "2026-11-01 13:00") ("in an hour" "2026-10-31 15:00")
                  ("in 3d" "2026-11-03 14:00") ("in 7 days" "2026-11-07 14:00")
                  ("in 1 week" "2026-11-07 14:00") ("in 2wks" "2026-11-14 14:00")
                  ("  IN   3D " "2026-11-03 14:00") ("in 5 mins" "2026-10-31 14:05")))
    (should (equal (drover-schedule-test-parse (car case)) (cadr case)))))

(ert-deftest drover-schedule-test-parse-days ()
  "Days default to 9:00; weekdays and dates are their next occurrence, today included while ahead."
  (dolist (case '(("tomorrow" "2026-11-01 09:00") ("tomorrow 9am" "2026-11-01 09:00")
                  ("tomorrow evening" "2026-11-01 18:00") ("tomorrow at noon" "2026-11-01 12:00")
                  ("tomorrow night" "2026-11-01 21:00") ("Tomorrow at 5:30PM" "2026-11-01 17:30")
                  ("on monday morning" "2026-11-02 09:00") ("monday 9am" "2026-11-02 09:00")
                  ("mon at 17:30" "2026-11-02 17:30") ("thurs afternoon" "2026-11-05 14:00")
                  ("saturday 3pm" "2026-10-31 15:00") ("saturday 1pm" "2026-11-07 13:00")
                  ("saturday" "2026-11-07 09:00") ("9am tomorrow" "2026-11-01 09:00")
                  ("5pm on friday" "2026-11-06 17:00") ("on 11/4 2pm" "2026-11-04 14:00")
                  ("11/4" "2026-11-04 09:00") ("10/31 3pm" "2026-10-31 15:00")
                  ("on 10/31 1pm" "2027-10-31 13:00") ("2/29" "2028-02-29 09:00")
                  ("11/4/2027 2pm" "2027-11-04 14:00") ("12/31 11:59pm" "2026-12-31 23:59")))
    (should (equal (drover-schedule-test-parse (car case)) (cadr case)))))

(ert-deftest drover-schedule-test-parse-times ()
  "A time alone is today, or tomorrow once past; tonight is today at 21:00."
  (dolist (case '(("at 17:30" "2026-10-31 17:30") ("at 5pm" "2026-10-31 17:00")
                  ("5pm" "2026-10-31 17:00") ("at 1pm" "2026-11-01 13:00")
                  ("at 12am" "2026-11-01 00:00") ("at 12pm" "2026-11-01 12:00")
                  ("at 0:30" "2026-11-01 00:30") ("evening" "2026-10-31 18:00")
                  ("morning" "2026-11-01 09:00") ("at 2 pm" "2026-11-01 14:00")
                  ("tonight" "2026-10-31 21:00")))
    (should (equal (drover-schedule-test-parse (car case)) (cadr case))))
  ;; An explicit day can be past; the reader refuses it.
  (let ((now (drover-schedule-test-now 22)))
    (dolist (text '("tonight" "today" "today 9am" "11/4/2025"))
      (should-not (time-less-p now (drover-schedule-parse-time text now drover-schedule-test-zone)))))
  (should (equal (drover-schedule-test-parse "today 3pm") "2026-10-31 15:00")))

(ert-deftest drover-schedule-test-parse-events-and-refusals ()
  "Events defined are read by their label; anything ambiguous or malformed is nil rather than a guess."
  (should-not (drover-schedule-test-parse "after OpenCode restart"))
  (drover-schedule-test-with-restarts
    (should (eq (drover-schedule-test-parse "after OpenCode restart") 'opencode-restart))
    (should (eq (drover-schedule-test-parse "after the next emacs restart") 'emacs-restart))
    (should (eq (drover-schedule-test-parse "AFTER the  Emacs   Restart") 'emacs-restart))
    (dolist (text '("" "someday" "next monday" "9" "tomorrow 9" "in 3" "in three days" "in 3 fortnights"
                    "13:60" "25:00" "0am" "13pm" "2/30" "13/1" "2/29/2027" "after reboot" "tonight 11pm"
                    "monday 9am please" "at" "on" "in 2h30m" "11/4 2pm 2027" "noon tomorrow tomorrow"
                    "after the" "after"))
      (should-not (drover-schedule-test-parse text)))))

(ert-deftest drover-schedule-test-parse-nonexistent-local-time ()
  "A clock time the spring DST change skips is refused; elapsed and calendar delays still resolve."
  (let ((now (drover-schedule-test-now 12 13 3 2027)))
    (should-not (drover-schedule-test-parse "tomorrow 2:30am" now))
    (should (equal (drover-schedule-test-parse "tomorrow 3:30am" now) "2027-03-14 03:30"))
    (should (equal (drover-schedule-test-parse "in 1d" now) "2027-03-14 12:00"))
    (should (equal (drover-schedule-test-parse "in 24h" now) "2027-03-14 13:00"))))

(ert-deftest drover-schedule-test-parse-utc ()
  "The zone argument decides the calendar; nil means the local zone."
  (let ((now (encode-time (list 0 0 23 31 10 2026 nil nil t))))
    (should (equal (format-time-string "%F %R" (drover-schedule-parse-time "at 17:30" now t) t)
                   "2026-11-01 17:30"))
    (should (equal (format-time-string "%F %R" (drover-schedule-parse-time "tomorrow" now t) t)
                   "2026-11-01 09:00")))
  (let ((now (current-time)))
    (should (time-equal-p (drover-schedule-parse-time "in 90m" now) (time-add now 5400)))))

(ert-deftest drover-schedule-test-describe ()
  "Times show the weekday and date, with the year only when it differs."
  (drover-schedule-test-with-restarts
    (let ((now (drover-schedule-test-now)))
      (should (equal (drover-schedule-describe
                      (drover-schedule-parse-time "monday morning" now drover-schedule-test-zone)
                      now drover-schedule-test-zone)
                     "Mon 2 Nov 09:00"))
      (should (equal (drover-schedule-describe
                      (drover-schedule-parse-time "10/31 1pm" now drover-schedule-test-zone)
                      now drover-schedule-test-zone)
                     "Sun 31 Oct 2027 13:00"))
      (should (equal (drover-schedule-describe 'opencode-restart now) "after the next OpenCode restart"))
      (should (equal (drover-schedule-describe 'undefined now) "after the next undefined"))
      (should (equal (drover-schedule-describe 'turn-end now) "after the running turn")))))

(ert-deftest drover-schedule-test-read-when-asks-again ()
  "Unreadable and past input is refused with the reason, and read again."
  (let ((inputs '("in 3x" "11/4/2020" "when turn ends" "in 2h")) prompts defaults)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt _table _pred _require _initial _history default)
                 (push prompt prompts) (push default defaults) (pop inputs))))
      (let ((chosen (drover-schedule-read-when)))
        (should (equal (nth 1 chosen) "in 2h"))
        (should (time-less-p (nth 2 chosen) (car chosen)))
        (should (time-equal-p (car chosen) (time-add (nth 2 chosen) 7200)))))
    (setq prompts (nreverse prompts))
    (should (equal (car prompts) "Send when: "))
    (should (string-match-p "cannot read “in 3x” as a time" (nth 1 prompts)))
    (should (string-match-p "Wed 4 Nov 2020 09:00 is past" (nth 2 prompts)))
    (should (string-match-p "no turn is running" (nth 3 prompts)))
    (should (equal (delete-dups defaults) '("in 24h")))))

(ert-deftest drover-schedule-test-read-when-defaults ()
  "Empty input takes the default: the running turn's end when there is one, else a day."
  (drover-schedule-test-with-restarts
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "")))
      (should (eq (car (drover-schedule-read-when t)) 'turn-end))
      (let ((chosen (drover-schedule-read-when)))
        (should (equal (nth 1 chosen) "in 24h"))
        (should (time-equal-p (car chosen) (time-add (nth 2 chosen) 86400)))))
    (let (table)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _) (setq table collection) "after Emacs restart")))
        (should (eq (car (drover-schedule-read-when t)) 'emacs-restart)))
      (should (equal (all-completions "" table)
                     '("when turn ends" "in 24h" "in 55m" "in 8h" "in 7 days"
                       "after OpenCode restart" "after Emacs restart")))
      (should (eq (completion-metadata-get (completion-metadata "" table nil) 'annotation-function)
                  'drover-schedule--annotate)))))

(ert-deftest drover-schedule-test-preview ()
  "The minibuffer shows what the input, or the default when empty, resolves to."
  (drover-schedule-test-with-restarts
    (should (string-match-p "\\`  → [A-Z][a-z][a-z] " (drover-schedule--annotate "in 24h")))
    (should (equal (drover-schedule--annotate "after Emacs restart") "  → after the next Emacs restart"))
    (let ((drover-schedule--turn t))
      (should (equal (drover-schedule--preview-text "when turn ends") "→ after the running turn")))
    (should (equal (drover-schedule--preview-text "when turn ends") "not a time"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'minibuffer-contents-no-properties) #'buffer-string)
                ((symbol-function 'minibuffer-prompt-end) #'point-min))
        (let ((drover-schedule--default "after OpenCode restart"))
          (drover-schedule--preview)
          (should (string-match-p "after the next OpenCode restart"
                                  (overlay-get drover-schedule--preview-overlay 'after-string)))
          (insert "in 3x")
          (drover-schedule--preview)
          (should (string-match-p "not a time" (overlay-get drover-schedule--preview-overlay 'after-string)))
          (erase-buffer)
          (insert "11/4/2020")
          (drover-schedule--preview)
          (should (string-match-p "is past" (overlay-get drover-schedule--preview-overlay 'after-string))))))))

;;;; Where a prompt goes

(ert-deftest drover-schedule-test-target ()
  "A session's buffer names its conversation; a list, another buffer or an unknown conversation cannot."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (let ((buffer (drover-schedule-test-buffer)))
      (with-current-buffer buffer
        (should (equal (drover-schedule-target)
                       (list :link "agent-session:claude-code/fixture-session" :backend 'fake
                             :directory default-directory :title (buffer-name)))))
      (should (equal (plist-get (drover-schedule-target buffer) :title) (buffer-name buffer)))
      ;; Not from a list, nor a buffer showing a session it is not.
      (with-temp-buffer
        (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "fixture")))
        (should-error (drover-schedule-target) :type 'user-error)
        (setq-local drover-view-p t)
        (should-error (drover-schedule-target) :type 'user-error))
      ;; A conversation whose identifier is not known yet cannot be scheduled for.
      (drover-schedule-test-session :engine-id nil)
      (setf (drover-session-engine-id (car drover-test-sessions)) nil)
      (should-error (drover-schedule-target buffer) :type 'user-error))))

;;;; Scheduling from a session's buffer

(defun drover-schedule-test-chosen (&optional when)
  "Return a `drover-schedule-read-when' result for WHEN, an hour ahead by default."
  (let ((now (current-time)))
    (list (or when (time-add now 3600)) (if (and when (symbolp when)) "after Emacs restart" "in 1h") now)))

(defmacro drover-schedule-test-opencode (input &rest body)
  "Run BODY in a fake OpenCode session buffer whose prompt holds INPUT after a transcript."
  (declare (indent 1))
  `(with-temp-buffer
     (setq major-mode 'opencode-session-mode)
     (setq-local opencode-session-status 'idle)
     (setq-local opencode--attachments nil)
     (drover-prompt-define 'schedule #'drover-opencode-schedule-input)
     (insert "transcript\n> ")
     (let ((pmark (point)))
       (insert ,input)
       (cl-letf (((symbol-function 'opencode-pmark) (lambda () pmark))
                 ((symbol-function 'get-buffer-process) (lambda (_) 'process))
                 ((symbol-function 'drover-schedule-target)
                  (lambda (&optional _) (drover-schedule-test-target))))
         ,@body))))

(ert-deftest drover-schedule-test-session-input ()
  "An OpenCode session's input is scheduled, then cleared; one outcome is told."
  (drover-schedule-test-state
    (drover-schedule-test-opencode "  check the build\n"
      (let (logged)
        (cl-letf (((symbol-function 'drover-schedule-read-when)
                   (lambda (&optional _) (drover-schedule-test-chosen)))
                  ((symbol-function 'read-string) (lambda (&rest _) (ert-fail "The input is the prompt")))
                  ((symbol-function 'message)
                   (lambda (format &rest args)
                     (when (and format message-log-max) (push (apply #'format-message format args) logged)))))
          (call-interactively #'drover-schedule-prompt))
        (should (equal (buffer-string) "transcript\n> "))
        (let ((job (car (drover-schedule--jobs))))
          (should (equal (plist-get job :prompt) "check the build"))
          (should (equal (plist-get job :instruction) "in 1h"))
          (should (eq (plist-get job :backend) 'fake))
          (should-not (plist-get job :options)))
        (should (= 1 (length logged)))
        (should (string-prefix-p "Scheduled \"check the build\" for " (car logged)))))))

(ert-deftest drover-schedule-test-session-reads-empty-prompt ()
  "Without input, or in another front end, the prompt is read; a blank one is refused."
  (drover-schedule-test-state
    (drover-schedule-test-opencode "   "
      (cl-letf (((symbol-function 'drover-schedule-read-when)
                 (lambda (&optional _) (drover-schedule-test-chosen)))
                ((symbol-function 'read-string) (lambda (&rest _) " from the minibuffer ")))
        (drover-schedule-prompt))
      (should (equal (plist-get (car (drover-schedule--jobs)) :prompt) "from the minibuffer"))
      (should (equal (buffer-string) "transcript\n>    ")))
    (drover-schedule-test-session)
    (with-current-buffer (drover-schedule-test-buffer)
      (cl-letf (((symbol-function 'drover-schedule-read-when)
                 (lambda (&optional turn) (should-not turn) (drover-schedule-test-chosen)))
                ((symbol-function 'read-string) (lambda (&rest _) "terminal prompt")))
        (drover-schedule-prompt))
      (should (equal (plist-get (car (drover-schedule--jobs)) :prompt) "terminal prompt"))
      (cl-letf (((symbol-function 'drover-schedule-read-when) (lambda (&optional _) (drover-schedule-test-chosen)))
                ((symbol-function 'read-string) (lambda (&rest _) "  ")))
        (should-error (drover-schedule-prompt) :type 'user-error))
      (should (= 2 (length (drover-schedule--jobs)))))))

(ert-deftest drover-schedule-test-session-turn-end ()
  "While an OpenCode turn runs, its end is offered and queues the input through opencode.el, outside the queue."
  (drover-schedule-test-state
    (dolist (input '("follow up" ""))
      (drover-schedule-test-opencode input
        (setq-local opencode-session-status 'busy)
        (let (queued offered)
          (cl-letf (((symbol-function 'drover-schedule-read-when)
                     (lambda (&optional turn) (setq offered turn) (drover-schedule-test-chosen 'turn-end)))
                    ((symbol-function 'read-string) (lambda (&rest _) "typed later"))
                    ((symbol-function 'opencode-queue-input)
                     (lambda () (setq queued (list (point) nil (buffer-string))))))
            (goto-char (point-min))
            (drover-schedule-prompt))
          (should offered)
          (should (= (car queued) (point-max)))
          (should (equal (nth 2 queued) (concat "transcript\n> " (if (equal input "") "typed later" input))))
          (should-not (drover-schedule--jobs)))))))

(ert-deftest drover-schedule-test-session-attachments ()
  "Input attaching files is refused and kept, since a scheduled prompt carries text only."
  (drover-schedule-test-state
    (drover-schedule-test-opencode "with a file"
      (setq-local opencode--attachments '((:file "/tmp/x")))
      (cl-letf (((symbol-function 'drover-schedule-read-when) (lambda (&optional _) (drover-schedule-test-chosen))))
        (should-error (drover-schedule-prompt) :type 'user-error))
      (should (equal (buffer-string) "transcript\n> with a file"))
      (should-not (drover-schedule--jobs)))))

(ert-deftest drover-schedule-test-session-event ()
  "An event chosen as the time is waited for, asked for, and the input cleared."
  (drover-schedule-test-state
    (let ((requested 0))
      (drover-schedule-define-trigger 'emacs-restart "Emacs restart"
                                      :request (lambda () (cl-incf requested) "agent:emacs-restart"))
      (drover-schedule-test-opencode "verify"
        (cl-letf (((symbol-function 'drover-schedule-read-when)
                   (lambda (&optional _) (drover-schedule-test-chosen 'emacs-restart))))
          (drover-schedule-prompt))
        (let ((job (car (drover-schedule--jobs))))
          (should (drover-schedule--waiting-p job "agent:emacs-restart"))
          (should (equal (plist-get job :prompt) "verify"))
          (should (equal (plist-get job :instruction) "after Emacs restart")))
        (should (= requested 1))
        (should (= (drover-schedule-waiting-count "agent:emacs-restart") 1))
        (should (equal (buffer-string) "transcript\n> "))))))

;;;; Scheduling drover's prompt

(defun drover-schedule-test-model (&rest engines)
  "Return a fixture model ENGINES run."
  (drover-model-create :provider-id "openai" :provider-name "OpenAI" :id "gpt-5.5" :name "GPT-5.5"
                       :variants '((low) (high)) :variants-known t :engines engines))

(defmacro drover-schedule-test-prompt (model &rest body)
  "Run BODY in drover's prompt holding an Org prompt with explicit options and MODEL."
  (declare (indent 1))
  `(let* ((drover-prompt-buffer-name " *drover schedule test*")
          (prompt-buf (drover-prompt-buffer t))
          (session (drover-schedule-test-session :title "Scheduler fixture"))
          (target-buf (drover-schedule-test-buffer))
          picked)
     (unwind-protect
         (with-current-buffer prompt-buf
           (insert "Look:\n#+begin_src sh\nls\n#+end_src\n")
           (setq drover-prompt-picked-agent "plan"
                 drover-prompt-picked-model ,model
                 drover-prompt-picked-variant 'high)
           (cl-letf (((symbol-function 'drover-schedule-read-when)
                      (lambda (&optional turn) (should-not turn) (drover-schedule-test-chosen)))
                     ((symbol-function 'drover-prompt-read-target)
                      (lambda (_prompt &rest keys) (setq picked keys) session)))
             ,@body))
       (ignore target-buf)
       (when (buffer-live-p prompt-buf) (kill-buffer prompt-buf)))))

(ert-deftest drover-schedule-test-prompt-action ()
  "drover's prompt schedules through `drover-schedule-prompt', OpenCode buffers their input."
  (let ((drover-prompt-buffer-name " *drover schedule action*"))
    (unwind-protect
        (let ((buffer (drover-prompt-buffer t)))
          (should (eq (alist-get 'schedule (buffer-local-value 'drover-prompt-actions buffer))
                      #'drover-schedule-prompt))
          (dolist (action '(send clear attach agent model effort directory copy-block open-link))
            (should (drover-prompt-defines-p buffer action))))
      (when-let* ((buffer (get-buffer " *drover schedule action*")))
        (kill-buffer buffer))))
  (should (eq (alist-get 'schedule drover-opencode--prompt-actions) #'drover-opencode-schedule-input)))

(ert-deftest drover-schedule-test-prompt-buffer ()
  "The prompt's Markdown goes to an existing session with its explicit options, then the prompt closes."
  (drover-schedule-test-state
    (let ((model (drover-schedule-test-model 'claude-code)))
      (drover-schedule-test-prompt model
        (drover-schedule-prompt)
        (should (equal picked '(:existing-only t :options-menu t)))
        (should-not (buffer-live-p prompt-buf))
        (let ((job (car (drover-schedule--jobs))))
          (should (equal (plist-get job :prompt) "Look:\n```sh\nls\n```"))
          (should (equal (plist-get job :link) "agent-session:claude-code/fixture-session"))
          (should (equal (plist-get job :options) (list :agent "plan" :model model :variant 'high))))
        ;; The model survives the file, as the data it is made of.
        (with-temp-buffer
          (insert-file-contents (drover-schedule-file))
          (should-not (search-forward "#s(drover-model" nil t)))
        (drover-schedule-test-reload)
        (let ((options (plist-get (car (drover-schedule--jobs)) :options)))
          (should (drover-model-p (plist-get options :model)))
          (should (equal (drover-model-label (plist-get options :model)) (drover-model-label model))))))))

(ert-deftest drover-schedule-test-prompt-region ()
  "With an active region, only the region is scheduled, and the rest of the prompt stays."
  (drover-schedule-test-state
    (drover-schedule-test-prompt nil
      (erase-buffer)
      (insert "first part\nsecond part\n")
      (let ((transient-mark-mode t))
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 1)
        (activate-mark)
        (drover-schedule-prompt))
      (should (buffer-live-p prompt-buf))
      (should (equal (buffer-string) "second part\n"))
      (should-not mark-active)
      (should (equal (plist-get (car (drover-schedule--jobs)) :prompt) "first part")))))

(ert-deftest drover-schedule-test-prompt-buffer-incompatible-model ()
  "A model the session cannot run is reported at once and dropped with its variant."
  (drover-schedule-test-state
    (let (warned)
      (drover-schedule-test-prompt (drover-schedule-test-model 'opencode)
        (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) (push args warned))))
          (drover-schedule-prompt))
        (should warned)
        (should (equal (plist-get (car (drover-schedule--jobs)) :options)
                       '(:agent "plan" :model nil :variant nil)))))))

(ert-deftest drover-schedule-test-prompt-buffer-empty ()
  "An empty prompt reads the text to send; no explicit option stores none."
  (drover-schedule-test-state
    (drover-schedule-test-prompt nil
      (erase-buffer)
      (setq drover-prompt-picked-agent nil drover-prompt-picked-variant nil)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "read instead")))
        (drover-schedule-prompt))
      (should-not (buffer-live-p prompt-buf))
      (let ((job (car (drover-schedule--jobs))))
        (should (equal (plist-get job :prompt) "read instead"))
        (should-not (plist-get job :options))))))

;;;; Records and the file

(ert-deftest drover-schedule-test-options-validity ()
  "Stored options must be choices the delivery can apply; a host may be named."
  (let ((job (append (list :id "x" :due 1.0 :prompt "p" :status 'pending)
                     (drover-schedule-test-target))))
    (should (drover-schedule-job-valid-p job))
    (should (drover-schedule-job-valid-p
             (append job (list :options (list :agent "plan" :model (drover-schedule-test-model 'opencode)
                                              :variant 'high)))))
    (should-not (drover-schedule-job-valid-p (append job '(:options (:model "gpt-5.5")))))
    (should-not (drover-schedule-job-valid-p (append job '(:options (:agent plan)))))
    (should-not (drover-schedule-job-valid-p (append job '(:options (:variant "high")))))
    (should (drover-schedule-job-valid-p (plist-put (copy-sequence job) :link "agent-session://aiur/codex/r1")))
    (should-not (drover-schedule-job-valid-p (plist-put (copy-sequence job) :link "agent-session:nothing")))
    (should-not (drover-schedule-job-valid-p (plist-put (copy-sequence job) :backend nil)))
    ;; An event not defined still names one: the prompt waits for it.
    (should (drover-schedule-job-valid-p
             (append (list :id "y" :due nil :after "key" :trigger 'later :prompt "p" :status 'pending)
                     (drover-schedule-test-target))))))

(defconst drover-schedule-test-former
  "(:version 1 :jobs ((:id \"BEE7A6B2-E54B-4478-83A5-E0128333FD57\" :due 1793408400.0 :prompt \"Scheduled prompt: check\" :instruction \"in 2h\" :created 1793029983.533437 :status sent :error nil :sent-at 1793408401.473811 :link \"agent-session:opencode/ses_e8733d22bffetaOlm1T2B2Q5o5\" :frontend opencode :directory \"/Users/val/\" :title \"*opencode: one*\") (:id \"pending-ide\" :due 4102444800.0 :prompt \"later\" :instruction \"in 7 days\" :created 1793029983.0 :status pending :link \"agent-session:claude-code/48d12bd1-15e6-44a6-a7a3-271cda186c41\" :frontend ide :directory \"/work/\" :title \"*claude* two\") (:id \"waiting\" :due nil :after \"opencode:server\" :trigger opencode-restart :prompt \"after\" :instruction \"after OpenCode restart\" :created 1793029983.0 :status pending :error nil :sent-at nil :link \"agent-session:opencode/ses_eba979e63ffeBx0KYpNaHT1YON\" :frontend opencode :directory \"/Users/val/\" :title \"*opencode: three*\") (:id \"modelled\" :due 4102444800.0 :prompt \"p\" :instruction \"x\" :created 1.0 :status sending :error nil :sent-at nil :options (:agent nil :model #s(drover-model \"stale\") :variant max) :link \"agent-session:codex/r1\" :frontend agent-shell :directory \"/work/\" :title \"shell\")))"
  "A queue as an earlier configuration wrote it, its first prompt the user's own.")

(ert-deftest drover-schedule-test-former-file ()
  "A queue of an earlier configuration joins drover's once, nothing lost, and its file is moved aside."
  (drover-schedule-test-state
    (let ((former (expand-file-name "agent/scheduled-prompts.eld" drover-state-directory)))
      (drover-test-write former drover-schedule-test-former)
      (let ((drover-schedule-former-files (list former)))
        ;; drover's queue has one of its prompts already, as it saved it last.
        (setf (drover-schedule--queue-jobs drover-schedule--queue)
              (list (append (list :id "pending-ide" :due 4102444800.0 :prompt "kept as drover saved it"
                                  :status 'cancelled :error nil :sent-at nil)
                            (drover-schedule-test-target))))
        (drover-schedule--save)
        (drover-schedule-test-reload)
        (let ((jobs (drover-schedule--jobs)))
          (should (equal (mapcar (lambda (job) (plist-get job :id)) jobs)
                         '("pending-ide" "BEE7A6B2-E54B-4478-83A5-E0128333FD57" "waiting" "modelled")))
          (should (equal (plist-get (car jobs) :prompt) "kept as drover saved it"))
          (let ((sent (nth 1 jobs)) (waiting (nth 2 jobs)) (modelled (nth 3 jobs)))
            (should (eq (plist-get sent :backend) 'opencode))
            (should-not (plist-member sent :frontend))
            (should (eq (plist-get sent :status) 'sent))
            (should (drover-schedule--waiting-p waiting "opencode:server"))
            (should (eq (plist-get waiting :trigger) 'opencode-restart))
            (should (eq (plist-get modelled :backend) 'agent-shell))
            ;; A model that does not read goes with its variant; the job stays.
            (should (equal (plist-get modelled :options) '(:agent nil :model nil :variant nil)))
            (should (eq (plist-get modelled :status) 'interrupted))))
        (should-not (file-exists-p former))
        (should (file-exists-p (concat former ".migrated")))
        ;; Saved in drover's file, of its version, read alone next time.
        (drover-schedule-test-reload)
        (should (= 4 (length (drover-schedule--jobs))))
        (with-temp-buffer
          (insert-file-contents (drover-schedule-file))
          (should (search-forward "(:version 2" nil t)))))))

(ert-deftest drover-schedule-test-former-file-unreadable ()
  "A former queue that does not read stays where it is, told of, and drover's queue goes on."
  (drover-schedule-test-state
    (let ((former (expand-file-name "agent/scheduled-prompts.eld" drover-state-directory))
          (warned nil))
      (drover-test-write former "(:version 1 :jobs ((:id \"bad\")))")
      (let ((drover-schedule-former-files (list former)))
        (cl-letf (((symbol-function 'drover-warn) (lambda (&rest args) (push args warned))))
          (drover-schedule-test-reload))
        (should warned)
        (should-not (drover-schedule--jobs))
        (should (file-exists-p former))))))

(ert-deftest drover-schedule-test-roundtrip-and-recovery ()
  "Prompts Emacs stopped while opening their session go again; one it stopped while submitting is interrupted."
  (drover-schedule-test-state
    (let ((job (drover-schedule-test-job)))
      (setf (plist-get job :due) (- (float-time) 60) (plist-get job :status) 'waiting)
      (drover-schedule--save)
      (drover-schedule-test-reload)
      (should (eq (plist-get (car (drover-schedule--jobs)) :status) 'pending))
      (should (= 1 (hash-table-count (drover-schedule--timers))))
      (drover-schedule--start)
      (should (= 1 (hash-table-count (drover-schedule--timers))))
      (setf (plist-get (car (drover-schedule--jobs)) :status) 'sending)
      (drover-schedule--save)
      (drover-schedule-test-reload)
      (should (eq (plist-get (car (drover-schedule--jobs)) :status) 'interrupted))
      (should (= 0 (hash-table-count (drover-schedule--timers)))))))

(ert-deftest drover-schedule-test-corrupt-file ()
  "A queue that does not read is neither replaced nor added to; restoring tells it on one line."
  (drover-schedule-test-state
    (let ((content "(:version 2 :jobs ((:id \"bad\")))")
          (reported nil))
      (drover-test-write (drover-schedule-file) content)
      (setq drover-schedule--queue (drover-schedule--queue-make))
      (should-error (drover-schedule-load))
      (should-not (drover-schedule--queue-loaded drover-schedule--queue))
      (should-error (drover-schedule--save))
      (should-error (drover-schedule-test-job))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) reported))))
        (should-not (drover-schedule-restore)))
      (should (= 1 (length reported)))
      (should (string-prefix-p "Drover scheduled prompts: error: Invalid queue" (car reported)))
      (with-temp-buffer
        (insert-file-contents (drover-schedule-file))
        (should (equal (buffer-string) content))))))

(ert-deftest drover-schedule-test-stale-queue ()
  "A queue another version of drover made is made anew, from the file."
  (drover-schedule-test-state
    (drover-schedule-test-job)
    (let ((messages nil))
      (setq drover-schedule--queue (record 'drover-schedule--queue 'old-layout nil t nil nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) messages))))
        (should (= 1 (length (drover-schedule--jobs))))
        (drover-schedule--start))
      (should (equal (plist-get (car (drover-schedule--jobs)) :prompt) "check"))
      (should (member "Drover: the queue of scheduled prompts made by another version of drover, made anew"
                      messages)))))

(ert-deftest drover-schedule-test-latency-catch-up ()
  "A prompt for a time that passed while it was read goes at once."
  (drover-schedule-test-state
    (let ((job (drover-schedule-add (drover-schedule-test-target)
                                    (time-subtract (current-time) 10) "check" "in ten seconds"
                                    (time-subtract (current-time) 20))))
      (should (eq (plist-get job :status) 'pending))
      (should (= 1 (hash-table-count (drover-schedule--timers))))
      (should-error (drover-schedule-add (drover-schedule-test-target) (time-subtract (current-time) 10) "late")))))

(ert-deftest drover-schedule-test-save-before-arm ()
  "A prompt that could not be saved is not scheduled."
  (drover-schedule-test-state
    (cl-letf (((symbol-function 'drover-schedule--save) (lambda () (error "Disk unavailable"))))
      (should-error (drover-schedule-test-job)))
    (should-not (drover-schedule--jobs))
    (should (= 0 (hash-table-count (drover-schedule--timers))))))

;;;; Events

(defmacro drover-schedule-test-events (&rest body)
  "Run BODY with the event `later' defined, its requests and releases in `asked' and `released'."
  (declare (indent 0) (debug t))
  `(let ((asked 0) (released nil))
     (drover-schedule-define-trigger 'later "later event"
                                     :request (lambda () (cl-incf asked) "later:key")
                                     :release (lambda (key) (push key released)))
     ,@body))

(ert-deftest drover-schedule-test-event-fired ()
  "Prompts waiting for an event wait across restarts, then go once it happened."
  (drover-schedule-test-state
    (drover-schedule-test-events
      (let ((first (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "verify"))
            (second (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "again")))
        (should (= asked 2))
        (should (= 0 (hash-table-count (drover-schedule--timers))))
        (should (= 2 (drover-schedule-waiting-count "later:key")))
        (should (= 0 (drover-schedule-waiting-count "other")))
        ;; Waiting once read again, never armed.
        (drover-schedule-test-reload)
        (should (= 2 (seq-count #'drover-schedule--waiting-p (drover-schedule--jobs))))
        (should (= 0 (hash-table-count (drover-schedule--timers))))
        (should (= 0 (drover-schedule-fire "other")))
        (should (= 2 (drover-schedule-fire "later:key")))
        (should (= 2 (hash-table-count (drover-schedule--timers))))
        (dolist (job (drover-schedule--jobs))
          (should (eq (plist-get job :status) 'pending))
          (should (<= (plist-get job :due) (float-time))))
        (should-not released)
        (ignore first second)))))

(ert-deftest drover-schedule-test-event-cancelled-and-released ()
  "An event that will not happen cancels its prompts; the request goes with the last prompt waiting."
  (drover-schedule-test-state
    (drover-schedule-test-events
      (let ((job (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "verify")))
        (drover-schedule-fire "later:key" "dismissed")
        (should (eq (plist-get job :status) 'cancelled))
        (should (equal (plist-get job :error) "The later event it waited for was dismissed")))
      (let ((job (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "verify"))
            (other (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "other")))
        (drover-schedule-cancel job)
        (should-not released)
        (drover-schedule-cancel other)
        (should (equal released '("later:key")))
        (should (eq (plist-get other :status) 'cancelled)))
      ;; Sending a waiting prompt now releases the request too.
      (setq released nil)
      (let ((job (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "now")))
        (cl-letf (((symbol-function 'drover-schedule--at-point) (lambda () job))
                  ((symbol-function 'y-or-n-p) #'always))
          (drover-schedule-retry))
        (should (numberp (plist-get job :due)))
        (should (gethash (plist-get job :id) (drover-schedule--timers)))
        (should (equal released '("later:key"))))
      ;; A prompt that could not be saved releases its request.
      (setq released nil)
      (cl-letf (((symbol-function 'drover-schedule--save) (lambda () (error "Disk unavailable"))))
        (should-error (drover-schedule-add-after-trigger (drover-schedule-test-target) 'later "lost")))
      (should (equal released '("later:key"))))))

(ert-deftest drover-schedule-test-event-without-request ()
  "An event without a request is waited for by its name."
  (drover-schedule-test-state
    (drover-schedule-define-trigger 'plain "plain event")
    (let ((job (drover-schedule-add-after-trigger (drover-schedule-test-target) 'plain "go")))
      (should (equal (plist-get job :after) "plain"))
      (should (= 1 (drover-schedule-fire "plain"))))))

;;;; Delivery

(ert-deftest drover-schedule-test-delivery ()
  "A due prompt finds its session's buffer, waits until it takes a prompt, then is submitted once."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (let* ((buffer (drover-schedule-test-buffer))
           (job (drover-schedule-test-job))
           (displayed nil)
           (drover-schedule-display-function (lambda (shown title) (setq displayed (list shown title)) nil))
           (drover-schedule-test-blocker "the input holds a draft")
           run)
      (drover-schedule--run (plist-get job :id))
      (setq run (gethash (plist-get job :id) (drover-schedule--active)))
      (should (equal displayed (list buffer "Scheduler fixture")))
      (should (eq (plist-get run :buffer) buffer))
      (should (eq (plist-get job :status) 'waiting))
      (should (timerp (plist-get run :timer)))
      (drover-schedule--poll job run)
      (should-not drover-schedule-test-submitted)
      (setq drover-schedule-test-blocker nil)
      (drover-schedule--poll job run)
      (drover-schedule--poll job run)
      (should (= 1 (length drover-schedule-test-submitted)))
      (pcase-let ((`(,id ,text ,then ,_else) (car drover-schedule-test-submitted)))
        (should (equal (list id text) '("fixture" "check")))
        (should (eq (plist-get job :status) 'sending))
        (funcall then)
        ;; Once only.
        (funcall then))
      (should (eq (plist-get job :status) 'sent))
      (should (numberp (plist-get job :sent-at)))
      (should (= 0 (hash-table-count (drover-schedule--active))))
      (drover-schedule-test-reload)
      (should (numberp (plist-get (car (drover-schedule--jobs)) :sent-at))))))

(ert-deftest drover-schedule-test-delivery-interrupted ()
  "A submission the front end refuses is interrupted: whether it went out is not known."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (drover-schedule-test-buffer)
    (let ((job (drover-schedule-test-job)) run)
      (drover-schedule--run (plist-get job :id))
      (setq run (gethash (plist-get job :id) (drover-schedule--active)))
      (drover-schedule--poll job run)
      (funcall (nth 3 (car drover-schedule-test-submitted)) "Connection lost")
      (should (eq (plist-get job :status) 'interrupted))
      (should (equal (plist-get job :error) "Connection lost"))
      (should-error (cl-letf (((symbol-function 'drover-schedule--at-point) (lambda () job))
                              ((symbol-function 'y-or-n-p) #'ignore))
                      (drover-schedule-retry)
                      (error "Not retried")))
      (cl-letf (((symbol-function 'drover-schedule--at-point) (lambda () job))
                ((symbol-function 'y-or-n-p) #'always))
        (drover-schedule-retry))
      (should (eq (plist-get job :status) 'pending)))))

(ert-deftest drover-schedule-test-delivery-applies-options ()
  "Stored options switch the session before the prompt goes out, unless the run ended meanwhile."
  (drover-schedule-test-state
    (let ((session (drover-schedule-test-session))
          (buffer (drover-schedule-test-buffer)))
      (let* ((job (drover-schedule-test-job)) (run (list :buffer buffer)) applied)
        (setf (plist-get job :options) '(:agent "plan" :model nil :variant high))
        (puthash (plist-get job :id) run (drover-schedule--active))
        (cl-letf (((symbol-function 'drover-prompt-apply-options)
                   (lambda (target options then) (setq applied (list target options then)))))
          (drover-schedule--send job run)
          (should (eq (plist-get job :status) 'sending))
          (should (equal (butlast applied) (list session '(:agent "plan" :model nil :variant high))))
          (should-not drover-schedule-test-submitted)
          (funcall (nth 2 applied))
          (should (= 1 (length drover-schedule-test-submitted)))
          (funcall (nth 2 (car drover-schedule-test-submitted)))
          (should (eq (plist-get job :status) 'sent))
          ;; A late callback of a finished run sends nothing.
          (funcall (nth 2 applied))
          (should (= 1 (length drover-schedule-test-submitted)))))
      (let* ((closed (generate-new-buffer " *schedule-closed*"))
             (job (drover-schedule-test-job)) (run (list :buffer closed)) then)
        (setf (plist-get job :options) '(:agent "plan"))
        (puthash (plist-get job :id) run (drover-schedule--active))
        (cl-letf (((symbol-function 'drover-prompt-apply-options) (lambda (_ _ next) (setq then next))))
          (drover-schedule--send job run)
          (kill-buffer closed)
          (funcall then))
        (should (eq (plist-get job :status) 'failed))))))

(ert-deftest drover-schedule-test-exact-session ()
  "The buffer delivered to shows the conversation in the job's backend, with a live process."
  (drover-schedule-test-state
    (let ((other (drover-test-session "other" :engine 'claude-code :engine-id "other-session")))
      (drover-schedule-test-session)
      (push other drover-test-sessions)
      (drover-schedule-test-buffer "other")
      (should-not (drover-schedule--buffer (drover-schedule-test-job)))
      (let ((buffer (drover-schedule-test-buffer)))
        (should (eq buffer (drover-schedule--buffer (drover-schedule-test-job))))
        (should-not (drover-schedule--buffer (plist-put (drover-schedule-test-job) :backend 'elsewhere)))
        (delete-process (get-buffer-process buffer))
        (should-not (drover-schedule--buffer (drover-schedule-test-job)))))))

(ert-deftest drover-schedule-test-resume ()
  "A conversation without a buffer resumes in its backend, in the window given, from its directory."
  (drover-schedule-test-state
    (let ((drover--resumers nil)
          (drover--resumer-properties nil)
          (resumed nil)
          (window (selected-window)))
      (drover-schedule-test-session)
      (drover-register-resumer 'claude-code 'fake
                               (lambda (id directory)
                                 (push (list id directory (selected-window)) resumed)
                                 (drover-schedule-test-buffer)))
      (let* ((drover-schedule-display-function (lambda (buffer _title) (should-not buffer) window))
             (job (drover-schedule-test-job)))
        (drover-schedule--run (plist-get job :id))
        (should (equal resumed (list (list "fixture-session" temporary-file-directory window))))
        (should (eq (plist-get (gethash (plist-get job :id) (drover-schedule--active)) :buffer)
                    (alist-get "fixture" drover-schedule-test-buffers nil nil #'equal)))
        (should (eq (plist-get job :status) 'waiting))))))

(ert-deftest drover-schedule-test-resume-later ()
  "A front end opening the conversation later attaches its buffer then; one failing fails the prompt."
  (drover-schedule-test-state
    (let ((drover--resumers nil)
          (drover--resumer-properties nil)
          (later nil))
      (drover-schedule-test-session)
      (drover-register-resumer 'claude-code 'fake
                               (lambda (_id _directory then else) (setq later (list then else)))
                               :by-id t :async t)
      (let ((job (drover-schedule-test-job)) run)
        (drover-schedule--run (plist-get job :id))
        (setq run (gethash (plist-get job :id) (drover-schedule--active)))
        (should (eq (plist-get job :status) 'opening))
        (should (equal (drover-schedule--wait-reason job run) "the session never opened"))
        (funcall (car later) (drover-schedule-test-buffer))
        (should (eq (plist-get job :status) 'waiting))
        (drover-schedule-cancel job)
        (kill-buffer (alist-get "fixture" drover-schedule-test-buffers nil nil #'equal)))
      (let ((job (drover-schedule-test-job)))
        (drover-schedule--run (plist-get job :id))
        (funcall (cadr later) "No OpenCode session")
        (should (eq (plist-get job :status) 'failed))
        (should (equal (plist-get job :error) "No OpenCode session"))))))

(defun drover-schedule-test--archived-by-drover ()
  "Have drover archive the fake sessions itself, as for a backend without an archive."
  (let ((plist (alist-get 'fake drover--backends))
        (kept nil))
    (while plist
      (unless (eq (car plist) :archive)
        (setq kept (append kept (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    (setf (alist-get 'fake drover--backends) kept)))

(ert-deftest drover-schedule-test-delivery-restores-archived ()
  "A prompt to a conversation drover archived restores it, then goes to its buffer, still open.
One that cannot be restored is sent nothing and fails, saying why."
  (drover-schedule-test-state
    (drover-schedule-test--archived-by-drover)
    (let ((session (drover-schedule-test-session))
          (buffer (drover-schedule-test-buffer)))
      (drover-archive-session session)
      (should (drover-session-archived-p session))
      (should (buffer-live-p buffer))
      (should-not (drover-sessions))
      (let ((job (drover-schedule-test-job)) run)
        (drover-schedule--run (plist-get job :id))
        (setq run (gethash (plist-get job :id) (drover-schedule--active)))
        ;; Restored as delivery begins, its open buffer taken.
        (should-not (drover-session-archived-p session))
        (should (eq (plist-get run :buffer) buffer))
        (drover-schedule--poll job run)
        (should (equal (mapcar #'car drover-schedule-test-submitted) '("fixture")))
        (funcall (nth 2 (car drover-schedule-test-submitted)))
        (should (eq (plist-get job :status) 'sent)))
      ;; Archived again; restored, no backend would list it.
      (setq drover-schedule-test-submitted nil)
      (drover-archive-session session)
      (cl-letf (((symbol-function 'drover--followed-p) #'ignore))
        (let ((job (drover-schedule-test-job)) run)
          (drover-schedule--run (plist-get job :id))
          (setq run (gethash (plist-get job :id) (drover-schedule--active)))
          (should (eq (plist-get run :buffer) buffer))
          (drover-schedule--poll job run)
          (should-not drover-schedule-test-submitted)
          (should (eq (plist-get job :status) 'failed))
          (should (string-prefix-p "Could not restore Session fixture from the archive: no front end follows"
                                   (plist-get job :error)))
          (should (drover-session-archived-p session)))))))

(ert-deftest drover-schedule-test-delivery-restores-resumed ()
  "A conversation drover archived, without a buffer, is resumed, then restored before the prompt goes."
  (drover-schedule-test-state
    (drover-schedule-test--archived-by-drover)
    (let ((drover--resumers nil)
          (drover--resumer-properties nil)
          (session (drover-schedule-test-session)))
      (drover-archive-session session)
      ;; No backend lists it any more, but drover's archive.
      (setq drover-test-sessions nil)
      (drover-register-resumer 'claude-code 'fake
                               (lambda (_id _directory)
                                 (drover-schedule-test-session)
                                 (drover-schedule-test-buffer)))
      (let ((job (drover-schedule-test-job)) run)
        (drover-schedule--run (plist-get job :id))
        (setq run (gethash (plist-get job :id) (drover-schedule--active)))
        (should (plist-get run :buffer))
        ;; Not before it was resumed, as nothing listed it.
        (should (drover-session-archived-p (drover-find 'fake "fixture")))
        (drover-schedule--poll job run)
        (should-not (drover-session-archived-p (drover-find 'fake "fixture")))
        (should (equal (mapcar #'car drover-schedule-test-submitted) '("fixture")))))))

(ert-deftest drover-schedule-test-turn-of-archived ()
  "The turn of a session drover archived is waited for, as of any session."
  (drover-schedule-test-state
    (drover-schedule-test--archived-by-drover)
    (let ((session (drover-schedule-test-session :status 'working)))
      (drover-schedule-test-buffer)
      (drover-archive-session session)
      (let ((job (drover-schedule-add-after-turn (drover-schedule-test-target) "next")))
        (drover-schedule--turn-check)
        (should (drover-schedule--turn-waiting-p job))
        (setf (drover-session-status session) 'idle)
        (drover-schedule--turn-check)
        (should-not (drover-schedule--turn-waiting-p job))
        (should (gethash (plist-get job :id) (drover-schedule--timers)))))))

(ert-deftest drover-schedule-test-submit-not-sent ()
  "A front end telling nothing was sent fails the prompt, rather than leave it uncertain."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (drover-schedule-test-buffer)
    (let ((job (drover-schedule-test-job)) run)
      (drover-schedule--run (plist-get job :id))
      (setq run (gethash (plist-get job :id) (drover-schedule--active)))
      (drover-schedule--poll job run)
      (funcall (nth 3 (car drover-schedule-test-submitted)) "Could not restore it from the archive" t)
      (should (eq (plist-get job :status) 'failed))
      (should (equal (plist-get job :error) "Could not restore it from the archive")))))

(ert-deftest drover-schedule-test-delivery-refused ()
  "A conversation of another host, of a missing directory or a backend not delivering, fails at once."
  (drover-schedule-test-state
    (let ((remote (drover-schedule-add (plist-put (drover-schedule-test-target) :link
                                                  "agent-session://aiur/claude-code/x")
                                       (time-add (current-time) 3600) "check")))
      (drover-schedule--run (plist-get remote :id))
      (should (eq (plist-get remote :status) 'failed))
      (should (string-match-p "other hosts" (plist-get remote :error))))
    (let ((gone (drover-schedule-add (plist-put (drover-schedule-test-target) :directory "/nonexistent/dir/")
                                     (time-add (current-time) 3600) "check")))
      (drover-schedule--run (plist-get gone :id))
      (should (eq (plist-get gone :status) 'failed))
      (should (string-match-p "unavailable" (plist-get gone :error))))
    (should (= 0 (hash-table-count (drover-schedule--active))))))

(ert-deftest drover-schedule-test-one-at-a-time ()
  "Two prompts due for one conversation go one after the other."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (drover-schedule-test-buffer)
    (let ((first (drover-schedule-test-job)) (second (drover-schedule-test-job)))
      (drover-schedule--run (plist-get first :id))
      (drover-schedule--run (plist-get second :id))
      (should (eq (plist-get first :status) 'waiting))
      (should (eq (plist-get second :status) 'pending))
      (should (gethash (plist-get second :id) (drover-schedule--timers))))))

(ert-deftest drover-schedule-test-cancel-late-callback ()
  "A prompt cancelled while delivered ignores what comes back late."
  (drover-schedule-test-state
    (let* ((job (drover-schedule-test-job)) (run (list :deadline (+ (float-time) 100))))
      (puthash (plist-get job :id) run (drover-schedule--active))
      (drover-schedule--status job 'waiting)
      (drover-schedule-cancel job)
      (drover-schedule--finish job run 'sent)
      (should (eq (plist-get job :status) 'cancelled))
      (should (= 0 (hash-table-count (drover-schedule--active))))
      (should (= 0 (hash-table-count (drover-schedule--timers))))
      (should-error (drover-schedule-cancel job) :type 'user-error))))

(ert-deftest drover-schedule-test-timeout ()
  "A session that never opens fails the prompt at the deadline, the prompt kept."
  (drover-schedule-test-state
    (let* ((job (drover-schedule-test-job)) (run (list :deadline (- (float-time) 1))))
      (puthash (plist-get job :id) run (drover-schedule--active))
      (drover-schedule--status job 'waiting)
      (drover-schedule--poll job run)
      (should (eq (plist-get job :status) 'failed))
      (should (equal (plist-get job :error) "Timed out: the session never opened"))
      (should (equal (plist-get job :prompt) "check"))
      (should (= 0 (hash-table-count (drover-schedule--active)))))))

(ert-deftest drover-schedule-test-timeout-reason ()
  "A deadline passed tells what the session's buffer was still waiting for."
  (drover-schedule-test-state
    (drover-schedule-test-session)
    (let* ((buffer (drover-schedule-test-buffer))
           (drover-schedule-test-blocker "the cursor is not in an empty input box")
           (job (drover-schedule-test-job))
           (run (list :buffer buffer :deadline (- (float-time) 1))))
      (puthash (plist-get job :id) run (drover-schedule--active))
      (drover-schedule--status job 'waiting)
      (drover-schedule--poll job run)
      (should (eq (plist-get job :status) 'failed))
      (should (equal (plist-get job :error) "Timed out: the cursor is not in an empty input box"))
      ;; A buffer showing another conversation is no session of the prompt.
      (should (equal (drover-schedule--blocker (plist-put (copy-sequence job) :link "agent-session:claude-code/x")
                                               buffer)
                     "the buffer shows another session")))))

;;;; Waiting for a turn to end

(defmacro drover-schedule-test-turns (statuses &rest body)
  "Run BODY with drover listing the fixture's session in the first of STATUSES.
STATUSES is a variable holding a list: its car is the status of the
session, nil for a session drover does not list.  Starting drover's
backends does nothing."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'drover-start) #'ignore)
             ((symbol-function 'drover-known-sessions)
              (lambda (&rest _)
                (and (car ,statuses)
                     (list (drover-test-session "fixture" :engine 'claude-code :engine-id "fixture-session"
                                                :status (car ,statuses)))))))
     ,@body))

(ert-deftest drover-schedule-test-prompt-turn-end ()
  "From drover's prompt, the session comes first, and its running turn is offered and waited for."
  (drover-schedule-test-state
    (let ((statuses (list 'working)) (asked nil))
      (drover-schedule-test-turns statuses
        (drover-schedule-test-prompt (drover-schedule-test-model 'claude-code)
          (setf (drover-session-status session) 'retrying)
          (cl-letf (((symbol-function 'drover-prompt-read-target)
                     (lambda (_prompt &rest _) (push 'session asked) session))
                    ((symbol-function 'drover-schedule-read-when)
                     (lambda (&optional turn)
                       (push (list 'when turn) asked)
                       (drover-schedule-test-chosen 'turn-end))))
            (drover-schedule-prompt))
          (should (memq #'drover-schedule--turn-check drover-changed-hook))
          ;; The session first, then the time, offering the end of its turn.
          (should (equal (reverse asked) '(session (when t))))
          (should-not (buffer-live-p prompt-buf))
          (let ((job (car (drover-schedule--jobs))))
            (should (drover-schedule--turn-waiting-p job))
            (should (equal (plist-get job :prompt) "Look:\n```sh\nls\n```"))
            (should (drover-model-p (plist-get (plist-get job :options) :model)))
            (should (= 0 (hash-table-count (drover-schedule--timers))))
            ;; Working, then waiting for an answer, are the turn still.
            (drover-schedule--turn-check)
            (setcar statuses 'waiting)
            (drover-schedule--turn-check)
            (should (drover-schedule--turn-waiting-p job))
            (should (= 0 (hash-table-count (drover-schedule--timers))))
            ;; Done: the prompt gets a time, now, and is armed and saved.
            (setcar statuses 'done)
            (drover-schedule--turn-check)
            (should-not (drover-schedule--turn-waiting-p job))
            (should (<= (plist-get job :due) (float-time)))
            (should (gethash (plist-get job :id) (drover-schedule--timers)))
            (cl-letf (((symbol-function 'drover-schedule--turn-check) #'ignore))
              (drover-schedule-test-reload))
            (should (numberp (plist-get (car (drover-schedule--jobs)) :due)))
            (should (plist-get (car (drover-schedule--jobs)) :turn)))))))
  ;; An idle session is offered no turn's end.
  (drover-schedule-test-state
    (drover-schedule-test-prompt nil
      (cl-letf (((symbol-function 'drover-schedule-read-when)
                 (lambda (&optional turn) (should-not turn) (drover-schedule-test-chosen))))
        (drover-schedule-prompt))
      (should (numberp (plist-get (car (drover-schedule--jobs)) :due))))))

(ert-deftest drover-schedule-test-turn-end-on-restore ()
  "Restored, a prompt waiting for a turn goes to a session idle at once, and waits for one at work."
  (drover-schedule-test-state
    (let ((statuses (list 'working)))
      (drover-schedule-test-turns statuses
        (let ((job (drover-schedule-add-after-turn (drover-schedule-test-target) "next" "when turn ends")))
          (should (drover-schedule--turn-waiting-p job))
          (should (equal (plist-get job :instruction) "when turn ends"))
          (drover-schedule-test-reload)
          (should (drover-schedule--turn-waiting-p (car (drover-schedule--jobs))))
          (should (= 0 (hash-table-count (drover-schedule--timers))))
          (setcar statuses 'idle)
          (drover-schedule-test-reload)
          (should-not (drover-schedule--turn-waiting-p (car (drover-schedule--jobs))))
          (should (gethash (plist-get (car (drover-schedule--jobs)) :id) (drover-schedule--timers))))))
    ;; Not listed: a terminal's turn ended with it; a service's, once the
    ;; backend lists all it runs; a backend not loaded tells nothing.
    (let ((statuses (list nil)) (ready nil))
      (drover-test-register-simple 'terminal nil :runs-in-buffer t)
      (drover-test-register-simple 'service nil :ready (lambda () ready))
      (drover-schedule-test-turns statuses
        (let ((terminal (drover-schedule-add-after-turn (plist-put (drover-schedule-test-target) :backend 'terminal)
                                                        "a"))
              (service (drover-schedule-add-after-turn (plist-put (drover-schedule-test-target) :backend 'service)
                                                       "b"))
              (unloaded (drover-schedule-add-after-turn (plist-put (drover-schedule-test-target) :backend 'unloaded)
                                                        "c")))
          (should-not (drover-schedule--turn-waiting-p terminal))
          (should (drover-schedule--turn-waiting-p service))
          (setq ready t)
          (drover-schedule--turn-check)
          (should-not (drover-schedule--turn-waiting-p service))
          (should (drover-schedule--turn-waiting-p unloaded)))))))

;;;; The list

(ert-deftest drover-schedule-test-queue-view ()
  "The list shows when each prompt goes; r sends a waiting one now, k cancels, RET shows it."
  (let ((refresh (symbol-function 'drover-schedule--refresh)))
    (drover-schedule-test-state
      (let ((statuses (list 'working)))
        (drover-schedule-test-turns statuses
          (let ((job (drover-schedule-add-after-turn (drover-schedule-test-target) "later"))
                (other (drover-schedule-add-after-turn (drover-schedule-test-target) "never")))
            (should (drover-schedule-job-valid-p job))
            (should-not (drover-schedule-job-valid-p (plist-put (copy-sequence job) :turn "yes")))
            (unwind-protect
                (cl-letf (((symbol-function 'drover-schedule--refresh) refresh)
                          ((symbol-function 'pop-to-buffer) #'ignore))
                  (drover-schedule-list)
                  (with-current-buffer drover-schedule--list-buffer
                    (should (eq major-mode 'drover-schedule-list-mode))
                    (should (eq (keymap-lookup drover-schedule-list-mode-map "k") #'drover-schedule-cancel))
                    (should (eq (keymap-lookup drover-schedule-list-mode-map "r") #'drover-schedule-retry))
                    (should (eq (keymap-lookup drover-schedule-list-mode-map "RET") #'drover-schedule-inspect))
                    (should (string-match-p "after the turn of Scheduler fixture" (buffer-string)))
                    (cl-letf (((symbol-function 'drover-schedule--at-point) (lambda () job)))
                      (drover-schedule-inspect)
                      (should (string-match-p "Scheduled: after the turn of Scheduler fixture"
                                              (with-current-buffer "*drover scheduled prompt*" (buffer-string))))
                      (cl-letf (((symbol-function 'y-or-n-p)
                                 (lambda (prompt)
                                   (should (equal prompt "Send now instead of after the turn of Scheduler fixture? "))
                                   t)))
                        (drover-schedule-retry)))
                    (should (numberp (plist-get job :due)))
                    (should (gethash (plist-get job :id) (drover-schedule--timers)))
                    (drover-schedule-cancel other)
                    (should (eq (plist-get other :status) 'cancelled))
                    (should-not (drover-schedule--turn-waiting-p other))))
              (dolist (name (list drover-schedule--list-buffer "*drover scheduled prompt*"))
                (when (get-buffer name) (kill-buffer name))))))))))

(ert-deftest drover-schedule-test-dispatch ()
  "The menu lists scheduled prompts with #, which loads nothing until used."
  (should (eq (plist-get (cdr (transient-get-suffix 'drover-dispatch "#")) :command) #'drover-schedule-list))
  (should (commandp #'drover-schedule-list)))

(ert-deftest drover-schedule-test-prints-in-proportion ()
  "The queue prints in proportion to its prompts, deliveries in progress included.
Emacs prints a value whole in an error's data, or as `format' does: a
prompt or a delivery leading back to the queue would print it all again."
  (drover-schedule-test-state
    (let ((drover-schedule-test-blocker "the input holds a draft")
          (jobs nil))
      (dotimes (i 100)
        (let ((id (format "s%d" i)))
          (push (drover-test-session id :engine 'claude-code :engine-id (concat id "-session"))
                drover-test-sessions)
          (drover-schedule-test-buffer id)
          (push (drover-schedule-add (drover-schedule-test-target (concat id "-session"))
                                     (time-add (current-time) 3600) (format "check %d" i))
                jobs)
          (drover-schedule-add (drover-schedule-test-target (concat id "-session"))
                               (time-add (current-time) 7200) (format "then %d" i))))
      ;; Half wait for their sessions to take a prompt.
      (dolist (job (seq-take jobs 50))
        (drover-schedule--run (plist-get job :id))
        (should (eq (plist-get job :status) 'waiting)))
      (let ((job (car jobs)))
        (drover-test-prints-in-proportion
         200 (list (cons "a prompt" job)
                   (cons "a delivery" (gethash (plist-get job :id) (drover-schedule--active)))))))))

(provide 'drover-schedule-test)
;;; drover-schedule-test.el ends here
