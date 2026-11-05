;;; drover-goal-test.el --- Tests of goals: checks run until they pass  -*- lexical-binding: t; -*-

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

;; Goals over a repository made here: its check, a script whose outcome
;; a file of the worktree decides, runs for real through
;; `drover-command-run'; the session is the fake backend's, its turns
;; played by changing its record, its follow-ups taken by a :submit
;; recording them, through the queue of scheduled prompts.  The model,
;; the usage and the harness's own goal are stubs.  Nothing is shown.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-goal)
(require 'drover-repo)
(require 'json)

(defvar drover-goal-test-submitted nil
  "Prompts the fake backend took, newest first, as (ID TEXT THEN ELSE).")

(defvar drover-goal-test-blocker nil
  "What the fake backend's :blocker says, nil when its buffer takes a prompt.")

(defvar drover-goal-test-billed nil
  "Dollars billed for the fake session, or nil when unknown.")

(defvar drover-goal-test-harness nil
  "The fake harness's goal: a plist of :status :rounds :reason, or nil.")

(defvar drover-goal-test-harness-calls nil
  "What the fake harness's goal was asked, newest first, as (ACTION TEXT).")

(defun drover-goal-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most."
  (let ((deadline (+ (float-time) (or seconds 20))) value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun drover-goal-test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY; return its output, or fail."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (zerop (apply #'call-process "git" nil t nil arguments))
        (error "git %s: %s" (string-join arguments " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defconst drover-goal-test-check
  "if [ -f ../outside ]; then state=$(cat ../outside); else state=$(cat state); fi
if [ \"$state\" = pass ]; then echo all good; exit 0; fi
echo running the suite
echo \"FAIL test_login: expected 200, got $state\"
echo done
exit 1
"
  "The check of the fixture: it passes once the file state says so.")

(defun drover-goal-test-session (&rest properties)
  "Serve the fixture's session with PROPERTIES over its own, and return it."
  (let ((session (apply #'drover-test-session "s1"
                        (append properties (list :engine 'claude-code :engine-id "goal-session"
                                                 :directory (bound-and-true-p drover-goal-test-repo))))))
    (setq drover-test-sessions (list session))
    session))

(defvar drover-goal-test-repo nil
  "The fixture's repository, while a test runs.")

(defvar drover-goal-test-buffer nil
  "The buffer of the fixture's session, with a process.")

(defmacro drover-goal-test-with (&rest body)
  "Run BODY over a fresh repository, `drover-goal-test-repo', and a session.
The fake backend serves the session, in a buffer taking prompts; goals,
the queue and the state are fresh, nothing shown, no mode turned on."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (let* ((root (file-name-as-directory (file-truename (make-temp-file "drover-goal-test" t))))
              (drover-goal-test-repo (expand-file-name "repo/" root))
              (process-environment (append (list (concat "HOME=" root) "GIT_CONFIG_NOSYSTEM=1"
                                                 "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                                                 "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=test@example.com")
                                           process-environment))
              (drover-goal--table (drover-goal--table-make :loaded t))
              (drover-goal-mode t)
              (drover-schedule--queue (drover-schedule--queue-make :loaded t))
              (drover-schedule--triggers nil)
              (drover-schedule-former-files nil)
              (drover-schedule-display-function #'ignore)
              (drover-schedule-changed-hook (list #'drover-goal--schedule-changed))
              (drover-goal-test-submitted nil)
              (drover-goal-test-blocker nil)
              (drover-goal-test-billed nil)
              (drover-goal-test-harness nil)
              (drover-goal-test-harness-calls nil)
              (drover-goal-ask-questions t)
              (drover-goal-iterations 5)
              (drover-goal-minutes 60)
              (drover-goal-budget 10.0)
              (drover-goal-test-buffer nil)
              (drover-repo--cache 'unread)
              (drover-repo-generate-function (lambda (_prompt _system callback)
                                               (funcall callback (list :failure "no model in tests")))))
         (make-directory drover-goal-test-repo t)
         (drover-goal-test-git drover-goal-test-repo "init" "-q" "-b" "main")
         (drover-test-write (expand-file-name "check.sh" drover-goal-test-repo) drover-goal-test-check)
         (drover-test-write (expand-file-name "state" drover-goal-test-repo) "500\n")
         (drover-test-write (expand-file-name "test/login_test.sh" drover-goal-test-repo) "exit 0\n")
         (drover-test-write (expand-file-name "src/app.txt" drover-goal-test-repo) "app\n")
         (drover-goal-test-git drover-goal-test-repo "add" "-A")
         (drover-goal-test-git drover-goal-test-repo "commit" "-q" "-m" "First")
         (setq drover-goal-test-buffer (generate-new-buffer " *goal session*"))
         (with-current-buffer drover-goal-test-buffer
           (make-pipe-process :name "goal-session" :buffer drover-goal-test-buffer :noquery t)
           (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s1"))))
         (setf (alist-get 'fake drover--backends)
               (append (list :buffer (lambda (_session) (and (buffer-live-p drover-goal-test-buffer)
                                                             drover-goal-test-buffer))
                             :blocker (lambda (_session) drover-goal-test-blocker)
                             :submit (lambda (session text then else)
                                       (push (list (drover-session-id session) text then else)
                                             drover-goal-test-submitted))
                             :goal-command (lambda (_session action text then _else)
                                             (push (list action text) drover-goal-test-harness-calls)
                                             (pcase action
                                               ('set (setq drover-goal-test-harness (list :status 'active :rounds 0)))
                                               ('resume (setf (plist-get drover-goal-test-harness :status) 'active))
                                               ('pause (setf (plist-get drover-goal-test-harness :status) 'paused))
                                               ('clear (setq drover-goal-test-harness nil)))
                                             (funcall then))
                             :goal-state (lambda (_session callback)
                                           (funcall callback (or drover-goal-test-harness (list :status nil)))))
                       (alist-get 'fake drover--backends)))
         (drover-goal-test-session :status 'idle :since 1.0 :updated 1.0)
         (unwind-protect
             (cl-letf (((symbol-function 'drover-known-sessions) (lambda (&rest _) drover-test-sessions))
                       ((symbol-function 'drover-archived-live-sessions) #'ignore)
                       ((symbol-function 'drover-goal--billed) (lambda (_session) drover-goal-test-billed)))
               ,@body)
           (drover-test-wait-commands 10)
           (when (drover-schedule--queue-p drover-schedule--queue)
             (maphash (lambda (_ timer) (cancel-timer timer)) (drover-schedule--queue-timers drover-schedule--queue))
             (maphash (lambda (_ run) (when-let* ((timer (plist-get run :timer))) (cancel-timer timer)))
                      (drover-schedule--queue-active drover-schedule--queue)))
           (maphash (lambda (_ run) (when (timerp (plist-get run :timer)) (cancel-timer (plist-get run :timer))))
                    (drover-goal--table-runs drover-goal--table))
           (when (buffer-live-p drover-goal-test-buffer) (kill-buffer drover-goal-test-buffer))
           (delete-directory root t))))))

(defun drover-goal-test-state (state)
  "Write STATE in the fixture's file deciding its check."
  (drover-test-write (expand-file-name "state" drover-goal-test-repo) (concat state "\n")))

(defun drover-goal-test-start (&rest properties)
  "Start a goal of the fixture's check on its session, with PROPERTIES."
  (apply #'drover-goal-start (car drover-test-sessions)
         (append properties (list :commands '("sh check.sh") :sources '("test")))))

(defun drover-goal-test-take ()
  "Wait for a follow-up, take it as the front end does, and return its text."
  (should (drover-goal-test-wait (lambda () drover-goal-test-submitted)))
  (pcase-let ((`(,_id ,text ,then ,_else) (pop drover-goal-test-submitted)))
    (funcall then)
    text))

(defun drover-goal-test-turn (goal &optional status)
  "Play a turn of the fixture's session ending in STATUS, idle by default, for GOAL."
  (let ((now (+ (float-time) 1)))
    (drover-goal-test-session :status 'working :since now :updated now)
    (drover-goal--turn-check)
    (drover-goal-test-session :status (or status 'idle) :since (+ now 1) :updated (+ now 1))
    (drover-goal--turn-check)
    goal))

(defun drover-goal-test-settled (goal &rest states)
  "Wait for GOAL to reach one of STATES, and return its state."
  (drover-goal-test-wait (lambda () (memq (plist-get goal :state) states)))
  (plist-get goal :state))

;;;; The loop

(ert-deftest drover-goal-test-failure-follow-up-then-met ()
  "A failing check sends the failing lines and the output's end; passing then meets the goal."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start)))
      ;; Idle, no prompt: the checks run now, and fail.
      (should (eq (drover-goal-test-settled goal 'sending 'paused 'given-up) 'sending))
      (let ((text (drover-goal-test-take)))
        (should (string-match-p "Goal check failed (follow-up 1 of 5): `sh check.sh` exited 1" text))
        (should (string-match-p "Failing lines:\n\n```\nFAIL test_login: expected 200, got 500\n```" text))
        (should (string-match-p "Last lines of its output:" text))
        (should (string-match-p "running the suite" text))
        (should (string-match-p "Do not weaken, skip or delete tests" text))
        (should (<= (length text) drover-goal-feedback-limit)))
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting))
      (should (= (plist-get goal :iteration) 1))
      ;; The follow-up left the queue once sent.
      (should-not (drover-schedule--jobs))
      (should (equal (drover-goal--list-text (car drover-test-sessions) 'value) '(waiting 1 5 nil)))
      (drover-goal-test-state "pass")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'met 'sending 'given-up) 'met))
      (should (string-match-p "sh check.sh pass" (plist-get goal :reason)))
      (should (string-match-p "✓ 1/5" (drover-goal--list-text (car drover-test-sessions) 'column)))
      (drover-goal--mode-line-update)
      (should (string-match-p "✓1" drover-goal-mode-line))
      (with-temp-buffer
        (insert-file-contents (drover-goal-log-file goal))
        (should (string-match-p "FAIL test_login" (buffer-string)))
        (should (string-match-p "Goal met" (buffer-string)))))))

(ert-deftest drover-goal-test-waits-for-the-turn ()
  "A session at work: nothing runs until its turn ends; a pending question is part of the turn."
  (drover-goal-test-with
    (drover-goal-test-session :status 'working :since (float-time) :updated (float-time))
    (let ((goal (drover-goal-test-start)))
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting))
      (drover-goal-test-session :status 'waiting :since (+ 1 (float-time)) :updated (+ 1 (float-time)))
      (drover-goal--turn-check)
      (should (eq (plist-get goal :state) 'waiting))
      (drover-goal-test-session :status 'done :since (+ 2 (float-time)) :updated (+ 2 (float-time)))
      (drover-goal--turn-check)
      (should (memq (plist-get goal :state) '(checking sending)))
      (drover-goal-test-take))))

(ert-deftest drover-goal-test-draft-never-overwritten ()
  "A follow-up waits while the session's buffer holds a draft, then goes."
  (drover-goal-test-with
    (setq drover-goal-test-blocker "the input holds a draft")
    (let ((goal (drover-goal-test-start)))
      (should (eq (drover-goal-test-settled goal 'sending) 'sending))
      (drover-goal-test-wait (lambda () nil) 1)
      (should-not drover-goal-test-submitted)
      (setq drover-goal-test-blocker nil)
      (drover-goal-test-take)
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting)))))

(ert-deftest drover-goal-test-iteration-and-time-limits ()
  "Past its follow-ups, or its minutes, a goal gives up, telling what still fails."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :iterations 1)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      (drover-goal-test-state "teapot")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'given-up 'sending) 'given-up))
      (should (string-match-p "1 follow-ups sent, its limit; still failing" (plist-get goal :reason)))
      (should (string-match-p "✗ 1/1" (drover-goal--list-text (car drover-test-sessions) 'column)))))
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :minutes 0)))
      (should (eq (drover-goal-test-settled goal 'given-up 'sending) 'given-up))
      (should (string-match-p "0 minutes passed" (plist-get goal :reason))))))

(ert-deftest drover-goal-test-budget ()
  "Billed past its budget since it began, a goal gives up; what was billed before does not count."
  (drover-goal-test-with
    (setq drover-goal-test-billed 40.0)
    (let ((goal (drover-goal-test-start :budget 5.0)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      (setq drover-goal-test-billed 46.5)
      (drover-goal-test-state "teapot")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'given-up 'sending) 'given-up))
      (should (string-match-p "\\$6.50 billed, its limit \\$5.00" (plist-get goal :reason))))))

(ert-deftest drover-goal-test-no-progress ()
  "The same failure twice, or a turn changing nothing, give up."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      ;; The worktree changes, the failure does not.
      (drover-test-write (expand-file-name "src/app.txt" drover-goal-test-repo) "app, edited\n")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'given-up 'sending) 'given-up))
      (should (string-match-p "the same failure twice" (plist-get goal :reason)))))
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      ;; Another failure each turn, decided outside the worktree, which the turns leave alone.
      (drover-test-write (expand-file-name "../outside" drover-goal-test-repo) "teapot\n")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'given-up 'sending) 'given-up))
      (should (string-match-p "changed nothing in the worktree" (plist-get goal :reason))))))

(ert-deftest drover-goal-test-question-pauses ()
  "A reply ending in a question pauses the goal rather than send failures; a model tells of one before."
  (drover-goal-test-with
    (setq drover-test-recaps '(("s1" :reply "I could fix it two ways.\nShould I change the API?")))
    (let ((goal (drover-goal-test-start)))
      (should (eq (drover-goal-test-settled goal 'paused 'sending) 'paused))
      (should (string-match-p "the agent asks: Should I change the API\\?" (plist-get goal :reason)))
      (should-not drover-goal-test-submitted)))
  (drover-goal-test-with
    (setq drover-test-recaps '(("s1" :reply "Is the old endpoint still used?\nI stopped here.")))
    (let* ((asked nil)
           (drover-repo-generate-function (lambda (_prompt _system callback)
                                            (setq asked t)
                                            (funcall callback (list :text "Yes")))))
      (let ((goal (drover-goal-test-start)))
        (should (eq (drover-goal-test-settled goal 'paused 'sending) 'paused))
        (should asked))))
  ;; A question mark long before the end asks nothing.
  (drover-goal-test-with
    (setq drover-test-recaps '(("s1" :reply "Why?\nBecause.\nDone.\nTests run.\nAll fine.")))
    (let ((goal (drover-goal-test-start)))
      (should (eq (drover-goal-test-settled goal 'paused 'sending) 'sending)))))

(ert-deftest drover-goal-test-turn-failed-pauses ()
  "A turn ending in failure pauses the goal, telling why."
  (drover-goal-test-with
    (drover-goal-test-session :status 'working :since (float-time) :updated (float-time) :detail "rate limited")
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-settled goal 'waiting)
      (drover-goal-test-session :status 'failed :since (+ 1 (float-time)) :updated (+ 1 (float-time))
                                :detail "rate limited")
      (drover-goal--turn-check)
      (should (eq (plist-get goal :state) 'paused))
      (should (string-match-p "the turn failed: rate limited" (plist-get goal :reason))))))

(ert-deftest drover-goal-test-tampering-flagged ()
  "Editing a test pauses the goal, even when the checks pass; resuming accepts the files."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      (drover-test-write (expand-file-name "test/login_test.sh" drover-goal-test-repo) "exit 0 # skipped\n")
      ;; A new test is no tampering; a new CI file is.
      (drover-test-write (expand-file-name "test/new_test.sh" drover-goal-test-repo) "exit 0\n")
      (drover-goal-test-state "pass")
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'paused 'met) 'paused))
      (should (string-match-p "protected files changed though the checks pass: M test/login_test.sh"
                              (plist-get goal :reason)))
      (should (equal (plist-get goal :flagged) '("M test/login_test.sh")))
      (drover-goal-resume goal)
      (should (eq (drover-goal-test-settled goal 'met 'paused) 'met))))
  (should (equal (drover-goal--flagged '("A\t.github/workflows/ci.yml" "A\tsrc/x_test.py" "D\tpackage-lock.json"
                                         "M\tsrc/app.ts" "M\tsrc/__snapshots__/a.snap"))
                 '("A .github/workflows/ci.yml" "D package-lock.json" "M src/__snapshots__/a.snap"))))

(ert-deftest drover-goal-test-baseline ()
  "The baseline refuses checks that cannot run; failures there already go with the goal's statement."
  (drover-goal-test-with
    (let ((goal (drover-goal-start (car drover-test-sessions) :commands '("no-such-command-here --check")
                                   :baseline t)))
      (should (eq (drover-goal-test-settled goal 'stopped 'sending) 'stopped))
      (should (string-match-p "cannot run here" (plist-get goal :reason)))
      (should-not drover-goal-test-submitted)))
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :baseline t :prompt "Fix the login.")))
      (should (eq (drover-goal-test-settled goal 'sending) 'sending))
      (let ((text (drover-goal-test-take)))
        (should (string-prefix-p "Fix the login.\n\nDrover keeps this session under a goal" text))
        (should (string-match-p "- `sh check.sh`" text))
        (should (string-match-p "Before your changes, the checks failed already: `sh check.sh` exited 1" text))
        (should (string-match-p "FAIL test_login" text)))
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting)))))

(ert-deftest drover-goal-test-setup ()
  "The setup runs once first; failing, it stops the goal."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :setup '("touch set-up"))))
      (drover-goal-test-take)
      (should (file-exists-p (expand-file-name "set-up" drover-goal-test-repo)))
      (should (eq (plist-get goal :state) 'waiting))))
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :setup '("exit 3"))))
      (should (eq (drover-goal-test-settled goal 'stopped) 'stopped))
      (should (string-match-p "its setup failed" (plist-get goal :reason))))))

(ert-deftest drover-goal-test-restart-pauses ()
  "Goals that ran come back paused once read again; one paused resumes."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-take)
      (drover-goal-test-settled goal 'waiting)
      (let ((drover-goal--table (drover-goal--table-make)))
        (let ((read (car (drover-goal--goals))))
          (should (equal (plist-get read :id) (plist-get goal :id)))
          (should (eq (plist-get read :state) 'paused))
          (should (string-match-p "Emacs restarted" (plist-get read :reason)))
          (should (= (plist-get read :iteration) 1))
          (drover-goal-test-state "pass")
          (drover-goal-resume read)
          (should (eq (drover-goal-test-settled read 'met) 'met)))))))

(ert-deftest drover-goal-test-unreadable-file-kept ()
  "A file of goals that does not read is never written over, and no goal is set."
  (drover-goal-test-with
    (drover-test-write (drover-goal-file) "(:version 1 :goals (oops")
    (let ((drover-goal--table (drover-goal--table-make)))
      (should-error (drover-goal-test-start) :type 'user-error)
      (with-temp-buffer
        (insert-file-contents (drover-goal-file))
        (should (equal (buffer-string) "(:version 1 :goals (oops"))))))

(ert-deftest drover-goal-test-pause-stop ()
  "Paused, a goal's follow-up does not go; stopped, it is over."
  (drover-goal-test-with
    (setq drover-goal-test-blocker "draft")
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-settled goal 'sending)
      (drover-goal-pause goal)
      (should (eq (plist-get goal :state) 'paused))
      (should-not (plist-get goal :delivery))
      (setq drover-goal-test-blocker nil)
      (drover-goal-test-wait (lambda () nil) 1)
      (should-not drover-goal-test-submitted)
      (drover-goal-stop goal)
      (should (eq (plist-get goal :state) 'stopped))
      (should-error (drover-goal-resume goal) :type 'user-error))))

(ert-deftest drover-goal-test-remote-refused ()
  "A session of another host gets no goal here."
  (let ((session (drover-test-session "r1" :host "core" :engine 'claude-code :engine-id "x" :directory "/tmp/")))
    (should-error (drover-goal-start session :commands '("true")) :type 'user-error)))

;;;; Criterion goals

(ert-deftest drover-goal-test-criterion ()
  "The harness's own goal runs; done, drover's checks gate it, their failures set it again."
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :kind 'criterion :objective "The login works")))
      (drover-goal-test-settled goal 'waiting)
      (should (equal (car drover-goal-test-harness-calls) '(set "The login works")))
      (should (eq (plist-get goal :state) 'waiting))
      ;; Still active between rounds: waiting goes on.
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting))
      ;; Done in the harness's eyes, but the checks fail.
      (setf (plist-get drover-goal-test-harness :status) 'complete)
      (drover-goal-test-turn goal)
      (drover-goal-test-wait (lambda () (>= (length drover-goal-test-harness-calls) 2)) 5)
      (should (= (plist-get goal :iteration) 1))
      (let ((call (car drover-goal-test-harness-calls)))
        (should (eq (car call) 'set))
        (should (string-prefix-p "The login works\n\nDrover ran the checks once you judged it done" (cadr call)))
        (should (string-match-p "FAIL test_login" (cadr call))))
      (should (eq (drover-goal-test-settled goal 'waiting) 'waiting))
      (drover-goal-test-state "pass")
      (setf (plist-get drover-goal-test-harness :status) 'complete)
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'met) 'met))))
  ;; The harness pausing pauses the goal; its rounds and drover's budget are bounded.
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :kind 'criterion :objective "Done")))
      (drover-goal-test-settled goal 'waiting)
      (setq drover-goal-test-harness (list :status 'paused :reason "blocked on credentials"))
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'paused) 'paused))
      (should (string-match-p "the harness paused it: blocked on credentials" (plist-get goal :reason)))))
  (drover-goal-test-with
    (let ((goal (drover-goal-test-start :kind 'criterion :objective "Done" :rounds 3)))
      (drover-goal-test-settled goal 'waiting)
      (setf (plist-get drover-goal-test-harness :rounds) 3)
      (drover-goal-test-turn goal)
      (should (eq (drover-goal-test-settled goal 'given-up) 'given-up))
      (should (eq (car (car drover-goal-test-harness-calls)) 'pause))))
  (drover-goal-test-with
    (setq drover-goal-test-billed 1.0)
    (let ((goal (drover-goal-test-start :kind 'criterion :objective "Done" :budget 2.0)))
      (drover-goal-test-settled goal 'waiting)
      (drover-goal-test-session :status 'working :since (+ 1 (float-time)) :updated (+ 1 (float-time)))
      (setq drover-goal-test-billed 3.5)
      (drover-goal--turn-check)
      (should (eq (plist-get goal :state) 'given-up))
      (should (eq (car (car drover-goal-test-harness-calls)) 'pause))
      (should (assq :interrupt drover-test-calls)))))

(ert-deftest drover-goal-test-criterion-unsupported ()
  "A harness without a goal of its own takes no criterion goal."
  (drover-goal-test-with
    (setf (alist-get 'fake drover--backends)
          (let ((properties (copy-sequence (alist-get 'fake drover--backends))))
            (cl-remf properties :goal-command)
            properties))
    (should-error (drover-goal-test-start :kind 'criterion :objective "Done") :type 'user-error)))

;;;; Checks of repositories

(defconst drover-goal-test-agents
  "# Agents

## Done

A change is finished when typecheck, test and format:check all pass.

Run `make lint` before you push.

Install with `npm ci` first.

Finish: merge your branch into local main, rerun the checks, and push main. Never open a pull request.
Worktrees go beside the checkout, as ../repo-<topic>.
"
  "An AGENTS.md of a fixture repository.")

(ert-deftest drover-repo-test-validate ()
  "What the model reports stays only when the text quotes it and names its commands."
  (let* ((documents (list (cons "AGENTS.md" drover-goal-test-agents)))
         (hints (list :scripts '("typecheck" "test" "format:check" "build") :manager 'npm :make '("lint")))
         (answer (drover-repo--parse
                  (concat "Here: ```json\n"
                          (json-encode
                           '((checks . [((command . "npm run typecheck")
                                         (evidence . "A change is finished when typecheck, test and format:check all pass."))
                                        ((command . "npm test")
                                         (evidence . "A change is finished when typecheck, test and format:check all pass."))
                                        ((command . "npm run build")
                                         (evidence . "A change is finished when typecheck, test and format:check all pass."))
                                        ((command . "make lint") (evidence . "Run `make lint` before you push."))
                                        ((command . "rm -rf /") (evidence . "Invented, quoted nowhere."))])
                             (setup . [((command . "npm ci") (evidence . "Install with npm ci first."))])
                             (finish . ((method . "merge-push")
                                        (evidence . "Finish: merge your branch into local main, rerun the checks, and push main.")))
                             (worktree . ((placement . "sibling")
                                          (evidence . "Worktrees go beside the checkout, as ../repo-<topic>.")))))
                          "\n```")))
         (facts (drover-repo-validate answer documents hints)))
    (should (equal (mapcar (lambda (item) (plist-get item :command)) (plist-get facts :checks))
                   '("npm run typecheck" "npm test" "make lint")))
    (should (string-prefix-p "AGENTS.md: “A change is finished" (plist-get (car (plist-get facts :checks)) :source)))
    (should (equal (plist-get (car (plist-get facts :setup)) :command) "npm ci"))
    (should (eq (plist-get (plist-get facts :finish) :method) 'merge-push))
    (should (eq (plist-get (plist-get facts :placement) :place) 'sibling))
    (should (= (plist-get facts :dropped) 2))
    ;; A method or a quote not in the text is dropped.
    (let ((facts (drover-repo-validate '((finish . ((method . "pr") (evidence . "Open a pull request."))))
                                       documents hints)))
      (should-not (plist-get facts :finish)))))

(ert-deftest drover-repo-test-facts-cached-and-conventional ()
  "Facts come from the model once per content of the instructions; without them, from the conventional files."
  (drover-goal-test-with
    (let ((asked 0)
          (repo drover-goal-test-repo))
      (drover-test-write (expand-file-name "package.json" repo)
                         "{\"scripts\": {\"test\": \"vitest run\", \"lint\": \"eslint .\"}}")
      (drover-test-write (expand-file-name "package-lock.json" repo) "{}")
      (let ((facts (drover-repo-facts-now repo)))
        (should (equal (mapcar (lambda (item) (plist-get item :command)) (plist-get facts :checks))
                       '("npm run lint" "npm test")))
        (should (plist-get facts :checks-conventional))
        (should (equal (plist-get (car (plist-get facts :setup)) :command) "npm ci"))
        (should (equal (plist-get (car (plist-get facts :setup)) :unless) "node_modules")))
      (drover-test-write (expand-file-name "AGENTS.md" repo) "Check with `sh check.sh` before finishing.\n")
      (let ((drover-repo-generate-function
             (lambda (_prompt _system callback)
               (cl-incf asked)
               (funcall callback (list :text "{\"checks\": [{\"command\": \"sh check.sh\", \"evidence\": \"Check with sh check.sh before finishing.\"}], \"setup\": [], \"finish\": {\"method\": \"\", \"evidence\": \"\"}, \"worktree\": {\"placement\": \"\", \"evidence\": \"\"}}")))))
        (let ((facts (drover-repo-facts-now repo)))
          (should (equal (mapcar (lambda (item) (plist-get item :command)) (plist-get facts :checks))
                         '("sh check.sh")))
          (should-not (plist-get facts :checks-conventional))
          ;; No setup named: the lockfile's.
          (should (plist-get facts :setup-conventional)))
        (drover-repo-facts-now repo)
        (should (= asked 1))
        ;; Read from the cache's file by another Emacs.
        (let ((drover-repo--cache 'unread))
          (drover-repo-facts-now repo)
          (should (= asked 1)))
        (drover-test-write (expand-file-name "AGENTS.md" repo) "Check with `sh check.sh` before finishing, always.\n")
        (drover-repo-facts-now repo)
        (should (= asked 2))))))

(ert-deftest drover-repo-test-model-failure ()
  "A model that cannot be asked leaves the conventional files, said, and nothing cached."
  (drover-goal-test-with
    (drover-test-write (expand-file-name "AGENTS.md" drover-goal-test-repo) "Run `make test`.\n")
    (drover-test-write (expand-file-name "Makefile" drover-goal-test-repo) "test:\n\tsh check.sh\n")
    (let ((facts (drover-repo-facts-now drover-goal-test-repo)))
      (should (string-match-p "the model could not be asked: no model in tests" (plist-get facts :note)))
      (should (equal (plist-get (car (plist-get facts :checks)) :command) "make test"))
      (should-not (drover-repo--entries)))))

(ert-deftest drover-repo-test-split-commands ()
  "A heading's commands are joined by &&, quotes respected."
  (should (equal (drover-repo-split-commands "npm run typecheck && npm test&&echo 'a && b'")
                 '("npm run typecheck" "npm test" "echo 'a && b'")))
  (should (equal (drover-repo-join-commands '("a" (:command "b"))) "a && b"))
  (should-not (drover-repo-split-commands "  ")))

(ert-deftest drover-goal-test-heading-checks ()
  "DROVER_GOAL_CHECKS on the heading wins over the repository's."
  (should (equal (drover-goal--heading-checks '(:goal-checks "make check && make lint"))
                 '((:command "make check" :source "DROVER_GOAL_CHECKS on the task's heading")
                   (:command "make lint" :source "DROVER_GOAL_CHECKS on the task's heading")))))

(ert-deftest drover-goal-test-confirm ()
  "The checks show with their sources; edited, they are the user's."
  (let ((prompts nil)
        (answers (list "Edit the checks" "Without a baseline" "Start")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt &rest _) (push prompt prompts) (pop answers)))
              ((symbol-function 'read-string) (lambda (&rest _) "make check && make lint")))
      (let ((confirmed (drover-goal--confirm "Fix" "/tmp/repo/" (list (list :command "npm test" :source "AGENTS.md: “x”"))
                                             nil t nil)))
        (should (equal (mapcar (lambda (item) (plist-get item :command)) (car confirmed)) '("make check" "make lint")))
        (should-not (nth 2 confirmed))
        (should (string-match-p "npm test  — AGENTS.md: “x”" (car (last prompts))))))))

;;;; Commands' output

(ert-deftest drover-goal-test-command-tail ()
  "A command's output read from its end keeps its last lines, from a line's start."
  (let ((drover-command-output-limit 200)
        (result nil))
    (drover-command-run "tail" (list "sh" "-c" "i=0; while [ $i -lt 100 ]; do echo line $i; i=$((i+1)); done")
                        (lambda (value) (setq result value)) temporary-file-directory t)
    (drover-goal-test-wait (lambda () result) 10)
    (should (plist-get result :truncated))
    (should (string-suffix-p "line 99\n" (plist-get result :output)))
    (should (string-prefix-p "line " (plist-get result :output)))
    (should (<= (length (plist-get result :output)) 200))))

(ert-deftest drover-goal-test-feedback-bounded ()
  "Long outputs are cut: the failing lines first, then the end, within the limit."
  (let* ((output (concat (mapconcat (lambda (i) (format "FAIL case %d: %s" i (make-string 200 ?x)))
                                    (number-sequence 1 300) "\n")
                         "\n" (make-string 50000 ?y) "\nthe end"))
         (goal (list :iteration 0 :iterations 5 :commands '("npm test")))
         (text (drover-goal-feedback goal (list :failed (list :command "npm test" :exit 1 :seconds 3
                                                              :output output)))))
    (should (<= (length text) drover-goal-feedback-limit))
    (should (string-match-p "FAIL case 1:" text))
    (should (string-match-p "Fix the code so that the checks pass" text)))
  (should (equal (drover-goal--fence "a ``` b") "````")))

(ert-deftest drover-goal-test-prints-in-proportion ()
  "Goals and what runs for them print in proportion."
  (drover-goal-test-with
    (setq drover-goal-test-blocker "draft")
    (let ((goal (drover-goal-test-start)))
      (drover-goal-test-settled goal 'sending)
      (drover-test-prints-in-proportion 1 (list (cons "a goal" goal))))))

(provide 'drover-goal-test)
;;; drover-goal-test.el ends here
