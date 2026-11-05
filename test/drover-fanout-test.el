;;; drover-fanout-test.el --- Tests of fan-outs: attempts, comparison, keeping one  -*- lexical-binding: t; -*-

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

;; Fan-outs over a repository made here, a bare repository its origin,
;; whose AGENTS.md names a setup and a check, read by a stub of the
;; cheap model, and an Org file written here: attempts started from a
;; heading, each in its worktree on its branch, their setups one at a
;; time, their sessions those of the fake backend with the harness,
;; model, effort and agent chosen; the attempts chosen last offered
;; again; the comparison's facts, checks on demand and as turns end, the
;; difference of two attempts; keeping one, its branch renamed, the
;; others snapshotted, removed, their branches deleted and their
;; sessions archived; a ladder climbing as its goals give up; the list
;; grouping attempts under their task; what sessions of a model cost
;; before.  Never the user's Org files, repositories nor state.

;;; Code:

(require 'drover-test-helpers)
(require 'org-id)
(require 'drover-fanout)
(require 'drover-list)
(require 'drover-usage)

;; Never the user's ID locations: neither read, nor written as this Emacs ends.
(setq org-id-locations-file (make-temp-file "drover-fanout-test-ids")
      org-id-locations (make-hash-table :test #'equal))
(remove-hook 'kill-emacs-hook #'org-id-locations-save)

;;;; Helpers

(defun drover-fanout-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most."
  (let ((deadline (+ (float-time) (or seconds 60)))
        value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun drover-fanout-test-call (function)
  "Call FUNCTION with a callback, wait for it; return what the callback got."
  (let ((box (list 'pending)))
    (funcall function (lambda (value) (setcar box value)))
    (drover-fanout-test-wait (lambda () (not (eq (car box) 'pending))))
    (should-not (eq (car box) 'pending))
    (car box)))

(defun drover-fanout-test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY; return its output, trimmed, or fail."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (zerop (apply #'call-process "git" nil t nil arguments))
        (error "git %s: %s" (string-join arguments " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun drover-fanout-test-git-ok-p (directory &rest arguments)
  "Return non-nil when Git with ARGUMENTS succeeds in DIRECTORY."
  (let ((default-directory (file-name-as-directory directory)))
    (zerop (apply #'call-process "git" nil nil nil arguments))))

(defun drover-fanout-test-commit (directory file content message)
  "Write CONTENT to FILE in DIRECTORY and commit it with MESSAGE."
  (drover-test-write (expand-file-name file directory) content)
  (drover-fanout-test-git directory "add" "--" file)
  (drover-fanout-test-git directory "commit" "-q" "-m" message))

(defconst drover-fanout-test-agents
  "# Agents

When a task is done, merge its branch into main locally. Never push.

Run `sh setup.sh` in a new worktree.

Run `sh check.sh` before finishing.
"
  "The AGENTS.md of the fixture's repository.")

(defconst drover-fanout-test-setup
  "echo \"start $(basename \"$PWD\")\" >> \"$DROVER_ROOT_PATH/../setup.log\"
sleep 0.2
echo \"end $(basename \"$PWD\")\" >> \"$DROVER_ROOT_PATH/../setup.log\"
"
  "The setup of the fixture: it logs when it starts and ends, in the root.")

(defconst drover-fanout-test-check
  "if [ -f fixed ]; then echo all good; exit 0; fi
echo \"FAIL test_login: expected 200, got 500\"
exit 1
"
  "The check of the fixture: it passes once the file fixed exists.")

(defun drover-fanout-test-model (_prompt _system callback)
  "Answer as a model reading `drover-fanout-test-agents', for CALLBACK."
  (funcall callback
           (list :text (concat "{\"checks\": [{\"command\": \"sh check.sh\", \"evidence\": \"Run `sh check.sh` before finishing.\"}],"
                               " \"setup\": [{\"command\": \"sh setup.sh\", \"evidence\": \"Run `sh setup.sh` in a new worktree.\"}],"
                               " \"finish\": {\"method\": \"merge\", \"evidence\": \"When a task is done, merge its branch into main locally. Never push.\"},"
                               " \"worktree\": {\"placement\": \"\", \"evidence\": \"\"}}"))))

(defconst drover-fanout-test-id "5A0E0000-0000-4000-8000-0000000000F0"
  "The ID of the fixture's task.")

(defconst drover-fanout-test-org
  (concat "* Work\n:PROPERTIES:\n:DROVER_REPO: test/repo\n:END:\n** TODO Fix the login\n:PROPERTIES:\n:ID: "
          "5A0E0000-0000-4000-8000-0000000000F0" "\n:END:\nIt fails after the redirect.\n")
  "The Org file of the fixture's task.")

(defvar drover-fanout-test-engine-ids 0
  "How many sessions the fixture's backend started, which numbers their conversations.")

(defun drover-fanout-test-create (directory engine options then _else)
  "Start a fake session of ENGINE in DIRECTORY with OPTIONS, given to THEN.
Its conversation has an identifier, as goals need."
  (push (list :create directory engine options) drover-test-calls)
  (let ((session (drover-test-session (format "new-%d" (length drover-test-sessions))
                                      :directory directory :engine engine
                                      :engine-id (format "conversation-%d" (cl-incf drover-fanout-test-engine-ids))
                                      :status 'idle :updated (float-time) :since (float-time)
                                      :title (plist-get options :title))))
    (setq drover-test-sessions (append drover-test-sessions (list session)))
    (funcall then session)))

(defmacro drover-fanout-test-with (&rest body)
  "Run BODY over a repository, its bare origin, a task's heading and fresh state.
`root' holds them, deleted after; `origin' and `checkout' are the
repositories, `drover-task-repositories' naming the checkout
\"test/repo\", its worktrees under root's worktrees/; the Org file of
`drover-fanout-test-org' is visited, its buffer current.  The fake
backend starts sessions of claude-code, whose conversations have
identifiers; fan-outs, goals and the state are fresh, no mode on."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (let* ((default-directory (expand-file-name default-directory))
              (root (file-name-as-directory (file-truename (make-temp-file "drover-fanout-test" t))))
              (origin (expand-file-name "origin.git/" root))
              (checkout (expand-file-name "repo/" root))
              (file (expand-file-name "work.org" root))
              (process-environment (append (list (concat "HOME=" root) "GIT_CONFIG_NOSYSTEM=1"
                                                 "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                                                 "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=test@example.com")
                                           process-environment))
              (drover-host "test")
              (drover-task-path-aliases nil)
              (drover-task-worktree-root (expand-file-name "worktrees/" root))
              (drover-task-repositories `(("test/repo" :checkout ,checkout)))
              (drover-task--noted (make-hash-table :test #'equal))
              (drover-task--infos (make-hash-table :test #'equal))
              (drover-task--links (make-hash-table :test #'equal))
              (drover-task-mode t)
              (drover-session-task-function #'drover-task--session-task)
              (drover-task-attempt-label-function #'drover-fanout-attempt-label)
              (drover-task-harness '(:backend fake :engine claude-code))
              (drover-repo--cache 'unread)
              (drover-repo-generate-function #'drover-fanout-test-model)
              (drover-fanout--table (drover-fanout--table-make))
              (drover-fanout-mode t)
              (drover-goal--table (drover-goal--table-make :loaded t))
              (drover-goal-mode t)
              (drover-goal-ask-questions nil)
              (drover-schedule--queue (drover-schedule--queue-make :loaded t))
              (drover-schedule-changed-hook nil)
              (drover-fanout-test-engine-ids 0)
              (org-id-locations (make-hash-table :test #'equal))
              (org-id-track-globally t)
              (org-id-locations-file (expand-file-name "id-locations" root))
              (org-agenda-files nil)
              (org-log-into-drawer t)
              (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)"))))
         (setq drover-test-backend-engines '(claude-code))
         (setf (alist-get 'fake drover--backends)
               (plist-put (copy-sequence (alist-get 'fake drover--backends)) :create #'drover-fanout-test-create))
         (unwind-protect
             (cl-letf (((symbol-function 'drover-usage--ensure) #'ignore)
                       ((symbol-function 'drover-usage-session-totals) #'ignore))
               (drover-fanout-test-git root "init" "-q" "--bare" "-b" "main" origin)
               (drover-fanout-test-git root "clone" "-q" origin checkout)
               (drover-fanout-test-commit checkout "README" "hello\n" "First")
               (drover-fanout-test-commit checkout "AGENTS.md" drover-fanout-test-agents "Agents")
               (drover-fanout-test-commit checkout "setup.sh" drover-fanout-test-setup "Setup")
               (drover-fanout-test-commit checkout "check.sh" drover-fanout-test-check "Check")
               (drover-fanout-test-commit checkout "test/login_test.sh" "exit 0\n" "Test")
               (drover-fanout-test-git checkout "push" "-q" "origin" "main")
               (drover-fanout-test-git checkout "remote" "set-head" "origin" "main")
               (drover-test-write file drover-fanout-test-org)
               (with-current-buffer (find-file-noselect file)
                 (org-mode)
                 (goto-char (point-min))
                 (re-search-forward "Fix the login")
                 ,@body))
           (drover-test-wait-commands 10)
           (maphash (lambda (_ run) (when (timerp (plist-get run :timer)) (cancel-timer (plist-get run :timer))))
                    (drover-goal--table-runs drover-goal--table))
           (dolist (buffer (buffer-list))
             (when (or (eq (buffer-local-value 'major-mode buffer) 'drover-attempts-mode)
                       (equal (buffer-file-name buffer) file))
               (with-current-buffer buffer
                 (when (timerp (bound-and-true-p drover-attempts--timer))
                   (cancel-timer drover-attempts--timer))
                 (set-buffer-modified-p nil))
               (kill-buffer buffer)))
           (delete-directory root t))))))

(defun drover-fanout-test-choice (model variant &optional agent)
  "Return an attempt's choice of the fake harness, MODEL, VARIANT and AGENT."
  (list :backend 'fake :engine 'claude-code :label "Fake session"
        :model (and model (concat "prov/" model)) :model-name (and model (concat "Prov: " (capitalize model)))
        :variant variant :agent agent))

(defun drover-fanout-test-answers (answers)
  "Return a `completing-read' answering ANSWERS, an alist (REGEXP . ANSWER).
A prompt no regexp matches gets its default, else the first candidate."
  (lambda (prompt collection &optional _predicate _require _initial _history default &rest _)
    (let ((entry (seq-find (lambda (entry) (string-match-p (car entry) prompt)) answers)))
      (cond (entry (cdr entry))
            (default (if (consp default) (car default) default))
            (t (car (all-completions "" collection)))))))

(defun drover-fanout-test-marker ()
  "Return a marker at the fixture's task."
  (drover-task-org-marker drover-fanout-test-id))

(defun drover-fanout-test-property (name)
  "Return the property NAME of the fixture's task."
  (org-entry-get (drover-fanout-test-marker) name))

(defun drover-fanout-test-start (choices &rest answers)
  "Fan the fixture's task out to CHOICES, answering ANSWERS; return the fan-out.
CHOICES are offered as those chosen last, and started as they are."
  (drover-fanout--remember-choices 'parallel choices)
  (cl-letf (((symbol-function 'completing-read) (drover-fanout-test-answers answers))
            ((symbol-function 'pop-to-buffer) #'ignore))
    (drover-fanout))
  (drover-fanout-of-task drover-fanout-test-id))

(defun drover-fanout-test-settled (fanout count)
  "Wait for COUNT attempts of FANOUT to run, and return them."
  (should (drover-fanout-test-wait
           (lambda () (= count (seq-count (lambda (attempt) (eq (plist-get attempt :state) 'running))
                                          (plist-get fanout :attempts))))))
  (plist-get fanout :attempts))

;;;; Starting attempts

(ert-deftest drover-fanout-test-start-attempts ()
  "Attempts from the same base, each on its branch, set up one at a time, run as chosen."
  (drover-fanout-test-with
    (let* ((base (drover-fanout-test-git origin "rev-parse" "main"))
           (choices (list (drover-fanout-test-choice "alpha" "high" "build")
                          (drover-fanout-test-choice "beta" nil)
                          (drover-fanout-test-choice nil nil)))
           (fanout (drover-fanout-test-start choices '("\\`Goal: " . "No goal: the checks run as turns end"))))
      (should fanout)
      (should (eq (plist-get fanout :mode) 'parallel))
      (should-not (plist-get fanout :goal))
      (should (equal (plist-get fanout :checks) '("sh check.sh")))
      (let ((attempts (drover-fanout-test-settled fanout 3)))
        (should (equal (mapcar (lambda (attempt) (plist-get attempt :n)) attempts) '(1 2 3)))
        ;; Each in a worktree of its own, on the task's branch and its numbered ones, from one base.
        (pcase-dolist (`(,number ,slug) '((1 "fix-the-login") (2 "fix-the-login-2") (3 "fix-the-login-3")))
          (let ((attempt (drover-fanout--attempt fanout number))
                (worktree (expand-file-name (concat "worktrees/test-repo/" slug "/") root)))
            (should (equal (plist-get attempt :worktree) worktree))
            (should (equal (plist-get attempt :branch) slug))
            (should (equal (drover-fanout-test-git worktree "rev-parse" "--abbrev-ref" "HEAD") slug))
            (should (equal (drover-fanout-test-git worktree "rev-parse" "HEAD") base))
            (should (equal (drover-task-git-read-marker worktree) (list :id drover-fanout-test-id :setup 'ok)))))
        ;; Written on the heading: the first unsuffixed, the others by their number.
        (should (equal (drover-fanout-test-property "DROVER_BRANCH") "fix-the-login"))
        (should (equal (drover-fanout-test-property "DROVER_BRANCH_2") "fix-the-login-2"))
        (should (equal (drover-fanout-test-property "DROVER_WORKTREE_3")
                       (abbreviate-file-name (expand-file-name "worktrees/test-repo/fix-the-login-3/" root))))
        (should (equal (drover-fanout-test-property "DROVER_BASE_2") base))
        (should (string-match-p "Fan-out, side by side: attempt 1 Fake · Prov: Alpha · high · @build" (buffer-string)))
        (should (string-match-p "Attempt 2 (Fake · Prov: Beta): Worktree .*fix-the-login-2/ on branch fix-the-login-2"
                                (buffer-string)))
        (should (= 3 (length (plist-get (drover-task-read (drover-fanout-test-marker) drover-fanout-test-id)
                                        :attempts))))
        ;; The setups ran one after the other.
        (should (equal (split-string (with-temp-buffer
                                       (insert-file-contents (expand-file-name "setup.log" root))
                                       (buffer-string))
                                     "\n" t)
                       '("start fix-the-login" "end fix-the-login" "start fix-the-login-2" "end fix-the-login-2"
                         "start fix-the-login-3" "end fix-the-login-3")))
        ;; The sessions, unshown, of the harness, model, effort and agent chosen, the same prompt each.
        (let ((creates (reverse (seq-filter (lambda (call) (eq (car call) :create)) drover-test-calls)))
              (sends (reverse (seq-filter (lambda (call) (eq (car call) :send)) drover-test-calls))))
          (should (= 3 (length creates)))
          (pcase-let ((`(:create ,directory claude-code ,options) (car creates)))
            (should (equal directory (plist-get (drover-fanout--attempt fanout 1) :worktree)))
            (should (equal (drover-model-id (plist-get options :model)) "alpha"))
            (should (equal (drover-model-provider-id (plist-get options :model)) "prov"))
            (should (eq (plist-get options :variant) 'high))
            (should (equal (plist-get options :agent) "build"))
            (should (plist-get options :hidden)))
          (should-not (plist-get (nth 3 (nth 2 creates)) :model))
          (should (= 3 (length sends)))
          (dolist (send sends)
            (should (equal (drover-task-prompt-id (nth 2 send)) drover-fanout-test-id))
            (should (string-match-p "It fails after the redirect." (nth 2 send)))))
        ;; Its estimate shown for each, from no history.
        (should (equal (plist-get (drover-fanout--attempt fanout 2) :estimate) "no history"))
        ;; Kept in the state, the choices for next time.
        (setq drover-fanout--table (drover-fanout--table-make))
        (should (equal (mapcar (lambda (attempt) (plist-get attempt :state))
                               (plist-get (drover-fanout-of-task drover-fanout-test-id) :attempts))
                       '(running running running)))
        (should (equal (drover-fanout-last-choices 'parallel) choices))
        ;; Finishing waits for one to be kept.
        (should-error (drover-task-finish) :type 'user-error)))))

(ert-deftest drover-fanout-test-last-choice-prefilled ()
  "The picker holds the attempts chosen last, by mode, kept across sessions."
  (drover-test-with-state
    (let ((drover-fanout--table (drover-fanout--table-make))
          (choices (list (drover-fanout-test-choice "alpha" "high") (drover-fanout-test-choice "beta" "low"))))
      (should-not (drover-fanout-last-choices 'parallel))
      (drover-fanout--remember-choices 'ladder (list (drover-fanout-test-choice "gamma" nil)))
      (drover-fanout--remember-choices 'parallel choices)
      (setq drover-fanout--table (drover-fanout--table-make))
      (should (equal (drover-fanout-last-choices 'parallel) choices))
      (should (eq (drover-fanout-last-mode) 'parallel))
      (should (equal (plist-get (car (drover-fanout-last-choices 'ladder)) :model) "prov/gamma"))
      ;; Offered as they are, each with what it may cost; Start takes them.
      (let ((shown nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt _collection &rest _) (setq shown prompt) "Start"))
                  ((symbol-function 'drover-usage-estimate)
                   (lambda (_engine provider model &rest _)
                     (and (equal provider "prov") (equal model "alpha")
                          (list :sessions 4 :median 1.5 :p75 3.0 :scope 'here :billing 'priced)))))
          (should (equal (drover-fanout--read-choices 'parallel "/tmp/" (drover-fanout-last-choices 'parallel)
                                                      2 4 1 nil)
                         choices)))
        (should (string-match-p "1\\. Fake · Prov: Alpha · high — billed; before: \\$1.50 median, \\$3.00 p75, of 4 sessions of this repository"
                                shown))
        (should (string-match-p "2\\. Fake · Prov: Beta · low — " shown))
        (should (string-match-p "Drover limits no cost" shown)))
      ;; Too few: no Start offered, and an attempt removed.
      (let ((offered nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (push (all-completions "" collection) offered)
                     (if (member "Start" (car offered)) "Start" "Remove attempt 3"))))
          (should (equal (drover-fanout--read-choices 'parallel "/tmp/" (append choices choices) 1 3 1 nil)
                         (append choices (list (car choices))))))))))

;;;; Comparing

(ert-deftest drover-fanout-test-compare-and-keep ()
  "The attempts compared, checked, told apart; one kept, the others gone, snapshots kept."
  (drover-fanout-test-with
    (let* ((fanout (drover-fanout-test-start (list (drover-fanout-test-choice "alpha" nil)
                                                   (drover-fanout-test-choice "beta" nil)
                                                   (drover-fanout-test-choice "gamma" nil))
                                             '("\\`Goal: " . "No goal: the checks run as turns end")))
           (attempts (drover-fanout-test-settled fanout 3))
           (first (nth 0 attempts))
           (second (nth 1 attempts))
           (third (nth 2 attempts))
           (base (plist-get fanout :base)))
      ;; Each attempt changes its worktree its own way.
      (drover-fanout-test-commit (plist-get second :worktree) "fixed" "yes\n" "Fix")
      (drover-fanout-test-commit (plist-get second :worktree) "test/redirect_test.sh" "exit 0\n" "A test")
      (drover-test-write (expand-file-name "fixed" (plist-get third :worktree)) "yes\n")
      (drover-test-write (expand-file-name "test/login_test.sh" (plist-get third :worktree)) "exit 1\n")
      (drover-test-write (expand-file-name "package.json" (plist-get third :worktree)) "{}\n")
      (let ((buffer (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
                      (drover-fanout-compare drover-fanout-test-id))))
        (should (drover-fanout-test-wait (lambda () (seq-every-p (lambda (attempt) (drover-fanout-known-stats fanout attempt))
                                                                 attempts))))
        (let ((stats (drover-fanout-known-stats fanout second)))
          (should (equal (mapcar #'car (plist-get stats :files)) '("fixed" "test/redirect_test.sh")))
          (should (= (plist-get stats :added) 2))
          (should (equal (drover-fanout-touched stats) '(:tests-added 1 :tests-changed 0 :protected nil))))
        (should (equal (drover-fanout-touched (drover-fanout-known-stats fanout third))
                       '(:tests-added 0 :tests-changed 1 :protected ("package.json (scripts)"))))
        (should-not (plist-get (drover-fanout-known-stats fanout first) :files))
        ;; What no other attempt touched.
        (should (equal (drover-fanout-only-here fanout second) '("test/redirect_test.sh")))
        (should (equal (sort (drover-fanout-only-here fanout third) #'string<) '("package.json" "test/login_test.sh")))
        ;; The checks, on demand, two at most at once.
        (drover-fanout-queue-check fanout second)
        (drover-fanout-queue-check fanout third)
        (should (drover-fanout-test-wait (lambda () (and (plist-get second :checks) (plist-get third :checks)))))
        (should (plist-get (plist-get second :checks) :ok))
        (should (plist-get (plist-get third :checks) :ok))
        ;; As a turn ends.
        (let ((session (drover-fanout--session fanout first t)))
          (setf (drover-session-status session) 'working)
          (drover-fanout--turn-check)
          (setf (drover-session-status session) 'idle)
          (drover-fanout--turn-check))
        (should (drover-fanout-test-wait (lambda () (plist-get first :checks))))
        (should-not (plist-get (plist-get first :checks) :ok))
        (should (equal (plist-get (plist-get first :checks) :command) "sh check.sh"))
        (should (string-match-p "FAIL test_login" (car (plist-get (plist-get first :checks) :lines))))
        ;; The comparison shows it all.
        (with-current-buffer buffer
          (drover-attempts--draw)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "Attempts at Fix the login" text))
            (should (string-match-p "#2 Fake · Prov: Beta" text))
            (should (string-match-p "Checks +✓ pass" text))
            (should (string-match-p "Checks +✗ `sh check.sh` exited 1: FAIL test_login" text))
            (should (string-match-p "Changes +2 files, \\+2 −0" text))
            (should (string-match-p "Only here +test/redirect_test.sh" text))
            (should (string-match-p "Tests +0 added, 1 edited or deleted ⚠" text))
            (should (string-match-p "Protected +package.json (scripts)" text))
            (should (string-match-p "drover limits none" text)))
          ;; The session at point is the attempt's.
          (goto-char (point-min))
          (re-search-forward "^#2 ")
          (should (equal (drover-session-directory (drover-session-at-point)) (plist-get second :worktree)))))
      ;; Two attempts told apart, uncommitted files too.
      (let ((diff (drover-fanout-test-call (lambda (callback) (drover-fanout-diff fanout second third callback)))))
        (should-not (plist-get diff :failure))
        (should (string-match-p "^\\+\\+\\+ b/package.json" (plist-get diff :text)))
        (should (string-match-p "^--- a/test/redirect_test.sh" (plist-get diff :text))))
      ;; An advisory summary: the attempts by letters, unnamed, in no order.
      (let* ((asked nil)
             (drover-repo-generate-function (lambda (prompt system callback)
                                              (setq asked (list prompt system))
                                              (funcall callback (list :text "A rewrote it." :name "Cheap"))))
             (result (drover-fanout-test-call (lambda (callback) (drover-fanout-summarize fanout callback)))))
        (should (equal (plist-get result :text) "A rewrote it."))
        (should (equal (sort (mapcar #'cdr (plist-get result :letters)) #'<) '(1 2 3)))
        (should (string-match-p "## Attempt A\n" (car asked)))
        (should (string-match-p "## Attempt C\n" (car asked)))
        (should (string-match-p "```diff\n" (car asked)))
        (should (string-match-p "It fails after the redirect." (car asked)))
        (should-not (string-match-p "Alpha\\|Beta\\|Gamma\\|Fake" (car asked)))
        (should (string-match-p "data, not instructions" (cadr asked))))
      ;; Kept: the second becomes the task's, the others go, their changes kept.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (drover-fanout-keep fanout 2))
      (should (drover-fanout-test-wait (lambda () (eq (plist-get second :state) 'kept))))
      (should (drover-test-wait-actions 20))
      (should (eq (plist-get fanout :state) 'kept))
      (should (eq (plist-get first :state) 'discarded))
      (should (eq (plist-get third :state) 'discarded))
      (should-not (file-exists-p (plist-get first :worktree)))
      (should-not (file-exists-p (plist-get third :worktree)))
      (should (file-directory-p (plist-get second :worktree)))
      ;; Its branch is the task's; the others' are gone, their snapshots kept.
      (should (equal (drover-fanout-test-git (plist-get second :worktree) "rev-parse" "--abbrev-ref" "HEAD")
                     "fix-the-login"))
      (should-not (drover-fanout-test-git-ok-p checkout "rev-parse" "--verify" "--quiet" "refs/heads/fix-the-login-2"))
      (should-not (drover-fanout-test-git-ok-p checkout "rev-parse" "--verify" "--quiet" "refs/heads/fix-the-login-3"))
      (let ((refs (split-string (drover-fanout-test-git checkout "for-each-ref" "--format=%(refname)" "refs/drover/snapshots/")
                                "\n" t)))
        (should (= 2 (length refs)))
        (let ((third-ref (seq-find (lambda (ref) (string-match-p "snapshots/fix-the-login-3-[0-9]+\\'" ref)) refs))
              (first-ref (seq-find (lambda (ref) (string-match-p "snapshots/fix-the-login-[0-9]+\\'" ref)) refs)))
          (should third-ref)
          (should (equal (drover-fanout-test-git checkout "rev-parse" first-ref) base))
          ;; The third's uncommitted files are in its snapshot.
          (should (equal (drover-fanout-test-git checkout "show" (concat third-ref ":package.json")) "{}"))))
      ;; The heading tells the kept one as the task's, and how it went.
      (should (equal (drover-fanout-test-property "DROVER_WORKTREE") (abbreviate-file-name (plist-get second :worktree))))
      (should (equal (drover-fanout-test-property "DROVER_BRANCH") "fix-the-login"))
      (dolist (name '("DROVER_WORKTREE_2" "DROVER_BRANCH_2" "DROVER_WORKTREE_3" "DROVER_BASE_3"))
        (should-not (drover-fanout-test-property name)))
      (should (string-match-p "Kept attempt 2 (Fake · Prov: Beta): checks pass, 2 files \\+2 −0" (buffer-string)))
      (should (string-match-p "Discarded attempts 1, 3: worktrees removed, branches deleted, sessions archived; snapshots refs/drover/snapshots/"
                              (buffer-string)))
      ;; Their sessions archived, the kept one's not.
      (let ((archived (mapcar #'cadr (seq-filter (lambda (call) (eq (car call) :archive)) drover-test-calls))))
        (should (= 2 (length archived)))
        (should-not (member (drover-session-id (drover-fanout--session fanout second t)) archived)))
      ;; One attempt left: the task finishes as tasks do.
      (should (= 1 (length (plist-get (drover-task-read (drover-fanout-test-marker) drover-fanout-test-id) :attempts)))))))

(ert-deftest drover-fanout-test-joins-a-started-task ()
  "A task at work keeps its worktree as the first attempt; others join it from its base, and more later."
  (drover-fanout-test-with
    (cl-letf (((symbol-function 'completing-read) (drover-fanout-test-answers nil)))
      (drover-task-start))
    (should (drover-fanout-test-wait (lambda () (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls))))
    (let ((worktree (expand-file-name "worktrees/test-repo/fix-the-login/" root))
          (base (drover-fanout-test-property "DROVER_BASE")))
      ;; Its work goes on meanwhile: the others start from its base, not from its commits.
      (drover-fanout-test-commit worktree "work" "begun\n" "Begun")
      (let* ((fanout (drover-fanout-test-start (list (drover-fanout-test-choice "beta" "low"))
                                               '("\\`Goal: " . "No goal: the checks run as turns end")))
             (attempts (drover-fanout-test-settled fanout 2)))
        (should (equal (mapcar (lambda (attempt) (plist-get attempt :n)) attempts) '(1 2)))
        (should (equal (plist-get (car attempts) :worktree) worktree))
        (should (equal (plist-get (plist-get (car attempts) :choice) :backend) 'fake))
        (should (equal (plist-get fanout :base) base))
        (should (equal (drover-fanout-test-git (plist-get (nth 1 attempts) :worktree) "rev-parse" "HEAD") base))
        (should (equal (plist-get (nth 1 attempts) :branch) "fix-the-login-2"))
        ;; One session of the first, one of the second.
        (should (= 2 (seq-count (lambda (call) (eq (car call) :create)) drover-test-calls)))
        ;; & again compares; with a prefix argument, another joins.
        (let ((compared nil))
          (cl-letf (((symbol-function 'drover-fanout-compare) (lambda (task) (setq compared task))))
            (drover-fanout))
          (should (equal compared drover-fanout-test-id)))
        (cl-letf (((symbol-function 'completing-read) (drover-fanout-test-answers nil)))
          (drover-fanout '(4)))
        (drover-fanout-test-settled fanout 3)
        (should (equal (plist-get (nth 2 (plist-get fanout :attempts)) :branch) "fix-the-login-3"))
        (should (equal (drover-fanout-test-property "DROVER_BRANCH_3") "fix-the-login-3"))))))

;;;; Ladders

(ert-deftest drover-fanout-test-ladder ()
  "One attempt at a time under the goal; the next only once one gives up; none limited in money."
  (drover-fanout-test-with
    (drover-fanout--remember-choices 'ladder (list (drover-fanout-test-choice "alpha" nil)
                                                   (drover-fanout-test-choice "beta" nil)
                                                   (drover-fanout-test-choice "gamma" nil)))
    (cl-letf (((symbol-function 'completing-read)
               (drover-fanout-test-answers '(("\\`Attempts: " . "Ladder: one at a time under the checks, the next only once one gives up"))))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (drover-fanout))
    (let* ((fanout (drover-fanout-of-task drover-fanout-test-id))
           (attempts (plist-get fanout :attempts)))
      (should (eq (plist-get fanout :mode) 'ladder))
      (should (equal (mapcar (lambda (item) (plist-get item :command)) (car (plist-get fanout :goal))) '("sh check.sh")))
      (drover-fanout-test-settled fanout 1)
      (should (equal (mapcar (lambda (attempt) (plist-get attempt :state)) attempts) '(running pending pending)))
      (let ((goal (drover-goal--find (plist-get (car attempts) :goal))))
        (should goal)
        (should (equal (plist-get goal :title) "Fix the login #1"))
        (should (equal (plist-get goal :commands) '("sh check.sh")))
        ;; Drover limits no cost: no budget.
        (should-not (plist-get goal :budget))
        (should (eq (plist-get goal :on-end) 'drover-fanout-goal-ended))
        (should (string-match-p "Drover keeps this session under a goal"
                                (nth 2 (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls))))
        ;; It gives up: the next starts, from the same base.
        (drover-goal--end goal 'given-up "no progress"))
      (drover-fanout-test-settled fanout 2)
      (should (eq (plist-get (nth 2 attempts) :state) 'pending))
      (should (equal (drover-fanout-test-git (plist-get (nth 1 attempts) :worktree) "rev-parse" "HEAD")
                     (plist-get fanout :base)))
      (should (equal (drover-fanout-test-property "DROVER_BRANCH_2") "fix-the-login-2"))
      ;; The second meets it: the ladder is over, the third never runs.
      (drover-goal--end (drover-goal--find (plist-get (nth 1 attempts) :goal)) 'met "sh check.sh pass")
      (should (eq (plist-get fanout :state) 'settled))
      (should (equal (plist-get fanout :note) "attempt 2 met the goal"))
      (should (eq (plist-get (nth 2 attempts) :state) 'skipped))
      (should-not (file-exists-p (expand-file-name "worktrees/test-repo/fix-the-login-3/" root)))
      (should (string-match-p "Ladder over: attempt 2 met the goal." (buffer-string))))))

;;;; The list

(ert-deftest drover-fanout-test-list-groups-attempts ()
  "The sessions of attempts show under their task, each with its number and model."
  (drover-test-with-state
    (drover-test-with-backend (list (drover-test-session "a" :directory "/tmp/wt/task/")
                                    (drover-test-session "b" :directory "/tmp/wt/task-2/sub/")
                                    (drover-test-session "c" :directory "/tmp/elsewhere/"))
      (let ((drover-task--noted (make-hash-table :test #'equal))
            (drover-task--links (make-hash-table :test #'equal))
            (drover-task--infos (make-hash-table :test #'equal))
            (drover-task-mode t)
            (drover-session-task-function #'drover-task--session-task)
            (drover-task-attempt-label-function #'drover-fanout-attempt-label)
            (drover-task-path-aliases nil)
            (drover-fanout--table (drover-fanout--table-make :loaded t))
            (drover-group-by 'task))
        (drover-task-note-session '(fake . "a") "T1")
        (drover-task-note-session '(fake . "b") "T1")
        (drover-task--take "T1" (list :id "T1" :title "Fix the login" :category "work"
                                      :attempts (list (list :number 1 :directory "/tmp/wt/task/")
                                                      (list :number 2 :directory "/tmp/wt/task-2/"))
                                      :tick (cons (current-buffer) (buffer-chars-modified-tick))))
        (push (list :id "f" :task "T1" :title "Fix the login" :mode 'parallel :state 'running :place nil :checks nil
                    :created 1.0
                    :attempts (list (drover-fanout--new-attempt 1 (drover-fanout-test-choice "alpha" "high"))
                                    (drover-fanout--new-attempt 2 (drover-fanout-test-choice "beta" nil))))
              (drover-fanout--table-fanouts drover-fanout--table))
        (should (equal (mapcar #'car (drover-view-groups)) '((task "T1" . "Fix the login") (task))))
        (let* ((sessions (drover-sessions))
               (a (seq-find (lambda (session) (equal (drover-session-id session) "a")) sessions))
               (b (seq-find (lambda (session) (equal (drover-session-id session) "b")) sessions))
               (c (seq-find (lambda (session) (equal (drover-session-id session) "c")) sessions)))
          (should (eql (plist-get (drover-session-task a) :attempt) 1))
          (should (eql (plist-get (drover-session-task b) :attempt) 2))
          (should-not (drover-session-task c))
          (should (equal (substring-no-properties (drover-list--attempt-text a)) "#1 Alpha high"))
          (should (equal (substring-no-properties (drover-list--attempt-text b)) "#2 Beta"))
          (should (equal (get-text-property 3 'help-echo (drover-list--attempt-text b)) "Fake · Prov: Beta"))
          ;; One place left: no attempt shown.
          (drover-task--take "T1" (list :id "T1" :title "Fix the login" :category "work"
                                        :attempts (list (list :number 2 :directory "/tmp/wt/task-2/"))
                                        :tick (cons (current-buffer) (buffer-chars-modified-tick))))
          (should-not (plist-get (drover-session-task b) :attempt))
          (should-not (drover-list--attempt-text b)))))))

;;;; What attempts may cost

(ert-deftest drover-fanout-test-estimate ()
  "Sessions of a model cost before: their median and p75, of the repository when enough ran there."
  (let* ((day (drover-usage-day (float-time)))
         (facts (list
                 ;; Root sessions of the model, their subagents counted with them.
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "alpha" :id "r1" :day day
                                          :directory "/repo/" :output 100 :cost 1.0 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "cheap" :id "r1/sub" :day day
                                          :output 10 :cost 0.5 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "alpha" :id "r2" :day day
                                          :directory "/repo/" :output 100 :cost 2.0 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "alpha" :id "r3" :day day
                                          :directory "/elsewhere/" :output 100 :cost 4.0 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "alpha" :id "r4" :day day
                                          :directory "/repo/wt/" :output 100 :cost 8.0 :billing 'priced)
                 ;; Of another model, another provider, too old.
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "beta" :id "r5" :day day
                                          :output 100 :cost 50.0 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "other" :model "alpha" :id "r6" :day day
                                          :output 100 :cost 60.0 :billing 'priced)
                 (drover-usage--fact-make :harness 'opencode :provider "prov" :model "alpha" :id "r7" :day 20000101
                                          :output 100 :cost 70.0 :billing 'priced)))
         (parents (make-hash-table :test #'equal)))
    (puthash '(opencode . "r1/sub") "r1" parents)
    (cl-letf (((symbol-function 'drover-usage--ensure) #'ignore)
              ((symbol-function 'drover-usage--current-facts)
               (lambda () (drover-usage--facts-make :facts facts :parents parents)))
              ((symbol-function 'drover-usage-billing) (lambda (_harness _provider) 'priced)))
      (let ((drover-usage--started t))
        (should (equal (drover-usage-estimate 'opencode "prov" "alpha" '("/repo/"))
                       '(:sessions 3 :median 2.0 :p75 8.0 :scope here :billing priced)))
        (should (equal (drover-usage-estimate 'opencode "prov" "alpha" '("/nowhere/"))
                       '(:sessions 4 :median 2.0 :p75 4.0 :scope all :billing priced)))
        (should (equal (drover-usage-estimate 'opencode "prov" "gone")
                       '(:sessions 0 :median nil :p75 nil :scope all :billing priced)))
        (should (equal (drover-fanout-estimate-text (drover-usage-estimate 'opencode "prov" "alpha" '("/repo/")))
                       "billed; before: $2.00 median, $8.00 p75, of 3 sessions of this repository"))
        (should (equal (drover-fanout-estimate-text '(:sessions 2 :billing included))
                       "included; 2 sessions of it in 30 days"))))))

;;;; Robustness

(ert-deftest drover-fanout-test-file-checked ()
  "Fan-outs read as kept, those not of a known shape dropped; a file not read never written over."
  (drover-test-with-state
    (let ((drover-fanout--table (drover-fanout--table-make))
          (good (list :id "f1" :task "T1" :title "Fix" :mode 'parallel :state 'running :place nil :checks '("make")
                      :goal nil :created 1.0
                      :attempts (list (drover-fanout--new-attempt 1 (drover-fanout-test-choice "alpha" nil))))))
      (drover-write-data (drover-fanout-file) "Fan-outs" drover-fanout--version :fanouts
                         (list good (list :id "f2" :task "T2" :mode 'parallel)
                               (append (list :goal '(("make"))) good)))
      (should (equal (mapcar (lambda (fanout) (plist-get fanout :id)) (drover-fanout--fanouts)) '("f1")))
      (setq drover-fanout--table (drover-fanout--table-make))
      (drover-test-write (drover-fanout-file) "(:version 1 :fanouts (")
      (cl-letf (((symbol-function 'drover-warn) #'ignore))
        (should-not (drover-fanout--fanouts)))
      (should-error (drover-fanout--save))
      (should (equal (with-temp-buffer (insert-file-contents (drover-fanout-file)) (buffer-string))
                     "(:version 1 :fanouts (")))))

(ert-deftest drover-fanout-test-prints-in-proportion ()
  "What fan-outs keep prints in proportion to them."
  (drover-test-with-state
    (let ((drover-fanout--table (drover-fanout--table-make :loaded t)))
      (dotimes (n 20)
        (push (list :id (format "f%d" n) :task "T" :title "Fix" :mode 'parallel :state 'running :place nil
                    :checks nil :goal nil :created 1.0
                    :attempts (list (drover-fanout--new-attempt 1 (drover-fanout-test-choice "alpha" nil))))
              (drover-fanout--table-fanouts drover-fanout--table)))
      (should (drover-test-printed-length drover-fanout--table 65536)))))

(provide 'drover-fanout-test)
;;; drover-fanout-test.el ends here
