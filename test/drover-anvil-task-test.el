;;; drover-anvil-task-test.el --- Tests for the tools starting tasks and forking  -*- lexical-binding: t; -*-

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

;; The tools agents hand work off with, drover-task-start and
;; drover-session-fork, on the fake backend, with the repository and
;; the Org file of the tasks' tests: a start with each argument, each
;; refusal, a task that continues, a fork linked to a task, a backend
;; that cannot fork, the agent a session or a fork runs, and one its
;; harness does not offer.  Then the links the tools telling of a
;; session tell.  Nothing is shown.

;;; Code:

(require 'ert)
(require 'drover-test-helpers)
;; Their fixtures: the caller of the tools, a repository, an Org file.
(require 'drover-anvil-test)
(require 'drover-task-test)
(require 'drover-repo)

(defun drover-anvil-task-test-add (&rest properties)
  "Give the fake backend PROPERTIES over its own."
  (setf (alist-get 'fake drover--backends)
        (append properties (alist-get 'fake drover--backends))))

;;;; The tools

(defun drover-anvil-task-test-answer (text)
  "Return TEXT, a tool's answer, parsed, its job waited for when it is pending.
A job failing signals, as the tool's refusal does."
  (let ((answer (drover-anvil-test-json text)))
    (if (not (eq (alist-get 'pending answer) t))
        answer
      (let ((job (alist-get 'job answer)))
        (should (drover-task-test-wait (lambda () (not (eq (drover-anvil--job-get job :state) 'pending))) 60))
        (drover-anvil-test-json (drover-anvil--tool-job job))))))

(defun drover-anvil-task-test-start (&rest arguments)
  "Return the answer of drover-task-start called with ARGUMENTS, waited for."
  (drover-anvil-task-test-answer (apply #'drover-anvil--tool-task-start arguments)))

(defun drover-anvil-task-test-refusal (&rest arguments)
  "Return why drover-task-start called with ARGUMENTS is refused."
  (cadr (should-error (apply #'drover-anvil-task-test-start arguments))))

(defun drover-anvil-task-test-property (id property)
  "Return PROPERTY of the heading of ID."
  (org-entry-get (drover-task-org-marker id) property))

(defmacro drover-anvil-task-test-with-task (content &rest body)
  "Run BODY visiting an Org file of CONTENT, with a repository and the tools.
The fake backend starts the sessions of tasks, the tools are called by
the fake session \"me\", and `checkout' is the checkout of test/repo,
whose facts the user has not accepted yet."
  (declare (indent 1) (debug t))
  `(drover-anvil-test-with nil "me"
     (drover-task-test-with-repo
       (drover-test-with-engines
         (drover-test-register-engine 'fake :directory #'ignore)
         (let ((drover-task-harness '(:backend fake :engine fake))
               (drover-task--links nil)
               (drover-repo--cache 'unread)
               (drover-repo-generate-function #'drover-task-test-model))
           (drover-task-test-with-org ,content
             ,@body))))))

(defconst drover-anvil-task-test-id "5A0E0000-0000-4000-8000-000000000001"
  "The ID of the heading of the fixture's task.")

(defconst drover-anvil-task-test-org
  (concat "* Work\n:PROPERTIES:\n:DROVER_REPO: test/repo\n:END:\n** TODO Fix the login\n:PROPERTIES:\n:ID: "
          drover-anvil-task-test-id "\n:END:\nIt fails after the redirect.\n")
  "An Org file whose task works in test/repo.")

(defun drover-anvil-task-test-alone (&optional properties)
  "Return an Org file of one task, naming no repository, with PROPERTIES."
  (concat "* TODO Read the logs\n:PROPERTIES:\n:ID: " drover-anvil-task-test-id "\n" (or properties "")
          ":END:\nSince Monday.\n"))

(ert-deftest drover-anvil-task-test-task-start ()
  "A heading's task starts unshown, by its ID or its link; started again, it continues."
  (drover-anvil-task-test-with-task drover-anvil-task-test-org
    ;; How its repository works is the user's to accept, once.
    (should (string-match-p "\\`How test/repo works was not accepted yet"
                            (drover-anvil-task-test-refusal drover-anvil-task-test-id)))
    (should-not (assq :create drover-test-calls))
    (should-not (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_WORKTREE"))
    (drover-repo-confirm (drover-repo-facts-now checkout))
    (let ((answer (drover-anvil-task-test-start (format "[[id:%s][Fix the login]]" drover-anvil-task-test-id)))
          (worktree (expand-file-name "worktrees/test-repo/fix-the-login/" root)))
      (should (equal (alist-get 'outcome answer) "started"))
      (should (equal (alist-get 'key answer) "fake/new-0"))
      (should (equal (alist-get 'directory answer) (abbreviate-file-name worktree)))
      (should (equal (alist-get 'branch answer) "fix-the-login"))
      (should (equal (alist-get 'task answer) drover-anvil-task-test-id))
      (should (equal (alist-get 'title answer) "Fix the login"))
      ;; Its conversation is not known yet: no link.
      (should-not (assq 'link answer))
      (should (file-directory-p worktree))
      (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_WORKTREE")
                     (abbreviate-file-name worktree)))
      (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_BRANCH") "fix-the-login"))
      (pcase-let ((`(:create ,directory fake ,options) (assq :create drover-test-calls)))
        (should (equal directory worktree))
        (should (plist-get options :hidden)))
      (let ((sent (nth 2 (assq :send drover-test-calls))))
        (should (string-search (concat "id:" drover-anvil-task-test-id) sent))
        (should (string-search "It fails after the redirect." sent)))
      ;; The caller's, awaited from its first prompt.
      (should (equal (gethash '(fake . "new-0") drover-anvil--started) '(fake . "me")))
      (should (gethash '(fake . "new-0") drover-anvil--sent))
      (should (seq-find (lambda (line)
                          (string-match-p (concat "drover-task-start\tcaller=fake/me\tsession=-\tacting\ttask "
                                                  drover-anvil-task-test-id)
                                          line))
                        (drover-anvil-test-log)))
      ;; Working somewhere, it continues: another session in its place, nothing made.
      (let ((again (drover-anvil-task-test-start drover-anvil-task-test-id "nosuch/repo")))
        (should (equal (alist-get 'outcome again) "continued"))
        (should (equal (alist-get 'key again) "fake/new-1"))
        (should (equal (alist-get 'directory again) (abbreviate-file-name worktree)))
        (should (equal (alist-get 'branch again) "fix-the-login")))
      (should (equal (drover-session-directory (drover-find 'fake "new-1")) worktree))
      (should-not (file-exists-p (expand-file-name "worktrees/test-repo/fix-the-login-2/" root)))
      (should-not (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_REPO"))
      ;; Its place gone, nothing is made again unasked.
      (drover-task-test-git checkout "worktree" "remove" "--force" "--force" (directory-file-name worktree))
      (should (string-match-p "is gone" (drover-anvil-task-test-refusal drover-anvil-task-test-id))))))

(ert-deftest drover-anvil-task-test-task-start-repo ()
  "A heading naming no repository is refused with those that work; one given is written first."
  (drover-anvil-task-test-with-task (drover-anvil-task-test-alone)
    (drover-repo-confirm (drover-repo-facts-now checkout))
    (should (string-match-p "neither a repository nor a folder: give repo, one of test/repo, or directory, the folder it works in\\'"
                            (drover-anvil-task-test-refusal drover-anvil-task-test-id)))
    (should (string-match-p "\\`No repository nosuch in .drover-task-repositories.: give repo, one of test/repo\\'"
                            (drover-anvil-task-test-refusal drover-anvil-task-test-id "nosuch")))
    (should (string-match-p "\\`Give repo or directory, not both"
                            (drover-anvil-task-test-refusal drover-anvil-task-test-id "test/repo" root)))
    (should-not (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_REPO"))
    (should-not (assq :create drover-test-calls))
    (let ((answer (drover-anvil-task-test-start drover-anvil-task-test-id "test/repo")))
      (should (equal (alist-get 'outcome answer) "started"))
      (should (equal (alist-get 'branch answer) "read-the-logs"))
      (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_REPO") "test/repo"))
      (should (equal (drover-session-directory (drover-find 'fake "new-0"))
                     (expand-file-name "worktrees/test-repo/read-the-logs/" root))))))

(ert-deftest drover-anvil-task-test-task-start-directory ()
  "A task works in the folder given, written on its heading first, and made."
  (drover-anvil-task-test-with-task (drover-anvil-task-test-alone)
    (let ((folder (expand-file-name "investigation/" root)))
      (should (string-match-p "is not a directory of this host"
                              (cadr (should-error (drover-anvil--tool-task-start drover-anvil-task-test-id nil "logs")))))
      (let ((answer (drover-anvil-task-test-start drover-anvil-task-test-id nil (directory-file-name folder))))
        (should (equal (alist-get 'outcome answer) "started"))
        (should (equal (alist-get 'directory answer) (abbreviate-file-name folder)))
        (should-not (assq 'branch answer))
        (should (file-directory-p folder))
        (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_FOLDER")
                       (abbreviate-file-name folder)))
        (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_DIRECTORY")
                       (abbreviate-file-name folder)))
        (should (equal (drover-session-directory (drover-find 'fake "new-0")) folder))))))

(ert-deftest drover-anvil-task-test-task-start-refusals ()
  "What the heading and the arguments leave open is refused, with the values that work."
  (drover-anvil-task-test-with-task (concat "* TODO Both\n:PROPERTIES:\n:ID: both\n:DROVER_REPO: test/repo other/repo\n:END:\n"
                                         "* TODO Elsewhere\n:PROPERTIES:\n:ID: elsewhere\n:DROVER_REPO: far/repo\n:END:\n"
                                         "* TODO Unknown\n:PROPERTIES:\n:ID: unknown\n:DROVER_REPO: gone/repo\n:END:\n"
                                         "* TODO Folder\n:PROPERTIES:\n:ID: folder\n:DROVER_FOLDER: " root "\n:END:\n")
    (let ((drover-task-repositories (append drover-task-repositories
                                            `(("other/repo" :checkout ,root)
                                              ("far/repo" :checkout (("core" . "c:/src/far/")))))))
      (should (string-match-p "\\`The heading names several repositories: give repo, one of test/repo, other/repo\\'"
                              (drover-anvil-task-test-refusal "both")))
      (should (string-match-p "\\`No checkout of far/repo on this host: give host, one of core\\'"
                              (drover-anvil-task-test-refusal "elsewhere")))
      (should (string-match-p "\\`The heading names the repository gone/repo, which .drover-task-repositories. does not: give repo, one of test/repo, other/repo, far/repo\\'"
                              (drover-anvil-task-test-refusal "unknown")))
      ;; A heading drover does not find, and what names none.
      (should (string-match-p "\\`No heading of ID nowhere in the Org files"
                              (drover-anvil-task-test-refusal "nowhere")))
      (should (string-match-p "\\`No Org ID in "
                              (cadr (should-error (drover-anvil--tool-task-start "not an id!")))))
      (should (string-match-p "\\`No Org ID in "
                              (cadr (should-error (drover-anvil--tool-task-start "")))))
      (should (string-match-p "\\`No host nowhere"
                              (cadr (should-error (drover-anvil--tool-task-start "both" nil nil "nowhere")))))
      (drover-anvil-test-with-peer nil
        (should (string-match-p "\\`A task in a folder starts on this host: leave host out\\'"
                                (drover-anvil-task-test-refusal "folder" nil nil "core"))))
      (should-not (assq :create drover-test-calls))
      ;; Nothing was written for a start refused.
      (should-not (drover-anvil-task-test-property "both" "DROVER_WORKTREE")))))

(ert-deftest drover-anvil-task-test-task-start-several-places ()
  "A task working in several places continues in the one named."
  (drover-anvil-task-test-with-task ""
    (let ((first (expand-file-name "one/" root))
          (second (expand-file-name "two/" root)))
      (make-directory first)
      (make-directory second)
      (erase-buffer)
      (insert "* TODO Compare\n:PROPERTIES:\n:ID: compare\n:DROVER_DIRECTORY: " first
              "\n:DROVER_HOST: test\n:DROVER_DIRECTORY_2: " second "\n:DROVER_HOST_2: test\n:END:\n")
      (save-buffer)
      (should (equal (drover-anvil-task-test-refusal "compare")
                     (format "The task works in several places: give directory, one of %s, %s" first second)))
      (let ((answer (drover-anvil-task-test-start "compare" nil second)))
        (should (equal (alist-get 'outcome answer) "continued"))
        (should (equal (alist-get 'directory answer) (abbreviate-file-name second))))
      (should (equal (drover-session-directory (drover-find 'fake "new-0")) second))
      ;; Nothing is written on a task that continues.
      (should-not (drover-anvil-task-test-property "compare" "DROVER_FOLDER")))))

(ert-deftest drover-anvil-task-test-task-start-agent ()
  "A task's session runs the agent given; one its harness does not offer is refused, those it offers named."
  (drover-anvil-task-test-with-task drover-anvil-task-test-org
    (drover-repo-confirm (drover-repo-facts-now checkout))
    (setq drover-test-engine-agents '((fake "build" "plan")))
    (let ((worktree (expand-file-name "worktrees/test-repo/fix-the-login/" root)))
      ;; Refused before its place is made: nothing starts.
      (should (equal (drover-anvil-task-test-refusal drover-anvil-task-test-id nil nil nil "review")
                     (format "Fake offers no agent review in %s: it offers build, plan; nothing started"
                             (abbreviate-file-name checkout))))
      (should-not (file-directory-p worktree))
      (should-not (assq :create drover-test-calls))
      (should-not (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_WORKTREE"))
      ;; Its harness asked for its agents where the task works, never waited for.
      (should (equal (car drover-test-engine-calls) (list :agents 'fake checkout)))
      ;; One it offers: its session runs it.
      (let ((answer (drover-anvil-task-test-start drover-anvil-task-test-id nil nil nil "plan")))
        (should (equal (alist-get 'outcome answer) "started"))
        (pcase-let ((`(:create ,directory fake ,options) (assq :create drover-test-calls)))
          (should (equal directory worktree))
          (should (equal (plist-get options :agent) "plan"))))
      (should (seq-find (lambda (line) (string-match-p "drover-task-start\t.*\tacting\ttask .*, agent plan" line))
                        (drover-anvil-test-log)))
      ;; Continued, another session in its place, with the agent given.
      (setq drover-test-calls nil)
      (should (string-match-p "\\`Fake offers no agent review in .*: it offers build, plan; no session started"
                              (drover-anvil-task-test-refusal drover-anvil-task-test-id nil nil nil "review")))
      (should-not (assq :create drover-test-calls))
      (should (equal (alist-get 'outcome (drover-anvil-task-test-start drover-anvil-task-test-id nil nil nil "build"))
                     "continued"))
      (should (equal (plist-get (nth 3 (assq :create drover-test-calls)) :agent) "build"))
      ;; Without one, as before: the harness's default, or the user's for tasks.
      (setq drover-test-calls nil)
      (drover-anvil-task-test-start drover-anvil-task-test-id)
      (should-not (plist-get (nth 3 (assq :create drover-test-calls)) :agent))
      (let ((drover-task-agent "plan"))
        (setq drover-test-calls nil)
        (drover-anvil-task-test-start drover-anvil-task-test-id)
        (should (equal (plist-get (nth 3 (assq :create drover-test-calls)) :agent) "plan")))
      ;; A harness that cannot tell its agents refuses none: the agent is tried.
      (setq drover-test-engine-agents nil
            drover-test-calls nil)
      (drover-anvil-task-test-start drover-anvil-task-test-id nil nil nil "review")
      (should (equal (plist-get (nth 3 (assq :create drover-test-calls)) :agent) "review")))))

(ert-deftest drover-anvil-task-test-task-start-on-another-host ()
  "A task starts on the host named, which answers where; it is told once listed here."
  (let ((there nil) (asked nil) (reply nil))
    (drover-anvil-task-test-with-task drover-anvil-task-test-org
      (drover-anvil-test-with-peer there
        (cl-letf (((symbol-function 'drover-remote-ask-task)
                   (lambda (name request callback &optional _seconds)
                     (setq asked (list name request) reply callback))))
          (let* ((pending (drover-anvil-test-json (drover-anvil--tool-task-start drover-anvil-task-test-id nil nil "Core")))
                 (job (alist-get 'job pending)))
            (should job)
            (should (equal (car asked) "core"))
            (should (equal (plist-get (cadr asked) :kind) "start"))
            (should (equal (plist-get (cadr asked) :task) drover-anvil-task-test-id))
            (should (equal (plist-get (cadr asked) :repo) "test/repo"))
            (should (equal (plist-get (cadr asked) :mode) "worktree"))
            (should (equal (plist-get (cadr asked) :body) "It fails after the redirect."))
            ;; Nothing is made here.
            (should-not (file-exists-p (expand-file-name "worktrees/" root)))
            (funcall reply (list :worktree "c:/w/fix-the-login/" :directory "c:/w/fix-the-login/"
                                 :branch "fix-the-login" :base "0123456789abcdef" :base-ref "origin/main"
                                 :key ["opencode" "ses_far"] :title "Fix the login")
                     nil)
            (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_HOST") "core"))
            (should (equal (drover-anvil-task-test-property drover-anvil-task-test-id "DROVER_WORKTREE") "c:/w/fix-the-login/"))
            ;; Started there, not listed here yet: still pending.
            (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
            (setq there (list (drover-anvil-test-peer-session "ses_far" :status 'working
                                                              :engine 'opencode :engine-id "ses_far")))
            (drover-backend-changed 'remote:core)
            (let ((started (drover-anvil-test-json (drover-anvil--tool-job job))))
              (should (equal (alist-get 'outcome started) "started"))
              (should (equal (alist-get 'key started) "remote:core/opencode/ses_far"))
              (should (equal (alist-get 'link started) "agent-session://core/opencode/ses_far"))
              (should (equal (alist-get 'host started) "core"))
              (should (equal (alist-get 'directory started) "c:/w/fix-the-login/"))
              (should (equal (alist-get 'branch started) "fix-the-login")))
            (should (equal (drover-task-id-of (car there)) drover-anvil-task-test-id))
            ;; No agent given: the request names none, which any drover reads.
            (should-not (plist-member (cadr asked) :agent))
            ;; Continued, a session starts in its place there, with the agent given:
            ;; its host tells whether its harness offers it.
            (let ((job (alist-get 'job (drover-anvil-test-json
                                        (drover-anvil--tool-task-start drover-anvil-task-test-id nil nil nil "plan")))))
              (should (equal (plist-get (cadr asked) :kind) "session"))
              (should (equal (plist-get (cadr asked) :worktree) "c:/w/fix-the-login/"))
              (should (equal (plist-get (cadr asked) :agent) "plan"))
              (funcall reply nil "OpenCode offers no agent plan in c:/w/fix-the-login/: it offers build")
              (should (string-match-p "core: OpenCode offers no agent plan in c:/w/fix-the-login/: it offers build"
                                      (cadr (should-error (drover-anvil--tool-job job))))))))))))

;;;; Forking

(defvar drover-anvil-task-test-forks 0
  "How many sessions the fake :fork-hidden forked.")

(defun drover-anvil-task-test-fork-hidden (session then _else)
  "Fork SESSION as the fake backend's :fork-hidden: serve the copy, and tell THEN."
  (let ((fork (drover-test-session (format "fork-%d" (cl-incf drover-anvil-task-test-forks))
                                   :engine (drover-session-engine session)
                                   :engine-id (format "conv-fork-%d" drover-anvil-task-test-forks)
                                   :directory (drover-session-directory session))))
    (push (list :fork-hidden (drover-session-id session)) drover-test-calls)
    (setq drover-test-sessions (append drover-test-sessions (list fork)))
    (funcall then fork)))

(defmacro drover-anvil-task-test-with-fork (&rest body)
  "Run BODY with the tools called by the fake session \"me\", which has a conversation.
An Org file holds the fixture's task; the fake backend forks no session
until BODY gives it a :fork-hidden."
  (declare (indent 0) (debug t))
  `(drover-anvil-test-with (list (drover-test-session "me" :engine 'fake :engine-id "conv-me"
                                                      :directory temporary-file-directory)
                                 (drover-test-session "other" :engine 'fake :engine-id "conv-other"
                                                      :directory temporary-file-directory))
       "me"
     (drover-test-with-engines
       (drover-test-register-engine 'fake :directory #'ignore)
       (let ((drover-task--links nil)
             (drover-task--noted (make-hash-table :test #'equal))
             (drover-task--infos (make-hash-table :test #'equal))
             (drover-anvil-task-test-forks 0))
         (drover-task-test-with-org (drover-anvil-task-test-alone)
           ,@body)))))

(ert-deftest drover-anvil-task-test-session-fork-refused ()
  "A session whose harness cannot fork is refused, the harness named; nothing else starts."
  (drover-anvil-task-test-with-fork
    (should (equal (cadr (should-error (drover-anvil--tool-fork)))
                   "fake/me cannot be forked: its harness, Fake, forks no session"))
    ;; Its backend forks, but not this session.
    (drover-anvil-task-test-add :fork-hidden #'drover-anvil-task-test-fork-hidden
                             :supports (lambda (session operation)
                                         (not (and (eq operation :fork-hidden)
                                                   (equal (drover-session-id session) "other")))))
    (should (string-match-p "\\`fake/other cannot be forked: its harness, Fake, cannot fork this session"
                            (cadr (should-error (drover-anvil--tool-fork "fake/other")))))
    ;; A session of another host is its host's to fork.
    (drover-anvil-test-with-peer (list (drover-anvil-test-peer-session "ses_1"))
      (drover-backend-changed 'remote:core)
      (should (string-match-p "\\`remote:core/opencode/ses_1 cannot be forked: its harness, .*, runs on "
                              (cadr (should-error (drover-anvil--tool-fork "remote:core/opencode/ses_1"))))))
    ;; A caller its client does not name names the session.
    (let ((anvil-server-request-meta nil))
      (should (string-match-p "\\`Name the session to fork" (cadr (should-error (drover-anvil--tool-fork))))))
    (should-not (assq :fork-hidden drover-test-calls))
    (should-not (assq :create drover-test-calls))
    (should (seq-find (lambda (line) (string-match-p "drover-session-fork\tcaller=fake/me\tsession=fake/me\trefused" line))
                      (drover-anvil-test-log)))))

(ert-deftest drover-anvil-task-test-session-fork ()
  "The caller's session is forked unshown, linked to a heading, and sent a prompt."
  (drover-anvil-task-test-with-fork
    (drover-anvil-task-test-add :fork-hidden #'drover-anvil-task-test-fork-hidden)
    ;; A heading drover does not find: nothing is forked.
    (should (string-match-p "\\`No heading of ID nowhere in the Org files drover reads: nothing was forked"
                            (cadr (should-error (drover-anvil-task-test-answer (drover-anvil--tool-fork nil "nowhere"))))))
    (should-not (assq :fork-hidden drover-test-calls))
    (let* ((answer (drover-anvil-task-test-answer
                    (drover-anvil--tool-fork nil (format "[[id:%s]]" drover-anvil-task-test-id) "Take the logs on.")))
           (fork (drover-find 'fake "fork-1")))
      (should (equal (alist-get 'outcome answer) "forked"))
      (should (equal (alist-get 'key answer) "fake/fork-1"))
      (should (equal (alist-get 'link answer) "agent-session:fake/conv-fork-1"))
      (should (equal (alist-get 'forked_from answer) "fake/me"))
      (should (equal (alist-get 'task answer) drover-anvil-task-test-id))
      (should (eq (alist-get 'sent answer) t))
      (should (equal (assq :fork-hidden drover-test-calls) '(:fork-hidden "me")))
      (should (equal (assq :send drover-test-calls) '(:send "fork-1" "Take the logs on." nil)))
      ;; Its first prompt is the original's: the link tells its task, and is kept.
      (should (equal (drover-task-id-of fork) drover-anvil-task-test-id))
      (should (file-exists-p (drover-task-links-file)))
      (should-not (drover-task-id-of (drover-find 'fake "me")))
      (should (equal (gethash '(fake . "fork-1") drover-anvil--started) '(fake . "me")))
      (should (gethash '(fake . "fork-1") drover-anvil--sent)))
    ;; Another session, linked to no task and sent nothing.
    (let ((answer (drover-anvil-task-test-answer (drover-anvil--tool-fork "fake/other"))))
      (should (equal (alist-get 'key answer) "fake/fork-2"))
      (should (equal (alist-get 'forked_from answer) "fake/other"))
      (should (eq (alist-get 'sent answer) :false))
      (should-not (assq 'task answer))
      (should-not (drover-task-id-of (drover-find 'fake "fork-2")))
      (should-not (gethash '(fake . "fork-2") drover-anvil--sent)))
    ;; A fork that fails says why.
    (drover-anvil-task-test-add :fork-hidden (lambda (_session _then else) (funcall else "the service refused")))
    (should (string-match-p "the service refused"
                            (cadr (should-error (drover-anvil-task-test-answer (drover-anvil--tool-fork))))))))

(ert-deftest drover-anvil-task-test-session-fork-agent ()
  "A fork runs the agent given, set before its prompt goes; one its harness does not offer is refused."
  (drover-anvil-task-test-with-fork
    (drover-anvil-task-test-add :fork-hidden #'drover-anvil-task-test-fork-hidden)
    (setq drover-test-engine-agents '((fake "build" "plan")))
    ;; Refused, those it offers named: nothing is forked.
    (should (equal (cadr (should-error (drover-anvil-task-test-answer
                                        (drover-anvil--tool-fork nil nil "Take the logs on." "review"))))
                   (format "Fake offers no agent review in %s: it offers build, plan: nothing was forked"
                           (abbreviate-file-name temporary-file-directory))))
    (should-not (assq :fork-hidden drover-test-calls))
    ;; One it offers: the fork is switched to it, then sent its prompt.
    (let ((answer (drover-anvil-task-test-answer
                   (drover-anvil--tool-fork nil drover-anvil-task-test-id "Take the logs on." "plan"))))
      (should (equal (alist-get 'outcome answer) "forked"))
      (should (equal (alist-get 'key answer) "fake/fork-1"))
      (should (equal (alist-get 'agent answer) "plan"))
      (should (eq (alist-get 'sent answer) t))
      (pcase-let ((`(,forked ,applied ,sent)
                   (seq-filter (lambda (call) (memq (car call) '(:fork-hidden :apply-options :send)))
                               (reverse drover-test-calls))))
        (should (equal forked '(:fork-hidden "me")))
        (should (equal (seq-take applied 2) '(:apply-options "fork-1")))
        (should (equal (plist-get (nth 2 applied) :agent) "plan"))
        (should (equal sent '(:send "fork-1" "Take the logs on." nil))))
      (should (equal (drover-task-id-of (drover-find 'fake "fork-1")) drover-anvil-task-test-id)))
    (should (seq-find (lambda (line) (string-match-p "drover-session-fork\t.*\tacting\t.*, agent plan" line))
                      (drover-anvil-test-log)))
    ;; Without one, the fork runs what its session runs: nothing is set.
    (setq drover-test-calls nil)
    (let ((answer (drover-anvil-task-test-answer (drover-anvil--tool-fork nil nil "Go on."))))
      (should-not (assq 'agent answer))
      (should-not (assq :apply-options drover-test-calls)))
    ;; A harness that sets no agent on a running session refuses, before forking.
    (setq drover-test-calls nil
          drover-test-settable '(("me" :model)))
    (should (equal (cadr (should-error (drover-anvil--tool-fork nil nil nil "plan")))
                   "fake/me cannot be forked to run plan: its harness, Fake, sets no agent on a running session: leave agent out"))
    (should-not (assq :fork-hidden drover-test-calls))
    ;; The agent could not be set: the fork exists, is named, and is sent nothing.
    (setq drover-test-settable nil)
    (drover-anvil-task-test-add :apply-options (lambda (_session _options _then else)
                                                 (funcall else "the service refused the agent")))
    (let ((why (cadr (should-error (drover-anvil-task-test-answer
                                    (drover-anvil--tool-fork nil nil "Take the logs on." "build"))))))
      (should (string-match-p "\\`The fork exists, fake/fork-3, but its agent could not be set to build, and it was sent nothing: the service refused the agent"
                              why)))
    (should (assq :fork-hidden drover-test-calls))
    (should-not (assq :send drover-test-calls))))

;;;; Through Anvil

(ert-deftest drover-anvil-task-test-registered ()
  "Both tools are registered with Anvil, under their names and arguments."
  (drover-anvil-test-with (list (drover-test-session "a")) nil
    (drover-anvil-test-with-anvil
      (let* ((list (json-parse-string
                    (anvil-server-process-jsonrpc
                     (json-serialize '((jsonrpc . "2.0") (id . 1) (method . "tools/list")))
                     drover-anvil-server-id)
                    :object-type 'alist :array-type 'list))
             (tools (alist-get 'tools (alist-get 'result list))))
        (pcase-dolist (`(,name ,required . ,arguments)
                       '(("drover-task-start" ("id") "agent" "directory" "host" "id" "repo")
                         ("drover-session-fork" nil "agent" "key" "prompt" "task")))
          (let* ((tool (seq-find (lambda (tool) (equal (alist-get 'name tool) name)) tools))
                 (schema (alist-get 'inputSchema tool)))
            (should tool)
            (should (equal (sort (mapcar (lambda (property) (symbol-name (car property)))
                                         (alist-get 'properties schema))
                                 #'string<)
                           arguments))
            (should (equal (alist-get 'required schema) required)))))
      ;; A refusal reads as the tool's error, the harness named.
      (should (string-match-p "fake/a cannot be forked: its harness, Fake, forks no session"
                              (drover-anvil-test-tool-text
                               (anvil-server-process-jsonrpc
                                (drover-anvil-test-call 2 "drover-session-fork" '((key . "fake/a")))
                                drover-anvil-server-id))))
      (should (string-match-p "No Org ID in"
                              (drover-anvil-test-tool-text
                               (anvil-server-process-jsonrpc
                                (drover-anvil-test-call 3 "drover-task-start" '((id . "no id here")))
                                drover-anvil-server-id)))))))

;;;; Links

(ert-deftest drover-anvil-task-test-links ()
  "The tools telling of a session tell the target of its Org link, once its conversation is known."
  (drover-anvil-test-with (list (drover-test-session "known" :engine 'fake :engine-id "conv-1")
                                (drover-test-session "terminal" :engine 'fake)
                                (drover-test-session "plain"))
      "known"
    (drover-test-with-engines
      (drover-test-register-engine 'fake :directory #'ignore)
      (drover-anvil-test-with-peer (list (drover-anvil-test-peer-session "ses_1" :engine 'opencode :engine-id "ses_1"))
        (drover-backend-changed 'remote:core)
        (let ((links (mapcar (lambda (session) (cons (alist-get 'key session) (alist-get 'link session)))
                             (alist-get 'sessions (drover-anvil-test-json (drover-anvil--tool-sessions))))))
          (should (equal (alist-get "fake/known" links nil nil #'equal) "agent-session:fake/conv-1"))
          ;; As `org-store-link' stores it.
          (should (equal (alist-get "fake/known" links nil nil #'equal)
                         (drover-links-link (drover-links-conversation (drover-find 'fake "known")))))
          ;; A conversation not known yet has no link, though its session has a key.
          (should (assoc "fake/terminal" links))
          (should-not (alist-get "fake/terminal" links nil nil #'equal))
          (should-not (alist-get "fake/plain" links nil nil #'equal))
          ;; Another host's names its host.
          (should (equal (alist-get "remote:core/opencode/ses_1" links nil nil #'equal)
                         "agent-session://core/opencode/ses_1")))
        (should (equal (alist-get 'link (drover-anvil-test-json (drover-anvil--tool-session "fake/known" :json-false)))
                       "agent-session:fake/conv-1"))
        (should-not (assq 'link (drover-anvil-test-json (drover-anvil--tool-session "fake/terminal" :json-false))))
        (should (equal (alist-get 'link (drover-anvil-test-json
                                         (drover-anvil--tool-session "remote:core/opencode/ses_1" :json-false)))
                       "agent-session://core/opencode/ses_1")))
      ;; A session started knows its conversation or not, as its harness does.
      (let ((project (drover-anvil-test-project)))
        (should-not (assq 'link (drover-anvil-test-json (drover-anvil--tool-create nil project))))
        (drover-anvil-task-test-add
         :create (lambda (directory engine _options then _else)
                   (let ((session (drover-test-session "made" :directory directory :engine engine
                                                       :engine-id "conv-made")))
                     (setq drover-test-sessions (append drover-test-sessions (list session)))
                     (funcall then session))))
        (should (equal (alist-get 'link (drover-anvil-test-json (drover-anvil--tool-create nil project)))
                       "agent-session:fake/conv-made"))))))

(provide 'drover-anvil-task-test)
;;; drover-anvil-task-test.el ends here
