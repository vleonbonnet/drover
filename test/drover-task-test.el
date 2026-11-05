;;; drover-task-test.el --- Tests of tasks: headings, worktrees, colours  -*- lexical-binding: t; -*-

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

;; Tasks over repositories made here, a bare repository their origin,
;; and Org files written here, never the user's: worktrees made from a
;; fresh origin, the ignored files `.worktreeinclude' names, setups
;; succeeding and failing, locks, each way of finishing (a local merge,
;; a merge pushed to the bare origin, a pull request through a stub of
;; gh), conflicts, snapshots and dirty worktrees, branches deleted once
;; merged; headings found by ID and ORIG_ID, their projects, what drover
;; writes on them and what it leaves alone; the palette of projects; the
;; list grouping and filtering by task and project.

;;; Code:

(require 'drover-test-helpers)
(require 'org-id)
(require 'drover-task)
(require 'drover-list)

(defvar drover-opencode-database)

;; Never the user's ID locations: neither read, nor written as this Emacs ends.
(setq org-id-locations-file (make-temp-file "drover-task-test-ids")
      org-id-locations (make-hash-table :test #'equal))
(remove-hook 'kill-emacs-hook #'org-id-locations-save)

;;;; Helpers

(defun drover-task-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most."
  (let ((deadline (+ (float-time) (or seconds 30)))
        value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun drover-task-test-call (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and a callback; return what the callback got."
  (let ((box (list 'pending)))
    (apply function (append arguments (list (lambda (value) (setcar box value)))))
    (drover-task-test-wait (lambda () (not (eq (car box) 'pending))) 60)
    (should-not (eq (car box) 'pending))
    (car box)))

(defun drover-task-test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY; return its output, trimmed, or fail."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (zerop (apply #'call-process "git" nil t nil arguments))
        (error "git %s: %s" (string-join arguments " ") (buffer-string)))
      (string-trim (buffer-string)))))

(defun drover-task-test-git-ok-p (directory &rest arguments)
  "Return non-nil when Git with ARGUMENTS succeeds in DIRECTORY."
  (let ((default-directory (file-name-as-directory directory)))
    (zerop (apply #'call-process "git" nil nil nil arguments))))

(defun drover-task-test-commit (directory file content message)
  "Write CONTENT to FILE in DIRECTORY and commit it with MESSAGE."
  (drover-test-write (expand-file-name file directory) content)
  (drover-task-test-git directory "add" "--" file)
  (drover-task-test-git directory "commit" "-q" "-m" message))

(defmacro drover-task-test-with-repo (&rest body)
  "Run BODY with a repository: `origin', bare, and `checkout', its clone.
`root' is the temporary directory holding them, deleted after;
`drover-task-repositories' names the checkout \"test/repo\", whose
worktrees go under root's worktrees/.  Git's identity and defaults are
the test's own, not the user's: its home is root, the current directory
expanded first, as it may be under the user's home."
  (declare (indent 0) (debug t))
  `(let* ((default-directory (expand-file-name default-directory))
          (root (file-name-as-directory (file-truename (make-temp-file "drover-task-test" t))))
          (origin (expand-file-name "origin.git/" root))
          (checkout (expand-file-name "repo/" root))
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
          (org-id-locations (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (drover-task-test-git root "init" "-q" "--bare" "-b" "main" origin)
           (drover-task-test-git root "clone" "-q" origin checkout)
           (drover-task-test-git checkout "config" "pull.rebase" "true")
           (drover-task-test-commit checkout "README" "hello\n" "First")
           (drover-task-test-git checkout "push" "-q" "origin" "main")
           (drover-task-test-git checkout "remote" "set-head" "origin" "main")
           ,@body)
       (drover-test-wait-commands 10)
       (delete-directory root t))))

(defun drover-task-test-plan (checkout &rest more)
  "Return a plan of a task in a worktree of CHECKOUT, with MORE over it."
  (append more
          (list :id "TASK-1" :title "Fix the login" :jira "TASK-1" :slug "TASK-1-fix-the-login"
                :kind 'worktree :repo "test/repo" :entry (drover-task-repository "test/repo")
                :checkout checkout :target "main" :branch-prefix "" :base-choice 'fresh :hidden t)))

(defun drover-task-test-prepare (plan)
  "Make the place of PLAN, waited for; return (PLAN . WHY)."
  (drover-task-test-call #'drover-task-prepare plan))

(defun drover-task-test-worktree (checkout path)
  "Return the entry of `git worktree list' of CHECKOUT for PATH, as a plist."
  (seq-find (lambda (worktree) (drover-task-same-file-p (plist-get worktree :path) path))
            (drover-task-git-parse-worktrees
             (drover-task-test-git checkout "worktree" "list" "--porcelain"))))

;;;; Names and paths

(ert-deftest drover-task-test-slug ()
  "A task's name: its Jira key, then the words of its title, cut between words."
  (should (equal (drover-task-slug "Fix the login redirect, again!" "RNDSEC-12")
                 "RNDSEC-12-fix-the-login-redirect-again"))
  (let ((drover-task-slug-length 20))
    (should (equal (drover-task-slug "Fix the login redirect, again!" "RNDSEC-12")
                   "RNDSEC-12-fix-the-login"))
    (should (equal (drover-task-slug "RNDSEC-12 Onboard Kondukto" "RNDSEC-12") "RNDSEC-12-onboard-kondukto"))
    (should (equal (drover-task-slug "Être à l'heure") "etre-a-l-heure"))
    (should (equal (drover-task-slug "Supercalifragilisticexpialidocious and more") "supercalifragilistic"))
    (should (equal (drover-task-slug "!!!") "task"))))

(ert-deftest drover-task-test-canonical ()
  "One directory, however spelt: aliases, MSYS's /c/, a drive's letter."
  (let ((drover-task-path-aliases '(("/Users/val/Documents/" . "/Users/val/local/"))))
    (should (equal (drover-task-canonical "/Users/val/Documents/source/repo/") "/Users/val/local/source/repo/"))
    (should (drover-task-same-file-p "/Users/val/Documents/source/repo" "/Users/val/local/source/repo/"))
    (should-not (drover-task-same-file-p "/Users/val/Documents/x" "/Users/val/local/y")))
  (let ((system-type 'windows-nt))
    (should (equal (drover-task--native-name "/c/Users/val/repo/") "c:/Users/val/repo/"))
    (should (equal (drover-task--native-name "/C") "c:/"))
    (should (equal (drover-task--native-name "C:/Users/val/repo/") "c:/Users/val/repo/"))
    (should (equal (drover-task--native-name "c:/Users/val/repo/") "c:/Users/val/repo/")))
  (should (equal (drover-task--native-name "/c/Users/") "/c/Users/")))

(ert-deftest drover-task-test-repository-names ()
  "A repository's name from its remote: OWNER/NAME on a forge, its base name else."
  (should (equal (drover-task-git-repository-name "git@github.com:10gen/secateur.git") "10gen/secateur"))
  (should (equal (drover-task-git-repository-name "https://github.com/vleonbonnet/drover") "vleonbonnet/drover"))
  (should (equal (drover-task-git-repository-name "/Volumes/Interceptor/Tech/configuration/emacs") "emacs")))

;;;; Worktrees

(ert-deftest drover-task-test-worktree-from-fresh-origin ()
  "A worktree is made from origin's default branch fetched, on a named branch, locked, marked."
  (drover-task-test-with-repo
    ;; Origin moves on after the checkout was cloned: the task starts from there.
    (let ((other (expand-file-name "other/" root)))
      (drover-task-test-git root "clone" "-q" origin other)
      (drover-task-test-commit other "NEWS" "news\n" "Second")
      (drover-task-test-git other "push" "-q" "origin" "main"))
    (let* ((fresh (drover-task-test-git origin "rev-parse" "main"))
           (made nil)
           (outcome (drover-task-test-call (lambda (plan callback)
                                             (drover-task-prepare plan callback (lambda (plan) (setq made plan))))
                                           (drover-task-test-plan checkout)))
           (plan (car outcome))
           (worktree (plist-get plan :worktree)))
      (should-not (cdr outcome))
      (should made)
      (should (equal (plist-get plan :base) fresh))
      (should (equal (plist-get plan :base-ref) "origin/main"))
      (should (equal (plist-get plan :branch) "TASK-1-fix-the-login"))
      (should (equal worktree (expand-file-name "worktrees/test-repo/TASK-1-fix-the-login/" root)))
      (should (file-exists-p (expand-file-name "NEWS" worktree)))
      (should (equal (drover-task-test-git worktree "rev-parse" "--abbrev-ref" "HEAD") "TASK-1-fix-the-login"))
      (let ((entry (drover-task-test-worktree checkout worktree)))
        (should (string-match-p "\\`drover: task TASK-1 Fix the login" (plist-get entry :locked))))
      (should (equal (drover-task-git-read-marker worktree) '(:id "TASK-1" :setup ok)))
      ;; A second task of the same name gets another.
      (let ((second (car (drover-task-test-prepare (drover-task-test-plan checkout)))))
        (should (equal (plist-get second :branch) "TASK-1-fix-the-login-2"))))))

(ert-deftest drover-task-test-worktree-from-head-without-origin ()
  "Origin out of reach: the task starts from HEAD, and says so."
  (drover-task-test-with-repo
    (drover-task-test-git checkout "remote" "set-url" "origin" (expand-file-name "nowhere.git" root))
    (drover-task-test-commit checkout "LOCAL" "local\n" "Local")
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout)))))
      (should (equal (plist-get plan :base-ref) "HEAD"))
      (should (equal (plist-get plan :base) (drover-task-test-git checkout "rev-parse" "HEAD")))
      (should (string-match-p "based on HEAD" (plist-get plan :base-note))))))

(ert-deftest drover-task-test-worktree-sibling-and-prefix ()
  "A repository's worktrees beside its checkout, its branches prefixed."
  (drover-task-test-with-repo
    (let* ((drover-task-repositories `(("test/repo" :checkout ,checkout :branch-prefix "val.leon/")))
           (plan (car (drover-task-test-prepare
                       (drover-task-test-plan checkout :branch-prefix "val.leon/" :slug "topic"
                                              :placement 'sibling)))))
      (should (equal (plist-get plan :worktree) (expand-file-name "repo-topic/" root)))
      (should (equal (plist-get plan :branch) "val.leon/topic")))))

(ert-deftest drover-task-test-carry-changes ()
  "The checkout's uncommitted changes carried into the worktree, the checkout left as it was."
  (drover-task-test-with-repo
    (drover-test-write (expand-file-name "README" checkout) "changed\n")
    (let ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout :carry t)))))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "README" (plist-get plan :worktree)))
                                       (buffer-string))
                     "changed\n"))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "README" checkout)) (buffer-string))
                     "changed\n")))))

(ert-deftest drover-task-test-worktreeinclude ()
  "Only the files `.worktreeinclude' names that Git ignores are copied, nothing overwritten."
  (drover-task-test-with-repo
    (drover-task-test-commit checkout ".gitignore" ".env\nsecret.txt\nnode_modules/\n" "Ignore")
    (drover-task-test-commit checkout ".worktreeinclude" ".env\nconfig.local\nnode_modules/\n" "Include")
    (drover-task-test-git checkout "push" "-q" "origin" "main")
    (drover-test-write (expand-file-name ".env" checkout) "TOKEN=1\n")
    (drover-test-write (expand-file-name "secret.txt" checkout) "secret\n")
    ;; Named, but not ignored: never copied, it could be committed.
    (drover-test-write (expand-file-name "config.local" checkout) "local\n")
    (drover-test-write (expand-file-name "node_modules/a/index.js" checkout) "module\n")
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (worktree (plist-get plan :worktree)))
      (should (file-exists-p (expand-file-name ".env" worktree)))
      (should (file-exists-p (expand-file-name "node_modules/a/index.js" worktree)))
      (should-not (file-exists-p (expand-file-name "secret.txt" worktree)))
      (should-not (file-exists-p (expand-file-name "config.local" worktree)))
      (should (equal (drover-task-test-git worktree "status" "--porcelain") "")))))

(ert-deftest drover-task-test-setup ()
  "The setup runs in the worktree before any session; a failure says so and starts none."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare
                       (drover-task-test-plan checkout :setup '("echo \"$DROVER_TASK\" > setup-ran" "echo done")))))
           (worktree (plist-get plan :worktree)))
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "setup-ran" worktree))
                                       (buffer-string))
                     "TASK-1\n"))
      (should (eq (plist-get (drover-task-git-read-marker worktree) :setup) 'ok))
      (should (string-match-p "Setup done" (with-current-buffer (plist-get plan :buffer) (buffer-string)))))
    (let* ((outcome (drover-task-test-prepare (drover-task-test-plan checkout :slug "failing"
                                                                     :setup '("echo before" "exit 3" "echo never"))))
           (worktree (plist-get (car outcome) :worktree)))
      (should (string-match-p "\\`setup: exit 3 failed (exit 3)" (cdr outcome)))
      (should (plist-get (car outcome) :made))
      (should (file-directory-p worktree))
      (should (eq (plist-get (drover-task-git-read-marker worktree) :setup) 'failed))
      (should-not (string-match-p "never" (with-current-buffer (plist-get (car outcome) :buffer) (buffer-string)))))))

(ert-deftest drover-task-test-session-started-after-setup ()
  "A session starts in the worktree once made, its first prompt linking the heading."
  (drover-test-with-backend nil
    (drover-task-test-with-repo
      (let* ((drover-task-harness '(:backend fake :engine fake))
             (plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
             (session (drover-task-test-call #'drover-task-start-session plan t)))
        (should (drover-session-p session))
        (should (equal (drover-session-directory session) (plist-get plan :worktree)))
        (pcase-let ((`(:create ,directory fake ,options) (car (last drover-test-calls))))
          (should (equal directory (plist-get plan :worktree)))
          (should (equal (plist-get options :title) "Fix the login")))
        (let ((sent (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls)))
          (should (equal (drover-task-prompt-id (nth 2 sent)) "TASK-1")))
        ;; Serving the task, by what it was started for and by its worktree.
        (should (equal (drover-task-id-of session) "TASK-1"))
        (remhash (drover-session-key session) drover-task--noted)
        (drover-task--changed)
        (should (equal (drover-task-id-of session) "TASK-1"))))))

;;;; Finishing

(defun drover-task-test-finish (plan &rest more)
  "Finish PLAN, with MORE over it, waited for; return the outcome."
  (drover-task-test-call #'drover-task-finish-plan
                         (append more (list :cleanup t :exists t) plan)
                         t))

(ert-deftest drover-task-test-finish-merge ()
  "Merged into the checkout's target, then the worktree and branch gone, a snapshot kept."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (worktree (plist-get plan :worktree))
           (branch (plist-get plan :branch)))
      (drover-task-test-commit worktree "fix.txt" "fixed\n" "Fix it")
      (let ((tip (drover-task-test-git worktree "rev-parse" "HEAD"))
            (outcome (drover-task-test-finish plan :method 'merge)))
        (should-not (plist-get outcome :failure))
        (should (plist-get outcome :removed))
        (should (string-match-p "merged 1 commit into main" (plist-get outcome :note)))
        (should (equal (drover-task-test-git checkout "log" "-1" "--format=%s")
                       "Merge TASK-1-fix-the-login: Fix the login (TASK-1)"))
        (should (file-exists-p (expand-file-name "fix.txt" checkout)))
        (should-not (file-exists-p worktree))
        (should-not (drover-task-test-git-ok-p checkout "rev-parse" "--verify" "--quiet" (concat "refs/heads/" branch)))
        (should (equal (drover-task-test-git checkout "rev-parse" (concat "refs/drover/snapshots/" branch)) tip))))))

(ert-deftest drover-task-test-finish-merge-push ()
  "Merged, checked and pushed to origin; checks failing push nothing."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout :checks '("test -f fix.txt"))))))
      (drover-task-test-commit (plist-get plan :worktree) "fix.txt" "fixed\n" "Fix it")
      (let ((outcome (drover-task-test-finish plan :method 'merge-push)))
        (should-not (plist-get outcome :failure))
        (should (string-match-p "pushed" (plist-get outcome :note)))
        (should (equal (drover-task-test-git origin "rev-parse" "main")
                       (drover-task-test-git checkout "rev-parse" "main")))))
    (let* ((before (drover-task-test-git origin "rev-parse" "main"))
           (plan (car (drover-task-test-prepare (drover-task-test-plan checkout :slug "second"
                                                                       :checks '("test -f nothing-here"))))))
      (drover-task-test-commit (plist-get plan :worktree) "more.txt" "more\n" "More")
      (let ((outcome (drover-task-test-finish plan :method 'merge-push)))
        (should (string-match-p "checks failed, not pushed" (plist-get outcome :failure)))
        (should (equal (drover-task-test-git origin "rev-parse" "main") before))
        ;; Kept: the user fixes it.
        (should (file-directory-p (plist-get plan :worktree)))))))

(ert-deftest drover-task-test-finish-conflicts ()
  "A branch conflicting with its target is found before anything changes."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (worktree (plist-get plan :worktree)))
      (drover-task-test-commit worktree "README" "theirs\n" "Theirs")
      (drover-task-test-commit checkout "README" "ours\n" "Ours")
      (let ((head (drover-task-test-git checkout "rev-parse" "HEAD"))
            (outcome (drover-task-test-finish plan :method 'merge)))
        (should (string-match-p "conflicts with main in README" (plist-get outcome :failure)))
        (should (equal (drover-task-test-git checkout "rev-parse" "HEAD") head))
        (should (file-directory-p worktree))))))

(ert-deftest drover-task-test-finish-dirty-snapshot ()
  "Uncommitted changes the user agreed to lose are kept in the snapshot, the worktree removed."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (worktree (plist-get plan :worktree))
           (branch (plist-get plan :branch)))
      (drover-task-test-commit worktree "started.txt" "started\n" "Started")
      (drover-test-write (expand-file-name "draft.txt" worktree) "draft\n")
      (drover-test-write (expand-file-name "README" worktree) "edited\n")
      (let ((outcome (drover-task-test-finish plan :method nil :dirty '("README" "draft.txt"))))
        (should-not (plist-get outcome :failure))
        (should-not (file-exists-p worktree))
        (let ((ref (concat "refs/drover/snapshots/" branch)))
          (should (equal (drover-task-test-git checkout "show" (concat ref ":draft.txt")) "draft"))
          (should (equal (drover-task-test-git checkout "show" (concat ref ":README")) "edited")))
        ;; Not merged: the branch stays.
        (should (string-match-p (format "branch %s kept" branch) (plist-get outcome :note)))
        (should (drover-task-test-git-ok-p checkout "rev-parse" "--verify" "--quiet" (concat "refs/heads/" branch)))))))

(ert-deftest drover-task-test-finish-refuses-dirty-unconfirmed ()
  "Without the user's word, a dirty worktree is not removed: git refuses, nothing is lost."
  (drover-task-test-with-repo
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (worktree (plist-get plan :worktree)))
      (drover-test-write (expand-file-name "draft.txt" worktree) "draft\n")
      (let ((outcome (drover-task-test-finish plan :method nil)))
        (should (string-match-p "git worktree remove failed" (plist-get outcome :failure)))
        (should (file-exists-p (expand-file-name "draft.txt" worktree)))))))

(defun drover-task-test-gh (root script)
  "Write a stub of gh in ROOT running SCRIPT, a shell script; return its file."
  (let ((file (expand-file-name "bin/gh" root)))
    (drover-test-write file (concat "#!/bin/sh\n" script))
    (set-file-modes file #o755)
    file))

(ert-deftest drover-task-test-finish-pull-request ()
  "Pushed and a pull request opened; cleaned up only once merged, a squashed branch deleted."
  (drover-task-test-with-repo
    (let* ((log (expand-file-name "gh.log" root))
           (drover-task-gh-program
            (drover-task-test-gh root (format "echo \"$@\" >> %s\necho https://github.com/test/repo/pull/7\n"
                                              (shell-quote-argument log))))
           (plan (car (drover-task-test-prepare (drover-task-test-plan checkout :body "The details."))))
           (worktree (plist-get plan :worktree))
           (branch (plist-get plan :branch)))
      (drover-task-test-commit worktree "fix.txt" "fixed\n" "Fix it")
      (let ((outcome (drover-task-test-call #'drover-task-finish-plan
                                            (append (list :method 'pr :cleanup nil :exists t) plan) t)))
        (should (equal (plist-get outcome :pr) "https://github.com/test/repo/pull/7"))
        (should (equal (drover-task-test-git origin "rev-parse" branch) (drover-task-test-git worktree "rev-parse" "HEAD")))
        (should (string-match-p "pr create --base main --head TASK-1-fix-the-login --title Fix the login (TASK-1) --body The details."
                                (with-temp-buffer (insert-file-contents log) (buffer-string))))
        (should (file-directory-p worktree)))
      ;; Squash-merged, its remote branch deleted: Git does not know, the pull request does.
      (drover-task-test-git checkout "push" "-q" "origin" "--delete" branch)
      (drover-task-test-git checkout "branch" "--unset-upstream" branch)
      (let* ((tip (drover-task-test-git worktree "rev-parse" "HEAD"))
             (outcome (drover-task-test-finish (append (list :pr "https://github.com/test/repo/pull/7") plan)
                                               :method 'pr :pr-state (list :state "MERGED" :head tip))))
        (should-not (plist-get outcome :failure))
        (should-not (file-exists-p worktree))
        (should-not (drover-task-test-git-ok-p checkout "rev-parse" "--verify" "--quiet" (concat "refs/heads/" branch)))))))

;;;; Org headings

(defmacro drover-task-test-with-org (content &rest body)
  "Run BODY visiting a fresh Org file of CONTENT, `file', its buffer current.
The ID locations are the test's, empty; the file is deleted after."
  (declare (indent 1) (debug t))
  `(let* ((directory (file-name-as-directory (make-temp-file "drover-task-org" t)))
          (file (expand-file-name "work.org" directory))
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-track-globally t)
          (org-id-locations-file (expand-file-name "id-locations" directory))
          (org-agenda-files nil)
          (org-log-into-drawer t)
          (org-todo-keywords '((sequence "TODO(t!)" "STARTED(s!)" "|" "DONE(d!)" "CANCELED(c!)"))))
     (unwind-protect
         (progn
           (drover-test-write file ,content)
           (with-current-buffer (find-file-noselect file)
             (org-mode)
             ,@body))
       (when-let* ((buffer (find-buffer-visiting file)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory directory t))))

(defconst drover-task-test-ejira
  "#+TITLE: Tracker
* Epic
:PROPERTIES:
:ID:       RNDSEC-1
:TYPE:     ejira-epic
:END:
** TODO [#1] Onboard the scanner
:PROPERTIES:
:ID:       RNDSEC-42
:ORIG_ID:  0B1E5E9C-0000-4000-8000-000000000042
:TYPE:     ejira-task
:Status:   To Do
:LocalHash: abc
:END:
The description, pushed to Jira.
- a checklist item
*** Comments
**** A comment
:PROPERTIES:
:TYPE:     ejira-comment
:END:
Said once.
"
  "An Org file of tasks as ejira writes them.")

(ert-deftest drover-task-test-heading-by-id-and-orig-id ()
  "A heading is found by its ID and by the ID ejira replaced, never by reading every file."
  (drover-task-test-with-org drover-task-test-ejira
    (cl-letf (((symbol-function 'org-id-update-id-locations)
               (lambda (&rest _) (error "Every file read"))))
      (dolist (id '("RNDSEC-42" "0B1E5E9C-0000-4000-8000-000000000042"))
        (let ((marker (drover-task-org-marker id)))
          (should marker)
          (should (equal (org-with-point-at marker (org-get-heading t t t t)) "Onboard the scanner"))))
      (should-not (drover-task-org-marker "NOWHERE-1"))
      ;; Known to the ID locations, unvisited: visited then.
      (puthash "RNDSEC-42" file org-id-locations)
      (let ((info (drover-task-org-read (drover-task-org-marker "RNDSEC-42"))))
        (should (equal (plist-get info :jira) "RNDSEC-42"))
        (should (plist-get info :ejira))
        (should (equal (plist-get info :category) "work"))
        (should (equal (plist-get info :orig-id) "0B1E5E9C-0000-4000-8000-000000000042"))))))

(ert-deftest drover-task-test-heading-found-by-ripgrep ()
  "A heading of an unvisited file the ID locations do not name is found by ripgrep."
  (skip-unless (executable-find "rg"))
  (let* ((directory (file-name-as-directory (make-temp-file "drover-task-org" t)))
         (file (expand-file-name "other.org" directory))
         (org-id-locations (make-hash-table :test #'equal))
         (org-agenda-files (list file)))
    (unwind-protect
        (progn
          (drover-test-write file drover-task-test-ejira)
          (let ((marker (drover-task-test-call #'drover-task-org-find "0B1E5E9C-0000-4000-8000-000000000042")))
            (should marker)
            (should (equal (org-with-point-at marker (org-entry-get nil "ID")) "RNDSEC-42"))))
      (when-let* ((buffer (find-buffer-visiting file))) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest drover-task-test-projects-are-categories ()
  "A task's project is its Org category: the file's name, else #+CATEGORY, else :CATEGORY:."
  (drover-task-test-with-org "#+CATEGORY: secateur\n* TODO A\n:PROPERTIES:\n:ID: a\n:END:\n* B\n:PROPERTIES:\n:CATEGORY: aegis\n:END:\n** TODO C\n:PROPERTIES:\n:ID: c\n:END:\n"
    (should (equal (plist-get (drover-task-org-read (drover-task-org-marker "a")) :category) "secateur"))
    (should (equal (plist-get (drover-task-org-read (drover-task-org-marker "c")) :category) "aegis")))
  (drover-task-test-with-org "* TODO A\n:PROPERTIES:\n:ID: a\n:END:\n"
    (should (equal (plist-get (drover-task-org-read (drover-task-org-marker "a")) :category) "work"))))

(ert-deftest drover-task-test-titles-plain ()
  "A task's title is plain text, its links their descriptions, its name of words alone."
  (drover-task-test-with-org "* TODO Read: [[mu4e:msgid:0100019d3e@x.com][Welcome aboard]] now\n:PROPERTIES:\n:ID: a\n:END:\n"
    (goto-char (point-min))
    (put-text-property (point) (line-end-position) 'line-prefix "**")
    (let ((title (plist-get (drover-task-org-read (drover-task-org-marker "a")) :title)))
      (should (equal title "Read: Welcome aboard now"))
      (should-not (text-properties-at 0 title))
      (should (equal (drover-task-slug title) "read-welcome-aboard-now")))))

(ert-deftest drover-task-test-writes-only-its-own ()
  "Drover writes DROVER_ properties and a LOGBOOK note, and nothing else of the heading."
  (drover-task-test-with-org drover-task-test-ejira
    (let* ((marker (drover-task-org-marker "RNDSEC-42"))
           (before (buffer-string))
           (headings (count-matches "^\\*+ " (point-min) (point-max)))
           (pushing 'unbound))
      (defvar ejira-push-on-save)
      (let ((ejira-push-on-save t))
        (add-hook 'before-save-hook (lambda () (setq pushing ejira-push-on-save)) nil t)
        (should (drover-task-org-write marker '(("DROVER_WORKTREE" . "~/w/x") ("DROVER_BRANCH" . "x"))
                                       "Worktree ~/w/x on branch x.")))
      ;; Saved as a change ejira counts and does not review.
      (should-not pushing)
      (should-not (buffer-modified-p))
      (should (equal (count-matches "^\\*+ " (point-min) (point-max)) headings))
      (org-with-point-at marker
        (should (equal (org-entry-get nil "DROVER_WORKTREE") "~/w/x"))
        (should (equal (org-entry-get nil "Status") "To Do"))
        (should (equal (org-entry-get nil "LocalHash") "abc"))
        (should (equal (org-get-todo-state) "TODO"))
        (should (equal (org-get-heading t t t t) "Onboard the scanner"))
        ;; The note in the drawer, the description after it unchanged.
        (should (equal (drover-task-org-body) "The description, pushed to Jira.\n- a checklist item")))
      (should (string-match-p ":LOGBOOK:\n- Note taken on \\[[^]]+\\] \\\\\\\\\n  Worktree ~/w/x on branch x.\n:END:\nThe description"
                              (buffer-string)))
      (should (equal (with-temp-buffer (insert before)
                                       (goto-char (point-min))
                                       (re-search-forward "^\\*\\*\\* Comments")
                                       (buffer-substring (point) (point-max)))
                     (progn (goto-char (point-min))
                            (re-search-forward "^\\*\\*\\* Comments")
                            (buffer-substring (point) (point-max)))))
      ;; Nothing but its own properties.
      (should-error (drover-task-org-write marker '(("Status" . "Done"))))
      (should-error (drover-task-org-write marker '(("ID" . "X"))))
      ;; Deleting.
      (drover-task-org-write marker '(("DROVER_BRANCH")))
      (should-not (org-entry-get marker "DROVER_BRANCH")))))

(ert-deftest drover-task-test-unsaved-edits-stay-unsaved ()
  "A buffer the user was editing is written but not saved: the user saves it."
  (drover-task-test-with-org "* TODO A\n:PROPERTIES:\n:ID: a\n:END:\n"
    (goto-char (point-max))
    (insert "user's draft\n")
    (should-not (drover-task-org-write (drover-task-org-marker "a") '(("DROVER_HOST" . "test"))))
    (should (buffer-modified-p))
    (should (equal (org-entry-get (drover-task-org-marker "a") "DROVER_HOST") "test"))))

(ert-deftest drover-task-test-refused-files ()
  "Copies of tracked files in linked worktrees, and files refused, are never written."
  (drover-task-test-with-repo
    (drover-task-test-commit checkout "todo.org" "* TODO A\n:PROPERTIES:\n:ID: a\n:END:\n" "Tasks")
    (let* ((plan (car (drover-task-test-prepare (drover-task-test-plan checkout))))
           (copy (expand-file-name "todo.org" (plist-get plan :worktree))))
      (should (drover-task-org-refused-p copy))
      (should-not (drover-task-org-refused-p (expand-file-name "todo.org" checkout)))
      (let ((drover-task-refused-files (list checkout)))
        (should (drover-task-org-refused-p (expand-file-name "todo.org" checkout))))
      (let ((org-id-locations (make-hash-table :test #'equal)))
        (puthash "a" copy org-id-locations)
        (should-not (drover-task-org-marker "a")))
      (when-let* ((buffer (find-buffer-visiting copy))) (kill-buffer buffer)))))

(ert-deftest drover-task-test-repository-resolution ()
  "DROVER_REPO, inherited, first; then the repository of the Org file; then a folder linked."
  (drover-task-test-with-repo
    (let ((drover-task-repositories `(("test/repo" :checkout ,checkout)
                                      ("other/repo" :checkout ,root))))
      (drover-task-test-with-org (format "* Project\n:PROPERTIES:\n:DROVER_REPO: other/repo\n:END:\n** TODO Child\n:PROPERTIES:\n:ID: child\n:END:\n* TODO Alone\n:PROPERTIES:\n:ID: alone\n:END:\n[[file:%s][Project folder]]\n" root)
        (let* ((child (drover-task-org-marker "child"))
               (alone (drover-task-org-marker "alone")))
          (should (equal (car (drover-task--resolve-repository (drover-task-org-read child) child nil)) "other/repo"))
          (should (eq (drover-task--resolve-repository (drover-task-org-read alone) alone nil) 'folder))
          (should (equal (plist-get (drover-task-org-read alone) :folder) root))
          ;; The child's worktree is its own, not inherited.
          (drover-task-org-write (drover-task-org-marker "child") '(("DROVER_WORKTREE" . "/w")))
          (should-not (plist-get (drover-task-org-read (drover-task-org-marker "alone")) :attempts)))))
    ;; An Org file in a repository: that repository.
    (let ((org (expand-file-name "notes.org" checkout)))
      (drover-test-write org "* TODO In repo\n:PROPERTIES:\n:ID: inrepo\n:END:\n")
      (with-current-buffer (find-file-noselect org)
        (unwind-protect
            (let ((marker (drover-task-org-marker "inrepo")))
              (should (equal (car (drover-task--resolve-repository (drover-task-org-read marker) marker nil))
                             "test/repo")))
          (kill-buffer))))))

(ert-deftest drover-task-test-mark-done ()
  "Marked done the normal Org way, as ejira's review then offers the transition."
  (drover-task-test-with-org "* TODO A\n:PROPERTIES:\n:ID: a\n:END:\n"
    (should (equal (drover-task-org-mark-done (drover-task-org-marker "a")) "DONE"))
    (goto-char (point-min))
    (should (looking-at "\\* DONE A"))
    (should-not (buffer-modified-p))))

(ert-deftest drover-task-test-prompt-ids ()
  "The heading a first prompt links, in Org or in Markdown."
  (should (equal (drover-task-prompt-id "Work on [[id:RNDSEC-42][Onboard]]. Plan it.") "RNDSEC-42"))
  (should (equal (drover-task-prompt-id "Work on [Onboard](id:0B1E5E9C-0000-4000-8000-000000000042).")
                 "0B1E5E9C-0000-4000-8000-000000000042"))
  (should-not (drover-task-prompt-id "No link [[file:x][y]] here, id:nothing"))
  (let ((text (drover-task-prompt (list :id "a" :title "Fix [it]" :kind 'folder :directory "/tmp/x/"
                                        :body "Body."))))
    (should (string-prefix-p "Work on [[id:a][Fix it]]." text))
    (should (string-suffix-p "The task:\n\nBody." text))))

;;;; Colours

(defmacro drover-task-test-with-theme (colors &rest body)
  "Run BODY with a theme giving the faces of COLORS, an alist (FACE . COLOR), in force."
  (declare (indent 1) (debug t))
  `(let ((custom-enabled-themes nil)
         (drover-task-palette--current nil)
         (drover-task-palette-faces (mapcar #'car ,colors)))
     (cl-letf (((symbol-function 'face-foreground)
                (lambda (face &rest _) (cdr (assq face ,colors))))
               ((symbol-function 'face-background)
                (lambda (&rest _) "#101010")))
       ,@body)))

(ert-deftest drover-task-test-palette-from-theme ()
  "The palette: the theme's colours, in order, greys, near ones and dark ones left out."
  (drover-task-test-with-theme '((font-lock-keyword-face . "#ff5f5f") (font-lock-string-face . "#5fd75f")
                                 (font-lock-comment-face . "#808080") (font-lock-type-face . "#ff6060")
                                 (font-lock-constant-face . "#202040") (font-lock-function-name-face . "#5f87ff"))
    (should (equal (mapcar #'car (drover-task-palette)) '("#ff5f5f" "#5fd75f" "#5f87ff")))
    ;; More projects than colours: more made, each apart from the others.
    (let ((palette (drover-task-palette 12)))
      (should (= (length palette) 12))
      (should (equal (mapcar #'car (seq-take palette 3)) '("#ff5f5f" "#5fd75f" "#5f87ff")))
      (seq-do-indexed (lambda (entry index)
                        (dotimes (other index)
                          (should (> (drover-task-palette-delta (cdr entry) (cdr (aref palette other))) 0.03))))
                      palette)
      ;; The same, made again.
      (setq drover-task-palette--current nil)
      (should (equal (mapcar #'car (drover-task-palette 12)) (mapcar #'car palette))))))

(ert-deftest drover-task-test-slots-stable ()
  "A project keeps its slot, kept across Emacs sessions; another theme colours it alike."
  (drover-test-with-state
    (let ((drover-task-palette--slots nil))
      (should (= (drover-task-palette-slot "secateur") 0))
      (should (= (drover-task-palette-slot "aegis") 1))
      (should (= (drover-task-palette-slot "secateur") 0))
      (setq drover-task-palette--slots nil)
      (should (= (drover-task-palette-slot "aegis") 1))
      (should (= (drover-task-palette-slot "emacs") 2))
      (drover-task-test-with-theme '((font-lock-keyword-face . "#ff5f5f") (font-lock-string-face . "#5fd75f"))
        (should (equal (drover-task-project-color "secateur") "#ff5f5f"))
        (should (equal (drover-task-project-color "aegis") "#5fd75f")))
      (drover-task-test-with-theme '((font-lock-keyword-face . "#0000ff") (font-lock-string-face . "#ffaa00"))
        (should (equal (drover-task-project-color "secateur") "#0000ff"))
        (should (equal (drover-task-project-color "aegis") "#ffaa00"))))))

;;;; Lists

(ert-deftest drover-task-test-list-groups-and-filters ()
  "Sessions grouped by task and by its project, filtered by project, its pill in its colour."
  (drover-test-with-state
    (drover-test-with-backend (list (drover-test-session "a" :directory "/tmp/a/" :project "repo")
                                    (drover-test-session "b" :directory "/tmp/b/" :project "repo")
                                    (drover-test-session "c" :directory "/tmp/c/" :project "repo"))
      (let ((drover-task--noted (make-hash-table :test #'equal))
            (drover-task--links (make-hash-table :test #'equal))
            (drover-task--infos (make-hash-table :test #'equal))
            (drover-task-palette--slots (make-hash-table :test #'equal))
            (drover-task-mode t)
            (drover-session-task-function #'drover-task--session-task))
        (drover-task-test-with-theme '((font-lock-keyword-face . "#ff5f5f") (font-lock-string-face . "#5fd75f"))
          (drover-task-note-session '(fake . "a") "T1")
          (drover-task-note-session '(fake . "b") "T2")
          (dolist (entry '(("T1" "Fix the login" "secateur") ("T2" "Onboard" "aegis")))
            (puthash (car entry) (list :id (car entry) :title (nth 1 entry) :category (nth 2 entry)
                                       :tick (cons (current-buffer) (buffer-chars-modified-tick)))
                     drover-task--infos)
            (drover-task--take (car entry) (gethash (car entry) drover-task--infos)))
          (let* ((sessions (drover-sessions))
                 (a (seq-find (lambda (s) (equal (drover-session-id s) "a")) sessions))
                 (c (seq-find (lambda (s) (equal (drover-session-id s) "c")) sessions)))
            (should (equal (drover-session-view-project a) "secateur"))
            (should (equal (drover-session-view-project c) "repo"))
            (should (equal (plist-get (drover-session-task a) :face) '(:foreground "#ff5f5f")))
            (let ((drover-group-by 'task))
              (should (equal (mapcar #'car (drover-view-groups))
                             '((task "T1" . "Fix the login") (task "T2" . "Onboard") (task)))))
            (let ((drover-group-by 'project))
              (should (equal (sort (mapcar #'cdar (drover-view-groups)) #'string<) '("aegis" "repo" "secateur"))))
            (let ((drover-filter '(:project "aegis")))
              (should (equal (mapcar (lambda (node) (drover-session-id (drover-node-session node)))
                                     (cdar (drover-view-groups)))
                             '("b"))))
            (should (equal (drover-group-label 'task '("T1" . "Fix the login")) "Fix the login"))
            (should (equal (drover-group-label 'task nil) "No task"))
            ;; The pill in the project's colour, kept alone where room is short.
            (let ((text (drover-list--location-text a "secateur")))
              (should (string-match-p "secateur" text))
              (should (drover-list--kept-head text))
              (should (equal (get-text-property 0 'font-lock-face text) '(:foreground "#ff5f5f"))))
            (should (equal (substring-no-properties (drover-list--location-text c "repo")) "repo"))
            (should-not (drover-list--kept-head (drover-list--location-text c "repo"))))
          ;; Another theme: the same slots, its colours.
          (cl-letf (((symbol-function 'face-foreground)
                     (lambda (face &rest _) (cdr (assq face '((font-lock-keyword-face . "#0000ff")
                                                               (font-lock-string-face . "#ffaa00")))))))
            (drover-task--theme-changed)
            (should (equal (plist-get (drover-task-view "T1") :face) '(:foreground "#0000ff")))
            (should (equal (plist-get (drover-task-view "T2") :face) '(:foreground "#ffaa00")))))))))

(ert-deftest drover-task-test-links-by-hand ()
  "A session linked by hand serves its task, across Emacs sessions; unlinked, no longer."
  (drover-test-with-state
    (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_a"))
      (let ((drover-task--links nil)
            (drover-task--noted (make-hash-table :test #'equal))
            (session (car (drover-sessions))))
        (should-not (drover-task-id-of session))
        (drover-task-link session "T1")
        (setq drover-task--links nil)
        (should (equal (drover-task-id-of session) "T1"))
        (drover-task-link session nil)
        (setq drover-task--links nil)
        (should-not (drover-task-id-of session))))))

(ert-deftest drover-task-test-opencode-first-prompts ()
  "OpenCode's sessions serve the task their first prompt links, as its database tells."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let* ((directory (make-temp-file "drover-task-oc" t))
         (drover-opencode-database (expand-file-name "opencode.db" directory))
         (drover-task--oc-prompts (make-hash-table :test #'equal))
         (drover-task--oc-read nil)
         (drover-task--prompts (make-hash-table :test #'eq :weakness 'key))
         (now (* 1000 (floor (float-time)))))
    (unwind-protect
        (let ((db (sqlite-open drover-opencode-database)))
          (sqlite-execute db "CREATE TABLE session_message (id text PRIMARY KEY, session_id text, type text, seq integer, time_created integer, time_updated integer, data text)")
          (pcase-dolist (`(,id ,session ,type ,seq ,text)
                         `(("m1" "ses_a" "user" 1 "Work on [[id:T1][Fix]]. Plan it.")
                           ("m2" "ses_a" "user" 3 "Then [[id:T2][other]]")
                           ("m3" "ses_b" "user" 1 "Hello")
                           ("m4" "ses_b" "user" 2 "see [[id:T3][x]]")
                           ("m5" "ses_c" "assistant" 1 "[[id:T4][x]]")))
            (sqlite-execute db "INSERT INTO session_message VALUES (?, ?, ?, ?, ?, ?, ?)"
                            (list id session type seq now now (json-encode `((text . ,text))))))
          (sqlite-close db)
          (drover-task--oc-read)
          (should (equal (gethash "ses_a" drover-task--oc-prompts) "T1"))
          (should-not (gethash "ses_b" drover-task--oc-prompts))
          (should-not (gethash "ses_c" drover-task--oc-prompts))
          (let ((session (drover-session-create :backend 'opencode :id "ses_a" :engine 'opencode :engine-id "ses_a")))
            (should (equal (drover-task--prompt-id session) "T1"))))
      (delete-directory directory t))))

;;;; From a heading to the end

(defconst drover-task-test-agents
  "# Agents\n\nWhen a task is done, merge its branch into main locally. Never push.\n\nRun `sh check.sh` before finishing.\n"
  "The AGENTS.md of the fixture's repository.")

(defun drover-task-test-model (_prompt _system callback)
  "Answer as a model reading `drover-task-test-agents', for CALLBACK."
  (funcall callback
           (list :text (concat "{\"checks\": [{\"command\": \"sh check.sh\", \"evidence\": \"Run `sh check.sh` before finishing.\"}],"
                               " \"setup\": [], \"finish\": {\"method\": \"merge\", \"evidence\": \"When a task is done, merge its branch into main locally. Never push.\"},"
                               " \"worktree\": {\"placement\": \"\", \"evidence\": \"\"}}"))))

(ert-deftest drover-task-test-how-from-the-repository ()
  "How a task works is the repository's, its heading overriding it; changes go on the heading."
  (drover-task-test-with-repo
    (drover-task-test-commit checkout "AGENTS.md" drover-task-test-agents "Agents")
    (let ((drover-repo--cache 'unread)
          (drover-repo-generate-function #'drover-task-test-model))
      (drover-task-test-with-org "* Work\n:PROPERTIES:\n:DROVER_REPO: test/repo\n:DROVER_SETUP: make deps\n:END:\n** TODO Fix\n:PROPERTIES:\n:ID: fix\n:END:\n"
        (let* ((marker (drover-task-org-marker "fix"))
               (answers (list "Finish otherwise for this task" "pr" "Accept")))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers))))
            (let ((how (drover-task-how marker (drover-task-read marker "fix") checkout)))
              (should (eq (plist-get how :finish) 'pr))
              (should (equal (plist-get how :setup) '("make deps")))
              (should (equal (plist-get how :checks) '("sh check.sh")))
              (should-not (plist-get how :placement))))
          ;; Written on the heading, for this task alone; the parent's setup inherited.
          (org-with-point-at marker
            (should (equal (org-entry-get nil "DROVER_FINISH") "pr")))
          (should-not (org-entry-get (point-min) "DROVER_FINISH"))
          ;; Confirmed once: not asked again until the instructions change.
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Asked again"))))
            (should (eq (plist-get (drover-task-how marker (drover-task-read marker "fix") checkout) :finish) 'pr)))
          (drover-task-test-commit checkout "AGENTS.md" (concat drover-task-test-agents "\nMore.\n") "More")
          (let ((asked nil))
            (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (setq asked t) "Accept")))
              (drover-task-how marker (drover-task-read marker "fix") checkout))
            (should asked)))))))

(ert-deftest drover-task-test-overrides-parsed ()
  "A heading's overrides read as drover takes them; what does not parse is left out."
  (should (equal (drover-task--overrides '(:finish "Merge-Push" :placement "sibling" :checks "a && b"
                                          :target "release/1.0" :teardown "x"))
                 '(:finish merge-push :placement sibling :checks ("a" "b") :teardown ("x") :target "release/1.0")))
  (should-not (drover-task--overrides '(:finish "squash" :placement "elsewhere" :target "a b")))
  (should (equal (drover-task--how (list :finish '(:method pr :source "x") :checks '((:command "c")))
                                   '(:finish none))
                 '(:finish nil :placement nil :setup nil :checks ("c") :teardown nil :target nil)))
  (should (equal (drover-task--message-overrides '(:finish-method "merge" :check-commands "make test" :setup-commands :false))
                 '(:finish merge :checks ("make test")))))

(ert-deftest drover-task-test-start-and-finish-from-heading ()
  "A heading's task started, continued and finished: its worktree, sessions, notes and state."
  (drover-test-with-backend nil
    (drover-task-test-with-repo
      (drover-task-test-commit checkout "AGENTS.md" drover-task-test-agents "Agents")
      (drover-task-test-git checkout "push" "-q" "origin" "main")
      (let ((drover-task-harness '(:backend fake :engine fake))
            (drover-task-mode t)
            (drover-session-task-function #'drover-task--session-task)
            (drover-repo--cache 'unread)
            (drover-repo-generate-function #'drover-task-test-model)
            (asked nil))
        (drover-task-test-with-org "* Work\n:PROPERTIES:\n:DROVER_REPO: test/repo\n:END:\n** TODO Fix the login\n:PROPERTIES:\n:ID: 5A0E0000-0000-4000-8000-000000000001\n:END:\nIt fails after the redirect.\n"
          (goto-char (point-min))
          (re-search-forward "Fix the login")
          ;; How the repository works is shown once, and accepted.
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt &rest _) (setq asked prompt) "Accept")))
            (drover-task-start))
          (should (string-match-p "Finish: merged into the main branch here, not pushed — AGENTS.md: “When a task is done" asked))
          (should (string-match-p "Checks:\n  sh check.sh  — AGENTS.md" asked))
          (should (drover-task-test-wait (lambda () (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls))))
          (let* ((marker (drover-task-org-marker "5A0E0000-0000-4000-8000-000000000001"))
                 (worktree (expand-file-name "worktrees/test-repo/fix-the-login/" root))
                 (session (car drover-test-sessions)))
            (org-with-point-at marker
              (should (equal (org-entry-get nil "DROVER_WORKTREE") (abbreviate-file-name worktree)))
              (should (equal (org-entry-get nil "DROVER_BRANCH") "fix-the-login"))
              (should (equal (org-entry-get nil "DROVER_HOST") "test"))
              (should (equal (org-entry-get nil "DROVER_BASE") (drover-task-test-git origin "rev-parse" "main")))
              ;; Not inherited from the parent, nor written there.
              (should-not (org-entry-get (point-min) "DROVER_WORKTREE")))
            (should (string-match-p "- Note taken on .*\n +Worktree .*fix-the-login/ on branch fix-the-login, from origin/main"
                                    (buffer-string)))
            (should-not (buffer-modified-p))
            (should (equal (drover-session-directory session) worktree))
            (let ((sent (nth 2 (seq-find (lambda (call) (eq (car call) :send)) drover-test-calls))))
              (should (string-match-p "id:5A0E0000-0000-4000-8000-000000000001" sent))
              (should (string-match-p "It fails after the redirect." sent)))
            ;; The list shows it with its task.
            (should (equal (plist-get (drover-task--session-task session) :title) "Fix the login"))
            (should (equal (plist-get (drover-task--session-task session) :project) "work"))
            ;; Continued: another session in the same worktree.
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _) (seq-find (lambda (choice) (string-prefix-p "New session" choice))
                                                                   (all-completions "" choices)))))
              (goto-char (point-max))
              (drover-task-start))
            (should (drover-task-test-wait (lambda () (= (length drover-test-sessions) 2))))
            (should (equal (drover-session-directory (nth 1 drover-test-sessions)) worktree))
            (should (= (length (drover-task-sessions (drover-task-read marker))) 2))
            ;; Finished: merged, cleaned up, the sessions archived, the heading done.
            (drover-task-test-commit worktree "fix.txt" "fixed\n" "Fix it")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (drover-task-finish))
            (should (drover-task-test-wait (lambda () (not (file-exists-p worktree)))))
            (should (drover-task-test-wait (lambda () (org-with-point-at marker (equal (org-get-todo-state) "DONE")))))
            (org-with-point-at marker
              (should-not (org-entry-get nil "DROVER_WORKTREE"))
              (should-not (org-entry-get nil "DROVER_BRANCH")))
            (should (string-match-p "merged 1 commit into main" (buffer-string)))
            (should (equal (drover-task-test-git checkout "log" "-1" "--format=%s")
                           "Merge fix-the-login: Fix the login"))
            (should (= (length (seq-filter (lambda (call) (eq (car call) :archive)) drover-test-calls)) 2))))))))

(provide 'drover-task-test)
;;; drover-task-test.el ends here
