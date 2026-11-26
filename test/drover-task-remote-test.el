;;; drover-task-remote-test.el --- Tests of tasks of other hosts  -*- lexical-binding: t; -*-

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

;; A task of another host, started and finished by that host for this
;; one: a second batch Emacs over 127.0.0.1 (test/drover-remote-agent.el)
;; owns a repository made here, its worktrees and its sessions.

;;; Code:

(require 'drover-remote-test)
(require 'drover-task-test)

(defun drover-task-remote-test-ask (request &optional seconds)
  "Ask the agent REQUEST about a task; return (REPLY ERROR), waited for."
  (drover-task-test-call (lambda (callback)
                           (drover-remote-ask-task "agent" request
                                                   (lambda (reply error) (funcall callback (list reply error)))
                                                   (or seconds 60)))))

(defun drover-task-remote-test-start (slug)
  "Have the agent start the task of SLUG; return its reply, checked."
  (pcase-let ((`(,reply ,error)
               (drover-task-remote-test-ask (list :kind "start" :task (upcase slug) :title (concat "Task " slug)
                                                  :repo "test/repo" :mode "worktree" :slug slug :jira :false
                                                  :base "fresh" :carry :false :body "Do it."))))
    (should-not error)
    (should-not (plist-get reply :failure))
    reply))

(defmacro drover-task-remote-test-with-repo (&rest body)
  "Run BODY as `drover-task-test-with-repo' does, the agent stopped before the repository goes.
The agent works in the repository, and Windows removes no directory a
process still uses."
  (declare (indent 0) (debug t))
  `(drover-task-test-with-repo
     (unwind-protect (progn ,@body)
       (drover-remote-test-stop-agent))))

(ert-deftest drover-task-remote-test-start-and-finish ()
  "Another host starts a task in its repository, then finishes it, as this one asks."
  (drover-remote-test-with nil
    (drover-task-remote-test-with-repo
      (let ((process-environment (append (list "DROVER_AGENT_ACCESS=full" (concat "DROVER_AGENT_TASKS=" checkout))
                                         process-environment)))
        (drover-remote-test-connect (drover-remote-test-start-agent dir)))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'full)))
      (let* ((reply (drover-task-remote-test-start "t1-fix"))
             (worktree (plist-get reply :worktree)))
        (should (equal worktree (expand-file-name "agent/worktrees/test-repo/t1-fix/" dir)))
        (should (equal (plist-get reply :branch) "t1-fix"))
        (should (equal (plist-get reply :base) (drover-task-test-git origin "rev-parse" "main")))
        (should (file-directory-p worktree))
        (should (equal (plist-get (drover-task-git-read-marker worktree) :id) "T1-FIX"))
        ;; Its session there, in no window, sent the task's prompt.
        (should (vectorp (plist-get reply :key)))
        (should (equal (car (car (drover-remote-test-calls "create"))) "create"))
        (should (equal (nth 1 (car (drover-remote-test-calls "create"))) worktree))
        (should (string-match-p "id:T1-FIX" (nth 2 (car (drover-remote-test-calls "prompt")))))
        ;; Finished there: merged in its checkout, cleaned up.
        (drover-task-test-commit worktree "fix.txt" "fixed\n" "Fix it")
        (pcase-let ((`(,finished ,error)
                     (drover-task-remote-test-ask (list :kind "finish" :task "T1-FIX" :title "Task t1-fix"
                                                        :repo "test/repo" :worktree worktree :directory worktree
                                                        :branch "t1-fix" :jira :false :pr :false :discard :false
                                                        ;; As the task's heading says: the repository says nothing.
                                                        :finish-method "merge"))))
          (should-not error)
          (should-not (plist-get finished :failure))
          (should (string-match-p "merged 1 commit into main" (plist-get finished :note)))
          (should (eq (plist-get finished :removed) t))
          (should-not (file-exists-p worktree))
          (should (equal (drover-task-test-git checkout "log" "-1" "--format=%s") "Merge t1-fix: Task t1-fix"))))
      ;; Uncommitted changes are told back, and lost only once this host says so.
      (let* ((reply (drover-task-remote-test-start "t2-draft"))
             (worktree (plist-get reply :worktree))
             (request (list :kind "finish" :task "T2-DRAFT" :title "Task t2-draft" :repo "test/repo"
                            :worktree worktree :directory worktree :branch "t2-draft"
                            :jira :false :pr :false :discard :false)))
        (drover-test-write (expand-file-name "draft.txt" worktree) "draft\n")
        (pcase-let ((`(,finished ,_) (drover-task-remote-test-ask request)))
          (should (equal (plist-get finished :dirty) "draft.txt"))
          (should (file-exists-p (expand-file-name "draft.txt" worktree))))
        (pcase-let ((`(,finished ,_) (drover-task-remote-test-ask (plist-put (copy-sequence request) :discard t))))
          (should-not (plist-get finished :failure))
          (should-not (file-exists-p worktree))
          (should (equal (drover-task-test-git checkout "show" "refs/drover/snapshots/t2-draft:draft.txt") "draft"))))
      ;; Never a worktree of another task, nor a directory of no repository it knows.
      (pcase-let ((`(,reply ,error)
                   (drover-task-remote-test-ask (list :kind "finish" :task "T9" :title "x" :repo "test/repo"
                                                      :worktree checkout :directory checkout :branch "main"
                                                      :jira :false :pr :false :discard t))))
        (should-not reply)
        (should (string-match-p "serves no task T9" error)))
      (pcase-let ((`(,reply ,error)
                   (drover-task-remote-test-ask (list :kind "start" :task "T3" :title "x" :repo "other/repo"
                                                      :mode "worktree" :slug "t3" :base "fresh"))))
        (should-not reply)
        (should (string-match-p "no repository other/repo" error))))))

(ert-deftest drover-task-remote-test-access ()
  "A peer of act access starts tasks but finishes none; one of read access does neither."
  (drover-remote-test-with nil
    (drover-task-remote-test-with-repo
      (let ((process-environment (append (list "DROVER_AGENT_ACCESS=act" (concat "DROVER_AGENT_TASKS=" checkout))
                                         process-environment)))
        (drover-remote-test-connect (drover-remote-test-start-agent dir)))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'act)))
      (let ((worktree (plist-get (drover-task-remote-test-start "t1") :worktree)))
        (pcase-let ((`(,reply ,error)
                     (drover-task-remote-test-ask (list :kind "finish" :task "T1" :title "x" :repo "test/repo"
                                                        :worktree worktree :directory worktree :branch "t1"
                                                        :jira :false :pr :false :discard t))))
          (should-not reply)
          (should (string-match-p "act access, which finishes no task" error))
          (should (file-directory-p worktree)))))))

(ert-deftest drover-task-remote-test-start-under-a-goal ()
  "Another host starts a task's session under a goal it runs, and another session in its place; a goal takes full access."
  (require 'drover-remote-goal)
  (drover-remote-test-with nil
    (drover-task-remote-test-with-repo
      (with-environment-variables (("DROVER_AGENT_ACCESS" "full") ("DROVER_AGENT_TASKS" checkout)
                                   ("DROVER_AGENT_LINKABLE" "1"))
        (drover-remote-test-connect (drover-remote-test-start-agent dir)))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'full)))
      (let* ((goal (drover-remote-goal-wire-properties (list :commands '("test -f README") :sources '("given")
                                                             :baseline t :task "T4-GOAL")))
             (reply (pcase-let ((`(,reply ,error)
                                 (drover-task-remote-test-ask
                                  (list :kind "start" :task "T4-GOAL" :title "Task t4-goal" :repo "test/repo"
                                        :mode "worktree" :slug "t4-goal" :jira :false :base "fresh" :carry :false
                                        :body "Do it." :goal goal))))
                      (should-not error)
                      (should-not (plist-get reply :failure))
                      reply))
             (worktree (plist-get reply :worktree)))
        (should (stringp (plist-get reply :goal)))
        (should (vectorp (plist-get reply :key)))
        ;; Its first prompt tells the task, then the goal.
        (let ((prompt (nth 2 (car (drover-remote-test-calls "prompt")))))
          (should (string-match-p "id:T4-GOAL" prompt))
          (should (string-search "test -f README" prompt)))
        ;; The goal is the agent's, for this host's task, its baseline run there.
        (let ((there (seq-find (lambda (entry) (equal (aref entry 0) (plist-get reply :goal)))
                               (plist-get (drover-remote-test-command "goals") :goals))))
          (should there)
          (should (equal (aref there 3) "T4-GOAL"))
          (should (eq (aref there 4) :false))
          (should (member (aref there 1) '("setup" "waiting"))))
        ;; Another session in its place, under a goal too.
        (pcase-let ((`(,again ,error)
                     (drover-task-remote-test-ask (list :kind "session" :task "T4-GOAL" :title "Task t4-goal"
                                                        :repo "test/repo" :worktree worktree :in-place :false
                                                        :branch "t4-goal" :slug "t4-goal" :jira :false
                                                        :body "Again." :goal goal))))
          (should-not error)
          (should (stringp (plist-get again :goal)))
          (should (equal (nth 1 (car (drover-remote-test-calls "create"))) worktree)))
        ;; Never in the place of another task, nor without one.
        (pcase-let ((`(,reply ,error)
                     (drover-task-remote-test-ask (list :kind "session" :task "T9" :title "x" :repo "test/repo"
                                                        :worktree worktree :in-place :false))))
          (should-not reply)
          (should (string-match-p "serves no task T9" error)))
        (pcase-let ((`(,reply ,error)
                     (drover-task-remote-test-ask (list :kind "session" :task "T4-GOAL" :title "x" :repo "test/repo"
                                                        :worktree :false :in-place :false))))
          (should-not reply)
          (should (string-match-p "no place named" error)))))))

(ert-deftest drover-task-remote-test-goal-takes-full-access ()
  "A peer of act access starts a task's session, but none under a goal."
  (drover-remote-test-with nil
    (drover-task-remote-test-with-repo
      (with-environment-variables (("DROVER_AGENT_ACCESS" "act") ("DROVER_AGENT_TASKS" checkout))
        (drover-remote-test-connect (drover-remote-test-start-agent dir)))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'act)))
      (pcase-let ((`(,reply ,error)
                   (drover-task-remote-test-ask (list :kind "start" :task "T5" :title "x" :repo "test/repo"
                                                      :mode "worktree" :slug "t5" :base "fresh"
                                                      :goal (list :commands ["true"])))))
        (should-not reply)
        (should (string-match-p "act access: a goal runs its checks here, which takes full" error))))))

(ert-deftest drover-task-remote-test-agent ()
  "Another host starts a task's session with the agent asked, and refuses one its harness does not offer."
  (drover-remote-test-with nil
    (drover-task-remote-test-with-repo
      (with-environment-variables (("DROVER_AGENT_ACCESS" "act") ("DROVER_AGENT_TASKS" checkout))
        (drover-remote-test-connect (drover-remote-test-start-agent dir)))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'act)))
      (should (drover-remote--capable-p (drover-remote--peer "agent") "task-agent"))
      (cl-flet ((start (slug agent)
                  (drover-task-remote-test-ask (list :kind "start" :task (upcase slug) :title (concat "Task " slug)
                                                     :repo "test/repo" :mode "worktree" :slug slug :jira :false
                                                     :base "fresh" :carry :false :body "Do it." :agent agent)))
                (created ()
                  (mapcar (lambda (call) (list (nth 1 call) (nth 2 call))) (drover-remote-test-calls "create"))))
        ;; One its harness there does not offer: refused, those it offers named, nothing made.
        (pcase-let ((`(,reply ,error) (start "t1" "review")))
          (should-not reply)
          (should (string-match-p "\\`Fake offers no agent review in .*: it offers build, plan" error))
          (should-not (file-exists-p (expand-file-name "agent/worktrees/test-repo/t1/" dir)))
          (should-not (created)))
        ;; A name of no known shape.
        (pcase-let ((`(,reply ,error) (start "t1" (make-string 80 ?a))))
          (should-not reply)
          (should (string-match-p "no agent of a known shape" error)))
        ;; One it offers: its session runs it.
        (pcase-let* ((`(,reply ,error) (start "t1" "plan"))
                     (worktree (plist-get reply :worktree)))
          (should-not error)
          (should (file-directory-p worktree))
          (should (seq-find (lambda (call) (and (equal (car call) worktree) (string-prefix-p "fake plan " (cadr call))))
                            (created)))
          ;; Another session in its place, with its agent, or refused.
          (cl-flet ((session (agent)
                      (drover-task-remote-test-ask (list :kind "session" :task "T1" :title "Task t1" :repo "test/repo"
                                                         :worktree worktree :in-place :false :branch "t1"
                                                         :jira :false :slug "t1" :body "Do it." :agent agent))))
            (pcase-let ((`(,reply ,error) (session "review")))
              (should-not reply)
              (should (string-match-p "\\`Fake offers no agent review in .*: it offers build, plan" error)))
            (pcase-let ((`(,reply ,error) (session "build")))
              (should-not error)
              (should (vectorp (plist-get reply :key)))
              (should (seq-find (lambda (call) (and (equal (car call) worktree)
                                                    (string-prefix-p "fake build " (cadr call))))
                                (created))))))))))

(ert-deftest drover-task-remote-test-agent-older-peer ()
  "A peer whose drover reads no agent of a task is not asked for one, and says why."
  (drover-remote-test-with nil
    (with-environment-variables (("DROVER_AGENT_ACCESS" "full")
                                 ("DROVER_AGENT_CAPS" "actions,bye,grant,mirror,unsent,search,clone,new,task"))
      (drover-remote-test-connect (drover-remote-test-start-agent dir)))
    (pcase-let ((`(,reply ,error) (drover-task-remote-test-ask (list :kind "start" :agent "plan"))))
      (should-not reply)
      (should (string-match-p "older drover, whose tasks run its own agent: leave agent out" error)))))

(ert-deftest drover-task-remote-test-older-peer ()
  "A peer whose drover has no tasks is not asked, and says why."
  (drover-remote-test-with nil
    (let ((process-environment (append '("DROVER_AGENT_ACCESS=full"
                                         "DROVER_AGENT_CAPS=actions,bye,grant,mirror,unsent,search,clone,new")
                                       process-environment)))
      (drover-remote-test-connect (drover-remote-test-start-agent dir)))
    (pcase-let ((`(,reply ,error) (drover-task-remote-test-ask (list :kind "start"))))
      (should-not reply)
      (should (string-match-p "older drover, which has no tasks" error)))))

(provide 'drover-task-remote-test)
;;; drover-task-remote-test.el ends here
