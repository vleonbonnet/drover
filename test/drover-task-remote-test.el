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

(ert-deftest drover-task-remote-test-start-and-finish ()
  "Another host starts a task in its repository, then finishes it, as this one asks."
  (drover-remote-test-with nil
    (drover-task-test-with-repo
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
    (drover-task-test-with-repo
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
