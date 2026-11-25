;;; drover-remote-goal-test.el --- Tests of goals of other hosts' sessions  -*- lexical-binding: t; -*-

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

;; A goal set here on a session of another host runs there: a second
;; batch Emacs over 127.0.0.1 (test/drover-remote-agent.el) owns the
;; session, in a repository made here, runs the goal's checks for real
;; and tells its goals as they change.  The session's turns are played
;; by changing its record there.

;;; Code:

(require 'drover-remote-test)
(require 'drover-task-test)
(require 'drover-remote-goal)

(defmacro drover-remote-goal-test-with (access &rest body)
  "Run BODY with the agent of ACCESS owning session s0, in `checkout'.
This host's goals are its own, in a fresh state directory, and those
the agent tells start unknown."
  (declare (indent 1) (debug t))
  `(drover-remote-test-with nil
     (drover-task-test-with-repo
       (drover-test-with-state
         (let ((drover-goal--table nil)
               (drover-remote-goal--peers (make-hash-table :test #'equal))
               (drover-remote-goal--states (make-hash-table :test #'equal)))
           (unwind-protect
               (progn
                 (with-environment-variables (("DROVER_AGENT_ACCESS" ,access) ("DROVER_AGENT_LINKABLE" "1"))
                   (drover-remote-test-connect (drover-remote-test-start-agent dir)))
                 (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent"))
                                                         (intern ,access))))
                 (drover-remote-test-command "sessions" :n 1)
                 (drover-remote-test-command "change" :ids ["s0"] :status "idle" :directory checkout :now t)
                 (drover-remote-test-synced 1 (lambda (session) (equal (drover-session-directory session) checkout)))
                 ,@body)
             (drover-goal-mode -1)
             (drover-remote-test-stop-agent)))))))

(defun drover-remote-goal-test-session ()
  "Return this host's record of the agent's session s0."
  (drover-remote-test-session "s0"))

(defun drover-remote-goal-test-turn ()
  "Play a turn of the agent's session s0, ending idle."
  (drover-remote-test-command "change" :ids ["s0"] :status "working" :now t)
  (drover-remote-test-settle 0.2)
  (drover-remote-test-command "change" :ids ["s0"] :status "idle" :now t))

(defun drover-remote-goal-test-state (goal)
  "Return the state of GOAL as the agent told it last."
  (plist-get (drover-remote-goal-find "agent" (plist-get goal :id)) :state))

(ert-deftest drover-remote-goal-test-set-and-follow ()
  "A goal set here on the agent's session runs there: its checks read there, its turn followed, met, noted here."
  (drover-remote-goal-test-with "full"
    (drover-goal-mode 1)
    (let* ((session (drover-remote-goal-test-session))
           (facts (drover-remote-goal-checks "agent" :key (drover-remote--wire-key session))))
      ;; Read there: its directory, its worktree clean.
      (should (equal (plist-get facts :directory) checkout))
      (should (eq (plist-get facts :fresh) t))
      (should-not (plist-get facts :criterion))
      (let ((goal (drover-goal-start session :commands '("test -f README") :sources '("given") :prompt "Go"
                                     :task "T-7")))
        (should (equal (plist-get goal :host) "agent"))
        (should (memq (plist-get goal :state) '(setup sending waiting)))
        (should (equal (plist-get goal :key) (drover-session-key session)))
        ;; The prompt, with the goal's statement, went to the session there.
        (should (drover-remote-test-wait (lambda () (drover-remote-test-calls "send"))))
        (let ((sent (nth 2 (car (drover-remote-test-calls "send")))))
          (should (string-match-p "\\`Go" sent))
          (should (string-search "test -f README" sent)))
        ;; Kept there, set by this host, for its task.
        (let ((there (aref (plist-get (drover-remote-test-command "goals") :goals) 0)))
          (should (equal (aref there 0) (plist-get goal :id)))
          (should (string-match-p (concat "\\`" (regexp-quote drover-host) "\\'") (aref there 2)))
          (should (equal (aref there 3) "T-7"))
          (should (eq (aref there 4) :false)))
        ;; Shown here as the session's goal.
        (should (equal (plist-get (drover-goal-of session) :id) (plist-get goal :id)))
        (should (string-search "on agent" (drover-goal-describe (drover-goal-of session))))
        (should (drover-goal--list-text session 'column))
        ;; Its turn ends there: the checks pass, the goal is met; this host's heading noted.
        (should (drover-remote-test-wait (lambda () (eq (drover-remote-goal-test-state goal) 'waiting)) 10))
        (let ((notes nil))
          (cl-letf (((symbol-function 'drover-task-org-marker) (lambda (id) (and (equal id "T-7") 'heading)))
                    ((symbol-function 'drover-task-org-write)
                     (lambda (marker _properties note) (push (list marker note) notes))))
            (drover-remote-goal-test-turn)
            (should (drover-remote-test-wait (lambda () (eq (drover-remote-goal-test-state goal) 'met)) 20))
            (should (equal (caar notes) 'heading))
            (should (string-match-p "\\`Goal met on agent: " (cadr (car notes))))
            (should (string-match-p "Checks: test -f README" (cadr (car notes))))))
        ;; Its log, read there.
        (pcase-let ((`(,now . ,log) (drover-remote-goal-act (drover-remote-goal-find "agent" (plist-get goal :id)) 'log)))
          (should (eq (plist-get now :state) 'met))
          (should (string-search "Goal set on" log))
          (should (string-search "passed" log)))))))

(ert-deftest drover-remote-goal-test-acts ()
  "A goal there is paused, resumed and stopped from here; one over is not stopped again."
  (drover-remote-goal-test-with "full"
    (drover-goal-mode 1)
    (let* ((session (drover-remote-goal-test-session))
           (goal (drover-goal-start session :commands '("test -f README") :prompt "Go")))
      (should (drover-remote-test-wait (lambda () (eq (drover-remote-goal-test-state goal) 'waiting)) 10))
      (let ((paused (drover-goal-pause (drover-remote-goal-find "agent" (plist-get goal :id)))))
        (should (eq (plist-get paused :state) 'paused))
        (should (equal (plist-get paused :reason) "paused by the user")))
      ;; Resumed on an idle session: the checks run there now, and pass.
      (drover-goal-resume (drover-remote-goal-find "agent" (plist-get goal :id)))
      (should (drover-remote-test-wait (lambda () (eq (drover-remote-goal-test-state goal) 'met)) 20))
      (should-error (drover-goal-stop (drover-remote-goal-find "agent" (plist-get goal :id))) :type 'user-error)
      ;; A second goal, stopped.
      (let ((second (drover-goal-start session :commands '("test -f README") :prompt "Again")))
        (should-not (equal (plist-get second :id) (plist-get goal :id)))
        (should (eq (plist-get (drover-goal-stop second) :state) 'stopped))
        (should (equal (plist-get (drover-goal-of session) :id) (plist-get second :id)))))))

(ert-deftest drover-remote-goal-test-refused ()
  "A goal there takes full access, commands of a known shape, a session it has; an unknown goal is told."
  (drover-remote-goal-test-with "act"
    (let* ((peer (drover-remote--peer "agent"))
           (session (drover-remote-goal-test-session))
           (key (drover-remote--wire-key session)))
      (cl-flet ((set (&rest fields)
                  (cadr (drover-remote--wait peer (append (list :op "goal-set" :id (drover-remote--action-id peer)
                                                                :key key)
                                                          fields)
                                             10))))
        (should (string-match-p "act access: a goal runs its checks here, which takes full"
                                (set :kind "check" :commands ["true"]))))
      ;; Reading goes on.
      (pcase-let ((`(,reply ,error) (drover-remote--wait peer (list :op "goals") 10)))
        (should-not error)
        (should (equal (plist-get reply :goals) [])))
      (should (string-match-p "no such goal here"
                              (cadr (drover-remote--wait peer (list :op "goal-act" :goal "nope" :action "pause") 10)))))
    (should (equal (drover-remote-goal-properties (list :kind "bad") "viewer") "no kind of goal of a known shape"))
    (should (equal (drover-remote-goal-properties (list :commands "true") "viewer") "commands of no known shape"))
    (should (equal (drover-remote-goal-properties (list :commands ["a\nb"]) "viewer") "commands of no known shape"))
    (should (equal (drover-remote-goal-properties (list :commands []) "viewer") "a goal of checks needs commands"))
    (should (equal (drover-remote-goal-properties (list :commands ["true"] :iterations -1) "viewer")
                   "limits of no known shape"))
    (should (equal (drover-remote-goal-properties (list :kind "criterion") "viewer")
                   "a criterion goal needs an objective of 3000 characters at most"))
    (let ((properties (drover-remote-goal-properties (list :commands ["true"] :by "agent x" :pt "T1" :budget :false)
                                                     "viewer")))
      (should (equal (plist-get properties :by) "agent x on viewer"))
      (should (equal (plist-get properties :peer-task) "T1"))
      (should (equal (plist-get properties :sources) '("given by viewer")))
      (should (plist-member properties :budget))
      (should-not (plist-get properties :budget)))))

(ert-deftest drover-remote-goal-test-older-peer ()
  "A peer whose drover runs no goal for others is not asked, and says why."
  (drover-remote-test-with nil
    (with-environment-variables (("DROVER_AGENT_ACCESS" "full")
                                 ("DROVER_AGENT_CAPS" "actions,bye,grant,mirror,unsent,search,clone,new,task"))
      (drover-remote-test-connect (drover-remote-test-start-agent dir)))
    (drover-remote-test-command "sessions" :n 1)
    (drover-remote-test-synced 1)
    (should (string-match-p "older drover, which runs no goal for other hosts"
                            (cadr (should-error (drover-goal-start (drover-remote-test-session "s0")
                                                                   :commands '("true"))
                                                :type 'user-error))))))

(provide 'drover-remote-goal-test)
;;; drover-remote-goal-test.el ends here
