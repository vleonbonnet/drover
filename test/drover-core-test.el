;;; drover-core-test.el --- Tests of the session model and backends  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'drover-test-helpers)

(defun drover-test-ids (sessions)
  "Return the identifiers of SESSIONS."
  (mapcar #'drover-session-id sessions))

(ert-deftest drover-test-host-labels ()
  "Display labels follow a host across local and remote roles, without peers."
  (let ((drover-host-labels '(("core" . "Core") ("reaver" . "Reaver") ("blank" . "")))
        (drover-host-label-function nil))
    (dolist (drover-host '("core" "reaver"))
      (let ((drover-host-aliases (list (concat drover-host ".local"))))
        (should (equal (drover-host-label nil) (cdr (assoc drover-host drover-host-labels))))
        (should (equal (drover-host-label (upcase (car drover-host-aliases))) (drover-host-label nil))))
      (should (equal (drover-host-label "CORE") "Core"))
      (should (equal (drover-host-label "reaver") "Reaver"))
      (should (equal (drover-host-label "unknown") "unknown"))
      (should (equal (drover-host-label "blank") "blank")))
    (let ((drover-host "unlabelled") (drover-host-aliases '("unlabelled.local")))
      (should (equal (drover-host-label nil) "unlabelled"))
      (should (equal (drover-host-label "unlabelled.local") "unlabelled")))))

(ert-deftest drover-test-sort-urgent-first ()
  (let ((sessions (list (drover-test-session "idle" :status 'idle :updated 50.0)
                        (drover-test-session "done-new" :status 'done :since 30.0)
                        (drover-test-session "working" :status 'working :updated 10.0)
                        (drover-test-session "waiting" :status 'waiting :since 40.0)
                        (drover-test-session "done-old" :status 'done :since 20.0)
                        (drover-test-session "working-new" :status 'working :updated 60.0))))
    (should (equal (drover-test-ids (drover-sort sessions))
                   '("waiting" "done-old" "done-new" "working-new" "working" "idle")))))

(ert-deftest drover-test-sort-unknown-since-last ()
  (let ((sessions (list (drover-test-session "unknown" :status 'waiting)
                        (drover-test-session "known" :status 'waiting :since 5.0))))
    (should (equal (drover-test-ids (drover-sort sessions)) '("known" "unknown")))))

(ert-deftest drover-test-sort-leaves-input-alone ()
  (let ((sessions (list (drover-test-session "b" :status 'idle)
                        (drover-test-session "a" :status 'waiting))))
    (drover-sort sessions)
    (should (equal (drover-test-ids sessions) '("b" "a")))))

(ert-deftest drover-test-visible-sessions-hide-children ()
  (drover-test-with-backend (list (drover-test-session "root")
                                  (drover-test-session "child" :parent "root")
                                  (drover-test-session "ended" :parent "root" :status 'done)
                                  (drover-test-session "asking" :parent "root" :status 'waiting))
                            (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "asking")))
                            (let ((drover-show-subagents 'all))
                              (should (equal (drover-test-ids (drover-visible-sessions)) '("root" "child" "ended" "asking"))))
                            (let ((drover-show-subagents 'none))
                              (should (equal (drover-test-ids (drover-visible-sessions)) '("root"))))))

(ert-deftest drover-test-attention-sessions ()
  (drover-test-with-backend (list (drover-test-session "idle")
                                  (drover-test-session "done" :status 'done :since 2.0)
                                  (drover-test-session "waiting" :status 'waiting :since 3.0))
    (should (equal (drover-test-ids (drover-attention-sessions)) '("waiting" "done")))))

(defun drover-test-subagent-family ()
  "Return a parent with subagents of every kind, as lists see them.
\"ended\" finished unseen, \"asking\" waits for the user under a quiet
subagent, \"busy\" works, under a subagent whose turn ended unseen."
  (list (drover-test-session "root" :status 'idle)
        (drover-test-session "ended" :parent "root" :status 'done :since 1.0)
        (drover-test-session "quiet" :parent "root" :status 'idle)
        (drover-test-session "asking" :parent "quiet" :status 'waiting :since 2.0)
        (drover-test-session "path" :parent "root" :status 'failed :since 3.0)
        (drover-test-session "busy" :parent "path" :status 'working)
        (drover-test-session "alone" :status 'done :since 4.0)))

(ert-deftest drover-test-surfaced-sessions ()
  "Sessions surface as lists show them, and subagents waiting for the user always."
  (drover-test-with-backend (drover-test-subagent-family)
    ;; Active: the way to an active subagent shows, the failed one on it
    ;; included, as in lists.
    (should (equal (drover-test-ids (drover-surfaced-sessions))
                   '("root" "quiet" "asking" "path" "busy" "alone")))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-test-ids (drover-surfaced-sessions))
                     (drover-test-ids drover-test-sessions))))
    ;; None: a subagent waiting for the user blocks its parent.
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-ids (drover-surfaced-sessions)) '("root" "asking" "alone"))))
    ;; The filter narrows them as it narrows lists, but unfiltered.
    (let ((drover-filter '(:project "elsewhere")))
      (should-not (drover-surfaced-sessions))
      (should (equal (drover-test-ids (drover-surfaced-sessions nil t))
                     '("root" "quiet" "asking" "path" "busy" "alone"))))
    ;; Given sessions, those.
    (should (equal (drover-test-ids (drover-surfaced-sessions (last drover-test-sessions)))
                   '("alone")))))

(ert-deftest drover-test-surfaced-sessions-filter ()
  "Under a filter, sessions surface as lists draw them, and subagents waiting in a tree kept.
A subagent waiting in a tree the filter keeps surfaces whether it
matches or not: it is answered from the top of its tree, which lists
draw.  One in a tree the filter leaves out does not, nor one alone,
its parent gone, that does not match."
  (drover-test-with-backend
      (list (drover-test-session "kept" :status 'idle :project "p")
            (drover-test-session "asking" :parent "kept" :status 'waiting :project "q")
            (drover-test-session "ended" :parent "kept" :status 'done :project "q")
            (drover-test-session "out" :status 'idle :project "q")
            (drover-test-session "blocked" :parent "out" :status 'waiting :project "q")
            (drover-test-session "matching" :parent "out" :status 'done :project "p")
            (drover-test-session "lost" :parent "gone" :status 'waiting :project "q")
            (drover-test-session "found" :parent "gone" :status 'waiting :project "p")
            (drover-test-session "far" :status 'done :project "p" :host "peer" :stale 10.0))
    (let ((drover-filter '(:project "p")))
      ;; Active: "out" shows as the way to the subagent matching, which
      ;; the filter shows, active or not.
      (should (equal (drover-test-ids (drover-surfaced-sessions))
                     '("kept" "asking" "out" "blocked" "matching" "found")))
      ;; None: the same, the subagent matching shown all the same.
      (let ((drover-show-subagents 'none))
        (should (equal (drover-test-ids (drover-surfaced-sessions))
                       '("kept" "asking" "out" "blocked" "matching" "found"))))
      ;; A tree no session of which matches leaves its subagent waiting
      ;; out too.
      (setq drover-test-sessions (seq-remove (lambda (session)
                                               (equal (drover-session-id session) "matching"))
                                             drover-test-sessions))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      (should (equal (drover-test-ids (drover-surfaced-sessions)) '("kept" "asking" "found")))
      ;; All: the subagents the filter keeps, the one ended not.
      (let ((drover-show-subagents 'all))
        (should (equal (drover-test-ids (drover-surfaced-sessions)) '("kept" "asking" "found"))))
      ;; Unfiltered, as lists would show them without the filter.
      (should (equal (drover-test-ids (drover-surfaced-sessions nil t))
                     '("kept" "asking" "out" "blocked" "lost" "found"))))))

(ert-deftest drover-test-attention-sessions-follow-subagent-mode ()
  "The sessions needing the user are those lists show, and every subagent asking."
  (drover-test-with-backend (drover-test-subagent-family)
    (should (equal (drover-test-ids (drover-attention-sessions)) '("asking" "path" "alone")))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-test-ids (drover-attention-sessions)) '("asking" "path" "ended" "alone"))))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-ids (drover-attention-sessions)) '("asking" "alone"))))))

(ert-deftest drover-test-actions-reach-backend ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (save-window-excursion
      (delete-other-windows)
      (let ((session (drover-find 'fake "s")))
        (drover-open session t)
        (drover-respond session)
        (drover-interrupt session)
        (drover-archive session)
        (drover-mark-seen session)
        ;; The window is drover's to choose: the backend shows the
        ;; session in the one selected.
        (should (equal (reverse drover-test-calls)
                       '((:open "s" nil) (:respond "s") (:interrupt "s") (:archive "s")
                         (:mark-seen "s"))))))))

(defun drover-test-asking-family ()
  "Return sessions whose requests are in their own trees, as `drover-test-respond-in-tree' reads them.
\"root\" asks nothing, its subagent \"near\" does, since 3.0, and
\"deep\", below the quiet \"mid\", since 2.0.  \"own\" asks, since 4.0,
and so does its subagent \"own-kid\", since 1.0.  Nothing in the tree
of \"none\" asks."
  (let ((request '((:kind question :summary "Which color?"))))
    (list (drover-test-session "root" :status 'working)
          (drover-test-session "mid" :parent "root" :status 'working)
          (drover-test-session "deep" :parent "mid" :status 'waiting :since 2.0 :requests request)
          (drover-test-session "near" :parent "root" :status 'waiting :since 3.0 :requests request)
          (drover-test-session "busy" :parent "root" :status 'working)
          (drover-test-session "own" :status 'waiting :since 4.0 :requests request)
          (drover-test-session "own-kid" :parent "own" :status 'waiting :since 1.0 :requests request)
          (drover-test-session "none" :status 'idle)
          (drover-test-session "none-kid" :parent "none" :status 'idle))))

(ert-deftest drover-test-respond-in-tree ()
  "A session asking nothing answers the oldest request below it, at any depth; its own come first."
  (drover-test-with-backend (drover-test-asking-family)
    (cl-flet ((answers (id)
                (setq drover-test-calls nil)
                (drover-respond (drover-find 'fake id))
                drover-test-calls))
      ;; A subagent of a subagent, waiting the longest.
      (should (equal (drover-session-id (drover-session-to-answer (drover-find 'fake "root"))) "deep"))
      (should (equal (answers "root") '((:respond "deep"))))
      ;; From a subagent's own row, its own.
      (should (equal (answers "near") '((:respond "near"))))
      (should (equal (answers "mid") '((:respond "deep"))))
      ;; Its own request before an older one of its subagent.
      (should (equal (answers "own") '((:respond "own"))))
      ;; Nothing to answer: the backend tells.
      (should-not (drover-session-to-answer (drover-find 'fake "none")))
      (should (equal (answers "none") '((:respond "none"))))
      ;; Once the deep one is answered, the next.
      (setf (drover-session-requests (nth 2 drover-test-sessions)) nil)
      (drover-backend-changed 'fake "deep")
      (should (equal (answers "root") '((:respond "near")))))))

(ert-deftest drover-test-handed-off ()
  "A session handed its turn is told of once the command ended, once per report run.
An answer brings the buffers of the sessions above it, a prompt only
its own; the buffer the user acted from follows; a failing function
stops neither the others nor the next session."
  (drover-test-with-backend (drover-test-asking-family)
                            (let* ((buffers (mapcar (lambda (id) (cons id (generate-new-buffer (concat "buffer " id))))
                                                    '("root" "mid" "deep" "own")))
                                   (acting (generate-new-buffer "acting"))
                                   (told nil)
                                   (drover--handed-off nil)
                                   (drover--handed-off-timer nil)
                                   (drover-handed-off-functions
                                    (list (lambda (&rest _) (error "Broken"))
                                          (lambda (session found how origin)
                                            (push (list (drover-session-id session) (mapcar #'buffer-name found) how origin)
                                                  told)))))
                              (unwind-protect
                                  (cl-letf (((symbol-function 'drover-report-failure) #'ignore))
                                    (apply #'drover-register-backend 'fake
                                           :buffer (lambda (session)
                                                     (alist-get (drover-session-id session) buffers nil nil #'equal))
                                           (alist-get 'fake drover--backends))
                                    (drover-note-handed-off (drover-find 'fake "deep") 'answer 'front-end acting)
                                    (drover-note-handed-off (drover-find 'fake "deep") 'prompt 'drover)
                                    (drover-note-handed-off (drover-find 'fake "own") 'prompt 'drover)
                                    (drover-note-handed-off nil 'prompt 'drover)
                                    (should-not told)
                                    (accept-process-output nil 0.05)
                                    (should (equal (reverse told)
                                                   '(("deep" ("buffer deep" "buffer mid" "buffer root" "acting") answer front-end)
                                                     ("own" ("buffer own") prompt drover))))
                                    (setq told nil)
                                    (kill-buffer (alist-get "mid" buffers nil nil #'equal))
                                    (drover-note-handed-off (drover-find 'fake "deep") 'prompt 'drover)
                                    (drover-note-handed-off (drover-find 'fake "mid") 'answer 'drover)
                                    (accept-process-output nil 0.05)
                                    (should (equal (reverse told)
                                                   '(("deep" ("buffer deep") prompt drover)
                                                     ("mid" ("buffer root") answer drover)))))
                                (mapc (lambda (entry) (when (buffer-live-p (cdr entry)) (kill-buffer (cdr entry)))) buffers)
                                (kill-buffer acting)))))

(ert-deftest drover-test-recap ()
  "A recap comes from the backend once; a missing or failing :recap is a failure."
  (drover-test-with-backend nil
    (let ((results nil)
          (pending nil))
      (drover-register-backend 'later :label "Later" :sessions #'ignore
                               :recap (lambda (session callback)
                                        (push (cons (drover-session-id session) callback) pending)))
      (drover-register-backend 'broken :label "Broken" :sessions #'ignore
                               :recap (lambda (_session _callback) (error "No connection")))
      (drover-register-backend 'twice :label "Twice" :sessions #'ignore
                               :recap (lambda (_session callback)
                                        (funcall callback '(:prompt "Hi"))
                                        (funcall callback '(:prompt "Again"))))
      (drover-register-backend 'bare :label "Bare" :sessions #'ignore)
      (dolist (backend '(later broken twice bare))
        (drover-recap (drover-session-create :backend backend :id "x")
                      (lambda (recap) (push (cons backend recap) results))))
      ;; Asynchronous: nothing yet from `later'.
      (should (equal (reverse results)
                     '((broken :failure "No connection") (twice :prompt "Hi")
                       (bare :failure "Bare sessions tell no recap"))))
      (funcall (cdar pending) '(:reply "Done"))
      (should (equal (car results) '(later :reply "Done"))))))

(ert-deftest drover-test-unsupported-action ()
  (drover-test-with-backend nil
    (drover-register-backend 'bare :label "Bare" :sessions (lambda () nil))
    (let ((session (drover-session-create :backend 'bare :id "x")))
      (should-not (drover-supports-p session :respond))
      (should (drover-supports-p (drover-test-session "s") :respond))
      (should-error (drover-respond session) :type 'user-error))))

(ert-deftest drover-test-register-requires-sessions ()
  (drover-test-with-backend nil
    (should-error (drover-register-backend 'broken :label "Broken"))))

(ert-deftest drover-test-start-runs-once ()
  (drover-test-with-backend nil
    (drover-start)
    (drover-start)
    (should (equal drover-test-calls '((:start nil))))))

(ert-deftest drover-test-late-backend-starts-on-registration ()
  (drover-test-with-backend nil
                            (let ((started 0))
                              (drover-register-backend 'late :label "Late" :sessions #'ignore
                                                       :start (lambda () (cl-incf started)))
                              (should (= started 0))
                              (drover-start)
                              (should (= started 1))
                              (drover-register-backend 'later :label "Later" :sessions #'ignore
                                                       :start (lambda () (cl-incf started)))
                              (should (= started 2))
                              (should (memq 'later drover--started)))))

(ert-deftest drover-test-register-again ()
  "Registering a backend again applies its new properties, and keeps what drover holds of it.
Its place among backends, its start, which runs once, and its older
sessions fetched stay; a property it no longer gives goes."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let ((started 0)
          (older (list (drover-test-session "old" :status 'idle))))
      (drover-register-backend 'second :label "Second" :sessions #'ignore
                               :start (lambda () (cl-incf started))
                               :history (lambda (callback) (funcall callback older))
                               :rename #'ignore)
      (drover-start)
      (drover-set-show-history t)
      (should (= started 1))
      (should (assq 'second drover--history))
      (should (equal (drover-backends) '(second fake)))
      ;; As a file loaded anew registers it.
      (drover-register-backend 'second :label "Second, anew" :sessions #'ignore
                               :start (lambda () (cl-incf started))
                               :history (lambda (callback) (funcall callback older))
                               :recorded-times #'ignore)
      (should (equal (drover-backend-label 'second) "Second, anew"))
      (should (eq (drover-backend-property 'second :recorded-times) #'ignore))
      (should-not (drover-backend-property 'second :rename))
      (should (= started 1))
      (should (memq 'second drover--started))
      (should (assq 'second drover--history))
      (should (equal (drover-backends) '(second fake)))
      (drover-set-show-history nil))))

(ert-deftest drover-test-harness-icons ()
  "Claude Code's harnesses show Claude's logo and Codex's OpenAI's, as opencode.el draws those providers.
The icons are those of `opencode-provider-icons', whatever it holds;
OpenCode and agent-shell have icons of their own."
  (let ((standard (eval (car (get 'opencode-provider-icons 'standard-value)) t))
        (drover-harnesses (eval (car (get 'drover-harnesses 'standard-value)) t)))
    (let ((opencode-provider-icons standard))
      (should (equal (drover-harness-icon 'claude-code-ide) (alist-get "\\`anthropic\\'" standard nil nil #'equal)))
      (should (equal (drover-harness-icon 'claude-code-ide) "\uec82"))
      (should (equal (drover-harness-icon 'claude-code) "\uec82"))
      (should (equal (drover-harness-icon 'codex) (alist-get "\\`openai\\'" standard nil nil #'equal)))
      (should (equal (drover-harness-icon 'codex) "\uec81"))
      (should (equal (drover-harness-icon 'opencode) "\U000F0C8C"))
      (should (equal (drover-harness-icon 'agent-shell) "\U000F07B7"))
      (should (equal (drover-harness-icon 'other) drover-harness-default-icon)))
    ;; Providers drawn otherwise, harnesses follow.
    (let ((opencode-provider-icons '(("\\`anthropic\\'" . "A") ("\\`openai\\'" . "O"))))
      (should (equal (drover-harness-icon 'claude-code) "A"))
      (should (equal (drover-harness-icon 'codex) "O")))
    ;; A provider without an icon: the default one.
    (let ((opencode-provider-icons nil))
      (should (equal (drover-harness-icon 'codex) drover-harness-default-icon)))))

(ert-deftest drover-test-session-buffer ()
  (drover-test-with-backend nil
                            (let ((live (generate-new-buffer " live"))
                                  (dead (generate-new-buffer " dead")))
                              (unwind-protect
                                  (progn
                                    (kill-buffer dead)
                                    (drover-register-backend 'buffers :label "Buffers" :sessions #'ignore
                                                             :buffer (lambda (session)
                                                                       (pcase (drover-session-id session)
                                                                         ("live" live) ("dead" dead) ("error" (error "Boom")))))
                                    (cl-flet ((buffer-of (id)
                                                (drover-session-buffer (drover-session-create :backend 'buffers :id id))))
                                      (should (eq (buffer-of "live") live))
                                      (should-not (buffer-of "dead"))
                                      (should-not (buffer-of "none"))
                                      (should-not (buffer-of "error")))
                                    (should-not (drover-session-buffer (drover-test-session "s"))))
                                (kill-buffer live)))))

(ert-deftest drover-test-session-title-override ()
  (should (equal (drover-session-title (drover-test-session "s")) "Session s"))
  (should (equal (drover-session-title (drover-test-session "s" :title "Mine")) "Mine")))

(ert-deftest drover-test-failing-backend-contributes-nothing ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-register-backend 'broken :label "Broken" :sessions (lambda () (error "Boom")))
    (should (equal (drover-test-ids (drover-sessions)) '("s")))))

(ert-deftest drover-test-changes-coalesce ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-backend-changed 'fake "a")
      (drover-backend-changed 'fake "b")
      (drover-changed-now)
      (should (= runs 1))
      (drover-changed-now)
      (should (= runs 1)))))

;;;; The index of sessions

(defmacro drover-test-with-index (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS, its index made.
`changed' holds what `drover-changed-sessions' told the last time the
hook ran, `asked' the identifiers the backend's :session was asked for,
and `listed' how many times its :sessions was called."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((changed 'none)
           (asked nil)
           (listed 0))
       (drover-test-register-simple
        'fake (lambda () (cl-incf listed) drover-test-sessions)
        :session (lambda (id)
                   (push id asked)
                   (seq-find (lambda (session) (equal (drover-session-id session) id))
                             drover-test-sessions)))
       (add-hook 'drover-changed-hook (lambda () (setq changed drover-changed-sessions)))
       (drover-index)
       (drover-changed-now)
       (setq listed 0 asked nil changed 'none)
       ,@body)))

(defun drover-test-index-ids (sessions)
  "Return the identifiers of SESSIONS, records or keys, sorted."
  (sort (mapcar (lambda (session) (if (consp session) (cdr session) (drover-session-id session)))
                sessions)
        #'string<))

(ert-deftest drover-test-index-follows-one-session ()
  "A session reported changed costs its backend's :session for it alone, and is told."
  (drover-test-with-index (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'idle))
    (setq drover-test-sessions (list (drover-test-session "a" :status 'done)
                                     (drover-test-session "b" :status 'idle)))
    (drover-backend-changed 'fake "a")
    (drover-changed-now)
    (should (equal asked '("a")))
    (should (zerop listed))
    (should (equal changed '((fake . "a"))))
    (should (eq (drover-session-status (drover-index-session '(fake . "a"))) 'done))
    ;; The index lists what `drover-sessions' lists, in the same order.
    (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                   (mapcar #'drover-session-key (drover-sessions))))
    ;; A session gone, and one coming.
    (setq drover-test-sessions (list (drover-test-session "b" :status 'idle)
                                     (drover-test-session "c" :status 'working)))
    (drover-backend-changed 'fake "a")
    (drover-backend-changed 'fake "c")
    (drover-changed-now)
    (should (equal (drover-test-index-ids changed) '("a" "c")))
    (should-not (drover-index-session '(fake . "a")))
    (should (equal (drover-test-index-ids (drover-index-sessions)) '("b" "c")))
    ;; Unreported changes wait for the index to be refreshed.
    (setf (drover-session-title (car drover-test-sessions)) "Renamed")
    (should-not (equal (drover-session-title (drover-index-session '(fake . "b"))) "Renamed"))
    (setq listed 0)
    (drover-index-refresh)
    (should (= listed 1))
    (should (equal changed '((fake . "b"))))
    (should (equal (drover-session-title (drover-index-session '(fake . "b"))) "Renamed"))
    ;; A refresh finding nothing new runs no hook.
    (setq changed 'none)
    (drover-index-refresh)
    (should (eq changed 'none))))

(ert-deftest drover-test-index-backend-changed ()
  "A backend reported changed without a session is listed anew; without a backend, views draw everything."
  (drover-test-with-index (list (drover-test-session "a") (drover-test-session "b"))
    (setq drover-test-sessions (list (drover-test-session "a" :status 'done) (drover-test-session "b")))
    (drover-backend-changed 'fake)
    (drover-changed-now)
    (should (= listed 1))
    (should-not asked)
    (should (equal changed '((fake . "a"))))
    (drover-backend-changed)
    (drover-changed-now)
    (should (eq changed t))
    ;; A backend without :session is listed anew for a session too.
    (drover-test-register-simple 'other (lambda () (list (drover-session-create :backend 'other :id "o"))))
    (drover-changed-now)
    (drover-backend-changed 'other "o")
    (drover-changed-now)
    (should (equal changed '((other . "o"))))
    ;; View settings change everything.
    (drover-set-show-subagents 'all)
    (should (eq changed t))))

(ert-deftest drover-test-index-trees ()
  "The index nests subagents as `drover-tree' does, as parents come and go."
  (drover-test-with-index (list (drover-test-session "sub" :parent "root" :status 'working)
                                (drover-test-session "deep" :parent "sub"))
    ;; The parent not listed, the subagent tops its tree.
    (should-not (drover-index-parent '(fake . "sub")))
    (should (equal (drover-index-root '(fake . "deep")) '(fake . "sub")))
    (setq drover-test-sessions (append drover-test-sessions (list (drover-test-session "root"))))
    (drover-backend-changed 'fake "root")
    (drover-changed-now)
    ;; The parent coming, its subagent's place changed, and is told.
    (should (equal (drover-test-index-ids changed) '("root" "sub")))
    (should (equal (drover-index-parent '(fake . "sub")) '(fake . "root")))
    (should (equal (drover-index-root '(fake . "deep")) '(fake . "root")))
    (should (equal (drover-test-index-ids (drover-index-tree-sessions '(fake . "root")))
                   '("deep" "root" "sub")))
    (should (equal (mapcar #'drover-session-key (drover-tree-sessions (drover-tree (drover-index-sessions))))
                   (mapcar #'drover-session-key (drover-tree-sessions (drover-tree (drover-sessions))))))
    ;; Going, it leaves its subagent at the top of its tree again.
    (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "root"))
                                           drover-test-sessions))
    (drover-backend-changed 'fake "root")
    (drover-changed-now)
    (should-not (drover-index-parent '(fake . "sub")))
    (should (drover-index-incremental-p))
    ;; Parents forming a cycle: views draw everything, from an index made anew.
    (setq drover-test-sessions (list (drover-test-session "sub" :parent "deep")
                                     (drover-test-session "deep" :parent "sub")))
    (drover-backend-changed 'fake "sub")
    (drover-changed-now)
    (should (eq changed t))
    (should (equal (drover-test-index-ids (drover-index-sessions)) '("deep" "sub")))
    (should-not (drover-index-incremental-p))))

(ert-deftest drover-test-index-merges-like-sessions ()
  "Records of two backends showing one conversation make views draw everything, merged."
  (drover-test-with-index (list (drover-test-session "a" :engine 'opencode :engine-id "x"))
    (drover-test-register-simple 'other (list (drover-session-create :backend 'other :id "o"
                                                                     :engine 'opencode :engine-id "x")))
    (drover-changed-now)
    (should (eq changed t))
    (should-not (drover-index-incremental-p))
    (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                   (mapcar #'drover-session-key (drover-sessions))))
    (should (= (length (drover-index-sessions)) 1))))

(defmacro drover-test-with-recorded (records &rest body)
  "Run BODY with the fake backend's index, and a backend `records' recording RECORDS.
`records' lists the conversations of the engine `fake' as their engine
records them (:recorded), its sessions named by their engine
identifiers; RECORDS is a variable BODY may set.  `recorded-asked'
holds the identifiers its :session was asked for."
  (declare (indent 1) (debug t))
  `(drover-test-with-index nil
     (let ((recorded-asked nil))
       (drover-test-register-simple
        'records (lambda () ,records)
        :recorded '(fake)
        :session (lambda (id)
                   (push id recorded-asked)
                   (seq-find (lambda (session) (equal (drover-session-id session) id)) ,records)))
       (drover-changed-now)
       (setq changed 'none)
       ,@body)))

(defun drover-test-record (id &rest properties)
  "Return the record of conversation ID of the engine `fake', by `records'."
  (apply #'drover-session-create
         (append properties (list :backend 'records :id id :title (concat "Record " id)
                                  :engine 'fake :engine-id id))))

(ert-deftest drover-test-recorded-yields-to-front-ends ()
  "A conversation a front end follows is its session; the record of it is not listed."
  (let ((records (list (drover-test-record "x") (drover-test-record "y"))))
    (drover-test-with-recorded records
      (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                     '((records . "x") (records . "y"))))
      ;; A front end opens x: its session alone is listed, followed in
      ;; place, its record leaving as the front end's session comes.
      (setq drover-test-sessions (list (drover-test-session "terminal" :engine 'fake :engine-id "x"
                                                            :status 'working)))
      (drover-backend-changed 'fake "terminal")
      (drover-changed-now)
      (should (equal (drover-test-index-ids changed) '("terminal" "x")))
      (should (equal recorded-asked '("x")))
      (should (drover-index-incremental-p))
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("terminal" "y")))
      (should (equal (drover-test-index-ids (drover-sessions)) '("terminal" "y")))
      ;; The record changing changes nothing listed.
      (setq records (list (drover-test-record "x" :title "Renamed") (drover-test-record "y")))
      (drover-backend-changed 'records "x")
      (drover-changed-now)
      (should-not (drover-index-session '(records . "x")))
      ;; Relisting either backend agrees.
      (drover-index-refresh)
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("terminal" "y")))
      ;; The front end's session following another conversation: x is
      ;; the record's again, and z, recorded too, the front end's.
      (setq records (append records (list (drover-test-record "z"))))
      (setq drover-test-sessions (list (drover-test-session "terminal" :engine 'fake :engine-id "z")))
      (drover-backend-changed 'fake "terminal")
      (drover-changed-now)
      (should (drover-index-incremental-p))
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("terminal" "x" "y")))
      (should (equal (drover-session-title (drover-index-session '(records . "x"))) "Renamed"))
      ;; Closed, its conversation is listed by its record.
      (setq drover-test-sessions nil)
      (drover-backend-changed 'fake "terminal")
      (drover-changed-now)
      (should (equal (drover-test-index-ids changed) '("terminal" "z")))
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("x" "y" "z")))
      ;; A front end's session not knowing its conversation yet hides nothing.
      (setq drover-test-sessions (list (drover-test-session "new" :engine 'fake)))
      (drover-backend-changed 'fake "new")
      (drover-changed-now)
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("new" "x" "y" "z")))
      ;; A subagent of a recorded conversation finds a front end showing it.
      (setq records (append records (list (drover-test-record "x/agent" :engine-id nil :parent "x"))))
      (setq drover-test-sessions (list (drover-test-session "terminal" :engine 'fake :engine-id "x")))
      (drover-backend-changed 'records)
      (drover-backend-changed 'fake "terminal")
      (drover-changed-now)
      (should (equal (drover-index-parent '(records . "x/agent")) '(fake . "terminal"))))))

(ert-deftest drover-test-recorded-built-index ()
  "An index made anew, and every listing, leave out the records of followed conversations."
  (let ((records (list (drover-test-record "x") (drover-test-record "y"))))
    (drover-test-with-backend (list (drover-test-session "terminal" :engine 'fake :engine-id "x"))
      (drover-test-register-simple 'records (lambda () records) :recorded '(fake))
      (should (equal (drover-test-index-ids (drover-sessions)) '("terminal" "y")))
      (should (equal (drover-test-index-ids (drover-index-sessions)) '("terminal" "y")))
      (should (drover-index-incremental-p))
      ;; Another engine's conversation of the same identifier is another.
      (setq records (list (drover-session-create :backend 'records :id "x" :engine 'other :engine-id "x")))
      (should (equal (drover-test-index-ids (drover-sessions)) '("terminal" "x"))))))

(ert-deftest drover-test-resumers ()
  "A conversation resumes in the front end preferred, else the one it ran in, else the first."
  (let ((drover--resumers nil)
        (drover--resumer-properties nil)
        (drover-resume-front-ends nil)
        (calls nil))
    (should-not (drover-resumer 'fake))
    (should-error (drover-resume 'fake "x" "/work/") :type 'user-error)
    (drover-register-resumer 'fake 'terminal (lambda (id directory) (push (list 'terminal id directory) calls)))
    (drover-register-resumer 'fake 'shell (lambda (id directory) (push (list 'shell id directory) calls)))
    (drover-register-resumer 'other 'shell #'ignore)
    (should (eq (car (drover-resumer 'fake)) 'terminal))
    (should (eq (car (drover-resumer 'fake 'shell)) 'shell))
    (should (eq (car (drover-resumer 'fake 'unloaded)) 'terminal))
    (let ((drover-resume-front-ends '((fake shell))))
      (should (eq (car (drover-resumer 'fake 'terminal)) 'shell)))
    (drover-resume 'fake "x" "/work/" 'shell)
    (should (equal calls '((shell "x" "/work/"))))
    ;; Registered again, it keeps its place.
    (drover-register-resumer 'fake 'terminal #'ignore)
    (should (equal (car (drover-resumer 'fake)) 'terminal))
    (drover-unregister-resumer 'fake 'terminal)
    (should (eq (car (drover-resumer 'fake)) 'shell))))

(ert-deftest drover-test-resume-with ()
  "A front end chosen resumes there: by identifier, or later, with THEN and ELSE."
  (let ((drover--resumers nil)
        (drover--resumer-properties nil)
        (drover-resume-front-ends '((fake shell)))
        (buffer (generate-new-buffer " resumed"))
        (calls nil)
        (later nil))
    (unwind-protect
        (progn
          (drover-register-resumer 'fake 'terminal
                                   (lambda (id directory) (push (list 'terminal id directory) calls) buffer))
          (drover-register-resumer 'fake 'shell (lambda (&rest _) (push 'shell calls) nil))
          (drover-register-resumer 'fake 'service
                                   (lambda (id directory then else)
                                     (push (list 'service id directory) calls)
                                     (setq later (list then else))
                                     'not-a-buffer)
                                   :by-id t :async t)
          (should (equal (drover-resumer-front-ends 'fake) '(terminal shell service)))
          (should (drover-resumer-property 'fake 'service :by-id))
          (should-not (drover-resumer-property 'fake 'terminal :by-id))
          ;; The front end chosen, not the one preferred.
          (let (got)
            (should (eq (drover-resume-with 'terminal 'fake "x" "/work/" (lambda (b) (setq got b))) buffer))
            (should (eq got buffer))
            (should (equal (car calls) '(terminal "x" "/work/"))))
          ;; A synchronous front end opening nothing tells ELSE.
          (let (failed)
            (drover-resume-with 'shell 'fake "x" "/work/" #'ignore (lambda (message) (setq failed message)))
            (should (stringp failed)))
          ;; A directory is needed, but by a front end finding conversations by identifier.
          (should-error (drover-resume-with 'terminal 'fake "x" nil) :type 'user-error)
          (let (got)
            (drover-resume-with 'service 'fake "y" nil (lambda (b) (setq got b)) #'ignore)
            (should (equal (car calls) '(service "y" nil)))
            (should-not got)
            (funcall (car later) buffer)
            (should (eq got buffer)))
          (should-error (drover-resume-with 'unknown 'fake "x" "/work/") :type 'user-error)
          (drover-unregister-resumer 'fake 'service)
          (should-not (drover-resumer-property 'fake 'service :by-id)))
      (kill-buffer buffer))))

(ert-deftest drover-test-archive-what-the-backend-does-not ()
  "A session its backend's archive does not take, drover archives itself."
  (drover-test-with-backend nil
    (drover-test-with-state
      (let ((sessions (list (drover-session-create :backend 'picky :id "p" :engine 'fake :engine-id "p")))
            (archived nil))
        (drover-test-register-simple 'picky (lambda () sessions)
                                     :archive (lambda (session) (push session archived))
                                     :supports (lambda (_session operation) (not (eq operation :archive))))
        (let ((session (drover-find 'picky "p")))
          (should (drover-archivable-p session))
          (drover-archive-session session)
          (should-not archived)
          (should (assoc '(fake . "p") (drover--archive)))
          (should-not (drover-sessions)))))))

(ert-deftest drover-test-recorded-title ()
  "A front end takes the title of the conversation it follows from the backend recording it."
  (drover-test-with-backend nil
    (drover-test-register-simple 'records #'ignore :recorded '(fake)
                                 :recorded-title (lambda (id)
                                                   (pcase id
                                                     ("c1" "Login redirect")
                                                     ("blank" " ")
                                                     ("bad" (error "Unreadable")))))
    (should (equal (drover-recorded-title 'fake "c1") "Login redirect"))
    (should-not (drover-recorded-title 'fake "blank"))
    (should-not (drover-recorded-title 'fake "unknown"))
    (should-not (drover-recorded-title 'fake nil))
    (should-not (drover-recorded-title 'other "c1"))
    (should-not (drover-recorded-title 'fake "bad"))))

(ert-deftest drover-test-restore-older-recorded ()
  "A conversation drover archived is restored when a recording backend lists it among older ones."
  (drover-test-with-backend nil
    (drover-test-with-state
      (let ((old (drover-test-record "old" :updated 1.0)))
        (drover-test-register-simple 'records (lambda () (list old)) :recorded '(fake))
        (drover-archive-session old)
        ;; Its record no longer listed live, but kept among older ones.
        (drover-test-register-simple 'records (lambda () nil) :recorded '(fake)
                                     :history (lambda (callback) (funcall callback (list old))))
        (let ((drover-show-archived t))
          (let ((archived (car (drover-archived-sessions))))
            (should (equal (drover-session-key archived) '(records . "old")))
            (drover-unarchive-session archived)))
        (should-not (drover--archive))))))

(ert-deftest drover-test-drover-archive-index-follows-subagents ()
  "The index follows the subagents going with a conversation drover archives.
Archived through the front end following it, the conversation takes the
subagents its recording backend lists out of the index at once, each
looked at alone, at any depth, the parent of a deeper one asked of its
backend once out of the index.  They stay out as they change, or come,
in an index made anew too, and come back with it, under it again.
Deleted, it leaves its subagents at the top of trees of their own."
  (let ((records (list (drover-test-record "x")
                       (drover-test-record "x/agent" :engine-id nil :parent "x")
                       (drover-test-record "s1" :parent "x")
                       (drover-test-record "s2" :parent "s1")
                       (drover-test-record "y"))))
    (drover-test-with-recorded records
      (drover-test-with-state
        (setq drover-test-sessions (list (drover-test-session "terminal" :engine 'fake :engine-id "x")))
        (drover-backend-changed 'fake "terminal")
        (drover-changed-now)
        (should (equal (drover-index-parent '(records . "x/agent")) '(fake . "terminal")))
        (should (equal (drover-index-parent '(records . "s2")) '(records . "s1")))
        (setq recorded-asked nil)
        (drover-archive-session (drover-find 'fake "terminal"))
        (drover-changed-now)
        (should (cl-subsetp '("s1" "s2" "terminal" "x/agent") (drover-test-index-ids changed) :test #'equal))
        (should (cl-subsetp '("x" "x/agent" "s1" "s2") recorded-asked :test #'equal))
        (should (drover-index-incremental-p))
        (should (equal (drover-test-index-ids (drover-index-sessions)) '("y")))
        (should (equal (drover-test-index-ids (drover-sessions)) '("y")))
        ;; Changing, or coming, they stay out.
        (setq records (append (list (drover-test-record "x/agent" :engine-id nil :parent "x" :title "Renamed")
                                    (drover-test-record "x/agent-2" :engine-id nil :parent "x"))
                              (cddr records)))
        (dolist (id '("x/agent" "x/agent-2" "s2"))
          (drover-backend-changed 'records id)
          (drover-changed-now)
          (should (equal (drover-test-index-ids (drover-index-sessions)) '("y"))))
        ;; An index made anew leaves them out too.
        (setq drover--index nil)
        (should (equal (drover-test-index-ids (drover-index-sessions)) '("y")))
        ;; Restored, they come back under it.
        (drover-unarchive-session (drover-find 'fake "terminal"))
        (drover-changed-now)
        (let ((back '("s1" "s2" "terminal" "x/agent" "x/agent-2" "y")))
          (should (drover-index-incremental-p))
          (should (equal (drover-test-index-ids (drover-index-sessions)) back))
          (should (equal (drover-index-parent '(records . "x/agent")) '(fake . "terminal")))
          (should (equal (drover-index-parent '(records . "x/agent-2")) '(fake . "terminal")))
          (should (equal (drover-index-parent '(records . "s2")) '(records . "s1")))
          (setq drover--index nil)
          (should (equal (drover-test-index-ids (drover-index-sessions)) back)))
        ;; Deleted archived, its conversation's subagents left behind come
        ;; back at the top of trees of their own.
        (drover-archive-session (drover-find 'fake "terminal"))
        (drover-changed-now)
        (should (equal (drover-test-index-ids (drover-index-sessions)) '("y")))
        (drover-test-register-simple 'fake (lambda () drover-test-sessions)
                                     :session (lambda (id)
                                                (seq-find (lambda (session) (equal (drover-session-id session) id))
                                                          drover-test-sessions))
                                     :delete (lambda (_session) (setq drover-test-sessions nil)))
        (setq records (seq-remove (lambda (record) (equal (drover-session-id record) "x")) records))
        (drover-delete-session (drover-find 'fake "terminal"))
        (drover-changed-now)
        (should (equal (drover-test-index-ids (drover-index-sessions)) '("s1" "s2" "x/agent" "x/agent-2" "y")))
        (should-not (drover-index-parent '(records . "x/agent")))
        (should (equal (drover-index-parent '(records . "s2")) '(records . "s1")))))))

(ert-deftest drover-test-format-age ()
  (should (equal (opencode-format-age nil) ""))
  (should (equal (opencode-format-age (- drover-test-now 42) drover-test-now) "42s"))
  (should (equal (opencode-format-age (- drover-test-now 300) drover-test-now) "5m"))
  (should (equal (opencode-format-age (- drover-test-now 7200) drover-test-now) "2h"))
  (should (equal (opencode-format-age (- drover-test-now 172800) drover-test-now) "2d")))

(ert-deftest drover-test-session-summary ()
  (should (equal (drover-session-summary
                  (drover-test-session "s" :requests '((:kind question :summary "Which branch?"))))
                 "asks: Which branch?"))
  (should (equal (drover-session-summary
                  (drover-test-session "s" :requests '((:kind permission :summary "bash: make"))))
                 "permit: bash: make"))
  (should (equal (drover-session-summary (drover-test-session "s" :detail "Rate limited"))
                 "Rate limited")))

(ert-deftest drover-test-session-at-point ()
  (with-temp-buffer
    (should-error (drover-session-at-point) :type 'user-error)
    (let ((session (drover-test-session "s")))
      (setq drover-session-at-point-function (lambda () session))
      (should (eq (drover-session-at-point) session)))))

;;;; Face remapping

(ert-deftest drover-test-remap-default-face-stays-local ()
  "Remapping a view's default face never changes the global remapping.
A global `face-remapping-alist', such as one setting a stipple, shares
its list with every buffer-local value made from it."
  (let ((face-remapping-alist (list (list 'default (list :stipple "alpha:65%")))))
    (let ((global (copy-tree face-remapping-alist)))
      (with-temp-buffer
        (drover-remap-default-face 'bold)
        (should (memq 'bold (flatten-tree (alist-get 'default face-remapping-alist)))))
      (should (equal (default-value 'face-remapping-alist) global))
      (with-temp-buffer
        (should (equal face-remapping-alist global))))))

(ert-deftest drover-test-views-keep-the-global-remapping ()
  "Opening the session list leaves the global remapping alone."
  (require 'drover-list)
  (let ((face-remapping-alist (list (list 'default (list :stipple "alpha:65%")))))
    (let ((global (copy-tree face-remapping-alist)))
      (with-temp-buffer
        (drover-list-mode))
      (should (equal (default-value 'face-remapping-alist) global)))))

;;;; Measuring ink

(defmacro drover-test-with-fake-fonts (graphic &rest body)
  "Run BODY with fonts faked, on a graphic display when GRAPHIC is non-nil.
Every character advances 7 pixels, and draws 7 pixels of ink but R,
whose ink reaches 9 pixels left of it and 20 right; N has no glyph.
`fonts-asked' collects, newest first, what `font-at' was asked for:
\(INDEX WINDOW FACE REMAPPING), FACE being the face property there and
REMAPPING the face remapping in force."
  (declare (indent 1) (debug t))
  `(let ((fonts-asked nil))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) ,graphic))
               ((symbol-function 'buffer-text-pixel-size)
                (lambda (&rest _) (cons (* 7 (- (point-max) (point-min))) 1)))
               ((symbol-function 'font-at)
                (lambda (index &optional window string)
                  (push (list index window (get-text-property index 'face string) face-remapping-alist)
                        fonts-asked)
                  'fake-font))
               ((symbol-function 'font-get-glyphs)
                (lambda (font from _to object)
                  (should (eq font 'fake-font))
                  (let ((char (aref object from)))
                    (vector (pcase char
                              (?R (vector from from char 1 7 -9 20 10 2 nil))
                              (?N nil)
                              (_ (vector from from char 1 7 0 7 10 2 nil))))))))
       ,@body)))

(ert-deftest drover-test-text-ink-extent ()
  "Ink reaching past the advance counts, in the font each character is drawn in."
  (let ((remapping '((default drover-list default)))
        (window (selected-window)))
    (drover-test-with-fake-fonts t
      ;; R, 7 pixels in, reaches 27 pixels, past the advance of the
      ;; string, 21, and 2 pixels left of its start.
      (should (equal (drover-text-ink-extent (concat "a" (propertize "R" 'face 'bold) "b")
                                             remapping window)
                     '(2 . 27)))
      ;; Asked for each character, in the window, with the remapping in
      ;; force, the default face named last, so that its remapping counts.
      (should (equal (reverse fonts-asked)
                     `((0 ,window (default) ,remapping)
                       (1 ,window (bold default) ,remapping)
                       (2 ,window (default) ,remapping))))
      ;; Without ink beyond the advance, the advance.
      (should (equal (drover-text-ink-extent "ab" remapping window) '(0 . 14)))
      ;; A character without a glyph, or displayed as something else,
      ;; counts by its advance.
      (setq fonts-asked nil)
      (should (equal (drover-text-ink-extent (concat "N" (propertize "R" 'display "x")) remapping window)
                     '(0 . 14)))
      (should (equal (mapcar #'car fonts-asked) '(0)))
      ;; Faces as lists and as attributes.
      (setq fonts-asked nil)
      (drover-text-ink-extent (concat (propertize "a" 'face '(bold italic))
                                      (propertize "b" 'face '(:weight bold)))
                              remapping window)
      (should (equal (mapcar #'cadr (mapcar #'cdr (reverse fonts-asked)))
                     '((bold italic default) ((:weight bold) default)))))
    ;; On a text terminal, the advance alone.
    (drover-test-with-fake-fonts nil
      (should (equal (drover-text-ink-extent "aRb" remapping window) '(0 . 21)))
      (should-not fonts-asked))))

;;;; Trees

(ert-deftest drover-test-tree-nests-subagents ()
  (let ((nodes (drover-tree
                (list (drover-test-session "child" :parent "root" :status 'working :updated 5.0)
                      (drover-test-session "root" :status 'idle :updated 1.0)
                      (drover-test-session "grandchild" :parent "child" :status 'idle)
                      (drover-test-session "other" :status 'working :updated 3.0)))))
    (should (equal (drover-test-tree-shape nodes)
                   '(("root" ("child" ("grandchild"))) ("other"))))
    (should (eq (drover-node-status (car nodes)) 'working))
    (should (= (drover-node-updated (car nodes)) 5.0))))

(ert-deftest drover-test-tree-rolls-up-attention ()
  "A parent whose subagent waits for the user ranks with the waiting sessions."
  (let ((nodes (drover-tree
                (list (drover-test-session "busy" :status 'working)
                      (drover-test-session "parent" :status 'done :since 50.0)
                      (drover-test-session "asking" :parent "parent" :status 'waiting :since 30.0)
                      (drover-test-session "waiting" :status 'waiting :since 40.0)))))
    (should (equal (drover-test-tree-shape nodes)
                   '(("parent" ("asking")) ("waiting") ("busy"))))
    (should (eq (drover-node-status (car nodes)) 'waiting))
    (should (= (drover-node-since (car nodes)) 30.0))
    (should (equal (mapcar #'car (drover-group-trees-by-status nodes)) '(waiting working)))))

(ert-deftest drover-test-tree-orphans-and-cycles ()
  (let ((nodes (drover-tree
                (list (drover-test-session "orphan" :parent "gone")
                      (drover-test-session "self" :parent "self")
                      (drover-test-session "a" :parent "b")
                      (drover-test-session "b" :parent "a")))))
    ;; An orphan and a self-parent are trees of their own, and so is a cycle.
    (should (equal (sort (mapcar #'car (drover-test-tree-shape nodes)) #'string<)
                   '("a" "orphan" "self")))
    (should (equal (length (drover-tree-sessions nodes)) 4))))

(ert-deftest drover-test-tree-other-backend-is-not-a-parent ()
  (let ((nodes (drover-tree
                (list (drover-test-session "root")
                      (drover-session-create :backend 'other :id "child" :parent "root")))))
    (should (= (length nodes) 2))))

(ert-deftest drover-test-tree-visible ()
  "Subagents show as `drover-show-subagents' says: all, active, none."
  (let ((nodes (drover-tree
                (list (drover-test-session "root")
                      (drover-test-session "quiet" :parent "root" :status 'idle)
                      (drover-test-session "deep" :parent "quiet" :status 'working)
                      (drover-test-session "asking" :parent "root" :status 'waiting)
                      (drover-test-session "ended" :parent "root" :status 'done)
                      (drover-test-session "broke" :parent "root" :status 'failed)
                      (drover-test-session "retry" :parent "root" :status 'retrying)))))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-tree-sessions (drover-tree-visible nodes))
                     (drover-tree-sessions nodes)))
      (should (= (drover-tree-hidden-count nodes) 0)))
    (let ((drover-show-subagents 'active))
      ;; Ended turns hide, unseen or not; a quiet subagent stays when a
      ;; session under it is active.
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes))
                     '(("root" ("asking") ("retry") ("quiet" ("deep"))))))
      (should (= (drover-tree-hidden-count nodes) 2)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes)) '(("root"))))
      (should (= (drover-tree-hidden-count nodes) 6)))
    ;; An unknown mode behaves as the default.
    (let ((drover-show-subagents 'bogus))
      (should (= (drover-tree-hidden-count nodes) 2)))))

(ert-deftest drover-test-tree-visible-orphans ()
  "A subagent whose parent is not listed shows as any subagent does.
Its parent archived, deleted or not loaded, it is at the top of a tree
of its own, a subagent still: an idle one hides in active mode, as it
did under a session archived alone while its subagents stayed."
  (let ((nodes (drover-tree
                (list (drover-test-session "root")
                      (drover-test-session "idle" :parent "archived" :status 'idle)
                      (drover-test-session "ended" :parent "gone" :status 'done)
                      (drover-test-session "busy" :parent "gone" :status 'working)
                      (drover-test-session "quiet" :parent "gone" :status 'idle)
                      (drover-test-session "deep" :parent "quiet" :status 'waiting)))))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-tree-sessions (drover-tree-visible nodes))
                     (drover-tree-sessions nodes)))
      (should (= (drover-tree-hidden-count nodes) 0)))
    (let ((drover-show-subagents 'active))
      ;; The way to an active subagent shows, as under a listed parent.
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes))
                     '(("quiet" ("deep")) ("busy") ("root"))))
      (should (= (drover-tree-hidden-count nodes) 2)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-tree-visible nodes)) '(("root"))))
      (should (= (drover-tree-hidden-count nodes) 5)))))

(ert-deftest drover-test-orphans-surface-as-shown ()
  "Subagents whose parent is not listed surface, count and are counted as subagents."
  (drover-test-with-backend (list (drover-test-session "root" :status 'idle)
                                  (drover-test-session "idle" :parent "archived" :status 'idle)
                                  (drover-test-session "ended" :parent "archived" :status 'done :since 1.0)
                                  (drover-test-session "asking" :parent "gone" :status 'waiting :since 2.0))
    (should (equal (drover-test-ids (drover-surfaced-sessions)) '("root" "asking")))
    (should (equal (drover-test-ids (drover-attention-sessions)) '("asking")))
    (should (equal (drover-test-tree-shape (drover-view-trees)) '(("asking") ("root"))))
    (should (equal (drover-view-subagent-counts) '(3 . 2)))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-test-ids (drover-attention-sessions)) '("asking" "ended")))
      (should (equal (drover-view-subagent-counts) '(3 . 0))))
    ;; None: one waiting for the user still surfaces, not in lists.
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-ids (drover-surfaced-sessions)) '("root" "asking")))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("root"))))
      (should (equal (drover-view-subagent-counts) '(3 . 3)))
      ;; Matching a filter, it has no listed parent to explain: the mode
      ;; hides it still.
      (setq drover-filter '(:search "ended"))
      (should-not (drover-view-trees))
      (should (equal (drover-view-subagent-counts) '(1 . 1))))))

(defun drover-test--orphans (quiet)
  "Return sessions with subagents whose parents are not listed, QUIET the status of one."
  (list (drover-test-session "root" :status 'idle)
        (drover-test-session "kid" :parent "root" :status 'idle :title "Wanted kid")
        (drover-test-session "idle" :parent "gone" :status 'idle)
        (drover-test-session "busy" :parent "gone" :status 'working)
        (drover-test-session "quiet" :parent "gone" :status quiet)
        (drover-test-session "deep" :parent "quiet" :status 'idle :title "Wanted deep")
        (drover-test-session "a" :parent "b" :status 'idle)
        (drover-test-session "b" :parent "a" :status 'idle)))

(ert-deftest drover-test-filter-leaves-orphans-to-the-mode ()
  "A filter never shows a subagent at the top of a tree of its own.
Its parent not listed, it explains no parent: it shows as
`drover-show-subagents' says, matching or not, and so does the top of
a cycle of parents.  A filter left on, such as one leaving out a host,
used to bring back every such subagent the mode hides.  A subagent
matching under a listed parent still shows."
  (drover-test-with-backend (drover-test--orphans 'idle)
    (dolist (mode drover-subagent-modes)
      (let ((drover-show-subagents mode)
            (drover-filter nil))
        (let ((shape (drover-test-tree-shape (drover-view-trees)))
              (counts (drover-view-subagent-counts)))
          (setq drover-filter '(:host (:exclude "elsewhere")))
          (should (equal (drover-test-tree-shape (drover-view-trees)) shape))
          (should (equal (drover-view-subagent-counts) counts))
          (when (eq mode 'active)
            (should (equal shape '(("busy") ("root"))))))))
    ;; Matching below a root it does not explain, a subagent shows as the
    ;; root does.
    (setq drover-filter '(:search "Wanted deep"))
    (should-not (drover-view-trees))
    (should (equal (drover-view-subagent-counts) '(2 . 2)))
    (setq drover-test-sessions (drover-test--orphans 'working))
    (should (equal (drover-test-tree-shape (drover-view-trees)) '(("quiet" ("deep")))))
    ;; Under a listed parent, a match shows whatever the mode.
    (let ((drover-show-subagents 'none))
      (setq drover-filter '(:search "Wanted kid"))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("root" ("kid"))))))))

(ert-deftest drover-test-subagent-modes-cycle ()
  "The modes cycle all, active, none; setting one redraws the views."
  (drover-test-with-backend nil
                            (let ((runs 0))
                              (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
                              (should (eq (drover-next-subagent-mode 'all) 'active))
                              (should (eq (drover-next-subagent-mode 'active) 'none))
                              (should (eq (drover-next-subagent-mode 'none) 'all))
                              (should (eq (drover-next-subagent-mode) 'none))
                              (drover-set-show-subagents 'all)
                              (should (eq drover-show-subagents 'all))
                              (should (= runs 1))
                              (should-error (drover-set-show-subagents t))
                              (should (eq (default-value 'drover-show-subagents) 'all))
                              (should (eq (eval (car (get 'drover-show-subagents 'standard-value)) t) 'active)))))

(ert-deftest drover-test-tree-active-subagents ()
  "Each session knows its subagents in the middle of a turn, at any depth."
  (let ((table (drover-tree-active-subagents
                (drover-tree
                 (list (drover-test-session "root" :status 'idle)
                       (drover-test-session "quiet" :parent "root" :status 'idle)
                       (drover-test-session "deep" :parent "quiet" :status 'working :updated 1.0)
                       (drover-test-session "asking" :parent "root" :status 'waiting :since 2.0)
                       (drover-test-session "ended" :parent "root" :status 'done)
                       (drover-test-session "retry" :parent "ended" :status 'retrying)
                       (drover-test-session "alone" :status 'working))))))
    ;; Most urgent first.
    (should (equal (drover-test-ids (gethash '(fake . "root") table)) '("asking" "retry" "deep")))
    (should (equal (drover-test-ids (gethash '(fake . "quiet") table)) '("deep")))
    (should (equal (drover-test-ids (gethash '(fake . "ended") table)) '("retry")))
    (dolist (id '("deep" "asking" "retry" "alone"))
      (should-not (gethash (cons 'fake id) table)))
    (should (= (hash-table-count table) 3))))

(ert-deftest drover-test-tree-keeps-order-of-ties ()
  "Trees and siblings that rank alike keep the backend's order, as `drover-sort' does."
  (let ((nodes (drover-tree (list (drover-test-session "a")
                                  (drover-test-session "b")
                                  (drover-test-session "a1" :parent "a")
                                  (drover-test-session "a2" :parent "a")
                                  (drover-test-session "c")))))
    (should (equal (drover-test-tree-shape nodes) '(("a" ("a1") ("a2")) ("b") ("c"))))))

(ert-deftest drover-test-tree-visible-ranks-by-what-shows ()
  "A hidden subagent does not place its parent in the subagent's group."
  (let ((nodes (drover-tree (list (drover-test-session "parent" :status 'idle)
                                  (drover-test-session "ended" :parent "parent" :status 'done)
                                  (drover-test-session "busy" :parent "parent" :status 'working)))))
    (should (eq (drover-node-status (car nodes)) 'done))
    ;; The finished subagent hides, so its parent is not listed as done.
    (let ((drover-show-subagents 'active))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'working)))
    (let ((drover-show-subagents 'none))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'idle)))
    (let ((drover-show-subagents 'all))
      (should (eq (drover-node-status (car (drover-tree-visible nodes))) 'done)))))

(defun drover-test--record-windows ()
  "Make the fake backend's :open record the window selected, as (:open ID OTHER WINDOW).
It shows a buffer of its own for the session there, as front ends do."
  (setf (alist-get 'fake drover--backends)
        (append (list :open (lambda (session other-window)
                              (push (list :open (drover-session-id session) other-window (selected-window))
                                    drover-test-calls)
                              (switch-to-buffer (get-buffer-create
                                                 (format " *session %s*" (drover-session-id session)))))
                      :buffer (lambda (session)
                                (get-buffer (format " *session %s*" (drover-session-id session)))))
                (alist-get 'fake drover--backends))))

(defmacro drover-test-in-windows (&rest body)
  "Run BODY with the selected window alone in its frame, restoring windows after.
The buffers of sessions shown are killed afterwards."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (save-window-excursion
         (delete-other-windows)
         (set-window-dedicated-p (selected-window) nil)
         ,@body)
     (dolist (buffer (buffer-list))
       (when (string-prefix-p " *session " (buffer-name buffer))
         (kill-buffer buffer)))))

(ert-deftest drover-test-open-in-main-window ()
  "A list naming its main window opens sessions there, selected; o in another one."
  (drover-test-with-backend (list (drover-test-session "a"))
    (drover-test--record-windows)
    (drover-test-in-windows
      (let* ((list-window (selected-window))
             (main (split-window list-window nil 'right))
             (session (car drover-test-sessions))
             (list-buffer (get-buffer-create " *session list*")))
        (set-window-buffer list-window list-buffer)
        (with-current-buffer list-buffer
          (setq-local drover-view-p t
                      drover-main-window-function (lambda () main))
          (drover-open session))
        (should (eq (selected-window) main))
        (should (equal (car drover-test-calls) (list :open "a" nil main)))
        ;; Another window than the main window: there is none, so the
        ;; main window splits, and the list keeps its window and buffer.
        (select-window list-window)
        (with-current-buffer list-buffer
          (drover-open session t))
        (let ((other (selected-window)))
          (should-not (memq other (list main list-window)))
          (should (equal (car drover-test-calls) (list :open "a" nil other)))
          (should (= (length (window-list nil 'nomini)) 3))
          (should (eq (window-buffer list-window) list-buffer))
          ;; A window showing the session wins over one used later.
          (set-window-buffer main (get-buffer-create " *session main*"))
          (let ((fourth (split-window main nil 'below)))
            (set-window-buffer fourth (get-buffer-create " *session fourth*"))
            (select-window fourth)
            (select-window list-window)
            (with-current-buffer list-buffer
              (drover-open session t))
            (should (eq (selected-window) other))
            ;; Without one, the window used last.
            (set-window-buffer other (get-buffer-create " *session other*"))
            (select-window fourth)
            (select-window list-window)
            (with-current-buffer list-buffer
              (drover-open session t))
            (should (eq (selected-window) fourth))
            (should (= (length (window-list nil 'nomini)) 4))
            (should (eq (window-buffer list-window) list-buffer))))))))

(ert-deftest drover-test-open-without-main-window ()
  "Without a main window, sessions open in the selected window, or another one."
  (drover-test-with-backend (list (drover-test-session "a"))
    (drover-test--record-windows)
    (drover-test-in-windows
      (let ((origin (selected-window))
            (session (car drover-test-sessions)))
        (with-temp-buffer
          (drover-open session))
        (should (equal (car drover-test-calls) (list :open "a" nil origin)))
        ;; A dead or missing main window is no main window either: the
        ;; session's own window is not another window, so ORIGIN splits.
        (with-temp-buffer
          (setq-local drover-main-window-function #'ignore)
          (drover-open session t))
        (let ((other (selected-window)))
          (should-not (eq other origin))
          (should (equal (car drover-test-calls) (list :open "a" nil other)))
          ;; From the new window, the other one is ORIGIN again.
          (select-window origin)
          (switch-to-buffer (get-buffer-create " *session b*"))
          (select-window other)
          (drover-open session t)
          (should (eq (selected-window) origin))
          (should (= (length (window-list nil 'nomini)) 2)))))))

(ert-deftest drover-test-other-window-never-takes-a-list ()
  "Side windows, dedicated windows and drover's views are never another window."
  (drover-test-with-backend nil
    (drover-test-in-windows
      (let* ((origin (selected-window))
             (view (split-window origin nil 'right))
             (dedicated (split-window origin nil 'below))
             (view-buffer (get-buffer-create " *session view*"))
             (side (display-buffer-in-side-window (get-buffer-create " *session side*")
                                                  '((side . left)))))
        (with-current-buffer view-buffer
          (setq-local drover-view-p t))
        (set-window-buffer view view-buffer)
        (set-window-buffer dedicated (get-buffer-create " *session dedicated*"))
        (set-window-dedicated-p dedicated t)
        ;; The only windows besides ORIGIN cannot be taken: ORIGIN splits.
        (let ((other (drover-other-window origin)))
          (should-not (memq other (list origin view dedicated side)))
          (should (eq (window-frame other) (selected-frame)))
          (delete-window other))
        (set-window-dedicated-p dedicated nil)
        (should (eq (drover-other-window origin) dedicated))
        (delete-window side)))))

(ert-deftest drover-test-group-trees-by-status ()
  "Trees group by status in `drover-status-order', unknown statuses last."
  (let ((groups (drover-group-trees-by-status
                 (drover-tree (list (drover-test-session "w" :status 'working :updated 2.0)
                                    (drover-test-session "odd" :status 'mystery)
                                    (drover-test-session "n" :status 'waiting)
                                    (drover-test-session "w2" :status 'working :updated 1.0)
                                    (drover-test-session "odd2" :status 'stranger))))))
    (should (equal (mapcar #'car groups) '(waiting working mystery stranger)))
    (should (equal (mapcar (lambda (node) (drover-session-id (drover-node-session node)))
                           (alist-get 'working groups))
                   '("w" "w2")))))

(ert-deftest drover-test-group-trees-by-project ()
  (let ((groups (drover-group-trees-by-project
                 (drover-tree (list (drover-test-session "a1" :project "a" :status 'idle)
                                    (drover-test-session "b1" :project "b" :status 'waiting :since 1.0)
                                    (drover-test-session "child" :parent "a1" :project "b" :status 'done))))))
    (should (equal (mapcar #'car groups) '("b" "a")))
    (should (equal (drover-test-tree-shape (cdr (assoc "a" groups))) '(("a1" ("child")))))))

;;;; New backend operations

(ert-deftest drover-test-new-operations-reach-backend ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((session (drover-find 'fake "s")))
      (drover-rename session "New title")
      (drover-delete session)
      (drover-fork session)
      (drover-compact session)
      (should (equal (reverse drover-test-calls)
                     '((:rename "s" "New title") (:delete "s") (:fork "s") (:compact "s")))))))

(ert-deftest drover-test-new-operations-unsupported ()
  (drover-test-with-backend nil
    (drover-register-backend 'bare :label "Bare" :sessions #'ignore)
    (let ((session (drover-session-create :backend 'bare :id "x")))
      (dolist (operation '(:rename :delete :fork :compact))
        (should-not (drover-supports-p session operation)))
      (should-error (drover-rename session "t") :type 'user-error)
      (should-error (drover-delete session) :type 'user-error)
      (should-error (drover-fork session) :type 'user-error)
      (should-error (drover-compact session) :type 'user-error))))

(ert-deftest drover-test-parent-and-subagents ()
  (drover-test-with-backend (list (drover-test-session "root" :updated 1.0)
                                  (drover-test-session "idle" :parent "root" :status 'idle)
                                  (drover-test-session "asking" :parent "root" :status 'waiting)
                                  (drover-test-session "orphan" :parent "gone")
                                  (drover-session-create :backend 'other :id "foreign" :parent "root"))
    (let ((root (drover-find 'fake "root")))
      (should (equal (drover-test-ids (drover-subagents root)) '("asking" "idle")))
      (should (eq (drover-parent (drover-find 'fake "asking")) (drover-find 'fake "root")))
      (should-not (drover-parent root))
      (should-not (drover-parent (drover-find 'fake "orphan"))))))

;;;; Commands acting on the session at point

(defmacro drover-test-at-point (id &rest body)
  "Run BODY in a buffer whose session at point is the fake session ID."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq-local drover-session-at-point-function (lambda () (drover-find 'fake ,id)))
     ,@body))

(ert-deftest drover-test-session-at-point-noerror ()
  (with-temp-buffer
    (should-not (drover-session-at-point t))
    (setq drover-session-at-point-function #'ignore)
    (should-not (drover-session-at-point t))
    (should-error (drover-session-at-point) :type 'user-error)))

(ert-deftest drover-test-session-list-p ()
  (with-temp-buffer
    (should-not (drover-session-list-p))
    (setq drover-view-p t)
    (should (drover-session-list-p)))
  (with-temp-buffer
    (drover-session-keys-mode)
    (should (drover-session-list-p))))

(ert-deftest drover-test-commands-act-at-point ()
  (drover-test-with-backend (list (drover-test-session "s" :title "Old"))
    (drover-test-at-point "s"
      (cl-letf (((symbol-function 'y-or-n-p) #'always)
                ((symbol-function 'yes-or-no-p) #'always)
                ((symbol-function 'read-string)
                 (lambda (prompt _initial _history default)
                   (should (string-match-p "Old" prompt))
                   (should (equal default "Old"))
                   "  Brand new  ")))
        (dolist (command '(drover-respond drover-interrupt drover-archive drover-mark-seen
                                          drover-rename drover-delete))
          (call-interactively command)))
      (should (equal (reverse drover-test-calls)
                     '((:respond "s") (:interrupt "s") (:archive "s") (:mark-seen "s")
                       (:rename "s" "Brand new") (:delete "s")))))))

(ert-deftest drover-test-commands-confirm ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-test-at-point "s"
      (let ((asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push (cons 'y prompt) asked) nil))
                  ((symbol-function 'yes-or-no-p) (lambda (prompt) (push (cons 'yes prompt) asked) nil)))
          (should-error (call-interactively #'drover-interrupt) :type 'user-error)
          (should-error (call-interactively #'drover-delete) :type 'user-error)
          (should-not drover-test-calls)
          ;; Archiving asks nothing.
          (call-interactively #'drover-archive))
        (should (equal drover-test-calls '((:archive "s"))))
        ;; Deleting is for good, so the answer is typed in full.
        (should (equal (reverse asked)
                       '((y . "Interrupt Session s? ") (yes . "Delete Session s for good? "))))))))

(ert-deftest drover-test-rename-needs-a-title ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (drover-test-at-point "s"
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (should-error (call-interactively #'drover-rename) :type 'user-error))
      (should-not drover-test-calls))))

(ert-deftest drover-test-commands-without-session ()
  (with-temp-buffer
    (dolist (command '(drover-respond drover-interrupt drover-archive drover-mark-seen
                                      drover-rename drover-delete))
      (should-error (call-interactively command) :type 'user-error))))

;;;; Prompt actions

(defvar drover-test-actions nil
  "Prompt actions performed, newest first, as (ACTION BUFFER WINDOW).")

(defun drover-test-action (action)
  "Return a command recording that it performed ACTION."
  (lambda ()
    (interactive)
    (push (list action (current-buffer) (selected-window)) drover-test-actions)))

(defmacro drover-test-with-buffers (sessions buffers &rest body)
  "Run BODY with the fake backend serving SESSIONS, showing them in BUFFERS.
BUFFERS is an alist (ID . BUFFER) of live buffers, killed afterwards."
  (declare (indent 2) (debug t))
  `(drover-test-with-backend ,sessions
     (let ((buffers ,buffers)
           (drover-test-actions nil)
           (drover-default-prompt-buffer-function nil))
       (unwind-protect
           (progn
             (setf (alist-get 'fake drover--backends)
                   (append (list :buffer (lambda (session)
                                           (cdr (assoc (drover-session-id session) buffers))))
                           (alist-get 'fake drover--backends)))
             ,@body)
         (mapc (lambda (entry) (when (buffer-live-p (cdr entry)) (kill-buffer (cdr entry))))
               buffers)))))

(ert-deftest drover-test-prompt-define ()
  (with-temp-buffer
    (drover-prompt-define 'send #'ignore)
    (drover-prompt-define 'attach #'ignore)
    (drover-prompt-define 'send #'identity)
    (should (equal drover-prompt-actions '((send . identity) (attach . ignore))))
    (drover-prompt-define 'send nil)
    (should (equal drover-prompt-actions '((attach . ignore))))
    (should (drover-prompt-defines-p (current-buffer) 'attach))
    (should-not (drover-prompt-defines-p (current-buffer) 'send))
    (let ((other (current-buffer)))
      (with-temp-buffer
        ;; The actions belong to their buffer.
        (should-not drover-prompt-actions)
        (should (drover-prompt-defines-p other 'attach))))))

(ert-deftest drover-test-prompt-target-current-buffer ()
  (let ((drover-prepared-prompt-function nil))
    (with-temp-buffer
      (should-not (drover-prompt-target 'send))
      (drover-prompt-define 'send #'ignore)
      (should (eq (drover-prompt-target 'send) (current-buffer)))
      (should-not (drover-prompt-target 'revert)))))

(ert-deftest drover-test-prompt-target-prepared-prompt ()
  "Send applies from anywhere to a prompt ready to send, a list included.
A buffer of its own to send comes first; the prompt counts only when it
defines Send, and a function failing tells none."
  (let* ((prepared (generate-new-buffer " prepared"))
         (ready t)
         (drover-prepared-prompt-function (lambda () (and ready prepared)))
         (drover-test-actions nil))
    (unwind-protect
        (progn
          (with-current-buffer prepared
            (drover-prompt-define 'send (drover-test-action 'send)))
          (with-temp-buffer
            (should (eq (drover-prompt-target 'send) prepared))
            ;; Only Send.
            (should-not (drover-prompt-target 'revert))
            (drover-prompt-run 'send)
            (should (equal (car (car drover-test-actions)) 'send))
            (should (eq (nth 1 (car drover-test-actions)) prepared))
            ;; In a list too.
            (setq-local drover-view-p t)
            (should (eq (drover-prompt-target 'send) prepared))
            (kill-local-variable 'drover-view-p)
            ;; Its own first.
            (drover-prompt-define 'send #'ignore)
            (should (eq (drover-prompt-target 'send) (current-buffer))))
          (with-temp-buffer
            (setq ready nil)
            (should-not (drover-prompt-target 'send))
            (should-error (drover-prompt-run 'send) :type 'user-error)
            (setq ready t)
            (with-current-buffer prepared
              (drover-prompt-define 'send nil))
            (should-not (drover-prompt-target 'send))
            (let ((drover-prepared-prompt-function (lambda () (error "Broken"))))
              (should-not (drover-prompt-target 'send)))))
      (kill-buffer prepared))))

(ert-deftest drover-test-prompt-target-session-buffer-in-list ()
  (let ((drover-prepared-prompt-function nil))
    (drover-test-with-buffers (list (drover-test-session "s"))
        (list (cons "s" (generate-new-buffer " s")))
      (with-current-buffer (cdr (assoc "s" buffers))
        (drover-prompt-define 'agent (drover-test-action 'agent))
        (drover-prompt-define 'revert (drover-test-action 'revert))
        (drover-prompt-define 'fork (drover-test-action 'fork)))
      (drover-test-at-point "s"
        ;; Outside a list, the session's buffer does not count.
        (should-not (drover-prompt-target 'agent))
        (setq-local drover-view-p t)
        (should (eq (drover-prompt-target 'agent) (cdr (assoc "s" buffers))))
        ;; Editing a prompt needs the prompt in sight: never from a list.
        (should-not (drover-prompt-target 'revert))
        (should-not (drover-prompt-target 'send))
        ;; In a list, forking acts on the whole session through its backend.
        (should-not (drover-prompt-target 'fork))
        (drover-prompt-run 'agent)
        (should (equal (car (car drover-test-actions)) 'agent))
        (should (eq (nth 1 (car drover-test-actions)) (cdr (assoc "s" buffers))))))))

(ert-deftest drover-test-prompt-target-looks-at-point-when-needed ()
  "Only the actions a list's session answers look for the session at point."
  (let ((drover-prepared-prompt-function nil)
        (looked 0))
    (drover-test-with-buffers (list (drover-test-session "s"))
        (list (cons "s" (generate-new-buffer " s")))
      (with-current-buffer (cdr (assoc "s" buffers))
        (drover-prompt-define 'model (drover-test-action 'model)))
      (drover-test-at-point "s"
        (setq-local drover-view-p t)
        (add-function :before (local 'drover-session-at-point-function)
                      (lambda () (cl-incf looked)))
        (dolist (action '(send revert fork compact))
          (should-not (drover-prompt-target action)))
        (should (= looked 0))
        (should (eq (drover-prompt-target 'model) (cdr (assoc "s" buffers))))
        (should (= looked 1))))))

(ert-deftest drover-test-prompt-target-default-buffer ()
  (let ((default (generate-new-buffer " default"))
        (drover-default-prompt-buffer-function nil)
        (drover-prepared-prompt-function nil))
    (unwind-protect
        (progn
          (with-current-buffer default
            (drover-prompt-define 'model #'ignore)
            (drover-prompt-define 'send #'ignore))
          (with-temp-buffer
            (should-not (drover-prompt-target 'model))
            (setq drover-default-prompt-buffer-function (lambda () default))
            (dolist (action '(model))
              (should (eq (drover-prompt-target action) default)))
            ;; Only agent, model, effort and directory fall back on it,
            ;; and only those it defines.
            (should-not (drover-prompt-target 'send))
            (should-not (drover-prompt-target 'agent))
            (with-current-buffer default
              (drover-prompt-define 'agent #'ignore)
              (drover-prompt-define 'effort #'ignore)
              (drover-prompt-define 'directory #'ignore))
            (dolist (action '(agent effort directory))
              (should (eq (drover-prompt-target action) default)))
            ;; The current buffer comes first.
            (drover-prompt-define 'model #'ignore)
            (should (eq (drover-prompt-target 'model) (current-buffer)))
            (setq drover-default-prompt-buffer-function #'ignore)
            (should-not (drover-prompt-target 'agent))))
      (kill-buffer default))))

(ert-deftest drover-test-prompt-target-last-answer ()
  (let ((older (generate-new-buffer " older"))
        (newer (generate-new-buffer " newer"))
        (drover-test-actions nil))
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (list older newer))
            (with-current-buffer buffer
              (drover-prompt-define 'last-answer (drover-test-action 'last-answer))))
          (with-temp-buffer
            (switch-to-buffer older)
            (switch-to-buffer newer)
            (switch-to-buffer (get-buffer-create " elsewhere"))
            (should (eq (drover-prompt-target 'last-answer) newer))
            (drover-prompt-run 'last-answer)
            ;; Shown, then performed there.
            (should (eq (window-buffer (selected-window)) newer))
            (should (equal (car drover-test-actions) (list 'last-answer newer (selected-window))))))
      (kill-buffer older)
      (kill-buffer newer)
      (when (get-buffer " elsewhere") (kill-buffer " elsewhere")))))

(ert-deftest drover-test-prompt-run-in-shown-window ()
  "A target buffer shown in a window runs the action there, at its point."
  (drover-test-with-buffers (list (drover-test-session "s"))
      (list (cons "s" (generate-new-buffer " s")))
    (let ((target (cdr (assoc "s" buffers))))
      (with-current-buffer target
        (drover-prompt-define 'model (drover-test-action 'model)))
      (save-window-excursion
        (let ((window (split-window)))
          (set-window-buffer window target)
          (drover-test-at-point "s"
            (setq-local drover-view-p t)
            (let ((origin (selected-window)))
              (drover-prompt-run 'model)
              (should (equal (car drover-test-actions) (list 'model target window)))
              (should (eq (selected-window) origin)))))))))

(ert-deftest drover-test-prompt-run-without-target ()
  (let ((drover-prepared-prompt-function nil))
    (with-temp-buffer
      (should-error (drover-prompt-run 'send) :type 'user-error)
      (should-error (drover-prompt-run 'last-answer) :type 'user-error))))

(ert-deftest drover-test-fork-and-compact-dispatch ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((drover-test-actions nil))
      ;; In a prompt buffer, at point.
      (drover-test-at-point "s"
        (drover-prompt-define 'fork (drover-test-action 'fork))
        (drover-prompt-define 'compact (drover-test-action 'compact))
        (call-interactively #'drover-fork)
        (call-interactively #'drover-compact)
        (should (equal (mapcar #'car drover-test-actions) '(compact fork)))
        (should-not drover-test-calls))
      ;; In a list, the whole session.
      (drover-test-at-point "s"
        (setq-local drover-view-p t)
        (call-interactively #'drover-fork)
        (call-interactively #'drover-compact)
        (should (equal (reverse drover-test-calls) '((:fork "s") (:compact "s")))))
      ;; Elsewhere, nothing.
      (drover-test-at-point "s"
        (should-error (call-interactively #'drover-fork) :type 'user-error)
        (should-error (call-interactively #'drover-compact) :type 'user-error)))))

;;;; View settings

(defun drover-test-filtered (filter sessions)
  "Return the identifiers of SESSIONS that match FILTER."
  (drover-test-ids (seq-filter (lambda (session) (drover-filter-match-p session filter))
                               sessions)))

(ert-deftest drover-test-filter-project ()
  (let ((sessions (list (drover-test-session "a" :project "drover")
                        (drover-test-session "b" :project "drover-extra")
                        (drover-test-session "c"))))
    ;; Exact, as `drover-session-project' returns it.
    (should (equal (drover-test-filtered '(:project "drover") sessions) '("a")))
    (should (equal (drover-test-filtered '(:project "Drover") sessions) nil))))

(ert-deftest drover-test-filter-statuses ()
  (let ((sessions (list (drover-test-session "w" :status 'waiting)
                        (drover-test-session "d" :status 'done)
                        (drover-test-session "i" :status 'idle))))
    (should (equal (drover-test-filtered '(:statuses (waiting done)) sessions) '("w" "d")))
    (should (equal (drover-test-filtered '(:statuses (working)) sessions) nil))))

(ert-deftest drover-test-filter-agent-and-model ()
  (let ((sessions (list (drover-test-session "a" :agent "build" :model "opus")
                        (drover-test-session "b" :agent "plan" :model "opus")
                        (drover-test-session "c"))))
    (should (equal (drover-test-filtered '(:agent "build") sessions) '("a")))
    (should (equal (drover-test-filtered '(:model "opus") sessions) '("a" "b")))
    (should (equal (drover-test-filtered '(:model "opus" :agent "plan") sessions) '("b")))
    (should (drover-filter-active-p '(:agent "build")))))

(ert-deftest drover-test-filter-provider ()
  "The provider filter matches the provider's name, not its display."
  (let ((sessions (list (drover-test-session "a" :provider "A" :provider-name "Anthropic")
                        (drover-test-session "b" :provider-name "OpenAI")
                        (drover-test-session "c" :provider "Anthropic"))))
    (should (equal (drover-test-filtered '(:provider "Anthropic") sessions) '("a")))
    (should (equal (drover-test-filtered '(:provider "OpenAI") sessions) '("b")))
    (should (drover-filter-active-p '(:provider "OpenAI")))
    (drover-test-with-backend nil
      (drover-set-filter :provider "OpenAI")
      (should (equal drover-filter '(:provider "OpenAI"))))))

(ert-deftest drover-test-filter-since ()
  (let ((sessions (list (drover-test-session "old" :updated 99.0)
                        (drover-test-session "edge" :updated 100.0)
                        (drover-test-session "new" :updated 150.0)
                        (drover-test-session "never"))))
    ;; Updated at or after.
    (should (equal (drover-test-filtered '(:since 100.0) sessions) '("edge" "new")))))

(ert-deftest drover-test-filter-search ()
  (let ((sessions (list (drover-test-session "title" :title "Fix AUTH flow")
                        (drover-test-session "summary" :requests '((:kind question :summary "Use OAuth?")))
                        (drover-test-session "detail" :detail "auth server down")
                        (drover-test-session "project" :project "authz")
                        (drover-test-session "branch" :branch "feature/Auth")
                        (drover-test-session "agent" :agent "auth-agent")
                        (drover-test-session "none"))))
    ;; Case-insensitive, in the title, summary, project and branch.
    (should (equal (drover-test-filtered '(:search "auth") sessions)
                   '("title" "summary" "detail" "project" "branch")))
    ;; A literal string, not a regexp.
    (should (equal (drover-test-filtered '(:search "a.th") sessions) nil))
    (should (equal (length (drover-test-filtered '(:search "") sessions)) 7))))

(ert-deftest drover-test-filter-every-member ()
  (let ((sessions (list (drover-test-session "match" :project "p" :status 'done :updated 10.0 :title "x auth")
                        (drover-test-session "project" :project "q" :status 'done :updated 10.0 :title "auth")
                        (drover-test-session "status" :project "p" :status 'idle :updated 10.0 :title "auth")
                        (drover-test-session "since" :project "p" :status 'done :updated 1.0 :title "auth")
                        (drover-test-session "search" :project "p" :status 'done :updated 10.0 :title "x"))))
    (should (equal (drover-test-filtered '(:project "p" :statuses (done) :since 5.0 :search "auth") sessions)
                   '("match")))))

(ert-deftest drover-test-filter-active-p ()
  (let ((drover-filter nil))
    (should-not (drover-filter-active-p))
    (should-not (drover-filter-active-p '(:project nil :search "")))
    (should (drover-filter-active-p '(:search "x")))
    (setq drover-filter '(:statuses (done)))
    (should (drover-filter-active-p))))

(defun drover-test-family ()
  "Return sessions of a root, its subagents, and another tree."
  (list (drover-test-session "root" :project "drover" :status 'idle :updated 1.0)
        (drover-test-session "explore" :parent "root" :project "drover" :status 'idle :title "auth explorer")
        (drover-test-session "deep" :parent "explore" :project "drover" :status 'done :since 5.0)
        (drover-test-session "build" :parent "root" :project "drover" :status 'working)
        (drover-test-session "other" :project "other" :status 'working :title "auth elsewhere")))

(ert-deftest drover-test-filter-trees-keeps-ancestors ()
  (let ((trees (drover-tree (drover-test-family))))
    ;; A matching subagent keeps the sessions above it, and nothing else.
    (should (equal (drover-test-tree-shape (drover-filter-trees trees '(:statuses (done))))
                   '(("root" ("explore" ("deep"))))))
    (should (equal (drover-test-tree-shape (drover-filter-trees trees '(:search "auth")))
                   '(("other") ("root" ("explore")))))
    (should-not (drover-filter-trees trees '(:project "nowhere")))
    ;; Trees rank by what remains.
    (should (eq (drover-node-status (car (drover-filter-trees trees '(:statuses (idle))))) 'idle))))

(ert-deftest drover-test-view-trees-without-filter ()
  (drover-test-with-backend (drover-test-family)
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   (drover-test-tree-shape (drover-tree-visible (drover-tree (drover-test-family))))))
    ;; The explorer is quiet, and its own subagent ended: both hide.
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")) ("other"))))
    (should (= (drover-view-hidden-count) 2))
    (should (equal (drover-view-subagent-counts) '(3 . 2)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-view-subagent-counts) '(3 . 3))))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-view-subagent-counts) '(3 . 0))))))

(ert-deftest drover-test-view-trees-show-matching-subagents ()
  (drover-test-with-backend (drover-test-family)
    ;; Hidden subagents show when they explain why their tree is listed.
    (setq drover-filter '(:statuses (working)))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")) ("other"))))
    (should (= (drover-view-hidden-count) 0))
    (setq drover-filter '(:search "auth"))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("other") ("root" ("explore")))))
    ;; Even when no subagent shows: the match explains its parent.
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("other") ("root" ("explore")))))
      (setq drover-filter '(:statuses (working)))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("root" ("build")) ("other")))))))

(ert-deftest drover-test-view-trees-matching-root-hides-subagents ()
  "Subagents of a matching session show as the mode says, matching or not."
  (drover-test-with-backend (drover-test-family)
    (setq drover-filter '(:project "drover"))
    (should (equal (drover-test-tree-shape (drover-view-trees))
                   '(("root" ("build")))))
    (should (= (drover-view-hidden-count) 2))
    (let ((drover-show-subagents 'all))
      (should (equal (drover-test-tree-shape (drover-view-trees))
                     '(("root" ("explore" ("deep")) ("build")))))
      (should (= (drover-view-hidden-count) 0)))
    (let ((drover-show-subagents 'none))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("root"))))
      (should (= (drover-view-hidden-count) 3)))))

(ert-deftest drover-test-view-trees-explicit-sessions ()
  (drover-test-with-backend nil
    (let ((sessions (list (drover-test-session "a" :status 'done :since 1.0)
                          (drover-test-session "b" :status 'idle))))
      (setq drover-filter '(:statuses (done)))
      (should (equal (drover-test-tree-shape (drover-view-trees sessions)) '(("a"))))
      (should (= (drover-view-hidden-count sessions) 0)))))

(ert-deftest drover-test-settings-redraw-at-once ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-backend-changed 'fake)
      (drover-set-filter :project "drover")
      ;; The pending change ran with it.
      (should (= runs 1))
      (should-not drover--change-timer)
      (should (equal drover-filter '(:project "drover")))
      (drover-set-filter :statuses '(done))
      (drover-set-filter :project "other")
      (should (equal drover-filter '(:statuses (done) :project "other")))
      (drover-set-filter :project nil)
      (drover-set-filter :search "")
      (should (equal drover-filter '(:statuses (done))))
      (drover-set-filter :agent "build")
      (should (equal drover-filter '(:statuses (done) :agent "build")))
      (drover-set-filter :agent nil)
      (drover-set-group-by 'project)
      (should (eq drover-group-by 'project))
      (dolist (grouping '(branch agent model updated nil))
        (drover-set-group-by grouping)
        (should (eq drover-group-by grouping)))
      (should (= runs 13))
      (should-error (drover-set-filter :colour "red"))
      (should-error (drover-set-group-by 'colour)))))

;;;; Prompt caches

(ert-deftest drover-test-cache-fraction ()
  (let ((now drover-test-now))
    (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
      ;; Four minutes left of five.
      (let ((live (session :cache-expires (+ now 240) :cache-ttl 300)))
        (should (= (drover-session-cache-remaining live now) 240))
        (should (= (drover-session-cache-fraction live now) 0.8))
        ;; It shrinks as time passes.
        (should (= (drover-session-cache-fraction live (+ now 90)) 0.5))
        (should (< (drover-session-cache-fraction live (+ now 200))
                   (drover-session-cache-fraction live (+ now 100))))
        ;; Expired, at its expiry or after.
        (should-not (drover-session-cache-fraction live (+ now 240)))
        (should-not (drover-session-cache-remaining live (+ now 300))))
      ;; Unknown expiry or lifetime.
      (should-not (drover-session-cache-fraction (session) now))
      (should-not (drover-session-cache-remaining (session :cache-ttl 300) now))
      (should-not (drover-session-cache-fraction (session :cache-expires (+ now 60)) now))
      (should (= (drover-session-cache-remaining (session :cache-expires (+ now 60)) now) 60))
      (should-not (drover-session-cache-fraction (session :cache-expires (+ now 60) :cache-ttl 0) now))
      ;; More left than a lifetime, as clocks disagree: full.
      (should (= (drover-session-cache-fraction (session :cache-expires (+ now 400) :cache-ttl 300) now)
                 1.0)))))

;;;; What sessions run

(ert-deftest drover-test-cache-description ()
  "A prompt cache tells when it expires, its window and the share of the prompt it holds."
  (let ((now drover-test-now))
    (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
      (should (equal (drover-session-cache-description
                      (session :cache-expires (+ now 240) :cache-ttl 3600
                               :cache-tokens 120000 :cache-prompt-tokens 130000)
                      now)
                     "expires in 4m (1h window), 120k of 130k tokens cached"))
      ;; A service not telling the whole prompt, or the tokens at all.
      (should (equal (drover-session-cache-description
                      (session :cache-expires (+ now 40) :cache-ttl 300 :cache-tokens 900) now)
                     "expires in 40s (5m window), 900 tokens cached"))
      (should (equal (drover-session-cache-description (session :cache-expires (+ now 40)) now)
                     "expires in 40s"))
      (should-not (drover-session-cache-description (session :cache-expires (- now 1) :cache-ttl 300) now))
      (should-not (drover-session-cache-description (session) now)))))

(ert-deftest drover-test-context-fraction ()
  "A context fills a share of its model's, beyond 1 past it, and tells its tier."
  (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
    (should (= (drover-session-context-fraction (session :context-tokens 50000 :context-limit 200000)) 0.25))
    (should (= (drover-session-context-fraction (session :context-tokens 300000 :context-limit 200000)) 1.5))
    (should-not (drover-session-context-fraction (session :context-tokens 50000)))
    (should-not (drover-session-context-fraction (session :context-limit 200000)))
    (should-not (drover-session-context-fraction (session :context-tokens 5 :context-limit 0)))
    (should (equal (drover-session-context-description (session :context-tokens 50000 :context-limit 200000))
                   "50k of 200k tokens (25%)"))
    (should (equal (drover-session-context-description (session :context-tokens 950))
                   "950 tokens"))
    (should (equal (drover-session-context-description
                    (session :context-tokens 1200000 :context-limit 1000000 :context-tier '(over 200000 "2×")))
                   "1.2M of 1M tokens (120%)\nPast 200k tokens: requests cost 2×"))
    (should (equal (drover-session-context-description
                    (session :context-tokens 186000 :context-limit 200000 :context-tier '(near 200000 "2×")))
                   "186k of 200k tokens (93%)\nNear 200k tokens, past which requests cost 2×"))
    (should-not (drover-session-context-description (session :context-limit 200000)))))

(ert-deftest drover-test-context-estimated ()
  "A context the service estimated after a compaction says so, and its tier too."
  (cl-flet ((session (&rest properties) (apply #'drover-test-session "s" properties)))
    (should-not (drover-session-context-estimated (session :context-tokens 50000)))
    (should (equal (drover-session-context-description
                    (session :context-tokens 50000 :context-limit 200000 :context-estimated t))
                   (concat "~50k of 200k tokens (25%)\n"
                           "Estimated by the service after compaction; the next step measures it")))
    (should (equal (drover-session-context-description
                    (session :context-tokens 1200000 :context-limit 1000000 :context-estimated t
                             :context-tier '(over 200000 "2×")))
                   (concat "~1.2M of 1M tokens (120%)\n"
                           "Estimated by the service after compaction; the next step measures it\n"
                           "Estimated past 200k tokens: requests cost 2×")))
    (should (equal (drover-session-context-description
                    (session :context-tokens 186000 :context-estimated t
                             :context-tier '(near 200000 "2×")))
                   (concat "~186k tokens\n"
                           "Estimated by the service after compaction; the next step measures it\n"
                           "Estimated near 200k tokens, past which requests cost 2×")))
    ;; Without a size, nothing is estimated.
    (should-not (drover-session-context-description (session :context-estimated t)))))

(ert-deftest drover-test-session-record-keeps-its-layout ()
  "New slots of the session record come last, after those compiled code reads."
  (let ((slots (mapcar #'car (cdr (cl-struct-slot-info 'drover-session)))))
    (should (equal (last slots 11) '(agent-display context-estimated archived host origin stale default-branch
                                     brief brief-recap changes brief-session)))
    (should (equal (seq-take slots 4) '(backend id title directory)))))

(ert-deftest drover-test-format-tokens ()
  (should (equal (mapcar #'opencode-format-tokens
                         '(0 950 999 1000 8400 9940 9960 182345 999499 999500 1200000 12345678))
                 '("0" "950" "999" "1k" "8.4k" "9.9k" "10k" "182k" "999k" "1M" "1.2M" "12M"))))

(ert-deftest drover-test-prefetch ()
  "Each backend is asked once for its own sessions, in order; one failing spares the others."
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b"))
    (let ((other nil))
      (drover-test-register-simple 'other nil
                                   :prefetch (lambda (sessions)
                                               (push (mapcar #'drover-session-id sessions) other)))
      (drover-test-register-simple 'broken nil :prefetch (lambda (_) (error "Down")))
      (drover-test-register-simple 'plain nil)
      (let ((x (drover-session-create :backend 'other :id "x"))
            (y (drover-session-create :backend 'other :id "y"))
            (z (drover-session-create :backend 'broken :id "z"))
            (p (drover-session-create :backend 'plain :id "p")))
        (let ((inhibit-message t))
          (drover-prefetch (list (drover-test-session "b") x z p (drover-test-session "a") y)))
        (should (equal drover-test-prefetched '(("b" "a"))))
        (should (equal other '(("x" "y"))))
        ;; Nothing to ask, nothing asked.
        (drover-prefetch nil)
        (should (= (length drover-test-prefetched) 1))))))

;;;; Sorting

(defun drover-test-sortable ()
  "Return trees to sort: three roots, one with three subagents."
  (drover-tree
   (list (drover-test-session "b" :title "Bravo" :status 'waiting :since 1.0 :agent "plan"
                              :created 30.0 :updated 5.0)
         (drover-test-session "b2" :parent "b" :title "zulu" :status 'idle :created 3.0 :updated 50.0)
         (drover-test-session "b1" :parent "b" :title "Alpha" :status 'idle :created 2.0 :updated 40.0)
         (drover-test-session "b3" :parent "b" :title "mike" :status 'working :created 1.0 :updated 45.0)
         (drover-test-session "c" :title "charlie" :status 'working :agent "build" :created 10.0
                              :updated 20.0)
         (drover-test-session "a" :title "alpha" :status 'idle :created 20.0 :updated 10.0))))

(ert-deftest drover-test-sort-trees-by-urgency ()
  (let ((trees (drover-test-sortable)))
    (should (equal (drover-test-tree-shape trees) '(("b" ("b3") ("b2") ("b1")) ("c") ("a"))))
    ;; The default order is that of the trees, untouched.
    (should (eq (drover-sort-trees trees 'urgency) trees))
    (let ((drover-sort-by 'urgency) (drover-sort-descending nil))
      (should (eq (drover-sort-trees trees) trees)))
    ;; Reversed: least urgent first, subagents still under their parent.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'urgency t))
                   '(("a") ("c") ("b" ("b1") ("b2") ("b3")))))))

(ert-deftest drover-test-sort-trees-keeps-subagents-under-parent ()
  (let ((trees (drover-test-sortable)))
    ;; Roots sorted, and siblings among themselves, ignoring case.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'title))
                   '(("a") ("b" ("b1") ("b3") ("b2")) ("c"))))
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'title t))
                   '(("c") ("b" ("b2") ("b3") ("b1")) ("a"))))
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'created))
                   '(("c") ("a") ("b" ("b3") ("b1") ("b2")))))
    ;; The last activity of a tree is its latest; running sessions,
    ;; active now, come first, b3 bringing its tree with it.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'updated t))
                   '(("b" ("b3") ("b2") ("b1")) ("c") ("a"))))
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'updated))
                   '(("a") ("c") ("b" ("b1") ("b2") ("b3")))))
    ;; The trees keep their ranks.
    (should (eq (drover-node-status (car (drover-sort-trees trees 'title t))) 'working))
    (should (equal (drover-test-tree-shape trees) '(("b" ("b3") ("b2") ("b1")) ("c") ("a"))))))

(ert-deftest drover-test-sort-trees-missing-values-last ()
  "Sessions without a value come last, both ways; ties keep urgency order."
  (let ((trees (drover-test-sortable)))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'agent)))
                   '("c" "b" "a")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'agent t)))
                   '("b" "c" "a")))
    ;; No project anywhere: the order of urgency stands.
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'project t)))
                   '("b" "c" "a")))
    (should-error (drover-sort-trees trees 'colour))))

(ert-deftest drover-test-sort-trees-by-provider ()
  "Trees sort by the name of their provider, sessions without one last."
  (let ((trees (drover-tree (list (drover-test-session "x" :status 'waiting :provider-name "openai")
                                  (drover-test-session "y" :status 'done)
                                  (drover-test-session "z" :provider "icon" :provider-name "Anthropic")))))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'provider)))
                   '("z" "x" "y")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'provider t)))
                   '("x" "z" "y")))))

(ert-deftest drover-test-set-sort ()
  (drover-test-with-backend nil
    (let ((runs 0))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-set-sort 'title t)
      (should (eq drover-sort-by 'title))
      (should (eq drover-sort-descending t))
      (drover-set-sort 'created)
      (should-not drover-sort-descending)
      (should (= runs 2))
      (should-error (drover-set-sort 'colour)))))

(ert-deftest drover-test-view-defaults ()
  "Views list sessions ungrouped, by last activity, newest first."
  (cl-flet ((standard (symbol) (eval (car (get symbol 'standard-value)) t)))
    (should (eq (standard 'drover-sort-by) 'updated))
    (should (eq (standard 'drover-sort-descending) t))
    (should-not (standard 'drover-group-by))
    (should (eql (standard 'drover-sort-activity-tolerance) 60))))

;;;; Activity

(ert-deftest drover-test-session-activity ()
  "A running session is active now, from the start of its turn; another from its last activity."
  (cl-flet ((activity (&rest properties)
              (drover-session-activity (apply #'drover-test-session "s" properties))))
    ;; Its events do not move a running session: its turn's start places it.
    (should (equal (activity :status 'working :since 100.0 :updated 190.0) '(t . 100.0)))
    (should (equal (activity :status 'retrying :since 100.0 :updated 190.0) '(t . 100.0)))
    ;; Without it, its last event.
    (should (equal (activity :status 'working :updated 190.0) '(t . 190.0)))
    ;; Needing the user, since it needs them; otherwise its last activity.
    (should (equal (activity :status 'waiting :since 150.0 :updated 140.0) '(nil . 150.0)))
    (should (equal (activity :status 'done :since 170.0 :updated 160.0) '(nil . 170.0)))
    (should (equal (activity :status 'idle :since 120.0 :updated 160.0) '(nil . 160.0)))
    (should (equal (activity :status 'idle :since 120.0) '(nil . 120.0)))
    (should (equal (activity :status 'idle) '(nil)))
    ;; Its age counts from its last activity, its last event while it runs.
    (should (= (drover-session-last-activity (drover-test-session "s" :status 'working :since 100.0 :updated 190.0))
               190.0))
    (should (= (drover-session-last-activity (drover-test-session "s" :status 'done :since 170.0 :updated 160.0))
               170.0))))

(ert-deftest drover-test-activity-tolerance ()
  "A session's time moves once its activity moves further than the tolerance from it."
  (drover-test-with-index (list (drover-test-session "a" :status 'idle :updated 1000.0)
                                (drover-test-session "b" :status 'idle :updated 1010.0))
    (cl-flet ((update (time)
                (setf (drover-session-updated (car drover-test-sessions)) time)
                (drover-backend-changed 'fake "a")
                (drover-changed-now))
              (sorted (&optional id)
                (cdr (drover-session-activity (drover-index-session (cons 'fake (or id "a"))))))
              (order ()
                (mapcar (lambda (node) (drover-session-id (drover-node-session node)))
                        (drover-sort-trees (drover-tree (drover-index-sessions)) 'updated t))))
      (should (= (sorted) 1000.0))
      (should (equal (order) '("b" "a")))
      ;; Within a minute of its time: it stays, below b though newer.
      (update 1030.0)
      (should (= (sorted) 1000.0))
      (should (equal (order) '("b" "a")))
      (update 1060.0)
      (should (= (sorted) 1000.0))
      ;; Further: it moves.
      (update 1061.0)
      (should (= (sorted) 1061.0))
      (should (equal (order) '("a" "b")))
      ;; Back by less than a minute, it stays; by more, it moves.
      (update 1020.0)
      (should (= (sorted) 1061.0))
      (update 1000.0)
      (should (= (sorted) 1000.0))
      ;; Records not from the index are placed alike.
      (should (= (cdr (drover-session-activity (drover-test-session "a" :updated 1040.0))) 1000.0))
      (should (= (cdr (drover-session-activity (drover-test-session "a" :updated 1070.0))) 1070.0))
      ;; The index made anew keeps the times.
      (update 1050.0)
      (setq drover--index nil)
      (drover-index)
      (should (= (sorted) 1000.0))
      (should (= (sorted "b") 1010.0))
      ;; Exact times without a tolerance.
      (dolist (tolerance '(nil 0))
        (let ((drover-sort-activity-tolerance tolerance))
          (should (= (sorted) 1050.0))
          (should (equal (order) '("a" "b")))))
      ;; A session gone is forgotten.
      (setq drover-test-sessions (cdr drover-test-sessions))
      (drover-backend-changed 'fake "a")
      (drover-changed-now)
      (should-not (gethash '(fake . "a") drover--activity-times))
      (should (gethash '(fake . "b") drover--activity-times)))))

(defun drover-test-active-trees ()
  "Return trees of sessions running, idle and needing the user."
  (drover-tree
   (list (drover-test-session "p" :status 'working :since 100.0 :updated 300.0)
         (drover-test-session "s1" :parent "p" :status 'working :since 200.0 :updated 290.0)
         (drover-test-session "s2" :parent "p" :status 'idle :updated 310.0)
         (drover-test-session "q" :status 'idle :updated 250.0)
         (drover-test-session "r" :status 'done :since 260.0)
         (drover-test-session "x" :parent "r" :status 'idle :updated 270.0)
         (drover-test-session "n" :status 'working :since 150.0 :updated 160.0)
         (drover-test-session "u" :status 'idle))))

(ert-deftest drover-test-tree-activity ()
  "A tree runs from the earliest turn of its running sessions, else is as active as its latest."
  (let ((trees (drover-test-active-trees)))
    (cl-flet ((activity (id)
                (drover-node-activity (seq-find (lambda (node) (equal (drover-session-id (drover-node-session node)) id))
                                                trees))))
      ;; The subagent's later turn does not move its parent's tree.
      (should (equal (activity "p") '(t . 100.0)))
      (should (equal (activity "n") '(t . 150.0)))
      (should (equal (activity "q") '(nil . 250.0)))
      (should (equal (activity "r") '(nil . 270.0)))
      (should-not (activity "u")))
    ;; Newest first: running trees, newest turn first, then the others,
    ;; latest first, the unknown last; subagents alike under their parent.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'updated t))
                   '(("n") ("p" ("s1") ("s2")) ("r" ("x")) ("q") ("u"))))
    ;; Oldest first, the running last, the unknown still last.
    (should (equal (drover-test-tree-shape (drover-sort-trees trees 'updated))
                   '(("q") ("r" ("x")) ("p" ("s2") ("s1")) ("n") ("u"))))
    ;; By urgency, running sessions come newest turn first, whatever
    ;; event came last.
    (should (equal (mapcar #'car (drover-test-tree-shape trees)) '("r" "n" "p" "q" "u")))
    (should (equal (mapcar #'drover-session-id
                           (drover-sort (list (drover-test-session "a" :status 'working :since 100.0 :updated 500.0)
                                              (drover-test-session "b" :status 'working :since 200.0 :updated 300.0)
                                              (drover-test-session "c" :status 'idle :updated 400.0))))
                   '("b" "a" "c")))))

(ert-deftest drover-test-activity-ties ()
  "Trees alike in activity keep the order of urgency, the order sessions are listed in."
  (let ((trees (drover-tree (list (drover-test-session "a" :status 'working :since 100.0 :updated 101.0)
                                  (drover-test-session "b" :status 'working :since 100.0 :updated 109.0)
                                  (drover-test-session "c" :status 'working :since 100.0 :updated 105.0)))))
    (should (equal (mapcar #'car (drover-test-tree-shape trees)) '("a" "b" "c")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'updated t))) '("a" "b" "c")))
    (should (equal (mapcar #'car (drover-test-tree-shape (drover-sort-trees trees 'updated))) '("a" "b" "c")))))

(ert-deftest drover-test-tree-activity-bucket ()
  "A tree goes in the group of the time it sorts by; a running one is active today."
  (let* ((now (+ (drover--start-of-day drover-test-now) 43200.0))
         (trees (drover-tree
                 (list (drover-test-session "run" :status 'working :since (- now (* 3 86400)) :updated (- now 10))
                       ;; Its last event days ago, it asked a minute ago.
                       (drover-test-session "ask" :status 'waiting :since (- now 60) :updated (- now (* 3 86400)))
                       (drover-test-session "old" :status 'idle :updated (- now (* 30 86400)))
                       (drover-test-session "none" :status 'idle)))))
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-group-trees trees 'updated now))
                   '((today "ask" "run") (older "old") (nil "none"))))))

;;;; Grouping

(ert-deftest drover-test-group-trees-by-fields ()
  (let ((trees (drover-tree
                (list (drover-test-session "a" :branch "main" :agent "build" :model "opus"
                                           :status 'idle)
                      (drover-test-session "b" :branch "dev" :agent "plan" :status 'waiting :since 1.0)
                      (drover-test-session "kid" :parent "a" :branch "dev" :agent "plan" :status 'done)
                      (drover-test-session "c" :branch "main" :model "opus" :status 'working)))))
    ;; By the root of each tree; a group comes with its most urgent tree.
    (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                           (drover-group-trees trees 'branch))
                   '(("dev" ("b")) ("main" ("a" ("kid")) ("c")))))
    (should (equal (mapcar #'car (drover-group-trees trees 'agent)) '("plan" "build" nil)))
    (should (equal (mapcar #'car (drover-group-trees trees 'model)) '(nil "opus")))
    (should (equal (mapcar #'car (drover-group-trees trees 'provider)) '(nil)))
    (should (equal (mapcar #'car (drover-group-trees trees 'status)) '(waiting done working)))
    (should (equal (mapcar #'car (drover-group-trees trees 'project)) '(nil)))
    (should (equal (drover-group-trees trees nil) (list (cons nil trees))))
    (should-not (drover-group-trees nil nil))
    (should-error (drover-group-trees trees 'colour))))

(ert-deftest drover-test-group-trees-by-provider ()
  "Trees go with the provider name of their root, a group with its most urgent tree."
  (let ((trees (drover-tree
                (list (drover-test-session "a" :provider "A" :provider-name "Anthropic" :status 'idle)
                      (drover-test-session "b" :provider-name "OpenAI" :status 'waiting :since 1.0)
                      (drover-test-session "kid" :parent "a" :provider-name "OpenAI" :status 'done)
                      (drover-test-session "c" :provider "A" :provider-name "Anthropic" :status 'working)
                      (drover-test-session "d" :status 'idle)))))
    (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                           (drover-group-trees trees 'provider))
                   '(("OpenAI" ("b")) ("Anthropic" ("a" ("kid")) ("c")) (nil ("d")))))
    (should (equal (drover-group-label 'provider nil) "No provider"))))

(ert-deftest drover-test-activity-buckets ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let ((now drover-test-now)
              (hour 3600.0))
          (set-time-zone-rule "UTC")
          ;; drover-test-now is 08:00 UTC.
          (should (eq (drover-activity-bucket now now) 'today))
          (should (eq (drover-activity-bucket (- now (* 8 hour)) now) 'today))
          (should (eq (drover-activity-bucket (- now (* 8 hour) 1) now) 'yesterday))
          (should (eq (drover-activity-bucket (- now (* 32 hour)) now) 'yesterday))
          (should (eq (drover-activity-bucket (- now (* 32 hour) 1) now) 'week))
          ;; A week, today included: five days before yesterday.
          (should (eq (drover-activity-bucket (- now (* 8 hour) (* 6 24 hour)) now) 'week))
          (should (eq (drover-activity-bucket (- now (* 8 hour) (* 6 24 hour) 1) now) 'older))
          (should-not (drover-activity-bucket nil now)))
      (set-time-zone-rule zone))))

(ert-deftest drover-test-group-trees-by-activity ()
  (let ((zone (getenv "TZ")))
    (unwind-protect
        (let* ((now drover-test-now)
               (trees (drover-tree
                       (list (drover-test-session "old" :updated (- now (* 30 86400)))
                             (drover-test-session "never")
                             ;; The most urgent tree, yet its group comes after newer ones.
                             (drover-test-session "asking" :status 'waiting :since 1.0
                                                  :updated (- now (* 40 86400)))
                             (drover-test-session "today" :updated (- now 60))
                             (drover-test-session "parent" :updated (- now (* 3 86400)))
                             ;; A subagent active today brings its tree with it.
                             (drover-test-session "kid" :parent "parent" :updated (- now 120))
                             (drover-test-session "week" :updated (- now (* 4 86400)))))))
          (set-time-zone-rule "UTC")
          (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                                 (drover-group-trees trees 'updated now))
                         '((today "today" "parent") (week "week") (older "asking" "old")
                           (nil "never")))))
      (set-time-zone-rule zone))))

(ert-deftest drover-test-view-groups ()
  "Groups keep their order; trees sort within each group."
  (drover-test-with-backend (list (drover-test-session "w2" :title "Zed" :status 'waiting :since 1.0)
                                  (drover-test-session "w1" :title "Ann" :status 'waiting :since 2.0)
                                  (drover-test-session "i" :title "Bob" :status 'idle))
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '(((status . waiting) "w2" "w1") ((status . idle) "i"))))
    (setq drover-sort-by 'title)
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '(((status . waiting) "w1" "w2") ((status . idle) "i"))))
    (setq drover-group-by nil)
    (should (equal (mapcar (lambda (group) (cons (car group) (mapcar #'car (drover-test-tree-shape (cdr group)))))
                           (drover-view-groups))
                   '((nil "w1" "i" "w2"))))
    (setq drover-test-sessions nil)
    (should-not (drover-view-groups))))

;;;; Older sessions

(defvar drover-test-history-calls nil
  "Callbacks the fake backend's :history received, newest first.")

(defun drover-test-register-history ()
  "Give the fake backend a :history function recording its callbacks."
  (setf (alist-get 'fake drover--backends)
        (append (list :history (lambda (callback) (push callback drover-test-history-calls)))
                (alist-get 'fake drover--backends))))

(ert-deftest drover-test-history-merges-older-sessions ()
  (drover-test-with-backend (list (drover-test-session "live" :status 'working :title "Live"))
    (let ((drover-test-history-calls nil)
          (runs 0))
      (should-not (drover-history-available-p))
      (drover-test-register-history)
      (should (drover-history-available-p))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-set-show-history t)
      (should drover-show-history)
      (should (= (length drover-test-history-calls) 1))
      (should (= runs 1))
      ;; Nothing yet: the live sessions alone.
      (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
      (funcall (car drover-test-history-calls)
               (list (drover-test-session "live" :status 'idle :title "Stale record")
                     (drover-test-session "old" :status 'idle :updated 1.0)
                     (drover-test-session "old" :status 'idle :title "Duplicate")
                     (drover-test-session "old-kid" :parent "old")))
      (drover-changed-now)
      (should (= runs 2))
      ;; The live record wins; older ones follow, once each.
      (let ((known (drover-known-sessions)))
        (should (equal (drover-test-ids known) '("live" "old" "old-kid")))
        (should (equal (drover-session-title (car known)) "Live")))
      ;; Older sessions are found, listed, and have subagents.
      (should (equal (drover-session-title (drover-find 'fake "old")) "Session old"))
      (should (equal (drover-session-title (drover-find 'fake "live")) "Live"))
      (should (equal (drover-test-ids (drover-subagents (drover-find 'fake "old"))) '("old-kid")))
      (should (equal (drover-test-tree-shape (drover-view-trees)) '(("live") ("old"))))
      ;; The live sessions stay the backends' own.
      (should (equal (drover-test-ids (drover-sessions)) '("live")))
      (should (equal (drover-test-ids (drover-attention-sessions)) nil))
      ;; Hidden, they are forgotten.
      (drover-set-show-history nil)
      (should-not drover--history)
      (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
      (should-not (drover-find 'fake "old"))
      (should (= runs 3)))))

(ert-deftest drover-test-history-drops-late-answers ()
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-test-register-history)
      (drover-set-show-history t)
      (let ((first (car drover-test-history-calls)))
        ;; Fetched again, as a refresh does: only the last answer counts.
        (drover-history-fetch)
        (funcall first (list (drover-test-session "first")))
        (should-not drover--history)
        (funcall (car drover-test-history-calls) (list (drover-test-session "second")))
        (should (equal (drover-test-ids (drover-known-sessions)) '("second")))
        ;; An answer arriving once hidden is dropped.
        (drover-history-fetch)
        (drover-set-show-history nil)
        (funcall (car drover-test-history-calls) (list (drover-test-session "late")))
        (should-not drover--history)
        (should-not (drover-known-sessions))))))

(ert-deftest drover-test-history-of-unregistered-backend ()
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-test-register-history)
      (drover-set-show-history t)
      (funcall (car drover-test-history-calls) (list (drover-test-session "old")))
      (should (drover-known-sessions))
      (drover-unregister-backend 'fake)
      (should-not drover--history)
      (should-not (drover-known-sessions)))))

(ert-deftest drover-test-history-failing-backend ()
  "A backend whose :history fails leaves the others fetching."
  (drover-test-with-backend nil
    (let ((drover-test-history-calls nil))
      (drover-register-backend 'broken :label "Broken" :sessions #'ignore
                               :history (lambda (_) (error "No service")))
      (drover-test-register-history)
      (drover-set-show-history t)
      (should (= (length drover-test-history-calls) 1)))))

(ert-deftest drover-test-kept-fetched ()
  "Sessions kept fetched stay whether lists show them or not, lists unchanged."
  (drover-test-with-backend (list (drover-test-session "live"))
    (let* ((drover-test-history-calls nil)
           (backend-changes 0)
           (runs 0)
           (serial (drover-fetched-serial))
           (counter (lambda (&rest _) (cl-incf backend-changes))))
      (drover-test-register-history)
      (advice-add 'drover-backend-changed :before counter)
      (unwind-protect
          (progn
            (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
            (should-not (drover-fetch-wanted-p 'history))
            (drover-keep-fetched '(history) 60)
            (should (drover-fetch-wanted-p 'history))
            (should-not drover-show-history)
            (should (length= drover-test-history-calls 1))
            (should (drover-fetch-pending-p))
            ;; Asked again so soon, not fetched again; nil fetches anew.
            (drover-keep-fetched '(history) 60)
            (should (length= drover-test-history-calls 1))
            (drover-keep-fetched '(history))
            (should (length= drover-test-history-calls 2))
            (funcall (car drover-test-history-calls) (list (drover-test-session "old")))
            (should-not (drover-fetch-pending-p))
            (should (> (drover-fetched-serial) serial))
            ;; Not shown: the views have nothing to draw, the hook runs.
            (should (= backend-changes 0))
            (drover-changed-now)
            (should (= runs 1))
            (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
            (should (equal (drover-test-ids (drover-known-sessions nil t)) '("live" "old")))
            ;; Shown, then hidden, they stay.
            (drover-set-show-history t)
            (funcall (car drover-test-history-calls) (list (drover-test-session "old")))
            (should (> backend-changes 0))
            (drover-set-show-history nil)
            (should drover--history)
            (should (equal (drover-test-ids (drover-known-sessions nil t)) '("live" "old")))
            ;; A fetch never answered stops counting as pending after a while.
            (drover-keep-fetched '(history))
            (should (drover-fetch-pending-p 10))
            (setf (alist-get '(history . fake) drover--fetch-started nil nil #'equal) (- (float-time) 20))
            (should-not (drover-fetch-pending-p 10)))
        (advice-remove 'drover-backend-changed counter)))))

;;;; Engines of sessions

(ert-deftest drover-test-supports ()
  "A backend's :supports tells which of its operations apply to a session."
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b"))
    (setf (alist-get 'fake drover--backends)
          (append (list :supports (lambda (session operation)
                                    (not (and (eq operation :fork)
                                              (equal (drover-session-id session) "b")))))
                  (alist-get 'fake drover--backends)))
    (let ((a (drover-find 'fake "a"))
          (b (drover-find 'fake "b")))
      (should (drover-supports-p a :fork))
      (should-not (drover-supports-p b :fork))
      (should (drover-supports-p b :compact))
      ;; Not implemented, whatever :supports says.
      (should-not (drover-supports-p a :unarchive))
      (drover-fork a)
      (should (equal drover-test-calls '((:fork "a"))))
      (should-error (drover-fork b) :type 'user-error)
      (should (equal drover-test-calls '((:fork "a")))))
    ;; A failing :supports allows nothing, and is reported.
    (setf (alist-get 'fake drover--backends)
          (append (list :supports (lambda (_session _operation) (error "Boom")))
                  (alist-get 'fake drover--backends)))
    (should-not (drover-supports-p (drover-find 'fake "a") :fork))))

(defvar drover-test-archived-calls nil
  "Callbacks the fake backend's :archived received, newest first.")

(defun drover-test-register-archived ()
  "Give the fake backend :archived, recording its callbacks, and :unarchive."
  (setf (alist-get 'fake drover--backends)
        (append (list :archived (lambda (callback) (push callback drover-test-archived-calls))
                      :unarchive (drover-test--recorder :unarchive))
                (alist-get 'fake drover--backends))))

(ert-deftest drover-test-archived-sessions-shown ()
  "Archived sessions are fetched while shown, listed apart from the live ones."
  (drover-test-with-backend (list (drover-test-session "live" :status 'done :title "Live"))
    (let ((drover-test-archived-calls nil)
          (runs 0))
      (should-not (drover-archived-available-p))
      (drover-test-register-archived)
      (should (drover-archived-available-p))
      (add-hook 'drover-changed-hook (lambda () (cl-incf runs)))
      (drover-set-show-archived t)
      (should (= (length drover-test-archived-calls) 1))
      (should (= runs 1))
      (funcall (car drover-test-archived-calls)
               (list (drover-test-session "live" :archived 5.0 :title "Stale record")
                     (drover-test-session "old" :status 'done :archived 7.0)
                     (drover-test-session "unknown")
                     (drover-test-session "old-kid" :parent "old" :archived 7.0)))
      (drover-changed-now)
      (should (= runs 2))
      (let ((known (drover-known-sessions)))
        (should (equal (drover-test-ids known) '("live" "old" "unknown" "old-kid")))
        (should (equal (drover-session-title (car known)) "Live"))
        ;; Every archived record says so.
        (should (equal (mapcar #'drover-session-archived (cdr known)) '(7.0 t 7.0))))
      ;; In a group of their own, last, never needing the user.
      (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-ids (drover-tree-sessions (cdr group)))))
                             (drover-group-trees (drover-view-trees) 'status))
                     '((done "live") (archived "old" "unknown"))))
      (should (equal (drover-test-ids (drover-sessions)) '("live")))
      (should (equal (drover-test-ids (drover-attention-sessions)) '("live")))
      (should (equal (drover-session-archived (drover-find 'fake "old")) 7.0))
      ;; Fetched again, only the last answer counts; hidden, they go.
      (drover-archived-fetch)
      (let ((first (cadr drover-test-archived-calls)))
        (funcall first (list (drover-test-session "first" :archived 1.0)))
        (should-not (drover-find 'fake "first")))
      (drover-set-show-archived nil)
      (funcall (car drover-test-archived-calls) (list (drover-test-session "late" :archived 1.0)))
      (should-not drover--archived)
      (should (equal (drover-test-ids (drover-known-sessions)) '("live")))
      (should-not (drover-find 'fake "old")))))

(ert-deftest drover-test-archived-before-history ()
  "An archived record wins over an older one; both give way to a live one."
  (drover-test-with-backend (list (drover-test-session "live"))
    (let ((drover-test-archived-calls nil)
          (drover-test-history-calls nil))
      (drover-test-register-archived)
      (drover-test-register-history)
      (drover-set-show-archived t)
      (drover-set-show-history t)
      (funcall (car drover-test-archived-calls)
               (list (drover-test-session "s" :archived 1.0 :title "Archived")))
      (funcall (car drover-test-history-calls)
               (list (drover-test-session "s" :title "Older") (drover-test-session "live")
                     (drover-test-session "t")))
      (should (equal (mapcar #'drover-session-title (drover-known-sessions))
                     '("Session live" "Archived" "Session t")))
      ;; Each kind is fetched again from one backend alone.
      (drover-history-fetch 'other)
      (should (= (length drover-test-history-calls) 1))
      (drover-archived-fetch 'fake)
      (should (= (length drover-test-archived-calls) 2)))))

(ert-deftest drover-test-unarchive ()
  "Unarchiving restores the archived sessions selected, through their backend."
  (drover-test-with-backend (list (drover-test-session "live"))
    (let ((drover-test-archived-calls nil))
      (drover-test-register-archived)
      (drover-set-show-archived t)
      (funcall (car drover-test-archived-calls)
               (list (drover-test-session "a" :archived 1.0) (drover-test-session "b" :archived 2.0)))
      (let ((live (drover-find 'fake "live"))
            (a (drover-find 'fake "a")))
        (should (drover-unarchivable-p a))
        (should-not (drover-unarchivable-p live))
        (should-not (drover-archivable-p a))
        (should (drover-archivable-p live))
        (should-error (drover-unarchive live) :type 'user-error)
        (should-error (drover-archive a) :type 'user-error)
        (drover-unarchive a)
        (should (equal drover-test-calls '((:unarchive "a"))))
        ;; Of a selection, those archived.
        (setq drover-test-calls nil)
        (let ((drover-selected-sessions-function
               (lambda () (list live a (drover-find 'fake "b")))))
          (call-interactively #'drover-unarchive))
        (should (equal (reverse drover-test-calls) '((:unarchive "a") (:unarchive "b"))))))))

(ert-deftest drover-test-deleted-sessions-forgotten ()
  "A session deleted leaves the archived and older sessions fetched at once."
  (drover-test-with-backend nil
    (let ((drover-test-archived-calls nil)
          (drover-test-history-calls nil))
      (drover-test-register-archived)
      (drover-test-register-history)
      (drover-set-show-archived t)
      (drover-set-show-history t)
      (funcall (car drover-test-archived-calls) (list (drover-test-session "a" :archived 1.0)))
      (funcall (car drover-test-history-calls) (list (drover-test-session "b")))
      (drover-delete (list (drover-find 'fake "a") (drover-find 'fake "b")))
      (should (equal (reverse drover-test-calls) '((:delete "a") (:delete "b"))))
      (should-not (drover-known-sessions)))))

;;;; Drover's own archive

(defun drover-test-shown (id &rest properties)
  "Return a session ID of the backend `shown', which has no archive, with PROPERTIES.
Its engine is `claude-code', its engine identifier \"conv-ID\"."
  (apply #'drover-session-create
         (append properties (list :backend 'shown :id id :title (concat "Shown " id)
                                  :engine 'claude-code :engine-id (concat "conv-" id)))))

(defmacro drover-test-with-shown (sessions &rest body)
  "Run BODY with the backend `shown' serving SESSIONS, a variable, and drover's state fresh.
`shown' has no archive of its own, but deletes."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (drover-test-register-simple 'shown (lambda () ,sessions)
                                    :delete (drover-test--recorder :delete))
       ,@body)))

(ert-deftest drover-test-drover-archive ()
  "Sessions of a backend without an archive drover archives itself, by conversation."
  (let ((sessions (list (drover-test-shown "a" :status 'done :directory "/work/a/" :project "a"
                                           :branch "dev" :agent "build" :model "Opus 5.5"
                                           :effort "high" :created 10.0 :updated 20.0)
                        (drover-test-shown "b")
                        (drover-test-shown "unknown" :engine-id nil))))
    (drover-test-with-shown sessions
      (let ((a (drover-find 'shown "a")))
        (should (drover-archivable-p a))
        (should-not (drover-archivable-p (drover-find 'shown "unknown")))
        (should-not (drover-unarchivable-p a))
        (should-error (drover-archive-session (drover-find 'shown "unknown")) :type 'user-error)
        (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 100.0)))
          (drover-archive a))
        ;; It leaves the live sessions, and what needs the user.
        (should (equal (mapcar #'drover-session-id (drover-sessions)) '("b" "unknown")))
        (should-not (drover-attention-sessions))
        (should (drover-session-archived-p a))
        (should-error (drover-archive-session a) :type 'user-error)
        ;; Its own record is still found, telling it is archived.
        (should (eql (drover-session-archived (drover-find 'shown "a")) 100.0))
        (should (drover-unarchivable-p (drover-find 'shown "a")))
        ;; Shown, it is its live record, archived.
        (should (drover-archived-available-p))
        (let ((drover-show-archived t))
          (let ((archived (drover-archived-sessions)))
            (should (equal (mapcar #'drover-session-key archived) '((shown . "a"))))
            (should (eql (drover-session-archived (car archived)) 100.0))
            (should (eq (drover-session-status (car archived)) 'done)))
          (should (equal (mapcar #'drover-session-id (drover-known-sessions)) '("b" "unknown" "a")))
          ;; Its buffer gone, the record drover kept stands for it.
          (setq sessions (cdr sessions))
          (let ((kept (car (drover-archived-sessions))))
            (should (equal (drover-session-key kept) '(shown . "conv-a")))
            (should (equal (drover-session-engine-key kept) '(claude-code . "conv-a")))
            (should (equal (list (drover-session-title kept) (drover-session-directory kept)
                                 (drover-session-project kept) (drover-session-branch kept)
                                 (drover-session-agent kept) (drover-session-model kept)
                                 (drover-session-effort kept) (drover-session-created kept)
                                 (drover-session-updated kept) (drover-session-status kept)
                                 (drover-session-archived kept))
                           '("Shown a" "/work/a/" "a" "dev" "build" "Opus 5.5" "high" 10.0 20.0 idle 100.0)))
            (should (equal (drover-session-title (drover-find 'shown "conv-a")) "Shown a"))
            ;; Restored, nothing would list it: it stays archived, and
            ;; says why.
            (let ((err (should-error (drover-unarchive kept) :type 'user-error)))
              (should (string-match-p "\\`Shown a stays archived: no front end follows its conversation"
                                      (cadr err))))
            (should (equal (mapcar #'drover-session-key (drover-archived-sessions)) '((shown . "conv-a"))))
            (should (drover-session-archived-p kept))
            ;; Of several, it is named among those that failed; the
            ;; others are restored.
            (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 100.0)))
              (drover-archive (drover-find 'shown "b")))
            (let ((messages nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (format &rest arguments) (push (apply #'format-message format arguments) messages))))
                (drover-unarchive (list kept (drover-find 'shown "b"))))
              (should (string-match-p "\\`Unarchived 1 session; failed for Shown a (Shown a stays archived"
                                      (car messages))))
            (should (equal (mapcar #'drover-session-key (drover-archived-sessions)) '((shown . "conv-a"))))
            (should (equal (mapcar #'drover-session-id (drover-sessions)) '("b" "unknown")))
            ;; Open again, it is restored.
            (setq sessions (cons (drover-test-shown "a") sessions))
            (drover-unarchive (car (drover-archived-sessions)))
            (should-not (drover-archived-sessions))
            (should-not (drover-archived-available-p))
            (should (equal (mapcar #'drover-session-id (drover-sessions)) '("a" "b" "unknown")))))))))

(ert-deftest drover-test-unarchive-to-send ()
  "Before a prompt goes, what drover archived is restored; what its backend archived is left to it."
  (let ((sessions (list (drover-test-shown "a") (drover-test-shown "b"))))
    (drover-test-with-shown sessions
      (let ((a (drover-find 'shown "a")))
        ;; Not archived: nothing to do.
        (should-not (drover-unarchive-to-send a))
        (should-not (drover-archived-live-sessions))
        (should-not (drover-archived-conversation '(claude-code . "conv-a")))
        (drover-archive a)
        ;; Its buffer open, it is live, archived.
        (should (equal (mapcar #'drover-session-key (drover-archived-live-sessions)) '((shown . "a"))))
        (should (equal (drover-session-key (drover-archived-conversation '(claude-code . "conv-a")))
                       '(shown . "a")))
        (should (drover-unarchive-to-send a))
        (should-not (drover-session-archived-p a))
        (should-not (drover-archived-live-sessions))
        ;; Gone from every list, drover's record stands for it, and it
        ;; cannot be restored: told on one line.
        (drover-archive a)
        (setq sessions (cdr sessions))
        (let ((kept (drover-archived-conversation '(claude-code . "conv-a"))))
          (should (equal (drover-session-key kept) '(shown . "conv-a")))
          (let ((err (should-error (drover-unarchive-to-send kept) :type 'user-error)))
            (should (equal (cadr err)
                           (concat "Could not restore Shown a from the archive: no front end follows its "
                                   "conversation, and its engine keeps no record of it that drover lists"))))
          (should (drover-session-archived-p kept))))
      ;; Archived by its backend: the backend restores it as it sends.
      (let ((native (drover-test-session "n" :archived 1.0 :engine 'claude-code :engine-id "conv-n")))
        (should-not (drover-unarchive-to-send native))))))

(ert-deftest drover-test-unarchive-confirmed ()
  "A backend confirming restores has the record fetched as archived forgotten once it took."
  (drover-test-with-backend nil
    (let ((drover-test-archived-calls nil)
          (asked nil)
          (warnings nil))
      (drover-test-register-archived)
      (setf (alist-get 'fake drover--backends)
            (append (list :unarchive (lambda (session then else)
                                       (push (list (drover-session-id session) then else) asked))
                          :unarchive-confirms t)
                    (alist-get 'fake drover--backends)))
      (drover-set-show-archived t)
      (funcall (car drover-test-archived-calls)
               (list (drover-test-session "a" :archived 1.0) (drover-test-session "b" :archived 1.0)))
      (cl-letf (((symbol-function 'drover-warn)
                 (lambda (&rest arguments) (push (apply #'format arguments) warnings))))
        (drover-unarchive (list (drover-find 'fake "a") (drover-find 'fake "b")))
        (should (equal (mapcar #'car asked) '("b" "a")))
        (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("a" "b")))
        (funcall (nth 1 (cadr asked)))
        (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("b")))
        (funcall (nth 2 (car asked)) (concat "Refused\n" (make-string 1000 ?x)))
        (should (equal (mapcar #'drover-session-id (drover-archived-sessions)) '("b")))
        ;; Told once both answered, on one line.
        (should (= (length warnings) 1))
        (should (string-prefix-p "Unarchived 1 session; failed for Session b (Refused" (car warnings)))
        (should-not (string-search "\n" (car warnings)))
        (should-not drover--bulk-jobs))
      ;; Forgetting a record not fetched changes nothing.
      (setq drover--changed-backends nil)
      (should-not (drover-archived-left 'fake "a"))
      (should-not drover--changed-backends)
      (should (drover-archived-left 'fake "b"))
      (should (memq 'fake drover--changed-backends))
      (should-not (drover-archived-sessions)))))

(ert-deftest drover-test-drover-archive-file ()
  "Drover keeps what it archived in its state directory, read anew when it changes."
  (let ((sessions (list (drover-test-shown "a") (drover-test-shown "b"))))
    (drover-test-with-shown sessions
      (drover-archive (drover-find 'shown "a"))
      (let ((file (drover-archive-file)))
        (should (equal (file-name-nondirectory file) "archive.eld"))
        (should (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (should (looking-at-p ";;; Sessions drover archived, written by drover"))
          (let ((form (read (current-buffer))))
            (should (eql (plist-get form :version) 1))
            (should (equal (mapcar #'car (plist-get form :sessions)) '((claude-code . "conv-a"))))))
        ;; Another Emacs archives b: it is read again.
        (let ((drover--archive nil)
              (drover--archive-stamp 'unread))
          (drover-archive (drover-find 'shown "b")))
        (set-file-times file (time-add nil 10))
        (should (equal (mapcar #'drover-session-id (drover-sessions)) nil))
        ;; Restoring rewrites it.
        (drover-unarchive (drover-find 'shown "a"))
        (should (equal (mapcar #'drover-session-id (drover-sessions)) '("a")))
        (let ((drover--archive nil)
              (drover--archive-stamp 'unread))
          (should (equal (mapcar #'car (drover--archive)) '((claude-code . "conv-b")))))))))

(ert-deftest drover-test-drover-archive-bad-file ()
  "A file of another version or malformed is ignored, with one warning."
  (drover-test-with-shown (list (drover-test-shown "a"))
    (let ((warnings nil))
      (cl-letf (((symbol-function 'drover-warn)
                 (lambda (format &rest arguments) (push (apply #'format-message format arguments) warnings))))
        (dolist (content '("(:version 2 :sessions nil)" "(:version 1 :sessions (((claude-code . 3) :time 1)))"
                           "(:version 1 :sessions (((claude-code . \"x\") :title \"No time\")))" "(unbalanced"))
          (drover-test-write (drover-archive-file) content)
          (set-file-times (drover-archive-file) (time-add nil (length warnings)))
          (setq drover--archive-stamp 'unread)
          (should-not (drover--archive))
          (should (equal (mapcar #'drover-session-id (drover-sessions)) '("a"))))
        (should (= (length warnings) 1))
        (should (string-match-p "Ignoring the sessions drover archived" (car warnings)))))))

(ert-deftest drover-test-drover-archive-hides-every-front-end ()
  "A conversation drover archived leaves every backend's list, the older sessions too."
  (let ((sessions (list (drover-test-shown "a"))))
    (drover-test-with-shown sessions
      (drover-test-register-simple 'other (list (drover-test-other "x" :engine 'claude-code
                                                                   :engine-id "conv-a")
                                                (drover-test-other "y")))
      (drover-archive (drover-find 'shown "a"))
      (should (equal (mapcar #'drover-session-key (drover-sessions)) '((other . "y"))))
      (let ((drover-show-history t)
            (drover--history (list (cons 'other (list (drover-test-other "z" :engine 'claude-code
                                                                         :engine-id "conv-a"))))))
        (should (equal (mapcar #'drover-session-key (drover-known-sessions)) '((other . "y"))))))))

(ert-deftest drover-test-drover-archive-deleted ()
  "Deleting a session drover archived forgets it."
  (let ((sessions (list (drover-test-shown "a"))))
    (drover-test-with-shown sessions
      (drover-archive (drover-find 'shown "a"))
      (setq sessions nil)
      (let ((drover-show-archived t))
        (let ((kept (car (drover-archived-sessions))))
          (drover-delete kept)
          (should (equal drover-test-calls '((:delete "conv-a"))))
          (should-not (drover-archived-sessions))
          (should-not (drover--archive)))))))

(defun drover-test-claude-record (id &rest properties)
  "Return the record of Claude Code conversation ID by the backend `records'.
Its engine identifier is ID, but as PROPERTIES say otherwise; it is
idle."
  (apply #'drover-session-create
         (append properties (list :backend 'records :id id :title (concat "Record " id) :status 'idle
                                  :engine 'claude-code :engine-id id))))

(defun drover-test-register-claude-records (records &rest properties)
  "Register `records', listing Claude Code conversations as their engine records them.
RECORDS is a function returning them; PROPERTIES are more properties of
the backend, such as :history."
  (apply #'drover-test-register-simple 'records records
         :recorded '(claude-code)
         :session (lambda (id)
                    (seq-find (lambda (record) (equal (drover-session-id record) id)) (funcall records)))
         properties))

(ert-deftest drover-test-drover-archive-takes-subagents ()
  "A subagent goes with the conversation drover archived above it, at any depth.
It leaves the sessions in use with it, as a Claude Code subagent, which
has no conversation of its own, used to stay, at the top of a tree of its
own.  It shows under it among the archived sessions, archived when it
was; it is restored with it, not alone, and comes back with it.  A
session of another host is its host's to archive."
  (let ((sessions (list (drover-test-shown "a") (drover-test-shown "b")))
        (records (list (drover-test-claude-record "conv-a")
                       (drover-test-claude-record "conv-a/agent-1" :engine-id nil :parent "conv-a")
                       (drover-test-claude-record "conv-s1" :parent "conv-a")
                       (drover-test-claude-record "conv-s2" :parent "conv-s1")
                       (drover-test-claude-record "conv-b/agent-1" :engine-id nil :parent "conv-b")
                       (drover-test-claude-record "elsewhere" :parent "conv-a" :host "core"))))
    (drover-test-with-shown sessions
      (drover-test-register-claude-records (lambda () records)
                                           :unarchive (drover-test--recorder :unarchive))
      (cl-flet ((ids (sessions) (sort (mapcar #'drover-session-id sessions) #'string<)))
        (let ((all '("a" "b" "conv-a/agent-1" "conv-b/agent-1" "conv-s1" "conv-s2" "elsewhere")))
          (should (equal (ids (drover-sessions)) all))
          (should (equal (ids (drover-index-sessions)) all))
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 100.0)))
            (drover-archive (drover-find 'shown "a")))
          (drover-changed-now)
          ;; Its subagents leave with it; another conversation's stay, and
          ;; another host's session.
          (should (equal (ids (drover-sessions)) '("b" "conv-b/agent-1" "elsewhere")))
          (should (equal (ids (drover-index-sessions)) '("b" "conv-b/agent-1" "elsewhere")))
          (should-not (drover-attention-sessions))
          ;; Found, a subagent tells it is archived, but is restored with
          ;; the conversation, not alone.
          (let ((s2 (drover-find 'records "conv-s2")))
            (should (eql (drover-session-archived s2) 100.0))
            (should (drover-session-archived-p s2))
            (should-not (drover-archivable-p s2))
            (should-not (drover-unarchivable-p s2))
            (let ((err (should-error (drover-unarchive-session s2) :type 'user-error)))
              (should (equal (cadr err) "Record conv-s2 goes with Shown a, archived: restore that"))))
          (should-not drover-test-calls)
          ;; Shown under it among the archived sessions.
          (let ((drover-show-archived t)
                (drover-show-subagents 'all))
            (should (equal (mapcar #'drover-session-key (drover-archived-sessions))
                           '((shown . "a") (records . "conv-a/agent-1")
                             (records . "conv-s1") (records . "conv-s2"))))
            (should (equal (mapcar #'drover-session-archived (drover-archived-sessions))
                           '(100.0 100.0 100.0 100.0)))
            (should (equal (drover-test-tree-shape (drover-tree (drover-archived-sessions)))
                           '(("a" ("conv-a/agent-1") ("conv-s1" ("conv-s2")))))))
          ;; Restored, they come back with it.
          (drover-unarchive-session (drover-find 'shown "a"))
          (drover-changed-now)
          (should (equal (ids (drover-sessions)) all))
          (should (equal (ids (drover-index-sessions)) all))
          (should (equal (drover-index-parent '(records . "conv-s2")) '(records . "conv-s1"))))))))

(ert-deftest drover-test-drover-archive-older-subagents ()
  "The subagents of an older conversation drover archived go with it.
A backend listing what its engine records lists them with it among the
older sessions: they leave those, and show under it among the archived
ones, until it is restored."
  (let ((older (list (drover-test-claude-record "conv-old")
                     (drover-test-claude-record "conv-old/agent-1" :engine-id nil :parent "conv-old")
                     (drover-test-claude-record "conv-kept")
                     (drover-test-claude-record "conv-kept/agent-1" :engine-id nil :parent "conv-kept"))))
    (drover-test-with-shown nil
      (drover-test-register-claude-records #'ignore
                                           :history (lambda (callback) (funcall callback older)))
      (drover-archive-session (car older))
      (let ((drover-show-subagents 'all))
        (drover-set-show-history t)
        (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                               (drover-group-trees (drover-view-trees) 'status))
                       '((idle ("conv-kept" ("conv-kept/agent-1"))))))
        (drover-set-show-archived t)
        (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                               (drover-group-trees (drover-view-trees) 'status))
                       '((idle ("conv-kept" ("conv-kept/agent-1")))
                         (archived ("conv-old" ("conv-old/agent-1"))))))
        (drover-unarchive-session (drover-find 'records "conv-old"))
        (should (equal (mapcar (lambda (group) (cons (car group) (drover-test-tree-shape (cdr group))))
                               (drover-group-trees (drover-view-trees) 'status))
                       '((idle ("conv-old" ("conv-old/agent-1")) ("conv-kept" ("conv-kept/agent-1"))))))))))

;;;; Closing the buffers of archived sessions

(defvar drover-test-closed nil
  "Sessions whose buffers the test backends' :close closed, newest first.")

(defun drover-test-term-buffer (id)
  "Return the buffer of the session ID of a test backend, made if need be.
Its text stands for the input typed and not sent."
  (get-buffer-create (format "*session %s*" id)))

(defun drover-test--close (session)
  "Close the buffer of SESSION as backends do: its text to the kill ring."
  (when-let* ((buffer (get-buffer (format "*session %s*" (drover-session-id session)))))
    (push (drover-session-id session) drover-test-closed)
    (prog1 (drover-keep-unsent (list (with-current-buffer buffer (buffer-string))))
      (drover-kill-buffer buffer))))

(defmacro drover-test-with-terms (sessions &rest body)
  "Run BODY with the backends `term' and `view' serving SESSIONS, a variable.
A `term' session's buffer runs its agent, as a terminal does, and drover
archives it; a `view' session's buffer shows a session running
elsewhere, which its backend archives.  Their buffers, made by
`drover-test-term-buffer', are killed afterwards; nothing may ask."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (let ((drover-test-closed nil)
             (kill-ring nil)
             (interprogram-cut-function nil)
             (messages nil))
         (cl-flet ((buffer (session)
                     (get-buffer (format "*session %s*" (drover-session-id session)))))
           (drover-test-register-simple
            'term (lambda () (seq-filter (lambda (session) (eq (drover-session-backend session) 'term))
                                         ,sessions))
            :buffer #'buffer :close #'drover-test--close :runs-in-buffer t)
           (drover-test-register-simple
            'view (lambda () (seq-filter (lambda (session) (eq (drover-session-backend session) 'view))
                                         ,sessions))
            :buffer #'buffer :close #'drover-test--close
            :archive (drover-test--recorder :archive)))
         (unwind-protect
             (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked")))
                       ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Asked")))
                       ((symbol-function 'message)
                        (lambda (format &rest arguments)
                          (push (apply #'format-message format arguments) messages))))
               ,@body)
           (dolist (buffer (buffer-list))
             (when (string-prefix-p "*session " (buffer-name buffer))
               (with-current-buffer buffer
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer buffer))))))))))

(defun drover-test-term (id &rest properties)
  "Return a session ID of the backend `term', with PROPERTIES, and make its buffer."
  (drover-test-term-buffer id)
  (apply #'drover-session-create
         (append properties (list :backend 'term :id id :title (concat "Term " id)
                                  :engine 'claude-code :engine-id (concat "conv-" id)))))

(defun drover-test-view (id &rest properties)
  "Return a session ID of the backend `view', with PROPERTIES, and make its buffer."
  (drover-test-term-buffer id)
  (apply #'drover-session-create
         (append properties (list :backend 'view :id id :title (concat "View " id)))))

(ert-deftest drover-test-archive-closes-buffers ()
  "Archiving closes the session's buffer at once, without asking, keeping what was typed."
  (let ((sessions (list (drover-test-view "v" :status 'working)
                        (drover-test-term "t" :status 'idle))))
    (drover-test-with-terms sessions
      (with-current-buffer (drover-test-term-buffer "v")
        (insert "A prompt half typed")
        ;; Whatever the buffer would ask, it asks nothing.
        (add-hook 'kill-buffer-query-functions (lambda () (error "Asked")) nil t))
      ;; A session running elsewhere closes even mid-turn.
      (drover-test-at-point "v"
        (setq-local drover-session-at-point-function (lambda () (drover-find 'view "v")))
        (call-interactively #'drover-archive))
      (should (equal drover-test-calls '((:archive "v"))))
      (should-not (get-buffer "*session v*"))
      (should (equal kill-ring '("A prompt half typed")))
      (should (equal messages '("Archived View v; its unsent input is in the kill ring")))
      ;; A buffer running its idle agent closes at once too.
      (setq messages nil)
      (drover-archive (drover-find 'term "t"))
      (should-not (get-buffer "*session t*"))
      (should (equal kill-ring '("A prompt half typed")))
      (should (equal messages '("Archived Term t")))
      (should (equal drover-test-closed '("t" "v")))
      ;; Its buffer gone, it stays among the archived sessions.
      (setq sessions nil)
      (let ((drover-show-archived t))
        (should (equal (mapcar #'drover-session-key (drover-archived-sessions))
                       '((term . "conv-t"))))))))

(ert-deftest drover-test-archive-closes-after-turn ()
  "A buffer running its agent mid-turn closes once the turn ended, wherever the user is."
  (let ((sessions (list (drover-test-term "t" :status 'working))))
    (drover-test-with-terms sessions
      (with-current-buffer (drover-test-term-buffer "t")
        (insert "Next question"))
      (drover-archive (drover-find 'term "t"))
      (should (equal messages '("Archived Term t")))
      (should (get-buffer "*session t*"))
      (should (drover-session-archived-p (drover-find 'term "t")))
      (should (memq #'drover--close-pending drover-changed-hook))
      ;; Waiting for an answer, the turn goes on.
      (setq sessions (list (drover-test-term "t" :status 'waiting)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should (get-buffer "*session t*"))
      ;; Ended: it closes, what was typed kept, which alone is told.
      (setq messages nil
            sessions (list (drover-test-term "t" :status 'done)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should-not (get-buffer "*session t*"))
      (should (equal kill-ring '("Next question")))
      (should (equal messages '("The unsent input of Term t, archived, is in the kill ring")))
      (should-not drover--closing)
      (should-not (memq #'drover--close-pending drover-changed-hook))
      (should (drover-session-archived-p (car sessions))))))

(ert-deftest drover-test-archive-waiting-keeps-buffer ()
  "A turn waiting for the user keeps its buffer until it ends, and says so."
  (let ((sessions (list (drover-test-term "t" :status 'waiting
                                          :requests '((:kind question :summary "Go?"))))))
    (drover-test-with-terms sessions
      (drover-archive (drover-find 'term "t"))
      (should (equal messages '("Archived Term t; it waits for you")))
      (should (get-buffer "*session t*"))
      (setq sessions (list (drover-test-term "t" :status 'idle)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should-not (get-buffer "*session t*")))))

(ert-deftest drover-test-archive-close-cancelled ()
  "Restored before its turn ended, a session keeps its buffer; one killed is forgotten."
  (let ((sessions (list (drover-test-term "t" :status 'working)
                        (drover-test-term "u" :status 'working))))
    (drover-test-with-terms sessions
      (drover-archive (list (drover-find 'term "t") (drover-find 'term "u")))
      (should (equal (car messages)
                     "Archived 2 sessions"))
      (should (= (length drover--closing) 2))
      (drover-unarchive (drover-find 'term "t"))
      (should (equal (mapcar #'car drover--closing) '((term . "u"))))
      ;; The other's buffer killed meanwhile, nothing is left to close.
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*session u*"))
      (setq sessions (list (drover-test-term "t" :status 'done)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should (get-buffer "*session t*"))
      (should-not drover--closing)
      (should-not (memq #'drover--close-pending drover-changed-hook))
      (should-not drover-test-closed))))

(ert-deftest drover-test-archive-close-failure ()
  "A buffer failing to close leaves the session archived, and says why."
  (let ((sessions (list (drover-test-term "t" :status 'idle))))
    (drover-test-with-terms sessions
      (setf (plist-get (alist-get 'term drover--backends) :close)
            (lambda (_session) (error "Busy elsewhere")))
      (drover-archive (drover-find 'term "t"))
      (should (equal messages '("Archived Term t; its buffer stays open: Busy elsewhere")))
      (should (drover-session-archived-p (drover-find 'term "t"))))))

(ert-deftest drover-test-archive-without-close ()
  "A backend closing no buffer keeps it."
  (let ((sessions (list (drover-test-term "t" :status 'idle))))
    (drover-test-with-terms sessions
      (setf (plist-get (alist-get 'term drover--backends) :close) nil)
      (drover-archive (drover-find 'term "t"))
      (should (equal messages '("Archived Term t")))
      (should (get-buffer "*session t*")))))

(defun drover-test-closing-file ()
  "Return the entries of the file of the buffers to close, as their keys and buffers."
  (mapcar (lambda (entry) (list (plist-get entry :key) (plist-get entry :engine) (plist-get entry :buffer)))
          (drover-read-data (drover-closing-file) drover--closing-version :buffers)))

(defun drover-test-restart ()
  "Start drover anew, as after Emacs restarted, its buffers kept: nothing in memory."
  (remove-hook 'drover-changed-hook #'drover--close-pending)
  (setq drover--closing nil
        drover--closing-saved nil
        drover--closing-deferred nil
        drover--closing-restored nil
        drover--closing-written 'unread
        drover--started nil
        drover--running nil)
  (drover-start))

(ert-deftest drover-test-archive-close-remembered ()
  "A buffer to close once its turn ends is remembered in a file, and forgotten once closed."
  (let ((sessions (list (drover-test-term "t" :status 'working))))
    (drover-test-with-terms sessions
      (drover-archive (drover-find 'term "t"))
      (should (equal (drover-test-closing-file)
                     '(((term . "t") (claude-code . "conv-t") "*session t*"))))
      (setq sessions (list (drover-test-term "t" :status 'done)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should-not (get-buffer "*session t*"))
      (should-not (drover-test-closing-file))
      ;; Restored before its turn ended, it is forgotten too.
      (setq sessions (list (drover-test-term "u" :status 'working)))
      (drover-archive (drover-find 'term "u"))
      (should (= (length (drover-test-closing-file)) 1))
      (drover-unarchive (drover-find 'term "u"))
      (should-not (drover-test-closing-file)))))

(ert-deftest drover-test-archive-close-after-restart ()
  "Started anew, drover closes the buffers whose turns ended, and waits for the others.
A buffer gone, restored meanwhile, or now showing another conversation
or under another name is forgotten, and kept open."
  (let ((sessions (list (drover-test-term "a" :status 'working)
                        (drover-test-term "b" :status 'working)
                        (drover-test-term "c" :status 'working)
                        (drover-test-term "d" :status 'working)
                        (drover-test-term "e" :status 'working))))
    (drover-test-with-terms sessions
      (drover-start)
      (with-current-buffer (drover-test-term-buffer "b")
        (insert "Typed meanwhile"))
      (drover-archive (mapcar (lambda (id) (drover-find 'term id)) '("a" "b" "c" "d" "e")))
      (should (drover-test-wait-actions))
      (should (= (length (drover-test-closing-file)) 5))
      ;; While drover was away: b's turn ended, c's buffer went, d's
      ;; buffer shows another conversation, e was restored.
      (let ((kill-buffer-query-functions nil))
        (kill-buffer "*session c*"))
      (drover--archive-forget (drover-find 'term "e"))
      (setq sessions (list (drover-test-term "a" :status 'working)
                           (drover-test-term "b" :status 'done)
                           (drover-test-term "d" :status 'working :engine-id "another")
                           (drover-test-term "e" :status 'working)))
      (setq messages nil)
      (drover-test-restart)
      (should-not (get-buffer "*session b*"))
      (should (equal kill-ring '("Typed meanwhile")))
      (should (equal messages '("The unsent input of Term b, archived, is in the kill ring")))
      (should (get-buffer "*session d*"))
      (should (get-buffer "*session e*"))
      (should (equal (mapcar #'car drover--closing) '((term . "a"))))
      (should (equal (mapcar #'car (drover-test-closing-file)) '((term . "a"))))
      ;; a's turn ends: it closes, and is forgotten.
      (setq sessions (list (drover-test-term "a" :status 'idle)))
      (drover-backend-changed 'term)
      (drover-changed-now)
      (should-not (get-buffer "*session a*"))
      (should-not drover--closing)
      (should-not (drover-test-closing-file)))))

(ert-deftest drover-test-archive-close-waits-for-backend ()
  "A buffer to close whose backend registers after drover started waits for it."
  (let ((sessions (list (drover-test-term "t" :status 'idle))))
    (drover-test-with-terms sessions
      (drover-write-data (drover-closing-file) "Buffers" drover--closing-version :buffers
                         (list (list :key '(term . "t") :engine '(claude-code . "conv-t")
                                     :buffer "*session t*" :time (float-time))
                               ;; Too old: forgotten.
                               (list :key '(term . "old") :engine nil :buffer "*session old*"
                                     :time (- (float-time) (* 86400 (1+ drover--closing-days))))))
      (drover--archive-own (drover-find 'term "t"))
      (let ((term (assq 'term drover--backends)))
        (drover-unregister-backend 'term)
        (drover-test-restart)
        (should (get-buffer "*session t*"))
        (should (equal (mapcar (lambda (entry) (plist-get entry :key)) drover--closing-deferred)
                       '((term . "t") (term . "old"))))
        (apply #'drover-register-backend 'term (cdr term)))
      (should-not (get-buffer "*session t*"))
      (should-not drover--closing-deferred)
      (should-not (drover-test-closing-file)))))

(ert-deftest drover-test-archive-close-file-unread ()
  "A file of buffers to close that does not read is told of, and taken as empty."
  (let ((sessions (list (drover-test-term "t" :status 'working))))
    (drover-test-with-terms sessions
      (let ((warnings nil))
        (drover-test-write (drover-closing-file) "(:version 1 :buffers ((:key oops)))")
        (cl-letf (((symbol-function 'drover-warn)
                   (lambda (format &rest args) (push (apply #'format-message format args) warnings))))
          (drover-test-restart))
        (should (string-prefix-p "Ignoring the buffers to close" (car warnings)))
        (should (get-buffer "*session t*"))
        (drover-archive (drover-find 'term "t"))
        (should (= (length (drover-test-closing-file)) 1))))))

(ert-deftest drover-test-archive-from-its-buffer ()
  "Archived from its own buffer, shown, a session closes it; its window shows another."
  (let ((sessions (list (drover-test-view "v"))))
    (drover-test-with-terms sessions
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create "*drover test before*"))
        (switch-to-buffer (drover-test-term-buffer "v"))
        (setq-local drover-session-at-point-function (lambda () (drover-find 'view "v")))
        (call-interactively #'drover-archive)
        (should-not (get-buffer "*session v*"))
        (should (equal (buffer-name (window-buffer)) "*drover test before*"))
        (should (equal messages '("Archived View v")))
        (kill-buffer "*drover test before*")))))

(ert-deftest drover-test-kill-buffer-windows ()
  "A buffer closing leaves dedicated and side windows as Emacs does, and lists alone."
  (let ((sessions (list (drover-test-view "a") (drover-test-view "b") (drover-test-view "c"))))
    (drover-test-with-terms sessions
      (let ((list (get-buffer-create "*drover test list*")))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (with-current-buffer list
                (setq-local drover-view-p t))
              (let* ((list-window (selected-window))
                     (main (split-window list-window nil 'right))
                     (dedicated (split-window main nil 'below))
                     (side (display-buffer-in-side-window (drover-test-term-buffer "c")
                                                          '((side . bottom)))))
                (set-window-buffer list-window list)
                ;; The list showed in the main window before the session.
                (set-window-buffer main list)
                (set-window-buffer main (drover-test-term-buffer "a"))
                (set-window-buffer dedicated (drover-test-term-buffer "b"))
                (set-window-dedicated-p dedicated t)
                (drover-archive (mapcar (lambda (id) (drover-find 'view id)) '("a" "b" "c")))
                (should (equal drover-test-closed '("c" "b" "a")))
                ;; The list's window still shows it, selected.
                (should (eq (window-buffer list-window) list))
                (should (eq (selected-window) list-window))
                ;; The main window shows something else than the list, shown
                ;; already.
                (should (window-live-p main))
                (should-not (eq (window-buffer main) list))
                (should-not (string-prefix-p "*session " (buffer-name (window-buffer main))))
                ;; The dedicated window and the side window, which showed
                ;; nothing else, are gone.
                (should-not (window-live-p dedicated))
                (should-not (window-live-p side))))
          (kill-buffer list))))))

(ert-deftest drover-test-write-data ()
  "State files are written whole, through a file renamed over them."
  (drover-test-with-state
    (let ((file (expand-file-name "sub/data.eld" drover-state-directory)))
      (drover-write-data file "Things" 3 :things '((a . "é") (b 1 2)))
      (should (equal (drover-read-data file 3 :things) '((a . "é") (b 1 2))))
      (should (equal (directory-files (file-name-directory file) nil "\\`[^.]\\|\\`\\.[^.]")
                     '("data.eld")))
      (should-error (drover-read-data file 4 :things))
      (should-not (drover-read-data (expand-file-name "missing.eld" drover-state-directory) 3 :things)))))

(ert-deftest drover-test-session-engine-key ()
  (should-not (drover-session-engine-key (drover-test-session "s")))
  (should-not (drover-session-engine-key (drover-test-session "s" :engine 'opencode)))
  (should-not (drover-session-engine-key (drover-test-session "s" :engine-id "ses_1")))
  (should (equal (drover-session-engine-key (drover-test-session "s" :engine 'opencode :engine-id "ses_1"))
                 '(opencode . "ses_1"))))

(ert-deftest drover-test-backend-engines ()
  (drover-test-with-backend nil
    (should (equal (drover-backend-engines 'fake) '(fake)))
    (let ((drover-test-backend-engines '(opencode codex)))
      (should (equal (drover-backend-engines (drover-test-session "s")) '(opencode codex))))
    (drover-test-register-simple 'listed nil :engines '(claude-code))
    (drover-test-register-simple 'broken nil :engines (lambda () (error "Boom")))
    (drover-test-register-simple 'none nil)
    (should (equal (drover-backend-engines 'listed) '(claude-code)))
    (should-not (drover-backend-engines 'broken))
    (should-not (drover-backend-engines 'none))))

(defun drover-test-other (id &rest properties)
  "Return a session ID of the backend `other', with PROPERTIES."
  (apply #'drover-session-create :backend 'other :id id properties))

(ert-deftest drover-test-duplicate-sessions-merge ()
  "Two backends showing one conversation list it once, the first registered's."
  (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1")
                                  (drover-test-session "b" :engine 'opencode :engine-id "ses_2")
                                  (drover-test-session "c" :engine 'opencode)
                                  (drover-test-session "d"))
    (drover-test-register-simple
     'other (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                  (drover-test-other "y" :engine 'codex :engine-id "ses_2")
                  (drover-test-other "z" :engine 'opencode)
                  (drover-test-other "w")))
    (cl-flet ((keys () (mapcar #'drover-session-key (drover-sessions))))
      ;; An unknown engine or identifier is no evidence.
      (should (equal (keys) '((other . "y") (other . "z") (other . "w")
                              (fake . "a") (fake . "b") (fake . "c") (fake . "d"))))
      ;; Each backend still finds its own record.
      (should (drover-find 'other "x"))
      (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_1")) 'fake))
      (should (eq (drover-session-backend (drover-find-engine-session 'codex "ses_2")) 'other))
      (should-not (drover-find-engine-session 'opencode "ses_3"))
      (should-not (drover-find-engine-session 'opencode nil))
      ;; Registering again keeps the order of registration.
      (drover-test-register)
      (should (equal (keys) '((other . "y") (other . "z") (other . "w")
                              (fake . "a") (fake . "b") (fake . "c") (fake . "d")))))))

(ert-deftest drover-test-duplicate-sessions-prefer-a-buffer ()
  "The backend whose buffer for the conversation was selected last keeps it."
  (let ((buffer (generate-new-buffer " other"))
        (own (generate-new-buffer " fake")))
    (unwind-protect
        (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1"))
          (drover-test-register-simple
           'other (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                        (drover-test-other "x2" :engine 'opencode :engine-id "ses_1"))
           :buffer (lambda (session) (and (equal (drover-session-id session) "x2") buffer)))
          ;; Records of one backend stay distinct sessions.
          (should (equal (mapcar #'drover-session-key (drover-sessions))
                         '((other . "x") (other . "x2"))))
          (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_1")) 'other))
          ;; Both with a buffer: the one selected last.
          (setf (alist-get 'fake drover--backends)
                (append (list :buffer (lambda (_) own)) (alist-get 'fake drover--backends)))
          (bury-buffer buffer)
          (switch-to-buffer own)
          (should (equal (mapcar #'drover-session-key (drover-sessions)) '((fake . "a"))))
          (switch-to-buffer buffer)
          (should (equal (mapcar #'drover-session-key (drover-sessions))
                         '((other . "x") (other . "x2"))))
          ;; No buffer any more: the first registered.
          (kill-buffer buffer)
          (kill-buffer own)
          (should (equal (mapcar #'drover-session-key (drover-sessions)) '((fake . "a")))))
      (dolist (b (list buffer own))
        (when (buffer-live-p b)
          (kill-buffer b))))))

(ert-deftest drover-test-tree-parent-shown-by-another-backend ()
  "A subagent stays under its parent when another backend shows the parent."
  (let ((buffer (generate-new-buffer " other")))
    (unwind-protect
        (drover-test-with-backend (list (drover-test-session "ses_1" :engine 'opencode :engine-id "ses_1")
                                        (drover-test-session "ses_2" :parent "ses_1"
                                                             :engine 'opencode :engine-id "ses_2"))
          (drover-test-register-simple
           'other (list (drover-test-other "tok" :engine 'opencode :engine-id "ses_1"))
           :buffer (lambda (_) buffer))
          (switch-to-buffer buffer)
          (let ((nodes (drover-tree (drover-sessions))))
            (should (= (length nodes) 1))
            (should (equal (drover-session-key (drover-node-session (car nodes))) '(other . "tok")))
            (should (equal (mapcar (lambda (node) (drover-session-key (drover-node-session node)))
                                   (drover-node-children (car nodes)))
                           '((fake . "ses_2"))))))
      (kill-buffer buffer))))

(ert-deftest drover-test-duplicate-sessions-in-history ()
  "An older record of a conversation a backend follows live is not listed."
  (drover-test-with-backend (list (drover-test-session "a" :engine 'opencode :engine-id "ses_1"))
    (let ((drover-test-history-calls nil)
          (answer nil))
      (drover-test-register-simple
       'other nil :history (lambda (callback) (setq answer callback)))
      (drover-test-register-history)
      (drover-set-show-history t)
      (funcall answer (list (drover-test-other "x" :engine 'opencode :engine-id "ses_1")
                            (drover-test-other "y" :engine 'opencode :engine-id "ses_2")))
      (funcall (car drover-test-history-calls)
               (list (drover-test-session "b" :engine 'opencode :engine-id "ses_2")
                     (drover-test-session "c" :engine 'opencode :engine-id "ses_3")))
      ;; The live record wins over an older one of another backend; older
      ;; records of one conversation merge, the first registered winning.
      (should (equal (mapcar #'drover-session-key (drover-known-sessions))
                     '((fake . "a") (fake . "b") (fake . "c"))))
      (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_2")) 'fake)))))

(ert-deftest drover-test-fake-prompt-operations ()
  "The fake backend's prompt operations record their calls, as tests expect."
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((session (drover-find 'fake "s"))
          (created nil)
          (failed nil)
          (applied 0))
      (should (equal (drover-backend-engines 'fake) '(fake)))
      (should (eq (drover-backend-call session :check "Hi" '(:agent "plan")) 'fake-token))
      (drover-backend-call session :apply-options '(:model nil) (lambda () (cl-incf applied)))
      (drover-backend-call session :send "Hi" 'fake-token)
      (should (= applied 1))
      (funcall (drover-backend-property 'fake :create) "/work/" 'fake '(:title "New")
               (lambda (new) (setq created new)) (lambda (message) (setq failed message)))
      (should (equal (drover-session-id created) "new-1"))
      (should (eq (drover-session-engine created) 'fake))
      (should (equal (drover-session-title created) "New"))
      ;; Served from then on.
      (should (drover-find 'fake "new-1"))
      (let ((drover-test-create-error "No service"))
        (funcall (drover-backend-property 'fake :create) "/work/" 'fake nil #'ignore
                 (lambda (message) (setq failed message))))
      (should (equal failed "No service"))
      (should (equal (reverse drover-test-calls)
                     '((:check "s" "Hi" (:agent "plan")) (:apply-options "s" (:model nil))
                       (:send "s" "Hi" fake-token) (:create "/work/" fake (:title "New"))
                       (:create "/work/" fake nil))))
      (setq drover-test-options '(("s" :model nil :variant high))
            drover-test-settable '(("s" :model)))
      (should (equal (drover-backend-call session :options) '(:model nil :variant high)))
      (should (equal (drover-backend-call session :settable) '(:model)))
      (should (equal (drover-backend-call created :settable) '(:agent :model :variant))))))

(ert-deftest drover-test-buffer-session ()
  (drover-test-with-backend (list (drover-test-session "s"))
    (let ((buffer (generate-new-buffer " session")))
      (should-not (drover-buffer-session buffer))
      (with-current-buffer buffer
        (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s"))))
      (should (equal (drover-session-id (drover-buffer-session buffer)) "s"))
      (kill-buffer buffer)
      (should-not (drover-buffer-session buffer))
      (should-not (drover-buffer-session nil)))))

;;;; Warnings and steps

(defmacro drover-test-with-warnings (&rest body)
  "Run BODY collecting the messages of `display-warning' in `warnings'.
They are newest first, as (TYPE MESSAGE LEVEL)."
  (declare (indent 0) (debug t))
  `(let ((warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level _buffer)
                  (push (list type message level) warnings))))
       ,@body)))

(ert-deftest drover-test-warn ()
  (drover-test-with-warnings
    (drover-warn "%s refused %s" "Session" "plan")
    (should (equal warnings '((drover "Session refused plan" :warning))))))

(ert-deftest drover-test-run-steps-in-order ()
  (drover-test-with-warnings
    (let ((ran nil)
          (done 0)
          (pending nil))
      (drover-run-steps (list (cons "agent" (lambda (next) (push 'agent ran) (funcall next)))
                              (cons "model" (lambda (next) (push 'model ran) (setq pending next)))
                              (cons "effort" (lambda (next) (push 'effort ran) (funcall next))))
                        (lambda () (cl-incf done)))
      ;; A step waits for its request to settle.
      (should (equal ran '(model agent)))
      (should (= done 0))
      (funcall pending)
      (should (equal ran '(effort model agent)))
      (should (= done 1))
      ;; A second call does nothing.
      (funcall pending)
      (should (= done 1))
      (should-not warnings)
      (drover-run-steps nil (lambda () (cl-incf done)))
      (should (= done 2)))))

(ert-deftest drover-test-run-steps-signal ()
  "A step that signals is reported, and the next one runs."
  (drover-test-with-warnings
    (let ((ran nil)
          (done 0))
      (drover-run-steps (list (cons "model" (lambda (_next) (error "No such model")))
                              (cons "effort" (lambda (next) (push 'effort ran) (funcall next))))
                        (lambda () (cl-incf done)))
      (should (equal ran '(effort)))
      (should (= done 1))
      (should (equal warnings
                     '((drover "Could not set model: No such model"
                               :warning))))
      ;; Signalling once settled warns nothing more.
      (setq warnings nil)
      (drover-run-steps (list (cons "model" (lambda (next) (funcall next) (error "Late"))))
                        (lambda () (cl-incf done)))
      (should (= done 2))
      (should-not warnings))))

(ert-deftest drover-test-run-steps-timeout ()
  "A step not settled in time is reported, and its late reply ignored."
  (drover-test-with-warnings
    (let ((drover-step-timeout 0.01)
          (done 0)
          (late nil))
      (drover-run-steps (list (cons "agent" (lambda (next) (setq late next))))
                        (lambda () (cl-incf done)))
      (should (= done 0))
      (with-timeout (5 (error "The step never timed out"))
        (while (zerop done)
          (accept-process-output nil 0.01)))
      (should (= done 1))
      (should (equal warnings
                     '((drover "Timed out setting agent" :warning))))
      (funcall late)
      (should (= done 1)))))

;;;; Git branches

(ert-deftest drover-test-git-directory-msys ()
  "A linked worktree's Git directory, as MSYS2's Git names it on Windows, is found."
  (let ((system-type 'windows-nt))
    (should (equal (drover--native-file-name "/c/Users/me/repo/.git/worktrees/wt") "c:/Users/me/repo/.git/worktrees/wt"))
    (should (equal (drover--native-file-name "/d") "d:/"))
    (should (equal (drover--native-file-name "/cd/x") "/cd/x")))
  (let ((system-type 'darwin))
    (should (equal (drover--native-file-name "/c/Users/me") "/c/Users/me")))
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "drover-git-directory" t))))
         (admin (expand-file-name "repo/.git/worktrees/wt/" root))
         (worktree (expand-file-name "wt/" root)))
    (unwind-protect
        (progn
          (make-directory admin t)
          (make-directory worktree t)
          (with-temp-file (expand-file-name ".git" worktree)
            (insert "gitdir: "
                    (if (and (eq system-type 'windows-nt) (string-match "\\`\\([a-zA-Z]\\):/" admin))
                        (concat "/" (downcase (match-string 1 admin)) "/" (substring admin (match-end 0)))
                      admin)
                    "\n"))
          (should (drover-test-same-directory-p (drover--git-directory worktree) admin)))
      (delete-directory root t))))

(defun drover-test-same-directory-p (a b)
  "Return non-nil when A and B name the same directory, case aside."
  (equal (downcase (directory-file-name (expand-file-name a)))
         (downcase (directory-file-name (expand-file-name b)))))

(ert-deftest drover-test-git-branch ()
  (let* ((root (file-name-as-directory (make-temp-file "drover-git" t)))
         (main (expand-file-name "main/" root))
         (linked (expand-file-name "linked/" root))
         (plain (expand-file-name "plain/" root))
         (drover--git-heads (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-flet ((write (file content)
                    (make-directory (file-name-directory file) t)
                    (with-temp-file file (insert content))))
          (write (expand-file-name ".git/HEAD" main) "ref: refs/heads/feature/x\n")
          (make-directory (expand-file-name "src" main) t)
          (write (expand-file-name ".git/worktrees/linked/HEAD" main) "ref: refs/heads/side\n")
          (write (expand-file-name ".git" linked)
                 (concat "gitdir: " (expand-file-name ".git/worktrees/linked" main) "\n"))
          (make-directory plain t)
          ;; Read without Git, from a subdirectory too.
          (should (equal (drover-git-branch (expand-file-name "src" main)) "feature/x"))
          (should (equal (drover-git-branch linked) "side"))
          (should-not (drover-git-branch plain))
          (should-not (drover-git-branch (expand-file-name "missing/" root)))
          (should-not (drover-git-branch nil))
          ;; Kept for `drover-branch-ttl' seconds.
          (write (expand-file-name ".git/worktrees/linked/HEAD" main) "ref: refs/heads/renamed\n")
          (should (equal (drover-git-branch (directory-file-name linked)) "side"))
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-branch linked) "renamed")))
          ;; Detached: a short commit.
          (write (expand-file-name ".git/HEAD" main) "0123456789abcdef\n")
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-branch main) "0123456"))))
      (delete-directory root t))))

(ert-deftest drover-test-git-branch-looks-again ()
  "A directory's HEAD is looked at once per refresh, and read only once it changed."
  (let* ((root (file-name-as-directory (make-temp-file "drover-git" t)))
         (repo (expand-file-name "repo/" root))
         (head (expand-file-name ".git/HEAD" repo))
         (drover--git-heads (make-hash-table :test #'equal))
         (drover--git-look 0)
         (drover-branch-ttl 3600)
         (reads 0)
         (looks 0)
         (read (symbol-function 'drover--read-git-head))
         (status (symbol-function 'drover--git-head-status)))
    (unwind-protect
        (cl-letf (((symbol-function 'drover--read-git-head)
                   (lambda (file) (cl-incf reads) (funcall read file)))
                  ((symbol-function 'drover--git-head-status)
                   (lambda (file) (cl-incf looks) (funcall status file))))
          (make-directory (file-name-directory head) t)
          (with-temp-file head (insert "ref: refs/heads/one\n"))
          (should (equal (drover-git-branch repo) "one"))
          (should (equal (list reads looks) '(1 1)))
          ;; Trusted until the next refresh: asked again for every
          ;; session there, by any of its names, it is neither looked at
          ;; nor read.
          (dotimes (_ 3)
            (should (equal (drover-git-branch repo) "one")))
          (should (equal (drover-git-branch (directory-file-name repo)) "one"))
          (should (equal (drover-git-branch (expand-file-name "repo/../repo" root)) "one"))
          (should (equal (list reads looks) '(1 1)))
          ;; A refresh looks at it once, and reads it only once changed.
          (drover-git-look-again)
          (should (equal (drover-git-branch repo) "one"))
          (should (equal (drover-git-branch repo) "one"))
          (should (equal (list reads looks) '(1 2)))
          (with-temp-file head (insert "ref: refs/heads/second\n"))
          (should (equal (drover-git-branch repo) "one"))
          (drover-git-look-again)
          (should (equal (drover-git-branch repo) "second"))
          (should (equal (drover-git-branch (directory-file-name repo)) "second"))
          (should (equal (list reads looks) '(2 4)))
          ;; Past `drover-branch-ttl', it is looked at without a refresh.
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-branch repo) "second")))
          (should (equal (list reads looks) '(2 5)))
          ;; No longer a worktree: nil, and found again once it is one.
          (delete-directory (expand-file-name ".git" repo) t)
          (drover-git-look-again)
          (should-not (drover-git-branch repo))
          (make-directory (file-name-directory head) t)
          (with-temp-file head (insert "ref: refs/heads/third\n"))
          (should-not (drover-git-branch repo))
          (drover-git-look-again)
          (should (equal (drover-git-branch repo) "third")))
      (delete-directory root t))))

(ert-deftest drover-test-index-refresh-reported-backend ()
  "A refresh asks a backend reporting its changes only for the sessions whose branch changed."
  (drover-test-with-backend nil
    (let* ((branches (list (cons "/w/a/" "main") (cons "/w/b/" "dev")))
           (sessions (mapcar (pcase-lambda (`(,id ,directory ,branch))
                               (drover-session-create :backend 'reporting :id id :title id
                                                      :directory directory :branch branch))
                             '(("1" "/w/a/" "main") ("2" "/w/b/" "dev") ("3" "/w/a/" "main")
                               ("4" nil nil))))
           (listed 0)
           (asked nil)
           (ran nil)
           (looks drover--git-look))
      (cl-letf (((symbol-function 'drover-git-branch)
                 (lambda (directory) (cdr (assoc directory branches)))))
        (drover-test-register-simple
         'reporting (lambda () (cl-incf listed) (mapcar #'copy-sequence sessions))
         :session (lambda (id)
                    (push id asked)
                    (copy-sequence (seq-find (lambda (session) (equal (drover-session-id session) id))
                                             sessions)))
         :reports-changes t)
        (drover-index)
        ;; Registering reported the backend changed.
        (drover-changed-now)
        (setq listed 0)
        (add-hook 'drover-changed-hook (lambda () (push drover-changed-sessions ran)))
        ;; Nothing changed: nothing listed, asked, nor drawn.
        (drover-index-refresh)
        (should (= drover--git-look (1+ looks)))
        (should (zerop listed))
        (should-not asked)
        (should-not ran)
        ;; A branch changed: the sessions of its directory alone.
        (setcdr (assoc "/w/a/" branches) "feature")
        (dolist (session sessions)
          (when (equal (drover-session-directory session) "/w/a/")
            (setf (drover-session-branch session) "feature")))
        (drover-index-refresh)
        (should (zerop listed))
        (should (equal (sort asked #'string<) '("1" "3")))
        (should (equal (sort (car ran) (lambda (a b) (string< (cdr a) (cdr b))))
                       '((reporting . "1") (reporting . "3"))))
        (should (equal (drover-session-branch (drover-index-session '(reporting . "3"))) "feature"))
        ;; Anything else changing unreported waits for a full refresh,
        ;; which lists every session, as `g' does.
        (setf (drover-session-title (nth 1 sessions)) "Renamed")
        (setq asked nil ran nil)
        (drover-index-refresh)
        (should-not ran)
        (drover-index-refresh t)
        (should (= listed 1))
        (should-not asked)
        (should (equal (car ran) '((reporting . "2"))))))))

(ert-deftest drover-test-index-refresh-other-host ()
  "A refresh never reads the branch of a session of another host, whose directory is there."
  (drover-test-with-backend nil
    (let* ((sessions (list (drover-session-create :backend 'remote:core :id '(fake . "1") :title "1"
                                                  :directory "/w/a/" :branch "main" :host "core")
                           (drover-session-create :backend 'remote:core :id '(fake . "2") :title "2"
                                                  :directory "/w/b/" :host "core")))
           (read nil)
           (listed 0)
           (asked nil)
           (ran nil))
      (cl-letf (((symbol-function 'drover-git-branch)
                 (lambda (directory) (push directory read) "elsewhere")))
        (drover-test-register-simple
         'remote:core (lambda () (cl-incf listed) (mapcar #'copy-sequence sessions))
         :session (lambda (id)
                    (push id asked)
                    (copy-sequence (seq-find (lambda (session) (equal (drover-session-id session) id))
                                             sessions)))
         :remote "core"
         :reports-changes t)
        (drover-index)
        (drover-changed-now)
        (setq listed 0)
        (add-hook 'drover-changed-hook (lambda () (push drover-changed-sessions ran)))
        (drover-index-refresh)
        (should-not read)
        (should (zerop listed))
        (should-not asked)
        (should-not ran)
        ;; A full refresh lists them again, still reading no branch.
        (drover-index-refresh t)
        (should (= listed 1))
        (should-not read)
        (should-not ran)))))

(ert-deftest drover-test-git-default-branch ()
  "The default branch is the one a remote's HEAD names, read without Git."
  (let* ((root (file-name-as-directory (make-temp-file "drover-git" t)))
         (main (expand-file-name "main/" root))
         (linked (expand-file-name "linked/" root))
         (fork (expand-file-name "fork/" root))
         (local (expand-file-name "local/" root))
         (drover--default-branches (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-flet ((write (file content)
                    (make-directory (file-name-directory file) t)
                    (with-temp-file file (insert content))))
          (write (expand-file-name ".git/HEAD" main) "ref: refs/heads/main\n")
          (write (expand-file-name ".git/refs/remotes/origin/HEAD" main)
                 "ref: refs/remotes/origin/develop\n")
          ;; A linked worktree finds it in the common Git directory.
          (write (expand-file-name ".git/worktrees/linked/HEAD" main) "ref: refs/heads/side\n")
          (write (expand-file-name ".git/worktrees/linked/commondir" main) "../..\n")
          (write (expand-file-name ".git" linked)
                 (concat "gitdir: " (expand-file-name ".git/worktrees/linked" main) "\n"))
          ;; Without origin, another remote.
          (write (expand-file-name ".git/HEAD" fork) "ref: refs/heads/trunk\n")
          (write (expand-file-name ".git/refs/remotes/upstream/HEAD" fork)
                 "ref: refs/remotes/upstream/trunk\n")
          ;; A repository whose remotes record no HEAD.
          (write (expand-file-name ".git/HEAD" local) "ref: refs/heads/master\n")
          (make-directory (expand-file-name ".git/refs/remotes/origin" local) t)
          (should (equal (drover-git-default-branch main) "develop"))
          (should (equal (drover-git-default-branch linked) "develop"))
          (should (equal (drover-git-default-branch fork) "trunk"))
          (should-not (drover-git-default-branch local))
          (should-not (drover-git-default-branch nil))
          (should-not (drover-git-default-branch (expand-file-name "missing/" root)))
          (cl-flet ((default-p (directory branch)
                      (drover-session-default-branch-p
                       (drover-session-create :directory directory :branch branch))))
            ;; Known, the repository's default alone is.
            (should (default-p main "develop"))
            (should-not (default-p main "main"))
            (should (default-p fork "trunk"))
            ;; Unknown, main and master are.
            (should (default-p local "master"))
            (should (default-p local "main"))
            (should-not (default-p local "feature"))
            (should-not (default-p nil nil)))
          ;; Kept for `drover-branch-ttl' seconds.
          (write (expand-file-name ".git/refs/remotes/origin/HEAD" main)
                 "ref: refs/remotes/origin/main\n")
          (should (equal (drover-git-default-branch main) "develop"))
          (let ((drover-branch-ttl 0))
            (should (equal (drover-git-default-branch main) "main"))))
      (delete-directory root t))))

(ert-deftest drover-test-default-branch-of-another-host ()
  "Another host's session has the default branch its owner read; this host reads nothing."
  (let* ((root (file-name-as-directory (make-temp-file "drover-git" t)))
         (drover--default-branches (make-hash-table :test #'equal))
         (looked nil))
    (unwind-protect
        (progn
          ;; The same directory here is another repository, whose default
          ;; branch is not the owner's.
          (make-directory (expand-file-name ".git/refs/remotes/origin" root) t)
          (with-temp-file (expand-file-name ".git/refs/remotes/origin/HEAD" root)
            (insert "ref: refs/remotes/origin/develop\n"))
          (advice-add 'drover--read-git-default-branch :before (lambda (directory) (push directory looked))
                      '((name . drover-test-looked)))
          (cl-flet ((default-p (branch &optional default)
                      (drover-session-default-branch-p
                       (drover-session-create :directory root :branch branch :host "core"
                                              :default-branch default))))
            (should (default-p "develop" "develop"))
            (should-not (default-p "main" "develop"))
            ;; The owner knows none: main and master stand for it.
            (should (default-p "main"))
            (should-not (default-p "develop"))
            (should-not looked)
            ;; This host's session in the same directory reads it.
            (should (drover-session-default-branch-p (drover-session-create :directory root :branch "develop")))
            (should (equal looked (list (file-name-as-directory (expand-file-name root)))))))
      (advice-remove 'drover--read-git-default-branch 'drover-test-looked)
      (delete-directory root t))))

(ert-deftest drover-test-local-directory ()
  "A session's directory is this host's to use only when the session is."
  (should (equal (drover-session-local-directory (drover-session-create :directory "/here/")) "/here/"))
  (should-not (drover-session-local-directory (drover-session-create :directory "/there/" :host "core")))
  (should (equal (drover-relative-file-name "/work/a/b.el" "/work/a/") "b.el"))
  (should (equal (drover-relative-file-name "/elsewhere/b.el" "/work/a/") "/elsewhere/b.el"))
  (should (equal (drover-relative-file-name "/work/a/b.el" nil) "/work/a/b.el"))
  (should (equal (drover-relative-file-name "b.el" "/work/a/") "b.el")))

;;;; Several sessions at once

(ert-deftest drover-test-selected-sessions ()
  "The selection is what the buffer selects, else the session at point."
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b"))
    (drover-test-at-point "a"
      (should (equal (drover-test-ids (drover-selected-sessions)) '("a")))
      (setq-local drover-selected-sessions-function (lambda () drover-test-sessions))
      (should (equal (drover-test-ids (drover-selected-sessions)) '("a" "b")))
      (setq-local drover-selected-sessions-function #'ignore)
      (should (equal (drover-test-ids (drover-selected-sessions)) '("a"))))
    (with-temp-buffer
      (should-not (drover-selected-sessions t))
      (should-error (drover-selected-sessions) :type 'user-error))))

(ert-deftest drover-test-commands-on-several ()
  "Commands act on several sessions with one confirmation and one report."
  (drover-test-with-backend (list (drover-test-session "a" :status 'done)
                                  (drover-test-session "b" :status 'working)
                                  (drover-test-session "c" :status 'idle))
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "x"
                                                                               :title "Bare x"
                                                                               :status 'done))))
    (drover-test-at-point "a"
      (setq-local drover-selected-sessions-function
                  (lambda () (append drover-test-sessions (drover--backend-sessions
                                                            (alist-get 'bare drover--backends)))))
      (let ((asked nil)
            (messages nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (push prompt asked) t))
                  ((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt asked) t))
                  ((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (push (apply #'format-message format-string arguments) messages))))
          (dolist (command '(drover-mark-seen drover-interrupt drover-archive drover-delete))
            (call-interactively command)))
        ;; The bare backend acts on nothing: its session is left out.
        (should (equal (reverse drover-test-calls)
                       '((:mark-seen "a") (:interrupt "b")
                         (:archive "a") (:archive "b") (:archive "c")
                         (:delete "a") (:delete "b") (:delete "c"))))
        ;; Archiving asks nothing.
        (should (equal (reverse asked)
                       '("Interrupt Session b? "
                         "Delete 3 sessions for good: Session a, Session b, Session c? ")))
        (should (equal (reverse messages)
                       '("Marked Session a seen" "Interrupted Session b"
                         "Archived 3 sessions" "Deleted 3 sessions")))))))

(ert-deftest drover-test-several-titles-cut ()
  "A confirmation names every session, each title on one line, cut short."
  (let ((sessions (list (drover-session-create :title (concat "Long\n" (make-string 60 ?l)))
                        (drover-session-create :id "id-only"))))
    (should (equal (drover--session-names sessions)
                   (concat "Long " (make-string 34 ?l) "…, id-only")))))

(ert-deftest drover-test-act-on-a-session-signals ()
  "A session alone whose backend fails signals, as before; listed, it is reported."
  (drover-test-with-backend (list (drover-test-session "a"))
    (drover-register-backend 'bare :label "Bare"
                             :sessions (lambda () (list (drover-session-create :backend 'bare :id "x"))))
    (let ((bare (drover-session-create :backend 'bare :id "x" :title "Bare x")))
      (should-error (drover-archive bare) :type 'user-error)
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest arguments)
                     (push (apply #'format-message format-string arguments) messages))))
          (drover-archive (list (drover-find 'fake "a") bare)))
        ;; Drover archives the sessions of a backend without an archive,
        ;; by conversation: a session telling none cannot be.
        (should (equal messages '("Archived 1 session; failed for Bare x (Bare x cannot be archived before its conversation is known)")))
        (should (equal drover-test-calls '((:archive "a"))))))))

;;;; The default prompt buffer

(ert-deftest drover-test-default-prompt-buffer-function ()
  "Drover's own prompt is the default prompt buffer, loaded on demand."
  (should (eq (eval (car (get 'drover-default-prompt-buffer-function 'standard-value)) t)
              #'drover-prompt-default-buffer))
  (should (fboundp 'drover-prompt-default-buffer)))

(provide 'drover-core-test)
;;; drover-core-test.el ends here
