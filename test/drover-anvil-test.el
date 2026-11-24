;;; drover-anvil-test.el --- Tests for drover-anvil  -*- lexical-binding: t; -*-

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

;; The tools run on the fake backend, given the operations agents use
;; (:submit, :requests, :answer, :blocker); the caller is named by a
;; function of `drover-anvil-callers' reading the `_meta' the tests
;; bind.  The last tests call the tools through Anvil's dispatcher, as
;; an MCP client does, when Anvil is on the load path (ANVIL_EL).

;;; Code:

(require 'ert)
(require 'json)
(require 'drover-test-helpers)
(require 'drover-anvil)
(require 'drover-claude-code)

(defvar anvil-server-request-meta nil)
(defvar anvil-server-transport-pid nil)

(defvar drover-anvil-test-requests nil
  "Requests of the fake sessions, an alist (ID . REQUESTS), for :requests.")

(defvar drover-anvil-test-blocker nil
  "What the fake :blocker answers, nil to take prompts.")

(defvar drover-anvil-test-queued nil
  "Why the fake :submit queues prompts rather than takes them, nil to take them.")

(defun drover-anvil-test-caller (meta)
  "Return the fake session META names in fake.session, for `drover-anvil-callers'."
  (when-let* ((id (alist-get 'fake.session meta)))
    (cons 'fake id)))

(defun drover-anvil-test--register-agent-operations ()
  "Give the fake backend the operations agents use, recorded in `drover-test-calls'."
  (apply #'drover-register-backend 'fake
         (append
          (list :submit (lambda (session text then _else)
                          (push (list :submit (drover-session-id session) text) drover-test-calls)
                          (if drover-anvil-test-queued (funcall then drover-anvil-test-queued) (funcall then)))
                :blocker (lambda (_session) drover-anvil-test-blocker)
                :buffer (lambda (session)
                          (and (equal (drover-session-id session) "busy") (get-buffer-create " *fake busy*")))
                :requests (lambda (session callback)
                            (funcall callback (alist-get (drover-session-id session) drover-anvil-test-requests
                                                         nil nil #'equal)))
                :answer (lambda (session id answer then _else)
                          (push (list :answer (drover-session-id session) id answer) drover-test-calls)
                          (funcall then)))
          (alist-get 'fake drover--backends))))

(defmacro drover-anvil-test-with (sessions caller &rest body)
  "Run BODY with the fake backend serving SESSIONS, the tools called by CALLER.
CALLER is the identifier of a fake session, or nil for none named."
  (declare (indent 2) (debug t))
  `(drover-test-with-backend ,sessions
                             (drover-test-with-state
                              (let ((drover-anvil-callers '(drover-anvil-test-caller))
                                    (anvil-server-request-meta (and ,caller (list (cons 'fake.session ,caller))))
                                    (drover-anvil-log-file nil)
                                    (drover-anvil--jobs (make-hash-table :test #'equal))
                                    (drover-anvil--started (make-hash-table :test #'equal))
                                    (drover-anvil--waits (make-hash-table :test #'equal))
                                    (drover-anvil--sent (make-hash-table :test #'equal))
                                    (drover-anvil-test-requests nil)
                                    (drover-anvil-test-blocker nil)
                                    (drover-anvil-test-queued nil))
                                (drover-anvil-test--register-agent-operations)
                                ,@body))))

(defun drover-anvil-test-json (text)
  "Return TEXT, a tool's answer, parsed."
  (let ((json-object-type 'alist) (json-array-type 'list) (json-false :false))
    (json-read-from-string text)))

(defun drover-anvil-test-keys (answer)
  "Return the keys of the sessions ANSWER, of drover-sessions, lists."
  (mapcar (lambda (session) (alist-get 'key session)) (alist-get 'sessions answer)))

(defun drover-anvil-test-log ()
  "Return the lines of the tools' log."
  (let ((file (drover-anvil--log-file)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (split-string (buffer-string) "\n" t)))))

(defun drover-anvil-test-project (&optional name)
  "Return a fresh repository directory, named after NAME, deleted with the state."
  (let ((directory (file-name-as-directory (expand-file-name (or name "project") drover-state-directory))))
    (make-directory (expand-file-name ".git" directory) t)
    directory))

;;;; Keys and data

(ert-deftest drover-anvil-test-keys-round-trip ()
  "Keys name sessions as BACKEND/ID, and those of a proxy by the owner's key."
  (drover-anvil-test-with (list (drover-test-session "a/b")) nil
    (let ((session (car drover-test-sessions)))
      (should (equal (drover-anvil-key-string session) "fake/a/b"))
      (should (equal (drover-anvil--parse-key "fake/a/b") '(fake . "a/b"))))
    (should-not (drover-anvil--parse-key "nosuchbackend/x"))
    (should-not (intern-soft "nosuchbackend"))
    (drover-test-register-simple 'remote:peer nil :remote "peer")
    (should (equal (drover-anvil--parse-key "remote:peer/opencode/ses_1")
                   '(remote:peer . (opencode . "ses_1"))))
    (should (equal (drover-anvil-key-string (drover-session-create :backend 'remote:peer :id '(opencode . "ses_1")))
                   "remote:peer/opencode/ses_1"))))

(ert-deftest drover-anvil-test-list-filters-and-marks-the-caller ()
  "The list filters by status and attention, marks the caller, and leaves out subagents."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'waiting
                                                     :requests '((:kind question :summary "Which one?")))
                                (drover-test-session "c" :status 'idle)
                                (drover-test-session "sub" :status 'working :parent "a"))
      "a"
    (let ((all (drover-anvil-test-json (drover-anvil--tool-sessions))))
      (should (equal (alist-get 'caller all) "fake/a"))
      (should (equal (sort (drover-anvil-test-keys all) #'string<) '("fake/a" "fake/b" "fake/c")))
      (should (eq (alist-get 'caller (seq-find (lambda (s) (equal (alist-get 'key s) "fake/a"))
                                               (alist-get 'sessions all)))
                  t)))
    (should (equal (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions "waiting,idle")))
                   '("fake/b" "fake/c")))
    (let ((waiting (drover-anvil-test-json (drover-anvil--tool-sessions nil nil nil nil t))))
      (should (equal (drover-anvil-test-keys waiting) '("fake/b")))
      (should (equal (alist-get 'summary (car (alist-get 'requests (car (alist-get 'sessions waiting)))))
                     "Which one?")))
    (should (member "fake/sub" (drover-anvil-test-keys
                                (drover-anvil-test-json (drover-anvil--tool-sessions nil nil nil nil nil nil t)))))
    (should (equal (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-sessions nil nil nil nil nil nil nil 1)))
                   3))
    (should (= (length (alist-get 'sessions (drover-anvil-test-json
                                             (drover-anvil--tool-sessions nil nil nil nil nil nil nil 1))))
               1))
    (should (string-match-p "\tdrover-sessions\tcaller=fake/a\t" (car (drover-anvil-test-log))))))

(ert-deftest drover-anvil-test-list-bounded ()
  "Titles and summaries are cut, whatever the backend reports."
  (drover-anvil-test-with (list (drover-test-session "a" :detail (make-string 100000 ?x)))
      nil
    (setf (drover-session-title (car drover-test-sessions)) (make-string 100000 ?y))
    (drover-backend-changed 'fake)
    (let ((text (drover-anvil--tool-sessions)))
      (should (< (length text) 2000)))))

;;;; The caller

(defconst drover-anvil-test-conversations
  '("11111111-1111-4111-8111-111111111111" "22222222-2222-4222-8222-222222222222"
    "33333333-3333-4333-8333-333333333333")
  "Identifiers of Claude Code conversations.")

(defun drover-anvil-test-claude-record (pid id &optional later)
  "Record, as Claude Code does, that its process PID runs the conversation ID.
LATER, seconds, dates the record that much after now, as a record
Claude Code rewrites later is."
  (let ((file (expand-file-name (format "%d.json" pid) (drover-claude-code-sessions-directory))))
    (drover-test-write file (format "{\"pid\": %d, \"sessionId\": \"%s\", \"kind\": \"interactive\"}" pid id))
    (when later
      (set-file-times file (time-add nil later)))))

(defun drover-anvil-test-default-callers ()
  "Return the functions `drover-anvil-callers' holds by default."
  (eval (car (get 'drover-anvil-callers 'standard-value)) t))

(defmacro drover-anvil-test-with-processes (processes &rest body)
  "Run BODY with `process-attributes' telling the processes PROCESSES names.
PROCESSES is a variable holding a list of (PID PPID START), START in
seconds since the epoch, which BODY may set."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'process-attributes)
              (lambda (pid)
                (when-let* ((process (assq pid ,processes)))
                  (list (cons 'ppid (nth 1 process))
                        (cons 'start (seconds-to-time (nth 2 process))))))))
     ,@body))

(ert-deftest drover-anvil-test-claude-code-caller ()
  "A Claude Code caller is the session of the conversation the process above the bridge runs.
The conversation is followed once /clear switches it, the process is
found above a program starting the bridge for it, within
`drover-anvil--client-depth', and never one started after the process
below it, which took the identifier of one that ended."
  (pcase-let ((`(,first ,second ,unknown) drover-anvil-test-conversations))
    (drover-test-with-homes
      (drover-anvil-test-with (list (drover-test-session "first" :engine 'claude-code :engine-id first)
                                    (drover-test-session "second" :engine 'claude-code :engine-id second))
          nil
        (let ((drover-anvil-callers (drover-anvil-test-default-callers))
              (processes '((100 200 30) (200 1 20) (1 0 10))))
          (drover-anvil-test-with-processes processes
            (drover-anvil-test-claude-record 200 first)
            ;; No bridge told its process.
            (should-not (drover-anvil--caller))
            (let ((anvil-server-transport-pid 100))
              (should (equal (drover-anvil--caller) '(fake . "first")))
              (drover-anvil-test-claude-record 200 second 10)
              (should (equal (drover-anvil--caller) '(fake . "second")))
              ;; The session OpenCode names comes first.
              (let ((anvil-server-request-meta '((ai.opencode/sessionID . "ses_1"))))
                (should (equal (drover-anvil--caller) '(opencode . "ses_1"))))
              (drover-anvil-test-claude-record 200 unknown 20)
              (should-not (drover-anvil--caller))
              ;; cmd started the bridge for Claude Code.
              (setq processes '((100 150 30) (150 200 25) (200 1 20)))
              (drover-anvil-test-claude-record 200 first 30)
              (should (equal (drover-anvil--caller) '(fake . "first")))
              (setq processes '((100 150 30) (150 160 28) (160 170 26) (170 200 25) (200 1 20)))
              (should-not (drover-anvil--caller))
              (setq processes '((100 200 30) (200 1 40)))
              (should-not (drover-anvil--caller)))))))))

;;;; Projects

(ert-deftest drover-anvil-test-project-filter ()
  "The project \"here\" lists the sessions of the caller's repository and its worktrees; any session is prompted."
  (drover-anvil-test-with nil "me"
    (let* ((project (drover-anvil-test-project "repo"))
           (worktree (file-name-as-directory (expand-file-name "wt" drover-state-directory)))
           (other (drover-anvil-test-project "other")))
      ;; A linked worktree: its .git is a file naming its git directory.
      (make-directory (expand-file-name ".git/worktrees/wt" project) t)
      (with-temp-file (expand-file-name ".git/worktrees/wt/commondir" project) (insert "../..\n"))
      (make-directory worktree t)
      (with-temp-file (expand-file-name ".git" worktree)
        (insert "gitdir: " (expand-file-name ".git/worktrees/wt" project) "\n"))
      (setq drover-test-sessions (list (drover-test-session "me" :directory (expand-file-name "sub" project))
                                       (drover-test-session "mine" :directory worktree)
                                       (drover-test-session "theirs" :directory other)))
      (drover-backend-changed 'fake)
      (should (equal (sort (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions))) #'string<)
                     '("fake/me" "fake/mine" "fake/theirs")))
      (should (equal (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions nil nil other)))
                     '("fake/theirs")))
      (should (equal (sort (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions nil nil "here")))
                           #'string<)
                     '("fake/me" "fake/mine")))
      ;; Sending: to the caller's project and to another alike.
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send "fake/mine" "Go on"))) "sent"))
      (should (equal (car drover-test-calls) '(:submit "mine" "Go on")))
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send "fake/theirs" "Go on"))) "sent"))
      (should (equal (car drover-test-calls) '(:submit "theirs" "Go on"))))))

(defun drover-anvil-test-prompts (&rest arguments)
  "Return the answer of drover-prompts called with ARGUMENTS, parsed."
  (drover-anvil-test-json (apply #'drover-anvil--tool-prompts arguments)))

(ert-deftest drover-anvil-test-prompts ()
  "The prompts of every host, of any project, narrowed by host and pages, and cut."
  (require 'drover-prompts)
  (drover-anvil-test-with (list (drover-test-session "me" :status 'working)) "me"
    (let* ((project (drover-anvil-test-project))
           (other (drover-anvil-test-project "other"))
           (drover-host "reaver")
           (asked nil)
           (prompts (list (list :engine 'opencode :id "ses_1" :time 1.0e9 :text "Mine, in the project"
                                :directory (expand-file-name "sub/" project) :title "Here")
                          (list :engine 'opencode :id "ses_2" :time 1.1e9 :text (make-string 100 ?o)
                                :directory other)
                          (list :engine 'claude-code :id "abc" :time 1.2e9 :text "On Core" :host "core"
                                :directory "c:/work/")))
           (hosts (list (list :host nil :label "Reaver" :status 'ok :count 2 :cut 0)
                        (list :host "core" :label "Core" :status 'ok :count 1 :cut 0
                              :failures '((codex . "unreadable"))))))
      (setf (drover-session-directory (car drover-test-sessions)) project)
      (drover-backend-changed 'fake)
      (cl-letf (((symbol-function 'drover-prompts-gather)
                 (lambda (since until callback &optional _seconds)
                   (push (list since until) asked)
                   (funcall callback (list :prompts prompts :hosts hosts))
                   #'ignore)))
        (let* ((answer (drover-anvil-test-prompts "2026-11-12T09:00:00-0800" "1795366800"))
               (first (car (alist-get 'prompts answer))))
          (should (equal (car asked) (list (float-time (encode-time (iso8601-parse "2026-11-12T09:00:00-0800")))
                                           1795366800.0)))
          (should (= (alist-get 'count answer) 3))
          (should (equal (mapcar (lambda (prompt) (alist-get 'host prompt)) (alist-get 'prompts answer))
                         '("reaver" "reaver" "core")))
          (should (equal (alist-get 'link first) "agent-session://reaver/opencode/ses_1"))
          (should (equal (alist-get 'title first) "Here"))
          (should (equal (alist-get 'engine (nth 2 (alist-get 'prompts answer))) "claude-code"))
          (should (equal (alist-get 'failures (nth 1 (alist-get 'hosts answer)))
                         '(((engine . "codex") (message . "unreadable"))))))
        ;; The default period: the seven days before now.
        (drover-anvil-test-prompts)
        (should (< (abs (- (nth 1 (car asked)) (float-time))) 5))
        (should (< (abs (- (- (nth 1 (car asked)) (nth 0 (car asked))) (* 7 86400))) 1))
        ;; One host's, a page of them, their texts cut.
        (should (equal (mapcar (lambda (prompt) (alist-get 'session prompt))
                               (alist-get 'prompts (drover-anvil-test-prompts nil nil "Core")))
                       '("abc")))
        (should (equal (mapcar (lambda (host) (alist-get 'host host))
                               (alist-get 'hosts (drover-anvil-test-prompts nil nil "local")))
                       '("reaver")))
        (let ((page (drover-anvil-test-prompts nil nil nil 1 1 30)))
          (should (eq (alist-get 'truncated page) t))
          (should (equal (alist-get 'session (car (alist-get 'prompts page))) "ses_2"))
          (should (length= (alist-get 'text (car (alist-get 'prompts page))) 31))
          (should (= (alist-get 'cut (car (alist-get 'prompts page))) 100)))
        ;; A caller its client does not name reads them all too.
        (let ((anvil-server-request-meta nil))
          (should (= (alist-get 'count (drover-anvil-test-prompts)) 3)))
        (should-error (drover-anvil--tool-prompts "1795366800" "1795000000"))))))

(ert-deftest drover-anvil-test-permissions ()
  "The permission prompts of every host, of any project, with their sessions' titles, narrowed by host."
  (require 'drover-permissions)
  (drover-anvil-test-with (list (drover-test-session "me" :status 'working :engine 'opencode :engine-id "ses_1"
                                                     :title "Session me"))
      "me"
    (let* ((project (drover-anvil-test-project))
           (drover-host "reaver")
           (prompts (list (list :engine 'opencode :id "ses_1" :time 1.0e9 :action "slack_send_message"
                                :reply "passed" :plugin "slack-send-ask" :directory project)
                          (list :engine 'opencode :id "ses_2" :time 1.1e9 :action "shell" :reply "once"
                                :resources '("git push") :rules '("shell git push* ask") :directory "/elsewhere/")
                          (list :engine 'claude-code :id "abc" :time 1.2e9 :action "Bash" :reply "reject"
                                :host "core" :directory "c:/work/")))
           (hosts (list (list :host nil :label "Reaver" :status 'ok :count 2 :cut 0)
                        (list :host "core" :label "Core" :status 'ok :count 1 :cut 0))))
      (setf (drover-session-directory (car drover-test-sessions)) project)
      (drover-backend-changed 'fake)
      (cl-letf (((symbol-function 'drover-permissions-gather)
                 (lambda (_since _until callback &optional _seconds)
                   (funcall callback (list :permissions prompts :hosts hosts))
                   #'ignore)))
        (let* ((answer (drover-anvil-test-json (drover-anvil--tool-permissions)))
               (listed (alist-get 'permissions answer)))
          (should (= (alist-get 'count answer) 3))
          (should (equal (mapcar (lambda (prompt) (list (alist-get 'host prompt) (alist-get 'action prompt)
                                                        (alist-get 'reply prompt)))
                                 listed)
                         '(("reaver" "slack_send_message" "passed") ("reaver" "shell" "once") ("core" "Bash" "reject"))))
          (should (equal (alist-get 'link (car listed)) "agent-session://reaver/opencode/ses_1"))
          (should (equal (alist-get 'title (car listed)) "Session me"))
          (should (equal (alist-get 'plugin (car listed)) "slack-send-ask"))
          (should (equal (alist-get 'resources (nth 1 listed)) '("git push")))
          (should (equal (alist-get 'rules (nth 1 listed)) '("shell git push* ask"))))
        (should (equal (mapcar (lambda (prompt) (alist-get 'session prompt))
                               (alist-get 'permissions (drover-anvil-test-json (drover-anvil--tool-permissions nil nil "core"))))
                       '("abc")))
        (let ((anvil-server-request-meta nil))
          (should (= (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-permissions))) 3)))))))

(ert-deftest drover-anvil-test-never-waits-for-itself ()
  "No session waits for itself nor for a session above it; every other action on them goes."
  (drover-anvil-test-with (list (drover-test-session "top" :status 'working)
                                (drover-test-session "me" :status 'working :parent "top"))
      "me"
    (should (string-match-p "cannot wait for itself" (cadr (should-error (drover-anvil--tool-wait "fake/me")))))
    (should (string-match-p "its turn waits for the caller's" (cadr (should-error (drover-anvil--tool-wait "fake/top")))))
    (dolist (key '("fake/me" "fake/top"))
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send key "hi"))) "sent")))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-interrupt "fake/top"))) "interrupted"))
    (should (assq :interrupt drover-test-calls))
    (should (equal (alist-get 'key (drover-anvil-test-json (drover-anvil--tool-session "fake/me" :json-false)))
                   "fake/me"))))

(ert-deftest drover-anvil-test-unknown-caller ()
  "A caller its client does not name does what any caller does, but what needs to know it."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working :directory default-directory)) nil
    (should (drover-anvil--tool-sessions))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send "fake/a" "hi"))) "sent"))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-interrupt "fake/a"))) "interrupted"))
    (should (string-match-p "needs the caller" (cadr (should-error (drover-anvil--tool-sessions nil nil "here")))))
    (should (string-match-p "Name the directory" (cadr (should-error (drover-anvil--tool-create nil nil "Start")))))))

(ert-deftest drover-anvil-test-blocker-refuses ()
  "A session whose buffer cannot take a prompt now is sent none, and the agent told why."
  (drover-anvil-test-with (list (drover-test-session "busy" :status 'working)) nil
    (let ((drover-anvil-test-blocker "a draft is typed"))
      (let ((err (should-error (drover-anvil--tool-send "fake/busy" "hi"))))
        (should (string-match-p "a draft is typed" (cadr err))))
      (should-not (assq :submit drover-test-calls))
      (setq drover-anvil-test-blocker nil)
      (should (drover-anvil--tool-send "fake/busy" "hi"))
      (should (assq :submit drover-test-calls)))))

(ert-deftest drover-anvil-test-send-queued ()
  "A prompt the session's host queued rather than took is told queued, with why."
  (drover-anvil-test-with (list (drover-test-session "far" :status 'working)) nil
    (let ((drover-anvil-test-queued "its turn runs: the prompt goes once it ends"))
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-send "fake/far" "later"))))
        (should (equal (alist-get 'outcome answer) "queued"))
        (should (string-match-p "its turn runs" (alist-get 'note answer)))))))

(ert-deftest drover-anvil-test-actions-logged-or-warned ()
  "Every call is logged; one that cannot be is taken all the same, and the user warned once."
  (drover-anvil-test-with (list (drover-test-session "a")) nil
    (drover-anvil--tool-send "fake/a" "Hello there")
    (should (seq-find (lambda (line) (string-match-p "drover-session-send.*acting.*11 characters: Hello there" line))
                      (drover-anvil-test-log)))
    (should (seq-find (lambda (line) (string-match-p "drover-session-send.*\tdone\tsent" line))
                      (drover-anvil-test-log)))
    (setq drover-test-calls nil)
    (let ((drover-anvil-log-file (expand-file-name "a-file/anvil.log" drover-state-directory))
          (drover--errors-reported (make-hash-table :test #'equal))
          (drover-error-interval 60)
          (warnings nil))
      (write-region "" nil (expand-file-name "a-file" drover-state-directory))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send "fake/a" "Hello"))) "sent"))
        (should (drover-anvil--tool-send "fake/a" "Again")))
      (should (equal (mapcar #'car (seq-filter (lambda (call) (eq (car call) :submit)) drover-test-calls))
                     '(:submit :submit)))
      (should (length= warnings 1))
      (should (string-match-p "go unlogged: .*a-file/anvil.log cannot be written" (car warnings))))))

;;;; Following up

(defvar drover-anvil-test-requested 0
  "How many times the OpenCode restart was asked for.")

(defmacro drover-anvil-test-with-schedule (&rest body)
  "Run BODY with an empty prompt queue, the events of restarts defined, and a watch.
The OpenCode restart asks for itself under the key \"oc-restart\",
noted in `drover-anvil-test-requested'."
  (declare (indent 0) (debug t))
  `(progn
     (require 'drover-schedule)
     (drover-test-with-engines
      (drover-test-register-engine 'opencode :directory #'ignore)
      (let ((drover-schedule--queue (drover-schedule--queue-make :loaded t))
            (drover-schedule--triggers nil)
            (drover-schedule-former-files nil)
            (drover-schedule-display-function #'ignore)
            (drover-anvil-test-requested 0))
        (drover-schedule-define-trigger 'opencode-restart "OpenCode restart"
                                        :request (lambda ()
                                                   (cl-incf drover-anvil-test-requested)
                                                   "oc-restart"))
        (drover-schedule-define-trigger 'emacs-restart "Emacs restart")
        (drover-schedule-define-trigger 'reply "reply" :repeat t)
        (unwind-protect
            (cl-letf (((symbol-function 'drover-schedule--refresh) #'ignore))
              ,@body)
          (maphash (lambda (_ timer) (cancel-timer timer))
                   (drover-schedule--queue-timers drover-schedule--queue)))))))

(ert-deftest drover-anvil-test-follow-up-scheduled ()
  "The caller's prompt waits for the event it names, which is asked for, and the call is logged."
  (drover-anvil-test-with (list (drover-test-session "me" :status 'working :engine 'opencode
                                                     :engine-id "ses_me" :directory temporary-file-directory))
      "me"
    (drover-anvil-test-with-schedule
      (let ((answer (drover-anvil-test-json
                     (drover-anvil--tool-after "opencode-restart" "Retry the Reynard tests."))))
        (should (equal (alist-get 'outcome answer) "scheduled"))
        (should (equal (alist-get 'event answer) "opencode-restart"))
        (should (equal (alist-get 'key answer) "fake/me"))
        (let ((job (car (drover-schedule--jobs))))
          (should (equal (plist-get job :id) (alist-get 'id answer)))
          (should (equal (plist-get job :after) "oc-restart"))
          (should (eq (plist-get job :trigger) 'opencode-restart))
          (should (equal (plist-get job :prompt) "Retry the Reynard tests."))
          (should (equal (plist-get job :link) "agent-session:opencode/ses_me"))
          (should (eq (plist-get job :backend) 'fake))))
      (should (= drover-anvil-test-requested 1))
      (should (seq-find (lambda (line) (string-match-p "drover-session-after\tcaller=fake/me\tsession=fake/me\tacting\tafter opencode-restart, 24 characters" line))
                        (drover-anvil-test-log)))
      ;; It is sent once the event happens, as the user's scheduled prompts are.
      (should (= (drover-schedule-fire "oc-restart") 1)))))

(ert-deftest drover-anvil-test-follow-up-refused ()
  "No follow-up waits for an unknown or repeating event, nor for a caller its client does not name."
  (drover-anvil-test-with (list (drover-test-session "me" :status 'working :engine 'opencode
                                                     :engine-id "ses_me" :directory temporary-file-directory))
      "me"
    (drover-anvil-test-with-schedule
      (let ((err (should-error (drover-anvil--tool-after "reply" "Again"))))
        (should (string-match-p "No event reply; drover knows opencode-restart (OpenCode restart), emacs-restart (Emacs restart)\\'"
                                (cadr err))))
      (should-error (drover-anvil--tool-after "opencode-restart" ""))
      (let ((anvil-server-request-meta nil))
        (should (string-match-p "caller is unknown"
                                (cadr (should-error (drover-anvil--tool-after "opencode-restart" "Retry"))))))
      (should-not (drover-schedule--jobs))
      (should (= drover-anvil-test-requested 0)))))

;;;; Scheduling

(ert-deftest drover-anvil-test-schedule ()
  "An agent schedules a prompt for a session of its project, its own included, once or recurring, hidden."
  (let ((project nil))
    (drover-anvil-test-with nil "me"
      (setq project (drover-anvil-test-project))
      (setq drover-test-sessions
            (list (drover-test-session "me" :status 'working :engine 'opencode :engine-id "ses_me" :directory project)
                  (drover-test-session "peer" :engine 'opencode :engine-id "ses_peer" :directory project)))
      (drover-anvil-test-with-schedule
        (let ((answer (drover-anvil-test-json
                       (drover-anvil--tool-schedule "fake/peer" "Write the brief." "every weekday 7:00"))))
          (should (equal (alist-get 'outcome answer) "scheduled"))
          (should (equal (alist-get 'key answer) "fake/peer"))
          (should (string-prefix-p "every weekday at 07:00, next " (alist-get 'when answer)))
          (should (alist-get 'next answer))
          (let ((job (drover-schedule--job (alist-get 'id answer))))
            (should (equal (plist-get job :every) '(:days (1 2 3 4 5) :at (7 . 0))))
            (should (eq (plist-get job :hidden) t))
            (should (equal (plist-get job :by) "agent fake/me"))
            (should (equal (plist-get job :link) "agent-session:opencode/ses_peer"))))
        ;; Its own session, though its turn runs: the prompt goes once it is over.
        (let ((answer (drover-anvil-test-json
                       (drover-anvil--tool-schedule "fake/me" "Check the deploy." "2099-01-01T07:00:00Z"))))
          (should (equal (alist-get 'outcome answer) "scheduled"))
          (should (= (plist-get (drover-schedule--job (alist-get 'id answer)) :due)
                     (float-time (encode-time (iso8601-parse "2099-01-01T07:00:00Z"))))))
        (let ((answer (drover-anvil-test-json (drover-anvil--tool-schedule "fake/me" "Go on." "after OpenCode restart"))))
          (should (equal (alist-get 'outcome answer) "scheduled"))
          (should (= drover-anvil-test-requested 1)))
        (should (seq-find (lambda (line) (string-match-p "drover-schedule\tcaller=fake/me\tsession=fake/peer\tacting\tevery weekday 7:00, 16 characters" line))
                          (drover-anvil-test-log)))
        (should (length= (drover-schedule--jobs) 3))))))

(ert-deftest drover-anvil-test-schedule-anywhere ()
  "A prompt is scheduled for any session, by any caller, but not for an unreadable or past time."
  (let ((project nil) (elsewhere nil))
    (drover-anvil-test-with nil "me"
      (setq project (drover-anvil-test-project)
            elsewhere (drover-anvil-test-project "elsewhere"))
      (setq drover-test-sessions
            (list (drover-test-session "me" :engine 'opencode :engine-id "ses_me" :directory project)
                  (drover-test-session "other" :engine 'opencode :engine-id "ses_other" :directory elsewhere)))
      (drover-anvil-test-with-schedule
        (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-schedule "fake/other" "Hi" "in 1h")))
                       "scheduled"))
        (dolist (case '(("soon" "cannot read “soon” as a time") ("2001-01-01T00:00:00Z" "is past")
                        ("every day 7:00 Mars/Olympus_Mons" "no time zone Mars/Olympus_Mons here")
                        ("" "Tell when")))
          (should (string-search (cadr case)
                                 (cadr (should-error (drover-anvil--tool-schedule "fake/me" "Hi" (car case)))))))
        (should-error (drover-anvil--tool-schedule "fake/me" "" "in 1h"))
        (let ((anvil-server-request-meta nil))
          (let ((answer (drover-anvil-test-json (drover-anvil--tool-schedule "fake/me" "Hi" "in 1h"))))
            (should (equal (plist-get (drover-schedule--job (alist-get 'id answer)) :by) "agent"))))
        (should (length= (drover-schedule--jobs) 2))))))

(ert-deftest drover-anvil-test-schedule-claude-code-caller ()
  "A Claude Code session schedules a prompt for itself, named as who scheduled it.
Without the bridge telling its process, the caller is unknown, and the
prompt is told scheduled by an agent alone."
  (let ((conversation (car drover-anvil-test-conversations))
        (processes '((100 200 30) (200 1 20))))
    (drover-test-with-homes
      (drover-anvil-test-with nil nil
        (setq drover-test-sessions (list (drover-test-session "terminal-12" :engine 'claude-code
                                                              :engine-id conversation
                                                              :directory (drover-anvil-test-project))))
        (drover-backend-changed 'fake)
        (drover-anvil-test-with-schedule
          (drover-test-register-engine 'claude-code :directory #'ignore)
          (let ((drover-anvil-callers (drover-anvil-test-default-callers)))
            (drover-anvil-test-claude-record 200 conversation)
            (drover-anvil-test-with-processes processes
              (let ((answer (drover-anvil-test-json
                             (drover-anvil--tool-schedule "fake/terminal-12" "Look for BA's email." "in 1h"))))
                (should (equal (plist-get (drover-schedule--job (alist-get 'id answer)) :by) "agent")))
              (let* ((anvil-server-transport-pid 100)
                     (answer (drover-anvil-test-json
                              (drover-anvil--tool-schedule "fake/terminal-12" "Look for BA's email." "in 1h"))))
                (should (equal (alist-get 'outcome answer) "scheduled"))
                (let ((job (drover-schedule--job (alist-get 'id answer))))
                  (should (equal (plist-get job :by) "agent fake/terminal-12"))
                  (should (equal (plist-get job :link) (concat "agent-session:claude-code/" conversation))))
                (should (seq-find (lambda (line)
                                    (string-match-p "drover-schedule\tcaller=fake/terminal-12\tsession=fake/terminal-12\tdone" line))
                                  (drover-anvil-test-log)))))))))))

(ert-deftest drover-anvil-test-fire ()
  "An agent fires an event by name; the prompts waiting go, with what happened."
  (drover-anvil-test-with (list (drover-test-session "me" :engine 'opencode :engine-id "ses_me"
                                                     :directory temporary-file-directory))
      "me"
    (drover-anvil-test-with-schedule
      (drover-schedule-add-after-trigger (drover-schedule-session-target (car drover-test-sessions))
                                         'emacs-restart "Verify the restart.")
      (should (string-match-p "No event weekly; drover knows opencode-restart (OpenCode restart)"
                              (cadr (should-error (drover-anvil--tool-fire "weekly")))))
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-fire "emacs-restart" "Restarted at 3:00"))))
        (should (equal answer '((outcome . "fired") (event . "emacs-restart") (released . 1)))))
      (should (equal (plist-get (car (drover-schedule--jobs)) :prompt) "Verify the restart.\n\nRestarted at 3:00"))
      (drover-schedule-add-after-trigger (drover-schedule-session-target (car drover-test-sessions))
                                         'emacs-restart "Never.")
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-fire "emacs-restart" nil t)))
                     "cancelled"))
      (should (seq-find (lambda (line) (string-match-p "drover-schedule-fire\tcaller=fake/me\tsession=-\tdone\t1 released" line))
                        (drover-anvil-test-log))))))

(ert-deftest drover-anvil-test-scheduled ()
  "The list tells each prompt's session, when, status and who scheduled it, of every session."
  (let ((project nil) (elsewhere nil))
    (drover-anvil-test-with nil "me"
      (setq project (drover-anvil-test-project)
            elsewhere (drover-anvil-test-project "elsewhere"))
      (setq drover-test-sessions
            (list (drover-test-session "me" :engine 'opencode :engine-id "ses_me" :directory project)
                  (drover-test-session "other" :engine 'opencode :engine-id "ses_other" :directory elsewhere)))
      (drover-anvil-test-with-schedule
        (drover-anvil--tool-schedule "fake/me" "Brief." "every day 7:00")
        (drover-schedule-add (drover-schedule-session-target (cadr drover-test-sessions))
                             (time-add (current-time) 3600) "Theirs.")
        (let ((answer (drover-anvil-test-json (drover-anvil--tool-scheduled))))
          (should (= (alist-get 'count answer) 2))
          (let ((mine (seq-find (lambda (job) (equal (alist-get 'key job) "fake/me")) (alist-get 'scheduled answer))))
            (should (equal (alist-get 'status mine) "recurring"))
            (should (equal (alist-get 'by mine) "agent fake/me"))
            (should (string-prefix-p "every day at 07:00, next " (alist-get 'when mine)))
            (should (equal (alist-get 'prompt mine) "Brief."))))
        (should (= 1 (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-scheduled "fake/other")))))
        (should (= 0 (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-scheduled nil "sent")))))
        (let ((anvil-server-request-meta nil))
          (should (= 2 (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-scheduled))))))))))

;;;; Reading

(ert-deftest drover-anvil-test-read-with-recap-and-requests ()
  "Reading tells the last turn's recap and what the pending requests ask."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'waiting :model "Opus"
                                                     :requests '((:kind question :summary "Pick")))) nil
    (setq drover-anvil-test-requests
          '(("a" (:id "q1" :kind question :title "Pick one"
                  :fields ((:key "choice" :type "string" :title "Which" :options ("x" "y") :custom nil))))))
    (let ((answer (drover-anvil-test-json (drover-anvil--tool-session "fake/a"))))
      (should (equal (alist-get 'reply answer) "Reply of a"))
      (should (equal (alist-get 'prompt answer) "Prompt of a"))
      (should (equal (alist-get 'model answer) "Opus"))
      (let ((request (car (alist-get 'pending_requests answer))))
        (should (equal (alist-get 'id request) "q1"))
        (should (equal (alist-get 'options (car (alist-get 'fields request))) '("x" "y")))))))

(ert-deftest drover-anvil-test-read-later-without-waiting-client ()
  "A recap arriving later is the answer of a job, which drover-job returns once there."
  (drover-anvil-test-with (list (drover-test-session "a")) nil
    (setq drover-test-recap-later t)
    (let* ((answer (drover-anvil-test-json (drover-anvil--tool-session "fake/a")))
           (job (alist-get 'job answer)))
      (should (eq (alist-get 'pending answer) t))
      (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
      (funcall (cdr (car drover-test-recap-pending)) '(:prompt "P" :reply "R"))
      (should (equal (alist-get 'reply (drover-anvil-test-json (drover-anvil--tool-job job))) "R"))
      ;; Taken once.
      (should-error (drover-anvil--tool-job job)))))

(ert-deftest drover-anvil-test-job-of-another-caller ()
  "A job's answer goes to whichever session asks for it."
  (drover-anvil-test-with (list (drover-test-session "a") (drover-test-session "b")) "a"
    (setq drover-test-recap-later t)
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-session "fake/b")))))
      (let ((anvil-server-request-meta '((fake.session . "b"))))
        (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))))))

;;;; Waiting

(defun drover-anvil-test-set-status (id status &optional since)
  "Give the fake session ID STATUS, begun at SINCE, and report it."
  (let ((session (seq-find (lambda (session) (equal (drover-session-id session) id)) drover-test-sessions)))
    (setq drover-test-sessions
          (cons (let ((copy (copy-sequence session)))
                  (setf (drover-session-status copy) status
                        (drover-session-since copy) (or since (float-time)))
                  copy)
                (delq session drover-test-sessions)))
    (drover-backend-changed 'fake id)))

(ert-deftest drover-anvil-test-wait-until-the-turn-ends ()
  "A wait answers once the session's turn ended, with its recap."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working :since 1.0)) nil
    (let* ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60)))))
      (should job)
      (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
      (drover-anvil-test-set-status "a" 'idle)
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-job job))))
        (should (equal (alist-get 'outcome answer) "ended"))
        (should (equal (alist-get 'status answer) "idle"))
        (should (equal (alist-get 'reply answer) "Reply of a"))))))

(ert-deftest drover-anvil-test-wait-for-the-turn-of-a-prompt-sent ()
  "After a prompt went, a session still idle from before is not done: its next turn is awaited."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'idle :since 1.0)) nil
    (drover-anvil--tool-send "fake/a" "Do it")
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60 nil :json-false)))))
      (should job)
      (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
      ;; The turn ran and ended between two polls.
      (drover-anvil-test-set-status "a" 'done (+ (float-time) 1))
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-job job))) "ended")))
    ;; Without a prompt sent, a session not running is done at once.
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60 nil :json-false)))
                   "ended"))))

(ert-deftest drover-anvil-test-wait-needs-user-and-times-out ()
  "A wait ends when the session needs the user, telling what it asks, or at its timeout."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'working))
      nil
    (setq drover-anvil-test-requests '(("a" (:id "p1" :kind permission :title "Run rm" :choices ("once" "reject")))))
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60)))))
      (drover-anvil-test-set-status "a" 'waiting)
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-job job))))
        (should (equal (alist-get 'outcome answer) "needs_user"))
        (should (equal (alist-get 'id (car (alist-get 'pending_requests answer))) "p1"))))
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/b" 60)))))
      (drover-anvil--job-put job :deadline (1- (float-time)))
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-job job))))
        (should (equal (alist-get 'outcome answer) "timeout"))
        (should (equal (alist-get 'status answer) "working"))))))

(ert-deftest drover-anvil-test-wait-cycle-refused ()
  "A session may not wait for one waiting for it."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'working))
      "a"
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/b" 60)))))
      (let ((anvil-server-request-meta '((fake.session . "b"))))
        (let ((err (should-error (drover-anvil--tool-wait "fake/a" 60))))
          (should (string-match-p "waits for the caller" (cadr err)))))
      ;; Once the wait ended, it may.
      (drover-anvil-test-set-status "b" 'idle)
      (drover-anvil--tool-job job)
      (let ((anvil-server-request-meta '((fake.session . "b"))))
        (should (drover-anvil--tool-wait "fake/a" 60))))))

(ert-deftest drover-anvil-test-wait-on-gone-session ()
  "A session leaving while awaited ends the wait."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)) nil
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60)))))
      (setq drover-test-sessions nil)
      (drover-backend-changed 'fake)
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-job job))) "gone")))))

;;;; Starting, answering, interrupting, archiving

(ert-deftest drover-anvil-test-create-in-the-project ()
  "A session starts unseen in the caller's directory, takes its prompt, and is told the caller's."
  (drover-anvil-test-with nil "me"
    (let ((project (drover-anvil-test-project)))
      (setq drover-test-sessions (list (drover-test-session "me" :directory project)))
      (drover-backend-changed 'fake)
      (let ((answer (drover-anvil-test-json (drover-anvil--tool-create nil nil "Start" nil nil "build" "Child"))))
        (should (equal (alist-get 'outcome answer) "started"))
        (let ((key (alist-get 'key answer))
              (create (assq :create drover-test-calls)))
          (should (equal (nth 1 create) project))
          (should (plist-get (nth 3 create) :hidden))
          (should (equal (plist-get (nth 3 create) :agent) "build"))
          (should (equal (plist-get (nth 3 create) :title) "Child"))
          (should (equal (assq :send drover-test-calls) (list :send (cdr (drover-anvil--parse-key key)) "Start" nil)))
          (drover-anvil-test-set-status (cdr (drover-anvil--parse-key key)) 'working)
          (should (eq (alist-get 'started_by_caller (drover-anvil-test-json (drover-anvil--tool-session key :json-false)))
                      t))
          (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-interrupt key)))
                         "interrupted"))
          (should (assq :interrupt drover-test-calls))
          (setq drover-anvil-test-requests
                `((,(cdr (drover-anvil--parse-key key))
                   (:id "q" :kind question :fields ((:key "name" :type "string")))
                   (:id "p" :kind permission :choices ("once" "always" "reject")))))
          (should (equal (alist-get 'outcome (drover-anvil-test-json
                                              (drover-anvil--tool-answer key "q" nil '((name . "Ada")))))
                         "answered"))
          (should (equal (assq :answer drover-test-calls)
                         (list :answer (cdr (drover-anvil--parse-key key)) "q" '(:answer (("name" . "Ada"))))))
          (drover-anvil--tool-answer key "p" "always")
          (should (equal (nth 3 (assq :answer drover-test-calls)) '(:decision "always")))
          (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-archive key)))
                         "archived"))
          (should (assq :archive drover-test-calls)))))))

(ert-deftest drover-anvil-test-create-anywhere ()
  "A session starts in any directory, of any harness starting sessions here, but not in a missing directory."
  (drover-anvil-test-with nil "me"
    (let ((project (drover-anvil-test-project))
          (other (drover-anvil-test-project "other")))
      (setq drover-test-sessions (list (drover-test-session "me" :directory project)))
      (drover-backend-changed 'fake)
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-create "fake" other "Start")))
                     "started"))
      (should (equal (nth 1 (assq :create drover-test-calls)) other))
      (setq drover-test-calls nil)
      (should (string-match-p "nosuch starts no session here; fake do"
                              (cadr (should-error (drover-anvil--tool-create "nosuch" nil "Start")))))
      (should-error (drover-anvil--tool-create nil (expand-file-name "missing" project) "Start"))
      ;; A model its engine does not run.
      (drover-test-with-engines
        (drover-register-model-source 'fake (lambda (_directory)
                                              (list (drover-test-model "elsewhere" :engines '(other)))))
        (let ((err (should-error (drover-anvil--tool-create nil nil "Start" "elsewhere"))))
          (should (string-match-p "does not run" (cadr err)))))
      (should-not (assq :create drover-test-calls)))))

(ert-deftest drover-anvil-test-not-own-sessions ()
  "Sessions the caller did not start are interrupted, answered and archived alike."
  (drover-anvil-test-with (list (drover-test-session "me") (drover-test-session "a" :status 'working)) "me"
    (setq drover-anvil-test-requests '(("a" (:id "q" :kind question :fields ((:key "x" :type "string")))
                                       (:id "p" :kind permission :choices ("once" "always" "reject")))))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-interrupt "fake/a"))) "interrupted"))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-answer "fake/a" "q" nil '((x . "y")))))
                   "answered"))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-answer "fake/a" "p" "once")))
                   "answered"))
    (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-archive "fake/a"))) "archived"))
    (should (equal (mapcar #'car (seq-filter (lambda (call) (memq (car call) '(:interrupt :archive :answer)))
                                             (reverse drover-test-calls)))
                   '(:interrupt :answer :answer :archive)))))

;;;; On other hosts

(defmacro drover-anvil-test-with-peer (sessions &rest body)
  "Run BODY with the peer core, its backend remote:core serving SESSIONS.
SESSIONS is a variable holding the peer's sessions here, which BODY may
set; `drover-remote-peer-named' knows core by its name or its label,
Core."
  (declare (indent 1) (debug t))
  `(progn
     (require 'drover-remote)
     (drover-test-register-simple 'remote:core (lambda () ,sessions) :remote "core")
     (cl-letf (((symbol-function 'drover-remote-peer-named)
                (lambda (host) (and (member (downcase host) '("core")) "core"))))
       ,@body)))

(defun drover-anvil-test-peer-session (id &rest properties)
  "Return the session of core whose own key is (opencode . ID), with PROPERTIES."
  (apply #'drover-session-create
         (append properties (list :backend 'remote:core :id (cons 'opencode id) :host "core"
                                  :title (concat "Core " id)))))

(ert-deftest drover-anvil-test-create-on-another-host ()
  "A session starts on the peer a host names, its model and effort found there; it answers once listed here."
  (let ((there nil) (asked nil) (answer nil))
    (drover-anvil-test-with nil "me"
      (let ((project (drover-anvil-test-project)))
        (setq drover-test-sessions (list (drover-test-session "me" :directory project)))
        (drover-backend-changed 'fake)
        (drover-anvil-test-with-peer there
          (cl-letf (((symbol-function 'drover-remote-start-later)
                     (lambda (name directory local harness engine choose text callback)
                       (setq asked (list name directory local harness engine text))
                       (let ((catalog (list :models (list (drover-test-model "deep-2" :engines '(opencode)
                                                                             :variants '((max :effort "max"))
                                                                             :variants-known t))))
                             (target (list :backend 'opencode :engine 'opencode :label "OpenCode session")))
                         (setq answer callback)
                         (funcall choose target catalog)))))
            (let* ((pending (drover-anvil-test-json
                             (drover-anvil--tool-create nil nil "Hello there" "deep-2" "max" "plan" "Far" nil "Core")))
                   (job (alist-get 'job pending)))
              (should job)
              ;; The caller's directory maps there, the host's to choose; OpenCode's by default.
              (should (equal asked (list "core" nil project nil nil "Hello there")))
              (funcall answer (list :key '(opencode . "ses_far") :title "Far" :directory "c:/work/"
                                    :backend 'opencode))
              ;; Started there, not listed here yet: still pending.
              (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
              (setq there (list (drover-anvil-test-peer-session "ses_far" :status 'working)))
              (drover-backend-changed 'remote:core)
              (let ((started (drover-anvil-test-json (drover-anvil--tool-job job))))
                (should (equal (alist-get 'outcome started) "started"))
                (should (equal (alist-get 'key started) "remote:core/opencode/ses_far"))
                (should (equal (alist-get 'host started) "core"))
                (should (equal (alist-get 'directory started) "c:/work/"))
                (should (eq (alist-get 'sent started) t)))
              ;; Told the caller's, and awaited from the prompt it was sent.
              (should (eq (alist-get 'started_by_caller
                                     (drover-anvil-test-json (drover-anvil--tool-session "remote:core/opencode/ses_far"
                                                                                         :json-false)))
                          t))
              (should (gethash '(remote:core . (opencode . "ses_far")) drover-anvil--sent)))))))))

(ert-deftest drover-anvil-test-create-on-another-host-chooses ()
  "On another host, the model must be one it offers, and its effort one the model runs; a refusal there fails the call."
  (let ((there nil) (choose nil) (answer nil))
    (drover-anvil-test-with (list (drover-test-session "me")) "me"
      (drover-anvil-test-with-peer there
        (cl-letf (((symbol-function 'drover-remote-start-later)
                   (lambda (_name _directory _local _harness _engine chooser _text callback)
                     (setq choose chooser answer callback))))
          (let ((target (list :backend 'opencode :engine 'opencode :label "OpenCode session"))
                (catalog (list :models (list (drover-test-model "deep-2" :engines '(opencode)
                                                                :variants '((max :effort "max"))
                                                                :variants-known t)
                                             (drover-test-model "other" :engines '(codex))))))
            (drover-anvil--tool-create nil "c:/work/" "Hi" "deep-2" "max" nil nil nil "core")
            (let ((options (funcall choose target catalog)))
              (should (equal (drover-model-id (plist-get options :model)) "deep-2"))
              (should (eq (plist-get options :variant) 'max)))
            (drover-anvil--tool-create nil nil "Hi" "nosuch" nil nil nil nil "core")
            (should (string-match-p "No model nosuch is offered for OpenCode session on core"
                                    (funcall choose target catalog)))
            (drover-anvil--tool-create nil nil "Hi" "other" nil nil nil nil "core")
            (should (string-match-p "does not run" (funcall choose target catalog)))
            (drover-anvil--tool-create nil nil "Hi" "deep-2" "ultra" nil nil nil "core")
            (should-error (funcall choose target catalog))
            ;; The host's refusal is the call's.
            (let ((job (alist-get 'job (drover-anvil-test-json
                                        (drover-anvil--tool-create nil nil "Hi" nil nil nil nil nil "core")))))
              (funcall answer (list :failure "Core has no directory c:/nowhere/"))
              (should (string-match-p "no directory" (cadr (should-error (drover-anvil--tool-job job))))))
            (should (string-match-p "No host mars" (cadr (should-error (drover-anvil--tool-create nil nil "Hi" nil nil nil nil nil "mars")))))))))))

(ert-deftest drover-anvil-test-schedule-on-another-host ()
  "A prompt for a session of another host is scheduled there, the time left for it to read, an ISO time sent as one."
  (let ((there (list (drover-anvil-test-peer-session "ses_far")))
        (asked nil)
        (answer nil))
    (drover-anvil-test-with (list (drover-test-session "me")) "me"
      (drover-anvil-test-with-peer there
        (drover-backend-changed 'remote:core)
        (cl-letf (((symbol-function 'drover-remote-schedule-later)
                   (lambda (session when text argument by callback)
                     (setq asked (list (drover-session-key session) when text argument by)
                           answer callback))))
          (let ((job (alist-get 'job (drover-anvil-test-json
                                      (drover-anvil--tool-schedule "remote:core/opencode/ses_far" "Brief." "after Core restart"
                                                                   "it")))))
            ;; Read there, with its own events: not here, where no such event is.
            (should (equal asked '((remote:core . (opencode . "ses_far")) "after Core restart" "Brief." "it" "agent fake/me")))
            (funcall answer (list :id "J1" :when "after the next Core restart" :next nil))
            (let ((scheduled (drover-anvil-test-json (drover-anvil--tool-job job))))
              (should (equal (alist-get 'outcome scheduled) "scheduled"))
              (should (equal (alist-get 'id scheduled) "J1"))
              (should (equal (alist-get 'host scheduled) "core"))
              (should (equal (alist-get 'when scheduled) "after the next Core restart"))))
          (drover-anvil--tool-schedule "remote:core/opencode/ses_far" "Later." "2099-01-01T07:00:00Z")
          (should (equal (cadr asked) (seconds-to-time (float-time (encode-time (iso8601-parse "2099-01-01T07:00:00Z"))))))
          (should (string-match-p "is past" (cadr (should-error (drover-anvil--tool-schedule "remote:core/opencode/ses_far"
                                                                                               "Late." "2001-01-01T00:00:00Z")))))
          (let ((job (alist-get 'job (drover-anvil-test-json
                                      (drover-anvil--tool-schedule "remote:core/opencode/ses_far" "Hi" "soon")))))
            (funcall answer (list :failure "cannot read “soon” as a time"))
            (should (string-match-p "soon" (cadr (should-error (drover-anvil--tool-job job)))))))))))

(ert-deftest drover-anvil-test-scheduled-on-other-hosts ()
  "The scheduled prompts of every host are listed together, the newest first, narrowed by host, session and status."
  (let ((there (list (drover-anvil-test-peer-session "ses_far"))))
    (drover-anvil-test-with (list (drover-test-session "me" :engine 'opencode :engine-id "ses_me"
                                                       :directory temporary-file-directory))
        "me"
      (drover-anvil-test-with-schedule
        (drover-anvil-test-with-peer there
          (drover-backend-changed 'remote:core)
          (drover-schedule-add (drover-schedule-session-target (car drover-test-sessions))
                               (time-add (current-time) 3600) "Here.")
          (cl-letf (((symbol-function 'drover-remote-jobs-later)
                     (lambda (callback &optional _seconds)
                       (funcall callback
                                (list (list :host "core" :label "Core"
                                            :jobs (list (list :host "core" :id "J1" :key '(remote:core . (opencode . "ses_far"))
                                                              :title "Core ses_far" :prompt "There." :status 'pending
                                                              :by "agent x on reaver" :created (+ (float-time) 10)
                                                              :every "every weekday at 09:00"
                                                              :next (+ (float-time) 7200))
                                                        (list :host "core" :id "J2" :status 'sent
                                                              :created (- (float-time) 10) :after "Emacs restart")))
                                      (list :host "adept" :label "Adept" :failure "not connected")))
                       #'ignore)))
            (let* ((answer (drover-anvil-test-json (drover-anvil--tool-scheduled)))
                   (listed (alist-get 'scheduled answer)))
              (should (= (alist-get 'count answer) 3))
              (should (equal (mapcar (lambda (job) (alist-get 'id job)) listed)
                             (list "J1" (plist-get (car (drover-schedule--jobs)) :id) "J2")))
              (let ((far (car listed)))
                (should (equal (alist-get 'host far) "core"))
                (should (equal (alist-get 'key far) "remote:core/opencode/ses_far"))
                (should (equal (alist-get 'when far) "every weekday at 09:00"))
                (should (equal (alist-get 'by far) "agent x on reaver"))
                (should (alist-get 'next far)))
              (should (equal (alist-get 'when (nth 2 listed)) "after the next Emacs restart"))
              (should (equal (alist-get 'hosts answer)
                             '(((host . "core") (count . 2)) ((host . "adept") (message . "not connected"))))))
            (should (equal (mapcar (lambda (job) (alist-get 'id job))
                                   (alist-get 'scheduled (drover-anvil-test-json
                                                          (drover-anvil--tool-scheduled "remote:core/opencode/ses_far"))))
                           '("J1")))
            (should (equal (mapcar (lambda (job) (alist-get 'id job))
                                   (alist-get 'scheduled (drover-anvil-test-json (drover-anvil--tool-scheduled nil "sent"))))
                           '("J2")))
            (should (= 2 (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-scheduled nil nil nil "Core")))))
            (should (= 1 (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-scheduled nil nil nil "local")))))))))))

;;;; Through Anvil's dispatcher

(defconst drover-anvil-test--anvil (locate-library "anvil-server")
  "Anvil's server, when on the load path.")

(defun drover-anvil-test-call (id tool arguments &optional meta)
  "Return the JSON-RPC text calling TOOL with ARGUMENTS, request ID, `_meta' META."
  (json-encode `((jsonrpc . "2.0") (id . ,id) (method . "tools/call")
                 (params . ((name . ,tool) (arguments . ,arguments)
                            ,@(and meta `((_meta . ,meta))))))))

(defun drover-anvil-test-tool-text (response)
  "Return the text of the tool result of RESPONSE, a JSON-RPC text."
  (let ((parsed (json-read-from-string response)))
    (or (alist-get 'text (aref (alist-get 'content (alist-get 'result parsed)) 0))
        (format "%S" parsed))))

(defmacro drover-anvil-test-with-anvil (&rest body)
  "Run BODY with Anvil's server running and the tools registered, or skip."
  (declare (indent 0) (debug t))
  `(progn
     (unless drover-anvil-test--anvil
       (ert-skip "Anvil is not on the load path (ANVIL_EL)"))
     (require 'anvil-server)
     (let ((anvil-server--running t)
           (drover-anvil-server-id "drover-anvil-test"))
       (drover-anvil-enable)
       (unwind-protect
           (progn ,@body)
         (drover-anvil-disable)))))

(ert-deftest drover-anvil-test-through-anvil ()
  "The tools answer through Anvil's dispatcher, the caller named by the call's `_meta'."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'idle))
      nil
    (drover-anvil-test-with-anvil
      (let* ((list (json-read-from-string
                    (anvil-server-process-jsonrpc
                     (json-encode '((jsonrpc . "2.0") (id . 1) (method . "tools/list")))
                     drover-anvil-server-id)))
             (names (mapcar (lambda (tool) (alist-get 'name tool)) (alist-get 'tools (alist-get 'result list)))))
        (dolist (id (drover-anvil-tool-ids))
          (should (member id names))))
      (let ((answer (drover-anvil-test-json
                     (drover-anvil-test-tool-text
                      (anvil-server-process-jsonrpc
                       (drover-anvil-test-call 2 "drover-sessions" '((status . "working")) '((fake.session . "b")))
                       drover-anvil-server-id)))))
        (should (equal (alist-get 'caller answer) "fake/b"))
        (should (equal (drover-anvil-test-keys answer) '("fake/a"))))
      ;; A refusal reads as the tool's error.
      (should (string-match-p "itself"
                              (drover-anvil-test-tool-text
                               (anvil-server-process-jsonrpc
                                (drover-anvil-test-call 3 "drover-session-wait" '((key . "fake/b")) '((fake.session . "b")))
                                drover-anvil-server-id))))
      ;; A client that cannot wait gets the job.
      (let* ((pending (drover-anvil-test-json
                       (drover-anvil-test-tool-text
                        (anvil-server-process-jsonrpc
                         (drover-anvil-test-call 4 "drover-session-wait" '((key . "fake/a") (timeout . 60)))
                         drover-anvil-server-id))))
             (job (alist-get 'job pending)))
        (should job)
        (drover-anvil-test-set-status "a" 'idle)
        (should (equal (alist-get 'outcome (drover-anvil-test-json
                                            (drover-anvil-test-tool-text
                                             (anvil-server-process-jsonrpc
                                              (drover-anvil-test-call 5 "drover-job" `((job . ,job)))
                                              drover-anvil-server-id))))
                       "ended"))))))

(ert-deftest drover-anvil-test-through-anvil-deferred ()
  "A client that waits gets the wait's answer once the turn ended, Anvil polling."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'working)) nil
    (drover-anvil-test-with-anvil
      (unless (fboundp 'anvil-server-deferred-poll)
        (ert-skip "This Anvil cannot defer answers"))
      (let* ((reply (let ((anvil-server-deferrable t))
                      (anvil-server-process-jsonrpc
                       (drover-anvil-test-call 9 "drover-session-wait" '((key . "fake/a") (timeout . 60)))
                       drover-anvil-server-id)))
             (job (and (string-match "\\`anvil-deferred:\\([^:]+\\):" reply) (match-string 1 reply))))
        (should job)
        (should (equal (anvil-server-deferred-poll job) "-"))
        (drover-anvil-test-set-status "a" 'idle)
        (let ((response (anvil-server-deferred-poll job)))
          (should (equal (alist-get 'id (json-read-from-string response)) 9))
          (let ((answer (drover-anvil-test-json (drover-anvil-test-tool-text response))))
            (should (equal (alist-get 'outcome answer) "ended"))
            (should (equal (alist-get 'reply answer) "Reply of a"))))
        (should (zerop (hash-table-count drover-anvil--jobs)))))))

(provide 'drover-anvil-test)
;;; drover-anvil-test.el ends here
