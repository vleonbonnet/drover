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

(defvar anvil-server-request-meta nil)

(defvar drover-anvil-test-requests nil
  "Requests of the fake sessions, an alist (ID . REQUESTS), for :requests.")

(defvar drover-anvil-test-blocker nil
  "What the fake :blocker answers, nil to take prompts.")

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
                          (funcall then))
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
             (drover-anvil-permissions (default-value 'drover-anvil-permissions))
             (drover-anvil-scratch-directories nil)
             (drover-anvil-create-harnesses '(fake))
             (drover-anvil-log-file nil)
             (drover-anvil--jobs (make-hash-table :test #'equal))
             (drover-anvil--started (make-hash-table :test #'equal))
             (drover-anvil--waits (make-hash-table :test #'equal))
             (drover-anvil--sent (make-hash-table :test #'equal))
             (drover-anvil-test-requests nil)
             (drover-anvil-test-blocker nil))
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

;;;; Scopes

(ert-deftest drover-anvil-test-project-scope ()
  "The scope `project' takes the sessions of the caller's repository and its worktrees."
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
      (setf (alist-get 'list drover-anvil-permissions) 'project)
      (should (equal (sort (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions))) #'string<)
                     '("fake/me" "fake/mine")))
      (should (equal (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions nil nil other)))
                     nil))
      (setf (alist-get 'list drover-anvil-permissions) 'any)
      (should (equal (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions nil nil other)))
                     '("fake/theirs")))
      (should (equal (sort (drover-anvil-test-keys (drover-anvil-test-json (drover-anvil--tool-sessions nil nil "here")))
                           #'string<)
                     '("fake/me" "fake/mine")))
      ;; Sending: to the caller's project, not to another.
      (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-send "fake/mine" "Go on"))) "sent"))
      (should (equal (car drover-test-calls) '(:submit "mine" "Go on")))
      (let ((err (should-error (drover-anvil--tool-send "fake/theirs" "Go on"))))
        (should (string-match-p "caller's project" (cadr err))))
      (setq drover-anvil-scratch-directories (list other))
      (should (drover-anvil--tool-send "fake/theirs" "Go on")))))

(ert-deftest drover-anvil-test-never-the-caller-nor-above ()
  "No tool acts on the caller nor on a session above it, nor waits for them."
  (drover-anvil-test-with (list (drover-test-session "top" :status 'working)
                                (drover-test-session "me" :status 'working :parent "top"))
      "me"
    (let ((drover-anvil-permissions '((send . any) (wait . any) (interrupt . any) (archive . any) (read . any))))
      (dolist (key '("fake/me" "fake/top"))
        (should-error (drover-anvil--tool-send key "hi"))
        (should-error (drover-anvil--tool-wait key))
        (should-error (drover-anvil--tool-interrupt key))
        (should-error (drover-anvil--tool-archive key)))
      (should-not (seq-find (lambda (call) (memq (car call) '(:submit :interrupt :archive))) drover-test-calls))
      ;; Reading oneself is fine.
      (should (equal (alist-get 'key (drover-anvil-test-json (drover-anvil--tool-session "fake/me" :json-false)))
                     "fake/me")))))

(ert-deftest drover-anvil-test-unknown-caller ()
  "A caller its client does not name may only do what applies to any session."
  (drover-anvil-test-with (list (drover-test-session "a" :status 'idle :directory default-directory)) nil
    (should (drover-anvil--tool-sessions))
    (should-error (drover-anvil--tool-send "fake/a" "hi"))
    (let ((drover-anvil-permissions '((send . any))))
      (should (drover-anvil--tool-send "fake/a" "hi")))
    (let ((drover-anvil-permissions '((list . nil))))
      (should-error (drover-anvil--tool-sessions)))))

(ert-deftest drover-anvil-test-blocker-refuses ()
  "A session whose buffer cannot take a prompt now is sent none, and the agent told why."
  (drover-anvil-test-with (list (drover-test-session "busy" :status 'working)) nil
    (let ((drover-anvil-permissions '((send . any)))
          (drover-anvil-test-blocker "a draft is typed"))
      (let ((err (should-error (drover-anvil--tool-send "fake/busy" "hi"))))
        (should (string-match-p "a draft is typed" (cadr err))))
      (should-not (assq :submit drover-test-calls))
      (setq drover-anvil-test-blocker nil)
      (should (drover-anvil--tool-send "fake/busy" "hi"))
      (should (assq :submit drover-test-calls)))))

(ert-deftest drover-anvil-test-actions-logged-or-refused ()
  "Every call is logged; an action that cannot be is not taken."
  (drover-anvil-test-with (list (drover-test-session "a")) nil
    (let ((drover-anvil-permissions '((send . any) (read . any))))
      (drover-anvil--tool-send "fake/a" "Hello there")
      (should (seq-find (lambda (line) (string-match-p "drover-session-send.*acting.*11 characters: Hello there" line))
                        (drover-anvil-test-log)))
      (should (seq-find (lambda (line) (string-match-p "drover-session-send.*\tdone\tsent" line))
                        (drover-anvil-test-log)))
      (setq drover-test-calls nil)
      (let ((drover-anvil-log-file (expand-file-name "a-file/anvil.log" drover-state-directory)))
        (write-region "" nil (expand-file-name "a-file" drover-state-directory))
        (should-error (drover-anvil--tool-send "fake/a" "Hello"))
        (should-not (assq :submit drover-test-calls))
        ;; Reading goes on.
        (should (drover-anvil--tool-session "fake/a" :json-false))))))

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
  "A job's answer goes to the session that asked for it alone."
  (drover-anvil-test-with (list (drover-test-session "a") (drover-test-session "b")) "a"
    (setq drover-test-recap-later t)
    (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-session "fake/b")))))
      (let ((anvil-server-request-meta '((fake.session . "b"))))
        (should-error (drover-anvil--tool-job job)))
      (should (drover-anvil--tool-job job)))))

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
    (let ((drover-anvil-permissions '((send . any) (wait . any))))
      (drover-anvil--tool-send "fake/a" "Do it")
      (let ((job (alist-get 'job (drover-anvil-test-json (drover-anvil--tool-wait "fake/a" 60 nil :json-false)))))
        (should job)
        (should (eq (alist-get 'pending (drover-anvil-test-json (drover-anvil--tool-job job))) t))
        ;; The turn ran and ended between two polls.
        (drover-anvil-test-set-status "a" 'done (+ (float-time) 1))
        (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-job job))) "ended"))))
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
  "A session starts unseen in the caller's project, takes its prompt, and is the caller's own."
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
          ;; Its own: interrupted, archived, its questions answered.
          (drover-anvil-test-set-status (cdr (drover-anvil--parse-key key)) 'working)
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
          ;; No permission, by default.
          (let ((err (should-error (drover-anvil--tool-answer key "p" "once"))))
            (should (string-match-p "answer-permission" (cadr err))))
          (let ((drover-anvil-permissions (cons '(answer-permission . own) drover-anvil-permissions)))
            (drover-anvil--tool-answer key "p" "always")
            (should (equal (nth 3 (assq :answer drover-test-calls)) '(:decision "always"))))
          (should (equal (alist-get 'outcome (drover-anvil-test-json (drover-anvil--tool-archive key)))
                         "archived"))
          (should (assq :archive drover-test-calls)))))))

(ert-deftest drover-anvil-test-create-refused-elsewhere ()
  "No session starts outside the caller's project, nor of a harness not allowed."
  (drover-anvil-test-with nil "me"
    (let ((project (drover-anvil-test-project))
          (other (drover-anvil-test-project "other")))
      (setq drover-test-sessions (list (drover-test-session "me" :directory project)))
      (drover-backend-changed 'fake)
      (should-error (drover-anvil--tool-create nil other "Start"))
      (should-error (drover-anvil--tool-create "opencode" nil "Start"))
      (should-error (drover-anvil--tool-create nil (expand-file-name "missing" project) "Start"))
      ;; A model its engine does not run.
      (drover-test-with-engines
        (drover-register-model-source 'fake (lambda (_directory)
                                              (list (drover-test-model "elsewhere" :engines '(other)))))
        (let ((err (should-error (drover-anvil--tool-create nil nil "Start" "elsewhere"))))
          (should (string-match-p "does not run" (cadr err)))))
      (should-not (assq :create drover-test-calls)))))

(ert-deftest drover-anvil-test-not-own-refused ()
  "By default, sessions the caller did not start are neither interrupted, archived nor answered."
  (drover-anvil-test-with (list (drover-test-session "me") (drover-test-session "a" :status 'working)) "me"
    (setq drover-anvil-test-requests '(("a" (:id "q" :kind question))))
    (should-error (drover-anvil--tool-interrupt "fake/a"))
    (should-error (drover-anvil--tool-archive "fake/a"))
    (should-error (drover-anvil--tool-answer "fake/a" nil nil '((x . "y"))))
    (should-not (seq-find (lambda (call) (memq (car call) '(:interrupt :archive :answer))) drover-test-calls))))

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
