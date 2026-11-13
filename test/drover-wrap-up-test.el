;;; drover-wrap-up-test.el --- Tests of wrapping sessions up  -*- lexical-binding: t; -*-

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

;; The session is the fake backend's, of a fake engine whose :changes
;; answers what a test says, in a buffer with a process: a terminal,
;; running its agent and archived by drover, or a service's session,
;; archived by its backend, which confirms it, and left out of :session
;; once archived.  Its turns are played by changing its record, its
;; prompt taken by a :submit recording it, through the queue of
;; scheduled prompts.  Nothing is shown.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-wrap-up)

(defvar drover-wrap-up-test-submitted nil
  "Prompts the fake backend took, newest first, as (ID TEXT THEN ELSE).")

(defvar drover-wrap-up-test-buffer nil
  "The buffer of the fixture's session, with a process.")

(defvar drover-wrap-up-test-directory nil
  "The directory of the fixture's session.")

(defvar drover-wrap-up-test-changes nil
  "What the fake engine's :changes answers.")

(defvar drover-wrap-up-test-archived nil
  "Non-nil while the service archived the fixture's session.")

(defvar drover-wrap-up-test-closed nil
  "Identifiers of the sessions whose buffers :close closed, newest first.")

(defvar drover-wrap-up-test-warnings nil
  "Warnings drover gave, newest first.")

(defvar drover-wrap-up-test-messages nil
  "Messages drover gave, newest first.")

(defun drover-wrap-up-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most."
  (let ((deadline (+ (float-time) (or seconds 10))) value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun drover-wrap-up-test-session (&rest properties)
  "Serve the fixture's session with PROPERTIES over its own, and return it."
  (let ((session (apply #'drover-test-session "s1"
                        (append properties (list :engine 'wrapfake :engine-id "conv-1"
                                                 :directory drover-wrap-up-test-directory
                                                 :created 1.0)))))
    (setq drover-test-sessions (list session))
    session))

(defun drover-wrap-up-test-record ()
  "Return the record of the fixture's session as served, archived or not."
  (car drover-test-sessions))

(defmacro drover-wrap-up-test-with (kind &rest body)
  "Run BODY over the fixture's session, of KIND, `terminal' or `service'.
A terminal runs its agent in its buffer, and drover archives it; a
service's session runs elsewhere, its backend archiving it and
confirming it.  The wrap-ups, the queue and the state are fresh."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (drover-test-with-engines
         (let* ((root (file-name-as-directory (file-truename (make-temp-file "drover-wrap-up-test" t))))
                (drover-wrap-up-test-directory (expand-file-name "project/" root))
                (drover-wrap-up--table (drover-wrap-up--table-make :loaded t))
                (drover-schedule--queue (drover-schedule--queue-make :loaded t))
                (drover-schedule--triggers nil)
                (drover-schedule-former-files nil)
                (drover-schedule-display-function #'ignore)
                (drover-schedule-changed-hook nil)
                (drover-wrap-up-attribution-timeout 2)
                (drover-wrap-up-removal-advice nil)
                (drover-wrap-up-agent 'build)
                (drover-wrap-up-test-submitted nil)
                (drover-wrap-up-test-changes '(:files nil))
                (drover-wrap-up-test-archived nil)
                (drover-wrap-up-test-closed nil)
                (drover-wrap-up-test-warnings nil)
                (drover-wrap-up-test-messages nil)
                (drover-wrap-up-test-buffer nil))
           (make-directory drover-wrap-up-test-directory t)
           (drover-test-register-engine 'wrapfake
                                        :directory (lambda (&rest _) drover-wrap-up-test-directory)
                                        :build-agent "build"
                                        :changes (lambda (_id _scope _directory callback)
                                                   (funcall callback drover-wrap-up-test-changes)))
           (setq drover-wrap-up-test-buffer (generate-new-buffer " *wrap-up session*"))
           (with-current-buffer drover-wrap-up-test-buffer
             (make-pipe-process :name "wrap-up-session" :buffer drover-wrap-up-test-buffer :noquery t)
             (setq-local drover-session-at-point-function (lambda () (drover-find-archived 'fake "s1"))))
           (let ((properties (alist-get 'fake drover--backends)))
             (setq properties
                   (append (list :buffer (lambda (_session) (and (buffer-live-p drover-wrap-up-test-buffer)
                                                                 drover-wrap-up-test-buffer))
                                 :blocker (lambda (_session) nil)
                                 :submit (lambda (session text then else)
                                           (push (list (drover-session-id session) text then else)
                                                 drover-wrap-up-test-submitted))
                                 :close (lambda (session)
                                          (push (drover-session-id session) drover-wrap-up-test-closed)
                                          (when (buffer-live-p drover-wrap-up-test-buffer)
                                            (let ((kill-buffer-query-functions nil))
                                              (kill-buffer drover-wrap-up-test-buffer)))
                                          nil))
                           properties))
             (if (eq ,kind 'terminal)
                 ;; Drover's own archive, as for claude-code-ide's terminals.
                 (setq properties (append (list :runs-in-buffer t)
                                          (drover-wrap-up-test-without properties :archive)))
               (setq properties
                     (append
                      (list :sessions (lambda () (unless drover-wrap-up-test-archived drover-test-sessions))
                            :session (lambda (id)
                                       (unless drover-wrap-up-test-archived
                                         (seq-find (lambda (session) (equal (drover-session-id session) id))
                                                   drover-test-sessions)))
                            :archived-session (lambda (id)
                                                (when-let* ((session (seq-find (lambda (session)
                                                                                 (equal (drover-session-id session) id))
                                                                               drover-test-sessions)))
                                                  (if drover-wrap-up-test-archived
                                                      (let ((copy (copy-sequence session)))
                                                        (setf (drover-session-archived copy) 1.0)
                                                        copy)
                                                    session)))
                            :archive (lambda (session then _else)
                                       (push (list :archive (drover-session-id session)) drover-test-calls)
                                       (setq drover-wrap-up-test-archived t)
                                       (funcall then))
                            :archive-confirms t
                            :unarchive (lambda (session then _else)
                                         (push (list :unarchive (drover-session-id session)) drover-test-calls)
                                         (setq drover-wrap-up-test-archived nil)
                                         (funcall then))
                            :unarchive-confirms t)
                      (drover-wrap-up-test-without properties :sessions :session :archive))))
             (setf (alist-get 'fake drover--backends) properties))
           (drover-wrap-up-test-session :status 'idle :since 1.0 :updated 1.0)
           (unwind-protect
               (cl-letf (((symbol-function 'drover-known-sessions) (lambda (&rest _) drover-test-sessions))
                         ((symbol-function 'drover-archived-live-sessions) #'ignore)
                         ((symbol-function 'drover-warn)
                          (lambda (format-string &rest arguments)
                            (push (apply #'format format-string arguments) drover-wrap-up-test-warnings)))
                         ((symbol-function 'drover-message)
                          (lambda (format-string &rest arguments)
                            (push (apply #'format format-string arguments) drover-wrap-up-test-messages))))
                 ,@body)
             (drover-test-wait-commands 10)
             (drover-wrap-up-unload-function)
             (when (drover-schedule--queue-p drover-schedule--queue)
               (maphash (lambda (_ timer) (cancel-timer timer)) (drover-schedule--queue-timers drover-schedule--queue))
               (maphash (lambda (_ run) (when-let* ((timer (plist-get run :timer))) (cancel-timer timer)))
                        (drover-schedule--queue-active drover-schedule--queue)))
             (when (buffer-live-p drover-wrap-up-test-buffer)
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer drover-wrap-up-test-buffer)))
             (delete-directory root t)))))))

(defun drover-wrap-up-test-without (plist &rest keys)
  "Return PLIST without KEYS and their values."
  (let ((result nil))
    (while plist
      (unless (memq (car plist) keys)
        (setq result (append result (list (car plist) (cadr plist)))))
      (setq plist (cddr plist)))
    result))

(defun drover-wrap-up-test-take ()
  "Wait for the wrap-up's prompt, take it as the front end does, and return its text."
  (should (drover-wrap-up-test-wait (lambda () drover-wrap-up-test-submitted)))
  (pcase-let ((`(,_id ,text ,then ,_else) (pop drover-wrap-up-test-submitted)))
    (funcall then)
    text))

(defun drover-wrap-up-test-entry ()
  "Return the wrap-up under way of the fixture's session, or nil."
  (drover-wrap-up--find "wrapfake/conv-1"))

(defun drover-wrap-up-test-state (state)
  "Wait for the fixture's wrap-up to reach STATE, and return whether it did."
  (drover-wrap-up-test-wait (lambda () (eq (plist-get (drover-wrap-up-test-entry) :state) state))))

(defun drover-wrap-up-test-turn (&optional status &rest properties)
  "Play the cleanup's turn of the fixture's session, ending in STATUS, idle by default.
PROPERTIES are those of its record as the turn ends."
  (let ((now (+ (float-time) 1)))
    (drover-wrap-up-test-session :status 'working :since now :updated now)
    (drover-wrap-up--changed)
    (apply #'drover-wrap-up-test-session :status (or status 'idle) :since (+ now 1) :updated (+ now 1)
           properties)
    (drover-wrap-up--changed)))

(defun drover-wrap-up-test-ended ()
  "Wait for the fixture's wrap-up to end, and return non-nil when it did."
  (drover-wrap-up-test-wait (lambda () (null (drover-wrap-up-test-entry)))))

;;;; Terminals

(ert-deftest drover-wrap-up-test-terminal-closes-once-cleaned ()
  "A terminal: prompt with the build agent, archived at once, its buffer closed once its turn ended."
  (drover-wrap-up-test-with 'terminal
    (should (drover-wrap-up-p (drover-wrap-up-test-record)))
    (drover-wrap-up (drover-wrap-up-test-record))
    (should (member "Wrapping up Session s1" drover-wrap-up-test-messages))
    (let ((text (drover-wrap-up-test-take)))
      (should (string-match-p "The user asked to close this session" text))
      (should (string-match-p "make DROVER-KEEP-OPEN alone the first line" text))
      (should-not (string-match-p "%[kra]" text)))
    ;; Its build agent first, as the queue applies options before sending.
    (should (equal (plist-get (nth 2 (assq :apply-options drover-test-calls)) :agent) "build"))
    (should (drover-wrap-up-test-state 'watching))
    (should (drover-session-archived-p (drover-find 'fake "s1")))
    (should-not (drover-wrap-up-p (drover-find 'fake "s1")))
    ;; The record tells no turn yet: the buffer stays, the turn about to run.
    (drover-wrap-up--changed)
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should (file-exists-p (drover-wrap-up-file)))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (equal drover-wrap-up-test-closed '("s1")))
    (should-not (buffer-live-p drover-wrap-up-test-buffer))
    (should (drover-session-archived-p (drover-test-session "s1" :engine 'wrapfake :engine-id "conv-1")))
    (should (member "Wrapped up Session s1" drover-wrap-up-test-messages))
    (should-not drover-wrap-up-test-warnings)
    ;; Nothing left to follow, nor kept.
    (should-not (file-exists-p (drover-wrap-up-file)))
    (should-not (memq #'drover-wrap-up--changed drover-changed-hook))))

(ert-deftest drover-wrap-up-test-keyword-keeps-open ()
  "The keyword opening the last message restores the session, its buffer kept, with why."
  (drover-wrap-up-test-with 'terminal
    (setq drover-test-recaps
          '(("s1" :prompt "close" :reply "**DROVER-KEEP-OPEN**\nThe branch fix-login is not merged: delete it?")))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should-not drover-wrap-up-test-closed)
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    (should (equal drover-wrap-up-test-warnings
                   '("Session s1 stays open: the agent asks: The branch fix-login is not merged: delete it?")))))

(ert-deftest drover-wrap-up-test-question-keeps-open-at-once ()
  "A question the agent waits on keeps the session open before its turn ends."
  (drover-wrap-up-test-with 'terminal
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up-test-session :status 'waiting :since (+ 1 (float-time)) :updated (+ 1 (float-time))
                                 :requests '((:kind question :summary "Remove the clone ~/src/mongo-2?")))
    (drover-wrap-up--changed)
    (should-not (drover-wrap-up-test-entry))
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    (should (equal drover-wrap-up-test-warnings
                   '("Session s1 stays open: it asks: Remove the clone ~/src/mongo-2?")))))

(ert-deftest drover-wrap-up-test-failed-turn-keeps-open ()
  "A cleanup failing keeps the session open, telling its error."
  (drover-wrap-up-test-with 'terminal
    (setq drover-test-recaps '(("s1" :reply "Removing…" :error "Request aborted")))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up-test-turn 'failed)
    (should (drover-wrap-up-test-ended))
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    (should (equal drover-wrap-up-test-warnings '("Session s1 stays open: its cleanup failed: Request aborted")))))

(ert-deftest drover-wrap-up-test-reply-read-again ()
  "A last reply that cannot be read is asked for again; one never read keeps the session open."
  (drover-wrap-up-test-with 'terminal
    (let ((asks 0)
          (failures 2)
          (drover-wrap-up--recap-delay 0.05))
      (cl-letf (((symbol-function 'drover-test--recap)
                 (lambda (_session callback)
                   (setq asks (1+ asks))
                   (funcall callback (if (<= asks failures)
                                         '(:failure "no connection to 127.0.0.1:28894 within 5 s")
                                       '(:reply "Removed ~/notes."))))))
        (drover-wrap-up (drover-wrap-up-test-record))
        (drover-wrap-up-test-take)
        (should (drover-wrap-up-test-state 'watching))
        (drover-wrap-up-test-turn)
        (should (drover-wrap-up-test-ended))
        (should (= asks 3))
        (should (member "Wrapped up Session s1" drover-wrap-up-test-messages))
        (should-not drover-wrap-up-test-warnings)
        ;; Never read, it stays open.
        (setq asks 0
              failures 99)
        (drover-unarchive-session (drover-find 'fake "s1"))
        (setq drover-wrap-up-test-buffer (generate-new-buffer " *wrap-up session*"))
        (with-current-buffer drover-wrap-up-test-buffer
          (make-pipe-process :name "wrap-up-session" :buffer drover-wrap-up-test-buffer :noquery t)
          (setq-local drover-session-at-point-function (lambda () (drover-find-archived 'fake "s1"))))
        (drover-wrap-up-test-session :status 'idle :since 1.0 :updated 1.0)
        (drover-wrap-up (drover-wrap-up-test-record))
        (drover-wrap-up-test-take)
        (should (drover-wrap-up-test-state 'watching))
        (drover-wrap-up-test-turn)
        (should (drover-wrap-up-test-ended))
        (should (= asks drover-wrap-up--recap-attempts))
        (should (buffer-live-p drover-wrap-up-test-buffer))
        (should (equal drover-wrap-up-test-warnings
                       '("Session s1 stays open: its last reply could not be read: no connection to 127.0.0.1:28894 within 5 s")))))))

(ert-deftest drover-wrap-up-test-question-at-the-end-keeps-open ()
  "A last message ending in a question keeps the session open, without the keyword."
  (drover-wrap-up-test-with 'terminal
    (setq drover-test-recaps '(("s1" :reply "Removed ~/notes.\nShould I also delete the branch wip?")))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should (equal drover-wrap-up-test-warnings
                   '("Session s1 stays open: the agent asks: Should I also delete the branch wip?")))))

(ert-deftest drover-wrap-up-test-waits-for-the-running-turn ()
  "A session at work gets its prompt once its turn ended, never while it runs."
  (drover-wrap-up-test-with 'terminal
    (drover-wrap-up-test-session :status 'working :since (float-time) :updated (float-time))
    (drover-wrap-up (drover-wrap-up-test-record))
    (should (drover-wrap-up-test-state 'waiting-turn))
    (accept-process-output nil 0.6)
    (should-not drover-wrap-up-test-submitted)
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    ;; Its turn ended before the prompt went.
    (drover-wrap-up-test-session :status 'done :since (float-time) :updated (float-time))
    (drover-wrap-up--changed)
    (should (string-match-p "close this session" (drover-wrap-up-test-take)))
    (should (drover-wrap-up-test-state 'watching))))

(ert-deftest drover-wrap-up-test-restored-by-the-user ()
  "A session restored while it cleans up is no longer wrapped up: its buffer stays."
  (drover-wrap-up-test-with 'terminal
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up--changed)
    (drover-archive-toggle (drover-find 'fake "s1"))
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    (drover-wrap-up-test-turn)
    (should-not (drover-wrap-up-test-entry))
    (should (buffer-live-p drover-wrap-up-test-buffer))
    (should (member "The wrap-up of Session s1 is called off: it was restored from the archive"
                    drover-wrap-up-test-messages))
    (should-not drover-wrap-up-test-warnings)))

(ert-deftest drover-wrap-up-test-refusals ()
  "Subagents, sessions waiting for the user and those under way already are refused."
  (drover-wrap-up-test-with 'terminal
    (drover-wrap-up-test-session :status 'waiting :requests '((:kind question :summary "Which?")))
    (should-not (drover-wrap-up-p (drover-wrap-up-test-record)))
    (should (string-match-p "answer it first"
                            (cadr (should-error (drover-wrap-up (drover-wrap-up-test-record)) :type 'user-error))))
    (drover-wrap-up-test-session :status 'idle :parent "p")
    (should-not (drover-wrap-up-p (drover-wrap-up-test-record)))
    (should (string-match-p "subagent"
                            (cadr (should-error (drover-wrap-up (drover-wrap-up-test-record)) :type 'user-error))))
    (drover-wrap-up-test-session :status 'idle :engine-id nil)
    (should-not (drover-wrap-up-p (drover-wrap-up-test-record)))
    (drover-wrap-up-test-session :status 'idle :since 1.0 :updated 1.0)
    (drover-wrap-up (drover-wrap-up-test-record))
    (should (string-match-p "being wrapped up already"
                            (cadr (should-error (drover-wrap-up (drover-wrap-up-test-record)) :type 'user-error))))))

(ert-deftest drover-wrap-up-test-tells-the-peer ()
  "A wrap-up another host asked for tells it how it ended, this host named."
  (drover-wrap-up-test-with 'terminal
    (let ((told nil)
          (drover-host "core"))
      (cl-letf (((symbol-function 'drover-remote-tell-peer)
                 (lambda (name text) (push (list name text) told))))
        (setq drover-test-recaps '(("s1" :reply "DROVER-KEEP-OPEN\nThe clone ~/src/x has local commits.")))
        (drover-wrap-up-session (drover-wrap-up-test-record) "laptop")
        (drover-wrap-up-test-take)
        (should (drover-wrap-up-test-state 'watching))
        (drover-wrap-up-test-turn)
        (should (drover-wrap-up-test-ended))
        (should (equal told '(("laptop" "Session s1 on core stays open: the agent asks: The clone ~/src/x has local commits."))))
        ;; Asked here, nobody is told.
        (setq told nil
              drover-test-recaps nil)
        (drover-wrap-up-test-session :status 'idle :since 1.0 :updated 1.0)
        (drover-wrap-up (drover-find 'fake "s1"))
        (drover-wrap-up-test-take)
        (should (drover-wrap-up-test-state 'watching))
        (drover-wrap-up-test-turn)
        (should (drover-wrap-up-test-ended))
        (should-not told)))))

;;;; Sessions running elsewhere

(ert-deftest drover-wrap-up-test-service-closes-at-once ()
  "A service's session: its buffer closes once the archive took, its cleanup followed while archived."
  (drover-wrap-up-test-with 'service
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (should (member '(:archive "s1") drover-test-calls))
    (should (equal drover-wrap-up-test-closed '("s1")))
    (should-not (buffer-live-p drover-wrap-up-test-buffer))
    ;; Left out of :session, archived, it is followed through :archived-session.
    (should-not (drover-find 'fake "s1"))
    (should (drover-session-archived-p (drover-find-archived 'fake "s1")))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should drover-wrap-up-test-archived)
    (should-not (member '(:unarchive "s1") drover-test-calls))
    (should (member "Wrapped up Session s1" drover-wrap-up-test-messages))))

(ert-deftest drover-wrap-up-test-service-kept-open ()
  "A service's session kept open is restored through its backend."
  (drover-wrap-up-test-with 'service
    (setq drover-test-recaps '(("s1" :reply "DROVER-KEEP-OPEN: ~/work/mongo has unpushed commits")))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (member '(:unarchive "s1") drover-test-calls))
    (should-not drover-wrap-up-test-archived)
    (should (equal drover-wrap-up-test-warnings
                   '("Session s1 stays open: the agent asks: ~/work/mongo has unpushed commits")))))

(ert-deftest drover-wrap-up-test-service-archive-shown-late ()
  "A session its backend shows archived a moment after confirming is not taken as restored."
  (drover-wrap-up-test-with 'service
    (let ((archive (plist-get (alist-get 'fake drover--backends) :archive)))
      ;; Confirmed before the record tells it.
      (setf (plist-get (alist-get 'fake drover--backends) :archive)
            (lambda (session then else)
              (funcall then)
              (run-at-time 0.3 nil (lambda () (funcall archive session #'ignore else))))))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up--changed)
    (should (drover-wrap-up-test-entry))
    (should (drover-wrap-up-test-wait (lambda () drover-wrap-up-test-archived)))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (member "Wrapped up Session s1" drover-wrap-up-test-messages))))

(ert-deftest drover-wrap-up-test-service-restored-as-the-turn-begins ()
  "A session another client restores as its cleanup begins: called off, and told.
`drover-wrap-up-under-way-p' tells such a client to leave it archived."
  (drover-wrap-up-test-with 'service
    (should-not (drover-wrap-up-under-way-p 'wrapfake "conv-1"))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up--changed)
    (should (drover-wrap-up-under-way-p 'wrapfake "conv-1"))
    (should-not (drover-wrap-up-under-way-p 'wrapfake "conv-2"))
    (should-not (drover-wrap-up-under-way-p 'other "conv-1"))
    ;; Restored by the service's other client, as the turn starts.
    (setq drover-wrap-up-test-archived nil)
    (drover-wrap-up-test-session :status 'working :since (+ (float-time) 1) :updated (+ (float-time) 1))
    (drover-wrap-up--changed)
    (should-not (drover-wrap-up-test-entry))
    (should-not (drover-wrap-up-under-way-p 'wrapfake "conv-1"))
    (should (member "The wrap-up of Session s1 is called off: it was restored from the archive"
                    drover-wrap-up-test-messages))
    (should-not (member "Wrapped up Session s1" drover-wrap-up-test-messages))
    (should-not drover-wrap-up-test-warnings)))

(ert-deftest drover-wrap-up-test-service-never-shown-archived ()
  "A session its backend never shows archived, past the grace, is called off with a warning."
  (drover-wrap-up-test-with 'service
    ;; Confirmed, never told.
    (setf (plist-get (alist-get 'fake drover--backends) :archive)
          (lambda (_session then _else) (funcall then)))
    (drover-wrap-up (drover-wrap-up-test-record))
    (drover-wrap-up-test-take)
    (should (drover-wrap-up-test-state 'watching))
    (drover-wrap-up--changed)
    (should (drover-wrap-up-test-entry))
    (setf (plist-get (drover-wrap-up-test-entry) :archived-at)
          (- (float-time) drover-wrap-up--archive-grace 1))
    (drover-wrap-up--changed)
    (should-not (drover-wrap-up-test-entry))
    (should (equal drover-wrap-up-test-warnings
                   '("The wrap-up of Session s1 is called off: it never showed archived")))))

;;;; Across Emacs sessions

(ert-deftest drover-wrap-up-test-restore ()
  "As drover starts: a wrap-up not begun is given up, a terminal's cut short, a service's followed."
  (drover-wrap-up-test-with 'service
    (let ((common (list :backend 'fake :front-end 'fake :directory drover-wrap-up-test-directory
                        :hint nil :mark 2.0 :active nil :archived t :archived-at 2.0 :job nil :peer nil
                        :started 1.0 :lost nil)))
      (drover-write-data (drover-wrap-up-file) "Sessions being wrapped up" drover-wrap-up--version :wrap-ups
                         (list (append (list :id "wrapfake/early" :engine 'wrapfake :engine-id "early" :session "e"
                                             :title "Early" :state 'preparing :runs-in-buffer nil)
                                       common)
                               (append (list :id "wrapfake/term" :engine 'wrapfake :engine-id "term" :session "t"
                                             :title "Terminal" :state 'watching :runs-in-buffer t)
                                       common)
                               (append (list :id "wrapfake/conv-1" :engine 'wrapfake :engine-id "conv-1"
                                             :session "s1" :title "Session s1" :state 'judging
                                             :runs-in-buffer nil)
                                       common)
                               '(:id "junk"))))
    (setq drover-wrap-up--table nil
          drover-wrap-up-test-archived t)
    (drover-wrap-up-restore)
    (should (equal (mapcar (lambda (entry) (plist-get entry :id)) (drover-wrap-up--entries)) '("wrapfake/conv-1")))
    (should (eq (plist-get (drover-wrap-up-test-entry) :state) 'watching))
    (should (member "The wrap-up of Early had not begun as Emacs stopped: Z begins it again"
                    drover-wrap-up-test-messages))
    (should (seq-find (lambda (warning) (string-prefix-p "The wrap-up of Terminal was cut short" warning))
                      drover-wrap-up-test-warnings))
    (should (memq #'drover-wrap-up--changed drover-changed-hook))
    (drover-wrap-up-test-turn)
    (should (drover-wrap-up-test-ended))
    (should (member "Wrapped up Session s1" drover-wrap-up-test-messages))))

;;;; Several at once

(ert-deftest drover-wrap-up-test-several ()
  "A selection is wrapped up as a job, each session on its own."
  (drover-wrap-up-test-with 'terminal
    (setq drover-test-sessions
          (list (drover-wrap-up-test-record)
                (drover-test-session "s2" :engine 'wrapfake :engine-id "conv-2" :status 'idle
                                     :directory drover-wrap-up-test-directory :created 1.0)
                (drover-test-session "s3" :engine 'wrapfake :engine-id "conv-3" :status 'waiting
                                     :directory drover-wrap-up-test-directory :created 1.0)))
    (let ((drover-selected-sessions-function (lambda () drover-test-sessions)))
      (call-interactively #'drover-wrap-up))
    (should (drover-test-wait-actions))
    (should (equal (sort (mapcar (lambda (entry) (plist-get entry :id)) (drover-wrap-up--entries)) #'string<)
                   '("wrapfake/conv-1" "wrapfake/conv-2")))))

;;;; The prompt

(ert-deftest drover-wrap-up-test-prompt-text ()
  "The prompt takes the keyword, the removal advice and the hint, each a paragraph when given."
  (let ((drover-wrap-up-keyword "KEEP")
        (drover-wrap-up-removal-advice nil)
        (drover-wrap-up-prompt "Clean up.%r\n\nSay %k, 100%%.%a"))
    (should (equal (drover-wrap-up-prompt-text "") "Clean up.\n\nSay KEEP, 100%."))
    (let ((drover-wrap-up-removal-advice "Use agent-cleanup.\n"))
      (should (equal (drover-wrap-up-prompt-text "Hint:\n- a")
                     "Clean up.\n\nUse agent-cleanup.\n\nSay KEEP, 100%.\n\nHint:\n- a"))))
  (should (string-match-p "temporary folder your harness made" (drover-wrap-up-prompt-text nil))))

(ert-deftest drover-wrap-up-test-keyword-line ()
  "The keyword counts alone at a line's start, markup around it, what follows telling why."
  (let ((drover-wrap-up-keyword "DROVER-KEEP-OPEN"))
    (should (equal (drover-wrap-up--keyword-line "DROVER-KEEP-OPEN\nThe branch is not merged.")
                   "The branch is not merged."))
    (should (equal (drover-wrap-up--keyword-line "Done.\n**DROVER-KEEP-OPEN**: unpushed work") "unpushed work"))
    (should (equal (drover-wrap-up--keyword-line "`DROVER-KEEP-OPEN`") "DROVER-KEEP-OPEN"))
    (should (equal (drover-wrap-up--keyword-line "> DROVER-KEEP-OPEN — two clones") "— two clones"))
    (should-not (drover-wrap-up--keyword-line "I did not need DROVER-KEEP-OPEN."))
    (should-not (drover-wrap-up--keyword-line "- DROVER-KEEP-OPEN was not needed"))
    (should-not (drover-wrap-up--keyword-line "DROVER-KEEP-OPENED"))
    (should-not (drover-wrap-up--keyword-line "drover-keep-open"))
    (should-not (drover-wrap-up--keyword-line nil))))

;;;; What drover attributes to a session

(defun drover-wrap-up-test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY; return its output, or fail."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory)))
      (unless (zerop (apply #'call-process "git" nil t nil arguments))
        (error "git %s: %s" (string-join arguments " ") (buffer-string)))
      (string-trim (buffer-string)))))

(ert-deftest drover-wrap-up-test-attribution ()
  "The hint: worktrees added since the start, directories of created files alone, other files."
  (drover-wrap-up-test-with 'terminal
    (let* ((home (file-name-as-directory (file-truename (make-temp-file "drover-wrap-up-home" t))))
           ;; The home is a temporary directory: the system's are the fixture's own.
           (process-environment (append (list (concat "HOME=" home) "GIT_CONFIG_NOSYSTEM=1"
                                              (concat "TEMP=" home "tmp") (concat "TMP=" home "tmp")
                                              (concat "TMPDIR=" home "tmp")
                                              "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=test@example.com"
                                              "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=test@example.com")
                                        process-environment))
           (abbreviated-home-dir nil)
           ;; The fixture's home is under the system's own temporary
           ;; directories, such as macOS's /var/folders/: they are not its.
           (drover-wrap-up--system-temporary-directories nil)
           (repo (expand-file-name "repo/" home))
           (temporary-file-directory (file-name-as-directory (expand-file-name "tmp" home)))
           (files (list "notes/a.md" "notes/deep/b.md" "scratch/c.txt" "repo/in-repo.txt"
                        "tmp/scratch.txt" "gone.txt")))
      (unwind-protect
          (progn
            (make-directory repo t)
            (make-directory temporary-file-directory t)
            (drover-wrap-up-test-git repo "init" "-q" "-b" "main")
            (drover-test-write (expand-file-name "README" repo) "readme\n")
            (drover-wrap-up-test-git repo "add" "-A")
            (drover-wrap-up-test-git repo "commit" "-q" "-m" "First")
            (drover-wrap-up-test-git repo "worktree" "add" "-q" "-b" "stray" (expand-file-name "wt-stray" home))
            (drover-wrap-up-test-git repo "worktree" "add" "-q" "-b" "kept" (expand-file-name "wt-locked" home))
            (drover-wrap-up-test-git repo "worktree" "lock" (expand-file-name "wt-locked" home))
            (drover-wrap-up-test-git repo "worktree" "add" "-q" "-b" "task" (expand-file-name "wt-task" home))
            (drover-task-git-write-marker (expand-file-name "wt-task" home) "task-id" 'ok)
            (dolist (file (butlast files))
              (drover-test-write (expand-file-name file home) "x\n"))
            ;; Not created by the session: the scratch directory is not its own.
            (drover-test-write (expand-file-name "scratch/other.txt" home) "y\n")
            (setq drover-wrap-up-test-changes
                  (list :files (append (mapcar (lambda (file)
                                                 (list :file (expand-file-name file home) :status 'added))
                                               files)
                                       (list (list :file (expand-file-name "notes/edited.md" home)
                                                   :status 'modified)))))
            (let ((session (drover-wrap-up-test-session :status 'idle :directory repo :created 1.0))
                  (hint nil))
              (drover-wrap-up--attribute session (lambda (text) (setq hint text)))
              (should (drover-wrap-up-test-wait (lambda () hint)))
              (should (equal hint
                             (concat "What drover attributes to this session, a hint rather than a list to delete:\n"
                                     "- worktrees added to its repository since it started:\n"
                                     "  - ~/wt-stray/ (branch stray)\n"
                                     "- directories holding only files its edits created:\n"
                                     "  - ~/notes/\n"
                                     "- files its edits created outside any repository:\n"
                                     "  - ~/scratch/c.txt"))))
            ;; Worktrees added before the session started are not its own.
            (let ((session (drover-wrap-up-test-session :status 'idle :directory repo
                                                        :created (+ (float-time) 3600)))
                  (hint nil))
              (setq drover-wrap-up-test-changes '(:files nil))
              (drover-wrap-up--attribute session (lambda (text) (setq hint text)))
              (should (drover-wrap-up-test-wait (lambda () hint)))
              (should (equal hint ""))))
        (delete-directory home t)))))

;;;; The archive's toggle

(ert-deftest drover-wrap-up-test-archive-toggle ()
  "z archives a session, restores an archived one; of several, it archives unless all are archived."
  (drover-wrap-up-test-with 'terminal
    (setq drover-test-sessions
          (list (drover-wrap-up-test-record)
                (drover-test-session "s2" :engine 'wrapfake :engine-id "conv-2" :status 'idle)))
    (drover-archive-toggle (drover-find 'fake "s1"))
    (should (drover-session-archived-p (drover-find 'fake "s1")))
    (drover-archive-toggle (drover-find 'fake "s1"))
    (should-not (drover-session-archived-p (drover-find 'fake "s1")))
    (drover-archive (drover-find 'fake "s1"))
    (let ((drover-selected-sessions-function (lambda () (list (drover-find 'fake "s1") (drover-find 'fake "s2")))))
      (call-interactively #'drover-archive-toggle)
      (should (drover-test-wait-actions))
      (should (drover-session-archived-p (drover-find 'fake "s1")))
      (should (drover-session-archived-p (drover-find 'fake "s2")))
      (call-interactively #'drover-archive-toggle)
      (should (drover-test-wait-actions))
      (should-not (drover-session-archived-p (drover-find 'fake "s1")))
      (should-not (drover-session-archived-p (drover-find 'fake "s2"))))))

(provide 'drover-wrap-up-test)
;;; drover-wrap-up-test.el ends here
