;;; drover-handoff-test.el --- Tests for hand-offs  -*- lexical-binding: t; -*-

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

;; `drover-handoff' on the fake backend, its prompt taken by a :submit
;; recording it, through the queue of scheduled prompts: the item from
;; the region or the minibuffer, a fork asked, a draft in the session's
;; prompt, a running turn, a session of another host.  Nothing is
;; shown.  The tools its prompt names are tested in
;; drover-anvil-task-test.el.

;;; Code:

(require 'ert)
(require 'drover-test-helpers)
;; Its fixture: a session of another host.
(require 'drover-anvil-test)
(require 'drover-handoff)
(require 'drover-keys)

;;;; The command

(defvar drover-handoff-test-submitted nil
  "Prompts the fake backend took, newest first, as (ID . TEXT).")

(defvar drover-handoff-test-blocker nil
  "What the fake :blocker answers, nil to take prompts.")

(defvar drover-handoff-test-buffer nil
  "The buffer of the fixture's session, with a process.")

(defvar drover-handoff-test-messages nil
  "Messages drover gave, newest first.")

(defvar drover-handoff-test-warnings nil
  "Warnings drover gave, newest first.")

(defun drover-handoff-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most."
  (let ((deadline (+ (float-time) (or seconds 10))) value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    value))

(defun drover-handoff-test-session (&rest properties)
  "Serve the fixture's session with PROPERTIES over its own, and return it."
  (let ((session (apply #'drover-test-session "s1"
                        (append properties (list :engine 'handfake :engine-id "conv-1"
                                                 :directory temporary-file-directory)))))
    (setq drover-test-sessions (list session))
    (drover-backend-changed 'fake)
    session))

(defun drover-handoff-test-add (&rest properties)
  "Give the fake backend PROPERTIES over its own."
  (setf (alist-get 'fake drover--backends)
        (append properties (alist-get 'fake drover--backends))))

(defmacro drover-handoff-test-with (&rest body)
  "Run BODY over the fixture's session, idle, its buffer taking prompts.
The queue of scheduled prompts and the state are fresh; what the fake
:submit takes goes to `drover-handoff-test-submitted'."
  (declare (indent 0) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (drover-test-with-engines
         (let ((drover-schedule--queue (drover-schedule--queue-make :loaded t))
               (drover-schedule--triggers nil)
               (drover-schedule-former-files nil)
               (drover-schedule-display-function #'ignore)
               (drover-schedule-changed-hook (list #'drover-handoff--schedule-changed))
               (drover-handoff-history nil)
               (drover-handoff-test-submitted nil)
               (drover-handoff-test-blocker nil)
               (drover-handoff-test-messages nil)
               (drover-handoff-test-warnings nil)
               (drover-handoff-test-buffer (generate-new-buffer " *hand-off session*")))
           (drover-test-register-engine 'handfake :directory (lambda (&rest _) temporary-file-directory))
           (with-current-buffer drover-handoff-test-buffer
             (make-pipe-process :name "hand-off-session" :buffer drover-handoff-test-buffer :noquery t)
             (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s1"))))
           (drover-handoff-test-add
            :buffer (lambda (session)
                      (and (equal (drover-session-id session) "s1")
                           (buffer-live-p drover-handoff-test-buffer)
                           drover-handoff-test-buffer))
            :blocker (lambda (_session) drover-handoff-test-blocker)
            :submit (lambda (session text then _else)
                      (push (cons (drover-session-id session) text) drover-handoff-test-submitted)
                      (funcall then)))
           (drover-handoff-test-session :status 'idle)
           (unwind-protect
               (cl-letf (((symbol-function 'drover-known-sessions) (lambda (&rest _) drover-test-sessions))
                         ((symbol-function 'drover-archived-live-sessions) #'ignore)
                         ((symbol-function 'drover-warn)
                          (lambda (format-string &rest arguments)
                            (push (apply #'format format-string arguments) drover-handoff-test-warnings)))
                         ((symbol-function 'drover-message)
                          (lambda (format-string &rest arguments)
                            (push (apply #'format format-string arguments) drover-handoff-test-messages))))
                 ,@body)
             (maphash (lambda (_ timer) (cancel-timer timer)) (drover-schedule--queue-timers drover-schedule--queue))
             (maphash (lambda (_ run) (when-let* ((timer (plist-get run :timer))) (cancel-timer timer)))
                      (drover-schedule--queue-active drover-schedule--queue))
             (when (buffer-live-p drover-handoff-test-buffer)
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer drover-handoff-test-buffer)))))))))

(defun drover-handoff-test-take ()
  "Wait for the fixture's session to take a prompt, and return its text."
  (should (drover-handoff-test-wait (lambda () drover-handoff-test-submitted)))
  (cdr (pop drover-handoff-test-submitted)))

(defun drover-handoff-test-jobs ()
  "Return the prompts of hand-offs the queue holds."
  (seq-filter (lambda (job) (equal (plist-get job :origin) "hand-off")) (drover-schedule--jobs)))

(ert-deftest drover-handoff-test-prompt ()
  "The prompt tells the item, the mode, and what the agent does for it."
  (let ((text (drover-handoff-prompt-text "The parser drops 100% of\nthe trailing commas" nil)))
    (should (string-match-p "\\`Hand this item of our work off to a fresh session:\n\nThe parser drops 100% of\nthe trailing commas\n" text))
    (should-not (string-match-p "%[im]" text))
    (dolist (told '("`handoff` skill" "File an Org heading" "a brief that reads alone"
                    "the goal, what is known, the decisions made, the limits, when it is done, and links"
                    "`drover-task-start`" "`drover-session-fork`" "A refused fork ends the hand-off"
                    "without asking for a review" "the heading's link" "the new session's link"))
      (should (string-search told text))))
  (should (string-prefix-p "Hand this item of our work off to a fork of this session:\n\nItem\n"
                           (drover-handoff-prompt-text "Item" t)))
  (let ((drover-handoff-prompt "%m <- %i (100%%)"))
    (should (equal (drover-handoff-prompt-text "it" t) "a fork of this session <- it (100%)"))))

(ert-deftest drover-handoff-test-region ()
  "In the session's buffer, the region is the item: it goes at once, out of every window."
  (drover-handoff-test-with
    (save-window-excursion
      (let ((windows (window-list)))
        (with-current-buffer drover-handoff-test-buffer
          (insert "Before.  The cache key ignores the host.  After.")
          (let ((transient-mark-mode t))
            (goto-char (point-min))
            (search-forward "The cache")
            (set-mark (match-beginning 0))
            (search-forward "host.")
            (activate-mark)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "Read in the minibuffer"))))
              (call-interactively #'drover-handoff))
            ;; Read, the region ends.
            (should-not (region-active-p))))
        (let ((job (car (drover-handoff-test-jobs))))
          (should (eq (plist-get job :hidden) t))
          (should (equal (plist-get job :link) "agent-session:handfake/conv-1")))
        (let ((text (drover-handoff-test-take)))
          (should (string-match-p "off to a fresh session:\n\nThe cache key ignores the host\\.\n" text))
          (should-not (string-search "Before" text)))
        (should (equal (window-list) windows))
        (should-not (get-buffer-window drover-handoff-test-buffer t))))
    (should (equal (car (last drover-handoff-test-messages))
                   "Session s1 hands off to a fresh session: The cache key ignores the host."))
    ;; Gone, its prompt leaves the queue.
    (should (drover-handoff-test-wait (lambda () (null (drover-handoff-test-jobs)))))
    (should-not drover-handoff-test-warnings)
    ;; The session keeps its buffer: the user goes on reading it.
    (should-not drover--handed-off)))

(ert-deftest drover-handoff-test-minibuffer ()
  "Without a region, and in a list, where a region selects sessions, the item is read."
  (drover-handoff-test-with
    (let ((asked nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _) (push prompt asked) "  The flaky test  ")))
        (with-current-buffer drover-handoff-test-buffer
          (call-interactively #'drover-handoff))
        (should (equal asked '("Hand off from Session s1, to a fresh session: ")))
        (should (string-match-p "session:\n\nThe flaky test\n" (drover-handoff-test-take)))
        ;; A list: its region selects sessions, and tells no item.
        (with-temp-buffer
          (setq-local drover-view-p t)
          (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s1")))
          (insert "A row of the list")
          (let ((transient-mark-mode t))
            (set-mark (point-min))
            (activate-mark)
            (call-interactively #'drover-handoff))
          (should (length= asked 2))
          (should (string-match-p "session:\n\nThe flaky test\n" (drover-handoff-test-take)))
          ;; From the menu, beside: the region of the buffer it was called from.
          (let ((drover-dispatch-region "Asked from the notes"))
            (call-interactively #'drover-handoff))
          (should (length= asked 2))
          (should (string-match-p "session:\n\nAsked from the notes\n" (drover-handoff-test-take)))
          ;; One session at a time.
          (setq-local drover-selected-sessions-function
                      (lambda () (list (drover-find 'fake "s1") (drover-test-session "other"))))
          (should (string-match-p "one session: 2 are selected"
                                  (cadr (should-error (call-interactively #'drover-handoff) :type 'user-error))))))
      ;; Nothing to hand off.
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
        (with-current-buffer drover-handoff-test-buffer
          (should-error (call-interactively #'drover-handoff) :type 'user-error)))
      (should-not drover-handoff-test-submitted)
      (should-not (drover-handoff-test-jobs)))))

(ert-deftest drover-handoff-test-menu-region ()
  "The menu acting on a list beside runs the command there, the region of this buffer kept."
  (save-window-excursion
    (delete-other-windows)
    (let ((notes (generate-new-buffer " *notes*"))
          (list (generate-new-buffer " *list*")))
      (unwind-protect
          (let ((list-window (split-window)))
            (set-window-buffer list-window list)
            (with-current-buffer list
              (setq-local drover-view-p t))
            (set-window-buffer (selected-window) notes)
            (with-current-buffer notes
              (insert "Keep this.  Not that.")
              (let ((transient-mark-mode t))
                (set-mark (point-min))
                (goto-char (+ (point-min) 10))
                (activate-mark)
                (cl-letf (((symbol-function 'drover--dispatch-window) (lambda () list-window)))
                  (should (equal (drover--dispatch-handoff (lambda () (list (current-buffer) drover-dispatch-region)))
                                 (list list "Keep this."))))
                (should-not (region-active-p))
                ;; Acting here, the command reads the region itself.
                (set-mark (point-min))
                (activate-mark)
                (cl-letf (((symbol-function 'drover--dispatch-window) #'ignore))
                  (should (equal (drover--dispatch-handoff (lambda () (list (current-buffer) drover-dispatch-region)))
                                 (list notes nil))))
                (should (region-active-p)))))
        (kill-buffer notes)
        (kill-buffer list)))))

(ert-deftest drover-handoff-test-fork ()
  "A prefix argument asks for a fork; a harness that cannot fork stops it, named."
  (drover-handoff-test-with
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "The retry loop")))
      (with-current-buffer drover-handoff-test-buffer
        (let* ((current-prefix-arg '(4))
               (err (should-error (call-interactively #'drover-handoff) :type 'user-error)))
          (should (equal (cadr err) "Session s1 cannot be forked: its harness, Fake, forks no session"))
          ;; Nothing went: never a fresh session in its place.
          (should-not (drover-handoff-test-jobs))
          (drover-handoff-test-add :fork-hidden #'ignore)
          (call-interactively #'drover-handoff))))
    (should (string-prefix-p "Hand this item of our work off to a fork of this session:\n\nThe retry loop\n"
                             (drover-handoff-test-take)))
    (should (equal (car (last drover-handoff-test-messages))
                   "Session s1 hands off to a fork of this session: The retry loop"))))

(ert-deftest drover-handoff-test-draft ()
  "A draft in the session's prompt is never written over: the prompt waits for it to go."
  (drover-handoff-test-with
    (setq drover-handoff-test-blocker "the input holds a draft")
    (drover-handoff (drover-find 'fake "s1") "The slow query")
    (should (drover-handoff-test-wait
             (lambda () (eq (plist-get (car (drover-handoff-test-jobs)) :status) 'waiting))))
    (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 1.2))
    (setq drover-handoff-test-blocker nil)
    (should (string-match-p "The slow query" (drover-handoff-test-take)))))

(ert-deftest drover-handoff-test-running-turn ()
  "A session in a turn gets the prompt once the turn ended."
  (drover-handoff-test-with
    (drover-handoff-test-session :status 'working)
    (drover-handoff (drover-find 'fake "s1") "The leak in the pool")
    (let ((job (car (drover-handoff-test-jobs))))
      (should (eq (plist-get job :turn) t))
      (should-not (plist-get job :due))
      (should (eq (plist-get job :hidden) t)))
    (should (equal (car drover-handoff-test-messages)
                   "Session s1 hands off to a fresh session, once its turn ends: The leak in the pool"))
    (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 0.8))
    ;; A question it asks is part of its turn.
    (drover-handoff-test-session :status 'waiting)
    (drover-schedule--turn-check)
    (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 0.8))
    (drover-handoff-test-session :status 'idle)
    (drover-schedule--turn-check)
    (should (string-match-p "The leak in the pool" (drover-handoff-test-take)))
    (should (drover-handoff-test-wait (lambda () (null (drover-handoff-test-jobs)))))))

(ert-deftest drover-handoff-test-refused-sessions ()
  "A session taking no prompt here is refused, and a failed delivery stays in the queue."
  (drover-handoff-test-with
    (setq drover-test-sessions (list (drover-test-session "bare")))
    (drover-backend-changed 'fake)
    ;; A backend taking no prompt from drover.
    (drover-test-register-simple 'mute (list (drover-session-create :backend 'mute :id "m")))
    (should-not (drover-handoff-p (drover-find 'mute "m")))
    (should-not (drover-handoff-p nil))
    (should (drover-handoff-p (drover-find 'fake "bare")))
    ;; Its conversation is not known: the queue names none.
    (should-error (drover-handoff (drover-find 'fake "bare") "An item") :type 'user-error)
    (should-not (drover-handoff-test-jobs))
    ;; A prompt its front end refuses stays, for the list of scheduled prompts.
    (drover-handoff-test-session :status 'idle)
    (drover-handoff-test-add :submit (lambda (_session _text _then else) (funcall else "refused" t)))
    (drover-handoff (drover-find 'fake "s1") "An item")
    (should (drover-handoff-test-wait
             (lambda () (eq (plist-get (car (drover-handoff-test-jobs)) :status) 'failed))))
    (should (length= (drover-handoff-test-jobs) 1))))

(ert-deftest drover-handoff-test-other-host ()
  "A session of another host gets the prompt through its host: after its turn, or as a prompt sent."
  (drover-handoff-test-with
    (let ((scheduled nil)
          (submitted nil)
          (there nil))
      (drover-test-register-simple
       'remote:core (lambda () there) :remote "core"
       :submit (lambda (session text then else) (push (list (drover-session-id session) text then else) submitted)))
      (setq there (list (drover-anvil-test-peer-session "ses_1" :status 'working)))
      (drover-backend-changed 'remote:core)
      (cl-letf (((symbol-function 'drover-remote-schedule-later)
                 (lambda (session when text argument by callback)
                   (push (list (drover-session-id session) when text argument by callback) scheduled))))
        (drover-handoff (drover-index-session '(remote:core . (opencode . "ses_1"))) "The stale lock" t)
        (pcase-let ((`(,id ,when ,text ,argument ,by ,callback) (car scheduled)))
          (should (equal id '(opencode . "ses_1")))
          (should (equal when "when turn ends"))
          ;; Whether it forks is its host's to tell.
          (should (string-prefix-p "Hand this item of our work off to a fork of this session:\n\nThe stale lock\n" text))
          (should-not argument)
          (should (equal by "hand-off"))
          (funcall callback (list :failure "it takes no prompt from another host")))
        (should (string-match-p "Could not hand off from Core ses_1: .* refused its prompt: it takes no prompt"
                                (car drover-handoff-test-warnings)))
        (should-not submitted)
        ;; Idle, its host takes it as a prompt sent, queued there behind a draft.
        (setq there (list (drover-anvil-test-peer-session "ses_1" :status 'idle)))
        (drover-backend-changed 'remote:core)
        (drover-handoff (drover-index-session '(remote:core . (opencode . "ses_1"))) "The stale lock")
        (should (length= scheduled 1))
        (should (string-match-p "off to a fresh session:\n\nThe stale lock\n" (nth 1 (car submitted))))
        ;; Nothing of it in this host's queue.
        (should-not (drover-handoff-test-jobs))))))

(provide 'drover-handoff-test)
;;; drover-handoff-test.el ends here
