;;; drover-notify-test.el --- Tests of desktop notifications  -*- lexical-binding: t; -*-

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

;; The choice of the sessions notifying the user, and the desktop's
;; notifications of them: told once due, of the sessions chosen alone,
;; not while looked at, not for what needed the user before, removed as
;; the session no longer needs the user.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-notify)

(defvar drover-notify-test--told nil
  "Notifications shown, newest first, as plists.")

(defvar drover-notify-test--removed nil
  "The identifiers of the notifications removed, newest first.")

(defmacro drover-notify-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS and `drover-notify-mode' on.
Notifications are recorded rather than shown; `looking' is the session
the user looks at, its identifier, or nil."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
                             (drover-test-with-state
                              (let ((drover-notify-test--told nil)
                                    (drover-notify-test--removed nil)
                                    (looking nil)
                                    (drover-notify-delay 0)
                                    (drover-notify--state nil)
                                    (drover-notify-mode nil))
                                (ignore looking)
                                (let ((drover-notify-function
                                       (lambda (notification)
                                         (push notification drover-notify-test--told)
                                         (let ((id (plist-get notification :id)))
                                           (lambda () (push id drover-notify-test--removed)))))
                                      (drover-notify-looking-function
                                       (lambda (session) (equal (drover-session-id session) looking))))
                                  (unwind-protect
                                      (progn
                                        (drover-index)
                                        (drover-notify-mode 1)
                                        ,@body)
                                    (drover-notify-mode -1)))))))

(defun drover-notify-test--settle ()
  "Run what changed, then the timers due."
  (drover-changed-now)
  (let ((end (+ (float-time) 0.05)))
    (while (< (float-time) end)
      (accept-process-output nil 0.01))))

(defun drover-notify-test--change (sessions)
  "Make SESSIONS the fake backend's, and follow them."
  (setq drover-test-sessions sessions)
  (drover-backend-changed 'fake)
  (drover-notify-test--settle))

(defun drover-notify-test--told-ids ()
  "Return the identifiers of the sessions told, oldest first."
  (mapcar (lambda (notification) (cdr (plist-get notification :key))) (reverse drover-notify-test--told)))

(defun drover-notify-test--on (id)
  "Turn notifications on for session ID, and follow."
  (drover-set-notify (drover-index-session (cons 'fake id)) t)
  (drover-notify-test--settle))

(ert-deftest drover-notify-test-choice ()
  "None notifies by default; the choice is the tree's top's, kept for its conversation.
Records tell it; it is read again from its file."
  (drover-test-with-backend (list (drover-test-session "root" :engine 'fake :engine-id "e-root")
                                  (drover-test-session "sub" :parent "root"))
                            (drover-test-with-state
                             (drover-index)
                             (should-not (drover-notify-p (drover-index-session '(fake . "root"))))
                             (should-not (drover-notify-p (drover-index-session '(fake . "sub"))))
                             ;; Chosen from the subagent, for the top of its tree.
                             (let ((inhibit-message t))
                               (drover-toggle-notify (drover-index-session '(fake . "sub"))))
                             (should (drover-notify-p (drover-index-session '(fake . "root"))))
                             (should (drover-notify-p (drover-index-session '(fake . "sub"))))
                             (should-not (drover-notify-chosen-p (drover-index-session '(fake . "sub"))))
                             ;; The records take it in, those of the tree changed for views.
                             (let ((drover-changed-hook (list (lambda () (should (member '(fake . "sub") drover-changed-sessions))))))
                               (drover-changed-now))
                             (should (drover-session-notify (drover-index-session '(fake . "root"))))
                             (should-not (drover-session-notify (drover-index-session '(fake . "sub"))))
                             ;; Kept by conversation, read again.
                             (should (equal (mapcar #'car (drover-read-data (drover-notify-file) 1 :sessions)) '((fake . "e-root"))))
                             (setq drover--notify-on nil)
                             (should (drover-notify-p (drover-index-session '(fake . "sub"))))
                             ;; Off again.
                             (drover-set-notify (drover-index-session '(fake . "root")) nil)
                             (should-not (drover-notify-p (drover-index-session '(fake . "sub"))))
                             (should-not (drover-read-data (drover-notify-file) 1 :sessions)))))

(ert-deftest drover-notify-test-unreadable-file ()
  "A file of notifications that does not read is told once, and none notifies."
  (drover-test-with-backend (list (drover-test-session "a"))
                            (drover-test-with-state
                             (make-directory drover-state-directory t)
                             (write-region "(:version 9 :sessions nil)" nil (drover-notify-file) nil 'silent)
                             (let ((warned nil))
                               (cl-letf (((symbol-function 'drover-warn) (lambda (&rest args) (push args warned))))
                                 (drover-index)
                                 (should-not (drover-notify-p (drover-index-session '(fake . "a"))))
                                 (should-not (drover-notify-p (drover-index-session '(fake . "a")))))
                               (should (length= warned 1))))))

(ert-deftest drover-notify-test-told-when-chosen ()
  "A session chosen is told once it comes to need the user, the others never.
Its notification goes once it no longer does."
  (drover-notify-test-with (list (drover-test-session "a" :status 'working)
                                 (drover-test-session "b" :status 'working))
    (drover-notify-test--on "a")
    (should-not drover-notify-test--told)
    (drover-notify-test--change
     (list (drover-test-session "a" :status 'done :since (float-time) :brief "Fixed the redirect")
           (drover-test-session "b" :status 'done :since (float-time))))
    (should (equal (drover-notify-test--told-ids) '("a")))
    (let ((told (car drover-notify-test--told)))
      (should (eq (plist-get told :status) 'done))
      (should (equal (plist-get told :title) "Session a"))
      (should (equal (plist-get told :body) "Fixed the redirect"))
      (should (equal (plist-get told :id) "drover:fake:a"))
      (should (equal (plist-get told :form) '(drover-notify-visit '(fake . "a")))))
    ;; Nothing more while it stands so.
    (drover-notify-test--change drover-test-sessions)
    (should (length= drover-notify-test--told 1))
    ;; Seen: removed.
    (drover-notify-test--change (list (drover-test-session "a" :status 'idle)
                                      (drover-test-session "b" :status 'done :since (float-time))))
    (should (equal drover-notify-test--removed '("drover:fake:a")))))

(ert-deftest drover-notify-test-requests ()
  "A request is told as such; another status replaces its notification."
  (drover-notify-test-with (list (drover-test-session "a" :status 'working))
    (drover-notify-test--on "a")
    (drover-notify-test--change
     (list (drover-test-session "a" :status 'waiting :since (float-time)
                                :requests '((:kind permission :summary "rm -rf build")))))
    (let ((told (car drover-notify-test--told)))
      (should (eq (plist-get told :kind) 'permission))
      (should (equal (plist-get told :body) "Permission: rm -rf build")))
    ;; Another request: told again, the first removed.
    (drover-notify-test--change
     (list (drover-test-session "a" :status 'waiting :since (float-time)
                                :requests '((:kind question :summary "Which branch?")))))
    (should (length= drover-notify-test--told 2))
    (should (equal drover-notify-test--removed '("drover:fake:a")))
    (should (equal (plist-get (car drover-notify-test--told) :body) "Asks: Which branch?"))
    ;; Failed.
    (drover-notify-test--change
     (list (drover-test-session "a" :status 'failed :since (float-time) :detail "Rate limited")))
    (should (equal (plist-get (car drover-notify-test--told) :body) "Failed: Rate limited"))))

(ert-deftest drover-notify-test-subagent ()
  "A subagent waiting is told with the top of its tree's choice, answered from there."
  (drover-notify-test-with (list (drover-test-session "root" :status 'working)
                                 (drover-test-session "sub" :status 'working :parent "root"))
    (drover-notify-test--on "root")
    (drover-notify-test--change
     (list (drover-test-session "root" :status 'working)
           (drover-test-session "sub" :status 'waiting :parent "root" :since (float-time)
                                :requests '((:kind question :summary "Go on?")))))
    (should (equal (drover-notify-test--told-ids) '("sub")))
    ;; Visited, it opens the top of its tree.
    (let ((drover-test-calls nil))
      (eval (plist-get (car drover-notify-test--told) :form) t)
      (should (equal (assq :open drover-test-calls) '(:open "root" nil))))
    ;; A subagent whose turn ended is not told: its parent is.
    (drover-notify-test--change
     (list (drover-test-session "root" :status 'working)
           (drover-test-session "sub" :status 'done :parent "root" :since (float-time))))
    (should (equal (drover-notify-test--told-ids) '("sub")))
    (should (equal drover-notify-test--removed '("drover:fake:sub")))))

(ert-deftest drover-notify-test-what-stood-before ()
  "What needed the user before is not told: before the choice, the mode, or a late report."
  (drover-notify-test-with (list (drover-test-session "a" :status 'done :since 100.0)
                                 (drover-test-session "b" :status 'working))
    ;; Chosen while it needs the user: what the user saw is not told.
    (drover-notify-test--on "a")
    (should-not drover-notify-test--told)
    ;; What comes after is.
    (drover-notify-test--change
     (list (drover-test-session "a" :status 'waiting :since (float-time)
                                :requests '((:kind question :summary "Next?")))
           (drover-test-session "b" :status 'working)))
    (should (equal (drover-notify-test--told-ids) '("a")))
    ;; Reported late, needing the user since before the mode: not told.
    (drover-notify-test--on "b")
    (drover-notify-test--change
     (append drover-test-sessions (list (drover-test-session "c" :status 'done :since 100.0))))
    (drover-notify-test--on "c")
    (should (equal (drover-notify-test--told-ids) '("a")))))

(ert-deftest drover-notify-test-looking ()
  "A session the user looks at is not told."
  (drover-notify-test-with (list (drover-test-session "a" :status 'working))
    (drover-notify-test--on "a")
    (setq looking "a")
    (drover-notify-test--change (list (drover-test-session "a" :status 'done :since (float-time))))
    (should-not drover-notify-test--told)
    ;; Nor told later for what was seen.
    (setq looking nil)
    (drover-notify-test--change drover-test-sessions)
    (should-not drover-notify-test--told)))

(ert-deftest drover-notify-test-passing-through ()
  "A status passing through before its delay tells nothing."
  (drover-notify-test-with (list (drover-test-session "a" :status 'working))
    (drover-notify-test--on "a")
    (let ((drover-notify-delay 0.3))
      (drover-notify-test--change
       (list (drover-test-session "a" :status 'waiting :since (float-time)
                                  :requests '((:kind permission :summary "ls")))))
      (drover-notify-test--change (list (drover-test-session "a" :status 'working)))
      (sleep-for 0.4)
      (drover-notify-test--settle)
      (should-not drover-notify-test--told))))

(ert-deftest drover-notify-test-off ()
  "Turned off, a session's notification goes; the mode off removes them all."
  (drover-notify-test-with (list (drover-test-session "a" :status 'working)
                                 (drover-test-session "b" :status 'working))
    (drover-notify-test--on "a")
    (drover-notify-test--on "b")
    (drover-notify-test--change (list (drover-test-session "a" :status 'done :since (float-time))
                                      (drover-test-session "b" :status 'done :since (float-time))))
    (should (equal (sort (drover-notify-test--told-ids) #'string<) '("a" "b")))
    (drover-set-notify (drover-index-session '(fake . "a")) nil)
    (drover-notify-test--settle)
    (should (equal drover-notify-test--removed '("drover:fake:a")))
    (drover-notify-mode -1)
    (should (equal drover-notify-test--removed '("drover:fake:b" "drover:fake:a")))))

(ert-deftest drover-notify-test-other-hosts ()
  "The sessions of another host are told by their owner, not here."
  (drover-notify-test-with nil
    (drover-test-register-simple 'peer (list (drover-session-create :backend 'peer :id "x" :host "core"
                                                                    :status 'working :notify t))
                                 :remote t)
    (drover-notify-test--change nil)
    (drover-test-register-simple 'peer (list (drover-session-create :backend 'peer :id "x" :host "core"
                                                                    :status 'done :since (float-time)
                                                                    :notify t))
                                 :remote t)
    (drover-backend-changed 'peer)
    (drover-notify-test--settle)
    (should (drover-notify-p (drover-index-session '(peer . "x"))))
    (should-not drover-notify-test--told)))

(provide 'drover-notify-test)
;;; drover-notify-test.el ends here
