;;; drover-notify.el --- Desktop notifications of the sessions needing you  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; `drover-notify-mode' tells on the desktop of this host's sessions as
;; they come to need you: waiting on a request, their turn failed, or
;; ended unseen.  Only the sessions you chose to be notified of do (see
;; `drover-toggle-notify', `!' on a session), none by default: the count
;; of the mode line and the list tell of every one already.
;;
;; It follows `drover-changed-hook', as the count and the pushes to
;; phones do, so that the sessions of every front end are told alike,
;; and the front ends' own notifications can be turned off.  A session
;; is told once it stood so `drover-notify-delay' seconds, unless you
;; look at it then, and again when it comes to need you otherwise, as
;; another request; its notification goes once it no longer needs you,
;; seen or answered.  Choosing to be notified of a session needing you
;; tells nothing of what you just saw: only what comes after.  Sessions
;; found needing you as the mode starts, and those a backend reports
;; late, as Emacs starts, needing you since before, are not told either.
;;
;; `drover-notify-function' shows a notification: by default with the
;; notifications of the operating system, as opencode.el shows its own.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 30))
  (load "drover-core" nil t))

(declare-function opencode-notify-os-native "opencode-notify" (notification))

(defcustom drover-notify-function #'drover-notify-native
  "Function showing a notification of a session needing you.
It is called with a plist:

 :key      the session's key, (BACKEND . ID)
 :id       a string naming the session's notifications
 :session  the session's record
 :status   its status: `waiting', `failed' or `done'
 :kind     for a session waiting, the kind of its oldest request,
           `permission' or `question'; else nil
 :title    the session's title, on a line
 :body     why it needs you, on a line
 :engine   the engine running it, a symbol, or nil
 :buffer   the buffer you answer it in, or nil
 :form     a form showing that session, printable, for a click to run

It returns a function of no argument removing the notification, which
drover calls once the session no longer needs you; any other value when
it cannot be removed."
  :type '(choice (function-item drover-notify-native) function)
  :group 'drover)

(defcustom drover-notify-delay 1
  "Seconds a session stands in need of you before it is told.
A request answered at once, as by a rule of its front end, or a status
passing through tells nothing."
  :type 'number
  :group 'drover)

(defvar drover-notify-looking-function #'drover-notify-looking-p
  "Function of a session record, non-nil when you look at that session.
A session you look at is not told: you see it already.")

;;;; State

(cl-defstruct (drover-notify--state (:constructor drover-notify--state-make)
                                    (:copier nil))
  "What the notifications of sessions keep."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-notify--state)
         :documentation "The layouts of the types it was made of.")
  (started nil :documentation "When the mode started, a float time.")
  (needs (make-hash-table :test #'equal)
         :documentation "This host's sessions needing you, told or not: signatures by key.")
  (shown (make-hash-table :test #'equal)
         :documentation "What was told, (SIGNATURE . REMOVE) by key; REMOVE nil when untold.")
  (pending (make-hash-table :test #'equal)
           :documentation "What is to be told, (SIGNATURE . DUE) by key.")
  (timer nil :documentation "Timer telling what is due."))

(drover-note-layout 'drover-notify--state)

(defvar drover-notify--state nil
  "What the notifications of sessions keep while `drover-notify-mode' is on.")

(defun drover-notify--current ()
  "Return the state of notifications, or nil; one of another version is dropped."
  (when drover-notify--state
    (if (drover-layout-current-p drover-notify--state 'drover-notify--state)
        drover-notify--state
      (setq drover-notify--state nil)
      (drover-stale-layout "the state of notifications")
      nil)))

;;;; What needs you

(defun drover-notify--needing (key)
  "Return the record of this host's session KEY when it needs you, else nil.
A session of this host's own backends, the one listed of its
conversation, whose status needs you, and at the top of its tree or
waiting itself, as the phones are pushed (see `drover-remote-push')."
  (when-let* ((record (drover-index-session key))
              ((drover-own-session-p record))
              ((drover-index-listed-p key record))
              ((memq (drover-session-status record) drover-attention-statuses))
              ((or (eq (drover-session-status record) 'waiting)
                   (null (drover-index-parent key)))))
    record))

(defun drover-notify-looking-p (session)
  "Return non-nil when you look at SESSION.
That is when the selected window of a frame with focus shows it, or the
session you answer it from (see `drover-attention-target')."
  (let ((target (drover-attention-target session)))
    (seq-some (lambda (frame)
                (and (eq (frame-focus-state frame) t)
                     (let ((window (frame-selected-window frame)))
                       (or (drover-window-shows-session-p window session)
                           (and (not (eq target session))
                                (drover-window-shows-session-p window target))))))
              (frame-list))))

(defun drover-notify--new-p (state record)
  "Return non-nil when RECORD came to need you once STATE's mode ran.
Not when it needed you before, as a session a backend reports late as
Emacs starts, or one whose status tells no start."
  (when-let* ((since (drover-session-since record)))
    (>= since (drover-notify--state-started state))))

;;;; Following sessions

(defun drover-notify--remove (state key)
  "Remove what STATE told of session KEY, if anything."
  (when-let* ((entry (gethash key (drover-notify--state-shown state))))
    (remhash key (drover-notify--state-shown state))
    (when (functionp (cdr entry))
      (drover-with-reporting "notification"
        (funcall (cdr entry))))))

(defun drover-notify--consider (state key)
  "Bring what STATE tells of session KEY up to date with the session.
A session needing you otherwise than STATE noted it before is told
once due, if you chose so; otherwise, what it was told goes."
  (let* ((needs (drover-notify--state-needs state))
         (shown (drover-notify--state-shown state))
         (pending (drover-notify--state-pending state))
         (record (drover-notify--needing key))
         (signature (and record (drover-session-need-signature record)))
         (before (gethash key needs)))
    (if signature
        (puthash key signature needs)
      (remhash key needs))
    (cond
     ((or (null signature) (not (drover-notify-p record)))
      (remhash key pending)
      (drover-notify--remove state key))
     ((equal signature (car (gethash key shown))))
     ((equal signature (car (gethash key pending))))
     ;; As it stood: it was noted needing you so, before you chose to be
     ;; notified of it, or before the mode ran.
     ((or (equal signature before)
          (and (null before) (not (drover-notify--new-p state record))))
      (remhash key pending)
      (drover-notify--remove state key)
      (puthash key (list signature) shown))
     (t
      (drover-notify--remove state key)
      (puthash key (cons signature (+ (float-time) drover-notify-delay)) pending)))))

(defun drover-notify--follow (state keys)
  "Bring what STATE tells up to date with the sessions KEYS, t for all."
  (when (eq keys t)
    (setq keys (delete-dups (append (hash-table-keys (drover-notify--state-needs state))
                                    (hash-table-keys (drover-notify--state-shown state))
                                    (hash-table-keys (drover-notify--state-pending state))
                                    (hash-table-keys (drover--index-records (drover-index)))))))
  (dolist (key keys)
    (drover-notify--consider state key))
  (drover-notify--schedule state))

(defun drover-notify--changed ()
  "Follow the changes of this host's sessions, as `drover-changed-sessions' tells.
For `drover-changed-hook'."
  (drover-with-reporting "notifications"
    (when-let* ((state (drover-notify--current)))
      (drover-notify--follow state drover-changed-sessions))))

;;;; Telling

(defun drover-notify--schedule (state)
  "Arm STATE's timer for the next notification due, if any."
  (let ((due nil))
    (maphash (lambda (_key entry) (when (or (null due) (< (cdr entry) due)) (setq due (cdr entry))))
             (drover-notify--state-pending state))
    (when (drover-notify--state-timer state)
      (cancel-timer (drover-notify--state-timer state))
      (setf (drover-notify--state-timer state) nil))
    (when due
      (setf (drover-notify--state-timer state)
            (run-at-time (max 0 (- due (float-time))) nil
                         (let ((link (drover-link state)))
                           (lambda ()
                             (drover-with-reporting "notifications"
                               (let ((state (drover-linked link)))
                                 (setf (drover-notify--state-timer state) nil)
                                 (when (eq state (drover-notify--current))
                                   (drover-notify--deliver state)))))))))))

(defun drover-notify--deliver (state)
  "Tell what STATE has due: the sessions still needing you as noted, unseen."
  (let ((now (float-time))
        (pending (drover-notify--state-pending state))
        (due nil))
    (maphash (lambda (key entry) (when (<= (cdr entry) (+ now 0.05)) (push (cons key (car entry)) due)))
             pending)
    (pcase-dolist (`(,key . ,signature) due)
      (remhash key pending)
      (when-let* ((record (drover-notify--needing key))
                  ((equal (drover-session-need-signature record) signature))
                  ((drover-notify-p record)))
        (drover-notify--remove state key)
        (puthash key (cons signature (unless (funcall drover-notify-looking-function record)
                                       (drover-notify--show record)))
                 (drover-notify--state-shown state))))
    (drover-notify--schedule state)))

(defun drover-notify--body (record)
  "Return why RECORD needs you, on a line."
  (let ((request (car (drover-session-requests record)))
        (why (or (drover-session-brief record) (drover-session-detail record))))
    (drover-bound-line
     (pcase (drover-session-status record)
       ('waiting (if (and request (plist-get request :summary)
                          (not (string-empty-p (plist-get request :summary))))
                     (concat (if (eq (plist-get request :kind) 'question) "Asks: " "Permission: ")
                             (plist-get request :summary))
                   "Waits for you"))
       ('failed (if why (concat "Failed: " why) "Failed"))
       (_ (or why "Turn ended")))
     240)))

(defun drover-notify-notification (record)
  "Return the notification of RECORD, as `drover-notify-function' takes it."
  (let* ((key (drover-session-key record))
         (status (drover-session-status record))
         (request (car (drover-session-requests record))))
    (list :key key
          :id (format "drover:%s:%s" (car key) (cdr key))
          :session record
          :status status
          :kind (and (eq status 'waiting) request
                     (if (eq (plist-get request :kind) 'question) 'question 'permission))
          :title (drover-bound-line (format "%s" (drover-session-display-title record)) 120)
          :body (drover-notify--body record)
          :engine (drover-session-engine record)
          :buffer (drover-session-buffer (drover-attention-target record))
          :form `(drover-notify-visit ',key))))

(defun drover-notify--show (record)
  "Tell of RECORD through `drover-notify-function'; return its remover, or nil."
  (let ((remove (drover-with-reporting "notification"
                  (funcall drover-notify-function (drover-notify-notification record)))))
    (and (functionp remove) remove)))

(defun drover-notify-visit (key)
  "Show session KEY, as clicking its notification does.
It is the session you answer it from: a subagent waiting is answered
from the top of its tree (see `drover-attention-target')."
  (drover-open (drover-attention-target (or (drover-index-session key)
                                            (user-error "That session is gone")))))

(defun drover-notify-native (notification)
  "Show NOTIFICATION with the notifications of the operating system.
Through opencode.el's (see `opencode-notify-os-native'): D-Bus, else
terminal-notifier or osascript on macOS, tray notifications on Windows;
in the echo area without them."
  (if (require 'opencode-notify nil t)
      (opencode-notify-os-native
       `((title . ,(plist-get notification :title))
         (message . ,(plist-get notification :body))
         (variant . ,(pcase (plist-get notification :status)
                       ('failed "error") ('waiting "warning") (_ "success")))
         (kind . ,(plist-get notification :status))
         (session . ,(plist-get notification :id))))
    (message "%s: %s" (plist-get notification :title) (plist-get notification :body))
    nil))

;;;; The mode

;;;###autoload
(define-minor-mode drover-notify-mode
  "Tell on the desktop of this host's sessions as they come to need you.
A session waiting on a request, whose turn failed or ended unseen is
told through `drover-notify-function' once it stood so
`drover-notify-delay' seconds, unless you look at it, and its
notification goes once it no longer needs you.  Only the sessions you
chose to be notified of are told, a subagent with the top of its tree
\(see `drover-toggle-notify'), none by default; and of those, only
what comes to need you after the choice and the mode, not what needed
you before.  Enabling the mode starts the backends; disabling it
removes what it told."
  :global t
  :group 'drover
  (when-let* ((state (drover-notify--current)))
    (when (drover-notify--state-timer state)
      (cancel-timer (drover-notify--state-timer state)))
    (dolist (key (hash-table-keys (drover-notify--state-shown state)))
      (drover-notify--remove state key)))
  (setq drover-notify--state nil)
  (remove-hook 'drover-changed-hook #'drover-notify--changed)
  (when drover-notify-mode
    (let ((state (drover-notify--state-make :started (float-time))))
      (setq drover-notify--state state)
      (add-hook 'drover-changed-hook #'drover-notify--changed)
      (drover-start)
      (drover-notify--follow state t))))

(defun drover-notify-unload-function ()
  "Disable `drover-notify-mode' before unloading."
  (drover-notify-mode -1)
  nil)

;; Reloaded with the mode on, follow anew: what was told stays.
(when (bound-and-true-p drover-notify-mode)
  (drover-with-reporting "notifications"
    (unless (drover-notify--current)
      (drover-notify-mode 1))))

(provide 'drover-notify)
;;; drover-notify.el ends here
