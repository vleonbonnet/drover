;;; drover-remote-push.el --- Push what needs the user to a phone  -*- lexical-binding: t; -*-

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

;; A phone, a peer owning no sessions, is connected while its
;; application is in front.  Otherwise this host pushes it what needs
;; the user, through UnifiedPush: the phone hands each host an endpoint
;; (an ntfy server's topic) and a key, over drover's channel; the host
;; posts the sessions of its own that need the user, encrypted with that
;; key, whenever that set changes: sessions coming to need the user, and
;; leaving it, seen or answered here, so that the phone's notifications
;; follow.  It holds what is new while the user types in this Emacs, and
;; pushes nothing while the phone is in front, connected.
;;
;; A body is `dp1.KID.' and the base64url of a nonce, the AES-256-GCM
;; ciphertext of the payload's JSON and its tag, the nonce the first four
;; bytes of the SHA-256 of this host's name and KID, then a counter never
;; repeated under a key, reserved ahead on disk.  Nothing of the
;; conversations reaches the push server in clear.  It goes through a
;; curl process, the token and the body on its standard input, never on
;; a command line: `url-retrieve' connects synchronously on Windows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'drover-core)
(require 'drover-remote)

(declare-function auth-source-pass-get "auth-source-pass" (key entry))

(defcustom drover-remote-push-endpoints nil
  "The push servers this host posts to, URLs such as \"https://ntfy.example/\".
A phone's endpoint is taken only when its scheme, host and port are
one's, its path a topic of letters, digits, dashes and underscores,
its query \"up=1\" or none: otherwise a peer could have this host post
its token anywhere.  Nil, the default, pushes nothing."
  :type '(repeat string)
  :group 'drover-remote)

(defcustom drover-remote-push-token nil
  "The token this host posts to push servers with, as a Bearer token.
Never written in the configuration: (pass NAME), the secret of NAME in
the password store, read through `auth-source-pass'; or a function
returning it.  Nil posts without a token."
  :type '(choice (const :tag "None" nil)
                 (list :tag "From pass" (const pass) string)
                 (function :tag "Function"))
  :group 'drover-remote)

(defcustom drover-remote-push-hold 120
  "Seconds of input in this Emacs during which pushes of what is new wait.
While the user typed here within that much, a session coming to need
them is held, and pushed once this Emacs has been idle that long, if it
still needs them; sessions leaving need, and watched ones, go at once.
A peer's :push-hold, or what a phone asks, takes precedence; nil or 0
holds nothing."
  :type '(choice (const :tag "Never hold" nil) number)
  :group 'drover-remote)

(defcustom drover-remote-push-delay 2
  "Seconds changes gather before a push: a push carries the state they leave."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-push-curl "curl"
  "The curl program posting pushes."
  :type 'string
  :group 'drover-remote)

(defvar drover-remote-push-wait 10
  "Seconds at most a change waits for its push while changes keep coming.")

(defvar drover-remote-push-spacing 5
  "Seconds at least between two pushes to one phone.")

(defconst drover-remote-push-limit 4096
  "Bytes at most of a push's body, the push server's limit of a message.")

(defconst drover-remote-push-registrations 8
  "Phones at most this host pushes to.")

(defconst drover-remote-push-reserve 64
  "Pushes whose counters are reserved on disk ahead of their sending.")

(defconst drover-remote-push-backoff '(10 30 60 300)
  "Seconds a phone's pushes wait after a push server refused or failed, in turn.")

(defvar drover-remote-push-gather 1.5
  "Seconds a push waits for what a notification shows: requests and excerpts.")

(defconst drover-remote-push-watches 16
  "Sessions at most a phone watches at once.")

(defconst drover-remote-push-watch-ttl (* 6 3600)
  "Seconds at most a watch lasts.")

(defconst drover-remote-push--version 1
  "Version of the format of the file of the phones pushed to.")

(defvar drover-remote-push-idle-function #'current-idle-time
  "Function returning how long this Emacs has been idle, a Lisp time or nil.")

(defvar drover-remote-push-send-function #'drover-remote-push--curl
  "Function posting a push: called with URL, TOKEN and BODY, then CALLBACK.
CALLBACK takes the HTTP status, an integer, or nil and a line telling
why it was not posted.")

;;;; State

(cl-defstruct (drover-remote-push--reg (:constructor drover-remote-push--reg-make)
                                       (:copier nil))
  "A phone this host pushes to."
  (peer nil :documentation "Its name, a peer's.")
  (url nil :documentation "Its endpoint.")
  (key nil :documentation "Its key, 32 bytes, a unibyte string.")
  (kid nil :documentation "The key's identifier, which bodies name.")
  (detail 'full :documentation "What a push tells: `full', `titles' or `counts'.")
  (hold nil :documentation "Seconds what is new is held, as the phone asked, or nil.")
  (seq 0 :documentation "The last counter of a nonce used.")
  (reserved 0 :documentation "The counter reserved on disk: none at or below it is used again.")
  (shown nil :documentation "What the phone was last told, a hash table of signatures by key.")
  (watch nil :documentation "Sessions watched, a hash table of (UNTIL . STATUS) by key.")
  (fg nil :documentation "Non-nil while the phone is in front, connected.")
  (created nil :documentation "When it registered.")
  (ok nil :documentation "When a push to it was last taken.")
  (error nil :documentation "The last failure, a line, or nil.")
  (failures 0 :documentation "Failures in a row.")
  (backoff nil :documentation "Until when its pushes wait after a failure, or nil.")
  (dead nil :documentation "Non-nil once its endpoint is gone, until it registers again.")
  (sending nil :documentation "Non-nil while a push to it is being posted.")
  (dirty nil :documentation "When a change it may not have been told of came, or nil."))

(cl-defstruct (drover-remote-push--state (:constructor drover-remote-push--state-make)
                                         (:copier nil))
  "What the pushes keep."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-remote-push--state 'drover-remote-push--reg)
         :documentation "The layouts of the types it was made of.")
  (regs (make-hash-table :test #'equal) :documentation "Phones by their peer's name.")
  (needs nil :documentation "This host's sessions needing the user, a hash table of signatures by key.")
  (timer nil :documentation "Timer of the next evaluation.")
  (idle nil :documentation "Idle timer evaluating once the user stopped typing.")
  (gathered (make-hash-table :test #'equal) :documentation "What notifications show, by (KEY . SIGNATURE).")
  (unreadable nil :documentation "Non-nil when the file did not read: it is never written over.")
  (token nil :documentation "The token read, or nil."))

(drover-note-layout 'drover-remote-push--state 'drover-remote-push--reg)

(defvar drover-remote-push--state nil
  "What the pushes keep while the remote layer runs.")

(defvar drover-remote-push--available 'unknown
  "Whether pushes can be made here, crypto and curl, once looked at.")

(defun drover-remote-push--current ()
  "Return the state of pushes, or nil; a state of another version is dropped."
  (when drover-remote-push--state
    (if (drover-layout-current-p drover-remote-push--state 'drover-remote-push--state)
        drover-remote-push--state
      (setq drover-remote-push--state nil)
      (drover-stale-layout "the state of pushes")
      nil)))

(defun drover-remote-push-available-p ()
  "Return non-nil when this host can push: AES-256-GCM through gnutls, and curl.
Gnutls's cryptographic functions crash Emacs on Windows unless
`gnutls-available-p' ran first in that process: it always runs first."
  (when (eq drover-remote-push--available 'unknown)
    (setq drover-remote-push--available
          (and (fboundp 'gnutls-available-p)
               (memq 'AEAD-ciphers (ignore-errors (gnutls-available-p)))
               (fboundp 'gnutls-symmetric-encrypt)
               (assq 'AES-256-GCM (ignore-errors (gnutls-ciphers)))
               (executable-find drover-remote-push-curl)
               t)))
  drover-remote-push--available)

(defun drover-remote-push--file ()
  "Return the file of the phones pushed to."
  (expand-file-name "push.eld" drover-state-directory))

;;;; Persistence

(defun drover-remote-push--entry (reg)
  "Return REG as `push.eld' keeps it, a plist."
  (list :peer (drover-remote-push--reg-peer reg)
        :url (drover-remote-push--reg-url reg)
        :key (base64url-encode-string (drover-remote-push--reg-key reg) t)
        :kid (drover-remote-push--reg-kid reg)
        :detail (drover-remote-push--reg-detail reg)
        :hold (drover-remote-push--reg-hold reg)
        :reserved (drover-remote-push--reg-reserved reg)
        :shown (let (pairs)
                 (maphash (lambda (key signature) (push (cons key signature) pairs))
                          (drover-remote-push--reg-shown reg))
                 pairs)
        :watch (let (pairs)
                 (maphash (lambda (key value) (push (cons key value) pairs))
                          (drover-remote-push--reg-watch reg))
                 pairs)
        :created (drover-remote-push--reg-created reg)
        :ok (drover-remote-push--reg-ok reg)
        :dead (drover-remote-push--reg-dead reg)))

(defun drover-remote-push--key-p (value)
  "Return non-nil when VALUE is a key of a session, (BACKEND . ID)."
  (and (consp value) (symbolp (car value))
       (or (stringp (cdr value)) (integerp (cdr value)))))

(defun drover-remote-push--from-entry (entry)
  "Return the registration ENTRY, read from `push.eld', checked, or nil."
  (when-let* (((plistp entry))
              (peer (plist-get entry :peer))
              ((stringp peer))
              (url (plist-get entry :url))
              ((stringp url))
              (kid (plist-get entry :kid))
              ((drover-remote-push--kid-p kid))
              (key (drover-remote-push--decode-key (plist-get entry :key)))
              (reserved (plist-get entry :reserved))
              ((natnump reserved)))
    (let ((shown (make-hash-table :test #'equal))
          (watch (make-hash-table :test #'equal)))
      (dolist (pair (seq-take (plist-get entry :shown) 10000))
        (when (and (consp pair) (drover-remote-push--key-p (car pair)) (stringp (cdr pair)))
          (puthash (car pair) (cdr pair) shown)))
      (dolist (pair (seq-take (plist-get entry :watch) drover-remote-push-watches))
        (when (and (consp pair) (drover-remote-push--key-p (car pair))
                   (consp (cdr pair)) (numberp (cadr pair)))
          (puthash (car pair) (cdr pair) watch)))
      (drover-remote-push--reg-make
       :peer peer :url url :key key :kid kid
       :detail (if (memq (plist-get entry :detail) '(full titles counts)) (plist-get entry :detail) 'full)
       :hold (and (numberp (plist-get entry :hold)) (plist-get entry :hold))
       ;; Counters up to the reserve may have been used before Emacs went.
       :seq reserved :reserved reserved
       :shown shown :watch watch
       :created (and (numberp (plist-get entry :created)) (plist-get entry :created))
       :ok (and (numberp (plist-get entry :ok)) (plist-get entry :ok))
       :dead (and (plist-get entry :dead) t)))))

(defun drover-remote-push--load (state)
  "Read the phones of `push.eld' into STATE.
A file that does not read is told once and never written over."
  (condition-case err
      (dolist (entry (drover-read-data (drover-remote-push--file) drover-remote-push--version :registrations))
        (when-let* ((reg (drover-remote-push--from-entry entry)))
          (when (< (hash-table-count (drover-remote-push--state-regs state)) drover-remote-push-registrations)
            (puthash (drover-remote-push--reg-peer reg) reg (drover-remote-push--state-regs state)))))
    (error
     (setf (drover-remote-push--state-unreadable state) t)
     (drover-warn "The phones pushed to, %s, do not read (%s): they are left as they are"
                  (drover-remote-push--file) (drover-error-message err)))))

(defun drover-remote-push--save (state)
  "Write STATE's phones to `push.eld', readable by its owner alone."
  (unless (drover-remote-push--state-unreadable state)
    (let ((file (drover-remote-push--file)))
      (condition-case err
          (progn
            (drover-write-data file "The phones pushed to" drover-remote-push--version :registrations
                               (mapcar #'drover-remote-push--entry
                                       (hash-table-values (drover-remote-push--state-regs state))))
            ;; It holds the keys: on Windows the profile's ACL protects it.
            (ignore-errors (set-file-modes file #o600)))
        (error (when (drover--report-due-p (list "push save"))
                 (drover-warn "Could not write %s: %s" file (drover-error-message err))))))))

;;;; What needs the user

(defun drover-remote-push--signature (record)
  "Return what a phone is told of RECORD changing, a short string.
Its status, and the kinds and summaries of its requests."
  (string-limit
   (mapconcat #'identity
              (cons (symbol-name (drover-session-status record))
                    (mapcar (lambda (request)
                              (format "%s:%s" (plist-get request :kind)
                                      (drover-bound-line (or (plist-get request :summary) "") 80)))
                            (seq-take (drover-session-requests record) 4)))
              "|")
   400))

(defun drover-remote-push--needing (key)
  "Return the record of this host's session KEY when it needs the user, else nil.
A session of this host's own backends, served to peers, whose status
needs the user, and at the top of its tree or waiting itself."
  (when-let* ((served (drover-remote--served key))
              (record (car served))
              ((not (drover-session-stale record)))
              ((memq (drover-session-status record) drover-attention-statuses))
              ((or (eq (drover-session-status record) 'waiting)
                   (and (null (drover-session-parent record)) (null (cdr served))))))
    record))

(defun drover-remote-push--follow (state keys)
  "Follow the sessions KEYS in STATE's set of those needing the user, t for all.
Return non-nil when the set changed."
  (let ((needs (or (drover-remote-push--state-needs state)
                   (setf (drover-remote-push--state-needs state) (make-hash-table :test #'equal))))
        (changed nil))
    (when (eq keys t)
      (setq keys (delete-dups (append (hash-table-keys needs)
                                      (mapcar #'drover-session-key (drover-index-sessions))))))
    (dolist (key keys)
      (let* ((record (drover-remote-push--needing key))
             (signature (and record (drover-remote-push--signature record))))
        (unless (equal signature (gethash key needs))
          (setq changed t)
          (if signature (puthash key signature needs) (remhash key needs)))))
    changed))

(defun drover-remote-push--watched-changed-p (state)
  "Return non-nil when a session a phone of STATE watches changed status."
  (let ((changed nil))
    (maphash (lambda (_name reg)
               (maphash (lambda (key value)
                          (let ((record (car (drover-remote--served key))))
                            (unless (eq (and record (drover-session-status record)) (cdr value))
                              (setq changed t))))
                        (drover-remote-push--reg-watch reg)))
             (drover-remote-push--state-regs state))
    changed))

(defun drover-remote-push-changed ()
  "Follow the changes of this host's sessions, for the phones pushed to.
For `drover-changed-hook'."
  (drover-with-reporting "remote push"
    (when-let* ((state (drover-remote-push--current))
                ((> (hash-table-count (drover-remote-push--state-regs state)) 0)))
      (when (or (drover-remote-push--follow state drover-changed-sessions)
                (drover-remote-push--watched-changed-p state))
        (drover-remote-push--dirty state)))))

;;;; Scheduling

(defun drover-remote-push--dirty (state)
  "Note in STATE that its phones may need a push, and schedule its evaluation."
  (let ((now (float-time)))
    (maphash (lambda (_name reg)
               (unless (drover-remote-push--reg-dirty reg)
                 (setf (drover-remote-push--reg-dirty reg) now)))
             (drover-remote-push--state-regs state))
    (drover-remote-push--schedule state)))

(defun drover-remote-push--due (reg now)
  "Return when REG's next push may go, at NOW, or nil when none is due."
  (when-let* ((dirty (drover-remote-push--reg-dirty reg))
              ((not (drover-remote-push--reg-dead reg)))
              ((not (drover-remote-push--reg-fg reg))))
    (max (min (+ now drover-remote-push-delay) (+ dirty drover-remote-push-wait))
         (+ (or (drover-remote-push--reg-ok reg) 0) drover-remote-push-spacing)
         (or (drover-remote-push--reg-backoff reg) 0))))

(defun drover-remote-push--schedule (state)
  "Arm STATE's timer for the next push due."
  (let* ((now (float-time))
         (dues (delq nil (mapcar (lambda (reg) (drover-remote-push--due reg now))
                                 (hash-table-values (drover-remote-push--state-regs state)))))
         (due (and dues (apply #'min dues))))
    (when (drover-remote-push--state-timer state)
      (cancel-timer (drover-remote-push--state-timer state))
      (setf (drover-remote-push--state-timer state) nil))
    (when due
      (setf (drover-remote-push--state-timer state)
            (run-at-time (max 0 (- due now)) nil
                         (let ((link (drover-link state)))
                           (lambda ()
                             (drover-with-reporting "remote push"
                               (let ((state (drover-linked link)))
                                 (setf (drover-remote-push--state-timer state) nil)
                                 (when (eq state (drover-remote-push--current))
                                   (drover-remote-push--evaluate state)))))))))))

(defun drover-remote-push--idle-seconds ()
  "Return how long this Emacs has been idle, in seconds."
  (let ((idle (funcall drover-remote-push-idle-function)))
    (if idle (float-time idle) 0.0)))

(defun drover-remote-push--hold (reg)
  "Return the seconds of input during which REG's new pushes wait."
  (let ((peer (drover-remote--peer (drover-remote-push--reg-peer reg))))
    (or (drover-remote-push--reg-hold reg)
        (and peer (numberp (plist-get (drover-remote--peer-properties peer) :push-hold))
             (plist-get (drover-remote--peer-properties peer) :push-hold))
        drover-remote-push-hold
        0)))

(defun drover-remote-push--arm-idle (state seconds)
  "Evaluate STATE's pushes once this Emacs has been idle SECONDS."
  (unless (drover-remote-push--state-idle state)
    (setf (drover-remote-push--state-idle state)
          (run-with-idle-timer seconds nil
                               (let ((link (drover-link state)))
                                 (lambda ()
                                   (drover-with-reporting "remote push"
                                     (let ((state (drover-linked link)))
                                       (setf (drover-remote-push--state-idle state) nil)
                                       (when (eq state (drover-remote-push--current))
                                         (drover-remote-push-fire-idle state))))))))))

(defun drover-remote-push-fire-idle (&optional state)
  "Push what was held while the user typed, to STATE's phones.
Run once this Emacs has been idle as long as phones hold."
  (when-let* ((state (or state (drover-remote-push--current))))
    (drover-remote-push--dirty state)))

;;;; Evaluating

(defun drover-remote-push--wanted (state reg held)
  "Return what REG's phone should be told now, of STATE's sessions.
A hash table of signatures by key.  With HELD, what is new is left
out, and what changed stays as it was shown: only what left the set
goes at once."
  (let ((wanted (make-hash-table :test #'equal))
        (shown (drover-remote-push--reg-shown reg)))
    (maphash (lambda (key signature)
               (let ((was (gethash key shown)))
                 (cond ((not held) (puthash key signature wanted))
                       (was (puthash key was wanted)))))
             (or (drover-remote-push--state-needs state) (make-hash-table)))
    wanted))

(defun drover-remote-push--same-p (a b)
  "Return non-nil when the hash tables of signatures A and B hold the same."
  (and (= (hash-table-count a) (hash-table-count b))
       (catch 'differ
         (maphash (lambda (key signature)
                    (unless (equal (gethash key b) signature) (throw 'differ nil)))
                  a)
         t)))

(defun drover-remote-push--watch-changes (reg)
  "Return the keys REG watches whose status changed since told.
Watches past their time are dropped."
  (let ((now (float-time)) (changed nil) (ended nil))
    (maphash (lambda (key value)
               (let* ((record (car (drover-remote--served key)))
                      (status (and record (drover-session-status record))))
                 (cond
                  ((> now (car value)) (push key ended))
                  ((not (eq status (cdr value))) (push key changed)))))
             (drover-remote-push--reg-watch reg))
    (dolist (key ended) (remhash key (drover-remote-push--reg-watch reg)))
    changed))

(defun drover-remote-push--evaluate (state)
  "Push each phone of STATE what changed since it was last told, if due."
  (let ((now (float-time))
        (later nil))
    (maphash
     (lambda (_name reg)
       (let ((due (drover-remote-push--due reg now)))
         (cond
          ((null due))
          ((drover-remote-push--reg-sending reg) (setq later t))
          ((> due (+ now 0.05)) (setq later t))
          (t
           (setf (drover-remote-push--reg-dirty reg) nil)
           (let* ((hold (drover-remote-push--hold reg))
                  (busy (and (> hold 0) (< (drover-remote-push--idle-seconds) hold)))
                  (full (drover-remote-push--wanted state reg nil))
                  (held (and busy (not (drover-remote-push--same-p
                                        full (drover-remote-push--wanted state reg t)))))
                  (wanted (if held (drover-remote-push--wanted state reg t) full))
                  (watched (drover-remote-push--watch-changes reg)))
             (when held
               (drover-remote-push--arm-idle state hold))
             (unless (and (drover-remote-push--same-p wanted (drover-remote-push--reg-shown reg))
                          (null watched))
               (drover-remote-push--push state reg wanted watched
                                         (cond ((drover-remote-push--grew-p wanted (drover-remote-push--reg-shown reg))
                                                "need")
                                               ((and watched (drover-remote-push--same-p
                                                              wanted (drover-remote-push--reg-shown reg)))
                                                "watch")
                                               (t "clear")))))))))
     (drover-remote-push--state-regs state))
    (when later (drover-remote-push--schedule state))))

(defun drover-remote-push--grew-p (wanted shown)
  "Return non-nil when WANTED holds a key SHOWN lacks, or with another signature."
  (catch 'grew
    (maphash (lambda (key signature)
               (unless (equal (gethash key shown) signature) (throw 'grew t)))
             wanted)
    nil))

;;;; Building a push

(defun drover-remote-push--wire-key (key)
  "Return KEY, (BACKEND . ID), as it travels."
  (vector (symbol-name (car key)) (cdr key)))

(defun drover-remote-push--gather (state keys callback)
  "Fetch what notifications show of KEYS into STATE, then call CALLBACK.
The oldest request of those waiting, and an excerpt of the last reply
of those done or failed, each cached by the session's signature;
CALLBACK runs once all came, or after `drover-remote-push-gather'."
  (let* ((gathered (drover-remote-push--state-gathered state))
         (left 0)
         (done nil)
         (timer nil)
         (finish (lambda ()
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (drover-with-reporting "remote push"
                       (funcall callback))))))
    (when (> (hash-table-count gathered) 1000)
      (clrhash gathered))
    (dolist (key keys)
      (when-let* ((record (car (drover-remote--served key)))
                  (cache (cons key (drover-remote-push--signature record)))
                  ((not (gethash cache gathered))))
        (cond
         ((and (drover-session-requests record) (drover-supports-p record :requests))
          (cl-incf left)
          (condition-case nil
              (drover-backend-call record :requests
                                   (lambda (requests)
                                     (unless done
                                       (when (and (consp requests) (not (eq (car requests) :failure)))
                                         (puthash cache (list :request (car requests)) gathered))
                                       (when (zerop (cl-decf left)) (funcall finish)))))
            (error (cl-decf left))))
         ((memq (drover-session-status record) '(done failed))
          (let ((brief (or (drover-session-brief-recap record) (drover-session-detail record))))
            (if brief
                (puthash cache (list :excerpt brief) gathered)
              (cl-incf left)
              (condition-case nil
                  (drover-recap record
                                (lambda (recap)
                                  (unless done
                                    (puthash cache (list :excerpt (or (plist-get recap :error)
                                                                      (plist-get recap :reply)))
                                             gathered)
                                    (when (zerop (cl-decf left)) (funcall finish)))))
                (error (cl-decf left)))))))))
    (if (zerop left)
        (funcall finish)
      (setq timer (run-at-time drover-remote-push-gather nil
                               (lambda () (drover-with-reporting "remote push" (funcall finish))))))))

(defun drover-remote-push--entry-of (state key detail)
  "Return the entry telling session KEY, of STATE, at DETAIL, or nil."
  (when-let* ((record (car (drover-remote--served key))))
    (let* ((signature (drover-remote-push--signature record))
           (extra (gethash (cons key signature) (drover-remote-push--state-gathered state)))
           (request (plist-get extra :request))
           (summary (car (drover-session-requests record)))
           (parent (drover-session-parent record)))
      (append
       (list :k (drover-remote-push--wire-key key)
             :ti (or (drover-bound-line (drover-session-title record) 80) "")
             :st (symbol-name (drover-session-status record))
             :u (or (drover-remote--number (drover-session-since record)) :false))
       (and parent (drover-remote--wire-id parent)
            (list :p (vector (symbol-name (car key)) (drover-remote--wire-id parent))))
       (and (drover-session-engine record)
            (list :g (symbol-name (drover-session-engine record))))
       (and summary
            (list :q (append
                      (list :k (if (eq (plist-get summary :kind) 'question) "question" "permission"))
                      (and (eq detail 'full)
                           (list :s (or (drover-bound-line (plist-get summary :summary) 160) "")))
                      (and request (stringp (plist-get request :id))
                           (list :id (drover-bound-line (plist-get request :id) 200)))
                      (and request (eq (plist-get request :kind) 'permission)
                           (list :ch (vconcat (seq-take (seq-filter #'stringp (plist-get request :choices)) 4)))))))
       (and (eq detail 'full) (stringp (plist-get extra :excerpt))
            (memq (drover-session-status record) '(done failed))
            (list :x (drover-bound-text (plist-get extra :excerpt) 4 200 t)))))))

(defconst drover-remote-push--order '(waiting failed done retrying working idle)
  "The order of entries in a push, by status.")

(defun drover-remote-push--payload (state reg wanted watched reason)
  "Return the payload telling REG's phone WANTED and WATCHED, of STATE, for REASON.
Entries are dropped to fit the body's bound, excerpts first, then
summaries, then entries, `m' then telling more were left out."
  (let* ((detail (drover-remote-push--reg-detail reg))
         (keys (seq-sort-by (lambda (key)
                              (let ((record (car (drover-remote--served key))))
                                (list (or (seq-position drover-remote-push--order
                                                        (and record (drover-session-status record)))
                                          9)
                                      (- (or (and record (drover-session-since record)) 0)))))
                            (lambda (a b) (or (< (car a) (car b))
                                              (and (= (car a) (car b)) (< (cadr a) (cadr b)))))
                            (hash-table-keys wanted)))
         (entries (and (not (eq detail 'counts))
                       (delq nil (mapcar (lambda (key) (drover-remote-push--entry-of state key detail)) keys))))
         (watches (delq nil (mapcar (lambda (key)
                                      (unless (gethash key wanted)
                                        (when-let* ((record (car (drover-remote--served key))))
                                          (list :k (drover-remote-push--wire-key key)
                                                :ti (or (drover-bound-line (drover-session-title record) 80) "")
                                                :st (symbol-name (drover-session-status record))
                                                :u (or (drover-remote--number (drover-session-since record))
                                                       :false)))))
                                    (seq-uniq (append watched (hash-table-keys (drover-remote-push--reg-watch reg)))))))
         (more nil))
    (cl-flet ((make ()
                (list :v 1 :h drover-host :s (drover-remote-push--reg-seq reg) :t (float-time) :r reason
                      :n (hash-table-count wanted) :m (if more t :false)
                      :e (vconcat entries) :w (vconcat watches))))
      ;; Within the body's bound: the encoded payload grows by a third.
      (let ((budget (- (floor (* 3 (- drover-remote-push-limit 64)) 4) 32)))
        (when (> (length (encode-coding-string (json-serialize (make)) 'utf-8)) budget)
          (setq entries (mapcar (lambda (entry) (drover-remote-push--without entry :x)) entries)))
        (when (> (length (encode-coding-string (json-serialize (make)) 'utf-8)) budget)
          (setq entries (mapcar (lambda (entry)
                                  (if-let* ((q (plist-get entry :q)))
                                      (plist-put (copy-sequence entry) :q (drover-remote-push--without q :s))
                                    entry))
                                entries)))
        (while (and (or entries watches)
                    (> (length (encode-coding-string (json-serialize (make)) 'utf-8)) budget))
          (setq more t)
          (if entries
              (setq entries (butlast entries (max 1 (/ (length entries) 4))))
            (setq watches (butlast watches))))
        (make)))))

(defun drover-remote-push--without (plist key)
  "Return PLIST without KEY."
  (let (out)
    (while plist
      (unless (eq (car plist) key)
        (setq out (cons (cadr plist) (cons (car plist) out))))
      (setq plist (cddr plist)))
    (nreverse out)))

;;;; Encryption

(defun drover-remote-push--kid-p (kid)
  "Return non-nil when KID may name a key."
  (and (stringp kid) (string-match-p "\\`[A-Za-z0-9_-]\\{4,32\\}\\'" kid)))

(defun drover-remote-push--decode-key (text)
  "Return the 32 bytes TEXT, base64url, encodes, or nil."
  (when (and (stringp text) (< (length text) 64))
    (let ((bytes (ignore-errors (base64-decode-string text t))))
      (and (stringp bytes) (= (length bytes) 32) bytes))))

(defun drover-remote-push--u64 (n)
  "Return N as eight bytes, big endian."
  (apply #'unibyte-string (mapcar (lambda (shift) (logand (ash n (- shift)) 255)) '(56 48 40 32 24 16 8 0))))

(defun drover-remote-push-nonce (host kid seq)
  "Return the nonce of the push SEQ of HOST under KID, twelve bytes."
  (concat (substring (secure-hash 'sha256 (encode-coding-string (concat host kid) 'utf-8) nil nil t) 0 4)
          (drover-remote-push--u64 seq)))

(defun drover-remote-push-seal (key kid seq payload &optional host)
  "Return the body of a push of PAYLOAD, a plist, under KEY and KID, counter SEQ.
HOST, by default `drover-host', names the sender in the nonce and the
additional data."
  (unless (drover-remote-push-available-p)
    (error "Pushes need AES-256-GCM through gnutls"))
  (let* ((host (or host drover-host))
         (nonce (drover-remote-push-nonce host kid seq))
         (plain (encode-coding-string (json-serialize payload) 'utf-8))
         (aad (encode-coding-string (format "drover-push/1 %s %s" host kid) 'utf-8))
         ;; Gnutls wipes the key it is given.
         (sealed (car (gnutls-symmetric-encrypt 'AES-256-GCM (copy-sequence key) nonce plain aad))))
    (concat "dp1." kid "." (base64url-encode-string (concat nonce sealed) t))))

(defun drover-remote-push-open (key kid body &optional host)
  "Return the payload BODY carries under KID, a JSON string, opened with KEY.
For tests and tools; HOST, by default `drover-host', sent it."
  (drover-remote-push-available-p)
  (let* ((host (or host drover-host))
         (prefix (concat "dp1." kid ".")))
    (unless (string-prefix-p prefix body) (error "Not a push of %s" kid))
    (let* ((bytes (base64-decode-string (substring body (length prefix)) t))
           (nonce (substring bytes 0 12))
           (aad (encode-coding-string (format "drover-push/1 %s %s" host kid) 'utf-8)))
      (decode-coding-string
       (car (gnutls-symmetric-decrypt 'AES-256-GCM (copy-sequence key) nonce (substring bytes 12) aad))
       'utf-8))))

;;;; Sending

(defun drover-remote-push--next-seq (state reg)
  "Return REG's next counter, reserving more on disk, in STATE, when needed."
  (let ((seq (1+ (drover-remote-push--reg-seq reg))))
    (setf (drover-remote-push--reg-seq reg) seq)
    (when (>= seq (drover-remote-push--reg-reserved reg))
      (setf (drover-remote-push--reg-reserved reg) (+ seq drover-remote-push-reserve))
      (drover-remote-push--save state))
    seq))

(defun drover-remote-push--push (state reg wanted watched reason)
  "Push REG's phone WANTED and WATCHED of STATE, for REASON.
Once what notifications show of them came."
  (setf (drover-remote-push--reg-sending reg) t)
  (let ((link (drover-link state))
        (reg-link (drover-link reg)))
    (drover-remote-push--gather
     state (hash-table-keys wanted)
     (lambda ()
       (let ((state (drover-linked link))
             (reg (drover-linked reg-link)))
         (if (not (eq state (drover-remote-push--current)))
             (setf (drover-remote-push--reg-sending reg) nil)
           (drover-remote-push--seal-and-send state reg wanted watched reason)))))))

(defun drover-remote-push--seal-and-send (state reg wanted watched reason)
  "Seal and post REG's push of WANTED and WATCHED, for REASON, in STATE."
  (let* ((seq (drover-remote-push--next-seq state reg))
         (payload (drover-remote-push--payload state reg wanted watched reason))
         (body (condition-case err
                   (drover-remote-push-seal (drover-remote-push--reg-key reg) (drover-remote-push--reg-kid reg)
                                            seq payload)
                 (error (drover-remote-push--failed state reg nil (drover-error-message err))
                        nil)))
         (statuses (let ((table (make-hash-table :test #'equal)))
                     (dolist (key (seq-uniq (append watched (hash-table-keys (drover-remote-push--reg-watch reg)))))
                       (let ((record (car (drover-remote--served key))))
                         (puthash key (and record (drover-session-status record)) table)))
                     table))
         (link (drover-link state))
         (reg-link (drover-link reg)))
    (when body
      (drover-remote-push--post
       reg body
       (lambda (status &optional why)
         (let ((state (drover-linked link))
               (reg (drover-linked reg-link)))
           (setf (drover-remote-push--reg-sending reg) nil)
           (when (eq state (drover-remote-push--current))
             (if (and (integerp status) (<= 200 status 299))
                 (drover-remote-push--delivered state reg wanted statuses)
               (drover-remote-push--failed state reg status why)))))))))

(defun drover-remote-push--post (reg body callback)
  "Post BODY to REG's endpoint, then call CALLBACK with the status, or nil and why."
  (let ((token (drover-remote-push--token)))
    (if (eq token 'failed)
        (funcall callback nil "no token: see `drover-remote-push-token'")
      (condition-case err
          (funcall drover-remote-push-send-function (drover-remote-push--reg-url reg) token body callback)
        (error (funcall callback nil (drover-error-message err)))))))

(defun drover-remote-push--delivered (state reg wanted statuses)
  "Note in STATE that REG's phone took a push telling WANTED.
STATUSES are those of the sessions it watches, as told."
  (setf (drover-remote-push--reg-shown reg) wanted
        (drover-remote-push--reg-ok reg) (float-time)
        (drover-remote-push--reg-error reg) nil
        (drover-remote-push--reg-failures reg) 0
        (drover-remote-push--reg-backoff reg) nil)
  (maphash (lambda (key status)
             (when-let* ((value (gethash key (drover-remote-push--reg-watch reg))))
               (if (memq status drover-active-statuses)
                   (puthash key (cons (car value) status) (drover-remote-push--reg-watch reg))
                 ;; Its turn ended, and the phone was told.
                 (remhash key (drover-remote-push--reg-watch reg)))))
           statuses)
  (drover-remote-push--save state)
  (drover-remote-push--schedule state))

(defun drover-remote-push--failed (state reg status why)
  "Note in STATE that a push to REG failed, of HTTP STATUS, or WHY.
Its pushes wait a while before trying again; a 404 or 410 means its
endpoint is gone, until the phone registers again."
  (let ((line (drover-bound-line (or why (format "HTTP %s" status)))))
    (setf (drover-remote-push--reg-error reg) line
          (drover-remote-push--reg-dirty reg) (or (drover-remote-push--reg-dirty reg) (float-time)))
    (if (memq status '(404 410))
        (progn (setf (drover-remote-push--reg-dead reg) t)
               (drover-remote-push--save state))
      (let ((n (cl-incf (drover-remote-push--reg-failures reg))))
        (setf (drover-remote-push--reg-backoff reg)
              (+ (float-time) (nth (min (1- n) (1- (length drover-remote-push-backoff)))
                                   drover-remote-push-backoff)))))
    (when (drover--report-due-p (list "push" (drover-remote-push--reg-peer reg)))
      (drover-warn "Pushing to %s failed: %s" (drover-remote-push--reg-peer reg) line))
    (drover-remote-push--schedule state)))

(defun drover-remote-push--token ()
  "Return the token to post with, nil for none, or `failed' when it does not read."
  (let ((state (drover-remote-push--current))
        (spec drover-remote-push-token))
    (or (and state (drover-remote-push--state-token state))
        (let ((token (condition-case nil
                         (pcase spec
                           ('nil nil)
                           (`(pass ,(and name (pred stringp)))
                            (require 'auth-source-pass)
                            (auth-source-pass-get 'secret name))
                           ((pred functionp) (funcall spec))
                           (_ 'failed))
                       (error 'failed))))
          (cond
           ((null spec) nil)
           ((and (stringp token) (string-match-p "\\`[A-Za-z0-9_-]\\{1,200\\}\\'" token))
            (when state (setf (drover-remote-push--state-token state) token))
            token)
           (t 'failed))))))

(defun drover-remote-push--curl (url token body callback)
  "Post BODY to URL with TOKEN through curl, then call CALLBACK with the status.
The configuration, the token and the body among it, goes on curl's
standard input; it is killed past 30 seconds."
  (let* ((output nil)
         (done nil)
         (process
          (make-process
           :name "drover-push" :command (list drover-remote-push-curl "-K" "-")
           :connection-type 'pipe :noquery t :coding 'utf-8-unix
           :filter (lambda (_process text)
                     (when (< (length output) 4096) (setq output (concat output text))))
           :sentinel (lambda (process _event)
                       (unless (or done (process-live-p process))
                         (setq done t)
                         (drover-with-reporting "remote push"
                           (let ((status (and output (string-match "\\([0-9]\\{3\\}\\)\\s-*\\'" output)
                                              (string-to-number (match-string 1 output)))))
                             (if (and status (> status 0))
                                 (funcall callback status)
                               (funcall callback nil (format "curl exited %s%s" (process-exit-status process)
                                                             (if output (concat ": " (drover-bound-line output)) ""))))))))))
         (timer (run-at-time 30 nil (lambda ()
                                      (when (process-live-p process)
                                        (delete-process process))))))
    (set-process-sentinel process
                          (let ((sentinel (process-sentinel process)))
                            (lambda (process event)
                              (unless (process-live-p process) (cancel-timer timer))
                              (funcall sentinel process event))))
    (process-send-string
     process
     (concat (format "url = \"%s\"\n" url)
             "request = \"POST\"\n"
             (if token (format "header = \"Authorization: Bearer %s\"\n" token) "")
             "header = \"Content-Type: text/plain\"\n"
             (format "data-binary = \"%s\"\n" body)
             "max-time = 20\nconnect-timeout = 10\nsilent\nshow-error\n"
             (format "output = \"%s\"\n" (replace-regexp-in-string "\\\\" "/" null-device))
             "write-out = \"%{http_code}\"\n"))
    (process-send-eof process)
    process))

;;;; Phones registering

(defun drover-remote-push--endpoint-p (url)
  "Return non-nil when URL is an endpoint of `drover-remote-push-endpoints'."
  (when (and (stringp url) (< (length url) 512) (not (string-match-p "[\"\\\\[:space:][:cntrl:]]" url)))
    (let ((parsed (url-generic-parse-url url)))
      (and (null (url-user parsed)) (null (url-password parsed))
           (string-match "\\`\\(/[A-Za-z0-9_-]\\{1,64\\}\\)\\(?:\\?\\(.*\\)\\)?\\'" (or (url-filename parsed) ""))
           (member (match-string 2 (url-filename parsed)) '(nil "up=1"))
           (seq-some (lambda (endpoint)
                       (let ((allowed (url-generic-parse-url endpoint)))
                         (and (equal (url-type allowed) (url-type parsed))
                              (url-host allowed)
                              (string-equal-ignore-case (url-host allowed) (url-host parsed))
                              (eql (url-port allowed) (url-port parsed)))))
                     drover-remote-push-endpoints)))))

(defun drover-remote-push-on-request (peer message reply)
  "Answer PEER's push request MESSAGE through REPLY: register, drop or test."
  (let ((state (drover-remote-push--current)))
    (cond
     ((not state) (funcall reply nil "this host pushes nothing"))
     ((drover-remote--peer-ephemeral peer) (funcall reply nil "only a peer configured here is pushed to"))
     ((eq (plist-get message :drop) t)
      (remhash (drover-remote--peer-name peer) (drover-remote-push--state-regs state))
      (drover-remote-push--save state)
      (funcall reply (list :ok t)))
     ((eq (plist-get message :test) t)
      (if-let* ((reg (gethash (drover-remote--peer-name peer) (drover-remote-push--state-regs state))))
          (let* ((seq (drover-remote-push--next-seq state reg))
                 (body (drover-remote-push-seal (drover-remote-push--reg-key reg) (drover-remote-push--reg-kid reg) seq
                                                (list :v 1 :h drover-host :s seq :t (float-time) :r "test"
                                                      :n 0 :m :false :e [] :w []))))
            (drover-remote-push--post reg body
                                      (lambda (status &optional why)
                                        (if status
                                            (funcall reply (list :st status))
                                          (funcall reply nil why)))))
        (funcall reply nil "no endpoint registered")))
     (t (drover-remote-push--register state peer (plist-get message :set) reply)))))

(defun drover-remote-push--register (state peer set reply)
  "Register PEER's phone as SET says in STATE, answering through REPLY."
  (let* ((set (and (consp set) (keywordp (car set)) set))
         (url (plist-get set :url))
         (key (drover-remote-push--decode-key (plist-get set :key)))
         (kid (plist-get set :kid))
         (detail (pcase (plist-get set :detail) ("titles" 'titles) ("counts" 'counts) (_ 'full)))
         (hold (plist-get set :hold))
         (name (drover-remote--peer-name peer))
         (old (gethash name (drover-remote-push--state-regs state))))
    (cond
     ((not set) (funcall reply nil "no registration"))
     ((not (drover-remote-push--endpoint-p url))
      (funcall reply nil (drover-remote--refuse-action peer "be pushed to" "an endpoint this host does not post to")))
     ((not key) (funcall reply nil "a key of 32 bytes, base64url, is needed"))
     ((not (drover-remote-push--kid-p kid)) (funcall reply nil "no key identifier"))
     ((and (not old) (>= (hash-table-count (drover-remote-push--state-regs state)) drover-remote-push-registrations))
      (funcall reply nil "this host pushes to as many phones as it may"))
     (t
      (let ((reg (if (and old (equal (drover-remote-push--reg-kid old) kid)
                          (equal (drover-remote-push--reg-key old) key))
                     old
                   ;; A counter from the clock: a key sent again after this
                   ;; host lost its file never sees a nonce again.
                   (let ((start (* 1000 (floor (float-time)))))
                     (drover-remote-push--reg-make :peer name :kid kid :key key
                                                   :seq start :reserved start
                                                   :shown (make-hash-table :test #'equal)
                                                   :watch (make-hash-table :test #'equal)
                                                   :created (float-time))))))
        (setf (drover-remote-push--reg-url reg) url
              (drover-remote-push--reg-detail reg) detail
              (drover-remote-push--reg-hold reg) (and (numberp hold) (>= hold 0) (min hold 86400))
              (drover-remote-push--reg-dead reg) nil)
        (puthash name reg (drover-remote-push--state-regs state))
        (unless (drover-remote-push--state-needs state)
          (drover-remote-push--follow state t))
        (drover-remote-push--save state)
        ;; A phone not telling it is in front is told what needs the user.
        (unless (drover-remote-push--reg-fg reg)
          (setf (drover-remote-push--reg-dirty reg) (float-time))
          (drover-remote-push--schedule state))
        (drover-remote--event peer 'push "pushed to through %s" (url-host (url-generic-parse-url url)))
        (funcall reply (list :ok t :kid kid :seq (drover-remote-push--reg-seq reg) :max drover-remote-push-limit)))))))

(defun drover-remote-push-on-watch (peer message reply)
  "Answer PEER's watch request MESSAGE through REPLY: watch or unwatch sessions."
  (let* ((state (drover-remote-push--current))
         (reg (and state (gethash (drover-remote--peer-name peer) (drover-remote-push--state-regs state))))
         (keys (plist-get message :keys))
         (ttl (plist-get message :ttl))
         (until (+ (float-time) (if (and (numberp ttl) (> ttl 0)) (min ttl drover-remote-push-watch-ttl)
                                  drover-remote-push-watch-ttl))))
    (cond
     ((not reg) (funcall reply nil "no endpoint registered"))
     ((not (and (vectorp keys) (<= (length keys) drover-remote-push-watches)))
      (funcall reply nil "no sessions, or too many"))
     (t
      (let ((watch (drover-remote-push--reg-watch reg)))
        (seq-doseq (wire keys)
          (when-let* ((key (drover-remote--key wire)))
            (if (not (eq (plist-get message :on) t))
                (remhash key watch)
              (when-let* ((record (car (drover-remote--served key))))
                (when (< (hash-table-count watch) drover-remote-push-watches)
                  (puthash key (cons until (drover-session-status record)) watch))))))
        (drover-remote-push--save state)
        (funcall reply (list :ok t :watching (vconcat (mapcar #'drover-remote-push--wire-key
                                                              (hash-table-keys watch))))))))))

(defun drover-remote-push-foreground (peer on)
  "Note that PEER's phone came to the front, ON non-nil, or left it.
In front, it is pushed nothing; leaving it, what it shows is what it
saw live, which it reconciles its notifications with."
  (when-let* ((state (drover-remote-push--current))
              (reg (gethash (drover-remote--peer-name peer) (drover-remote-push--state-regs state))))
    (if on
        (setf (drover-remote-push--reg-fg reg) t)
      (when (drover-remote-push--reg-fg reg)
        (setf (drover-remote-push--reg-fg reg) nil
              (drover-remote-push--reg-shown reg)
              (copy-hash-table (or (drover-remote-push--state-needs state) (make-hash-table :test #'equal)))
              (drover-remote-push--reg-dirty reg) nil)
        (drover-remote-push--save state)))))

(defun drover-remote-push-detached (peer)
  "Note that PEER's connection ended: a phone in front then is pushed what differs."
  (when-let* ((state (drover-remote-push--current))
              (reg (gethash (drover-remote--peer-name peer) (drover-remote-push--state-regs state)))
              ((drover-remote-push--reg-fg reg)))
    (setf (drover-remote-push--reg-fg reg) nil)
    (drover-remote-push--dirty state)))

(defun drover-remote-push-status (peer)
  "Return a line telling how pushes to PEER stand, or nil when none are made."
  (when-let* ((state (drover-remote-push--current))
              (reg (gethash (drover-remote--peer-name peer) (drover-remote-push--state-regs state))))
    (drover-bound-line
     (concat "pushed to"
             (cond ((drover-remote-push--reg-dead reg) ", its endpoint gone")
                   ((drover-remote-push--reg-fg reg) ", in front now")
                   ((drover-remote-push--reg-ok reg)
                    (format ", last %s ago" (drover-remote--duration (- (float-time) (drover-remote-push--reg-ok reg)))))
                   (t ", none yet"))
             (and (drover-remote-push--reg-error reg) (concat ": " (drover-remote-push--reg-error reg)))))))

;;;; Starting and stopping

(defun drover-remote-push-start ()
  "Make the state of pushes, reading the phones registered before."
  (drover-remote-push-stop)
  (let ((state (drover-remote-push--state-make)))
    (setq drover-remote-push--state state)
    (drover-remote-push--load state)
    (when (> (hash-table-count (drover-remote-push--state-regs state)) 0)
      (drover-remote-push--follow state t))
    (add-hook 'drover-changed-hook #'drover-remote-push-changed)
    state))

(defun drover-remote-push-stop ()
  "Stop pushing; the phones registered stay on disk."
  (when-let* ((state drover-remote-push--state))
    (when (drover-layout-current-p state 'drover-remote-push--state)
      (dolist (timer (list (drover-remote-push--state-timer state) (drover-remote-push--state-idle state)))
        (when timer (cancel-timer timer)))))
  (remove-hook 'drover-changed-hook #'drover-remote-push-changed)
  (setq drover-remote-push--state nil))

;;;###autoload
(defun drover-remote-push-test ()
  "Push a test to every phone registered here, telling how each went."
  (interactive)
  (let ((state (or (drover-remote-push--current) (user-error "`drover-remote-mode' is off"))))
    (setf (drover-remote-push--state-token state) nil)
    (when (eq (drover-remote-push--token) 'failed)
      (user-error "The token does not read: see `drover-remote-push-token'"))
    (maphash (lambda (name reg)
               (let ((seq (drover-remote-push--next-seq state reg)))
                 (drover-remote-push--post
                  reg (drover-remote-push-seal (drover-remote-push--reg-key reg) (drover-remote-push--reg-kid reg) seq
                                               (list :v 1 :h drover-host :s seq :t (float-time) :r "test"
                                                     :n 0 :m :false :e [] :w []))
                  (lambda (status &optional why)
                    (drover-message "Push to %s: %s" name (if status (format "HTTP %s" status) why))))))
             (drover-remote-push--state-regs state))))

(provide 'drover-remote-push)
;;; drover-remote-push.el ends here
