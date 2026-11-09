;;; drover-remote-push-test.el --- Tests of pushes to phones  -*- lexical-binding: t; -*-

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

;; Pushes to a phone, in one batch Emacs: what needs the user and what
;; leaves it, sealed and opened again, held while the user types,
;; nothing while the phone is in front, watched sessions, bounds, the
;; file of phones, and curl against a server of this Emacs's own.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-remote)
(require 'drover-remote-push)

(defvar drover-remote-push-test--sent nil
  "Pushes posted, newest first, as (URL TOKEN BODY).")

(defvar drover-remote-push-test--status 200
  "The HTTP status the stand-in push server answers.")

(defconst drover-remote-push-test--key (apply #'unibyte-string (number-sequence 1 32))
  "The phone's key in the tests.")

(defmacro drover-remote-push-test-with (sessions &rest body)
  "Run BODY as the host `core' serving SESSIONS, pushes posted to a stand-in.
`idle' is how long this Emacs has been idle, in seconds."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (drover-remote-push-available-p))
     (drover-test-with-backend ,sessions
       (let* ((dir (file-name-as-directory (make-temp-file "drover-push-test" t)))
              (drover-state-directory dir)
              (drover-host "core")
              (drover-remote--state (drover-remote--state-make :epoch "e1"))
              (drover-remote-push-endpoints '("https://ntfy.example/"))
              (drover-remote-push-token nil)
              (drover-remote-push-delay 0)
              (drover-remote-push-spacing 0)
              (drover-remote-push-gather 0.2)
              (drover-remote-push-hold 0)
              (drover-remote-push-test--sent nil)
              (drover-remote-push-test--status 200)
              (idle 1000.0)
              (drover-remote-push-idle-function (lambda () (seconds-to-time idle)))
              (drover-remote-push-send-function
               (lambda (url token body callback)
                 (push (list url token body) drover-remote-push-test--sent)
                 (funcall callback drover-remote-push-test--status)))
              (drover-remote-push--state nil))
         (ignore idle)
         (unwind-protect
             (progn
               (drover-index)
               (drover-remote-push-start)
               ,@body)
           (drover-remote-push-stop)
           (delete-directory dir t))))))

(defun drover-remote-push-test--peer (name &rest properties)
  "Return a peer NAME with PROPERTIES in the remote layer's state."
  (let ((peer (drover-remote--peer-make :name name :properties properties
                                        :records (make-hash-table :test #'equal))))
    (puthash name peer (drover-remote--state-peers drover-remote--state))
    (setf (drover-remote--state-order drover-remote--state)
          (append (drover-remote--state-order drover-remote--state) (list name)))
    peer))

(defun drover-remote-push-test--ask (function peer message)
  "Ask FUNCTION, a handler, MESSAGE of PEER; return (REPLY ERROR)."
  (let ((answer 'none))
    (funcall function peer message (lambda (reply &optional error) (setq answer (list reply error))))
    (drover-remote-push-test--settle (lambda () (not (eq answer 'none))))
    answer))

(defun drover-remote-push-test--register (peer &rest set)
  "Register PEER's phone, SET over the defaults; return (REPLY ERROR)."
  (drover-remote-push-test--ask #'drover-remote-push-on-request peer
                                (list :set (append set (list :url "https://ntfy.example/upTopic1?up=1"
                                                             :key (base64url-encode-string
                                                                   drover-remote-push-test--key t)
                                                             :kid "kid1")))))

(defun drover-remote-push-test--settle (&optional predicate seconds)
  "Run timers until PREDICATE holds or SECONDS, 2 by default, pass."
  (let ((end (+ (float-time) (or seconds 2))))
    (while (and (< (float-time) end) (not (and predicate (funcall predicate))))
      (accept-process-output nil 0.01))))

(defun drover-remote-push-test--change (sessions)
  "Make SESSIONS the fake backend's, telling drover they changed."
  (setq drover-test-sessions sessions)
  (drover-backend-changed 'fake)
  (drover-changed-now))

(defun drover-remote-push-test--last ()
  "Return the last push opened, as a plist."
  (let ((body (nth 2 (car drover-remote-push-test--sent))))
    (json-parse-string (drover-remote-push-open drover-remote-push-test--key "kid1" body)
                       :object-type 'plist :array-type 'list :false-object nil :null-object nil)))

(defun drover-remote-push-test--pushes-after (count)
  "Wait until more than COUNT pushes were posted; return non-nil when so."
  (drover-remote-push-test--settle (lambda () (> (length drover-remote-push-test--sent) count)))
  (> (length drover-remote-push-test--sent) count))

(ert-deftest drover-remote-push-test-seal ()
  "A push opens only with its key, host and identifier; the key is never wiped."
  (skip-unless (drover-remote-push-available-p))
  (let* ((key (copy-sequence drover-remote-push-test--key))
         (body (drover-remote-push-seal key "k1" 7 '(:v 1 :r "need") "core")))
    (should (equal key drover-remote-push-test--key))
    (should (string-prefix-p "dp1.k1." body))
    (should (string-match-p "\\`[A-Za-z0-9._-]+\\'" body))
    (should (equal (json-parse-string (drover-remote-push-open key "k1" body "core") :object-type 'plist)
                   '(:v 1 :r "need")))
    (let ((bytes (base64-decode-string (substring body 7) t)))
      (should (equal (substring bytes 0 12) (drover-remote-push-nonce "core" "k1" 7)))
      (should (equal (substring bytes 4 12) (unibyte-string 0 0 0 0 0 0 0 7))))
    (should-error (drover-remote-push-open key "k1" body "reaver"))
    (should-error (drover-remote-push-open (make-string 32 ?x) "k1" body "core"))
    ;; GCM's test case 14: an all-zero key, IV and block.
    (should (equal (car (gnutls-symmetric-encrypt 'AES-256-GCM (make-string 32 0) (make-string 12 0)
                                                  (make-string 16 0) ""))
                   (concat (unibyte-string #xce #xa7 #x40 #x3d #x4d #x60 #x6b #x6e
                                           #x07 #x4e #xc5 #xd3 #xba #xf3 #x9d #x18)
                           (unibyte-string #xd0 #xd1 #xc8 #xa7 #x99 #x99 #x6b #xf0
                                           #x26 #x5b #x98 #xb5 #xd4 #x8a #xb9 #x19))))))

(ert-deftest drover-remote-push-test-endpoints ()
  "Only endpoints of the push servers configured are taken."
  (let ((drover-remote-push-endpoints '("https://ntfy.example/" "http://other.example:8080/")))
    (dolist (good '("https://ntfy.example/upAbc?up=1" "https://NTFY.example/topic_1"
                    "http://other.example:8080/up-x?up=1"))
      (should (drover-remote-push--endpoint-p good)))
    (dolist (bad '("https://evil.example/upAbc?up=1" "http://ntfy.example/upAbc"
                   "https://ntfy.example:444/upAbc" "https://user:pw@ntfy.example/upAbc"
                   "https://ntfy.example/a/b" "https://ntfy.example/up?up=1&x=2"
                   "https://ntfy.example/\"quoted" "https://ntfy.example/up\nx" 42 nil))
      (ert-info ((format "%S" bad))
        (should-not (drover-remote-push--endpoint-p bad))))
    (should-not (drover-remote-push--endpoint-p (concat "https://ntfy.example/" (make-string 600 ?a))))))

(ert-deftest drover-remote-push-test-registration ()
  "A configured peer registers, hostile registrations are refused."
  (drover-remote-push-test-with nil
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t :access 'full))
          (stranger (drover-remote-push-test--peer "stranger")))
      (setf (drover-remote--peer-ephemeral stranger) t)
      (pcase-let ((`(,reply ,error) (drover-remote-push-test--register phone)))
        (should-not error)
        (should (eq (plist-get reply :ok) t))
        (should (equal (plist-get reply :kid) "kid1"))
        (should (> (plist-get reply :seq) 1000000000000)))
      (should (cadr (drover-remote-push-test--register stranger)))
      (dolist (set (list (list :url "https://evil.example/upX?up=1")
                         (list :key "short")
                         (list :key (make-string 500000 ?a))
                         (list :kid "x")
                         (list :kid (make-string 100 ?a))
                         (list :url 42)))
        (ert-info ((format "%S" (seq-take set 2)))
          (should (cadr (drover-remote-push-test--ask
                         #'drover-remote-push-on-request phone
                         (list :set (append set (list :url "https://ntfy.example/upTopic1?up=1"
                                                      :key (base64url-encode-string drover-remote-push-test--key t)
                                                      :kid "kid1"))))))))
      (should (cadr (drover-remote-push-test--ask #'drover-remote-push-on-request phone (list :set "nope"))))
      ;; Dropping, told so, and only by true.
      (should (cadr (drover-remote-push-test--ask #'drover-remote-push-on-request phone (list :drop :false :set nil))))
      (should (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state)))
      (drover-remote-push-test--ask #'drover-remote-push-on-request phone (list :drop t))
      (should-not (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state))))))

(ert-deftest drover-remote-push-test-need-and-clear ()
  "Sessions coming to need the user are pushed, and their leaving it."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'working)
                                      (drover-test-session "b" :status 'idle))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-test--register phone)
      ;; Nothing needs the user, as the phone holds: nothing to tell.
      (drover-remote-push-test--settle nil 0.3)
      (should (null drover-remote-push-test--sent))
      (drover-remote-push-test--change
       (list (drover-test-session "a" :status 'done :since 100.0 :detail "All tests pass")
             (drover-test-session "b" :status 'waiting :since 200.0
                                  :requests '((:kind permission :summary "rm -rf build")))
             (drover-test-session "c" :status 'done :parent "a")))
      (should (drover-remote-push-test--pushes-after 0))
      (let* ((push (drover-remote-push-test--last))
             (entries (plist-get push :e)))
        (should (equal (plist-get push :r) "need"))
        (should (equal (plist-get push :h) "core"))
        (should (= (plist-get push :n) 2))
        (should (equal (mapcar (lambda (e) (plist-get e :st)) entries) '("waiting" "done")))
        (should (equal (plist-get (car entries) :k) '("fake" "b")))
        (should (equal (plist-get (plist-get (car entries) :q) :k) "permission"))
        (should (equal (plist-get (plist-get (car entries) :q) :s) "rm -rf build"))
        (should (equal (plist-get (cadr entries) :x) "All tests pass")))
      ;; Seen here: the phone is told it no longer needs them.
      (drover-remote-push-test--change
       (list (drover-test-session "a" :status 'idle)
             (drover-test-session "b" :status 'waiting :since 200.0
                                  :requests '((:kind permission :summary "rm -rf build")))))
      (should (drover-remote-push-test--pushes-after 1))
      (let ((push (drover-remote-push-test--last)))
        (should (equal (plist-get push :r) "clear"))
        (should (= (plist-get push :n) 1)))
      ;; Nothing changed: nothing pushed; seqs rise.
      (drover-remote-push-test--change drover-test-sessions)
      (drover-remote-push-test--settle nil 0.3)
      (should (= (length drover-remote-push-test--sent) 2))
      (let ((seqs (mapcar (lambda (sent)
                            (let ((bytes (base64-decode-string (substring (nth 2 sent) 9) t)))
                              (string-to-number (mapconcat (lambda (b) (format "%02x" b)) (substring bytes 4 12) "") 16)))
                          drover-remote-push-test--sent)))
        (should (equal seqs (sort (copy-sequence seqs) #'>)))))))

(ert-deftest drover-remote-push-test-hold ()
  "What is new waits while the user types here; what leaves goes at once."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'done))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t :push-hold 120)))
      (setq idle 1000.0)
      (drover-remote-push-test--register phone)
      (should (drover-remote-push-test--pushes-after 0))
      (should (= (plist-get (drover-remote-push-test--last) :n) 1))
      (setq idle 3.0)
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'done)
                                             (drover-test-session "b" :status 'failed)))
      (drover-remote-push-test--settle nil 0.3)
      (should (= (length drover-remote-push-test--sent) 1))
      (should (drover-remote-push--state-idle drover-remote-push--state))
      ;; Seen meanwhile: told at once, the new one still held.
      (drover-remote-push-test--change (list (drover-test-session "b" :status 'failed)))
      (should (drover-remote-push-test--pushes-after 1))
      (should (equal (plist-get (drover-remote-push-test--last) :r) "clear"))
      (should (= (plist-get (drover-remote-push-test--last) :n) 0))
      ;; Idle long enough: the held one goes.
      (setq idle 200.0)
      (drover-remote-push-fire-idle)
      (should (drover-remote-push-test--pushes-after 2))
      (should (equal (plist-get (drover-remote-push-test--last) :r) "need"))
      (should (= (plist-get (drover-remote-push-test--last) :n) 1)))))

(ert-deftest drover-remote-push-test-foreground ()
  "Nothing is pushed while the phone is in front; leaving, it saw what is."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'idle))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-foreground phone t)
      (drover-remote-push-test--register phone)
      (drover-remote-push-foreground phone t)
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'done)))
      (drover-remote-push-test--settle nil 0.3)
      (should (null drover-remote-push-test--sent))
      (drover-remote-push-foreground phone nil)
      (drover-remote-push-test--settle nil 0.3)
      (should (null drover-remote-push-test--sent))
      ;; In front and dropped: what differs from what it was told goes.
      (drover-remote-push-foreground phone t)
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'failed)))
      (drover-remote-push-detached phone)
      (should (drover-remote-push-test--pushes-after 0))
      (should (equal (plist-get (car (plist-get (drover-remote-push-test--last) :e)) :st) "failed")))))

(ert-deftest drover-remote-push-test-watch ()
  "A watched session's turn is pushed as it goes, the watch ending with it."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'working))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-foreground phone t)
      (drover-remote-push-test--register phone)
      (pcase-let ((`(,reply ,error) (drover-remote-push-test--ask #'drover-remote-push-on-watch phone
                                                                  (list :keys [["fake" "a"]] :on t :ttl 600))))
        (should-not error)
        (should (equal (plist-get reply :watching) [["fake" "a"]])))
      (drover-remote-push-foreground phone nil)
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'retrying)))
      (should (drover-remote-push-test--pushes-after 0))
      (let ((push (drover-remote-push-test--last)))
        (should (equal (plist-get push :r) "watch"))
        (should (equal (plist-get (car (plist-get push :w)) :st) "retrying")))
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'done)))
      (should (drover-remote-push-test--pushes-after 1))
      (should (equal (plist-get (drover-remote-push-test--last) :r) "need"))
      (should (= (hash-table-count (drover-remote-push--reg-watch
                                    (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state))))
                 0))
      (should (cadr (drover-remote-push-test--ask #'drover-remote-push-on-watch phone
                                                  (list :keys (make-vector 17 ["fake" "a"]) :on t)))))))

(ert-deftest drover-remote-push-test-bounds ()
  "Thousands of sessions and huge titles fit a push's bound, `m' telling more."
  (drover-remote-push-test-with
      (cons (drover-test-session "big" :status 'waiting :title (make-string 500000 ?t)
                                 :requests (list (list :kind 'question :summary (make-string 500000 ?q))))
            (mapcar (lambda (i) (drover-test-session (format "s%d" i) :status 'done :detail (make-string 5000 ?d)))
                    (number-sequence 1 10000)))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-test--register phone)
      (should (drover-remote-push-test--pushes-after 0))
      (should (<= (length (nth 2 (car drover-remote-push-test--sent))) drover-remote-push-limit))
      (let ((push (drover-remote-push-test--last)))
        (should (eq (plist-get push :m) t))
        (should (= (plist-get push :n) 10001))
        (should (> (length (plist-get push :e)) 0))
        (should (<= (length (plist-get (car (plist-get push :e)) :ti)) 81))))))

(ert-deftest drover-remote-push-test-failures ()
  "A push refused waits before trying again; an endpoint gone stops them."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'done))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t))
          (drover-error-interval 0))
      (setq drover-remote-push-test--status 500)
      (drover-remote-push-test--register phone)
      (should (drover-remote-push-test--pushes-after 0))
      (let ((reg (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state))))
        (should (drover-remote-push--reg-backoff reg))
        (should (equal (drover-remote-push--reg-error reg) "HTTP 500"))
        (setq drover-remote-push-test--status 404)
        (setf (drover-remote-push--reg-backoff reg) nil)
        (drover-remote-push--schedule drover-remote-push--state)
        (should (drover-remote-push-test--pushes-after 1))
        (should (drover-remote-push--reg-dead reg))
        (drover-remote-push-test--change (list (drover-test-session "a" :status 'failed)))
        (drover-remote-push-test--settle nil 0.3)
        (should (= (length drover-remote-push-test--sent) 2))
        (should (string-match-p "endpoint gone" (drover-remote-push-status phone)))))))

(ert-deftest drover-remote-push-test-persistence ()
  "Phones and their counters survive a restart; a file that does not read stays."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'done))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-test--register phone)
      (should (drover-remote-push-test--pushes-after 0))
      (let* ((reg (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state)))
             (seq (drover-remote-push--reg-seq reg)))
        (drover-remote-push-stop)
        (drover-remote-push-start)
        (let ((again (gethash "adept" (drover-remote-push--state-regs drover-remote-push--state))))
          (should again)
          (should (> (drover-remote-push--reg-seq again) seq))
          (should (equal (drover-remote-push--reg-key again) drover-remote-push-test--key))
          (should (= (hash-table-count (drover-remote-push--reg-shown again)) 1))))
      (unless (eq system-type 'windows-nt)
        (should (= (file-modes (drover-remote-push--file)) #o600)))
      (drover-remote-push-stop)
      (with-temp-file (drover-remote-push--file) (insert "(:version 1 :registrations (garbage"))
      (let ((drover-error-interval 0))
        (drover-remote-push-start))
      (should (drover-remote-push--state-unreadable drover-remote-push--state))
      (drover-remote-push-test--register phone)
      (should (equal (with-temp-buffer (insert-file-contents (drover-remote-push--file)) (buffer-string))
                     "(:version 1 :registrations (garbage")))))

(ert-deftest drover-remote-push-test-restart-tells-what-changed ()
  "A restart tells each phone what changed while pushes stopped; a start does not.
What changes while the remote layer restarts never reaches the hook, as
the requests a restart answers in between: the phone kept the
notification of a session no longer needing the user.  As Emacs starts,
the index fills as the backends report, and nothing goes until a
session's need changes."
  (drover-remote-push-test-with (list (drover-test-session "a" :status 'done))
    (let ((phone (drover-remote-push-test--peer "adept" :viewer t)))
      (drover-remote-push-test--register phone)
      (should (drover-remote-push-test--pushes-after 0))
      (should (equal (plist-get (drover-remote-push-test--last) :r) "need"))
      ;; Stopped, a change goes unseen; started as Emacs starts, it stays so.
      (drover-remote-push-stop)
      (drover-remote-push-test--change (list (drover-test-session "a" :status 'idle)))
      (drover-remote-push-start)
      (drover-remote-push-test--settle nil 0.5)
      (should (= (length drover-remote-push-test--sent) 1))
      ;; Restarted, the phone is told.
      (drover-remote-push-stop)
      (drover-remote-push-start t)
      (should (drover-remote-push-test--pushes-after 1))
      (should (equal (plist-get (drover-remote-push-test--last) :r) "clear"))
      ;; Nothing changed since: restarting again pushes nothing.
      (drover-remote-push-stop)
      (drover-remote-push-start t)
      (drover-remote-push-test--settle nil 0.5)
      (should (= (length drover-remote-push-test--sent) 2)))))

(ert-deftest drover-remote-push-test-curl ()
  "Curl posts the body with the token to a server, telling its status."
  (skip-unless (and (drover-remote-push-available-p) (executable-find drover-remote-push-curl)))
  (let* ((requests nil)
         (status "200 OK")
         (server (make-network-process
                  :name "drover-push-test-server" :server t :host "127.0.0.1" :service t
                  :family 'ipv4 :coding 'binary :noquery t
                  :filter (lambda (process text)
                            (push text requests)
                            (when (string-match-p "dp1\\." text)
                              (process-send-string process (format "HTTP/1.1 %s\r\nContent-Length: 0\r\nConnection: close\r\n\r\n" status))
                              (delete-process process)))))
         (port (process-contact server :service))
         (answer nil))
    (unwind-protect
        (progn
          (drover-remote-push--curl (format "http://127.0.0.1:%d/upTopic?up=1" port) "tk_secret" "dp1.k1.AAAA"
                                    (lambda (code &optional why) (setq answer (list code why))))
          (drover-remote-push-test--settle (lambda () answer) 20)
          (should (equal answer '(200 nil)))
          (let ((request (apply #'concat (reverse requests))))
            (should (string-match-p "\\`POST /upTopic\\?up=1 " request))
            (should (string-match-p "Authorization: Bearer tk_secret" request))
            (should (string-match-p "dp1\\.k1\\.AAAA" request)))
          (setq answer nil requests nil status "404 Not Found")
          (drover-remote-push--curl (format "http://127.0.0.1:%d/upGone" port) nil "dp1.k1.BBBB"
                                    (lambda (code &optional why) (setq answer (list code why))))
          (drover-remote-push-test--settle (lambda () answer) 20)
          (should (equal answer '(404 nil)))
          (should-not (string-match-p "Authorization" (apply #'concat requests))))
      (delete-process server))))

(ert-deftest drover-remote-push-test-prints-in-proportion ()
  "What pushes keep prints in proportion, whatever the phones and sessions."
  (drover-remote-push-test-with (mapcar (lambda (i) (drover-test-session (format "s%d" i) :status 'done))
                                        (number-sequence 1 200))
    (dotimes (i 8)
      (drover-remote-push-test--register (drover-remote-push-test--peer (format "p%d" i) :viewer t)))
    (drover-remote-push-test--settle nil 0.5)
    (should (< (length (prin1-to-string drover-remote-push--state)) 200000))
    ;; Its timers reach the state through links, never whole.
    (drover-remote-push-test--change (list (drover-test-session "s1" :status 'failed)))
    (dolist (timer (list (drover-remote-push--state-timer drover-remote-push--state)
                         (drover-remote-push--state-idle drover-remote-push--state)))
      (when timer
        (should (< (length (prin1-to-string timer)) 2000))))))

(provide 'drover-remote-push-test)
;;; drover-remote-push-test.el ends here
