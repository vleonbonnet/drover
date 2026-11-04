;;; drover-remote-test.el --- Tests of drover across hosts  -*- lexical-binding: t; -*-

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

;; Records on the wire, the owner's revisions, the choice of a
;; connection, and two batch Emacs through 127.0.0.1: this one, the
;; host `viewer', and an agent (test/drover-remote-agent.el), the host
;; `agent', each serving fake sessions.  Skipped without a spyglass with
;; channels on the load path (SPYGLASS_EL in the Makefile).

;;; Code:

(require 'drover-test-helpers)
(require 'drover-remote)
(require 'drover-links)
(require 'drover-list)
(require 'drover-prompt)
(require 'spyglass nil t)

(defun drover-remote-test--available-p ()
  "Return non-nil when spyglass, with its channels and `spyglass-reconnect', is loaded."
  (and (featurep 'spyglass-channel) (fboundp 'spyglass-reconnect)))

;;;; Waiting

(defun drover-remote-test-step ()
  "Process events until the next timer is due, 10 ms at most."
  (let* ((timer (car timer-list))
         (until (and timer (timer-until timer nil))))
    (accept-process-output nil (max 0.001 (min 0.01 (or until 0.01))))))

(defun drover-remote-test-wait (predicate &optional timeout)
  "Process events until PREDICATE returns non-nil or TIMEOUT seconds pass.
Return PREDICATE's last value; TIMEOUT defaults to 20 seconds."
  (let ((end (+ (float-time) (or timeout 20))) value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) end))
      (drover-remote-test-step))
    value))

(defun drover-remote-test-settle (&optional seconds)
  "Process events for SECONDS, 0.2 by default."
  (let ((end (+ (float-time) (or seconds 0.2))))
    (while (< (float-time) end) (drover-remote-test-step))))

;;;; The agent

(defvar drover-remote-test-agent-name "agent"
  "The host name of the agent.")

(defvar drover-remote-test--agent nil
  "The process of the agent, or nil.")

(defvar drover-remote-test--agent-output nil
  "What the agent printed, newest first, as lines.")

(defvar drover-remote-test--agent-pending ""
  "What the agent printed after its last full line.")

(defun drover-remote-test--agent-filter (_process output)
  "Keep the lines of OUTPUT, from the agent."
  (let ((text (concat drover-remote-test--agent-pending output)))
    (while (string-match "\n" text)
      (push (string-trim-right (substring text 0 (match-beginning 0)) "\r")
            drover-remote-test--agent-output)
      (setq text (substring text (match-end 0))))
    (setq drover-remote-test--agent-pending text)))

(defun drover-remote-test--load-path ()
  "Return the -L arguments the agent needs: drover, its tests, opencode.el, spyglass."
  (mapcan (lambda (library) (list "-L" (file-name-directory (locate-library library))))
          '("drover-core" "drover-remote-agent" "opencode-look" "spyglass")))

(defun drover-remote-test-start-agent (dir &optional host peers)
  "Start the agent as HOST, \"agent\" by default, in DIR's \"agent/\", knowing PEERS.
PEERS is a comma-separated list of the names of hosts it knows.
Return the agent's registration file once it listens."
  (setq dir (file-name-as-directory (expand-file-name "agent" dir)))
  (make-directory dir t)
  (setq drover-remote-test--agent-output nil
        drover-remote-test--agent-pending "")
  (let ((process-environment (append (list (concat "DROVER_AGENT_DIR=" dir)
                                           (concat "DROVER_AGENT_HOST=" (or host "agent"))
                                           (concat "DROVER_AGENT_PEERS=" (or peers "")))
                                     process-environment)))
    (setq drover-remote-test--agent
          (make-process
           :name "drover-agent" :connection-type 'pipe :noquery t
           :command (list "sh" "-c"
                          (concat "trap '' PIPE; exec timeout 300 "
                                  (mapconcat #'shell-quote-argument
                                             (append (list (expand-file-name invocation-name
                                                                             invocation-directory)
                                                           "-Q" "--batch")
                                                     (drover-remote-test--load-path)
                                                     (list "--eval" "(setq load-prefer-newer t)"
                                                           "-l" "drover-remote-agent"
                                                           "-f" "drover-remote-agent-main"))
                                             " ")))
           :filter #'drover-remote-test--agent-filter)))
  (drover-remote-test-wait (lambda () (or (seq-some (lambda (line) (string-prefix-p "DROVER-AGENT READY" line))
                                                    drover-remote-test--agent-output)
                                          (not (process-live-p drover-remote-test--agent))))
                           60)
  (unless (seq-some (lambda (line) (string-prefix-p "DROVER-AGENT READY" line))
                    drover-remote-test--agent-output)
    (error "The agent did not start: %S" (seq-take drover-remote-test--agent-output 20)))
  (expand-file-name "server.json" dir))

(defun drover-remote-test-stop-agent ()
  "Stop the agent, by force if it does not quit.
With DROVER_REMOTE_TEST_DEBUG set, print what it printed."
  (when (getenv "DROVER_REMOTE_TEST_DEBUG")
    (message "agent said:\n%s" (string-join (reverse drover-remote-test--agent-output) "\n")))
  (when (process-live-p drover-remote-test--agent)
    (when (and (drover-remote--current) (drover-remote-test--agent-connection))
      (ignore-errors (drover-remote-test-command "quit"))
      (drover-remote-test-wait (lambda () (not (process-live-p drover-remote-test--agent))) 5))
    (when (process-live-p drover-remote-test--agent)
      (delete-process drover-remote-test--agent)))
  (setq drover-remote-test--agent nil))

(defun drover-remote-test--agent-connection ()
  "Return a live connection to the agent's Emacs."
  (when-let* ((peer (drover-remote--peer drover-remote-test-agent-name)))
    (or (drover-remote--peer-connection peer)
        (seq-find #'spyglass-connection-live-p
                  (mapcar #'car (drover-remote--peer-candidates peer))))))

(defun drover-remote-test-command (op &rest args)
  "Have the agent run OP with ARGS, a plist; return its answer."
  (drover-remote-test-wait #'drover-remote-test--agent-connection 20)
  (let ((connection (or (drover-remote-test--agent-connection) (error "Not connected to the agent")))
        (answer nil))
    (spyglass-channel-request connection "drover-test" (append (list :op op) args)
                              (lambda (reply error) (setq answer (list :reply reply :error error)))
                              60)
    (drover-remote-test-wait (lambda () answer) 60)
    (when (plist-get answer :error)
      (error "Agent: %s" (plist-get answer :error)))
    (plist-get answer :reply)))

;;;; This host

(defmacro drover-remote-test-with (sessions &rest body)
  "Run BODY as the host `viewer' serving SESSIONS of the fake backend.
`dir' is a fresh directory, deleted after, with this host's spyglass
registration; the agent, if BODY starts one, is stopped after."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (drover-remote-test--available-p))
     (drover-test-with-backend ,sessions
       (let* ((dir (file-name-as-directory (make-temp-file "drover-remote-test" t)))
              (spyglass-registration-file (expand-file-name "server.json" dir))
              (drover-host "viewer")
              (drover-host-aliases nil)
              (drover-refresh-delay 0.02)
              (drover-remote-standby-delay 1)
              (drover-remote-heartbeat '(1 . 4))
              (drover-remote-accept-unknown nil)
              (drover-remote-peers nil)
              (drover-group-by nil)
              (spyglass-error-functions
               (list (lambda (_connection message) (message "spyglass: %s" message)))))
         (unwind-protect
             (progn ,@body)
           (drover-remote-test-stop-agent)
           (drover-remote-mode -1)
           (dolist (connection (spyglass-connections))
             (ignore-errors (spyglass-disconnect connection)))
           (spyglass-server-stop)
           (drover-remote-test-settle 0.05)
           (delete-directory dir t))))))

(defun drover-remote-test-connect (registration &rest properties)
  "Dial the agent through REGISTRATION, a file, with PROPERTIES; wait for it."
  (setq drover-remote-peers `(("agent" :dial t :local ,registration ,@properties)))
  (drover-remote-mode 1)
  (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open)))
  (should (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open)))

(defun drover-remote-test-remote (&optional host)
  "Return the sessions of HOST, \"agent\" by default, in this host's index, by id."
  (let ((host (or host drover-remote-test-agent-name)))
    (seq-sort-by (lambda (session) (format "%s" (cdr (drover-session-id session)))) #'string<
                 (seq-filter (lambda (session) (equal (drover-session-host session) host))
                             (drover-index-sessions)))))

(defun drover-remote-test-synced (count &optional predicate)
  "Wait until the index holds COUNT sessions of the agent, all satisfying PREDICATE."
  (drover-remote-test-wait
   (lambda ()
     (drover-changed-now)
     (let ((sessions (drover-remote-test-remote)))
       (and (= (length sessions) count)
            (or (null predicate) (seq-every-p predicate sessions))))))
  (should (= (length (drover-remote-test-remote)) count)))

(defun drover-remote-test-stats (&optional name)
  "Return this host's measures of the peer NAME, \"agent\" by default."
  (drover-remote-peer-stats (or name drover-remote-test-agent-name)))

;;;; Records on the wire

(defun drover-remote-test--roundtrip (record &optional key parent offset)
  "Return RECORD encoded, through JSON, and decoded as the host `core' would."
  (let* ((key (or key (drover-session-key record)))
         (bytes (spyglass-channel-encode (drover-remote-encode-record key record parent))))
    (drover-remote-decode-record (spyglass-channel-decode bytes) "core" 'remote:core (or offset 0))))

(ert-deftest drover-remote-test-record-roundtrip ()
  "Every slot travels, faces kept and sanitized, times moved by the clock offset."
  (skip-unless (drover-remote-test--available-p))
  (let* ((agent (propertize "plan" 'face '(:foreground "red") 'display '(image :file "/x")))
         (record (drover-session-create
                  :backend 'opencode :id "ses_1" :title "Fix it" :directory "/work/" :project "work"
                  :branch "dev" :parent "ses_0" :status 'waiting
                  :requests '((:kind question :summary "Which?") (:kind permission :summary "Edit"))
                  :since 100.0 :updated 200.5 :created 50 :agent "plan" :model "Opus 5.5"
                  :effort "high" :effort-default t :provider (propertize "P" 'face 'bold)
                  :provider-name "Prov" :context-tokens 5000 :context-limit 200000
                  :context-tier '(near 200000 "2×") :detail "Busy\nreally" :engine 'opencode
                  :engine-id "ses_1" :cache-expires 400.0 :cache-ttl 300 :cache-tokens 10
                  :cache-prompt-tokens 20 :agent-display agent :context-estimated t :archived 300.0))
         (decoded (drover-remote-test--roundtrip record nil '(opencode . "ses_0") 10)))
    (should (equal (drover-session-id decoded) '(opencode . "ses_1")))
    (should (eq (drover-session-backend decoded) 'remote:core))
    (should (equal (drover-session-host decoded) "core"))
    (should (eq (drover-session-origin decoded) 'opencode))
    (should (equal (drover-session-parent decoded) '(opencode . "ses_0")))
    (should (equal (drover-session-key decoded) '(remote:core opencode . "ses_1")))
    (should (equal (drover-session-engine-key decoded) '("core" opencode . "ses_1")))
    (dolist (slot '(title directory project branch status requests agent model effort effort-default
                          provider-name context-tokens context-limit context-tier detail engine
                          engine-id cache-ttl cache-tokens cache-prompt-tokens context-estimated))
      (should (equal (cl-struct-slot-value 'drover-session slot decoded)
                     (cl-struct-slot-value 'drover-session slot record))))
    ;; The owner's clock is 10 s ahead of this host's.
    (should (= (drover-session-since decoded) 90.0))
    (should (= (drover-session-updated decoded) 190.5))
    (should (= (drover-session-created decoded) 40))
    (should (= (drover-session-cache-expires decoded) 390.0))
    (should (= (drover-session-archived decoded) 290.0))
    (should (equal-including-properties (drover-session-provider decoded) (propertize "P" 'face 'bold)))
    ;; The face stays, the display that would show an image does not.
    (should (equal (get-text-property 0 'face (drover-session-agent-display decoded)) '(:foreground "red")))
    (should-not (get-text-property 0 'display (drover-session-agent-display decoded)))
    ;; Without a parent listed, the record's own, in its backend.
    (should (equal (drover-session-parent (drover-remote-test--roundtrip record)) '(opencode . "ses_0")))))

(ert-deftest drover-remote-test-owner-reads-its-files ()
  "The owner tells the default branch and names the files changed: the viewer reads nothing of its own."
  (skip-unless (drover-remote-test--available-p))
  (let* ((root (file-name-as-directory (make-temp-file "drover-repo" t)))
         (drover--default-branches (make-hash-table :test #'equal))
         (looked nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git/refs/remotes/origin" root) t)
          (with-temp-file (expand-file-name ".git/refs/remotes/origin/HEAD" root)
            (insert "ref: refs/remotes/origin/develop\n"))
          (let* ((record (drover-session-create :backend 'opencode :id "s" :directory root :branch "develop"))
                 (encoded (drover-remote-encode-record '(opencode . "s") record)))
            (should (equal (plist-get encoded :default-branch) "develop"))
            ;; Without a branch, no default to tell.
            (should-not (plist-member (drover-remote-encode-record
                                       '(opencode . "s") (drover-session-create :directory root))
                                      :default-branch))
            ;; The viewer takes it, and never looks at its own directory of
            ;; that name, here the same one.
            (let ((bytes (spyglass-channel-encode encoded)))
              (advice-add 'drover--read-git-default-branch :before (lambda (directory) (push directory looked))
                          '((name . drover-remote-test-looked)))
              (clrhash drover--default-branches)
              (let ((decoded (drover-remote-decode-record (spyglass-channel-decode bytes) "core" 'remote:core 0)))
                (should (equal (drover-session-default-branch decoded) "develop"))
                (should (drover-session-default-branch-p decoded))
                (setf (drover-session-branch decoded) "main")
                (should-not (drover-session-default-branch-p decoded))
                (should-not looked)))))
      (advice-remove 'drover--read-git-default-branch 'drover-remote-test-looked)
      (delete-directory root t)))
  ;; Files changed are named relative to the session's directory there.
  (should (equal (plist-get (drover-remote--encode-recap
                             '(:changes (("/work/p/a.el" 1 2) ("/elsewhere/b.el" nil nil))) "/work/p/")
                            :changes)
                 [["a.el" 1 2] ["/elsewhere/b.el" :false :false]])))

(ert-deftest drover-remote-test-hostile-records ()
  "Records received are checked and bounded, whatever they hold, and never read."
  (skip-unless (drover-remote-test--available-p))
  (let ((huge (make-string 500000 ?x)))
    (cl-flet ((decode (data) (drover-remote-decode-record data "core" 'remote:core 0)))
      (let ((record (decode (list :k ["opencode" "s"] :title (concat huge "\nsecond")
                                  :detail (mapconcat #'identity (make-list 100 huge) "\n")
                                  :requests (vconcat (make-list 50 (list :kind "question" :summary huge)))
                                  :agent-display (list :S huge :p [[0 10 [(:y "face") (:y "bold")]]])))))
        (should (<= (length (drover-session-title record)) (1+ drover-line-limit)))
        (should (<= (length (drover-session-detail record)) (1+ drover-text-limit)))
        (should (= (length (drover-session-requests record)) 20))
        (should (<= (length (plist-get (car (drover-session-requests record)) :summary))
                    (1+ drover-text-limit)))
        (should (<= (length (drover-session-agent-display record)) (1+ drover-line-limit))))
      ;; Wrong types are dropped, unknown statuses idle, unknown members ignored.
      (let ((record (decode (list :k ["opencode" "s"] :title 42 :status "exploded" :since "yesterday"
                                  :context-tokens -5 :engine "Bad Engine!" :requests "no"
                                  :context-tier [1 2] :unknown (list :a 1)))))
        (should (eq (drover-session-status record) 'idle))
        (should-not (drover-session-title record))
        (should-not (drover-session-since record))
        (should-not (drover-session-context-tokens record))
        (should-not (drover-session-engine record))
        (should-not (drover-session-requests record))
        (should-not (drover-session-context-tier record)))
      ;; No key, or one naming no backend, is no record; nothing is interned
      ;; from a name that is not a short symbol.
      (should-not (decode (list :title "x")))
      (should-not (decode (list :k ["(eval evil)" "s"])))
      (should-not (decode (list :k ["opencode" (list :y "s")])))
      (should-not (decode "not an object"))
      (should-not (intern-soft "(eval evil)"))
      ;; Deep nesting decodes to nothing harmful.
      (let ((deep "x"))
        (dotimes (_ 200) (setq deep (list :c (vector deep deep))))
        (should (decode (list :k ["opencode" "s"] :provider deep)))))))

;;;; The owner's revisions

(defmacro drover-remote-test-owning (sessions &rest body)
  "Run BODY with this host serving SESSIONS, its messages to peers recorded.
`sent' holds the messages sent on fake connections, newest first, as
\(CONNECTION . MESSAGE); `state' is the remote layer's state."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (drover-remote-test--available-p))
     (drover-test-with-backend ,sessions
       (let* ((drover-remote--state (drover-remote--state-make :epoch "e1"))
              (state drover-remote--state)
              (sent nil)
              (drover-changed-hook (list #'drover-remote--owner-changed)))
         (drover-index)
         (cl-letf (((symbol-function 'spyglass-connection-live-p) (lambda (_) t))
                   ((symbol-function 'spyglass-channel-send)
                    (lambda (connection _channel message) (push (cons connection message) sent) t)))
           ,@body)))))

(defun drover-remote-test--peer (state name)
  "Return a peer NAME in STATE, without a backend."
  (puthash name (drover-remote--peer-make :name name :records (make-hash-table :test #'equal))
           (drover-remote--state-peers state)))

(ert-deftest drover-remote-test-revisions ()
  "A snapshot, then updates of what changed, and the changes since a revision held."
  (drover-remote-test-owning (list (drover-test-session "a") (drover-test-session "b"))
    (let ((peer (drover-remote-test--peer state "core"))
          (connection 'c1))
      (drover-remote--subscribe state connection peer (list :epoch nil :rev nil))
      (let* ((sub (gethash connection (drover-remote--state-subs state))))
        (drover-remote--flush sub)
        (let ((snap (cdar sent)))
          (should (equal (plist-get snap :t) "snap"))
          (should (eq (plist-get snap :first) t))
          (should (eq (plist-get snap :last) t))
          (should (= (length (plist-get snap :recs)) 2))
          (should (= (plist-get snap :to) 2)))
        ;; A change, coalesced while the snapshot is not acknowledged.
        (setf (drover-session-title (car drover-test-sessions)) "A1")
        (drover-backend-changed 'fake "a") (drover-changed-now)
        (setq drover-test-sessions (cdr drover-test-sessions))
        (setcar drover-test-sessions (copy-sequence (car drover-test-sessions)))
        (setf (drover-session-title (car drover-test-sessions)) "B1")
        (drover-backend-changed 'fake "a") (drover-backend-changed 'fake "b") (drover-changed-now)
        (should (= (length sent) 1))
        (drover-remote--acked state connection (list :n 1))
        (let ((up (cdar sent)))
          (should (equal (plist-get up :t) "up"))
          (should (= (plist-get up :from) 2))
          (should (equal (append (plist-get up :gone) nil) '(["fake" "a"])))
          (should (equal (mapcar (lambda (r) (plist-get r :title)) (plist-get up :recs)) '("B1")))
          (should (= (plist-get up :to) (drover-remote--state-rev state))))
        (should (= (hash-table-count (drover-remote--state-tombs state)) 1))
        ;; Coming back at revision 2: the changes since, not a snapshot.
        (setq sent nil)
        (drover-remote--subscribe state 'c2 peer (list :epoch "e1" :rev 2))
        (should-not (gethash connection (drover-remote--state-subs state)))
        (drover-remote--flush (gethash 'c2 (drover-remote--state-subs state)))
        (let ((up (cdar sent)))
          (should (equal (plist-get up :t) "up"))
          (should (= (plist-get up :from) 2))
          (should (= (length (plist-get up :recs)) 1))
          (should (= (length (plist-get up :gone)) 1)))
        ;; Another epoch, or a revision before the tombstones: a snapshot.
        (setq sent nil)
        (drover-remote--subscribe state 'c3 peer (list :epoch "e0" :rev 2))
        (drover-remote--flush (gethash 'c3 (drover-remote--state-subs state)))
        (should (equal (plist-get (cdar sent) :t) "snap"))
        (setq sent nil)
        (setf (drover-remote--state-floor state) 3)
        (drover-remote--subscribe state 'c4 peer (list :epoch "e1" :rev 2))
        (drover-remote--flush (gethash 'c4 (drover-remote--state-subs state)))
        (should (equal (plist-get (cdar sent) :t) "snap"))))))

(ert-deftest drover-remote-test-tombstones-bounded ()
  "Tombstones are kept for the last sessions gone only, the floor rising past them."
  (drover-remote-test-owning nil
    (let ((drover-remote-tombstones 3))
      (dotimes (i 5) (drover-remote--tomb state (cons 'fake (format "%d" i)) (1+ i)))
      (should (= (drover-remote--state-tomb-count state) 3))
      (should (= (hash-table-count (drover-remote--state-tombs state)) 3))
      (should (= (drover-remote--state-floor state) 2)))))

(ert-deftest drover-remote-test-lag-coalesces ()
  "A peer not acknowledging gets one update of a session changed many times."
  (drover-remote-test-owning (list (drover-test-session "a"))
    (let ((peer (drover-remote-test--peer state "core")))
      (drover-remote--subscribe state 'c1 peer nil)
      (let ((sub (gethash 'c1 (drover-remote--state-subs state))))
        (drover-remote--flush sub)
        (dotimes (i 500)
          (setf (car drover-test-sessions) (copy-sequence (car drover-test-sessions)))
          (setf (drover-session-title (car drover-test-sessions)) (format "T%d" i))
          (drover-backend-changed 'fake "a")
          (drover-changed-now)
          (drover-remote--flush sub))
        (should (= (length sent) 1))
        (should (= (hash-table-count (drover-remote--sub-dirty sub)) 1))
        (drover-remote--acked state 'c1 (list :n 1))
        (should (= (length sent) 2))
        (should (equal (plist-get (aref (plist-get (cdar sent) :recs) 0) :title) "T499"))))))

(ert-deftest drover-remote-test-serves-its-own-only ()
  "A host serves its own backends' sessions: never those of a peer, a conversation once."
  (drover-remote-test-owning (list (drover-test-session "a" :engine 'fake :engine-id "x"))
    (drover-test-register-simple 'remote:core
                                 (list (drover-session-create :backend 'remote:core :id '(fake . "z")
                                                              :host "core" :title "theirs"))
                                 :remote "core")
    (drover-test-register-simple 'other
                                 (list (drover-test-session "b" :backend 'other
                                                            :engine 'fake :engine-id "x")))
    (setq drover--index nil)
    (drover-index)
    (drover-remote--sync-all state)
    (should (equal (hash-table-keys (drover-remote--state-sent state)) '((fake . "a"))))))

;;;; Choosing a connection

(ert-deftest drover-remote-test-election ()
  "Of two connections of one Emacs, the one dialed by the host sorting first is kept."
  (skip-unless (drover-remote-test--available-p))
  (let ((used nil) (closed nil) (refused nil))
    (cl-letf (((symbol-function 'spyglass-connection-live-p) (lambda (c) (not (memq c closed))))
              ((symbol-function 'spyglass-disconnect) (lambda (c) (push c closed)))
              ((symbol-function 'drover-remote--use)
               (lambda (_state peer connection) (setf (drover-remote--peer-connection peer) connection)
                 (push connection used)))
              ((symbol-function 'drover-remote--refuse) (lambda (c why &rest _) (push (cons c why) refused))))
      (dolist (case '(("reaver" "core" ours theirs) ("core" "reaver" theirs ours)))
        (pcase-let ((`(,me ,them ,first ,second) case))
          (let* ((drover-host me)
                 (state (drover-remote--state-make))
                 (peer (drover-remote-test--peer state them)))
            (setq used nil closed nil refused nil)
            (setf (drover-remote--peer-dialed peer) 'ours
                  (drover-remote--peer-candidates peer)
                  (list (cons first (list :inst "i1" :ours (eq first 'ours) :time 1.0))))
            (drover-remote--elect state peer)
            (should (eq (drover-remote--peer-connection peer) first))
            (push (cons second (list :inst "i1" :ours (eq second 'ours) :time 2.0))
                  (drover-remote--peer-candidates peer))
            (drover-remote--elect state peer)
            ;; core sorts first: the connection core dialed is kept.
            (let ((kept (if (string< me them) 'ours 'theirs)))
              (should (eq (drover-remote--peer-connection peer) kept))
              (should (equal closed (if (eq kept 'ours) nil '(ours)))))
            ;; Another Emacs of the same host is refused while one is kept.
            (push (cons 'other (list :inst "i2" :ours nil :time 3.0)) (drover-remote--peer-candidates peer))
            (drover-remote--elect state peer)
            (should (assq 'other refused))
            (should-not (eq (drover-remote--peer-connection peer) 'other))))))))

;;;; Two Emacs

(ert-deftest drover-remote-test-registered-anew ()
  "Its file loaded anew, as reloading drover does, the peers' backends take its properties.
Their sessions stay, as received."
  (drover-remote-test-with nil
    (let ((registration (drover-remote-test-start-agent dir)))
      (drover-remote-test-connect registration)
      (drover-remote-test-command "sessions" :n 3)
      (drover-remote-test-synced 3)
      (let ((sessions (drover-backend-property 'remote:agent :sessions)))
        ;; As an older drover registered it.
        (drover-register-backend 'remote:agent :label "agent" :remote "agent" :sessions sessions)
        (should-not (drover-backend-property 'remote:agent :recap))
        (load "drover-remote" nil t)
        (should (drover-backend-property 'remote:agent :recap))
        (should (drover-backend-property 'remote:agent :reports-changes))
        (drover-remote-test-synced 3)))))

(ert-deftest drover-remote-test-snapshot-and-updates ()
  "Another Emacs's sessions arrive, subagents under their parents, and follow it."
  (drover-remote-test-with nil
    (let ((registration (drover-remote-test-start-agent dir)))
      (drover-remote-test-connect registration)
      (drover-remote-test-command "sessions" :n 30 :children 2)
      (drover-remote-test-synced 90)
      (let ((child (drover-index-session '(remote:agent fake . "s3-1"))))
        (should child)
        (should (equal (drover-session-host child) "agent"))
        (should (equal (drover-session-title child) "Subagent s3-1"))
        (should (equal (drover-index-parent '(remote:agent fake . "s3-1")) '(remote:agent fake . "s3"))))
      (should (= (plist-get (drover-remote-test-stats) :snapshots) 1))
      ;; Changes, removals and additions follow, as updates.
      (drover-remote-test-command "change" :ids ["s1" "s2"] :title "Renamed" :status "failed")
      (drover-remote-test-command "remove" :ids ["s5-0" "s5-1"])
      (drover-remote-test-command "add" :ids ["new"] :title "Brand new")
      (drover-remote-test-synced 89 (lambda (_) (equal (drover-session-title
                                                        (drover-index-session '(remote:agent fake . "s1")))
                                                       "Renamed")))
      (should (eq (drover-session-status (drover-index-session '(remote:agent fake . "s2"))) 'failed))
      (should-not (drover-index-session '(remote:agent fake . "s5-0")))
      (should (equal (drover-session-title (drover-index-session '(remote:agent fake . "new"))) "Brand new"))
      (should (= (plist-get (drover-remote-test-stats) :snapshots) 1))
      ;; What the agent holds, this host holds.
      (let ((titles (plist-get (drover-remote-test-command "titles") :titles)))
        (seq-doseq (entry titles)
          (let ((session (drover-index-session (cons 'remote:agent (cons 'fake (aref entry 0))))))
            (should session)
            (should (equal (drover-session-title session) (aref entry 1)))
            (should (equal (symbol-name (drover-session-status session)) (aref entry 2))))))
      ;; Reporting every change, branches too, its sessions are neither
      ;; listed again by the minute's refresh nor their branches read
      ;; here: their directories are on the agent.
      (should (drover-backend-property 'remote:agent :reports-changes))
      (let ((read nil) (ran nil))
        (cl-letf (((symbol-function 'drover-git-branch)
                   (lambda (directory) (push directory read) "elsewhere")))
          (let ((drover-changed-hook (list (lambda () (push drover-changed-sessions ran)))))
            (drover-index-refresh)))
        (should-not read)
        (should-not ran)
        (should (equal (drover-session-branch (drover-index-session '(remote:agent fake . "s1"))) "main")))
      ;; How the peer stands.
      (save-window-excursion
        (drover-remote-status)
        (with-current-buffer "*drover remote*"
          (should (string-match-p "connected" (buffer-string)))
          (should (string-match-p "89 sessions" (buffer-string)))
          (kill-buffer)))
      ;; Its sessions need the user here too, while it is connected.
      (should (seq-some (lambda (session) (equal (drover-session-host session) "agent"))
                        (drover-attention-sessions))))))

(ert-deftest drover-remote-test-recap-and-preview ()
  "Recaps and previews of another host's sessions come through it."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 3)
    (drover-remote-test-synced 3)
    (let ((session (drover-index-session '(remote:agent fake . "s1")))
          (recap nil) (preview nil))
      (drover-recap session (lambda (value) (setq recap value)))
      (drover-preview session (lambda (value) (setq preview value)))
      (drover-remote-test-wait (lambda () (and recap preview)))
      (should (equal recap '(:prompt "Prompt of s1" :reply "Reply of s1" :changes (("a.el" 3 1)))))
                             (should (string-prefix-p "# Preview of s1\n" preview))
      ;; Opening it shows what its host tells.
      (save-window-excursion
        (drover-open session)
        (should (derived-mode-p 'drover-remote-session-mode))
        (should (equal (drover-session-id (drover-session-at-point)) '(fake . "s1")))
        (drover-remote-test-wait (lambda () (save-excursion (goto-char (point-min))
                                                            (search-forward "Prompt of s1" nil t))))
        (should (save-excursion (goto-char (point-min)) (search-forward "Preview of s1" nil t)))
        (kill-buffer))
      ;; An unknown session is refused by its host, on one line.
      (let ((ghost (copy-sequence session)) (answer nil))
        (setf (drover-session-id ghost) '(fake . "ghost"))
        (drover-recap ghost (lambda (value) (setq answer value)))
        (drover-remote-test-wait (lambda () answer))
        (should (string-match-p "no such session" (plist-get answer :failure))))
      ;; A link to a conversation of the agent shows it.
      (save-window-excursion
        (drover-links-open (list :engine 'fake :id "s2" :host "agent"))
        (should (equal (drover-session-id (drover-session-at-point)) '(fake . "s2")))
        (kill-buffer)))))

(ert-deftest drover-remote-test-older-and-archived ()
  "Older and archived sessions of another host come through it, while shown."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-synced 2)
    (cl-flet ((listed (id) (seq-find (lambda (session) (equal (drover-session-id session) (cons 'fake id)))
                                     (drover-known-sessions))))
      (drover-set-show-history t)
      (drover-remote-test-wait (lambda () (listed "old2")))
      (should (equal (drover-session-host (listed "old1")) "agent"))
      (should (equal (drover-session-title (listed "old2")) "Session old2"))
      (drover-set-show-archived t)
      (drover-remote-test-wait (lambda () (listed "gone1")))
      (should (= (drover-session-archived (listed "gone1"))
                 (- 1700000000.0 (or (spyglass-connection-clock-offset
                                      (drover-remote--peer-connection (drover-remote--peer "agent")))
                                     0))))
      (drover-set-show-history nil)
      (drover-set-show-archived nil)
      (should-not (listed "old1"))
      (should-not (listed "gone1"))
      (should (listed "s1")))))

(ert-deftest drover-remote-test-reconnect-resumes ()
  "A broken stream dims the sessions, keeps them out of the count, and resumes from its revision."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 40)
    (drover-remote-test-synced 40)
    (let ((records (plist-get (drover-remote-test-stats) :records))
          (peer (drover-remote--peer "agent")))
      (spyglass-connection-drop-stream (drover-remote--peer-dialed peer))
      (drover-changed-now)
      ;; At once: stale, dimmed, out of the count, the host saying so.
      (should (memq (drover-remote--peer-state peer) '(reconnecting waiting)))
      (let ((session (drover-index-session '(remote:agent fake . "s3"))))
        (should (drover-session-stale session))
        (should-not (seq-some (lambda (s) (equal (drover-session-host s) "agent"))
                              (drover-attention-sessions)))
        (should (string-match-p "\\`reconnecting" (drover-host-state "agent"))))
      ;; Back: the changes since, not a snapshot.
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)))
      (drover-remote-test-synced 40 (lambda (session) (not (drover-session-stale session))))
      (should (seq-some (lambda (s) (equal (drover-session-host s) "agent")) (drover-attention-sessions)))
      (should (= (plist-get (drover-remote-test-stats) :snapshots) 1))
      (should (= (plist-get (drover-remote-test-stats) :records) records))
      ;; Changes made while the agent's streams are down come as an update.
      (drover-remote-test-command "drop")
      (drover-remote-test-wait (lambda () (not (eq (drover-remote--peer-state peer) 'open))) 5)
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)))
      (drover-remote-test-command "change" :ids ["s1" "s7"] :title "Meanwhile" :now t)
      (drover-remote-test-synced 40 (lambda (_) (equal (drover-session-title
                                                       (drover-index-session '(remote:agent fake . "s7")))
                                                      "Meanwhile")))
      (should (= (plist-get (drover-remote-test-stats) :snapshots) 1))
      (should (<= (plist-get (drover-remote-test-stats) :records) (+ records 2))))))

(ert-deftest drover-remote-test-restart-snapshot ()
  "A host starting anew sends a snapshot; meanwhile its rows stay, stale, with when it was last heard."
  (drover-remote-test-with nil
    (let ((registration (drover-remote-test-start-agent dir)))
      (drover-remote-test-connect registration)
      (drover-remote-test-command "sessions" :n 10 :prefix "old")
      (drover-remote-test-synced 10)
      (drover-remote-test-stop-agent)
      (drover-remote-test-wait (lambda () (drover-remote--peer-stale (drover-remote--peer "agent"))) 10)
      (drover-changed-now)
      (should (= (length (drover-remote-test-remote)) 10))
      (should (seq-every-p #'drover-session-stale (drover-remote-test-remote)))
      (should (string-match-p "last heard from" (drover-host-state "agent")))
      (should (string-match-p "(not connected)" (drover-group-label 'host "agent")))
      ;; The same host, a new Emacs: a snapshot replaces what was held.
      (drover-remote-test-start-agent dir)
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open)))
      (drover-remote-test-command "sessions" :n 5 :prefix "new")
      (drover-remote-test-synced 5 (lambda (session) (and (not (drover-session-stale session))
                                                           (string-prefix-p "Session new"
                                                                            (drover-session-title session)))))
      (should (>= (plist-get (drover-remote-test-stats) :snapshots) 2)))))

(ert-deftest drover-remote-test-both-dial ()
  "When both hosts dial, both keep the connection dialed by the host whose name sorts first."
  (dolist (me '("viewer" "aaa"))
    (drover-remote-test-with nil
      (let* ((drover-host me)
             (registration (drover-remote-test-start-agent dir "agent" me)))
        (drover-remote-test-connect registration)
        (drover-remote-test-command "sessions" :n 5)
        (drover-remote-test-synced 5)
        (drover-remote-test-command "dial" :name me :local spyglass-registration-file)
        (let ((kept (if (string< me "agent") 'dialed 'accepted)))
          (drover-remote-test-wait
           (lambda () (and (eq (plist-get (drover-remote-test-stats) :through) kept)
                           (eq (plist-get (drover-remote-test-stats) :dialed) (eq kept 'dialed))))
           10)
          (should (eq (plist-get (drover-remote-test-stats) :through) kept))
          (should (eq (plist-get (drover-remote-test-stats) :dialed) (eq kept 'dialed)))
          (drover-remote-test-settle 0.5)
          ;; The agent agrees, and serves this host on one connection.
          (let ((theirs (plist-get (drover-remote-test-command "stats" :peer me) :peer)))
            (should (equal (plist-get theirs :through) (if (eq kept 'dialed) "accepted" "dialed")))
            (should (equal (plist-get theirs :state) "open")))
          (should (= (plist-get (plist-get (drover-remote-test-command "stats") :owner) :subscribers) 1)))
        ;; Sessions keep coming, through the connection kept.
        (drover-remote-test-command "change" :ids ["s2"] :title "After" :now t)
        (drover-remote-test-synced 5 (lambda (_) (equal (drover-session-title
                                                        (drover-index-session '(remote:agent fake . "s2")))
                                                       "After")))))))

(ert-deftest drover-remote-test-no-relaying ()
  "Each host sends its own sessions only: none comes back, none goes on."
  (drover-remote-test-with (list (drover-test-session "v1") (drover-test-session "v2"))
    (drover-remote-test-connect (drover-remote-test-start-agent dir "agent" "viewer"))
    (drover-remote-test-command "sessions" :n 4)
    (drover-remote-test-synced 4)
    ;; The agent lists this host's two sessions, as this host's.
    (drover-remote-test-wait (lambda () (= (length (plist-get (drover-remote-test-command "hosts") :remote)) 2)))
    (let ((theirs (plist-get (drover-remote-test-command "hosts") :remote)))
      (should (equal (sort (mapcar (lambda (entry) (aref entry 1)) theirs) #'string<)
                     '("Session v1" "Session v2")))
      (should (seq-every-p (lambda (entry) (equal (aref entry 0) "viewer")) theirs)))
    ;; It serves its own four, not this host's two back.
    (should (= (plist-get (plist-get (drover-remote-test-command "stats") :owner) :served) 4))
    (drover-remote-test-settle 0.3)
    (drover-changed-now)
    (should (= (length (drover-remote-test-remote)) 4))
    (should-not (drover-remote-test-remote "viewer"))
    (should (= (length (seq-filter (lambda (s) (null (drover-session-host s))) (drover-index-sessions))) 2))))

(ert-deftest drover-remote-test-lagging-viewer ()
  "A viewer applying nothing for a while gets a session changed many times once."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 3)
    (drover-remote-test-synced 3)
    (let* ((paused t)
           (hold (lambda (work peer) (unless paused (funcall work peer))))
           (messages (plist-get (drover-remote-test-stats) :messages)))
      (advice-add 'drover-remote--work :around hold)
      (unwind-protect
          (progn
            (drover-remote-test-command "churn" :id "s1" :count 300)
            (drover-remote-test-settle 0.5)
            (let ((serving (plist-get (plist-get (drover-remote-test-command "stats" :peer "viewer") :peer)
                                      :serving)))
              (should (<= (plist-get serving :dirty) 1))))
        (setq paused nil)
        (advice-remove 'drover-remote--work hold))
      (drover-remote--schedule-work (drover-remote--peer "agent"))
      (drover-remote-test-synced 3 (lambda (_) (equal (drover-session-title
                                                       (drover-index-session '(remote:agent fake . "s1")))
                                                      "Churn 299")))
      (should (<= (- (plist-get (drover-remote-test-stats) :messages) messages) 4)))))

(ert-deftest drover-remote-test-bounded-text ()
  "A title of 500 kB arrives as one line, cut; the request carrying it went in chunks."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "add" :ids ["big"] :title (concat (make-string 500000 ?t) "\nsecond line"))
    (drover-remote-test-synced 1)
    (let ((title (drover-session-title (drover-index-session '(remote:agent fake . "big")))))
      (should (<= (length title) (1+ drover-line-limit)))
      (should-not (string-search "\n" title)))
    (should (> (plist-get (spyglass-channel-stats (drover-remote--peer-connection (drover-remote--peer "agent")))
                          :chunks-out)
               0))))

(ert-deftest drover-remote-test-refusals-and-bye ()
  "A host not known is refused; a host turning the mode off takes its sessions away."
  (drover-remote-test-with nil
    (let ((registration (let ((process-environment (cons "DROVER_AGENT_STRICT=1" process-environment)))
                          (drover-remote-test-start-agent dir "agent"))))
      (setq drover-remote-peers `(("agent" :dial t :local ,registration)))
      (drover-remote-mode 1)
      (drover-remote-test-wait (lambda () (drover-remote--peer-error (drover-remote--peer "agent"))) 10)
      (should (string-match-p "agent does not know viewer"
                              (drover-remote--peer-error (drover-remote--peer "agent"))))
      (should-not (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open))
      (drover-remote-test-stop-agent))
    (drover-remote-mode -1)
    (let ((registration (drover-remote-test-start-agent dir "agent")))
      (drover-remote-test-connect registration)
      (drover-remote-test-command "sessions" :n 2)
      (drover-remote-test-synced 2)
      ;; This host dropping the peer from its configuration: its sessions go.
      (setq drover-remote-peers nil)
      (drover-remote-reconfigure)
      (should-not (drover-remote--peers))
      (drover-changed-now)
      (should-not (drover-remote-test-remote))
      (setq drover-remote-peers `(("agent" :dial t :local ,registration)))
      (drover-remote-reconfigure)
      (drover-remote-test-synced 2)
      ;; The agent turning its mode off: its sessions go here too.
      (drover-remote-test-command "quit" :off t)
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'closed)) 10)
      (should (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'closed))
      (drover-changed-now)
      (should-not (drover-remote-test-remote)))))

(ert-deftest drover-remote-test-greet-once ()
  "Each host greets once per stream: a greeting answered is not answered back."
  (drover-remote-test-with nil
    (let ((greetings 0))
      (advice-add 'drover-remote--on-hello :before (lambda (&rest _) (cl-incf greetings))
                  '((name . drover-remote-test-greetings)))
      (unwind-protect
          (progn
            (drover-remote-test-connect (drover-remote-test-start-agent dir))
            (drover-remote-test-settle 1.0)
            ;; One greeting each way, and one more when asked: not
            ;; thousands a second, each answered back.
            (should (<= greetings 2))
            (should (= (plist-get (drover-remote-test-stats) :hellos) greetings))
            (should (<= (plist-get (plist-get (drover-remote-test-command "stats" :peer "viewer") :peer) :hellos)
                        2)))
        (advice-remove 'drover-remote--on-hello 'drover-remote-test-greetings)))))

(ert-deftest drover-remote-test-answers-survive-a-drop ()
  "A request whose stream breaks before its answer is asked again once connected anew."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 3)
    (drover-remote-test-synced 3)
    (let ((session (drover-index-session '(remote:agent fake . "s1")))
          (peer (drover-remote--peer "agent"))
          (recap nil) (preview nil))
      (drover-recap session (lambda (value) (setq recap value)))
      (drover-preview session (lambda (value) (setq preview value)))
      ;; Broken before the agent could answer.
      (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
      (should-not recap)
      (should (= (length (drover-remote--peer-asks peer)) 2))
      (drover-remote-test-wait (lambda () (and recap preview)))
      (should (equal recap '(:prompt "Prompt of s1" :reply "Reply of s1" :changes (("a.el" 3 1)))))
                             (should (string-prefix-p "# Preview of s1\n" preview))
      (should-not (drover-remote--peer-asks peer))
      ;; Asked while not connected, it fails at once; a peer gone for good
      ;; fails what it was to be asked again.
      (let ((answer nil))
        (drover-remote--ask-later peer (list :op "recap") (lambda (_reply error) (setq answer error))
                                  (+ (float-time) 60))
        (drover-remote--close peer "gone")
        (should (string-match-p "closed: gone" answer))))))

(ert-deftest drover-remote-test-older-sessions-after-a-drop ()
  "Older sessions shown are fetched again from a peer connected anew, whose messages a drop lost."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-synced 2)
    (let ((fetches 0)
          (peer (drover-remote--peer "agent")))
      (advice-add 'drover-remote--fetch :before (lambda (&rest _) (cl-incf fetches))
                  '((name . drover-remote-test-fetches)))
      (unwind-protect
          (progn
            (drover-set-show-history t)
            (should (= fetches 1))
            (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
            (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)))
            (should (= fetches 2))
            (drover-remote-test-wait (lambda () (seq-find (lambda (session)
                                                            (equal (drover-session-id session) '(fake . "old2")))
                                                          (drover-known-sessions))))
            (should (seq-find (lambda (session) (equal (drover-session-id session) '(fake . "old2")))
                              (drover-known-sessions))))
        (advice-remove 'drover-remote--fetch 'drover-remote-test-fetches)
        (drover-set-show-history nil)))))

(ert-deftest drover-remote-test-spyglass-errors ()
  "A write failing is quiet, spyglass dropping its stream; another error left open is told once.
Drover breaks no stream itself: spyglass drops a stream whose write
failed, and the subscription made anew resends what it lost."
  (skip-unless (drover-remote-test--available-p))
  (drover-test-with-backend nil
    (let* ((drover-remote--state (drover-remote--state-make :epoch "e1"))
           (state drover-remote--state)
           (peer (drover-remote-test--peer state "core"))
           (live '(c1 c2))
           (broken nil)
           (warnings nil)
           (drover-remote--error-grace 0)
           (drover--errors-reported (make-hash-table :test #'equal)))
      (setf (drover-remote--peer-dialed peer) 'c1)
      (puthash 'c2 (list :peer "core") (drover-remote--state-hellos state))
      (cl-letf (((symbol-function 'spyglass-connection-live-p) (lambda (c) (memq c live)))
                ((symbol-function 'spyglass-connection-drop-stream) (lambda (c) (push c broken)))
                ((symbol-function 'spyglass-reconnect) (lambda (c) (push c broken)))
                ((symbol-function 'drover-warn) (lambda (&rest args) (push (apply #'format args) warnings))))
        ;; The stream ends as the write fails: nothing told.
        (drover-remote--spyglass-error 'c1 "Sending failed: Process spyglass no longer connected to pipe")
        (setq live '(c2))
        (drover-remote--next-stream state 'c1)
        (drover-remote-test-settle 0.05)
        (should-not warnings)
        (should-not (drover-remote--peer-error peer))
        ;; Failing to connect is the peer's last error, no warning.
        (drover-remote--spyglass-error 'c1 "Cannot connect to core: refused")
        (drover-remote-test-settle 0.05)
        (should (equal (drover-remote--peer-error peer) "Cannot connect to core: refused"))
        (should-not warnings)
        ;; A write failing on a stream still open, which spyglass is to
        ;; drop: counted, neither told nor broken by drover.
        (dotimes (_ 3)
          (drover-remote--spyglass-error 'c2 "Sending failed: Connection reset by peer"))
        (drover-remote-test-settle 0.05)
        (should-not warnings)
        (should (= (plist-get (drover-remote--peer-stats peer) :write-failures) 4))
        (should (equal (drover-remote--peer-error peer) "Cannot connect to core: refused"))
        ;; Another error the stream outlives: told once.
        (dotimes (_ 3)
          (drover-remote--spyglass-error 'c2 "Unknown message type \"x\""))
        (drover-remote-test-settle 0.05)
        (should (equal warnings '("core: Unknown message type \"x\"")))
        (should (= (plist-get (drover-remote--peer-stats peer) :unexpected) 3))
        (should-not broken)
        ;; A connection drover does not use is not its own to tell.
        (drover-remote--spyglass-error 'c3 "Unknown message type \"y\"")
        (drover-remote-test-settle 0.05)
        (should (= (length warnings) 1))))))

(ert-deftest drover-remote-test-needs-spyglass-dropping-failed-writes ()
  "The remote layer refuses a spyglass that keeps a stream whose write failed."
  (skip-unless (drover-remote-test--available-p))
  (drover-test-with-backend nil
    (let ((drover-remote-mode nil)
          (drover-remote--state nil))
      (cl-letf (((symbol-function 'spyglass-reconnect) nil))
        (should-error (drover-remote-mode 1) :type 'user-error)
        (should-not drover-remote-mode)
        (should-not drover-remote--state)))))

(ert-deftest drover-remote-test-failed-write-resumes ()
  "A write failing drops its stream at once, quietly; the request it carried is answered after.
Spyglass drops a stream whose write failed: the peer reconnects, its
sessions resynchronized, without a warning nor an error kept."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 3)
    (drover-remote-test-synced 3)
    (let* ((peer (drover-remote--peer "agent"))
           (connection (drover-remote--peer-connection peer))
           (process (spyglass-connection-process connection))
           (send (symbol-function 'process-send-string))
           (session (drover-index-session '(remote:agent fake . "s1")))
           (failed nil)
           (warnings nil)
           (recap nil))
      (cl-letf (((symbol-function 'drover-warn) (lambda (&rest args) (push (apply #'format args) warnings))))
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (target string)
                     (if (and (eq target process) (not failed))
                         (progn (setq failed t)
                                (signal 'file-error (list "Writing to process" "Input/output error")))
                       (funcall send target string)))))
          (drover-recap session (lambda (value) (setq recap value))))
        (should failed)
        ;; At once, not when the heartbeat times out.
        (should-not (process-live-p process))
        (should (= (plist-get (drover-remote-test-stats) :write-failures) 1))
        (drover-remote-test-wait (lambda () recap))
        (should (equal recap '(:prompt "Prompt of s1" :reply "Reply of s1" :changes (("a.el" 3 1)))))
        (drover-remote-test-wait (lambda () (and (eq (drover-remote--peer-state peer) 'open)
                                                 (not (drover-remote--peer-stale peer)))))
        (should (eq (drover-remote--peer-state peer) 'open))
        (should-not (drover-remote--peer-stale peer))
        (drover-remote-test-command "sessions" :n 4)
        (drover-remote-test-synced 4)
        (drover-remote-test-settle 0.6)
        (should-not warnings)
        (should-not (drover-remote--peer-error peer))
        (should-not (plist-get (drover-remote-test-stats) :unexpected))))))

(ert-deftest drover-remote-test-stale-out-of-count ()
  "Sessions of a host not connected are listed but leave the mode-line count."
  (require 'drover-attention)
  (drover-test-with-backend (list (drover-test-session "a" :status 'done))
    (let* ((live (drover-session-create :backend 'remote:core :id '(fake . "w") :host "core"
                                        :status 'waiting))
           (stale (drover-session-create :backend 'remote:core :id '(fake . "x") :host "core"
                                         :status 'waiting :stale 1.0)))
      (drover-test-register-simple 'remote:core (list live stale) :remote "core")
      (should (equal (mapcar #'drover-session-id (drover-attention-sessions))
                     '((fake . "w") "a")))
      (should (= (length (drover-index-sessions)) 3))
      (should (drover-attention--needs-p (drover-session-key live) live))
      (should-not (drover-attention--needs-p (drover-session-key stale) stale)))))

(ert-deftest drover-remote-test-held-is-a-revision ()
  "What a subscriber holds, :held, is a revision of this host, which every change raises.
It is no count of sessions: a peer stopped while 2549 sessions changed
some 3000 times held, resumed, the revision 5474, and was sent the 1712
sessions that changed meanwhile, once each.  Without a subscriber,
changes are not counted, and a session changed many times meanwhile is
one revision."
  (drover-remote-test-with (list (drover-test-session "a") (drover-test-session "b") (drover-test-session "c"))
    (drover-remote-test-connect (drover-remote-test-start-agent dir "agent" "viewer"))
    (cl-flet ((serving () (plist-get (drover-remote-test-stats) :serving))
              (rev () (plist-get (drover-remote-owner-stats) :rev))
              (change (id times)
                (dotimes (i times)
                  (setf (drover-session-title (drover-find 'fake id)) (format "Change %d of %s" i id))
                  (drover-backend-changed 'fake id)
                  (drover-changed-now))))
      (drover-remote-test-wait (lambda () (eql (plist-get (serving) :held) 3)))
      (should (= (rev) 3))
      (should (= (plist-get (serving) :records) 3))
      ;; Subscribed, a session changing ten times raises the revision ten
      ;; times, and is sent once, as it stands.
      (change "a" 10)
      (should (= (rev) 13))
      (drover-remote-test-wait (lambda () (eql (plist-get (serving) :held) 13)))
      (should (= (plist-get (serving) :held) 13))
      (should (= (plist-get (serving) :records) 4))
      ;; The agent's streams drop; another session changes ten times
      ;; meanwhile.  Back, the peer resumes from the revision it held,
      ;; and is sent that session, once.
      (drover-remote-test-command "drop")
      (drover-remote-test-wait (lambda () (not (serving))) 5)
      (should-not (serving))
      (change "b" 10)
      (drover-remote-test-wait (lambda () (eql (plist-get (serving) :held) 14)))
      (should (= (rev) 14))
      (should (= (plist-get (serving) :held) 14))
      (should (= (plist-get (serving) :resumes) 1))
      (should (= (plist-get (serving) :records) 1))
      (should (= (plist-get (serving) :dirty) 0)))))

(ert-deftest drover-remote-test-frozen-peer ()
  "A peer that stops answering goes stale within its heartbeat's timeout, and back after.
Its Emacs stopped by SIGSTOP, its connection stays open to the system:
only the heartbeat tells.  Spyglass looked at it every half interval,
which `drover-remote-heartbeat''s integers halved to whole seconds, 1
to 0: a timer that never repeated, and the peer stayed open."
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 3)
    (drover-remote-test-synced 3)
    (let* ((peer (drover-remote--peer "agent"))
           (timer (spyglass-connection-timer (drover-remote--peer-connection peer)))
           (top (process-id drover-remote-test--agent))
           ;; The agent's Emacs, under `timeout'.
           (emacs (seq-find (lambda (pid) (eql (alist-get 'ppid (process-attributes pid)) top))
                            (list-system-processes)))
           (start nil))
      (should emacs)
      (should (= (timer--repeat-delay timer) 0.5))
      (signal-process emacs 'SIGSTOP)
      (unwind-protect
          (progn
            (setq start (float-time))
            (drover-remote-test-wait (lambda () (not (eq (drover-remote--peer-state peer) 'open))) 15)
            ;; The timeout, 4 s, and a look every half second.
            (should-not (eq (drover-remote--peer-state peer) 'open))
            (should (< (- (float-time) start) 5.5))
            (drover-changed-now)
            (should (= (length (drover-remote-test-remote)) 3))
            (should (seq-every-p #'drover-session-stale (drover-remote-test-remote))))
        (signal-process emacs 'SIGCONT))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)) 30)
      (should (eq (drover-remote--peer-state peer) 'open))
      (drover-remote-test-synced 3 (lambda (session) (not (drover-session-stale session)))))))

(ert-deftest drover-remote-test-prints-in-proportion ()
  "What the remote layer keeps prints in proportion to the sessions, both ways.
Emacs prints a value whole in an error's data, or as `format' does: a
peer's or a subscriber's state, a connection, or a record received,
leading back to what holds it would print it again from each."
  (drover-remote-test-with (cl-loop for i below 200 collect (drover-test-session (format "v%d" i)))
    (drover-remote-test-connect (drover-remote-test-start-agent dir "agent" "viewer"))
    (drover-remote-test-command "sessions" :n 100 :children 1)
    (drover-remote-test-synced 200)
    (drover-remote-test-wait (lambda () (> (hash-table-count (drover-remote--state-subs drover-remote--state)) 0)))
    ;; A terminal of the agent mirrored here.
    (drover-remote-test-command "terminal" :id "s1")
    (let ((mirror (save-window-excursion (drover-remote-test-open "s1"))))
      (drover-remote-test-live mirror))
    (drover-set-show-history t)
    (drover-remote-test-wait (lambda () (drover-index-session '(remote:agent fake . "s1"))))
    (save-window-excursion
      (drover-remote-status)
      (with-current-buffer (drover-list--buffer)
        (revert-buffer)
        (unwind-protect
            (let* ((peer (drover-remote--peer "agent"))
                   (connection (drover-remote--peer-connection peer)))
              (should connection)
              ;; Recaps asked, none answered yet: each request held by
              ;; the connection, once leading back to the peer, printed
              ;; it and all its sessions again.
              (dolist (session (seq-take (drover-remote-test-remote) 50))
                (funcall (drover-backend-property 'remote:agent :recap) session #'ignore))
              (drover-test-prints-in-proportion
               400 (list (cons "a record received" (drover-index-session '(remote:agent fake . "s1")))
                         (cons "a row" (gethash '(remote:agent fake . "s1")
                                                (drover-list--model-rows drover-list--model))))))
          (drover-set-show-history nil)
          (kill-buffer)
          (kill-buffer "*drover remote*"))))))

;;;; Actions on a peer's sessions

(defun drover-remote-test-start-full (dir &rest environment)
  "Start the agent in DIR granting full access, with more ENVIRONMENT, and connect."
  (let ((process-environment (append (cons "DROVER_AGENT_ACCESS=full" environment) process-environment)))
    (drover-remote-test-connect (drover-remote-test-start-agent dir)))
  (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'full)))
  (should (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'full)))

(defun drover-remote-test-calls (&optional action)
  "Return the agent's actions performed, of ACTION if given, as (ACTION ID ARGUMENT)."
  (seq-filter (lambda (call) (or (null action) (equal (car call) action)))
              (mapcar (lambda (call) (append call nil))
                      (plist-get (drover-remote-test-command "calls") :calls))))

(defun drover-remote-test-session (id)
  "Return this host's record of the agent's session ID, or nil."
  (drover-changed-now)
  (drover-index-session (cons 'remote:agent (cons 'fake id))))

(defun drover-remote-test-events (&optional name)
  "Return the kinds of the events of this host's link with the peer NAME, oldest first."
  (mapcar #'cadr (reverse (drover-remote--peer-history (drover-remote--peer (or name "agent"))))))

(ert-deftest drover-remote-test-actions ()
  "Each action on another host's session is performed there once, its outcome shown as its records change."
  (drover-remote-test-with nil
                           (drover-remote-test-start-full dir)
                           (drover-remote-test-command "sessions" :n 6)
                           (drover-remote-test-synced 6)
                           (let ((done (drover-remote-test-session "s2"))
                                 (working (drover-remote-test-session "s1")))
                             (should (eq (drover-session-status done) 'done))
                             ;; What the owner allows travels with each record.
                             (should (drover-supports-p done :mark-seen))
                             (should (drover-supports-p done :delete))
                             (should (drover-archivable-p done))
                             ;; Marked seen there: its status changes there, then here.
                             (drover-mark-seen done)
                             (drover-remote-test-wait (lambda () (eq (drover-session-status (drover-remote-test-session "s2")) 'idle)))
                             (should (eq (drover-session-status (drover-remote-test-session "s2")) 'idle))
                             (drover-interrupt working)
                             (drover-remote-test-wait (lambda () (eq (drover-session-status (drover-remote-test-session "s1")) 'idle)))
                             (drover-rename (drover-remote-test-session "s3") "Renamed there")
                             (drover-remote-test-wait (lambda () (equal (drover-session-title (drover-remote-test-session "s3"))
                                                                        "Renamed there")))
                             ;; Archived there: gone from the sessions in use, listed among the archived.
                             (drover-archive (drover-remote-test-session "s4"))
                             (drover-remote-test-wait (lambda () (null (drover-remote-test-session "s4"))))
                             (should-not (drover-remote-test-session "s4"))
                             (drover-set-show-archived t)
                             (let ((archived nil))
                               (drover-remote-test-wait
                                (lambda () (setq archived (seq-find (lambda (session) (equal (drover-session-id session) '(fake . "s4")))
                                                                    (drover-known-sessions)))))
                               (should archived)
                               (should (drover-unarchivable-p archived))
                               (drover-unarchive archived))
                             (drover-remote-test-wait (lambda () (drover-remote-test-session "s4")))
                             (should (drover-remote-test-session "s4"))
                             (drover-set-show-archived nil)
                             (drover-delete (drover-remote-test-session "s5"))
                             (drover-remote-test-wait (lambda () (null (drover-remote-test-session "s5"))))
                             ;; A prompt goes to its owner, which submits it.
                             (funcall (drover-backend-property 'remote:agent :send) (drover-remote-test-session "s0") "Hello there" nil)
                             (drover-remote-test-wait (lambda () (drover-remote-test-calls "send")))
                             ;; Each was performed once.
                             (should (equal (mapcar (lambda (call) (list (car call) (cadr call))) (drover-remote-test-calls))
                                            '(("mark-seen" "s2") ("interrupt" "s1") ("rename" "s3") ("archive" "s4")
                                              ("unarchive" "s4") ("delete" "s5") ("send" "s0"))))
                             (should (equal (nth 2 (car (drover-remote-test-calls "send"))) "Hello there"))
                             ;; Sessions of the agent are offered to the prompt, without buffers.
                             (should (assoc (drover-remote-test-session "s0") (drover-prompt--target-sessions))))))

(ert-deftest drover-remote-test-answer-request ()
  "A request of another host's session is answered here, what it asks coming from its owner."
  (drover-remote-test-with nil
                           (drover-remote-test-start-full dir)
                           (drover-remote-test-command "sessions" :n 2)
                           (drover-remote-test-command "ask" :ids ["s1"])
                           (drover-remote-test-synced 2 (lambda (_) (drover-session-requests (drover-remote-test-session "s1"))))
                           (let ((asked nil))
                             (cl-letf (((symbol-function 'completing-read)
                                        (lambda (prompt collection &rest _)
                                          (setq asked (list prompt collection))
                                          "always")))
                               (drover-respond (drover-remote-test-session "s1")))
                             (should (string-match-p "Shell command: ls (Agent)" (car asked)))
                             (should (equal (cadr asked) '("once" "always" "reject"))))
                           (drover-remote-test-wait (lambda () (drover-remote-test-calls "answer")))
                           (should (equal (car (drover-remote-test-calls "answer")) '("answer" "s1" "always")))
                           (drover-remote-test-wait (lambda () (null (drover-session-requests (drover-remote-test-session "s1")))))
                           (should (eq (drover-session-status (drover-remote-test-session "s1")) 'working))))

(ert-deftest drover-remote-test-refused-by-access ()
  "A peer of read access acts on nothing: greyed out here, refused there whatever is asked, told once."
  (drover-remote-test-with nil
                           (drover-remote-test-connect (drover-remote-test-start-agent dir))
                           (drover-remote-test-command "sessions" :n 3)
                           (drover-remote-test-synced 3)
                           (drover-remote-test-wait (lambda () (drover-remote--peer-granted (drover-remote--peer "agent"))))
                           (let ((session (drover-remote-test-session "s2"))
                                 (peer (drover-remote--peer "agent")))
                             (should (eq (drover-remote--peer-granted peer) 'read))
                             (should-not (drover-supports-p session :mark-seen))
                             (should-not (drover-archivable-p session))
                             ;; Recaps still come.
                             (should (drover-supports-p session :recap))
                             (should (string-match-p "grants this host read access"
                                                     (cadr (should-error (drover-mark-seen session) :type 'user-error))))
                             ;; Asked anyway, past this host's checks: refused there.
                             (let ((outcome nil))
                               (drover-remote--send-chunk peer "mark-seen" nil
                                                          (list (list "mark-seen" '(fake . "s2") nil
                                                                      (lambda () (setq outcome 'done))
                                                                      (lambda (message) (setq outcome message)))))
                               (drover-remote-test-wait (lambda () outcome))
                               (should (stringp outcome))
                               (should (string-match-p "read access" outcome)))
                             (should-not (drover-remote-test-calls))
                             (should (member "refused" (mapcar (lambda (event) (aref event 0))
                                                               (plist-get (drover-remote-test-command "history" :peer "viewer")
                                                                          :events)))))))

(ert-deftest drover-remote-test-older-peer-read-only ()
  "A peer running an older drover keeps listing its sessions here, actions disabled with a note."
  (drover-remote-test-with nil
                           (let ((process-environment (append '("DROVER_AGENT_ACCESS=full" "DROVER_AGENT_OLD=1") process-environment)))
                             (drover-remote-test-connect (drover-remote-test-start-agent dir)))
                           (drover-remote-test-command "sessions" :n 3)
                           (drover-remote-test-synced 3)
                           (let ((session (drover-remote-test-session "s2")))
                             (should-not (drover-supports-p session :mark-seen))
                             (should (string-match-p "older drover" (cadr (should-error (drover-mark-seen session) :type 'user-error))))
                             (should (string-match-p "older drover" (drover-remote--refusal (drover-remote--peer "agent")))))))

(ert-deftest drover-remote-test-action-survives-a-drop ()
  "An action whose stream breaks before its answer is asked again, and performed once."
  (drover-remote-test-with nil
                           (drover-remote-test-start-full dir)
                           (drover-remote-test-command "sessions" :n 3)
                           (drover-remote-test-synced 3)
                           (drover-remote-test-command "hold")
                           (let ((outcome nil)
                                 (peer (drover-remote--peer "agent")))
                             (drover--call-confirming (drover-remote-test-session "s2") :mark-seen
                                                      (lambda () (setq outcome 'done)) (lambda (message) (setq outcome message)))
                             (drover-remote-test-wait (lambda () (drover-remote-test-calls "mark-seen")))
                             (spyglass-connection-drop-stream (drover-remote--peer-dialed peer))
                             (drover-remote-test-wait (lambda () (not (eq (drover-remote--peer-state peer) 'open))) 5)
                             (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)))
                             (drover-remote-test-settle 0.3)
                             (drover-remote-test-command "release")
                             (drover-remote-test-wait (lambda () outcome))
                             (should (eq outcome 'done))
                             (should (= (length (drover-remote-test-calls "mark-seen")) 1)))))

(ert-deftest drover-remote-test-action-times-out ()
  "An action whose peer never answers fails at its deadline, on one line."
  (drover-remote-test-with nil
                           (drover-remote-test-start-full dir)
                           (drover-remote-test-command "sessions" :n 3)
                           (drover-remote-test-synced 3)
                           (drover-remote-test-command "hold")
                           (let ((outcome nil)
                                 (drover-remote-action-timeout 1.5))
                             (drover--call-confirming (drover-remote-test-session "s2") :mark-seen
                                                      (lambda () (setq outcome 'done)) (lambda (message) (setq outcome message)))
                             ;; Its peer gone meanwhile, for longer than the action waits.
                             (drover-remote-test-command "drop" :away 5)
                             (drover-remote-test-wait (lambda () outcome) 10)
                             (should (stringp outcome))
                             (should (string-match-p "\\`Agent: \\|did not come back" outcome))
                             (should-not (string-search "\n" outcome)))))

(ert-deftest drover-remote-test-bulk-batched ()
  "Archiving hundreds of another host's sessions goes in a few requests, reported once."
  (drover-remote-test-with nil
                           (drover-remote-test-start-full dir)
                           (drover-remote-test-command "sessions" :n 300)
                           (drover-remote-test-synced 300)
                           (let ((messages nil))
                             (cl-letf (((symbol-function 'message)
                                        (lambda (format-string &rest args)
                                          (when format-string (push (apply #'format format-string args) messages)))))
                               (drover-archive (drover-remote-test-remote))
                               (drover-remote-test-wait (lambda () (null drover--bulk-jobs)) 30))
                             (should (= (length (drover-remote-test-calls "archive")) 300))
                             (should (seq-some (lambda (text) (string-match-p "\\`Archived 300 sessions" text)) messages))
                             (drover-remote-test-synced 0)
                             (should (= (plist-get (drover-remote-test-stats) :actions-sent) 300))
                             ;; Requests of 100 sessions at most, and few.
                             (let ((asked (apply #'+ (mapcar (lambda (event) (if (equal (aref event 0) "act") (aref event 2) 0))
                                                             (plist-get (drover-remote-test-command "history" :peer "viewer") :events)))))
                               (should (<= 3 asked 8))))))

;;;; The link

(ert-deftest drover-remote-test-history-bounded ()
  "Each peer keeps the last events of its link, errors with their messages, bounded."
  (skip-unless (drover-remote-test--available-p))
  (let ((peer (drover-remote--peer-make :name "core")))
    (dotimes (i 100)
      (drover-remote--event peer 'drop "stream ended %d" i))
    (should (= (length (drover-remote--peer-history peer)) drover-remote-history-size))
    (should (equal (nth 2 (car (drover-remote--peer-history peer))) "stream ended 99"))
    (drover-remote--fail peer (concat "Error in filter: (excessive-lisp-nesting 1601) " (make-string 5000 ?x)))
    (let ((event (car (drover-remote--peer-history peer))))
      (should (eq (nth 1 event) 'error))
      (should (string-match-p "excessive-lisp-nesting" (nth 2 event)))
      (should (<= (length (nth 2 event)) 201)))
    ;; The last error stays, with its time.
    (should (string-match-p "excessive-lisp-nesting" (drover-remote--peer-error peer)))
    (should (drover-remote--peer-error-time peer))))

(ert-deftest drover-remote-test-restart-keeps-rows ()
  "A peer restarting its remote layer says so: its rows stay, it greets again, and a snapshot of its new epoch comes."
  (drover-remote-test-with nil
                           (drover-remote-test-connect (drover-remote-test-start-agent dir))
                           (drover-remote-test-command "sessions" :n 5)
                           (drover-remote-test-synced 5)
                           (let* ((peer (drover-remote--peer "agent"))
                                  (epoch (drover-remote--peer-epoch peer))
                                  (snapshots (plist-get (drover-remote-test-stats) :snapshots))
                                  (dimmed nil))
                             (drover-remote-test-command "restart")
                             (drover-remote-test-wait (lambda ()
                                                        (drover-changed-now)
                                                        (when (seq-some #'drover-session-stale (drover-remote-test-remote))
                                                          (setq dimmed t))
                                                        (and (not (equal (drover-remote--peer-epoch peer) epoch))
                                                             (eq (drover-remote--peer-state peer) 'open))))
                             (should-not (equal (drover-remote--peer-epoch peer) epoch))
                             (should (> (plist-get (drover-remote-test-stats) :snapshots) snapshots))
                             (should (memq 'bye (drover-remote-test-events)))
                             (should (string-match-p "restart" (seq-some (lambda (event) (and (eq (nth 1 event) 'bye) (nth 2 event)))
                                                                         (drover-remote--peer-history peer))))
                             (should-not dimmed)
                             (drover-remote-test-synced 5))))

(ert-deftest drover-remote-test-relayout-resubscribes ()
  "A peer whose remote layer starts anew on the same connection greets again: this host subscribes again, and gets its new epoch."
  (drover-remote-test-with (list (drover-test-session "v1"))
                           (drover-remote-test-connect (drover-remote-test-start-agent dir "agent" "viewer"))
                           (drover-remote-test-command "sessions" :n 4)
                           (drover-remote-test-synced 4)
                           (let* ((peer (drover-remote--peer "agent"))
                                  (connection (drover-remote--peer-connection peer))
                                  (epoch (drover-remote--peer-epoch peer)))
                             (drover-remote-test-command "relayout")
                             (drover-remote-test-wait (lambda () (and (drover-remote--peer-epoch peer)
                                                                      (not (equal (drover-remote--peer-epoch peer) epoch)))))
                             ;; The same connection, a new epoch, a snapshot of it.
                             (should (eq (drover-remote--peer-connection peer) connection))
                             (should-not (equal (drover-remote--peer-epoch peer) epoch))
                             (should (seq-some (lambda (event) (and (eq (nth 1 event) 'snapshot) (string-match-p "its epoch was" (nth 2 event))))
                                               (drover-remote--peer-history peer)))
                             ;; Changes keep coming, through it.
                             (drover-remote-test-command "change" :ids ["s1"] :title "After the restart" :now t)
                             (drover-remote-test-synced 4 (lambda (_) (equal (drover-session-title (drover-remote-test-session "s1"))
                                                                             "After the restart")))
                             ;; It subscribed to this host again too: it lists this host's session.
                             (drover-remote-test-wait (lambda () (= (length (plist-get (drover-remote-test-command "hosts") :remote)) 1)))
                             (should (= (length (plist-get (drover-remote-test-command "hosts") :remote)) 1)))))

(ert-deftest drover-remote-test-grace-then-dimmed ()
  "A peer whose own connection ends has a grace to dial again before its rows dim; quitting, they dim at once."
  (drover-remote-test-with nil
                           (let ((drover-remote-grace 1.5)
                                 (drover-remote-peers '(("agent"))))
                             (drover-remote-mode 1)
                             (let ((process-environment (cons (concat "DROVER_AGENT_DIAL=" spyglass-registration-file)
                                                              process-environment)))
                               (drover-remote-test-start-agent dir "agent" "viewer"))
                             (let ((peer (drover-remote--peer "agent")))
                               (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)))
                               (drover-remote-test-command "sessions" :n 3)
                               (drover-remote-test-synced 3)
                               ;; Back within the grace: never dimmed.
                               (let ((dimmed nil))
                                 (drover-remote-test-command "drop")
                                 (drover-remote-test-wait (lambda () (not (eq (drover-remote--peer-state peer) 'open))) 5)
                                 (drover-remote-test-wait (lambda ()
                                                            (when (drover-remote--peer-stale peer) (setq dimmed t))
                                                            (eq (drover-remote--peer-state peer) 'open)))
                                 (should (eq (drover-remote--peer-state peer) 'open))
                                 (should-not dimmed)
                                 (should (memq 'grace (drover-remote-test-events))))
                               ;; Away past it: dimmed, then fresh once back.
                               (drover-remote-test-command "drop" :away 4)
                               (drover-remote-test-wait (lambda () (drover-remote--peer-stale peer)) 5)
                               (should (drover-remote--peer-stale peer))
                               (drover-remote-test-wait (lambda () (and (eq (drover-remote--peer-state peer) 'open)
                                                                        (not (drover-remote--peer-stale peer))))
                                                        10)
                               (should-not (drover-remote--peer-stale peer))
                               ;; Quitting, it says so: dimmed at once, with no grace.
                               (drover-remote-test-command "quit")
                               (drover-remote-test-wait (lambda () (drover-remote--peer-stale peer)) 1)
                               (should (drover-remote--peer-stale peer))
                               (should (equal (car (drover-remote--peer-bye peer)) "quit"))))))

(ert-deftest drover-remote-test-restart-subscribes-once ()
  "This host restarting its remote layer, both hosts dialing, subscribes once: the greeting it asked for is no restart of the peer's."
  (drover-remote-test-with nil
    (let ((registration (let ((process-environment (cons (concat "DROVER_AGENT_DIAL=" spyglass-registration-file)
                                                         process-environment)))
                          (drover-remote-test-start-agent dir "agent" "viewer"))))
      (drover-remote-test-connect registration)
      (drover-remote-test-command "sessions" :n 3)
      (drover-remote-test-synced 3)
      ;; The agent's connection is kept, its name sorting first.
      (drover-remote-test-wait (lambda () (eq (plist-get (drover-remote-test-stats) :through) 'accepted)) 10)
      (should (eq (plist-get (drover-remote-test-stats) :through) 'accepted))
      (drover-remote-mode 1)
      (drover-remote-test-wait (lambda () (and (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open)
                                               (drover-remote--peer-epoch (drover-remote--peer "agent"))))
                               10)
      (drover-remote-test-settle 1.0)
      (drover-remote-test-synced 3)
      (should-not (seq-some (lambda (event) (string-prefix-p "greeted again" (nth 2 event)))
                            (drover-remote--peer-history (drover-remote--peer "agent"))))
      (should (= (plist-get (drover-remote-test-stats) :subscriptions) 1)))))

;;;; Showing a session of a peer

(ert-deftest drover-remote-test-show-renders-markdown ()
  "A session of another host shows its last reply as OpenCode's buffers render Markdown, read only."
  (drover-remote-test-with nil
                           (let ((treesit-extra-load-path (cons (expand-file-name "~/.emacs.d/tree-sitter")
                                                                (bound-and-true-p treesit-extra-load-path))))
                             (skip-unless (drover-remote--markdown-ready-p))
                             (drover-remote-test-connect (drover-remote-test-start-agent dir))
                             (drover-remote-test-command "sessions" :n 3)
                             (drover-remote-test-synced 3)
                             (save-window-excursion
                               (with-current-buffer (drover-remote-show (drover-remote-test-session "s1"))
                                 (drover-remote-test-wait (lambda () (save-excursion (goto-char (point-min))
                                                                                     (search-forward "Prompt of s1" nil t))))
                                 (goto-char (point-min))
                                 (should (search-forward "Preview of s1" nil t))
                                 ;; A heading, its markup gone; bold; code fontified by its language.
                                 (should (memq 'markdown-ts-heading-1 (ensure-list (get-text-property (match-beginning 0) 'face))))
                                 (should-not (save-excursion (goto-char (point-min)) (search-forward "**" nil t)))
                                 (should (search-forward "bold" nil t))
                                 (should (memq 'markdown-ts-bold (ensure-list (get-text-property (match-beginning 0) 'face))))
                                 (should (search-forward "defun" nil t))
                                 (should (memq 'font-lock-keyword-face (ensure-list (get-text-property (match-beginning 0) 'face))))
                                 ;; Its status in its face, the labels dim, read only.
                                 (goto-char (point-min))
                                 (should (search-forward "Working" nil t))
                                 (should (eq (get-text-property (match-beginning 0) 'face) (opencode-session-status-face 'working)))
                                 (should buffer-read-only)
                                 (kill-buffer))))))

(ert-deftest drover-remote-test-tooltips-travel ()
  "Strings with faces keep their tooltips across hosts, bounded; tooltips made by functions stay home."
  (skip-unless (drover-remote-test--available-p))
  (let* ((agent (concat (propertize "plan" 'face 'bold 'help-echo (concat "Agent plan\n" (make-string 50000 ?x)))
                        (propertize "!" 'help-echo #'ignore)))
         (record (drover-session-create :backend 'opencode :id "s" :agent-display agent))
         (decoded (drover-remote-test--roundtrip record)))
    (should (equal (get-text-property 0 'face (drover-session-agent-display decoded)) 'bold))
    (should (string-prefix-p "Agent plan" (get-text-property 0 'help-echo (drover-session-agent-display decoded))))
    (should (< (length (get-text-property 0 'help-echo (drover-session-agent-display decoded))) 5000))
    (should-not (get-text-property 4 'help-echo (drover-session-agent-display decoded)))))

;;;; Terminals mirrored

(defun drover-remote-test--ptys-p ()
  "Return non-nil when processes may run in ptys, whose size `stty' reads."
  (not (memq system-type '(windows-nt ms-dos))))

(defun drover-remote-test-digest (buffer)
  "Return the MD5 of BUFFER's text, as the agent's terminal-state tells its own."
  (with-current-buffer buffer
    (secure-hash 'md5 (encode-coding-string (buffer-substring-no-properties (point-min) (point-max))
                                            'utf-8-unix t))))

(defun drover-remote-test-terminal (id)
  "Return what the agent tells of the terminal of its session ID, a plist."
  (drover-remote-test-command "terminal-state" :id id))

(defun drover-remote-test-shares ()
  "Return the agent's shares, as (ID VIEWERS)."
  (mapcar (lambda (share) (append share nil)) (plist-get (drover-remote-test-command "shares") :shares)))

(defun drover-remote-test-open (id)
  "Open the agent's session ID here, as RET does, and return the buffer shown."
  (drover-open (drover-remote-test-session id))
  (window-buffer (selected-window)))

(defun drover-remote-test-live (mirror)
  "Wait until MIRROR, a buffer, is live, and check it is."
  (drover-remote-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)))
  (should (eq (spyglass-mirror-state mirror) 'live)))

(defun drover-remote-test-shows (buffer text)
  "Wait until BUFFER holds TEXT, and check it does."
  (drover-remote-test-wait (lambda () (with-current-buffer buffer
                                        (save-excursion (goto-char (point-min)) (search-forward text nil t)))))
  (should (with-current-buffer buffer (save-excursion (goto-char (point-min)) (search-forward text nil t)))))

(defun drover-remote-test-report-size (mirror)
  "Report the size of the window showing MIRROR, as window changes would."
  (setf (spyglass--mirror-sent-size (buffer-local-value 'spyglass--mirror mirror)) nil)
  (spyglass--mirror-report-sizes))

(ert-deftest drover-remote-test-mirror-terminal ()
  "RET on a session whose terminal runs on its host mirrors it: typed here, echoed there, sized by this window, closed leaving it running."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-test-with nil
    (let ((process-environment (cons "DROVER_AGENT_UNSHARE=1" process-environment)))
      (drover-remote-test-start-full dir))
    (drover-remote-test-command "sessions" :n 2)
    (should (eq (plist-get (drover-remote-test-command "terminal" :id "s1") :pty) t))
    (drover-remote-test-synced 2)
    (save-window-excursion
      (delete-other-windows)
      (let ((mirror (drover-remote-test-open "s1")))
        (should (equal (buffer-local-value 'drover-remote--mirror-kind mirror) "terminal"))
        (drover-remote-test-live mirror)
        (should (string-match-p "Session s1@Agent" (with-current-buffer mirror (spyglass--mirror-header-line))))
        ;; A session buffer: drover's keys act on the session there.
        (with-current-buffer mirror
          (should (equal (drover-session-id (drover-session-at-point)) '(fake . "s1"))))
        (should (eq (drover-session-buffer (drover-remote-test-session "s1")) mirror))
        ;; Shared with this host alone, under drover's name.
        (should (equal (mapcar #'car (drover-remote-test-shares)) '("drover/fake/s1")))
        ;; Typed here, echoed there, shown here.
        (spyglass-mirror-send-string "hello there" mirror)
        (spyglass-mirror-send-key "RET" mirror)
        (drover-remote-test-shows mirror "hello there")
        (should (string-search "hello there" (plist-get (drover-remote-test-terminal "s1") :text)))
        ;; Sized by the window showing it, as it changes.
        (drover-remote-test-report-size mirror)
        (let ((size (spyglass-window-cells (selected-window))))
          (drover-remote-test-wait (lambda () (equal (append (plist-get (drover-remote-test-terminal "s1") :size) nil)
                                                     (list (car size) (cdr size)))))
          (should (equal (append (plist-get (drover-remote-test-terminal "s1") :size) nil)
                         (list (car size) (cdr size)))))
        (split-window nil 8)
        (split-window nil 30 t)
        (drover-remote-test-report-size mirror)
        (let ((size (spyglass-window-cells (selected-window))))
          (should (= (cdr size) 6))
          (drover-remote-test-wait (lambda () (equal (append (plist-get (drover-remote-test-terminal "s1") :size) nil)
                                                     (list (car size) (cdr size)))))
          (should (equal (append (plist-get (drover-remote-test-terminal "s1") :size) nil)
                         (list (car size) (cdr size)))))
        ;; RET again shows the same mirror.
        (should (eq (drover-remote-test-open "s1") mirror))
        ;; Closed here: the terminal runs on there, unshared a while later.
        (kill-buffer mirror)
        (drover-remote-test-wait (lambda () (null (drover-remote-test-shares))) 10)
        (should-not (drover-remote-test-shares))
        (should (eq (plist-get (drover-remote-test-terminal "s1") :process) t))
        (should-not (gethash '(fake . "s1") (drover-remote--peer-mirrors (drover-remote--peer "agent"))))))
    ;; An OpenCode session shows as its host tells it, saying why.
    (let ((session (copy-sequence (drover-remote-test-session "s0"))))
      (setf (drover-session-origin session) 'opencode)
      (should (string-match-p "OpenCode" (drover-remote--why-no-mirror (drover-remote--peer "agent") session))))))

(ert-deftest drover-remote-test-mirror-shell ()
  "A shell mirrored takes text and submits it with RET, through the shell's own input."
  (skip-unless (executable-find "cat"))
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s0" :kind "comint")
    (drover-remote-test-synced 2)
    (save-window-excursion
      (let ((mirror (drover-remote-test-open "s0")))
        (should (equal (buffer-local-value 'drover-remote--mirror-kind mirror) "shell"))
        (drover-remote-test-live mirror)
        (spyglass-mirror-send-string "echo me" mirror)
        (spyglass-mirror-send-key "RET" mirror)
        (drover-remote-test-wait (lambda () (with-current-buffer mirror (>= (how-many "echo me" (point-min) (point-max)) 2))))
        (should (with-current-buffer mirror (>= (how-many "echo me" (point-min) (point-max)) 2)))
        (kill-buffer mirror)))))

(ert-deftest drover-remote-test-mirror-resumes-conversation ()
  "A conversation without a terminal is resumed there, out of its windows, then mirrored here; once."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-synced 2)
    (let ((windows (plist-get (drover-remote-test-command "windows") :windows)))
      (save-window-excursion
        (let ((mirror (drover-remote-test-open "s0")))
          (should (equal (buffer-local-value 'drover-remote--mirror-kind mirror) "terminal"))
          (drover-remote-test-live mirror)
          (should (equal (mapcar #'cadr (drover-remote-test-calls "open")) '("s0")))
          ;; Its front end displayed it, as resuming does; no window took it.
          (should (equal (plist-get (drover-remote-test-command "windows") :windows) windows))
          (spyglass-mirror-send-string "resumed" mirror)
          (spyglass-mirror-send-key "RET" mirror)
          (drover-remote-test-shows mirror "resumed")
          (kill-buffer mirror)
          ;; Opened again: mirrored, not resumed twice.
          (let ((again (drover-remote-test-open "s0")))
            (drover-remote-test-live again)
            (should (= (length (drover-remote-test-calls "open")) 1))
            (kill-buffer again)))))))

(ert-deftest drover-remote-test-mirror-survives-a-drop ()
  "A mirror whose stream breaks resumes as it comes back, what the terminal wrote meanwhile shown."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-test-with nil
    (let ((drover-remote-backoff '(1.0 1.0)))
      (drover-remote-test-start-full dir)
      (drover-remote-test-command "sessions" :n 2)
      (drover-remote-test-command "terminal" :id "s1")
      (drover-remote-test-synced 2)
      (save-window-excursion
        (let ((mirror (drover-remote-test-open "s1"))
              (peer (drover-remote--peer "agent")))
          (drover-remote-test-live mirror)
          (drover-remote-test-command "emit" :id "s1" :text "written during the outage\n" :delay 0.4)
          (spyglass-connection-drop-stream (drover-remote--peer-dialed peer))
          (should (eq (spyglass-mirror-state mirror) 'reconnecting))
          (drover-remote-test-live mirror)
          (drover-remote-test-shows mirror "written during the outage")
          (should (>= (plist-get (spyglass-mirror-stats mirror) :resumes) 1))
          ;; Still typed into.
          (spyglass-mirror-send-string "after" mirror)
          (spyglass-mirror-send-key "RET" mirror)
          (drover-remote-test-shows mirror "after")
          (kill-buffer mirror))))))

(ert-deftest drover-remote-test-mirror-reopened-after-restart ()
  "A peer restarting its remote layer unshares its terminals: the mirror opens anew in its buffer."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s1")
    (drover-remote-test-synced 2)
    (save-window-excursion
      (let ((mirror (drover-remote-test-open "s1")))
        (drover-remote-test-live mirror)
        (drover-remote-test-command "restart")
        (drover-remote-test-wait (lambda () (memq 'bye (drover-remote-test-events))))
        (drover-remote-test-wait (lambda () (and (eq (spyglass-mirror-state mirror) 'live)
                                                 (seq-some (lambda (event)
                                                             (and (eq (nth 1 event) 'mirror)
                                                                  (string-prefix-p "opening anew" (nth 2 event))))
                                                           (drover-remote--peer-history (drover-remote--peer "agent")))))
                                 20)
        (drover-remote-test-live mirror)
        (should (eq (window-buffer (selected-window)) mirror))
        (should (eq (gethash '(fake . "s1") (drover-remote--peer-mirrors (drover-remote--peer "agent"))) mirror))
        (spyglass-mirror-send-string "again" mirror)
        (spyglass-mirror-send-key "RET" mirror)
        (drover-remote-test-shows mirror "again")
        (kill-buffer mirror)))))

(ert-deftest drover-remote-test-mirror-read-only ()
  "A host granting read access lets this one watch its terminals, not type, nor resume."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-test-with nil
    (drover-remote-test-connect (drover-remote-test-start-agent dir))
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s1")
    (drover-remote-test-command "emit" :id "s1" :text "visible\n")
    (drover-remote-test-synced 2)
    (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) 'read)))
    (save-window-excursion
      (let ((mirror (drover-remote-test-open "s1")))
        (drover-remote-test-live mirror)
        (drover-remote-test-shows mirror "visible")
        (drover-remote-test-wait (lambda () (eq (plist-get (spyglass--mirror-meta (buffer-local-value 'spyglass--mirror mirror)) :in)
                                                :false)))
        (should (string-match-p "read-only" (cadr (should-error (spyglass-mirror-send-string "x" mirror)
                                                                 :type 'user-error))))
        ;; Sent anyway, past this host's check: refused there.
        (spyglass--send (drover-remote--peer-connection (drover-remote--peer "agent"))
                        (list :t "in" :sub (spyglass--mirror-id (buffer-local-value 'spyglass--mirror mirror))
                              :k "s" :v "typed anyway"))
        (drover-remote-test-settle 0.5)
        (should-not (string-search "typed anyway" (plist-get (drover-remote-test-terminal "s1") :text)))
        (kill-buffer mirror))
      ;; Without a terminal there, nothing is resumed: its view, saying why.
      (let ((view (drover-remote-test-open "s0")))
        (should (eq (buffer-local-value 'major-mode view) 'drover-remote-session-mode))
        (should (string-match-p "read access" (buffer-local-value 'drover-remote--note view)))
        (should-not (drover-remote-test-calls "open"))
        (kill-buffer view)))))

(ert-deftest drover-remote-test-mirror-older-peer ()
  "A peer whose drover shares no terminal shows its sessions as it tells them, saying why."
  (drover-remote-test-with nil
    (let ((process-environment (append '("DROVER_AGENT_ACCESS=full" "DROVER_AGENT_CAPS=actions,bye,grant")
                                       process-environment)))
      (drover-remote-test-connect (drover-remote-test-start-agent dir)))
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s1")
    (drover-remote-test-synced 2)
    (drover-remote-test-wait (lambda () (drover-remote--peer-granted (drover-remote--peer "agent"))))
    (save-window-excursion
      (let ((view (drover-remote-test-open "s1")))
        (should (eq (buffer-local-value 'major-mode view) 'drover-remote-session-mode))
        (should (string-match-p "older drover" (buffer-local-value 'drover-remote--note view)))
        (with-current-buffer view
          (should (search-forward "Terminal" nil t)))
        (should-not (drover-remote-test-shares))
        ;; Its actions still work.
        (should (drover-supports-p (drover-remote-test-session "s1") :rename))
        (kill-buffer view)))))

(ert-deftest drover-remote-test-mirror-fast-output ()
  "A terminal writing fast reaches its mirror whole, the mirror never keeping Emacs long."
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s1")
    (drover-remote-test-synced 2)
    (save-window-excursion
      (let ((mirror (drover-remote-test-open "s1"))
            (last (float-time))
            (worst 0.0)
            (start nil)
            (ticker nil))
        (drover-remote-test-live mirror)
        (setq ticker (run-with-timer 0.005 0.005 (lambda ()
                                                    (let ((now (float-time)))
                                                      (setq worst (max worst (- now last)) last now)))))
        (unwind-protect
            (progn
              (setq start (float-time) last start)
              ;; 8 MB written as a program would, its scrollback 512 kB.
              (drover-remote-test-command "emit" :id "s1" :bytes (* 8 1024 1024) :keep (* 512 1024))
              (drover-remote-test-wait (lambda () (with-current-buffer mirror
                                                    (save-excursion (goto-char (point-max))
                                                                    (search-backward "EMITTED" nil t))))
                                       120))
          (cancel-timer ticker))
        (let* ((elapsed (- (float-time) start))
               (state (drover-remote-test-terminal "s1")))
          (drover-remote-test-wait (lambda () (equal (drover-remote-test-digest mirror)
                                                     (plist-get (drover-remote-test-terminal "s1") :digest)))
                                   20)
          (should (equal (drover-remote-test-digest mirror)
                         (plist-get (drover-remote-test-terminal "s1") :digest)))
          (ignore state)
          ;; The mirror holds the scrollback, not all that was written.
          (should (<= (buffer-size mirror) (+ (* 512 1024) 100)))
          (message "Fast output: 8 MB in %.2f s, the longest Emacs kept from its timers %.1f ms"
                   elapsed (* 1000 worst))
          (should (< worst 0.1)))
        (kill-buffer mirror)))))

;;;; Backing off

(defun drover-remote-test--newest (peer kind)
  "Return the text of the newest event of KIND in PEER's history, or nil."
  (nth 2 (seq-find (lambda (event) (eq (nth 1 event) kind)) (drover-remote--peer-history peer))))

(defun drover-remote-test--dead-registration (dir)
  "Return a registration file in DIR of a server gone: nothing listens on its port."
  (let* ((listener (make-network-process :name "drover-dead" :server t :host "127.0.0.1" :service t
                                         :noquery t))
         (port (process-contact listener :service))
         (file (expand-file-name "gone.json" dir)))
    (delete-process listener)
    (with-temp-file file
      (insert (json-serialize (list :proto spyglass-wire-protocol :port port :token (make-string 64 ?a)))))
    file))

(ert-deftest drover-remote-test-backoff ()
  "A peer unreachable is tried less and less often, its failures counted once in its history, tried at once when the user or the network says so."
  (drover-remote-test-with nil
    (let ((drover-remote-backoff '(0.05 0.1 2.0)))
      (setq drover-remote-peers `(("agent" :dial t :local ,(drover-remote-test--dead-registration dir))))
      (drover-remote-mode 1)
      (let* ((peer (drover-remote--peer "agent"))
             (connection (drover-remote--peer-dialed peer)))
        (drover-remote-test-wait (lambda () (>= (spyglass-connection-attempts connection) 3)) 10)
        (should (>= (spyglass-connection-attempts connection) 3))
        (drover-remote-test-wait (lambda () (spyglass-connection-next-attempt connection)) 2)
        ;; Backing off: the next attempt in about 2 s, which the state tells.
        (should (> (spyglass-connection-next-attempt connection) 0.5))
        (should (string-match-p "attempt [0-9]+, next in [0-9]s" (drover-remote--state-line peer)))
        ;; Each failure alike counted on one line.
        (let ((errors (seq-filter (lambda (event) (eq (nth 1 event) 'error)) (drover-remote--peer-history peer))))
          (should (= (length errors) 1))
          (should (>= (nth 3 (car errors)) 3)))
        (with-current-buffer (save-window-excursion (drover-remote-status) (get-buffer "*drover remote*"))
          (should (string-match-p "times since" (buffer-string)))
          (kill-buffer))
        ;; The user checking: tried now, its delays starting over.
        (drover-remote-check)
        (should (string-match-p "checked by the user" (drover-remote-test--newest peer 'retry)))
        (should (<= (spyglass-connection-attempts connection) 1))
        (drover-remote-test-wait (lambda () (and (spyglass-connection-next-attempt connection)
                                                 (> (spyglass-connection-next-attempt connection) 0.5)))
                                 5)
        ;; Waking from sleep: tried now.
        (setf (car (drover-remote--state-network drover-remote--state)) (- (float-time) 3600))
        (drover-remote--look-out)
        (should (string-match-p "woke" (drover-remote-test--newest peer 'retry)))
        (drover-remote-test-wait (lambda () (and (spyglass-connection-next-attempt connection)
                                                 (> (spyglass-connection-next-attempt connection) 0.5)))
                                 5)
        ;; The network changing: tried now.
        (setf (cdr (drover-remote--state-network drover-remote--state)) '("elsewhere"))
        (drover-remote--look-out)
        (should (string-match-p "network changed" (drover-remote-test--newest peer 'retry)))
        ;; Nothing changing: left to its delays.
        (drover-remote-test-wait (lambda () (and (spyglass-connection-next-attempt connection)
                                                 (> (spyglass-connection-next-attempt connection) 0.5)))
                                 5)
        (let ((retries (seq-count (lambda (event) (eq (nth 1 event) 'retry)) (drover-remote--peer-history peer))))
          (drover-remote--look-out)
          (should (= (seq-count (lambda (event) (eq (nth 1 event) 'retry)) (drover-remote--peer-history peer))
                     retries)))))))

(provide 'drover-remote-test)
;;; drover-remote-test.el ends here
