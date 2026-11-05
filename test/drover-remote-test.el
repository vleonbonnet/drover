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
(require 'drover-search)
(require 'drover-summary)
(require 'drover-transcript-fixtures)
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

(ert-deftest drover-remote-test-summaries-by-their-owner ()
  "A host's sessions carry the summaries it wrote; its peers summarize none of them."
  (drover-remote-test-with nil
    (let* ((asked nil)
           (drover-summary-delay 0)
           (drover-summary-catch-up 3600)
           (drover-summary-generate-function (lambda (prompt _callback) (push prompt asked))))
      (drover-summary-mode 1)
      (unwind-protect
          (progn
            (drover-remote-test-connect (drover-remote-test-start-agent dir))
            (drover-remote-test-command "sessions" :n 3)
            (drover-remote-test-synced 3)
            (drover-remote-test-command "summaries")
            ;; The turn of s0 ends there: its owner summarizes it.
            (drover-remote-test-command "change" :ids ["s0"] :status "idle" :now t)
            (drover-remote-test-synced
             3 (lambda (_) (equal (drover-session-brief (drover-index-session '(remote:agent fake . "s0")))
                                  "Done: summarized on the agent")))
            (let ((session (drover-index-session '(remote:agent fake . "s0"))))
              (should (equal (drover-session-brief-recap session) "Its recap"))
              (should (equal (drover-session-summary session) "Done: summarized on the agent")))
            ;; Its turns, recent as they are, are never sent from here.
            (drover-remote-test-settle 0.3)
            (should-not asked)
            (should-not (seq-some (lambda (key) (drover-session-host (drover-index-session key)))
                                  (hash-table-keys drover-summary--seen))))
        (drover-summary-mode -1)))))

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
        (should (string-match-p "Session s1@Agent" (spyglass--mirror-state-line
                                                     (buffer-local-value 'spyglass--mirror mirror))))
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
          ;; Eight lines, its mode line one; no header line, the owner having none.
          (should (= (cdr size) 7))
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

(ert-deftest drover-remote-test-mirror-shell-own-commands ()
  "A shell submitting through a command of its own, as agent-shell's, submits from its mirror; the cursor follows the input."
  (skip-unless (executable-find "cat"))
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 2)
    (drover-remote-test-command "terminal" :id "s0" :kind "agent-shell")
    (drover-remote-test-synced 2)
    (save-window-excursion
      (delete-other-windows)
      (let ((mirror (drover-remote-test-open "s0")))
        (drover-remote-test-live mirror)
        ;; Read elsewhere here: typing brings point back to the input.
        (goto-char (point-min))
        (spyglass-mirror-send-string "confirmed it works" mirror)
        (drover-remote-test-shows mirror "Agent> confirmed it works")
        (drover-remote-test-wait (lambda () (eql (spyglass-mirror-owner-point mirror)
                                                 (with-current-buffer mirror (point-max)))))
        (should (eql (spyglass-mirror-owner-point mirror) (with-current-buffer mirror (point-max))))
        (should (= (window-point) (spyglass-mirror-owner-point mirror)))
        (should (= (plist-get (drover-remote-test-terminal "s0") :point)
                   (1+ (length "Agent> confirmed it works"))))
        ;; Edited at the cursor.
        (spyglass-mirror-send-key "<left>" mirror)
        (spyglass-mirror-send-key "DEL" mirror)
        (spyglass-mirror-send-string "K" mirror)
        (drover-remote-test-shows mirror "confirmed it worKs")
        ;; RET submits through the shell's own command, not comint's.
        (spyglass-mirror-send-key "RET" mirror)
        (drover-remote-test-wait (lambda () (> (length (plist-get (drover-remote-test-terminal "s0") :submitted)) 0)))
        (should (equal (plist-get (drover-remote-test-terminal "s0") :submitted) ["confirmed it worKs"]))
        ;; Its interrupt is drover's, without asking there.
        (spyglass-mirror-send-key "C-c" mirror)
        (drover-remote-test-wait (lambda () (drover-remote-test-calls "interrupt")))
        (should (equal (mapcar #'cadr (drover-remote-test-calls "interrupt")) '("s0")))
        (kill-buffer mirror)))))

(ert-deftest drover-remote-test-mirror-closes-when-archived ()
  "Archiving a session whose shell this host mirrors closes the mirror, what was typed in it kept in the kill ring here."
  (skip-unless (executable-find "cat"))
  (drover-remote-test-with nil
    (drover-remote-test-start-full dir)
    (drover-remote-test-command "sessions" :n 4)
    (drover-remote-test-command "terminal" :id "s0" :kind "agent-shell")
    (drover-remote-test-command "terminal" :id "s3" :kind "agent-shell")
    (drover-remote-test-synced 4)
    (save-window-excursion
      ;; Its terminal killed there, the session still listed: the mirror
      ;; stays, readable, saying why.
      (let ((mirror (drover-remote-test-open "s3")))
        (drover-remote-test-live mirror)
        (drover-remote-test-command "kill-terminal" :id "s3")
        (drover-remote-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'closed)))
        (drover-remote-test-settle 0.5)
        (should (buffer-live-p mirror))
        (should (equal (spyglass-mirror-closed-reason mirror) "killed"))
        (kill-buffer mirror))
      (let ((mirror (drover-remote-test-open "s0"))
            (kill-ring nil)
            (messages nil))
        (drover-remote-test-live mirror)
        ;; The mirror's mode line, while no window shows it there: the
        ;; session's, as its host tells it.
        (with-current-buffer mirror
          (let ((line (funcall spyglass-mirror-mode-line-function)))
            (should (string-match-p "Opus 5.5 high" line))
            (should (string-match-p "Idle" line))))
        (spyglass-mirror-send-string "draft not sent" mirror)
        (drover-remote-test-wait (lambda () (string-search "draft not sent"
                                                           (plist-get (drover-remote-test-terminal "s0") :text))))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (when format-string (push (apply #'format format-string args) messages)))))
          (drover-archive (drover-remote-test-session "s0"))
          (drover-remote-test-wait (lambda () (and (not (buffer-live-p mirror)) kill-ring)) 10))
        (should-not (buffer-live-p mirror))
        (should (equal (car kill-ring) "draft not sent"))
        (should (seq-some (lambda (text) (string-match-p "the unsent input of Session s0 is in the kill ring" text))
                          messages))
        ;; Closed there too, its share gone.
        (should-not (drover-remote-test-shares))))))

(ert-deftest drover-remote-test-agent-shell-keys ()
  "A real agent-shell's keys, as a peer's mirror runs them: its submit, its override, drover's interrupt."
  (skip-unless (and (drover-remote-test--available-p) (require 'agent-shell nil t)))
  (with-temp-buffer
    (agent-shell-mode)
    (drover-remote--shell-keys (current-buffer))
    (should (eq (cdr (assoc "RET" spyglass-key-commands)) 'agent-shell-submit))
    (should (eq (cdr (assoc "<return>" spyglass-key-commands)) 'agent-shell-submit))
    (should (eq (cdr (assoc "M-RET" spyglass-key-commands)) 'agent-shell-submit-override))
    (should (eq (cdr (assoc "C-c" spyglass-key-commands)) 'drover-remote--shell-interrupt))
    ;; Never a command of its, as TAB moving between items, for a key not listed.
    (should-not (assoc "TAB" spyglass-key-commands))
    ;; The arrows recall the history, whatever M-p is bound to there.
    (should-not (assoc "<up>" spyglass-key-commands))))

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
              ;; What earlier tests allocated is collected now, not as
              ;; this measures: a heap of all of them takes 130 ms.
              (garbage-collect)
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

;;;; OpenCode sessions in full buffers

(defvar opencode-service-registration-file)
(defvar opencode-command)
(defvar opencode-notification-os-filter)

(defun drover-remote-test-oc-session (id)
  "Return this host's record of the agent's OpenCode session ID, or nil."
  (drover-changed-now)
  (drover-index-session (cons 'remote:agent (cons 'opencode id))))

(defun drover-remote-test-oc-requests (&optional method)
  "Return what reached the agent's OpenCode service, of METHOD if given, as (METHOD PATH BODY)."
  (mapcar (lambda (request) (append request nil))
          (plist-get (apply #'drover-remote-test-command "oc-requests" (and method (list :method method)))
                     :requests)))

(defun drover-remote-test-start-oc (dir access &rest environment)
  "Start the agent in DIR with its OpenCode service, granting ACCESS, ENVIRONMENT more; connect.
Its sessions ses_a and ses_b are listed here once it is connected."
  (let ((process-environment (append (list "DROVER_AGENT_OPENCODE=1" (concat "DROVER_AGENT_ACCESS=" access))
                                     environment process-environment)))
    (drover-remote-test-connect (drover-remote-test-start-agent dir)))
  (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) (intern access))))
  (drover-remote-test-command "oc-add" :sessions [["ses_a" "Agent work"] ["ses_b" "Other work"]])
  (drover-remote-test-wait (lambda () (and (drover-remote-test-oc-session "ses_a") (drover-remote-test-oc-session "ses_b"))))
  (should (drover-remote-test-oc-session "ses_a")))

(defun drover-remote-test-open-oc (id)
  "Open the agent's OpenCode session ID as RET does; return its buffer once shown."
  (drover-open (drover-remote-test-oc-session id))
  (let ((buffer (drover-remote-test-wait
                 (lambda ()
                   (let ((buffer (window-buffer (selected-window))))
                     (and (eq (buffer-local-value 'major-mode buffer) 'opencode-session-mode)
                          (equal (buffer-local-value 'opencode-session-id buffer) id)
                          (buffer-local-value 'drover-remote--shown buffer)
                          buffer))))))
    (should buffer)
    buffer))

(defmacro drover-remote-test-oc-isolated (&rest body)
  "Run BODY with opencode.el loaded, its local service out of reach.
Its registration and CLI name nothing, and drover's OpenCode backend,
which loading opencode.el registers, is dropped, lest it follow the
user's service: only the services BODY registers are reached."
  (declare (indent 0) (debug t))
  `(progn
     (require 'opencode nil t)
     (let ((opencode-service-registration-file
            (expand-file-name (make-temp-name "drover-no-opencode-") temporary-file-directory))
           (opencode-command "drover-test-no-opencode")
           (opencode-notification-os-filter nil))
       (when (assq 'opencode drover--backends)
         (drover-unregister-backend 'opencode))
       ,@body)))

(defmacro drover-remote-test-with-oc (&rest body)
  "Run BODY with this host's opencode.el following no service but those BODY opens."
  (declare (indent 0) (debug t))
  `(progn
     (require 'opencode nil t)
     (skip-unless (fboundp 'opencode-service-register))
     (unwind-protect (drover-remote-test-oc-isolated ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-live-p buffer) (buffer-local-value 'opencode--buffer-service buffer))
           (kill-buffer buffer)))
       (when-let* ((service (opencode-service-get "agent")))
         (opencode-service-unregister service)
         (opencode--service-reset service)
         (remhash "agent" opencode--services))
       (opencode-transport-unregister-relay "agent")
       (clrhash drover-remote--oc-buffers))))

(ert-deftest drover-remote-test-oc-pieces ()
  "An event stream goes to a peer in pieces of whole characters, lines when short."
  (let ((drover-remote--oc-piece 8))
    (should (= (drover-remote--oc-cut (encode-coding-string "ab\ncd" 'utf-8)) 3))
    (should (= (drover-remote--oc-cut (encode-coding-string "abc" 'utf-8)) 0))
    ;; é is two bytes, the second a continuation: never cut between them.
    (let ((bytes (encode-coding-string "abcdefgéxyz" 'utf-8)))
      (should (= (drover-remote--oc-cut bytes) 7))
      (should (equal (decode-coding-string (substring bytes 0 7) 'utf-8) "abcdefg")))
    (let ((sent nil)
          (pending (list (encode-coding-string "éééééé\nrest" 'utf-8))))
      (cl-letf (((symbol-function 'drover-remote--send)
                 (lambda (_connection message) (push (plist-get message :d) sent))))
        (drover-remote--oc-flush nil "id" pending))
      ;; Nothing lost nor cut within a character; a short tail without
      ;; its line's end waits for it.
      (should (equal (concat (apply #'concat (reverse sent)) (decode-coding-string (car pending) 'utf-8))
                     "éééééé\nrest"))
      (should (< (length (car pending)) 8))
      (should (seq-every-p (lambda (piece) (<= (string-bytes piece) 8)) sent)))))

(ert-deftest drover-remote-test-oc-refusals ()
  "A peer's OpenCode requests are checked against its access, whatever it greyed out."
  (should-not (drover-remote--oc-refusal 'read 'get "/api/session/ses_a/message?limit=5"))
  (should (drover-remote--oc-refusal 'read 'post "/api/session/ses_a/prompt"))
  (should-not (drover-remote--oc-refusal 'act 'post "/api/session/ses_a/prompt"))
  (should-not (drover-remote--oc-refusal 'act 'delete "/api/session/ses_a/inbox/msg_1"))
  (should-not (drover-remote--oc-refusal 'act 'delete "/api/session/ses_a/revert"))
  (should (drover-remote--oc-refusal 'act 'delete "/api/session/ses_a"))
  (should (drover-remote--oc-refusal 'act 'delete "/api/project/prj_1"))
  (should-not (drover-remote--oc-refusal 'full 'delete "/api/session/ses_a"))
  (should (drover-remote--oc-refusal 'full 'get "/etc/passwd"))
  (should (drover-remote--oc-refusal 'full 'get "/api/session x"))
  (should (drover-remote--oc-refusal 'full nil "/api/session"))
  (should (drover-remote--oc-refusal nil 'post "/api/session")))

(ert-deftest drover-remote-test-oc-buffer ()
  "RET on another host's OpenCode session opens it in a full buffer, talking to its service through its host."
  (drover-remote-test-with nil
    (drover-remote-test-with-oc
      (drover-remote-test-start-oc dir "act")
      (save-window-excursion
        (let* ((buffer (drover-remote-test-open-oc "ses_a"))
               (service (opencode-service-get "agent")))
          (should (equal (buffer-name buffer) "*OpenCode@agent: Agent work*"))
          (drover-remote-test-shows buffer "First reply of Agent work")
          ;; Its session is the host's: drover's keys act on it through the host.
          (with-current-buffer buffer
            (should (equal (drover-session-id (drover-session-at-point)) '(opencode . "ses_a")))
            (should (equal (drover-session-host (drover-session-at-point)) "agent"))
            (should (equal (opencode-location) "C:/Users/agent/project/")))
          (should (eq (drover-session-buffer (drover-remote-test-oc-session "ses_a")) buffer))
          ;; This host holds no password of that service: its host adds its own.
          (should-not (plist-get (opencode-call-with-service service (lambda () (opencode-sv opencode-service-connection)))
                                 :password))
          (should (string-prefix-p "relay://" (plist-get (opencode-call-with-service
                                                           service (lambda () (opencode-sv opencode-service-connection)))
                                                          :url)))
          ;; Its events stream through the host: one stream there.
          (drover-remote-test-wait (lambda () (opencode-call-with-service service #'opencode-events-connected-p)))
          (should (= (plist-get (drover-remote-test-command "oc-streams") :streams) 1))
          ;; A prompt sent here reaches the service there; its reply streams back.
          (with-current-buffer buffer
            (opencode-session-send "hello there" :show t))
          (drover-remote-test-shows buffer "Echo: hello there")
          (should (seq-find (lambda (request) (equal (nth 1 request) "/api/session/ses_a/prompt"))
                            (drover-remote-test-oc-requests "POST")))
          ;; A turn another client runs there shows here too.
          (drover-remote-test-command "oc-turn" :id "ses_a" :text "from elsewhere")
          (drover-remote-test-shows buffer "Echo: from elsewhere")
          ;; RET again shows the same buffer.
          (should (eq (drover-remote-test-open-oc "ses_a") buffer))
          ;; Archived there: closed here, as archiving closes it here,
          ;; what was typed at its prompt kept in the kill ring.
          (let ((other (drover-remote-test-open-oc "ses_b"))
                (kill-ring nil))
            (drover-remote-test-shows other "First reply of Other work")
            (with-current-buffer other
              (goto-char (point-max))
              (insert "typed, not sent"))
            (drover-remote-test-command "oc-archive" :id "ses_b")
            (drover-remote-test-wait (lambda () (not (buffer-live-p other))))
            (should-not (buffer-live-p other))
            (should (equal (car kill-ring) "typed, not sent")))
          ;; The local service is untouched: no buffer, no stream.
          (should-not (gethash "ses_a" opencode-session-buffers))
          ;; Killed here, the session runs on there.
          (kill-buffer buffer)
          (should-not (drover-remote--oc-buffer "agent" '(opencode . "ses_a")))
          (drover-remote-test-wait (lambda () (= (plist-get (drover-remote-test-command "oc-streams") :streams) 0)) 10)
          (should (= (plist-get (drover-remote-test-command "oc-streams") :streams) 0)))))))

(ert-deftest drover-remote-test-oc-read-only ()
  "A host granting read access lets this one read its OpenCode sessions, not change them."
  (drover-remote-test-with nil
    (drover-remote-test-with-oc
      (drover-remote-test-start-oc dir "read")
      (save-window-excursion
        (let ((buffer (drover-remote-test-open-oc "ses_a"))
              (failure nil))
          (drover-remote-test-shows buffer "First reply of Agent work")
          (with-current-buffer buffer
            (opencode-session-send "not allowed" :else (lambda (error) (setq failure error))))
          (drover-remote-test-wait (lambda () failure))
          (should (eql (opencode-error-status failure) 403))
          (should (string-match-p "read access" (opencode-error-message failure)))
          ;; Nothing reached the service but what reads.
          (should-not (drover-remote-test-oc-requests "POST"))
          (should-not (drover-remote-test-oc-requests "DELETE"))
          (kill-buffer buffer))))))

(ert-deftest drover-remote-test-oc-survives-a-drop ()
  "The link dropping, an OpenCode buffer follows its session again as it comes back, what ran meanwhile shown."
  (drover-remote-test-with nil
    (drover-remote-test-with-oc
      (let ((drover-remote-backoff '(1.0 1.0)))
        (drover-remote-test-start-oc dir "act")
        (save-window-excursion
          (let ((buffer (drover-remote-test-open-oc "ses_a"))
                (service (opencode-service-get "agent"))
                (peer (drover-remote--peer "agent")))
            (drover-remote-test-wait (lambda () (opencode-call-with-service service #'opencode-events-connected-p)))
            (drover-remote-test-command "oc-turn" :id "ses_a" :text "during the outage" :delay 0.4)
            (spyglass-connection-drop-stream (drover-remote--peer-dialed peer))
            (drover-remote-test-wait (lambda () (not (opencode-call-with-service service #'opencode-events-connected-p))) 5)
            (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state peer) 'open)) 20)
            (drover-remote-test-wait (lambda () (opencode-call-with-service service #'opencode-events-connected-p)) 20)
            (drover-remote-test-shows buffer "Echo: during the outage")
            ;; Still talking to it.
            (with-current-buffer buffer
              (opencode-session-send "after" :show t))
            (drover-remote-test-shows buffer "Echo: after")
            (kill-buffer buffer)))))))

(ert-deftest drover-remote-test-oc-older-peer ()
  "A peer whose drover relays no OpenCode service shows its OpenCode sessions as it tells them, saying why."
  (drover-remote-test-with nil
    (drover-remote-test-with-oc
      (drover-remote-test-start-oc dir "full" "DROVER_AGENT_CAPS=actions,bye,grant,mirror")
      (save-window-excursion
        (drover-open (drover-remote-test-oc-session "ses_a"))
        (let ((view (window-buffer (selected-window))))
          (should (eq (buffer-local-value 'major-mode view) 'drover-remote-session-mode))
          (should (string-match-p "older drover" (buffer-local-value 'drover-remote--note view)))
          (should-not (opencode-service-get "agent"))
          (should-not (drover-remote-test-oc-requests))
          (kill-buffer view))))))

;;;; Searching what was said there

(defconst drover-remote-test--capybara "11111111-1111-4111-8111-111111111111"
  "The conversation of the agent's Claude Code speaking of a capybara.")

(defconst drover-remote-test--build "22222222-2222-4222-8222-222222222222"
  "The conversation of the agent's Claude Code speaking of something else.")

(defun drover-remote-test-start-searching (dir &rest environment)
  "Start the agent in DIR with Claude Code's transcripts and an OpenCode service.
ENVIRONMENT is more of it.  Two conversations of Claude Code, one
speaking of a capybara, and two OpenCode sessions, ses_a speaking of it
too, are listed here once it is connected."
  (let ((claude (expand-file-name "agent-claude" dir)))
    (let ((drover-claude-code-home claude))
      (drover-fixture-claude drover-remote-test--capybara (expand-file-name "work/alpha/" dir)
                             :prompt "Teach the capybara to swim" :reply "The capybara swims now")
      (drover-fixture-claude drover-remote-test--build (expand-file-name "work/beta/" dir)
                             :prompt "Fix the build" :reply "It builds"))
    (apply #'drover-remote-test-start-oc dir "read" (concat "DROVER_AGENT_CLAUDE=" claude) environment))
  (drover-remote-test-command "oc-turn" :id "ses_a" :text "the capybara dives")
  (drover-remote-test-wait
   (lambda () (= 2 (seq-count (lambda (session) (eq (drover-session-origin session) 'claude-code))
                              (drover-remote-test-remote)))))
  (should (= 2 (seq-count (lambda (session) (eq (drover-session-origin session) 'claude-code))
                          (drover-remote-test-remote)))))

(defun drover-remote-test-search (query)
  "Search what was said for QUERY as `drover-search-text' does; return its result."
  (let ((result nil))
    (drover-search-text query (lambda (value) (setq result value)))
    (drover-remote-test-wait (lambda () result) 30)
    (should result)
    result))

(defun drover-remote-test-found (result)
  "Return the sessions RESULT of `drover-search-text' found, as (HOST ID SNIPPET)."
  (sort (mapcar (pcase-lambda (`(,session ,snippet ,_count))
                  (list (drover-session-host session) (or (drover-session-engine-id session)
                                                          (cdr-safe (drover-session-id session)))
                        snippet))
                (plist-get result :sessions))
        :key #'cadr))

(ert-deftest drover-remote-test-search-matches ()
  "A match travels named by its session, its snippet and position bounded; anything else is dropped."
  (let ((huge (make-string 500000 ?x)))
    (should (equal (drover-remote--decode-match '(:k ["opencode" "ses_a"] :s "the capybara" :p "msg_1")
                                                "core" 'remote:core)
                   '(:key (remote:core opencode . "ses_a") :snippet "the capybara" :position "msg_1")))
    (should (equal (drover-remote--decode-match '(:e "claude-code" :i "u-1" :s "x" :p 12) "core" 'remote:core)
                   '(:engine claude-code :id "u-1" :host "core" :snippet "x" :position 12)))
    (let ((match (drover-remote--decode-match (list :k ["opencode" "ses_a"] :s huge :p huge) "core" 'remote:core)))
      ;; Cut, an ellipsis after.
      (should (<= (length (plist-get match :snippet)) (1+ drover-remote-search-snippet)))
      (should (<= (length (plist-get match :position)) 129)))
    (should-not (drover-remote--decode-match '(:k ["Bad Name" "x"] :s "x") "core" 'remote:core))
    (should-not (drover-remote--decode-match '(:e "claude-code" :i [1 2]) "core" 'remote:core))
    (should-not (drover-remote--decode-match ["not" "an" "object"] "core" 'remote:core))
    (should-not (plist-get (drover-remote--decode-match '(:k ["a" "b"] :p (1 2)) "core" 'remote:core)
                           :position))
    ;; As this host sends them: plain, bounded, by key or by engine.
    (should (equal (drover-remote--wire-match (list :key '(opencode . "ses_a")
                                                    :snippet (propertize "hit" 'face 'bold) :position "msg_1"))
                   '(:k ["opencode" "ses_a"] :s "hit" :p "msg_1")))
    (should (equal (drover-remote--wire-match '(:engine claude-code :id "u-1" :snippet "hit" :position 4))
                   '(:e "claude-code" :i "u-1" :s "hit" :p 4)))
    (should (<= (length (plist-get (drover-remote--wire-match (list :key '(a . "b") :snippet huge)) :s))
                (1+ drover-remote-search-snippet)))
    (should-not (drover-remote--wire-match '(:snippet "no session")))))

(ert-deftest drover-remote-test-search-there ()
  "Searching what was said here searches another host's sessions there, with its own backends, as scoped here."
  (skip-unless (executable-find "rg"))
  (drover-remote-test-with nil
    (drover-remote-test-start-searching dir)
    (let ((result (drover-remote-test-search "capybara")))
      ;; Found there by its transcripts and its OpenCode service, named
      ;; here as the agent's sessions, with snippets of what was said.
      (should (equal (mapcar #'butlast (drover-remote-test-found result))
                     `(("agent" ,drover-remote-test--capybara) ("agent" "ses_a"))))
      (should (seq-every-p (lambda (found) (string-match-p "capybara" (nth 2 found)))
                           (drover-remote-test-found result)))
      (should-not (plist-get result :failed))
      (should (equal (plist-get (drover-remote-test-stats) :searches) 1))
      (should (member '("search" "capybara" :false) (drover-remote-test-calls "search"))))
    ;; As many matches as the search takes, there too.
    (let ((drover-search-text-limit 1))
      (should (= 1 (length (plist-get (drover-remote-test-search "capybara") :sessions)))))
    ;; Only the sessions offered here are searched there: under a filter
    ;; on another host, none, nothing asked.
    (let ((drover-filter '(:host (local))))
      (should-not (plist-get (drover-remote-test-search "capybara") :sessions))
      (should (= 2 (length (drover-remote-test-calls "search")))))
    (let ((drover-filter '(:project "beta")))
      (should-not (plist-get (drover-remote-test-search "capybara") :sessions)))
    ;; An archived session there is searched while archived ones show here.
    (drover-remote-test-command "oc-archive" :id "ses_a")
    (drover-remote-test-wait (lambda () (let ((session (drover-remote-test-oc-session "ses_a")))
                                          (or (null session) (drover-session-archived-p session)))))
    (should-not (member "ses_a" (mapcar #'cadr (drover-remote-test-found (drover-remote-test-search "capybara")))))
    (drover-set-show-archived t)
    (unwind-protect
        (progn
          (drover-remote-test-wait (lambda () (seq-find (lambda (session)
                                                          (equal (drover-session-id session) '(opencode . "ses_a")))
                                                        (drover-known-sessions))))
          (should (member "ses_a" (mapcar #'cadr (drover-remote-test-found (drover-remote-test-search "capybara"))))))
      (drover-set-show-archived nil))))

(ert-deftest drover-remote-test-search-stops-there ()
  "A search typed past, or whose link drops, stops there; one failing there says so."
  (drover-remote-test-with nil
    (drover-remote-test-start-searching dir)
    (drover-remote-test-command "search-set" :delay 5)
    ;; Typed past: stopped there.
    (let ((stop (drover-search-text "capybara" #'ignore)))
      (drover-remote-test-wait (lambda () (drover-remote-test-calls "search")))
      (funcall stop)
      (drover-remote-test-wait (lambda () (drover-remote-test-calls "search-cancel")))
      (should (equal (mapcar #'cadr (drover-remote-test-calls "search-cancel")) '("capybara")))
      (should (equal (plist-get (plist-get (drover-remote-test-command "stats" :peer "viewer") :peer)
                                :searches-stopped)
                     1)))
    (should (= 0 (hash-table-count drover-remote--searches)))
    ;; The link dropping ends it here, and stops it there.
    (let ((result nil))
      (drover-search-text "capybara" (lambda (value) (setq result value)))
      (drover-remote-test-wait (lambda () (= 2 (length (drover-remote-test-calls "search")))))
      (spyglass-connection-drop-stream (drover-remote--peer-connection (drover-remote--peer "agent")))
      (drover-remote-test-wait (lambda () result))
      (should (string-match-p "link to agent ended" (cdr (assq 'remote:agent (plist-get result :failed)))))
      (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state (drover-remote--peer "agent")) 'open)))
      (drover-remote-test-wait (lambda () (= 2 (length (drover-remote-test-calls "search-cancel")))))
      (should (= 2 (length (drover-remote-test-calls "search-cancel")))))
    ;; Failing there: what it found stays, the failure told.
    (drover-remote-test-command "search-set" :delay 0.01 :failure "its index is broken")
    (let ((result (drover-remote-test-search "capybara")))
      (should (equal (cdr (assq 'remote:agent (plist-get result :failed)))
                     "OpenCode: its index is broken")))))

(ert-deftest drover-remote-test-search-older-peer ()
  "A peer whose drover searches nothing for others is left out of the search, silently."
  (drover-remote-test-with nil
    (drover-remote-test-start-searching dir "DROVER_AGENT_CAPS=actions,bye,grant,mirror,opencode")
    (let ((result (drover-remote-test-search "capybara")))
      (should-not (plist-get result :sessions))
      (should-not (plist-get result :failed))
      (should-not (drover-remote-test-calls "search")))))

;;;; Cloning a conversation to another host

(ert-deftest drover-remote-test-clone-rewrite ()
  "The records' spelling of a directory is rewritten as pieces arrive, one cut between two pieces found whole."
  (let* ((pairs (list (cons "\"cwd\":\"/a/b\"" "\"cwd\":\"C:\\\\x\"")))
         (text "{\"cwd\":\"/a/b\",\"x\":1}\n{\"cwd\":\"/a/bc\"}\n{\"cwd\":\"/a/b\"}\n")
         (expected (string-replace "\"cwd\":\"/a/b\"" "\"cwd\":\"C:\\\\x\"" text)))
    (dotimes (cut (1+ (length text)))
      (let* ((first (drover-remote--clone-rewrite "" (substring text 0 cut) pairs nil))
             (second (drover-remote--clone-rewrite (cdr first) (substring text cut) pairs t)))
        (should (equal (concat (car first) (car second)) expected))))
    ;; A byte a piece.
    (let ((carry "") (out nil))
      (dotimes (i (length text))
        (let ((cut (drover-remote--clone-rewrite carry (substring text i (1+ i)) pairs (= i (1- (length text))))))
          (push (car cut) out)
          (setq carry (cdr cut))))
      (should (equal (apply #'concat (nreverse out)) expected)))))

(ert-deftest drover-remote-test-clone-directories ()
  "A clone goes under the directory a peer's table maps, else under this host's home, as it was under the peer's."
  (let* ((root (file-name-as-directory (make-temp-file "drover-clone-dirs" t)))
         (peer (drover-remote--peer-make
                :name "core"
                :properties (list :directories (list (cons "C:/Users/val/local/" (expand-file-name "mine/" root)))))))
    (unwind-protect
        (progn
          (should (equal (drover-remote--clone-map peer "C:/Users/val/local/source/x/" "local/source/x/")
                         (cons (expand-file-name "mine/source/x/" root) nil)))
          (make-directory (expand-file-name "mine/source/x/" root) t)
          ;; Drive letters and case as Windows has them, backslashes too.
          (should (equal (drover-remote--clone-map peer "c:\\users\\VAL\\local\\source\\x" nil)
                         (cons (expand-file-name "mine/source/x/" root) t)))
          ;; Not mapped: under this host's home, as it was under the peer's.
          (should (equal (car (drover-remote--clone-map peer "/home/val/work/y/" "work/y/"))
                         (file-name-as-directory (expand-file-name "work/y/" "~"))))
          (should-not (drover-remote--clone-map peer "/home/val/../../etc/" "../../etc/"))
          (should-not (drover-remote--clone-map peer "/srv/x/" "/srv/x/"))
          (should-not (drover-remote--clone-map peer "/srv/x/" nil))
          (should (equal (drover-remote--clone-home-relative (expand-file-name "~/local/x")) "local/x/"))
          (should-not (drover-remote--clone-home-relative "/nowhere/x/")))
      (delete-directory root t))))

(ert-deftest drover-remote-test-clone-description ()
  "What a peer describes of a clone is checked: names of files under the clone alone, sizes, engines, a bound."
  (let ((good (list :x "viewer-1-2" :engine "claude-code" :id drover-remote-test--capybara :title "T"
                    :directory "/a/" :home "a/" :cwd "/a" :origin "agent-session://viewer/claude-code/x"
                    :size 3 :parts [["x.jsonl" 3]])))
    (should (equal (plist-get (drover-remote--clone-description good) :parts) '(("x.jsonl" 3))))
    (should (eq (plist-get (drover-remote--clone-description good) :engine) 'claude-code))
    (dolist (name '("../x" "/etc/passwd" "a\\b" "C:x" "a//b" "~/x" "" "a/./b"))
      (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :parts (vector (vector name 1)))))))
    (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :parts [["x" -1]]))))
    (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :engine "rm"))))
    (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :id "a b/../c"))))
    (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :x "a b"))))
    (let ((drover-remote-clone-limit 10))
      (should (stringp (drover-remote--clone-description (plist-put (copy-sequence good) :parts [["a" 6] ["b" 6]])))))
    (should (<= (length (plist-get (drover-remote--clone-description
                                    (plist-put (copy-sequence good) :title (make-string 500000 ?t)))
                                   :title))
                (1+ drover-line-limit)))))

(ert-deftest drover-remote-test-clone-needs-the-remote-layer ()
  "Without the remote layer, nothing is cloned, and the command says what it needs."
  (let ((drover-clone-function nil)
        (session (drover-session-create :backend 'fake :id "x" :engine 'claude-code :engine-id "x")))
    (should-not (drover-clone-p session))
    (should-error (drover-clone session) :type 'user-error)))

(ert-deftest drover-remote-test-clone-records ()
  "A file of records is copied to its last whole record; its working directory is read as written."
  (let ((file (make-temp-file "drover-records")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "{\"cwd\":\"C:\\\\Users\\\\vål\",\"a\":1}\n{\"b\":2}\n{\"partial" nil file nil 'silent))
          (should (= (drover-lines-size file) (string-bytes "{\"cwd\":\"C:\\\\Users\\\\vål\",\"a\":1}\n{\"b\":2}\n")))
          (should (equal (drover-recorded-cwd file) "C:\\\\Users\\\\vål")))
      (delete-file file))))

(ert-deftest drover-remote-test-clone-opencode-body ()
  "An OpenCode export becomes an import as it is, its location added; one shaped otherwise is parsed."
  (let ((export "{\"data\":{\"info\":{\"id\":\"ses_a\",\"title\":\"vå\"},\"messages\":[{\"id\":\"m1\"}]}}\n"))
    (should (equal (drover-remote--oc-import-body (encode-coding-string export 'utf-8) "/w/x")
                   (encode-coding-string
                    "{\"info\":{\"id\":\"ses_a\",\"title\":\"vå\"},\"messages\":[{\"id\":\"m1\"}],\"location\":{\"directory\":\"/w/x\"}}"
                    'utf-8))))
  (let ((body (drover-remote--oc-import-body
               (encode-coding-string "{\"data\":{\"messages\":[],\"info\":{\"id\":\"ses_b\",\"x\":null}}}" 'utf-8)
               "/w/y")))
    (should (equal (json-parse-string (decode-coding-string body 'utf-8) :object-type 'plist :null-object :null)
                   '(:info (:id "ses_b" :x :null) :messages [] :location (:directory "/w/y")))))
  (should (equal (drover-remote--oc-clone-directory "/w/x/") "/w/x"))
  (when (memq system-type '(windows-nt ms-dos))
    (should (equal (drover-remote--oc-clone-directory "c:/Users/x/") "C:/Users/x"))))

(defmacro drover-remote-test-cloning (&rest body)
  "Run BODY as the host `viewer', its engines' homes throwaway under `home'."
  (declare (indent 0) (debug t))
  `(drover-remote-test-with nil
     (drover-test-with-homes
       ,@body)))

(defun drover-remote-test-clone-start (dir access &rest environment)
  "Start the agent in DIR granting ACCESS, with ENVIRONMENT more, and connect."
  (let ((process-environment (append (cons (concat "DROVER_AGENT_ACCESS=" access) environment)
                                     process-environment)))
    (drover-remote-test-connect (drover-remote-test-start-agent dir)))
  (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted (drover-remote--peer "agent")) (intern access))))
  (drover-remote-test-wait (lambda () (drover-remote--capable-p (drover-remote--peer "agent") "clone"))))

(defun drover-remote-test-engine-session (engine id)
  "Return this host's record of the agent's conversation ID of ENGINE, once listed."
  (drover-remote-test-wait
   (lambda ()
     (drover-changed-now)
     (seq-find (lambda (session) (and (eq (drover-session-engine session) engine)
                                      (equal (drover-session-engine-id session) id)))
               (drover-remote-test-remote)))))

(defun drover-remote-test-rewritten (file from to)
  "Return FILE's bytes, the records' working directory FROM written TO."
  (string-replace (encode-coding-string (concat "\"cwd\":" (json-serialize from)) 'utf-8)
                  (encode-coding-string (concat "\"cwd\":" (json-serialize to)) 'utf-8)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally file)
                    (buffer-string))))

(defun drover-remote-test-bytes (file)
  "Return FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(ert-deftest drover-remote-test-clone-claude-here ()
  "A Claude Code conversation of another host is cloned here, in a directory of this host, its origin noted, then resumed."
  (drover-remote-test-cloning
    (let* ((agent-claude (expand-file-name "agent-claude" dir))
           (there (expand-file-name "work/alpha" dir))
           (id drover-remote-test--capybara)
           (source (let ((drover-claude-code-home agent-claude))
                     (drover-fixture-claude id there :prompt "Teach the capybara to swim"
                                            :reply "The capybara swims now" :filler 700000
                                            :subagents '(("agent-a1" "Look around" "Explore")))))
           (here (file-name-as-directory (expand-file-name "clones/alpha" home)))
           (opened nil))
      (make-directory here t)
      (drover-remote-test-clone-start dir "read" (concat "DROVER_AGENT_CLAUDE=" agent-claude))
      (let ((session (drover-remote-test-engine-session 'claude-code id)))
        (should session)
        (should (drover-clone-p session))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "viewer"))
                  ((symbol-function 'read-directory-name) (lambda (&rest _) here))
                  ((symbol-function 'drover-links-open) (lambda (conversation &rest _) (setq opened conversation))))
          (drover-clone session))
        (should (equal opened (list :engine 'claude-code :id id :host nil)))
        ;; Its transcript where Claude Code resumes it here, its records
        ;; naming this host's directory, nothing else changed; its
        ;; subagents beside it.
        (let ((file (drover-fixture-claude-file id here)))
          (should (file-exists-p file))
          (should (equal (drover-remote-test-bytes file)
                         (drover-remote-test-rewritten source there (drover-claude-code--record-directory here))))
          (should (file-exists-p (expand-file-name (concat id "/subagents/agent-a1.jsonl") (file-name-directory file))))
          (should (file-exists-p (expand-file-name (concat id "/subagents/agent-a1.meta.json") (file-name-directory file)))))
        (should (equal (drover-remote-clone-origin (drover-session-create :engine 'claude-code :engine-id id))
                       (concat "agent-session://agent/claude-code/" id)))
        ;; Its owner forgot what it exported.
        (drover-remote-test-wait (lambda () (zerop (plist-get (drover-remote-test-command "clone-state") :exports))))
        (should (zerop (plist-get (drover-remote-test-command "clone-state") :exports)))
        (should (zerop (hash-table-count drover-remote--clones)))
        ;; Once only: a conversation recorded here is not replaced.
        (setq opened nil)
        (let ((said nil))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "viewer"))
                    ((symbol-function 'read-directory-name) (lambda (&rest _) here))
                    ((symbol-function 'drover-links-open) (lambda (conversation &rest _) (setq opened conversation)))
                    ((symbol-function 'drover-message) (lambda (format &rest args) (push (apply #'format format args) said))))
            (drover-clone session))
          (should-not opened)
          (should (seq-find (lambda (line) (string-match-p "recorded this conversation here already" line)) said)))))))

(ert-deftest drover-remote-test-clone-claude-there ()
  "A Claude Code conversation of this host is cloned to a peer granting full access, in its directory, then opened there."
  (drover-remote-test-cloning
    (let* ((id drover-remote-test--build)
           (here (expand-file-name "work/beta" home))
           (source (drover-fixture-claude id here :prompt "Fix the build" :reply "It builds"))
           (target (file-name-as-directory (expand-file-name "agent-target" dir)))
           (record (drover-session-create :backend 'fake :id "local-1" :title "Fix the build"
                                          :directory (file-name-as-directory here)
                                          :engine 'claude-code :engine-id id))
           (opened nil))
      (make-directory target t)
      (drover-remote-test-clone-start dir "full" "DROVER_AGENT_CLAUDE=here")
      (should (equal (mapcar #'car (drover-remote--clone-targets record)) '("agent")))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "agent"))
                ((symbol-function 'read-string) (lambda (&rest _) target))
                ((symbol-function 'drover-open) (lambda (session &rest _) (setq opened session))))
        (drover-clone record))
      ;; There, where Claude Code resumes it, listed there, opened here through it.
      (let* ((agent-home (plist-get (drover-remote-test-command "claude-home") :home))
             (file (let ((drover-claude-code-home agent-home)) (drover-fixture-claude-file id target))))
        (should (file-exists-p file))
        (should (equal (drover-remote-test-bytes file)
                       (drover-remote-test-rewritten source here (drover-claude-code--record-directory target)))))
      (should opened)
      (should (equal (drover-session-host opened) "agent"))
      (should (equal (drover-session-engine-id opened) id))
      ;; There, its origin is this host's conversation.
      (should (string-search (concat "agent-session://viewer/claude-code/" id)
                             (with-temp-buffer
                               (insert-file-contents (expand-file-name "agent/state/clones.eld" dir))
                               (buffer-string))))
      (should (zerop (hash-table-count drover-remote--exports))))))

(ert-deftest drover-remote-test-clone-needs-full-access ()
  "Cloning to a peer needs full access there, whatever this host asks."
  (drover-remote-test-cloning
    (let* ((id drover-remote-test--build)
           (here (expand-file-name "work/beta" home))
           (record (progn (drover-fixture-claude id here)
                          (drover-session-create :backend 'fake :id "local-1" :title "Fix the build"
                                                 :directory (file-name-as-directory here)
                                                 :engine 'claude-code :engine-id id))))
      (drover-remote-test-clone-start dir "act" "DROVER_AGENT_CLAUDE=here")
      (should-not (drover-remote--clone-targets record))
      (should-not (drover-clone-p record))
      (should (string-match-p "full access" (drover-remote--clone-why-not record)))
      (should-error (drover-clone record) :type 'user-error)
      ;; Asked all the same, it refuses.
      (pcase-let ((`(,_reply ,error) (drover-remote--wait (drover-remote--peer "agent")
                                                          (list :op "clone-where" :directory here) 10)))
        (should (string-match-p "not full" error)))
      (pcase-let ((`(,_reply ,error)
                   (drover-remote--wait (drover-remote--peer "agent")
                                        (list :op "clone" :target dir :x "viewer-x-1" :engine "claude-code"
                                              :id id :parts (vector (vector (concat id ".jsonl") 10)))
                                        10)))
        (should (string-match-p "not full" error)))
      (should (zerop (plist-get (drover-remote-test-command "clone-state") :clones))))))

(ert-deftest drover-remote-test-clone-codex-here ()
  "A Codex conversation of another host is cloned here: its rollout among this host's, its directory rewritten."
  (drover-remote-test-cloning
    (let* ((agent-codex (expand-file-name "agent-codex" dir))
           (there (expand-file-name "work/gamma" dir))
           (id "019a0000-0000-7000-8000-000000000001")
           (source (let ((drover-codex-home agent-codex))
                     (drover-fixture-codex id there :prompt "Find the axolotl")))
           (here (file-name-as-directory (expand-file-name "clones/gamma" home)))
           (opened nil))
      (make-directory here t)
      (drover-remote-test-clone-start dir "read" (concat "DROVER_AGENT_CODEX=" agent-codex))
      (let ((session (drover-remote-test-engine-session 'codex id)))
        (should session)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "viewer"))
                  ((symbol-function 'read-directory-name) (lambda (&rest _) here))
                  ((symbol-function 'drover-links-open) (lambda (conversation &rest _) (setq opened conversation))))
          (drover-clone session))
        (should (equal opened (list :engine 'codex :id id :host nil)))
        (let ((file (drover-fixture-codex-file id 1790000000.0)))
          (should (file-exists-p file))
          (should (equal (drover-codex-rollout-file id) file))
          (should (equal (drover-remote-test-bytes file)
                         (drover-remote-test-rewritten source there (drover-codex--record-directory here))))
          (should (equal (drover-codex-session-directory id)
                         (file-name-as-directory (drover-codex--record-directory here)))))))))

(ert-deftest drover-remote-test-clone-opencode-here ()
  "An OpenCode session of another host is cloned into this host's service, under the directory the peer's table maps."
  (drover-remote-test-cloning
    (require 'drover-fake-opencode)
    (require 'opencode)
    (skip-unless (fboundp 'opencode-service-register))
    (let* ((mine (file-name-as-directory (expand-file-name "mine" home)))
           (service-dir (file-name-as-directory (expand-file-name "viewer-service" dir)))
           (opened nil))
      (make-directory (expand-file-name "project" mine) t)
      (make-directory service-dir t)
      (let ((opencode-service-registration-file (drover-fake-opencode-start service-dir))
            (opencode-command "drover-test-no-opencode")
            (opencode-notification-os-filter nil))
        (unwind-protect
            (progn
              (let ((process-environment (append '("DROVER_AGENT_OPENCODE=1" "DROVER_AGENT_ACCESS=read")
                                                 process-environment)))
                (drover-remote-test-connect (drover-remote-test-start-agent dir)
                                            :directories `(("C:/Users/agent/" . ,mine))))
              (drover-remote-test-command "oc-add" :sessions [["ses_a" "Agent work"]])
              (drover-remote-test-command "oc-turn" :id "ses_a" :text "the capybara dives")
              (let ((session (drover-remote-test-wait (lambda () (drover-remote-test-oc-session "ses_a")))))
                (should session)
                (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "viewer"))
                          ((symbol-function 'read-directory-name) (lambda (_prompt default &rest _) default))
                          ((symbol-function 'drover-links-open) (lambda (conversation &rest _) (setq opened conversation))))
                  (drover-clone session))
                (should (equal opened (list :engine 'opencode :id "ses_a" :host nil)))
                ;; In this host's service, its messages all, run where the table maps.
                (let ((info (gethash "ses_a" drover-fake-opencode--sessions)))
                  (should info)
                  (should (equal (alist-get 'directory (alist-get 'location info))
                                 (directory-file-name (expand-file-name "project" mine))))
                  (should (equal (alist-get 'title info) "Agent work"))
                  (should (seq-find (lambda (message) (equal (alist-get 'text message) "the capybara dives"))
                                    (gethash "ses_a" drover-fake-opencode--messages))))
                ;; Not twice.
                (setq opened nil)
                (let ((said nil))
                  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "viewer"))
                            ((symbol-function 'read-directory-name) (lambda (_prompt default &rest _) default))
                            ((symbol-function 'drover-links-open) (lambda (conversation &rest _) (setq opened conversation)))
                            ((symbol-function 'drover-message) (lambda (format &rest args) (push (apply #'format format args) said))))
                    (drover-clone session))
                  (should-not opened)
                  (should (seq-find (lambda (line) (string-match-p "has this session already" line)) said)))))
          (when (process-live-p drover-fake-opencode--server)
            (delete-process drover-fake-opencode--server))
          (when (fboundp 'opencode--service-reset)
            (ignore-errors (opencode--service-reset (opencode-service-get nil)))))))))

(ert-deftest drover-remote-test-clone-link-drops ()
  "A clone whose link drops fails, saying so, its staged files removed; its owner forgets the export later."
  (drover-remote-test-cloning
    (let* ((agent-claude (expand-file-name "agent-claude" dir))
           (id drover-remote-test--capybara)
           (here (file-name-as-directory (expand-file-name "clones/alpha" home)))
           (outcome nil))
      (let ((drover-claude-code-home agent-claude))
        (drover-fixture-claude id (expand-file-name "work/alpha" dir) :filler (* 8 1024 1024)))
      (make-directory here t)
      (drover-remote-test-clone-start dir "read" (concat "DROVER_AGENT_CLAUDE=" agent-claude))
      (let* ((peer (drover-remote--peer "agent"))
             (session (drover-remote-test-engine-session 'claude-code id))
             (description (drover-remote--clone-description
                           (car (drover-remote--wait peer (list :op "clone-export"
                                                                :key (drover-remote--wire-key session))
                                                     20))))
             (xid (drover-remote--clone-receive peer description here
                                                (lambda (id why) (setq outcome (or id why)))))
             (staging (plist-get (gethash xid drover-remote--clones) :staging)))
        (should (file-directory-p staging))
        (drover-remote-test-wait (lambda () (> (plist-get (gethash xid drover-remote--clones) :received) 0)))
        (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
        (drover-remote-test-wait (lambda () outcome))
        (should (string-match-p "ended" outcome))
        (should-not (file-exists-p staging))
        (should (zerop (hash-table-count drover-remote--clones)))
        (should-not (file-exists-p (drover-fixture-claude-file id here)))))))

(provide 'drover-remote-test)
;;; drover-remote-test.el ends here
