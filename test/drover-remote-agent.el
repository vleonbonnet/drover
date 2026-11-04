;;; drover-remote-agent.el --- Another Emacs for the tests of drover's remote layer  -*- lexical-binding: t; -*-

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

;; A batch Emacs the tests and the bench of the remote layer start, on
;; this host or on another through SSH: drover's core and remote layer,
;; spyglass, and a fake backend, `fake', of sessions made on command.
;; It runs `drover-remote-mode' as the host DROVER_AGENT_HOST, its
;; spyglass server registered in DROVER_AGENT_DIR/server.json, its state
;; there too, and takes commands as requests on the channel
;; "drover-test" of any connection: make, change and remove sessions,
;; break streams, dial back, tell what it holds, restart its remote layer.
;; Its backend performs actions, noting each, its confirmations held on
;; command.  With DROVER_AGENT_STRICT set, it refuses hosts it does not
;; know; DROVER_AGENT_ACCESS is the access it grants the others, `read' by
;; default; DROVER_AGENT_DIAL, a registration file, has it dial the host
;; registered there as it starts; DROVER_AGENT_OLD has it greet as a drover
;; telling no capabilities, as an older one, DROVER_AGENT_CAPS as one
;; telling those, comma-separated.  Its fake sessions may run terminals
;; on command, a process in a pty (`cat'), or through pipes where there is
;; no pty, or a comint buffer, which its backend shares as front ends'
;; terminals and shells are; opening a session without one starts one,
;; as a front end resuming a conversation would, displayed as front ends
;; display them.  DROVER_AGENT_UNSHARE is `drover-remote-unshare-delay'.
;; It says READY on
;; standard error once it listens, kills itself past 2 GB of resident
;; memory, and after ten minutes without a command.  It loads neither
;; drover's lists nor its test helpers, so that its files are few to copy.

;;; Code:

(require 'cl-lib)
(require 'drover-core)
(require 'drover-remote)
(require 'spyglass)
(require 'comint)

(defvar drover-remote-agent--sessions (make-hash-table :test #'equal)
  "The fake backend's sessions, by identifier.")

(defvar drover-remote-agent--order nil
  "The identifiers of the fake sessions, in order.")

(defvar drover-remote-agent--last-command (float-time)
  "When the last command came.")

(defvar drover-remote-agent--quit nil
  "Non-nil once told to quit.")

(defvar drover-remote-agent--archived (make-hash-table :test #'equal)
  "The fake backend's archived sessions, by identifier.")

(defvar drover-remote-agent--calls nil
  "The actions the fake backend performed, newest first, as [ACTION ID ARGUMENT].")

(defvar drover-remote-agent--terminals (make-hash-table :test #'equal)
  "The buffers of the fake sessions running terminals, by identifier.")

(defvar drover-remote-agent--hold nil
  "Non-nil while confirmations are held; then the functions confirming, newest first.")

(defun drover-remote-agent--confirm (function)
  "Call FUNCTION, a confirmation, now or once released."
  (if drover-remote-agent--hold
      (push function (cdr drover-remote-agent--hold))
    (funcall function)))

(defun drover-remote-agent--action (action)
  "Return the fake backend's operation ACTION, a string, confirming.
It notes the call, changes the session as ACTION would, and confirms."
  (lambda (session &optional then else)
    (let ((id (drover-session-id session)))
      (push (vector action id :false) drover-remote-agent--calls)
      (drover-remote-agent--confirm
       (lambda ()
         (let ((live (gethash id drover-remote-agent--sessions))
               (archived (gethash id drover-remote-agent--archived)))
           (if (not (or live archived))
               (when else (funcall else "no such session"))
             (pcase action
               ("interrupt" (drover-remote-agent--set id (drover-remote-agent--with live :status 'idle)))
               ("mark-seen" (drover-remote-agent--set id (drover-remote-agent--with live :status 'idle)))
               ("archive" (when live
                            (puthash id (drover-remote-agent--with live :archived (float-time))
                                     drover-remote-agent--archived)
                            (drover-remote-agent--set id nil)))
               ("unarchive" (when archived
                              (remhash id drover-remote-agent--archived)
                              (drover-remote-agent--set id (drover-remote-agent--with archived :archived nil))))
               ("delete" (remhash id drover-remote-agent--archived)
                (drover-remote-agent--set id nil)))
             (drover-changed-now)
             (when then (funcall then)))))))))

(defun drover-remote-agent--with (session &rest slots)
  "Return a copy of SESSION with SLOTS, a plist of slot names as keywords, set."
  (let ((copy (copy-sequence session)))
    (cl-loop for (slot value) on slots by #'cddr
             do (setf (cl-struct-slot-value 'drover-session (intern (substring (symbol-name slot) 1)) copy)
                      value))
    (setf (drover-session-updated copy) (float-time))
    copy))

(defun drover-remote-agent--say (format-string &rest args)
  "Tell the driver FORMAT-STRING with ARGS, on standard error."
  (message "DROVER-AGENT %s" (apply #'format format-string args)))

(defun drover-remote-agent--terminal (id kind)
  "Start the terminal of fake session ID, of KIND, \"pty\" or \"comint\", and return it.
A pty runs `cat', sized by viewers as front ends' terminals are; where
processes run through pipes, as on Windows, `cat' or, without it, a
buffer echoing what is typed."
  (let ((buffer (generate-new-buffer (format "*fake terminal %s*" id))))
    (with-current-buffer buffer
      (setq default-directory temporary-file-directory)
      (pcase kind
        ("comint"
         (if (executable-find "cat")
             (make-comint-in-buffer (format "fake-shell-%s" id) buffer "cat")
           (comint-mode)))
        (_
         (if (executable-find "cat")
             (progn
               (make-process :name (format "fake-terminal-%s" id) :buffer buffer :command '("cat")
                             :connection-type 'pty :noquery t)
               (setq-local spyglass-refit-function #'spyglass-refit-process))
           (setq-local spyglass-input-function
                       (lambda (kind value)
                         (goto-char (point-max))
                         (insert (if (eq kind 'string) value (if (member value '("RET" "C-j")) "\n" "")))))))))
    (puthash id buffer drover-remote-agent--terminals)
    (drover-backend-changed 'fake id)
    buffer))

(defun drover-remote-agent--digest (buffer)
  "Return the MD5 of BUFFER's text, encoded as UTF-8 with Unix newlines.
Not of the buffer as `secure-hash' encodes it, with its coding system,
whose newlines are CRLF on Windows."
  (with-current-buffer buffer
    (secure-hash 'md5 (encode-coding-string (buffer-substring-no-properties (point-min) (point-max))
                                            'utf-8-unix t))))

(defun drover-remote-agent--buffer (session)
  "Return the live terminal of SESSION, a fake session, or nil."
  (let ((buffer (gethash (drover-session-id session) drover-remote-agent--terminals)))
    (and (buffer-live-p buffer) buffer)))

(defun drover-remote-agent--open (session &optional _other-window)
  "Show SESSION's terminal, starting one as a front end resuming it would."
  (push (vector "open" (drover-session-id session) :false) drover-remote-agent--calls)
  (pop-to-buffer-same-window (or (drover-remote-agent--buffer session)
                                 (drover-remote-agent--terminal (drover-session-id session) "pty"))))

(defun drover-remote-agent--register ()
  "Register the fake backend, and a front end resuming its conversations."
  (drover-register-resumer 'fake 'fake (lambda (id _directory)
                                         (drover-remote-agent--terminal id "pty"))
                           :by-id t)
  (drover-register-backend
   'fake :label "Fake"
   :buffer #'drover-remote-agent--buffer
   :runs-in-buffer t
   :open #'drover-remote-agent--open
   :sessions (lambda () (delq nil (mapcar (lambda (id) (gethash id drover-remote-agent--sessions))
                                          drover-remote-agent--order)))
   :session (lambda (id) (gethash id drover-remote-agent--sessions))
   :recap (lambda (session callback)
            (funcall callback (list :prompt (concat "Prompt of " (drover-session-id session))
                                    :reply (concat "Reply of " (drover-session-id session))
                                    :changes (list (list "a.el" 3 1)))))
   :preview (lambda (session callback)
              (funcall callback (concat "# Preview of " (drover-session-id session)
                                        "\n\nSome **bold** text and `code`.\n\n"
                                        "```elisp\n(defun drover-agent-f () 1)\n```\n")))
   :interrupt (drover-remote-agent--action "interrupt") :interrupt-confirms t
   :mark-seen (drover-remote-agent--action "mark-seen") :mark-seen-confirms t
   :archive (drover-remote-agent--action "archive") :archive-confirms t
   :unarchive (drover-remote-agent--action "unarchive") :unarchive-confirms t
   :delete (drover-remote-agent--action "delete") :delete-confirms t
   :rename (lambda (session title)
             (push (vector "rename" (drover-session-id session) title) drover-remote-agent--calls)
             (drover-remote-agent--set (drover-session-id session)
                                       (drover-remote-agent--with session :title title))
             (drover-changed-now))
   :submit (lambda (session text then _else)
             (push (vector "send" (drover-session-id session) text) drover-remote-agent--calls)
             (drover-remote-agent--confirm then))
   :requests (lambda (session callback)
               (funcall callback
                        (and (drover-session-requests session)
                             (list (list :id (concat "perm-" (drover-session-id session)) :kind 'permission
                                         :title "Shell command: ls" :detail "ls -la"
                                         :choices '("once" "always" "reject"))))))
   :answer (lambda (session id answer then else)
             (if (not (equal id (concat "perm-" (drover-session-id session))))
                 (funcall else "no such request pending")
               (push (vector "answer" (drover-session-id session) (plist-get answer :decision))
                     drover-remote-agent--calls)
               (drover-remote-agent--set (drover-session-id session)
                                         (drover-remote-agent--with session :requests nil :status 'working))
               (drover-changed-now)
               (funcall then)))
   :history (lambda (callback)
              ;; Later, as a service answers.
              (run-at-time 0.05 nil
                           (lambda ()
                             (funcall callback (list (drover-remote-agent--session "old1" 0 :status 'idle)
                                                     (drover-remote-agent--session "old2" 0 :status 'idle))))))
   :archived (lambda (callback)
               (funcall callback (cons (drover-remote-agent--session "gone1" 0 :status 'idle
                                                                     :archived 1700000000.0)
                                       (hash-table-values drover-remote-agent--archived))))))

(defun drover-remote-agent--session (id index &rest properties)
  "Return a fake session ID, the INDEXth, with PROPERTIES over the defaults."
  (let ((now (float-time)))
    (apply #'drover-session-create
           (append properties
                   (list :backend 'fake :id id :title (format "Session %s" id)
                         :directory "/work/project/" :project "project" :branch "main"
                         :status (nth (% index 4) '(idle working done waiting))
                         :since (- now (* 60 index)) :updated (- now (* 30 index))
                         :created (- now 86400) :agent "build" :model "Opus 5.5"
                         :effort "high" :provider-name "Anthropic"
                         :context-tokens (* 1000 index) :context-limit 200000
                         :engine 'fake :engine-id id)))))

(defun drover-remote-agent--set (id session)
  "Make SESSION the fake session ID, nil removing it, and report it."
  (if session
      (progn (unless (gethash id drover-remote-agent--sessions)
               (setq drover-remote-agent--order (append drover-remote-agent--order (list id))))
             (puthash id session drover-remote-agent--sessions))
    (remhash id drover-remote-agent--sessions)
    (setq drover-remote-agent--order (delete id drover-remote-agent--order)))
  (drover-backend-changed 'fake id))

(defun drover-remote-agent--json (value)
  "Return VALUE, a plist parsed from JSON, with false as nil."
  (cl-loop for (k v) on value by #'cddr nconc (list k (if (eq v :false) nil v))))

(defun drover-remote-agent--command (_connection message reply)
  "Run MESSAGE, a command, and answer through REPLY."
  (setq drover-remote-agent--last-command (float-time))
  (let ((args (drover-remote-agent--json message)))
    (pcase (plist-get args :op)
      ("sessions"
       (let ((n (plist-get args :n))
             (prefix (or (plist-get args :prefix) "s"))
             (children (or (plist-get args :children) 0)))
         (clrhash drover-remote-agent--sessions)
         (setq drover-remote-agent--order nil)
         (dotimes (i n)
           (let ((id (format "%s%d" prefix i)))
             (drover-remote-agent--set id (drover-remote-agent--session id i))
             (dotimes (j children)
               (let ((child (format "%s%d-%d" prefix i j)))
                 (drover-remote-agent--set child (drover-remote-agent--session
                                                  child j :parent id
                                                  :title (format "Subagent %s" child)))))))
         (drover-backend-changed 'fake)
         (drover-changed-now)
         (funcall reply (list :count (hash-table-count drover-remote-agent--sessions)))))
      ("change"
       (let ((count 0))
         (seq-doseq (id (plist-get args :ids))
           (when-let* ((session (gethash id drover-remote-agent--sessions)))
             (let ((copy (copy-sequence session)))
               (when (plist-get args :title) (setf (drover-session-title copy) (plist-get args :title)))
               (when (plist-get args :status)
                 (setf (drover-session-status copy) (intern (plist-get args :status))))
               (setf (drover-session-updated copy) (float-time))
               (drover-remote-agent--set id copy)
               (cl-incf count))))
         (when (plist-get args :now) (drover-changed-now))
         (funcall reply (list :changed count))))
      ("add"
       (seq-doseq (id (plist-get args :ids))
         (drover-remote-agent--set id (drover-remote-agent--session
                                       id 1 :title (or (plist-get args :title) (format "Session %s" id)))))
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("remove"
       (seq-doseq (id (plist-get args :ids))
         (drover-remote-agent--set id nil))
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("churn"
       ;; Each change taken in by the index before the next: a revision each.
       (let ((id (plist-get args :id)))
         (dotimes (i (plist-get args :count))
           (when-let* ((session (gethash id drover-remote-agent--sessions)))
             (let ((copy (copy-sequence session)))
               (setf (drover-session-title copy) (format "Churn %d" i))
               (drover-remote-agent--set id copy)
               (drover-changed-now))))
         (funcall reply (list :rev (plist-get (drover-remote-owner-stats) :rev)))))
      ("ask"
       ;; Sessions waiting for the user: a permission each.
       (seq-doseq (id (plist-get args :ids))
         (when-let* ((session (gethash id drover-remote-agent--sessions)))
           (drover-remote-agent--set id (drover-remote-agent--with
                                         session :status 'waiting
                                         :requests (list (list :kind 'permission :summary "ls"))))))
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("calls"
       (funcall reply (list :calls (vconcat (reverse drover-remote-agent--calls)))))
      ("hold"
       (setq drover-remote-agent--hold (list 'held))
       (funcall reply (list :ok t)))
      ("release"
       (let ((held (reverse (cdr drover-remote-agent--hold))))
         (setq drover-remote-agent--hold nil)
         (mapc #'funcall held)
         (funcall reply (list :released (length held)))))
      ("history"
       ;; The events of its link with PEER, oldest first, as [KIND TEXT COUNT].
       (let ((peer (drover-remote--peer (plist-get args :peer))))
         (funcall reply (list :events (vconcat (mapcar (lambda (event)
                                                         (vector (symbol-name (nth 1 event)) (nth 2 event)
                                                                 (or (nth 3 event) 1)))
                                                       (reverse (and peer (drover-remote--peer-history peer)))))))))
      ("restart"
       ;; The mode enabled again, as a restart of the remote layer.
       (funcall reply (list :ok t))
       (run-at-time 0.05 nil (lambda () (drover-remote-mode 1))))
      ("relayout"
       ;; The state made by another version of drover: made anew, its
       ;; connections kept, as reloading a newer drover does.
       (setf (drover-remote--state-stamp drover-remote--state) 'older)
       (funcall reply (list :ok t))
       (run-at-time 0.05 nil (lambda () (drover-remote--current))))
      ("titles"
       (funcall reply (list :titles (vconcat (mapcar (lambda (id)
                                                       (let ((s (gethash id drover-remote-agent--sessions)))
                                                         (vector id (drover-session-title s)
                                                                 (symbol-name (drover-session-status s)))))
                                                     drover-remote-agent--order)))))
      ("hosts"
       ;; The sessions of other hosts this Emacs lists: what it learnt.
       (funcall reply (list :remote (vconcat (mapcar (lambda (session)
                                                       (vector (drover-session-host session)
                                                               (drover-session-title session)))
                                                     (seq-filter #'drover-session-host
                                                                 (drover-index-sessions)))))))
      ("stats"
       (funcall reply (list :owner (drover-remote-owner-stats)
                            :peer (and (plist-get args :peer)
                                       (let ((stats (drover-remote-peer-stats (plist-get args :peer))))
                                         (cl-loop for (k v) on stats by #'cddr
                                                  nconc (list k (if (symbolp v) (format "%s" v) v)))))
                            :rss (or (cdr (assq 'rss (process-attributes (emacs-pid)))) 0))))
      ("dial"
       (setq drover-remote-peers
             (list (list (plist-get args :name) :dial t :local (plist-get args :local))))
       (drover-remote-reconfigure)
       (funcall reply (list :ok t)))
      ("reset"
       ;; Measures start anew.
       (when-let* ((state (drover-remote--current)))
         (maphash (lambda (_connection sub)
                    (setf (drover-remote--sub-stats sub)
                          (plist-put (plist-put (drover-remote--sub-stats sub) :slice-max 0)
                                     :slice-gc-max 0)))
                  (drover-remote--state-subs state)))
       (funcall reply (list :ok t)))
      ("dial-ssh"
       (setq drover-remote-peers
             (list (list (plist-get args :name) :dial t :ssh (plist-get args :ssh)
                         :program (plist-get args :program)
                         :args (append (plist-get args :args) nil)
                         :registration (plist-get args :registration))))
       (drover-remote-reconfigure)
       (funcall reply (list :ok t)))
      ("drop"
       ;; Streams broken, then CHANGE sessions changed while they are;
       ;; with AWAY, those it dialed connect again AWAY seconds later.
       (funcall reply (list :ok t))
       (when (plist-get args :away)
         (setq spyglass-reconnect-delays (list (plist-get args :away) 0.2)
               drover-remote-backoff spyglass-reconnect-delays)
         ;; Connections it dialed have the delays drover gave them.
         (dolist (peer (drover-remote--peers))
           (when-let* ((dialed (drover-remote--peer-dialed peer)))
             (drover-remote--back-off dialed))))
       (let ((change (or (plist-get args :change) 0)))
         (run-at-time 0.05 nil
                      (lambda ()
                        (dolist (connection (spyglass-connections))
                          (when (spyglass-connection-live-p connection)
                            (spyglass-connection-drop-stream connection)))
                        (cl-loop for id in drover-remote-agent--order
                                 for i below change
                                 do (let ((copy (copy-sequence (gethash id drover-remote-agent--sessions))))
                                      (setf (drover-session-title copy) (format "Outage %d" i))
                                      (drover-remote-agent--set id copy)))
                        (drover-changed-now)))))
      ("terminal"
       (let ((buffer (drover-remote-agent--terminal (plist-get args :id) (or (plist-get args :kind) "pty"))))
         (funcall reply (list :name (buffer-name buffer)
                              :pty (if (process-tty-name (get-buffer-process buffer)) t :false)))))
      ("emit"
       ;; Output of a terminal: TEXT, or BYTES of lines, as a program
       ;; writing fast, 10 kB at a time from timers, its buffer keeping
       ;; its last KEEP characters as a terminal's scrollback; after
       ;; DELAY seconds.
       (let ((buffer (gethash (plist-get args :id) drover-remote-agent--terminals))
             (text (plist-get args :text))
             (bytes (or (plist-get args :bytes) 0))
             (keep (plist-get args :keep))
             (line 0) (written 0))
         (funcall reply (list :ok t))
         (cl-labels ((chunk ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (save-excursion
                             (goto-char (point-max))
                             (dotimes (_ 200)
                               (let ((text (format "line %08d %s\n" (cl-incf line) (make-string 40 ?x))))
                                 (insert text)
                                 (setq written (+ written (length text))))))
                           (when (and keep (> (buffer-size) keep))
                             (delete-region (point-min) (- (point-max) keep))))
                         (if (< written bytes)
                             (run-at-time 0.001 nil #'chunk)
                           (with-current-buffer buffer
                             (save-excursion (goto-char (point-max)) (insert "EMITTED\n")))))))
           (run-at-time (or (plist-get args :delay) 0) nil
                        (lambda ()
                          (if text
                              (with-current-buffer buffer
                                (save-excursion (goto-char (point-max)) (insert text)))
                            (chunk)))))))
      ("terminal-state"
       (let* ((buffer (gethash (plist-get args :id) drover-remote-agent--terminals))
              (process (and (buffer-live-p buffer) (get-buffer-process buffer)))
              (tty (and process (process-tty-name process)))
              (size (and tty (with-temp-buffer
                               (call-process "sh" nil t nil "-c"
                                             (format "stty size < %s" (shell-quote-argument tty)))
                               (split-string (buffer-string))))))
         (funcall reply
                  (list :live (if (buffer-live-p buffer) t :false)
                        :process (if (process-live-p process) t :false)
                        :text (if (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (buffer-substring-no-properties (max (point-min) (- (point-max) 2000))
                                                                    (point-max)))
                                  "")
                        :digest (if (buffer-live-p buffer)
                                    (drover-remote-agent--digest buffer)
                                  :false)
                        :size (if (= (length size) 2)
                                  (vector (string-to-number (cadr size)) (string-to-number (car size)))
                                :false)))))
      ("shares"
       (funcall reply (list :shares (vconcat (mapcar (lambda (share)
                                                       (vector (spyglass-share-id share)
                                                               (length (spyglass-share-viewers share))))
                                                     (spyglass-shares))))))
      ("windows"
       (funcall reply (list :windows (vconcat (mapcar (lambda (window) (buffer-name (window-buffer window)))
                                                      (window-list nil 'no-minibuffer))))))
      ("quit"
       ;; Quitting Emacs, its peers keep its sessions, stale; turning
       ;; the mode off first, they drop them.
       (funcall reply (list :bye t))
       (setq drover-remote-agent--quit (if (plist-get args :off) 'off t)))
      (op (funcall reply nil (format "unknown command %s" op))))))

(defun drover-remote-agent--step ()
  "Process events until the next timer is due, 10 ms at most.
A batch Emacs waiting longer does not wake for timers armed meanwhile."
  (let* ((timer (car timer-list))
         (until (and timer (timer-until timer nil))))
    (accept-process-output nil (max 0.001 (min 0.01 (or until 0.01))))))

(defun drover-remote-agent-main ()
  "Serve as the host DROVER_AGENT_HOST until told to quit."
  (let* ((dir (file-name-as-directory (or (getenv "DROVER_AGENT_DIR") default-directory)))
         (peers (getenv "DROVER_AGENT_PEERS")))
    (setq drover-host (or (getenv "DROVER_AGENT_HOST") "agent")
          drover-host-aliases nil
          drover-state-directory (expand-file-name "state/" dir)
          ;; The fake backend's sessions alone, not those this host's engines recorded.
          drover-transcript-engines nil
          drover-refresh-delay 0.02
          spyglass-registration-file (expand-file-name "server.json" dir)
          drover-remote-standby-delay 1
          drover-remote-accept-unknown (unless (getenv "DROVER_AGENT_STRICT")
                                         (intern (or (getenv "DROVER_AGENT_ACCESS") "read")))
          drover-remote-peers (and peers (not (string-empty-p peers))
                                   (mapcar (lambda (name)
                                             (list name :access (intern (or (getenv "DROVER_AGENT_ACCESS") "read"))))
                                           (split-string peers ","))))
    (when (getenv "DROVER_AGENT_OLD")
      (setq drover-remote-capabilities nil))
    (when-let* ((caps (getenv "DROVER_AGENT_CAPS")))
      (setq drover-remote-capabilities (split-string caps "," t)))
    (when-let* ((delay (getenv "DROVER_AGENT_UNSHARE")))
      (setq drover-remote-unshare-delay (string-to-number delay)))
    (when-let* ((dial (getenv "DROVER_AGENT_DIAL")))
      (setq drover-remote-peers (mapcar (lambda (entry) (append entry (list :dial t :local dial)))
                                        drover-remote-peers)))
    (add-hook 'spyglass-error-functions
              (lambda (_connection message) (drover-remote-agent--say "spyglass: %s" message)))
    (drover-remote-agent--register)
    (spyglass-register-channel "drover-test" #'ignore :request #'drover-remote-agent--command)
    (drover-remote-mode 1)
    (drover-remote-agent--say "READY %s %s" drover-host spyglass-registration-file)
    (let ((checked 0))
      (while (and (not drover-remote-agent--quit)
                  (< (- (float-time) drover-remote-agent--last-command) 600))
        (drover-remote-agent--step)
        (when (> (- (float-time) checked) 2)
          (setq checked (float-time))
          (let ((rss (cdr (assq 'rss (process-attributes (emacs-pid))))))
            (when (and rss (> rss (* 2 1024 1024)))
              (drover-remote-agent--say "ABORT resident memory %d kB" rss)
              (kill-emacs 3))))))
    (when (eq drover-remote-agent--quit 'off)
      (drover-remote-mode -1))
    (let ((end (+ (float-time) 0.2)))
      (while (< (float-time) end) (drover-remote-agent--step)))
    (kill-emacs 0)))

(provide 'drover-remote-agent)
;;; drover-remote-agent.el ends here
