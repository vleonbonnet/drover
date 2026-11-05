;;; drover-remote-bench.el --- Measure drover's remote layer between two Emacs  -*- lexical-binding: t; -*-

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

;; `make bench-remote' starts an agent (test/drover-remote-agent.el)
;; owning 2511 fake sessions, 837 with two subagents each as drover's
;; bench, and has this batch Emacs, the viewer, follow them: on this
;; host through 127.0.0.1, or with REMOTE=HOST on another, the agent's
;; files copied to a temporary directory there, through SSH -W, the
;; directory removed after.
;;
;;   make bench-remote
;;   make bench-remote REMOTE=core SSH=myssh REMOTE_EMACS=/ucrt64/bin/emacs
;;
;; With SEARCH=WORDS, comma-separated, it measures instead searches of
;; what was said in the conversations Claude Code recorded on the
;; agent's host, read only, through the agent, for each word:
;;
;;   make bench-remote REMOTE=core SSH=myssh REMOTE_EMACS=/ucrt64/bin/emacs SEARCH=error,capybara
;;
;; With BACK=PROGRAM, the agent also dials this host back through SSH
;; run as PROGRAM there, with BACK_ARGS (comma-separated), so that both
;; directions are checked and the connection both keep is reported.
;;
;; It reports the time to take in 2511 sessions as they appear, as a
;; snapshot, an update of one session and of 50, a resynchronization
;; after 10 and after 500 changes made while the stream was broken; a
;; terminal of the agent mirrored here: opened, a line typed here until
;; its echo shows here, 8 MB written there as fast as a program writes,
;; its mirror resuming after a drop; the
;; longest slice of the viewer installing records and of the owner
;; serializing them; the longest block of this Emacs's main thread, as
;; a timer every 5 ms finds it late; resident memory of both.  Every
;; Emacs kills itself past 2 GB.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-remote-test)
(require 'drover-attention)

(defvar opencode-service-registration-file)
(defvar opencode-command)

(defvar drover-remote-bench--late 0.0
  "The longest lateness of the probe timer, in seconds, since reset.")

(defvar drover-remote-bench--last nil
  "When the probe timer last ran.")

(defconst drover-remote-bench--interval 0.005
  "Seconds between two runs of the probe timer.")

(defun drover-remote-bench--probe ()
  "Note how late this run of the probe timer is."
  (let ((now (float-time)))
    (when drover-remote-bench--last
      (setq drover-remote-bench--late
            (max drover-remote-bench--late (- now drover-remote-bench--last drover-remote-bench--interval))))
    (setq drover-remote-bench--last now)))

(defun drover-remote-bench--reset ()
  "Start measuring anew."
  (setq drover-remote-bench--late 0.0
        drover-remote-bench--last nil)
  (when-let* ((peer (drover-remote--peer drover-remote-test-agent-name)))
    (setf (drover-remote--peer-stats peer)
          (plist-put (plist-put (drover-remote--peer-stats peer) :slice-max 0) :slice-gc-max 0)))
  (drover-remote-test-command "reset"))

(defun drover-remote-bench--getenv (name &optional default)
  "Return environment variable NAME, or DEFAULT when unset or empty."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun drover-remote-bench--ms (seconds)
  "Return SECONDS in milliseconds, for the table."
  (format "%.1f" (* 1000 (or seconds 0))))

(defvar drover-remote-bench--spyglass-errors 0
  "How many errors spyglass told of.")

(defvar drover-remote-bench--warnings 0
  "How many warnings drover gave.")

(defvar drover-remote-bench--rows nil
  "Rows of the table, newest first.")

(defun drover-remote-bench--row (what seconds &optional records)
  "Note a row: WHAT took SECONDS, RECORDS records received."
  (let* ((stats (drover-remote-test-stats))
         (serving (plist-get (plist-get (drover-remote-test-command "stats" :peer drover-host) :peer)
                             :serving))
         (row (list what (drover-remote-bench--ms seconds)
                    (if records (format "%d" records) "-")
                    (format "%s (%s)" (drover-remote-bench--ms (plist-get stats :slice-max))
                            (drover-remote-bench--ms (plist-get stats :slice-gc-max)))
                    (format "%s (%s)" (drover-remote-bench--ms (plist-get serving :slice-max))
                            (drover-remote-bench--ms (plist-get serving :slice-gc-max)))
                    (drover-remote-bench--ms drover-remote-bench--late))))
    (message "bench: %S" row)
    (push row drover-remote-bench--rows)))

(defun drover-remote-bench--records ()
  "Return the records this Emacs received from the agent so far."
  (or (plist-get (drover-remote-test-stats) :records) 0))

(defun drover-remote-bench--time-until (predicate)
  "Return the seconds until PREDICATE holds and `drover-changed-hook' ran.
PREDICATE reads the peer's records, not the index, which would take the
changes in at once: views follow them as they do in use, after
`drover-refresh-delay' seconds of quiet."
  (let ((start (float-time)))
    (unless (drover-remote-test-wait (lambda () (and (funcall predicate) (null drover--change-timer))) 300)
      (error "Timed out"))
    (- (float-time) start)))

(defun drover-remote-bench--records-here ()
  "Return the agent's sessions as this host holds them."
  (hash-table-values (drover-remote--peer-records (drover-remote--peer drover-remote-test-agent-name))))

(defun drover-remote-bench--title (id)
  "Return the title of the agent's session ID here."
  (when-let* ((session (gethash (cons 'fake id)
                                (drover-remote--peer-records
                                 (drover-remote--peer drover-remote-test-agent-name)))))
    (and (not (drover-session-stale session)) (drover-session-title session))))

;;;; The agent on another host

(defvar drover-remote-bench--remote nil
  "The host the agent runs on, or nil for this one.")

(defvar drover-remote-bench--remote-dir nil
  "The agent's directory on its host.")

(defun drover-remote-bench--ssh ()
  "Return the SSH program reaching the agent's host."
  (drover-remote-bench--getenv "DROVER_BENCH_SSH" "ssh"))

(defun drover-remote-bench--remote-run (command)
  "Run COMMAND on the agent's host and return its output, trimmed."
  (with-temp-buffer
    (let ((status (call-process (drover-remote-bench--ssh) nil t nil "-o" "BatchMode=yes"
                                drover-remote-bench--remote command)))
      (unless (eql status 0)
        (error "Remote command failed (%s): %s: %s" status command (buffer-string)))
      (string-trim (buffer-string)))))

(defun drover-remote-bench--copy (file dir)
  "Copy FILE to DIR on the agent's host."
  (unless (eql 0 (call-process "sh" nil nil nil "-c"
                               (format "cat %s | %s -o BatchMode=yes %s %s"
                                       (shell-quote-argument file)
                                       (shell-quote-argument (drover-remote-bench--ssh))
                                       (shell-quote-argument drover-remote-bench--remote)
                                       (shell-quote-argument
                                        (format "cat > %s/%s" dir (file-name-nondirectory file))))))
    (error "Copying %s failed" file)))

(defun drover-remote-bench--sources ()
  "Return the files the agent needs."
  (append (mapcar #'locate-library '("drover-core.el" "drover-ui.el" "drover-remote.el" "drover-remote-agent.el"
                                     "drover-model.el" "drover-command.el" "drover-transcript.el"
                                     "drover-claude-code.el" "drover-claude-code-transcripts.el"
                                     "opencode-look.el" "opencode-common.el" "opencode-transport.el"
                                     "opencode-service.el" "drover-fake-opencode.el"))
          (file-expand-wildcards
           (expand-file-name "spyglass*.el" (file-name-directory (locate-library "spyglass.el"))))))

(defun drover-remote-bench--start-remote (back)
  "Start the agent on `drover-remote-bench--remote', return its registration file there.
With BACK, the agent will be told to dial this host back."
  (let* ((dir (drover-remote-bench--remote-run "mktemp -d /tmp/drover-remote-XXXXXX"))
         (emacs (drover-remote-bench--getenv "DROVER_BENCH_REMOTE_EMACS" "emacs")))
    (setq drover-remote-bench--remote-dir dir)
    (message "bench: copying to %s:%s" drover-remote-bench--remote dir)
    (dolist (file (drover-remote-bench--sources))
      (drover-remote-bench--copy file dir))
    (drover-remote-bench--remote-run
     ;; Warnings fail drover's files alone: the others' are their own.
     (format "cd %s && timeout 300 %s -Q --batch -L . -f batch-byte-compile opencode-common.el opencode-look.el opencode-transport.el opencode-service.el spyglass*.el 2>&1 && timeout 300 %s -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile drover-core.el drover-ui.el drover-remote.el drover-model.el drover-command.el drover-transcript.el drover-claude-code.el drover-claude-code-transcripts.el 2>&1"
             dir emacs emacs))
    (setq drover-remote-test--agent-output nil
          drover-remote-test--agent-pending ""
          drover-remote-test--agent
          (make-process
           :name "drover-agent" :connection-type 'pipe :noquery t
           :command (list (drover-remote-bench--ssh) "-o" "BatchMode=yes" drover-remote-bench--remote
                          (format "cd %s && DROVER_AGENT_OPENCODE=1 DROVER_AGENT_ACCESS=full DROVER_AGENT_HOST=%s DROVER_AGENT_PEERS=%s%s exec timeout 300 %s -Q --batch -L . -l drover-remote-agent -f drover-remote-agent-main"
                                  dir drover-remote-test-agent-name (if back drover-host "")
                                  (if (drover-remote-bench--search-words) " DROVER_AGENT_CLAUDE=1" "")
                                  emacs))
           :filter #'drover-remote-test--agent-filter))
    (drover-remote-test-wait (lambda () (or (seq-some (lambda (line) (string-prefix-p "DROVER-AGENT READY" line))
                                                      drover-remote-test--agent-output)
                                            (not (process-live-p drover-remote-test--agent))))
                             120)
    (unless (seq-some (lambda (line) (string-prefix-p "DROVER-AGENT READY" line))
                      drover-remote-test--agent-output)
      (error "The agent did not start: %S" drover-remote-test--agent-output))
    (concat dir "/server.json")))

;;;; Mirrors

(defun drover-remote-bench--mirror-shows-p (mirror text)
  "Return non-nil when MIRROR, a buffer, holds TEXT near its end."
  (with-current-buffer mirror
    (save-excursion
      (goto-char (point-max))
      (search-backward text (max (point-min) (- (point-max) 4096)) t))))

(defun drover-remote-bench--mirror ()
  "Measure a terminal of the agent mirrored here, adding rows.
Opening it, a line typed until its echo shows, 8 MB written there with
a scrollback of 512 kB, and resuming after a drop."
  (let* ((peer (drover-remote--peer drover-remote-test-agent-name))
         (pty (eq (plist-get (drover-remote-test-command "terminal" :id "s1") :pty) t)))
    (drover-remote-test-wait (lambda () (eq (drover-remote--peer-granted peer) 'full)) 10)
    (drover-changed-now)
    (save-window-excursion
      (delete-other-windows)
      (drover-remote-bench--reset)
      (let* ((start (float-time))
             (mirror (drover-remote-open (gethash '(fake . "s1") (drover-remote--peer-records peer)))))
        (unless (drover-remote-test-wait (lambda () (eq (spyglass-mirror-state mirror) 'live)) 30)
          (error "The mirror did not open"))
        (drover-remote-bench--row (format "terminal mirrored (%s)" (if pty "pty" "pipes")) (- (float-time) start))
        ;; A line typed until its echo shows.
        (drover-remote-bench--reset)
        (let ((times nil))
          (dotimes (i 20)
            (let ((token (format "typed%02d" i))
                  (start (float-time)))
              (spyglass-mirror-send-string token mirror)
              (spyglass-mirror-send-key "C-j" mirror)
              (unless (drover-remote-test-wait (lambda () (drover-remote-bench--mirror-shows-p mirror token)) 10)
                (error "No echo of %s" token))
              (push (- (float-time) start) times)))
          (setq times (sort times #'<))
          (drover-remote-bench--row "a line typed to its echo (median of 20)" (nth 10 times))
          (drover-remote-bench--row "a line typed to its echo (slowest of 20)" (car (last times))))
        ;; A program writing fast there.
        (drover-remote-bench--reset)
        (let ((start (float-time)))
          (drover-remote-test-command "emit" :id "s1" :bytes (* 8 1024 1024) :keep (* 512 1024))
          (unless (drover-remote-test-wait (lambda () (drover-remote-bench--mirror-shows-p mirror "EMITTED")) 120)
            (error "The output did not arrive"))
          (let ((elapsed (- (float-time) start)))
            (drover-remote-bench--row (format "8 MB written there, %.1f MB/s" (/ 8.0 elapsed)) elapsed))
          (let ((same (drover-remote-test-wait
                       (lambda () (equal (drover-remote-test-digest mirror)
                                         (plist-get (drover-remote-test-command "terminal-state" :id "s1")
                                                    :digest)))
                       20)))
            (push (list "the mirror shows the terminal exactly" (if same "yes" "NO")
                        (format "%d" (buffer-size mirror)) "-" "-" "-")
                  drover-remote-bench--rows)))
        ;; A drop: the mirror resumes, what was written meanwhile shown.
        (drover-remote-bench--reset)
        (let ((start (float-time)))
          (drover-remote-test-command "emit" :id "s1" :text "during the drop\n" :delay 0.3)
          (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
          (unless (drover-remote-test-wait (lambda () (and (eq (spyglass-mirror-state mirror) 'live)
                                                           (drover-remote-bench--mirror-shows-p
                                                            mirror "during the drop")))
                                           60)
            (error "The mirror did not resume"))
          (drover-remote-bench--row "mirror resumed after a drop" (- (float-time) start)))
        (kill-buffer mirror)))))

;;;; OpenCode sessions

(defun drover-remote-bench--free-port ()
  "Return a port of 127.0.0.1 free a moment ago."
  (let* ((server (make-network-process :name "free-port" :server t :host "127.0.0.1" :service t
                                       :family 'ipv4 :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun drover-remote-bench--forward (port)
  "Forward a port of this host to the agent's PORT; return (PROCESS . LOCAL-PORT).
Through SSH, as the design's forward would, when the agent runs on
another host; on this host, nothing: its port is reached directly."
  (if (not drover-remote-bench--remote)
      (cons nil port)
    (let* ((local (drover-remote-bench--free-port))
           (process (make-process
                     :name "bench-forward" :noquery t :connection-type 'pipe
                     :command (list (drover-remote-bench--ssh) "-N" "-o" "BatchMode=yes"
                                    "-o" "ExitOnForwardFailure=yes"
                                    "-L" (format "127.0.0.1:%d:127.0.0.1:%d" local port)
                                    drover-remote-bench--remote))))
      (cons process local))))

(defun drover-remote-bench--forward-ready (port password)
  "Wait until the service answers through 127.0.0.1's PORT, with PASSWORD."
  (let ((ready nil)
        (deadline (+ (float-time) 30)))
    (while (and (not ready) (< (float-time) deadline))
      (let ((outcome nil))
        (opencode-transport-request 'get (format "http://127.0.0.1:%d/api/info" port)
                                    :headers (list (cons "Authorization"
                                                         (opencode-service-authorization (list :password password))))
                                    :timeout 5 :connect-timeout 2
                                    :then (lambda (_) (setq outcome 'ok))
                                    :else (lambda (_) (setq outcome 'failed)))
        (drover-remote-test-wait (lambda () outcome) 10)
        (if (eq outcome 'ok)
            (setq ready t)
          (drover-remote-test-settle 0.1))))
    (unless ready (error "The forwarded port did not answer"))))

(defun drover-remote-bench--oc-buffer-shows-p (buffer text)
  "Return non-nil when BUFFER, a session buffer, shows TEXT."
  (with-current-buffer buffer
    (save-excursion (goto-char (point-max)) (search-backward text nil t))))

(defun drover-remote-bench--oc-round-trips (service id)
  "Return the median and slowest of 20 GETs of the session ID from SERVICE, in seconds."
  (let ((times nil))
    (dotimes (_ 20)
      (let ((start (float-time))
            (done nil))
        (opencode-call-with-service service #'opencode-request 'get (concat "/api/session/" id)
                                    :directory nil :timeout 10 :buffer nil
                                    :then (lambda (_) (setq done t))
                                    :else (lambda (error) (setq done error)))
        (unless (drover-remote-test-wait (lambda () done) 20)
          (error "No answer"))
        (unless (eq done t)
          (error "A GET failed: %s" (opencode-error-describe done)))
        (push (- (float-time) start) times)))
    (setq times (sort times #'<))
    (list (nth 10 times) (car (last times)))))

(defun drover-remote-bench--oc-echoes (buffer label)
  "Measure prompts sent from BUFFER until their echo shows there; LABEL the rows."
  (let ((times nil))
    (dotimes (i 10)
      (let ((token (format "%s-%02d" label i))
            (start (float-time)))
        (with-current-buffer buffer (opencode-session-send token :show t))
        (unless (drover-remote-test-wait (lambda () (drover-remote-bench--oc-buffer-shows-p
                                                     buffer (concat "Echo: " token)))
                                         20)
          (error "No echo of %s" token))
        (push (- (float-time) start) times)))
    (setq times (sort times #'<))
    (drover-remote-bench--row (format "a prompt to its echo, %s (median of 10)" label) (nth 5 times))
    (drover-remote-bench--row (format "a prompt to its echo, %s (slowest of 10)" label) (car (last times)))))

(defun drover-remote-bench--oc-large (buffer id label)
  "Measure a turn of 1 MB of the session ID streamed there until BUFFER shows it.
LABEL names the row."
  (drover-remote-bench--reset)
  (let ((start (float-time))
        (token (format "large-%s" label)))
    (drover-remote-test-command "oc-turn" :id id :text token :size (* 1024 1024))
    (unless (drover-remote-test-wait (lambda () (drover-remote-bench--oc-buffer-shows-p buffer (concat token "-END")))
                                     120)
      (error "The large turn did not arrive"))
    (drover-remote-bench--row (format "1 MB event streamed, %s" label) (- (float-time) start))))

(defun drover-remote-bench--oc-open (opener finder label)
  "Open one of the agent's sessions with OPENER, a function, and measure it.
FINDER returns its buffer once there.  LABEL names the row; return the
buffer once it shows the transcript."
  (drover-remote-bench--reset)
  (let ((start (float-time))
        (buffer nil))
    (funcall opener)
    (unless (drover-remote-test-wait (lambda () (and (setq buffer (funcall finder))
                                                     (buffer-live-p buffer)
                                                     (drover-remote-bench--oc-buffer-shows-p
                                                      buffer "First reply of Bench session")))
                                     60)
      (error "The %s OpenCode buffer did not open" label))
    (drover-remote-bench--row (format "OpenCode buffer opened, %s" label) (- (float-time) start))
    buffer))

(defun drover-remote-bench--oc-measure (service buffer id label)
  "Measure SERVICE's requests and events in BUFFER, the session ID's; LABEL the rows."
  (drover-remote-test-wait (lambda () (opencode-call-with-service service #'opencode-events-connected-p)) 30)
  (drover-remote-bench--reset)
  (pcase-let ((`(,median ,slowest) (drover-remote-bench--oc-round-trips service id)))
    (drover-remote-bench--row (format "a GET of a session, %s (median of 20)" label) median)
    (drover-remote-bench--row (format "a GET of a session, %s (slowest of 20)" label) slowest))
  (drover-remote-bench--reset)
  (drover-remote-bench--oc-echoes buffer label)
  (drover-remote-bench--oc-large buffer id label))

(defun drover-remote-bench--opencode ()
  "Measure an OpenCode session of the agent's service here, two ways, adding rows.
Through a port forwarded to the service, the design's first idea, then
relayed by the agent's drover, as opencode.el reaches a peer's service:
the buffer opened, requests, a prompt until its echo, a large event,
and the relayed buffer catching up after the link dropped.  One buffer
at a time, each shown, of a session of its own, so that neither
renders the other's large turn."
  (drover-remote-test-command "oc-add" :sessions [["ses_a" "Bench session"] ["ses_b" "Bench session"]])
  (let* ((opencode-notification-os-filter nil)
         (name drover-remote-test-agent-name)
         (key (cons (intern (concat "remote:" name)) (cons 'opencode "ses_b")))
         (record (drover-remote-test-wait (lambda () (drover-changed-now) (drover-index-session key)) 20))
         (info (drover-remote-test-command "oc-streams"))
         (forward nil)
         (direct nil))
    (unless record (error "The agent's OpenCode session is not listed"))
    (save-window-excursion
      (delete-other-windows)
      (unwind-protect
          (let ((label (if drover-remote-bench--remote "forwarded" "direct")))
            ;; Through a forwarded port.
            (drover-remote-bench--reset)
            (let ((start (float-time)))
              (setq forward (drover-remote-bench--forward (plist-get info :port)))
              (drover-remote-bench--forward-ready (cdr forward) (plist-get info :password))
              (when (car forward)
                (drover-remote-bench--row "SSH -L forward started, answering" (- (float-time) start))))
            (setq direct (opencode-service-register
                          "bench-direct" :label "Direct"
                          :connect (let ((url (format "http://127.0.0.1:%d" (cdr forward)))
                                         (password (plist-get info :password)))
                                     (lambda (callback _)
                                       (funcall callback (list :url url :password password))))))
            (let* ((opened nil)
                   (buffer (drover-remote-bench--oc-open
                            (lambda ()
                              (opencode-call-with-service direct #'opencode-session-open-by-id "ses_a"
                                                          :no-display t
                                                          :callback (lambda (_) (setq opened (current-buffer)))))
                            (lambda () opened)
                            label)))
              ;; Shown, as the relayed one is: Comint scrolls the windows showing it.
              (switch-to-buffer buffer)
              (drover-remote-bench--oc-measure direct buffer "ses_a" label)
              (kill-buffer buffer))
            ;; Relayed.
            (let* ((buffer (drover-remote-bench--oc-open
                            (lambda () (drover-open record))
                            (lambda () (drover-remote--oc-buffer name (cons 'opencode "ses_b")))
                            "relayed"))
                   (service (opencode-service-get name)))
              (drover-remote-bench--oc-measure service buffer "ses_b" "relayed")
              ;; The link dropping: the relayed buffer follows its session again.
              (drover-remote-bench--reset)
              (let ((start (float-time))
                    (peer (drover-remote--peer name)))
                (drover-remote-test-command "oc-turn" :id "ses_b" :text "during the drop" :delay 0.3)
                (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
                (unless (drover-remote-test-wait (lambda () (drover-remote-bench--oc-buffer-shows-p
                                                             buffer "Echo: during the drop"))
                                                 60)
                  (error "The relayed buffer did not catch up"))
                (drover-remote-bench--row "relayed buffer caught up after a drop" (- (float-time) start)))
              (kill-buffer buffer)))
        (when direct
          (opencode-service-unregister direct))
        (when (process-live-p (car forward))
          (delete-process (car forward)))))))

;;;; Searching what was said there

(defun drover-remote-bench--search-words ()
  "Return the words to search the agent's conversations for, or nil.
DROVER_BENCH_SEARCH, comma-separated: then the bench measures searches
alone, of what the agent's host recorded of Claude Code, read only."
  (and (drover-remote-bench--getenv "DROVER_BENCH_SEARCH")
       (split-string (getenv "DROVER_BENCH_SEARCH") "," t "[ \t]+")))

(defun drover-remote-bench--conversations ()
  "Return how many of the agent's Claude Code sessions this host holds."
  (seq-count (lambda (session) (eq (drover-session-origin session) 'claude-code))
             (drover-remote-bench--records-here)))

(defun drover-remote-bench--search (words)
  "Measure searches of the agent's Claude Code conversations for WORDS, adding rows.
Once this host holds as many as the agent lists, the agent's count
steady for 3 s, each word is searched five times through the agent's
backend here, as the search of sessions asks it, every conversation
offered: the time to the first match and to the end, and the matches,
at most `drover-search-text-limit'."
  (let ((name drover-remote-test-agent-name)
        (start (float-time))
        (last -1)
        (since (float-time)))
    (while (not (and (> last 0) (> (- (float-time) since) 3)
                     (= last (drover-remote-bench--conversations))))
      (when (> (- (float-time) start) 300)
        (error "The agent's conversations were not listed here"))
      (let ((there (plist-get (drover-remote-test-command "recorded") :claude)))
        (unless (eql there last)
          (setq last there since (float-time))))
      (drover-remote-test-settle 0.5))
    (drover-remote-bench--row (format "%d conversations listed there, then here" last)
                              (- (float-time) start) last)
    (dolist (word words)
      (let ((firsts nil)
            (ends nil)
            (count 0))
        (dotimes (_ 5)
          (drover-remote-bench--reset)
          (let* ((scope (drover-search--scope (drover-search--build)))
                 (begin (float-time))
                 (first nil)
                 (end nil)
                 (failure nil))
            (setq count 0)
            (drover-remote--search name word scope
                                   (lambda (result)
                                     (cond ((eq result 'done) (setq end (float-time)))
                                           ((eq (car-safe result) :failure)
                                            (setq failure (cadr result) end (float-time)))
                                           (t (setq first (or first (float-time)))
                                              (setq count (+ count (length result)))))))
            (unless (drover-remote-test-wait (lambda () end) 120)
              (error "The search of %s did not end" word))
            (when failure
              (error "The search of %s failed: %s" word failure))
            (push (- (or first end) begin) firsts)
            (push (- end begin) ends)))
        (setq firsts (sort firsts #'<)
              ends (sort ends #'<))
        (drover-remote-bench--row (format "search \"%s\": first match (median of 5)" word) (nth 2 firsts) count)
        (drover-remote-bench--row (format "search \"%s\": all matches (median of 5)" word) (nth 2 ends) count)
        (drover-remote-bench--row (format "search \"%s\": all matches (slowest of 5)" word) (car (last ends))
                                  count)))))

;;;; Main

(defun drover-remote-bench--print ()
  "Print the table of the measures."
  (let* ((rows (cons '("step" "time ms" "records" "viewer slice ms (with GC)" "owner slice ms (with GC)"
                       "longest block ms")
                     (reverse drover-remote-bench--rows)))
         (widths (apply #'cl-mapcar (lambda (&rest cells) (apply #'max (mapcar #'length cells))) rows)))
    (cl-flet ((line (cells) (concat "| " (mapconcat #'identity (cl-mapcar #'string-pad cells widths) " | ") " |")))
      (princ (concat (line (car rows)) "\n"))
      (princ (concat "|" (mapconcat (lambda (w) (make-string (+ 2 w) ?-)) widths "+") "|\n"))
      (dolist (row (cdr rows)) (princ (concat (line row) "\n"))))))

(defun drover-remote-bench-main ()
  "Measure the remote layer and print the table."
  (setq drover-remote-bench--remote (drover-remote-bench--getenv "DROVER_BENCH_REMOTE"))
  (let* ((back (drover-remote-bench--getenv "DROVER_BENCH_BACK"))
         (search-words (drover-remote-bench--search-words))
         (back-args (and (drover-remote-bench--getenv "DROVER_BENCH_BACK_ARGS")
                         (split-string (getenv "DROVER_BENCH_BACK_ARGS") "," t)))
         (dir (file-name-as-directory (make-temp-file "drover-remote-bench" t)))
         (opencode-service-registration-file (expand-file-name "no-opencode-service.json" dir))
         (opencode-command "drover-bench-no-opencode")
         (drover-remote-test-agent-name (if drover-remote-bench--remote "core" "agent"))
         (drover-host (if drover-remote-bench--remote (drover--system-host) "viewer"))
         (spyglass-registration-file (expand-file-name "server.json" dir))
         (drover-refresh-delay 0.1)
         (drover-remote-standby-delay 5)
         (probe (run-at-time 0 drover-remote-bench--interval #'drover-remote-bench--probe))
         (guard (run-at-time 2 2 (lambda ()
                                   (let ((rss (cdr (assq 'rss (process-attributes (emacs-pid))))))
                                     (when (and rss (> rss (* 2 1024 1024)))
                                       (message "bench: aborting at %d kB" rss)
                                       (kill-emacs 3))))))
         (failed nil))
    ;; What spyglass tells drover, which drover reports as the user would
    ;; see it, a warning, if at all: the bench's drops are expected.
    (add-hook 'spyglass-error-functions (lambda (_c _message) (cl-incf drover-remote-bench--spyglass-errors)))
    (advice-add 'drover-warn :before (lambda (&rest _) (cl-incf drover-remote-bench--warnings))
                '((name . drover-remote-bench)))
    (unwind-protect
        (condition-case err
            (drover-test-with-backend (mapcar (lambda (i) (drover-test-session (format "here%d" i)))
                                              (number-sequence 1 5))
              (let* ((drover-group-by nil)
                     (registration (if drover-remote-bench--remote
                                       (drover-remote-bench--start-remote back)
                                     (let ((process-environment (append '("DROVER_AGENT_ACCESS=full"
                                                                          "DROVER_AGENT_OPENCODE=1")
                                                                        (and search-words '("DROVER_AGENT_CLAUDE=1"))
                                                                        process-environment)))
                                       (drover-remote-test-start-agent dir drover-remote-test-agent-name
                                                                       (and back drover-host))))))
                (setq drover-remote-peers
                      (list (if drover-remote-bench--remote
                                (list drover-remote-test-agent-name :dial t :ssh drover-remote-bench--remote
                                      :program (drover-remote-bench--ssh) :registration registration)
                              (list drover-remote-test-agent-name :dial t :local registration))))
                (drover-attention-mode 1)
                ;; opencode.el loaded first, its own service out of reach:
                ;; only the agent's is measured.
                (require 'opencode)
                (when (assq 'opencode drover--backends)
                  (drover-unregister-backend 'opencode))
                (drover-remote-mode 1)
                (drover-remote-test-wait (lambda () (eq (drover-remote--peer-state
                                                         (drover-remote--peer drover-remote-test-agent-name))
                                                        'open))
                                         60)
                (message "bench: connected")
                (if search-words
                    (drover-remote-bench--search search-words)
                  (progn
                    ;; 2511 sessions appear: an update of all of them.
                    (drover-remote-bench--reset)
                    (let ((before (drover-remote-bench--records))
			  (start (float-time)))
                      (drover-remote-test-command "sessions" :n 837 :children 2)
                      (drover-remote-bench--time-until (lambda () (= (length (drover-remote-bench--records-here)) 2511)))
                      (drover-remote-bench--row "2511 sessions appearing" (- (float-time) start)
						(- (drover-remote-bench--records) before)))
                    ;; A snapshot of them, after a reconnection under no epoch.
                    (drover-remote-bench--reset)
                    (let* ((peer (drover-remote--peer drover-remote-test-agent-name))
			   (before (drover-remote-bench--records))
			   (start (float-time)))
                      (setf (drover-remote--peer-epoch peer) nil (drover-remote--peer-rev peer) nil)
                      (spyglass-connection-drop-stream (drover-remote--peer-connection peer))
                      (drover-remote-bench--time-until
                       (lambda () (and (eq (drover-remote--peer-state peer) 'open)
                                       (drover-remote--peer-rev peer)
                                       (not (drover-remote--peer-stale peer)))))
                      (drover-remote-bench--row "snapshot of 2511" (- (float-time) start)
						(- (drover-remote-bench--records) before)))
                    ;; One session changing: from the command to this host's index.
                    (drover-remote-bench--reset)
                    (let ((times nil))
                      (dotimes (i 20)
			(let ((title (format "One %d" i))
                              (start (float-time)))
			  (drover-remote-test-command "change" :ids ["s5"] :title title :now t)
			  (drover-remote-bench--time-until (lambda () (equal (drover-remote-bench--title "s5") title)))
			  (push (- (float-time) start) times)))
                      (setq times (sort times #'<))
                      (drover-remote-bench--row "one session changed (median of 20)" (nth 10 times) 1)
                      (drover-remote-bench--row "one session changed (slowest of 20)" (car (last times)) 1))
                    ;; Fifty at once.
                    (drover-remote-bench--reset)
                    (let ((before (drover-remote-bench--records))
			  (start (float-time)))
                      (drover-remote-test-command "change" :ids (vconcat (mapcar (lambda (i) (format "s%d" i))
										 (number-sequence 100 149)))
						  :title "Fifty" :now t)
                      (drover-remote-bench--time-until (lambda () (equal (drover-remote-bench--title "s149") "Fifty")))
                      (drover-remote-bench--row "50 sessions changed" (- (float-time) start)
						(- (drover-remote-bench--records) before)))
                    ;; The minute's refresh, the peer reporting every change,
                    ;; against a full one, as `g' asks.
                    (drover-changed-now)
                    (dolist (full '(nil t))
                      (drover-remote-bench--reset)
                      (let ((times nil))
			(dotimes (_ 5)
			  (let ((start (float-time)))
                            (drover-index-refresh full)
                            (push (- (float-time) start) times)))
			(drover-remote-bench--row (if full
                                                      "a full refresh, as g (median of 5)"
                                                    "the minute's refresh (median of 5)")
						  (nth 2 (sort times #'<)))))
                    ;; Resynchronizations after changes during an outage.
                    (dolist (count '(10 500))
                      (drover-remote-bench--reset)
                      (let* ((peer (drover-remote--peer drover-remote-test-agent-name))
                             (snapshots (or (plist-get (drover-remote-test-stats) :snapshots) 0))
                             (before (drover-remote-bench--records))
                             (start (float-time))
                             )
			(drover-remote-test-command "drop" :change count)
			(drover-remote-test-wait (lambda () (not (eq (drover-remote--peer-state peer) 'open))) 10)
			(drover-remote-bench--time-until
			 (lambda () (and (eq (drover-remote--peer-state peer) 'open)
					 (not (drover-remote--peer-stale peer))
					 (= (seq-count (lambda (s) (string-prefix-p "Outage" (or (drover-session-title s) "")))
                                                       (drover-remote-bench--records-here))
                                            count))))
			(drover-remote-bench--row (format "resync after %d changes%s" count
							  (if (= snapshots (or (plist-get (drover-remote-test-stats)
											  :snapshots)
                                                                               0))
                                                              "" " (SNAPSHOT)"))
						  (- (float-time) start)
						  (- (drover-remote-bench--records) before))
			;; The next round counts from fresh titles.
			(drover-remote-test-command "sessions" :n 837 :children 2)
			(drover-remote-bench--time-until
			 (lambda () (zerop (seq-count (lambda (s) (string-prefix-p "Outage" (or (drover-session-title s) "")))
                                                      (drover-remote-bench--records-here)))))))
                    ;; A terminal there mirrored here.
                    (drover-remote-bench--mirror)
                    ;; An OpenCode session there in a full buffer here.
                    (drover-remote-bench--opencode)
                    ;; Both directions, when asked.
                    (when back
                      (unless drover-remote-bench--remote
			(drover-remote-test-command "dial" :name drover-host :local spyglass-registration-file))
                      (when drover-remote-bench--remote
			(drover-remote-test-command "dial-ssh" :name drover-host
                                                    :ssh (drover-remote-bench--getenv "DROVER_BENCH_BACK_HOST"
                                                                                      (drover--system-host))
                                                    :program back :args (vconcat back-args)
                                                    :registration spyglass-registration-file))
                      (drover-remote-test-wait (lambda () (eq (plist-get (drover-remote-test-stats) :through)
                                                              (if (string< drover-host drover-remote-test-agent-name)
								  'dialed 'accepted)))
                                               30)
                      (drover-remote-test-wait
                       (lambda () (= 5 (length (plist-get (drover-remote-test-command "hosts") :remote)))) 30)
                      (let ((theirs (plist-get (drover-remote-test-command "stats" :peer drover-host) :peer)))
			(message "bench: both dial: here through %s, there through %s (%s), there error %s"
				 (plist-get (drover-remote-test-stats) :through)
				 (plist-get theirs :through) (plist-get theirs :state)
				 (plist-get theirs :error))
			(push (list "both directions: kept here/there, sessions of here there"
                                    (format "%s/%s" (plist-get (drover-remote-test-stats) :through)
                                            (plist-get theirs :through))
                                    (format "%d" (length (plist-get (drover-remote-test-command "hosts")
                                                                    :remote)))
                                    "-" "-" "-")
                              drover-remote-bench--rows)))))
                ;; What the agent holds, this host holds.
                (let* ((titles (plist-get (drover-remote-test-command "titles") :titles))
                       (fake (seq-filter (lambda (session) (eq (car (drover-session-id session)) 'fake))
                                         (drover-remote-bench--records-here)))
                       (wrong (seq-count (lambda (entry)
                                           (let ((session (gethash (cons 'fake (aref entry 0))
                                                                   (drover-remote--peer-records
                                                                    (drover-remote--peer drover-remote-test-agent-name)))))
                                             (not (and session
                                                       (equal (drover-session-title session) (aref entry 1))
                                                       (equal (symbol-name (drover-session-status session))
                                                              (aref entry 2))))))
                                         titles)))
                  (unless (and (= (length titles) (length fake)) (zerop wrong))
                    (setq failed t))
                  (push (list "the same sessions on both hosts" (if (and (not failed) (zerop wrong)) "yes" "NO")
                              (format "%d" (length titles)) "-" "-" "-")
                        drover-remote-bench--rows))
                (let ((stats (drover-remote-test-stats)))
                  (push (list "errors spyglass told / drover reported"
                              (format "%d/%d" drover-remote-bench--spyglass-errors drover-remote-bench--warnings)
                              "-" "-" "-" "-")
                        drover-remote-bench--rows)
                  (push (list "greetings received, 4 streams" (format "%d" (or (plist-get stats :hellos) 0))
                              "-" "-" "-" "-")
                        drover-remote-bench--rows))
                (let ((agent (drover-remote-test-command "stats")))
                  (push (list "resident memory, viewer/agent MB"
                              (format "%d/%d" (/ (cdr (assq 'rss (process-attributes (emacs-pid)))) 1024)
                                      (/ (plist-get agent :rss) 1024))
                              "-" "-" "-" "-")
                        drover-remote-bench--rows))))
          (error (setq failed t)
                 (message "bench: failed: %s" (error-message-string err))))
      (cancel-timer probe)
      (cancel-timer guard)
      (ignore-errors (drover-remote-test-stop-agent))
      (ignore-errors (drover-remote-mode -1))
      (when (and drover-remote-bench--remote drover-remote-bench--remote-dir)
        (ignore-errors (drover-remote-bench--remote-run (format "rm -rf %s" drover-remote-bench--remote-dir))))
      (delete-directory dir t))
    (princ (format "\ndrover remote bench, %s, viewer %s (Emacs %s), agent %s\n\n"
                   (format-time-string "%F %R") (system-name) emacs-version
                   (or drover-remote-bench--remote "localhost")))
    (drover-remote-bench--print)
    (kill-emacs (if failed 1 0))))

(provide 'drover-remote-bench)
;;; drover-remote-bench.el ends here
