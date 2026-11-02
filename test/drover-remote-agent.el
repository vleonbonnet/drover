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
;; break streams, dial back, tell what it holds.  With DROVER_AGENT_STRICT
;; set, it refuses hosts it does not know.  It says READY on
;; standard error once it listens, kills itself past 2 GB of resident
;; memory, and after ten minutes without a command.  It loads neither
;; drover's lists nor its test helpers, so that its files are few to copy.

;;; Code:

(require 'cl-lib)
(require 'drover-core)
(require 'drover-remote)
(require 'spyglass)

(defvar drover-remote-agent--sessions (make-hash-table :test #'equal)
  "The fake backend's sessions, by identifier.")

(defvar drover-remote-agent--order nil
  "The identifiers of the fake sessions, in order.")

(defvar drover-remote-agent--last-command (float-time)
  "When the last command came.")

(defvar drover-remote-agent--quit nil
  "Non-nil once told to quit.")

(defun drover-remote-agent--say (format-string &rest args)
  "Tell the driver FORMAT-STRING with ARGS, on standard error."
  (message "DROVER-AGENT %s" (apply #'format format-string args)))

(defun drover-remote-agent--register ()
  "Register the fake backend."
  (drover-register-backend
   'fake :label "Fake"
   :sessions (lambda () (delq nil (mapcar (lambda (id) (gethash id drover-remote-agent--sessions))
                                          drover-remote-agent--order)))
   :session (lambda (id) (gethash id drover-remote-agent--sessions))
   :recap (lambda (session callback)
            (funcall callback (list :prompt (concat "Prompt of " (drover-session-id session))
                                    :reply (concat "Reply of " (drover-session-id session))
                                    :changes (list (list "a.el" 3 1)))))
   :preview (lambda (session callback)
              (funcall callback (concat "Preview of " (drover-session-id session))))
   :history (lambda (callback)
              ;; Later, as a service answers.
              (run-at-time 0.05 nil
                           (lambda ()
                             (funcall callback (list (drover-remote-agent--session "old1" 0 :status 'idle)
                                                     (drover-remote-agent--session "old2" 0 :status 'idle))))))
   :archived (lambda (callback)
               (funcall callback (list (drover-remote-agent--session "gone1" 0 :status 'idle
                                                                     :archived 1700000000.0))))))

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
       ;; Streams broken, then CHANGE sessions changed while they are.
       (funcall reply (list :ok t))
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
          drover-refresh-delay 0.02
          spyglass-registration-file (expand-file-name "server.json" dir)
          drover-remote-standby-delay 1
          drover-remote-accept-unknown (unless (getenv "DROVER_AGENT_STRICT") 'read)
          drover-remote-peers (and peers (not (string-empty-p peers))
                                   (mapcar (lambda (name) (list name)) (split-string peers ","))))
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
