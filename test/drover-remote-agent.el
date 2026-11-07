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
;; break streams, stop answering a while, dial back, tell what it holds,
;; restart its remote layer.
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
;; With DROVER_AGENT_OPENCODE set, it runs a fake OpenCode service
;; (test/drover-fake-opencode.el), which opencode.el finds as its
;; service, and a backend `opencode' of its sessions, which drover's
;; remote layer relays to peers, whose search of what was said is a
;; stand-in of the backend's, as it answers through `opencode-search'.
;; With DROVER_AGENT_CLAUDE set, it lists the conversations Claude Code
;; recorded in the home it names, its directory's for "here", or the
;; user's for 1, read only; DROVER_AGENT_CODEX likewise for Codex.
;; Its backends start sessions for peers (:create): the fake one a
;; session of the engine `fake' with a terminal, displayed as front ends
;; display theirs, failing in a directory named \"fail\" and starting a
;; second late in one named \"slow\"; the OpenCode one a session of its
;; fake service; each engine with its agents, models and defaults.
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

(defvar drover-task-repositories)
(defvar drover-task-worktree-root)
(defvar drover-task-harness)
(defvar drover-fake-opencode--sessions)
(defvar drover-fake-opencode--order)
(defvar drover-fake-opencode--streams)
(defvar drover-fake-opencode--password)
(defvar drover-fake-opencode-directory)
(defvar opencode-service-registration-file)
(defvar opencode-command)
(defvar drover-claude-code-home)
(defvar drover-claude-code-history-days)
(defvar drover-claude-code-agents-interval)
(defvar drover-codex-home)
(defvar drover-codex-history-days)
(defvar drover-summary-delay)
(defvar drover-summary-catch-up)
(defvar drover-summary-generate-function)
(defvar drover-changes-delay)
(declare-function drover-summary-mode "drover-summary" (&optional arg))
(declare-function drover-fake-opencode-start "drover-fake-opencode" (directory))
(declare-function drover-fake-opencode-add "drover-fake-opencode" (id title))
(declare-function drover-fake-opencode-turn "drover-fake-opencode" (id text &optional inbox))
(declare-function drover-fake-opencode-requests "drover-fake-opencode" (&optional method))
(declare-function drover-fake-opencode-port "drover-fake-opencode" ())
(declare-function drover-fake-opencode-archive "drover-fake-opencode" (id))
(declare-function drover-claude-code-encode-directory "drover-claude-code" (dir))
(declare-function drover-claude-code-projects-directory "drover-claude-code" ())
(declare-function drover-claude-code-native-directory "drover-claude-code" (dir))

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

(defvar drover-remote-agent--pushes nil
  "Pushes posted, newest first, as [URL TOKEN BODY], with DROVER_AGENT_PUSH.")

(defvar drover-remote-agent--idle 1000.0
  "How long the agent tells it has been idle, in seconds, for pushes.")

(defvar drover-remote-agent--push-status 200
  "The HTTP status the stand-in push server answers.")

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

(defvar-local drover-remote-agent--submitted nil
  "What the stand-in shell of a fake session submitted, newest first.")

(defun drover-remote-agent--shell-submit ()
  "Submit the input of the stand-in shell, as agent-shell's submit does."
  (interactive)
  (goto-char (point-max))
  (let ((mark (process-mark (get-buffer-process (current-buffer)))))
    (push (buffer-substring-no-properties mark (point-max)) drover-remote-agent--submitted)
    (insert "\n")
    (set-marker mark (point-max))))

(defun drover-remote-agent--shell-interrupt ()
  "Interrupt the stand-in shell, asking first as agent-shell does."
  (interactive)
  (when (y-or-n-p "Interrupt? ")
    (push "INTERRUPTED" drover-remote-agent--submitted)))

(defvar-keymap drover-remote-agent--shell-mode-map
  :parent comint-mode-map
  "<remap> <comint-send-input>" #'drover-remote-agent--shell-submit
  "C-c C-c" #'drover-remote-agent--shell-interrupt
  "M-RET" #'drover-remote-agent--shell-submit)

(define-derived-mode drover-remote-agent--shell-mode comint-mode "Stand-in agent-shell"
  "A shell submitting as agent-shell does, through a command of its own.
`comint-send-input' alone only commits the line, as shell-maker's
`comint-input-sender' only keeps the input: nothing is submitted."
  (setq-local comint-input-sender (lambda (_process _input))))

(defun drover-remote-agent--terminal (id kind)
  "Start the terminal of fake session ID, of KIND, and return it.
KIND \"pty\" runs `cat' in a pty, sized by viewers as front ends'
terminals are; where processes run through pipes, as on Windows, `cat'
or, without it, a buffer echoing what is typed.  \"comint\" is a comint
buffer running `cat'; \"agent-shell\" one submitting and interrupting
through commands of its own, as agent-shell does."
  (let ((buffer (generate-new-buffer (format "*fake terminal %s*" id))))
    (with-current-buffer buffer
      (setq default-directory temporary-file-directory)
      (pcase kind
        ("agent-shell"
         (make-comint-in-buffer (format "fake-agent-shell-%s" id) buffer "cat")
         (drover-remote-agent--shell-mode)
         (insert "Agent> ")
         (set-marker (process-mark (get-buffer-process buffer)) (point-max))
         (let ((session-id id))
           (setq-local drover-session-at-point-function
                       (lambda () (gethash session-id drover-remote-agent--sessions)))))
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
    ;; A session buffer, as front ends' terminals are: drover's prompts
    ;; scheduled for it find it.
    (with-current-buffer buffer
      (unless drover-session-at-point-function
        (let ((session-id id))
          (setq-local drover-session-at-point-function
                      (lambda () (gethash session-id drover-remote-agent--sessions))))))
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

(defun drover-remote-agent--close (session)
  "Close SESSION's terminal, its input not sent to the kill ring, as front ends do."
  (when-let* ((buffer (drover-remote-agent--buffer session)))
    (prog1 (drover-keep-unsent
            (list (when-let* ((process (get-buffer-process buffer))
                              ((with-current-buffer buffer (derived-mode-p 'comint-mode))))
                    (with-current-buffer buffer
                      (buffer-substring-no-properties (process-mark process) (point-max)))))
            buffer)
      (drover-kill-buffer buffer))))

(defun drover-remote-agent--open (session &optional _other-window)
  "Show SESSION's terminal, starting one as a front end resuming it would."
  (push (vector "open" (drover-session-id session) :false) drover-remote-agent--calls)
  (pop-to-buffer-same-window (or (drover-remote-agent--buffer session)
                                 (drover-remote-agent--terminal (drover-session-id session) "pty"))))

(defvar drover-remote-agent--created 0
  "How many sessions its backends started for peers.")

(defun drover-remote-agent--models ()
  "Return the models of the engine `fake'."
  (list (drover-model-create :provider-id "fakeco" :provider-name "FakeCo" :id "fast-1" :name "Fast 1"
                             :variants '((low :effort "low") (high :effort "high")) :default-variant 'low
                             :engines '(fake))
        (drover-model-create :provider-id "fakeco" :provider-name "FakeCo" :id "deep-2" :name "Deep 2"
                             :variants '((max :effort "max")) :default-variant 'max
                             :engines '(fake))))

(defun drover-remote-agent--note-create (directory engine options)
  "Note that a session of ENGINE was asked in DIRECTORY with start OPTIONS."
  (let ((model (plist-get options :model)))
    (push (vector "create" directory
                  (format "%s %s %s %s %s %s" engine (or (plist-get options :agent) "-")
                          (if (drover-model-p model) (drover-model-id model) "-")
                          (or (plist-get options :variant) "-")
                          (or (plist-get options :title) "-")
                          (if (plist-get options :hidden) "hidden" "shown")))
          drover-remote-agent--calls)))

(defun drover-remote-agent--create (directory engine options then else)
  "Start a fake session of ENGINE in DIRECTORY with start OPTIONS, its terminal shown.
As a front end does: THEN gets its session, ELSE why not, in a
directory named \"fail\"; in one named \"slow\", a second later."
  (drover-remote-agent--note-create directory engine options)
  (let ((start (lambda ()
                 (if (string-match-p "fail" directory)
                     (funcall else "the fake front end failed")
                   (let* ((id (format "new%d" (cl-incf drover-remote-agent--created)))
                          (session (drover-remote-agent--session
                                    id 0 :status 'idle :directory directory
                                    :title (or (plist-get options :title) (format "New %s" id)))))
                     (drover-remote-agent--set id session)
                     (pop-to-buffer (drover-remote-agent--terminal id "pty"))
                     (drover-changed-now)
                     (funcall then session))))))
    (if (string-match-p "slow" directory)
        (run-at-time 1 nil start)
      (funcall start))))

(defun drover-remote-agent--register-engines ()
  "Register the engine `fake', its agents, models and defaults."
  (apply #'drover-register-engine 'fake :label "Fake"
                          ;; Its conversations linked, as engines telling where they ran are.
                          (append
                           (and (getenv "DROVER_AGENT_LINKABLE")
                                (list :directory (lambda (_id then _else) (funcall then "/work/project/"))))
                           (list
                          :defaults (lambda (_directory callback)
                                      (run-at-time 0.02 nil (lambda () (funcall callback (list :agent "build")))))
                          :resolve (lambda (defaults agent model variant)
                                     (list :agent (or agent (plist-get defaults :agent))
                                           :model (or model (car (drover-remote-agent--models)))
                                           :variant (or variant (and (not model) 'high))))
                          :agents (lambda (_directory) (list "build" "plan")))))
  (drover-register-model-source 'fake (lambda (_directory) (drover-remote-agent--models)) 0))

(defun drover-remote-agent--register ()
  "Register the fake backend, and a front end resuming its conversations."
  (drover-remote-agent--register-engines)
  (drover-register-resumer 'fake 'fake (lambda (id _directory)
                                         (drover-remote-agent--terminal id "pty"))
                           :by-id t)
  (drover-register-backend
   'fake :label "Fake"
   :buffer #'drover-remote-agent--buffer
   :close #'drover-remote-agent--close
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
   ;; Options applied as a front end would, the agent changing at once.
   :apply-options (lambda (session options then)
                    (push (vector "options" (drover-session-id session)
                                  (or (plist-get options :agent) :false)
                                  (let ((model (plist-get options :model)))
                                    (if (drover-model-p model) (drover-model-id model) :false))
                                  (let ((variant (plist-get options :variant)))
                                    (if variant (symbol-name variant) :false)))
                          drover-remote-agent--calls)
                    (when-let* ((agent (plist-get options :agent))
                                (live (gethash (drover-session-id session) drover-remote-agent--sessions)))
                      (drover-remote-agent--set (drover-session-id session)
                                                (drover-remote-agent--with live :agent agent))
                      (drover-changed-now))
                    (funcall then))
   :options (lambda (_session) nil)
   :settable (lambda (_session) '(:agent :variant))
   :engines '(fake)
   :create #'drover-remote-agent--create
   :send (lambda (session text _token &optional _attachments)
           (push (vector "prompt" (drover-session-id session) text) drover-remote-agent--calls))
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

(defvar drover-remote-agent--oc-archived (make-hash-table :test #'equal)
  "When the fake OpenCode service's sessions archived were, by identifier.")

(defun drover-remote-agent--opencode-session (id)
  "Return the record of the fake OpenCode service's session ID, or nil."
  (when-let* ((info (gethash id drover-fake-opencode--sessions)))
    (drover-session-create :backend 'opencode :id id :title (alist-get 'title info)
                           :directory drover-fake-opencode-directory :status 'idle
                           :engine 'opencode :engine-id id :agent "build"
                           :archived (gethash id drover-remote-agent--oc-archived)
                           :updated (float-time) :created (float-time))))

(defvar drover-remote-agent--search-delay 0.01
  "Seconds the stand-in of OpenCode's search takes to answer.")

(defvar drover-remote-agent--search-failure nil
  "What the stand-in of OpenCode's search fails with, or nil when it answers.")

(defun drover-remote-agent--oc-texts (id)
  "Return the texts of the fake OpenCode session ID's messages, as (MESSAGE-ID . TEXT)."
  (delq nil (mapcar (lambda (message)
                      (when-let* ((text (or (alist-get 'text message)
                                            (alist-get 'text (seq-first (alist-get 'content message))))))
                        (cons (alist-get 'id message) text)))
                    (gethash id drover-fake-opencode--messages))))

(defun drover-remote-agent--oc-search (query scope callback)
  "Search the fake OpenCode service's messages for QUERY, for CALLBACK.
A stand-in of drover's OpenCode backend's :search, as it answers
through `opencode-search': later, a match per session naming it by its
key, the archived sessions only while SCOPE says they show; the calls
noted, and their cancellations."
  (push (vector "search" query :false) drover-remote-agent--calls)
  (let* ((needle (downcase query))
         (timer (run-at-time
                 drover-remote-agent--search-delay nil
                 (lambda ()
                   (if drover-remote-agent--search-failure
                       (funcall callback (list :failure drover-remote-agent--search-failure))
                     (let ((matches nil))
                       (dolist (id drover-fake-opencode--order)
                         (when (and (or (plist-get scope :archived)
                                        (not (gethash id drover-remote-agent--oc-archived)))
                                    (funcall (plist-get scope :wanted) (list :key (cons 'opencode id))))
                           (when-let* ((found (seq-find (lambda (entry) (string-search needle (downcase (cdr entry))))
                                                        (drover-remote-agent--oc-texts id))))
                             (push (list :key (cons 'opencode id) :snippet (cdr found) :position (car found))
                                   matches))))
                       (funcall callback (nreverse matches))
                       (funcall callback 'done)))))))
    (lambda ()
      (push (vector "search-cancel" query :false) drover-remote-agent--calls)
      (cancel-timer timer))))

(defun drover-remote-agent--opencode ()
  "Start the fake OpenCode service, and the backend of its sessions."
  (require 'drover-fake-opencode)
  (require 'opencode-service)
  (setq opencode-service-registration-file
        (drover-fake-opencode-start (or (getenv "DROVER_AGENT_DIR") default-directory))
        ;; Never the user's CLI, whose version opencode.el asks as it
        ;; finds the service, waiting for it, silent meanwhile.
        opencode-command "drover-agent-no-opencode")
  (drover-register-engine 'opencode :label "OpenCode"
                          :defaults (lambda (_directory callback) (funcall callback nil))
                          :agents (lambda (_directory) (list "build")))
  (drover-register-backend
   'opencode :label "OpenCode"
   :sessions (lambda () (delq nil (mapcar #'drover-remote-agent--opencode-session drover-fake-opencode--order)))
   :session #'drover-remote-agent--opencode-session
   :search #'drover-remote-agent--oc-search
   ;; A session of its service, as drover's OpenCode backend starts one.
   :engines '(opencode)
   :create (lambda (directory engine options then _else)
             (drover-remote-agent--note-create directory engine options)
             (let ((id (format "ses_new%d" (cl-incf drover-remote-agent--created))))
               (drover-fake-opencode-add id (or (plist-get options :title) "New session"))
               (drover-backend-changed 'opencode id)
               (drover-changed-now)
               (run-at-time 0.05 nil (lambda () (funcall then (drover-remote-agent--opencode-session id))))))
   :send (lambda (session text _token &optional _attachments)
           (push (vector "prompt" (drover-session-id session) text) drover-remote-agent--calls)
           (drover-fake-opencode-turn (drover-session-id session) text))))

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
               ;; A session telling no activity, as some backends' do.
               (when (plist-get args :untimed)
                 (setf (drover-session-since copy) nil (drover-session-updated copy) nil))
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
      ("review-set"
       ;; The changes the fake engine tells of each conversation: a file
       ;; whose patch adds LINES lines, naming the conversation and the
       ;; scope, and a file deleted.
       (let ((lines (or (plist-get args :lines) 2)))
         (require 'drover-model)
         (drover-register-engine
          'fake :label "Fake"
          :changes (lambda (id scope _directory callback)
                     (funcall callback
                              (list :files (list (list :file "src/a.el" :status 'modified
                                                       :additions lines :deletions 1
                                                       :patch (concat (format "@@ -1,1 +1,%d @@ %s %s\n" lines id scope)
                                                                      "-old\n"
                                                                      (mapconcat (lambda (index) (format "+new %d\n" index))
                                                                                 (number-sequence 1 lines) "")))
                                                 (list :file "gone.el" :status 'deleted))
                                    :notes (list "A note")))))
         (funcall reply (list :ok t))))
      ("calls"
       (funcall reply (list :calls (vconcat (reverse drover-remote-agent--calls)))))
      ("summaries"
       ;; Summaries written as turns end, by a stand-in model.
       (require 'drover-summary)
       (setq drover-summary-delay 0
             drover-summary-catch-up nil
             drover-summary-generate-function
             (lambda (_prompt callback)
               (funcall callback (list :text "Done: summarized on the agent\nIts recap\n---\nIts story on the agent."))))
       (drover-summary-mode 1)
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("changes"
       ;; What sessions changed, read from the fake engine as peers ask.
       (require 'drover-changes)
       (setq drover-changes-delay 0
             drover-changes-prefetch-function #'drover-changes-prefetch)
       (funcall reply (list :ok t)))
      ("search-set"
       ;; How the stand-in of OpenCode's search answers: after DELAY
       ;; seconds, failing with FAILURE when given.
       (setq drover-remote-agent--search-delay (or (plist-get args :delay) 0.01)
             drover-remote-agent--search-failure (plist-get args :failure))
       (funcall reply (list :ok t)))
      ("clone-state"
       ;; The conversations it exported for peers to clone, and the clones
       ;; it receives.
       (funcall reply (list :exports (hash-table-count drover-remote--exports)
                            :clones (hash-table-count drover-remote--clones))))
      ("file-info"
       ;; The size and MD5 of FILE, its bytes as they are, or nothing.
       (let ((file (plist-get args :file)))
         (funcall reply
                  (if (and (stringp file) (file-regular-p file))
                      (list :size (file-attribute-size (file-attributes file))
                            :md5 (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (insert-file-contents-literally file)
                                   (secure-hash 'md5 (current-buffer))))
                    (list :size :false)))))
      ("mkdir"
       ;; A directory NAME of its own directory, made; its name.  With
       ;; FILE, a file of that name in it too, holding TEXT.
       (let ((directory (file-name-as-directory
                         (expand-file-name (plist-get args :name)
                                           (or (getenv "DROVER_AGENT_DIR") default-directory)))))
         (make-directory directory t)
         (when (plist-get args :file)
           (with-temp-file (expand-file-name (plist-get args :file) directory)
             (insert (or (plist-get args :text) ""))))
         (funcall reply (list :directory directory))))
      ("claude-file"
       ;; Where Claude Code records conversation ID run in DIRECTORY,
       ;; and how its records spell DIRECTORY.
       (let ((directory (plist-get args :directory)))
         (funcall reply (list :file (expand-file-name (concat (plist-get args :id) ".jsonl")
                                                      (expand-file-name (drover-claude-code-encode-directory directory)
                                                                        (drover-claude-code-projects-directory)))
                              :spelled (drover-claude-code-native-directory
                                        (file-truename (expand-file-name directory)))))))
      ("claude-home"
       (funcall reply (list :home (expand-file-name drover-claude-code-home)
                            :dir (expand-file-name (or (getenv "DROVER_AGENT_DIR") default-directory)))))
      ("recorded"
       ;; How many conversations engines recorded the index lists, by engine.
       (funcall reply (list :claude (seq-count (lambda (session) (eq (drover-session-backend session) 'claude-code))
                                               (drover-index-sessions))
                            :codex (seq-count (lambda (session) (eq (drover-session-backend session) 'codex))
                                              (drover-index-sessions)))))
      ("oc-add"
       ;; Sessions of the fake OpenCode service, as (ID TITLE).
       (seq-doseq (pair (plist-get args :sessions))
         (drover-fake-opencode-add (aref pair 0) (aref pair 1))
         (drover-backend-changed 'opencode (aref pair 0)))
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("oc-turn"
       ;; A turn run by another client of the service, DELAY seconds
       ;; later; with SIZE, its prompt is TEXT and SIZE characters more,
       ;; as a long tool output streams.
       (let ((id (plist-get args :id))
             (text (concat (plist-get args :text)
                           (and (plist-get args :size)
                                (concat " " (make-string (plist-get args :size) ?x) " "
                                        (plist-get args :text) "-END")))))
         (funcall reply (list :ok t))
         (run-at-time (or (plist-get args :delay) 0) nil
                      (lambda () (drover-fake-opencode-turn id text)))))
      ("oc-archive"
       (puthash (plist-get args :id) (float-time) drover-remote-agent--oc-archived)
       (drover-fake-opencode-archive (plist-get args :id))
       (drover-backend-changed 'opencode (plist-get args :id))
       (drover-changed-now)
       (funcall reply (list :ok t)))
      ("oc-requests"
       ;; What reached the service, oldest first, as [METHOD PATH BODY].
       (funcall reply (list :requests (vconcat (mapcar (lambda (request)
                                                         (vector (nth 0 request) (nth 1 request)
                                                                 (or (nth 2 request) "")))
                                                       (drover-fake-opencode-requests
                                                        (plist-get args :method)))))))
      ("oc-streams"
       (funcall reply (list :streams (length (seq-filter #'process-live-p drover-fake-opencode--streams))
                            :port (drover-fake-opencode-port)
                            :password drover-fake-opencode--password)))
      ("hold"
       (setq drover-remote-agent--hold (list 'held))
       (funcall reply (list :ok t)))
      ("release"
       (let ((held (reverse (cdr drover-remote-agent--hold))))
         (setq drover-remote-agent--hold nil)
         (mapc #'funcall held)
         (funcall reply (list :released (length held)))))
      ("freeze"
       ;; Its Emacs stuck once it answered, as in a question a dialog box
       ;; asks: it reads nothing, runs no timer, its connections open to
       ;; the system, until the file THAW exists, or SECONDS passed, 60
       ;; by default.  As SIGSTOP stops it, which Windows has not.
       (let ((thaw (plist-get args :thaw))
             (seconds (or (plist-get args :seconds) 60)))
         (funcall reply (list :ok t))
         (run-at-time 0 nil
                      (lambda ()
                        (let ((quiet (make-pipe-process :name "drover-agent-frozen" :noquery t))
                              (end (+ (float-time) seconds)))
                          (unwind-protect
                              (while (and (< (float-time) end) (not (file-exists-p thaw)))
                                ;; Reading QUIET alone, which says nothing,
                                ;; running no timer.
                                (accept-process-output quiet 0.05 nil 1))
                            (delete-process quiet)))))))
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
      ("pushes"
       (funcall reply (list :pushes (vconcat (reverse drover-remote-agent--pushes)))))
      ("idle"
       (setq drover-remote-agent--idle (float (plist-get args :seconds)))
       (funcall reply (list :ok t)))
      ("fire-idle"
       (drover-remote-push-fire-idle)
       (funcall reply (list :ok t)))
      ("host-names"
       (funcall reply (list :names (vconcat (drover-remote--host-names)))))
      ("peer-link"
       ;; How the link to a peer is kept: its heartbeats and resume window.
       (let* ((peer (drover-remote--peer (plist-get args :peer)))
              (connection (and peer (drover-remote--peer-connection peer))))
         (funcall reply (if (not connection)
                            (list :none t)
                          (list :interval (spyglass--heartbeat-interval connection)
                                :timeout (spyglass--heartbeat-timeout connection)
                                :resume (spyglass--resume-window connection)
                                :viewer (if (drover-remote--viewer-p peer) t :false)
                                :line (drover-remote--state-line peer))))))
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
                        :submitted (if (buffer-live-p buffer)
                                       (vconcat (reverse (buffer-local-value 'drover-remote-agent--submitted buffer)))
                                     [])
                        :point (if (buffer-live-p buffer) (with-current-buffer buffer (point)) 0)
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
      ("reload"
       ;; Spyglass and the remote layer loaded anew, as updating them in a
       ;; running Emacs does; with RELAYOUT, the layer's state made by
       ;; another version too, made anew without stopping.
       (funcall reply (list :ok t))
       (let ((relayout (plist-get args :relayout)))
         (run-at-time 0.05 nil
                      (lambda ()
                        (dolist (file '("spyglass-wire" "spyglass-conn" "spyglass-channel" "spyglass-owner"
                                        "spyglass-mirror" "spyglass-window" "spyglass" "drover-remote"))
                          (load file nil t))
                        (when relayout
                          (setf (drover-remote--state-stamp drover-remote--state) 'older)
                          (drover-remote--current))))))
      ("unshare"
       ;; The share of a terminal gone, its session running on.
       (let ((share (seq-find (lambda (share) (equal (spyglass-share-id share) (plist-get args :id)))
                              (spyglass-shares))))
         (when share (spyglass-unshare share))
         (funcall reply (list :ok (if share t :false)))))
      ("kill-terminal"
       ;; Its terminal killed there, its session still listed.
       (let ((buffer (gethash (plist-get args :id) drover-remote-agent--terminals)))
         (drover-kill-buffer buffer)
         (drover-backend-changed 'fake (plist-get args :id))
         (funcall reply (list :ok t))))
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
          ;; The fake backend's sessions alone, not those this host's
          ;; engines recorded, unless asked for Claude Code's.
          drover-transcript-engines (append (and (getenv "DROVER_AGENT_CLAUDE") '(claude-code))
                                            (and (getenv "DROVER_AGENT_CODEX") '(codex)))
          drover-refresh-delay 0.02
          ;; Kills stay in its kill ring, off the system's clipboard (see
          ;; test/batch-tests.el).
          interprogram-cut-function nil
          interprogram-paste-function nil
          ;; What sessions changed is read once a test asks for it.
          drover-changes-prefetch-function nil
          spyglass-registration-file (expand-file-name "server.json" dir)
          drover-remote-standby-delay 1
          drover-remote-accept-unknown (unless (getenv "DROVER_AGENT_STRICT")
                                         (intern (or (getenv "DROVER_AGENT_ACCESS") "read")))
          drover-remote-peers (and peers (not (string-empty-p peers))
                                   (mapcar (lambda (name)
                                             (list name :access (intern (or (getenv "DROVER_AGENT_ACCESS") "read"))))
                                           (split-string peers ","))))
    ;; Hosts owning no sessions, as phones, with links of their own.
    (when-let* ((viewers (getenv "DROVER_AGENT_VIEWERS")))
      (setq drover-remote-peers
            (append drover-remote-peers
                    (mapcar (lambda (name)
                              (list name :viewer t :heartbeat '(2 . 8) :resume 30
                                    :access (intern (or (getenv "DROVER_AGENT_ACCESS") "read"))))
                            (split-string viewers "," t)))))
    ;; Pushes posted to a stand-in, recorded: what the phone would get.
    (when (getenv "DROVER_AGENT_PUSH")
      (require 'drover-remote-push)
      (setq drover-remote-push-endpoints '("https://ntfy.example/")
            drover-remote-push-delay 0
            drover-remote-push-spacing 0
            drover-remote-push-gather 0.3
            drover-remote-push-idle-function (lambda () (seconds-to-time drover-remote-agent--idle))
            drover-remote-push-send-function
            (lambda (url token body callback)
              (push (vector url (or token :false) body) drover-remote-agent--pushes)
              (funcall callback drover-remote-agent--push-status))))
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
    (when (getenv "DROVER_AGENT_OPENCODE")
      (drover-remote-agent--opencode))
    (when-let* ((checkout (getenv "DROVER_AGENT_TASKS")))
      ;; A repository whose tasks it starts and finishes for its peers,
      ;; in sessions of its fake backend.
      (require 'drover-task)
      ;; How the repository works is read from it: no model is asked here.
      (setq drover-repo-generate-function (lambda (_prompt _system callback)
                                            (funcall callback (list :failure "no model in tests"))))
      (setq drover-task-repositories `(("test/repo" :checkout ,checkout))
            drover-task-worktree-root (expand-file-name "worktrees/" dir)
            drover-task-harness '(:backend fake :engine fake)))
    (when-let* ((claude (getenv "DROVER_AGENT_CLAUDE")))
      ;; Its conversations listed, as drover starts: every one live,
      ;; none asked of the CLI.
      (require 'drover-claude-code)
      (pcase claude
        ((or "" "1"))
        ("here"
         ;; Recorded, so that drover lists what lands there.
         (setq drover-claude-code-home (expand-file-name "claude" dir))
         (make-directory (expand-file-name "projects" drover-claude-code-home) t))
        (_ (setq drover-claude-code-home claude)))
      (setq drover-claude-code-history-days nil
            drover-claude-code-agents-interval nil))
    (when-let* ((codex (getenv "DROVER_AGENT_CODEX")))
      (require 'drover-codex)
      (setq drover-codex-home (if (equal codex "here") (expand-file-name "codex" dir) codex)
            drover-codex-history-days nil))
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
