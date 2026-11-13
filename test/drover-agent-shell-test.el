;;; drover-agent-shell-test.el --- Tests of the agent-shell backend  -*- lexical-binding: t; -*-

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

;; Shells are real `agent-shell-mode' buffers with agent-shell's own
;; state and event dispatch, but no agent process: tests set the state
;; an agent would report and emit the events it would cause.  Requests
;; to agents (`agent-shell--start', the option setters, inserting and
;; queueing prompts) are stubbed.
;;
;; Loading agent-shell loads the backend, which registers itself and
;; follows shells globally; this file undoes that at once, so that the
;; tests loaded after it see no trace of it, and every test installs
;; what it needs within its own extent.

;;; Code:

(require 'drover-test-helpers)
;; agent-shell first: drover loads the backend once agent-shell is loaded.
(require 'agent-shell)
(require 'drover-agent-shell)
(require 'drover-transcript-fixtures)

(drover-agent-shell-unload-function)

(defvar drover-agent-shell-test--buffers nil
  "Buffers the running test made, killed when it ends.")

(defvar drover-agent-shell-test--calls nil
  "Calls of stubbed agent-shell functions, newest first, as (FUNCTION . ARGUMENTS).")

(defvar drover-agent-shell-test--warnings nil
  "Messages of `drover-warn', newest first.")

(defmacro drover-agent-shell-test-with (&rest body)
  "Run BODY with the agent-shell backend alone registered, in isolation.
Engines, model sources, the models learnt and the change timer are
isolated, engines read their local state from the throwaway homes of
`drover-test-with-homes', `drover-warn' collects its messages in
`drover-agent-shell-test--warnings', and the buffers BODY makes with
`drover-agent-shell-test-shell' are killed."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (let ((drover--backends nil)
           (drover--started nil)
           (drover--running nil)
           (drover--change-timer nil)
           (drover--changed-keys nil)
           (drover--changed-backends nil)
           (drover--changed-all nil)
           (drover--changed-applied nil)
           (drover--index nil)
           (drover-changed-hook nil)
           (drover--closing nil)
           (drover--closing-saved nil)
           (drover--closing-deferred nil)
           (drover--closing-restored t)
           (drover--closing-written 'unread)
           (drover-show-history nil)
           (drover--history nil)
           (drover-show-archived nil)
           (drover--archived nil)
           (drover--fetched-serial 0)
           (drover--fetch-generations nil)
           (drover--fetch-kept nil)
           (drover--fetch-started nil)
           (drover--fetch-pending nil)
           (drover--engines nil)
           (drover--model-sources nil)
           (drover--model-memory nil)
           (drover--model-memory-stamp 'unread)
           (drover-agent-shell--gemini-registered nil)
           (drover--resumers nil)
           (drover--resumer-properties nil)
           (drover-claude-code-transcripts--users nil)
           (drover-claude-code-transcripts--index nil)
           (drover-codex-rollouts--users nil)
           (drover-codex-rollouts--index nil)
           (drover-agent-shell-test--buffers nil)
           (drover-agent-shell-test--calls nil)
           (drover-agent-shell-test--warnings nil)
           ;; Busy test shells must not ask the system to stay awake.
           (agent-shell-inhibit-system-sleep nil))
       (unwind-protect
           (cl-letf (((symbol-function 'drover-warn)
                      (lambda (format-string &rest args)
                        (push (apply #'format-message format-string args)
                              drover-agent-shell-test--warnings))))
             (drover-agent-shell--register)
             ,@body)
         (dolist (buffer drover-agent-shell-test--buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))
         (when (timerp drover--change-timer)
           (cancel-timer drover--change-timer))))))

(defun drover-agent-shell-test--recorder (function &optional value)
  "Return a stub of FUNCTION recording its arguments and returning VALUE."
  (lambda (&rest arguments)
    (push (cons function arguments) drover-agent-shell-test--calls)
    value))

(defun drover-agent-shell-test--calls-of (function)
  "Return the arguments of the recorded calls of FUNCTION, oldest first."
  (nreverse (mapcar #'cdr (seq-filter (lambda (call) (eq (car call) function))
                                      drover-agent-shell-test--calls))))

(defun drover-agent-shell-test-shell (engine &optional name)
  "Return a new shell of ENGINE, named NAME, as agent-shell would make it.
It has agent-shell's state and buffer-local shell configuration but no
agent; drover does not follow it yet (see `drover-agent-shell--setup')."
  (let ((buffer (generate-new-buffer (or name (format "%s shell" engine)))))
    (push buffer drover-agent-shell-test--buffers)
    (with-current-buffer buffer
      ;; agent-shell runs its mode hook itself once the state exists.
      (let ((agent-shell-mode-hook nil))
        (agent-shell-mode))
      (setq default-directory (file-name-as-directory temporary-file-directory))
      (setq-local shell-maker--config (agent-shell--make-shell-maker-config
                                       :prompt "> " :prompt-regexp "^> "))
      (setq-local agent-shell--state (agent-shell--make-state
                                      :agent-config `((:identifier . ,engine))
                                      :buffer buffer)))
    buffer))

(defun drover-agent-shell-test-initialize (buffer &optional session-id)
  "Make the shell of BUFFER initialized, with ACP SESSION-ID."
  (with-current-buffer buffer
    (map-put! (map-elt agent-shell--state :session) :id (or session-id "acp-session"))
    (map-put! agent-shell--state :init-finished t)))

(defun drover-agent-shell-test-options (buffer &rest options)
  "Give the shell of BUFFER config OPTIONS, each (ID CATEGORY CURRENT VALUES...).
VALUES are strings, named by themselves capitalized."
  (with-current-buffer buffer
    (map-put! (map-elt agent-shell--state :session) :config-options
              (mapcar (pcase-lambda (`(,id ,category ,current . ,values))
                        `((:id . ,id) (:category . ,category) (:current-value . ,current)
                          (:options . ,(mapcar (lambda (value)
                                                 `((:value . ,value) (:name . ,(capitalize value))))
                                               values))))
                      options))))

(defun drover-agent-shell-test-acp-models (buffer models current &optional levels level)
  "Give the shell of BUFFER the model option MODELS, running CURRENT.
MODELS are (VALUE . NAME); LEVELS, the values of a thought level option
whose current value is LEVEL, or nil for none, as an agent offers no
effort for a model without effort."
  (with-current-buffer buffer
    (map-put! (map-elt agent-shell--state :session) :config-options
              (append
               `(((:id . "model") (:category . "model") (:current-value . ,current)
                  (:options . ,(mapcar (pcase-lambda (`(,value . ,name))
                                         `((:value . ,value) (:name . ,name)))
                                       models))))
               (and levels
                    `(((:id . "effort") (:category . "thought_level") (:current-value . ,level)
                       (:options . ,(mapcar (lambda (value)
                                              `((:value . ,value) (:name . ,(capitalize value))))
                                            levels)))))))))

(defun drover-agent-shell-test-emit (buffer event &rest data)
  "Emit EVENT in the shell of BUFFER with DATA, a plist turned into an alist."
  (with-current-buffer buffer
    (let (alist)
      (while data
        (push (cons (pop data) (pop data)) alist))
      (agent-shell--emit-event :event event :data (nreverse alist)))))

(defun drover-agent-shell-test-session (buffer)
  "Return the session the backend lists for the shell of BUFFER."
  (seq-find (lambda (session) (eq (drover-session-buffer session) buffer))
            (drover-agent-shell--sessions)))

(defun drover-agent-shell-test-subscriptions (buffer)
  "Return the event subscriptions of the shell of BUFFER."
  (with-current-buffer buffer
    (map-elt agent-shell--state :event-subscriptions)))

;;;; Loading

(ert-deftest drover-agent-shell-test-upstream-api ()
  "What the backend uses of agent-shell, private parts included, exists."
  (dolist (function '(agent-shell-buffers agent-shell-get-config agent-shell-initialized-p
                                          agent-shell-status agent-shell-last-activity-time agent-shell-subscribe-to
                                          agent-shell-unsubscribe agent-shell-insert agent-shell-prompt-queue
                                          agent-shell-interrupt agent-shell-get-model-name
                                          agent-shell-jump-to-latest-permission-button-row
                                          agent-shell--start agent-shell--display-buffer agent-shell--state
                                          agent-shell--config-option-set-mode-id agent-shell--config-option-set-model-id
                                          agent-shell--config-option-set-thought-level-id
                                          agent-shell--config-option-by-category agent-shell--current-model-id
                                          agent-shell--current-mode-id agent-shell--current-thought-level-id
                                          agent-shell--get-available-models agent-shell--get-available-thought-levels
                                          agent-shell--project-name agent-shell--stop-reason-description
                                          agent-shell--format-buffer-name
                                          agent-shell-viewport--buffer agent-shell-viewport--shell-buffer
                                          shell-maker-busy shell-maker-set-buffer-name shell-maker-last-output
                                          agent-shell-fork acp-send-request acp-make-session-delete-request
                                          agent-shell-viewport-edit-mode
                                          agent-shell--prompt-queue-display agent-shell--prompt-queue-process-next))
    (should (fboundp function)))
  ;; What its state tells of a session drover reads.
  (let ((state (agent-shell--make-state :agent-config nil :buffer nil)))
    (dolist (key '(:client :supports-session-fork :available-commands :pending-prompts))
      (should (assq key state))))
  (dolist (variable '(agent-shell--state agent-shell-viewport--suffix agent-shell-mode-hook
                                         agent-shell-viewport--compose-snapshot))
    (should (boundp variable)))
  (let ((usage (car (help-split-fundoc (documentation 'agent-shell--start t) 'agent-shell--start))))
    ;; Resuming starts a shell loading a session, in a directory.
    (dolist (key '("CONFIG" "NO-FOCUS" "NEW-SESSION" "SESSION-STRATEGY" "SESSION-ID" "CWD"))
      (should (string-match-p (concat "&key .*\\_<" key "\\_>") usage))))
  ;; The start effort relies on default config options, applied after the model.
  (should (assq :default-config-options (agent-shell-make-agent-config)))
  (dolist (maker (mapcar #'cdr drover-agent-shell-configs))
    (should (fboundp maker))))

(ert-deftest drover-agent-shell-test-registered-on-load ()
  "Loading the backend registers it and follows the shells already there."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex)))
      (with-temp-buffer
        (let ((drover--backends nil)
              (drover--engines nil)
              (drover--model-sources nil)
              (drover-agent-shell--gemini-registered nil)
              (agent-shell-mode-hook nil)
              (window-selection-change-functions nil)
              (window-buffer-change-functions nil)
              (after-focus-change-function #'ignore))
          (load "drover-agent-shell" nil t)
          ;; With it, the backends listing the conversations Claude Code
          ;; and Codex recorded, which its shells resume.
          (should (equal (sort (drover-backends) #'string<) '(agent-shell claude-code codex)))
          (should (equal (car (drover-resumer 'codex)) 'agent-shell))
          (should (equal (drover-backend-label 'agent-shell) "agent-shell"))
          (let ((session (drover-session-create :backend 'agent-shell :id "shell-0")))
            (dolist (operation '(:sessions :open :respond :interrupt :mark-seen :preview :recap :new
                                           :buffer :rename :create :apply-options :send :options :settable))
              (should (drover-supports-p session operation)))
            ;; Forking, compacting and deleting need a shell, which this
            ;; session has not.
            (dolist (operation '(:delete :fork :compact :supports))
              (should (functionp (drover-backend-property 'agent-shell operation))))
            (dolist (operation '(:delete :fork :compact))
              (should-not (drover-supports-p session operation)))
            ;; Drover archives its sessions: it has no archive of its own.
            (dolist (operation '(:archive :unarchive :archived :check :history))
              (should-not (drover-supports-p session operation))))
          (should (equal (drover-backend-engines 'agent-shell) '(opencode claude-code codex)))
          (should (equal (drover-engine-label 'gemini-cli) "Gemini CLI"))
          (should (equal (sort (mapcar (lambda (source) (cons (car source) (nth 1 source)))
                                       drover--model-sources)
                               (lambda (a b) (string< (car a) (car b))))
                         '((claude-code . -10) (codex . 0) (gemini-cli . 0) (opencode . 10))))
          (should (memq #'drover-agent-shell--setup agent-shell-mode-hook))
          (should (memq #'drover-agent-shell--window-changed window-selection-change-functions))
          (should (memq #'drover-agent-shell--window-changed window-buffer-change-functions))
          (should (advice-function-member-p #'drover-agent-shell--window-changed
                                            after-focus-change-function))
          (should (eq (buffer-local-value 'drover-session-at-point-function shell)
                      #'drover-agent-shell--buffer-session))
          (should (= (length (drover-agent-shell-test-subscriptions shell)) 1))
          ;; A shell that ran before drover followed it has no known age.
          (should-not (buffer-local-value 'drover-agent-shell--created shell))
          (drover-agent-shell-unload-function)
          (should-not (drover-agent-shell-test-subscriptions shell))
          (should-not (buffer-local-value 'drover-session-at-point-function shell))
          (should-not (drover-backends))
          (should-not drover--engines)
          (should-not drover--model-sources)
          (should-not agent-shell-mode-hook)
          (should-not window-selection-change-functions)
          (should-not window-buffer-change-functions)
          (should-not (advice-function-member-p #'drover-agent-shell--window-changed
                                                after-focus-change-function)))))))

(ert-deftest drover-agent-shell-test-leaves-no-global-state ()
  "Loading agent-shell in the tests leaves nothing of the backend behind."
  (should-not (assq 'agent-shell (default-value 'drover--backends)))
  (should-not (seq-find (lambda (source) (memq (nth 2 source) (mapcar #'caddr drover-agent-shell--model-sources)))
                        (default-value 'drover--model-sources)))
  (should-not (memq #'drover-agent-shell--setup (default-value 'agent-shell-mode-hook)))
  (should-not (memq #'drover-agent-shell--window-changed
                    (default-value 'window-selection-change-functions)))
  (should-not (memq #'drover-agent-shell--window-changed
                    (default-value 'window-buffer-change-functions)))
  (should-not (advice-function-member-p #'drover-agent-shell--window-changed
                                        (default-value 'after-focus-change-function))))

(ert-deftest drover-agent-shell-test-keeps-another-gemini-engine ()
  "A `gemini-cli' engine registered by another package is neither replaced nor removed."
  (let ((drover--engines nil)
        (drover--model-sources nil)
        (drover--backends nil)
        (drover--started nil)
        (drover--running nil)
        (drover--change-timer nil)
        (drover--changed-keys nil)
        (drover--changed-backends nil)
        (drover--changed-all nil)
        (drover--changed-applied nil)
        (drover--index nil)
        (drover-agent-shell--gemini-registered nil))
    (unwind-protect
        (progn
          (drover-register-engine 'gemini-cli :label "Gemini" :last-used #'ignore)
          (drover-agent-shell--register)
          (should (eq (drover-engine-property 'gemini-cli :last-used) #'ignore))
          (drover-agent-shell--unregister)
          (should (equal (drover-engine-label 'gemini-cli) "Gemini")))
      (when (timerp drover--change-timer)
        (cancel-timer drover--change-timer)))))

;;;; Starting shells

(defun drover-agent-shell-test--config (engine)
  "Return a fresh agent-shell configuration of ENGINE with every start key."
  (copy-tree `((:identifier . ,engine)
               (:default-model-id)
               (:default-session-mode-id)
               (:session-meta (claudeCode (options (effort . "high")))))))

(ert-deftest drover-agent-shell-test-start-config-agents ()
  "A new shell takes plan and build as session modes and other agents as session meta."
  (let* ((base (drover-agent-shell-test--config 'claude-code))
         (plan (drover-agent-shell-start-config base nil "plan"))
         (build (drover-agent-shell-start-config base nil "build"))
         (reviewer (drover-agent-shell-start-config base "opus" "reviewer")))
    (should (equal (funcall (map-elt plan :default-session-mode-id)) "plan"))
    (should (equal (funcall (map-elt build :default-session-mode-id)) "auto"))
    (should-not (map-nested-elt plan '(:session-meta claudeCode options agent)))
    (should-not (map-elt reviewer :default-session-mode-id))
    (should (equal (funcall (map-elt reviewer :default-model-id)) "opus"))
    (should (equal (map-nested-elt reviewer '(:session-meta claudeCode options agent)) "reviewer"))
    (should (equal (map-nested-elt reviewer '(:session-meta claudeCode options effort)) "high"))
    (should (equal base (drover-agent-shell-test--config 'claude-code))))
  (let ((opencode (drover-agent-shell-start-config (drover-agent-shell-test--config 'opencode)
                                                   "anthropic/claude-opus" "plan")))
    (should (equal (funcall (map-elt opencode :default-session-mode-id)) "plan"))
    (should (equal (funcall (map-elt opencode :default-model-id)) "anthropic/claude-opus")))
  (let ((codex (drover-agent-shell-test--config 'codex)))
    (should (eq (drover-agent-shell-start-config codex "" "plan") codex))
    (should (eq (drover-agent-shell-start-config codex nil "") codex))))

(ert-deftest drover-agent-shell-test-start-config-missing-keys ()
  "A configuration without the start keys gains them."
  (let* ((base '((:identifier . claude-code)))
         (config (drover-agent-shell-start-config base "sonnet" "reviewer")))
    (should (equal (funcall (map-elt config :default-model-id)) "sonnet"))
    (should (equal (map-nested-elt config '(:session-meta claudeCode options agent)) "reviewer"))
    (should (equal base '((:identifier . claude-code))))))

(ert-deftest drover-agent-shell-test-start-effort ()
  "The start effort is matched against the levels the shell offers when it asks."
  (drover-agent-shell-test-with
    (let* ((base '((:identifier . claude-code)
                   (:default-config-options . (lambda () '(("fast" . "on"))))))
           (config (drover-agent-shell--with-start-effort base '("xhigh" "high")))
           (options (map-elt config :default-config-options))
           (shell (drover-agent-shell-test-shell 'claude-code)))
      (should (eq (drover-agent-shell--with-start-effort base nil) base))
      (should (equal (map-elt base :default-config-options) '(lambda () '(("fast" . "on")))))
      (should (equal (funcall options) '(("fast" . "on"))))
      (with-current-buffer shell
        (should (equal (funcall options) '(("fast" . "on"))))
        (drover-agent-shell-test-options shell '("effort" "thought_level" "low" "low" "high"))
        (should (equal (funcall options) '(("fast" . "on") ("thought_level" . "high"))))
        (drover-agent-shell-test-options shell '("effort" "thought_level" "low" "low" "xhigh" "high"))
        (should (equal (funcall options) '(("fast" . "on") ("thought_level" . "xhigh")))))
      (should (equal (funcall (map-elt (drover-agent-shell--with-start-effort
                                        '((:identifier . codex)) '("high"))
                                       :default-config-options))
                     nil)))))

(defmacro drover-agent-shell-test-with-start (&rest body)
  "Run BODY with shells started as test shells, and test configurations.
`agent-shell--start' returns a new test shell, recording its arguments
and `default-directory'; `agent-shell--display-buffer' records its
argument."
  (declare (indent 0) (debug t))
  `(let ((drover-agent-shell-configs
          (mapcar (lambda (engine)
                    (cons engine (lambda () (drover-agent-shell-test--config engine))))
                  '(claude-code opencode codex)))
         (drover-agent-shell-engines '(opencode claude-code codex gemini-cli)))
     (cl-letf (((symbol-function 'agent-shell--start)
                (lambda (&rest arguments)
                  (push (cons 'agent-shell--start (cons default-directory arguments))
                        drover-agent-shell-test--calls)
                  (drover-agent-shell-test-shell
                   (map-elt (plist-get arguments :config) :identifier))))
               ((symbol-function 'agent-shell--display-buffer)
                (drover-agent-shell-test--recorder 'agent-shell--display-buffer)))
       ,@body)))

(defun drover-agent-shell-test--started-config ()
  "Return the configuration of the shell started last."
  (plist-get (cdar (last (drover-agent-shell-test--calls-of 'agent-shell--start))) :config))

(defun drover-agent-shell-test--create (directory engine options)
  "Start a shell of ENGINE in DIRECTORY with OPTIONS, and return (RESULT . CONFIG).
RESULT is (then SESSION) or (else MESSAGE); CONFIG is the configuration
the shell started with."
  (let (result)
    (drover-agent-shell--create directory engine options
                                (lambda (session) (setq result (list 'then session)))
                                (lambda (message) (setq result (list 'else message))))
    (cons result (drover-agent-shell-test--started-config))))

(ert-deftest drover-agent-shell-test-resume ()
  "A conversation resumes in a new shell of its engine, loading it by identifier."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let* ((directory (file-name-as-directory (make-temp-file "drover-resume" t)))
             (buffer (save-window-excursion
                       (drover-agent-shell-resume 'codex "019f-thread" directory)))
             (start (car (drover-agent-shell-test--calls-of 'agent-shell--start))))
        (unwind-protect
            (progn
              (should (equal (car start) directory))
              (should (equal (cdr start)
                             (list :config (drover-agent-shell-test--started-config) :no-focus t
                                   :new-session t :session-strategy 'new :session-id "019f-thread"
                                   :cwd directory)))
              (should (eq (map-elt (drover-agent-shell-test--started-config) :identifier) 'codex))
              (should (buffer-live-p buffer))
              ;; Every engine of a configuration resumes in a shell.
              (drover-agent-shell--register)
              (should (equal (mapcar (lambda (engine) (car (drover-resumer engine)))
                                     '(claude-code codex opencode))
                             '(agent-shell agent-shell agent-shell)))
              (should-error (drover-agent-shell-resume 'unknown "x" directory) :type 'user-error))
          (delete-directory directory t))))))

(ert-deftest drover-agent-shell-test-create ()
  "A new shell starts on the explicit model, agent and effort, displayed."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let* ((model (drover-model-create :id "opus" :engines '(claude-code)
                                         :variants '((high) (max))))
             (created (drover-agent-shell-test--create
                       "/work/drover" 'claude-code
                       (list :agent "plan" :model model :variant 'max :title "Fix the build")))
             (session (cadr (car created)))
             (config (cdr created))
             (start (car (drover-agent-shell-test--calls-of 'agent-shell--start)))
             (shell (drover-session-buffer session)))
        (should (eq (caar created) 'then))
        (should (equal (car start) (expand-file-name "/work/drover/")))
        (should (equal (cdr start) (list :config config :no-focus t :new-session t
                                         :session-strategy 'new)))
        (should (equal (drover-agent-shell-test--calls-of 'agent-shell--display-buffer)
                       (list (list shell))))
        (should (equal (funcall (map-elt config :default-model-id)) "opus"))
        (should (equal (funcall (map-elt config :default-session-mode-id)) "plan"))
        (drover-agent-shell-test-options shell '("effort" "thought_level" "high" "high" "max"))
        (with-current-buffer shell
          (should (equal (funcall (map-elt config :default-config-options))
                         '(("thought_level" . "max")))))
        (should (eq (drover-session-backend session) 'agent-shell))
        (should (eq (drover-session-engine session) 'claude-code))
        (should (equal (drover-session-title session) "Fix the build"))
        (should (equal (buffer-name shell) "Fix the build"))
        (should-not drover-agent-shell-test--warnings)
        ;; Hidden, as for another host: started, not displayed.
        (should (eq (car (car (drover-agent-shell-test--create "/work/drover" 'codex '(:hidden t)))) 'then))
        (should (= (length (drover-agent-shell-test--calls-of 'agent-shell--start)) 2))
        (should (= (length (drover-agent-shell-test--calls-of 'agent-shell--display-buffer)) 1))))))

(ert-deftest drover-agent-shell-test-create-opencode-inherits ()
  "A new OpenCode shell without a model starts on what the engine resolves."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let* ((pin (drover-model-create :provider-id "anthropic" :id "claude-opus"
                                       :engines '(opencode claude-code)
                                       :variants '((high :effort "high") (max :effort "max"))
                                       :default-variant 'high))
             (resolved nil))
        (drover-register-engine 'opencode
                                :resolve (lambda (defaults agent model variant)
                                           (push (list defaults agent model variant) resolved)
                                           (list :agent (or agent "build")
                                                 :model (plist-get defaults :model)
                                                 :variant (or variant (plist-get defaults :variant)))))
        (let* ((created (drover-agent-shell-test--create
                         "/work" 'opencode (list :agent "plan" :defaults '(:model nil))))
               (config (cdr created)))
          (should (eq (caar created) 'then))
          (should (equal resolved '(((:model nil) "plan" nil nil))))
          (should-not (map-elt config :default-model-id))
          (should-not (map-elt config :default-config-options)))
        (setq drover-agent-shell-test--calls nil)
        ;; The inherited model with its inherited variant.
        (let ((config (cdr (drover-agent-shell-test--create
                            "/work" 'opencode (list :defaults (list :model pin :variant 'max))))))
          (should (equal (funcall (map-elt config :default-model-id)) "anthropic/claude-opus"))
          (with-current-buffer (drover-agent-shell-test-shell 'opencode)
            (drover-agent-shell-test-options (current-buffer)
                                             '("variant" "thought_level" "high" "high" "max"))
            (should (equal (funcall (map-elt config :default-config-options))
                           '(("thought_level" . "max"))))))
        (setq drover-agent-shell-test--calls nil)
        ;; An explicit model without a variant starts on its default variant.
        (let ((config (cdr (drover-agent-shell-test--create
                            "/work" 'opencode (list :model pin :defaults '(:variant max))))))
          (should (equal (funcall (map-elt config :default-model-id)) "anthropic/claude-opus"))
          (with-current-buffer (drover-agent-shell-test-shell 'opencode)
            (drover-agent-shell-test-options (current-buffer)
                                             '("variant" "thought_level" "max" "high" "max"))
            (should (equal (funcall (map-elt config :default-config-options))
                           '(("thought_level" . "high"))))))))))

(ert-deftest drover-agent-shell-test-create-incompatible-model ()
  "A model the engine cannot run is reported and left out."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let ((config (cdr (drover-agent-shell-test--create
                          "/work" 'codex (list :model (drover-model-create :id "opus" :name "Opus"
                                                                           :engines '(claude-code)))))))
        (should-not (map-elt config :default-model-id))
        (should (equal drover-agent-shell-test--warnings
                       '("codex cannot run Opus; starting on its default model")))))))

(ert-deftest drover-agent-shell-test-create-failures ()
  "A shell that cannot start calls ELSE with why, never THEN."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let ((created (drover-agent-shell-test--create "/work" 'gemini-cli nil)))
        (should (equal (car created) '(else "No agent-shell configuration for Gemini CLI"))))
      (cl-letf (((symbol-function 'agent-shell--start)
                 (lambda (&rest _) (error "Not an existing directory: /nowhere"))))
        (should (equal (car (drover-agent-shell-test--create "/nowhere" 'codex nil))
                       '(else "Not an existing directory: /nowhere"))))
      (should-not (drover-agent-shell-test--calls-of 'agent-shell--display-buffer)))))

(ert-deftest drover-agent-shell-test-new ()
  "A shell started from a list asks for its engine among those that can start."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-start
      (let ((drover-agent-shell-engines '(codex gemini-cli)))
        (should (equal (drover-agent-shell--engines) '(codex)))
        (drover-agent-shell--new "/work/")
        (should (eq (map-elt (drover-agent-shell-test--started-config) :identifier) 'codex)))
      (setq drover-agent-shell-test--calls nil)
      (drover-register-engine 'claude-code :label "Claude Code")
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (setq offered (mapcar #'car candidates))
                     "Claude Code")))
          (drover-agent-shell--new "/work/"))
        (should (equal offered '("opencode" "Claude Code" "codex")))
        (should (eq (map-elt (drover-agent-shell-test--started-config) :identifier) 'claude-code)))
      (let ((drover-agent-shell-engines nil))
        (should-error (drover-agent-shell--new "/work/") :type 'user-error)))))

;;;; Sending

(defmacro drover-agent-shell-test-with-submit (&rest body)
  "Run BODY with the prompt queue and insertion recorded."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'agent-shell-prompt-queue)
              (lambda (text)
                (push (list 'agent-shell-prompt-queue (current-buffer) text)
                      drover-agent-shell-test--calls)))
             ((symbol-function 'agent-shell-insert)
              (drover-agent-shell-test--recorder 'agent-shell-insert)))
     ,@body))

(ert-deftest drover-agent-shell-test-send-waits-for-initialization ()
  "A prompt waits for the shell to start, then is queued while busy or submitted."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (dolist (busy '(nil t))
        (dolist (initialized '(nil t))
          (setq drover-agent-shell-test--calls nil)
          (let ((shell (drover-agent-shell-test-shell 'claude-code)))
            (with-current-buffer shell
              (setq-local shell-maker--busy busy))
            (when initialized
              (drover-agent-shell-test-initialize shell))
            (drover-agent-shell-send-text shell "hello")
            (unless initialized
              (should-not drover-agent-shell-test--calls)
              (should (= (length (drover-agent-shell-test-subscriptions shell)) 1))
              ;; Not before the default model and mode are set.
              (drover-agent-shell-test-emit shell 'prompt-ready)
              (should-not drover-agent-shell-test--calls)
              (drover-agent-shell-test-initialize shell)
              (drover-agent-shell-test-emit shell 'init-finished)
              (should-not (drover-agent-shell-test-subscriptions shell))
              ;; Once only.
              (drover-agent-shell-test-emit shell 'init-finished))
            (should (equal drover-agent-shell-test--calls
                           (if busy
                               (list (list 'agent-shell-prompt-queue shell "hello"))
                             (list (list 'agent-shell-insert :shell-buffer shell :text "hello"
                                         :submit t :no-focus t)))))
            (should-not drover-agent-shell-test--warnings)))))))

(ert-deftest drover-agent-shell-test-scheduled-prompt ()
  "A scheduled prompt waits for an initialized, idle shell with nothing typed, then is submitted."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let* ((shell (drover-agent-shell-test-shell 'codex "later"))
             (start nil)
             (session (with-current-buffer shell
                        (drover-agent-shell--setup t)
                        (drover-session-at-point)))
             (outcome nil))
        (cl-letf (((symbol-function 'agent-shell--prompt-input-start) (lambda () start)))
          (should (equal (drover-agent-shell--blocker session) "the shell has not initialized"))
          (drover-agent-shell-test-initialize shell)
          (with-current-buffer shell
            (setq-local shell-maker--busy t))
          (should (equal (drover-agent-shell--blocker session) "the agent is still busy"))
          (with-current-buffer shell
            (setq-local shell-maker--busy nil))
          (should (equal (drover-agent-shell--blocker session) "the shell shows no prompt"))
          (with-current-buffer shell
            (goto-char (point-max))
            (insert "> ")
            (setq start (point))
            (insert "a draft"))
          (should (equal (drover-agent-shell--blocker session) "the input holds a draft"))
          (with-current-buffer shell
            (delete-region start (point-max)))
          (should-not (drover-agent-shell--blocker session))
          (drover-agent-shell--deliver session "hello" (lambda () (setq outcome 'sent)) #'ignore)
          (should (eq outcome 'sent))
          (should (equal (drover-agent-shell-test--calls-of 'agent-shell-insert)
                         (list (list :shell-buffer shell :text "hello" :submit t :no-focus t))))
          (cl-letf (((symbol-function 'agent-shell-insert) (lambda (&rest _) (error "No prompt"))))
            (drover-agent-shell--deliver session "again" #'ignore (lambda (message) (setq outcome message))))
          (should (equal outcome "No prompt"))
          (kill-buffer shell)
          (should (equal (drover-agent-shell--blocker session) "the shell is gone")))))))

(ert-deftest drover-agent-shell-test-send-failures ()
  "A prompt a shell cannot take is reported as in the kill ring."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let ((failing (drover-agent-shell-test-shell 'codex "failing"))
            (closed (drover-agent-shell-test-shell 'codex "closed"))
            (cancelled (drover-agent-shell-test-shell 'claude-code "cancelled"))
            (dead (drover-agent-shell-test-shell 'codex "dead")))
        (drover-agent-shell-send-text failing "hello")
        (drover-agent-shell-test-emit failing 'error :code -32603 :message "Authentication required")
        (should-not (drover-agent-shell-test-subscriptions failing))
        (drover-agent-shell-send-text closed "hello")
        (drover-agent-shell-test-emit closed 'clean-up)
        (drover-agent-shell-send-text cancelled "hello")
        (drover-agent-shell-test-emit cancelled 'session-selection-cancelled)
        (should-not (drover-agent-shell-test-subscriptions cancelled))
        (kill-buffer dead)
        (drover-agent-shell-send-text dead "hello")
        (should-not drover-agent-shell-test--calls)
        (should (equal (reverse drover-agent-shell-test--warnings)
                       '("failing did not take the prompt (Authentication required); it is in the kill ring"
                         "closed did not take the prompt (it was closed); it is in the kill ring"
                         "cancelled did not take the prompt (its session choice was cancelled); it is in the kill ring"
                         "The agent shell did not take the prompt (its buffer is gone); it is in the kill ring")))))))

(ert-deftest drover-agent-shell-test-send-after-start-failure ()
  "A prompt for a shell whose start failed is reported at once."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let ((shell (drover-agent-shell-test-shell 'codex "failed")))
        (with-current-buffer shell
          (drover-agent-shell--setup))
        (drover-agent-shell-test-emit shell 'error :message "No codex-acp")
        (drover-agent-shell-send-text shell "hello")
        (should (equal drover-agent-shell-test--warnings
                       '("failed did not take the prompt (No codex-acp); it is in the kill ring")))
        ;; Only the shell's own subscription is left.
        (should (= (length (drover-agent-shell-test-subscriptions shell)) 1))))))

(ert-deftest drover-agent-shell-test-send-disconnected ()
  "A new prompt reaches the reconnecting submit path, not a dormant subscription."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let ((shell (drover-agent-shell-test-shell 'codex "disconnected")) reason)
        (with-current-buffer shell
          (setf (map-elt agent-shell--state :connection-error) "Adapter disconnected"))
        (drover-agent-shell--when-initialized shell
                                              (lambda () (ert-fail "Not connected"))
                                              (lambda (message) (setq reason message)))
        (should (equal reason "Adapter disconnected"))
        (drover-agent-shell-send-text shell "hello")
        (should-not (drover-agent-shell-test-subscriptions shell))
        (should (equal (drover-agent-shell-test--calls-of 'agent-shell-insert)
                       (list (list :shell-buffer shell :text "hello" :submit t :no-focus t))))))))

(ert-deftest drover-agent-shell-test-send-operation ()
  "The backend's :send submits to the session's shell, and reports a gone one."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let* ((shell (drover-agent-shell-test-shell 'codex "sending"))
             (session (progn (drover-agent-shell-test-initialize shell)
                             (drover-agent-shell-test-session shell))))
        (drover-backend-call session :send "hello" nil)
        (should (equal (drover-agent-shell-test--calls-of 'agent-shell-insert)
                       (list (list :shell-buffer shell :text "hello" :submit t :no-focus t))))
        (kill-buffer shell)
        (drover-backend-call session :send "again" nil)
        (should (equal drover-agent-shell-test--warnings
                       '("sending is gone; the prompt is in the kill ring")))))))

;;;; Options of running shells

(defmacro drover-agent-shell-test-with-setters (&rest body)
  "Run BODY with agent-shell's option setters recorded and succeeding.
Each records (SETTER VALUE) and calls its :on-success, unless the value
is in `drover-agent-shell-test--refused', when it calls :on-failure."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'agent-shell--config-option-set-mode-id)
              (lambda (&rest args) (drover-agent-shell-test--set 'mode (plist-get args :mode-id) args)))
             ((symbol-function 'agent-shell--config-option-set-model-id)
              (lambda (&rest args) (drover-agent-shell-test--set 'model (plist-get args :model-id) args)))
             ((symbol-function 'agent-shell--config-option-set-thought-level-id)
              (lambda (&rest args)
                (drover-agent-shell-test--set 'thought-level (plist-get args :thought-level-id) args))))
     ,@body))

(defvar drover-agent-shell-test--refused nil
  "Values the stubbed option setters refuse.")

(defvar drover-agent-shell-test--on-model nil
  "Function the stubbed model setter calls before succeeding, or nil.")

(defun drover-agent-shell-test--set (setter value args)
  "Record SETTER setting VALUE in the current buffer, then settle as ARGS say."
  (push (list setter (current-buffer) value) drover-agent-shell-test--calls)
  (if (member value drover-agent-shell-test--refused)
      (funcall (plist-get args :on-failure) '((code . -32602) (message . "Unknown value")) nil)
    (when (and (eq setter 'model) drover-agent-shell-test--on-model)
      (funcall drover-agent-shell-test--on-model))
    (funcall (plist-get args :on-success))))

(ert-deftest drover-agent-shell-test-option-steps-agents ()
  "A running Claude Code shell switches to plan or auto mode; other agents are reported."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-setters
      (let ((shell (drover-agent-shell-test-shell 'claude-code "claude")) done)
        (let ((steps (drover-agent-shell--option-steps shell 'claude-code "plan" nil nil nil nil)))
          (should (equal (mapcar #'car steps) '("agent plan")))
          (funcall (cdar steps) (lambda () (setq done t)))
          (should done))
        (let ((steps (drover-agent-shell--option-steps shell 'claude-code "build" nil nil nil nil)))
          (should (equal (mapcar #'car steps) '("agent build")))
          (funcall (cdar steps) #'ignore))
        (should (equal (drover-agent-shell-test--calls-of 'mode) `((,shell "plan") (,shell "auto"))))
        (should-not drover-agent-shell-test--warnings)
        (drover-run-steps (drover-agent-shell--option-steps shell 'claude-code "reviewer" nil nil nil nil) #'ignore)
        (drover-run-steps (drover-agent-shell--option-steps shell 'codex "plan" nil nil nil nil) #'ignore)
        (should (equal (reverse drover-agent-shell-test--warnings)
                       '("claude takes agent reviewer at session start"
                         "claude takes agent plan at session start")))
        (let ((steps (drover-agent-shell--option-steps shell 'opencode "review" nil nil nil nil)))
          (funcall (cdar steps) #'ignore)
          (should (equal (car (last (drover-agent-shell-test--calls-of 'mode))) (list shell "review"))))))))

(ert-deftest drover-agent-shell-test-apply-options-in-order ()
  "A running shell takes the agent, then the model, then the effort its model offers."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-setters
      (let* ((shell (drover-agent-shell-test-shell 'opencode "opencode"))
             (model (drover-model-create :provider-id "anthropic" :id "claude-opus"
                                         :engines '(opencode)
                                         :variants '((high :effort "high") (max :effort "max"))
                                         :default-variant 'max))
             (drover-agent-shell-test--on-model
              (lambda ()
                (drover-agent-shell-test-options
                 shell '("variant" "thought_level" "high" "high" "max"))))
             session sent)
        (drover-agent-shell-test-initialize shell "ses_1")
        (drover-agent-shell-test-options shell '("variant" "thought_level" "low" "low"))
        (setq session (drover-agent-shell-test-session shell))
        (drover-agent-shell--apply-options session (list :agent "plan" :model model)
                                           (lambda () (push 'sent sent)))
        (should (equal sent '(sent)))
        ;; The OpenCode adapter would pick the first effort: the default is set.
        (should (equal (reverse (mapcar (lambda (call) (list (car call) (nth 2 call)))
                                        drover-agent-shell-test--calls))
                       '((mode "plan") (model "anthropic/claude-opus") (thought-level "max"))))
        (should-not drover-agent-shell-test--warnings)))))

(ert-deftest drover-agent-shell-test-apply-options-variant-and-refusals ()
  "A lone variant sets the effort; refusals and missing levels are reported."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-setters
      (let* ((shell (drover-agent-shell-test-shell 'claude-code "claude"))
             (model (drover-model-create :id "opus" :engines '(claude-code)))
             (drover-agent-shell-test--refused '("opus"))
             session sent)
        (drover-agent-shell-test-initialize shell)
        (drover-agent-shell-test-options shell '("effort" "thought_level" "high" "low" "high"))
        (setq session (drover-agent-shell-test-session shell))
        ;; The current level is not set again.
        (drover-agent-shell--apply-options session '(:variant high) (lambda () (push 1 sent)))
        (should-not drover-agent-shell-test--calls)
        (drover-agent-shell--apply-options session '(:variant low) (lambda () (push 2 sent)))
        (should (equal (drover-agent-shell-test--calls-of 'thought-level) `((,shell "low"))))
        (drover-agent-shell--apply-options session '(:variant max) (lambda () (push 3 sent)))
        (drover-agent-shell--apply-options session (list :model model) (lambda () (push 4 sent)))
        (should (equal sent '(4 3 2 1)))
        (should (equal (reverse drover-agent-shell-test--warnings)
                       '("claude offers no thought level for max"
                         "claude refused model opus: Unknown value")))
        (let (failure)
          (drover-agent-shell--apply-options session (list :model model :variant 'low)
                                             (lambda () (ert-fail "Sent after a refused model"))
                                             (lambda (reason) (setq failure reason)))
          (should (string-match-p "refused model" failure)))
        ;; Nothing to apply.
        (drover-agent-shell--apply-options session nil (lambda () (push 5 sent)))
        (should (equal (car sent) 5))))))

(ert-deftest drover-agent-shell-test-apply-options-waits-for-initialization ()
  "Options go to a starting shell once its ACP session is initialized."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-setters
      (let* ((shell (drover-agent-shell-test-shell 'claude-code "starting"))
             (session (drover-agent-shell-test-session shell))
             (failing (drover-agent-shell-test-shell 'claude-code "failing"))
             sent)
        (drover-agent-shell--apply-options session '(:agent "plan") (lambda () (push 'sent sent)))
        (should-not drover-agent-shell-test--calls)
        (should-not sent)
        (drover-agent-shell-test-initialize shell)
        (drover-agent-shell-test-emit shell 'init-finished)
        (should (equal (drover-agent-shell-test--calls-of 'mode) `((,shell "plan"))))
        (should (equal sent '(sent)))
        ;; A shell that does not start still lets the prompt go to :send.
        (drover-agent-shell--apply-options (drover-agent-shell-test-session failing) '(:agent "plan")
                                           (lambda () (push 'failed sent)))
        (drover-agent-shell-test-emit failing 'error :message "No agent")
        (should (equal sent '(failed sent)))
        (should (= (length (drover-agent-shell-test--calls-of 'mode)) 1))))))

(ert-deftest drover-agent-shell-test-options-and-settable ()
  "A shell tells the model and variant it runs, and what it can change."
  (drover-agent-shell-test-with
    (let ((claude (drover-agent-shell-test-shell 'claude-code "claude"))
          (opencode (drover-agent-shell-test-shell 'opencode "opencode"))
          (codex (drover-agent-shell-test-shell 'codex "codex")))
      (drover-agent-shell-test-initialize claude)
      (drover-agent-shell-test-initialize opencode "ses_1")
      (drover-agent-shell-test-options claude
                                       '("model" "model" "opus" "opus" "sonnet")
                                       '("effort" "thought_level" "high" "low" "high"))
      (drover-agent-shell-test-options opencode '("model" "model" "anthropic/claude-opus" "anthropic/claude-opus"))
      (let ((options (drover-agent-shell--options (drover-agent-shell-test-session claude))))
        (should (eq (plist-get options :variant) 'high))
        ;; Without the CLI's catalog, the alias is the only name known.
        (should (equal (plist-get options :model)
                       (drover-model-create :provider-name "Anthropic" :id "opus" :name "Opus"
                                            :variants '((low) (high)) :engines '(claude-code)))))
      (let ((options (drover-agent-shell--options (drover-agent-shell-test-session opencode))))
        (should-not (plist-get options :variant))
        (should (equal (plist-get options :model)
                       (drover-model-create :provider-id "anthropic" :id "claude-opus"
                                            :name "Anthropic/Claude-Opus"
                                            :engines '(opencode claude-code)))))
      (should-not (drover-agent-shell--options (drover-agent-shell-test-session codex)))
      (should (equal (drover-agent-shell--settable (drover-agent-shell-test-session claude))
                     '(:agent :model :variant)))
      (should (equal (drover-agent-shell--settable (drover-agent-shell-test-session opencode))
                     '(:agent :model)))
      (drover-agent-shell-test-initialize codex)
      (drover-agent-shell-test-options codex '("effort" "thought_level" "low" "low"))
      (should (equal (drover-agent-shell--settable (drover-agent-shell-test-session codex))
                     '(:model :variant))))))

;;;; Models of live shells

(ert-deftest drover-agent-shell-test-live-models ()
  "Live shells offer the models of their engine, the one each runs with its thought levels."
  (drover-agent-shell-test-with
    (let ((claude (drover-agent-shell-test-shell 'claude-code))
          (other (drover-agent-shell-test-shell 'claude-code))
          (opencode (drover-agent-shell-test-shell 'opencode)))
      (drover-agent-shell-test-options claude
                                       '("model" "model" "opus" "opus" "sonnet")
                                       '("effort" "thought_level" "high" "low" "high"))
      (drover-agent-shell-test-options other '("model" "model" "opus" "opus" "haiku"))
      (drover-agent-shell-test-options opencode
                                       '("model" "model" "openai/gpt-6" "openai/gpt-6" "local/llama"))
      (let ((models (drover-agent-shell--live-models 'claude-code)))
        (should (equal (mapcar #'drover-model-id models) '("opus" "sonnet" "haiku")))
        (should (equal (drover-model-variants (car models)) '((low) (high))))
        (should (drover-model-variants-known (car models)))
        ;; Models no shell runs have unknown variants, not those of opus.
        (should-not (drover-model-variants (nth 1 models)))
        (should-not (drover-model-variants-known (nth 1 models)))
        (should-not (drover-model-variants-known (nth 2 models)))
        (should (equal (drover-model-engines (car models)) '(claude-code))))
      (let ((models (drover-agent-shell--opencode-models "/work")))
        (should (equal (mapcar #'drover-model-provider-id models) '("openai" "local")))
        (should (equal (mapcar #'drover-model-id models) '("gpt-6" "llama")))
        (should (drover-model-variants-known (car models)))
        (should-not (drover-model-variants-known (cadr models)))
        (should (equal (drover-model-engines (car models)) '(opencode codex)))
        (should (equal (drover-model-engines (cadr models)) '(opencode))))
      (should-not (drover-agent-shell--codex-models "/work"))
      (should-not (drover-agent-shell--gemini-cli-models "/work")))))

(ert-deftest drover-agent-shell-test-live-models-made-once ()
  "Shells listing the same models make them once, merged as each shell's would be."
  (drover-agent-shell-test-with
    (let ((shells (list (drover-agent-shell-test-shell 'claude-code)
                        (drover-agent-shell-test-shell 'claude-code)
                        (drover-agent-shell-test-shell 'claude-code)))
          (made 0))
      (dolist (shell (seq-take shells 2))
        (drover-agent-shell-test-options shell
                                         '("model" "model" "opus" "opus" "sonnet")
                                         '("effort" "thought_level" "high" "low" "high")))
      (drover-agent-shell-test-options (nth 2 shells) '("model" "model" "sonnet" "opus" "sonnet"))
      (let ((each (drover-model-merge-all
                   (mapcan (lambda (buffer)
                             (with-current-buffer buffer
                               (copy-sequence (drover-agent-shell--shell-models 'claude-code agent-shell--state))))
                           (drover-agent-shell--shells)))))
        (advice-add 'drover-claude-code-acp-models :before (lambda (&rest _) (cl-incf made))
                    '((name . drover-agent-shell-test-count)))
        (unwind-protect
            (progn
              (should (equal (drover-agent-shell--live-models 'claude-code) each))
              (should (= made 2))
              ;; Kept while models are, until they change.
              (setq made 0)
              (drover-keeping-models
                (drover-agent-shell--live-models 'claude-code)
                (drover-agent-shell--live-models 'claude-code)
                (should (= made 2))
                (drover-models-changed 'claude-code)
                (should (equal (drover-agent-shell--live-models 'claude-code) each))
                (should (= made 4))))
          (advice-remove 'drover-claude-code-acp-models 'drover-agent-shell-test-count))))))

(ert-deftest drover-agent-shell-test-claude-models-identified ()
  "A Claude shell's aliases are the catalog's models, each once, with its own levels."
  (drover-agent-shell-test-with
    (drover-test-write-claude-catalog)
    (let ((shell (drover-agent-shell-test-shell 'claude-code)))
      ;; As claude-agent-acp offers effort to a client without recommended
      ;; values: "default" first, which leaves the effort to the CLI.
      (drover-agent-shell-test-acp-models shell drover-test-claude-acp-models "default"
                                          '("default" "low" "medium" "high") "default")
      (let ((models (drover-agent-shell--claude-code-models "/work")))
        (should (equal (mapcar #'drover-model-id models)
                       '("claude-opus-5-5" "claude-opus-5-5[1m]" "claude-sonnet-5-5"
                         "claude-haiku-4-5-20251001")))
        (should (equal (mapcar (lambda (model) (drover-model-id-for model 'claude-code 'agent-shell)) models)
                       '("opus" "opus[1m]" "sonnet" "haiku")))
        ;; "default" is no level; Opus offers max, which the session, on
        ;; Sonnet, does not.
        (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                       '((low medium high xhigh max) (low medium high xhigh max) (low medium high) nil))))
      ;; The shell runs the default: Sonnet 5.5 at its default effort.
      (drover-agent-shell-test-initialize shell)
      (let ((options (drover-agent-shell--options (drover-agent-shell-test-session shell))))
        (should (equal (drover-model-id (plist-get options :model)) "claude-sonnet-5-5"))
        (should (equal (drover-model-id-for (plist-get options :model) 'claude-code 'agent-shell) "sonnet"))
        (should (eq (plist-get options :variant) 'medium))))))

(ert-deftest drover-agent-shell-test-default-effort-shown ()
  "A shell leaving the effort to its agent tells the model's default level, as a default.
Not the agent's own \"default\": the level the catalog, else what drover
learnt, gives the model; nothing when unknown."
  (drover-agent-shell-test-with
    (let ((claude (drover-agent-shell-test-shell 'claude-code))
          (gemini (drover-agent-shell-test-shell 'gemini-cli)))
      ;; Unknown: no catalog yet, nothing learnt.
      (drover-agent-shell-test-initialize claude "abc")
      (drover-agent-shell-test-acp-models claude drover-test-claude-acp-models "sonnet"
                                          '("default" "low" "medium" "high") "default")
      (let ((session (drover-agent-shell-test-session claude)))
        (should-not (drover-session-effort session))
        (should-not (drover-session-effort-default session)))
      ;; The catalog gives Sonnet 5.5 its default level.
      (drover-test-write-claude-catalog)
      (let ((session (drover-agent-shell-test-session claude)))
        (should (equal (drover-session-effort session) "medium"))
        (should (drover-session-effort-default session)))
      ;; A level chosen is no default, even the model's.
      (drover-agent-shell-test-acp-models claude drover-test-claude-acp-models "sonnet"
                                          '("default" "low" "medium" "high") "medium")
      (let ((session (drover-agent-shell-test-session claude)))
        (should (equal (drover-session-effort session) "medium"))
        (should-not (drover-session-effort-default session)))
      ;; An engine whose agent tells no default: what drover learnt of
      ;; the model, else nothing.
      (drover-agent-shell-test-initialize gemini "g1")
      (drover-agent-shell-test-acp-models gemini '(("gemini-3-pro" . "Gemini 3 Pro")) "gemini-3-pro"
                                          '("default" "low" "high") "default")
      (let ((session (drover-agent-shell-test-session gemini)))
        (should-not (drover-session-effort session))
        (should-not (drover-session-effort-default session)))
      (let ((drover-remember-models nil))
        (drover--remember-models 'gemini-cli
                                 (list (drover-model-create :id "gemini-3-pro" :name "Gemini 3 Pro"
                                                            :variants '((low) (high)) :default-variant 'high
                                                            :engines '(gemini-cli))))
        (let ((session (drover-agent-shell-test-session gemini)))
          (should (equal (drover-session-effort session) "high"))
          (should (drover-session-effort-default session)))))))

(ert-deftest drover-agent-shell-test-claude-alias-sent ()
  "A Claude model, however a source named it, reaches a shell by the alias the shell lists."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-setters
      (drover-agent-shell-test-with-start
        (drover-test-write-claude-catalog)
        (let ((drover-models-changed-hook nil)
              (shell (drover-agent-shell-test-shell 'claude-code "claude"))
              ;; As the CLI's catalog lists it, by identifier.
              (opus (car (drover-claude-code-catalog-models (drover-claude-code-catalog)))))
          (drover-agent-shell-test-acp-models shell drover-test-claude-acp-models "sonnet"
                                              '("default" "low" "medium" "high") "low")
          (drover-agent-shell-test-initialize shell)
          (with-current-buffer shell
            (drover-agent-shell--setup))
          (drover-agent-shell-test-emit shell 'init-session)
          ;; The picker offers the model the shell knows by its alias.
          (let ((offered (seq-find (lambda (model) (equal (drover-model-key model) (drover-model-key opus)))
                                   (drover-models "/work"))))
            (should (equal (drover-model-id-for offered 'claude-code 'agent-shell) "opus"))
            (drover-agent-shell--apply-options (drover-agent-shell-test-session shell)
                                               (list :model offered) #'ignore)
            (should (equal (drover-agent-shell-test--calls-of 'model) `((,shell "opus"))))
            ;; A new shell starts on it by that name.
            (let ((config (cdr (drover-agent-shell-test--create "/work" 'claude-code
                                                                (list :model offered)))))
              (should (equal (funcall (map-elt config :default-model-id)) "opus"))))
          ;; A model the shell does not list goes by its identifier.
          (let ((older (cadr (drover-claude-code-catalog-models (drover-claude-code-catalog)))))
            (should (equal (drover-model-id-for older 'claude-code 'agent-shell) "claude-opus-5"))))))))

(ert-deftest drover-agent-shell-test-variants-accumulate ()
  "Each model keeps the levels told while a shell ran it, once the shell switched."
  (drover-agent-shell-test-with
    (let ((drover-models-changed-hook nil)
          (shell (drover-agent-shell-test-shell 'gemini-cli))
          (models '(("gemini-pro" . "Gemini Pro") ("gemini-flash" . "Gemini Flash"))))
      (with-current-buffer shell
        (drover-agent-shell--setup))
      (drover-agent-shell-test-acp-models shell models "gemini-pro" '("low" "high") "high")
      (drover-agent-shell-test-emit shell 'init-session)
      (cl-flet ((variants ()
                  (mapcar (lambda (model)
                            (and (drover-model-variants-known model)
                                 (mapcar #'car (drover-model-variants model))))
                          (drover-models "/work"))))
        (should (equal (variants) '((low high) nil)))
        (drover-agent-shell-test-acp-models shell models "gemini-flash" '("minimal" "low") "low")
        (drover-agent-shell-test-emit shell 'config-option-update)
        (should (equal (variants) '((low high) (minimal low))))
        ;; The picker offers each model its own levels only.
        (let ((flash (cadr (drover-models "/work"))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt table &rest _)
                       (should (equal (all-completions "" table) '("Default" "minimal" "low")))
                       "minimal")))
            (should (eq (drover-read-variant flash) 'minimal))))))))

(ert-deftest drover-agent-shell-test-codex-models-per-model ()
  "A Codex shell's models take the levels its agent lists apart for each."
  (drover-agent-shell-test-with
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home) "{\"models\": []}")
    (let ((shell (drover-agent-shell-test-shell 'codex)))
      (drover-agent-shell-test-acp-models shell '(("gpt-5.5" . "5.5") ("gpt-6-astra" . "6 Astra"))
                                          "gpt-5.5" '("low" "medium") "medium")
      (with-current-buffer shell
        ;; codex-acp's session models: every model at every level.
        (map-put! (map-elt agent-shell--state :session) :models
                  (mapcar (lambda (value) `((:model-id . ,value) (:name . ,value)))
                          '("gpt-5.5[low]" "gpt-5.5[medium]" "gpt-6-astra[low]" "gpt-6-astra[max]"))))
      (let ((models (drover-agent-shell--codex-models "/work")))
        (should (equal (mapcar #'drover-model-id models) '("gpt-5.5" "gpt-6-astra")))
        (should (equal (mapcar #'drover-model-variants models) '(((low) (medium)) ((low) (max)))))
        (should (equal (drover-model-id-for (cadr models) 'codex 'agent-shell) "gpt-6-astra"))))))

(ert-deftest drover-agent-shell-test-model-source-depths ()
  "A Claude shell answers before other sources; an OpenCode shell after them."
  (drover-agent-shell-test-with
    (let ((claude (drover-agent-shell-test-shell 'claude-code))
          (opencode (drover-agent-shell-test-shell 'opencode)))
      (drover-agent-shell-test-options claude '("model" "model" "opus" "opus"))
      (drover-agent-shell-test-options opencode '("model" "model" "openai/gpt-6" "openai/gpt-6"))
      (drover-register-model-source 'claude-code (lambda (_) (list (drover-model-create :id "claude-api"))))
      (drover-register-model-source 'opencode (lambda (_) (list (drover-model-create :provider-id "x" :id "catalog"))))
      (should (equal (mapcar #'drover-model-id (drover-models "/work")) '("opus" "catalog")))
      (drover-agent-shell--unregister)
      (should (equal (mapcar #'drover-model-id (drover-models "/work")) '("claude-api" "catalog"))))))

(ert-deftest drover-agent-shell-test-model-sources-live ()
  "Every model source of shells is live: what it answers is remembered."
  (drover-agent-shell-test-with
    (should (equal (sort (mapcar (lambda (source) (list (car source) (nth 3 source)))
                                 drover--model-sources)
                         (lambda (a b) (string< (car a) (car b))))
                   '((claude-code t) (codex t) (gemini-cli t) (opencode t))))))

(ert-deftest drover-agent-shell-test-models-changed ()
  "A shell tells drover its models may have changed, as its session and options come."
  (drover-agent-shell-test-with
    (let* ((told nil)
           (timers nil)
           (drover-models-changed-hook (list (lambda (engine) (push engine told))))
           (shell (drover-agent-shell-test-shell 'codex)))
      (with-current-buffer shell
        (drover-agent-shell--setup))
      (should-not told)
      (dolist (event '(init-session init-model init-config-options config-option-update
                       session-selected session-restored))
        (drover-agent-shell-test-emit shell event)
        (should (equal told '(codex)))
        (setq told nil))
      (dolist (event '(agent-message-chunk turn-complete usage-update))
        (drover-agent-shell-test-emit shell event))
      (should-not told)
      ;; A shell drover follows late tells at once.
      (with-current-buffer shell
        (drover-agent-shell--setup t))
      (should (equal told '(codex)))
      (setq told nil)
      ;; A killed shell tells once it is gone.
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (push (cons function arguments) timers))))
        (kill-buffer shell))
      (should-not told)
      (pcase-dolist (`(,function . ,arguments) timers)
        (apply function arguments))
      (should (equal told '(codex))))))

(ert-deftest drover-agent-shell-test-models-remembered ()
  "A shell's models are remembered as it starts, and offered once it is gone."
  (drover-test-with-homes
    (drover-agent-shell-test-with
      (let ((drover-remember-models t)
            (drover--model-memory nil)
            (drover--model-memory-stamp 'unread)
            (drover-models-changed-hook nil)
            (shell (drover-agent-shell-test-shell 'claude-code)))
        (with-current-buffer shell
          (drover-agent-shell--setup))
        (drover-agent-shell-test-options shell
                                         '("model" "model" "opus" "opus" "sonnet")
                                         '("effort" "thought_level" "high" "low" "high"))
        (drover-agent-shell-test-emit shell 'init-session)
        (should (file-exists-p (drover-model-memory-file)))
        (cl-letf (((symbol-function 'run-at-time) #'ignore))
          (kill-buffer shell))
        ;; A new Emacs session, before any shell.
        (setq drover--model-memory nil
              drover--model-memory-stamp 'unread)
        (let ((models (drover-models "/work")))
          (should (equal (mapcar #'drover-model-id models) '("opus" "sonnet")))
          (should (equal (drover-model-variants (car models)) '((low) (high))))
          (should (equal (drover-model-engines (car models)) '(claude-code)))
          (should (cl-every #'drover-model-remembered models)))))))

(ert-deftest drover-agent-shell-test-picker-follows-shell-start ()
  "A model picker open as a shell starts offers the shell's models."
  (drover-agent-shell-test-with
    (let ((drover-models-changed-hook nil)
          (shell (drover-agent-shell-test-shell 'codex))
          (seen nil))
      (with-current-buffer shell
        (drover-agent-shell--setup))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (push (all-completions "" table) seen)
                   ;; The shell's session starts while the picker reads.
                   (drover-agent-shell-test-options shell '("model" "model" "gpt-6" "gpt-6" "gpt-6-mini"))
                   (drover-agent-shell-test-emit shell 'init-session)
                   (push (all-completions "" table) seen)
                   "OpenAI: Gpt-6-Mini")))
        (should (equal (drover-model-id (drover-read-model nil "/work")) "gpt-6-mini")))
      ;; Named by themselves capitalized, as `drover-agent-shell-test-options' does.
      (should (equal seen '(("Default" "OpenAI: Gpt-6" "OpenAI: Gpt-6-Mini") ("Default"))))
      (should-not drover-models-changed-hook))))

;;;; Sessions

(ert-deftest drover-agent-shell-test-session-fields ()
  "A shell's session tells its engine, ACP session, title, place, agent and model."
  (drover-agent-shell-test-with
    (let* ((directory (file-name-as-directory (make-temp-file "drover-shell" t)))
           (shell (drover-agent-shell-test-shell 'opencode "OpenCode @ drover")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" directory))
            (with-temp-file (expand-file-name ".git/HEAD" directory)
              (insert "ref: refs/heads/feature\n"))
            (with-current-buffer shell
              (setq default-directory directory))
            (let ((session (drover-agent-shell-test-session shell)))
              (should (equal (drover-session-title session) "OpenCode @ drover"))
              (should (eq (drover-session-engine session) 'opencode))
              (should-not (drover-session-engine-id session))
              (should (equal (drover-session-directory session) directory))
              (should (equal (drover-session-branch session) "feature"))
              (should (equal (drover-session-project session)
                             (file-name-nondirectory (directory-file-name directory)))))
            (drover-agent-shell-test-initialize shell "ses_abc")
            (drover-agent-shell-test-options shell
                                             '("mode" "mode" "plan" "build" "plan")
                                             '("model" "model" "anthropic/claude-opus" "anthropic/claude-opus")
                                             '("effort" "thought_level" "high" "low" "high"))
            (with-current-buffer shell
              (map-put! (map-elt agent-shell--state :session) :title "Fix\n  the build")
              (map-put! agent-shell--state :last-activity-time (seconds-to-time 1000)))
            (let ((session (drover-agent-shell-test-session shell)))
              (should (equal (drover-session-engine-id session) "ses_abc"))
              (should (equal (drover-session-engine-key session) '(opencode . "ses_abc")))
              (should (equal (drover-session-title session) "Fix the build"))
              (should (equal (drover-session-agent session) "plan"))
              (should (equal (drover-session-model session) "Anthropic/Claude-Opus"))
              ;; The thought level it runs, whose being the default the
              ;; shell does not tell, and the provider OpenCode names.
              (should (equal (drover-session-effort session) "high"))
              (should-not (drover-session-effort-default session))
              (should (equal (drover-session-provider-name session) "anthropic"))
              (should-not (drover-session-provider session))
              (should-not (drover-session-context-tokens session))
              ;; What the agent sent as it loaded the conversation is no
              ;; activity of the conversation: no turn ran here.
              (should-not (drover-session-updated session))
              ;; The identifier is a token of the buffer, stable while it lives.
              (should (equal (drover-session-id session)
                             (drover-session-id (drover-agent-shell-test-session shell))))
              (should (eq (drover-session-buffer session) shell))
              (should (eq (drover-backend-call session :buffer) shell))))
        (delete-directory directory t)))))

(ert-deftest drover-agent-shell-test-buffer-by-id ()
  "A session's shell is found by its identifier, without listing the shells."
  (drover-agent-shell-test-with
    (let* ((one (drover-agent-shell-test-shell 'opencode "one"))
           (two (drover-agent-shell-test-shell 'claude-code "two"))
           (sessions (mapcar #'drover-agent-shell-test-session (list one two)))
           (other (generate-new-buffer " not a shell"))
           (listed 0))
      (push other drover-agent-shell-test--buffers)
      (should (= (length (delq nil sessions)) 2))
      ;; A buffer carrying a shell's identifier is no shell.
      (with-current-buffer other
        (setq-local drover-agent-shell--id (drover-session-id (car sessions))))
      (bury-buffer one)
      (advice-add 'agent-shell-buffers :before (lambda () (cl-incf listed))
                  '((name . drover-agent-shell-test-count)))
      (unwind-protect
          (progn
            (should (eq (drover-session-buffer (car sessions)) one))
            (should (eq (drover-session-buffer (cadr sessions)) two))
            (should (= listed 0))
            ;; A shell without agent-shell's state is gone, and a killed one.
            (with-current-buffer two
              (setq-local agent-shell--state nil))
            (should-not (drover-session-buffer (cadr sessions)))
            (kill-buffer one)
            (should-not (drover-session-buffer (car sessions))))
        (advice-remove 'agent-shell-buffers 'drover-agent-shell-test-count)))))

(ert-deftest drover-agent-shell-test-resumed-conversation ()
  "A shell loading a conversation shows it as old as its newest event.
What the agent sends as the shell loads it is no activity of the
conversation; a turn is, from its prompt to its end."
  (drover-agent-shell-test-with
    (drover-codex-rollouts-install 'test)
    (unwind-protect
        (let* ((project (file-name-as-directory (expand-file-name "work" home)))
               (id "019f0000-0000-7000-8000-0000000000c1")
               (other "019f0000-0000-7000-8000-0000000000c2")
               (start 1790000000.0)
               (opened (+ start (* 14 86400)))
               (clock opened)
               (float-time (symbol-function 'float-time))
               (shell (drover-agent-shell-test-followed 'codex)))
          (drover-fixture-codex id project :start start)
          (drover-fixture-codex other project :start (+ start 86400))
          (drover-transcript-scan drover-codex-rollouts--index)
          (cl-letf (((symbol-function 'float-time)
                     (lambda (&optional time) (if time (funcall float-time time) clock))))
            (cl-flet ((session () (drover-agent-shell-test-session shell)))
              (with-current-buffer shell
                (setq drover-agent-shell--created opened)
                (map-put! (map-elt agent-shell--state :session) :id id)
                ;; The conversation replayed as it loads.
                (map-put! agent-shell--state :last-activity-time (seconds-to-time (+ opened 3))))
              (drover-agent-shell-test-emit shell 'agent-message-chunk :text-chunk "The export…")
              (should (eql (drover-session-created (session)) start))
              (should (eql (drover-session-updated (session)) (+ start 5)))
              ;; A turn.
              (setq clock (+ opened 60))
              (drover-agent-shell-test-emit shell 'input-submitted :prompt "Go on")
              (should (eql (drover-session-updated (session)) (+ opened 60)))
              (with-current-buffer shell
                (map-put! agent-shell--state :last-activity-time (seconds-to-time (+ opened 70))))
              (should (eql (drover-session-updated (session)) (+ opened 70)))
              (setq clock (+ opened 80))
              (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
              (should (eql (drover-session-updated (session)) (+ opened 80)))
              ;; Another conversation loaded is as old as it is.
              (setq clock (+ opened 900))
              (with-current-buffer shell
                (map-put! (map-elt agent-shell--state :session) :id other)
                (map-put! agent-shell--state :last-activity-time (seconds-to-time (+ opened 900))))
              (drover-agent-shell-test-emit shell 'session-selected)
              (should (eql (drover-session-updated (session)) (+ start 86405))))))
      (drover-codex-rollouts-uninstall 'test))))

(ert-deftest drover-agent-shell-test-session-runs ()
  "A shell of an engine naming models bare tells no provider; without levels, no effort.
A Claude model goes by its compact name, as in opencode.el's mode line."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex))
          (claude (drover-agent-shell-test-shell 'claude-code)))
      (drover-agent-shell-test-initialize shell "thr_1")
      (drover-agent-shell-test-options shell '("model" "model" "gpt-5.5" "gpt-5.5"))
      (let ((session (drover-agent-shell-test-session shell)))
        (should (equal (drover-session-model session) "Gpt-5.5"))
        (should-not (drover-session-provider-name session))
        (should-not (drover-session-effort session)))
      (drover-agent-shell-test-initialize claude "abc")
      (drover-agent-shell-test-options claude '("model" "model" "claude opus" "claude opus"))
      (should (equal (drover-session-model (drover-agent-shell-test-session claude)) "Opus")))))

(ert-deftest drover-agent-shell-test-recorded-run ()
  "A shell of Claude Code tells the provider, context and cache its transcript tells.
One of OpenCode keeps the provider it names, and has nothing recorded."
  (drover-agent-shell-test-with
      (drover-claude-code-transcripts-install 'test)
      (unwind-protect
          (let* ((project (file-name-as-directory (expand-file-name "work" home)))
                 (file (drover-fixture-claude "abc" project))
                 (claude (drover-agent-shell-test-shell 'claude-code)))
            (drover-fixture-claude-append
             file
             (drover-fixture-claude-user "abc" project 1790000100.0 "Go on")
             (drover-fixture-claude-assistant "abc" project 1790000101.0 "Done." :parent "abc-u1790000100"
                                              :usage (drover-fixture-claude-usage :input 10 :minutes 500
                                                                                  :read 9500)))
            (drover-transcript-scan drover-claude-code-transcripts--index)
            (drover-agent-shell-test-initialize claude "abc")
            (let ((session (drover-agent-shell-test-session claude)))
              (should (equal (list (drover-session-provider-name session) (drover-session-context-tokens session)
                                   (drover-session-cache-expires session) (drover-session-cache-ttl session))
                             (list "Anthropic" 10010 (+ 1790000100.0 300) 300))))
            (should (drover-backend-caches-p 'agent-shell)))
        (drover-claude-code-transcripts-uninstall 'test))))

(ert-deftest drover-agent-shell-test-project-cached ()
  "A shell looks its project up once per directory."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex))
          (lookups 0))
      (cl-letf (((symbol-function 'agent-shell--project-name)
                 (lambda () (cl-incf lookups) (concat "project of " default-directory))))
        (drover-agent-shell--sessions)
        (drover-agent-shell--sessions)
        (should (= lookups 1))
        (with-current-buffer shell
          (setq default-directory "/"))
        (should (equal (drover-session-project (drover-agent-shell-test-session shell)) "project of /"))
        (should (= lookups 2))))))

(ert-deftest drover-agent-shell-test-duplicate-opencode-session ()
  "A shell showing an OpenCode session the OpenCode backend lists is listed once."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'opencode)))
      (drover-agent-shell-test-initialize shell "ses_1")
      (drover-test-register-simple
       'opencode (list (drover-session-create :backend 'opencode :id "ses_1"
                                              :engine 'opencode :engine-id "ses_1")
                       (drover-session-create :backend 'opencode :id "ses_2"
                                              :engine 'opencode :engine-id "ses_2")))
      (let ((sessions (drover-sessions)))
        (should (seq-set-equal-p (mapcar #'drover-session-key sessions)
                                 (list (cons 'agent-shell (buffer-local-value 'drover-agent-shell--id shell))
                                       '(opencode . "ses_2"))))
        (should (eq (drover-session-backend (drover-find-engine-session 'opencode "ses_1"))
                    'agent-shell))))))

(ert-deftest drover-agent-shell-test-session-at-point ()
  "A followed shell tells its session; a shell no longer followed does not."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex)))
      (with-current-buffer shell
        (should-not (drover-session-at-point t))
        (drover-agent-shell--setup)
        (let ((session (drover-session-at-point)))
          (should (eq (drover-session-backend session) 'agent-shell))
          (should (equal (drover-session-key session)
                         (drover-session-key (drover-agent-shell-test-session shell))))
          (should (equal (drover-session-key (drover-buffer-session shell))
                         (drover-session-key session))))
        (drover-agent-shell--teardown)
        (should-not (drover-session-at-point t))
        (should-not (drover-agent-shell-test-subscriptions shell))))))

;;;; Statuses

(defun drover-agent-shell-test-status (buffer)
  "Return (STATUS DETAIL REQUESTS) of the session of the shell of BUFFER."
  (let ((session (drover-agent-shell-test-session buffer)))
    (list (drover-session-status session)
          (drover-session-detail session)
          (drover-session-requests session))))

(defun drover-agent-shell-test-followed (engine &optional name)
  "Return an initialized shell of ENGINE named NAME that drover follows."
  (let ((shell (drover-agent-shell-test-shell engine name)))
    (drover-agent-shell-test-initialize shell)
    (with-current-buffer shell
      (drover-agent-shell--setup))
    shell))

(ert-deftest drover-agent-shell-test-status-starting ()
  "A shell still starting works; one whose start failed says why."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex)))
      (with-current-buffer shell
        (drover-agent-shell--setup))
      (should (equal (drover-agent-shell-test-status shell) '(working "starting" nil)))
      (should (drover-session-since (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-emit shell 'error :message "No codex-acp")
      (should (equal (drover-agent-shell-test-status shell) '(failed "failed to start: No codex-acp" nil)))
      (drover-agent-shell--mark-seen (drover-agent-shell-test-session shell))
      (should (equal (drover-agent-shell-test-status shell) '(idle "failed to start: No codex-acp" nil)))
      ;; Initialization going on means it had not failed for good.
      (drover-agent-shell-test-emit shell 'init-handshake)
      (should (equal (drover-agent-shell-test-status shell) '(working "starting" nil))))))

(ert-deftest drover-agent-shell-test-status-start-cancelled ()
  "A shell left without a session by a cancelled choice is idle and says why."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let ((shell (drover-agent-shell-test-shell 'claude-code "abandoned"))
            (running (drover-agent-shell-test-followed 'claude-code "running")))
        (with-current-buffer shell
          (drover-agent-shell--setup))
        (should (equal (drover-agent-shell-test-status shell) '(working "starting" nil)))
        ;; agent-shell would kill the shell next; this one survives, as when a
        ;; query to save its transcript was quit.
        (drover-agent-shell-test-emit shell 'session-selection-cancelled)
        (should (equal (drover-agent-shell-test-status shell)
                       '(idle "failed to start: session choice cancelled" nil)))
        (should-not (drover-session-attention-p (drover-agent-shell-test-session shell)))
        ;; A prompt sent to it now is refused at once.
        (drover-agent-shell-send-text shell "hello")
        (should (equal drover-agent-shell-test--warnings
                       '("abandoned did not take the prompt (session choice cancelled); it is in the kill ring")))
        ;; A shell that started is not told it failed to, nor is its recap.
        (let ((before (drover-agent-shell-test-status running))
              recap)
          (drover-agent-shell-test-emit running 'session-selection-cancelled)
          (should (equal (drover-agent-shell-test-status running) before))
          (drover-recap (drover-agent-shell-test-session running) (lambda (value) (setq recap value)))
          (should-not (plist-get recap :error)))))))

(ert-deftest drover-agent-shell-test-status-disconnected ()
  "Seeing a disconnected shell cannot clear its transport failure."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code)))
      (with-current-buffer shell
        (setf (map-elt agent-shell--state :connection-error) "Adapter disconnected"
              (map-elt agent-shell--state :connection-since) 123.0
              (map-elt agent-shell--state :init-finished) nil))
      (drover-agent-shell-test-emit shell 'error :message "Adapter disconnected")
      (drover-agent-shell--mark-seen (drover-agent-shell-test-session shell))
      (should (equal (drover-agent-shell-test-status shell)
                     '(failed "Adapter disconnected" nil)))
      (should (= (drover-session-since (drover-agent-shell-test-session shell)) 123.0))
      (with-current-buffer shell
        (setf (map-elt agent-shell--state :connection-retrying) t))
      (drover-agent-shell-test-emit shell 'init-started)
      (should (equal (drover-agent-shell-test-status shell) '(working "starting" nil)))
      (with-current-buffer shell
        (setf (map-elt agent-shell--state :connection-error) nil
              (map-elt agent-shell--state :connection-retrying) nil
              (map-elt agent-shell--state :init-finished) t))
      (drover-agent-shell-test-emit shell 'init-finished)
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil))))))

(ert-deftest drover-agent-shell-test-status-working ()
  "A busy shell works since its prompt was submitted."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code)))
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil)))
      (with-current-buffer shell
        (setq-local shell-maker--busy t))
      (drover-agent-shell-test-emit shell 'input-submitted :prompt "hello")
      (should (equal (drover-agent-shell-test-status shell) '(working nil nil)))
      (should (equal (drover-session-since (drover-agent-shell-test-session shell))
                     (buffer-local-value 'drover-agent-shell--turn-start shell))))))

(ert-deftest drover-agent-shell-test-status-waiting ()
  "A shell whose agent waits for a permission or an answer waits, oldest first."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code)))
      (with-current-buffer shell
        (setq-local shell-maker--busy t)
        (map-put! agent-shell--state :tool-calls
                  (list (cons "tc1" (list (cons :title "Run ls") (cons :permission-request-id 7)))
                        (cons "tc2" (list (cons :title "Read the docs")))
                        (cons "tc3" (list (cons :title "Ask the user")))))
        (map-put! agent-shell--state :elicitations
                  (list (cons 9 (list (cons :id 9) (cons :message "Which branch?") (cons :status 'pending)))
                        (cons 10 (list (cons :id 10) (cons :tool-call-id "tc3") (cons :status 'pending)))
                        (cons 11 (list (cons :id 11) (cons :message "Done") (cons :status 'accepted))))))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 100.0)))
        (drover-agent-shell-test-emit shell 'elicitation-request :request-id 9 :message "Which branch?"))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 200.0)))
        (drover-agent-shell-test-emit shell 'permission-request :request-id 7))
      (should (equal (drover-agent-shell-test-status shell)
                     '(waiting nil ((:kind question :summary "Which branch?")
                                    (:kind permission :summary "Run ls")
                                    (:kind question :summary "Ask the user")))))
      (should (= (drover-session-since (drover-agent-shell-test-session shell)) 100.0))
      ;; Answered: the agent clears its own request.
      (with-current-buffer shell
        (map-put! agent-shell--state :tool-calls nil)
        (map-put! agent-shell--state :elicitations nil))
      (drover-agent-shell-test-emit shell 'permission-response :request-id 7)
      (drover-agent-shell-test-emit shell 'elicitation-response :request-id 9)
      (should-not (buffer-local-value 'drover-agent-shell--request-times shell))
      (should (equal (drover-agent-shell-test-status shell) '(working nil nil))))))

(ert-deftest drover-agent-shell-test-handed-back ()
  "What the user's commands give a shell hands its session the turn.
A prompt submitted, a permission or a form answered; not one cancelled
or dismissed, nor what runs out of a command, nor drover's own send."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code))
          (noted nil))
      (cl-letf (((symbol-function 'drover-note-handed-off)
                 (lambda (session how origin &optional buffer)
                   (push (list (eq (drover-session-buffer session) shell) how origin
                               (and buffer (buffer-name buffer)))
                         noted)))
                ((symbol-function 'agent-shell-insert)
                 (lambda (&rest _) (drover-agent-shell-test-emit shell 'input-submitted :prompt "sent"))))
        (save-window-excursion
          (set-window-buffer (selected-window) shell)
          ;; Out of a command: a queued prompt, a responder's answer.
          (let ((this-command nil))
            (drover-agent-shell-test-emit shell 'input-submitted :prompt "queued")
            (drover-agent-shell-test-emit shell 'permission-response :request-id 1 :option-id "allow"))
          (let ((this-command 'shell-maker-submit))
            (drover-agent-shell-test-emit shell 'input-submitted :prompt "typed")
            (drover-agent-shell--submit shell "sent")
            (drover-agent-shell-test-emit shell 'permission-response :request-id 2 :cancelled t)
            (drover-agent-shell-test-emit shell 'permission-response :request-id 3 :option-id "allow")
            (drover-agent-shell-test-emit shell 'elicitation-response :request-id 4 :action 'cancel)
            (drover-agent-shell-test-emit shell 'elicitation-response :request-id 5 :action 'accept)
            (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
            (drover-agent-shell-test-emit shell 'input-submitted :prompt "elsewhere"))))
      (should (equal (reverse noted)
                     `((t prompt front-end ,(buffer-name shell))
                       (t answer front-end ,(buffer-name shell))
                       (t answer front-end ,(buffer-name shell))
                       (t prompt front-end nil)))))))

(ert-deftest drover-agent-shell-test-status-ended-turns ()
  "A turn ending unseen is done or failed until seen; a cancelled one is not."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code)))
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
      (should (equal (drover-agent-shell-test-status shell) '(done nil nil)))
      (should (drover-session-since (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-emit shell 'input-submitted :prompt "again")
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil)))
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "max_tokens")
      (should (equal (drover-agent-shell-test-status shell) '(failed "Max token limit reached" nil)))
      (drover-agent-shell-test-emit shell 'input-submitted :prompt "again")
      (drover-agent-shell-test-emit shell 'error :code -32603 :message "Overloaded")
      (should (equal (drover-agent-shell-test-status shell) '(failed "Overloaded" nil)))
      ;; A failure is not hidden by a later success it was not seen before.
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
      (should (equal (drover-agent-shell-test-status shell) '(failed "Overloaded" nil)))
      (drover-agent-shell--mark-seen (drover-agent-shell-test-session shell))
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil)))
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "cancelled")
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil)))
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
      (drover-agent-shell-test-emit shell 'session-selected)
      (should (equal (drover-agent-shell-test-status shell) '(idle nil nil))))))

(defmacro drover-agent-shell-test-with-focus (buffer &rest body)
  "Run BODY with BUFFER in the selected window of a frame with the focus."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (set-window-buffer (selected-window) ,buffer)
     (cl-letf (((symbol-function 'frame-focus-state) (lambda (&optional _) t)))
       ,@body)))

(ert-deftest drover-agent-shell-test-status-seen-in-front ()
  "A turn ending in front of the user is seen; looking at a shell sees its turn."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code "front"))
          (viewport (generate-new-buffer "front [viewport]")))
      (push viewport drover-agent-shell-test--buffers)
      (drover-agent-shell-test-with-focus shell
        (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
        (should (equal (drover-agent-shell-test-status shell) '(idle nil nil))))
      ;; Shown, but in a frame without the focus.
      (save-window-excursion
        (set-window-buffer (selected-window) shell)
        (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
        (should (equal (drover-agent-shell-test-status shell) '(done nil nil)))
        (drover-agent-shell--window-changed)
        (should (equal (drover-agent-shell-test-status shell) '(done nil nil))))
      (drover-agent-shell-test-with-focus shell
        (drover-agent-shell--window-changed (selected-frame))
        (should (equal (drover-agent-shell-test-status shell) '(idle nil nil))))
      ;; Its viewport is the shell too.
      (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
      (with-current-buffer viewport
        (setq major-mode 'agent-shell-viewport-edit-mode))
      (drover-agent-shell-test-with-focus viewport
        (should (drover-agent-shell--focused-p shell))
        (drover-agent-shell--window-changed)
        (should (equal (drover-agent-shell-test-status shell) '(idle nil nil)))))))

(ert-deftest drover-agent-shell-test-changes-reported ()
  "A shell reports its events to drover, but not those streaming within a turn."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'codex))
          (changes 0))
      (cl-letf (((symbol-function 'drover-backend-changed)
                 (lambda (backend &optional _id)
                   (should (eq backend 'agent-shell))
                   (cl-incf changes))))
        (drover-agent-shell-test-emit shell 'input-submitted :prompt "hi")
        (should (= changes 1))
        (dolist (event '(agent-message-chunk tool-call-update usage-update file-write idle))
          (drover-agent-shell-test-emit shell event))
        (should (= changes 1))
        (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn")
        (should (= changes 2))
        (drover-agent-shell-test-emit shell 'config-option-update)
        (should (= changes 3))
        (drover-agent-shell--mark-seen (drover-agent-shell-test-session shell))
        (should (= changes 4))
        ;; Nothing to mark seen: no change.
        (drover-agent-shell--mark-seen (drover-agent-shell-test-session shell))
        (should (= changes 4))
        (kill-buffer shell)
        (should (= changes 5))))))

(ert-deftest drover-agent-shell-test-setup-replaces-subscription ()
  "Following a shell again keeps one subscription; new shells are followed from the hook."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'codex)))
      (with-current-buffer shell
        (drover-agent-shell--setup)
        (drover-agent-shell--setup))
      (should (= (length (drover-agent-shell-test-subscriptions shell)) 1))
      (should (buffer-local-value 'drover-agent-shell--created shell))
      (let ((agent-shell-mode-hook nil)
            (window-selection-change-functions nil)
            (window-buffer-change-functions nil)
            (after-focus-change-function #'ignore)
            new)
        (drover-agent-shell--install)
        (setq new (drover-agent-shell-test-shell 'codex))
        (should-not (drover-agent-shell-test-subscriptions new))
        (with-current-buffer new
          (run-hooks 'agent-shell-mode-hook))
        (should (= (length (drover-agent-shell-test-subscriptions new)) 1))
        (should (eq (buffer-local-value 'drover-session-at-point-function new)
                    #'drover-agent-shell--buffer-session))
        (drover-agent-shell--uninstall)
        (should-not (drover-agent-shell-test-subscriptions new))
        (should-not (drover-agent-shell-test-subscriptions shell))))))

;;;; Actions

(ert-deftest drover-agent-shell-test-actions ()
  "Opening, answering, interrupting and previewing act on the session's shell."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "acting"))
           (session (drover-agent-shell-test-session shell)))
      (save-window-excursion
        (cl-letf (((symbol-function 'agent-shell-jump-to-latest-permission-button-row)
                   (lambda () (push (list 'jump (current-buffer)) drover-agent-shell-test--calls)))
                  ((symbol-function 'agent-shell-interrupt)
                   (lambda (&optional force)
                     (push (list 'interrupt (current-buffer) force) drover-agent-shell-test--calls)))
                  ((symbol-function 'shell-maker-last-output)
                   (lambda () (propertize (concat "\n" (make-string 5000 ?x) "end\n") 'face 'bold))))
          (drover-open session)
          (should (eq (window-buffer (selected-window)) shell))
          ;; In another window, the one selected keeping its buffer.
          (delete-other-windows)
          (switch-to-buffer "*scratch*")
          (let ((origin (selected-window)))
            (drover-open session t)
            (should-not (eq (selected-window) origin))
            (should (eq (window-buffer (selected-window)) shell))
            (should (eq (window-buffer origin) (get-buffer "*scratch*")))
            (delete-other-windows origin))
          (switch-to-buffer "*scratch*")
          (drover-respond session)
          (should (eq (window-buffer (selected-window)) shell))
          (drover-backend-call session :interrupt)
          (should (equal (reverse drover-agent-shell-test--calls)
                         `((jump ,shell) (interrupt ,shell t))))
          (let (preview)
            (drover-preview session (lambda (text) (setq preview text)))
            (should (= (length preview) (1+ drover-agent-shell--preview-limit)))
            (should (string-prefix-p "…" preview))
            (should (string-suffix-p "xend" preview))
            (should-not (text-properties-at 1 preview))))
        (cl-letf (((symbol-function 'shell-maker-last-output) (lambda () "  \n")))
          (drover-preview session (lambda (text) (should-not text))))
        (kill-buffer shell)
        (should-error (drover-open session) :type 'user-error)
        (drover-preview session (lambda (text) (should-not text)))))))

(defun drover-agent-shell-test--until (predicate)
  "Run timers and processes until PREDICATE returns non-nil, a second at most."
  (let ((deadline (+ (float-time) 1)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))))

(defmacro drover-agent-shell-test-with-interrupt (shell &rest body)
  "Run BODY with SHELL busy and agent-shell's queue, interrupt and questions stubbed.
Interrupting is recorded; showing or sending the queue is recorded, as
`queue-display' and `queue-sent' with the queue then; any question
fails the test."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'agent-shell-interrupt)
              (lambda (&optional force)
                (push (list 'interrupt (current-buffer) force) drover-agent-shell-test--calls)))
             ((symbol-function 'agent-shell--prompt-queue-display)
              (lambda (&rest _)
                (push (list 'queue-display (map-elt agent-shell--state :pending-prompts))
                      drover-agent-shell-test--calls)))
             ((symbol-function 'agent-shell--prompt-queue-process-next)
              (lambda ()
                (push (list 'queue-sent (map-elt agent-shell--state :pending-prompts))
                      drover-agent-shell-test--calls)
                (map-put! agent-shell--state :pending-prompts nil)))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked")))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Asked"))))
     (with-current-buffer ,shell
       (setq-local shell-maker--busy t))
     ,@body))

(defun drover-agent-shell-test--queue (shell &optional prompts)
  "Return the prompts SHELL queued, after setting them to PROMPTS when given."
  (with-current-buffer shell
    (when prompts
      (map-put! agent-shell--state :pending-prompts prompts))
    (map-elt agent-shell--state :pending-prompts)))

(defun drover-agent-shell-test--turn-ends (shell reason)
  "End the turn of SHELL for REASON, as agent-shell does, asking nothing.
The event is emitted first, then agent-shell decides about its queue,
which drover set aside: it would ask with prompts queued, after a
cancelled turn."
  (drover-agent-shell-test-emit shell 'turn-complete :stop-reason reason)
  (with-current-buffer shell
    (setq-local shell-maker--busy nil)
    (when (and (equal reason "cancelled") (map-elt agent-shell--state :pending-prompts))
      (y-or-n-p "Pending prompts: continue? "))))

(ert-deftest drover-agent-shell-test-interrupt-asks-nothing-later ()
  "Interrupting a shell with prompts queued asks nothing once its turn ends: they wait, shown."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "interrupted"))
           (session (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-with-interrupt shell
        (drover-agent-shell-test--queue shell '("what's the status?"))
        (drover-backend-call session :interrupt)
        (should (equal (drover-agent-shell-test--calls-of 'interrupt) `((,shell t))))
        ;; Set aside, so that agent-shell finds nothing to ask about.
        (should-not (drover-agent-shell-test--queue shell))
        ;; Typed while the cancel goes, as in a mirror from another host.
        (drover-agent-shell-test--queue shell '("and then?"))
        ;; Kept if the shell closes before the turn ends.
        (should (member "what's the status?" (drover-agent-shell--unsent shell)))
        (drover-agent-shell-test--turn-ends shell "cancelled")
        (drover-agent-shell-test--until (lambda () (drover-agent-shell-test--queue shell)))
        (should (equal (drover-agent-shell-test--queue shell) '("what's the status?" "and then?")))
        (should (equal (drover-agent-shell-test--calls-of 'queue-display)
                       '((("what's the status?" "and then?")))))
        (should-not (drover-agent-shell-test--calls-of 'queue-sent))
        (with-current-buffer shell
          (should-not drover-agent-shell--held)
          (should-not drover-agent-shell--holding))))))

(ert-deftest drover-agent-shell-test-interrupt-too-late ()
  "A turn its agent ended before the interrupt took sends the prompts queued, as agent-shell would."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "finished"))
           (session (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-with-interrupt shell
        (drover-agent-shell-test--queue shell '("next"))
        (drover-backend-call session :interrupt)
        (drover-agent-shell-test--turn-ends shell "end_turn")
        (drover-agent-shell-test--until (lambda () (drover-agent-shell-test--calls-of 'queue-sent)))
        (should (equal (drover-agent-shell-test--calls-of 'queue-sent) '((("next")))))
        (should-not (drover-agent-shell-test--calls-of 'queue-display))))))

(ert-deftest drover-agent-shell-test-interrupt-failed ()
  "Prompts set aside come back as the turn's request fails, but not while a turn runs on."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "failing"))
           (session (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-with-interrupt shell
        (drover-agent-shell-test--queue shell '("later"))
        (drover-backend-call session :interrupt)
        ;; Another request failed: the turn runs on, the prompts stay aside.
        (drover-agent-shell-test-emit shell 'error :message "Setting the mode failed")
        (drover-agent-shell-test--until (lambda () nil))
        (should-not (drover-agent-shell-test--queue shell))
        (with-current-buffer shell
          (should drover-agent-shell--holding))
        ;; The turn's own request failed: it runs no more.
        (with-current-buffer shell
          (setq-local shell-maker--busy nil))
        (drover-agent-shell-test-emit shell 'error :message "Overloaded")
        (drover-agent-shell-test--until (lambda () (drover-agent-shell-test--queue shell)))
        (should (equal (drover-agent-shell-test--queue shell) '("later")))
        (should (equal (drover-agent-shell-test--calls-of 'queue-display) '((("later")))))))))

(ert-deftest drover-agent-shell-test-interrupt-then-closed ()
  "A shell closed while its interrupted turn ends keeps the prompts set aside in the kill ring."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "closing"))
           (session (drover-agent-shell-test-session shell))
           (kill-ring nil)
           (interprogram-cut-function nil))
      (drover-agent-shell-test-with-interrupt shell
        (drover-agent-shell-test--queue shell '("never sent"))
        (drover-backend-call session :interrupt)
        (drover-agent-shell-test-emit shell 'clean-up)
        (should (equal (car kill-ring) "never sent"))
        (with-current-buffer shell
          (should-not drover-agent-shell--held))))))

(ert-deftest drover-agent-shell-test-interrupt-idle ()
  "Interrupting a shell running no turn sets nothing aside."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "idle"))
           (session (drover-agent-shell-test-session shell)))
      (drover-agent-shell-test-with-interrupt shell
        (with-current-buffer shell
          (setq-local shell-maker--busy nil))
        (drover-agent-shell-test--queue shell '("queued"))
        (drover-backend-call session :interrupt)
        (should (equal (drover-agent-shell-test--queue shell) '("queued")))
        (with-current-buffer shell
          (should-not drover-agent-shell--holding))))))

(ert-deftest drover-agent-shell-test-fork ()
  "A shell whose agent forks is forked by agent-shell, in its own buffer."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-followed 'claude-code "forking")))
      (cl-letf (((symbol-function 'agent-shell-fork)
                 (lambda () (push (list 'fork (current-buffer)) drover-agent-shell-test--calls))))
        (should-not (drover-supports-p (drover-agent-shell-test-session shell) :fork))
        (should-error (drover-fork (drover-agent-shell-test-session shell)) :type 'user-error)
        (with-current-buffer shell
          (map-put! agent-shell--state :supports-session-fork t))
        (should (drover-supports-p (drover-agent-shell-test-session shell) :fork))
        (drover-fork (drover-agent-shell-test-session shell))
        (should (equal drover-agent-shell-test--calls `((fork ,shell))))))))

(ert-deftest drover-agent-shell-test-compact ()
  "A shell whose agent offers /compact is sent it, as a prompt."
  (drover-agent-shell-test-with
    (drover-agent-shell-test-with-submit
      (let ((shell (drover-agent-shell-test-followed 'claude-code "compacting")))
        (should-not (drover-supports-p (drover-agent-shell-test-session shell) :compact))
        (with-current-buffer shell
          (map-put! agent-shell--state :available-commands
                    '(((name . "context") (description . "Context")))))
        (should-not (drover-supports-p (drover-agent-shell-test-session shell) :compact))
        (with-current-buffer shell
          (map-put! agent-shell--state :available-commands
                    '(((name . "context") (description . "Context"))
                      ((name . "compact") (description . "Compact")))))
        (should (drover-supports-p (drover-agent-shell-test-session shell) :compact))
        (drover-compact (drover-agent-shell-test-session shell))
        (should (equal drover-agent-shell-test--calls
                       `((agent-shell-insert :shell-buffer ,shell :text "/compact" :submit t :no-focus t))))))))

(ert-deftest drover-agent-shell-test-delete ()
  "A session is deleted by its agent, then its shell closed; an agent refusing keeps it."
  (drover-agent-shell-test-with
    (let ((shell (drover-agent-shell-test-shell 'claude-code "deleting"))
          (requests nil))
      (with-current-buffer shell
        (map-put! agent-shell--state :client 'client))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest arguments) (push arguments requests))))
        ;; Not initialized: nothing to delete yet.
        (should-not (drover-supports-p (drover-agent-shell-test-session shell) :delete))
        (drover-agent-shell-test-initialize shell "acp-1")
        (with-current-buffer shell
          (drover-agent-shell--setup))
        (let ((session (drover-agent-shell-test-session shell)))
          (should (drover-supports-p session :delete))
          (drover-delete session)
          (let ((request (car requests)))
            (should (eq (plist-get request :client) 'client))
            (should (equal (plist-get request :request)
                           '((:method . "session/delete") (:params (sessionId . "acp-1")))))
            ;; Refused: the shell stays, and the refusal is told.
            (funcall (plist-get request :on-failure) '((code . -32601) (message . "Method not found")) nil)
            (should (buffer-live-p shell))
            (should (equal drover-agent-shell-test--warnings
                           '("The agent of deleting did not delete its session (Method not found); its shell stays")))
            ;; Deleted: the shell goes.
            (funcall (plist-get request :on-success) nil)
            (should-not (buffer-live-p shell))))))))

(ert-deftest drover-agent-shell-test-delete-codex-subagents ()
  "A Codex conversation deleted by its agent has its subagents' deleted then, and only it."
  (drover-agent-shell-test-with
    (let ((requests nil)
          (subagents nil))
      (cl-letf (((symbol-function 'acp-send-request)
                 (lambda (&rest arguments) (push arguments requests)))
                ((symbol-function 'drover-codex-rollouts-delete-subagents)
                 (lambda (id title) (push (list id title) subagents))))
        (dolist (engine '(codex claude-code))
          (let ((shell (drover-agent-shell-test-shell engine (format "deleting %s" engine))))
            (with-current-buffer shell
              (map-put! agent-shell--state :client 'client))
            (drover-agent-shell-test-initialize shell (format "%s-thread" engine))
            (with-current-buffer shell
              (drover-agent-shell--setup))
            (drover-delete (drover-agent-shell-test-session shell))
            ;; Not before the agent deleted the conversation.
            (should-not subagents)
            (funcall (plist-get (car requests) :on-success) nil)
            (should-not (buffer-live-p shell))
            (should (equal subagents (and (eq engine 'codex) '(("codex-thread" "deleting codex")))))
            (setq subagents nil)))))))

(defun drover-agent-shell-test-type-input (shell text)
  "Give SHELL a process, as shell-maker does, and type TEXT at its prompt."
  (with-current-buffer shell
    (let ((process (make-pipe-process :name "drover-test-shell" :buffer shell :noquery t)))
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (insert text))))

(ert-deftest drover-agent-shell-test-archive-closes-shell ()
  "Archiving closes an idle shell at once, without asking, keeping what its agent never got."
  (drover-agent-shell-test-with
    (drover-test-with-state
      (let ((editing (drover-agent-shell-test-followed 'claude-code "editing"))
            (viewing (drover-agent-shell-test-followed 'codex "viewing"))
            (kill-ring nil)
            (interprogram-cut-function nil)
            (messages nil))
        (drover-agent-shell-test-initialize editing "acp-editing")
        (drover-agent-shell-test-initialize viewing "acp-viewing")
        (drover-agent-shell-test-type-input editing "Typed, not sent")
        (with-current-buffer editing
          (map-put! agent-shell--state :pending-prompts '("Queued after a cancel"))
          ;; As agent-shell sets a shell up as it starts: it cleans up as
          ;; the shell goes, and asks about queued prompts, interactively.
          (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
          (add-hook 'kill-buffer-query-functions (lambda () (error "Asked")) nil t))
        (let ((composing (generate-new-buffer "editing [viewport]"))
              (reading (generate-new-buffer "viewing [viewport]")))
          (push composing drover-agent-shell-test--buffers)
          (push reading drover-agent-shell-test--buffers)
          (with-current-buffer composing
            (setq major-mode 'agent-shell-viewport-edit-mode)
            (insert "Composed in the viewport"))
          (with-current-buffer reading
            (setq major-mode 'agent-shell-viewport-view-mode)
            (insert "An answer read")
            (setq-local agent-shell-viewport--compose-snapshot
                        '((:content . "Composed, then put aside") (:location . 1))))
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest arguments)
                       (push (apply #'format-message format arguments) messages))))
            (drover-archive (drover-agent-shell-test-session editing))
            (should-not (buffer-live-p editing))
            (should-not (buffer-live-p composing))
            (should (equal kill-ring '("Typed, not sent\n\nQueued after a cancel\n\nComposed in the viewport")))
            (should (equal messages '("Archived editing; its unsent input is in the kill ring")))
            (drover-archive (drover-agent-shell-test-session viewing))
            (should-not (buffer-live-p viewing))
            (should (equal (car kill-ring) "Composed, then put aside"))))
        ;; Their shells gone, drover keeps them among the archived sessions.
        (let ((drover-show-archived t))
          (should (equal (sort (mapcar #'drover-session-engine-id (drover-archived-sessions)) #'string<)
                         '("acp-editing" "acp-viewing"))))))))

(ert-deftest drover-agent-shell-test-archive-closes-after-turn ()
  "A busy shell archived closes once its turn ended; restored first, it stays."
  (drover-agent-shell-test-with
    (drover-test-with-state
      (let ((busy (drover-agent-shell-test-followed 'claude-code "busy"))
            (restored (drover-agent-shell-test-followed 'claude-code "restored"))
            (messages nil))
        (drover-agent-shell-test-initialize busy "acp-busy")
        (drover-agent-shell-test-initialize restored "acp-restored")
        (dolist (shell (list busy restored))
          (with-current-buffer shell
            (setq-local shell-maker--busy t))
          (drover-agent-shell-test-emit shell 'input-submitted :prompt "Work"))
        (drover-changed-now)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest arguments)
                     (push (apply #'format-message format arguments) messages))))
          (drover-archive (drover-agent-shell-test-session busy))
          (should (equal messages '("Archived busy")))
          (drover-archive (drover-agent-shell-test-session restored))
          (drover-unarchive (drover-agent-shell-test-session restored))
          (should (buffer-live-p busy))
          (setq messages nil)
          ;; The turn ends: the buffer closes, without a word.
          (dolist (shell (list busy restored))
            (with-current-buffer shell
              (setq-local shell-maker--busy nil))
            (drover-agent-shell-test-emit shell 'turn-complete :stop-reason "end_turn"))
          (drover-changed-now))
        (should-not (buffer-live-p busy))
        (should-not messages)
        (should (buffer-live-p restored))
        (should-not drover--closing)))))

(ert-deftest drover-agent-shell-test-recap ()
  "A shell's recap is what its events told of its last turn."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code))
           (session (drover-agent-shell-test-session shell)))
      (cl-flet ((recap ()
                  (let (value)
                    (drover-recap session (lambda (recap) (setq value recap)))
                    value)))
        (should (equal (recap) '(:prompt nil :reply nil :changes nil :error nil)))
        (drover-agent-shell-test-emit shell 'input-submitted :prompt "Fix the build")
        (drover-agent-shell-test-emit shell 'agent-message-chunk :text-chunk "Let me ")
        (drover-agent-shell-test-emit shell 'agent-message-chunk :text-chunk "look.")
        (should (equal (plist-get (recap) :reply) "Let me look."))
        ;; A tool call parts two messages: the newest is the reply.
        (drover-agent-shell-test-emit shell 'tool-call-update
                                      :tool-call '((:status . "pending")
                                                   (:diffs ((:file . "/w/proposed.el")))))
        (drover-agent-shell-test-emit shell 'tool-call-update
                                      :tool-call '((:status . "completed")
                                                   (:diffs ((:file . "/w/a.el")) ((:file . "/w/b.el")))))
        (drover-agent-shell-test-emit shell 'file-write :path "/w/a.el")
        (drover-agent-shell-test-emit shell 'usage-update)
        (drover-agent-shell-test-emit shell 'agent-message-chunk :text-chunk "Fixed ")
        (drover-agent-shell-test-emit shell 'usage-update)
        (drover-agent-shell-test-emit shell 'agent-message-chunk :text-chunk "it.\n")
        (should (equal (recap) '(:prompt "Fix the build" :reply "Fixed it."
                                 :changes (("/w/b.el" nil nil) ("/w/a.el" nil nil)) :error nil)))
        (drover-agent-shell-test-emit shell 'error :message "Overloaded")
        (should (equal (plist-get (recap) :error) "Overloaded"))
        ;; A new prompt begins a new turn.
        (drover-agent-shell-test-emit shell 'input-submitted :prompt "Again")
        (should (equal (recap) '(:prompt "Again" :reply nil :changes nil :error nil)))
        (kill-buffer shell)
        (should (equal (recap) '(:failure "The shell is gone")))))))

(ert-deftest drover-agent-shell-test-respond-from-side-list ()
  "Answering from a list in a side window shows the shell where the user works."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'claude-code "asking"))
           (session (drover-agent-shell-test-session shell))
           (list (generate-new-buffer "list")))
      (push list drover-agent-shell-test--buffers)
      (save-window-excursion
        (delete-other-windows)
        (let* ((main (selected-window))
               (side (split-window)))
          (set-window-buffer side list)
          (select-window side)
          (cl-letf (((symbol-function 'agent-shell-jump-to-latest-permission-button-row) #'ignore))
            (with-current-buffer list
              (setq-local drover-main-window-function (lambda () main))
              (drover-respond session)))
          (should (eq (window-buffer main) shell))
          (should (eq (window-buffer side) list))
          (should (eq (selected-window) main)))))))

(ert-deftest drover-agent-shell-test-rename ()
  "Renaming names the shell, its viewport and its session."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'codex "old name"))
           (viewport (generate-new-buffer "old name [viewport]"))
           (session (drover-agent-shell-test-session shell)))
      (push viewport drover-agent-shell-test--buffers)
      (with-current-buffer shell
        (map-put! (map-elt agent-shell--state :session) :title "The agent's title"))
      (should (equal (drover-session-title (drover-agent-shell-test-session shell)) "The agent's title"))
      (drover-rename session "Fix the build")
      (should (equal (buffer-name shell) "Fix the build"))
      (should (equal (buffer-name viewport) "Fix the build [viewport]"))
      (should (eq (agent-shell-viewport--shell-buffer viewport) shell))
      (let ((renamed (drover-agent-shell-test-session shell)))
        (should (equal (drover-session-id renamed) (drover-session-id session)))
        (should (equal (drover-session-title renamed) "Fix the build"))))))

(defun drover-agent-shell-test-title (buffer title)
  "Title the session of the shell of BUFFER TITLE, as agent-shell does.
That is with the first prompt, then with what the agent tells."
  (with-current-buffer buffer
    (agent-shell--set-session-title title)))

(ert-deftest drover-agent-shell-test-named-after-title ()
  "A shell and its viewport are named after its session's title as it changes."
  (drover-agent-shell-test-with
    (let* ((shell (drover-agent-shell-test-followed 'codex "Codex Agent @ drover"))
           (viewport (generate-new-buffer "Codex Agent @ drover [viewport]"))
           (session (drover-agent-shell-test-session shell)))
      (push viewport drover-agent-shell-test--buffers)
      (drover-agent-shell-test-title shell "Fix the build")
      (should (equal (buffer-name shell) "Fix the build"))
      (should (equal (buffer-name viewport) "Fix the build [viewport]"))
      (should (eq (agent-shell-viewport--shell-buffer viewport) shell))
      (let ((renamed (drover-agent-shell-test-session shell)))
        (should (equal (drover-session-id renamed) (drover-session-id session)))
        (should (equal (drover-session-title renamed) "Fix the build")))
      ;; The agent's title replaces the first prompt's.
      (drover-agent-shell-test-title shell "Make the build pass")
      (should (equal (buffer-name shell) "Make the build pass"))
      (should (equal (buffer-name viewport) "Make the build pass [viewport]"))
      ;; A name another buffer has is made unique, and stays so.
      (push (generate-new-buffer "Release") drover-agent-shell-test--buffers)
      (drover-agent-shell-test-title shell "Release")
      (should (equal (buffer-name shell) "Release<2>"))
      (should (equal (buffer-name viewport) "Release<2> [viewport]"))
      (should (eq (agent-shell-viewport--shell-buffer viewport) shell))
      (cl-letf (((symbol-function 'shell-maker-set-buffer-name)
                 (drover-agent-shell-test--recorder 'shell-maker-set-buffer-name)))
        (drover-agent-shell-test-emit shell 'session-title-changed :title "Release"))
      (should-not (drover-agent-shell-test--calls-of 'shell-maker-set-buffer-name))
      (should (equal (buffer-name shell) "Release<2>")))))

(ert-deftest drover-agent-shell-test-named-after-long-title ()
  "A shell named after a title takes it on one line, cut to the limit.
A blank title leaves its name."
  (drover-agent-shell-test-with
    (let ((drover-agent-shell-name-limit 20)
          (shell (drover-agent-shell-test-followed 'codex "Codex Agent @ drover")))
      (drover-agent-shell-test-title shell "Look at\n  the failing tests of the list and fix them")
      (should (equal (buffer-name shell) "Look at the failing…"))
      (drover-agent-shell-test-title shell " \n ")
      (should (equal (buffer-name shell) "Look at the failing…")))))

(ert-deftest drover-agent-shell-test-named-by-user ()
  "A name the user gives the session or the buffer stays as the title changes."
  (drover-agent-shell-test-with
    (let ((renamed (drover-agent-shell-test-followed 'codex "Codex Agent @ drover"))
          (by-hand (drover-agent-shell-test-followed 'codex "Codex Agent @ drover<2>")))
      (drover-rename (drover-agent-shell-test-session renamed) "Release")
      (drover-agent-shell-test-title renamed "The agent's title")
      (should (equal (buffer-name renamed) "Release"))
      (should (equal (drover-session-title (drover-agent-shell-test-session renamed)) "Release"))
      (drover-agent-shell-test-title by-hand "First title")
      (should (equal (buffer-name by-hand) "First title"))
      (shell-maker-set-buffer-name by-hand "Mine")
      (drover-agent-shell-test-title by-hand "Second title")
      (should (equal (buffer-name by-hand) "Mine"))
      (should (equal (drover-session-title (drover-agent-shell-test-session by-hand)) "Second title")))))

(ert-deftest drover-agent-shell-test-not-named-after-title ()
  "With `drover-agent-shell-name-after-title' nil, a shell keeps agent-shell's name."
  (drover-agent-shell-test-with
    (let ((drover-agent-shell-name-after-title nil)
          (shell (drover-agent-shell-test-followed 'codex "Codex Agent @ drover")))
      (drover-agent-shell-test-title shell "Fix the build")
      (should (equal (buffer-name shell) "Codex Agent @ drover"))
      (should (equal (drover-session-title (drover-agent-shell-test-session shell)) "Fix the build")))))

(ert-deftest drover-agent-shell-test-named-after-title-late ()
  "A shell drover follows late is named after its title if agent-shell named it.
One the user named keeps its name."
  (drover-agent-shell-test-with
    (cl-letf (((symbol-function 'agent-shell--project-name) (lambda () "drover")))
      (let ((default (drover-agent-shell-test-shell 'codex "Codex Agent @ drover<2>"))
            (mine (drover-agent-shell-test-shell 'codex "Mine")))
        (dolist (shell (list default mine))
          (with-current-buffer shell
            (map-put! agent-shell--state :agent-config '((:identifier . codex) (:buffer-name . "Codex")))
            (map-put! (map-elt agent-shell--state :session) :title "Fix the build")
            (drover-agent-shell--setup t)))
        (should (equal (buffer-name default) "Fix the build"))
        (should (equal (buffer-name mine) "Mine"))
        (drover-agent-shell-test-title mine "Release")
        (should (equal (buffer-name mine) "Mine"))
        (drover-agent-shell-test-title default "Release")
        (should (equal (buffer-name default) "Release"))))))

;;;; The brief above the prompt

(defun drover-agent-shell-test-prompt (shell)
  "Give SHELL an answer and a live prompt awaiting input, as shell-maker writes them."
  (with-current-buffer shell
    (let ((process (or (get-buffer-process shell)
                       (make-pipe-process :name "drover-test-shell" :buffer shell :noquery t)))
          (inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize "An answer.\n\n" 'field 'output))
      (let ((start (point)))
        (insert (propertize "> " 'field 'output))
        (set-marker (process-mark process) (point))
        (setq-local comint-last-prompt (cons (copy-marker start) (copy-marker (point))))))))

(defun drover-agent-shell-test-brief-string (shell)
  "Return what SHELL shows of its brief above its prompt, or nil."
  (with-current-buffer shell
    (when (and (overlayp drover-agent-shell--brief-overlay)
               (overlay-buffer drover-agent-shell--brief-overlay))
      (substring-no-properties (overlay-get drover-agent-shell--brief-overlay 'before-string)))))

(ert-deftest drover-agent-shell-test-brief-above-prompt ()
  "An idle shell shows its session's brief just above its prompt, never while it works."
  (drover-agent-shell-test-with
    (let* ((brief "Done: fixed the build")
           (story nil)
           (drover-brief-function
            (lambda (record)
              (if (and brief (eq (drover-session-backend record) 'agent-shell))
                  (let ((copy (copy-sequence record)))
                    (setf (drover-session-brief copy) brief
                          (drover-session-brief-recap copy) "Fixed it.\nTests pass."
                          (drover-session-brief-session copy) story)
                    copy)
                record)))
           (drover-brief--buffers (make-hash-table :test #'eq :weakness 'key))
           (opencode-session-summary-icon "※")
           (shell (drover-agent-shell-test-shell 'claude-code)))
      (add-hook 'drover-changed-hook #'drover-brief--changed)
      (drover-agent-shell-test-initialize shell)
      (drover-agent-shell-test-prompt shell)
      (with-current-buffer shell
        (drover-agent-shell--setup))
      (drover-changed-now)
      (with-current-buffer shell
        (should (equal (drover-agent-shell-test-brief-string shell) "※ Done: fixed the build …\n\n"))
        (let ((overlay drover-agent-shell--brief-overlay))
          (should (= (overlay-start overlay) (marker-position (car comint-last-prompt))))
          ;; Not text: the prompt's line and the input are as they were.
          (should-not (string-search "Done:" (buffer-string)))
          (goto-char (point-max))
          (insert "next prompt")
          (should (eq drover-agent-shell--brief-overlay overlay))
          (should (equal (buffer-substring-no-properties (process-mark (get-buffer-process shell)) (point-max))
                         "next prompt"))
          ;; Line motion passes over it.
          (save-window-excursion
            (set-window-buffer (selected-window) shell)
            (set-window-start (selected-window) (point-min))
            (goto-char (point-max))
            (line-move -1 t)
            (should (< (point) (marker-position (car comint-last-prompt)))))
          ;; Unfolded on a click.
          (drover-agent-shell-toggle-brief-recap)
          (should (equal (drover-agent-shell-test-brief-string shell)
                         "※ Done: fixed the build\n  Fixed it.\n  Tests pass.\n\n"))))
      ;; What was written of the whole session shows whole instead.
      (setq story "Fixing the build: the flags are fixed; next, the tests.")
      (with-current-buffer shell (drover-agent-shell--changed))
      (drover-changed-now)
      (should (equal (drover-agent-shell-test-brief-string shell)
                     "※ Fixing the build: the flags are fixed; next, the tests.\n\n"))
      (setq story nil)
      (with-current-buffer shell (drover-agent-shell--changed))
      (drover-changed-now)
      ;; At work, it goes at once, whatever drover knows yet.
      (with-current-buffer shell
        (setq-local shell-maker--busy t)
        (drover-agent-shell--brief-place)
        (should-not (drover-agent-shell-test-brief-string shell))
        (setq-local shell-maker--busy nil)
        (drover-agent-shell--brief-place)
        (should (drover-agent-shell-test-brief-string shell)))
      ;; Without a brief any more, nothing shows.
      (setq brief nil)
      (with-current-buffer shell (drover-agent-shell--changed))
      (drover-changed-now)
      (should-not (drover-agent-shell-test-brief-string shell))
      ;; Unfollowed, the shell shows none.
      (setq brief "Done: again")
      (with-current-buffer shell (drover-agent-shell--changed))
      (drover-changed-now)
      (should (drover-agent-shell-test-brief-string shell))
      (with-current-buffer shell
        (drover-agent-shell--teardown))
      (should-not (drover-agent-shell-test-brief-string shell))
      (should-not (gethash shell drover-brief--buffers)))))

(ert-deftest drover-agent-shell-test-brief-after-request ()
  "A shell whose agent waits for a permission or an answer shows its brief at its end, after the request."
  (drover-agent-shell-test-with
    (let* ((drover-brief-function
            (lambda (record)
              (if (eq (drover-session-backend record) 'agent-shell)
                  (let ((copy (copy-sequence record)))
                    (setf (drover-session-brief copy) "Waiting: approval to run ls")
                    copy)
                record)))
           (drover-brief--buffers (make-hash-table :test #'eq :weakness 'key))
           (opencode-session-summary-icon "※")
           (shell (drover-agent-shell-test-shell 'claude-code)))
      (add-hook 'drover-changed-hook #'drover-brief--changed)
      (drover-agent-shell-test-initialize shell)
      (with-current-buffer shell
        (drover-agent-shell--setup)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "Run ls\n[Allow] [Reject]" 'field 'output)))
        (setq-local shell-maker--busy t)
        (map-put! agent-shell--state :tool-calls
                  (list (cons "tc1" (list (cons :title "Run ls") (cons :permission-request-id 7))))))
      (drover-changed-now)
      (with-current-buffer shell
        (should (eq (agent-shell-status) 'blocked))
        (should (equal (drover-agent-shell-test-brief-string shell) "\n\n※ Waiting: approval to run ls\n\n"))
        (should (= (overlay-start drover-agent-shell--brief-overlay) (point-max)))
        ;; Not text: the buffer is as it was.
        (should-not (string-search "Waiting:" (buffer-string)))
        ;; Output written meanwhile goes before it.
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "\nStill running.\n" 'field 'output)))
        (should (= (overlay-start drover-agent-shell--brief-overlay) (point-max)))
        (should (equal (drover-agent-shell-test-brief-string shell) "\n※ Waiting: approval to run ls\n\n"))
        ;; Answered, the agent works again: it goes at once.
        (map-put! agent-shell--state :tool-calls nil)
        (drover-agent-shell--brief-place)
        (should-not (drover-agent-shell-test-brief-string shell))))))

(provide 'drover-agent-shell-test)
;;; drover-agent-shell-test.el ends here
