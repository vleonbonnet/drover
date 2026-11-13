;;; drover-agent-shell.el --- agent-shell sessions in drover  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools

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

;; The `agent-shell' backend: the shells of agent-shell.el, each running
;; an engine through the Agent Client Protocol: `opencode',
;; `claude-code', `codex' or `gemini-cli', as the shell's configuration
;; names it.  A session's identifier is a token of its buffer, stable
;; while the buffer lives; its engine identifier is the ACP session's,
;; which for OpenCode is OpenCode's own session identifier, so a shell
;; showing an OpenCode session the OpenCode backend lists too is listed
;; once.
;;
;; A shell's buffer is named after its session's title as the title
;; changes, the agent's or, until the agent gives one, the first prompt
;; agent-shell titles a session with, until the user names the session
;; or the buffer (`drover-agent-shell-name-after-title').
;;
;; Statuses come from the shell and its events: a shell still starting
;; works, one whose start failed or whose session choice was cancelled
;; says why, one whose agent waits for a permission or an answer waits, a
;; busy one works, and a turn that ended or failed while the shell was
;; not in front of the user is done or failed until the user looks at
;; it, sends another prompt or marks it seen.  Every shell is subscribed
;; to its events from `agent-shell-mode-hook', which also tell the
;; recap of its last turn (:recap): the prompt submitted, the agent's
;; newest message since, and the files it wrote.
;;
;; The prompt starts shells set to start on a model and agent, sends to
;; them once they are initialized, and switches a running shell's
;; session mode, model and thought level through ACP before sending.  A
;; live shell also offers the models of its engine: a live source, which
;; `drover-model' remembers, told again whenever a shell's models may
;; have changed.  ACP tells the thought levels of the model a shell runs
;; only: the others take the levels their engine tells, else none known
;; (see `drover-agent-shell--models-of'), and a model is sent to a shell
;; by the name the shell lists it under, such as Claude Code's aliases.
;;
;; A shell's agent forks its session (agent-shell's own fork, ACP's
;; session/fork), compacts it (the command /compact, sent as a prompt)
;; and deletes it (ACP's session/delete, after which the shell closes),
;; as far as it advertises it can, which :supports tells.  agent-shell
;; has no archive: drover archives shells' sessions itself, closing their
;; shells once no turn runs (:close, :runs-in-buffer), what the agent
;; never got, typed, queued or composed, going to the kill ring.
;;
;; A shell resumes a conversation of its engine by identifier
;; (`drover-agent-shell-resume'), through ACP's session/load: drover
;; opens the closed conversations of Claude Code and Codex this way, as
;; the `claude-code' and `codex' backends list them from what the
;; engines record, installed with this one (see
;; `drover-claude-code-transcripts', `drover-codex-rollouts').
;;
;; A shell shows the brief of its session (see `drover-brief'), the
;; line a model wrote on where it stands, just above its prompt while it
;; awaits input, and at the end of its buffer, after the request, while
;; its agent waits for a permission or an answer, as opencode.el's
;; session buffers do: an overlay's string, at the start of the prompt's
;; line before the labels agent-shell's chat mode draws there, never
;; text.
;;
;; `drover' loads this file once agent-shell.el is loaded.

;;; Code:

(require 'cl-lib)
(require 'drover-brief)
(require 'drover-core)
(require 'drover-model)
(require 'drover-claude-code)
(require 'drover-claude-code-transcripts)
(require 'drover-codex)
(require 'drover-codex-rollouts)
(require 'map)
(require 'opencode-look)
(require 'seq)
(require 'subr-x)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 15))
  (load "drover-core" nil t))

;; Drover does not require agent-shell, so this file compiles without
;; it: what it uses of agent-shell and shell-maker is declared here, and
;; agent-shell is loaded only when the backend activates
;; (`drover-agent-shell--activate').  Its functions of keyword arguments
;; are declared without an arglist, which the byte compiler would check
;; as positional.
(defvar agent-shell--state)
(defvar comint-last-prompt)
(defvar agent-shell-viewport--compose-snapshot)
(defvar agent-shell-viewport--suffix)
(declare-function agent-shell--config-option-by-category "ext:agent-shell-config" (state category))
(declare-function agent-shell--config-option-set-mode-id "ext:agent-shell" t)
(declare-function agent-shell--config-option-set-model-id "ext:agent-shell" t)
(declare-function agent-shell--config-option-set-thought-level-id "ext:agent-shell" t)
(declare-function agent-shell--current-mode-id "ext:agent-shell-config" (state))
(declare-function agent-shell--current-model-id "ext:agent-shell-config" (state))
(declare-function agent-shell--current-thought-level-id "ext:agent-shell-config" (state))
(declare-function agent-shell--display-buffer "ext:agent-shell" (shell-buffer))
(declare-function agent-shell--format-buffer-name "ext:agent-shell" (agent-name project-name))
(declare-function agent-shell--get-available-models "ext:agent-shell-config" (state))
(declare-function agent-shell--get-available-thought-levels "ext:agent-shell-config" (state))
(declare-function agent-shell--project-name "ext:agent-shell-project" ())
(declare-function agent-shell--live-input-prompt-p "ext:agent-shell-prompt" (prompt))
(declare-function agent-shell--prompt-input-start "ext:agent-shell-prompt" ())
(declare-function agent-shell--prompt-queue-display "ext:agent-shell-prompt-queue" t)
(declare-function agent-shell--prompt-queue-process-next "ext:agent-shell-prompt-queue" ())
(declare-function agent-shell--start "ext:agent-shell" t)
(declare-function agent-shell--state "ext:agent-shell" ())
(declare-function agent-shell--stop-reason-description "ext:agent-shell" (stop-reason))
(declare-function agent-shell-buffers "ext:agent-shell" ())
(declare-function agent-shell-get-config "ext:agent-shell" (buffer))
(declare-function agent-shell-get-model-name "ext:agent-shell" (state))
(declare-function agent-shell-initialized-p "ext:agent-shell" t)
(declare-function agent-shell-insert "ext:agent-shell" t)
(declare-function agent-shell-interrupt "ext:agent-shell" (&optional force))
(declare-function agent-shell-jump-to-latest-permission-button-row "ext:agent-shell" ())
(declare-function agent-shell-last-activity-time "ext:agent-shell" t)
(declare-function agent-shell-prompt-queue "ext:agent-shell-prompt-queue" (prompt))
(declare-function agent-shell-status "ext:agent-shell" t)
(declare-function agent-shell-subscribe-to "ext:agent-shell" t)
(declare-function agent-shell-unsubscribe "ext:agent-shell" t)
(declare-function agent-shell-viewport--buffer "ext:agent-shell-viewport" t)
(declare-function agent-shell-viewport--shell-buffer "ext:agent-shell-viewport" (&optional viewport-buffer))
(declare-function acp-make-session-delete-request "ext:acp" t)
(declare-function acp-send-request "ext:acp" t)
(declare-function agent-shell-fork "ext:agent-shell" ())
(declare-function shell-maker-busy "ext:shell-maker" ())
(declare-function shell-maker-last-output "ext:shell-maker" ())
(declare-function shell-maker-set-buffer-name "ext:shell-maker" (buffer new-name))

;;;; Options

(defcustom drover-agent-shell-configs
  '((claude-code . agent-shell-anthropic-make-claude-code-config)
    (opencode . agent-shell-opencode-make-agent-config)
    (codex . agent-shell-openai-make-codex-config)
    (gemini-cli . agent-shell-google-make-gemini-config))
  "Functions making the agent-shell configuration of each engine.
Each entry is (ENGINE . FUNCTION), FUNCTION of no argument returning
the configuration a new shell running ENGINE starts from."
  :type '(alist :key-type (symbol :tag "Engine") :value-type (function :tag "Configuration"))
  :group 'drover)

(defcustom drover-agent-shell-engines '(opencode claude-code codex)
  "Engines whose sessions drover starts in agent-shell.
The prompt offers a new shell of each; `drover-new-session' asks which
when there are several."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-agent-shell-name-after-title t
  "Whether a shell's buffer is named after its session's title.
Non-nil names it after the title drover lists the session under as the
title changes, cut to `drover-agent-shell-name-limit' characters: the
agent's, or until the agent gives one the first prompt, which
agent-shell titles a session with.  It does until the user names the
session (`drover-rename') or the buffer.  Nil leaves the name
agent-shell gives a shell, such as \"Claude Agent @ PROJECT\"."
  :type 'boolean
  :group 'drover)

(defcustom drover-agent-shell-name-limit 60
  "Characters at most of a shell's buffer name taken from its session's title.
A longer title, such as a first prompt, is cut, ending in an ellipsis
\(see `drover-agent-shell-name-after-title')."
  :type 'natnum
  :group 'drover)

(defconst drover-agent-shell--preview-limit 4000
  "Characters of a shell's last output a preview shows at most.")

;;;; What drover keeps of a shell

(defvar drover-agent-shell--counter 0
  "Count of the identifiers given to shells.")

(defvar-local drover-agent-shell--id nil
  "Identifier of this shell's session, given when it is first listed.")

(defvar-local drover-agent-shell--title nil
  "Title the user gave this shell's session, or nil.")

(defvar-local drover-agent-shell--given-name nil
  "Name agent-shell or drover last gave this shell's buffer, or nil.
The buffer is named after its session's title while it keeps that name;
nil for a shell drover followed late under a name of the user's.")

(defvar-local drover-agent-shell--project nil
  "Project of this shell, as (DIRECTORY . NAME), once looked up.")

(defvar-local drover-agent-shell--created nil
  "When this shell was created, as a float time, or nil when unknown.")

(defvar-local drover-agent-shell--turn-start nil
  "When the running turn of this shell was submitted, as a float time.")

(defvar-local drover-agent-shell--in-turn nil
  "Non-nil from a prompt this shell submitted until its turn ends.")

(defvar-local drover-agent-shell--activity nil
  "When a turn of this shell's conversation last did something here, or nil.
A float time: a prompt submitted, a request to the user, a turn ending.
Not what the agent sends as the shell loads a conversation again, which
the conversation did not do now.")

(defvar-local drover-agent-shell--outcome nil
  "How the last turn of this shell ended while the user was elsewhere, or nil.
A plist: :status `done' or `failed', :detail a string or nil, :time a
float time.  Nil once seen.")

(defvar-local drover-agent-shell--start-error nil
  "Why this shell failed to start, or nil.")

(defvar-local drover-agent-shell--request-times nil
  "When this shell's pending requests arrived.
An alist (REQUEST-ID . TIME), TIME a float time.")

(defvar-local drover-agent-shell--subscription nil
  "Token of this shell's subscription to its events.")

(defvar-local drover-agent-shell--prompt nil
  "The prompt this shell submitted last, or nil before one.")

(defvar-local drover-agent-shell--reply nil
  "The text of the agent's newest message since the last prompt, or nil.")

(defvar-local drover-agent-shell--reply-open nil
  "Non-nil while the agent's newest message streams.
A text chunk then continues it; after a tool call or a request to the
user, a chunk begins a new message.")

(defvar-local drover-agent-shell--written nil
  "Files the agent wrote since the last prompt, newest first.")

(defvar-local drover-agent-shell--holding nil
  "Non-nil from drover interrupting this shell's turn until that turn ends.
The prompts the shell queues meanwhile are set aside as the turn ends
\(see `drover-agent-shell--hold').")

(defvar-local drover-agent-shell--held nil
  "The prompts this shell queued, set aside as drover interrupted its turn.
Oldest first; they are queued again once the turn ended.")

;;;; Shells

(defun drover-agent-shell--live-p (buffer)
  "Return non-nil when BUFFER is a live shell, as agent-shell made it."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'agent-shell-mode) agent-shell--state
              (bound-and-true-p shell-maker--config) t))))

(defun drover-agent-shell--shells ()
  "Return the live agent-shell buffers, most recently used first."
  (seq-filter #'drover-agent-shell--live-p (agent-shell-buffers)))

(defun drover-agent-shell--engine (&optional buffer)
  "Return the engine of the shell of BUFFER, the current one by default.
It is the :identifier of the shell's configuration, such as
`claude-code'."
  (map-elt (agent-shell-get-config (or buffer (current-buffer))) :identifier))

(defun drover-agent-shell--shell-of (buffer)
  "Return the shell BUFFER shows, itself or the shell of its viewport, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond ((derived-mode-p 'agent-shell-mode) buffer)
            ((derived-mode-p 'agent-shell-viewport-view-mode 'agent-shell-viewport-edit-mode)
             (agent-shell-viewport--shell-buffer buffer))))))

(defun drover-agent-shell--initialized-p (buffer)
  "Return non-nil when the shell of BUFFER finished initializing."
  (ignore-errors (agent-shell-initialized-p :shell-buffer buffer)))

;;;; Starting shells

(defun drover-agent-shell-start-config (base-config model-id agent)
  "Return agent-shell configuration BASE-CONFIG set to start on MODEL-ID as AGENT.
MODEL-ID is the identifier the engine takes, or nil; AGENT an agent
name, or nil.  OpenCode takes AGENT as its session mode; Claude Code
too when `drover-claude-code-agent-modes' runs it as a permission mode,
else as a session-start option; other engines ignore it.  BASE-CONFIG
is not modified, and returned as is when there is nothing to set."
  (let* ((model-id (and model-id (not (string-empty-p model-id)) model-id))
         (agent (and agent (not (string-empty-p agent)) agent))
         (engine (map-elt base-config :identifier))
         (mode (pcase engine
                 ('opencode agent)
                 ('claude-code (drover-claude-code-agent-mode agent))))
         (claude-agent (and (eq engine 'claude-code) (not mode) agent)))
    (if (not (or model-id mode claude-agent))
        base-config
      (let ((config (copy-tree base-config)))
        ;; `setf' rather than `map-put!': a configuration may lack the key.
        (when model-id
          (setf (alist-get :default-model-id config) (lambda () model-id)))
        (when mode
          (setf (alist-get :default-session-mode-id config) (lambda () mode)))
        (when claude-agent
          (let* ((meta (copy-tree (alist-get :session-meta config)))
                 (claude-code (alist-get 'claudeCode meta))
                 (options (alist-get 'options claude-code)))
            (setf (alist-get 'agent options) claude-agent
                  (alist-get 'options claude-code) options
                  (alist-get 'claudeCode meta) claude-code
                  (alist-get :session-meta config) meta)))
        config))))

(defun drover-agent-shell--find-thought-level (levels candidates)
  "Return the value of the first of LEVELS a string of CANDIDATES names, or nil.
LEVELS are the values of a thought level option, each with :value;
CANDIDATES are tried in order."
  (seq-some (lambda (candidate)
              (when-let* ((level (seq-find (lambda (level) (equal candidate (map-elt level :value)))
                                           levels)))
                (map-elt level :value)))
            candidates))

(defun drover-agent-shell--thought-level (state candidates)
  "Return (OPTION . TARGET) for the thought level CANDIDATES name in STATE.
OPTION is the session's thought level option, nil when the agent
advertises none; TARGET the value of its first level a string of
CANDIDATES names, or nil."
  (let ((option (agent-shell--config-option-by-category state "thought_level")))
    (cons option
          (and option (drover-agent-shell--find-thought-level (map-elt option :options) candidates)))))

(defun drover-agent-shell--with-start-effort (config candidates)
  "Return agent-shell CONFIG also setting the thought level CANDIDATES name.
CANDIDATES are effort strings, as `drover-model-effort-candidates'
returns them.  agent-shell applies default config options after the
default model, which brings its own thought levels, so the level is
matched against those when the shell asks; nothing is set when none
matches.  CONFIG is not modified; its own default config options come
first."
  (if (null candidates)
      config
    (let ((config (copy-tree config))
          (base (map-elt config :default-config-options)))
      (setf (alist-get :default-config-options config)
            (lambda ()
              (append (and base (funcall base))
                      (when-let* (((derived-mode-p 'agent-shell-mode))
                                  (level (cdr (drover-agent-shell--thought-level
                                               (agent-shell--state) candidates))))
                        (list (cons "thought_level" level))))))
      config)))

(defun drover-agent-shell--start-config (engine options)
  "Return the configuration of a new shell of ENGINE starting with OPTIONS.
OPTIONS is a start-options plist (see `drover-model').  Without an
explicit model, an OpenCode shell starts on the model and variant a new
OpenCode session would run, those of the prompt's defaults, as its ACP
adapter does not know them; with one, on its default variant, as the
adapter otherwise picks the first effort it offers.  A model ENGINE
cannot run is reported and left out."
  (let* ((maker (alist-get engine drover-agent-shell-configs))
         (agent (let ((agent (plist-get options :agent)))
                  (and agent (not (string-empty-p agent)) agent)))
         (explicit (plist-get options :model))
         (requested (plist-get options :variant))
         (inherited (and (not explicit) (eq engine 'opencode)
                         (drover-engine-resolve 'opencode (plist-get options :defaults)
                                                agent nil requested)))
         (model (or explicit (plist-get inherited :model)))
         (variant (or requested
                      (plist-get inherited :variant)
                      (and model (eq engine 'opencode) (drover-model-default-variant model))))
         (model-id (and model (drover-model-id-for model engine 'agent-shell))))
    (unless (functionp maker)
      (error "No agent-shell configuration for %s" (drover-engine-label engine)))
    (when (and model (not model-id))
      (drover-warn "%s cannot run %s; starting on its default model"
                   (drover-engine-label engine) (drover-model-label model)))
    (drover-agent-shell--with-start-effort
     (drover-agent-shell-start-config (funcall maker) model-id agent)
     (drover-model-effort-candidates model variant))))

(defun drover-agent-shell--start (directory config &optional hidden)
  "Start a shell of agent-shell CONFIG for DIRECTORY, display it and return it.
The shell's directory is the one `agent-shell-cwd' finds from
DIRECTORY, as when the user starts a shell there.  HIDDEN non-nil
leaves it undisplayed."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (buffer (agent-shell--start :config config :no-focus t :new-session t
                                     :session-strategy 'new)))
    (unless hidden
      (agent-shell--display-buffer buffer))
    buffer))

(defun drover-agent-shell--create (directory engine options then else)
  "Start and display a shell of ENGINE in DIRECTORY with start OPTIONS.
Call THEN with its session at once, :send waiting for it to start, or
ELSE with a message when it cannot start.  A title in OPTIONS names
the shell; :hidden there leaves it undisplayed."
  (let ((buffer (condition-case err
                    (drover-agent-shell--start directory
                                               (drover-agent-shell--start-config engine options)
                                               (plist-get options :hidden))
                  (error (funcall else (drover-error-message err))
                         nil))))
    (when buffer
      (when-let* ((title (plist-get options :title))
                  ((not (string-empty-p (string-trim title)))))
        (drover-agent-shell--set-title buffer (string-trim title)))
      (funcall then (drover-agent-shell--session buffer)))))

(defun drover-agent-shell--engines ()
  "Return the engines of `drover-agent-shell-engines' agent-shell can start."
  (seq-filter (lambda (engine) (functionp (alist-get engine drover-agent-shell-configs)))
              drover-agent-shell-engines))

(defun drover-agent-shell--read-engine ()
  "Return the engine of a new shell, asking when there are several.
The engine of the agent-shell session at point is the default."
  (let ((engines (drover-agent-shell--engines)))
    (cond
     ((null engines) (user-error "No engine to start in agent-shell"))
     ((null (cdr engines)) (car engines))
     (t (let* ((candidates (mapcar (lambda (engine) (cons (drover-engine-label engine) engine))
                                   engines))
               (session (drover-session-at-point t))
               (current (and session (eq (drover-session-backend session) 'agent-shell)
                             (rassq (drover-session-engine session) candidates))))
          (cdr (assoc (completing-read (format-prompt "Start a shell of" (car current))
                                       candidates nil t nil nil (car current))
                      candidates)))))))

(defun drover-agent-shell--new (directory)
  "Start a shell in DIRECTORY, of an engine read from the user, and display it."
  (drover-agent-shell--create directory (drover-agent-shell--read-engine) nil
                              #'ignore (lambda (message) (user-error "%s" message))))

(defun drover-agent-shell-resume (engine session-id directory)
  "Resume the conversation SESSION-ID of ENGINE in a new shell, and return it.
The shell starts in DIRECTORY, the conversation's, and loads it through
ACP (session/load, or session/resume), as agent-shell resumes a session
by its identifier; it shows in the selected window."
  (let* ((maker (alist-get engine drover-agent-shell-configs))
         (directory (file-name-as-directory (expand-file-name directory))))
    (unless (functionp maker)
      (user-error "No agent-shell configuration for %s" (drover-engine-label engine)))
    (let* ((default-directory directory)
           (buffer (agent-shell--start :config (funcall maker) :no-focus t :new-session t
                                       :session-strategy 'new :session-id session-id
                                       :cwd directory)))
      (pop-to-buffer-same-window buffer)
      buffer)))

(defun drover-agent-shell--resumers ()
  "Return the engines whose conversations a shell resumes, with how.
As (ENGINE . FUNCTION) for `drover-register-resumer'."
  (mapcar (lambda (entry)
            (let ((engine (car entry)))
              (cons engine (lambda (id directory) (drover-agent-shell-resume engine id directory)))))
          drover-agent-shell-configs))

;;;; Sending

(defun drover-agent-shell--when-initialized (buffer then else)
  "Call THEN once the shell of BUFFER is initialized, now or later.
Call ELSE with a message instead when it cannot be: BUFFER is dead,
its start failed, or it fails, its session choice is cancelled or it is
killed before it is initialized.
One of them is called, once."
  (cond
   ((not (buffer-live-p buffer)) (funcall else "its buffer is gone"))
   ((drover-agent-shell--initialized-p buffer) (funcall then))
   ((and (map-elt (buffer-local-value 'agent-shell--state buffer) :connection-error)
         (not (map-elt (buffer-local-value 'agent-shell--state buffer) :connection-retrying)))
    (funcall else (map-elt (buffer-local-value 'agent-shell--state buffer) :connection-error)))
   ((buffer-local-value 'drover-agent-shell--start-error buffer)
    (funcall else (buffer-local-value 'drover-agent-shell--start-error buffer)))
   (t
    (let (token)
      (setq token
            (agent-shell-subscribe-to
             :shell-buffer buffer
             :on-event (lambda (event)
                         (drover-with-reporting "agent-shell start"
                           (let ((kind (map-elt event :event)))
                             (when (memq kind '(init-finished error session-selection-cancelled clean-up))
                               (with-current-buffer buffer
                                 (agent-shell-unsubscribe :subscription token))
                               (pcase kind
                                 ('init-finished (funcall then))
                                 ('error (funcall else (or (map-nested-elt event '(:data :message))
                                                           "it failed to start")))
                                 ('session-selection-cancelled
                                  (funcall else "its session choice was cancelled"))
                                 (_ (funcall else "it was closed")))))))))))))

(defvar drover-agent-shell--submitting nil
  "Non-nil while drover submits a prompt to a shell.
Its `input-submitted' event is drover's send, not the user's at the
shell: drover tells of it itself, if it was the user's (see
`drover-agent-shell--handed-back').")

(defun drover-agent-shell--submit (buffer text)
  "Submit TEXT to the initialized shell of BUFFER, queued while it is busy."
  (with-current-buffer buffer
    (if (shell-maker-busy)
        (agent-shell-prompt-queue text)
      (let ((drover-agent-shell--submitting t))
        (agent-shell-insert :shell-buffer buffer :text text :submit t :no-focus t)))))

(defun drover-agent-shell-send-text (buffer text)
  "Submit TEXT as a prompt to the shell of BUFFER.
A shell still starting gets it once initialized; a busy one queues it;
a ready one takes it at once, without taking the focus.  A disconnected
shell takes it through its submission path, which reconnects first."
  ;; Not on `prompt-ready', which comes before the default model and
  ;; mode are set: submitting then would start a second bootstrap.
  (if (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (and (map-elt agent-shell--state :connection-error)
                  (not (map-elt agent-shell--state :connection-retrying)))))
      (drover-agent-shell--submit buffer text)
    (drover-agent-shell--when-initialized
     buffer
     (lambda () (drover-agent-shell--submit buffer text))
     (lambda (reason)
       (drover-warn "%s did not take the prompt (%s); it is in the kill ring"
                    (if (buffer-live-p buffer) (buffer-name buffer) "The agent shell") reason)))))

(defun drover-agent-shell--send (session text _token)
  "Submit TEXT to the shell of SESSION, as the backend's :send."
  (if-let* ((buffer (drover-session-buffer session)))
      (drover-agent-shell-send-text buffer text)
    (drover-warn "%s is gone; the prompt is in the kill ring"
                 (drover-session-display-title session))))

;;;; Prompts sent later

(defun drover-agent-shell--blocker (session)
  "Say why the shell of SESSION cannot take a prompt now, or nil once it can.
It can once initialized, idle, with nothing typed at its prompt, so
that no draft is touched.  The backend's :blocker."
  (if-let* ((buffer (drover-session-buffer session)))
      (with-current-buffer buffer
        (cond ((not (drover-agent-shell--initialized-p buffer)) "the shell has not initialized")
              ((shell-maker-busy) "the agent is still busy")
              ((let ((start (agent-shell--prompt-input-start)))
                 (cond ((not start) "the shell shows no prompt")
                       ((not (string-blank-p (buffer-substring-no-properties start (point-max))))
                        "the input holds a draft"))))))
    "the shell is gone"))

(defun drover-agent-shell--deliver (session text then else)
  "Submit TEXT to the shell of SESSION now, then call THEN, or ELSE with a message.
The backend's :submit."
  (if-let* ((buffer (drover-session-buffer session)))
      (let ((failure (condition-case err
                         (progn (agent-shell-insert :shell-buffer buffer :text text :submit t :no-focus t)
                                nil)
                       (error (drover-error-message err)))))
        (if failure (funcall else failure) (funcall then)))
    (funcall else "the shell is gone")))

;;;; Options of running shells

(defun drover-agent-shell--step-failure (buffer label next)
  "Return an ACP failure handler passing BUFFER's refusal of LABEL to NEXT."
  (lambda (acp-error &rest _)
    (drover-with-reporting "agent-shell options"
      (funcall next (drover-format-message "%s refused %s: %s"
                                           (buffer-name buffer) label
                                           (or (and (consp acp-error) (map-elt acp-error 'message)) acp-error))))))

(defun drover-agent-shell--option-steps (buffer engine agent model-id model effort-variant variant)
  "Return the steps applying a prompt's choices to the shell of BUFFER.
ENGINE is the shell's engine.  AGENT is switched through the ACP
session mode, which OpenCode maps to its agents and Claude Code to the
permission modes of `drover-claude-code-agent-modes'; any other agent
of Claude Code is a session-start option, so it and an AGENT for any
other engine are reported and skipped.  MODEL-ID is the name
agent-shell takes for MODEL (see `drover-model-id-for').  EFFORT-VARIANT
is the variant whose effort to set, VARIANT the explicit one: only an
explicit variant without a matching thought level is worth a warning.
Each step finds its target when it runs, so effort is matched against
the thought levels the model switch before it brought.  See
`drover-run-steps'."
  (let ((candidates (drover-model-effort-candidates model effort-variant))
        (name (buffer-name buffer)))
    (delq nil
          (list
           (when agent
             (if-let* ((mode (pcase engine
                               ('opencode agent)
                               ('claude-code (drover-claude-code-agent-mode agent)))))
                 (let ((label (format "agent %s" agent)))
                   (cons label
                         (lambda (next)
                           (with-current-buffer buffer
                             (agent-shell--config-option-set-mode-id
                              :mode-id mode :on-success next
                              :on-failure (drover-agent-shell--step-failure buffer label next))))))
               (cons "agent" (lambda (next)
                               (funcall next (format "%s takes agent %s at session start" name agent))))))
           (when model
             (if model-id
                 (let ((label (format "model %s" model-id)))
                   (cons label
                         (lambda (next)
                           (with-current-buffer buffer
                             (agent-shell--config-option-set-model-id
                              :model-id model-id :on-success next
                              :on-failure (drover-agent-shell--step-failure buffer label next))))))
               (cons "model" (lambda (next)
                               (funcall next (format "%s cannot run %s" name (drover-model-label model)))))))
           (when candidates
             (let ((label (format "effort %s" effort-variant)))
               (cons label
                     (lambda (next)
                       (pcase-let ((`(,option . ,target)
                                    (with-current-buffer buffer
                                      (drover-agent-shell--thought-level (agent-shell--state) candidates))))
                         (cond
                          ((null target)
                           (funcall next (and variant (format "%s offers no thought level for %s" name variant))))
                          ((equal target (map-elt option :current-value)) (funcall next))
                          (t (with-current-buffer buffer
                               (agent-shell--config-option-set-thought-level-id
                                :thought-level-id target :on-success next
                                :on-failure (drover-agent-shell--step-failure buffer label next))))))))))))))

(defun drover-agent-shell--apply-options (session options then &optional else)
  "Apply explicit OPTIONS to the running shell of SESSION, then call THEN once.
OPTIONS are filtered for SESSION already.  A shell still starting is
waited for, as options are set on its ACP session.  An explicit
OpenCode model also sets its default variant, since OpenCode's ACP
adapter otherwise picks the first effort it offers.  ELSE receives a
failure string instead of THEN running when a switch fails."
  (let ((buffer (drover-session-buffer session)))
    (if (null buffer)
        (if else (funcall else "The shell is gone") (funcall then))
      (let* ((engine (drover-session-engine session))
             (agent (let ((agent (plist-get options :agent)))
                      (and agent (not (string-empty-p agent)) agent)))
             (model (plist-get options :model))
             (variant (plist-get options :variant))
             (effort-variant (or variant
                                 (and model (eq engine 'opencode) (drover-model-default-variant model))))
             (steps (drover-agent-shell--option-steps
                     buffer engine agent (and model (drover-model-id-for model engine 'agent-shell))
                     model effort-variant variant)))
        (if (null steps)
            (funcall then)
          (drover-agent-shell--when-initialized
           buffer
           (lambda () (drover-run-steps steps then else))
           ;; :send reports a shell that will not start.
           (lambda (reason) (if else (funcall else reason) (funcall then)))))))))

(defun drover-agent-shell--split-model-id (engine value)
  "Return (PROVIDER . ID) of model VALUE of a shell of ENGINE.
Engines of `drover-model-qualified-engines' name models \"PROVIDER/ID\";
the others by bare identifier, PROVIDER being nil."
  (if (and (memq engine drover-model-qualified-engines)
           (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" value))
      (cons (match-string 1 value) (match-string 2 value))
    (cons nil value)))

(defun drover-agent-shell--levels (state)
  "Return the thought levels the shell of STATE offers for its model, symbols.
\"default\", which Claude Code's agent offers to leave the effort to
the CLI, is no level: drover's pickers offer the default already."
  (delq nil (mapcar (lambda (level)
                      (when-let* ((value (map-elt level :value))
                                  ((stringp value))
                                  ((not (member value '("" "default")))))
                        (intern value)))
                    (agent-shell--get-available-thought-levels state))))

(defun drover-agent-shell--model-options (state)
  "Return the models the shell of STATE lists, as (VALUE . NAME), in order.
VALUE is how the shell's agent names the model, NAME how it shows it."
  (delq nil (mapcar (lambda (entry)
                      (let ((value (map-elt entry :model-id)))
                        (and (stringp value) (not (string-empty-p value))
                             (cons value (map-elt entry :name)))))
                    (agent-shell--get-available-models state))))

(defun drover-agent-shell--models-of (engine options current levels state)
  "Return the models OPTIONS name, which a shell of ENGINE in STATE lists.
OPTIONS are (VALUE . NAME), as `drover-agent-shell--model-options'
returns them; CURRENT is the VALUE the shell runs, and LEVELS the
thought levels it offers for it.  ACP tells the thought levels of the
model a session runs only: that model has them as variants, and the
others the variants their engine tells otherwise, else unknown ones.
Claude Code's models are identified through the catalog its CLI
caches, the alias the shell lists kept as the name agent-shell takes
\(see `drover-claude-code-acp-models'); Codex's take the levels its
agent lists apart, per model (see `drover-codex-acp-models').

While `drover-keeping-models' runs, shells listing the same models make
them once: a shell's take two or three milliseconds, its catalog read
and each model described."
  (let ((models (and (eq engine 'codex) (map-nested-elt state '(:session :models)))))
    (drover-models-kept
     (list 'agent-shell engine options current levels models)
     (lambda ()
       (pcase engine
         ('claude-code (drover-claude-code-acp-models options current levels 'agent-shell))
         ('codex (drover-codex-acp-models
                  options current levels
                  (drover-codex-acp-model-efforts
                   (mapcar (lambda (model) (map-elt model :model-id)) models))))
         (_ (mapcar (pcase-lambda (`(,value . ,name))
                      (pcase-let ((`(,provider . ,id) (drover-agent-shell--split-model-id engine value))
                                  (current-p (equal value current)))
                        (drover-model-create
                         :provider-id provider
                         :id id
                         :name name
                         :variants (and current-p (mapcar #'list levels))
                         :variants-known current-p
                         :engines (delete-dups (cons engine (drover-model-engines-for provider id))))))
                    options)))))))

(defun drover-agent-shell--shell-models (engine state)
  "Return the models the shell of ENGINE offers in STATE.
They are `drover-model' records; see `drover-agent-shell--models-of'."
  (let ((current (agent-shell--current-model-id state)))
    (drover-agent-shell--models-of engine (drover-agent-shell--model-options state)
                                   (and (stringp current) current)
                                   (drover-agent-shell--levels state)
                                   state)))

(defun drover-agent-shell--current-model (engine state &optional options)
  "Return the `drover-model' of the model the current shell of ENGINE runs, or nil.
STATE is its state.  The model is described as its option alone names
it (see `drover-agent-shell--models-of'); with OPTIONS, the shell's
model options, as the shell lists it among them, which may know it by
another name."
  (when-let* ((value (ignore-errors (agent-shell--current-model-id state)))
              ((stringp value)))
    (let* ((levels (drover-agent-shell--levels state))
           (alone (car (drover-agent-shell--models-of
                        engine (list (or (assoc value (or options (drover-agent-shell--model-options state)))
                                         (list value)))
                        value levels state))))
      (and alone
           (or (and options
                    (seq-find (lambda (model)
                                (equal (drover-model-key model) (drover-model-key alone)))
                              (drover-agent-shell--models-of engine options value levels state)))
               alone)))))

(defun drover-agent-shell--options (session)
  "Return what the shell of SESSION runs, or nil.
The value is (:model MODEL :variant VARIANT), MODEL a `drover-model' as
the shell's models describe it (see `drover-agent-shell--models-of'),
and VARIANT its thought level, MODEL's default variant when the shell
leaves the effort to its agent."
  (when-let* ((buffer (drover-session-buffer session)))
    (with-current-buffer buffer
      (when-let* ((state agent-shell--state)
                  (value (agent-shell--current-model-id state))
                  ((stringp value)))
        (let* ((model (drover-agent-shell--current-model
                       (drover-agent-shell--engine) state (drover-agent-shell--model-options state)))
               (level (agent-shell--current-thought-level-id state)))
          (list :model model
                :variant (cond ((not (and (stringp level) (not (string-empty-p level)))) nil)
                               ((equal level "default") (and model (drover-model-default-variant model)))
                               (t (intern level)))))))))

(defun drover-agent-shell--settable (session)
  "Return which of :agent, :model and :variant the shell of SESSION can change.
A shell still starting tells nothing yet: all three.  Otherwise the
agent of an engine mapping agents to session modes, the model, and the
variant when the shell offers thought levels."
  (let ((buffer (drover-session-buffer session)))
    (if (not (and buffer (drover-agent-shell--initialized-p buffer)))
        '(:agent :model :variant)
      (with-current-buffer buffer
        (delq nil (list (and (memq (drover-agent-shell--engine) '(opencode claude-code)) :agent)
                        :model
                        (and (agent-shell--config-option-by-category agent-shell--state "thought_level")
                             :variant)))))))

;;;; Models of live shells

(defun drover-agent-shell--live-models (engine)
  "Return the models the live shells of ENGINE offer, each once.
An ACP agent publishes its models once its session exists, so a
running shell is the catalog of its engine.  Each shell tells the
thought levels of the model it runs (see
`drover-agent-shell--shell-models'): a model several shells list is
merged, each telling what it knows, most recently used shell first.
Shells listing the same models, often all of an engine's, make them
once (see `drover-keeping-models'), and are merged once."
  (drover-keeping-models
    (let ((lists nil))
      (dolist (buffer (drover-agent-shell--shells))
        (with-current-buffer buffer
          (when (eq (drover-agent-shell--engine) engine)
            ;; The same list for the same models, kept: not to be joined
            ;; destructively.
            (let ((models (drover-agent-shell--shell-models engine agent-shell--state)))
              (unless (memq models lists)
                (push models lists))))))
      (drover-model-merge-all (apply #'append (nreverse lists))))))

(defun drover-agent-shell--claude-code-models (_directory)
  "Return the Claude Code models live shells offer."
  (drover-agent-shell--live-models 'claude-code))

(defun drover-agent-shell--codex-models (_directory)
  "Return the Codex models live shells offer."
  (drover-agent-shell--live-models 'codex))

(defun drover-agent-shell--gemini-cli-models (_directory)
  "Return the Gemini CLI models live shells offer."
  (drover-agent-shell--live-models 'gemini-cli))

(defun drover-agent-shell--opencode-models (_directory)
  "Return the OpenCode models live shells offer."
  (drover-agent-shell--live-models 'opencode))

(defconst drover-agent-shell--model-sources
  '((claude-code -10 drover-agent-shell--claude-code-models)
    (codex 0 drover-agent-shell--codex-models)
    (gemini-cli 0 drover-agent-shell--gemini-cli-models)
    (opencode 10 drover-agent-shell--opencode-models))
  "Model sources of live shells, as (ENGINE DEPTH FUNCTION).
A live Claude Code shell knows its models before any catalog; an
OpenCode one only stands in when the service's catalog cannot answer.
Each is registered live: what it answers is remembered.")

(defconst drover-agent-shell--model-events
  '(init-session init-model init-config-options config-option-update
                 session-selected session-restored)
  "Events after which a shell may offer other models or thought levels.
Its session, new or loaded, brings the agent's models; its options, set
at start or changed by the agent, the thought levels of its model.")

(defun drover-agent-shell--models-changed (&optional buffer)
  "Tell drover the models of the shell of BUFFER may have changed.
BUFFER is the current buffer by default."
  (when-let* ((engine (drover-agent-shell--engine buffer)))
    (drover-models-changed engine)))

;;;; Sessions

(defun drover-agent-shell--ensure-id ()
  "Return the identifier of the current shell's session, giving it one first."
  (or drover-agent-shell--id
      (setq drover-agent-shell--id (format "shell-%d" (cl-incf drover-agent-shell--counter)))))

(defun drover-agent-shell--project-name ()
  "Return the project of the current shell, looked up once per directory.
Looking a project up may walk the file system, slow on network mounts,
and sessions are listed often."
  (unless (equal (car drover-agent-shell--project) default-directory)
    (setq drover-agent-shell--project
          (cons default-directory (ignore-errors (agent-shell--project-name)))))
  (cdr drover-agent-shell--project))

(defun drover-agent-shell--display-title (state)
  "Return the title of the current shell's session, from its STATE.
The title the user gave, else the ACP session's, else the buffer name,
on one line."
  (let ((title (or drover-agent-shell--title
                   (let ((title (map-nested-elt state '(:session :title))))
                     (and (stringp title) (not (string-blank-p title)) title))
                   (buffer-name))))
    (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " title))))

(defun drover-agent-shell--requests (state)
  "Return the requests the agent of STATE waits for, oldest first.
Each is (TIME :kind KIND :summary SUMMARY): permissions its tool calls
wait for, then questions it asked; TIME is when it arrived, or nil."
  (let ((tool-calls (map-elt state :tool-calls))
        requests)
    (dolist (entry tool-calls)
      (when-let* ((request (map-elt (cdr entry) :permission-request-id)))
        (push (list (alist-get request drover-agent-shell--request-times nil nil #'equal)
                    :kind 'permission
                    :summary (or (map-elt (cdr entry) :title) "Agent action"))
              requests)))
    (dolist (entry (map-elt state :elicitations))
      (when (eq (map-elt (cdr entry) :status) 'pending)
        (push (list (alist-get (car entry) drover-agent-shell--request-times nil nil #'equal)
                    :kind 'question
                    :summary (or (map-elt (cdr entry) :message)
                                 (map-nested-elt tool-calls (list (map-elt (cdr entry) :tool-call-id) :title))
                                 "Question"))
              requests)))
    ;; `sort' is stable: requests of unknown time keep their order, last.
    (sort (nreverse requests)
          (lambda (a b) (< (or (car a) most-positive-fixnum) (or (car b) most-positive-fixnum))))))

(defun drover-agent-shell--status (state)
  "Return the status of the current shell, from its STATE.
The value is a plist of :status, :since, :detail and :requests, as
`drover-session' records hold them."
  (let ((outcome drover-agent-shell--outcome))
    (cond
     ((and (map-elt state :connection-error)
           (not (map-elt state :connection-retrying)))
      (list :status 'failed
            :since (map-elt state :connection-since)
            :detail (drover-transcript-text (map-elt state :connection-error))))
     ((not (drover-agent-shell--initialized-p (current-buffer)))
      (if drover-agent-shell--start-error
          (list :status (if (eq (plist-get outcome :status) 'failed) 'failed 'idle)
                :since (plist-get outcome :time)
                :detail (concat "failed to start: " drover-agent-shell--start-error))
        (list :status 'working :since drover-agent-shell--created :detail "starting")))
     (t
      (pcase (ignore-errors (agent-shell-status))
        ('blocked
         (let ((requests (drover-agent-shell--requests state)))
           (list :status 'waiting
                 :since (seq-some #'car requests)
                 :requests (mapcar #'cdr requests))))
        ('busy (list :status 'working :since drover-agent-shell--turn-start))
        (_ (if outcome
               (list :status (plist-get outcome :status)
                     :since (plist-get outcome :time)
                     :detail (plist-get outcome :detail))
             (list :status 'idle))))))))

(defun drover-agent-shell--effort (state)
  "Return the effort the current shell runs, from its STATE, as (LEVEL . DEFAULT).
LEVEL is a string, or nil when unknown.  Claude Code's agent offers a
level \"default\", leaving the effort to the CLI: the model's default
level stands for it, as `drover-agent-shell--options' runs it, its
model as its sources describe it (Claude Code's catalog for Claude's
models), else as drover learnt it (see `drover-learnt-model'), and
DEFAULT is non-nil; nil when unknown.  Whether another level is the
model's default the shell does not tell."
  (let ((level (ignore-errors (agent-shell--current-thought-level-id state))))
    (cond ((not (and (stringp level) (not (string-empty-p level)))) nil)
          ((not (equal level "default")) (cons level nil))
          (t (when-let* ((engine (drover-agent-shell--engine))
                         (model (drover-agent-shell--current-model engine state))
                         (variant (or (drover-model-default-variant model)
                                      (when-let* ((learnt (drover-learnt-model engine (drover-model-key model))))
                                        (drover-model-default-variant learnt)))))
               (cons (symbol-name variant) t))))))

(defun drover-agent-shell--provider (state)
  "Return the provider of the model the current shell runs, from its STATE, or nil.
Only shells of engines naming models \"PROVIDER/ID\", such as
OpenCode's, tell it."
  (let ((value (ignore-errors (agent-shell--current-model-id state))))
    (and (stringp value)
         (car (drover-agent-shell--split-model-id (drover-agent-shell--engine) value)))))

(defun drover-agent-shell--last-activity ()
  "Return when a turn of the current shell last did something, or nil.
While a turn of its own runs, the shell's last activity, which every
notification of the agent moves; otherwise the time
`drover-agent-shell--activity' noted: a shell loading a conversation
again receives it whole, which is no activity of the conversation."
  (let ((activity (and drover-agent-shell--in-turn
                       (ignore-errors (agent-shell-last-activity-time)))))
    (if (and activity drover-agent-shell--activity)
        (max drover-agent-shell--activity (float-time activity))
      (or drover-agent-shell--activity (and activity (float-time activity))))))

(defun drover-agent-shell--session (buffer)
  "Return the `drover-session' of the shell of BUFFER."
  (with-current-buffer buffer
    (let* ((state agent-shell--state)
           (status (drover-agent-shell--status state))
           (engine (drover-agent-shell--engine))
           (engine-id (map-nested-elt state '(:session :id)))
           (times (drover-followed-times engine engine-id drover-agent-shell--created
                                         (drover-agent-shell--last-activity)))
           (effort (drover-agent-shell--effort state))
           ;; Its provider, context and cache, as recorded, when the shell
           ;; does not tell them.
           (run (drover-recorded-run engine engine-id)))
      (apply
       #'drover-session-create
       :backend 'agent-shell
       :id (drover-agent-shell--ensure-id)
       :title (drover-agent-shell--display-title state)
       :directory default-directory
       :project (drover-agent-shell--project-name)
       :branch (drover-git-branch default-directory)
       :status (plist-get status :status)
       :requests (plist-get status :requests)
       :since (plist-get status :since)
       :updated (cdr times)
       :created (car times)
       :agent (ignore-errors (agent-shell--current-mode-id state))
       :model (drover-model-short-name (ignore-errors (agent-shell-get-model-name state)))
       :effort (car effort)
       :effort-default (cdr effort)
       :provider-name (or (drover-agent-shell--provider state) (plist-get run :provider-name))
       :detail (plist-get status :detail)
       :engine engine
       :engine-id engine-id
       run))))

(defun drover-agent-shell--sessions ()
  "Return the sessions of the live shells."
  (mapcar #'drover-agent-shell--session (drover-agent-shell--shells)))

(defun drover-agent-shell--buffer (session)
  "Return the live buffer of the shell of SESSION, or nil.
It is found by its identifier among the buffers in agent-shell's mode,
rather than among `drover-agent-shell--shells', which visits every
buffer: lists ask for the buffer of every session they draw.  The mode
is read first, as reading a variable in a buffer where it is not local
searches every variable local there, hundreds in an Org buffer."
  (let ((id (drover-session-id session)))
    (seq-find (lambda (buffer)
                (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'agent-shell-mode)
                     (equal (buffer-local-value 'drover-agent-shell--id buffer) id)
                     (drover-agent-shell--live-p buffer)))
              (buffer-list))))

(defun drover-agent-shell--buffer-session ()
  "Return the session of the current shell."
  (when (and (derived-mode-p 'agent-shell-mode) agent-shell--state)
    (drover-agent-shell--session (current-buffer))))

;;;; Events and attention

(defconst drover-agent-shell--quiet-events
  '(agent-message-chunk tool-call-update usage-update file-write idle)
  "Events that change nothing drover shows of a shell.
They stream many times a turn: reporting them would hold back the
redraw drover runs once backends are quiet.")

(defun drover-agent-shell--focused-p (buffer)
  "Return non-nil when the shell of BUFFER, or its viewport, has the focus.
It does when shown in the selected window of a frame that has the
focus."
  (let ((viewport (ignore-errors (agent-shell-viewport--buffer :shell-buffer buffer :existing-only t))))
    (seq-some (lambda (window)
                (let ((frame (window-frame window)))
                  (and (eq (frame-focus-state frame) t)
                       (eq window (frame-selected-window frame)))))
              (append (get-buffer-window-list buffer nil t)
                      (and viewport (get-buffer-window-list viewport nil t))))))

(defun drover-agent-shell--end-turn (status detail)
  "Note that the current shell's turn ended with STATUS and DETAIL.
STATUS is `done' or `failed'.  The user looking at the shell sees it
end; otherwise it stays to be seen, a failure winning over a later
success."
  (cond
   ((drover-agent-shell--focused-p (current-buffer))
    (setq drover-agent-shell--outcome nil))
   ((not (and (eq status 'done) (eq (plist-get drover-agent-shell--outcome :status) 'failed)))
    (setq drover-agent-shell--outcome (list :status status :detail detail :time (float-time))))))

(defun drover-agent-shell--handed-back (how)
  "Note that the user handed the current shell its turn back, as HOW.
HOW is `prompt' or `answer'.  Only what a command of the user's gives
counts: not a prompt drover submits (see
`drover-agent-shell--submitting'), nor one agent-shell takes from its
queue as a turn ends, nor an answer an agent-shell responder or
another host gives, all of which run out of any command.  The buffer
of the selected window goes with it when it shows this shell, as its
viewport does."
  (when (and this-command (not drover-agent-shell--submitting))
    (when-let* ((session (drover-agent-shell--buffer-session)))
      (let ((acting (window-buffer (selected-window))))
        (drover-note-handed-off session how 'front-end
                                (and (eq (drover-agent-shell--shell-of acting) (current-buffer))
                                     acting))))))

(defun drover-agent-shell--on-event (event)
  "Follow EVENT of the current shell, and report what changed to drover."
  (drover-with-reporting "agent-shell event"
    (let* ((kind (map-elt event :event))
           (data (map-elt event :data))
           (request (map-elt data :request-id)))
      (pcase kind
        ((or 'input-submitted 'permission-response 'elicitation-response)
         ;; A request cancelled, or a form dismissed, does not give the agent its answer.
         (unless (or (map-elt data :cancelled) (equal (format "%s" (map-elt data :action)) "cancel"))
           (drover-agent-shell--handed-back (if (eq kind 'input-submitted) 'prompt 'answer)))))
      (pcase kind
        ('input-submitted
         (setq drover-agent-shell--outcome nil
               drover-agent-shell--turn-start (float-time)
               drover-agent-shell--in-turn t
               drover-agent-shell--activity drover-agent-shell--turn-start
               drover-agent-shell--prompt (map-elt data :prompt)
               drover-agent-shell--reply nil
               drover-agent-shell--reply-open nil
               drover-agent-shell--written nil))
        ('agent-message-chunk
         (when-let* ((text (map-elt data :text-chunk))
                     ((stringp text)))
           (setq drover-agent-shell--reply (if drover-agent-shell--reply-open
                                               (concat drover-agent-shell--reply text)
                                             text)
                 drover-agent-shell--reply-open t)))
        ('tool-call-update
         (let ((tool-call (map-elt data :tool-call)))
           ;; Proposed edits carry diffs too: only those done count.
           (when (equal (map-elt tool-call :status) "completed")
             (dolist (diff (map-elt tool-call :diffs))
               (drover-agent-shell--note-change (map-elt diff :file))))))
        ('file-write
         (drover-agent-shell--note-change (map-elt data :path)))
        ('turn-complete
         (setq drover-agent-shell--request-times nil)
         (drover-agent-shell--end-activity)
         (let ((reason (map-elt data :stop-reason)))
           (drover-agent-shell--held-turn-end reason)
           (pcase reason
             ("end_turn" (drover-agent-shell--end-turn 'done nil))
             ("cancelled" (setq drover-agent-shell--outcome nil))
             (_ (drover-agent-shell--end-turn 'failed (agent-shell--stop-reason-description reason))))))
        ('error
         (let ((message (or (map-elt data :message) "Agent request failed")))
           (unless (drover-agent-shell--initialized-p (current-buffer))
             (setq drover-agent-shell--start-error message))
           (when drover-agent-shell--in-turn
             (drover-agent-shell--end-activity))
           (drover-agent-shell--held-failure)
           (drover-agent-shell--end-turn 'failed message)))
        ((or 'permission-request 'elicitation-request)
         (setq drover-agent-shell--activity (float-time))
         (when request
           (setf (alist-get request drover-agent-shell--request-times nil nil #'equal) (float-time))))
        ((or 'permission-response 'elicitation-response)
         (setq drover-agent-shell--activity (float-time))
         (when request
           (setf (alist-get request drover-agent-shell--request-times nil t #'equal) nil)))
        ('session-title-changed
         (drover-agent-shell--follow-title))
        ('session-selected
         ;; Another conversation, whose activity is its own.
         (setq drover-agent-shell--outcome nil
               drover-agent-shell--request-times nil
               drover-agent-shell--in-turn nil
               drover-agent-shell--activity nil))
        ('session-selection-cancelled
         ;; agent-shell kills the shell next, but a kill refused, a query to
         ;; save its transcript quit, leaves a shell that will never start.
         (unless (drover-agent-shell--initialized-p (current-buffer))
           (setq drover-agent-shell--start-error "session choice cancelled")))
        ('clean-up
         ;; Prompts set aside, which agent-shell did not count as queued
         ;; before its shell went.
         (when (drover-keep-unsent drover-agent-shell--held (current-buffer))
           (drover-message "%s closed with prompts it never sent, now in the kill ring" (buffer-name)))
         (setq drover-agent-shell--held nil
               drover-agent-shell--holding nil)
         (setq drover-agent-shell--outcome nil
               drover-agent-shell--request-times nil
               drover-agent-shell--in-turn nil)))
      ;; Initialization went on, so it had not failed for good.
      (when (string-prefix-p "init-" (symbol-name kind))
        (setq drover-agent-shell--start-error nil))
      ;; What comes between two messages of the agent parts them.
      (when (memq kind '(tool-call-update permission-request elicitation-request turn-complete))
        (setq drover-agent-shell--reply-open nil))
      (unless (memq kind drover-agent-shell--quiet-events)
        (drover-backend-changed 'agent-shell drover-agent-shell--id))
      (when (memq kind drover-agent-shell--model-events)
        (drover-agent-shell--models-changed))
      (when (eq kind 'input-submitted)
        (drover-agent-shell--sent-text (map-elt data :prompt))))))

(defun drover-agent-shell--sent-text (text)
  "Note TEXT, a prompt the user submitted to the current shell.
Only what a command of the user's submits, as for
`drover-agent-shell--handed-back': not a prompt drover submits, nor one
agent-shell takes from its queue."
  (when (and this-command (not drover-agent-shell--submitting) (stringp text))
    (when-let* ((session (drover-agent-shell--buffer-session)))
      (drover-note-sent-text session text (current-buffer)))))

(defun drover-agent-shell--end-activity ()
  "Note that the turn of the current shell ended now."
  (setq drover-agent-shell--activity (float-time)
        drover-agent-shell--in-turn nil))

(defun drover-agent-shell--note-change (file)
  "Note that the agent of the current shell changed FILE, a file name or nil."
  (when (and (stringp file) (not (string-empty-p file)))
    (setq drover-agent-shell--written (cons file (delete file drover-agent-shell--written)))))

(defun drover-agent-shell--seen (buffer)
  "Mark the ended turn of the shell of BUFFER seen, reporting the change."
  (when (buffer-local-value 'drover-agent-shell--outcome buffer)
    (with-current-buffer buffer
      (setq drover-agent-shell--outcome nil))
    (drover-backend-changed 'agent-shell (buffer-local-value 'drover-agent-shell--id buffer))))

(defun drover-agent-shell--window-changed (&optional frame)
  "Mark seen the shell the user now looks at in FRAME.
FRAME is the selected frame by default.  This runs from
`window-selection-change-functions', `window-buffer-change-functions'
and `after-focus-change-function'."
  (drover-with-reporting "agent-shell windows"
    (let* ((frame (if (frame-live-p frame) frame (selected-frame)))
           (shell (drover-agent-shell--shell-of (window-buffer (frame-selected-window frame)))))
      (when (and shell
                 (buffer-local-value 'drover-agent-shell--outcome shell)
                 (drover-agent-shell--focused-p shell))
        (drover-agent-shell--seen shell)))))

;;;; Actions

(defun drover-agent-shell--live-buffer (session)
  "Return the buffer of the shell of SESSION, or signal a `user-error'."
  (or (drover-session-buffer session)
      (user-error "The shell of %s is gone" (drover-session-display-title session))))

(defun drover-agent-shell--open (session other-window)
  "Display the shell of SESSION, in another window with OTHER-WINDOW."
  (let ((buffer (drover-agent-shell--live-buffer session)))
    (if other-window
        (pop-to-buffer buffer t)
      (pop-to-buffer-same-window buffer))))

(defun drover-agent-shell--respond (session)
  "Display the shell of SESSION at its latest permission request.
It is displayed as `drover-open' does: from a list in a side window, in
the window where the user works."
  (drover-open session)
  (with-current-buffer (drover-agent-shell--live-buffer session)
    (agent-shell-jump-to-latest-permission-button-row)))

(defun drover-agent-shell--interrupt (session)
  "Stop the running turn of SESSION, without asking: drover asked already.
Nor is anything asked once the turn ends: the prompts its shell queued
are set aside until then, then queued again and shown, unsent (see
`drover-agent-shell--hold')."
  (with-current-buffer (drover-agent-shell--live-buffer session)
    (let ((running (shell-maker-busy)))
      (when running
        (drover-agent-shell--hold))
      (condition-case err
          (agent-shell-interrupt t)
        (error (when running
                 (setq drover-agent-shell--holding nil)
                 (drover-agent-shell--requeue))
               (signal (car err) (cdr err)))))))

;; A turn cancelled while prompts wait in its shell's queue has
;; agent-shell ask whether to send them, from the process filter reading
;; the agent's answer: of whoever looks at this Emacs then, or of nobody,
;; when another host interrupted it.  On Windows the question is a dialog
;; box, which stops Emacs whole, its connections to other hosts included,
;; until answered.  The prompts are set aside before agent-shell sees the
;; turn end, then queued again and shown, as answering no leaves them.

(defun drover-agent-shell--hold ()
  "Set aside the prompts the current shell queued, until its turn ends.
They go after those set aside already; agent-shell, finding its queue
empty as the turn ends, asks nothing about it."
  (setq drover-agent-shell--held (append drover-agent-shell--held
                                         (map-elt agent-shell--state :pending-prompts))
        drover-agent-shell--holding t)
  (setf (map-elt agent-shell--state :pending-prompts) nil))

(defun drover-agent-shell--requeue ()
  "Queue again in the current shell the prompts set aside, before any queued since.
Return non-nil when there were some."
  (when drover-agent-shell--held
    (setf (map-elt agent-shell--state :pending-prompts)
          (append drover-agent-shell--held (map-elt agent-shell--state :pending-prompts)))
    (setq drover-agent-shell--held nil)
    t))

(defun drover-agent-shell--held-turn-end (reason)
  "Note that the current shell's interrupted turn ended, for REASON.
REASON is the stop reason ACP told, or nil when its request failed.  On
the turn's end, before agent-shell decides about its queue: what it
holds is set aside too, and all of it queued again once agent-shell is
done with the turn (see `drover-agent-shell--release')."
  (when drover-agent-shell--holding
    (drover-agent-shell--hold)
    (setq drover-agent-shell--holding nil)
    (when drover-agent-shell--held
      (let ((buffer (current-buffer)))
        (run-at-time 0 nil (lambda ()
                             (drover-with-reporting "agent-shell queued prompts"
                               (drover-agent-shell--release buffer reason))))))))

(defun drover-agent-shell--held-failure ()
  "Note that a request of the current shell failed while its turn was interrupted.
Its turn may run on, the failure another request's: once agent-shell
reported it, from a timer, the prompts set aside are released only if
no turn runs any more."
  (when drover-agent-shell--holding
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil (lambda ()
                           (drover-with-reporting "agent-shell queued prompts"
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (when (and drover-agent-shell--holding (not (shell-maker-busy)))
                                   (setq drover-agent-shell--holding nil)
                                   (drover-agent-shell--release buffer nil))))))))))

(defun drover-agent-shell--release (buffer reason)
  "Queue again in shell BUFFER the prompts set aside, its turn ended for REASON.
A turn its agent ended (\"end_turn\") before the interrupt took sends
them, as agent-shell would; otherwise they wait in the queue, shown with
how to send or remove them, as agent-shell leaves them when told not to
send them.  A turn running again meanwhile takes them as it ends; one
interrupted again meanwhile keeps them aside until it ends."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (not drover-agent-shell--holding) (drover-agent-shell--requeue))
        (cond ((shell-maker-busy))
              ((equal reason "end_turn") (agent-shell--prompt-queue-process-next))
              (t (agent-shell--prompt-queue-display)))))))

(defun drover-agent-shell--mark-seen (session)
  "Mark the ended turn of SESSION seen."
  (drover-agent-shell--seen (drover-agent-shell--live-buffer session)))

(defun drover-agent-shell--preview (session callback)
  "Call CALLBACK with the end of the last output of SESSION's shell, or nil."
  (funcall callback
           (when-let* ((buffer (drover-session-buffer session))
                       (output (ignore-errors
                                 (with-current-buffer buffer (shell-maker-last-output))))
                       (output (string-trim (substring-no-properties output)))
                       ((not (string-empty-p output))))
             (if (> (length output) drover-agent-shell--preview-limit)
                 (concat "…" (substring output (- drover-agent-shell--preview-limit)))
               output))))

(defun drover-agent-shell--recap (session callback)
  "Call CALLBACK with a recap of the last turn of SESSION's shell.
It is what the shell's events told drover since it followed the shell:
the prompt submitted last, the agent's newest message since, the files
it wrote, their lines uncounted, and why the turn failed.  The backend's
:recap."
  (funcall callback
           (if-let* ((buffer (drover-session-buffer session)))
               (with-current-buffer buffer
                 (let ((reply (and drover-agent-shell--reply (string-trim drover-agent-shell--reply)))
                       (outcome drover-agent-shell--outcome))
                   (list :prompt drover-agent-shell--prompt
                         :reply (and reply (not (string-empty-p reply)) reply)
                         :changes (mapcar (lambda (file) (list file nil nil))
                                          (reverse drover-agent-shell--written))
                         :error (or (and (eq (plist-get outcome :status) 'failed)
                                         (plist-get outcome :detail))
                                    drover-agent-shell--start-error))))
             (list :failure "The shell is gone"))))

(defun drover-agent-shell--name-buffer (buffer name)
  "Name the shell of BUFFER NAME, made unique, and its viewport after it.
The viewport follows, as agent-shell finds a viewport by the shell's
name.  The name the shell takes is noted as the one drover gave it."
  (let ((viewport (agent-shell-viewport--buffer :shell-buffer buffer :existing-only t)))
    (shell-maker-set-buffer-name buffer name)
    (when (buffer-live-p viewport)
      (with-current-buffer viewport
        (rename-buffer (concat (buffer-name buffer) agent-shell-viewport--suffix) t))))
  (with-current-buffer buffer
    (setq drover-agent-shell--given-name (buffer-name))))

(defun drover-agent-shell--set-title (buffer title)
  "Name the shell of BUFFER and its session TITLE.
The buffer no longer follows the agent's title."
  (drover-agent-shell--name-buffer buffer title)
  (with-current-buffer buffer
    (setq drover-agent-shell--title title))
  (drover-backend-changed 'agent-shell (buffer-local-value 'drover-agent-shell--id buffer)))

(defun drover-agent-shell--default-name-p ()
  "Return non-nil when the current shell has the name agent-shell gives shells.
That is the name `agent-shell--format-buffer-name' makes of its agent
and its project, made unique as Emacs makes buffer names unique."
  (when-let* ((agent (map-nested-elt agent-shell--state '(:agent-config :buffer-name)))
              (project (drover-agent-shell--project-name))
              (name (ignore-errors (agent-shell--format-buffer-name agent project)))
              ((stringp name)))
    (string-match-p (concat "\\`" (regexp-quote name) "\\(?:<[0-9]+>\\)?\\'") (buffer-name))))

(defun drover-agent-shell--follow-title ()
  "Name the current shell after its session's title, unless the user named it.
It is named so while `drover-agent-shell-name-after-title' is non-nil,
the user gave the session no title and the buffer keeps the name
agent-shell or drover gave it: a name the user gave it stays.  The
title is the one drover lists the session under, cut to
`drover-agent-shell-name-limit' characters."
  (when-let* (((and drover-agent-shell-name-after-title
                    (not drover-agent-shell--title)
                    drover-agent-shell--given-name
                    (equal (buffer-name) drover-agent-shell--given-name)))
              (title (map-nested-elt agent-shell--state '(:session :title)))
              ((and (stringp title) (not (string-blank-p title))))
              (name (drover-bound-line (drover-agent-shell--display-title agent-shell--state)
                                       drover-agent-shell-name-limit))
              ;; Unique already, as when another buffer has the name.
              ((not (equal (generate-new-buffer-name name (buffer-name)) (buffer-name)))))
    (drover-agent-shell--name-buffer (current-buffer) (substring-no-properties name))))

(defun drover-agent-shell--rename (session title)
  "Rename the shell of SESSION to TITLE."
  (drover-agent-shell--set-title (drover-agent-shell--live-buffer session) title))

(defun drover-agent-shell--session-id (buffer)
  "Return the ACP session of the initialized shell of BUFFER, or nil."
  (when (and (buffer-live-p buffer) (drover-agent-shell--initialized-p buffer))
    (let ((id (map-nested-elt (buffer-local-value 'agent-shell--state buffer) '(:session :id))))
      (and (stringp id) id))))

(defun drover-agent-shell--acp-session (buffer)
  "Return the ACP session of the shell of BUFFER with its client, or nil.
The value is (CLIENT . SESSION-ID), what requests about the session
are sent with."
  (when-let* ((id (drover-agent-shell--session-id buffer))
              (client (map-elt (buffer-local-value 'agent-shell--state buffer) :client)))
    (cons client id)))

(defun drover-agent-shell--offers-command-p (buffer name)
  "Return non-nil when the agent of the shell of BUFFER offers command NAME.
Agents tell the slash commands they take as the session goes, such as
\"compact\"; a shell not initialized offers none."
  (and (drover-agent-shell--session-id buffer)
       (seq-some (lambda (command) (equal (map-elt command 'name) name))
                 (map-elt (buffer-local-value 'agent-shell--state buffer) :available-commands))
       t))

(defun drover-agent-shell--fork (session)
  "Fork SESSION whole into a new shell, which agent-shell displays.
It is agent-shell's own fork, through the ACP session/fork of agents
able to fork.  The backend's :fork."
  (with-current-buffer (drover-agent-shell--live-buffer session)
    (agent-shell-fork)))

(defun drover-agent-shell--compact (session)
  "Compact the conversation of SESSION, sending its agent /compact.
The command goes as a prompt, as ACP agents take their commands, queued
while the shell is busy.  The backend's :compact."
  (drover-agent-shell-send-text (drover-agent-shell--live-buffer session) "/compact"))

(defun drover-agent-shell--close (buffer)
  "Kill the shell of BUFFER without asking, as deleting and archiving do.
agent-shell shuts its agent down as the buffer goes."
  (drover-kill-buffer buffer))

(defun drover-agent-shell--unsent (buffer)
  "Return what the shell of BUFFER holds that its agent never got, as strings.
That is the input typed at its prompt, the prompts it queued, which a
turn ending sends but a failed or cancelled one leaves, those set aside
while its turn is interrupted, and a prompt composed in its viewport, or
kept there while it shows an answer."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer))
          (viewport (ignore-errors
                      (agent-shell-viewport--buffer :shell-buffer buffer :existing-only t))))
      (append (list (and process (buffer-substring-no-properties (process-mark process) (point-max))))
              drover-agent-shell--held
              (seq-filter #'stringp (map-elt agent-shell--state :pending-prompts))
              (list (and (buffer-live-p viewport)
                         (with-current-buffer viewport
                           (if (derived-mode-p 'agent-shell-viewport-edit-mode)
                               (buffer-substring-no-properties (point-min) (point-max))
                             (map-elt agent-shell-viewport--compose-snapshot :content)))))))))

(defun drover-agent-shell--close-session (session)
  "Close the shell of SESSION without asking, as archiving does.
Drover calls it once no turn runs, as closing ends the turn.  What the
agent never got goes to the kill ring first (see
`drover-agent-shell--unsent'); the value is non-nil when there was
some.  Its queued prompts kept, agent-shell has nothing to ask before
the shell goes.  The backend's :close."
  (when-let* ((buffer (drover-agent-shell--buffer session)))
    (prog1 (drover-keep-unsent (drover-agent-shell--unsent buffer) buffer)
      (with-current-buffer buffer
        (setq drover-agent-shell--held nil))
      (drover-agent-shell--close buffer))))

(defun drover-agent-shell--delete (session)
  "Delete SESSION for good, through its agent, then close its shell.
The agent deletes the session as ACP's session/delete asks, all it
keeps of it included, such as Claude Code's transcripts; an agent that
cannot is reported, and its shell kept.  The request is answered later:
the shell goes once the agent deleted the session.  Codex's agent
leaves the conversations of its subagents, which drover then deletes
with Codex's CLI (see `drover-codex-rollouts-delete-subagents').  The
backend's :delete."
  (let* ((buffer (drover-agent-shell--live-buffer session))
         (title (drover-session-display-title session))
         (acp (or (drover-agent-shell--acp-session buffer)
                  (user-error "%s has no session to delete yet" title)))
         (codex (and (eq (drover-session-engine session) 'codex)
                     (or (drover-session-engine-id session) (cdr acp)))))
    (acp-send-request
     :client (car acp)
     :request (acp-make-session-delete-request :session-id (cdr acp))
     :on-success (lambda (_response)
                   (drover-with-reporting "agent-shell delete"
                     (when (buffer-live-p buffer)
                       (drover-agent-shell--close buffer))
                     (when codex
                       (drover-codex-rollouts-delete-subagents codex title))))
     :on-failure (lambda (error &rest _)
                   (drover-with-reporting "agent-shell delete"
                     (drover-warn "The agent of %s did not delete its session (%s); its shell stays"
                                  title (or (and (consp error) (map-elt error 'message)) error)))))))

(defun drover-agent-shell--supports (session operation)
  "Return non-nil when OPERATION applies to SESSION.
Forking needs an agent able to fork, compacting one offering the
command \"compact\", deleting an initialized shell, whose agent tells
whether it can.  The backend's :supports."
  (let ((buffer (drover-session-buffer session)))
    (pcase operation
      (:fork (and (drover-agent-shell--session-id buffer)
                  (map-elt (buffer-local-value 'agent-shell--state buffer) :supports-session-fork)
                  t))
      (:compact (drover-agent-shell--offers-command-p buffer "compact"))
      (:delete (and (drover-agent-shell--acp-session buffer) t))
      (_ t))))

;;;; Shell buffers

(defun drover-agent-shell--changed ()
  "Report a change of the current shell to drover."
  (drover-backend-changed 'agent-shell drover-agent-shell--id))

(defun drover-agent-shell--killed ()
  "Report the current shell going away, and the models it no longer offers.
The models are told once the buffer is gone, as a dying shell is still
live."
  (drover-with-reporting "agent-shell"
    (drover-agent-shell--changed)
    (when-let* ((engine (ignore-errors (drover-agent-shell--engine))))
      (run-at-time 0 nil #'drover-models-changed engine))))

(defun drover-agent-shell--setup (&optional existing)
  "Follow the current shell and tell drover its session.
EXISTING is non-nil for a shell that ran before drover followed it,
whose creation time is unknown, and whose name may be the user's: its
buffer is named after its session's title only when it has the name
agent-shell gives shells (see `drover-agent-shell--follow-title')."
  (drover-with-reporting "agent-shell"
    (when (and (derived-mode-p 'agent-shell-mode) agent-shell--state)
      (unless (or existing drover-agent-shell--created)
        (setq drover-agent-shell--created (float-time)))
      (unless drover-agent-shell--given-name
        (setq drover-agent-shell--given-name
              (and (or (not existing) (drover-agent-shell--default-name-p))
                   (buffer-name))))
      (drover-agent-shell--follow-title)
      (when drover-agent-shell--subscription
        (ignore-errors (agent-shell-unsubscribe :subscription drover-agent-shell--subscription)))
      (setq drover-agent-shell--subscription
            (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                      :on-event #'drover-agent-shell--on-event))
      (setq-local drover-session-at-point-function #'drover-agent-shell--buffer-session)
      (add-hook 'kill-buffer-hook #'drover-agent-shell--killed nil t)
      (drover-agent-shell--brief-setup)
      (drover-agent-shell--changed)
      ;; A shell drover follows late may offer models already.
      (when existing
        (drover-agent-shell--models-changed)))))

(defun drover-agent-shell--teardown ()
  "Undo `drover-agent-shell--setup' in the current shell."
  (when drover-agent-shell--subscription
    (ignore-errors (agent-shell-unsubscribe :subscription drover-agent-shell--subscription))
    (setq drover-agent-shell--subscription nil))
  (when (eq drover-session-at-point-function #'drover-agent-shell--buffer-session)
    (kill-local-variable 'drover-session-at-point-function))
  (remove-hook 'kill-buffer-hook #'drover-agent-shell--killed t)
  (drover-agent-shell--brief-teardown))

;;;; The brief above the prompt, or after a request

(defvar-local drover-agent-shell--brief nil
  "The brief of this shell's session as last asked, (LINE . RECAP), or nil.")

(defvar-local drover-agent-shell--brief-unfolded nil
  "Non-nil when the recap of `drover-agent-shell--brief' shows.")

(defvar-local drover-agent-shell--brief-overlay nil
  "Overlay showing the brief as its string, or nil.")

(defvar-keymap drover-agent-shell-brief-map
  :doc "Keys of the brief above a shell's prompt, which a click unfolds."
  "<mouse-1>" #'drover-agent-shell-brief-click
  "<mouse-2>" #'drover-agent-shell-brief-click)

(defun drover-agent-shell--brief-at ()
  "Return where the brief shows, or nil.
The start of the live prompt's line while the shell is ready; the end
of the buffer, after the request, while its agent waits for a permission
or an answer (see `agent-shell-status'); nil while it works, and
without a prompt awaiting input."
  (pcase (ignore-errors (agent-shell-status))
    ('blocked (without-restriction (point-max)))
    ('ready
     (when-let* ((prompt comint-last-prompt)
                 ((markerp (car-safe prompt)))
                 ((ignore-errors (agent-shell--live-input-prompt-p prompt))))
       (save-excursion
         (without-restriction
           (goto-char (car prompt))
           (let ((inhibit-field-text-motion t))
             (line-beginning-position))))))))

(defun drover-agent-shell--brief-lead (at)
  "Return the newlines the brief shown at AT starts with, after a blank line."
  (cond ((= at (point-min)) "")
        ((/= (char-before at) ?\n) "\n\n")
        ((or (= (1- at) (point-min)) (/= (char-before (1- at)) ?\n)) "\n")
        (t "")))

(defun drover-agent-shell--brief-place (&rest _)
  "Keep the brief where `drover-agent-shell--brief-at' puts it, or nowhere.
Output and prompts coming and going move it, so this runs after every
change and before redisplay; without a brief it costs nothing."
  (when drover-agent-shell--brief
    (let ((at (drover-agent-shell--brief-at))
          (overlay drover-agent-shell--brief-overlay))
      (if (not at)
          (when (overlayp overlay)
            (delete-overlay overlay)
            (setq drover-agent-shell--brief-overlay nil))
        (unless (and (overlayp overlay) (overlay-buffer overlay))
          (setq overlay (make-overlay at at nil t t)
                drover-agent-shell--brief-overlay overlay)
          (overlay-put overlay 'drover-brief t)
          (overlay-put overlay 'drover-brief-body
                       (opencode-session-summary-text (car drover-agent-shell--brief)
                                                      (cdr drover-agent-shell--brief)
                                                      drover-agent-shell--brief-unfolded
                                                      drover-agent-shell-brief-map)))
        (unless (= (overlay-start overlay) at)
          (move-overlay overlay at at))
        (let ((lead (save-restriction (widen) (drover-agent-shell--brief-lead at))))
          (unless (equal lead (overlay-get overlay 'drover-brief-lead))
            (overlay-put overlay 'drover-brief-lead lead)
            (overlay-put overlay 'before-string
                         (concat lead (overlay-get overlay 'drover-brief-body)))))))))

(defun drover-agent-shell--brief-place-window (_window)
  "Keep the brief in place before redisplay, for `pre-redisplay-functions'."
  (drover-with-reporting "agent-shell brief"
    (drover-agent-shell--brief-place)))

(defun drover-agent-shell--brief-placed (&rest _)
  "Keep the brief in place after a change, for `after-change-functions'."
  (drover-with-reporting "agent-shell brief"
    (drover-agent-shell--brief-place)))

(defun drover-agent-shell--brief-show (brief &optional anew)
  "Show BRIEF, (LINE . RECAP) or nil, its recap folded when new.
ANEW draws it again even when it is the brief shown, as unfolding does."
  (unless (and (not anew) (equal brief drover-agent-shell--brief))
    (unless (equal brief drover-agent-shell--brief)
      (setq drover-agent-shell--brief-unfolded nil))
    (setq drover-agent-shell--brief brief)
    (when (overlayp drover-agent-shell--brief-overlay)
      (delete-overlay drover-agent-shell--brief-overlay)
      (setq drover-agent-shell--brief-overlay nil)))
  (drover-agent-shell--brief-place))

(defun drover-agent-shell--brief-refresh ()
  "Ask anew the brief of the current shell's session, and show it.
A brief on the whole session shows whole, without a recap to unfold."
  (let ((brief (drover-brief-of-buffer #'drover-agent-shell--brief-refresh)))
    (drover-agent-shell--brief-show (if (stringp brief) (cons brief nil) brief))))

(defun drover-agent-shell-toggle-brief-recap ()
  "Show or hide the recap below the brief this shell shows."
  (interactive)
  (unless (cdr drover-agent-shell--brief)
    (user-error "No recap of the last turn to show"))
  (setq drover-agent-shell--brief-unfolded (not drover-agent-shell--brief-unfolded))
  (drover-agent-shell--brief-show drover-agent-shell--brief t))

(defun drover-agent-shell-brief-click (event)
  "Show or hide the recap of the brief clicked.
EVENT is the click."
  (interactive "e")
  (let ((window (posn-window (event-end event))))
    (when (windowp window)
      (with-current-buffer (window-buffer window)
        (drover-agent-shell-toggle-brief-recap)))))

(defun drover-agent-shell--brief-setup ()
  "Show the brief of this shell's session, before its prompt or at its end."
  (add-hook 'after-change-functions #'drover-agent-shell--brief-placed nil t)
  (add-hook 'pre-redisplay-functions #'drover-agent-shell--brief-place-window nil t)
  (drover-agent-shell--brief-refresh))

(defun drover-agent-shell--brief-teardown ()
  "Undo `drover-agent-shell--brief-setup' in the current shell."
  (remove-hook 'after-change-functions #'drover-agent-shell--brief-placed t)
  (remove-hook 'pre-redisplay-functions #'drover-agent-shell--brief-place-window t)
  (drover-brief-forget (current-buffer))
  (setq drover-agent-shell--brief nil)
  (when (overlayp drover-agent-shell--brief-overlay)
    (delete-overlay drover-agent-shell--brief-overlay)
    (setq drover-agent-shell--brief-overlay nil)))

;;;; Registering and installing

(defun drover-agent-shell--caches ()
  "Return non-nil when shells may tell prompt caches now.
Those of Claude Code's conversations do, as their transcripts tell them:
see `drover-recorded-run'.  The backend's :caches."
  (drover-backend-caches-p 'claude-code))

(defvar drover-agent-shell--gemini-registered nil
  "Non-nil when this file registered the `gemini-cli' engine.")

(defun drover-agent-shell--register ()
  "Register the `agent-shell' backend, `gemini-cli' engine and model sources."
  (drover-register-backend
   'agent-shell
   :label "agent-shell"
   :caches #'drover-agent-shell--caches
   :sessions #'drover-agent-shell--sessions
   :open #'drover-agent-shell--open
   :respond #'drover-agent-shell--respond
   :interrupt #'drover-agent-shell--interrupt
   :mark-seen #'drover-agent-shell--mark-seen
   :preview #'drover-agent-shell--preview
   :recap #'drover-agent-shell--recap
   :new #'drover-agent-shell--new
   :buffer #'drover-agent-shell--buffer
   :close #'drover-agent-shell--close-session
   :runs-in-buffer t
   :rename #'drover-agent-shell--rename
   :fork #'drover-agent-shell--fork
   :compact #'drover-agent-shell--compact
   :delete #'drover-agent-shell--delete
   :supports #'drover-agent-shell--supports
   :engines #'drover-agent-shell--engines
   :create #'drover-agent-shell--create
   :apply-options #'drover-agent-shell--apply-options
   :send #'drover-agent-shell--send
   :options #'drover-agent-shell--options
   :settable #'drover-agent-shell--settable
   :blocker #'drover-agent-shell--blocker
   :submit #'drover-agent-shell--deliver)
  ;; Another package may describe Gemini CLI better: keep its registration.
  (unless (memq 'gemini-cli (drover-engines))
    (drover-register-engine 'gemini-cli :label "Gemini CLI")
    (setq drover-agent-shell--gemini-registered t))
  (pcase-dolist (`(,engine ,depth ,function) drover-agent-shell--model-sources)
    (drover-register-model-source engine function depth t))
  (pcase-dolist (`(,engine . ,function) (drover-agent-shell--resumers))
    (drover-register-resumer engine 'agent-shell function))
  (drover-claude-code-transcripts-install 'agent-shell)
  (drover-codex-rollouts-install 'agent-shell))

(defun drover-agent-shell--unregister ()
  "Undo `drover-agent-shell--register'."
  (pcase-dolist (`(,engine ,_depth ,function) drover-agent-shell--model-sources)
    (drover-unregister-model-source engine function))
  (pcase-dolist (`(,engine . ,_) drover-agent-shell-configs)
    (drover-unregister-resumer engine 'agent-shell))
  (drover-claude-code-transcripts-uninstall 'agent-shell)
  (drover-codex-rollouts-uninstall 'agent-shell)
  (when drover-agent-shell--gemini-registered
    (drover-unregister-engine 'gemini-cli)
    (setq drover-agent-shell--gemini-registered nil))
  (drover-unregister-backend 'agent-shell))

(defun drover-agent-shell--install ()
  "Follow agent-shell's shells, existing and future, and where the user looks."
  (add-hook 'agent-shell-mode-hook #'drover-agent-shell--setup)
  (dolist (buffer (drover-agent-shell--shells))
    (with-current-buffer buffer
      (drover-agent-shell--setup t)))
  (add-hook 'window-selection-change-functions #'drover-agent-shell--window-changed)
  (add-hook 'window-buffer-change-functions #'drover-agent-shell--window-changed)
  (add-function :after after-focus-change-function #'drover-agent-shell--window-changed))

(defun drover-agent-shell--uninstall ()
  "Undo `drover-agent-shell--install'."
  (remove-hook 'agent-shell-mode-hook #'drover-agent-shell--setup)
  (dolist (buffer (drover-agent-shell--shells))
    (with-current-buffer buffer
      (drover-agent-shell--teardown)))
  (remove-hook 'window-selection-change-functions #'drover-agent-shell--window-changed)
  (remove-hook 'window-buffer-change-functions #'drover-agent-shell--window-changed)
  (remove-function after-focus-change-function #'drover-agent-shell--window-changed))

(defun drover-agent-shell--activate ()
  "Load agent-shell, register the backend and follow agent-shell's shells.
agent-shell is loaded here rather than when this file compiles, which
drover does without agent-shell; `drover' loads this file once
agent-shell is loaded."
  (require 'agent-shell)
  (drover-agent-shell--register)
  (drover-agent-shell--install))

(drover-agent-shell--activate)

(defun drover-agent-shell-unload-function ()
  "Forget the `agent-shell' backend and stop following agent-shell's shells."
  (drover-agent-shell--uninstall)
  (drover-agent-shell--unregister)
  nil)

(provide 'drover-agent-shell)
;;; drover-agent-shell.el ends here
