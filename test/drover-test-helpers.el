;;; drover-test-helpers.el --- Shared test helpers for drover  -*- lexical-binding: t; -*-

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

;; A fake backend, `fake', serves `drover-test-sessions', one by one
;; through :session as well as all at once, and records
;; every action in `drover-test-calls', its prompt operations included:
;; it starts sessions of the engines `drover-test-backend-engines'.  What
;; views ask it to fetch goes to `drover-test-prefetched', the recaps
;; they ask for to `drover-test-recap-calls'.
;; `drover-test-with-backend' isolates the registry, the change hook and
;; its timer, and the view settings.  `drover-test-register-simple'
;; registers more backends, such as a second front end showing the same
;; conversations.
;;
;; Fake engines (`drover-test-register-engine') answer from
;; `drover-test-engine-defaults', `drover-test-engine-agents' and
;; `drover-test-engine-last-used', and record their calls in
;; `drover-test-engine-calls'; `drover-test-with-engines' isolates the
;; engine and model source registries.
;;
;; `drover-test-with-homes' points every engine's local state at a
;; fresh directory, so tests never read the user's;
;; `drover-test-with-state' does drover's own.

;;; Code:

(require 'ert)
(require 'drover)
(require 'drover-model)
(require 'drover-claude-code)
(require 'drover-codex)
(require 'drover-claude-code-transcripts)
(require 'drover-codex-rollouts)
(require 'drover-command)

;; No test may touch the user's state.  Models are not remembered unless
;; a test asks for it, so that tests of sources see their answers alone,
;; and the state directory is a temporary one.
(setq drover-remember-models nil
      drover-state-directory (make-temp-file "drover-test-state" t))
(let ((directory drover-state-directory))
  (add-hook 'kill-emacs-hook (lambda () (delete-directory directory t))))

;; Nor the conversations the engines recorded: their homes are empty
;; directories, but in the tests binding others.
(let ((home (make-temp-file "drover-test-home" t)))
  (setq drover-claude-code-home (expand-file-name "claude" home)
        drover-codex-home (expand-file-name "codex" home)
        drover-claude-code-agents-interval nil)
  (add-hook 'kill-emacs-hook (lambda () (delete-directory home t))))

;;;; The fake backend

(defvar drover-test-sessions nil
  "Sessions the fake backend serves.")

(defvar drover-test-calls nil
  "Actions the fake backend received, newest first, as (OPERATION ID . ARGUMENTS).
Its :create is recorded as (:create DIRECTORY ENGINE OPTIONS).")

(defvar drover-test-backend-engines '(fake)
  "Engines whose sessions the fake backend starts, its :engines.")

(defvar drover-test-create-error nil
  "Message the fake backend's :create fails with, or nil to succeed.")

(defvar drover-test-check-result 'fake-token
  "What the fake backend's :check returns: `cancel', or a token for :send.")

(defvar drover-test-options nil
  "What the fake sessions run, as an alist (ID . PLIST), for :options.")

(defvar drover-test-prefetched nil
  "Sessions the fake backend was asked to complete, by :prefetch, newest call first.
Each entry lists the identifiers of one call, in order.")

(defvar drover-test-recaps nil
  "Recaps of the fake sessions, as an alist (ID . RECAP), for :recap.
A session missing here recaps a prompt \"Prompt of ID\" and a reply
\"Reply of ID\".")

(defvar drover-test-recap-calls nil
  "Sessions whose recap the fake backend was asked for, newest first.")

(defvar drover-test-recap-later nil
  "Non-nil when the fake backend's :recap answers later.
Its callbacks then go to `drover-test-recap-pending', newest first, as
\(ID . CALLBACK).")

(defvar drover-test-recap-pending nil
  "Callbacks of :recap not answered yet, as (ID . CALLBACK), newest first.")

(defun drover-test--recap (session callback)
  "Call CALLBACK with the recap of SESSION, as :recap does, now or later."
  (let ((id (drover-session-id session)))
    (push id drover-test-recap-calls)
    (if drover-test-recap-later
        (push (cons id callback) drover-test-recap-pending)
      (funcall callback (if-let* ((entry (assoc id drover-test-recaps)))
                            (cdr entry)
                          (list :prompt (concat "Prompt of " id) :reply (concat "Reply of " id)))))))

(defvar drover-test-settable nil
  "What the fake sessions can change, as an alist (ID . KEYWORDS).
It answers :settable; a session missing here can change all three.")

(defconst drover-test-now 1800000000.0
  "A fixed current time for tests.")

(defun drover-test-session (id &rest properties)
  "Return a fake session ID with PROPERTIES, keyword arguments of `drover-session-create'.
The title defaults to \"Session ID\"."
  (apply #'drover-session-create
         (append properties (list :backend 'fake :id id :title (concat "Session " id)))))

(defun drover-test-tree-shape (nodes)
  "Return trees NODES as nested lists of session identifiers."
  (mapcar (lambda (node)
            (cons (drover-session-id (drover-node-session node))
                  (drover-test-tree-shape (drover-node-children node))))
          nodes))

(defun drover-test--recorder (operation)
  "Return a fake backend function recording OPERATION."
  (lambda (session &rest arguments)
    (push (append (list operation (drover-session-id session)) arguments) drover-test-calls)))

(defun drover-test--create (directory engine options then else)
  "Start a fake session of ENGINE in DIRECTORY with OPTIONS, as :create does.
The session is served from then on, and given to THEN; with
`drover-test-create-error', ELSE gets that message instead."
  (push (list :create directory engine options) drover-test-calls)
  (if drover-test-create-error
      (funcall else drover-test-create-error)
    (let ((session (drover-test-session (format "new-%d" (length drover-test-sessions))
                                        :directory directory :engine engine
                                        :title (plist-get options :title))))
      (setq drover-test-sessions (append drover-test-sessions (list session)))
      (funcall then session))))

(defun drover-test--check (session text options)
  "Record the checks of TEXT and OPTIONS for SESSION, as :check does."
  (push (list :check (drover-session-id session) text options) drover-test-calls)
  drover-test-check-result)

(defun drover-test--apply-options (session options then)
  "Record OPTIONS applied to SESSION, then call THEN, as :apply-options does."
  (push (list :apply-options (drover-session-id session) options) drover-test-calls)
  (funcall then))

(defun drover-test-register ()
  "Register the fake backend."
  (drover-register-backend
   'fake
   :label "Fake"
   :sessions (lambda () drover-test-sessions)
   :session (lambda (id)
              (seq-find (lambda (session) (equal (drover-session-id session) id))
                        drover-test-sessions))
   :start (lambda () (push '(:start nil) drover-test-calls))
   :open (drover-test--recorder :open)
   :respond (drover-test--recorder :respond)
   :interrupt (drover-test--recorder :interrupt)
   :archive (drover-test--recorder :archive)
   :mark-seen (drover-test--recorder :mark-seen)
   :rename (drover-test--recorder :rename)
   :delete (drover-test--recorder :delete)
   :fork (drover-test--recorder :fork)
   :compact (drover-test--recorder :compact)
   :new (lambda (directory) (push (list :new directory) drover-test-calls))
   :preview (lambda (session callback)
              (push (list :preview (drover-session-id session)) drover-test-calls)
              (funcall callback (concat "Preview of " (drover-session-id session))))
   :engines (lambda () drover-test-backend-engines)
   :create #'drover-test--create
   :check #'drover-test--check
   :apply-options #'drover-test--apply-options
   :send (drover-test--recorder :send)
   :options (lambda (session) (alist-get (drover-session-id session) drover-test-options
                                         nil nil #'equal))
   :recap #'drover-test--recap
   :prefetch (lambda (sessions)
               (push (mapcar #'drover-session-id sessions) drover-test-prefetched))
   :settable (lambda (session)
               (if-let* ((entry (assoc (drover-session-id session) drover-test-settable)))
                   (cdr entry)
                 '(:agent :model :variant)))))

(defun drover-test-register-simple (name sessions &rest properties)
  "Register backend NAME serving SESSIONS, a list or a function returning one.
PROPERTIES are more properties of `drover-register-backend', such as
:buffer.  The label is NAME, capitalized."
  (apply #'drover-register-backend name
         :label (capitalize (symbol-name name))
         :sessions (if (functionp sessions) sessions (lambda () sessions))
         properties))

(defmacro drover-test-with-backend (sessions &rest body)
  "Run BODY with the fake backend alone registered, serving SESSIONS."
  (declare (indent 1) (debug t))
  `(let ((drover--backends nil)
         (drover--started nil)
         (drover--running nil)
         (drover--change-timer nil)
         (drover--changed-keys nil)
         (drover--changed-backends nil)
         (drover--changed-all nil)
         (drover--changed-applied nil)
         (drover--index nil)
         (drover--activity-times (make-hash-table :test #'equal))
         (drover-sort-activity-tolerance 60)
         (drover-changed-hook nil)
         (drover--closing nil)
         (drover-show-subagents 'active)
         (drover-filter nil)
         ;; Not the defaults: most tests read the groups of statuses and
         ;; the order of urgency, and say so here once.
         (drover-group-by 'status)
         (drover-sort-by 'urgency)
         (drover-sort-descending nil)
         (drover-show-history nil)
         (drover--history nil)
         (drover-show-archived nil)
         (drover--archived nil)
         (drover--fetched-layout (drover-layout 'drover-session))
         (drover--archive nil)
         (drover--archive-stamp 'unread)
         (drover-test-sessions ,sessions)
         (drover-test-calls nil)
         (drover-test-backend-engines '(fake))
         (drover-test-create-error nil)
         (drover-test-check-result 'fake-token)
         (drover-test-options nil)
         (drover-test-prefetched nil)
         (drover-test-recaps nil)
         (drover-test-recap-calls nil)
         (drover-test-recap-later nil)
         (drover-test-recap-pending nil)
         (drover-test-settable nil))
     (unwind-protect
         (progn (drover-test-register) ,@body)
       (when (timerp drover--change-timer)
         (cancel-timer drover--change-timer)))))

;;;; Drover's state

(defmacro drover-test-with-state (&rest body)
  "Run BODY with `drover-state-directory' a fresh directory, deleted afterwards.
What drover archived itself is read from there anew, and its warnings
are given again."
  (declare (indent 0) (debug t))
  `(let ((drover-state-directory (file-name-as-directory (make-temp-file "drover-state" t)))
         (drover--archive nil)
         (drover--archive-stamp 'unread)
         (drover--archive-warned nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory drover-state-directory t))))

;;;; Fake engines

(defvar drover-test-engine-calls nil
  "Calls the fake engines received, newest first.
Each is (OPERATION ENGINE . ARGUMENTS).")

(defvar drover-test-engine-defaults nil
  "Defaults data of the fake engines, as an alist (ENGINE . DEFAULTS).
An engine missing here does not call back.")

(defvar drover-test-engine-agents nil
  "Agents of the fake engines, as an alist (ENGINE . NAMES).")

(defvar drover-test-engine-last-used nil
  "When the fake engines last took a prompt, as an alist (ENGINE . TIME).")

(defun drover-test-register-engine (name &rest properties)
  "Register NAME as a fake engine, with PROPERTIES over the fake ones.
Its label is NAME, capitalized.  Its :defaults records (:defaults NAME
DIRECTORY) and calls back at once with NAME's entry of
`drover-test-engine-defaults', if any; its :agents records (:agents
NAME DIRECTORY) and returns NAME's entry of
`drover-test-engine-agents'; its :last-used returns NAME's entry of
`drover-test-engine-last-used'.  It has no :resolve unless PROPERTIES
give one, so it runs the choices as they are."
  (let ((fake (list :label (capitalize (symbol-name name))
                    :defaults (lambda (directory callback)
                                (push (list :defaults name directory) drover-test-engine-calls)
                                (when-let* ((entry (assq name drover-test-engine-defaults)))
                                  (funcall callback (cdr entry))))
                    :agents (lambda (directory)
                              (push (list :agents name directory) drover-test-engine-calls)
                              (alist-get name drover-test-engine-agents))
                    :last-used (lambda () (alist-get name drover-test-engine-last-used)))))
    (while properties
      (setq fake (plist-put fake (pop properties) (pop properties))))
    (apply #'drover-register-engine name fake)))

(defmacro drover-test-with-engines (&rest body)
  "Run BODY with no engine nor model source registered but those BODY registers.
No model is remembered, nor read from the file of remembered models
until BODY reads it."
  (declare (indent 0) (debug t))
  `(let ((drover--engines nil)
         (drover--model-sources nil)
         (drover--model-memory nil)
         (drover--model-memory-stamp 'unread)
         (drover--model-memory-warned nil)
         (drover--json-cache (make-hash-table :test #'equal))
         (drover-test-engine-calls nil)
         (drover-test-engine-defaults nil)
         (drover-test-engine-agents nil)
         (drover-test-engine-last-used nil))
     ,@body))

(defun drover-test-model (id &rest properties)
  "Return a `drover-model' ID with PROPERTIES.
PROPERTIES are keyword arguments of `drover-model-create'; the engines
default to (fake)."
  (apply #'drover-model-create (append properties (list :id id :engines '(fake)))))

;;;; Engine homes

(defconst drover-test-claude-catalog
  "{\"version\": 2, \"fetchedAt\": 1793029978843, \"staleAt\": 1793033473038,
    \"catalog\": {\"surface\": \"cc\",
     \"config\": {\"id\": \"cc\", \"models\": [
       {\"id\": \"claude-opus-5-5\", \"name\": \"Opus 5.5\", \"short_name\": \"Opus\", \"section\": \"main\",
        \"thinking\": {\"type\": \"effort\", \"effort_options\": [
          {\"id\": \"low\", \"name\": \"Low\"},
          {\"id\": \"medium\", \"name\": \"Medium\", \"badge\": {\"message\": \"Recommended\", \"variant\": \"neutral\"}},
          {\"id\": \"high\", \"name\": \"High\"}, {\"id\": \"xhigh\", \"name\": \"Extra\"}]},
        \"runtime\": {\"max_input_tokens\": 1000000, \"effort_levels\": [\"low\", \"medium\", \"high\", \"xhigh\", \"max\"],
                    \"default_effort\": \"medium\", \"family\": \"opus\"}},
       {\"id\": \"claude-opus-5\", \"name\": \"Opus 5\", \"short_name\": \"Opus\", \"section\": \"main\",
        \"thinking\": {\"type\": \"effort\", \"effort_options\": [
          {\"id\": \"low\", \"name\": \"Low\"}, {\"id\": \"medium\", \"name\": \"Medium\"},
          {\"id\": \"high\", \"name\": \"High\", \"badge\": {\"message\": \"Recommended\", \"variant\": \"neutral\"}},
          {\"id\": \"xhigh\", \"name\": \"Extra\"}]},
        \"runtime\": {\"effort_levels\": [\"low\", \"medium\", \"high\", \"xhigh\"], \"default_effort\": \"high\",
                    \"family\": \"opus\"}},
       {\"id\": \"claude-sonnet-5-5\", \"name\": \"Sonnet 5.5\", \"short_name\": \"Sonnet\", \"section\": \"main\",
        \"badge\": {\"message\": \"Org default\", \"variant\": \"neutral\"},
        \"thinking\": {\"type\": \"effort\", \"effort_options\": [
          {\"id\": \"low\", \"name\": \"Low\"},
          {\"id\": \"medium\", \"name\": \"Medium\", \"admin_default\": true,
           \"badge\": {\"message\": \"Default\", \"variant\": \"neutral\"}},
          {\"id\": \"high\", \"name\": \"High\"}]},
        \"runtime\": {\"effort_levels\": [\"low\", \"medium\", \"high\"], \"default_effort\": \"medium\",
                    \"family\": \"sonnet\"}},
       {\"id\": \"claude-haiku-4-5-20251001\", \"name\": \"Haiku 4.5\", \"short_name\": \"Haiku\", \"section\": \"main\",
        \"auto_permission_mode\": \"unavailable\"}],
      \"provider_alias_targets\": {
        \"haiku\": {\"default\": \"claude-haiku-4-5-20251001\"},
        \"opus\": {\"default\": \"claude-opus-5-5\",
                 \"per_provider\": {\"bedrock\": \"claude-opus-5\", \"vertex\": \"claude-opus-5-5\"}},
        \"sonnet\": {\"default\": \"claude-sonnet-5-5\"}},
      \"settings_vocabulary\": {\"effort_level\": {\"high\": 3, \"low\": 1, \"max\": 5, \"medium\": 2, \"xhigh\": 4}}},
     \"state\": {\"id\": \"cc\", \"model\": \"claude-sonnet-5-5\", \"selection_source\": \"org_configuration\",
               \"thinking\": {\"type\": \"effort\", \"effort\": \"medium\"},
               \"thinking_by_model\": [
                 {\"id\": \"claude-opus-5-5\", \"thinking\": {\"type\": \"effort\", \"effort\": \"medium\"}},
                 {\"id\": \"claude-opus-5\", \"thinking\": {\"type\": \"effort\", \"effort\": \"high\"}}]}}}"
  "A model catalog as Claude Code 2.1.316 caches it, trimmed.
Its models tell their effort levels twice: those of the picker,
`thinking.effort_options', and those the CLI runs them at,
`runtime.effort_levels', which hold max for Opus 5.5 where the picker
does not.  `provider_alias_targets' maps aliases to identifiers, Bedrock
to its own Opus; the state serves Sonnet 5.5 as the default.")

(defun drover-test-write-claude-catalog (&optional content)
  "Write CONTENT, by default `drover-test-claude-catalog', as Claude Code's catalog."
  (drover-test-write (expand-file-name "cache/model-catalog/org-cc.json" drover-claude-code-home)
                     (or content drover-test-claude-catalog)))

(defconst drover-test-claude-acp-models
  '(("default" . "Default (recommended)") ("opus" . "Opus 5.5")
    ("opus[1m]" . "Opus 5.5 (1M context)") ("sonnet" . "Sonnet 5.5") ("haiku" . "Haiku 4.5"))
  "The models Claude Code's ACP agent lists, as (VALUE . NAME).
The values of its model option, as claude-agent-acp 0.81 builds it from
the models the SDK supports: aliases, \"default\" first, names given
their version.")

(defconst drover-test-codex-models-cache
  "{\"fetched_at\": \"2026-09-18T21:02:56.909415Z\", \"client_version\": \"0.155.0\",
    \"models\": [
     {\"slug\": \"gpt-5.5\", \"display_name\": \"GPT-5.5\", \"visibility\": \"list\",
      \"default_reasoning_level\": \"xhigh\",
      \"supported_reasoning_levels\": [{\"effort\": \"low\", \"description\": \"Fast\"},
                                     {\"effort\": \"medium\"}, {\"effort\": \"high\"},
                                     {\"effort\": \"xhigh\"}]},
     {\"slug\": \"gpt-6-astra\", \"display_name\": \"GPT-6-Astra\", \"visibility\": \"list\",
      \"default_reasoning_level\": \"low\",
      \"supported_reasoning_levels\": [{\"effort\": \"low\"}, {\"effort\": \"medium\"},
                                     {\"effort\": \"high\"}, {\"effort\": \"xhigh\"},
                                     {\"effort\": \"max\"}, {\"effort\": \"ultra\"}]}]}"
  "Codex's models cache, models_cache.json, as Codex 0.155 writes it, trimmed.")

(defun drover-test-write (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defun drover-test-wait-commands (&optional seconds)
  "Wait for the commands of `drover-command-run' to end, SECONDS at most.
SECONDS defaults to 10.  Return non-nil when they all ended."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (> (hash-table-count drover-command--jobs) 0)
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (zerop (hash-table-count drover-command--jobs))))

(defun drover-test-lag (function)
  "Call FUNCTION, and return the longest Emacs went meanwhile without running timers.
In seconds: a timer ticks every hundredth of a second, so that a call
making Emacs wait, a process starting or a command running, shows."
  (let* ((last (float-time))
         (lag 0)
         (timer (run-at-time 0 0.01 (lambda ()
                                      (let ((now (float-time)))
                                        (setq lag (max lag (- now last))
                                              last now))))))
    (unwind-protect
        (progn (funcall function)
               (max lag (- (float-time) last)))
      (cancel-timer timer))))

(defmacro drover-test-with-slow-starts (&rest body)
  "Run BODY with every process Emacs starts taking a second to start.
As on a managed Mac, where security agents hold each program's execution
while `posix_spawn' makes Emacs wait.  Advising `make-process' may
compile a trampoline for it first, which takes a while: what is timed
runs within BODY."
  (declare (indent 0) (debug t))
  `(let ((slow (lambda (start &rest arguments)
                 ;; Not `sleep-for', which runs timers meanwhile.
                 (let ((end (+ (float-time) 1)))
                   (while (< (float-time) end)))
                 (apply start arguments))))
     (advice-add 'make-process :around slow)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'make-process slow))))

(defmacro drover-test-with-homes (&rest body)
  "Run BODY with the local state of every engine under a fresh directory.
The directory is bound to `home' in BODY, and deleted afterwards.
`drover-claude-code-home' is its \"claude\", the managed settings
`managed', `drover-codex-home' \"codex\", `drover-opencode-database'
\"opencode/opencode.db\" and `drover-state-directory' \"state\";
Claude Code gets no start flags, and starts in the directory asked
for.  Environment variables overriding them are cleared, the Windows
registry reads nothing, and cached JSON is forgotten."
  (declare (indent 0) (debug t))
  `(let ((home (file-name-as-directory (make-temp-file "drover-homes" t))))
     ;; `dlet': `drover-opencode-database' may not be defined yet.
     (dlet ((drover-claude-code-home (expand-file-name "claude" home))
            (drover-claude-code-managed-directory (expand-file-name "managed" home))
            (drover-state-directory (expand-file-name "state" home))
            (drover-codex-home (expand-file-name "codex" home))
            (drover-opencode-database (expand-file-name "opencode/opencode.db" home))
            (drover-claude-code-start-flags-function (lambda () ""))
            (drover-claude-code-project-directory-function nil)
            (drover--json-cache (make-hash-table :test #'equal))
            (process-environment
             (seq-remove (lambda (entry)
                           (string-match-p
                            "\\`\\(?:ANTHROPIC_\\|CLAUDE_CODE_EFFORT_LEVEL=\\|CLAUDE_CODE_USE_\\|CLAUDE_CONFIG_DIR=\\|CODEX_HOME=\\|XDG_DATA_HOME=\\)"
                            entry))
                         process-environment)))
       (unwind-protect
           (cl-letf (((symbol-function 'w32-read-registry) #'ignore))
             ,@body)
         (delete-directory home t)))))

;;;; What prints

;; Emacs prints a value whole in an error's data, in `format', in a
;; value evaluated; its printer, without `print-circle', prints a
;; structure again from every path reaching it.  A structure leading
;; back to what holds it prints with the square of what that holds.

(defun drover-test-printed-length (object limit)
  "Return how many characters `prin1' writes for OBJECT, or nil past LIMIT.
It prints whole, as Emacs prints an error's data, and stops at LIMIT."
  (let ((count 0)
        (print-length nil)
        (print-level nil)
        (print-circle nil))
    (catch 'drover-test-printed-length
      (prin1 object (lambda (_char)
                      (when (> (setq count (1+ count)) limit)
                        (throw 'drover-test-printed-length nil))))
      count)))

(defconst drover-test-kept-regexp "\\`\\(?:drover\\|spyglass\\|magit-root-section\\'\\)"
  "What the names of the variables holding drover's state match.")

(defun drover-test-kept ()
  "Return what drover keeps, as (NAME . OBJECT).
Those are the values of the variables `drover-test-kept-regexp' matches,
global and local to any buffer, the timers running functions other than
Emacs's own, and the filters, sentinels and properties of processes."
  (let ((kept nil))
    (mapatoms (lambda (symbol)
                (when (and (boundp symbol)
                           (not (keywordp symbol))
                           (symbol-value symbol)
                           (string-match-p drover-test-kept-regexp (symbol-name symbol)))
                  (push (cons (symbol-name symbol) (symbol-value symbol)) kept))))
    (dolist (buffer (buffer-list))
      (pcase-dolist (`(,variable . ,value) (seq-filter #'consp (buffer-local-variables buffer)))
        (when (and value (string-match-p drover-test-kept-regexp (symbol-name variable)))
          (push (cons (format "%s in %s" variable (buffer-name buffer)) value) kept))))
    (dolist (timer (append timer-list timer-idle-list))
      (push (cons (format "a timer of %S" (let ((function (timer--function timer)))
                                            (if (symbolp function) function (type-of function))))
                  timer)
            kept))
    (dolist (process (process-list))
      (push (cons (format "process %s" (process-name process))
                  (list (process-filter process) (process-sentinel process) (process-plist process)))
            kept))
    kept))

(defun drover-test-prints-in-proportion (sessions &optional objects)
  "Check that what drover keeps prints in proportion to SESSIONS, a count.
Each value `drover-test-kept' returns prints within 10 kB a session, and
each of OBJECTS, an alist (NAME . OBJECT) of things held once a session,
such as a record or a row, within 20 kB whatever the sessions: one
leading back to what holds it would print it all."
  (let ((limit (+ 65536 (* 10000 sessions))))
    (pcase-dolist (`(,name . ,object) (drover-test-kept))
      (ert-info ((format "%s, for %d sessions" name sessions))
        (should (drover-test-printed-length object limit)))))
  (pcase-dolist (`(,name . ,object) objects)
    (ert-info ((format "%s, alone" name))
      (should object)
      (should (drover-test-printed-length object 20000)))))

(provide 'drover-test-helpers)
;;; drover-test-helpers.el ends here
