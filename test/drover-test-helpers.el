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

;; A fake backend, `fake', serves `drover-test-sessions' and records
;; every action in `drover-test-calls', its prompt operations included:
;; it starts sessions of the engines `drover-test-backend-engines'.  What
;; views ask it to fetch goes to `drover-test-prefetched'.
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
;; fresh directory, so tests never read the user's.

;;; Code:

(require 'ert)
(require 'drover)
(require 'drover-model)
(require 'drover-claude-code)
(require 'drover-codex)

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
         (drover-changed-hook nil)
         (drover-show-subagents 'active)
         (drover-filter nil)
         (drover-group-by 'status)
         (drover-sort-by 'urgency)
         (drover-sort-descending nil)
         (drover-show-history nil)
         (drover--history nil)
         (drover-test-sessions ,sessions)
         (drover-test-calls nil)
         (drover-test-backend-engines '(fake))
         (drover-test-create-error nil)
         (drover-test-check-result 'fake-token)
         (drover-test-options nil)
         (drover-test-prefetched nil)
         (drover-test-settable nil))
     (unwind-protect
         (progn (drover-test-register) ,@body)
       (when (timerp drover--change-timer)
         (cancel-timer drover--change-timer)))))

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
  "Run BODY with no engine nor model source registered but those BODY registers."
  (declare (indent 0) (debug t))
  `(let ((drover--engines nil)
         (drover--model-sources nil)
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

(defun drover-test-write (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert content)))

(defmacro drover-test-with-homes (&rest body)
  "Run BODY with the local state of every engine under a fresh directory.
The directory is bound to `home' in BODY, and deleted afterwards.
`drover-claude-code-home' is its \"claude\", the managed settings
`managed', `drover-codex-home' \"codex\" and `drover-opencode-database'
\"opencode/opencode.db\"; Claude Code gets no start flags, and starts
in the directory asked for.  Environment variables overriding them are
cleared, the Windows registry reads nothing, and cached JSON is
forgotten."
  (declare (indent 0) (debug t))
  `(let ((home (file-name-as-directory (make-temp-file "drover-homes" t))))
     ;; `dlet': `drover-opencode-database' may not be defined yet.
     (dlet ((drover-claude-code-home (expand-file-name "claude" home))
            (drover-claude-code-managed-directory (expand-file-name "managed" home))
            (drover-codex-home (expand-file-name "codex" home))
            (drover-opencode-database (expand-file-name "opencode/opencode.db" home))
            (drover-claude-code-start-flags-function (lambda () ""))
            (drover-claude-code-project-directory-function nil)
            (drover--json-cache (make-hash-table :test #'equal))
            (process-environment
             (seq-remove (lambda (entry)
                           (string-match-p
                            "\\`\\(?:ANTHROPIC_\\|CLAUDE_CODE_EFFORT_LEVEL=\\|CLAUDE_CONFIG_DIR=\\|CODEX_HOME=\\|XDG_DATA_HOME=\\)"
                            entry))
                         process-environment)))
       (unwind-protect
           (cl-letf (((symbol-function 'w32-read-registry) #'ignore))
             ,@body)
         (delete-directory home t)))))

(provide 'drover-test-helpers)
;;; drover-test-helpers.el ends here
