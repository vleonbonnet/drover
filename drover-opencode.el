;;; drover-opencode.el --- OpenCode sessions in drover  -*- lexical-binding: t; -*-

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

;; The `opencode' backend: the sessions opencode.el tracks in its
;; `opencode-state' registry, which follows the service's event stream
;; whether or not a session has a buffer.  The registry is authoritative,
;; so every call of the :sessions function maps its entries afresh; only
;; the service's projects, which give sessions their display name, are
;; kept here, fetched when the backend starts and again whenever the
;; service reports a project change or the event stream reconnects.
;;
;; What a session runs, its agent, model, effort, provider and context,
;; the records tell as opencode.el's mode line does, through its displays,
;; which read the cached catalogs of the session's directory: drover
;; redraws as catalogs arrive.  The registry knows the context size of
;; the sessions it saw take a step; the others it fetches when asked,
;; and the backend asks for those a view shows (:prefetch).
;;
;; The backend also fits opencode.el's session buffers into drover's
;; keys: each tells its session (`drover-session-at-point-function') and
;; its prompt actions (`drover-prompt-define').  opencode.el lists no
;; sessions of its own: drover's list does.  The menu `drover-dispatch'
;; gains opencode.el's session search.  RET on an earlier prompt of a
;; session buffer puts it in drover's prompt, set to go to that session
;; (`drover-opencode-edit-old-input', for `opencode-old-input-function').
;;
;; Older sessions, beyond those of the registry, come from the service
;; on demand (:history): every unarchived session, of every project;
;; so do the archived ones (:archived), which :unarchive restores.  The
;; service archives a session alone, its subagents staying unarchived:
;; they go with it, left out of the sessions in use and listed with the
;; archived ones (`drover-opencode--archived-above').  Both
;; are fetched again as sessions are archived, restored or deleted, and
;; as the event stream reconnects, while lists show them.  Archiving a
;; session closes its buffer (:close) once the service confirmed the
;; archive (:archive-confirms), as the session runs in the service,
;; input typed and not sent going to the kill ring; a refused archive
;; leaves the buffer open.
;; The recap of a session's last turn (:recap), which the list shows as
;; a session expands, is opencode.el's `opencode-session-last-turn'.
;;
;; The file also registers the `opencode' engine (see `drover-model'):
;; what a new OpenCode session would run, from the service's catalogs of
;; its directory, which opencode.el caches; the catalog's models, as a
;; model source; when OpenCode last took a prompt, from its database.
;; What a model runs without a variant is opencode.el's to know
;; (`opencode-model-default-variant').  The prompt (`drover-prompt')
;; starts sessions and sends to them through the backend's prompt
;; operations, which go through opencode.el's own: its session creation,
;; its send checks, its agent and model switches.
;;
;; `drover' loads this file once opencode.el is loaded.

;;; Code:

(require 'cl-lib)
(require 'drover-core)
(require 'drover-keys)
(require 'drover-model)
(require 'opencode)
(require 'seq)
(require 'subr-x)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 8))
  (load "drover-core" nil t))

;; Recent additions to opencode.el.
(declare-function opencode-fetch-sessions "opencode-sessions" (query callback &optional else cursor accumulated))
(declare-function opencode-session-delete "opencode-session" (id))
(declare-function opencode-session-fork "opencode-session" (id &optional message-id))
(declare-function opencode-session-compact "opencode-session" (id))
(declare-function opencode-session-last-turn "opencode-session" (id callback))
(declare-function opencode-agent-display "opencode-display" (id &optional directory))

;; Scheduling is drover's, loaded as the user schedules.
(declare-function drover-schedule-load "drover-schedule" ())
(declare-function drover-schedule-target "drover-schedule" (&optional buffer))
(declare-function drover-schedule-read-when "drover-schedule" (&optional turn))
(declare-function drover-schedule-read-prompt "drover-schedule" ())
(declare-function drover-schedule-enqueue "drover-schedule" (target chosen text))
(declare-function drover-prompt-edit-for "drover-prompt" (session text))
(defvar opencode--attachments)
;; From opencode.el's versions that edit earlier prompts.
(defvar opencode-old-input-function)

;;;; The OpenCode engine

(defcustom drover-opencode-timeout 20
  "Seconds drover waits for an OpenCode catalog, or a new session, before going on."
  :type 'number
  :group 'drover)

(defcustom drover-opencode-database
  (expand-file-name "opencode/opencode.db" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "OpenCode's session database, which every OpenCode front end writes.
The engine reads when a session last changed there."
  :type 'file
  :group 'drover)

(defun drover-opencode-model (info providers)
  "Return the `drover-model' of INFO, an entry of OpenCode's model catalog.
PROVIDERS is the provider catalog, naming the provider and giving the
default variant (see `opencode-model-default-variant').  Each variant
is (SYMBOL :effort EFFORT), EFFORT the effort it sends as
`opencode-request-effort' reads it, or (SYMBOL) without one; the
engines are those of `drover-model-engines-for'."
  (let* ((provider-id (alist-get 'providerID info))
         (provider (seq-find (lambda (entry) (equal (alist-get 'id entry) provider-id))
                             providers))
         (id (alist-get 'id info)))
    (drover-model-create
     :provider-id provider-id
     :provider-name (alist-get 'name provider)
     :id id
     :name (alist-get 'name info)
     :variants (mapcar (lambda (variant)
                         (cons (intern (alist-get 'id variant))
                               (when-let* ((effort (opencode-request-effort
                                                    (alist-get 'settings variant)
                                                    (alist-get 'body variant))))
                                 (list :effort effort))))
                       (alist-get 'variants info))
     :default-variant (opencode-model-default-variant provider info)
     :engines (drover-model-engines-for provider-id id))))

(defun drover-opencode-model-ref (model &optional variant)
  "Return MODEL, a `drover-model', as an OpenCode model reference.
The reference is ((id . ID) (providerID . PROVIDER)), followed by
\(variant . NAME) when VARIANT, a symbol, is non-nil; nil when MODEL
has no provider."
  (when-let* ((provider (drover-model-provider-id model)))
    `((id . ,(drover-model-id model)) (providerID . ,provider)
      ,@(when variant `((variant . ,(format "%s" variant)))))))

(defun drover-opencode--ref-variant (ref)
  "Return the variant model reference REF names, as a symbol, or nil for none."
  (when-let* ((variant (alist-get 'variant ref))
              (name (format "%s" variant))
              ((not (member name '("" "default")))))
    (intern name)))

(defun drover-opencode--ref-with-variant (ref variant)
  "Return model reference REF naming VARIANT instead, or REF when VARIANT is nil."
  (if variant
      `(,@(assq-delete-all 'variant (copy-alist ref)) (variant . ,(format "%s" variant)))
    ref))

(defun drover-opencode--catalog-models (models providers)
  "Return the `drover-model' records of the MODELS and PROVIDERS catalogs."
  (mapcar (lambda (info) (drover-opencode-model info providers)) models))

(defun drover-opencode--ref-model (ref &optional catalog)
  "Return the `drover-model' named by REF, an OpenCode model reference.
It is the model of CATALOG, a list of `drover-model' records, when
CATALOG has it, else a model carrying only its identifiers and engines,
since a session still runs a model the catalog lost.  Nil when REF
names no model."
  (let-alist ref
    (when (and (stringp .id) (stringp .providerID))
      (or (seq-find (lambda (model)
                      (and (equal (drover-model-provider-id model) .providerID)
                           (equal (drover-model-id model) .id)))
                    catalog)
          (drover-model-create :provider-id .providerID :id .id
                               :engines (drover-model-engines-for .providerID .id))))))

;;;;; Catalogs

(defun drover-opencode--catalog-fetch (kind directory callback)
  "Call CALLBACK with the KIND catalog of DIRECTORY, or with nil.
The catalog comes through opencode.el's cache, which session buffers
share.  CALLBACK gets nil when the catalog does not arrive within
`drover-opencode-timeout' seconds, or when the fetch fails, which is
reported.  It is called once, possibly at once."
  (let ((done nil)
        (timer nil))
    (cl-flet ((finish (catalog)
                (unless done
                  (setq done t)
                  (when timer (cancel-timer timer))
                  ;; Called back from a timer or a process.
                  (drover-with-reporting "OpenCode catalog"
                    (funcall callback catalog)))))
      (setq timer (run-at-time drover-opencode-timeout nil #'finish nil))
      (condition-case err
          (opencode-catalog-fetch kind #'finish directory
                                  (lambda (error)
                                    (opencode-report-error (opencode-error-describe error))
                                    (finish nil)))
        (error (setq done t)
               (cancel-timer timer)
               (signal (car err) (cdr err)))))))

(defun drover-opencode--catalog-wait (kind directory)
  "Return the KIND catalog of DIRECTORY, waiting for it when not cached.
Unlike `opencode-catalog-wait', a catalog that does not arrive within
`drover-opencode-timeout' seconds, or that fails, is nil: the failure
is reported, and the caller goes on without it.  Only commands may wait."
  (let ((result :pending)
        ;; The timer of the fetch ends the wait; this is a safety net.
        (deadline (+ (float-time) drover-opencode-timeout 1)))
    (drover-opencode--catalog-fetch kind directory (lambda (catalog) (setq result catalog)))
    (while (and (eq result :pending) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (eq result :pending) result)))

(defun drover-opencode--catalog-source (directory)
  "Return the models of the OpenCode catalogs of DIRECTORY, or nil.
The model and provider catalogs give names, variants and default
variants.  Nil when the model catalog does not arrive in time."
  (when-let* ((models (drover-opencode--catalog-wait 'model directory)))
    (drover-opencode--catalog-models models (drover-opencode--catalog-wait 'provider directory))))

(defun drover-opencode--agents (directory)
  "Return the names of the agents a session in DIRECTORY can run, or nil.
The agent catalog is waited for, as `drover-opencode--catalog-wait' does."
  (when (drover-opencode--catalog-wait 'agent directory)
    (mapcar (lambda (agent) (alist-get 'id agent)) (opencode-primary-agents directory))))

;;;;; Defaults

(defun drover-opencode--last-pick (&optional catalog)
  "Return (MODEL . VARIANT), the model and variant picked last in opencode.el.
New opencode.el sessions start from `opencode-last-model', which is
read live, so that a pick made since the defaults were fetched counts.
MODEL is resolved against CATALOG as `drover-opencode--ref-model' does.
VARIANT is a symbol, nil for the model's default; it is dropped only
when CATALOG proves MODEL lacks it.  Nil when no model was picked."
  (when-let* ((ref opencode-last-model)
              (model (drover-opencode--ref-model ref catalog)))
    (let ((variant (drover-opencode--ref-variant ref)))
      (cons model
            (and variant
                 (or (not (memq model catalog))
                     (assq variant (drover-model-variants model)))
                 variant)))))

(defun drover-opencode--defaults-from (agents models providers default-model)
  "Return the defaults data the catalogs of one directory give.
AGENTS, MODELS and PROVIDERS are its agent, model and provider
catalogs, and DEFAULT-MODEL the model info a session without a model
runs, or nil.  The plist holds :agent, the agent a session without one
runs, which the service lists first among those a session can run;
:agents, an alist mapping each agent pinning a model to (MODEL
. VARIANT); :default-model; and :catalog, the models as `drover-model'
records, against which the last pick is resolved."
  (let ((catalog (drover-opencode--catalog-models models providers)))
    (list :agent (alist-get 'id (seq-find #'opencode-agent-selectable-p agents))
          :agents (delq nil
                        (mapcar (lambda (agent)
                                  (when-let* ((ref (alist-get 'model agent))
                                              (model (drover-opencode--ref-model ref catalog)))
                                    (cons (alist-get 'id agent)
                                          (cons model (drover-opencode--ref-variant ref)))))
                                agents))
          :default-model (when default-model
                           (let ((model (drover-opencode--ref-model default-model catalog)))
                             (if (memq model catalog)
                                 model
                               (drover-opencode-model default-model providers))))
          :catalog catalog)))

(defun drover-opencode--fetch-defaults (directory callback)
  "Call CALLBACK with the defaults data of new OpenCode sessions in DIRECTORY.
The data is what `drover-opencode--defaults-from' makes of the agent,
model and provider catalogs and of the default model, all asked for
DIRECTORY at once.  A default model that cannot be fetched counts as
unset; a catalog that cannot be fetched is reported by opencode.el, and
CALLBACK is then never called.  The engine's :defaults."
  (condition-case err
      (let ((pending 4) agents models providers default-model)
        (cl-flet ((deliver ()
                    (when (zerop (cl-decf pending))
                      ;; Called from a process filter: report rather than signal.
                      (condition-case err
                          (funcall callback (drover-opencode--defaults-from
                                             agents models providers default-model))
                        (error (drover-warn "Could not read the OpenCode defaults of %s: %s"
                                            (abbreviate-file-name directory)
                                            (drover-error-message err)))))))
          (opencode-catalog-fetch 'agent (lambda (data) (setq agents data) (deliver)) directory)
          (opencode-catalog-fetch 'model (lambda (data) (setq models data) (deliver)) directory)
          (opencode-catalog-fetch 'provider (lambda (data) (setq providers data) (deliver)) directory)
          (opencode-request 'get "/api/model/default"
                            :directory directory :buffer nil
                            :then (lambda (response)
                                    (setq default-model (opencode-request-data response))
                                    (deliver))
                            :else (lambda (_error) (deliver)))))
    (error (drover-warn "Could not ask OpenCode for the defaults of %s: %s"
                        (abbreviate-file-name directory) (drover-error-message err)))))

(defun drover-opencode--resolve (defaults agent model variant)
  "Return the agent, model and variant a new OpenCode session would run.
DEFAULTS is the data of `drover-opencode--fetch-defaults', or nil while
it is fetched; AGENT, MODEL and VARIANT are the explicit choices, nil
when not made.  This is what a new opencode.el session does with them:
the agent chosen, else the default one; the model chosen, else the one
the agent pins, else the last pick (`drover-opencode--last-pick'), else
the service's default model.  The variant chosen wins; else a model
chosen runs its own default variant, since the variant an agent pins
belongs to the agent's model; else a pinned model runs its pinned
variant, the last pick its own; else the model's default.  The engine's
:resolve."
  (let* ((agent (or agent (plist-get defaults :agent)))
         (pin (cdr (assoc agent (plist-get defaults :agents))))
         (pin-model (car pin))
         (last-pick (and (not model) (not pin-model)
                         (drover-opencode--last-pick (plist-get defaults :catalog))))
         (effective (or model pin-model (car last-pick) (plist-get defaults :default-model)))
         (variant (or variant
                      (and (not model) pin-model (cdr pin))
                      (and (not model) (cdr last-pick))
                      (and effective (drover-model-default-variant effective)))))
    (list :agent agent :model effective :variant variant)))

(defun drover-opencode--last-used ()
  "Return when an OpenCode session last changed, as a float time, or nil.
It is read from `drover-opencode-database', which every OpenCode front
end writes: OpenCode 2 keeps sessions in its `session_v2' table.
Without SQLite support, or before that table exists, the modification
times of the database files stand in.  The engine's :last-used."
  (or (and (fboundp 'sqlite-available-p) (sqlite-available-p)
           (file-readable-p drover-opencode-database)
           (ignore-errors
             (let ((db (sqlite-open drover-opencode-database t)))
               (unwind-protect
                   (when-let* ((ms (caar (sqlite-select
                                          db "select max(time_updated) from session_v2")))
                               ((numberp ms)))
                     (/ ms 1000.0))
                 (sqlite-close db)))))
      (when-let* ((times (delq nil (mapcar #'drover-file-mtime
                                           (list drover-opencode-database
                                                 (concat drover-opencode-database "-wal"))))))
        (apply #'max times))))

(defun drover-opencode--register-engine ()
  "Register the `opencode' engine and its catalog as a model source."
  (drover-register-engine 'opencode
                          :label "OpenCode"
                          :defaults #'drover-opencode--fetch-defaults
                          :resolve #'drover-opencode--resolve
                          :agents #'drover-opencode--agents
                          :last-used #'drover-opencode--last-used
                          :model-resets-variant t
                          :directory #'drover-opencode--directory)
  (drover-register-model-source 'opencode #'drover-opencode--catalog-source 0))

(drover-opencode--register-engine)

(defun drover-opencode--unregister-engine ()
  "Undo `drover-opencode--register-engine'.
Other front ends may run the engine too, such as agent-shell, whose
model sources then stay, with the engine's label alone."
  (drover-unregister-model-source 'opencode #'drover-opencode--catalog-source)
  (if (drover-engine-model-sources 'opencode)
      (drover-register-engine 'opencode :label "OpenCode")
    (drover-unregister-engine 'opencode)))

;;;; Conversations by identifier

(defun drover-opencode--directory (id then else)
  "Call THEN with the directory OpenCode session ID runs in, as the service tells.
ELSE gets a message when the service has no such session.  The
engine's :directory (see `drover-register-engine')."
  (opencode-session-fetch
   id
   (lambda (info)
     (drover-with-reporting "OpenCode session"
       (let ((directory (alist-get 'directory (alist-get 'location info))))
         (if (and (stringp directory) (not (string-empty-p directory)))
             (funcall then (file-name-as-directory directory))
           (funcall else (format "The service tells no directory of OpenCode session %s"
                                 (drover-bound-line id)))))))
   nil
   (lambda (error)
     (drover-with-reporting "OpenCode session"
       (funcall else (format "No OpenCode session %s: %s" (drover-bound-line id)
                             (opencode-error-describe error)))))))

(defun drover-opencode-resume (id _directory &optional then else)
  "Open OpenCode session ID in the selected window, as the service has it.
The service finds a session by its identifier alone.  THEN gets its
buffer once its transcript is loaded, ELSE a message when the service
cannot open it, which is otherwise reported.  opencode.el follows the
session live as it opens it.  A resumer of `drover-register-resumer'."
  (opencode-session-open-by-id
   id :other-window nil
   :callback (lambda (_info)
               (when then
                 (let ((buffer (current-buffer)))
                   (drover-with-reporting "OpenCode session"
                     (funcall then buffer)))))
   :else (lambda (error)
           (drover-with-reporting "OpenCode session"
             (if else
                 (funcall else (opencode-error-describe error))
               (opencode-report-error (format "No OpenCode session %s: %s" (drover-bound-line id)
                                              (opencode-error-describe error))))))))

;;;; Projects

(defvar drover-opencode--projects nil
  "The service's projects, as `opencode-fetch-projects' last returned them.")

(defvar drover-opencode--projects-fetching nil
  "Non-nil while the projects are being fetched.")

(defvar drover-opencode--projects-stale nil
  "Non-nil when the projects changed during the fetch in progress.")

(defvar drover-opencode--projects-known nil
  "Non-nil once the projects were fetched, which events then keep current.")

(defun drover-opencode--fetch-projects ()
  "Fetch the service's projects, then redraw sessions.
A project change while a fetch is in progress fetches once more after
it, so a burst of changes costs at most two requests."
  (if drover-opencode--projects-fetching
      (setq drover-opencode--projects-stale t)
    (setq drover-opencode--projects-fetching t
          drover-opencode--projects-stale nil)
    (condition-case err
        (opencode-fetch-projects
         (lambda (projects)
           (drover-with-reporting "OpenCode projects"
             (setq drover-opencode--projects projects
                   drover-opencode--projects-known t)
             (drover-opencode--projects-fetched)
             (drover-backend-changed 'opencode)))
         ;; Project names only decorate sessions: keep the previous ones.
         (lambda (_error)
           (drover-with-reporting "OpenCode projects"
             (drover-opencode--projects-fetched))))
      (error (setq drover-opencode--projects-fetching nil)
             (signal (car err) (cdr err))))))

(defun drover-opencode--projects-fetched ()
  "Note the end of a projects fetch, fetching again when they changed meanwhile."
  (setq drover-opencode--projects-fetching nil)
  (when drover-opencode--projects-stale
    (drover-opencode--fetch-projects)))

(defun drover-opencode--projects-changed (event)
  "Follow EVENT, after which the projects may have changed.
A `project.updated' event carries its project whole, which replaces the
one of the same identifier without a request: each request starts a
process, which may hold Emacs for as long as the system takes to start
it.  The projects are fetched again when they were never fetched, while
a fetch is under way, after an event without a whole project, and after
`server.connected', after which changes missed while disconnected are
fetched like the registry is."
  (drover-with-reporting "OpenCode events"
    (let ((project (and (equal (alist-get 'type event) "project.updated")
                        (alist-get 'data event))))
      (if (and drover-opencode--projects-known
               (not drover-opencode--projects-fetching)
               (drover-opencode--whole-project-p project))
          (drover-opencode--project-updated project)
        (drover-opencode--fetch-projects)))))

(defun drover-opencode--whole-project-p (project)
  "Return non-nil when PROJECT, of an event, holds what names sessions."
  (and (drover-json-object-p project)
       (stringp (alist-get 'id project))
       (stringp (alist-get 'canonical project))
       (listp (alist-get 'sandboxes project))
       (seq-every-p #'stringp (alist-get 'sandboxes project))
       (let ((name (alist-get 'name project)))
         (or (null name) (stringp name)))))

(defun drover-opencode--project-naming (project)
  "Return what of PROJECT names sessions, to compare."
  (list (alist-get 'id project) (alist-get 'name project)
        (alist-get 'canonical project) (alist-get 'sandboxes project)))

(defun drover-opencode--project-updated (project)
  "Take PROJECT, which an event sent whole, among the projects.
Sessions are drawn again only when what names them changed."
  (let* ((id (alist-get 'id project))
         (old (seq-find (lambda (known) (equal (alist-get 'id known) id))
                        drover-opencode--projects)))
    (unless (and old (equal (drover-opencode--project-naming old)
                            (drover-opencode--project-naming project)))
      ;; A new list: records compare the projects they were made of by `eq'.
      (setq drover-opencode--projects
            (if old
                (mapcar (lambda (known) (if (eq known old) project known)) drover-opencode--projects)
              (append drover-opencode--projects (list project))))
      (drover-backend-changed 'opencode))))

(defun drover-opencode--project-name (project-id directory)
  "Return the name of the project with PROJECT-ID or containing DIRECTORY, or nil."
  (when-let* ((project (or (and project-id
                                (seq-find (lambda (project) (equal (alist-get 'id project) project-id))
                                          drover-opencode--projects))
                           (and directory
                                (opencode-project-for-directory drover-opencode--projects directory))))
              ;; `opencode-project-name' needs one of them.
              ((or (alist-get 'name project) (stringp (alist-get 'canonical project))))
              (name (opencode-project-name project))
              ((not (string-empty-p name))))
    name))

;;;; Sessions

(defun drover-opencode--detail (entry status)
  "Return one line on STATUS, the status of registry ENTRY, or nil."
  (pcase status
    ('failed (plist-get entry :error))
    ('retrying
     (when-let* ((retry (plist-get entry :retry)))
       (let ((attempt (plist-get retry :attempt))
             (at (plist-get retry :at))
             (message (plist-get retry :message)))
         (concat "retry"
                 (and attempt (format " %d" attempt))
                 (and at (concat " at " (format-time-string "%H:%M" at)))
                 (and message (concat ": " message))))))))

(defun drover-opencode--run (entry directory)
  "Return what registry ENTRY runs, as arguments of `drover-session-create'.
They are its model, compact, the effort it runs, dimmed when the model's
default, its provider, and its context, as opencode.el's mode line
shows them, read in the cached catalogs of DIRECTORY: none is fetched
here.  Until they arrive, the model is its identifier, and what needs
them nil; `drover-opencode--catalog-changed' redraws once they do.  The
context size is the one the registry knows, the service's estimate
after a compaction until the next step measures it:
`drover-opencode--prefetch' asks for the others."
  (let* ((ref (plist-get entry :model))
         (directory (or directory default-directory))
         (provider-id (alist-get 'providerID ref))
         (effort (and ref (opencode-variant-display ref directory)))
         (tokens (plist-get entry :context))
         (usage (and ref tokens (opencode-context-usage tokens ref directory))))
    (list :model (and ref (or (opencode-model-display ref directory) (alist-get 'id ref)))
          :effort (and effort (substring-no-properties effort))
          :effort-default (and effort (not (drover-opencode--ref-variant ref)))
          :provider (when-let* ((provider (and provider-id
                                               (opencode-provider-display provider-id directory))))
                      (substring-no-properties provider))
          :provider-name (and provider-id
                              (or (alist-get 'name (opencode-provider-info provider-id directory))
                                  provider-id))
          :context-tokens tokens
          :context-estimated (plist-get entry :context-estimated)
          :context-limit (plist-get usage :limit)
          :context-tier (plist-get usage :tier))))

(defun drover-opencode--session (entry)
  "Return the `drover-session' of `opencode-state' registry ENTRY."
  (let ((directory (plist-get entry :directory))
        (status (opencode-state-status entry))
        (cache (opencode-state-cache (plist-get entry :id))))
    (apply #'drover-session-create
           :backend 'opencode
           :id (plist-get entry :id)
           :engine 'opencode
           :engine-id (plist-get entry :id)
           :title (plist-get entry :title)
           :directory directory
           :project (drover-opencode--project-name (plist-get entry :project-id) directory)
           :branch (and directory (drover-git-branch directory))
           :parent (plist-get entry :parent-id)
           :status status
           :requests (mapcar (lambda (request)
                               (list :kind (plist-get request :kind)
                                     :summary (plist-get request :summary)))
                             (plist-get entry :requests))
           :since (opencode-state-since entry)
           :updated (plist-get entry :updated)
           :created (plist-get entry :created)
           :agent (plist-get entry :agent)
           ;; Plain until the agent catalog arrives, which redraws.
           :agent-display (when-let* ((agent (plist-get entry :agent)))
                            (or (opencode-agent-display agent directory) agent))
           :detail (drover-opencode--detail entry status)
           :cache-expires (plist-get cache :expires)
           :cache-ttl (plist-get cache :ttl)
           :cache-tokens (plist-get cache :tokens)
           :cache-prompt-tokens (plist-get cache :prompt-tokens)
           :archived (plist-get entry :archived)
           (drover-opencode--run entry directory))))

(defvar drover-opencode--made nil
  "The records the registry's entries made, as (LAYOUT . TABLE), or nil.
TABLE maps session IDs to (INPUTS . RECORD), INPUTS what RECORD was
made of (see `drover-opencode--inputs'); LAYOUT is that of
`drover-session' then.  OpenCode reports many changes without telling
which sessions changed, which lists them all anew: thousands of records
were made each time, displays of models and agents included, most the
same as before.")

;; Records of the version of this file loaded before are made anew.
(setq drover-opencode--made nil)

(defvar opencode-catalog-generation)

(defun drover-opencode--inputs (entry)
  "Return what the record of registry ENTRY is made of, as a vector.
ENTRY, its prompt cache, the service's projects, the branch of its
directory, and how many times catalogs changed, which displays of
models, providers and agents read."
  (let ((directory (plist-get entry :directory)))
    (vector entry
            (opencode-state-cache (plist-get entry :id))
            drover-opencode--projects
            (and directory (drover-git-branch directory))
            (bound-and-true-p opencode-catalog-generation))))

(defun drover-opencode--listed (entry)
  "Return the `drover-session' of registry ENTRY, as made before when it can be.
A record made of the same inputs (see `drover-opencode--inputs') is
the same record."
  (unless (and drover-opencode--made
               (drover-layout-matches-p (car drover-opencode--made)))
    (setq drover-opencode--made (cons (drover-layout 'drover-session)
                                      (make-hash-table :test #'equal))))
  (let* ((table (cdr drover-opencode--made))
         (id (plist-get entry :id))
         (made (gethash id table))
         (inputs (car-safe made))
         (directory (plist-get entry :directory)))
    ;; Compared before a vector of inputs is made: listing thousands of
    ;; sessions, most are the same.
    (if (and (vectorp inputs)
             (= (length inputs) 5)
             (eq (aref inputs 2) drover-opencode--projects)
             (eql (aref inputs 4) (bound-and-true-p opencode-catalog-generation))
             (eq (aref inputs 1) (opencode-state-cache id))
             (equal (aref inputs 3) (and directory (drover-git-branch directory)))
             (or (eq (aref inputs 0) entry) (equal (aref inputs 0) entry)))
        (cdr made)
      (let ((record (drover-opencode--session entry)))
        (puthash id (cons (drover-opencode--inputs entry) record) table)
        record))))

(defvar drover-opencode--hiding (make-hash-table :test #'equal)
  "Sessions above subagents left out with an archived session, a hash table.
It maps the IDs of the sessions on the way from such a subagent up to
the archived session, that one included, to t.  Whether the subagent
is left out changes with them: restored or deleted (see
`drover-opencode--updated').")

(defun drover-opencode--archived-above (entry)
  "Return the ID of the archived session registry ENTRY goes with, or nil.
The service archives a session alone: its subagents, unarchived, stay
among the sessions in use while their parent leaves them, in lists at
the top of trees of their own.  A subagent goes with the archived
session above it, at any depth, into the archive, as an archived
session goes there whether or not its buffer is open (see
`drover-archived-sessions').  A parent the registry does not hold,
deleted or never fetched, archives nothing.  The sessions on the way
are noted in `drover-opencode--hiding'."
  (when (plist-get entry :parent-id)
    (let ((current entry)
          (way nil)
          (found nil))
      (while current
        (let ((id (plist-get current :id)))
          (cond ((member id way)
                 (setq current nil))
                ((and way (plist-get current :archived))
                 (setq found id current nil))
                (t
                 (push id way)
                 (setq current (when-let* ((parent (plist-get current :parent-id)))
                                 (opencode-state-get parent)))))))
      (when found
        (dolist (id (cons found (cdr (reverse way))))
          (puthash id t drover-opencode--hiding)))
      found)))

(defun drover-opencode--sessions ()
  "Return the sessions of the `opencode-state' registry.
The subagents of an archived session are left out, as it is (see
`drover-opencode--archived-above').  The records of sessions gone are
forgotten."
  (clrhash drover-opencode--hiding)
  (let ((records (delq nil (mapcar (lambda (entry)
                                     (unless (drover-opencode--archived-above entry)
                                       (drover-opencode--listed entry)))
                                   (opencode-state-sessions)))))
    (when-let* ((table (cdr drover-opencode--made))
                ((> (hash-table-count table) (length records))))
      (let ((listed (make-hash-table :test #'equal :size (length records))))
        (dolist (record records)
          (puthash (drover-session-id record) t listed))
        (maphash (lambda (id _) (unless (gethash id listed) (remhash id table))) table)))
    records))

(defun drover-opencode--session-by-id (id)
  "Return the session ID of the `opencode-state' registry, or nil.
It is nil for a session `opencode-state-sessions' leaves out, as an
archived one: whether it does, it alone tells, an archived session's
buffer keeping it there in some versions; and for the subagent of an
archived session (see `drover-opencode--archived-above').  The
backend's :session, which drover calls for each session the registry
reports changed: a registry of thousands of sessions is then not listed
whole on every change."
  (when-let* ((entry (opencode-state-get id))
              ((or (not (plist-get entry :archived))
                   (memq entry (opencode-state-sessions))))
              ((not (drover-opencode--archived-above entry))))
    (drover-opencode--listed entry)))

(defun drover-opencode--prefetch (sessions)
  "Ask the registry for the context size of SESSIONS, which a view shows.
The registry knows the size of a session it saw take a step, but not of
one it loaded: `opencode-state-context' fetches that in the background,
a few at a time, the first time it is asked, and runs
`opencode-state-updated-functions' once it arrives, which redraws.  It
costs nothing for a size known, being fetched, or that failed, and
asks nothing for a session the registry does not hold, as an older one.
Asking for every session of the registry, a hundred, would fetch the
sizes of sessions nobody looks at.  The backend's :prefetch."
  (dolist (session sessions)
    (opencode-state-context (drover-session-id session))))

;;;; Older sessions

(defun drover-opencode--info-entry (info)
  "Return a registry entry made from session INFO, as the service describes it.
The entry has what INFO tells, as `opencode-state-get' describes
entries: no running execution, request or retry, which only the
registry follows."
  (let-alist info
    (list :id .id :title .title
          :directory (and .location.directory (opencode-normalize-directory .location.directory))
          :project-id .projectID :parent-id .parentID :agent .agent :model .model
          :running nil :retry nil :requests nil
          :outcome (and (stringp .outcome) (intern .outcome)) :error nil
          :idle (opencode-time-seconds .time.idle)
          :viewed (opencode-time-seconds .time.viewed)
          :archived (opencode-time-seconds .time.archived)
          :created (opencode-time-seconds .time.created)
          :updated (opencode-time-seconds .time.updated)
          :started nil)))

(defun drover-opencode--fetch (archived what callback)
  "Fetch every session of every project, ARCHIVED or not, for CALLBACK.
CALLBACK receives `drover-session' records, mapped as registry entries
are; their status is what the service tells of them, idle unless a
turn ended unseen.  The service pages the list; the pages are fetched
one after the other, never waiting.  A failure is reported, naming
WHAT was fetched, and CALLBACK not called."
  ;; Listed from the home directory, as `opencode-list-all-sessions'
  ;; does: no project scopes the list.
  (let ((default-directory (expand-file-name "~/")))
    (opencode-fetch-sessions
     `((archived . ,(if archived "true" "false")))
     (lambda (infos)
       (drover-with-reporting (format "OpenCode %s sessions" what)
         (funcall callback (mapcar (lambda (info)
                                     (drover-opencode--session (drover-opencode--info-entry info)))
                                   infos))))
     (lambda (error)
       (drover-with-reporting (format "OpenCode %s sessions" what)
         (drover-message "Drover: cannot fetch %s OpenCode sessions: %s"
                         what (opencode-error-describe error)))))))

(defun drover-opencode--history (callback)
  "Fetch every unarchived session of every project, and call CALLBACK with them.
The subagents of archived sessions are left out, as they go with them:
see `drover-opencode--archived-above'.  See `drover-opencode--fetch'.
The backend's :history."
  (drover-opencode--fetch nil "older"
                          (lambda (sessions)
                            (funcall callback
                                     (seq-remove (lambda (session)
                                                   (drover-opencode--archived-above
                                                    (list :id (drover-session-id session)
                                                          :parent-id (drover-session-parent session))))
                                                 sessions)))))

(defvar drover-opencode--archived-ids nil
  "Identifiers of the sessions last fetched as archived, a hash table, or nil.
One the registry then tells restored leaves the archived sessions lists
show (see `drover-opencode--left-archive').")

(defun drover-opencode--archived (callback)
  "Fetch every archived session of every project, and call CALLBACK with them.
Their records tell when they were archived.  The subagents of archived
sessions the service lists unarchived, which lists of the sessions in
use leave out (see `drover-opencode--archived-above'), come with them,
from the registry.  See `drover-opencode--fetch'.  The backend's
:archived."
  (drover-opencode--fetch t "archived"
                          (lambda (sessions)
                            (let ((ids (make-hash-table :test #'equal :size (length sessions))))
                              (dolist (session sessions)
                                (puthash (drover-session-id session) t ids))
                              (setq drover-opencode--archived-ids ids)
                              (funcall callback
                                       (append sessions
                                               (delq nil (mapcar (lambda (entry)
                                                                   (and (not (gethash (plist-get entry :id) ids))
                                                                        (drover-opencode--archived-above entry)
                                                                        (drover-opencode--listed entry)))
                                                                 (opencode-state-sessions)))))))))

(defun drover-opencode--left-archive (id)
  "Forget session ID among the archived ones fetched, once restored.
ID nil stands for every session fetched as archived.  opencode.el
restores an archived session as it sends to it, telling the registry,
which runs `opencode-state-updated-functions'; the service's event
fetches the archived sessions again later (see
`drover-opencode--sessions-moved').  A session the registry does not
hold tells nothing."
  (when-let* ((ids (and drover-show-archived drover-opencode--archived-ids)))
    (dolist (id (if id (list id) (hash-table-keys ids)))
      (when-let* (((gethash id ids))
                  (entry (opencode-state-get id))
                  ((not (plist-get entry :archived))))
        (remhash id ids)
        (drover-archived-left 'opencode id)))))

(defvar drover-opencode--refetch-timer nil
  "Timer fetching again the older and archived sessions lists show.")

(defun drover-opencode--refetch ()
  "Fetch again the older and archived sessions lists show, if any."
  (drover-with-reporting "OpenCode fetch"
    (setq drover-opencode--refetch-timer nil)
    (when drover-show-history
      (drover-history-fetch 'opencode))
    (when drover-show-archived
      (drover-archived-fetch 'opencode))))

(defun drover-opencode--sessions-moved (_event)
  "Fetch the older and archived sessions again, after _EVENT, if lists show them.
_EVENT archived, restored or deleted a session, which moves it between
the lists the service gives, or reconnected the event stream, after
which events may have been missed.  Archiving a selection sends a burst
of events: they are fetched again once, half a second after the last."
  (drover-with-reporting "OpenCode events"
    (when (or drover-show-history drover-show-archived)
      (when (timerp drover-opencode--refetch-timer)
        (cancel-timer drover-opencode--refetch-timer))
      (setq drover-opencode--refetch-timer (run-at-time 0.5 nil #'drover-opencode--refetch)))))

(defconst drover-opencode--moving-events
  '("session.archived" "session.unarchived" "session.deleted" "server.connected")
  "Events after which the older and archived sessions are fetched again.")

;;;; Changes

(defun drover-opencode--updated (id)
  "Report a change of session ID to drover, a restore from the archive included.
The subagents below ID are reported too when ID may take them into the
archive or out of it (see `drover-opencode--archived-above'): when it
is archived, or on the way from them to an archived session, its
change then maybe a restore or a deletion."
  (drover-with-reporting "OpenCode changes"
    (drover-opencode--left-archive id)
    (drover-backend-changed 'opencode id)
    (when (and id (or (gethash id drover-opencode--hiding)
                      (plist-get (opencode-state-get id) :archived)))
      ;; Noted again as the subagents are looked at, if still on the way.
      (remhash id drover-opencode--hiding)
      (dolist (entry (opencode-state-descendants id))
        (drover-backend-changed 'opencode (plist-get entry :id))))))

(defun drover-opencode--catalog-changed (_kind _directory)
  "Report that sessions may read otherwise, now that a catalog changed.
Names, icons, efforts and context sizes come from a location's
catalogs, which opencode.el fetches the first time they are asked for,
and fetches again once an event or a reconnection made it forget them.
For `opencode-catalog-changed-functions'."
  (drover-with-reporting "OpenCode changes"
    (drover-backend-changed 'opencode)))

(defun drover-opencode--start ()
  "Follow the registry, session buffers, catalogs and projects; bootstrap it.
The registry does not change when a session's buffer opens or closes,
yet views show which sessions are open, nor when a catalog arrives,
yet views show what sessions run by their catalogs."
  (add-hook 'opencode-state-updated-functions #'drover-opencode--updated)
  (add-hook 'opencode-session-buffer-functions #'drover-opencode--updated)
  (add-hook 'opencode-catalog-changed-functions #'drover-opencode--catalog-changed)
  (opencode-events-register "project.updated" #'drover-opencode--projects-changed)
  (opencode-events-register "server.connected" #'drover-opencode--projects-changed)
  (dolist (type drover-opencode--moving-events)
    (opencode-events-register type #'drover-opencode--sessions-moved))
  ;; A bootstrap with no session runs no update hook, yet ends the wait.
  (opencode-state-ensure (lambda () (drover-backend-changed 'opencode)))
  (drover-opencode--fetch-projects))

;;;; Actions

(defun drover-opencode--open (session other-window)
  "Display SESSION, in another window when OTHER-WINDOW is non-nil.
A session without a buffer is fetched first; it is then shown from the
window selected when it was asked for, without taking the selection
back if the user moved on meanwhile."
  (let ((id (drover-session-id session))
        (window (selected-window)))
    (if (buffer-live-p (gethash id opencode-session-buffers))
        (opencode-session-open-by-id id :other-window other-window)
      (opencode-session-fetch
       id (lambda (info)
            (if (or (eq (selected-window) window) (not (window-live-p window)))
                (opencode-session-open info :other-window other-window)
              (with-selected-window window
                (opencode-session-open info :other-window other-window))))
       (drover-session-directory session)))))

(defun drover-opencode--buffer (session)
  "Return the live buffer of SESSION, or nil."
  (let ((buffer (gethash (drover-session-id session) opencode-session-buffers)))
    (and (buffer-live-p buffer) buffer)))

(defun drover-opencode--respond (session)
  "Answer the oldest request of SESSION from the current window.
Its buffer is not shown: the request's menu tells the session and what
it asks, and the buffer, opened in the background if need be, records
the answer."
  (opencode-respond-session (drover-session-id session) nil t))

(defun drover-opencode--interrupt (session)
  "Stop the running turn of SESSION."
  (opencode-session-interrupt (drover-session-id session)))

(defun drover-opencode--archive (session &optional then else)
  "Archive SESSION on the service; call THEN once it took, ELSE if refused.
ELSE gets the refusal's description; without it, opencode.el reports
the refusal.  The backend's :archive, which confirms (:archive-confirms):
the session's buffer closes only once the service archived it."
  (opencode-session-archive (drover-session-id session) nil
                            (and then (lambda (_info) (funcall then)))
                            (and else (lambda (error) (funcall else (opencode-error-describe error))))))

(defun drover-opencode--unarchive (session &optional then else)
  "Restore SESSION from the service's archive, then call THEN, or ELSE if refused.
ELSE gets the refusal's description; without it, opencode.el reports
the refusal.  The backend's :unarchive, which confirms
\(:unarchive-confirms)."
  (opencode-session-archive (drover-session-id session) t
                            (and then (lambda (_info) (funcall then)))
                            (and else (lambda (error) (funcall else (opencode-error-describe error))))))

(defun drover-opencode--close (session)
  "Close the buffer of SESSION without asking, as archiving does.
The session lives in the service: closing its buffer ends nothing, not
even a turn in progress, so it closes as soon as the service confirmed
the archive.  Input typed at its prompt and not sent, which opencode.el
keeps nowhere else, goes to the kill ring; the value is non-nil when
there was some.  The backend's :close."
  (when-let* ((buffer (drover-opencode--buffer session)))
    (prog1 (drover-keep-unsent
            (list (when-let* ((process (get-buffer-process buffer)))
                    (with-current-buffer buffer
                      (buffer-substring-no-properties (process-mark process) (point-max))))))
      (drover-kill-buffer buffer))))

(defun drover-opencode--mark-seen (session)
  "Mark the ended turn of SESSION viewed on the service, for every client."
  (opencode-session-view (drover-session-id session)))

(defun drover-opencode--preview (session callback)
  "Call CALLBACK with the last reply of SESSION, or nil."
  (opencode-session-last-reply (drover-session-id session) callback))

(defun drover-opencode--recap (session callback)
  "Call CALLBACK with a recap of the last turn of SESSION.
It is what `opencode-session-last-turn' fetches: the last prompt, the
reply since, the error it ended with and the files it changed, in
drover's form already.  The backend's :recap."
  (opencode-session-last-turn (drover-session-id session) callback))

(defun drover-opencode--new (directory)
  "Create a session in DIRECTORY and display it."
  (opencode-session-create :directory directory))

(defun drover-opencode--rename (session title)
  "Rename SESSION to TITLE on the service."
  (opencode-rename-session (drover-session-id session) title))

(defun drover-opencode--delete (session)
  "Delete SESSION on the service, without asking."
  (opencode-session-delete (drover-session-id session)))

(defun drover-opencode--fork (session)
  "Fork SESSION whole into a new session, and display it."
  (opencode-session-fork (drover-session-id session)))

(defun drover-opencode--compact (session)
  "Compact the conversation of SESSION."
  (opencode-session-compact (drover-session-id session)))

;;;; Prompt operations

(defun drover-opencode--once (function)
  "Return a function calling FUNCTION with its arguments, the first time only.
An error FUNCTION signals is reported with `drover-warn': such callbacks
may run from a process filter or a timer, where no error may escape."
  (let ((called nil))
    (lambda (&rest arguments)
      (unless called
        (setq called t)
        (condition-case err
            (apply function arguments)
          (error (drover-warn "OpenCode: %s" (drover-error-message err))))))))

(defun drover-opencode--in-buffer (session function else)
  "Call FUNCTION with no argument in the buffer of SESSION.
A session without a buffer is opened first, without displaying it,
FUNCTION then running once its transcript is loaded.  ELSE receives a
message when the session cannot be opened, or when FUNCTION signals."
  (condition-case err
      (if-let* ((buffer (drover-opencode--buffer session)))
          (with-current-buffer buffer
            (funcall function))
        (opencode-session-open-by-id
         (drover-session-id session)
         :no-display t
         :callback (lambda (_info)
                     (condition-case err
                         (funcall function)
                       (error (funcall else (drover-error-message err)))))
         :else (lambda (error) (funcall else (opencode-error-describe error)))))
    (error (funcall else (drover-error-message err)))))

(defun drover-opencode--switch-failure (buffer label next)
  "Return a switch's failure handler reporting LABEL of BUFFER, then calling NEXT.
The prompt still goes out.  The handler takes an `opencode-error', or
opencode.el's message for an agent the session cannot run."
  (lambda (error)
    (drover-warn "%s refused %s (%s); sending the prompt anyway"
                 (buffer-name buffer) label (opencode-error-describe error))
    (funcall next)))

(defun drover-opencode--variant-ref (variant)
  "Return the model reference this session buffer runs, naming VARIANT, or nil."
  (and opencode-session-model
       (drover-opencode--ref-with-variant opencode-session-model variant)))

(defun drover-opencode--option-steps (buffer agent model variant)
  "Return the steps applying explicit options to the session of BUFFER.
They are steps of `drover-run-steps'.  AGENT switches through
`opencode-switch-agent', which selects the model the agent pins, or
MODEL with VARIANT when MODEL is chosen; without AGENT, MODEL is
selected alone.  A VARIANT alone applies to the model the session runs
once the agent switched.  Each step settles before the next, since the
service applies every switch to the turns that follow."
  (let ((explicit (and model (drover-opencode-model-ref model variant))))
    (cl-flet ((in-buffer (function)
                (lambda (next)
                  (if (buffer-live-p buffer)
                      (with-current-buffer buffer (funcall function next))
                    (funcall next)))))
      (delq nil
            (list
             (when agent
               (let ((label (format "agent %s" agent)))
                 (cons label
                       (in-buffer
                        (lambda (next)
                          (opencode-switch-agent
                           agent :model explicit :then next
                           :else (drover-opencode--switch-failure buffer label next)))))))
             (when (or (and explicit (not agent)) (and variant (not explicit)))
               (let ((label (if explicit
                                (format "model %s" (drover-model-label model))
                              (format "variant %s" variant))))
                 (cons label
                       (in-buffer
                        (lambda (next)
                          (if-let* ((ref (or explicit (drover-opencode--variant-ref variant))))
                              (opencode-switch-model
                               ref :then next
                               :else (drover-opencode--switch-failure buffer label next))
                            (drover-warn "%s names no model to run variant %s on; keeping its default"
                                         (buffer-name buffer) variant)
                            (funcall next))))))))))))

(defun drover-opencode--switch-target (buffer agent model variant)
  "Return what the session of BUFFER runs once AGENT, MODEL and VARIANT applied.
The value is a plist of :agent and :model, a model reference, for
`opencode-check-send', naming only what the steps of
`drover-opencode--option-steps' change; an agent the session cannot
run, which the switch refuses, is left out.  The agent catalog is
waited for, as its pins decide the model."
  (with-current-buffer buffer
    (let* ((info (and agent
                      (drover-opencode--catalog-wait 'agent default-directory)
                      (seq-find (lambda (entry) (equal (alist-get 'id entry) agent))
                                (opencode-primary-agents))))
           (ref (cond
                 (model (drover-opencode-model-ref model variant))
                 (variant (drover-opencode--ref-with-variant
                           (or (alist-get 'model info) opencode-session-model) variant))
                 (t (alist-get 'model info)))))
      `(,@(when (and info (not (equal agent opencode-session-agent))) (list :agent agent))
        ,@(when (and ref (not (opencode-same-model-p ref opencode-session-model)))
            (list :model ref))))))

(defun drover-opencode--check (session text options)
  "Run the send checks of SESSION on TEXT, as for input typed in its buffer.
OPTIONS are the explicit options about to be applied, so the checks
consider the turn they lead to (see `drover-opencode--switch-target').
Return what `opencode-check-send' returns: `cancel', or the metadata to
send with.  The checks may ask, such as the prompt cache guard before
an idle session sends its context uncached, so they run from the
command.  They read the session's buffer: a session without one is
checked by nothing.  The backend's :check."
  (when-let* ((buffer (drover-opencode--buffer session)))
    (with-current-buffer buffer
      (opencode-check-send
       (append (list :kind 'prompt :text text)
               (drover-opencode--switch-target buffer (plist-get options :agent)
                                               (plist-get options :model)
                                               (plist-get options :variant)))))))

(defun drover-opencode--apply-options (session options then)
  "Apply the explicit OPTIONS to the running SESSION, then call THEN.
OPTIONS are already filtered for SESSION.  Each option is a request to
the service, chained by `drover-opencode--option-steps': the agent
first, keeping the current model and variant unless it pins its own or
OPTIONS name one, then a lone variant on the model the session runs.
THEN runs once they all settled, whatever happened.  The backend's
:apply-options."
  (let ((agent (plist-get options :agent))
        (model (plist-get options :model))
        (variant (plist-get options :variant))
        (then (drover-opencode--once then)))
    (if (not (or agent model variant))
        (funcall then)
      (drover-opencode--in-buffer
       session
       (lambda ()
         (drover-run-steps (drover-opencode--option-steps (current-buffer) agent model variant)
                           then))
       (lambda (message)
         (drover-warn "Could not open %s: %s; sending the prompt anyway"
                      (drover-session-display-title session) message)
         (funcall then))))))

(defun drover-opencode--restore-refused-p (error)
  "Return non-nil when ERROR, of a send, is the refusal to restore its session.
opencode.el restores an archived session before sending to it, through
a PATCH of the session, whose error the send's ELSE then gets."
  (and (opencode-error-p error) (eq (opencode-error-method error) 'patch)))

(defun drover-opencode--restore-failure (session error)
  "Return, on one line, that SESSION could not leave the archive, as ERROR tells."
  (format "Could not restore %s from the archive: %s"
          (drover-bound-line (drover-session-display-title session))
          (opencode-error-describe error)))

(defun drover-opencode--send-failure (session name error)
  "Return, on one line, why sending to SESSION failed with ERROR.
ERROR is an `opencode-error'.  A refused restore from the archive says
SESSION could not be restored, nothing sent; another, that NAME, the
session's buffer or title, did not take the prompt."
  (if (drover-opencode--restore-refused-p error)
      (drover-opencode--restore-failure session error)
    (format "%s did not take the prompt: %s" name (opencode-error-describe error))))

(defun drover-opencode--send (session text token)
  "Send TEXT as a prompt of SESSION, with the metadata TOKEN holds.
TOKEN is what `drover-opencode--check' returned.  The prompt goes
straight to the service, past any input half typed in the buffer; the
transcript shows it once the service delivered it.  The service queues
or steers a prompt sent while a turn runs, so the session needs not be
idle.  An archived session is restored first, by opencode.el, a refusal
told as such.  The backend's :send."
  (drover-opencode--in-buffer
   session
   (lambda ()
     (let ((buffer (current-buffer)))
       (opencode-session-send
        text :show t :metadata (and (consp token) token)
        :else (lambda (error)
                (drover-with-reporting "OpenCode prompt"
                  (drover-warn "%s; it is in the kill ring"
                               (drover-opencode--send-failure session (buffer-name buffer) error)))))))
   (lambda (message)
     (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                  (drover-session-display-title session) message))))

(defun drover-opencode--blocker (session)
  "Say why the buffer of SESSION cannot take a prompt now, or nil once it can.
The service queues or steers a prompt past a draft and a running turn,
so the buffer needs only be loaded and connected.  The backend's
:blocker."
  (if-let* ((buffer (drover-opencode--buffer session)))
      (with-current-buffer buffer
        (unless (opencode-session-ready-p)
          "the session is not loaded or not connected to OpenCode"))
    "the session has no buffer"))

(defun drover-opencode--submit (session text then else)
  "Send TEXT as a prompt of SESSION now, then call THEN, or ELSE with a message.
TEXT goes literally, a leading slash included, past any draft, and shows
once delivered.  It consents to the service replacing stale thinking with
its text, since no one is there to be asked.  A session without a
buffer, as one archiving closed, is opened first, not shown, as
`drover-opencode--send' opens it; an archived session is restored by
opencode.el first.  When nothing was sent, the session not opened or
not restored, ELSE gets a second argument, t.  The backend's :submit."
  (let ((settled nil))
    (cl-flet ((settle (function &rest arguments)
                (unless settled
                  (setq settled t)
                  (drover-with-reporting "OpenCode prompt"
                    (apply function arguments)))))
      (drover-opencode--in-buffer
       session
       (lambda ()
         (opencode-session-send
          text :show t :accept-thinking-loss t
          :then (lambda (_item) (settle then))
          :else (lambda (error)
                  (if (drover-opencode--restore-refused-p error)
                      (settle else (drover-opencode--restore-failure session error) t)
                    (settle else (opencode-error-describe error))))))
       (lambda (message) (settle else message t))))))

(defun drover-opencode--ready ()
  "Return non-nil once the registry lists the sessions the service runs.
The backend's :ready."
  (opencode-state-ready-p))

;;;; Scheduling the input

(defun drover-opencode-schedule-input ()
  "Schedule the input of this session buffer for a later turn of its session.
Ask when (see `drover-schedule-read-when'), the end of the running turn
first while one runs: that queues the input after the turn through
opencode.el, which shows it in the buffer, where it can still be edited
or withdrawn, outside drover's queue.  At another time, the input is
the prompt, cleared once scheduled; with nothing typed, the prompt is
read.  Input attaching files cannot be scheduled.  The prompt action
`schedule' of OpenCode session buffers."
  (interactive)
  (require 'drover-schedule)
  (drover-schedule-load)
  (let* ((target (drover-schedule-target))
         (input (and (get-buffer-process (current-buffer))
                     (buffer-substring-no-properties (opencode-pmark) (point-max))))
         (typed (and input (not (string-blank-p input))))
         (chosen (drover-schedule-read-when (eq opencode-session-status 'busy))))
    (cond
     ((eq (car chosen) 'turn-end)
      (unless typed
        (let ((text (drover-schedule-read-prompt)))
          (delete-region (opencode-pmark) (point-max))
          (goto-char (point-max))
          (insert text)))
      (goto-char (point-max))
      (opencode-queue-input))
     ((and typed opencode--attachments)
      (user-error "The input attaches files, which a scheduled prompt cannot carry"))
     (t
      (drover-schedule-enqueue target chosen
                               (if typed (string-trim input) (drover-schedule-read-prompt)))
      (when typed
        (delete-region (opencode-pmark) (point-max)))))))

(defun drover-opencode--new-session-model (defaults agent model variant)
  "Return the model reference a new session starts on, or nil.
Nil lets `opencode-session-create' choose: the model AGENT pins, AGENT
nil standing for the default agent, else the last pick.  MODEL, when
chosen, runs VARIANT; a VARIANT alone applies to the model
`drover-opencode--resolve' finds in DEFAULTS, the data of
`drover-opencode--fetch-defaults' or nil: the pin, else the last pick,
else the default model."
  (cond
   (model (drover-opencode-model-ref model variant))
   ((null variant) nil)
   (t (when-let* ((resolved (plist-get (drover-opencode--resolve defaults agent nil nil) :model)))
        (drover-opencode-model-ref resolved variant)))))

(defun drover-opencode--create (directory engine options then else)
  "Start an OpenCode session in DIRECTORY with start OPTIONS, and display it.
ENGINE must be `opencode', the one engine the backend starts.  The
session runs the agent of OPTIONS when DIRECTORY has it as an agent
a session can run, else the default agent, named so that the agent's
reminder is stored before the first prompt; and the model of
`drover-opencode--new-session-model'.  The agent catalog is waited for
at most `drover-opencode-timeout' seconds, and so is the session, both
asynchronously.  THEN receives the new `drover-session' once its buffer
exists, ELSE a message.  The backend's :create."
  (let* ((agent (plist-get options :agent))
         (agent (and agent (not (string-empty-p agent)) agent))
         (title (plist-get options :title))
         (title (and title (not (string-empty-p title)) title))
         (timer nil)
         (settle (drover-opencode--once
                  (lambda (function value)
                    (when timer (cancel-timer timer))
                    ;; Called back from a timer or a process.
                    (drover-with-reporting "OpenCode session start"
                      (funcall function value))))))
    (cl-flet ((fail (message) (funcall settle else message)))
      (if (not (eq engine 'opencode))
          (fail (format "OpenCode sessions run OpenCode, not %s" (drover-engine-label engine)))
        (condition-case err
            (drover-opencode--catalog-fetch
             'agent directory
             (lambda (catalog)
               (condition-case err
                   (progn
                     ;; A catalog that did not arrive proves nothing about the agent.
                     (when (and agent catalog
                                (not (seq-find (lambda (entry) (equal (alist-get 'id entry) agent))
                                               (opencode-primary-agents directory))))
                       (drover-warn "%s has no primary OpenCode agent %s; starting its default one"
                                    (abbreviate-file-name directory) agent)
                       (setq agent nil))
                     ;; A session created without an agent runs the default one but
                     ;; stores none, so that agent's reminder would be stored only
                     ;; after the first step, whose thinking then no longer matches
                     ;; the next request.
                     (when (and (not agent) catalog)
                       (setq agent (opencode-default-agent directory)))
                     (setq timer (run-at-time drover-opencode-timeout nil
                                              #'fail "Timed out waiting for the new OpenCode session"))
                     (opencode-session-create
                      :directory directory :title title :agent agent
                      :model (drover-opencode--new-session-model
                              (plist-get options :defaults) agent
                              (plist-get options :model) (plist-get options :variant))
                      :callback (lambda (info)
                                  (funcall settle then
                                           (drover-opencode--session-of
                                            opencode-session-id (or (alist-get 'title info) title)
                                            default-directory)))
                      :else (lambda (error) (fail (opencode-error-describe error)))))
                 (error (fail (drover-error-message err))))))
          (error (fail (drover-error-message err))))))))

(defun drover-opencode--options (session)
  "Return the model and variant SESSION runs, as (:model MODEL :variant VARIANT).
They are read from its buffer, else from the registry, and the cached
catalogs of its directory, never fetched: a model the catalog lacks
carries its identifiers only.  The variant is the one the model
reference names, else the model's default.  Nil when the model is
unknown.  The backend's :options."
  (let* ((buffer (drover-opencode--buffer session))
         (ref (if buffer
                  (buffer-local-value 'opencode-session-model buffer)
                (plist-get (opencode-state-get (drover-session-id session)) :model)))
         (directory (if buffer
                        (buffer-local-value 'default-directory buffer)
                      (or (drover-session-directory session) default-directory))))
    (when-let* ((ref)
                (model (if-let* ((info (opencode-model-info ref directory)))
                           (drover-opencode-model info (opencode-catalog 'provider directory))
                         (drover-opencode--ref-model ref))))
      (list :model model
            :variant (or (drover-opencode--ref-variant ref) (drover-model-default-variant model))))))

(defun drover-opencode--register ()
  "Register the `opencode' backend."
  (drover-register-backend
   'opencode
   :label "OpenCode"
   :start #'drover-opencode--start
   :sessions #'drover-opencode--sessions
   :session #'drover-opencode--session-by-id
   ;; Registry, buffers, catalogs and projects: all but the branches.
   :reports-changes t
   :open #'drover-opencode--open
   :respond #'drover-opencode--respond
   :interrupt #'drover-opencode--interrupt
   :archive #'drover-opencode--archive
   :archive-confirms t
   :unarchive #'drover-opencode--unarchive
   :unarchive-confirms t
   :archived #'drover-opencode--archived
   :mark-seen #'drover-opencode--mark-seen
   :preview #'drover-opencode--preview
   :recap #'drover-opencode--recap
   :new #'drover-opencode--new
   :buffer #'drover-opencode--buffer
   :close #'drover-opencode--close
   :rename #'drover-opencode--rename
   :delete #'drover-opencode--delete
   :fork #'drover-opencode--fork
   :compact #'drover-opencode--compact
   :history #'drover-opencode--history
   :prefetch #'drover-opencode--prefetch
   ;; Only a service of the fork lists them.
   :caches #'opencode-state-cache-supported-p
   :engines '(opencode)
   :create #'drover-opencode--create
   :check #'drover-opencode--check
   :apply-options #'drover-opencode--apply-options
   :send #'drover-opencode--send
   :options #'drover-opencode--options
   :blocker #'drover-opencode--blocker
   :submit #'drover-opencode--submit
   :ready #'drover-opencode--ready)
  (drover-register-resumer 'opencode 'opencode #'drover-opencode-resume :by-id t :async t))

(drover-opencode--register)

;;;; Session buffers

(defun drover-opencode--session-of (id &optional title directory parent)
  "Return the drover session ID of the OpenCode backend.
A session the registry does not list, such as an old or archived one,
is described by TITLE, DIRECTORY and PARENT, its parent's ID, as an
idle session, archived when the registry knows it is."
  (or (drover-find 'opencode id)
      (drover-session-create :backend 'opencode :id id :engine 'opencode :engine-id id
                             :title title :directory directory :parent parent
                             :archived (plist-get (opencode-state-get id) :archived))))

(defun drover-opencode--buffer-session ()
  "Return the session of the current OpenCode session buffer, or nil."
  (when opencode-session-id
    (drover-opencode--session-of opencode-session-id
                                 (alist-get 'title opencode-session-info)
                                 default-directory
                                 (alist-get 'parentID opencode-session-info))))

(defun drover-opencode--move-session ()
  "Move this session to another directory, which `cd' reads.
opencode.el asks the service to move a session whose buffer changes
directory."
  (interactive)
  (call-interactively #'cd))

(defun drover-opencode--clear-input ()
  "Erase the input being written at the prompt of this session buffer.
Everything after the process mark goes; \\[undo] brings it back."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (unless process
      (user-error "No prompt in this buffer"))
    (let ((start (marker-position (process-mark process))))
      (when (< start (point-max))
        (delete-region start (point-max))))))

(defun drover-opencode--compact-session ()
  "Compact the conversation of this session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (opencode-session-compact opencode-session-id))

(defconst drover-opencode--prompt-actions
  '((send . opencode-send-input)
    (clear . drover-opencode--clear-input)
    (attach . opencode-attach)
    (slash . opencode-insert-slash-command)
    (mention . opencode-mention-subagent)
    (revert . opencode-revert-to-point)
    (revert-cancel . opencode-revert-clear)
    (copy-block . opencode-yank-code-block)
    (agent . opencode-select-agent)
    (model . opencode-select-model)
    (effort . opencode-select-variant)
    (directory . drover-opencode--move-session)
    (compact . drover-opencode--compact-session)
    (fork . opencode-fork-session)
    (last-answer . opencode-scroll-to-last-response-start)
    (open-link . opencode-open-link-at-point)
    (schedule . drover-opencode-schedule-input))
  "The prompt actions of OpenCode session buffers, as (ACTION . COMMAND).")

(defun drover-opencode--setup-session-buffer ()
  "Tell drover the session and the prompt actions of this session buffer."
  (drover-with-reporting "OpenCode buffer"
    (setq-local drover-session-at-point-function #'drover-opencode--buffer-session)
    (pcase-dolist (`(,action . ,command) drover-opencode--prompt-actions)
      (drover-prompt-define action command))))

(defun drover-opencode--teardown-session-buffer ()
  "Undo `drover-opencode--setup-session-buffer' in this buffer."
  (when (eq drover-session-at-point-function #'drover-opencode--buffer-session)
    (kill-local-variable 'drover-session-at-point-function))
  ;; Actions others defined in the buffer stay.
  (pcase-dolist (`(,action . ,command) drover-opencode--prompt-actions)
    (when (eq (alist-get action drover-prompt-actions) command)
      (drover-prompt-define action nil))))

;;;; Earlier prompts

(defun drover-opencode-edit-old-input (text buffer)
  "Put TEXT, an earlier prompt of OpenCode session BUFFER, in drover's prompt.
The prompt shows, set to go to BUFFER's session, which the prompt's
send then reaches without the picker (see `drover-prompt-edit-for').
Attachments do not go with it: their labels stay as text.  For
`opencode-old-input-function', which drover sets to it when nil, so
that RET on an earlier prompt edits it in drover's prompt; set that
option to `opencode-put-old-input' to edit it in the session's input."
  (require 'drover-prompt)
  (let ((session (with-current-buffer buffer
                   (or (drover-opencode--buffer-session)
                       (user-error "Not an OpenCode session buffer")))))
    (drover-prompt-edit-for session (substring-no-properties text))))

;;;; The dispatch menu

(defconst drover-opencode--dispatch-suffixes
  '(("S" "Search sessions" opencode-consult-sessions))
  "Entries of opencode.el commands in `drover-dispatch', in order.")

(defun drover-opencode--add-to-dispatch ()
  "Add opencode.el's session commands to `drover-dispatch', after drover's lists.
They follow \"p\", the last of drover's commands listing sessions."
  (let ((after "p"))
    (dolist (suffix drover-opencode--dispatch-suffixes)
      (transient-append-suffix 'drover-dispatch after suffix)
      (setq after (car suffix)))))

(defun drover-opencode--remove-from-dispatch ()
  "Remove what `drover-opencode--add-to-dispatch' added to `drover-dispatch'."
  (dolist (suffix drover-opencode--dispatch-suffixes)
    (transient-remove-suffix 'drover-dispatch (nth 2 suffix))))

;;;; Key hints

(defconst drover-opencode--commands
  '((opencode-respond . drover-respond)
    (opencode-revert-clear . drover-prompt-revert-cancel)
    (opencode-interrupt . drover-interrupt))
  "Commands of opencode.el that its hints name, and drover's commands for them.")

(defun drover-opencode--command-keys (command)
  "Return the keys of `drover-dispatch' running opencode.el COMMAND, or nil.
They are the global key of the menu followed by the key of the entry,
such as \"C-\\=' A\".  For `opencode-command-keys-function'."
  (when-let* ((drover (alist-get command drover-opencode--commands))
              (menu (where-is-internal #'drover-dispatch nil t))
              (key (plist-get (cdr (ignore-errors (transient-get-suffix 'drover-dispatch drover)))
                              :key)))
    (concat (key-description menu) " " key)))

;;;; Installing

(defun drover-opencode--buffers (mode)
  "Return the live buffers whose major mode derives from MODE."
  (match-buffers `(derived-mode . ,mode)))

(defun drover-opencode--install ()
  "Fit opencode.el's session buffers, existing and future, into drover's keys.
Their earlier prompts are edited in drover's prompt, unless the user
chose a function of their own (see `drover-opencode-edit-old-input')."
  (add-hook 'opencode-session-mode-hook #'drover-opencode--setup-session-buffer)
  (dolist (buffer (drover-opencode--buffers 'opencode-session-mode))
    (with-current-buffer buffer
      (drover-opencode--setup-session-buffer)))
  (drover-opencode--add-to-dispatch)
  ;; A user's own function stays.
  (unless opencode-command-keys-function
    (setq opencode-command-keys-function #'drover-opencode--command-keys))
  (unless (bound-and-true-p opencode-old-input-function)
    (setq opencode-old-input-function #'drover-opencode-edit-old-input)))

(defun drover-opencode--uninstall ()
  "Undo `drover-opencode--install'."
  (remove-hook 'opencode-session-mode-hook #'drover-opencode--setup-session-buffer)
  (dolist (buffer (drover-opencode--buffers 'opencode-session-mode))
    (with-current-buffer buffer
      (drover-opencode--teardown-session-buffer)))
  (drover-opencode--remove-from-dispatch)
  (when (eq opencode-command-keys-function #'drover-opencode--command-keys)
    (setq opencode-command-keys-function nil))
  (when (eq (bound-and-true-p opencode-old-input-function) #'drover-opencode-edit-old-input)
    (setq opencode-old-input-function nil)))

(drover-opencode--install)

(defun drover-opencode-unload-function ()
  "Forget the `opencode' backend and stop following opencode.el.
OpenCode buffers lose drover's keys, and `drover-dispatch' the
commands of opencode.el."
  (remove-hook 'opencode-state-updated-functions #'drover-opencode--updated)
  (remove-hook 'opencode-session-buffer-functions #'drover-opencode--updated)
  (remove-hook 'opencode-catalog-changed-functions #'drover-opencode--catalog-changed)
  (opencode-events-unregister "project.updated" #'drover-opencode--projects-changed)
  (opencode-events-unregister "server.connected" #'drover-opencode--projects-changed)
  (dolist (type drover-opencode--moving-events)
    (opencode-events-unregister type #'drover-opencode--sessions-moved))
  (when (timerp drover-opencode--refetch-timer)
    (cancel-timer drover-opencode--refetch-timer))
  (drover-opencode--uninstall)
  (drover-unregister-backend 'opencode)
  (drover-unregister-resumer 'opencode 'opencode)
  (drover-opencode--unregister-engine)
  nil)

(provide 'drover-opencode)
;;; drover-opencode.el ends here
