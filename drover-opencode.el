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
;; The backend also fits opencode.el's buffers into drover's keys: a
;; session buffer tells its session (`drover-session-at-point-function')
;; and its prompt actions (`drover-prompt-define'); a session list tells
;; the session at point and gets the session keys
;; (`drover-session-keys-mode').  The menu `drover-dispatch' gains
;; opencode.el's session lists.
;;
;; `drover' loads this file once opencode.el is loaded.

;;; Code:

(require 'drover-core)
(require 'drover-keys)
(require 'opencode)
(require 'seq)
(require 'subr-x)

;; Recent additions to opencode.el.
(declare-function opencode-session-delete "opencode-session" (id))
(declare-function opencode-session-fork "opencode-session" (id &optional message-id))
(declare-function opencode-session-compact "opencode-session" (id))
(declare-function opencode-session-list-id-at-point "opencode-sessions" ())

;;;; Projects

(defvar drover-opencode--projects nil
  "The service's projects, as `opencode-fetch-projects' last returned them.")

(defvar drover-opencode--projects-fetching nil
  "Non-nil while the projects are being fetched.")

(defvar drover-opencode--projects-stale nil
  "Non-nil when the projects changed during the fetch in progress.")

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
           (setq drover-opencode--projects projects)
           (drover-opencode--projects-fetched)
           (drover-backend-changed 'opencode))
         ;; Project names only decorate sessions: keep the previous ones.
         (lambda (_error) (drover-opencode--projects-fetched)))
      (error (setq drover-opencode--projects-fetching nil)
             (signal (car err) (cdr err))))))

(defun drover-opencode--projects-fetched ()
  "Note the end of a projects fetch, fetching again when they changed meanwhile."
  (setq drover-opencode--projects-fetching nil)
  (when drover-opencode--projects-stale
    (drover-opencode--fetch-projects)))

(defun drover-opencode--projects-changed (_event)
  "Fetch the projects again after EVENT.
EVENT is a `project.updated' event, or `server.connected', after which
changes missed while disconnected are fetched like the registry is."
  (drover-opencode--fetch-projects))

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

(defun drover-opencode--model (entry)
  "Return the display name of the model of registry ENTRY, or nil."
  (when-let* ((model (plist-get entry :model)))
    ;; Model catalogs belong to a location.
    (let ((default-directory (or (plist-get entry :directory) default-directory)))
      (opencode-model-label model))))

(defun drover-opencode--session (entry)
  "Return the `drover-session' of `opencode-state' registry ENTRY."
  (let ((directory (plist-get entry :directory))
        (status (opencode-state-status entry)))
    (drover-session-create
     :backend 'opencode
     :id (plist-get entry :id)
     :title (plist-get entry :title)
     :directory directory
     :project (drover-opencode--project-name (plist-get entry :project-id) directory)
     :branch (and directory (opencode-git-branch directory))
     :parent (plist-get entry :parent-id)
     :status status
     :requests (mapcar (lambda (request)
                         (list :kind (plist-get request :kind)
                               :summary (plist-get request :summary)))
                       (plist-get entry :requests))
     :since (opencode-state-since entry)
     :updated (plist-get entry :updated)
     :agent (plist-get entry :agent)
     :model (drover-opencode--model entry)
     :detail (drover-opencode--detail entry status))))

(defun drover-opencode--sessions ()
  "Return the sessions of the `opencode-state' registry."
  (mapcar #'drover-opencode--session (opencode-state-sessions)))

;;;; Changes

(defun drover-opencode--updated (id)
  "Report a change of session ID to drover."
  (drover-backend-changed 'opencode id))

(defun drover-opencode--start ()
  "Follow the registry and the projects, and bootstrap the registry."
  (add-hook 'opencode-state-updated-functions #'drover-opencode--updated)
  (opencode-events-register "project.updated" #'drover-opencode--projects-changed)
  (opencode-events-register "server.connected" #'drover-opencode--projects-changed)
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
  "Answer the oldest request of SESSION in its buffer."
  (opencode-respond-session (drover-session-id session)))

(defun drover-opencode--interrupt (session)
  "Stop the running turn of SESSION."
  (opencode-session-interrupt (drover-session-id session)))

(defun drover-opencode--archive (session)
  "Archive SESSION on the service."
  (opencode-session-archive (drover-session-id session)))

(defun drover-opencode--mark-seen (session)
  "Mark the ended turn of SESSION viewed on the service, for every client."
  (opencode-session-view (drover-session-id session)))

(defun drover-opencode--preview (session callback)
  "Call CALLBACK with the last reply of SESSION, or nil."
  (opencode-session-last-reply (drover-session-id session) callback))

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

(defun drover-opencode--register ()
  "Register the `opencode' backend."
  (drover-register-backend
   'opencode
   :label "OpenCode"
   :start #'drover-opencode--start
   :sessions #'drover-opencode--sessions
   :open #'drover-opencode--open
   :respond #'drover-opencode--respond
   :interrupt #'drover-opencode--interrupt
   :archive #'drover-opencode--archive
   :mark-seen #'drover-opencode--mark-seen
   :preview #'drover-opencode--preview
   :new #'drover-opencode--new
   :buffer #'drover-opencode--buffer
   :rename #'drover-opencode--rename
   :delete #'drover-opencode--delete
   :fork #'drover-opencode--fork
   :compact #'drover-opencode--compact))

(drover-opencode--register)

;;;; Session buffers

(defun drover-opencode--session-of (id &optional title directory parent)
  "Return the drover session ID of the OpenCode backend.
A session the registry does not hold, such as an old one, is described
by TITLE, DIRECTORY and PARENT, its parent's ID, as an idle session."
  (or (drover-find 'opencode id)
      (drover-session-create :backend 'opencode :id id :title title
                             :directory directory :parent parent)))

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

(defun drover-opencode--compact-session ()
  "Compact the conversation of this session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (opencode-session-compact opencode-session-id))

(defconst drover-opencode--prompt-actions
  '((send . opencode-send-input)
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
    (open-link . opencode-open-link-at-point))
  "The prompt actions of OpenCode session buffers, as (ACTION . COMMAND).")

(defun drover-opencode--setup-session-buffer ()
  "Tell drover the session and the prompt actions of this session buffer."
  (setq-local drover-session-at-point-function #'drover-opencode--buffer-session)
  (pcase-dolist (`(,action . ,command) drover-opencode--prompt-actions)
    (drover-prompt-define action command)))

(defun drover-opencode--teardown-session-buffer ()
  "Undo `drover-opencode--setup-session-buffer' in this buffer."
  (when (eq drover-session-at-point-function #'drover-opencode--buffer-session)
    (kill-local-variable 'drover-session-at-point-function))
  ;; Actions others defined in the buffer, such as `schedule', stay.
  (pcase-dolist (`(,action . ,command) drover-opencode--prompt-actions)
    (when (eq (alist-get action drover-prompt-actions) command)
      (drover-prompt-define action nil))))

;;;; Session lists

(defun drover-opencode--list-session ()
  "Return the session of the row at point of an opencode.el session list, or nil."
  (when-let* ((id (opencode-session-list-id-at-point)))
    (drover-opencode--session-of id)))

(defun drover-opencode--setup-list ()
  "Give this opencode.el session list the session keys of drover."
  (setq-local drover-session-at-point-function #'drover-opencode--list-session
              drover-toggle-subagents-function #'opencode-session-list-toggle-subagents)
  (drover-session-keys-mode))

(defun drover-opencode--teardown-list ()
  "Undo `drover-opencode--setup-list' in this buffer."
  (when (eq drover-session-at-point-function #'drover-opencode--list-session)
    (kill-local-variable 'drover-session-at-point-function))
  (when (eq drover-toggle-subagents-function #'opencode-session-list-toggle-subagents)
    (kill-local-variable 'drover-toggle-subagents-function))
  (drover-session-keys-mode -1))

;;;; The dispatch menu

(defconst drover-opencode--dispatch-suffixes
  '(("p" "Pick project" opencode-select-project)
    ("S" "Search sessions" opencode-consult-sessions)
    ("1" "Project sessions" opencode)
    ("2" "All sessions" opencode-list-all-sessions))
  "Entries of opencode.el commands in `drover-dispatch', in order.")

(defun drover-opencode--add-to-dispatch ()
  "Add opencode.el's session commands to `drover-dispatch', before drover's views."
  (dolist (suffix drover-opencode--dispatch-suffixes)
    (transient-insert-suffix 'drover-dispatch "3" suffix)))

(defun drover-opencode--remove-from-dispatch ()
  "Remove what `drover-opencode--add-to-dispatch' added to `drover-dispatch'."
  (dolist (suffix drover-opencode--dispatch-suffixes)
    (transient-remove-suffix 'drover-dispatch (nth 2 suffix))))

;;;; Installing

(defun drover-opencode--buffers (mode)
  "Return the live buffers whose major mode derives from MODE."
  (match-buffers `(derived-mode . ,mode)))

(defun drover-opencode--install ()
  "Fit opencode.el's buffers, existing and future, into drover's keys."
  (add-hook 'opencode-session-mode-hook #'drover-opencode--setup-session-buffer)
  (add-hook 'opencode-session-list-mode-hook #'drover-opencode--setup-list)
  (dolist (buffer (drover-opencode--buffers 'opencode-session-mode))
    (with-current-buffer buffer
      (drover-opencode--setup-session-buffer)))
  (dolist (buffer (drover-opencode--buffers 'opencode-session-list-mode))
    (with-current-buffer buffer
      (drover-opencode--setup-list)))
  (drover-opencode--add-to-dispatch))

(defun drover-opencode--uninstall ()
  "Undo `drover-opencode--install'."
  (remove-hook 'opencode-session-mode-hook #'drover-opencode--setup-session-buffer)
  (remove-hook 'opencode-session-list-mode-hook #'drover-opencode--setup-list)
  (dolist (buffer (drover-opencode--buffers 'opencode-session-mode))
    (with-current-buffer buffer
      (drover-opencode--teardown-session-buffer)))
  (dolist (buffer (drover-opencode--buffers 'opencode-session-list-mode))
    (with-current-buffer buffer
      (drover-opencode--teardown-list)))
  (drover-opencode--remove-from-dispatch))

(drover-opencode--install)

(defun drover-opencode-unload-function ()
  "Forget the `opencode' backend and stop following opencode.el.
OpenCode buffers lose drover's keys, and `drover-dispatch' the
commands of opencode.el."
  (remove-hook 'opencode-state-updated-functions #'drover-opencode--updated)
  (opencode-events-unregister "project.updated" #'drover-opencode--projects-changed)
  (opencode-events-unregister "server.connected" #'drover-opencode--projects-changed)
  (drover-opencode--uninstall)
  (drover-unregister-backend 'opencode)
  nil)

(provide 'drover-opencode)
;;; drover-opencode.el ends here
