;;; drover-core.el --- Sessions of every agent backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: internal

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

;; A backend is an agent front end, such as opencode.el, registered with
;; `drover-register-backend'.  It describes its sessions as
;; `drover-session' records and performs actions on them; drover keeps
;; no session state of its own beyond what the interface needs.  A
;; backend calls `drover-backend-changed' whenever its sessions change,
;; and drover runs `drover-changed-hook' once the burst is over.
;;
;; Every session has one status.  `waiting' means a permission request
;; or question waits for the user; `failed' and `done' are turns that
;; ended while the user was not looking; `retrying' and `working' run;
;; `idle' needs nothing.  `drover-status-order' ranks them, and
;; `drover-sort' puts the most urgent first, the longest wait first
;; within a status.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup drover nil
  "Manage the sessions of coding agents."
  :group 'tools
  :prefix "drover-")

(defcustom drover-status-order '(waiting failed done retrying working idle)
  "Session statuses, most urgent first."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-attention-statuses '(waiting failed done)
  "Statuses of the sessions that need the user."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-refresh-delay 0.1
  "Seconds of quiet after a backend change before `drover-changed-hook' runs."
  :type 'number
  :group 'drover)

(defcustom drover-show-children nil
  "Whether session lists show the sessions of subagents.
A child that needs the user is always shown."
  :type 'boolean
  :group 'drover)

;;;; Sessions

(cl-defstruct (drover-session (:constructor drover-session-create)
                              (:copier nil))
  "One agent session, as its backend describes it."
  (backend nil :documentation "Symbol of the backend that owns the session.")
  (id nil :documentation "Identifier of the session within its backend.")
  (title nil :documentation "Title, or nil.")
  (directory nil :documentation "Working directory, or nil.")
  (project nil :documentation "Project name, or nil.")
  (branch nil :documentation "Git branch of the directory, or nil.")
  (parent nil :documentation "Identifier of the parent session of a subagent, or nil.")
  (status 'idle :documentation "One of `drover-status-order'.")
  (requests nil :documentation "Pending requests, oldest first.
Each is a plist with :kind, `permission' or `question', and :summary.")
  (since nil :documentation "Time, as a float, at which STATUS began, or nil.")
  (updated nil :documentation "Time, as a float, of the last activity, or nil.")
  (agent nil :documentation "Agent name, or nil.")
  (model nil :documentation "Model name, or nil.")
  (detail nil :documentation "One line on the status, such as an error, or nil."))

(defun drover-session-key (session)
  "Return the key of SESSION, unique across backends."
  (cons (drover-session-backend session) (drover-session-id session)))

(defun drover-session-attention-p (session)
  "Return non-nil when SESSION needs the user."
  (memq (drover-session-status session) drover-attention-statuses))

(defun drover-status-rank (status)
  "Return the rank of STATUS in `drover-status-order', lowest most urgent."
  (or (seq-position drover-status-order status) (length drover-status-order)))

(defun drover--time (time)
  "Return TIME, or a time later than any for nil."
  (or time most-positive-fixnum))

(defun drover--before-p (status-a since-a updated-a status-b since-b updated-b)
  "Return non-nil when a session or tree A comes before another, B.
STATUS-A, SINCE-A and UPDATED-A describe A: its status, the time it
began and the time A was last updated; STATUS-B, SINCE-B and UPDATED-B
describe B.  The more urgent status comes first; within a status needing
the user, the longest wait; within another, the latest update."
  (let ((rank-a (drover-status-rank status-a))
        (rank-b (drover-status-rank status-b)))
    (cond
     ((/= rank-a rank-b) (< rank-a rank-b))
     ((memq status-a drover-attention-statuses)
      (< (drover--time since-a) (drover--time since-b)))
     (t (> (or updated-a 0) (or updated-b 0))))))

(defun drover-sort (sessions)
  "Return SESSIONS, most urgent first.
Within a status, sessions that need the user come longest wait first;
the others most recently updated first."
  (sort (copy-sequence sessions)
        (lambda (a b)
          (drover--before-p (drover-session-status a) (drover-session-since a) (drover-session-updated a)
                            (drover-session-status b) (drover-session-since b) (drover-session-updated b)))))

(defun drover-group-by-status (sessions)
  "Return SESSIONS sorted and grouped as an alist (STATUS . SESSIONS).
Statuses come in `drover-status-order'; empty groups are left out."
  (let ((sorted (drover-sort sessions)))
    (delq nil
          (mapcar (lambda (status)
                    (when-let* ((group (seq-filter (lambda (session)
                                                     (eq (drover-session-status session) status))
                                                   sorted)))
                      (cons status group)))
                  drover-status-order))))

(defun drover-group-by-project (sessions)
  "Return SESSIONS sorted and grouped as an alist (PROJECT . SESSIONS).
PROJECT is a name, or nil for sessions without one.  A project comes
before another when its most urgent session does."
  (let (groups)
    (dolist (session (drover-sort sessions))
      (let* ((project (drover-session-project session))
             (group (assoc project groups)))
        (if group
            (setcdr group (cons session (cdr group)))
          (push (list project session) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
            (nreverse groups))))

;;;; Trees

(cl-defstruct (drover-node (:constructor drover-node--create)
                           (:copier nil))
  "A session with the sessions of its subagents, nested."
  (session nil :documentation "The `drover-session' at the root of the tree.")
  (children nil :documentation "Trees of the session's subagents, most urgent first.")
  (status nil :documentation "The most urgent status in the tree.")
  (since nil :documentation "When the oldest wait of that status began, or nil.")
  (updated nil :documentation "The latest activity in the tree, or nil."))

(defun drover--node-before-p (a b)
  "Return non-nil when tree A comes before tree B."
  (drover--before-p (drover-node-status a) (drover-node-since a) (drover-node-updated a)
                    (drover-node-status b) (drover-node-since b) (drover-node-updated b)))

(defun drover--node (session children)
  "Return the tree of SESSION with CHILDREN, trees of its subagents."
  (let* ((children (sort children #'drover--node-before-p))
         (status (drover-session-status session))
         (since (drover-session-since session))
         (updated (drover-session-updated session)))
    (dolist (child children)
      (let ((rank (drover-status-rank (drover-node-status child))))
        (cond ((< rank (drover-status-rank status))
               (setq status (drover-node-status child)
                     since (drover-node-since child)))
              ((and (= rank (drover-status-rank status)) (drover-node-since child))
               (setq since (min (drover--time since) (drover-node-since child))))))
      (when (drover-node-updated child)
        (setq updated (max (or updated 0) (drover-node-updated child)))))
    (drover-node--create :session session :children children :status status
                         :since (unless (eql since most-positive-fixnum) since)
                         :updated updated)))

(defun drover-tree (sessions)
  "Return SESSIONS as trees, each subagent under its parent, most urgent first.
A tree ranks by the most urgent status within it, so a parent whose
subagent waits for the user comes with the sessions that need the user.
A subagent whose parent is not among SESSIONS is a tree of its own.
Trees and siblings that rank alike keep the order of SESSIONS."
  (let ((by-key (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (roots nil))
    (dolist (session sessions)
      (puthash (drover-session-key session) session by-key))
    (dolist (session sessions)
      (let ((parent (and (drover-session-parent session)
                         (cons (drover-session-backend session) (drover-session-parent session)))))
        (if (and parent (gethash parent by-key)
                 (not (equal parent (drover-session-key session))))
            (push session (gethash parent children))
          (push session roots))))
    (setq roots (nreverse roots))
    (let ((visiting (make-hash-table :test #'equal))
          (nodes nil))
      (cl-labels ((build (session)
                    (let ((key (drover-session-key session)))
                      ;; A cycle of parents would otherwise recurse forever.
                      (puthash key t visiting)
                      (drover--node session
                                    (delq nil (mapcar (lambda (child)
                                                        (unless (gethash (drover-session-key child) visiting)
                                                          (build child)))
                                                      (reverse (gethash key children))))))))
        (setq nodes (mapcar #'build roots))
        ;; Sessions whose parents form a cycle reach no root: each cycle
        ;; becomes a tree of its own rather than disappearing.
        (dolist (session sessions)
          (unless (gethash (drover-session-key session) visiting)
            (setq nodes (append nodes (list (build session))))))
        (sort nodes #'drover--node-before-p)))))

(defun drover-tree-sessions (nodes)
  "Return the sessions of the trees NODES, each before its subagents."
  (mapcan (lambda (node)
            (cons (drover-node-session node) (drover-tree-sessions (drover-node-children node))))
          nodes))

(defun drover-tree-visible (nodes)
  "Return trees NODES without the subtrees of subagents to hide.
A subagent tree is kept when `drover-show-children' is non-nil or when
some session in it needs the user.  Each tree ranks by what remains
visible, so a hidden subagent never places its parent in a group its
own status does not explain."
  (sort (mapcar (lambda (node)
                  (drover--node
                   (drover-node-session node)
                   (drover-tree-visible
                    (seq-filter (lambda (child)
                                  (or drover-show-children
                                      (memq (drover-node-status child) drover-attention-statuses)))
                                (drover-node-children node)))))
                nodes)
        #'drover--node-before-p))

(defun drover-tree-hidden-count (nodes)
  "Return how many subagent sessions of trees NODES are hidden.
They are those `drover-tree-visible' leaves out."
  (- (length (drover-tree-sessions nodes))
     (length (drover-tree-sessions (drover-tree-visible nodes)))))

(defun drover-group-trees-by-status (nodes)
  "Return trees NODES grouped as an alist (STATUS . NODES).
A tree goes in the group of its most urgent status; statuses come in
`drover-status-order', and empty groups are left out."
  (delq nil
        (mapcar (lambda (status)
                  (when-let* ((group (seq-filter (lambda (node) (eq (drover-node-status node) status))
                                                 nodes)))
                    (cons status group)))
                drover-status-order)))

(defun drover-group-trees-by-project (nodes)
  "Return trees NODES grouped as an alist (PROJECT . NODES).
A tree goes with the project of its root session; PROJECT is nil for
sessions without one.  A project comes before another when its most
urgent tree does."
  (let (groups)
    (dolist (node nodes)
      (let* ((project (drover-session-project (drover-node-session node)))
             (group (assoc project groups)))
        (if group
            (setcdr group (cons node (cdr group)))
          (push (list project node) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
            (nreverse groups))))

;;;; Backends

(defvar drover--backends nil
  "Registered backends, as an alist (NAME . PROPERTIES).")

(defvar drover--started nil
  "Names of the backends whose :start function ran.")

(defvar drover--running nil
  "Non-nil once `drover-start' ran; later backends start as they register.")

(defun drover-register-backend (name &rest properties)
  "Register backend NAME, a symbol, with PROPERTIES.
PROPERTIES is a plist of:

 :label      Display name.
 :start      Function of no argument preparing the backend, such as
             opening its connection; run once by `drover-start'.
 :sessions   Function of no argument returning the backend's sessions
             as `drover-session' records, from what it already knows.
 :open       Function of a session and OTHER-WINDOW displaying it.
 :respond    Function of a session answering its oldest request.
 :interrupt  Function of a session stopping its running turn.
 :archive    Function of a session archiving it.
 :mark-seen  Function of a session marking its ended turn seen.
 :preview    Function of a session and a CALLBACK, which it calls with
             a string showing where the session stands, or nil.
 :new        Function of a directory starting a session there.
 :buffer     Function of a session returning the live buffer showing
             it, or nil.

Only :label and :sessions are required.  Registering NAME again
replaces its properties.  A backend registered after `drover-start'
ran is started at once."
  (unless (functionp (plist-get properties :sessions))
    (error "Backend %s has no :sessions function" name))
  (setf (alist-get name drover--backends) properties)
  (when drover--running
    (drover--start-backend name))
  (drover-backend-changed name))

(defun drover-session-buffer (session)
  "Return the live buffer showing SESSION, or nil.
Backends without a :buffer function never report one."
  (when-let* ((function (drover-backend-property session :buffer))
              ((functionp function))
              (buffer (with-demoted-errors "Drover backend buffer: %S"
                        (funcall function session))))
    (and (bufferp buffer) (buffer-live-p buffer) buffer)))

(defun drover-unregister-backend (name)
  "Forget backend NAME."
  (setq drover--backends (assq-delete-all name drover--backends)
        drover--started (delq name drover--started))
  (drover-backend-changed name))

(defun drover-backends ()
  "Return the names of the registered backends."
  (mapcar #'car drover--backends))

(defun drover-backend-property (backend property)
  "Return PROPERTY of BACKEND, a name or a session."
  (let ((name (if (drover-session-p backend) (drover-session-backend backend) backend)))
    (plist-get (alist-get name drover--backends) property)))

(defun drover-backend-label (backend)
  "Return the display name of BACKEND, a name or a session."
  (or (drover-backend-property backend :label)
      (symbol-name (if (drover-session-p backend) (drover-session-backend backend) backend))))

(defun drover-supports-p (session operation)
  "Return non-nil when the backend of SESSION implements OPERATION.
OPERATION is a keyword such as :respond."
  (functionp (drover-backend-property session operation)))

(defun drover-backend-call (session operation &rest arguments)
  "Call OPERATION of SESSION's backend with SESSION and ARGUMENTS.
Signal a `user-error' when the backend does not implement it."
  (let ((function (drover-backend-property session operation)))
    (unless (functionp function)
      (user-error "%s sessions do not support %s"
                  (drover-backend-label session) (substring (symbol-name operation) 1)))
    (apply function session arguments)))

(defun drover--start-backend (name)
  "Run the :start function of backend NAME unless it ran already."
  (unless (memq name drover--started)
    (push name drover--started)
    (when-let* ((start (plist-get (alist-get name drover--backends) :start)))
      (with-demoted-errors "Drover backend start: %S"
        (funcall start)))))

(defun drover-start ()
  "Prepare every registered backend, and those registered from now on."
  (setq drover--running t)
  (mapc #'drover--start-backend (drover-backends)))

(defun drover-sessions ()
  "Return the sessions of every registered backend, unsorted.
A backend whose :sessions function fails contributes nothing."
  (mapcan (lambda (entry)
            (with-demoted-errors "Drover backend sessions: %S"
              (copy-sequence (funcall (plist-get (cdr entry) :sessions)))))
          drover--backends))

(defun drover-visible-sessions (&optional sessions)
  "Return SESSIONS, or every session, without hidden subagents.
Subagents are hidden unless `drover-show-children' is non-nil or they
need the user."
  (seq-filter (lambda (session)
                (or drover-show-children
                    (null (drover-session-parent session))
                    (drover-session-attention-p session)))
              (or sessions (drover-sessions))))

(defun drover-attention-sessions ()
  "Return the sessions that need the user, most urgent first."
  (drover-sort (seq-filter #'drover-session-attention-p (drover-sessions))))

(defun drover-find (backend id)
  "Return the session ID of BACKEND, or nil."
  (seq-find (lambda (session)
              (and (eq (drover-session-backend session) backend)
                   (equal (drover-session-id session) id)))
            (drover-sessions)))

;;;; Changes

(defvar drover-changed-hook nil
  "Hook run once backends stop reporting changes for `drover-refresh-delay'.")

(defvar drover--change-timer nil
  "Timer running `drover-changed-hook'.")

(defun drover--run-changed ()
  "Run `drover-changed-hook'."
  (setq drover--change-timer nil)
  (run-hooks 'drover-changed-hook))

(defun drover-backend-changed (&optional _backend _id)
  "Note that sessions of a backend changed.
The optional arguments name the backend and session, for callers that
know them.  `drover-changed-hook' runs after `drover-refresh-delay'
seconds without another change."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer))
  (setq drover--change-timer
        (run-at-time drover-refresh-delay nil #'drover--run-changed)))

(defun drover-changed-now ()
  "Run a pending `drover-changed-hook' at once."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer)
    (drover--run-changed)))

;;;; Actions

(defun drover-open (session &optional other-window)
  "Display SESSION, in another window when OTHER-WINDOW is non-nil."
  (drover-backend-call session :open other-window))

(defun drover-respond (session)
  "Answer the oldest request of SESSION."
  (drover-backend-call session :respond))

(defun drover-interrupt (session)
  "Stop the running turn of SESSION."
  (drover-backend-call session :interrupt))

(defun drover-archive (session)
  "Archive SESSION."
  (drover-backend-call session :archive))

(defun drover-mark-seen (session)
  "Mark the ended turn of SESSION seen."
  (drover-backend-call session :mark-seen))

(defun drover-preview (session callback)
  "Call CALLBACK with a string showing where SESSION stands, or nil."
  (drover-backend-call session :preview callback))

(provide 'drover-core)
;;; drover-core.el ends here
