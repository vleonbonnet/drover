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
;; A session runs an engine, the agent program such as Claude Code,
;; which `drover-model' registers with its models and defaults; a
;; backend is the front end showing it.  Two backends may show one
;; conversation of an engine: drover lists it once.
;;
;; Every session has one status.  `waiting' means a permission request
;; or question waits for the user; `failed' and `done' are turns that
;; ended while the user was not looking; `retrying' and `working' run;
;; `idle' needs nothing.  `drover-status-order' ranks them, and
;; `drover-sort' puts the most urgent first, the longest wait first
;; within a status.
;;
;; Views draw `drover-view-groups': the sessions as trees, filtered by
;; `drover-filter', grouped as `drover-group-by' says and sorted as
;; `drover-sort-by' says.  Those settings are global, so every view
;; shows the same sessions.  With `drover-show-history', views also
;; list the older sessions backends keep (`drover-known-sessions').
;;
;; Commands act on the session at point, which every list of sessions
;; and every prompt buffer tells through
;; `drover-session-at-point-function'.  A prompt buffer, where the user
;; writes to an agent, also lists the prompt actions it implements in
;; `drover-prompt-actions'; `drover-prompt-target' finds the buffer a
;; prompt action applies to from wherever the user is.

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
  (created nil :documentation "Time, as a float, at which the session was created, or nil.")
  (agent nil :documentation "Agent name, or nil.")
  (model nil :documentation "Model name, or nil.")
  (detail nil :documentation "One line on the status, such as an error, or nil.")
  (engine nil :documentation "Symbol of the engine running the session, such as `claude-code', or nil.
Engines are the agent programs, registered with `drover-register-engine';
a backend is the front end showing the session.")
  (engine-id nil :documentation "The engine's own identifier of the conversation, or nil while unknown.
Two records of different backends with the same ENGINE and ENGINE-ID
are one session (see `drover-sessions').")
  (cache-expires nil :documentation "Time, as a float, at which the prompt cache expires, or nil.
The prompt cache is the provider's: until it expires, the next turn
reuses the conversation already sent.  Nil when unknown.")
  (cache-ttl nil :documentation "Lifetime of the prompt cache, in seconds, or nil.
It is the time from the last cache write to CACHE-EXPIRES."))

(defun drover-session-key (session)
  "Return the key of SESSION, unique across backends."
  (cons (drover-session-backend session) (drover-session-id session)))

(defun drover-session-engine-key (session)
  "Return (ENGINE . ENGINE-ID) of SESSION, or nil unless both are known.
It names the conversation across backends: two front ends may show the
same conversation of one engine."
  (let ((engine (drover-session-engine session))
        (id (drover-session-engine-id session)))
    (and engine id (cons engine id))))

(defun drover-session-attention-p (session)
  "Return non-nil when SESSION needs the user."
  (memq (drover-session-status session) drover-attention-statuses))

(defun drover-session-display-title (session)
  "Return the title of SESSION, or its identifier."
  (or (drover-session-title session) (drover-session-id session)))

(defun drover-session-summary (session)
  "Return one line on why SESSION has its status, or nil.
A waiting session shows its oldest request, another its detail."
  (if-let* ((request (car (drover-session-requests session))))
      (concat (if (eq (plist-get request :kind) 'question) "asks: " "permit: ")
              (plist-get request :summary))
    (drover-session-detail session)))

(defun drover-session-cache-remaining (session &optional now)
  "Return the seconds left before the prompt cache of SESSION expires, or nil.
It is nil when the expiry is unknown or past at NOW, the current time
by default."
  (when-let* ((expires (drover-session-cache-expires session))
              ((numberp expires))
              (remaining (- expires (or now (float-time))))
              ((> remaining 0)))
    remaining))

(defun drover-session-cache-fraction (session &optional now)
  "Return the fraction of its lifetime the prompt cache of SESSION has left.
The value is in (0, 1] at NOW, the current time by default, or nil when
the cache is expired or its expiry or lifetime is unknown."
  (when-let* ((remaining (drover-session-cache-remaining session now))
              (ttl (drover-session-cache-ttl session))
              ((and (numberp ttl) (> ttl 0))))
    (min 1.0 (/ remaining (float ttl)))))

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

(defun drover--parent-key (session by-key by-engine)
  "Return the key of the parent of SESSION among the sessions listed, or nil.
BY-KEY maps the keys of the sessions listed to them, BY-ENGINE their
engine keys.  A parent another backend shows, its own record merged
away (see `drover-sessions'), is found through the engine of SESSION,
when SESSION's backend names sessions by their engine identifiers, as
the OpenCode backend does; otherwise the parent is that of SESSION's
backend, listed or not."
  (when-let* ((parent (drover-session-parent session)))
    (let ((own (cons (drover-session-backend session) parent)))
      (if (gethash own by-key)
          own
        (let ((shown (and (drover-session-engine session)
                          (gethash (cons (drover-session-engine session) parent) by-engine))))
          (if shown (drover-session-key shown) own))))))

(defun drover-tree (sessions)
  "Return SESSIONS as trees, each subagent under its parent, most urgent first.
A tree ranks by the most urgent status within it, so a parent whose
subagent waits for the user comes with the sessions that need the user.
A subagent whose parent is not among SESSIONS is a tree of its own.
Trees and siblings that rank alike keep the order of SESSIONS."
  (let ((by-key (make-hash-table :test #'equal))
        (by-engine (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        (roots nil))
    (dolist (session sessions)
      (puthash (drover-session-key session) session by-key)
      (when-let* ((engine-key (drover-session-engine-key session)))
        (puthash engine-key session by-engine)))
    (dolist (session sessions)
      (let ((parent (drover--parent-key session by-key by-engine)))
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

(defun drover-tree-visible (nodes &optional shown)
  "Return trees NODES without the subtrees of subagents to hide.
A subagent tree is kept when `drover-show-children' is non-nil, when
some session in it needs the user, or when SHOWN, a function of a tree,
returns non-nil for it.  Each tree ranks by what remains visible, so a
hidden subagent never places its parent in a group its own status does
not explain."
  (sort (mapcar (lambda (node)
                  (drover--node
                   (drover-node-session node)
                   (drover-tree-visible
                    (seq-filter (lambda (child)
                                  (or drover-show-children
                                      (memq (drover-node-status child) drover-attention-statuses)
                                      (and shown (funcall shown child))))
                                (drover-node-children node))
                    shown)))
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
`drover-status-order', and empty groups are left out.  Statuses missing
from `drover-status-order' come last, in the order they first appear,
so that no tree goes unlisted."
  (let (groups)
    (dolist (node nodes)
      (let* ((status (drover-node-status node))
             (group (assq status groups)))
        (if group
            (setcdr group (cons node (cdr group)))
          (push (list status node) groups))))
    (setq groups (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
                         (nreverse groups)))
    (append (delq nil (mapcar (lambda (status) (assq status groups)) drover-status-order))
            (seq-remove (lambda (group) (memq (car group) drover-status-order)) groups))))

(defun drover--group-trees-by (nodes key)
  "Return trees NODES grouped as an alist (VALUE . NODES).
VALUE is what KEY, a function of a tree, returns for the trees of the
group.  Groups come in the order their first tree has in NODES."
  (let (groups)
    (dolist (node nodes)
      (let* ((value (funcall key node))
             (group (assoc value groups)))
        (if group
            (setcdr group (cons node (cdr group)))
          (push (list value node) groups))))
    (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
            (nreverse groups))))

(defun drover-group-trees-by-project (nodes)
  "Return trees NODES grouped as an alist (PROJECT . NODES).
A tree goes with the project of its root session; PROJECT is nil for
sessions without one.  A project comes before another when its most
urgent tree does."
  (drover--group-trees-by nodes (lambda (node) (drover-session-project (drover-node-session node)))))

(defun drover--start-of-day (&optional time)
  "Return the float time at which the day of TIME began, today by default."
  (pcase-let ((`(,_ ,_ ,_ ,day ,month ,year) (decode-time time)))
    ;; Midnight may fall on the other side of a daylight saving change.
    (float-time (encode-time (list 0 0 0 day month year nil -1 nil)))))

(defconst drover-activity-buckets '(today yesterday week older nil)
  "Buckets of last activity, newest first; nil holds unknown activity.")

(defun drover-activity-bucket (time &optional now)
  "Return the bucket of `drover-activity-buckets' TIME, a float time, falls in.
It is `today' or `yesterday' for those days of NOW, the current time
by default; `week' for the five days before yesterday, completing a
week; `older' before; nil when TIME is nil."
  (when time
    (let ((today (drover--start-of-day now)))
      (cond ((>= time today) 'today)
            ((>= time (drover--start-of-day (- today 1))) 'yesterday)
            ;; Six days back from the start of today: a week with today.
            ((>= time (drover--start-of-day (- today (* 5.5 86400)))) 'week)
            (t 'older)))))

(defconst drover-groupings '(status project branch agent model updated nil)
  "The values `drover-group-by' may take.")

(defun drover-group-trees (nodes grouping &optional now)
  "Return trees NODES grouped by GROUPING, as an alist (VALUE . NODES).
GROUPING is one of `drover-groupings':

 status   VALUE is the most urgent status of the tree, in the order of
          `drover-group-trees-by-status'.
 project, branch, agent, model
          VALUE is that of the root session, or nil; a group comes
          before another when its most urgent tree does.
 updated  VALUE is the bucket of the tree's last activity at NOW (see
          `drover-activity-bucket'), newest first.
 nil      One group, of VALUE nil, holding every tree; none without
          trees.

Within a group, trees keep their order in NODES."
  (pcase grouping
    ('status (drover-group-trees-by-status nodes))
    ('project (drover-group-trees-by-project nodes))
    ((or 'branch 'agent 'model)
     (let ((field (pcase grouping
                    ('branch #'drover-session-branch)
                    ('agent #'drover-session-agent)
                    (_ #'drover-session-model))))
       (drover--group-trees-by nodes (lambda (node) (funcall field (drover-node-session node))))))
    ('updated
     (let ((groups (drover--group-trees-by
                    nodes (lambda (node) (drover-activity-bucket (drover-node-updated node) now)))))
       (delq nil (mapcar (lambda (bucket) (assq bucket groups)) drover-activity-buckets))))
    ('nil (and nodes (list (cons nil nodes))))
    (_ (error "Unknown grouping %S" grouping))))

;;;; Sorting trees

(defconst drover-sort-fields '(urgency title project branch agent model updated created)
  "The values `drover-sort-by' may take.")

(defcustom drover-sort-by 'urgency
  "What session views sort sessions by, within each group.
The value is one of `drover-sort-fields': `urgency', the most urgent
first; `title', `project', `branch', `agent' or `model', in
alphabetical order; `updated', the last activity, or `created', the
time of creation, oldest first.  `drover-sort-descending' reverses the
order.  Subagents stay under their parent whatever the order."
  :type '(choice (const :tag "Urgency" urgency)
                 (const :tag "Title" title)
                 (const :tag "Project" project)
                 (const :tag "Branch" branch)
                 (const :tag "Agent" agent)
                 (const :tag "Model" model)
                 (const :tag "Last activity" updated)
                 (const :tag "Creation" created))
  :group 'drover)

(defcustom drover-sort-descending nil
  "Whether session views sort sessions in descending order of `drover-sort-by'."
  :type 'boolean
  :group 'drover)

(defun drover--sort-key (field node index)
  "Return the value tree NODE sorts by on FIELD, or nil when it has none.
INDEX is the position of NODE in the order of urgency.  Strings are
compared ignoring case; times are those of the root session, but for
`updated', the latest activity in the tree."
  (let ((session (drover-node-session node)))
    (pcase field
      ('urgency index)
      ('updated (drover-node-updated node))
      ('created (drover-session-created session))
      (_ (when-let* ((value (pcase field
                              ('title (drover-session-display-title session))
                              ('project (drover-session-project session))
                              ('branch (drover-session-branch session))
                              ('agent (drover-session-agent session))
                              ('model (drover-session-model session))
                              (_ (error "Unknown sort field %S" field))))
                     (text (format "%s" value))
                     ((not (string-empty-p text))))
           (downcase text))))))

(defun drover--sort-key-less-p (a b descending)
  "Return non-nil when sort key A comes before B, DESCENDING or not.
Keys are numbers, strings or nil; nil comes last either way."
  (cond ((null b) (and a t))
        ((null a) nil)
        (descending (if (numberp a) (> a b) (string< b a)))
        (t (if (numberp a) (< a b) (string< a b)))))

(defun drover-sort-trees (nodes &optional field descending)
  "Return trees NODES sorted by FIELD, DESCENDING when non-nil.
FIELD is one of `drover-sort-fields'; it defaults to `drover-sort-by',
and DESCENDING to `drover-sort-descending' when FIELD is nil too.
NODES must come most urgent first, as `drover-tree' returns them.  A
subagent never leaves its parent: the roots are sorted, then the
subagents of each session among themselves.  Sessions without a value
for FIELD come last, and ties keep the order of urgency."
  (let ((descending (if field descending drover-sort-descending))
        (field (or field drover-sort-by)))
    (if (and (eq field 'urgency) (not descending))
        nodes
      (cl-labels ((arrange (nodes)
                    (let ((keyed (seq-map-indexed
                                  (lambda (node index)
                                    (cons (drover--sort-key field node index)
                                          (drover-node--create
                                           :session (drover-node-session node)
                                           :children (arrange (drover-node-children node))
                                           :status (drover-node-status node)
                                           :since (drover-node-since node)
                                           :updated (drover-node-updated node))))
                                  nodes)))
                      ;; `sort' is stable: ties keep the order of urgency.
                      (mapcar #'cdr (sort keyed (lambda (a b)
                                                  (drover--sort-key-less-p (car a) (car b)
                                                                           descending)))))))
        (arrange nodes)))))

;;;; Backends

(defvar drover--backends nil
  "Registered backends, as an alist (NAME . PROPERTIES).")

(defvar drover--started nil
  "Names of the backends whose :start function ran.")

(defvar drover--running nil
  "Non-nil once `drover-start' ran; later backends start as they register.")

(defvar drover--history nil
  "Older sessions of the backends, as an alist (BACKEND . SESSIONS).
Filled by `drover-history-fetch' while `drover-show-history' is set.")

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
 :rename     Function of a session and a TITLE, a string, renaming it.
 :delete     Function of a session deleting it for good, without asking:
             drover asks the user first.
 :fork       Function of a session copying it whole into a new session,
             and displaying the copy.
 :compact    Function of a session compacting its conversation.
 :history    Function of a CALLBACK fetching the backend's older
             sessions, those :sessions does not report, and calling
             CALLBACK with them as `drover-session' records, as soon
             as they arrive; it may never call it, when they cannot be
             fetched.  Their statuses are what the backend reports of
             them, idle as a rule.  See `drover-show-history'.

The prompt (`drover-prompt') sends to sessions and starts them through
the following operations, all optional.  Callbacks are called at most
once, possibly before the operation returns; THEN receives the value,
ELSE a string describing the failure; nothing signals from a process
filter or a timer, but reports with `drover-warn'.  Options are plists
as `drover-model' describes them: explicit choices (:agent :model
:variant), the model a `drover-model' record, the variant a symbol.

 :engines    The engines whose sessions :create starts, as a list of
             symbols or a function of no argument returning one.  See
             `drover-backend-engines'.
 :create     Function of DIRECTORY, ENGINE, OPTIONS, THEN and ELSE
             starting and displaying a session of ENGINE in DIRECTORY.
             OPTIONS is a start-options plist: the explicit :agent,
             :model and :variant, unfiltered, the session's :title or
             nil, and the :defaults the prompt holds for ENGINE (see
             `drover-register-engine'), or nil.  It calls THEN with
             the new `drover-session' as soon as :send may be given
             it, :send waiting for the session to be ready, or ELSE.
 :check      Function of a SESSION, the TEXT about to be sent and the
             explicit OPTIONS about to be applied to it, already
             filtered for SESSION, running the checks that may ask the
             user before anything is sent.  It runs synchronously, from
             the command.  It returns the symbol `cancel' to send
             nothing, else a token handed to :send.
 :apply-options
             Function of a SESSION, explicit OPTIONS filtered for it and
             THEN, applying OPTIONS to the running SESSION and calling
             THEN exactly once, whatever happened.  What cannot be
             applied is reported with `drover-warn', the prompt being
             sent anyway.
 :send       Function of a SESSION, a TEXT and the TOKEN :check returned
             (nil without :check, or for a new session), submitting TEXT
             as a prompt once SESSION is ready.  A failure is reported
             with `drover-warn', saying the prompt is in the kill ring.
 :options    Function of a SESSION returning what it runs now, as a
             plist (:model MODEL :variant VARIANT), or nil.  It is
             called for every session a picker offers, so it must be
             cheap.
 :settable   Function of a SESSION returning which of :agent, :model
             and :variant the running SESSION can change; without it,
             all three.

A session's parent and subagents need no operation: drover finds them
among the sessions through `drover-session-parent'.

Only :label and :sessions are required.  Registering NAME again
replaces its properties and keeps its place: backends registered first
win over later ones showing the same session (see `drover-sessions').
A backend registered after `drover-start' ran is started at once."
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
        drover--started (delq name drover--started)
        drover--history (assq-delete-all name drover--history))
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

(defun drover-backend-engines (backend)
  "Return the engines whose sessions BACKEND, a name or a session, starts.
They are its :engines, a list or what the function there returns; nil
when it has none, or when the function fails."
  (let ((engines (drover-backend-property backend :engines)))
    (if (functionp engines)
        (with-demoted-errors "Drover backend engines: %S"
          (funcall engines))
      engines)))

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

(defun drover--backend-sessions (properties)
  "Return the sessions of the backend of PROPERTIES, or nil when they fail."
  (with-demoted-errors "Drover backend sessions: %S"
    (copy-sequence (funcall (plist-get properties :sessions)))))

(defun drover--registration-rank (backend)
  "Return how many of the registered backends were registered before BACKEND."
  (or (seq-position (reverse (drover-backends)) backend)
      (length drover--backends)))

(defun drover--preferred-backend (records)
  "Return the backend that keeps the session RECORDS describe.
RECORDS are records of one conversation by several backends.  It is
the backend whose buffer for its record the user selected last, else,
without any buffer, the one registered first."
  (let ((buffers (buffer-list))
        (best nil)
        (best-rank nil))
    (dolist (record records)
      (when-let* ((buffer (drover-session-buffer record))
                  (rank (seq-position buffers buffer)))
        (when (or (null best-rank) (< rank best-rank))
          (setq best (drover-session-backend record)
                best-rank rank))))
    (or best
        (car (sort (delete-dups (mapcar #'drover-session-backend records))
                   (lambda (a b) (< (drover--registration-rank a) (drover--registration-rank b))))))))

(defun drover--merge-duplicates (sessions)
  "Return SESSIONS with each conversation shown by one backend only.
Records of different backends with the same `drover-session-engine-key'
are one session: the records of the backend `drover--preferred-backend'
chooses stay, in their place, and the others go.  Records of one
backend are distinct sessions whatever their keys."
  (let ((groups (make-hash-table :test #'equal))
        (keepers (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (when-let* ((key (drover-session-engine-key session)))
        (push session (gethash key groups))))
    (maphash (lambda (key records)
               (when (cdr (delete-dups (mapcar #'drover-session-backend records)))
                 (puthash key (drover--preferred-backend (reverse records)) keepers)))
             groups)
    (if (zerop (hash-table-count keepers))
        sessions
      (seq-filter (lambda (session)
                    (let ((keeper (gethash (drover-session-engine-key session) keepers)))
                      (or (null keeper) (eq keeper (drover-session-backend session)))))
                  sessions))))

(defun drover-sessions ()
  "Return the sessions of every registered backend, unsorted.
A backend whose :sessions function fails contributes nothing.  These
are the sessions backends follow live; see `drover-known-sessions' for
those lists show.

Two front ends may show one conversation of an engine, such as an
OpenCode session both opencode.el and agent-shell show: records of
different backends with the same engine and engine identifier (see
`drover-session-engine-key') are one session, listed once.  The record
kept is that of the backend whose buffer for it the user selected last,
else that of the backend registered first."
  (drover--merge-duplicates
   (mapcan (lambda (entry) (drover--backend-sessions (cdr entry)))
           drover--backends)))

(defvar drover-show-history nil
  "Non-nil when session lists show older sessions too.
Older sessions are those backends keep beyond the ones they follow, as
their :history function fetches them.  Change it with
`drover-set-show-history', which fetches them.")

(defun drover--history-sessions ()
  "Return the older sessions fetched, of the backends still registered."
  (mapcan (lambda (entry)
            (and (assq (car entry) drover--backends)
                 (copy-sequence (cdr entry))))
          drover--history))

(defun drover-known-sessions (&optional live)
  "Return the sessions lists show, unsorted.
They are LIVE, the sessions `drover-sessions' returns by default, and
while `drover-show-history' is set, the older sessions fetched from the
backends; a live session wins over an older record of the same
session, whether of its own backend or of another one with the same
engine and engine identifier, and the first of several older records
of one backend wins.  Older records of different backends showing one
conversation are merged as `drover-sessions' merges live ones."
  (let ((live (or live (drover-sessions))))
    (if (not drover-show-history)
        live
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (session live)
          (puthash (drover-session-key session) t seen)
          (when-let* ((key (drover-session-engine-key session)))
            (puthash (cons 'engine key) t seen)))
        (append live
                (drover--merge-duplicates
                 (seq-remove (lambda (session)
                               (let ((key (drover-session-key session))
                                     (engine-key (drover-session-engine-key session)))
                                 (prog1 (or (gethash key seen)
                                            (and engine-key (gethash (cons 'engine engine-key) seen)))
                                   (puthash key t seen))))
                             (drover--history-sessions))))))))

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
  "Return the session ID of BACKEND, or nil.
A live session is preferred; while `drover-show-history' is set, an
older session fetched from BACKEND is found too.  The record is
BACKEND's own, even when `drover-sessions' lists the session through
another backend showing the same conversation."
  (cl-flet ((find (sessions)
              (seq-find (lambda (session)
                          (and (eq (drover-session-backend session) backend)
                               (equal (drover-session-id session) id)))
                        sessions)))
    (or (when-let* ((properties (alist-get backend drover--backends)))
          (find (drover--backend-sessions properties)))
        (and drover-show-history
             (find (cdr (assq backend drover--history)))))))

(defun drover-find-engine-session (engine engine-id)
  "Return a session of ENGINE whose engine identifier is ENGINE-ID, or nil.
It may be of any backend: the one `drover-sessions' lists, which is
that of a backend with a live buffer for it when there is one.  While
`drover-show-history' is set, an older session is found too, after
the live ones."
  (when (and engine engine-id)
    (let ((key (cons engine engine-id)))
      (seq-find (lambda (session) (equal (drover-session-engine-key session) key))
                (drover-known-sessions)))))

(defun drover-buffer-session (buffer)
  "Return the session BUFFER belongs to, or nil.
It is the session at point in BUFFER (see
`drover-session-at-point-function'), nil for a dead buffer or one that
tells no session."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (drover-session-at-point t))))

(defun drover-parent (session)
  "Return the parent session of SESSION, a subagent, or nil.
It is nil too when the parent is not among the sessions."
  (when-let* ((parent (drover-session-parent session)))
    (drover-find (drover-session-backend session) parent)))

(defun drover-subagents (session &optional sessions)
  "Return the sessions of the subagents of SESSION, most urgent first.
They are taken from SESSIONS, by default those lists show (see
`drover-known-sessions')."
  (let ((backend (drover-session-backend session))
        (id (drover-session-id session)))
    (drover-sort (seq-filter (lambda (candidate)
                               (and (eq (drover-session-backend candidate) backend)
                                    (equal (drover-session-parent candidate) id)
                                    (not (equal (drover-session-id candidate) id))))
                             (or sessions (drover-known-sessions))))))

;;;; Changes

(defvar drover-changed-hook nil
  "Hook run once backends stop reporting changes for `drover-refresh-delay'.
It also runs at once when view settings change, through
`drover-view-settings-changed': views redraw from it.")

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

;;;; View settings

(defcustom drover-group-by 'status
  "How session views group sessions.
The value is one of `drover-groupings': `status', the most urgent
first; `project', `branch', `agent' or `model', of the session at the
top of each tree; `updated', the last activity, as today, yesterday,
this week and older; or nil for no groups.  See `drover-group-trees'."
  :type '(choice (const :tag "By status" status)
                 (const :tag "By project" project)
                 (const :tag "By branch" branch)
                 (const :tag "By agent" agent)
                 (const :tag "By model" model)
                 (const :tag "By last activity" updated)
                 (const :tag "No groups" nil))
  :group 'drover)

(defvar drover-filter nil
  "Which sessions every view lists, as a plist; nil lists them all.
Its members are:

 :project   A project name, as `drover-session-project' returns it.
 :statuses  A list of statuses: sessions with one of them.
 :agent     An agent name, as `drover-session-agent' returns it.
 :model     A model name, as `drover-session-model' returns it.
 :since     A float time: sessions last updated then or later.
 :search    A string found, ignoring case, in the title, summary,
            project or branch of the session.

A nil or missing member, or an empty search string, matches every
session.  Change it with `drover-set-filter' so that views follow.")

(defvar-local drover-view-p nil
  "Non-nil in the buffers of drover's own session views.
Those follow `drover-filter', `drover-group-by' and `drover-sort-by'.")

(defconst drover--filter-keys '(:project :statuses :agent :model :since :search)
  "The members of `drover-filter', in the order they are described.")

(defun drover--filter-value (filter key)
  "Return member KEY of FILTER, or nil when it matches every session."
  (let ((value (plist-get filter key)))
    (unless (and (stringp value) (string-empty-p value))
      value)))

(defun drover-filter-active-p (&optional filter)
  "Return non-nil when FILTER, `drover-filter' by default, leaves sessions out."
  (let ((filter (or filter drover-filter)))
    (seq-some (lambda (key) (drover--filter-value filter key)) drover--filter-keys)))

(defun drover-filter-match-p (session &optional filter)
  "Return non-nil when SESSION satisfies FILTER, `drover-filter' by default.
SESSION must satisfy every member of FILTER that is set."
  (let* ((filter (or filter drover-filter))
         (project (drover--filter-value filter :project))
         (statuses (drover--filter-value filter :statuses))
         (agent (drover--filter-value filter :agent))
         (model (drover--filter-value filter :model))
         (since (drover--filter-value filter :since))
         (search (drover--filter-value filter :search)))
    (and (or (null project) (equal (drover-session-project session) project))
         (or (null statuses) (memq (drover-session-status session) statuses))
         (or (null agent) (equal (drover-session-agent session) agent))
         (or (null model) (equal (drover-session-model session) model))
         (or (null since)
             (let ((updated (drover-session-updated session)))
               (and updated (>= updated since))))
         (or (null search)
             (let ((case-fold-search t)
                   (regexp (regexp-quote search)))
               (seq-some (lambda (text) (and text (string-match-p regexp text)))
                         (list (drover-session-title session)
                               (drover-session-summary session)
                               (drover-session-project session)
                               (drover-session-branch session))))))))

(defun drover--filter-trees (nodes filter)
  "Return trees NODES cut down to FILTER, and the trees to show.
The value is (TREES . SHOWN): TREES keep the sessions matching FILTER
and the sessions above them; SHOWN is a hash table of the keys of the
sessions on the way to a match from its root, the match included, when
no session above that match matches too."
  (let ((shown (make-hash-table :test #'equal)))
    (cl-labels ((cut (node matched-above)
                  (let* ((session (drover-node-session node))
                         (match (drover-filter-match-p session filter))
                         (children (delq nil (mapcar (lambda (child)
                                                       (cut child (or matched-above match)))
                                                     (drover-node-children node)))))
                    (when (or match children)
                      ;; Kept, so a match is at or under it.
                      (unless matched-above
                        (puthash (drover-session-key session) t shown))
                      (drover--node session children)))))
      (cons (sort (delq nil (mapcar (lambda (node) (cut node nil)) nodes))
                  #'drover--node-before-p)
            shown))))

(defun drover-filter-trees (nodes &optional filter)
  "Return trees NODES cut down to FILTER, `drover-filter' by default.
A tree stays when a session in it matches; it keeps the sessions that
match and those above them, so a matching subagent shows under its
parent.  Each tree ranks by what remains."
  (car (drover--filter-trees nodes (or filter drover-filter))))

(defun drover--view-trees (sessions)
  "Return the trees of SESSIONS a view draws, and those it could draw.
The value is (VISIBLE . KEPT): KEPT are the trees `drover-filter'
leaves, VISIBLE the same without the subagents to hide."
  (let ((trees (drover-tree sessions)))
    (if (drover-filter-active-p)
        (pcase-let ((`(,kept . ,shown) (drover--filter-trees trees drover-filter)))
          (cons (drover-tree-visible
                 kept (lambda (node) (gethash (drover-session-key (drover-node-session node)) shown)))
                kept))
      (cons (drover-tree-visible trees) trees))))

(defun drover-view-trees (&optional sessions)
  "Return the trees a session view draws, most urgent first.
They are the trees of SESSIONS, by default of the sessions lists show,
those of `drover-known-sessions', cut down to `drover-filter' (see
`drover-filter-trees'), without the subagents to hide (see
`drover-tree-visible').  While a filter is set, a subagent that matches
it shows even when subagents are hidden, unless a session above it
matches too: the view then explains why each tree is listed without
listing every subagent of a matching session."
  (car (drover--view-trees (or sessions (drover-known-sessions)))))

(defun drover-view-hidden-count (&optional sessions)
  "Return how many subagents of SESSIONS a view hides.
SESSIONS defaults to the sessions lists show.  They are the subagents
of the trees `drover-filter' leaves that `drover-view-trees' leaves
out."
  (pcase-let ((`(,visible . ,kept) (drover--view-trees (or sessions (drover-known-sessions)))))
    (- (length (drover-tree-sessions kept)) (length (drover-tree-sessions visible)))))

(defun drover-view-groups (&optional sessions now)
  "Return the groups of trees a session view draws, in order.
The trees are those of `drover-view-trees' for SESSIONS, grouped as
`drover-group-by' says, at NOW for groups of last activity (see
`drover-group-trees'), and sorted within each group as `drover-sort-by'
and `drover-sort-descending' say (see `drover-sort-trees').  The value
is a list of (GROUP . TREES): GROUP is (GROUPING . VALUE), or nil when
sessions are not grouped.  It is nil when no tree is drawn."
  (mapcar (pcase-lambda (`(,value . ,nodes))
            (cons (and drover-group-by (cons drover-group-by value))
                  (drover-sort-trees nodes)))
          (drover-group-trees (drover-view-trees sessions) drover-group-by now)))

(defun drover-view-settings-changed ()
  "Redraw every session view at once, after view settings changed.
Views redraw from `drover-changed-hook', which runs now."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer))
  (drover--run-changed)
  (force-mode-line-update t))

(defun drover-set-filter (key value)
  "Set member KEY of `drover-filter' to VALUE, nil removing it, and redraw views.
KEY is one of :project, :statuses, :agent, :model, :since and :search."
  (unless (memq key drover--filter-keys)
    (error "Unknown filter member %S" key))
  (let ((filter (copy-sequence drover-filter)))
    (cl-remf filter key)
    (when (and value (not (equal value "")))
      (setq filter (append filter (list key value))))
    (setq drover-filter filter))
  (drover-view-settings-changed))

(defun drover-set-group-by (grouping)
  "Group sessions in every view by GROUPING, and redraw views.
GROUPING is one of `drover-groupings'."
  (unless (memq grouping drover-groupings)
    (error "Unknown grouping %S" grouping))
  (setq drover-group-by grouping)
  (drover-view-settings-changed))

(defun drover-set-sort (field &optional descending)
  "Sort sessions in every view by FIELD, DESCENDING when non-nil, and redraw.
FIELD is one of `drover-sort-fields'."
  (unless (memq field drover-sort-fields)
    (error "Unknown sort field %S" field))
  (setq drover-sort-by field
        drover-sort-descending (and descending t))
  (drover-view-settings-changed))

;;;; Older sessions

(defvar drover--history-generation 0
  "Count of the fetches of older sessions; answers to an earlier one are dropped.")

(defun drover-history-available-p ()
  "Return non-nil when a registered backend fetches older sessions."
  (seq-some (lambda (backend) (functionp (drover-backend-property backend :history)))
            (drover-backends)))

(defun drover-history-fetch ()
  "Fetch the older sessions of every backend that keeps them.
Each backend's answer replaces what it answered before and redraws the
views, as long as `drover-show-history' stays set and no later fetch
started meanwhile."
  (let ((generation (cl-incf drover--history-generation)))
    (dolist (backend (drover-backends))
      (when-let* ((function (drover-backend-property backend :history))
                  ((functionp function)))
        (with-demoted-errors "Drover backend history: %S"
          (funcall function
                   (lambda (sessions)
                     (when (and drover-show-history (= generation drover--history-generation))
                       (setf (alist-get backend drover--history) (copy-sequence sessions))
                       (drover-backend-changed backend)))))))))

(defun drover-set-show-history (show)
  "Show older sessions in every view when SHOW is non-nil, else hide them.
Showing them fetches them (see `drover-history-fetch'); hiding them
forgets them.  Views redraw at once, and again as older sessions
arrive."
  (setq drover-show-history (and show t))
  (if drover-show-history
      (drover-history-fetch)
    (cl-incf drover--history-generation)
    (setq drover--history nil))
  (drover-view-settings-changed))

;;;; Warnings and asynchronous steps

(defcustom drover-step-timeout 15
  "Seconds a step of `drover-run-steps' may take before the next one runs.
Steps are requests to a running session, such as switching its model
before a prompt is sent; one that takes longer is reported and given
up, so the prompt still goes out."
  :type 'number
  :group 'drover)

(defun drover-warn (format-string &rest args)
  "Report something that went wrong on its own, out of a command.
Such as an option a session refused, or a prompt it did not take.  The
message is a warning of type `drover', FORMAT-STRING and ARGS as for
`format-message'."
  (display-warning 'drover (apply #'format-message format-string args) :warning))

(defun drover-run-steps (steps then)
  "Run asynchronous STEPS in order, then call THEN with no argument.
STEPS is a list of (LABEL . FUNCTION): FUNCTION is called with NEXT, a
function of no argument it calls once its request settled, whichever
way.  A step that signals, or that has not called NEXT after
`drover-step-timeout' seconds, is reported with `drover-warn', naming
LABEL, and the next step runs; NEXT called late, or twice, does
nothing.  THEN is called exactly once."
  (if (null steps)
      (funcall then)
    (let* ((label (caar steps))
           (settled nil)
           (timer nil)
           (next (lambda ()
                   (unless settled
                     (setq settled t)
                     (when timer (cancel-timer timer))
                     (drover-run-steps (cdr steps) then)))))
      (setq timer (run-at-time drover-step-timeout nil
                               (lambda ()
                                 (unless settled
                                   (drover-warn "Timed out setting %s; sending the prompt anyway" label)
                                   (funcall next)))))
      (condition-case err
          (funcall (cdar steps) next)
        (error
         (unless settled
           (drover-warn "Could not set %s: %s; sending the prompt anyway"
                        label (error-message-string err))
           (funcall next)))))))

;;;; Git branches

(defcustom drover-branch-ttl 30
  "Seconds `drover-git-branch' keeps the branch of a directory it read."
  :type 'natnum
  :group 'drover)

(defvar drover--branches (make-hash-table :test #'equal)
  "Directories mapped to (BRANCH . TIME), as `drover-git-branch' read them.")

(defun drover--read-git-branch (directory)
  "Return the branch checked out in the worktree of DIRECTORY, or a short commit.
Return nil outside Git.  HEAD is read directly, without running Git."
  (when-let* ((root (locate-dominating-file directory ".git"))
              (dotgit (expand-file-name ".git" root))
              (gitdir (if (file-directory-p dotgit)
                          dotgit
                        ;; A linked worktree's .git file names its Git directory.
                        (with-temp-buffer
                          (insert-file-contents dotgit)
                          (when (re-search-forward "^gitdir: *\\(.+?\\) *$" nil t)
                            (expand-file-name (match-string 1) root)))))
              (head (expand-file-name "HEAD" gitdir))
              ((file-readable-p head)))
    (with-temp-buffer
      (insert-file-contents head)
      (cond ((looking-at "ref: refs/heads/\\(.+?\\) *$") (match-string 1))
            ((looking-at "[0-9a-f]\\{7\\}") (match-string 0))))))

(defun drover-git-branch (directory)
  "Return the Git branch of DIRECTORY, or a short commit when detached, or nil.
It is read from HEAD without running Git, so backends may call it for
every session they list: a directory's branch is kept for
`drover-branch-ttl' seconds.  Nil outside Git and for a missing
directory."
  (when directory
    (let* ((directory (file-name-as-directory (expand-file-name directory)))
           (entry (gethash directory drover--branches))
           (now (float-time)))
      (if (and entry (< (- now (cdr entry)) drover-branch-ttl))
          (car entry)
        (let ((branch (and (file-directory-p directory)
                           (ignore-errors (drover--read-git-branch directory)))))
          (puthash directory (cons branch now) drover--branches)
          branch)))))

;;;; The session at point

(defvar-local drover-session-at-point-function nil
  "Function of no argument returning the session at point, or nil.
Every list of sessions sets it in its buffer, and so does every buffer
belonging to one session, such as a prompt buffer, to return that
session.")

(defvar drover-session-keys-mode)

(defun drover-session-at-point (&optional noerror)
  "Return the session at point.
Without one, signal a `user-error', or return nil when NOERROR is
non-nil."
  (or (and drover-session-at-point-function
           (funcall drover-session-at-point-function))
      (unless noerror
        (user-error "No session at point"))))

(defun drover-session-list-p ()
  "Return non-nil when the current buffer lists sessions.
Drover's views are lists, and so are the buffers where
`drover-session-keys-mode' is on, the lists of other front ends."
  (or drover-view-p (bound-and-true-p drover-session-keys-mode)))

(defun drover--confirm (prompt session &optional strongly)
  "Ask PROMPT about SESSION, signalling a `user-error' unless the user agrees.
PROMPT is a format string for the session's title.  With STRONGLY, the
answer is typed in full."
  (unless (funcall (if strongly #'yes-or-no-p #'y-or-n-p)
                   (format prompt (drover-session-display-title session)))
    (user-error "Abort")))

;;;; Prompt actions

(defvar-local drover-prompt-actions nil
  "Prompt actions of the current buffer, as an alist (ACTION . FUNCTION).
ACTION is a symbol such as `send' or `revert', and FUNCTION the command
performing it, called with `call-interactively' in this buffer.  Define
actions with `drover-prompt-define'.  The actions drover knows are:

 send          Send the prompt.
 schedule      Schedule the prompt.
 clear         Erase the prompt being written.
 attach        Attach the region, or a file or buffer.
 slash         Insert a slash command.
 mention       Mention a subagent.
 revert        Stage a revert of what follows point.
 revert-cancel Cancel the staged revert.
 copy-block    Copy the code block at point.
 agent         Choose the agent.
 model         Choose the model.
 effort        Choose the reasoning effort.
 directory     Move the session to another directory.
 compact       Compact the conversation.
 fork          Fork the conversation from point.
 last-answer   Show the start of the last answer.
 open-link     Open the link at point.")

(autoload 'drover-prompt-default-buffer "drover-prompt")

(defcustom drover-default-prompt-buffer-function #'drover-prompt-default-buffer
  "Function of no argument returning the default prompt buffer, or nil.
The prompt actions `agent', `model', `effort' and `directory' apply to
that buffer when the current one has none of its own.  It is called
only when one of these actions runs, so it may create the buffer; menus
deciding whether the actions apply only check that it is set.  The
default is drover's own prompt, the buffer of `drover-prompt-dwim',
created on demand, so those actions choose what the next prompt runs
from anywhere; nil keeps them to prompt buffers and lists."
  :type '(choice (const :tag "None" nil) function)
  :group 'drover)

(defconst drover--default-prompt-actions '(agent model effort directory)
  "Prompt actions that fall back on `drover-default-prompt-buffer-function'.")

(defconst drover--backend-prompt-actions '((compact . :compact) (fork . :fork))
  "Prompt actions that, in a list, are backend operations on the session.")

(defconst drover--list-prompt-actions '(agent model effort directory last-answer)
  "Prompt actions that, in a list, apply to the buffer of the session at point.
The others edit or read what the user sees in a prompt buffer, the input
or the text at point, and so apply only there.")

(defun drover-prompt-define (action function)
  "Make FUNCTION perform prompt ACTION in the current buffer.
FUNCTION is a command, called with no argument; nil removes ACTION.
See `drover-prompt-actions' for the actions."
  (let ((actions (assq-delete-all action (copy-alist drover-prompt-actions))))
    (setq-local drover-prompt-actions
                (if function (cons (cons action function) actions) actions))))

(defun drover-prompt-defines-p (buffer action)
  "Return non-nil when BUFFER is live and defines prompt ACTION."
  (and (buffer-live-p buffer)
       (assq action (buffer-local-value 'drover-prompt-actions buffer))))

(defun drover--prompt-target (action session list &optional probe)
  "Return the buffer prompt ACTION applies to, or nil.
SESSION is the session at point, or nil; LIST is non-nil in a list of
sessions.  See `drover-prompt-target'.  With PROBE, only tell whether
ACTION applies: `drover-default-prompt-buffer-function', which may
create its buffer, is then not called, and its presence stands for the
symbol `default'."
  (cond
   ((drover-prompt-defines-p (current-buffer) action) (current-buffer))
   ((and list (assq action drover--backend-prompt-actions)) nil)
   ((and list session (memq action drover--list-prompt-actions)
         (let ((buffer (drover-session-buffer session)))
           (and (drover-prompt-defines-p buffer action) buffer))))
   ((memq action drover--default-prompt-actions)
    (when-let* ((function drover-default-prompt-buffer-function))
      (if probe
          'default
        (let ((buffer (funcall function)))
          (and (drover-prompt-defines-p buffer action) buffer)))))
   ((eq action 'last-answer)
    (seq-find (lambda (buffer) (drover-prompt-defines-p buffer action))
              (buffer-list)))))

(defun drover-prompt-target (action)
  "Return the buffer prompt ACTION applies to from the current buffer, or nil.
It is, in order:

 1. the current buffer, when it defines ACTION;
 2. in a list of sessions, for `agent', `model', `effort', `directory'
    and `last-answer', the live buffer of the session at point, when it
    defines ACTION; for `compact' and `fork' there is none, as the
    backend operates on the session instead;
 3. for `agent', `model', `effort' and `directory', the buffer
    `drover-default-prompt-buffer-function' returns, when it defines
    ACTION;
 4. for `last-answer', the most recently selected buffer defining it."
  (let ((list (drover-session-list-p)))
    (drover--prompt-target action (and list (drover-session-at-point t)) list)))

(defun drover--prompt-call (buffer action)
  "Perform prompt ACTION in BUFFER.
A buffer other than the current one is used in a window showing it, if
any, so that commands see the point the user sees; `last-answer' is
displayed and selected first."
  (let ((function (alist-get action (buffer-local-value 'drover-prompt-actions buffer)))
        window)
    (cond ((eq buffer (current-buffer)) (call-interactively function))
          ((eq action 'last-answer)
           (pop-to-buffer buffer)
           (call-interactively function))
          ((setq window (get-buffer-window buffer))
           (with-selected-window window
             (call-interactively function)))
          (t (with-current-buffer buffer
               (call-interactively function))))))

(defun drover-prompt-run (action)
  "Perform prompt ACTION where `drover-prompt-target' finds it.
Signal a `user-error' when no buffer applies."
  (drover--prompt-call (or (drover-prompt-target action)
                           (user-error "No prompt here to %s" (drover--prompt-action-label action)))
                       action))

(defun drover--prompt-action-label (action)
  "Return what prompt ACTION does, as a verb phrase for messages."
  (pcase action
    ('revert-cancel "cancel a revert")
    ('copy-block "copy a code block from")
    ('last-answer "show the last answer of")
    ('open-link "open a link from")
    ('slash "insert a slash command in")
    ('mention "mention a subagent in")
    ('effort "choose the effort of")
    ((or 'agent 'model) (format "choose the %s of" action))
    ('directory "move to another directory")
    (_ (symbol-name action))))

(defun drover--prompt-or-backend (action)
  "Perform prompt ACTION, or in a list its backend operation.
ACTION is `compact' or `fork'; in a list, the backend operates on the
session at point."
  (if-let* ((buffer (drover-prompt-target action)))
      (drover--prompt-call buffer action)
    (if (drover-session-list-p)
        (drover-backend-call (drover-session-at-point)
                             (alist-get action drover--backend-prompt-actions))
      (user-error "No prompt here to %s" (drover--prompt-action-label action)))))

;;;; Actions

(defvar-local drover-main-window-function nil
  "Function of no argument returning the window to show sessions in, or nil.
A list in a side window, such as `drover-list-side', sets it to the
window where the user works: `drover-open' displays sessions there, and
`drover-follow-mode' shows them there.")

(defun drover-open (session &optional other-window)
  "Display SESSION, in another window when OTHER-WINDOW is non-nil.
When the current buffer's `drover-main-window-function' returns a
window, SESSION is displayed in that window, which is selected, whatever
OTHER-WINDOW."
  (let ((window (and drover-main-window-function
                     (drover-supports-p session :open)
                     (funcall drover-main-window-function))))
    (when (window-live-p window)
      (select-window window))
    (drover-backend-call session :open (and (not (window-live-p window)) other-window))))

(defun drover-respond (session)
  "Answer the oldest request of SESSION.
Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (drover-backend-call session :respond))

(defun drover-interrupt (session)
  "Stop the running turn of SESSION.
Interactively, SESSION is the session at point, after confirmation."
  (interactive (let ((session (drover-session-at-point)))
                 (drover--confirm "Interrupt %s? " session)
                 (list session)))
  (drover-backend-call session :interrupt))

(defun drover-archive (session)
  "Archive SESSION.
Interactively, SESSION is the session at point, after confirmation."
  (interactive (let ((session (drover-session-at-point)))
                 (drover--confirm "Archive %s? " session)
                 (list session)))
  (drover-backend-call session :archive))

(defun drover-mark-seen (session)
  "Mark the ended turn of SESSION seen.
Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (drover-backend-call session :mark-seen))

(defun drover-preview (session callback)
  "Call CALLBACK with a string showing where SESSION stands, or nil."
  (drover-backend-call session :preview callback))

(defun drover-rename (session title)
  "Rename SESSION to TITLE.
Interactively, SESSION is the session at point, and TITLE is read with
its current title as the default."
  (interactive (let* ((session (drover-session-at-point))
                      (current (drover-session-title session))
                      (title (string-trim (read-string (format-prompt "Rename to" current)
                                                       nil nil current))))
                 (when (string-empty-p title)
                   (user-error "A session needs a title"))
                 (list session title)))
  (drover-backend-call session :rename title))

(defun drover-delete (session)
  "Delete SESSION for good.
Interactively, SESSION is the session at point, after confirmation."
  (interactive (let ((session (drover-session-at-point)))
                 (drover--confirm "Delete %s for good? " session t)
                 (list session)))
  (drover-backend-call session :delete))

(defun drover-fork (&optional session)
  "Fork SESSION whole into a new session, which its backend displays.
Interactively, or when SESSION is nil, fork from where the user is: the
prompt action `fork' of the buffer `drover-prompt-target' finds, which
forks the conversation up to point, else in a list the whole session
at point."
  (interactive)
  (if session
      (drover-backend-call session :fork)
    (drover--prompt-or-backend 'fork)))

(defun drover-compact (&optional session)
  "Compact the conversation of SESSION.
Interactively, or when SESSION is nil, compact from where the user is:
through the prompt action `compact' of the buffer `drover-prompt-target'
finds, else in a list the session at point."
  (interactive)
  (if session
      (drover-backend-call session :compact)
    (drover--prompt-or-backend 'compact)))

(provide 'drover-core)
;;; drover-core.el ends here
