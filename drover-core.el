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
;; naming the session when it can, and drover runs `drover-changed-hook'
;; once the burst is over, telling which sessions changed
;; (`drover-changed-sessions').  Drover keeps the live sessions as
;; backends last reported them in an index (`drover-index'), so that a
;; change of one session costs that session alone.
;;
;; A session runs an engine, the agent program such as Claude Code,
;; which `drover-model' registers with its models and defaults; a
;; backend is the front end showing it.  Two backends may show one
;; conversation of an engine: drover lists it once.  A backend may list
;; the conversations an engine recorded instead (:recorded), which a
;; front end following one replaces, and which open by resuming them in
;; a front end (`drover-resume').
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
;; `drover-sort-by' says, by default ungrouped, the most recently
;; active first, a session running a turn placed by the start of its
;; turn rather than its every event (`drover-session-activity'), with
;; the subagents `drover-show-subagents' shows.  Those settings are
;; global, so every view shows the same sessions.  With
;; `drover-show-history', views also list the older sessions backends
;; keep, and with `drover-show-archived' the archived ones
;; (`drover-known-sessions').  Backends without an archive of their own
;; have their sessions archived by drover, which keeps them in a file
;; (`drover-archive-session').
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

(declare-function no-littering-expand-var-file-name "ext:no-littering" (file))

(defcustom drover-state-directory
  (if (fboundp 'no-littering-expand-var-file-name)
      (no-littering-expand-var-file-name "drover/")
    (locate-user-emacs-file "drover/"))
  "Directory where drover keeps what it remembers between Emacs sessions.
Such as the models live sessions offered (see `drover-remember-models').
It is \"drover/\" in `user-emacs-directory', or in no-littering's
\"var/\" directory when no-littering is loaded before drover."
  :type 'directory
  :group 'drover)

(defconst drover-active-statuses '(waiting retrying working)
  "Statuses of the sessions in the middle of a turn.
Such a session works, waits to retry, or waits for the user to answer
before it goes on.")

(defconst drover-subagent-modes '(all active none)
  "The values `drover-show-subagents' may take.
They come in the order `drover-toggle-subagents' cycles through them.")

(defcustom drover-show-subagents 'active
  "Which sessions of subagents session lists show, under their parent.
The value is one of `drover-subagent-modes':

 all     Every subagent.
 active  The subagents in the middle of a turn, those whose status is
         one of `drover-active-statuses': working, waiting to retry, or
         waiting for you.  A subagent whose turn ended, unseen or not,
         is hidden, and so is a quiet one, unless a subagent of its own
         is active.
 none    No subagent.

Whatever the value, a subagent matching `drover-filter' shows when it
explains why its tree is listed (see `drover-view-trees').  A hidden
subagent never places its parent in a group; whether a parent has
active subagents, shown or not, its line tells.  Change it for every
view with `drover-toggle-subagents', U in lists, or with
`drover-set-show-subagents'."
  :type '(choice (const :tag "All subagents" all)
                 (const :tag "Active subagents" active)
                 (const :tag "No subagent" none))
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
  (model nil :documentation "Compact display name of the model, such as \"Opus 5.5\", or nil.
Lists show it in a column of its own, followed by EFFORT, and group,
sort and filter sessions by it, so it names the model alone: not its
provider, which PROVIDER tells, nor its effort.")
  (effort nil :documentation "Display name of the reasoning effort or variant the model runs, or nil.
Such as \"high\": the one chosen, or else the one the model runs by
default, when the backend knows it.")
  (effort-default nil :documentation "Non-nil when EFFORT is the model's default rather than a choice.
Lists dim a default effort, as the prompt dims what a new session
would inherit.")
  (provider nil :documentation "Compact display of the provider serving the model, or nil.
It may be an icon, such as a Nerd Font glyph; lists show it as it is,
in a column of its own, and fall back to PROVIDER-NAME without it.")
  (provider-name nil :documentation "Display name of the provider serving the model, or nil.
Such as \"GitHub Copilot\"; tooltips name the provider with it.")
  (context-tokens nil :documentation "Tokens the next request of the session sends again, or nil.
That is the size of its context, as of its last step.")
  (context-limit nil :documentation "Context size of the model, in tokens, or nil.
See `drover-session-context-fraction'.")
  (context-tier nil :documentation "The long-context price tier CONTEXT-TOKENS nears or passes, or nil.
It is (STATE SIZE LABEL): STATE is `near' or `over', SIZE the tokens
past which requests cost more, and LABEL how much more, such as \"2×\".")
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
It is the time from the last cache write to CACHE-EXPIRES.")
  (cache-tokens nil :documentation "Prompt tokens the prompt cache holds, or nil.")
  (cache-prompt-tokens nil :documentation "Prompt tokens of the last request, cached or not, or nil.
CACHE-TOKENS of them are in the prompt cache: the share a next turn
reads from the cache rather than sends anew.")
  (agent-display nil :documentation "The agent as lists show it, a string in its faces, or nil.
Such as the name the agent's catalog gives it, in the agent's colour.
Without it, lists show AGENT in a face of `drover-agent-faces'.  AGENT
stays what views group, sort and filter by.")
  ;; New slots go last, so that code compiled against the record before
  ;; them keeps reading the others at their place.
  (context-estimated nil :documentation "Non-nil when CONTEXT-TOKENS is the service's estimate after a compaction.
The next step measures it.")
  (archived nil :documentation "Time, as a float, at which the session was archived, t when unknown, or nil.
Archived sessions show only while `drover-show-archived' is set; see
`drover-session-archived-p'."))

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

(defun drover-session-active-p (session)
  "Return non-nil when SESSION is in the middle of a turn.
See `drover-active-statuses'."
  (memq (drover-session-status session) drover-active-statuses))

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

(defun drover-session-context-fraction (session)
  "Return how much of its model's context the context of SESSION fills.
The value is CONTEXT-TOKENS over CONTEXT-LIMIT, a float beyond 1 past
the limit, or nil when either is unknown."
  (let ((tokens (drover-session-context-tokens session))
        (limit (drover-session-context-limit session)))
    (and (numberp tokens) (numberp limit) (> limit 0)
         (/ tokens (float limit)))))

(defun drover-status-rank (status)
  "Return the rank of STATUS in `drover-status-order', lowest most urgent."
  ;; Sorting thousands of sessions ranks statuses tens of thousands of
  ;; times: `memq' rather than the generic `seq-position'.
  (let ((tail (memq status drover-status-order))
        (count (length drover-status-order)))
    (if tail (- count (length tail)) count)))

(defun drover--time (time)
  "Return TIME, or a time later than any for nil."
  (or time most-positive-fixnum))

;;;; Activity

;; A running session's last activity changes with every event of its
;; turn, many times a second: sorted by it, sessions running together
;; took the first place in turn.  Views sort a running session by when
;; its turn started instead, and every session by a time that moves
;; only once its activity moved further than a tolerance from it.

(defcustom drover-sort-activity-tolerance 60
  "Seconds a session's activity may move without moving the session in views.
Views ordering sessions by activity, the sort by last activity and the
order of urgency among sessions of one status, keep the time each
session sorts by while its activity stays within this many seconds of
it, and take the new time once it moves further: a session whose
activity moves a little, such as one going from done to idle, keeps its
place among sessions whose activity is close to it.  What changes then
is the session's own time, compared exactly with the others' times, so
that the order stays one order, the same for every view, drawn whole
or in place.  Nil or 0 sorts by the exact times.  See
`drover-session-activity'."
  :type '(choice (const :tag "Exact times" nil) (number :tag "Seconds"))
  :group 'drover)

(defun drover-session-running-p (session)
  "Return non-nil when SESSION runs a turn and does not wait for the user.
That is a status of `drover-active-statuses' that is not one of
`drover-attention-statuses': working, or waiting to retry."
  (and (drover-session-active-p session) (not (drover-session-attention-p session))))

(defun drover-session-last-activity (session)
  "Return the time of the last activity of SESSION, a float, or nil.
A session needing the user counts from when its status began, as it
has done nothing since; the others from their last activity.  Lists
show its age."
  (if (drover-session-attention-p session)
      (or (drover-session-since session) (drover-session-updated session))
    (or (drover-session-updated session) (drover-session-since session))))

(defvar drover--activity-times (make-hash-table :test #'equal)
  "The time each session of the index sorts by, by key.
The index notes it as it takes a record in (`drover--note-activity'),
and keeps it when made anew, so that every view sorts a session by the
same time, whether drawn whole or in place.")

(defun drover--activity-time (session)
  "Return the time the activity of SESSION places it at, or nil.
A running session is placed at the start of its turn, which its events
do not move; another at its last activity (see
`drover-session-last-activity')."
  (if (drover-session-running-p session)
      (or (drover-session-since session) (drover-session-updated session))
    (drover-session-last-activity session)))

(defun drover--tolerated-time (time kept)
  "Return KEPT while TIME is near it, else TIME.
Near is within `drover-sort-activity-tolerance' seconds."
  (if (and kept time
           (numberp drover-sort-activity-tolerance)
           (<= (abs (- time kept)) drover-sort-activity-tolerance))
      kept
    time))

(defun drover--sort-time (session)
  "Return the time SESSION sorts by, a float, or nil when unknown.
It is the time the index keeps for SESSION while the activity of
SESSION stays within `drover-sort-activity-tolerance' of it, else the
time of that activity (see `drover--activity-time')."
  (drover--tolerated-time (drover--activity-time session)
                          (gethash (drover-session-key session) drover--activity-times)))

(defun drover--note-activity (key record &optional times)
  "Note in TIMES the time RECORD, the record of session KEY, sorts by.
TIMES defaults to `drover--activity-times'.  RECORD nil forgets KEY."
  (let ((times (or times drover--activity-times))
        (time (and record (drover--sort-time record))))
    (if time (puthash key time times) (remhash key times))))

(defun drover-session-activity (session)
  "Return where the activity of SESSION places it, as (RUNNING . TIME).
RUNNING is non-nil when SESSION runs a turn (see
`drover-session-running-p'): its activity is now, whatever event came
last, so it is newer than any session not running.  TIME is when its
turn started for a running session, its last activity for another,
held while it moves within `drover-sort-activity-tolerance', or nil
when unknown."
  (cons (and (drover-session-running-p session) t) (drover--sort-time session)))

(defun drover--before-p (status-a since-a time-a status-b since-b time-b)
  "Return non-nil when a session or tree A comes before another, B.
STATUS-A, SINCE-A and TIME-A describe A: its status, the time it began
and the time A sorts by (see `drover-session-activity'); STATUS-B,
SINCE-B and TIME-B describe B.  The more urgent status comes first;
within a status needing the user, the longest wait; within another, the
latest activity, the newest turn among running ones."
  (let ((rank-a (drover-status-rank status-a))
        (rank-b (drover-status-rank status-b)))
    (cond
     ((/= rank-a rank-b) (< rank-a rank-b))
     ((memq status-a drover-attention-statuses)
      (< (drover--time since-a) (drover--time since-b)))
     (t (> (or time-a 0) (or time-b 0))))))

(defun drover-sort (sessions)
  "Return SESSIONS, most urgent first.
Within a status, sessions that need the user come longest wait first;
the others latest activity first, newest turn first for running ones,
see `drover-session-activity'."
  (mapcar #'car
          (sort (mapcar (lambda (session) (cons session (drover--sort-time session))) sessions)
                (lambda (a b)
                  (let ((session-a (car a))
                        (session-b (car b)))
                    (drover--before-p (drover-session-status session-a) (drover-session-since session-a) (cdr a)
                                      (drover-session-status session-b) (drover-session-since session-b) (cdr b)))))))

;;;; Trees

(cl-defstruct (drover-node (:constructor drover-node--create)
                           (:copier nil))
  "A session with the sessions of its subagents, nested."
  (session nil :documentation "The `drover-session' at the root of the tree.")
  (children nil :documentation "Trees of the session's subagents, most urgent first.")
  (status nil :documentation "The most urgent status in the tree.")
  (since nil :documentation "When the oldest wait of that status began, or nil.")
  (updated nil :documentation "The latest activity in the tree, or nil.")
  (activity nil :documentation "Where the activity of the tree places it, as (RUNNING . TIME), or nil.
RUNNING is non-nil when a session of the tree runs a turn: TIME is then
the earliest time its running sessions sort by, which the turns of
subagents starting under a running parent do not move; otherwise the
latest time a session of the tree sorts by.  See
`drover-session-activity'.  Nil when neither is known."))

(defun drover--node-before-p (a b)
  "Return non-nil when tree A comes before tree B."
  (drover--before-p (drover-node-status a) (drover-node-since a) (cdr (drover-node-activity a))
                    (drover-node-status b) (drover-node-since b) (cdr (drover-node-activity b))))

(defun drover--activity-join (a b)
  "Return where activities A and B place a tree holding both, as (RUNNING . TIME).
Each is a value of `drover-session-activity', or nil when unknown."
  (cond ((or (null a) (and (not (car a)) (null (cdr a)) b)) b)
        ((or (null b) (and (not (car b)) (null (cdr b)))) a)
        ((not (eq (car a) (car b))) (if (car a) a b))
        ((null (cdr a)) b)
        ((null (cdr b)) a)
        ((car a) (if (<= (cdr a) (cdr b)) a b))
        (t (if (>= (cdr a) (cdr b)) a b))))

(defun drover--node (session children)
  "Return the tree of SESSION with CHILDREN, trees of its subagents."
  (let* ((children (sort children #'drover--node-before-p))
         (status (drover-session-status session))
         (since (drover-session-since session))
         (updated (drover-session-updated session))
         (activity (drover-session-activity session)))
    (dolist (child children)
      (let ((rank (drover-status-rank (drover-node-status child))))
        (cond ((< rank (drover-status-rank status))
               (setq status (drover-node-status child)
                     since (drover-node-since child)))
              ((and (= rank (drover-status-rank status)) (drover-node-since child))
               (setq since (min (drover--time since) (drover-node-since child))))))
      (when (drover-node-updated child)
        (setq updated (max (or updated 0) (drover-node-updated child))))
      (setq activity (drover--activity-join activity (drover-node-activity child))))
    (drover-node--create :session session :children children :status status
                         :since (unless (eql since most-positive-fixnum) since)
                         :updated updated
                         :activity (and (or (car activity) (cdr activity)) activity))))

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
  (let* ((size (length sessions))
         (by-key (make-hash-table :test #'equal :size size))
         (by-engine (make-hash-table :test #'equal :size size))
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
    (let ((visiting (make-hash-table :test #'equal :size size))
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

(defun drover-tree-active-p (node)
  "Return non-nil when a session of tree NODE is in the middle of a turn.
See `drover-session-active-p'."
  (or (drover-session-active-p (drover-node-session node))
      (seq-some #'drover-tree-active-p (drover-node-children node))))

(defun drover--subagent-shown-p (node)
  "Return non-nil when subagent tree NODE shows.
It does as `drover-show-subagents' says."
  (pcase drover-show-subagents
    ('all t)
    ('none nil)
    (_ (drover-tree-active-p node))))

(defun drover-tree-visible (nodes &optional shown)
  "Return trees NODES without the subtrees of subagents to hide.
A subagent tree is kept as `drover-show-subagents' says: always, when a
session in it is in the middle of a turn, or never; or when SHOWN, a
function of a tree, returns non-nil for it.  Each tree ranks by what
remains visible, so a hidden subagent never places its parent in a
group its own status does not explain.  NODES are ranked, as
`drover-tree' returns them."
  (if (and (eq drover-show-subagents 'all) (not shown))
      ;; Every subagent shows: the trees as they are, ranked alike.
      nodes
    (sort (mapcar (lambda (node)
                    (let ((children nil))
                      (dolist (child (drover-node-children node))
                        (when (or (drover--subagent-shown-p child)
                                  (and shown (funcall shown child)))
                          (push child children)))
                      (drover--node (drover-node-session node)
                                    (drover-tree-visible (nreverse children) shown))))
                  nodes)
          #'drover--node-before-p)))

(defun drover-tree-hidden-count (nodes)
  "Return how many subagent sessions of trees NODES are hidden.
They are those `drover-tree-visible' leaves out."
  (- (length (drover-tree-sessions nodes))
     (length (drover-tree-sessions (drover-tree-visible nodes)))))

(defun drover-tree-active-subagents (nodes)
  "Return the active subagents under each session of trees NODES.
The value is a hash table mapping the key of each session with
subagents in the middle of a turn (see `drover-session-active-p'), at
any depth below it, to those subagents, most urgent first.  Pass the
whole trees, as `drover-tree' returns them, for a session to tell its
active subagents whether or not they are shown."
  (let ((table (make-hash-table :test #'equal)))
    (cl-labels ((walk (node)
                  ;; The active sessions of the tree, NODE's own included.
                  (let* ((session (drover-node-session node))
                         (below (mapcan #'walk (drover-node-children node))))
                    (when below
                      (puthash (drover-session-key session) (drover-sort below) table))
                    (if (drover-session-active-p session) (cons session below) below))))
      (mapc #'walk nodes))
    table))

(defun drover-group-trees-by-status (nodes)
  "Return trees NODES grouped as an alist (STATUS . NODES).
A tree goes in the group of its most urgent status; statuses come in
`drover-status-order', and empty groups are left out.  Statuses missing
from `drover-status-order' come last, in the order they first appear,
so that no tree goes unlisted.  A tree whose root session is archived
goes in the group `archived', after every other (see
`drover-show-archived')."
  (let (groups)
    (dolist (node nodes)
      (let* ((status (if (drover-session-archived (drover-node-session node))
                         'archived
                       (drover-node-status node)))
             (group (assq status groups)))
        (if group
            (setcdr group (cons node (cdr group)))
          (push (list status node) groups))))
    (setq groups (mapcar (lambda (group) (cons (car group) (nreverse (cdr group))))
                         (nreverse groups)))
    (append (delq nil (mapcar (lambda (status) (assq status groups)) drover-status-order))
            (seq-remove (lambda (group) (memq (car group) (cons 'archived drover-status-order)))
                        groups)
            (and (assq 'archived groups) (list (assq 'archived groups))))))

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

(defun drover-tree-activity-bucket (node &optional now)
  "Return the bucket of `drover-activity-buckets' tree NODE falls in at NOW.
A tree running a turn is active today, whenever its turn started; another
falls where the time it sorts by does (see `drover-node-activity'), so
that groups of last activity follow the order of that sort."
  (let ((activity (drover-node-activity node)))
    (if (car activity)
        'today
      (drover-activity-bucket (cdr activity) now))))

(defconst drover-groupings '(status project branch agent model provider updated nil)
  "The values `drover-group-by' may take.")

(defun drover-group-trees (nodes grouping &optional now)
  "Return trees NODES grouped by GROUPING, as an alist (VALUE . NODES).
GROUPING is one of `drover-groupings':

 status   VALUE is the most urgent status of the tree, or `archived'
          for a tree whose root is archived, in the order of
          `drover-group-trees-by-status'.
 project, branch, agent, model
          VALUE is that of the root session, or nil; a group comes
          before another when its most urgent tree does.
 provider VALUE is the provider name of the root session (see
          `drover-session-provider-name'), or nil, likewise.
 updated  VALUE is the bucket of the tree's last activity at NOW (see
          `drover-tree-activity-bucket'), newest first.
 nil      One group, of VALUE nil, holding every tree; none without
          trees.

Within a group, trees keep their order in NODES."
  (pcase grouping
    ('status (drover-group-trees-by-status nodes))
    ('project (drover-group-trees-by-project nodes))
    ((or 'branch 'agent 'model 'provider)
     (let ((field (pcase grouping
                    ('branch #'drover-session-branch)
                    ('agent #'drover-session-agent)
                    ('model #'drover-session-model)
                    (_ #'drover-session-provider-name))))
       (drover--group-trees-by nodes (lambda (node) (funcall field (drover-node-session node))))))
    ('updated
     (let ((groups (drover--group-trees-by
                    nodes (lambda (node) (drover-tree-activity-bucket node now)))))
       (delq nil (mapcar (lambda (bucket) (assq bucket groups)) drover-activity-buckets))))
    ('nil (and nodes (list (cons nil nodes))))
    (_ (error "Unknown grouping %S" grouping))))

;;;; Sorting trees

(defconst drover-sort-fields '(urgency title project branch agent model provider updated created)
  "The values `drover-sort-by' may take.")

(defcustom drover-sort-by 'updated
  "What session views sort sessions by, within each group.
The value is one of `drover-sort-fields': `urgency', the most urgent
first; `title', `project', `branch', `agent', `model' or `provider',
the provider's name, in alphabetical order; `updated', the last
activity, or `created', the time of creation, oldest first.
`drover-sort-descending' reverses the order.  Subagents stay under
their parent whatever the order.

By last activity, a session running a turn is active now, whatever
event of its turn came last: running sessions come after the others,
among themselves in the order their turns started, so that the events
of a turn do not move its session.  The others come in the order of
their last activity, a session moving only once its activity moved
further than `drover-sort-activity-tolerance' from where it was placed
\(see `drover-session-activity').  Newest first, the default, lists the
running sessions first, the turn started last on top, then the others,
the most recently active first."
  :type '(choice (const :tag "Urgency" urgency)
                 (const :tag "Title" title)
                 (const :tag "Project" project)
                 (const :tag "Branch" branch)
                 (const :tag "Agent" agent)
                 (const :tag "Model" model)
                 (const :tag "Provider" provider)
                 (const :tag "Last activity" updated)
                 (const :tag "Creation" created))
  :group 'drover)

(defcustom drover-sort-descending t
  "Whether session views sort sessions in descending order of `drover-sort-by'."
  :type 'boolean
  :group 'drover)

(defun drover--sort-key (field node index)
  "Return the value tree NODE sorts by on FIELD, or nil when it has none.
INDEX is the position of NODE in the order of urgency.  Strings are
compared ignoring case; times are those of the root session, but for
`updated', where the activity of the tree places it (see
`drover-node-activity')."
  (let ((session (drover-node-session node)))
    (pcase field
      ('urgency index)
      ('updated (drover-node-activity node))
      ('created (drover-session-created session))
      (_ (when-let* ((value (pcase field
                              ('title (drover-session-display-title session))
                              ('project (drover-session-project session))
                              ('branch (drover-session-branch session))
                              ('agent (drover-session-agent session))
                              ('model (drover-session-model session))
                              ('provider (drover-session-provider-name session))
                              (_ (error "Unknown sort field %S" field))))
                     (text (format "%s" value))
                     ((not (string-empty-p text))))
           (downcase text))))))

(defun drover--sort-key-less-p (a b descending)
  "Return non-nil when sort key A comes before B, DESCENDING or not.
Keys are numbers, strings, activities (RUNNING . TIME) or nil; nil
comes last either way.  An activity running comes after one that does
not, and before it DESCENDING; activities alike compare their times,
an unknown time last either way."
  (cond ((null b) (and a t))
        ((null a) nil)
        ((consp a)
         (if (eq (not (car a)) (not (car b)))
             (drover--sort-key-less-p (cdr a) (cdr b) descending)
           (if descending (and (car a) t) (and (car b) t))))
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
                                           :updated (drover-node-updated node)
                                           :activity (drover-node-activity node))))
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

(defvar drover--archived nil
  "Archived sessions of the backends, as an alist (BACKEND . SESSIONS).
Filled by `drover-archived-fetch' while `drover-show-archived' is set,
from the backends' :archived.")

(defun drover-register-backend (name &rest properties)
  "Register backend NAME, a symbol, with PROPERTIES.
PROPERTIES is a plist of:

 :label      Display name.
 :start      Function of no argument preparing the backend, such as
             opening its connection; run once by `drover-start'.
 :sessions   Function of no argument returning the backend's sessions
             as `drover-session' records, from what it already knows:
             those it follows, but archived ones (see :archived).
 :session    Function of an ID returning the record :sessions would
             return for session ID, or nil when :sessions would not
             list it.  It is called for each session the backend
             reports changed (see `drover-backend-changed'), so it must
             be cheap; without it, drover lists all the backend's
             sessions on every change.
 :open       Function of a session and OTHER-WINDOW displaying it, in
             the selected window, or in another window when
             OTHER-WINDOW is non-nil.  `drover-open' chooses the window
             and selects it first, giving no OTHER-WINDOW, so that the
             sessions of every backend show in the same windows.
 :respond    Function of a session answering its oldest request.
 :interrupt  Function of a session stopping its running turn.
 :archive    Function of a session archiving it.  Sessions of a
             backend without it drover archives itself (see
             `drover-archive-session').
 :unarchive  Function of a session, archived, restoring it from the
             archive.
 :archived   Function of a CALLBACK fetching the backend's archived
             sessions, and calling CALLBACK with them as
             `drover-session' records, their ARCHIVED set, as soon as
             they arrive; like :history, it must never wait, and may
             never call CALLBACK.  See `drover-show-archived'.
 :mark-seen  Function of a session marking its ended turn seen.
 :preview    Function of a session and a CALLBACK, which it calls with
             a string showing where the session stands, or nil.
 :new        Function of a directory starting a session there.
 :buffer     Function of a session returning the live buffer showing
             it, or nil.
 :close      Function of a session closing its buffer, if it has one,
             without asking: archiving closes the buffer of the session
             archived (see `drover-archive-session').  What the buffer
             holds that the agent never got, such as input typed at its
             prompt, it first puts in the kill ring (see
             `drover-keep-unsent'), and returns non-nil when it did.
             Without it, archiving leaves the buffer open.
 :runs-in-buffer
             Non-nil when a session's buffer runs its agent, as a
             terminal or a shell does, rather than show a session that
             runs elsewhere: closing it would end a turn in progress, so
             archiving closes it only once no turn runs.
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
 :prefetch   Function of a list of SESSIONS a view shows, starting in
             the background the fetches of what their records lack
             and cost a request to know, such as the size of their
             context, and reporting what arrives with
             `drover-backend-changed'.  Views call it whenever they
             draw, so it must cost nothing for sessions whose details
             are known, being fetched, or cannot be fetched.  See
             `drover-prefetch'.
 :recap      Function of a session and a CALLBACK fetching a recap of
             its last turn, and calling CALLBACK once with it, possibly
             before returning; it must never wait.  See `drover-recap'
             for the recap.
 :supports   Function of a session and an OPERATION, a keyword the
             backend implements, returning nil when OPERATION does not
             apply to that session, such as :fork for a session whose
             agent cannot fork.  Without it, every operation the
             backend implements applies to all its sessions.  See
             `drover-supports-p'.  A backend not supporting :archive
             for a session has it archived by drover.
 :recorded   The engines whose conversations the backend lists from
             what they record themselves, such as transcripts on disk,
             rather than sessions a front end shows: a list of symbols.
             Such a backend's record of a conversation another backend
             follows, with the same `drover-session-engine-key', is not
             listed, the other backend's session standing for it.  Its
             session identifiers are the engine's identifiers, and its
             :session, and its :history if any, answer at once.

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
        drover--history (assq-delete-all name drover--history)
        drover--archived (assq-delete-all name drover--archived))
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
  "Return non-nil when the backend of SESSION performs OPERATION on it.
OPERATION is a keyword such as :respond.  The backend must implement
it, and its :supports function, if any, must allow it for SESSION."
  (and (functionp (drover-backend-property session operation))
       (let ((supports (drover-backend-property session :supports)))
         (or (not (functionp supports))
             (with-demoted-errors "Drover backend supports: %S"
               (funcall supports session operation))))
       t))

(defun drover--operation-name (operation)
  "Return OPERATION, a keyword, as a verb for messages."
  (substring (symbol-name operation) 1))

(defun drover-backend-call (session operation &rest arguments)
  "Call OPERATION of SESSION's backend with SESSION and ARGUMENTS.
Signal a `user-error' when the backend does not implement it, or when
its :supports function says it does not apply to SESSION."
  (let ((function (drover-backend-property session operation)))
    (unless (functionp function)
      (user-error "%s sessions do not support %s"
                  (drover-backend-label session) (drover--operation-name operation)))
    (unless (drover-supports-p session operation)
      (user-error "%s cannot %s" (drover-session-display-title session)
                  (drover--operation-name operation)))
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

(defun drover--backend-session (properties id)
  "Return session ID of the backend of PROPERTIES, through its :session, or nil.
Nil too when :session fails."
  (with-demoted-errors "Drover backend session: %S"
    (funcall (plist-get properties :session) id)))

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

(defun drover--recording-backends (engine)
  "Return the registered backends listing the conversations ENGINE records.
See :recorded in `drover-register-backend'."
  (let ((backends nil))
    (pcase-dolist (`(,name . ,properties) drover--backends)
      (when (memq engine (plist-get properties :recorded))
        (push name backends)))
    (nreverse backends)))

(defun drover--recorded-p (backend)
  "Return non-nil when BACKEND lists conversations as their engines record them.
BACKEND is a name."
  (and (plist-get (alist-get backend drover--backends) :recorded) t))

(defun drover--unshadowed (sessions)
  "Return SESSIONS but the records of conversations another backend follows.
A backend listing conversations as their engine records them (see
:recorded in `drover-register-backend') yields to a backend following
one, with the same engine key, among SESSIONS."
  (let ((engines (mapcan (lambda (entry) (copy-sequence (plist-get (cdr entry) :recorded)))
                         drover--backends))
        (followed nil))
    (when engines
      (dolist (session sessions)
        (when-let* (((memq (drover-session-engine session) engines))
                    ((not (drover--recorded-p (drover-session-backend session))))
                    (key (drover-session-engine-key session)))
          (unless followed
            (setq followed (make-hash-table :test #'equal)))
          (puthash key t followed))))
    (if (not followed)
        sessions
      (seq-remove (lambda (session)
                    (and (drover--recorded-p (drover-session-backend session))
                         (gethash (drover-session-engine-key session) followed)))
                  sessions))))

(defun drover--live-sessions ()
  "Return the sessions every registered backend follows, as they report them.
They are not merged, and those drover archived are among them; a
conversation another backend follows is left out of those a backend
lists as its engine records them (see `drover--unshadowed')."
  (drover--unshadowed (mapcan (lambda (entry) (drover--backend-sessions (cdr entry)))
                              drover--backends)))

(defun drover-sessions ()
  "Return the sessions of every registered backend, unsorted.
A backend whose :sessions function fails contributes nothing.  These
are the sessions backends follow live, but those drover archived (see
`drover-archive-session'); see `drover-known-sessions' for those lists
show.

Two front ends may show one conversation of an engine, such as an
OpenCode session both opencode.el and agent-shell show: records of
different backends with the same engine and engine identifier (see
`drover-session-engine-key') are one session, listed once.  The record
kept is that of the backend whose buffer for it the user selected last,
else that of the backend registered first."
  (drover--merge-duplicates (drover--unarchived (drover--live-sessions))))

(defun drover-prefetch (sessions)
  "Ask the backends of SESSIONS to fetch what their records lack.
Each backend with a :prefetch function is given its own SESSIONS, in
their order, once; what arrives it reports as a change.  Views call it
for the sessions they show, whenever they draw, so that a detail
costing a request is fetched for those sessions alone.  A backend whose
:prefetch fails is reported, the others asked all the same."
  (let ((by-backend nil))
    (dolist (session sessions)
      (push session (alist-get (drover-session-backend session) by-backend)))
    (pcase-dolist (`(,backend . ,records) (nreverse by-backend))
      (when-let* ((function (drover-backend-property backend :prefetch))
                  ((functionp function)))
        (with-demoted-errors "Drover backend prefetch: %S"
          (funcall function (nreverse records)))))))

(defvar drover-show-history nil
  "Non-nil when session lists show older sessions too.
Older sessions are those backends keep beyond the ones they follow, as
their :history function fetches them.  Change it with
`drover-set-show-history', which fetches them.")

(defvar drover-show-archived nil
  "Non-nil when session lists show archived sessions too.
They are those the backends' :archived functions fetch, and those
drover archived itself (see `drover-archive-session').  Change it with
`drover-set-show-archived', which fetches them.")

(defun drover--fetched-sessions (fetched)
  "Return the sessions of FETCHED, an alist (BACKEND . SESSIONS), still registered."
  (mapcan (lambda (entry)
            (and (assq (car entry) drover--backends)
                 (copy-sequence (cdr entry))))
          fetched))

(defun drover--history-sessions ()
  "Return the older sessions fetched, of the backends still registered.
The conversations drover archived are left out."
  (drover--unarchived (drover--fetched-sessions drover--history)))

(defun drover--recorded-history ()
  "Return the older conversations the recording backends list, by engine key.
They are what the :history of each backend recording conversations
answers at once (see :recorded in `drover-register-backend'), as a hash
table mapping each engine key to its record; the first record wins."
  (let ((table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,_ . ,properties) drover--backends)
      (when-let* (((plist-get properties :recorded))
                  (history (plist-get properties :history))
                  ((functionp history)))
        (with-demoted-errors "Drover backend history: %S"
          (funcall history
                   (lambda (sessions)
                     (dolist (session sessions)
                       (when-let* ((key (drover-session-engine-key session))
                                   ((not (gethash key table))))
                         (puthash key session table))))))))
    table))

(defun drover-archived-sessions (&optional live)
  "Return the archived sessions lists show while `drover-show-archived' is set.
They are those the backends' :archived functions fetched, those a
backend reports among the sessions it follows with their ARCHIVED set,
and the conversations drover archived itself: the record of a backend
following one live, among LIVE, the sessions `drover--live-sessions'
returns by default, else the record of a backend listing it among the
older conversations its engine records, else the record drover kept of
it, when its backend is still registered.  Each has its ARCHIVED set.
Records of different backends showing one conversation are merged as
`drover-sessions' merges them."
  (let* ((live (or live (drover--live-sessions)))
         (followed (make-hash-table :test #'equal))
         (own nil))
    (dolist (session live)
      (when-let* ((key (drover-session-engine-key session))
                  ((drover--archive-entry session)))
        (push (drover--mark-archived session) (gethash key followed))))
    (let ((older 'unread))
      (pcase-dolist (`(,key . ,entry) (drover--archive))
        (if-let* ((records (gethash key followed)))
            (setq own (append (nreverse records) own))
          (when (eq older 'unread)
            (setq older (drover--recorded-history)))
          (if-let* ((recorded (gethash key older)))
              (push (drover--mark-archived recorded) own)
            (when (assq (plist-get entry :backend) drover--backends)
              (push (drover--archive-record key entry) own))))))
    (append (drover--merge-duplicates (append (nreverse own)
                                              (seq-filter #'drover-session-archived live)))
            (mapcar (lambda (session)
                      (if (drover-session-archived session)
                          session
                        (let ((copy (copy-sequence session)))
                          (setf (drover-session-archived copy) t)
                          copy)))
                    (drover--fetched-sessions drover--archived)))))

(defun drover--not-seen (sessions seen)
  "Return SESSIONS but those SEEN holds, noting those kept in SEEN.
SEEN is a hash table of the keys of the sessions listed already, and of
their engine keys under (engine . KEY).  The first of several records
of one backend's session wins; records of different backends showing
one conversation are merged as `drover-sessions' merges them, and only
then is their conversation noted, so that the merge chooses among
them."
  (let ((kept (drover--merge-duplicates
               (seq-remove (lambda (session)
                             (let ((key (drover-session-key session))
                                   (engine-key (drover-session-engine-key session)))
                               (prog1 (or (gethash key seen)
                                          (and engine-key (gethash (cons 'engine engine-key) seen)))
                                 (puthash key t seen))))
                           sessions))))
    (dolist (session kept)
      (when-let* ((key (drover-session-engine-key session)))
        (puthash (cons 'engine key) t seen)))
    kept))

(defun drover-known-sessions (&optional live)
  "Return the sessions lists show, unsorted.
They are LIVE, the sessions `drover-sessions' returns by default; while
`drover-show-archived' is set, the archived sessions (see
`drover-archived-sessions'); while `drover-show-history' is set, the
older sessions fetched from the backends.  A live session wins over
another record of the same session, whether of its own backend or of
another one with the same engine and engine identifier, an archived
record over an older one, and the first of several records of one
backend wins.  Records of different backends showing one conversation
are merged as `drover-sessions' merges live ones."
  (let ((live (or live (drover-sessions))))
    (if (not (or drover-show-history drover-show-archived))
        live
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (session live)
          (puthash (drover-session-key session) t seen)
          (when-let* ((key (drover-session-engine-key session)))
            (puthash (cons 'engine key) t seen)))
        (append live
                (and drover-show-archived (drover--not-seen (drover-archived-sessions) seen))
                (and drover-show-history (drover--not-seen (drover--history-sessions) seen)))))))

(defun drover-visible-sessions (&optional sessions)
  "Return SESSIONS, or every session, without hidden subagents.
Subagents show as `drover-show-subagents' says: all of them, those in
the middle of a turn, or none."
  (seq-filter (lambda (session)
                (or (null (drover-session-parent session))
                    (pcase drover-show-subagents
                      ('all t)
                      ('none nil)
                      (_ (drover-session-active-p session)))))
              (or sessions (drover-sessions))))

(defun drover-surfaced-sessions (&optional sessions)
  "Return SESSIONS, or the live sessions, that drover points the user to.
They are the sessions lists show under `drover-show-subagents', every
subagent of `all', those in the middle of a turn and the way to them in
`active', none in `none' (see `drover-tree-visible'), and every
subagent waiting for the user, whatever the mode: it blocks its parent
until answered.  `drover-filter', which narrows lists only, does not
apply.  The mode-line count, `drover-next' and `drover-select' offer
them.  They keep their order in SESSIONS."
  (let ((sessions (or sessions (drover-sessions)))
        (shown (make-hash-table :test #'equal)))
    (dolist (session (drover-tree-sessions (drover-tree-visible (drover-tree sessions))))
      (puthash (drover-session-key session) t shown))
    (seq-filter (lambda (session)
                  (or (gethash (drover-session-key session) shown)
                      (eq (drover-session-status session) 'waiting)))
                sessions)))

(defun drover-attention-sessions ()
  "Return the sessions that need the user, most urgent first.
They are those of `drover-surfaced-sessions' whose status is one of
`drover-attention-statuses': the subagents lists hide do not count, but
those waiting for the user."
  (drover-sort (seq-filter #'drover-session-attention-p (drover-surfaced-sessions))))

(defun drover-find (backend id)
  "Return the session ID of BACKEND, or nil.
A live session is preferred, whether or not drover archived it; while
`drover-show-archived' is set, an archived session of BACKEND is found
too, and while `drover-show-history' is set, an older one.  The record
is BACKEND's own, even when `drover-sessions' lists the session through
another backend showing the same conversation; one drover archived has
its ARCHIVED set."
  (cl-flet ((find (sessions)
              (seq-find (lambda (session)
                          (and (eq (drover-session-backend session) backend)
                               (equal (drover-session-id session) id)))
                        sessions)))
    (when-let* ((session (or (when-let* ((properties (alist-get backend drover--backends)))
                               (if (functionp (plist-get properties :session))
                                   (drover--backend-session properties id)
                                 (find (drover--backend-sessions properties))))
                             (and drover-show-archived
                                  (find (drover-archived-sessions)))
                             (and drover-show-history
                                  (find (cdr (assq backend drover--history)))))))
      (drover--mark-archived session))))

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
`drover-view-settings-changed': views redraw from it.  While it runs,
`drover-changed-sessions' tells which sessions changed.")

(defvar drover-changed-sessions t
  "The sessions that changed, while `drover-changed-hook' runs.
A list of the keys of the sessions whose records changed, appeared or
went, or whose place among the trees of sessions changed (see
`drover-index-parent'), as `drover-index' has them now; or t when
anything may have changed, such as view settings or the sessions of a
backend drover could not follow one by one, and views draw everything
anew.  A key may be listed though its record is the same: a backend
reports a session whose buffer opened or closed, which its record does
not tell.")

(defvar drover--change-timer nil
  "Timer running `drover-changed-hook'.")

(defvar drover--changed-keys nil
  "Keys of the sessions reported changed since the hook last ran.")

(defvar drover--changed-backends nil
  "Backends reported changed, without a session, since the hook last ran.")

(defvar drover--changed-all nil
  "Non-nil when views must draw everything anew the next time the hook runs.")

(defvar drover--changed-applied nil
  "What the index took in of the changes reported since the hook last ran.
Keys, or t, as `drover-changed-sessions' tells them: a view drawing
before the hook runs brings the index up to date first (see
`drover-index'), and the hook then tells what changed all the same.")

(defun drover--apply-changed ()
  "Bring the index up to date with the changes reported, noting what changed.
See `drover--changed-applied'."
  (when (or drover--changed-keys drover--changed-backends)
    (let ((keys (nreverse drover--changed-keys))
          (backends (nreverse drover--changed-backends)))
      (setq drover--changed-keys nil
            drover--changed-backends nil)
      (let ((changed (drover--index-apply keys backends)))
        (setq drover--changed-applied
              (if (or (eq changed t) (eq drover--changed-applied t))
                  t
                (append drover--changed-applied changed)))))))

(defun drover--run-changed ()
  "Bring the index up to date, then run `drover-changed-hook'."
  (setq drover--change-timer nil)
  (drover--apply-changed)
  (let ((drover-changed-sessions (if drover--changed-all
                                     t
                                   (if (listp drover--changed-applied)
                                       (delete-dups drover--changed-applied)
                                     t))))
    (setq drover--changed-applied nil
          drover--changed-all nil)
    (run-hooks 'drover-changed-hook)))

(defun drover--schedule-changed ()
  "Run `drover-changed-hook' after `drover-refresh-delay' seconds of quiet."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer))
  (setq drover--change-timer
        (run-at-time drover-refresh-delay nil #'drover--run-changed)))

(defun drover-backend-changed (&optional backend id)
  "Note that sessions of BACKEND changed: session ID, or any of them.
`drover-changed-hook' runs after `drover-refresh-delay' seconds without
another change.  A change of session ID costs drover the backend's
:session for ID alone, when the backend has one; a change of BACKEND
without ID, listing all its sessions anew.  Without BACKEND either,
views draw everything anew."
  (cond ((and backend id) (push (cons backend id) drover--changed-keys))
        (backend (push backend drover--changed-backends))
        (t (setq drover--changed-all t
                 drover--changed-backends (append (drover-backends) drover--changed-backends))))
  (drover--schedule-changed))

(defun drover-changed-now ()
  "Run a pending `drover-changed-hook' at once."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer)
    (drover--run-changed)))

;;;; The index of sessions

;; Listing every session costs a record per session, and views used to
;; list them on every change: with thousands of sessions, a change cost
;; tens of milliseconds before anything was drawn.  The index keeps the
;; records `drover-sessions' would list, and the trees they make,
;; current through what backends report: a change of one session costs
;; its backend's :session for it, and tells views which sessions
;; changed (`drover-changed-sessions'), so that they draw only those.

(cl-defstruct (drover--index (:constructor drover--index-make)
                             (:copier nil))
  "The live sessions, kept current as backends report changes."
  (records (make-hash-table :test #'equal)
           :documentation "Copies of the records listed, by key: unarchived, not merged.")
  (serials (make-hash-table :test #'equal)
           :documentation "When each key was first listed, a number, for the listing order.")
  (serial 0 :documentation "The last serial number given.")
  (engines (make-hash-table :test #'equal)
           :documentation "Keys of the records of each engine key.")
  (shared 0 :documentation "How many engine keys have several records, to merge.")
  (parents (make-hash-table :test #'equal)
           :documentation "The key of the parent of each session that has one listed.")
  (children (make-hash-table :test #'equal)
            :documentation "The keys of the subagents of each session that has some.")
  (claims (make-hash-table :test #'equal)
          :documentation "Keys of the sessions naming each (BACKEND . ID) their parent.")
  (engine-claims (make-hash-table :test #'equal)
                 :documentation "Keys of the sessions whose parent may be each engine key.")
  (pending nil :documentation "Engine keys whose followers changed, for recorded backends to look at again.
See `drover--index-settle'.")
  (cyclic nil :documentation "Non-nil when parents form a cycle."))

(defvar drover--index nil
  "The index of the live sessions, or nil until a view asks for it.")

(defun drover--index-listed (record)
  "Return RECORD as `drover-sessions' lists it, a copy, or nil when it does not."
  (and record
       (not (drover-session-archived record))
       (not (drover--archive-entry record))
       (copy-sequence record)))

(defun drover--index-resolve (index key record)
  "Return the key of the listed parent of RECORD, of KEY, in INDEX, or nil.
It is found as `drover-tree' finds it: by RECORD's backend, else
through RECORD's engine."
  (when-let* ((parent-id (drover-session-parent record)))
    (let* ((own (cons (drover-session-backend record) parent-id))
           (parent (if (gethash own (drover--index-records index))
                       own
                     (when-let* ((engine (drover-session-engine record)))
                       (car (gethash (cons engine parent-id) (drover--index-engines index)))))))
      (and parent (not (equal parent key)) parent))))

(defun drover--index-file (index key record)
  "File RECORD, of KEY, in INDEX's tables of engines and claims.
With RECORD nil, take KEY's record out of them instead."
  (let* ((filed (or record (gethash key (drover--index-records index))))
         (engine-key (drover-session-engine-key filed))
         (parent (drover-session-parent filed)))
    (cl-flet ((file (table table-key)
                (let ((keys (gethash table-key table)))
                  (setq keys (if record (cons key keys) (delete key keys)))
                  (if keys (puthash table-key keys table) (remhash table-key table))
                  keys)))
      (when engine-key
        (let ((before (cdr (gethash engine-key (drover--index-engines index))))
              (after (cdr (file (drover--index-engines index) engine-key))))
          (cond ((and after (not before)) (cl-incf (drover--index-shared index)))
                ((and before (not after)) (cl-decf (drover--index-shared index))))))
      (when parent
        (file (drover--index-claims index) (cons (drover-session-backend filed) parent))
        (when-let* ((engine (drover-session-engine filed)))
          (file (drover--index-engine-claims index) (cons engine parent)))))))

(defun drover--index-link (index key parent)
  "Make PARENT the parent of KEY in INDEX, nil for none.
Return the keys whose place changed: KEY and its parents, before and
after, or nil when its parent stays."
  (let ((before (gethash key (drover--index-parents index)))
        (children (drover--index-children index)))
    (unless (equal before parent)
      (when before
        (let ((siblings (delete key (gethash before children))))
          (if siblings (puthash before siblings children) (remhash before children))))
      (if parent
          (progn (puthash key parent (drover--index-parents index))
                 (push key (gethash parent children)))
        (remhash key (drover--index-parents index)))
      (delq nil (list key before parent)))))

(defun drover--index-cycle-p (index key)
  "Return non-nil when following the parents of KEY in INDEX comes back to it."
  (let ((parents (drover--index-parents index))
        (seen (list key))
        (current key)
        (cycle nil))
    (while (and (not cycle) (setq current (gethash current parents)))
      (if (member current seen)
          (setq cycle t)
        (push current seen)))
    cycle))

(defun drover--index-put (index key record)
  "Make RECORD, as listed, the record of KEY in INDEX; nil removes it.
Return the keys whose place among the trees changed."
  (let* ((records (drover--index-records index))
         (old (gethash key records))
         (moved nil))
    (when old
      (drover--index-file index key nil))
    (drover--note-activity key record)
    (if record
        (progn
          (puthash key record records)
          (unless (gethash key (drover--index-serials index))
            (puthash key (cl-incf (drover--index-serial index)) (drover--index-serials index)))
          (drover--index-file index key record))
      (remhash key records)
      (remhash key (drover--index-serials index)))
    ;; Sessions naming KEY their parent, directly or through its
    ;; engine, find it or lose it as it comes, goes or changes engine.
    (let ((claimants (and (or (not (eq (null old) (null record)))
                              (not (equal (and old (drover-session-engine-key old))
                                          (and record (drover-session-engine-key record)))))
                          (append (gethash key (drover--index-claims index))
                                  (and old (drover-session-engine-key old)
                                       (gethash (drover-session-engine-key old)
                                                (drover--index-engine-claims index)))
                                  (and record (drover-session-engine-key record)
                                       (gethash (drover-session-engine-key record)
                                                (drover--index-engine-claims index)))))))
      (dolist (claimant (delete-dups (cons key claimants)))
        (let* ((claimant-record (gethash claimant records))
               (parent (and claimant-record (drover--index-resolve index claimant claimant-record)))
               (changed (drover--index-link index claimant parent)))
          (when changed
            (setq moved (append changed moved))
            (when (and parent (drover--index-cycle-p index claimant))
              (setf (drover--index-cyclic index) t))))))
    (unless record
      ;; Its subagents found it through their claims, now gone.
      (remhash key (drover--index-children index)))
    ;; A conversation followed, or no longer, is listed by the backends
    ;; recording it, or no longer: `drover--index-settle' looks again.
    (unless (drover--recorded-p (car key))
      (let ((before (and old (drover-session-engine-key old)))
            (after (and record (drover-session-engine-key record))))
        (unless (equal before after)
          (dolist (engine-key (list before after))
            (when (and engine-key (drover--recording-backends (car engine-key)))
              (push engine-key (drover--index-pending index)))))))
    moved))

(defun drover--index-followed-p (index engine-key)
  "Return non-nil when a front end follows the conversation ENGINE-KEY in INDEX.
That is a backend not recording conversations (see :recorded in
`drover-register-backend')."
  (seq-some (lambda (key) (not (drover--recorded-p (car key))))
            (gethash engine-key (drover--index-engines index))))

(defun drover--index-admit (index record)
  "Return RECORD, as listed, unless a front end follows its conversation in INDEX.
Only the record of a backend listing conversations as their engine
records them gives way (see :recorded in `drover-register-backend')."
  (and record
       (not (and (drover--recorded-p (drover-session-backend record))
                 (drover--index-followed-p index (drover-session-engine-key record))))
       record))

(defun drover--index-settle (index)
  "Look again at the conversations whose followers changed in INDEX.
A backend recording one lists it once no other backend follows it, and
no longer while one does.  Return (CHANGED . MOVED), as
`drover--index-relist' does."
  (let ((changed nil)
        (moved nil))
    (while (drover--index-pending index)
      (let ((engine-key (pop (drover--index-pending index))))
        (dolist (backend (drover--recording-backends (car engine-key)))
          (let* ((key (cons backend (cdr engine-key)))
                 (properties (alist-get backend drover--backends))
                 (record (drover--index-admit
                          index (drover--index-listed
                                 (and (functionp (plist-get properties :session))
                                      (drover--backend-session properties (cdr engine-key)))))))
            (unless (equal-including-properties record (gethash key (drover--index-records index)))
              (push key changed)
              (setq moved (append (drover--index-put index key record) moved)))))))
    (cons changed moved)))

(defun drover--index-build ()
  "Return a new index of the sessions `drover-sessions' lists, before merging.
The times sessions sort by are noted anew, from those noted before,
which a session listed again keeps while its activity stays near them."
  (let ((index (drover--index-make))
        (times (make-hash-table :test #'equal))
        (listed nil))
    (dolist (record (drover--unarchived (drover--live-sessions)))
      (let ((key (drover-session-key record)))
        ;; The first of several records of one key wins.
        (unless (gethash key (drover--index-records index))
          (puthash key (copy-sequence record) (drover--index-records index))
          (puthash key (cl-incf (drover--index-serial index)) (drover--index-serials index))
          (drover--index-file index key (gethash key (drover--index-records index)))
          (drover--note-activity key record times)
          (push key listed))))
    (setq drover--activity-times times)
    (dolist (key listed)
      (drover--index-link index key (drover--index-resolve
                                     index key (gethash key (drover--index-records index)))))
    (setf (drover--index-cyclic index)
          (seq-some (lambda (key) (drover--index-cycle-p index key)) listed))
    index))

(defun drover--index-relist (index backend)
  "List the sessions of BACKEND anew into INDEX, as they differ from its records.
Return (CHANGED . MOVED): the keys whose records changed, came or went,
and those whose place among the trees changed.  The sessions of a
backend no longer registered go."
  (let* ((properties (alist-get backend drover--backends))
         (fresh (make-hash-table :test #'equal))
         (order nil)
         (changed nil)
         (moved nil))
    (dolist (record (and properties (drover--unarchived (drover--backend-sessions properties))))
      (let ((key (drover-session-key record)))
        (unless (or (gethash key fresh) (not (drover--index-admit index record)))
          (puthash key record fresh)
          (push key order))))
    (let ((gone nil))
      (maphash (lambda (key _)
                 (when (and (eq (car key) backend) (not (gethash key fresh)))
                   (push key gone)))
               (drover--index-records index))
      (dolist (key gone)
        (push key changed)
        (setq moved (append (drover--index-put index key nil) moved))))
    (dolist (key (nreverse order))
      (let ((record (gethash key fresh)))
        (unless (equal-including-properties record (gethash key (drover--index-records index)))
          (push key changed)
          (setq moved (append (drover--index-put index key (copy-sequence record)) moved)))))
    (cons changed moved)))

(defun drover--index-apply (keys backends)
  "Bring the index up to date with the sessions KEYS and BACKENDS reported.
Each key is looked up with its backend's :session, or its backend is
listed anew when it has none, as BACKENDS are.  Return the keys whose
records or places changed, or t when views must draw everything anew:
without an index, or while records of several backends make one
session, to merge, or while parents form a cycle."
  (if (not drover--index)
      t
    (let ((index drover--index)
          (relist (delete-dups (copy-sequence backends)))
          (changed nil)
          (moved nil))
      (dolist (key (delete-dups (copy-sequence keys)))
        (let* ((properties (alist-get (car key) drover--backends))
               (function (plist-get properties :session)))
          (if (and properties (not (functionp function)))
              (progn (cl-pushnew (car key) relist)
                     ;; Reported, it may have changed what its record
                     ;; does not tell, as its buffer.
                     (push key changed))
            (pcase (if properties
                       (condition-case err
                           (list (drover--index-admit
                                  index (drover--index-listed (funcall function (cdr key)))))
                         (error (message "Drover backend session: %S" err) 'failed))
                     (list nil))
              ('failed (cl-pushnew (car key) relist))
              (`(,record)
               (push key changed)
               (setq moved (append (drover--index-put index key record) moved)))))))
      (dolist (backend relist)
        (pcase-let ((`(,relisted . ,relinked) (drover--index-relist index backend)))
          (setq changed (append relisted changed)
                moved (append relinked moved))))
      (pcase-let ((`(,settled . ,relinked) (drover--index-settle index)))
        (setq changed (append settled changed)
              moved (append relinked moved)))
      (cond ((drover--index-cyclic index)
             ;; A cycle is rare: lists rebuild the index rather than follow it.
             (setq drover--index nil)
             t)
            ((> (drover--index-shared index) 0) t)
            (t (delete-dups (append (nreverse changed) moved)))))))

(defun drover-index ()
  "Return the index of the live sessions, made if needed.
Views use it to draw only what changed, as `drover-changed-sessions'
tells it while `drover-changed-hook' runs."
  (if drover--index
      (drover--apply-changed)
    (setq drover--index (drover--index-build)))
  drover--index)

(defun drover-index-incremental-p ()
  "Return non-nil when views may follow the index session by session.
They may not while records of several backends make one session, which
`drover-sessions' merges by the buffers the user selected last, nor
while parents form a cycle."
  (let ((index (drover-index)))
    (and (zerop (drover--index-shared index))
         (not (drover--index-cyclic index)))))

(defun drover-index-sessions ()
  "Return the sessions `drover-sessions' would list, from the index.
They come in the order the index first listed them, merged when
records of several backends make one session."
  (let* ((index (drover-index))
         (serials (drover--index-serials index))
         (sessions nil))
    (maphash (lambda (_ record) (push record sessions)) (drover--index-records index))
    (setq sessions (sort sessions :key (lambda (record) (gethash (drover-session-key record) serials))
                         :in-place t))
    (if (> (drover--index-shared index) 0)
        (drover--merge-duplicates sessions)
      sessions)))

(defun drover-index-session (key)
  "Return the record of the session whose key is KEY in the index, or nil."
  (gethash key (drover--index-records (drover-index))))

(defun drover-index-serial (key)
  "Return the place of the session KEY in the order of the index, a number, or nil.
Sessions first listed earlier have lower numbers, as
`drover-index-sessions' orders them."
  (gethash key (drover--index-serials (drover-index))))

(defun drover-index-parent (key)
  "Return the key of the parent of the session KEY in the index, or nil.
It is nil for a session at the top of its tree, as `drover-tree' makes
them: no parent, or one not listed."
  (gethash key (drover--index-parents (drover-index))))

(defun drover-index-children (key)
  "Return the keys of the subagents of the session KEY in the index, unordered."
  (gethash key (drover--index-children (drover-index))))

(defun drover-index-root (key)
  "Return the key of the session at the top of the tree of session KEY."
  (let ((parents (drover--index-parents (drover-index)))
        (parent nil))
    (while (setq parent (gethash key parents))
      (setq key parent))
    key))

(defun drover-index-tree-sessions (root)
  "Return the records of the tree of session ROOT, in the order of the index.
That is ROOT and every subagent below it, as `drover-tree' would nest
them from the sessions `drover-index-sessions' lists."
  (let* ((index (drover-index))
         (children (drover--index-children index))
         (records (drover--index-records index))
         (serials (drover--index-serials index))
         (keys nil)
         (stack (list root)))
    (while stack
      (let ((key (pop stack)))
        (push key keys)
        (setq stack (append (gethash key children) stack))))
    (mapcar (lambda (key) (gethash key records))
            (sort keys :key (lambda (key) (gethash key serials)) :in-place t))))

(defun drover-index-refresh ()
  "List every session anew, and tell views what changed since the index knew.
Backends report their changes, but some details change unreported, such
as a branch: views refresh the index from time to time, and on demand.
Run `drover-changed-hook' when something changed."
  (when drover--index
    (let ((changed nil)
          (moved nil))
      (dolist (backend (delete-dups (append (drover-backends)
                                            (let ((backends nil))
                                              (maphash (lambda (key _) (push (car key) backends))
                                                       (drover--index-records drover--index))
                                              backends))))
        (pcase-let ((`(,relisted . ,relinked) (drover--index-relist drover--index backend)))
          (setq changed (append relisted changed)
                moved (append relinked moved))))
      (pcase-let ((`(,settled . ,relinked) (drover--index-settle drover--index)))
        (setq changed (append settled changed)
              moved (append relinked moved)))
      (when (or changed moved (drover--index-cyclic drover--index))
        (let ((drover-changed-sessions
               (cond ((drover--index-cyclic drover--index) (setq drover--index nil) t)
                     ((> (drover--index-shared drover--index) 0) t)
                     (t (delete-dups (append changed moved))))))
          (run-hooks 'drover-changed-hook))))))

;;;; View settings

(defcustom drover-group-by nil
  "How session views group sessions.
The value is one of `drover-groupings': `status', the most urgent
first; `project', `branch', `agent', `model' or `provider', the
provider's name, of the session at the top of each tree; `updated',
the last activity, as today, yesterday, this week and older; or nil
for no groups, the default.  See `drover-group-trees'."
  :type '(choice (const :tag "By status" status)
                 (const :tag "By project" project)
                 (const :tag "By branch" branch)
                 (const :tag "By agent" agent)
                 (const :tag "By model" model)
                 (const :tag "By provider" provider)
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
 :provider  A provider name, as `drover-session-provider-name' returns
            it.
 :since     A float time: sessions last updated then or later.
 :search    A string found, ignoring case, in the title, summary,
            project or branch of the session.

A nil or missing member, or an empty search string, matches every
session.  Change it with `drover-set-filter' so that views follow.")

(defvar-local drover-view-p nil
  "Non-nil in the buffers of drover's own session views.
Those follow `drover-filter', `drover-group-by' and `drover-sort-by'.")

(defconst drover--filter-keys '(:project :statuses :agent :model :provider :since :search)
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
         (provider (drover--filter-value filter :provider))
         (since (drover--filter-value filter :since))
         (search (drover--filter-value filter :search)))
    (and (or (null project) (equal (drover-session-project session) project))
         (or (null statuses) (memq (drover-session-status session) statuses))
         (or (null agent) (equal (drover-session-agent session) agent))
         (or (null model) (equal (drover-session-model session) model))
         (or (null provider) (equal (drover-session-provider-name session) provider))
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

(defun drover--view-trees (sessions &optional trees)
  "Return the trees of SESSIONS a view draws, and those it could draw.
The value is (VISIBLE . KEPT): KEPT are the trees `drover-filter'
leaves, VISIBLE the same without the subagents to hide.  TREES are the
trees of SESSIONS, when made already by `drover-tree'."
  (let ((trees (or trees (drover-tree sessions))))
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
it shows whatever `drover-show-subagents' says, even `none', unless a
session above it matches too: the view then explains why each tree is
listed without listing every subagent of a matching session."
  (car (drover--view-trees (or sessions (drover-known-sessions)))))

(defun drover-view-subagent-counts (&optional sessions)
  "Return how many subagents of SESSIONS a view could show, and hides.
SESSIONS defaults to the sessions lists show.  The value is (COUNT
. HIDDEN): COUNT is the number of subagents under the sessions of the
trees `drover-filter' leaves, HIDDEN how many of them
`drover-view-trees' leaves out."
  (pcase-let ((`(,visible . ,kept) (drover--view-trees (or sessions (drover-known-sessions)))))
    (let ((all (length (drover-tree-sessions kept))))
      (cons (- all (length kept))
            (- all (length (drover-tree-sessions visible)))))))

(defun drover-view-hidden-count (&optional sessions)
  "Return how many subagents of SESSIONS a view hides.
SESSIONS defaults to the sessions lists show.  They are the subagents
of the trees `drover-filter' leaves that `drover-view-trees' leaves
out."
  (cdr (drover-view-subagent-counts sessions)))

(defun drover-view-groups (&optional sessions now)
  "Return the groups of trees a session view draws, in order.
The trees are those of `drover-view-trees' for SESSIONS, grouped as
`drover-group-by' says, at NOW for groups of last activity (see
`drover-group-trees'), and sorted within each group as `drover-sort-by'
and `drover-sort-descending' say (see `drover-sort-trees').  The value
is a list of (GROUP . TREES): GROUP is (GROUPING . VALUE), or nil when
sessions are not grouped.  It is nil when no tree is drawn."
  (drover-view-arrange (drover-view-trees sessions) now))

(defun drover-view-arrange (trees &optional now)
  "Return TREES grouped and sorted as views draw them, a list of (GROUP . TREES).
TREES are the trees of `drover-view-trees'; the value is that of
`drover-view-groups', at NOW for groups of last activity."
  (mapcar (pcase-lambda (`(,value . ,nodes))
            (cons (and drover-group-by (cons drover-group-by value))
                  (drover-sort-trees nodes)))
          (drover-group-trees trees drover-group-by now)))

(defun drover-view-settings-changed ()
  "Redraw every session view at once, after view settings changed.
Views redraw from `drover-changed-hook', which runs now; their mode
lines, which hint at the settings, are drawn again, and only theirs."
  (when (timerp drover--change-timer)
    (cancel-timer drover--change-timer))
  (setq drover--changed-all t)
  (drover--run-changed)
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'drover-view-p buffer)
      (with-current-buffer buffer
        (force-mode-line-update)))))

(defun drover-set-filter (key value)
  "Set member KEY of `drover-filter' to VALUE, nil removing it, and redraw views.
KEY is one of :project, :statuses, :agent, :model, :provider, :since
and :search."
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

(defun drover-set-show-subagents (mode)
  "Show the subagents MODE says in every view, and redraw them.
MODE is one of `drover-subagent-modes'; see `drover-show-subagents'."
  (unless (memq mode drover-subagent-modes)
    (error "Unknown subagent mode %S" mode))
  (setq-default drover-show-subagents mode)
  (drover-view-settings-changed))

(defun drover-next-subagent-mode (&optional mode)
  "Return the mode of `drover-subagent-modes' after MODE, around.
MODE defaults to the global value of `drover-show-subagents'."
  (let ((modes drover-subagent-modes))
    (or (cadr (memq (or mode (default-value 'drover-show-subagents)) modes))
        (car modes))))

;;;; Older and archived sessions

;; A backend fetches its older sessions (:history) and its archived ones
;; (:archived) on demand, while lists show them.  Each fetch of a kind
;; and backend has a generation: an answer to an earlier fetch, or
;; arriving once its kind is hidden, is dropped.

(defvar drover--fetch-generations nil
  "Generations of the fetches of sessions, as an alist ((KIND . BACKEND) . N).
KIND is `history' or `archived'.")

(defun drover--fetch-generation (kind backend)
  "Return the generation of the last fetch of KIND from BACKEND."
  (alist-get (cons kind backend) drover--fetch-generations 0 nil #'equal))

(defun drover--forget-fetches (kind)
  "Make every fetch of KIND in progress stale, so that its answer is dropped."
  (dolist (entry drover--fetch-generations)
    (when (eq (caar entry) kind)
      (cl-incf (cdr entry)))))

(defun drover--fetch (kind backends)
  "Fetch the sessions of KIND from BACKENDS, those of every backend when nil.
KIND is `history', fetched by :history into `drover--history' while
`drover-show-history' is set, or `archived', by :archived into
`drover--archived' while `drover-show-archived' is set.  Each backend's
answer replaces what it answered before and redraws the views, as long
as KIND stays shown and no later fetch of KIND from that backend
started meanwhile.  A backend failing to start its fetch is reported,
the others fetching all the same."
  (let ((operation (if (eq kind 'history) :history :archived)))
    (dolist (backend (or backends (drover-backends)))
      (when-let* ((function (drover-backend-property backend operation))
                  ((functionp function)))
        (let ((generation (cl-incf (alist-get (cons kind backend) drover--fetch-generations
                                              0 nil #'equal))))
          (with-demoted-errors "Drover backend fetch: %S"
            (funcall function
                     (lambda (sessions)
                       (when (and (if (eq kind 'history) drover-show-history drover-show-archived)
                                  (= generation (drover--fetch-generation kind backend)))
                         (if (eq kind 'history)
                             (setf (alist-get backend drover--history) (copy-sequence sessions))
                           (setf (alist-get backend drover--archived) (copy-sequence sessions)))
                         (drover-backend-changed backend))))))))))

(defun drover-history-available-p ()
  "Return non-nil when a registered backend fetches older sessions."
  (seq-some (lambda (backend) (functionp (drover-backend-property backend :history)))
            (drover-backends)))

(defun drover-history-fetch (&optional backend)
  "Fetch the older sessions of BACKEND, or of every backend that keeps them.
Each backend's answer replaces what it answered before and redraws the
views, as long as `drover-show-history' stays set and no later fetch
started meanwhile."
  (drover--fetch 'history (and backend (list backend))))

(defun drover-set-show-history (show)
  "Show older sessions in every view when SHOW is non-nil, else hide them.
Showing them fetches them (see `drover-history-fetch'); hiding them
forgets them.  Views redraw at once, and again as older sessions
arrive."
  (setq drover-show-history (and show t))
  (if drover-show-history
      (drover-history-fetch)
    (drover--forget-fetches 'history)
    (setq drover--history nil))
  (drover-view-settings-changed))

(defun drover-archived-available-p ()
  "Return non-nil when there may be archived sessions to show.
That is when a registered backend fetches its archived sessions, or
when drover archived a conversation of a registered backend itself."
  (or (seq-some (lambda (backend) (functionp (drover-backend-property backend :archived)))
                (drover-backends))
      (seq-some (lambda (entry) (assq (plist-get (cdr entry) :backend) drover--backends))
                (drover--archive))))

(defun drover-archived-fetch (&optional backend)
  "Fetch the archived sessions of BACKEND, or of every backend that has them.
Each backend's answer replaces what it answered before and redraws the
views, as long as `drover-show-archived' stays set and no later fetch
started meanwhile.  What drover archived itself needs no fetch."
  (drover--fetch 'archived (and backend (list backend))))

(defun drover-set-show-archived (show)
  "Show archived sessions in every view when SHOW is non-nil, else hide them.
Showing them fetches them (see `drover-archived-fetch'); hiding them
forgets those fetched.  Views redraw at once, and again as archived
sessions arrive."
  (setq drover-show-archived (and show t))
  (if drover-show-archived
      (drover-archived-fetch)
    (drover--forget-fetches 'archived)
    (setq drover--archived nil))
  (drover-view-settings-changed))

(defun drover--forget-fetched (session)
  "Forget the records of SESSION fetched as older or archived, once it is gone."
  (let ((key (drover-session-key session)))
    (cl-flet ((forget (fetched)
                (mapcar (lambda (entry)
                          (cons (car entry)
                                (seq-remove (lambda (record) (equal (drover-session-key record) key))
                                            (cdr entry))))
                        fetched)))
      (setq drover--history (forget drover--history)
            drover--archived (forget drover--archived)))))

;;;; State files

(defun drover-write-data (file description version key entries)
  "Write ENTRIES to FILE as Lisp data, atomically, or signal an error.
FILE holds (:version VERSION KEY (ENTRY...)), after a comment saying
it is DESCRIPTION, written by drover.  It is written to a temporary
file in the same directory, created if needed, then renamed over FILE,
so that a reader never sees it half written."
  (let ((directory (file-name-directory file))
        (temporary nil))
    (unwind-protect
        (progn
          (make-directory directory t)
          (setq temporary (make-temp-file (expand-file-name (concat "." (file-name-base file) "-")
                                                            directory)
                                          nil ".eld"))
          (with-temp-buffer
            (insert ";;; " description ", written by drover.  -*- mode: lisp-data; coding: utf-8-unix -*-\n")
            (let ((print-length nil)
                  (print-level nil)
                  (print-circle nil)
                  (print-escape-newlines t))
              (insert (format "(:version %d\n %s\n (" version key))
              (dolist (entry entries)
                (unless (eq entry (car entries))
                  (insert "\n  "))
                (prin1 entry (current-buffer)))
              (insert "))\n"))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region nil nil temporary nil 'silent)))
          (rename-file temporary file t)
          (setq temporary nil))
      (when temporary
        (ignore-errors (delete-file temporary))))))

(defun drover-read-data (file version key)
  "Return the entries FILE holds, as `drover-write-data' wrote them, or nil.
Nil when FILE does not exist.  Signal an error when it holds another
VERSION, or no list under KEY."
  (when (file-exists-p file)
    (let ((form (with-temp-buffer
                  (insert-file-contents file)
                  (read (current-buffer)))))
      (unless (and (plistp form) (eql (plist-get form :version) version))
        (error "Unknown format"))
      (let ((entries (plist-get form key)))
        (unless (proper-list-p entries)
          (error "Malformed entries"))
        entries))))

;;;; Drover's own archive

;; A backend without an archive of its own, such as claude-code-ide's
;; terminals or agent-shell's shells, has its sessions archived by
;; drover: it keeps the conversations it archived, by engine and engine
;; identifier, in a file of `drover-state-directory', read again
;; whenever another Emacs wrote it, with what lists show of each, so
;; that an archived session whose buffer is gone still shows among the
;; archived ones.  Archived, a conversation leaves `drover-sessions'
;; whichever backend follows it.

(defconst drover--archive-version 1
  "Version of the format of the file of the sessions drover archived.")

(defconst drover--archive-fields
  '(:backend :title :directory :project :branch :agent :model :effort :created :updated)
  "What drover keeps of a session it archives, besides when.")

(defvar drover--archive nil
  "Conversations drover archived, as an alist ((ENGINE . ENGINE-ID) . PLIST).
PLIST holds :time, when, and the fields of `drover--archive-fields'.")

(defvar drover--archive-stamp 'unread
  "Modification time of the archive's file when last read or written.
`unread' before it is read, nil when it does not exist.")

(defvar drover--archive-warned nil
  "Non-nil once a problem with the archive's file was reported.
It is reported once per Emacs session, as sessions are listed often.")

(defun drover-archive-file ()
  "Return the file of the sessions drover archived, in `drover-state-directory'."
  (expand-file-name "archive.eld" drover-state-directory))

(defun drover--archive-warn (format-string &rest args)
  "Report a problem with the archive's file, once per session.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless drover--archive-warned
    (setq drover--archive-warned t)
    (apply #'drover-warn format-string args)))

(defun drover--archive-entry-p (entry)
  "Return non-nil when ENTRY is an entry of `drover--archive'."
  (pcase entry
    (`((,(and engine (pred symbolp)) . ,(and id (pred stringp))) . ,plist)
     (and engine (not (string-empty-p id))
          (plistp plist)
          (numberp (plist-get plist :time))
          (symbolp (plist-get plist :backend))
          (seq-every-p (lambda (field)
                         (let ((value (plist-get plist field)))
                           (or (null value)
                               (if (memq field '(:created :updated)) (numberp value) (stringp value)))))
                       (remq :backend drover--archive-fields))))))

(defun drover--archive-read ()
  "Return the conversations the archive's file holds, nil when there is none.
A file of another version, or with a malformed entry, is reported once
and ignored."
  (let ((file (drover-archive-file)))
    (condition-case err
        (let ((entries (drover-read-data file drover--archive-version :sessions)))
          (unless (seq-every-p #'drover--archive-entry-p entries)
            (error "Malformed entries"))
          entries)
      (error
       (drover--archive-warn "Ignoring the sessions drover archived in %s: %s"
                             (abbreviate-file-name file) (error-message-string err))
       nil))))

(defun drover--archive ()
  "Return the conversations drover archived, reading the file when it changed."
  (let ((stamp (file-attribute-modification-time (file-attributes (drover-archive-file)))))
    (unless (equal stamp drover--archive-stamp)
      (setq drover--archive (drover--archive-read)
            drover--archive-stamp stamp))
    drover--archive))

(defun drover--archive-write (archive)
  "Make ARCHIVE the conversations drover archived, and write it to its file.
A failure is reported once; ARCHIVE stays for this Emacs session."
  (setq drover--archive archive)
  (let ((file (drover-archive-file)))
    (condition-case err
        (drover-write-data file "Sessions drover archived" drover--archive-version :sessions archive)
      (error (drover--archive-warn "Could not remember the sessions drover archived in %s: %s"
                                   (abbreviate-file-name file) (error-message-string err))))
    (setq drover--archive-stamp (file-attribute-modification-time (file-attributes file)))))

(defun drover--archive-entry (session)
  "Return what drover keeps of SESSION's conversation, archived, or nil."
  (when-let* ((key (drover-session-engine-key session)))
    (alist-get key (drover--archive) nil nil #'equal)))

(defun drover--unarchived (sessions)
  "Return SESSIONS but those archived: by their backend, or by drover.
A backend reports its archived sessions through :archived, not among
the sessions it follows; a record telling it is archived is left out
all the same."
  (let ((archive (drover--archive)))
    (seq-remove (lambda (session)
                  (or (drover-session-archived session)
                      (and archive
                           (when-let* ((key (drover-session-engine-key session)))
                             (assoc key archive)))))
                sessions)))

(defun drover--mark-archived (session)
  "Return SESSION, or a copy with its ARCHIVED set when drover archived it."
  (if-let* (((not (drover-session-archived session)))
            (entry (drover--archive-entry session)))
      (let ((copy (copy-sequence session)))
        (setf (drover-session-archived copy) (plist-get entry :time))
        copy)
    session))

(defun drover--archive-record (key entry)
  "Return the session drover archived as KEY, (ENGINE . ENGINE-ID), from ENTRY.
It stands for a conversation no backend follows any more, such as one
whose buffer is gone: idle, identified by its engine identifier."
  (drover-session-create :backend (plist-get entry :backend) :id (cdr key)
                         :engine (car key) :engine-id (cdr key)
                         :title (plist-get entry :title)
                         :directory (plist-get entry :directory)
                         :project (plist-get entry :project)
                         :branch (plist-get entry :branch)
                         :agent (plist-get entry :agent)
                         :model (plist-get entry :model)
                         :effort (plist-get entry :effort)
                         :created (plist-get entry :created)
                         :updated (plist-get entry :updated)
                         :status 'idle
                         :archived (plist-get entry :time)))

(defun drover-session-archived-p (session)
  "Return non-nil when SESSION is archived, by its backend or by drover."
  (or (and (drover-session-archived session) t)
      (and (drover--archive-entry session) t)))

(defun drover--native-archive-p (session)
  "Return non-nil when the backend of SESSION archives it itself."
  (drover-supports-p session :archive))

(defun drover-archivable-p (session)
  "Return non-nil when SESSION may be archived: by its backend, else by drover.
Drover archives a session of a backend without an archive of its own,
or whose archive does not take it (see `drover-supports-p'), once its
engine identifier is known, as it keeps it by conversation."
  (and (not (drover-session-archived-p session))
       (or (drover--native-archive-p session)
           (and (drover-session-engine-key session) t))))

(defun drover-unarchivable-p (session)
  "Return non-nil when SESSION is archived and may be restored."
  (or (and (drover--archive-entry session) t)
      (and (drover-session-archived session)
           (drover-supports-p session :unarchive))))

(defun drover--archive-changed (session)
  "Report that drover archived or restored the conversation of SESSION.
Its backend lists its sessions anew; the backends recording the
conversation (see :recorded in `drover-register-backend') look at it
alone."
  (drover-backend-changed (drover-session-backend session))
  (when-let* ((key (drover-session-engine-key session)))
    (dolist (backend (drover--recording-backends (car key)))
      (unless (eq backend (drover-session-backend session))
        (drover-backend-changed backend (cdr key))))))

(defun drover-archive-session (session)
  "Archive SESSION, through its backend, else in drover's own archive.
A backend without an archive of its own, or whose archive does not take
SESSION, has its sessions archived by drover, by conversation, so that
they leave every list until restored with `drover-unarchive-session',
which they show among while `drover-show-archived' is set.  Signal a
`user-error' when SESSION is archived already, or cannot be.

Archived, SESSION's buffer closes (see `drover--close-archived'): the
value tells how, for `drover--archive-note'."
  (let ((title (drover-session-display-title session)))
    (cond
     ((drover-session-archived-p session) (user-error "%s is archived already" title))
     ((drover--native-archive-p session)
      (drover-backend-call session :archive))
     ((not (drover-session-engine-key session))
      (user-error "%s cannot be archived before its conversation is known" title))
     (t
      (cl-flet ((text (value) (and (stringp value) (substring-no-properties value))))
        (let ((archive (copy-alist (drover--archive)))
              (entry (list :time (float-time)
                           :backend (drover-session-backend session)
                           :title (text (drover-session-title session))
                           :directory (text (drover-session-directory session))
                           :project (text (drover-session-project session))
                           :branch (text (drover-session-branch session))
                           :agent (text (drover-session-agent session))
                           :model (text (drover-session-model session))
                           :effort (text (drover-session-effort session))
                           :created (drover-session-created session)
                           :updated (drover-session-updated session))))
          (setf (alist-get (drover-session-engine-key session) archive nil nil #'equal) entry)
          (drover--archive-write archive)
          (drover--archive-changed session)))))
    (drover--close-archived session)))

(defun drover-unarchive-session (session)
  "Restore SESSION from the archive, drover's own or its backend's.
Signal a `user-error' when SESSION is not archived, and when drover
archived it and no backend lists its conversation any more, neither a
front end following it nor one listing what its engine records, as for
a Claude Code conversation whose transcript is gone: restored, it would
leave every list, so it stays archived, and listed among the archived
sessions.  A buffer archiving was to close once its turn ended stays open."
  (cond
   ((drover--archive-entry session)
    (unless (drover--followed-p session)
      (user-error "%s stays archived: no front end follows its conversation, and its engine keeps no record of it that drover lists"
                  (drover-session-display-title session)))
    (drover--archive-forget session)
    (drover--close-cancel session)
    (drover--archive-changed session))
   ((drover-session-archived session)
    (drover-backend-call session :unarchive)
    (drover--close-cancel session))
   (t (user-error "%s is not archived" (drover-session-display-title session)))))

(defun drover--followed-p (session)
  "Return non-nil when a backend lists the conversation of SESSION.
That is when a backend lists it, live or among the older sessions it
fetched or those a backend recording conversations lists at once,
whether drover's archive hides it or not: restored, it shows again."
  (when-let* ((key (drover-session-engine-key session)))
    (or (seq-some (lambda (record) (equal (drover-session-engine-key record) key))
                  (append (drover--live-sessions) (drover--fetched-sessions drover--history)))
        (and (drover--recording-backends (car key))
             (gethash key (drover--recorded-history))
             t))))

(defun drover--archive-forget (session)
  "Remove SESSION's conversation from drover's archive, if there."
  (when-let* ((key (drover-session-engine-key session))
              ((assoc key (drover--archive))))
    (drover--archive-write (seq-remove (lambda (entry) (equal (car entry) key))
                                       (drover--archive)))))

;;;; Closing the buffers of archived sessions

;; Archiving a session closes its buffer: the user is done with it.  A
;; buffer showing a session that runs elsewhere, as an OpenCode session
;; runs in the service, closes at once, ending nothing.  A buffer that
;; runs its agent, a terminal or a shell, would end a turn in progress:
;; it closes once the turn ended, which `drover-changed-hook' tells,
;; whether or not a list shows, unless the session is restored first.
;; A turn waiting for the user's answer cannot end before it: the buffer
;; stays, for the answer, rather than lose the turn's work.

(defvar drover--closing nil
  "Buffers of archived sessions to close once their turns end.
An alist (KEY . BUFFER): KEY is the key of the session archived (see
`drover-session-key'), BUFFER its buffer, which runs its agent.")

(defun drover-keep-unsent (texts)
  "Put TEXTS in the kill ring, as one entry, unless every one is blank.
TEXTS are strings, or nil: what a buffer about to close holds that its
agent never got, such as input typed at its prompt.  Return non-nil
when they went to the kill ring."
  (when-let* ((texts (seq-remove (lambda (text) (or (null text) (string-blank-p text))) texts)))
    (kill-new (mapconcat #'string-trim texts "\n\n"))
    t))

(defun drover--skip-shown-views (skip)
  "Return a value of `switch-to-prev-buffer-skip' skipping drover's views shown.
A view, a buffer whose `drover-view-p' is set, shown in another window
already, is skipped, as are the buffers SKIP, the value the user set,
skips."
  (let ((skip (cond ((or (functionp skip) (memq skip '(nil t visible 0))) skip)
                    (t 'frame))))
    (lambda (window buffer bury-or-kill)
      (or (and (buffer-local-value 'drover-view-p buffer)
               (seq-some (lambda (other) (not (eq other window)))
                         (get-buffer-window-list buffer nil t)))
          (switch-to-prev-buffer-skip-p (if (eq skip 'frame) (window-frame window) skip)
                                        window buffer bury-or-kill)))))

(defun drover-kill-buffer (buffer)
  "Kill BUFFER without asking.
The caller kept first what killing would lose, such as input not sent,
with `drover-keep-unsent': `kill-buffer-query-functions', which would
ask about a live process or work in progress, are not run.  Its windows
show what Emacs shows in place of a killed buffer, a dedicated window
going as usual, but while one of drover's views shows, none of them
shows that view a second time."
  (let ((switch-to-prev-buffer-skip
         (if (seq-some (lambda (window) (buffer-local-value 'drover-view-p (window-buffer window)))
                       (window-list-1 nil 'nomini t))
             (drover--skip-shown-views switch-to-prev-buffer-skip)
           switch-to-prev-buffer-skip)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Bound in BUFFER: buffers ask through local values too, as
        ;; agent-shell's do about prompts queued.
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun drover--close (session)
  "Close the buffer of SESSION through its backend's :close.
Return `kept' when what the buffer held unsent went to the kill ring,
`closed' otherwise, or (failed . MESSAGE) when closing failed."
  (condition-case err
      (if (funcall (drover-backend-property session :close) session) 'kept 'closed)
    (error (cons 'failed (error-message-string err)))))

(defun drover--close-archived (session)
  "Close the buffer of SESSION, just archived, now or once its turn ends.
Return how it went, for `drover--archive-note': nil without a buffer,
or for a backend closing none (without :close); `closed' or `kept', as
`drover--close' returns them, or (failed . MESSAGE); `later' when its
buffer runs its agent (see :runs-in-buffer) in the middle of a turn,
which closing would end, so that it closes once the turn ended, and
`waiting' when that turn waits for the user."
  (when-let* (((functionp (drover-backend-property session :close)))
              (buffer (drover-session-buffer session)))
    ;; The record as the backend tells it now: SESSION may be the one a
    ;; list drew a while ago.
    (if-let* (((drover-backend-property session :runs-in-buffer))
              (current (or (drover-find (drover-session-backend session) (drover-session-id session))
                           session))
              ((drover-session-active-p current)))
        (progn
          (setf (alist-get (drover-session-key session) drover--closing nil nil #'equal) buffer)
          (add-hook 'drover-changed-hook #'drover--close-pending)
          (if (eq (drover-session-status current) 'waiting) 'waiting 'later))
      (drover--close session))))

(defun drover--close-cancel (session)
  "Keep open the buffer of SESSION, restored before its turn ended."
  (setq drover--closing (seq-remove (lambda (entry) (equal (car entry) (drover-session-key session)))
                                    drover--closing))
  (unless drover--closing
    (remove-hook 'drover-changed-hook #'drover--close-pending)))

(defun drover--close-pending ()
  "Close the buffers of archived sessions whose turns ended, and say so.
For `drover-changed-hook', while buffers wait (see `drover--closing').
A buffer killed meanwhile, or showing another session, or whose session
was restored, is forgotten."
  (dolist (entry (copy-sequence drover--closing))
    (pcase-let* ((`(,key . ,buffer) entry)
                 (record (and (buffer-live-p buffer) (drover-find (car key) (cdr key))))
                 (still (and record
                             (eq (drover-session-buffer record) buffer)
                             (drover-session-archived-p record))))
      (unless (and still (drover-session-active-p record))
        (setq drover--closing (delq entry drover--closing))
        (when still
          (let ((title (drover-session-display-title record))
                (outcome (drover--close record)))
            (if (eq (car-safe outcome) 'failed)
                (drover-warn "Could not close the buffer of %s, archived: %s" title (cdr outcome))
              (message "%s" (concat (format "Closed %s, archived, now that its turn ended" title)
                                    (if (eq outcome 'kept)
                                        "; its unsent input is in the kill ring"
                                      "")))))))))
  (unless drover--closing
    (remove-hook 'drover-changed-hook #'drover--close-pending)))

(defun drover--archive-note (outcome title)
  "Return the note on closing the buffer of a session archived, or nil.
OUTCOME is what `drover-archive-session' returned; TITLE the session's
title, or nil for a session acted on alone, which the report names."
  (pcase outcome
    ('kept (if title
               (format "the unsent input of %s is in the kill ring" title)
             "its unsent input is in the kill ring"))
    ('later (if title
                (format "the buffer of %s closes once its turn ends" title)
              "its buffer closes once its turn ends"))
    ('waiting (format "%s waits for you: its buffer closes once its turn ends" (or title "it")))
    (`(failed . ,message)
     (format "%s stays open: %s" (if title (format "the buffer of %s" title) "its buffer") message))))

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

;; Read from the file system: views ask as they draw, and must never
;; wait for a Git process.

(defun drover--git-directory (directory)
  "Return the Git directory of the worktree of DIRECTORY, or nil outside Git."
  (when-let* ((root (locate-dominating-file directory ".git"))
              (dotgit (expand-file-name ".git" root)))
    (if (file-directory-p dotgit)
        dotgit
      ;; A linked worktree's .git file names its Git directory.
      (with-temp-buffer
        (insert-file-contents dotgit)
        (when (re-search-forward "^gitdir: *\\(.+?\\) *$" nil t)
          (expand-file-name (match-string 1) root))))))

(defun drover--read-git-branch (directory)
  "Return the branch checked out in the worktree of DIRECTORY, or a short commit.
Return nil outside Git.  HEAD is read directly, without running Git."
  (when-let* ((gitdir (drover--git-directory directory))
              (head (expand-file-name "HEAD" gitdir))
              ((file-readable-p head)))
    (with-temp-buffer
      (insert-file-contents head)
      (cond ((looking-at "ref: refs/heads/\\(.+?\\) *$") (match-string 1))
            ((looking-at "[0-9a-f]\\{7\\}") (match-string 0))))))

(defun drover--read-git-default-branch (directory)
  "Return the default branch of the repository of DIRECTORY, or nil when unknown.
It is the branch the HEAD of the remote origin names, else that of
another remote, as cloning or `git remote set-head' records it in the
common Git directory, which linked worktrees share."
  (when-let* ((gitdir (drover--git-directory directory))
              (common (let ((file (expand-file-name "commondir" gitdir)))
                        (if (file-readable-p file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (expand-file-name (string-trim (buffer-string)) gitdir))
                          gitdir)))
              (remotes (expand-file-name "refs/remotes" common))
              ((file-directory-p remotes)))
    (seq-some (lambda (remote)
                (let ((head (expand-file-name "HEAD" (expand-file-name remote remotes))))
                  (when (file-readable-p head)
                    (with-temp-buffer
                      (insert-file-contents head)
                      (and (looking-at "ref: refs/remotes/[^/\n]+/\\(.+?\\) *$")
                           (match-string 1))))))
              (cons "origin" (remove "origin" (directory-files remotes nil "\\`[^.]"))))))

(defun drover--git-cached (table directory reader)
  "Return what READER reads of DIRECTORY, kept in TABLE for a while.
It is read again once `drover-branch-ttl' seconds passed.
Nil for a nil or missing directory."
  (when directory
    (let* ((directory (file-name-as-directory (expand-file-name directory)))
           (entry (gethash directory table))
           (now (float-time)))
      (if (and entry (< (- now (cdr entry)) drover-branch-ttl))
          (car entry)
        (let ((value (and (file-directory-p directory)
                          (ignore-errors (funcall reader directory)))))
          (puthash directory (cons value now) table)
          value)))))

(defun drover-git-branch (directory)
  "Return the Git branch of DIRECTORY, or a short commit when detached, or nil.
It is read from HEAD without running Git, so backends may call it for
every session they list: a directory's branch is kept for
`drover-branch-ttl' seconds.  Nil outside Git and for a missing
directory."
  (drover--git-cached drover--branches directory #'drover--read-git-branch))

(defvar drover--default-branches (make-hash-table :test #'equal)
  "Directories mapped to (BRANCH . TIME), as `drover-git-default-branch' read them.")

(defun drover-git-default-branch (directory)
  "Return the default branch of the repository of DIRECTORY, or nil when unknown.
It is the branch the HEAD of a remote names, origin's first, read
without running Git and kept `drover-branch-ttl' seconds, so that views
may ask for it as they draw.  Nil outside Git, for a missing directory,
and for a repository whose remotes record no HEAD."
  (drover--git-cached drover--default-branches directory #'drover--read-git-default-branch))

(defcustom drover-default-branches '("main" "master")
  "Branches taken for the default one of a repository whose own is unknown.
See `drover-session-default-branch-p'."
  :type '(repeat string)
  :group 'drover)

(defun drover-session-default-branch-p (session)
  "Return non-nil when SESSION works on the default branch of its repository.
The default branch is the one a remote of the repository names (see
`drover-git-default-branch'), else one of `drover-default-branches'.
Nil for a session without a branch."
  (when-let* ((branch (drover-session-branch session)))
    (if-let* ((default (drover-git-default-branch (drover-session-directory session))))
        (equal branch default)
      (and (member branch drover-default-branches) t))))

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

(defvar-local drover-selected-sessions-function nil
  "Function of no argument returning the sessions the user selected, or nil.
A list of sessions sets it to return those its active region selects,
as drover's list does; nil stands for no selection, and the commands
acting on selected sessions then act on the session at point.")

(defun drover-selected-sessions (&optional noerror)
  "Return the sessions selected in the current buffer, else the session at point.
The selection is what `drover-selected-sessions-function' returns, such
as the sessions an active region selects in drover's list.  Without
one, the value lists the session at point alone; without that either,
signal a `user-error', or return nil when NOERROR is non-nil."
  (or (and drover-selected-sessions-function
           (funcall drover-selected-sessions-function))
      (when-let* ((session (drover-session-at-point noerror)))
        (list session))))

(defun drover--running-p (session)
  "Return non-nil when SESSION runs a turn, which can be interrupted."
  (drover-session-active-p session))

(defun drover--ended-unseen-p (session)
  "Return non-nil when SESSION ended a turn the user did not see."
  (memq (drover-session-status session) '(done failed)))

(defun drover--selected-for (operation &optional applies doing)
  "Return the sessions a command performing OPERATION acts on.
They are the selected sessions (see `drover-selected-sessions'), as a
session alone or a list of several.  Of several, only those whose
backend performs OPERATION, a keyword (see `drover-supports-p'), unless
OPERATION is nil, and for which APPLIES, a function of a session,
returns non-nil, are kept: a region sweeping over sessions means those
it applies to.  A session alone is kept whatever: its backend tells
what it cannot do.  Signal a `user-error' when none of several is kept,
DOING telling what none does, such as \"runs a turn\"."
  (let ((sessions (drover-selected-sessions)))
    (if (null (cdr sessions))
        (car sessions)
      (or (seq-filter (lambda (session)
                        (and (or (null operation) (drover-supports-p session operation))
                             (or (null applies) (funcall applies session))))
                      sessions)
          (user-error "None of the %d selected sessions %s" (length sessions) doing)))))

(defun drover--session-names (sessions)
  "Return the titles of SESSIONS, each on one line and cut short, joined by commas."
  (mapconcat (lambda (session)
               (truncate-string-to-width
                (replace-regexp-in-string "[\n\r\t]+" " "
                                          (format "%s" (drover-session-display-title session)))
                40 nil nil (truncate-string-ellipsis)))
             sessions ", "))

(defun drover--confirm (verb sessions &optional suffix strongly)
  "Ask whether to VERB SESSIONS, signalling a `user-error' unless the user agrees.
SESSIONS is a session or a list of them.  VERB is capitalized, such
as \"Archive\", and SUFFIX, such as \" for good\", follows what it acts
on: a session by its title, several by their count, then their
titles, as \"Archive 3 sessions: A, B, C? \".  With STRONGLY, the
answer is typed in full."
  (let ((sessions (if (drover-session-p sessions) (list sessions) sessions)))
    (unless (funcall (if strongly #'yes-or-no-p #'y-or-n-p)
                     (if (cdr sessions)
                         (format "%s %d sessions%s: %s? " verb (length sessions) (or suffix "")
                                 (drover--session-names sessions))
                       (format "%s %s%s? " verb (drover-session-display-title (car sessions))
                               (or suffix ""))))
      (user-error "Abort"))))

(defun drover--act (sessions operation done &optional note)
  "Perform OPERATION on SESSIONS, a session or a list, and say so.
OPERATION is a keyword, the backend operation, or a function of a
session.  DONE is a format string for what was done, given the title of
a session or a count of sessions, such as \"Archived %s\".  A session
alone failing signals, as `drover-backend-call' does; of a list, each
session is acted on whatever happens to the others, and those failing
are named after how many were acted on.  NOTE, if any, is a function of
what OPERATION returned for a session and of its title, nil for a
session acted on alone, returning a note on the outcome, or nil: the
notes follow what was done.  Acting on a list in a buffer selecting
sessions ends its selection."
  (cl-flet ((perform (session)
              (if (keywordp operation)
                  (drover-backend-call session operation)
                (funcall operation session)))
            (notes (texts)
              (mapconcat (lambda (text) (concat "; " text)) (delq nil texts) "")))
    (if (drover-session-p sessions)
        (let ((value (perform sessions)))
          (message "%s" (concat (format done (drover-session-display-title sessions))
                                (notes (list (and note (funcall note value nil)))))))
      (let ((failures nil)
            (noted nil))
        (dolist (session sessions)
          (condition-case err
              (let ((value (perform session)))
                (when note
                  (push (funcall note value (drover-session-display-title session)) noted)))
            (error (push (format "%s (%s)" (drover--session-names (list session))
                                 (error-message-string err))
                         failures))))
        ;; Done with the selection, as Magit's commands are: kept, it
        ;; would select other sessions once these went.
        (when drover-selected-sessions-function
          (deactivate-mark))
        (let ((count (- (length sessions) (length failures))))
          (message "%s" (concat
                         (format done (if (and (= count 1) (null failures))
                                          (drover-session-display-title (car sessions))
                                        (format "%d session%s" count (if (= count 1) "" "s"))))
                         (notes (nreverse noted))
                         (if failures
                             (format "; failed for %s" (string-join (nreverse failures) ", "))
                           ""))))))))

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

(defun drover--session-window-p (window)
  "Return non-nil when WINDOW may be given a session to show.
Side windows, windows dedicated to their buffer and windows showing
one of drover's views may not: a session must never replace a list."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-dedicated-p window))
       (not (buffer-local-value 'drover-view-p (window-buffer window)))))

(defun drover-other-window (window &optional buffer)
  "Return a window other than WINDOW in which to show a session.
It is a window of WINDOW's frame showing BUFFER, the session's buffer,
if one does, else the one of them used most recently, else a new
window split from WINDOW.  Side windows, windows dedicated to their
buffer and those showing one of drover's views are never chosen.
Signal a `user-error' when WINDOW cannot be split either."
  (let ((candidates (seq-filter (lambda (other)
                                  (and (not (eq other window)) (drover--session-window-p other)))
                                (window-list (window-frame window) 'nomini))))
    (or (and buffer (seq-find (lambda (other) (eq (window-buffer other) buffer)) candidates))
        (car (sort candidates (lambda (a b) (> (window-use-time a) (window-use-time b)))))
        (split-window-sensibly window)
        (ignore-errors (split-window window))
        (user-error "No window to show the session in"))))

(defun drover-open (session &optional other-window)
  "Display SESSION and select its window; with OTHER-WINDOW, in another one.
SESSION shows where the current buffer sends sessions: in the window
its `drover-main-window-function' returns, such as the window where the
user works beside a list in a side window, else in the selected window.
With OTHER-WINDOW, it shows in another window than that one, chosen by
`drover-other-window', never in a list.  The window is chosen here and
selected before the backend's :open runs, given no OTHER-WINDOW, so
that sessions of every backend show in the same windows."
  (if (not (drover-supports-p session :open))
      (drover-backend-call session :open nil)
    (let* ((main (and drover-main-window-function (funcall drover-main-window-function)))
           (window (if (window-live-p main) main (selected-window))))
      (when other-window
        (setq window (drover-other-window window (drover-session-buffer session))))
      (unless (eq window (selected-window))
        (select-window window))
      (drover-backend-call session :open nil))))

(defun drover-respond (session)
  "Answer the oldest request of SESSION.
Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (drover-backend-call session :respond))

(defun drover-interrupt (sessions)
  "Stop the running turns of SESSIONS, a session or a list of sessions.
Interactively, SESSIONS are the selected sessions running a turn, or
the session at point (see `drover--selected-for'), after confirmation."
  (interactive (let ((sessions (drover--selected-for :interrupt #'drover--running-p "runs a turn")))
                 (drover--confirm "Interrupt" sessions)
                 (list sessions)))
  (drover--act sessions :interrupt "Interrupted %s"))

(defun drover-archive (sessions)
  "Archive SESSIONS, a session or a list of sessions, and close their buffers.
Each is archived by its backend, else by drover, then its buffer closes,
without asking, at once or, for a buffer running its agent, once its
turn ends (see `drover-archive-session').  Interactively, SESSIONS are
the selected sessions that may be archived, or the session at point, as
`drover--selected-for' tells, without confirmation: the conversations
stay, and what a buffer holds unsent goes to the kill ring."
  (interactive (list (drover--selected-for nil #'drover-archivable-p "can be archived")))
  (drover--act sessions #'drover-archive-session "Archived %s" #'drover--archive-note))

(defun drover-unarchive (sessions)
  "Restore SESSIONS from the archive, a session or a list of sessions.
Each comes back from its backend's archive or drover's (see
`drover-unarchive-session').  Interactively, SESSIONS are the selected
sessions that are archived, or the session at point (see
`drover--selected-for'); archived sessions show while
`drover-show-archived' is set."
  (interactive (list (drover--selected-for nil #'drover-unarchivable-p "is archived")))
  (drover--act sessions #'drover-unarchive-session "Unarchived %s"))

(defun drover-mark-seen (sessions)
  "Mark the ended turns of SESSIONS seen, a session or a list of sessions.
Interactively, SESSIONS are the selected sessions whose turn ended
unseen, or the session at point (see `drover--selected-for')."
  (interactive (list (drover--selected-for :mark-seen #'drover--ended-unseen-p
                                           "ended a turn unseen")))
  (drover--act sessions :mark-seen "Marked %s seen"))

(defun drover-preview (session callback)
  "Call CALLBACK with a string showing where SESSION stands, or nil."
  (drover-backend-call session :preview callback))

(defun drover-recap (session callback)
  "Fetch a recap of the last turn of SESSION, and call CALLBACK with it.
The recap is a plist; each member is nil when unknown or empty:

 :prompt   The user's last prompt, as text.
 :reply    The agent's newest reply to it, as text: what it said last.
 :changes  The files the turn changed, a list of (FILE ADDITIONS
           DELETIONS): FILE a name, absolute or relative to the
           session's directory, ADDITIONS and DELETIONS counts of lines,
           or nil when the backend does not know them.
 :error    The error that ended the turn, in full.

When nothing could be fetched, the recap is (:failure MESSAGE).  The
backend's :recap fetches it in the background; CALLBACK is called once,
possibly before this returns.  A backend without :recap, or failing
from the start, calls CALLBACK with a failure at once."
  (let ((called nil))
    (cl-flet ((deliver (recap)
                (unless called
                  (setq called t)
                  (funcall callback recap))))
      (if-let* ((function (drover-backend-property session :recap))
                ((functionp function)))
          (condition-case err
              (funcall function session #'deliver)
            (error (deliver (list :failure (error-message-string err)))))
        (deliver (list :failure (format "%s sessions tell no recap"
                                        (drover-backend-label session))))))))

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

(defun drover-delete-session (session)
  "Delete SESSION for good through its backend, without asking.
Drover then forgets it: the records of it fetched as older or archived,
and its conversation in drover's own archive."
  (drover-backend-call session :delete)
  (drover--forget-fetched session)
  (drover--archive-forget session)
  (drover--close-cancel session)
  (drover--archive-changed session))

(defun drover-delete (sessions)
  "Delete SESSIONS for good, a session or a list of sessions.
Interactively, SESSIONS are the selected sessions, or the session at
point (see `drover--selected-for'), after a confirmation naming them,
typed in full."
  (interactive (let ((sessions (drover--selected-for :delete nil "can be deleted")))
                 (drover--confirm "Delete" sessions " for good" t)
                 (list sessions)))
  (drover--act sessions #'drover-delete-session "Deleted %s"))

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

;;;; Resuming conversations

(declare-function drover-engine-label "drover-model" (engine))

;; A backend listing conversations as their engine records them shows
;; none itself: opening one resumes it in a front end able to, which
;; registers how (`drover-register-resumer').

(defvar drover--resumers nil
  "Front ends resuming conversations, an alist ((ENGINE . FRONT-END) . FUNCTION).
In the order they registered.")

(defcustom drover-resume-front-ends nil
  "The front ends conversations of each engine resume in, by preference.
An alist (ENGINE . FRONT-ENDS): ENGINE a symbol such as `claude-code',
FRONT-ENDS a list of front ends such as `claude-code-ide' and
`agent-shell', the first one loaded resuming.  Without an entry, a
conversation resumes in the front end it ran in, when its engine tells
it and that front end is loaded, else in the one loaded first."
  :type '(alist :key-type (symbol :tag "Engine")
                :value-type (repeat (symbol :tag "Front end")))
  :group 'drover)

(defun drover-register-resumer (engine front-end function)
  "Let FRONT-END resume the conversations of ENGINE with FUNCTION.
FUNCTION takes the engine's identifier of a conversation and the
directory it ran in, starts a session of FRONT-END continuing it,
displays it in the selected window and returns its buffer.  Registering
again replaces FUNCTION and keeps its place."
  (setf (alist-get (cons engine front-end) drover--resumers nil nil #'equal) function))

(defun drover-unregister-resumer (engine front-end)
  "Forget how FRONT-END resumes the conversations of ENGINE."
  (setq drover--resumers (seq-remove (lambda (entry) (equal (car entry) (cons engine front-end)))
                                     drover--resumers)))

(defun drover-resumer (engine &optional front-end)
  "Return (FRONT-END . FUNCTION) resuming conversations of ENGINE, or nil.
It is the first registered of the front ends `drover-resume-front-ends'
names for ENGINE, then FRONT-END, the one the conversation ran in, if
known, then those registered, in the order they registered."
  (let ((registered (seq-filter (lambda (entry) (eq (caar entry) engine))
                                (reverse drover--resumers))))
    (seq-some (lambda (candidate)
                (when-let* ((entry (assoc (cons engine candidate) registered)))
                  (cons candidate (cdr entry))))
              (append (alist-get engine drover-resume-front-ends)
                      (and front-end (list front-end))
                      (mapcar #'cdar registered)))))

(defun drover-resume (engine id directory &optional front-end)
  "Resume conversation ID of ENGINE, run in DIRECTORY, and return its buffer.
FRONT-END is the front end it ran in, when known; the one resuming it
is chosen by `drover-resumer'.  Signal a `user-error' when no front end
loaded can resume it."
  (pcase-let ((`(,_ . ,function) (or (drover-resumer engine front-end)
                                     (user-error "No front end loaded resumes %s conversations"
                                                 (or (and (fboundp 'drover-engine-label)
                                                          (drover-engine-label engine))
                                                     engine)))))
    (funcall function id directory)))

(provide 'drover-core)
;;; drover-core.el ends here
