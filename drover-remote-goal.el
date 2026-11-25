;;; drover-remote-goal.el --- Goals of the sessions of other hosts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; A goal runs on the host owning its session (see `drover-goal'): its
;; checks run in that host's worktree, its follow-ups go through that
;; host's queue, its limits and pauses are that host's.  This file has
;; another host set one there and follow it, as this host's own.
;;
;; The owner answers four requests of its peers telling the capability
;; `goals': `goals', its goals, followed as they change with :follow, in
;; messages `goals'; `goal-checks', the checks and setup a goal in a
;; directory of its would run, read there, and whether that directory's
;; worktree is fresh; `goal-set', a goal set on one of its sessions; and
;; `goal-act', pausing, resuming, stopping a goal, marking it seen or
;; reading its log.  Setting a goal runs the commands a peer names on
;; the owner, outside any harness: it takes `full' access.  The goal's
;; task, when it serves one, is the peer's, whose heading gets the
;; LOGBOOK note as the goal ends.
;;
;; The viewer follows the goals of each peer telling `goals' while
;; `drover-goal-mode' is on, and gives them to `drover-goal' as goals of
;; their sessions, which lists show and its command acts on.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-remote)
(require 'drover-goal)

(declare-function spyglass-connection-clock-offset "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-live-p "ext:spyglass-conn" (connection))

(defconst drover-remote-goal-limit 200
  "Goals at most a host tells its peers, the newest.")

(defconst drover-remote-goal-log-limit 65536
  "Characters at most of the end of a goal's log a host tells a peer.")

(defconst drover-remote-goal-commands-limit 20
  "Commands at most of a goal's checks or setup a peer names.")

;;;; On the owner

(defvar drover-remote-goal--followers (make-hash-table :test #'eq :weakness 'key)
  "Connections whose peers follow this host's goals.")

(defvar drover-remote-goal--timer nil
  "The timer telling the followers that goals changed, or nil.")

(defun drover-remote-goal--strings (value limit length)
  "Return VALUE, strings received, as LIMIT strings of LENGTH at most.
Return `invalid' when VALUE is not a vector of strings, and nil for
none."
  (cond
   ((or (null value) (eq value :false)) nil)
   ((not (and (vectorp value) (<= (length value) limit)
              (seq-every-p (lambda (item) (and (stringp item) (not (string-blank-p item))
                                               (<= (length item) length)
                                               (not (string-search "\n" item))))
                           value)))
    'invalid)
   (t (mapcar #'substring-no-properties value))))

(defun drover-remote-goal--sessions-by-key ()
  "Return this host's sessions by the engine key of their conversation."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (session (append (drover-known-sessions) (drover-archived-live-sessions)))
      (when-let* (((null (drover-session-host session)))
                  (key (drover-session-engine-key session)))
        (unless (gethash key table)
          (puthash key session table))))
    table))

(defun drover-remote-goal--wire (goal &optional sessions)
  "Return GOAL, one of this host's, as it travels to peers.
SESSIONS, as `drover-remote-goal--sessions-by-key' returns them, finds
its session; without, it is looked for."
  (let* ((key (drover-goal--key goal))
         (session (if sessions (and key (gethash key sessions)) (drover-goal--session goal)))
         (id (and session (null (drover-session-host session)) (drover-remote--wire-id (drover-session-id session)))))
    (list :id (plist-get goal :id)
          :k (if id (vector (symbol-name (drover-session-backend session)) id) :false)
          :ln (or (drover-remote--plain (plist-get goal :link)) :false)
          :ti (or (drover-remote--plain (drover-bound-line (plist-get goal :title) 160)) "")
          :kd (symbol-name (plist-get goal :kind))
          :st (symbol-name (plist-get goal :state))
          :rs (or (drover-remote--plain (drover-bound-line (plist-get goal :reason) 400)) :false)
          :it (plist-get goal :iteration)
          :its (plist-get goal :iterations)
          :cm (vconcat (mapcar (lambda (command) (drover-remote--plain (drover-bound-line command 500)))
                               (seq-take (plist-get goal :commands) drover-remote-goal-commands-limit)))
          :ob (or (drover-remote--plain (drover-bound-line (plist-get goal :objective) 3000)) :false)
          :fl (vconcat (mapcar (lambda (file) (drover-remote--plain (drover-bound-line file 300)))
                               (seq-take (plist-get goal :flagged) 50)))
          :dir (or (drover-remote--plain (plist-get goal :directory)) "")
          :s (plist-get goal :started)
          :e (or (plist-get goal :ended) :false)
          :sm (drover-goal--summary goal)
          :sn (if (plist-get goal :seen) t :false)
          :by (or (drover-remote--plain (plist-get goal :by)) :false)
          :pt (or (drover-remote--plain (plist-get goal :peer-task)) :false))))

(defun drover-remote-goal--wire-goals ()
  "Return this host's goals as they travel, the newest first."
  (let ((sessions (drover-remote-goal--sessions-by-key)))
    (vconcat (mapcar (lambda (goal) (drover-remote-goal--wire goal sessions))
                     (seq-take (drover-goal--goals) drover-remote-goal-limit)))))

(defun drover-remote-goal--on-goals (connection message reply)
  "Tell CONNECTION's peer, through REPLY, this host's goals.
With MESSAGE's :follow, the peer is told them again as they change, in
`goals' messages."
  (condition-case err
      (progn
        (drover-goal-load)
        (when (eq (plist-get message :follow) t)
          (puthash connection t drover-remote-goal--followers)
          (add-hook 'drover-goal-changed-hook #'drover-remote-goal--changed))
        (funcall reply (list :goals (drover-remote-goal--wire-goals))))
    (error (funcall reply nil (drover-error-message err)))))

(defun drover-remote-goal--changed ()
  "Tell the peers following the goals that they changed, half a second later.
For `drover-goal-changed-hook'."
  (drover-with-reporting "remote goals"
    (unless drover-remote-goal--timer
      (setq drover-remote-goal--timer
            (run-at-time 0.5 nil
                         (lambda ()
                           (drover-with-reporting "remote goals"
                             (setq drover-remote-goal--timer nil)
                             (when (> (hash-table-count drover-remote-goal--followers) 0)
                               (let ((goals (drover-remote-goal--wire-goals)))
                                 (dolist (connection (hash-table-keys drover-remote-goal--followers))
                                   (if (spyglass-connection-live-p connection)
                                       (drover-remote--send connection (list :t "goals" :goals goals))
                                     (remhash connection drover-remote-goal--followers))))))))))))

(defun drover-remote-goal--encode-commands (items)
  "Return ITEMS, plists of :command and :source, as they travel."
  (vconcat (mapcar (lambda (item)
                     (vector (drover-remote--plain (drover-bound-line (plist-get item :command) 2000))
                             (or (drover-remote--plain (drover-bound-line (plist-get item :source) 300)) "")))
                   (seq-take items drover-remote-goal-commands-limit))))

(defun drover-remote-goal--checks-directory (message)
  "Return the directory MESSAGE asks a goal's checks for, or why not.
Its :key, a session of this host, runs there; its :repo names a
repository of `drover-task-repositories', whose checkout it is; its
:directory is an absolute directory of this host.  Return (DIRECTORY
. SESSION), SESSION nil but for a key, or a line."
  (let* ((key (drover-remote--key (plist-get message :key)))
         (record (and key (car (drover-remote--served key))))
         (repo (drover-remote--line (plist-get message :repo) 200))
         (entry (and repo (require 'drover-task nil t) (drover-task-repository repo)))
         (given (drover-remote--line (plist-get message :directory) 1024))
         (directory (cond (key (and record (drover-session-local-directory record)))
                          (repo (and entry (drover-task-checkout entry)))
                          ((and given (file-name-absolute-p given)) given))))
    (cond
     ((and key (not record)) "no such session here")
     ((and repo (not entry)) (format "no repository %s here" repo))
     ((not directory) "no directory named")
     ((not (file-directory-p directory)) (format "no directory %s here" directory))
     (t (cons (file-name-as-directory directory) record)))))

(defun drover-remote-goal--on-checks (peer message reply)
  "Tell PEER, through REPLY, the checks of a goal where MESSAGE says.
See `drover-remote-goal--checks-directory'.  The answer holds the
directory, the checks and the setup needed there, as plists of the
command and where it comes from, read as this host reads them for its
own goals, a note on them, whether the worktree has no change, and
whether the session takes a criterion goal.  Reading them may ask this
host's cheap model: it takes `act' access, as setting a goal begins."
  (pcase (or (drover-remote-goal--allowed peer "read a goal's checks")
             (drover-remote-goal--checks-directory message))
    ((and (pred stringp) why) (funcall reply nil why))
    (`(,directory . ,record)
     (drover-repo-facts
      directory
      (lambda (facts)
        (drover-with-reporting "remote goal checks"
          (drover-task-git
           directory '("status" "--porcelain=v1" "--untracked-files=normal")
           (lambda (result)
             (drover-with-reporting "remote goal checks"
               (funcall reply
                        (list :directory directory
                              :checks (drover-remote-goal--encode-commands (plist-get facts :checks))
                              :setup (drover-remote-goal--encode-commands
                                      (drover-repo-setup-needed (plist-get facts :setup) directory))
                              :note (or (drover-remote--plain (drover-bound-line (plist-get facts :note) 300)) :false)
                              :fresh (if (and (plist-get result :ok) (string-blank-p (or (plist-get result :output) "")))
                                         t
                                       :false)
                              :criterion (if (and record (drover-supports-p record :goal-command)) t :false))))))))))))

(defun drover-remote-goal-properties (data peer-name)
  "Return the properties of a goal DATA, as a peer sends it, or why not, a line.
A plist of `drover-goal-start': :kind, :commands, :sources, :setup,
:baseline, :prompt, :objective, :title, its limits, and :by, PEER-NAME
and who asked there, and :peer-task, the task the peer's heading names."
  (let* ((kind (pcase (plist-get data :kind) ((or 'nil "check") 'check) ("criterion" 'criterion)))
         (commands (drover-remote-goal--strings (plist-get data :commands) drover-remote-goal-commands-limit 2000))
         (sources (drover-remote-goal--strings (plist-get data :sources) drover-remote-goal-commands-limit 300))
         (setup (drover-remote-goal--strings (plist-get data :setup) drover-remote-goal-commands-limit 2000))
         (prompt (plist-get data :prompt))
         (objective (plist-get data :objective))
         (limit (lambda (key low high)
                  (let ((value (plist-get data key)))
                    (cond ((null value) nil)
                          ((and (numberp value) (<= low value high)) value)
                          (t 'invalid)))))
         (iterations (funcall limit :iterations 1 1000))
         (minutes (funcall limit :minutes 1 10080))
         (rounds (funcall limit :rounds 1 1000))
         (budget (plist-get data :budget))
         (by (drover-remote--line (plist-get data :by) 200))
         (task (drover-remote--line (plist-get data :pt) 200)))
    (cond
     ((not kind) "no kind of goal of a known shape")
     ((memq 'invalid (list commands sources setup)) "commands of no known shape")
     ((and (eq kind 'check) (null commands)) "a goal of checks needs commands")
     ((and prompt (not (eq prompt :false)) (not (stringp prompt))) "a prompt of no known shape")
     ((and (eq kind 'criterion)
           (not (and (stringp objective) (not (string-blank-p objective)) (<= (length objective) 3000))))
      "a criterion goal needs an objective of 3000 characters at most")
     ((memq 'invalid (list iterations minutes rounds)) "limits of no known shape")
     ((not (or (null budget) (eq budget :false) (and (numberp budget) (>= budget 0)))) "a budget of no known shape")
     (t
      (append
       (list :kind kind :commands commands
             :sources (or sources (make-list (length commands) (format "given by %s" peer-name)))
             :setup setup
             :baseline (eq (plist-get data :baseline) t)
             :prompt (and (stringp prompt) (not (string-blank-p prompt)) (substring-no-properties prompt))
             :objective (and (eq kind 'criterion) (substring-no-properties objective))
             :title (drover-remote--line (plist-get data :title) 160)
             :by (if by (format "%s on %s" by peer-name) peer-name)
             :peer-task task)
       (and iterations (list :iterations iterations))
       (and minutes (list :minutes minutes))
       (and rounds (list :rounds rounds))
       (cond ((eq budget :false) (list :budget nil))
             (budget (list :budget budget))))))))

(defun drover-remote-goal--allowed (peer what)
  "Return why PEER may not WHAT, a line naming the action, or nil.
Setting a goal runs commands on this host outside any harness: it takes
`full' access; pausing, resuming and stopping one take `act'."
  (let ((access (drover-remote--access peer)))
    (cond
     ((and (equal what "set a goal") (not (eq access 'full)))
      (drover-remote--refuse-action peer what (format "%s has %s access: a goal runs its checks here, which takes full"
                                                      (drover-remote--label peer) access)))
     ((not (drover-remote--allows-p access "send"))
      (drover-remote--refuse-action peer what (format "%s has %s access" (drover-remote--label peer) access))))))

(defun drover-remote-goal--on-set (state peer message reply)
  "Set for PEER the goal MESSAGE asks on one of this host's sessions, in STATE.
MESSAGE's :key names the session, the rest the goal (see
`drover-remote-goal-properties').  Its :id has it set once, asked
again after a stream ended.  REPLY gets the goal, or why it was not set."
  (let* ((id (plist-get message :id))
         (known (and (stringp id) (gethash id (drover-remote--state-outcomes state)))))
    (cond
     ((drover-remote-goal--allowed peer "set a goal")
      (funcall reply nil (drover-remote-goal--allowed peer "set a goal")))
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((and known (car known)) (drover-remote--new-reply reply (cdr known)))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (let* ((key (drover-remote--key (plist-get message :key)))
             (record (and key (car (drover-remote--served key))))
             (properties (drover-remote-goal-properties message (drover-remote--peer-name peer))))
        (cond
         ((not record) (funcall reply nil "no such session here"))
         ((stringp properties) (funcall reply nil (drover-remote--refuse-action peer "set a goal" properties)))
         (t
          (drover-remote--begin-outcome state id reply)
          (let ((answer (condition-case err
                            (let ((goal (drover-quietly (apply #'drover-goal-start record properties))))
                              (drover-remote--event peer 'goal "set a goal on %s" (plist-get goal :title))
                              (list :goal (drover-remote-goal--wire goal)))
                          (error (list :failure (or (drover-bound-line (drover-error-message err)) "failed")))))
                (entry (gethash id (drover-remote--state-outcomes state))))
            (puthash id (cons t answer) (drover-remote--state-outcomes state))
            (dolist (pending (cdr entry))
              (drover-with-reporting "remote goal"
                (drover-remote--new-reply pending answer)))))))))))

(defun drover-remote-goal--log-tail (goal)
  "Return the end of GOAL's log.
As `drover-remote-goal-log-limit' characters at most."
  (let ((file (drover-goal-log-file goal)))
    (if (not (file-readable-p file))
        ""
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)
              (size (or (file-attribute-size (file-attributes file)) 0)))
          (insert-file-contents file nil (max 0 (- size (* 4 drover-remote-goal-log-limit))) size))
        (buffer-substring-no-properties (max (point-min) (- (point-max) drover-remote-goal-log-limit))
                                        (point-max))))))

(defun drover-remote-goal--on-act (peer message reply)
  "Act for PEER on the goal MESSAGE names, as its :action says, telling REPLY.
`pause', `resume' and `stop' take `act' access; `seen' marks it seen,
`log' reads it, as `read' access does.  The answer holds the goal, and
for `log' the end of its log."
  (let* ((id (drover-remote--line (plist-get message :goal) 100))
         (action (plist-get message :action))
         (goal (and id (drover-goal--find id))))
    (cond
     ((not goal) (funcall reply nil "no such goal here"))
     ((not (member action '("pause" "resume" "stop" "seen" "log"))) (funcall reply nil "no such action on a goal"))
     ((and (member action '("pause" "resume" "stop")) (drover-remote-goal--allowed peer (concat action " a goal")))
      (funcall reply nil (drover-remote-goal--allowed peer (concat action " a goal"))))
     (t
      (condition-case err
          (progn
            (drover-quietly
              (pcase action
                ("pause" (drover-goal-pause goal))
                ("resume" (drover-goal-resume goal))
                ("stop" (drover-goal-stop goal))
                ("seen" (unless (plist-get goal :seen)
                          (setf (plist-get goal :seen) t)
                          (drover-goal--changed goal)))))
            (when (member action '("pause" "resume" "stop"))
              (drover-remote--event peer 'goal "%s the goal of %s" action (plist-get goal :title)))
            (funcall reply (append (list :goal (drover-remote-goal--wire goal))
                                   (and (equal action "log") (list :log (drover-remote-goal--log-tail goal))))))
        (error (funcall reply nil (drover-error-message err))))))))

;;;###autoload
(defun drover-remote-goal-answer (state peer connection message reply)
  "Answer PEER's request MESSAGE about this host's goals, through REPLY.
STATE is the remote layer's, CONNECTION the one MESSAGE came by."
  (pcase (plist-get message :op)
    ("goals" (drover-remote-goal--on-goals connection message reply))
    ("goal-checks" (drover-remote-goal--on-checks peer message reply))
    ("goal-set" (drover-remote-goal--on-set state peer message reply))
    ("goal-act" (drover-remote-goal--on-act peer message reply))
    (_ (funcall reply nil "unknown request"))))

;;;###autoload
(defun drover-remote-goal-task-start (peer-name data info plan then)
  "Start the task INFO's session for PEER-NAME, in PLAN's place, under DATA.
DATA is the goal the peer asks, as `drover-remote-goal-properties'
reads it, its :setup run first, then its baseline when it says so.
THEN gets the session and the goal once both are, else why not, a
line, and nil."
  (let ((properties (drover-remote-goal-properties data peer-name)))
    (if (stringp properties)
        (funcall then properties nil)
      (drover-goal-mode-ensure)
      (drover-goal--start-task-session
       nil info plan
       (list (cl-mapcar (lambda (command source) (list :command command :source source))
                        (plist-get properties :commands) (plist-get properties :sources))
             (mapcar (lambda (command) (list :command command :source "given")) (plist-get properties :setup))
             (plist-get properties :baseline))
       ;; The task's heading is the peer's: no note here.
       (append (list :task nil)
               (drover-goal--plist-without properties '(:kind :commands :sources :setup :baseline :prompt :objective)))
       then))))

;;;; On the viewer

(defvar drover-remote-goal--peers (make-hash-table :test #'equal)
  "The goals each peer told, by its name.
As `drover-remote-goal--decode' reads them, the newest first.")

(defvar drover-remote-goal--states (make-hash-table :test #'equal)
  "The state each goal of a peer was seen in last, by (PEER . ID).
A goal of this host's task whose state goes from running or paused to
over notes the task's heading, once.")

(defun drover-remote-goal--offset (peer)
  "Return how far PEER's clock is ahead of this host's, in seconds."
  (or (when-let* ((connection (drover-remote--peer-connection peer)))
        (spyglass-connection-clock-offset connection))
      0))

(defun drover-remote-goal--decode (name data offset)
  "Return DATA, a goal the peer NAME told, as a goal of `drover-goal', or nil.
Its times are moved OFFSET seconds; :host names NAME, :key its
session's key here."
  (when-let* (((and (consp data) (keywordp (car data))))
              (id (drover-remote--line (plist-get data :id) 100))
              (state (car (memq (drover-remote--symbol (plist-get data :st)) drover-goal--states)))
              (kind (car (memq (drover-remote--symbol (plist-get data :kd)) '(check criterion))))
              (link (drover-remote--line (plist-get data :ln) 300)))
    (let ((key (drover-remote--key (plist-get data :k)))
          (strings (lambda (value limit length)
                     (and (vectorp value)
                          (seq-keep (lambda (item) (drover-remote--line item length))
                                    (seq-take value limit)))))
          (count (lambda (value) (if (natnump value) (min value 100000) 0))))
      (list :host name :id id
            :key (and key (drover-remote-session-key name key))
            :link link
            :title (or (drover-remote--line (plist-get data :ti) 160) "")
            :kind kind :state state
            :reason (drover-remote--line (plist-get data :rs) 400)
            :iteration (funcall count (plist-get data :it))
            :iterations (funcall count (plist-get data :its))
            :commands (funcall strings (plist-get data :cm) drover-remote-goal-commands-limit 500)
            :objective (drover-remote--line (plist-get data :ob) 3000)
            :flagged (funcall strings (plist-get data :fl) 50 300)
            :directory (or (drover-remote--line (plist-get data :dir) 1024) "")
            :started (or (drover-remote--time (plist-get data :s) offset) 0)
            :ended (drover-remote--time (plist-get data :e) offset)
            :summary (drover-remote--line (plist-get data :sm) 200)
            :seen (eq (plist-get data :sn) t)
            :by (drover-remote--line (plist-get data :by) 200)
            :peer-task (drover-remote--line (plist-get data :pt) 200)))))

(defun drover-remote-goal--note (goal)
  "Note the end of GOAL, a peer's, on the heading of the task it serves here."
  (when-let* ((id (plist-get goal :peer-task)))
    (condition-case err
        (when-let* ((marker (drover-task-org-marker id)))
          (drover-task-org-write
           marker nil
           (format "Goal %s on %s: %s.%s"
                   (pcase (plist-get goal :state) ('met "met") ('given-up "given up") (_ "stopped"))
                   (drover-host-label (plist-get goal :host))
                   (string-join (delq nil (list (plist-get goal :summary) (plist-get goal :reason))) ", ")
                   (if (plist-get goal :commands)
                       (format "\nChecks: %s" (drover-repo-join-commands (plist-get goal :commands)))
                     ""))))
      (error (drover-warn "Goal of %s on %s: no note on its heading: %s" (plist-get goal :title)
                          (plist-get goal :host) (drover-error-message err))))))

(defun drover-remote-goal--store (name goals offset)
  "Keep GOALS, what the peer NAME told of its goals, its clock OFFSET ahead.
Lists showing their sessions are drawn again, and the heading of a task
of this host whose goal ended is noted."
  (when (vectorp goals)
    (let* ((decoded (seq-keep (lambda (data) (drover-remote-goal--decode name data offset))
                              (seq-take goals drover-remote-goal-limit)))
           (old (gethash name drover-remote-goal--peers))
           (keys nil))
      (dolist (goal decoded)
        (let* ((key (cons name (plist-get goal :id)))
               (before (gethash key drover-remote-goal--states)))
          (when (and before (memq before (cons 'paused drover-goal--active))
                     (memq (plist-get goal :state) '(met given-up stopped)))
            (drover-remote-goal--note goal))
          (puthash key (plist-get goal :state) drover-remote-goal--states)))
      (puthash name decoded drover-remote-goal--peers)
      (dolist (goal (append old decoded))
        (when-let* ((key (plist-get goal :key)))
          (push key keys)))
      (when keys
        (drover-views-changed (delete-dups keys)))
      (drover-goal--list-refresh))))

(defun drover-remote-goal-follow (peer)
  "Follow PEER's goals, as it tells them now and as they change."
  (let ((name (drover-remote--peer-name peer)))
    (drover-remote--request
     peer (list :op "goals" :follow t)
     (lambda (reply error)
       (drover-with-reporting "remote goals"
         (unless error
           (drover-remote-goal--store name (plist-get reply :goals) (drover-remote-goal--offset peer)))))
     (+ (float-time) drover-remote-request-timeout))))

(defun drover-remote-goal-follow-all ()
  "Follow the goals of every connected peer telling `goals'."
  (dolist (peer (drover-remote--peers))
    (when (and (eq (drover-remote--peer-state peer) 'open)
               (not (drover-remote--viewer-p peer))
               (drover-remote--capable-p peer "goals"))
      (drover-remote-goal-follow peer))))

;;;###autoload
(defun drover-remote-goal-on-message (peer message)
  "Take MESSAGE, PEER's `goals' message: its goals as they are now."
  (drover-remote-goal--store (drover-remote--peer-name peer) (plist-get message :goals)
                             (drover-remote-goal--offset peer)))

(defun drover-remote-goal-goals ()
  "Return the goals the peers told, newest first by peer."
  (let ((goals nil))
    (dolist (peer (drover-remote--peers))
      (setq goals (append goals (gethash (drover-remote--peer-name peer) drover-remote-goal--peers))))
    goals))

(defun drover-remote-goal-of (session)
  "Return the latest goal of SESSION, a session of a peer, or nil."
  (when-let* ((name (drover-session-host session))
              (key (drover-session-engine-key session)))
    (seq-find (lambda (goal) (or (equal (plist-get goal :key) (drover-session-key session))
                                 (equal (drover-goal--key goal) key)))
              (gethash name drover-remote-goal--peers))))

(defun drover-remote-goal-find (name id)
  "Return the goal ID the peer NAME told, or nil."
  (seq-find (lambda (goal) (equal (plist-get goal :id) id)) (gethash name drover-remote-goal--peers)))

(defun drover-remote-goal--peer (name)
  "Return the peer NAME, to set or act on a goal there, or signal."
  (let ((peer (drover-remote--peer name)))
    (cond
     ((not peer) (user-error "%s is not a peer" name))
     ((not (eq (drover-remote--peer-state peer) 'open))
      (user-error "%s is not connected" (drover-remote--label peer)))
     ((not (drover-remote--capable-p peer "goals"))
      (user-error "%s runs an older drover, which runs no goal for other hosts: update it there"
                  (drover-remote--label peer)))
     (t peer))))

(defun drover-remote-goal--ask (peer message &optional seconds)
  "Ask PEER MESSAGE, waited for SECONDS at most; return its reply.
Signal a `user-error' telling why when it fails."
  (pcase-let ((`(,reply ,error) (drover-remote--wait peer message (or seconds drover-remote-request-timeout))))
    (when error
      (user-error "%s: %s" (drover-remote--label peer) error))
    reply))

(defun drover-remote-goal--decode-commands (value)
  "Return VALUE, commands told as [COMMAND SOURCE], as plists.
Each of :command and :source."
  (and (vectorp value)
       (seq-keep (lambda (pair)
                   (when-let* (((and (vectorp pair) (length= pair 2)))
                               (command (drover-remote--line (aref pair 0) 2000))
                               ((not (string-blank-p command))))
                     (list :command command :source (or (drover-remote--line (aref pair 1) 300) ""))))
                 (seq-take value drover-remote-goal-commands-limit))))

(defun drover-remote-goal-checks (name &rest where)
  "Return the checks of a goal on the peer NAME, as it reads them, waited for.
WHERE is :key, the key of one of NAME's sessions as it travels, :repo,
a repository's name, or :directory, one of NAME's directories.  A plist
of :directory, :checks and :setup, plists of :command and :source,
:note, :fresh and :criterion (see `drover-remote-goal--on-checks')."
  (let* ((peer (drover-remote-goal--peer name))
         (reply (progn
                  (drover-message "Reading the checks on %s…" (drover-remote--label peer))
                  (drover-remote-goal--ask peer (append (list :op "goal-checks") where)
                                           (+ drover-repo-wait drover-remote-request-timeout)))))
    (drover-message "")
    (list :directory (or (drover-remote--line (plist-get reply :directory) 1024)
                         (user-error "%s told no directory" (drover-remote--label peer)))
          :checks (drover-remote-goal--decode-commands (plist-get reply :checks))
          :setup (drover-remote-goal--decode-commands (plist-get reply :setup))
          :note (drover-remote--line (plist-get reply :note) 300)
          :fresh (eq (plist-get reply :fresh) t)
          :criterion (eq (plist-get reply :criterion) t))))

(defun drover-remote-goal-wire-properties (properties)
  "Return PROPERTIES, of `drover-goal-start', as a goal travels to its host."
  (append
   (list :kind (symbol-name (or (plist-get properties :kind) 'check))
         :commands (vconcat (plist-get properties :commands))
         :sources (vconcat (plist-get properties :sources))
         :setup (vconcat (plist-get properties :setup))
         :baseline (if (plist-get properties :baseline) t :false)
         :prompt (or (plist-get properties :prompt) :false)
         :objective (or (plist-get properties :objective) :false)
         :title (or (plist-get properties :title) :false)
         :iterations (or (plist-get properties :iterations) drover-goal-iterations)
         :minutes (or (plist-get properties :minutes) drover-goal-minutes)
         :rounds (or (plist-get properties :rounds) drover-goal-rounds)
         :budget (let ((budget (if (plist-member properties :budget) (plist-get properties :budget) drover-goal-budget)))
                   (or budget :false))
         :by (or (plist-get properties :by) :false)
         :pt (or (plist-get properties :task) :false))))

(defun drover-remote-goal-set (session properties)
  "Set a goal of PROPERTIES on SESSION, of a peer, there; return it.
PROPERTIES are those of `drover-goal-start', :task the heading here.
Waited for in the command."
  (let* ((name (drover-session-host session))
         (peer (drover-remote-goal--peer name))
         (reply (drover-remote-goal--ask
                 peer (append (list :op "goal-set" :id (drover-remote--action-id peer)
                                    :key (drover-remote--wire-key session))
                              (drover-remote-goal-wire-properties properties))))
         (goal (or (drover-remote-goal--decode name (plist-get reply :goal) (drover-remote-goal--offset peer))
                   (user-error "%s told no goal" (drover-remote--label peer)))))
    (drover-remote-goal--keep goal)
    goal))

(defun drover-remote-goal--keep (goal)
  "Keep GOAL, a goal of a peer as it told it now, among those it told."
  (let* ((name (plist-get goal :host))
         (others (seq-remove (lambda (other) (equal (plist-get other :id) (plist-get goal :id)))
                             (gethash name drover-remote-goal--peers))))
    (puthash (cons name (plist-get goal :id)) (plist-get goal :state) drover-remote-goal--states)
    (puthash name (cons goal others) drover-remote-goal--peers)
    (when-let* ((key (plist-get goal :key)))
      (drover-views-changed (list key)))
    (drover-goal--list-refresh)))

(defun drover-remote-goal-act (goal action)
  "Have the host of GOAL ACTION it: `pause', `resume', `stop' or `log'.
Waited for in the command.  Return the goal as it is then, and for
`log' (GOAL . LOG), LOG the end of its log."
  (let* ((name (plist-get goal :host))
         (peer (drover-remote-goal--peer name))
         (reply (drover-remote-goal--ask peer (list :op "goal-act" :goal (plist-get goal :id)
                                                    :action (symbol-name action))))
         (now (or (drover-remote-goal--decode name (plist-get reply :goal) (drover-remote-goal--offset peer))
                  goal)))
    (drover-remote-goal--keep now)
    (if (eq action 'log)
        (cons now (or (drover-remote--plain (plist-get reply :log)) ""))
      now)))

(defun drover-remote-goal-seen (goal)
  "Have the host of GOAL, a peer's, mark it seen, without waiting."
  (setf (plist-get goal :seen) t)
  (when-let* ((peer (drover-remote--peer (plist-get goal :host)))
              ((eq (drover-remote--peer-state peer) 'open))
              ((drover-remote--capable-p peer "goals")))
    (drover-remote--request peer (list :op "goal-act" :goal (plist-get goal :id) :action "seen") #'ignore
                            (+ (float-time) drover-remote-request-timeout))))

(defun drover-remote-goal-task (name request goal callback)
  "Ask the peer NAME REQUEST about a task, its session under GOAL.
GOAL holds the properties of `drover-goal-start' the goal takes; the
request then takes `full' access.  CALLBACK gets (REPLY ERROR) once."
  (drover-remote-goal--peer name)
  (drover-remote-ask-task name (append request (list :goal (drover-remote-goal-wire-properties goal)))
                          callback (+ drover-task-setup-timeout 180)))

(provide 'drover-remote-goal)
;;; drover-remote-goal.el ends here
