;;; drover-anvil.el --- Agents acting on drover's sessions through Anvil  -*- lexical-binding: t; -*-

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

;; MCP tools, served by Anvil, through which agents list, read, start,
;; prompt, wait on, answer, interrupt and archive the sessions drover
;; knows, of every harness and host, through drover's operations as
;; the user's commands do, have their own session prompted after an
;; event such as a restart, schedule prompts for later, once or
;; recurring, and fire events: `drover-anvil-enable' registers them
;; with Anvil, `drover-anvil-disable' removes them.
;;
;; What an agent may do is the user's to say, `drover-anvil-permissions',
;; action by action: on any session, on those of the caller's project,
;; on those the caller started, or never.  By default agents read every
;; session and wait on any, start and prompt sessions of their own
;; project, interrupt, archive and answer the questions of the sessions
;; they started, and answer no permission.  The caller is the session
;; whose agent calls, as its client tells it (`drover-anvil-callers'):
;; OpenCode names it in each call's `_meta', and Claude Code records the
;; conversation of the process that started Anvil's bridge.  No tool
;; acts on the caller itself nor on a session its turn is part of, the
;; sessions above it, nor waits on one of those, or on a session
;; waiting on the caller: that would wait on itself.  A caller its
;; client does not name may only do what applies to any session.
;; Every call is logged, in `drover-anvil-log-file', and a mutating
;; call that cannot be logged is refused.
;;
;; What ends later (a turn, a recap, a session starting) is a job:
;; while the client waits (`anvil-server-can-defer-p', Anvil's stdio
;; bridge), the tool answers once the job did, Anvil polling it, never
;; Emacs waiting; otherwise the tool answers with the job, whose answer
;; `drover-job' returns once there.  Outputs are bounded: lines and
;; texts as drover bounds them, lists to `drover-anvil-list-limit'
;; sessions; nothing of the hosts' or services' credentials is told.
;;
;; The file needs Anvil only once the tools are registered: it declares
;; what it uses of it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'iso8601)
(require 'drover-core)
(require 'drover-model)

(declare-function anvil-server-register-tool "ext:anvil-server" (handler &rest properties))
(declare-function anvil-server-unregister-tool "ext:anvil-server" (tool-id &optional server-id))
(declare-function anvil-server-defer "ext:anvil-server" (poll &rest keys))
(declare-function anvil-server-can-defer-p "ext:anvil-server" ())
(declare-function drover-claude-code-pid-session-id "drover-claude-code" (pid))
(defvar anvil-server-request-meta)
(defvar anvil-server-transport-pid)

;;;; Options

(defgroup drover-anvil nil
  "Agents acting on drover's sessions through Anvil's MCP tools."
  :group 'drover
  :prefix "drover-anvil-")

(defcustom drover-anvil-permissions
  '((list . any) (read . any) (wait . any)
    (create . project) (send . project)
    (interrupt . own) (archive . own)
    (answer-question . own) (answer-permission . nil)
    (follow-up . self) (schedule . project) (fire . nil))
  "What agents may do through the tools, by action.
Each entry is (ACTION . SCOPE).  ACTION is one of `list', `read',
`wait', `create', `send', `interrupt', `archive', `answer-question',
`answer-permission', `follow-up', `schedule', having drover prompt a
session later, once, recurring or after an event (`drover-schedule'),
and `fire', telling drover an event happened, which releases the
prompts waiting for it, of any session.  SCOPE is:

 `any'      any session, of any host;
 `project'  the sessions of this host running in the caller's project,
            the repository of its directory, worktrees included, or in
            `drover-anvil-scratch-directories'; for `create', those
            directories;
 `own'      the sessions the caller started through the tools, and its
            subagents;
 `self'     for `follow-up' and `schedule', the caller itself;
 nil        none.

`fire' takes `any' or nil: an event concerns no one session.

An action not listed is allowed nowhere.  Whatever the scope, no tool
acts on the caller itself nor on the sessions above it, whose turns
wait for its own, but `follow-up' and `schedule', which have drover
prompt a session later, once its turn is long over; and a caller its
client does not name may only do what applies to `any' session, so it
has no follow-up."
  :type '(alist :key-type (choice (const list) (const read) (const wait) (const create) (const send)
                                  (const interrupt) (const archive)
                                  (const answer-question) (const answer-permission)
                                  (const follow-up) (const schedule) (const fire))
                :value-type (choice (const :tag "Any session" any)
                                    (const :tag "The caller's project" project)
                                    (const :tag "Started by the caller" own)
                                    (const :tag "The caller itself, for follow-up and schedule" self)
                                    (const :tag "None" nil)))
  :group 'drover-anvil)

(defcustom drover-anvil-scratch-directories nil
  "Directories counting as part of every caller's project.
Sessions running in them or below, and new sessions started there, are
in the scope `project' of `drover-anvil-permissions', such as a
scratch directory where agents try things out."
  :type '(repeat directory)
  :group 'drover-anvil)

(defcustom drover-anvil-create-harnesses '(opencode)
  "Harnesses whose sessions agents may start, backend names.
Agents start sessions without showing them in a window (see the
option :hidden of :create in `drover-register-backend')."
  :type '(repeat symbol)
  :group 'drover-anvil)

(defcustom drover-anvil-server-id "emacs-eval"
  "Identifier of Anvil's server the tools are registered with."
  :type 'string
  :group 'drover-anvil)

(defcustom drover-anvil-callers '(drover-anvil-opencode-caller drover-anvil-claude-code-caller)
  "Functions naming the session whose agent calls a tool.
Each is called with the `_meta' of the call, an alist, and returns the
key of the calling session, (BACKEND . ID), or nil; the first non-nil
wins.  A function may look at the call's transport too, as
`drover-anvil-claude-code-caller' does."
  :type '(repeat function)
  :group 'drover-anvil)

(defcustom drover-anvil-wait-timeout 600
  "Seconds `drover-session-wait' waits by default."
  :type 'natnum
  :group 'drover-anvil)

(defcustom drover-anvil-wait-limit 3600
  "Seconds `drover-session-wait' waits at most, whatever the agent asks."
  :type 'natnum
  :group 'drover-anvil)

(defcustom drover-anvil-list-limit 200
  "Sessions `drover-sessions' lists at most in one answer."
  :type 'natnum
  :group 'drover-anvil)

(defcustom drover-anvil-prompt-limit 50000
  "Characters a prompt sent through the tools may hold at most."
  :type 'natnum
  :group 'drover-anvil)

(defcustom drover-anvil-log-file nil
  "File logging every call of the tools, or nil for the default.
The default is anvil.log in `drover-state-directory'.  Each line tells
when, which tool, the caller, the session, what came of it and a
bounded note.  Past `drover-anvil-log-size' bytes, the file is renamed
with the suffix .1, over the one before, and a new one begins."
  :type '(choice (const :tag "In drover's state directory" nil) file)
  :group 'drover-anvil)

(defcustom drover-anvil-log-size 1048576
  "Bytes past which the log of the tools begins anew, the older one kept."
  :type 'natnum
  :group 'drover-anvil)

(defconst drover-anvil-actions
  '(list read wait create send interrupt archive answer-question answer-permission follow-up schedule fire)
  "The actions of `drover-anvil-permissions'.")

;;;; Text and data

(defun drover-anvil--line (value &optional limit)
  "Return VALUE, a string, as a plain line of LIMIT characters at most, or nil."
  (and (stringp value)
       (not (string-empty-p value))
       (substring-no-properties (drover-bound-line value limit))))

(defun drover-anvil--text (value &optional lines limit)
  "Return VALUE, a string, as a plain text bounded to LINES and LIMIT, or nil."
  (and (stringp value)
       (not (string-blank-p value))
       (substring-no-properties (drover-bound-text value lines limit))))

(defun drover-anvil--time (time)
  "Return TIME, a float time or nil, as an ISO 8601 string, or nil."
  (and (numberp time) (> time 0)
       (format-time-string "%FT%T%z" time)))

(defun drover-anvil--read-time (value)
  "Return VALUE, epoch seconds or an ISO 8601 string, as a float time, or nil."
  (cond
   ((numberp value) (float value))
   ((and (stringp value) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
    (string-to-number value))
   ((and (stringp value) (< (length value) 64))
    (condition-case nil
        (float-time (encode-time (iso8601-parse value)))
      (error nil)))))

(defun drover-anvil--true-p (value)
  "Return non-nil when VALUE, a parameter received, means true."
  (or (eq value t) (equal value "true")))

(defun drover-anvil--false-p (value)
  "Return non-nil when VALUE, a parameter received, means false."
  (or (eq value :json-false) (equal value "false")))

(defun drover-anvil--compact (alist)
  "Return ALIST without its entries whose value is nil."
  (seq-remove (lambda (entry) (null (cdr entry))) alist))

(defun drover-anvil--json (alist)
  "Return ALIST, without its nil entries, as JSON text."
  (let ((json-encoding-pretty-print nil))
    (json-encode (drover-anvil--compact alist))))

(define-error 'drover-anvil-refused "Refused")

(defun drover-anvil--refuse (format-string &rest args)
  "Signal that the tool refuses, as FORMAT-STRING and ARGS tell, to the agent.
The error is Anvil's tool error, which the agent reads."
  (signal (if (get 'anvil-server-tool-error 'error-conditions) 'anvil-server-tool-error 'drover-anvil-refused)
          (list (drover-bound-line (apply #'format-message format-string args) 500))))

;;;; Keys

(defun drover-anvil-key-string (session)
  "Return the key of SESSION as the tools name it, a string.
BACKEND/ID, and for a session of another host, which its proxy names
by its owner's key, BACKEND/OWNER-BACKEND/ID."
  (let ((id (drover-session-id session)))
    (if (consp id)
        (format "%s/%s/%s" (drover-session-backend session) (car id) (cdr id))
      (format "%s/%s" (drover-session-backend session) id))))

(defun drover-anvil--parse-key (string)
  "Return the key STRING names, (BACKEND . ID), or nil.
Only registered backends are named: nothing is interned."
  (when (and (stringp string) (< (length string) 512)
             (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" string))
    (let* ((backend (intern-soft (match-string 1 string)))
           (rest (match-string 2 string)))
      (when (and backend (memq backend (drover-backends)))
        (if (drover-backend-property backend :remote)
            (when (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" rest)
              (when-let* ((owner (intern-soft (match-string 1 rest))))
                (cons backend (cons owner (match-string 2 rest)))))
          (cons backend rest))))))

(defun drover-anvil--session (string)
  "Return the live session STRING names, or refuse."
  (let ((key (drover-anvil--parse-key string)))
    (or (and key (drover-index-session key))
        (drover-anvil--refuse "No live session %s: list them with drover-sessions"
                              (drover-bound-line (format "%s" string) 200)))))

;;;; The caller

(defun drover-anvil-opencode-caller (meta)
  "Return the key of the OpenCode session META names, or nil.
OpenCode names the session calling an MCP tool in the call's `_meta',
as ai.opencode/sessionID."
  (let ((id (alist-get 'ai.opencode/sessionID meta)))
    (and (stringp id)
         (string-match-p "\\`[A-Za-z0-9_-]\\{1,128\\}\\'" id)
         (cons 'opencode id))))

(defconst drover-anvil--client-depth 3
  "How many processes above Anvil's bridge are looked at for its client.
The client is the bridge's parent, or above it when a program, such as
cmd or env, starts the bridge for it.")

(defun drover-anvil--claude-code-conversation (pid)
  "Return the conversation of the nearest Claude Code process above PID, or nil.
PID is a process identifier, as `process-attributes' takes it.  The
walk stops at a process started after the one below it: it took the
identifier of a process that ended.  See
`drover-anvil-claude-code-caller'."
  (let ((attributes (process-attributes pid))
        (depth 0)
        (id nil))
    (while (and attributes (null id) (< depth drover-anvil--client-depth))
      (let* ((parent (alist-get 'ppid attributes))
             (above (and (natnump parent) (> parent 0) (process-attributes parent)))
             (start (alist-get 'start attributes))
             (parent-start (alist-get 'start above)))
        (setq depth (1+ depth)
              attributes (and above
                              (not (and start parent-start (time-less-p start parent-start)))
                              above))
        (when attributes
          (setq id (drover-claude-code-pid-session-id parent)))))
    id))

(defun drover-anvil-claude-code-caller (_meta)
  "Return the key of the Claude Code session calling the tool, or nil.
Claude Code names no session in a call's `_meta'.  It starts Anvil's
stdio bridge, a process for each of its sessions, which tells its
process (`anvil-server-transport-pid'), and it records the conversation
each of its processes runs, rewritten as /clear or /resume switches it
\(`drover-claude-code-pid-session-id').  The caller is the session
listed for the conversation of the nearest Claude Code process above
the bridge, among `drover-anvil--client-depth' processes (see
`drover-index-conversation'): a front end's buffer showing the
conversation before its transcript.  A conversation drover has not
learned yet, as that of a terminal just started, names none."
  (when-let* (((featurep 'drover-claude-code))
              (pid (bound-and-true-p anvil-server-transport-pid))
              ((natnump pid))
              (id (drover-anvil--claude-code-conversation pid)))
    (drover-index-conversation (cons 'claude-code id))))

(defun drover-anvil--caller ()
  "Return the key of the session calling the tool, or nil when unknown."
  (let ((meta (and (boundp 'anvil-server-request-meta) anvil-server-request-meta)))
    (when (listp meta)
      (seq-some (lambda (function)
                  (drover-with-reporting "Anvil caller"
                    (funcall function meta)))
                drover-anvil-callers))))

(defun drover-anvil--above (key)
  "Return the keys of the sessions above the session KEY, nearest first."
  (let ((above nil)
        (seen (make-hash-table :test #'equal)))
    (while (and key (not (gethash key seen)))
      (puthash key t seen)
      (setq key (drover-index-parent key))
      (when key
        (push key above)))
    (nreverse above)))

(defun drover-anvil--busy-with-caller-p (caller key)
  "Return non-nil when the session KEY is CALLER, or a session above it.
Its turn waits for the caller's: acting on it, or waiting for it, would
act on or wait for the caller itself."
  (and caller (or (equal key caller) (member key (drover-anvil--above caller)))))

;;;; Scopes

(defvar drover-anvil--started (make-hash-table :test #'equal)
  "The sessions agents started through the tools, by key.
Each maps to the key of the session that started it.")

(defun drover-anvil--worktree-main (dotgit)
  "Return the main worktree of the repository of DOTGIT, a .git file, or nil.
DOTGIT is the file a linked worktree has in place of a directory."
  (when-let* ((text (with-temp-buffer
                      (insert-file-contents dotgit nil 0 4096)
                      (buffer-string)))
              ((string-match "\\`gitdir: *\\(.+?\\)[ \t\r]*$" text))
              (gitdir (expand-file-name (drover--native-file-name (match-string 1 text))
                                        (file-name-directory dotgit)))
              (commondir (expand-file-name "commondir" gitdir))
              ((file-readable-p commondir))
              (common (with-temp-buffer
                        (insert-file-contents commondir nil 0 4096)
                        (string-trim (buffer-string))))
              (common (directory-file-name (expand-file-name (drover--native-file-name common) gitdir))))
    (and (equal (file-name-nondirectory common) ".git")
         (file-name-directory common))))

(defvar drover-anvil--roots nil
  "The projects of the directories looked at during a call, by directory, or nil.
A call looking at many sessions binds it to a table, so that each
directory's repository is looked for once.")

(defun drover-anvil--project-root (directory)
  "Return the project of DIRECTORY, as a true directory name, or nil.
It is the main worktree of DIRECTORY's repository, the same for every
worktree of it, else DIRECTORY itself; nil for no DIRECTORY."
  (when (and (stringp directory) (not (file-remote-p directory)))
    (let ((directory (file-name-as-directory (expand-file-name directory))))
      (or (and drover-anvil--roots (gethash directory drover-anvil--roots))
          (let* ((top (ignore-errors (locate-dominating-file directory ".git")))
                 (dotgit (and top (expand-file-name ".git" top)))
                 (root (file-name-as-directory
                        (file-truename (cond ((null top) directory)
                                             ((file-directory-p dotgit) top)
                                             ((ignore-errors (drover-anvil--worktree-main dotgit)))
                                             (t top))))))
            (when drover-anvil--roots
              (puthash directory root drover-anvil--roots))
            root)))))

(defun drover-anvil--in-directory-p (directory root)
  "Return non-nil when DIRECTORY is ROOT or below it."
  (and (stringp directory) (stringp root)
       (string-prefix-p (file-name-as-directory (file-truename root))
                        (file-name-as-directory (file-truename directory)))))

(defun drover-anvil--in-project-p (caller directory)
  "Return non-nil when DIRECTORY is in the project of the session CALLER, a key."
  (when-let* ((record (and caller (drover-index-session caller)))
              ((null (drover-session-host record)))
              (own (drover-session-directory record))
              ((stringp directory))
              ((not (file-remote-p directory))))
    (or (seq-some (lambda (root) (drover-anvil--in-directory-p directory root))
                  drover-anvil-scratch-directories)
        (equal (drover-anvil--project-root own) (drover-anvil--project-root directory)))))

(defun drover-anvil--own-p (caller key)
  "Return non-nil when the session KEY is the session CALLER's.
That is a session CALLER started, or one below it or below CALLER."
  (and caller
       (or (equal (gethash key drover-anvil--started) caller)
           (member caller (drover-anvil--above key))
           (seq-some (lambda (above) (equal (gethash above drover-anvil--started) caller))
                     (drover-anvil--above key)))))

(defun drover-anvil--scope (action)
  "Return the scope `drover-anvil-permissions' gives ACTION."
  (alist-get action drover-anvil-permissions))

(defun drover-anvil--in-scope-p (scope caller session)
  "Return non-nil when SESSION is in SCOPE for the session CALLER."
  (pcase scope
    ('any t)
    ('project (and (null (drover-session-host session))
                   (drover-anvil--in-project-p caller (drover-session-directory session))))
    ('own (drover-anvil--own-p caller (drover-session-key session)))
    ('self (and caller (equal caller (drover-session-key session))))))

(defun drover-anvil--refusal (action caller session)
  "Return why the session CALLER may not ACTION SESSION, or nil when it may."
  (let ((scope (drover-anvil--scope action)))
    (cond
     ((null scope)
      (format "drover-anvil-permissions allows no %s" action))
     ;; A prompt scheduled goes once the turn is over: it waits for nothing.
     ((and (not (memq action '(list read schedule)))
           (drover-anvil--busy-with-caller-p caller (drover-session-key session)))
      (if (equal caller (drover-session-key session))
          "a session cannot do this to itself"
        "this session's turn waits for the caller's, which would wait for itself"))
     ((and (null caller) (not (eq scope 'any)))
      (format "the caller is unknown, and drover-anvil-permissions allows %s only on %s"
              action (pcase scope
                       ('own "the caller's own sessions")
                       ('self "the caller itself")
                       (_ "the caller's project's sessions"))))
     ((not (drover-anvil--in-scope-p scope caller session))
      (format "drover-anvil-permissions allows %s only on %s" action
              (pcase scope
                ('own "the sessions the caller started, and its subagents")
                ('self "the caller itself")
                (_ "the sessions of the caller's project, on this host")))))))

(defun drover-anvil--check (tool action caller session)
  "Refuse TOOL's ACTION on SESSION unless the session CALLER may do it.
A refusal is logged."
  (when-let* ((why (drover-anvil--refusal action caller session)))
    (drover-anvil--log tool caller (drover-session-key session) "refused" why)
    (drover-anvil--refuse "%s: %s" (drover-anvil-key-string session) why)))

;;;; The log

(defun drover-anvil--log-file ()
  "Return the file logging the calls of the tools."
  (or drover-anvil-log-file (expand-file-name "anvil.log" drover-state-directory)))

(defun drover-anvil--log (tool caller key outcome &optional note)
  "Log the call of TOOL by CALLER on the session KEY, with its OUTCOME and NOTE.
CALLER and KEY are keys or nil, OUTCOME a word, NOTE a string or nil.
Return non-nil once logged; a failure is reported, once in a while."
  (let ((file (drover-anvil--log-file))
        (line (concat (format-time-string "%FT%T%z")
                      "\t" tool
                      "\tcaller=" (if caller (drover-anvil--key-of caller) "-")
                      "\tsession=" (if key (drover-anvil--key-of key) "-")
                      "\t" outcome
                      (if note (concat "\t" (replace-regexp-in-string "[\t\n]+" " " (drover-bound-line note 300))) "")
                      "\n")))
    (condition-case err
        (let ((inhibit-message t)
              (message-log-max nil)
              (coding-system-for-write 'utf-8-unix)
              (attributes (file-attributes file)))
          (make-directory (file-name-directory file) t)
          (when (and attributes (> (file-attribute-size attributes) drover-anvil-log-size))
            (rename-file file (concat file ".1") t))
          (write-region line nil file t 'silent)
          t)
      (error
       (drover-report-failure "Anvil tools log" err)
       nil))))

(defun drover-anvil--log-or-refuse (tool caller key note)
  "Log that TOOL acts for CALLER on the session KEY, with NOTE, or refuse.
An action that cannot be logged is not taken."
  (unless (drover-anvil--log tool caller key "acting" note)
    (drover-anvil--refuse "The action could not be logged in %s, so it is not taken"
                          (abbreviate-file-name (drover-anvil--log-file)))))

;;;; Jobs

;; A job is a plist: :tool, :caller and :key, as logged; :state,
;; `pending', `done' or `failed'; :value, the answer, an alist, or the
;; failure's message; :poll, a function of the job's identifier run as
;; it is polled, settling it when it can; :deadline, past which
;; :timeout, a function of the identifier returning the answer,
;; settles it, or it fails; :finally, a function of the identifier run
;; once it settled; :born, :settled; and what its tool notes as it
;; goes.  Functions name their job by its identifier, never hold it.

(defvar drover-anvil--jobs (make-hash-table :test #'equal)
  "The jobs of the tools, by identifier.")

(defvar drover-anvil--job-counter 0
  "Counter of the identifiers of jobs.")

(defconst drover-anvil--job-keep 600
  "Seconds a job settled and not taken is kept.")

(defconst drover-anvil--jobs-max 200
  "Jobs kept at most; past it, new ones are refused.")

(defun drover-anvil--job-get (id property)
  "Return PROPERTY of the job ID."
  (plist-get (gethash id drover-anvil--jobs) property))

(defun drover-anvil--job-put (id property value)
  "Set PROPERTY of the job ID, if any, to VALUE."
  (when-let* ((job (gethash id drover-anvil--jobs)))
    (puthash id (plist-put job property value) drover-anvil--jobs)))

(defun drover-anvil--sweep ()
  "Forget the jobs settled and never taken, and those pending, long ago.
That is `drover-anvil--job-keep' seconds after they settled, or after
their deadline."
  (let ((now (float-time))
        (gone nil))
    (maphash (lambda (id job)
               (when (if (eq (plist-get job :state) 'pending)
                         (> now (+ (plist-get job :deadline) drover-anvil--job-keep))
                       (> now (+ (plist-get job :settled) drover-anvil--job-keep)))
                 (push id gone)))
             drover-anvil--jobs)
    (mapc #'drover-anvil--drop gone)))

(cl-defun drover-anvil--job (tool caller key &key poll (seconds 30) timeout finally)
  "Start a job of TOOL for CALLER on the session KEY, and return its identifier.
POLL, TIMEOUT and FINALLY are its functions, SECONDS how long it may
take; see the plist of jobs."
  (drover-anvil--sweep)
  (when (>= (hash-table-count drover-anvil--jobs) drover-anvil--jobs-max)
    (drover-anvil--refuse "Too many jobs pending; take their answers with drover-job"))
  (let ((id (format "j%d-%d" (cl-incf drover-anvil--job-counter) (truncate (float-time)))))
    (puthash id (list :tool tool :caller caller :key key :state 'pending
                      :poll poll :timeout timeout :finally finally
                      :born (float-time) :deadline (+ (float-time) seconds))
             drover-anvil--jobs)
    id))

(defun drover-anvil--settle (id state value)
  "Settle the job ID, pending, in STATE, `done' or `failed', with VALUE.
Its outcome is logged, and its :finally run."
  (when (eq (drover-anvil--job-get id :state) 'pending)
    (drover-anvil--job-put id :state state)
    (drover-anvil--job-put id :value value)
    (drover-anvil--job-put id :settled (float-time))
    (drover-anvil--log (drover-anvil--job-get id :tool) (drover-anvil--job-get id :caller)
                       (drover-anvil--job-get id :key) (symbol-name state)
                       (if (stringp value) value (alist-get 'outcome value)))
    (when-let* ((finally (drover-anvil--job-get id :finally)))
      (drover-with-reporting "Anvil job"
        (funcall finally id)))))

(defun drover-anvil--done (id value)
  "Settle the job ID with VALUE, an alist."
  (drover-anvil--settle id 'done value))

(defun drover-anvil--fail (id message)
  "Fail the job ID, telling MESSAGE."
  (drover-anvil--settle id 'failed (drover-bound-line (format "%s" (or message "failed")) 500)))

(defun drover-anvil--drop (id)
  "Forget the job ID, settling it first if pending: no one waits for it."
  (drover-anvil--settle id 'failed "no one waits for it any more")
  (remhash id drover-anvil--jobs))

(defun drover-anvil--advance (id)
  "Poll the job ID, if pending: run its :poll, then its :timeout past its deadline."
  (when (eq (drover-anvil--job-get id :state) 'pending)
    (when-let* ((poll (drover-anvil--job-get id :poll)))
      (condition-case err
          (drover-quietly
            (funcall poll id))
        (error (drover-anvil--fail id (drover-error-message err)))))
    (when (and (eq (drover-anvil--job-get id :state) 'pending)
               (> (float-time) (drover-anvil--job-get id :deadline)))
      (if-let* ((timeout (drover-anvil--job-get id :timeout)))
          (condition-case err
              (drover-anvil--done id (funcall timeout id))
            (error (drover-anvil--fail id (drover-error-message err))))
        (drover-anvil--fail id "it did not end in time")))))

(defun drover-anvil--take (id)
  "Return the answer of the job ID as JSON text once settled, else nil.
The job is forgotten once its answer is taken.
A job failed signals its message, as the tool's error."
  (drover-anvil--advance id)
  (pcase (drover-anvil--job-get id :state)
    ('nil (drover-anvil--refuse "No job %s: its answer was taken, or it was forgotten"
                                (drover-bound-line (format "%s" id) 64)))
    ('pending nil)
    (state (let ((value (drover-anvil--job-get id :value)))
             (remhash id drover-anvil--jobs)
             (if (eq state 'done)
                 (drover-anvil--json value)
               (drover-anvil--refuse "%s" value))))))

(defun drover-anvil--answer (id)
  "Return what the tool starting the job ID answers.
The job's answer when there already; while the client waits, a
deferral answering once the job did; otherwise the job, for
`drover-job'."
  (or (drover-anvil--take id)
      (let ((seconds (+ 30 (max 0 (- (drover-anvil--job-get id :deadline) (float-time))))))
        (if (and (fboundp 'anvil-server-can-defer-p) (anvil-server-can-defer-p))
            (anvil-server-defer (lambda () (drover-anvil--take id))
                                :seconds seconds
                                :cancel (lambda () (drover-anvil--drop id)))
          (drover-anvil--json
           `((pending . t) (job . ,id)
             (note . "This client cannot wait for the answer: call drover-job with this job until it is not pending")))))))

;;;; Calling backends for a job

(defun drover-anvil--call (id function &rest arguments)
  "Call FUNCTION with ARGUMENTS for the job ID, failing it on an error."
  (condition-case err
      (drover-quietly
        (apply function arguments))
    (error (drover-anvil--fail id (drover-error-message err)))))

(defun drover-anvil--else (id)
  "Return a function of a message and more arguments failing the job ID."
  (lambda (&optional message &rest _)
    (drover-with-reporting "Anvil job"
      (drover-anvil--fail id (or message "refused")))))

;;;; Sessions as data

(defun drover-anvil--requests-data (session)
  "Return the requests of SESSION, as its record summarizes them, as data."
  (vconcat (mapcar (lambda (request)
                     (drover-anvil--compact
                      `((kind . ,(and (symbolp (plist-get request :kind)) (symbol-name (plist-get request :kind))))
                        (summary . ,(drover-anvil--line (plist-get request :summary) 300)))))
                   (seq-take (seq-filter #'listp (drover-session-requests session)) 5))))

(defun drover-anvil--key-of (key)
  "Return the key KEY, of a live session or not, as the tools name it."
  (if-let* ((record (drover-index-session key)))
      (drover-anvil-key-string record)
    (drover-bound-line (if (consp (cdr key))
                           (format "%s/%s/%s" (car key) (cadr key) (cddr key))
                         (format "%s/%s" (car key) (cdr key)))
                       300)))

(defun drover-anvil--session-data (session caller)
  "Return SESSION as data the tools answer with, bounded, for the session CALLER."
  (let* ((key (drover-session-key session))
         (parent (drover-index-parent key)))
    (drover-anvil--compact
     `((key . ,(drover-anvil-key-string session))
       (title . ,(drover-anvil--line (drover-session-display-title session) 160))
       (status . ,(symbol-name (drover-session-status session)))
       (since . ,(drover-anvil--time (drover-session-since session)))
       (updated . ,(drover-anvil--time (drover-session-updated session)))
       (needs_user . ,(and (drover-session-attention-p session) t))
       (summary . ,(drover-anvil--line (drover-session-summary session) 300))
       (requests . ,(and (drover-session-requests session) (drover-anvil--requests-data session)))
       (harness . ,(symbol-name (drover-session-harness session)))
       (engine . ,(and (drover-session-engine session) (symbol-name (drover-session-engine session))))
       (host . ,(drover-anvil--line (drover-session-host session) 100))
       (stale . ,(and (drover-session-stale session) t))
       (project . ,(drover-anvil--line (drover-session-project session) 100))
       (directory . ,(and (stringp (drover-session-directory session))
                          (drover-anvil--line (abbreviate-file-name (drover-session-directory session)) 300)))
       (branch . ,(drover-anvil--line (drover-session-branch session) 100))
       (agent . ,(drover-anvil--line (drover-session-agent session) 60))
       (model . ,(drover-anvil--line (drover-session-model session) 80))
       (effort . ,(drover-anvil--line (drover-session-effort session) 40))
       (provider . ,(drover-anvil--line (drover-session-provider-name session) 60))
       (parent . ,(and parent (drover-anvil--key-of parent)))
       (archived . ,(and (drover-session-archived-p session) t))
       (caller . ,(and caller (equal key caller) t))
       (started_by_caller . ,(and caller (equal (gethash key drover-anvil--started) caller) t))))))

(defun drover-anvil--recap-data (recap)
  "Return RECAP, as `drover-recap' tells it, as data."
  (if-let* ((failure (plist-get recap :failure)))
      `((recap_failure . ,(drover-anvil--line failure 300)))
    (drover-anvil--compact
     `((prompt . ,(drover-anvil--text (plist-get recap :prompt)))
       (reply . ,(drover-anvil--text (plist-get recap :reply)))
       (error . ,(drover-anvil--text (plist-get recap :error)))
       (changes . ,(and (plist-get recap :changes)
                        (vconcat (mapcar (lambda (change)
                                           (drover-anvil--compact
                                            `((file . ,(drover-anvil--line (format "%s" (car change)) 300))
                                              (additions . ,(and (natnump (nth 1 change)) (nth 1 change)))
                                              (deletions . ,(and (natnump (nth 2 change)) (nth 2 change))))))
                                         (seq-take (seq-filter #'consp (plist-get recap :changes)) 50)))))))))

(defun drover-anvil--request-detail (request)
  "Return REQUEST, as a backend's :requests tells it, as data."
  (drover-anvil--compact
   `((id . ,(drover-anvil--line (format "%s" (plist-get request :id)) 200))
     (kind . ,(and (symbolp (plist-get request :kind)) (symbol-name (plist-get request :kind))))
     (title . ,(drover-anvil--line (plist-get request :title) 300))
     (detail . ,(drover-anvil--text (plist-get request :detail) 20 2000))
     (choices . ,(and (plist-get request :choices) (vconcat (seq-take (plist-get request :choices) 3))))
     (fields . ,(and (plist-get request :fields)
                     (vconcat (mapcar (lambda (field)
                                        (drover-anvil--compact
                                         `((key . ,(drover-anvil--line (plist-get field :key) 64))
                                           (type . ,(plist-get field :type))
                                           (title . ,(drover-anvil--line (plist-get field :title) 300))
                                           (options . ,(and (plist-get field :options)
                                                            (vconcat (mapcar (lambda (option) (drover-anvil--line option 200))
                                                                             (seq-take (plist-get field :options) 50)))))
                                           (custom . ,(if (plist-get field :custom) t :json-false)))))
                                      (seq-take (plist-get request :fields) 20))))))))

;;;; Details fetched for a job

(defun drover-anvil--fetch-details (id session recap requests)
  "Fetch for the job ID the RECAP of SESSION's last turn and what its REQUESTS ask.
Each when non-nil; they arrive in the job's :recap and :requests, nil
until then."
  (when recap
    (drover-anvil--job-put id :recap-asked t)
    (drover-anvil--call id #'drover-recap session
                        (lambda (recap)
                          (drover-anvil--job-put id :recap (or recap '(:failure "no recap"))))))
  (when requests
    (drover-anvil--job-put id :requests-asked t)
    (if (not (drover-supports-p session :requests))
        (drover-anvil--job-put id :requests 'none)
      (drover-anvil--call id #'drover-backend-call session :requests
                          (lambda (requests)
                            (drover-with-reporting "Anvil requests"
                              (drover-anvil--job-put id :requests (or requests 'none))))))))

(defun drover-anvil--details-ready-p (id)
  "Return non-nil once every detail the job ID asked for arrived."
  (not (or (and (drover-anvil--job-get id :recap-asked) (null (drover-anvil--job-get id :recap)))
           (and (drover-anvil--job-get id :requests-asked) (null (drover-anvil--job-get id :requests))))))

(defun drover-anvil--details (id)
  "Return the details the job ID fetched, as data, with a note on those missing."
  (let ((recap (drover-anvil--job-get id :recap))
        (requests (drover-anvil--job-get id :requests)))
    (append
     (and recap (drover-anvil--recap-data recap))
     (pcase requests
       ((or 'nil 'none) nil)
       (`(:failure ,message . ,_) `((requests_failure . ,(drover-anvil--line message 300))))
       (_ `((pending_requests . ,(vconcat (mapcar #'drover-anvil--request-detail
                                                  (seq-take (seq-filter #'consp requests) 20)))))))
     (and (not (drover-anvil--details-ready-p id))
          '((details_note . "Some details did not arrive in time"))))))

;;;; Listing

(defun drover-anvil--split (value)
  "Return VALUE, a string of words or an array of strings, as a list of words."
  (cond
   ((vectorp value) (delq nil (mapcar (lambda (item) (and (stringp item) (string-trim item))) value)))
   ((stringp value) (split-string value "[ ,]+" t))))

(defun drover-anvil--matcher (status harness project host attention query subagents caller)
  "Return a predicate of a session matching the filters of `drover-sessions'.
STATUS, HARNESS, PROJECT, HOST, ATTENTION, QUERY and SUBAGENTS are its
parameters; CALLER the calling session's key."
  (let ((statuses (drover-anvil--split status))
        (harnesses (drover-anvil--split harness))
        (root (cond ((member project '("here" ".")) 'caller)
                    ;; Absolute as this host spells it: c:/... on Windows.
                    ((and (stringp project) (file-name-absolute-p project))
                     (drover-anvil--project-root project))))
        (query (and (stringp query) (not (string-blank-p query)) (downcase query))))
    (when (and (eq root 'caller) (null caller))
      (drover-anvil--refuse "The project \"here\" needs the caller, which its client does not name"))
    (lambda (session)
      (and (or (null statuses) (member (symbol-name (drover-session-status session)) statuses))
           (or (null harnesses) (member (symbol-name (drover-session-harness session)) harnesses))
           (cond
            ((eq root 'caller) (and (null (drover-session-host session))
                                    (drover-anvil--in-project-p caller (drover-session-directory session))))
            (root (and (null (drover-session-host session))
                       (equal (drover-anvil--project-root (drover-session-directory session)) root)))
            ((and (stringp project) (not (string-empty-p project)))
             (equal (downcase (or (drover-session-project session) "")) (downcase project)))
            (t t))
           (pcase host
             ((or 'nil "") t)
             ((or "local" "here") (null (drover-session-host session)))
             (_ (and (drover-session-host session)
                     (or (equal (drover-session-host session) host)
                         (equal (drover-host-label (drover-session-host session)) host)))))
           (or (not (drover-anvil--true-p attention)) (drover-session-attention-p session))
           (or (not (drover-anvil--false-p attention)) (not (drover-session-attention-p session)))
           (or (drover-anvil--true-p subagents)
               (null (drover-index-parent (drover-session-key session)))
               (equal (drover-session-key session) caller))
           (or (null query)
               (string-search query (downcase (or (drover-session-display-title session) ""))))))))

(defun drover-anvil--tool-sessions (&optional status harness project host attention query subagents limit)
  "List the live sessions drover knows, the most urgent first.
STATUS, HARNESS, PROJECT, HOST, ATTENTION, QUERY and SUBAGENTS filter
them, LIMIT bounds them, as the parameters below tell.

MCP Parameters:
  status - Statuses to keep, comma-separated, among waiting, failed,
      done, retrying, working and idle
  harness - Harnesses to keep, comma-separated, such as opencode,
      claude-code-ide, agent-shell, claude-code or codex
  project - A project's name, \"here\" for the caller's project, or a
      directory, whose repository's sessions to keep
  host - \"local\" for this host's sessions, or a host's name
  attention - (boolean) true for the sessions needing the user alone,
      false for the others
  query - Text their titles hold
  subagents - (boolean) true to list subagents too
  limit - (integer) Sessions listed at most, 30 by default"
  (drover-start)
  (let ((caller (drover-anvil--caller))
        (drover-anvil--roots (make-hash-table :test #'equal)))
    (unless (drover-anvil--scope 'list)
      (drover-anvil--log "drover-sessions" caller nil "refused" "list allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no list"))
    (let* ((match (drover-anvil--matcher status harness project host attention query subagents caller))
           (limit (max 1 (min drover-anvil-list-limit (if (natnump limit) limit 30))))
           (sessions (drover-sort (seq-filter (lambda (session)
                                                (and (null (drover-anvil--refusal 'list caller session))
                                                     (funcall match session)))
                                              (drover-index-sessions)))))
      (drover-anvil--log "drover-sessions" caller nil "done" (format "%d listed" (length sessions)))
      (drover-anvil--json
       `((caller . ,(and caller (drover-anvil--key-of caller)))
         (count . ,(length sessions))
         (sessions . ,(vconcat (mapcar (lambda (session) (drover-anvil--session-data session caller))
                                       (seq-take sessions limit))))
         (truncated . ,(and (> (length sessions) limit) t)))))))

;;;; Reading

(defun drover-anvil--now-data (key caller)
  "Return the session KEY as it stands now, as data for CALLER.
A session no longer live is told gone."
  (if-let* ((record (drover-index-session key)))
      (drover-anvil--session-data record caller)
    `((key . ,(drover-anvil--key-of key)) (gone . t))))

(defun drover-anvil--tool-session (key &optional recap)
  "Read the session KEY: what it is, where it stands, its last turn.
Unless RECAP is false, the recap of its last turn comes too, the
user's last prompt and the agent's reply, and what its pending
requests ask.

MCP Parameters:
  key - The session's key, as drover-sessions tells it
  recap - (boolean) false to leave out the last turn and the requests'
      details, true by default"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session))
         (recap (not (drover-anvil--false-p recap))))
    (drover-anvil--check "drover-session" 'read caller session)
    (let ((id (drover-anvil--job
               "drover-session" caller key :seconds 20
               :poll (lambda (id)
                       (when (drover-anvil--details-ready-p id)
                         (drover-anvil--done id (append (drover-anvil--now-data key caller)
                                                        (drover-anvil--details id)))))
               :timeout (lambda (id)
                          (append (drover-anvil--now-data key caller) (drover-anvil--details id))))))
      (if recap
          (drover-anvil--fetch-details id session t (and (drover-session-requests session) t))
        (drover-anvil--done id (drover-anvil--now-data key caller)))
      (drover-anvil--answer id))))

;;;; Waiting

(defvar drover-anvil--waits (make-hash-table :test #'equal)
  "The waits of the tools, by caller.
Each caller's key maps to the identifiers of the jobs of its waits.")

(defvar drover-anvil--sent (make-hash-table :test #'equal)
  "When a prompt last went to each session through the tools, by key.")

(defun drover-anvil--waited-by (caller)
  "Return the keys of the sessions the session CALLER waits for through the tools."
  (delq nil (mapcar (lambda (id) (drover-anvil--job-get id :key))
                    (gethash caller drover-anvil--waits))))

(defun drover-anvil--waits-for-p (from to)
  "Return non-nil when the session FROM waits for the session TO.
Through these tools, directly or through other sessions waiting."
  (let ((seen (make-hash-table :test #'equal))
        (stack (list from))
        (found nil))
    (while (and stack (not found))
      (let ((key (pop stack)))
        (unless (gethash key seen)
          (puthash key t seen)
          (dolist (next (drover-anvil--waited-by key))
            (if (equal next to)
                (setq found t)
              (push next stack))))))
    found))

(defun drover-anvil--turn-active-p (session)
  "Return non-nil while SESSION runs a turn, or has prompts waiting to run."
  (or (memq (drover-session-status session) '(working retrying))
      (> (drover-pending-prompts session) 0)))

(defun drover-anvil--outcome (session)
  "Return how the turn of SESSION, ended or waiting for the user, came out, a word."
  (pcase (drover-session-status session)
    ('waiting "needs_user")
    ('failed "failed")
    (_ "ended")))

(defun drover-anvil--wait-poll (id key since caller recap)
  "Poll the wait of the job ID on the session KEY, for CALLER.
SINCE is the time after which the turn awaited ends, or nil; RECAP non-nil
to fetch the last turn's recap once it ended."
  (let ((record (drover-index-session key)))
    (cond
     ((null record)
      (drover-anvil--done id `((outcome . "gone") (key . ,(drover-anvil--key-of key))
                               (note . "The session is no longer live: archived, deleted, or its host gone"))))
     ((drover-anvil--turn-active-p record)
      (drover-anvil--job-put id :active t))
     ((drover-anvil--job-get id :settling)
      (when (or (drover-anvil--details-ready-p id)
                (> (float-time) (drover-anvil--job-get id :settling)))
        (drover-anvil--done id (append `((outcome . ,(drover-anvil--outcome record))
                                         (waited . ,(round (- (float-time) (drover-anvil--job-get id :born)))))
                                       (drover-anvil--session-data record caller)
                                       (drover-anvil--details id)))))
     ((or (drover-anvil--job-get id :active)
          (null since)
          (>= (max (or (drover-session-since record) 0) (or (drover-session-updated record) 0)) since))
      ;; Its turn ended: the details follow, for a few seconds at most.
      (drover-anvil--job-put id :settling (+ (float-time) 15))
      (drover-anvil--fetch-details id record recap
                                   (and recap (eq (drover-session-status record) 'waiting)))
      (drover-anvil--wait-poll id key since caller recap)))))

(defun drover-anvil--tool-wait (key &optional timeout since recap)
  "Wait until the turn of the session KEY ends, or it needs the user.
TIMEOUT bounds the wait, SINCE tells which turn, RECAP whether its
recap comes, as the parameters below tell.  Emacs never waits: the
client does, while Anvil polls.

MCP Parameters:
  key - The session's key, as drover-sessions tells it
  timeout - (integer) Seconds to wait at most, 600 by default
  since - The time after which the turn awaited ends, seconds since
      the epoch or ISO 8601; by default, when the last prompt sent
      through these tools went, else any turn not running counts
  recap - (boolean) false to leave out the turn's recap, true by default"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session))
         (since (or (drover-anvil--read-time since) (gethash key drover-anvil--sent)))
         (seconds (max 1 (min drover-anvil-wait-limit
                              (if (natnump timeout) timeout drover-anvil-wait-timeout))))
         (recap (not (drover-anvil--false-p recap))))
    (drover-anvil--check "drover-session-wait" 'wait caller session)
    (when (and caller (drover-anvil--waits-for-p key caller))
      (drover-anvil--log "drover-session-wait" caller key "refused" "it waits for the caller")
      (drover-anvil--refuse "%s waits for the caller: waiting for it would wait for itself"
                            (drover-anvil-key-string session)))
    (drover-anvil--log "drover-session-wait" caller key "waiting" (format "%ds" seconds))
    (let ((id (drover-anvil--job
               "drover-session-wait" caller key :seconds seconds
               :poll (lambda (id) (drover-anvil--wait-poll id key since caller recap))
               :timeout (lambda (_id)
                          (append `((outcome . "timeout") (waited . ,seconds))
                                  (drover-anvil--now-data key caller)))
               :finally (lambda (id)
                          (when caller
                            (let ((left (delete id (gethash caller drover-anvil--waits))))
                              (if left
                                  (puthash caller left drover-anvil--waits)
                                (remhash caller drover-anvil--waits))))))))
      (when caller
        (push id (gethash caller drover-anvil--waits)))
      (drover-anvil--answer id))))

;;;; Prompting

(defun drover-anvil--prompt (prompt)
  "Return PROMPT, a prompt received, or refuse it."
  (cond
   ((not (and (stringp prompt) (not (string-blank-p prompt))))
    (drover-anvil--refuse "The prompt is empty"))
   ((> (length prompt) drover-anvil-prompt-limit)
    (drover-anvil--refuse "The prompt is longer than %d characters" drover-anvil-prompt-limit))
   (t (substring-no-properties prompt))))

(defun drover-anvil--prompt-note (prompt)
  "Return a note on PROMPT for the log: its length and beginning."
  (format "%d characters: %s" (length prompt) (drover-bound-line prompt 80)))

(defun drover-anvil--tool-send (key prompt)
  "Send PROMPT to the session KEY, as its front end delivers prompts.
OpenCode queues it or steers the running turn with it, as it delivers
prompts sent while a turn runs; a terminal or a shell takes it only
with no draft typed and no turn running, and the tool says why not,
but another host's queues it until it can, which the outcome
\"queued\" tells, with why.

MCP Parameters:
  key - The session's key, as drover-sessions tells it
  prompt - The prompt, Markdown"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session))
         (prompt (drover-anvil--prompt prompt)))
    (drover-anvil--check "drover-session-send" 'send caller session)
    (unless (drover-supports-p session :submit)
      (drover-anvil--refuse "%s takes no prompt from an agent" (drover-anvil-key-string session)))
    (when-let* (((drover-session-buffer session))
                (blocker (drover-backend-property session :blocker))
                (why (drover-with-reporting "Anvil prompt"
                       (funcall blocker session))))
      (drover-anvil--log "drover-session-send" caller key "refused" why)
      (drover-anvil--refuse "%s cannot take a prompt now: %s" (drover-anvil-key-string session) why))
    (drover-anvil--log-or-refuse "drover-session-send" caller key (drover-anvil--prompt-note prompt))
    (let ((id (drover-anvil--job "drover-session-send" caller key :seconds 60)))
      (condition-case err
          (drover-quietly
            (drover-unarchive-to-send session))
        (error (drover-anvil--fail id (drover-error-message err))))
      (when (eq (drover-anvil--job-get id :state) 'pending)
        (drover-anvil--call
         id #'drover-backend-call session :submit prompt
         (lambda (&optional queued &rest _)
           (drover-with-reporting "Anvil prompt"
             (let ((now (float-time)))
               (puthash key now drover-anvil--sent)
               (drover-anvil--done id `((outcome . ,(if (stringp queued) "queued" "sent"))
                                        (key . ,(drover-anvil--key-of key))
                                        (sent_at . ,now)
                                        (note . ,(if (stringp queued)
                                                     (format "Its host queued it: %s" queued)
                                                   "Wait for its turn with drover-session-wait")))))))
         (drover-anvil--else id)))
      (drover-anvil--answer id))))

;;;; Starting

(defun drover-anvil--find-model (name directory)
  "Return the model NAME names among those offered in DIRECTORY, or refuse.
NAME is PROVIDER/ID, an identifier or a display name."
  (let* ((models (drover-models directory))
         (wanted (downcase name)))
    (or (seq-find (lambda (model)
                    (or (and (drover-model-provider-id model)
                             (equal (downcase (concat (drover-model-provider-id model) "/" (drover-model-id model)))
                                    wanted))
                        (equal (downcase (drover-model-id model)) wanted)
                        (equal (downcase (or (drover-model-name model) "")) wanted)))
                  models)
        (drover-anvil--refuse "No model %s is offered in %s" (drover-bound-line name 100)
                              (abbreviate-file-name directory)))))

(defun drover-anvil--find-variant (effort model)
  "Return the variant EFFORT names, a symbol, for MODEL, or refuse."
  (cond
   ((null effort) nil)
   ((and model (drover-model-variants-known model))
    (or (car (seq-find (lambda (variant)
                         (or (equal (symbol-name (car variant)) effort)
                             (equal (plist-get (cdr variant) :effort) effort)))
                       (drover-model-variants model)))
        (drover-anvil--refuse "%s runs no effort %s" (drover-model-label model) (drover-bound-line effort 40))))
   ((string-match-p "\\`[a-z0-9-]\\{1,20\\}\\'" effort) (intern effort))
   (t (drover-anvil--refuse "No effort %s" (drover-bound-line effort 40)))))

(defun drover-anvil--create-refusal (caller directory)
  "Return why the session CALLER may not start a session in DIRECTORY, or nil."
  (pcase (drover-anvil--scope 'create)
    ('nil "drover-anvil-permissions allows no create")
    ('any nil)
    (_ (cond
        ((null caller) "the caller is unknown, and drover-anvil-permissions allows create only in its project")
        ((not (drover-anvil--in-project-p caller directory))
         "drover-anvil-permissions allows create only in the caller's project")))))

(defun drover-anvil--tool-create (&optional harness directory prompt model effort agent title engine)
  "Start a session, without showing it, and send it PROMPT; return its key.
HARNESS, DIRECTORY, MODEL, EFFORT, AGENT, TITLE and ENGINE tell what
starts where, as the parameters below tell.

MCP Parameters:
  harness - The harness starting it, by default the first the user
      allows agents, opencode
  directory - Its directory, the caller's by default
  prompt - Its first prompt, Markdown; none to start it idle
  model - Its model, PROVIDER/ID or ID, the harness's default otherwise
  effort - Its reasoning effort, a variant of the model, such as high
  agent - Its agent, such as build or plan
  title - Its title
  engine - The engine it runs, for a harness running several"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (record (and caller (drover-index-session caller)))
         (directory (cond ((and (stringp directory) (not (string-blank-p directory)))
                           (expand-file-name directory))
                          ((and record (stringp (drover-session-directory record))
                                (null (drover-session-host record)))
                           (drover-session-directory record))
                          (t (drover-anvil--refuse "Name the directory: the caller's is unknown"))))
         (backend (let ((name (if (and (stringp harness) (not (string-blank-p harness)))
                                  harness
                                (symbol-name (car drover-anvil-create-harnesses)))))
                    (or (seq-find (lambda (backend) (equal (symbol-name backend) name)) drover-anvil-create-harnesses)
                        (drover-anvil--refuse "Agents may start sessions of %s only (drover-anvil-create-harnesses)"
                                              (mapconcat #'symbol-name drover-anvil-create-harnesses ", ")))))
         (engines (drover-backend-engines backend))
         (engine (if (and (stringp engine) (not (string-blank-p engine)))
                     (or (seq-find (lambda (candidate) (equal (symbol-name candidate) engine)) engines)
                         (drover-anvil--refuse "%s runs no engine %s" backend (drover-bound-line engine 40)))
                   (car engines)))
         (prompt (and prompt (not (equal prompt "")) (drover-anvil--prompt prompt))))
    (when-let* ((why (or (and (not (file-directory-p directory))
                              (format "%s is not a directory" (abbreviate-file-name directory)))
                         (and (not (and (memq backend (drover-backends))
                                        (functionp (drover-backend-property backend :create))
                                        engine))
                              (format "%s starts no session here" backend))
                         (drover-anvil--create-refusal caller directory))))
      (drover-anvil--log "drover-session-create" caller nil "refused" why)
      (drover-anvil--refuse "%s" why))
    (let* ((model (and (stringp model) (not (string-blank-p model)) (drover-anvil--find-model model directory)))
           (_ (when (and model (not (drover-model-runs-on-p model engine)))
                (drover-anvil--refuse "%s does not run %s" (drover-engine-label engine) (drover-model-label model))))
           (variant (drover-anvil--find-variant (and (stringp effort) (not (string-blank-p effort)) effort) model))
           (options (list :agent (and (stringp agent) (not (string-blank-p agent)) (drover-bound-line agent 60))
                          :model model :variant variant
                          :title (and (stringp title) (not (string-blank-p title)) (drover-bound-line title 160))
                          :hidden t)))
      (drover-anvil--log-or-refuse "drover-session-create" caller nil
                                   (format "%s in %s%s" backend (abbreviate-file-name directory)
                                           (if prompt (concat ", " (drover-anvil--prompt-note prompt)) "")))
      (let ((id (drover-anvil--job "drover-session-create" caller nil :seconds 90)))
        (drover-anvil--call
         id (drover-backend-property backend :create) directory engine options
         (lambda (session)
           (drover-with-reporting "Anvil start"
             (let ((key (drover-session-key session)))
               (drover-anvil--job-put id :key key)
               (when caller
                 (puthash key caller drover-anvil--started))
               (when prompt
                 (drover-quietly
                   (drover-backend-call session :send prompt nil))
                 (puthash key (float-time) drover-anvil--sent))
               (drover-anvil--done id `((outcome . "started")
                                        (key . ,(drover-anvil-key-string session))
                                        (directory . ,(abbreviate-file-name directory))
                                        (sent . ,(if prompt t :json-false))
                                        (note . ,(if prompt
                                                     "Wait for its first turn with drover-session-wait"
                                                   "Send it a prompt with drover-session-send")))))))
         (drover-anvil--else id))
        (drover-anvil--answer id)))))

;;;; Answering

(defun drover-anvil--answer-value (value)
  "Return VALUE, a field's answer received, as a backend's :answer takes it."
  (cond
   ((stringp value) (drover-bound-text value 20 4096))
   ((vectorp value) (and (seq-every-p #'stringp value)
                         (vconcat (mapcar (lambda (text) (drover-bound-text text 20 4096)) (seq-take value 50)))))
   ((numberp value) value)
   ((eq value t) t)
   ((eq value :json-false) :false)))

(defun drover-anvil--answer-of (request decision answers dismiss)
  "Return the answer to REQUEST that DECISION, ANSWERS and DISMISS tell.
A string tells why they answer nothing."
  (cond
   ((drover-anvil--true-p dismiss) (list :dismiss t))
   ((eq (plist-get request :kind) 'permission)
    (if (member decision (or (plist-get request :choices) '("once" "always" "reject")))
        (list :decision decision)
      "a permission is answered with a decision: once, always or reject"))
   ((not (and (listp answers) answers))
    "a question is answered with answers, an object of its fields' keys")
   (t (let ((pairs (delq nil (mapcar (lambda (entry)
                                       (when-let* ((key (car-safe entry))
                                                   (value (drover-anvil--answer-value (cdr entry))))
                                         (cons (drover-bound-line (format "%s" key) 64) value)))
                                     (seq-take answers 20)))))
        (if pairs (list :answer pairs) "no answer fits the question's fields")))))

(defun drover-anvil--answer-requests (id key caller requests request decision answers dismiss)
  "Answer, for the job ID, one of REQUESTS of the session KEY, as CALLER asks.
REQUEST names it, else the oldest; DECISION, ANSWERS and DISMISS tell
the answer."
  (let* ((record (drover-index-session key))
         (chosen (if (stringp request)
                     (seq-find (lambda (candidate) (equal (format "%s" (plist-get candidate :id)) request)) requests)
                   (car requests)))
         (action (if (eq (plist-get chosen :kind) 'permission) 'answer-permission 'answer-question))
         (answer (and chosen (drover-anvil--answer-of chosen decision answers dismiss))))
    (cond
     ((plist-get requests :failure) (drover-anvil--fail id (plist-get requests :failure)))
     ((null record) (drover-anvil--fail id "the session is no longer live"))
     ((null chosen) (drover-anvil--fail id (if request "no such request pending" "no request pending")))
     ((drover-anvil--refusal action caller record)
      (drover-anvil--fail id (drover-anvil--refusal action caller record)))
     ((stringp answer) (drover-anvil--fail id answer))
     (t (drover-anvil--call
         id #'drover-backend-call record :answer (plist-get chosen :id) answer
         (lambda (&rest _)
           (drover-with-reporting "Anvil answer"
             (drover-anvil--done id `((outcome . "answered") (key . ,(drover-anvil--key-of key))
                                      (request . ,(drover-anvil--line (format "%s" (plist-get chosen :id)) 200))
                                      (kind . ,(symbol-name (plist-get chosen :kind)))))))
         (drover-anvil--else id))))))

(defun drover-anvil--tool-answer (key &optional request decision answers dismiss)
  "Answer a pending request of the session KEY: a permission or a question.
REQUEST names it, the oldest by default; DECISION answers a permission,
ANSWERS a question, DISMISS dismisses it.  drover-session tells what
pending requests ask, their identifiers, choices and fields.

MCP Parameters:
  key - The session's key, as drover-sessions tells it
  request - The request's identifier, the oldest pending by default
  decision - For a permission: once, always or reject
  answers - (object) For a question: each field's key to its answer, a
      string, an array of strings for a multiselect, a number or a
      boolean
  dismiss - (boolean) true to dismiss the request instead"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session)))
    (unless (and (drover-supports-p session :requests) (drover-supports-p session :answer))
      (drover-anvil--refuse "%s's requests are answered in its own buffer" (drover-anvil-key-string session)))
    (unless (seq-some (lambda (action) (null (drover-anvil--refusal action caller session)))
                      '(answer-question answer-permission))
      (drover-anvil--check "drover-session-answer" 'answer-question caller session))
    (drover-anvil--log-or-refuse "drover-session-answer" caller key
                                 (format "%s %s" (or request "oldest")
                                         (cond ((drover-anvil--true-p dismiss) "dismiss")
                                               ((stringp decision) (drover-bound-line decision 20))
                                               (t "answers"))))
    (let ((id (drover-anvil--job "drover-session-answer" caller key :seconds 30)))
      (drover-anvil--call
       id #'drover-backend-call session :requests
       (lambda (requests)
         (drover-with-reporting "Anvil answer"
           (drover-quietly
             (drover-anvil--answer-requests id key caller requests request decision answers dismiss)))))
      (drover-anvil--answer id))))

;;;; Interrupting and archiving

(defun drover-anvil--tool-interrupt (key)
  "Stop the running turn of the session KEY.

MCP Parameters:
  key - The session's key, as drover-sessions tells it"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session)))
    (drover-anvil--check "drover-session-interrupt" 'interrupt caller session)
    (if (not (drover-session-active-p session))
        (drover-anvil--json `((outcome . "idle") (key . ,(drover-anvil-key-string session))
                              (note . "No turn runs")))
      (unless (drover-supports-p session :interrupt)
        (drover-anvil--refuse "%s cannot be interrupted from here" (drover-anvil-key-string session)))
      (drover-anvil--log-or-refuse "drover-session-interrupt" caller key nil)
      (let ((id (drover-anvil--job "drover-session-interrupt" caller key :seconds 30)))
        (drover-anvil--call id #'drover--call-confirming session :interrupt
                            (lambda ()
                              (drover-with-reporting "Anvil interrupt"
                                (drover-anvil--done id `((outcome . "interrupted")
                                                         (key . ,(drover-anvil--key-of key))))))
                            (drover-anvil--else id))
        (drover-anvil--answer id)))))

(defun drover-anvil--tool-archive (key)
  "Archive the session KEY, and close its buffer, its conversation kept.

MCP Parameters:
  key - The session's key, as drover-sessions tells it"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session)))
    (drover-anvil--check "drover-session-archive" 'archive caller session)
    (unless (drover-archivable-p session)
      (drover-anvil--refuse "%s cannot be archived" (drover-anvil-key-string session)))
    (drover-anvil--log-or-refuse "drover-session-archive" caller key nil)
    (let ((id (drover-anvil--job "drover-session-archive" caller key :seconds 30)))
      (drover-anvil--call id #'drover--archive-step session
                          (lambda (ok &optional value)
                            (drover-with-reporting "Anvil archive"
                              (if (not ok)
                                  (drover-anvil--fail id value)
                                (drover-quietly
                                  (drover--archive-finish session value))
                                (drover-anvil--done id `((outcome . "archived")
                                                         (key . ,(drover-anvil--key-of key))))))))
      (drover-anvil--answer id))))

;;;; Prompts

(declare-function drover-prompts-gather "drover-prompts" (since until callback &optional seconds))
(defvar drover-prompts-text-limit)
(defvar drover-prompts-limit)
(defvar drover-prompts-timeout)

(defun drover-anvil--prompt-allowed-p (scope caller prompt)
  "Return non-nil when SCOPE lets the session CALLER read PROMPT.
SCOPE is that of `read' in `drover-anvil-permissions'."
  (pcase scope
    ('any t)
    ('project (and (null (plist-get prompt :host))
                   (drover-anvil--in-project-p caller (plist-get prompt :directory))))
    ('own (and (null (plist-get prompt :host))
               (eq (plist-get prompt :engine) 'opencode)
               (drover-anvil--own-p caller (cons 'opencode (plist-get prompt :id)))))))

(defun drover-anvil--host-match-p (host wanted)
  "Return non-nil when HOST, a name or nil for this one, is the host WANTED names.
WANTED is \"local\" or \"here\" for this host, a host's name or label,
or nil or empty for any."
  (pcase wanted
    ((or 'nil "") t)
    ((or "local" "here") (null host))
    (_ (let ((name (or host drover-host)))
         (or (string-equal-ignore-case name wanted)
             (string-equal-ignore-case (drover-host-label host) wanted))))))

(defun drover-anvil--prompt-data (prompt text-limit)
  "Return PROMPT, as `drover-prompts-gather' tells it, as data.
Its text is cut to TEXT-LIMIT characters."
  (let* ((host (or (plist-get prompt :host) drover-host))
         (engine (symbol-name (plist-get prompt :engine)))
         (text (plist-get prompt :text))
         (cut (or (plist-get prompt :cut) (and (length> text text-limit) (length text)))))
    (drover-anvil--compact
     `((host . ,(drover-anvil--line host 100))
       (engine . ,engine)
       (session . ,(plist-get prompt :id))
       (link . ,(format "agent-session://%s/%s/%s" host engine (plist-get prompt :id)))
       (title . ,(drover-anvil--line (plist-get prompt :title) 160))
       (directory . ,(and (plist-get prompt :directory)
                          (drover-anvil--line (abbreviate-file-name (plist-get prompt :directory)) 300)))
       (time . ,(drover-anvil--time (plist-get prompt :time)))
       (text . ,(if (length> text text-limit) (concat (substring text 0 text-limit) "…") text))
       (cut . ,cut)))))

(defun drover-anvil--hosts-data (hosts)
  "Return HOSTS, as `drover-prompts-gather' tells how each answered, as data."
  (vconcat (mapcar (lambda (entry)
                     (drover-anvil--compact
                      `((host . ,(drover-anvil--line (or (plist-get entry :host) drover-host) 100))
                        (label . ,(drover-anvil--line (plist-get entry :label) 100))
                        (status . ,(symbol-name (plist-get entry :status)))
                        (count . ,(plist-get entry :count))
                        (cut . ,(and (> (or (plist-get entry :cut) 0) 0) (plist-get entry :cut)))
                        (message . ,(drover-anvil--line (plist-get entry :message) 300))
                        (failures . ,(and (plist-get entry :failures)
                                          (vconcat (mapcar (lambda (failure)
                                                             `((engine . ,(symbol-name (car failure)))
                                                               (message . ,(drover-anvil--line (cdr failure) 300))))
                                                           (seq-take (plist-get entry :failures) 20))))))))
                   hosts)))

(defun drover-anvil--tool-prompts (&optional since until host offset limit text_limit)
  "Return the prompts the user sent agents between SINCE and UNTIL, of every host.
HOST, OFFSET, LIMIT and TEXT_LIMIT narrow the answer, as the parameters
below tell.  Each host reads its own engines; the prompts of subagents,
of programs and what the engines add as the user are left out, and so
are those the user's configuration leaves out.

MCP Parameters:
  since - Start of the period, seconds since the epoch or ISO 8601;
      seven days before its end by default
  until - End of the period, seconds since the epoch or ISO 8601; now
      by default
  host - \"local\" for this host's prompts, or a host's name; every
      host's by default
  offset - (integer) Prompts to skip, oldest first, 0 by default
  limit - (integer) Prompts answered at most, 1000 by default
  text_limit - (integer) Characters of each prompt at most, its whole
      length told past them"
  (drover-start)
  (require 'drover-prompts)
  (let* ((caller (drover-anvil--caller))
         (scope (drover-anvil--scope 'read))
         (until (float (or (drover-anvil--read-time until) (float-time))))
         (since (float (or (drover-anvil--read-time since) (- until (* 7 86400)))))
         (offset (if (natnump offset) offset 0))
         (limit (max 1 (min drover-prompts-limit (if (natnump limit) limit 1000))))
         (text-limit (max 20 (min drover-prompts-text-limit
                                  (if (natnump text_limit) text_limit drover-prompts-text-limit))))
         (stop nil))
    (unless scope
      (drover-anvil--log "drover-prompts" caller nil "refused" "read allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no read"))
    (unless (< since until)
      (drover-anvil--refuse "The period ends before it starts"))
    (let ((id (drover-anvil--job
               "drover-prompts" caller nil :seconds (+ drover-prompts-timeout 30)
               :finally (lambda (_id) (when stop (funcall stop))))))
      (condition-case err
          (setq stop
                (drover-prompts-gather
                 since until
                 (lambda (result)
                   (let* ((prompts (seq-filter (lambda (prompt)
                                                 (and (drover-anvil--host-match-p (plist-get prompt :host) host)
                                                      (drover-anvil--prompt-allowed-p scope caller prompt)))
                                               (plist-get result :prompts)))
                          (page (seq-take (nthcdr offset prompts) limit)))
                     (drover-anvil--done
                      id `((outcome . ,(format "%d prompts" (length prompts)))
                           (since . ,(drover-anvil--time since))
                           (until . ,(drover-anvil--time until))
                           (count . ,(length prompts))
                           (offset . ,offset)
                           (truncated . ,(and (> (length prompts) (+ offset limit)) t))
                           (hosts . ,(drover-anvil--hosts-data
                                      (seq-filter (lambda (entry)
                                                    (drover-anvil--host-match-p (plist-get entry :host) host))
                                                  (plist-get result :hosts))))
                           (prompts . ,(vconcat (mapcar (lambda (prompt) (drover-anvil--prompt-data prompt text-limit))
                                                        page)))))))))
        (error (drover-anvil--fail id (drover-error-message err))))
      (drover-anvil--answer id))))

;;;; Following up

(declare-function drover-schedule-triggers "drover-schedule" (&optional all))
(declare-function drover-schedule--trigger-get "drover-schedule" (trigger property))
(declare-function drover-schedule-session-target "drover-schedule" (session))
(declare-function drover-schedule-add-after-trigger "drover-schedule"
                  (target trigger prompt &optional instruction argument))

(defun drover-anvil--follow-up-events ()
  "Return the events a follow-up may wait for, as (NAME . LABEL), in order.
They are those of `drover-schedule-triggers' that do not repeat: a watch
would prompt the caller on each, and one taking an argument only the
user can tell."
  (seq-remove (lambda (entry) (drover-schedule--trigger-get (car entry) :repeat))
              (drover-schedule-triggers)))

(defun drover-anvil--tool-after (event prompt)
  "Have drover send PROMPT to the calling session after the next EVENT.
EVENT names an event drover knows, such as a restart of the service
the caller's tools come from: the follow-up waits for it as a prompt
the user scheduled does, asking for it, and the user sees it among the
scheduled prompts.  For a step that needs the event, so that the
session carries on once it happened.

MCP Parameters:
  event - The event, by name, such as opencode-restart or emacs-restart;
      a name drover does not know is refused with those it knows
  prompt - The prompt the session gets then, Markdown, such as which
      step to retry"
  (drover-start)
  (require 'drover-schedule)
  (let* ((caller (drover-anvil--caller))
         (record (and caller (drover-index-session caller)))
         (events (drover-anvil--follow-up-events))
         (entry (seq-find (lambda (entry) (equal (symbol-name (car entry)) event)) events))
         (prompt (drover-anvil--prompt prompt)))
    (cond
     ((not (eq (drover-anvil--scope 'follow-up) 'self))
      (drover-anvil--log "drover-session-after" caller caller "refused" "follow-up allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no follow-up"))
     ((null record)
      (drover-anvil--log "drover-session-after" caller nil "refused" "unknown caller")
      (drover-anvil--refuse "The caller is unknown: its client names no session drover knows, so none can be followed up"))
     ((null entry)
      (drover-anvil--refuse "No event %s; drover knows %s" (drover-bound-line (format "%s" event) 60)
                            (if events
                                (mapconcat (lambda (entry) (format "%s (%s)" (car entry) (cdr entry))) events ", ")
                              "none"))))
    (let ((target (condition-case err
                      (drover-schedule-session-target record)
                    (user-error
                     (drover-anvil--log "drover-session-after" caller caller "refused" (error-message-string err))
                     (drover-anvil--refuse "%s" (error-message-string err))))))
      (drover-anvil--log-or-refuse "drover-session-after" caller caller
                                   (format "after %s, %s" (car entry) (drover-anvil--prompt-note prompt)))
      (let ((job (drover-quietly
                   (drover-schedule-add-after-trigger target (car entry) prompt
                                                      (concat "after the next " (cdr entry))))))
        (drover-anvil--json
         `((outcome . "scheduled")
           (event . ,(symbol-name (car entry)))
           (key . ,(drover-anvil-key-string record))
           (id . ,(plist-get job :id))
           (note . ,(format "This session gets the prompt after the next %s; the user may cancel it among the scheduled prompts"
                            (cdr entry)))))))))

;;;; Scheduling

(declare-function drover-schedule-when "drover-schedule" (text &optional now))
(declare-function drover-schedule-for-session "drover-schedule"
                  (session text prompt &optional properties argument))
(declare-function drover-schedule-describe "drover-schedule" (when now &optional zone))
(declare-function drover-schedule-trigger-named "drover-schedule" (name))
(declare-function drover-schedule-fire-event "drover-schedule" (trigger &optional cancelled text argument))
(declare-function drover-schedule-session-named "drover-schedule" (name))
(declare-function drover-schedule-load "drover-schedule" ())
(declare-function drover-schedule--jobs "drover-schedule" ())
(declare-function drover-schedule--when "drover-schedule" (job))
(declare-function drover-schedule--status-text "drover-schedule" (job))

(defconst drover-anvil--iso-time-regexp
  "\\`\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T\\|[0-9]\\{9,\\}\\(?:\\.[0-9]+\\)?\\'\\)"
  "Regexp of a time as ISO 8601 or seconds since the epoch, as agents tell times.")

(defun drover-anvil--when (when)
  "Return WHEN, as an agent tells it, as `drover-schedule-for-session' takes it.
The text the user's scheduling reads, or an ISO 8601 time or seconds
since the epoch, as a time.  Refuse what reads as neither."
  (unless (and (stringp when) (not (string-blank-p when)) (length< when 200))
    (drover-anvil--refuse "Tell when, such as in 90m, tomorrow 7:00, every day 7:00 or an ISO 8601 time"))
  (if-let* (((string-match-p drover-anvil--iso-time-regexp (string-trim when)))
            (time (drover-anvil--read-time (string-trim when))))
      (seconds-to-time time)
    (condition-case err
        (progn (drover-schedule-when when) when)
      (user-error (drover-anvil--refuse "%s" (error-message-string err))))))

(defun drover-anvil--by (caller)
  "Return how a prompt the session CALLER schedules names who scheduled it."
  (if caller (concat "agent " (drover-anvil--key-of caller)) "agent"))

(defun drover-anvil--tool-schedule (key prompt when &optional argument)
  "Have drover send PROMPT to the session KEY later.
Once, recurring, or after an event: WHEN tells when, as the user's own
scheduling reads it; ARGUMENT is what an event taking one concerns.
The prompt joins the user's scheduled prompts, who sees and may cancel
it, survives restarts, and is delivered out of every window.  For a
session of this host; the caller may name its own.

MCP Parameters:
  key - The session's key, as drover-sessions tells it
  prompt - The prompt it gets then, Markdown
  when - When it goes: \"in 90m\", \"tomorrow 7:00\", \"monday 5pm\",
      \"every day 7:00\", \"every weekday 9am Europe/Paris\", \"every
      mon,wed,fri 18:00\", \"every 30m\", \"when turn ends\", \"after
      LABEL\" of an event drover knows, or an ISO 8601 time
  argument - What the event concerns, for an event that takes one"
  (drover-start)
  (require 'drover-schedule)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--session key))
         (key (drover-session-key session))
         (prompt (drover-anvil--prompt prompt)))
    (drover-anvil--check "drover-schedule" 'schedule caller session)
    (let ((when (drover-anvil--when when))
          (argument (and (stringp argument) (not (string-blank-p argument)) (drover-bound-line argument 2000))))
      (drover-anvil--log-or-refuse "drover-schedule" caller key
                                   (format "%s, %s" (if (stringp when) (drover-bound-line when 80)
                                                      (drover-anvil--time (float-time when)))
                                           (drover-anvil--prompt-note prompt)))
      (let ((job (condition-case err
                     (drover-quietly
                       (drover-schedule-for-session session when prompt
                                                    (list :hidden t :by (drover-anvil--by caller))
                                                    argument))
                   (error
                    (drover-anvil--log "drover-schedule" caller key "refused" (drover-error-message err))
                    (drover-anvil--refuse "%s" (drover-error-message err))))))
        (drover-anvil--log "drover-schedule" caller key "done" (plist-get job :id))
        (drover-anvil--json
         `((outcome . "scheduled")
           (id . ,(plist-get job :id))
           (key . ,(drover-anvil--key-of key))
           (when . ,(drover-anvil--line (drover-schedule--when job) 200))
           (next . ,(drover-anvil--time (plist-get job :due)))
           (note . "The user sees it among the scheduled prompts, and may cancel it")))))))

(defun drover-anvil--tool-fire (event &optional text cancelled argument)
  "Tell drover the event EVENT happened, releasing the prompts waiting for it.
TEXT, what happened, follows each prompt released; CANCELLED tells the
event will not happen, cancelling them instead; ARGUMENT releases only
the prompts waiting for the event on it.

MCP Parameters:
  event - The event's name, such as opencode-restart
  text - What happened, which follows each prompt released
  cancelled - (boolean) true when the event will not happen: the
      prompts waiting for it are cancelled
  argument - What the event concerns, to release only the prompts
      waiting for the event on it"
  (require 'drover-schedule)
  (let* ((caller (drover-anvil--caller))
         (trigger (progn (drover-schedule-load) (drover-schedule-trigger-named event)))
         (cancelled (drover-anvil--true-p cancelled))
         (text (and (stringp text) (not (string-blank-p text)) (drover-bound-text text 40 4000)))
         (argument (and (stringp argument) (not (string-blank-p argument)) argument)))
    (unless (eq (drover-anvil--scope 'fire) 'any)
      (drover-anvil--log "drover-schedule-fire" caller nil "refused" "fire allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no fire"))
    (unless trigger
      (drover-anvil--refuse "No event %s; drover knows %s" (drover-bound-line (format "%s" event) 60)
                            (or (mapconcat (lambda (entry) (format "%s (%s)" (car entry) (cdr entry)))
                                           (drover-schedule-triggers t) ", ")
                                "none")))
    (drover-anvil--log-or-refuse "drover-schedule-fire" caller nil
                                 (format "%s%s" trigger (if cancelled ", cancelled" "")))
    (let ((count (drover-quietly (drover-schedule-fire-event trigger cancelled text argument))))
      (drover-anvil--log "drover-schedule-fire" caller nil "done" (format "%d released" count))
      (drover-anvil--json
       `((outcome . ,(if cancelled "cancelled" "fired"))
         (event . ,(symbol-name trigger))
         (released . ,count))))))

(defun drover-anvil--job-session (job)
  "Return the live session the scheduled prompt JOB goes to, or nil."
  (condition-case nil
      (drover-schedule-session-named (plist-get job :link))
    (user-error nil)))

(defun drover-anvil--tool-scheduled (&optional key status limit)
  "List the prompts scheduled for later, the newest first.
KEY, STATUS and LIMIT narrow the list, as the parameters below tell.

MCP Parameters:
  key - A session's key, whose prompts alone to list
  status - Statuses to keep, comma-separated, among pending, opening,
      waiting, sending, sent, failed, interrupted and cancelled
  limit - (integer) Prompts listed at most, 50 by default"
  (drover-start)
  (require 'drover-schedule)
  (drover-schedule-load)
  (let* ((caller (drover-anvil--caller))
         (scope (drover-anvil--scope 'read))
         (only (and (stringp key) (not (string-empty-p key)) (drover-anvil--session key)))
         (statuses (drover-anvil--split status))
         (limit (max 1 (min 200 (if (natnump limit) limit 50))))
         (by (drover-anvil--by caller))
         (jobs (seq-filter
                (lambda (job)
                  (let ((session (drover-anvil--job-session job)))
                    (and (or (null statuses) (member (symbol-name (plist-get job :status)) statuses))
                         (or (null only) (and session (equal (drover-session-key session) (drover-session-key only))))
                         (or (eq scope 'any)
                             (equal (plist-get job :by) by)
                             (and session scope (drover-anvil--in-scope-p scope caller session))))))
                (sort (drover-schedule--jobs) :key (lambda (job) (or (plist-get job :created) 0)) :reverse t))))
    (unless scope
      (drover-anvil--log "drover-scheduled" caller nil "refused" "read allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no read"))
    (drover-anvil--log "drover-scheduled" caller nil "done" (format "%d listed" (length jobs)))
    (drover-anvil--json
     `((count . ,(length jobs))
       (scheduled
        . ,(vconcat
            (mapcar (lambda (job)
                      (let ((session (drover-anvil--job-session job)))
                        (drover-anvil--compact
                         `((id . ,(plist-get job :id))
                           (key . ,(and session (drover-anvil-key-string session)))
                           (link . ,(drover-anvil--line (plist-get job :link) 300))
                           (title . ,(drover-anvil--line (plist-get job :title) 160))
                           (when . ,(drover-anvil--line (drover-schedule--when job) 200))
                           (status . ,(drover-schedule--status-text job))
                           (prompt . ,(drover-anvil--line (plist-get job :prompt) 300))
                           (by . ,(drover-anvil--line (plist-get job :by) 200))
                           (created . ,(drover-anvil--time (plist-get job :created)))
                           (sent_at . ,(drover-anvil--time (plist-get job :sent-at)))
                           (error . ,(drover-anvil--line (plist-get job :error) 300))))))
                    (seq-take jobs limit))))
       (truncated . ,(and (> (length jobs) limit) t))))))

;;;; Permission prompts

(declare-function drover-permissions-gather "drover-permissions" (since until callback &optional seconds))
(defvar drover-permissions-limit)

(defun drover-anvil--titles ()
  "Return the titles of the sessions drover knows, by (HOST ENGINE ID).
HOST is nil for this host's, ENGINE a symbol, ID the conversation's."
  (let ((titles (make-hash-table :test #'equal)))
    (dolist (session (drover-index-sessions))
      (when-let* ((engine (drover-session-engine session))
                  (id (drover-session-engine-id session)))
        (puthash (list (drover-session-host session) engine id)
                 (drover-session-display-title session) titles)))
    titles))

(defun drover-anvil--permission-data (prompt titles)
  "Return PROMPT, as `drover-permissions-gather' tells it, as data.
TITLES are the sessions' titles, as `drover-anvil--titles' tells them."
  (let ((host (or (plist-get prompt :host) drover-host))
        (engine (symbol-name (plist-get prompt :engine))))
    (drover-anvil--compact
     `((host . ,(drover-anvil--line host 100))
       (engine . ,engine)
       (session . ,(plist-get prompt :id))
       (link . ,(format "agent-session://%s/%s/%s" host engine (plist-get prompt :id)))
       (title . ,(drover-anvil--line (gethash (list (plist-get prompt :host) (plist-get prompt :engine)
                                                    (plist-get prompt :id))
                                              titles)
                                     160))
       (directory . ,(and (plist-get prompt :directory)
                          (drover-anvil--line (abbreviate-file-name (plist-get prompt :directory)) 300)))
       (time . ,(drover-anvil--time (plist-get prompt :time)))
       (action . ,(plist-get prompt :action))
       (resources . ,(and (plist-get prompt :resources) (vconcat (plist-get prompt :resources))))
       (rules . ,(and (plist-get prompt :rules) (vconcat (plist-get prompt :rules))))
       (reply . ,(plist-get prompt :reply))
       (message . ,(plist-get prompt :message))
       (plugin . ,(plist-get prompt :plugin))))))

(defun drover-anvil--tool-permissions (&optional since until host offset limit)
  "Return the permission prompts agents gave from SINCE to UNTIL, on every host.
Each is what the agent asked, the rules that asked, and the user's
reply, read from the ledger each host's harnesses append to; HOST,
OFFSET and LIMIT narrow the answer, as the parameters below tell.

MCP Parameters:
  since - Start of the period, seconds since the epoch or ISO 8601;
      seven days before its end by default
  until - End of the period, seconds since the epoch or ISO 8601; now
      by default
  host - \"local\" for this host's prompts, or a host's name; every
      host's by default
  offset - (integer) Prompts to skip, oldest first, 0 by default
  limit - (integer) Prompts answered at most, 1000 by default"
  (drover-start)
  (require 'drover-permissions)
  (let* ((caller (drover-anvil--caller))
         (scope (drover-anvil--scope 'read))
         (until (float (or (drover-anvil--read-time until) (float-time))))
         (since (float (or (drover-anvil--read-time since) (- until (* 7 86400)))))
         (offset (if (natnump offset) offset 0))
         (limit (max 1 (min drover-permissions-limit (if (natnump limit) limit 1000))))
         (stop nil))
    (unless scope
      (drover-anvil--log "drover-permissions" caller nil "refused" "read allowed nowhere")
      (drover-anvil--refuse "drover-anvil-permissions allows no read"))
    (unless (< since until)
      (drover-anvil--refuse "The period ends before it starts"))
    (let ((id (drover-anvil--job
               "drover-permissions" caller nil :seconds (+ drover-prompts-timeout 30)
               :finally (lambda (_id) (when stop (funcall stop))))))
      (condition-case err
          (setq stop
                (drover-permissions-gather
                 since until
                 (lambda (result)
                   (let* ((prompts (seq-filter (lambda (prompt)
                                                 (and (drover-anvil--host-match-p (plist-get prompt :host) host)
                                                      (drover-anvil--prompt-allowed-p scope caller prompt)))
                                               (plist-get result :permissions)))
                          (page (seq-take (nthcdr offset prompts) limit))
                          (titles (drover-anvil--titles)))
                     (drover-anvil--done
                      id `((outcome . ,(format "%d permission prompts" (length prompts)))
                           (since . ,(drover-anvil--time since))
                           (until . ,(drover-anvil--time until))
                           (count . ,(length prompts))
                           (offset . ,offset)
                           (truncated . ,(and (> (length prompts) (+ offset limit)) t))
                           (hosts . ,(drover-anvil--hosts-data
                                      (seq-filter (lambda (entry)
                                                    (drover-anvil--host-match-p (plist-get entry :host) host))
                                                  (plist-get result :hosts))))
                           (permissions . ,(vconcat (mapcar (lambda (prompt) (drover-anvil--permission-data prompt titles))
                                                            page)))))))))
        (error (drover-anvil--fail id (drover-error-message err))))
      (drover-anvil--answer id))))

;;;; Jobs' answers

(defun drover-anvil--tool-job (job)
  "Return the answer of JOB, a job a tool answered with, once it is there.
While the client waits, it waits for the answer; otherwise it tells
the job is still pending.

MCP Parameters:
  job - The job's identifier"
  (let ((caller (drover-anvil--caller)))
    (unless (and (stringp job) (gethash job drover-anvil--jobs))
      (drover-anvil--refuse "No job %s: its answer was taken, or it was forgotten"
                            (drover-bound-line (format "%s" job) 64)))
    (unless (equal (drover-anvil--job-get job :caller) caller)
      (drover-anvil--refuse "Job %s is another session's" job))
    (drover-anvil--answer job)))

;;;; Registering

(defconst drover-anvil--tools
  '((drover-anvil--tool-sessions
     :id "drover-sessions" :read-only t
     :description "List the agent sessions drover knows, across harnesses (OpenCode, Claude Code, agent-shell, Codex) and hosts, most urgent first: key, title, status, what it waits for, harness, host, project, directory, model.  Filter by status, harness, project, host, attention.  The caller's own session is marked caller.")
    (drover-anvil--tool-session
     :id "drover-session" :read-only t
     :description "Read one session by key: status, model, what it waits for, and the recap of its last turn (the user's last prompt, the agent's reply, the files it changed) and the pending requests' identifiers, choices and fields.")
    (drover-anvil--tool-create
     :id "drover-session-create"
     :description "Start an agent session, unseen, in a directory (the caller's by default), on a harness (opencode by default), model, effort and agent, and send it a first prompt.  Returns its key; follow its turn with drover-session-wait.  Allowed where the user's policy allows, by default in the caller's project.")
    (drover-anvil--tool-send
     :id "drover-session-send"
     :description "Send a prompt to a session, as its front end delivers prompts (OpenCode queues or steers a running turn).  Then wait for its turn with drover-session-wait.  Allowed where the user's policy allows, by default the sessions of the caller's project; never the caller itself.")
    (drover-anvil--tool-wait
     :id "drover-session-wait" :read-only t
     :description "Wait until a session's turn ends or it needs the user (a permission or a question), with a timeout; answers the outcome, its status and the recap of the turn.  Emacs never waits: the call does.  Never on the caller itself nor a session waiting on it.")
    (drover-anvil--tool-answer
     :id "drover-session-answer"
     :description "Answer a session's pending question (answers by field key) or permission (decision once, always or reject), or dismiss it.  By default agents answer only the questions of sessions they started, and no permission.")
    (drover-anvil--tool-interrupt
     :id "drover-session-interrupt"
     :description "Stop a session's running turn.  By default only the sessions the caller started.")
    (drover-anvil--tool-archive
     :id "drover-session-archive"
     :description "Archive a session, closing its buffer; its conversation stays.  By default only the sessions the caller started.")
    (drover-anvil--tool-after
     :id "drover-session-after"
     :description "Have drover send a prompt to your own session after the next event it knows, such as opencode-restart or emacs-restart.  Use it when a step needs that event, such as tools that load only once the service restarts, then end your turn: the session carries on once it happened.  It asks for the event, as a prompt the user schedules does, and the user can cancel it.")
    (drover-anvil--tool-schedule
     :id "drover-schedule"
     :description "Have drover send a session a prompt later: once (in 90m, tomorrow 7:00, an ISO 8601 time), recurring (every weekday 9am Europe/Paris, every 30m), when its running turn ends, or after an event drover knows.  It survives restarts, and the user may cancel it.")
    (drover-anvil--tool-fire
     :id "drover-schedule-fire"
     :description "Tell drover an event happened, by name, releasing the prompts waiting for it, with what happened; or that it will not happen, cancelling them.  Allowed nowhere unless the user's policy allows it.")
    (drover-anvil--tool-scheduled
     :id "drover-scheduled" :read-only t
     :description "List the prompts scheduled for later, the newest first: when each goes, recurring or not, its status, its session, who scheduled it.")
    (drover-anvil--tool-prompts
     :id "drover-prompts" :read-only t
     :description "Return the prompts the user sent agents over a period (seven days by default, 31 at most), of every harness and host, oldest first: host, engine, conversation, its link, title, directory, time and text.  Subagents' prompts, programs' and what engines add as the user are left out.  For reviews of how the user works with agents.")
    (drover-anvil--tool-permissions
     :id "drover-permissions" :read-only t
     :description "Return the permission prompts agents gave the user over a period, seven days by default and 31 at most, across harnesses and hosts, oldest first, each with its session, action, rules and reply.")
    (drover-anvil--tool-job
     :id "drover-job" :read-only t
     :description "Return the answer of a job another drover tool answered with, for clients that cannot wait for answers."))
  "The tools, as (HANDLER . PROPERTIES) for `anvil-server-register-tool'.")

(defun drover-anvil-tool-ids ()
  "Return the identifiers of the tools."
  (mapcar (lambda (spec) (plist-get (cdr spec) :id)) drover-anvil--tools))

(defun drover-anvil-disable ()
  "Remove the tools from Anvil's server."
  (interactive)
  (dolist (id (drover-anvil-tool-ids))
    (while (anvil-server-unregister-tool id drover-anvil-server-id))))

(defun drover-anvil-enable ()
  "Register the tools with Anvil's server, anew when they were already."
  (interactive)
  (require 'anvil-server)
  (drover-anvil-disable)
  (dolist (spec drover-anvil--tools)
    (apply #'anvil-server-register-tool (car spec)
           :server-id drover-anvil-server-id
           :intent '(agents sessions)
           :layer 'workflow
           (cdr spec))))

(provide 'drover-anvil)
;;; drover-anvil.el ends here
