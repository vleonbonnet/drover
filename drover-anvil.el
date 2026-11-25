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
;; fork, prompt, wait on, answer, interrupt and archive the sessions
;; drover knows, of every harness and host, through drover's operations
;; as the user's commands do, start the task of an Org heading, have
;; their own session prompted after an event such as a restart, schedule
;; prompts for later, once or recurring, and fire events:
;; `drover-anvil-enable' registers them with Anvil,
;; `drover-anvil-disable' removes them.
;;
;; Agents may do everything the tools offer, on every session of every
;; host: drover refuses only what cannot be done.  What a harness lets
;; its agents call is the harness's permissions to say.  The caller is
;; the session whose agent calls, as its client tells it
;; (`drover-anvil-callers'): OpenCode names it in each call's `_meta',
;; and Claude Code records the conversation of the process that started
;; Anvil's bridge.  No session waits on itself, on a session above it,
;; whose turn waits for its own, nor on a session waiting on it: such a
;; wait would only end at its timeout.  Every call is logged, in
;; `drover-anvil-log-file'; a log that cannot be written is warned
;; about, and the call goes on.
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
(require 'drover-links)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 32))
  (load "drover-core" nil t))

(declare-function anvil-server-register-tool "ext:anvil-server" (handler &rest properties))
(declare-function anvil-server-unregister-tool "ext:anvil-server" (tool-id &optional server-id))
(declare-function anvil-server-defer "ext:anvil-server" (poll &rest keys))
(declare-function anvil-server-can-defer-p "ext:anvil-server" ())
(declare-function drover-claude-code-pid-session-id "drover-claude-code" (pid))
(declare-function drover-remote-peer-named "drover-remote" (host))
(declare-function drover-remote-session-key "drover-remote" (name key))
(declare-function drover-remote-start-later "drover-remote"
                  (name directory local harness engine choose text callback))
(declare-function drover-remote-schedule-later "drover-remote" (session when text argument by callback))
(defvar anvil-server-request-meta)
(defvar anvil-server-transport-pid)

;;;; Options

(defgroup drover-anvil nil
  "Agents acting on drover's sessions through Anvil's MCP tools."
  :group 'drover
  :prefix "drover-anvil-")

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

(defconst drover-anvil--refusal-length 20000
  "Characters at most of a refusal.
One may name the values that work, or each item of a list refused,
`drover-items-limit' at most, in a few hundred characters each.")

(defun drover-anvil--refuse (format-string &rest args)
  "Signal that the tool refuses, as FORMAT-STRING and ARGS tell, to the agent.
The error is Anvil's tool error, which the agent reads."
  (signal (if (get 'anvil-server-tool-error 'error-conditions) 'anvil-server-tool-error 'drover-anvil-refused)
          (list (drover-bound-line (apply #'format-message format-string args) drover-anvil--refusal-length))))

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

(defun drover-anvil--link (session)
  "Return the target of the Org link to SESSION, or nil.
As `org-store-link' stores it, such as agent-session:opencode/ses_1,
naming its host for a session of another: what an agent writes in an
Org heading, where a key is no link, a terminal's naming no
conversation.  Nil while its conversation is not known yet, and for an
engine links do not name (see `drover-links-conversation')."
  (when-let* ((conversation (drover-links-conversation session)))
    (drover-anvil--line (drover-links-link conversation) 300)))

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
Its turn waits for the caller's: waiting for it would wait for the
caller itself."
  (and caller (or (equal key caller) (member key (drover-anvil--above caller)))))

;;;; Projects

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

(defun drover-anvil--in-project-p (caller directory)
  "Return non-nil when DIRECTORY is in the project of the session CALLER, a key."
  (when-let* ((record (and caller (drover-index-session caller)))
              ((null (drover-session-host record)))
              (own (drover-session-directory record))
              ((stringp directory))
              ((not (file-remote-p directory))))
    (equal (drover-anvil--project-root own) (drover-anvil--project-root directory))))

;;;; The log

(defun drover-anvil--log-file ()
  "Return the file logging the calls of the tools."
  (or drover-anvil-log-file (expand-file-name "anvil.log" drover-state-directory)))

(defun drover-anvil--log (tool caller key outcome &optional note)
  "Log the call of TOOL by CALLER on the session KEY, with its OUTCOME and NOTE.
CALLER and KEY are keys or nil, OUTCOME a word, NOTE a string or nil.
Return non-nil once logged; a failure is warned about, once in a while,
since what agents do then leaves no account."
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
       (when (drover--report-due-p (list "Anvil tools log" (car-safe err)))
         (drover-warn "Agents' calls of drover's tools go unlogged: %s cannot be written: %s"
                      (abbreviate-file-name file) (drover-error-message err)))
       nil))))

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
  (drover-anvil--settle id 'failed (drover-bound-line (format "%s" (or message "failed"))
                                                       drover-anvil--refusal-length)))

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
       (link . ,(drover-anvil--link session))
       (title . ,(drover-anvil--line (drover-session-display-title session) 160))
       (status . ,(symbol-name (drover-session-status session)))
       (since . ,(drover-anvil--time (drover-session-since session)))
       (updated . ,(drover-anvil--time (drover-session-updated session)))
       (needs_user . ,(and (drover-session-attention-p session) t))
       (asks . ,(and (drover-session-ask session)
                     (drover-anvil--line (plist-get (drover-session-ask session) :summary) 200)))
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
    (let* ((match (drover-anvil--matcher status harness project host attention query subagents caller))
           (limit (max 1 (min drover-anvil-list-limit (if (natnump limit) limit 30))))
           (sessions (drover-sort (seq-filter match (drover-index-sessions)))))
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
    (when-let* ((why (cond ((equal key caller) "a session cannot wait for itself")
                           ((drover-anvil--busy-with-caller-p caller key)
                            "its turn waits for the caller's: waiting for it would wait for itself")
                           ((and caller (drover-anvil--waits-for-p key caller))
                            "it waits for the caller: waiting for it would wait for itself"))))
      (drover-anvil--log "drover-session-wait" caller key "refused" why)
      (drover-anvil--refuse "%s: %s" (drover-anvil-key-string session) why))
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
    (unless (drover-supports-p session :submit)
      (drover-anvil--refuse "%s takes no prompt from an agent" (drover-anvil-key-string session)))
    (when-let* (((drover-session-buffer session))
                (blocker (drover-backend-property session :blocker))
                (why (drover-with-reporting "Anvil prompt"
                       (funcall blocker session))))
      (drover-anvil--log "drover-session-send" caller key "refused" why)
      (drover-anvil--refuse "%s cannot take a prompt now: %s" (drover-anvil-key-string session) why))
    (drover-anvil--log "drover-session-send" caller key "acting" (drover-anvil--prompt-note prompt))
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

(defun drover-anvil--match-model (name models)
  "Return the model of MODELS NAME names, or nil.
NAME is PROVIDER/ID, an identifier or a display name, case ignored."
  (let ((wanted (downcase name)))
    (seq-find (lambda (model)
                (or (and (drover-model-provider-id model)
                         (equal (downcase (concat (drover-model-provider-id model) "/" (drover-model-id model)))
                                wanted))
                    (equal (downcase (drover-model-id model)) wanted)
                    (equal (downcase (or (drover-model-name model) "")) wanted)))
              models)))

(defun drover-anvil--find-model (name directory)
  "Return the model NAME names among those offered in DIRECTORY, or refuse.
NAME is PROVIDER/ID, an identifier or a display name."
  (or (drover-anvil--match-model name (drover-models directory))
      (drover-anvil--refuse "No model %s is offered in %s" (drover-bound-line name 100)
                            (abbreviate-file-name directory))))

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

(defun drover-anvil--startable-backends ()
  "Return the backends starting sessions here, OpenCode's first."
  (let ((backends (seq-filter (lambda (backend)
                                (and (functionp (drover-backend-property backend :create))
                                     (drover-backend-engines backend)))
                              (drover-backends))))
    (if (memq 'opencode backends)
        (cons 'opencode (delq 'opencode backends))
      backends)))

(defun drover-anvil--tool-create (&optional harness directory prompt model effort agent title engine host)
  "Start a session, without showing it, and send it PROMPT; return its key.
HARNESS, DIRECTORY, MODEL, EFFORT, AGENT, TITLE, ENGINE and HOST tell
what starts where, as the parameters below tell.

MCP Parameters:
  harness - The harness starting it, such as opencode, claude-code-ide
      or agent-shell; opencode by default
  directory - Its directory, the caller's by default; on another host,
      one of that host's, where the caller's directory maps there by
      default
  host - The host starting it, by name, such as core; this one by
      default
  prompt - Its first prompt, Markdown; none to start it idle
  model - Its model, PROVIDER/ID or ID, the harness's default otherwise
  effort - Its reasoning effort, a variant of the model, such as high
  agent - Its agent, such as build or plan
  title - Its title
  engine - The engine it runs, for a harness running several"
  (drover-start)
  (if-let* ((name (drover-anvil--peer-of host)))
      (drover-anvil--create-there name harness directory prompt model effort agent title engine)
    (drover-anvil--create-here harness directory prompt model effort agent title engine)))

(defun drover-anvil--peer-of (host)
  "Return the name of the peer HOST names, or nil for this host; refuse another.
HOST is nil, empty, \"local\" or \"here\", or this host's name, for this
one, else a peer's name or label."
  (cond
   ((or (not (stringp host)) (string-blank-p host)
        (member (downcase host) '("local" "here"))
        (drover-host-local-p host))
    nil)
   ((and (featurep 'drover-remote) (drover-remote-peer-named host)))
   (t (drover-anvil--refuse "No host %s: %s" (drover-bound-line host 60)
                            (if (featurep 'drover-remote)
                                "drover-sessions tells the hosts of the sessions drover knows"
                              "drover-remote-mode is off, so drover knows no other host")))))

(defun drover-anvil--create-here (harness directory prompt model effort agent title engine)
  "Start a session on this host, as `drover-anvil--tool-create' tells.
HARNESS, DIRECTORY, PROMPT, MODEL, EFFORT, AGENT, TITLE and ENGINE are
its parameters."
  (let* ((caller (drover-anvil--caller))
         (record (and caller (drover-index-session caller)))
         (directory (cond ((and (stringp directory) (not (string-blank-p directory)))
                           (expand-file-name directory))
                          ((and record (stringp (drover-session-directory record))
                                (null (drover-session-host record)))
                           (drover-session-directory record))
                          (t (drover-anvil--refuse "Name the directory: the caller's is unknown"))))
         (startable (drover-anvil--startable-backends))
         (backend (if (and (stringp harness) (not (string-blank-p harness)))
                      (or (seq-find (lambda (backend) (equal (symbol-name backend) harness)) startable)
                          (drover-anvil--refuse "%s starts no session here; %s do"
                                                (drover-bound-line harness 40)
                                                (if startable (mapconcat #'symbol-name startable ", ") "none")))
                    (or (car startable) (drover-anvil--refuse "No harness starts sessions here"))))
         (engines (drover-backend-engines backend))
         (engine (if (and (stringp engine) (not (string-blank-p engine)))
                     (or (seq-find (lambda (candidate) (equal (symbol-name candidate) engine)) engines)
                         (drover-anvil--refuse "%s runs no engine %s" backend (drover-bound-line engine 40)))
                   (car engines)))
         (prompt (and prompt (not (equal prompt "")) (drover-anvil--prompt prompt))))
    (unless (file-directory-p directory)
      (drover-anvil--log "drover-session-create" caller nil "refused" "not a directory")
      (drover-anvil--refuse "%s is not a directory" (abbreviate-file-name directory)))
    (let* ((model (and (stringp model) (not (string-blank-p model)) (drover-anvil--find-model model directory)))
           (_ (when (and model (not (drover-model-runs-on-p model engine)))
                (drover-anvil--refuse "%s does not run %s" (drover-engine-label engine) (drover-model-label model))))
           (variant (drover-anvil--find-variant (and (stringp effort) (not (string-blank-p effort)) effort) model))
           (options (list :agent (and (stringp agent) (not (string-blank-p agent)) (drover-bound-line agent 60))
                          :model model :variant variant
                          :title (and (stringp title) (not (string-blank-p title)) (drover-bound-line title 160))
                          :hidden t)))
      (drover-anvil--log "drover-session-create" caller nil "acting"
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
                                        (link . ,(drover-anvil--link session))
                                        (directory . ,(abbreviate-file-name directory))
                                        (sent . ,(if prompt t :json-false))
                                        (note . ,(if prompt
                                                     "Wait for its first turn with drover-session-wait"
                                                   "Send it a prompt with drover-session-send")))))))
         (drover-anvil--else id))
        (drover-anvil--answer id)))))

;; On another host, nothing is waited for in the call: the remote layer
;; answers from its process filter, settling the job, which then waits
;; for the session to be listed here, so that its key is of use at once.

(defun drover-anvil--told-link (host engine id)
  "Return the link to the conversation ID of ENGINE on HOST, as its host told it.
Nil unless both are told: a session not listed here yet has no record
to name its conversation."
  (and (symbolp engine) engine (stringp id) (not (string-empty-p id))
       (drover-anvil--line (drover-links-link (list :engine engine :id id :host host)) 300)))

(defun drover-anvil--listed-poll (id)
  "Settle the job ID once the session another host started for it is listed here.
With its :answer, the session's key in its :started; its link is then
the one its record tells, when it tells one."
  (when-let* ((key (drover-anvil--job-get id :started))
              (record (drover-index-session key)))
    (let ((answer (copy-alist (drover-anvil--job-get id :answer))))
      (when-let* ((link (drover-anvil--link record)))
        (setf (alist-get 'link answer) link))
      (drover-anvil--done id answer))))

(defun drover-anvil--unlisted-answer (id host)
  "Return what the job ID answers when HOST's session is not listed in time.
Its :answer, telling so; signal an error when HOST never answered."
  (if-let* ((answer (drover-anvil--job-get id :answer)))
      (append answer `((listed . :json-false)
                       (listed_note . ,(format "%s started it, but it is not listed here yet" host))))
    (error "%s did not answer in time" host)))

(defun drover-anvil--create-there (name harness directory prompt model effort agent title engine)
  "Have the peer NAME start a session, as `drover-anvil--tool-create' tells.
HARNESS, DIRECTORY, PROMPT, MODEL, EFFORT, AGENT, TITLE and ENGINE are
its parameters, DIRECTORY one of NAME's."
  (let* ((caller (drover-anvil--caller))
         (record (and caller (drover-index-session caller)))
         (local (and record (null (drover-session-host record)) (drover-session-directory record)))
         (symbol (lambda (value)
                   (and (stringp value) (string-match-p "\\`[a-z0-9-]\\{1,40\\}\\'" value) (intern value))))
         (harness (and (stringp harness) (not (string-blank-p harness))
                       (or (funcall symbol harness) (drover-anvil--refuse "No harness %s" (drover-bound-line harness 40)))))
         (engine (and (stringp engine) (not (string-blank-p engine))
                      (or (funcall symbol engine) (drover-anvil--refuse "No engine %s" (drover-bound-line engine 40)))))
         (directory (and (stringp directory) (not (string-blank-p directory)) (string-trim directory)))
         (prompt (and prompt (not (equal prompt "")) (drover-anvil--prompt prompt)))
         (model (and (stringp model) (not (string-blank-p model)) model))
         (effort (and (stringp effort) (not (string-blank-p effort)) effort))
         (options (list :agent (and (stringp agent) (not (string-blank-p agent)) (drover-bound-line agent 60))
                        :title (and (stringp title) (not (string-blank-p title)) (drover-bound-line title 160))))
         (choose (lambda (target catalog)
                   (let ((chosen (and model (drover-anvil--match-model model (plist-get catalog :models)))))
                     (cond
                      ((and model (not chosen))
                       (format "No model %s is offered for %s on %s" (drover-bound-line model 100)
                               (plist-get target :label) name))
                      ((and chosen (drover-model-engines chosen)
                            (not (memq (plist-get target :engine) (drover-model-engines chosen))))
                       (format "%s does not run %s" (plist-get target :label) (drover-model-label chosen)))
                      (t (append options (list :model chosen
                                               :variant (drover-anvil--find-variant effort chosen))))))))
         (id nil))
    (drover-anvil--log "drover-session-create" caller nil "acting"
                       (format "%s on %s in %s%s" (or harness "default harness") name (or directory "default directory")
                               (if prompt (concat ", " (drover-anvil--prompt-note prompt)) "")))
    (setq id (drover-anvil--job
              "drover-session-create" caller nil :seconds 120
              :poll #'drover-anvil--listed-poll
              :timeout (lambda (id) (drover-anvil--unlisted-answer id name))))
    (drover-remote-start-later
     name directory local harness engine choose prompt
     (lambda (result)
       (drover-with-reporting "Anvil start"
         (if-let* ((why (plist-get result :failure)))
             (drover-anvil--fail id why)
           (let* ((key (drover-remote-session-key name (plist-get result :key)))
                  (unsent (plist-get result :unsent))
                  (sent (and prompt (not unsent))))
             (drover-anvil--job-put id :key key)
             (when caller
               (puthash key caller drover-anvil--started))
             (when sent
               (puthash key (float-time) drover-anvil--sent))
             (drover-anvil--job-put
              id :answer
              `((outcome . "started")
                (key . ,(drover-anvil--key-of key))
                (link . ,(drover-anvil--told-link name (plist-get result :engine) (plist-get result :eid)))
                (host . ,name)
                (directory . ,(plist-get result :directory))
                (harness . ,(symbol-name (plist-get result :backend)))
                (title . ,(plist-get result :title))
                (sent . ,(if sent t :json-false))
                (note . ,(cond (unsent (format "Its harness did not take the prompt: %s; send it with drover-session-send"
                                               unsent))
                               (prompt "Wait for its first turn with drover-session-wait")
                               (t "Send it a prompt with drover-session-send")))))
             (drover-anvil--job-put id :started key)
             (drover-anvil--advance id))))))
    (drover-anvil--answer id)))

;;;; Tasks

(declare-function drover-task-prompt-id "drover-task" (text))
(declare-function drover-task-org-find "drover-task-org" (id callback))
(declare-function drover-task-start-quietly "drover-task" (marker choices callback))
(declare-function drover-task-link "drover-task" (session id))
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(defvar drover-task-setup-timeout)

(defconst drover-anvil--task-id-regexp "\\`[A-Za-z0-9][A-Za-z0-9_.:-]\\{0,99\\}\\'"
  "What the Org ID of a heading looks like, as links to tasks name it.")

(defun drover-anvil--given (value &optional limit)
  "Return VALUE, a parameter received, trimmed, when it is a nonblank string.
Of LIMIT characters at most, 1000 by default; nil otherwise."
  (and (stringp value)
       (not (string-blank-p value))
       (length< value (or limit 1000))
       (substring-no-properties (string-trim value))))

(defun drover-anvil--task-id (value)
  "Return the Org ID VALUE names, or refuse.
VALUE is the ID itself, or a link to its heading, in Org or in
Markdown: [[id:ID]], [[id:ID][TITLE]], [TITLE](id:ID) or id:ID."
  (let* ((text (drover-anvil--given value))
         (id (cond ((null text) nil)
                   ((drover-task-prompt-id text))
                   ((string-prefix-p "id:" text) (substring text 3))
                   (t text))))
    (if (and id (string-match-p drover-anvil--task-id-regexp id))
        id
      (drover-anvil--refuse "No Org ID in %s: give a heading's ID, or a link to it, [[id:ID]]"
                            (drover-bound-line (format "%s" value) 100)))))

(defun drover-anvil--task-directory (value)
  "Return the directory VALUE names, a parameter received, or nil for none.
Refuse one that is not absolute, or of another machine."
  (when-let* ((directory (drover-anvil--given value)))
    (unless (and (or (file-name-absolute-p directory) (string-prefix-p "~" directory))
                 (not (file-remote-p directory)))
      (drover-anvil--refuse "%s is not a directory of this host, absolute or under ~"
                            (drover-bound-line directory 200)))
    (file-name-as-directory (expand-file-name directory))))

(defun drover-anvil--task-started (id caller result)
  "Settle the job ID, of drover-task-start for CALLER, with RESULT.
RESULT is what `drover-task-start-quietly' answers.  A session of this
host settles it at once; one another host started, once listed here, so
that its key is of use (see `drover-anvil--listed-poll')."
  (if-let* ((why (plist-get result :failure)))
      (drover-anvil--fail id why)
    (let* ((session (plist-get result :session))
           (key (if session (drover-session-key session) (plist-get result :key)))
           (host (plist-get result :host))
           (directory (plist-get result :directory))
           (answer `((outcome . ,(symbol-name (plist-get result :outcome)))
                     (key . ,(if session (drover-anvil-key-string session) (drover-anvil--key-of key)))
                     (link . ,(and session (drover-anvil--link session)))
                     (host . ,(drover-anvil--line host 100))
                     (task . ,(drover-anvil--line (plist-get result :id) 100))
                     (title . ,(drover-anvil--line (plist-get result :title) 160))
                     ;; Another host's directory is named as that host names it.
                     (directory . ,(and (stringp directory)
                                        (drover-anvil--line (if host directory (abbreviate-file-name directory)) 300)))
                     (branch . ,(drover-anvil--line (plist-get result :branch) 200))
                     (note . ,(concat (if-let* ((note (plist-get result :note))) (concat note ".  ") "")
                                      "Its first prompt links the heading and quotes its body: wait for its turn with drover-session-wait")))))
      (when caller
        (puthash key caller drover-anvil--started))
      (puthash key (float-time) drover-anvil--sent)
      (if session
          (drover-anvil--done id answer)
        (drover-anvil--job-put id :answer answer)
        (drover-anvil--job-put id :started key)
        (drover-anvil--advance id)))))

(defun drover-anvil--tool-task-start (id &optional repo directory host)
  "Start the task of the Org heading ID, as `drover-task-start' does on it.
In no window, asking nothing: its place is made, a worktree of its
repository, its main checkout or a folder, and a session started there,
whose first prompt links the heading and quotes its body.  REPO,
DIRECTORY and HOST choose what the heading leaves open, as the
parameters below tell; a choice still open is refused, naming what is
missing and the values that work, never asked.  A task working
somewhere already continues: another session starts in its place.

MCP Parameters:
  id - The heading's Org ID, or a link to it, [[id:ID]]
  repo - Its repository, a name of drover-task-repositories, written on
      the heading as DROVER_REPO; for a heading naming several
      repositories, or neither a repository nor a folder
  directory - The folder of a task that works in a folder, absolute or
      under ~, written on the heading as DROVER_FOLDER; for a task
      working in several places already, the one to continue in
  host - The host starting it, by name, such as core; this one by
      default"
  (drover-start)
  (require 'drover-task)
  (let* ((caller (drover-anvil--caller))
         (id (drover-anvil--task-id id))
         (repo (drover-anvil--given repo 200))
         (directory (drover-anvil--task-directory directory))
         (peer (drover-anvil--peer-of host))
         (job nil))
    (drover-anvil--log "drover-task-start" caller nil "acting"
                       (concat "task " id
                               (if repo (concat ", repo " repo) "")
                               (if directory (concat ", in " (abbreviate-file-name directory)) "")
                               (if peer (concat ", on " peer) "")))
    (setq job (drover-anvil--job
               "drover-task-start" caller nil
               ;; Its place is made, then set up, before its session starts.
               :seconds (+ drover-task-setup-timeout 180)
               :poll #'drover-anvil--listed-poll
               :timeout (lambda (job)
                          (if (drover-anvil--job-get job :answer)
                              (drover-anvil--unlisted-answer job peer)
                            (error "The task did not start in time: its place may still be set up, and its session starts once it is; drover-sessions lists it then")))))
    (drover-anvil--call
     job #'drover-task-org-find id
     (lambda (marker)
       (drover-anvil--call
        job (lambda ()
              (if (null marker)
                  (drover-anvil--fail job (format "No heading of ID %s in the Org files drover reads: those visited, and those the ID locations and the agenda name"
                                                  id))
                (drover-task-start-quietly
                 marker (list :repo repo :directory directory :host peer)
                 (lambda (result)
                   (drover-anvil--call job #'drover-anvil--task-started job caller result))))))))
    (drover-anvil--answer job)))

;;;; Forking

(defun drover-anvil--fork (id caller session task prompt)
  "Fork SESSION in no window for the job ID, of CALLER, and settle it.
The fork is linked to TASK, the ID of an Org heading, when given, and
sent PROMPT, when given."
  (let ((source (drover-anvil-key-string session)))
    (drover-anvil--call
     id #'drover-backend-call session :fork-hidden
     (lambda (fork)
       (drover-anvil--call
        id (lambda ()
             (let ((key (drover-session-key fork)))
               (when caller
                 (puthash key caller drover-anvil--started))
               ;; A fork's first prompt is the original's, which names no item.
               (when task
                 (drover-task-link fork task))
               (when prompt
                 (drover-backend-call fork :send prompt nil)
                 (puthash key (float-time) drover-anvil--sent))
               (drover-anvil--done
                id `((outcome . "forked")
                     (key . ,(drover-anvil-key-string fork))
                     (link . ,(drover-anvil--link fork))
                     (forked_from . ,source)
                     (task . ,task)
                     (directory . ,(and (stringp (drover-session-directory fork))
                                        (drover-anvil--line (abbreviate-file-name (drover-session-directory fork)) 300)))
                     (sent . ,(if prompt t :json-false))
                     (note . ,(if prompt
                                  "Wait for its turn with drover-session-wait"
                                "It holds the conversation so far: send it a prompt with drover-session-send"))))))))
     (drover-anvil--else id))))

(defun drover-anvil--tool-fork (&optional key task prompt)
  "Fork the session KEY whole into a new session, in no window; return its key.
The fork holds the conversation so far, and goes its own way from
there.  TASK links it to an Org heading, PROMPT is sent to it, as the
parameters below tell.  A session whose harness cannot fork is refused,
its harness named: nothing else is started in its place.

MCP Parameters:
  key - The session to fork, as drover-sessions tells it; the caller's
      own by default
  task - An Org heading's ID, or a link to it, [[id:ID]]: the fork is
      linked to that heading as its task, which its first prompt, the
      original's, cannot tell
  prompt - A prompt sent to the fork once it exists, Markdown, such as
      the item of the conversation it takes on"
  (drover-start)
  (let* ((caller (drover-anvil--caller))
         (session (if (drover-anvil--given key)
                      (drover-anvil--session key)
                    (or (and caller (drover-index-session caller))
                        (drover-anvil--refuse "Name the session to fork: the caller is unknown, its client naming no session drover knows"))))
         (source (drover-session-key session))
         (task (and (drover-anvil--given task)
                    (progn (require 'drover-task) (drover-anvil--task-id task))))
         (prompt (and prompt (not (equal prompt "")) (drover-anvil--prompt prompt))))
    (when-let* ((why (drover-fork-refusal session)))
      (drover-anvil--log "drover-session-fork" caller source "refused" why)
      (drover-anvil--refuse "%s cannot be forked: %s" (drover-anvil-key-string session) why))
    (drover-anvil--log "drover-session-fork" caller source "acting"
                       (concat (if task (concat "task " task) "no task")
                               (if prompt (concat ", " (drover-anvil--prompt-note prompt)) "")))
    (let ((id (drover-anvil--job "drover-session-fork" caller source :seconds 90)))
      (if (null task)
          (drover-anvil--fork id caller session nil prompt)
        ;; The heading first: a fork linked to no heading would be lost to its task.
        (drover-anvil--call
         id #'drover-task-org-find task
         (lambda (marker)
           (drover-anvil--call
            id (lambda ()
                 (if (null marker)
                     (drover-anvil--fail id (format "No heading of ID %s in the Org files drover reads: nothing was forked"
                                                    task))
                   (drover-anvil--fork id caller session (or (org-entry-get marker "ID") task) prompt)))))))
      (drover-anvil--answer id))))

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

(defun drover-anvil--answer-requests (id key requests request decision answers dismiss)
  "Answer, for the job ID, one of REQUESTS of the session KEY.
REQUEST names it, else the oldest; DECISION, ANSWERS and DISMISS tell
the answer."
  (let* ((record (drover-index-session key))
         (chosen (if (stringp request)
                     (seq-find (lambda (candidate) (equal (format "%s" (plist-get candidate :id)) request)) requests)
                   (car requests)))
         (answer (and chosen (drover-anvil--answer-of chosen decision answers dismiss))))
    (cond
     ((plist-get requests :failure) (drover-anvil--fail id (plist-get requests :failure)))
     ((null record) (drover-anvil--fail id "the session is no longer live"))
     ((null chosen) (drover-anvil--fail id (if request "no such request pending" "no request pending")))
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
    (drover-anvil--log "drover-session-answer" caller key "acting"
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
             (drover-anvil--answer-requests id key requests request decision answers dismiss)))))
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
    (if (not (drover-session-active-p session))
        (drover-anvil--json `((outcome . "idle") (key . ,(drover-anvil-key-string session))
                              (note . "No turn runs")))
      (unless (drover-supports-p session :interrupt)
        (drover-anvil--refuse "%s cannot be interrupted from here" (drover-anvil-key-string session)))
      (drover-anvil--log "drover-session-interrupt" caller key "acting" nil)
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
    (unless (drover-archivable-p session)
      (drover-anvil--refuse "%s cannot be archived" (drover-anvil-key-string session)))
    (drover-anvil--log "drover-session-archive" caller key "acting" nil)
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
         (until (float (or (drover-anvil--read-time until) (float-time))))
         (since (float (or (drover-anvil--read-time since) (- until (* 7 86400)))))
         (offset (if (natnump offset) offset 0))
         (limit (max 1 (min drover-prompts-limit (if (natnump limit) limit 1000))))
         (text-limit (max 20 (min drover-prompts-text-limit
                                  (if (natnump text_limit) text_limit drover-prompts-text-limit))))
         (stop nil))
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
                                                 (drover-anvil--host-match-p (plist-get prompt :host) host))
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
      (drover-anvil--log "drover-session-after" caller caller "acting"
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

(defun drover-anvil--when (when &optional there)
  "Return WHEN, as an agent tells it, as `drover-schedule-for-session' takes it.
The text the user's scheduling reads, or an ISO 8601 time or seconds
since the epoch, as a time.  Refuse what reads as neither; but with
THERE non-nil, for another host, which reads the text with its own
events, the text is left for it to read."
  (unless (and (stringp when) (not (string-blank-p when)) (length< when 200))
    (drover-anvil--refuse "Tell when, such as in 90m, tomorrow 7:00, every day 7:00 or an ISO 8601 time"))
  (if-let* (((string-match-p drover-anvil--iso-time-regexp (string-trim when)))
            (time (drover-anvil--read-time (string-trim when))))
      (if (< time (float-time))
          (drover-anvil--refuse "%s is past" (string-trim when))
        (seconds-to-time time))
    (if there
        (string-trim when)
      (condition-case err
          (progn (drover-schedule-when when) when)
        (user-error (drover-anvil--refuse "%s" (drover-error-message err)))))))

(defun drover-anvil--by (caller)
  "Return how a prompt the session CALLER schedules names who scheduled it."
  (if caller (concat "agent " (drover-anvil--key-of caller)) "agent"))

(defun drover-anvil--tool-schedule (key prompt when &optional argument)
  "Have drover send PROMPT to the session KEY later.
Once, recurring, or after an event: WHEN tells when, as the user's own
scheduling reads it; ARGUMENT is what an event taking one concerns.
The prompt joins the user's scheduled prompts, who sees and may cancel
it, survives restarts, and is delivered out of every window.  For a
session of another host, it joins that host's, which reads WHEN with
its own clock, time zone and events; the caller may name its own.

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
  (let ((session (drover-anvil--session key)))
    (if (drover-session-host session)
        (drover-anvil--schedule-there session prompt when argument)
      (drover-anvil--schedule-here session prompt when argument))))

(defun drover-anvil--schedule-there (session prompt when argument)
  "Have the host of SESSION, a peer's, schedule PROMPT for it, WHEN tells.
ARGUMENT is what an event taking one concerns.  See
`drover-anvil--tool-schedule'."
  (unless (featurep 'drover-remote)
    (drover-anvil--refuse "%s runs on %s, and drover-remote-mode is off"
                          (drover-anvil-key-string session) (drover-session-host session)))
  (let* ((caller (drover-anvil--caller))
         (key (drover-session-key session))
         (prompt (drover-anvil--prompt prompt))
         (when (drover-anvil--when when t))
         (argument (and (stringp argument) (not (string-blank-p argument)) (drover-bound-line argument 2000)))
         (host (drover-session-host session))
         (id nil))
    (drover-anvil--log "drover-schedule" caller key "acting"
                       (format "on %s, %s, %s" host
                               (if (stringp when) (drover-bound-line when 80) (drover-anvil--time (float-time when)))
                               (drover-anvil--prompt-note prompt)))
    (setq id (drover-anvil--job "drover-schedule" caller key :seconds 60))
    (drover-anvil--call
     id #'drover-remote-schedule-later session when prompt argument (drover-anvil--by caller)
     (lambda (result)
       (drover-with-reporting "Anvil schedule"
         (if-let* ((why (plist-get result :failure)))
             (drover-anvil--fail id why)
           (drover-anvil--done
            id `((outcome . "scheduled")
                 (id . ,(plist-get result :id))
                 (key . ,(drover-anvil--key-of key))
                 (host . ,host)
                 (when . ,(plist-get result :when))
                 (next . ,(drover-anvil--time (plist-get result :next)))
                 (note . ,(format "The user sees it among %s's scheduled prompts, and may cancel it"
                                  host))))))))
    (drover-anvil--answer id)))

(defun drover-anvil--schedule-here (session prompt when argument)
  "Schedule PROMPT for SESSION, of this host, when WHEN tells.
ARGUMENT is what an event taking one concerns.  See
`drover-anvil--tool-schedule'."
  (let* ((caller (drover-anvil--caller))
         (key (drover-session-key session))
         (prompt (drover-anvil--prompt prompt)))
    (let ((when (drover-anvil--when when))
          (argument (and (stringp argument) (not (string-blank-p argument)) (drover-bound-line argument 2000))))
      (drover-anvil--log "drover-schedule" caller key "acting"
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
    (unless trigger
      (drover-anvil--refuse "No event %s; drover knows %s" (drover-bound-line (format "%s" event) 60)
                            (or (mapconcat (lambda (entry) (format "%s (%s)" (car entry) (cdr entry)))
                                           (drover-schedule-triggers t) ", ")
                                "none")))
    (drover-anvil--log "drover-schedule-fire" caller nil "acting"
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

(declare-function drover-remote-jobs-later "drover-remote" (callback &optional seconds))
(declare-function drover-links-conversation "drover-links" (session))
(declare-function drover-links-parse "drover-links" (path &optional engine))
(defvar drover-links-type)

(defun drover-anvil--link-conversation (link host)
  "Return the conversation LINK names, (HOST ENGINE . ID), or nil.
LINK is an agent-session link without a host, as a scheduled prompt
keeps it; HOST is the host keeping it, nil for this one."
  (let ((prefix (concat drover-links-type ":")))
    (when-let* (((stringp link))
                ((string-prefix-p prefix link))
                (conversation (drover-links-parse (substring link (length prefix)))))
      (cons host (cons (plist-get conversation :engine) (plist-get conversation :id))))))

(defun drover-anvil--scheduled-target (string)
  "Return the session STRING names, as the list of scheduled prompts finds it.
A plist of :key and :conversation, (HOST ENGINE . ID) or nil.  The
session need not be live: an archived or older one drover knows is
found too, and a key naming none matches the prompts keyed so.  Refuse
a key of no backend."
  (let* ((key (or (drover-anvil--parse-key string)
                  (drover-anvil--refuse "No session %s: list them with drover-sessions"
                                        (drover-bound-line (format "%s" string) 200))))
         (record (or (drover-index-session key)
                     (seq-find (lambda (session) (equal (drover-session-key session) key))
                               (drover-known-sessions nil t))))
         (conversation (and record (drover-links-conversation record))))
    (list :key key
          :conversation (and conversation
                             (cons (drover-session-host record)
                                   (cons (plist-get conversation :engine) (plist-get conversation :id)))))))

(defun drover-anvil--local-scheduled (job)
  "Return the scheduled prompt JOB, of this host's queue, as the list tells it.
A plist of :key, its session's key or nil, :status, its name, :created
and :data, what the list answers of it."
  (let ((session (drover-anvil--job-session job)))
    (list :key (and session (drover-session-key session))
          :conversation (drover-anvil--link-conversation (plist-get job :link) nil)
          :status (symbol-name (plist-get job :status))
          :created (plist-get job :created)
          :data (drover-anvil--compact
                 `((id . ,(plist-get job :id))
                   (host . ,drover-host)
                   (key . ,(and session (drover-anvil-key-string session)))
                   (link . ,(drover-anvil--line (plist-get job :link) 300))
                   (title . ,(drover-anvil--line (plist-get job :title) 160))
                   (when . ,(drover-anvil--line (drover-schedule--when job) 200))
                   (next . ,(drover-anvil--time (plist-get job :due)))
                   (status . ,(drover-schedule--status-text job))
                   (prompt . ,(drover-anvil--line (plist-get job :prompt) 300))
                   (by . ,(drover-anvil--line (plist-get job :by) 200))
                   (created . ,(drover-anvil--time (plist-get job :created)))
                   (sent_at . ,(drover-anvil--time (plist-get job :sent-at)))
                   (error . ,(drover-anvil--line (plist-get job :error) 300)))))))

(defun drover-anvil--remote-scheduled (job)
  "Return JOB, a prompt scheduled on another host, as the list tells it.
JOB is as `drover-remote-jobs-later' gives it; the plist returned is
as `drover-anvil--local-scheduled' returns."
  (let ((status (and (plist-get job :status) (symbol-name (plist-get job :status)))))
    (list :key (plist-get job :key)
          :conversation (drover-anvil--link-conversation (plist-get job :link) (plist-get job :host))
          :status status
          :created (plist-get job :created)
          :data (drover-anvil--compact
                 `((id . ,(plist-get job :id))
                   (host . ,(plist-get job :host))
                   (key . ,(and (plist-get job :key) (drover-anvil--key-of (plist-get job :key))))
                   (link . ,(plist-get job :link))
                   (title . ,(drover-anvil--line (plist-get job :title) 160))
                   (when . ,(cond ((plist-get job :every))
                                  ((plist-get job :turn) "when its turn ends")
                                  ((plist-get job :after)
                                   (concat "after the next " (plist-get job :after)
                                           (if (plist-get job :argument)
                                               (format " (%s)" (plist-get job :argument))
                                             "")))
                                  ((plist-get job :next) "once")))
                   (next . ,(drover-anvil--time (plist-get job :next)))
                   (status . ,status)
                   (prompt . ,(drover-anvil--line (plist-get job :prompt) 300))
                   (by . ,(plist-get job :by))
                   (created . ,(drover-anvil--time (plist-get job :created)))
                   (sent_at . ,(drover-anvil--time (plist-get job :sent)))
                   (error . ,(plist-get job :error)))))))

(defun drover-anvil--scheduled-answer (entries hosts only statuses limit)
  "Return the list of ENTRIES, scheduled prompts, the newest first, as data.
ENTRIES are as `drover-anvil--local-scheduled' returns them; ONLY, a
session as `drover-anvil--scheduled-target' finds it, or nil, and
STATUSES, names, narrow them, LIMIT bounds them.  HOSTS tells how each
other host answered, plists of :host and :jobs or :failure."
  (let ((kept (sort (seq-filter (lambda (entry)
                                  (and (or (null statuses) (member (plist-get entry :status) statuses))
                                       (or (null only)
                                           (equal (plist-get entry :key) (plist-get only :key))
                                           (and (plist-get only :conversation)
                                                (equal (plist-get entry :conversation)
                                                       (plist-get only :conversation))))))
                                entries)
                    :key (lambda (entry) (or (plist-get entry :created) 0)) :reverse t)))
    `((count . ,(length kept))
      (scheduled . ,(vconcat (mapcar (lambda (entry) (plist-get entry :data)) (seq-take kept limit))))
      (truncated . ,(and (> (length kept) limit) t))
      (hosts . ,(and hosts
                     (vconcat (mapcar (lambda (host)
                                        (drover-anvil--compact
                                         `((host . ,(plist-get host :host))
                                           (count . ,(and (plist-member host :jobs) (length (plist-get host :jobs))))
                                           (message . ,(drover-anvil--line (plist-get host :failure) 300)))))
                                      hosts)))))))

(defun drover-anvil--tool-scheduled (&optional key status limit host)
  "List the prompts scheduled for later, of every host, the newest first.
KEY, STATUS, LIMIT and HOST narrow the list, as the parameters below
tell.  Each host keeps the prompts of its own sessions, and tells them.

MCP Parameters:
  key - A session's key, whose prompts alone to list, live, archived
      or older
  status - Statuses to keep, comma-separated, among pending, opening,
      waiting, sending, sent, failed, interrupted and cancelled
  limit - (integer) Prompts listed at most, 50 by default
  host - \"local\" for this host's prompts, or a host's name; every
      host's by default"
  (drover-start)
  (require 'drover-schedule)
  (require 'drover-links)
  (drover-schedule-load)
  (let* ((caller (drover-anvil--caller))
         (only (and (stringp key) (not (string-empty-p key)) (drover-anvil--scheduled-target key)))
         (statuses (drover-anvil--split status))
         (limit (max 1 (min 200 (if (natnump limit) limit 50))))
         (host (and (stringp host) (not (string-blank-p host)) host))
         (local (and (drover-anvil--host-match-p nil host)
                     (mapcar #'drover-anvil--local-scheduled (drover-schedule--jobs)))))
    (if (or (not (featurep 'drover-remote)) (member host '("local" "here")))
        (let ((answer (drover-anvil--scheduled-answer local nil only statuses limit)))
          (drover-anvil--log "drover-scheduled" caller nil "done" (format "%d listed" (alist-get 'count answer)))
          (drover-anvil--json answer))
      (let* ((stop nil)
             (id (drover-anvil--job "drover-scheduled" caller nil :seconds 60
                                    :finally (lambda (_id) (when stop (funcall stop))))))
        (setq stop
              (drover-remote-jobs-later
               (lambda (hosts)
                 (drover-with-reporting "Anvil scheduled"
                   (let* ((hosts (seq-filter (lambda (entry) (drover-anvil--host-match-p (plist-get entry :host) host))
                                             hosts))
                          (remote (mapcan (lambda (entry)
                                            (mapcar #'drover-anvil--remote-scheduled (plist-get entry :jobs)))
                                          hosts))
                          (answer (drover-anvil--scheduled-answer (append local remote) hosts only statuses limit)))
                     (drover-anvil--done id (cons `(outcome . ,(format "%d listed" (alist-get 'count answer)))
                                                  answer)))))))
        (drover-anvil--answer id)))))

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
         (until (float (or (drover-anvil--read-time until) (float-time))))
         (since (float (or (drover-anvil--read-time since) (- until (* 7 86400)))))
         (offset (if (natnump offset) offset 0))
         (limit (max 1 (min drover-permissions-limit (if (natnump limit) limit 1000))))
         (stop nil))
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
                                                 (drover-anvil--host-match-p (plist-get prompt :host) host))
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

;;;; Lists of items

(declare-function drover-items-list-of "drover-items" (session))
(declare-function drover-items-items "drover-items" (list))
(declare-function drover-items-asked "drover-items" (list))
(declare-function drover-items-check "drover-items" (session given callback))
(declare-function drover-items-set "drover-items" (session items review))
(declare-function drover-items-find-headings "drover-items" (list callback))
(declare-function drover-items-heading-info "drover-items" (item))
(declare-function drover-items-heading-sessions "drover-items" (item))
(declare-function drover-items-item-status "drover-items" (item))
(declare-function drover-items-goes "drover-items" (item))
(declare-function drover-items-chosen-by "drover-items" (item))
(declare-function drover-items-link-text "drover-items" (item))
(defvar drover-items-limit)

(defconst drover-anvil--item-fields
  '((title . :title) (status . :status) (link . :link) (next . :next) (note . :note)
    (heading . :heading) (proposal . :proposal) (id . :id))
  "The fields of an item an agent gives, by their names, and as drover takes them.")

(defun drover-anvil--items-given (value)
  "Return the items VALUE gives, as `drover-items-check' takes them, or refuse.
VALUE is an array of objects, or a string holding one in JSON, as some
clients send arrays: each object becomes a plist of its fields, :index
its place from 1, :unknown the names of fields an item has not, and
:malformed for a value that is no object."
  (let ((items (cond ((vectorp value) (append value nil))
                     ((null value) nil)
                     ((and (stringp value) (length< value 1000000) (string-match-p "\\`[ \t\r\n]*\\[" value))
                      (condition-case nil
                          (append (json-parse-string value :object-type 'alist :array-type 'array
                                                     :null-object nil :false-object :json-false)
                                  nil)
                        (error (drover-anvil--refuse "items holds no JSON array of objects"))))
                     (t (drover-anvil--refuse "items must be an array of objects, each with a title and a status"))))
        (index 0))
    (when (length> items drover-items-limit)
      (drover-anvil--refuse "items holds %d items: a list holds %d at most" (length items) drover-items-limit))
    (mapcar (lambda (object)
              (cl-incf index)
              (if (not (and (listp object) (seq-every-p (lambda (entry) (and (consp entry) (symbolp (car entry))))
                                                        object)))
                  (list :index index :malformed t)
                (let ((given (list :index index))
                      (unknown nil))
                  (pcase-dolist (`(,name . ,field) object)
                    (if-let* ((key (alist-get name drover-anvil--item-fields)))
                        (setq given (plist-put given key field))
                      (push (symbol-name name) unknown)))
                  (if unknown (plist-put given :unknown (nreverse unknown)) given))))
            items)))

(defun drover-anvil--items-session (key caller)
  "Return the session whose list the tools act on: KEY's, else CALLER's; or refuse.
A session of another host is refused: its host keeps its list."
  (let ((session (if (drover-anvil--given key)
                     (drover-anvil--session key)
                   (or (and caller (drover-index-session caller))
                       (drover-anvil--refuse "Name the session whose list it is (key): the caller is unknown, its client naming no session drover knows")))))
    (when (drover-session-host session)
      (drover-anvil--refuse "%s runs on %s, which keeps its list: its agent calls the tools of the Emacs there"
                            (drover-anvil-key-string session) (drover-host-label (drover-session-host session))))
    session))

(defun drover-anvil--item-data (item)
  "Return ITEM, of a list of `drover-items', as data the tools answer with."
  (let ((info (drover-items-heading-info item))
        (sent (plist-get item :sent))
        (place (plist-get item :place)))
    (drover-anvil--compact
     `((id . ,(plist-get item :id))
       (title . ,(plist-get item :title))
       (status . ,(symbol-name (drover-items-item-status item)))
       (link . ,(drover-items-link-text item))
       (next . ,(plist-get item :next))
       (note . ,(plist-get item :note))
       (heading . ,(when-let* ((id (plist-get item :heading)))
                     (drover-anvil--compact
                      `((id . ,id)
                        (title . ,(drover-anvil--line (if info (plist-get info :title) (plist-get item :heading-title)) 300))
                        (found . ,(if info t :json-false))))))
       (sessions . ,(when-let* ((sessions (drover-items-heading-sessions item)))
                      (vconcat (mapcar (lambda (session)
                                         (drover-anvil--compact
                                          `((key . ,(drover-anvil-key-string session))
                                            (link . ,(drover-anvil--link session))
                                            (title . ,(drover-anvil--line (drover-session-display-title session) 160))
                                            (status . ,(symbol-name (drover-session-status session))))))
                                       (seq-take sessions 10)))))
       (goes . ,(symbol-name (or (drover-items-goes item) 'none)))
       (chosen_by . ,(symbol-name (drover-items-chosen-by item)))
       (place . ,(and place (drover-anvil--compact `((uri . ,(plist-get place :uri)) (name . ,(plist-get place :name))))))
       (sent . ,(and sent `((goes . ,(symbol-name (car sent))) (at . ,(drover-anvil--time (cdr sent))))))))))

(defun drover-anvil--items-answer (session list &optional outcome)
  "Return what the tools answer of SESSION's LIST, as data, with OUTCOME."
  (let ((asked (and list (drover-items-asked list)))
        (items (and list (drover-items-items list))))
    `((outcome . ,(or outcome "read"))
      (key . ,(drover-anvil-key-string session))
      (link . ,(drover-anvil--link session))
      (title . ,(drover-anvil--line (drover-session-display-title session) 160))
      (review . ,(if asked "asked" "none"))
      (asked_since . ,(drover-anvil--time asked))
      (count . ,(length items))
      (items . ,(vconcat (mapcar #'drover-anvil--item-data items)))
      (note . ,(cond
                ((null list) "This session has no list: give one with drover-session-items-set")
                (asked "The user is asked where each item goes; their choices come back to the session as a prompt.  Update an item with drover-session-items-set and its id")
                (t "Update an item with drover-session-items-set and its id; review true asks the user where each goes"))))))

(defun drover-anvil--items-refusal (problems)
  "Return the refusal of a list whose items PROBLEMS tell, each a string."
  (format "Nothing kept: %s.  Items are named by their place in items, from 1.  A link is an Org link to what it names: https://…, [[id:ID][title]] of a heading that exists, agent-session:… of a session drover-sessions lists, file:… of a file that exists; or give none"
          (string-join problems "; ")))

(defun drover-anvil--tool-items-set (items &optional key review)
  "Keep ITEMS as the list of the session KEY, the caller's by default.
Drover keeps the list of a session's items rather than its agent
prints it: across a compaction of the conversation and restarts of
Emacs.  An item given without id is added, numbered; one with the id of
an item the list holds updates it, the fields it gives replacing those
it had; the items not given stay.  Unless REVIEW is false, the user is
asked where each item goes: the session needs the user once its turn
ends, and the choices come back as a prompt.  A call naming a link
that cannot be what it says is refused, each such item named, and
nothing is kept.  It answers at once, with the list as kept.

MCP Parameters:
  items - (array) The items, each an object with title and status, one
      of open, blocked, scheduled, done and decided; and optionally link,
      an Org link to what it names, such as [[id:ID][title]],
      [[https://…][title]] or agent-session:…; next, its next step; note;
      heading, the Org ID of its heading; proposal, where you propose it
      goes: keep, session, fork or save; id, the number of an item the
      list holds, to update it
  key - The session whose list it is, as drover-sessions tells it; the
      caller's own by default
  review - (boolean) true by default, asking the user where each item
      goes; false keeps the list alone"
  (drover-start)
  (require 'drover-items)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--items-session key caller))
         (source (drover-session-key session))
         (given (drover-anvil--items-given items))
         (review (not (drover-anvil--false-p review))))
    (when (and (null given) (null (drover-items-list-of session)))
      (drover-anvil--refuse "%s has no list yet: give its items" (drover-anvil-key-string session)))
    (drover-anvil--log "drover-session-items-set" caller source "acting"
                       (format "%d item%s%s" (length given) (if (length= given 1) "" "s")
                               (if review ", review asked" "")))
    (let ((id (drover-anvil--job "drover-session-items-set" caller source :seconds 60)))
      (drover-anvil--call
       id #'drover-items-check session given
       (lambda (result)
         (drover-anvil--call
          id (lambda ()
               (if (cdr result)
                   (drover-anvil--fail id (drover-anvil--items-refusal (cdr result)))
                 (drover-anvil--done id (drover-anvil--items-answer
                                         session (drover-items-set session (car result) review) "kept")))))))
      (drover-anvil--answer id))))

(defun drover-anvil--tool-items (&optional key)
  "Return the list of items of the session KEY, the caller's by default.
Each item with its number, title, status, link, next step, note, its
heading and the sessions serving it, where it goes and who chose it,
and the place of its heading once the user chose one.  An item with a
heading takes the heading's status.

MCP Parameters:
  key - The session whose list it is, as drover-sessions tells it; the
      caller's own by default"
  (drover-start)
  (require 'drover-items)
  (let* ((caller (drover-anvil--caller))
         (session (drover-anvil--items-session key caller))
         (source (drover-session-key session))
         (list (drover-items-list-of session))
         (id (drover-anvil--job "drover-session-items" caller source :seconds 60)))
    (drover-anvil--log "drover-session-items" caller source "done"
                       (format "%d items" (length (and list (drover-items-items list)))))
    (if (null list)
        (drover-anvil--done id (drover-anvil--items-answer session nil))
      ;; Headings in no file visited are looked for first.
      (drover-anvil--call id #'drover-items-find-headings list
                          (lambda ()
                            (drover-anvil--call id (lambda ()
                                                     (drover-anvil--done id (drover-anvil--items-answer
                                                                             session (drover-items-list-of session))))))))
    (drover-anvil--answer id)))

;;;; Jobs' answers

(defun drover-anvil--tool-job (job)
  "Return the answer of JOB, a job a tool answered with, once it is there.
While the client waits, it waits for the answer; otherwise it tells
the job is still pending.

MCP Parameters:
  job - The job's identifier"
  (unless (and (stringp job) (gethash job drover-anvil--jobs))
    (drover-anvil--refuse "No job %s: its answer was taken, or it was forgotten"
                          (drover-bound-line (format "%s" job) 64)))
  (drover-anvil--answer job))

;;;; Registering

(defconst drover-anvil--tools
  '((drover-anvil--tool-sessions
     :id "drover-sessions" :read-only t
     :description "List the agent sessions drover knows, across harnesses (OpenCode, Claude Code, agent-shell, Codex) and hosts, most urgent first: key, link (the target of its Org link), title, status, what it waits for, harness, host, project, directory, model.  Filter by status, harness, project, host, attention.  The caller's own session is marked caller.")
    (drover-anvil--tool-session
     :id "drover-session" :read-only t
     :description "Read one session by key: its link, status, model, what it waits for, and the recap of its last turn (the user's last prompt, the agent's reply, the files it changed) and the pending requests' identifiers, choices and fields.")
    (drover-anvil--tool-create
     :id "drover-session-create"
     :description "Start an agent session, unseen, on this host or another (host), in a directory (the caller's by default), on a harness (opencode by default), model, effort and agent, and send it a first prompt.  Returns its key and its link; follow its turn with drover-session-wait.")
    (drover-anvil--tool-fork
     :id "drover-session-fork"
     :description "Fork a session whole into a new session, unseen: the caller's own by default.  The fork holds the conversation so far.  Link it to an Org heading as its task (task), and send it a prompt (prompt).  Returns its key and its link.  A harness that cannot fork is refused, and named.")
    (drover-anvil--tool-task-start
     :id "drover-task-start"
     :description "Start the task of an Org heading, by its ID, as the user does on the heading: its place is made (a worktree of its repository, its main checkout or a folder), and a session starts there, unseen, whose first prompt links the heading and quotes its body.  A task working somewhere already continues: another session starts in its place.  Returns the session's key, link, directory and branch.  Nothing is asked: a choice the heading leaves open (repo, directory) is refused, with the values that work.")
    (drover-anvil--tool-send
     :id "drover-session-send"
     :description "Send a prompt to a session, as its front end delivers prompts (OpenCode queues or steers a running turn).  Then wait for its turn with drover-session-wait.")
    (drover-anvil--tool-wait
     :id "drover-session-wait" :read-only t
     :description "Wait until a session's turn ends or it needs the user (a permission or a question), with a timeout; answers the outcome, its status and the recap of the turn.  Emacs never waits: the call does.  Never on the caller itself nor a session waiting on it.")
    (drover-anvil--tool-answer
     :id "drover-session-answer"
     :description "Answer a session's pending question (answers by field key) or permission (decision once, always or reject), or dismiss it.")
    (drover-anvil--tool-interrupt
     :id "drover-session-interrupt"
     :description "Stop a session's running turn.")
    (drover-anvil--tool-archive
     :id "drover-session-archive"
     :description "Archive a session, closing its buffer; its conversation stays.")
    (drover-anvil--tool-after
     :id "drover-session-after"
     :description "Have drover send a prompt to your own session after the next event it knows, such as opencode-restart or emacs-restart.  Use it when a step needs that event, such as tools that load only once the service restarts, then end your turn: the session carries on once it happened.  It asks for the event, as a prompt the user schedules does, and the user can cancel it.")
    (drover-anvil--tool-schedule
     :id "drover-schedule"
     :description "Have drover send a session a prompt later: once (in 90m, tomorrow 7:00, an ISO 8601 time), recurring (every weekday 9am Europe/Paris, every 30m), when its running turn ends, or after an event drover knows.  A session of another host is scheduled there, which reads the time with its own clock and events.  It survives restarts, and the user may cancel it.")
    (drover-anvil--tool-fire
     :id "drover-schedule-fire"
     :description "Tell drover an event happened, by name, releasing the prompts waiting for it, with what happened; or that it will not happen, cancelling them.")
    (drover-anvil--tool-scheduled
     :id "drover-scheduled" :read-only t
     :description "List the prompts scheduled for later, of every host, the newest first: when each goes, recurring or not, its status, its host and session, who scheduled it.")
    (drover-anvil--tool-prompts
     :id "drover-prompts" :read-only t
     :description "Return the prompts the user sent agents over a period (seven days by default, 31 at most), of every harness and host, oldest first: host, engine, conversation, its link, title, directory, time and text.  Subagents' prompts, programs' and what engines add as the user are left out.  For reviews of how the user works with agents.")
    (drover-anvil--tool-permissions
     :id "drover-permissions" :read-only t
     :description "Return the permission prompts agents gave the user over a period, seven days by default and 31 at most, across harnesses and hosts, oldest first, each with its session, action, rules and reply.")
    (drover-anvil--tool-items-set
     :id "drover-session-items-set"
     :description "Hand drover the list of the items of a session, the caller's by default, rather than print it: actions, decisions, follow-ups, each with a title and a status (open, blocked, scheduled, done, decided), and optionally a link, a next step, a note, its Org heading's ID and where you propose it goes (keep, session, fork, save).  Drover keeps it across compactions and restarts, numbers each item, and asks the user where each goes (review false does not); the choices come back as a prompt.  Give an item's id to update it; the items not given stay.  A link that cannot be what it says is refused, each such item named.  Returns the list as kept, at once.")
    (drover-anvil--tool-items
     :id "drover-session-items" :read-only t
     :description "Read the list of items drover keeps for a session, the caller's by default: each item's number, title, status (an item with an Org heading takes the heading's), link, next step, note, heading, the sessions serving that heading, where it goes and who chose it, and the place of its heading.")
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
