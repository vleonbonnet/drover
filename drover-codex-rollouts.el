;;; drover-codex-rollouts.el --- Codex's conversations  -*- lexical-binding: t; -*-

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

;; The `codex' backend: the conversations Codex recorded, one rollout
;; each, sessions/YEAR/MONTH/DAY/rollout-TIME-ID.jsonl under
;; `drover-codex-home', moved to archived_sessions/ once archived.  Every
;; Codex writes them, its CLI, its desktop app and the ACP agent
;; agent-shell runs; the state database newer ones keep beside them is
;; theirs to change, and not every one keeps it, so drover reads the
;; rollouts.  A conversation a front end follows, such as an agent-shell
;; shell, is that front end's session rather than this backend's (see
;; :recorded in `drover-register-backend').
;;
;; A rollout is read as `drover-transcript' reads files: its head tells
;; the session's directory, creation, origin and first prompt (its first
;; record, session_meta, may be longer than the window: its members are
;; found without parsing it); its tail the model and effort of the
;; newest turn and the last activity, the time of its newest event of a
;; turn, not of the records Codex keeps of its own.  A conversation's
;; name is the one session_index.jsonl gave it last, else its first
;; prompt.  Sessions
;; run by `codex exec' are left out, as the CLI's own picker leaves them
;; out, unless `drover-codex-include-non-interactive' says otherwise; a
;; subagent's rollout names its parent.
;;
;; A turn completed, or ended in an error, the user did not see is done,
;; or failed, as an OpenCode session's (see `drover-transcript-unseen-p'),
;; else idle; its newest count of tokens tells the size of its context
;; and of the model's window.  OpenAI tells no lifetime of its prompt
;; cache: none shows.
;;
;; Opening a conversation resumes it in a front end able to
;; (`drover-resume'), agent-shell, loading it through ACP.  Archiving,
;; restoring and deleting one are the CLI's own commands, \"codex
;; archive\", \"unarchive\" and \"delete\", when `drover-codex-program'
;; is found; otherwise drover archives it itself, and cannot delete it.
;;
;; agent-shell's backend installs this one as it installs itself
;; (`drover-codex-rollouts-install').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)
(require 'drover-codex)
(require 'drover-transcript)
(require 'drover-command)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 15))
  (load "drover-core" nil t))

(defcustom drover-codex-program "codex"
  "The Codex CLI, which archives, restores and deletes conversations."
  :type 'string
  :group 'drover)

(defcustom drover-codex-include-non-interactive nil
  "Whether drover lists the conversations `codex exec' ran.
Codex's own picker leaves them out unless asked."
  :type 'boolean
  :group 'drover)

(defcustom drover-codex-history-days nil
  "Days after which a closed Codex conversation counts as older.
As `drover-claude-code-history-days' for Claude Code; nil lists them
all."
  :type '(choice (const :tag "List them all" nil) natnum)
  :group 'drover)

;;;; State

(defvar drover-codex-rollouts--index nil
  "The index of Codex's rollouts, once installed.")

(defun drover-codex-rollouts--current-index ()
  "Return the index of Codex's rollouts, or nil before the backend is installed.
One made by another version of drover, before its files were loaded
anew, is replaced: its timers stop, and the new one, which reads what
was kept of the files, looks at them from now on if the backend
started."
  (let ((index drover-codex-rollouts--index))
    (if (or (null index) (drover-transcript-index-current-p index))
        index
      (drover-stale-layout "index of Codex's rollouts")
      (setq drover-codex-rollouts--index (drover-codex-rollouts--make-index))
      (when (memq 'codex drover--started)
        (drover-transcript-start drover-codex-rollouts--index))
      drover-codex-rollouts--index)))

(defvar drover-codex-rollouts--names nil
  "The names session_index.jsonl gives conversations, as (STAMP . TABLE).
STAMP is the file's (SIZE MTIME INODE) when read, TABLE a hash table of
names by identifier.")

(defvar drover-codex-rollouts--users nil
  "The front ends that installed the backend.")

(defconst drover-codex-rollouts--id-regexp
  "\\([[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\)\\.jsonl\\'"
  "Regexp matching the end of a rollout's name; group 1 is its conversation.")

;;;; Reading rollouts

(defun drover-codex-rollouts--archived-directory ()
  "Return the directory of the rollouts Codex archived."
  (file-name-as-directory (expand-file-name "archived_sessions" drover-codex-home)))

(defun drover-codex-rollouts--archived-file-p (file)
  "Return non-nil when FILE is the rollout of an archived conversation."
  (string-prefix-p (drover-codex-rollouts--archived-directory) file))

(defun drover-codex-rollouts--id (file)
  "Return the conversation rollout FILE records, or nil."
  (and (string-match drover-codex-rollouts--id-regexp file)
       (match-string 1 file)))

(defun drover-codex-rollouts--turn (beg end)
  "Return (MODEL EFFORT) of the newest turn context from BEG to END, or nil."
  (when-let* ((record (drover-transcript-last "\"type\":\"turn_context\""
                                              (lambda (record)
                                                (equal (alist-get 'type record) "turn_context"))
                                              beg end)))
    (let ((payload (alist-get 'payload record)))
      (list (and (stringp (alist-get 'model payload)) (alist-get 'model payload))
            (and (stringp (alist-get 'effort payload)) (alist-get 'effort payload))))))

(defun drover-codex-rollouts--cut-prompt (beg end)
  "Return the first prompt between BEG and END on a line too long to parse, or nil.
A prompt pasting a log may make a line longer than the window it is
read in, which cuts it: the event Codex records of it is read from its
start (see `drover-transcript-string-prefix')."
  (save-excursion
    (goto-char beg)
    (let ((found nil))
      (while (and (not found) (search-forward "\"type\":\"user_message\"" end t))
        (let ((line-beg (line-beginning-position))
              (line-end (min end (line-end-position))))
          (unless (drover-transcript-line)
            (goto-char line-beg)
            (when (re-search-forward "\"message\":[ \t]*" line-end t)
              (setq found (drover-transcript-text
                           (drover-codex-prompt
                            `((type . "event_msg")
                              (payload (type . "user_message")
                                       (message . ,(drover-transcript-string-prefix 2000)))))))))
          (goto-char line-end)
          (forward-line 1)))
      found)))

(defun drover-codex-rollouts--first-prompt (beg end)
  "Return the first prompt of the user between BEG and END, on one line, or nil.
The event Codex records of it comes first; else its message; else the
start of an event on a line too long to parse."
  (or (seq-some (lambda (record) (drover-transcript-text (drover-codex-prompt record)))
                (drover-transcript-lines "\"type\":\"user_message\"" nil beg end))
      (seq-some (lambda (record) (drover-transcript-text (drover-codex-prompt record)))
                (drover-transcript-lines "\"role\":\"user\"" nil beg end))
      (drover-codex-rollouts--cut-prompt beg end)))

(defun drover-codex-rollouts--messages-p (beg end)
  "Return non-nil when a message of the conversation is between BEG and END."
  (save-excursion
    (goto-char beg)
    (and (re-search-forward "\"type\":\"\\(?:user_message\\|agent_message\\)\"\\|\"role\":\"\\(?:user\\|assistant\\)\"" end t)
         t)))

(defconst drover-codex-rollouts--turn-items
  '("message" "agent_message" "reasoning" "local_shell_call" "function_call" "function_call_output"
    "tool_search_call" "tool_search_output" "custom_tool_call" "custom_tool_call_output"
    "web_search_call" "image_generation_call")
  "The types of the items of a turn's exchange Codex records, as response_item.
Not those of its own, as the tools or the configuration it updates.")

(defconst drover-codex-rollouts--turn-events
  '("user_message" "agent_message" "agent_reasoning" "agent_reasoning_raw_content"
    "task_started" "turn_started" "task_complete" "turn_complete" "turn_aborted" "item_completed"
    "exec_command_end" "patch_apply_end" "mcp_tool_call_end" "web_search_end" "image_generation_end"
    "entered_review_mode" "exited_review_mode" "context_compacted" "sub_agent_activity" "error")
  "The types of the events of a turn Codex records, as event_msg.
Not those of its own, as the tokens counted or the settings applied.")

(defun drover-codex-rollouts--event-p (record)
  "Return non-nil when RECORD, parsed, is an event of a turn of the conversation.
That is a message of the user or the agent, or what a turn did, not a
record Codex keeps of its own, as the session's metadata, the context of
a turn, the state of the world or the tokens counted, which it may
record as it resumes the conversation; nor what it tells the model of
its own, the messages of the developer role."
  (let ((payload (alist-get 'payload record)))
    (and (drover-json-object-p payload)
         (pcase (alist-get 'type record)
           ("response_item"
            (let ((type (alist-get 'type payload)))
              (and (member type drover-codex-rollouts--turn-items)
                   (or (not (equal type "message"))
                       (member (alist-get 'role payload) '("user" "assistant"))))))
           ("event_msg" (member (alist-get 'type payload) drover-codex-rollouts--turn-events)))
         t)))

(defun drover-codex-rollouts--last-event (beg end)
  "Return the newest event of a turn between BEG and END, parsed, or nil.
See `drover-codex-rollouts--event-p'."
  (drover-transcript-last
   "\"timestamp\":\""
   (lambda (record)
     (and (drover-codex-rollouts--event-p record)
          (drover-transcript-time (alist-get 'timestamp record))))
   beg end))

(defun drover-codex-rollouts--activity (beg end)
  "Return the time of the newest event of a turn between BEG and END, or nil.
See `drover-codex-rollouts--event-p'."
  (when-let* ((record (drover-codex-rollouts--last-event beg end)))
    (drover-transcript-time (alist-get 'timestamp record))))

(defun drover-codex-rollouts--outcome (record)
  "Return how the turn whose newest event is RECORD left the conversation.
`done' when the turn completed, or its newest event is the agent's
message, as rollouts of a Codex not recording a turn's completion end;
`failed' when it is an error; otherwise `idle': a turn interrupted,
which the user did, or cut, its process gone in the middle."
  (let* ((payload (alist-get 'payload record))
         (type (alist-get 'type payload)))
    (pcase (alist-get 'type record)
      ("event_msg"
       (pcase type
         ((or "task_complete" "turn_complete" "agent_message") 'done)
         ("item_completed"
          (if (equal (alist-get 'type (alist-get 'item payload)) "AgentMessage") 'done 'idle))
         ("error" 'failed)
         (_ 'idle)))
      ("response_item"
       (if (and (equal type "message") (equal (alist-get 'role payload) "assistant")) 'done 'idle))
      (_ 'idle))))

(defun drover-codex-rollouts--end (beg end)
  "Return how the conversation stands at its newest event from BEG to END.
A plist of its time, :updated; :outcome, as
`drover-codex-rollouts--outcome' tells it; :ended, its time again; and
:error, the text of an error; nil without an event of a turn there."
  (when-let* ((record (drover-codex-rollouts--last-event beg end)))
    (let ((time (drover-transcript-time (alist-get 'timestamp record)))
          (outcome (drover-codex-rollouts--outcome record)))
      (list :updated time :outcome outcome :ended time
            :error (and (eq outcome 'failed)
                        (drover-transcript-text (alist-get 'message (alist-get 'payload record))))))))

(defun drover-codex-rollouts--usage (beg end)
  "Return the size of the context the newest count of tokens from BEG to END tells.
A plist of :context, the tokens of the last request, its prompt and
what it generated, which the next one sends again, as Codex counts
those in the context window, and :context-window, the window Codex
gives the model, if told; nil without a count."
  (when-let* ((record (drover-transcript-last
                       "\"type\":\"token_count\""
                       (lambda (record)
                         (let ((payload (alist-get 'payload record)))
                           (and (drover-json-object-p payload)
                                (equal (alist-get 'type payload) "token_count")
                                (drover-json-object-p (alist-get 'info payload))
                                (drover-json-object-p (alist-get 'last_token_usage
                                                                 (alist-get 'info payload))))))
                       beg end)))
    (let* ((info (alist-get 'info (alist-get 'payload record)))
           (last (alist-get 'last_token_usage info))
           (total (alist-get 'total_tokens last))
           (window (alist-get 'model_context_window info)))
      (append (list :context (if (natnump total)
                                 total
                               (+ (let ((input (alist-get 'input_tokens last))) (if (natnump input) input 0))
                                  (let ((output (alist-get 'output_tokens last))) (if (natnump output) output 0)))))
              (and (natnump window) (> window 0) (list :context-window window))))))

(defun drover-codex-rollouts--merge (data told)
  "Return DATA with every member of TOLD, a plist, put in, nil ones included."
  (while told
    (setq data (plist-put data (pop told) (pop told))))
  data)

(defun drover-codex-rollouts--read-head (beg end)
  "Return what the records between BEG and END tell of the session's start.
A plist of its directory, creation time, origin, source, parent, branch,
first prompt and first model; members not told are left out."
  (let ((data nil)
        ;; The session's own members are those of its first record,
        ;; session_meta: the same names recur later, in tool calls.
        (meta (save-excursion (goto-char beg) (min end (line-end-position)))))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value))))
              (sniff (key) (save-excursion (goto-char beg) (drover-transcript-sniff key nil meta))))
      (add :cwd (sniff "cwd"))
      (add :created (drover-transcript-time (sniff "timestamp")))
      (add :originator (sniff "originator"))
      (add :source (or (sniff "source")
                       (save-excursion
                         (goto-char beg)
                         (and (re-search-forward "\"source\":[ \t]*{[ \t]*\"subagent\"" meta t)
                              "subagent"))))
      (add :parent (sniff "parent_thread_id"))
      (add :provider-id (sniff "model_provider"))
      (add :branch (sniff "branch"))
      (add :prompt (drover-codex-rollouts--first-prompt beg end))
      (pcase-let ((`(,model ,effort) (drover-codex-rollouts--turn beg end)))
        (add :model model)
        (add :effort effort))
      (add :messages (drover-codex-rollouts--messages-p beg end)))
    ;; A tail of other records leaves the head's newest event as the
    ;; last activity, and how it left the conversation.
    (drover-codex-rollouts--merge data (append (drover-codex-rollouts--end beg end)
                                               (drover-codex-rollouts--usage beg end)))))

(defun drover-codex-rollouts--read-tail (beg end)
  "Return what the records between BEG and END tell of the conversation's end.
A plist of the newest model and effort there, how its newest event of a
turn left it (see `drover-codex-rollouts--end') and the size of its
context (see `drover-codex-rollouts--usage'); members not told are left
out."
  (let ((data nil))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value)))))
      (pcase-let ((`(,model ,effort) (drover-codex-rollouts--turn beg end)))
        (add :model model)
        (add :effort effort))
      (add :messages (drover-codex-rollouts--messages-p beg end)))
    (drover-codex-rollouts--merge data (append (drover-codex-rollouts--end beg end)
                                               (drover-codex-rollouts--usage beg end)))))

(defun drover-codex-rollouts--read (file attributes old)
  "Return what rollout FILE, of ATTRIBUTES, tells of its conversation.
OLD is the entry read before, or nil: a rollout only appended to since,
the same file grown past its head, is read from where it ended.  Its
last activity is its newest event of a turn's (see
`drover-codex-rollouts--event-p'), looked for further back than the
tail when the tail holds none, never the file's modification time.  For
the index of `drover-transcript'."
  (let* ((size (file-attribute-size attributes))
         (before (and old (drover-transcript-data old)))
         (grown (and before
                     (equal (drover-transcript-inode old) (file-attribute-inode-number attributes))
                     (<= drover-transcript-window (drover-transcript-size old) size)
                     (drover-transcript-size old))))
    (with-temp-buffer
      (let* ((tail (drover-transcript-insert-ends file size grown))
             (data (if grown
                       (copy-sequence before)
                     (drover-codex-rollouts--read-head (point-min) (if (= tail (point-min))
                                                                       (point-max)
                                                                     tail)))))
        (let ((recent (drover-codex-rollouts--read-tail tail (point-max))))
          ;; What the tail does not tell, the lines before it may.
          (setq recent (append recent
                               (drover-transcript-look-before-tail-for
                                file size tail grown
                                (append (unless (plist-get recent :updated) (list #'drover-codex-rollouts--end))
                                        (unless (plist-get recent :context) (list #'drover-codex-rollouts--usage))))))
          (setq data (drover-codex-rollouts--merge data recent)))
        (setq data (plist-put data :id (drover-codex-rollouts--id file)))
        (setq data (plist-put data :archived (drover-codex-rollouts--archived-file-p file)))
        (let ((directory (plist-get data :cwd)))
          (setq data (plist-put data :directory (and directory (file-name-as-directory directory))))
          (setq data (plist-put data :project (drover-transcript-project (plist-get data :directory)))))
        (unless (plist-get data :updated)
          (setq data (plist-put data :updated (plist-get data :created))))
        data))))

;;;; Names

(defun drover-codex-rollouts--names ()
  "Return the names session_index.jsonl gives conversations, by identifier.
The file is read again once it changed, from where it ended when only
appended to: Codex appends a line per name given."
  (let* ((file (expand-file-name "session_index.jsonl" drover-codex-home))
         (attributes (file-attributes file))
         (stamp (and attributes (drover-transcript--stamp attributes)))
         (cached drover-codex-rollouts--names))
    (cond
     ((null stamp) (setq drover-codex-rollouts--names nil))
     ((equal stamp (car cached)))
     (t
      (let* ((grown (and cached
                         (equal (nth 2 stamp) (nth 2 (car cached)))
                         (>= (nth 0 stamp) (nth 0 (car cached)))))
             (table (if grown (copy-hash-table (cdr cached)) (make-hash-table :test #'equal))))
        (with-temp-buffer
          (drover-transcript-insert file (if grown (nth 0 (car cached)) 0) (nth 0 stamp))
          (goto-char (point-min))
          (while (not (eobp))
            (when-let* ((record (drover-transcript-line))
                        (id (alist-get 'id record))
                        ((stringp id)))
              (if-let* ((name (drover-transcript-text (alist-get 'thread_name record))))
                  (puthash id name table)
                (remhash id table)))
            (forward-line 1)))
        (setq drover-codex-rollouts--names (cons stamp table)))))
    (cdr drover-codex-rollouts--names)))

(defun drover-codex-rollouts--check-names ()
  "Read session_index.jsonl again if it changed, reporting the names that did."
  (let* ((before (cdr drover-codex-rollouts--names))
         (after (drover-codex-rollouts--names)))
    (unless (eq before after)
      (let ((changed nil))
        (when after
          (maphash (lambda (id name)
                     (unless (equal name (and before (gethash id before)))
                       (push id changed)))
                   after))
        (when before
          (maphash (lambda (id _) (unless (and after (gethash id after)) (push id changed))) before))
        (dolist (id (delete-dups changed))
          (when (drover-codex-rollouts--file id)
            (drover-backend-changed 'codex id)))))))

;;;; Records

(defun drover-codex-rollouts--listed-p (data)
  "Return non-nil when the rollout read as DATA records a conversation to list."
  (and (plist-get data :messages)
       (plist-get data :id)
       (or drover-codex-include-non-interactive
           (not (member (plist-get data :source) '("exec" "mcp"))))
       t))

(defun drover-codex-rollouts--recent-p (data &optional now)
  "Return non-nil when the conversation of DATA is listed live at NOW.
See `drover-codex-history-days'."
  (or (null drover-codex-history-days)
      (> (or (plist-get data :updated) 0)
         (- (or now (float-time)) (* 86400 drover-codex-history-days)))))

(defconst drover-codex-rollouts--name 'codex-rollouts
  "The name of the index of Codex's rollouts, which what was seen keys.")

(defun drover-codex-rollouts--unseen (data)
  "Return how the conversation of DATA ended, `done' or `failed', unless seen.
Nil when its newest event ended no turn, the user saw it since (see
`drover-transcript-unseen-p'), or `codex exec' ran it, whose reply went
to the program running it."
  (let ((outcome (plist-get data :outcome)))
    (and (memq outcome '(done failed))
         (not (member (plist-get data :source) '("exec" "mcp")))
         (drover-transcript-unseen-p drover-codex-rollouts--name (plist-get data :id)
                                     (plist-get data :ended))
         outcome)))

(defun drover-codex-rollouts--context-limit (data model)
  "Return the context window of the conversation of DATA, running MODEL, or nil.
The window its newest count of tokens tells, else the one Codex's models
cache gives MODEL, the share of it Codex fills (its
effective_context_window_percent), as Codex computes it."
  (or (plist-get data :context-window)
      (when-let* ((entry (and model (drover-codex--catalog-model model)))
                  (window (alist-get 'context_window entry))
                  ((natnump window)))
        (let ((percent (alist-get 'effective_context_window_percent entry)))
          (if (and (numberp percent) (< 0 percent 100))
              (floor (* window percent) 100)
            window)))))

(defun drover-codex-rollouts--provider (data)
  "Return the provider of the conversation of DATA, as arguments of a record.
The one its session's metadata names, OpenAI unless another is told."
  (let ((id (or (plist-get data :provider-id) "openai")))
    (drover-transcript-provider id (pcase id
                                     ("openai" "OpenAI")
                                     ("azure" "Azure")
                                     ("ollama" "Ollama")
                                     ("lmstudio" "LM Studio")
                                     ("oss" "OSS")
                                     (_ id)))))

(defun drover-codex-rollouts--ran-on (data &optional models)
  "Return what the conversation of DATA ran on, as arguments of a record.
Keyword arguments of `drover-session-create': its provider, once a turn
ran, and the size of its context and window.  Not its prompt cache:
OpenAI caches prompts on its own, for a lifetime it does not tell, so no
expiry is known to count down to.  MODELS caches the windows of models,
a hash table, or nil."
  (let ((model (plist-get data :model))
        (tokens (plist-get data :context)))
    (append (and model (drover-codex-rollouts--provider data))
            (and (natnump tokens) (> tokens 0)
                 (list :context-tokens tokens
                       :context-limit (if models
                                          (with-memoization (gethash (cons 'window model) models)
                                            (drover-codex-rollouts--context-limit data model))
                                        (drover-codex-rollouts--context-limit data model)))))))

(defun drover-codex-rollouts--record (data names models)
  "Return the `drover-session' of the conversation DATA tells, or nil.
NAMES are the names conversations were given; MODELS a hash table
caching the names of models, which Codex's models cache gives.  A turn
that ended unseen is done, or failed (see `drover-codex-rollouts--unseen')."
  (when (drover-codex-rollouts--listed-p data)
    (let* ((id (plist-get data :id))
           (model (plist-get data :model))
           (status (or (drover-codex-rollouts--unseen data) 'idle))
           ;; A window counted is the conversation's; a model's, every one's.
           (run (drover-codex-rollouts--ran-on data (and (not (plist-get data :context-window)) models))))
      (drover-session-create
             :backend 'codex :id id
             :engine 'codex :engine-id id
             :parent (plist-get data :parent)
             :title (or (and names (gethash id names)) (plist-get data :prompt) id)
             :directory (plist-get data :directory)
             :project (plist-get data :project)
             :branch (plist-get data :branch)
             :status status
             :since (and (not (eq status 'idle)) (or (plist-get data :ended) (plist-get data :updated)))
             :detail (and (eq status 'failed) (plist-get data :error))
             :updated (plist-get data :updated)
             :created (plist-get data :created)
             :model (and model
                         (with-memoization (gethash model models)
                           (or (alist-get 'display_name (drover-codex--catalog-model model)) model)))
             :effort (plist-get data :effort)
             :archived (and (plist-get data :archived) t)
             :provider (plist-get run :provider)
             :provider-name (plist-get run :provider-name)
             :context-tokens (plist-get run :context-tokens)
             :context-limit (plist-get run :context-limit)))))

(defun drover-codex-rollouts--records (predicate)
  "Return the records of the conversations PREDICATE keeps.
PREDICATE is a function of what was read of a conversation."
  (let ((records nil)
        (names (drover-codex-rollouts--names))
        (models (make-hash-table :test #'equal)))
    (when (drover-codex-rollouts--current-index)
      (drover-transcript-map (lambda (_file data)
                               (when (funcall predicate data)
                                 (when-let* ((record (drover-codex-rollouts--record data names models)))
                                   (push record records))))
                             (drover-codex-rollouts--current-index)))
    records))

(defun drover-codex-rollouts--sessions ()
  "Return the conversations listed live: not archived, of the last days.
The backend's :sessions."
  (let ((now (float-time)))
    (drover-codex-rollouts--records
     (lambda (data)
       (and (not (plist-get data :archived)) (drover-codex-rollouts--recent-p data now))))))

(defun drover-codex-rollouts--history (callback)
  "Call CALLBACK at once with the older conversations, not archived.
See `drover-codex-history-days'.  The backend's :history."
  (let ((now (float-time)))
    (funcall callback
             (and drover-codex-history-days
                  (drover-codex-rollouts--records
                   (lambda (data)
                     (and (not (plist-get data :archived))
                          (not (drover-codex-rollouts--recent-p data now)))))))))

(defun drover-codex-rollouts--archived (callback)
  "Call CALLBACK at once with the conversations Codex archived.
The backend's :archived."
  (funcall callback (drover-codex-rollouts--records (lambda (data) (plist-get data :archived)))))

(defun drover-codex-rollouts--file (id)
  "Return the rollout of conversation ID, or nil.
A conversation archived moves to another file: it is the one it has
now."
  (when-let* ((index (drover-codex-rollouts--current-index)))
    (drover-transcript-keyed-file index id)))

(defun drover-codex-rollouts--data (id)
  "Return what was read of conversation ID, or nil."
  (when-let* ((file (drover-codex-rollouts--file id)))
    (drover-transcript-lookup (drover-codex-rollouts--current-index) file)))

(defun drover-codex-rollouts--times (id)
  "Return (CREATED . UPDATED) of conversation ID, as its rollout tells.
UPDATED is the time of its newest event of a turn; nil when the
rollout was not read.  The backend's :recorded-times."
  (when-let* ((data (drover-codex-rollouts--data id)))
    (cons (plist-get data :created) (plist-get data :updated))))

(defun drover-codex-rollouts--recorded-run (id)
  "Return what conversation ID ran on, as its rollout tells, or nil.
Keyword arguments of `drover-session-create' (see
`drover-codex-rollouts--ran-on'), for a front end following it.  The
backend's :recorded-run."
  (when-let* ((data (drover-codex-rollouts--data id)))
    (drover-codex-rollouts--ran-on data)))

(defun drover-codex-rollouts--seen-now (id)
  "Return when the user sees conversation ID now, as its rollout dates events.
Now, or its newest event's time when later (see
`drover-claude-code-transcripts--seen-now')."
  (max (float-time) (or (plist-get (drover-codex-rollouts--data id) :ended) 0)))

(defun drover-codex-rollouts--mark-seen (session)
  "Mark the ended turn of the conversation of SESSION seen, for good.
The backend's :mark-seen."
  (let ((id (drover-session-id session)))
    (when (drover-transcript-mark-seen drover-codex-rollouts--name id
                                       (drover-codex-rollouts--seen-now id))
      (drover-backend-changed 'codex id))))

(defun drover-codex-rollouts--recorded-seen (id)
  "Note that the user saw conversation ID, which a front end follows, idle.
As `drover-claude-code-transcripts--recorded-seen' notes it.  The
backend's :recorded-seen."
  (let* ((file (drover-codex-rollouts--file id))
         (data (and file (drover-transcript-lookup (drover-codex-rollouts--current-index) file))))
    (when (or (null data)
              (drover-codex-rollouts--unseen data)
              (drover-transcript-check (drover-codex-rollouts--current-index) file))
      (drover-transcript-mark-seen drover-codex-rollouts--name id (drover-codex-rollouts--seen-now id)))))

(defun drover-codex-rollouts--session (id)
  "Return the record of conversation ID, as :sessions lists it, or nil.
Its rollout is looked at again soon when it changed since it was read.
The backend's :session."
  (let* ((file (drover-codex-rollouts--file id))
         (data (and file (drover-transcript-lookup (drover-codex-rollouts--current-index) file))))
    (when file
      (drover-transcript-check (drover-codex-rollouts--current-index) file))
    (when (and data (not (plist-get data :archived)) (drover-codex-rollouts--recent-p data))
      (drover-codex-rollouts--record data (drover-codex-rollouts--names)
                                     (make-hash-table :test #'equal)))))

;;;; Following the rollouts

(defun drover-codex-rollouts--changed (_file old new)
  "Report that what was read of a rollout went from OLD to NEW."
  (let ((id (plist-get (or new old) :id)))
    (when id
      (drover-backend-changed 'codex id)
      (when (and (or (plist-get old :archived) (plist-get new :archived)) drover-show-archived)
        (drover-archived-fetch 'codex)))))

(defun drover-codex-rollouts--looked ()
  "Follow the end of a look at the rollouts: names given meanwhile."
  (drover-codex-rollouts--check-names))

(defun drover-codex-rollouts--roots ()
  "Return where Codex's rollouts are, for the index."
  (let ((regexp "\\`rollout-.*\\.jsonl\\'"))
    (list (list (expand-file-name "sessions" drover-codex-home) 3 regexp)
          (list (drover-codex-rollouts--archived-directory) 0 regexp))))

(defconst drover-codex-rollouts--reader-version 3
  "Version of `drover-codex-rollouts--read'.
Raised when it reads anew what it read before, so that what an older
one read and kept between Emacs sessions is read again.")

(defun drover-codex-rollouts--make-index ()
  "Return a new index of Codex's rollouts."
  (drover-transcript-index-create 'codex-rollouts
                                  #'drover-codex-rollouts--roots
                                  #'drover-codex-rollouts--read
                                  #'drover-codex-rollouts--changed
                                  #'drover-codex-rollouts--looked
                                  #'drover-transcript-id
                                  drover-codex-rollouts--reader-version))

;;;; Searching

(defconst drover-codex-rollouts--text-starts
  '("\"type\":\"(?:user_message|agent_message)\",\"message\":\""
    "\"type\":\"message\",(?:\"id\":\"[^\"]*\",)?\"role\":\"(?:user|assistant)\",\"content\":\\[\\{\"type\":\"(?:input_text|output_text)\",\"text\":\"")
  "What begins the text of a prompt or reply in a rollout, for ripgrep.
The events of a prompt or reply, and the messages of the user or the
assistant of a turn's items, as Codex records them: tool calls, their
outputs and reasoning begin otherwise.")

(defun drover-codex-rollouts--search-text (session)
  "Return the first prompt of the conversation of SESSION, or nil.
As its rollout tells it, for any backend's session of Codex's
conversation.  The backend's :search-text."
  (when-let* ((id (drover-session-engine-id session))
              ((eq (drover-session-engine session) 'codex))
              (data (drover-codex-rollouts--data id)))
    (delq nil (list (plist-get data :prompt)))))

(defun drover-codex-rollouts--search (query scope callback)
  "Search the rollouts for QUERY said in prompts and replies, for CALLBACK.
Those of the conversations the search offers, as the :wanted of SCOPE
tells, archived ones while shown, are searched with ripgrep (see
`drover-transcript-search'); CALLBACK gets matches naming each
conversation by its engine and identifier.  The backend's :search."
  (when-let* ((index (drover-codex-rollouts--current-index)))
    (let ((wanted (plist-get scope :wanted))
          (files nil))
      (drover-transcript-map
       (lambda (file data)
         (let ((id (plist-get data :id)))
           (when (and id (plist-get data :messages)
                      (or (not (functionp wanted))
                          (funcall wanted (list :engine 'codex :id id))))
             (push (list file id (plist-get data :updated)) files))))
       index)
      (drover-transcript-search
       files drover-codex-rollouts--text-starts query
       (lambda (result)
         (funcall callback (if (and (consp result) (consp (car result)))
                               (mapcar (lambda (match) (append (list :engine 'codex) match)) result)
                             result)))
       (plist-get scope :limit)))))

;;;; Actions

(defun drover-codex-rollouts--open (session other-window)
  "Show the conversation of SESSION, resuming it in a front end.
One a front end follows already shows there, in another window when
OTHER-WINDOW is non-nil.  The backend's :open."
  (let* ((id (drover-session-id session))
         (data (or (drover-codex-rollouts--data id)
                   (user-error "No rollout of %s" (drover-session-display-title session))))
         (followed (drover-find-engine-session 'codex id)))
    (if (and followed (not (eq (drover-session-backend followed) 'codex)))
        (drover-backend-call followed :open other-window)
      (let ((directory (plist-get data :directory)))
        (unless (and directory (file-directory-p directory))
          (user-error "The directory of %s is gone: %s" (drover-session-display-title session)
                      (or directory "unknown")))
        (drover-resume 'codex id directory)
        ;; Resumed, its turn shows: seen.
        (drover-codex-rollouts--mark-seen session)))))

(defun drover-codex-rollouts--recap (session callback)
  "Call CALLBACK at once with a recap of the last turn of SESSION.
It is read from the end of its rollout (see `drover-codex-recap').  The
backend's :recap."
  (let ((file (drover-codex-rollouts--file (drover-session-id session))))
    (funcall callback
             (or (and file (file-readable-p file)
                      (drover-with-reporting "Codex recap"
                        (drover-codex-recap file)))
                 (list :failure "No rollout of this conversation")))))

(defun drover-codex-rollouts--preview (session callback)
  "Call CALLBACK with the newest reply of SESSION, from its rollout, or nil."
  (drover-codex-rollouts--recap session (lambda (recap) (funcall callback (plist-get recap :reply)))))

(defun drover-codex-rollouts--program ()
  "Return the Codex CLI's executable, or nil when it is not found."
  (and drover-codex-program (executable-find drover-codex-program)))

(defun drover-codex-rollouts-command (command id)
  "Return the command line running the Codex CLI's COMMAND on conversation ID.
COMMAND is a string such as \"archive\"; the conversation is named by
its identifier, which the CLI takes before names."
  (list (or (drover-codex-rollouts--program) drover-codex-program) command id))

(defun drover-codex-rollouts--run (session command done)
  "Run the Codex CLI's COMMAND on the conversation of SESSION, without waiting.
DONE, a past participle such as \"archived\", tells the outcome once it
ran; a failure is reported with what the CLI said.  The rollouts are
looked at again either way."
  (let ((title (drover-session-display-title session)))
    (drover-command-run
     (concat "codex " command)
     (drover-codex-rollouts-command command (drover-session-id session))
     (lambda (result)
       (unwind-protect
           (if (eql (plist-get result :exit) 0)
               (drover-message "Codex %s %s" done title)
             (drover-warn "Codex could not %s %s: %s" command title
                          (or (plist-get result :failure)
                              (let ((said (string-trim (concat (plist-get result :errors) "\n"
                                                               (plist-get result :output)))))
                                (if (string-empty-p said)
                                    (format "exit status %s" (plist-get result :exit))
                                  said)))))
         (when-let* ((index (drover-codex-rollouts--current-index)))
           (drover-transcript-look-soon index)))))))

(defun drover-codex-rollouts--archive (session)
  "Archive the conversation of SESSION with \"codex archive\".
The backend's :archive."
  (drover-codex-rollouts--run session "archive" "archived"))

(defun drover-codex-rollouts--unarchive (session)
  "Restore the conversation of SESSION with \"codex unarchive\".
The backend's :unarchive."
  (drover-codex-rollouts--run session "unarchive" "restored"))

(defun drover-codex-rollouts--delete (session)
  "Delete the conversation of SESSION with \"codex delete\".
The backend's :delete."
  (drover-codex-rollouts--run session "delete" "deleted"))

(defun drover-codex-rollouts--supports (_session operation)
  "Return non-nil when OPERATION applies, as the backend's :supports.
Archiving, restoring and deleting are the Codex CLI's, when found."
  (if (memq operation '(:archive :unarchive :delete))
      (and (drover-codex-rollouts--program) t)
    t))

;;;; Installing

(defun drover-codex-rollouts--start ()
  "Look at the rollouts from now on.  The backend's :start.
The first time, note it: replies before count as seen (see
`drover-transcript-seen-since')."
  (drover-transcript-seen-since drover-codex-rollouts--name)
  (drover-transcript-start (drover-codex-rollouts--current-index)))

(defun drover-codex-rollouts--register ()
  "Register the `codex' backend."
  (drover-register-backend
   'codex
   :label "Codex"
   :recorded '(codex)
   :recorded-times #'drover-codex-rollouts--times
   :recorded-run #'drover-codex-rollouts--recorded-run
   :recorded-seen #'drover-codex-rollouts--recorded-seen
   :start #'drover-codex-rollouts--start
   :sessions #'drover-codex-rollouts--sessions
   :session #'drover-codex-rollouts--session
   :history #'drover-codex-rollouts--history
   :archived #'drover-codex-rollouts--archived
   :search #'drover-codex-rollouts--search
   :search-text #'drover-codex-rollouts--search-text
   :open #'drover-codex-rollouts--open
   :preview #'drover-codex-rollouts--preview
   :recap #'drover-codex-rollouts--recap
   :mark-seen #'drover-codex-rollouts--mark-seen
   :archive #'drover-codex-rollouts--archive
   :unarchive #'drover-codex-rollouts--unarchive
   :delete #'drover-codex-rollouts--delete
   :supports #'drover-codex-rollouts--supports))

(defun drover-codex-rollouts--save ()
  "Keep what was read of the rollouts, as Emacs exits.
An error is reported, and lets Emacs exit."
  (drover-with-reporting "keeping Codex's rollouts"
    (when-let* ((index (drover-codex-rollouts--current-index)))
      (drover-transcript-save index t))
    (drover-transcript-seen-save)))

(defun drover-codex-rollouts-install (front-end)
  "List Codex's conversations, for FRONT-END to resume them.
The backend stays until every front end that installed it uninstalled
it.  Installed again, as a front end loaded anew installs it, it is
registered anew, so that its registration has this file's properties."
  (unless (drover-codex-rollouts--current-index)
    (setq drover-codex-rollouts--index (drover-codex-rollouts--make-index)))
  (drover-codex-rollouts--register)
  (cl-pushnew front-end drover-codex-rollouts--users)
  (add-hook 'kill-emacs-hook #'drover-codex-rollouts--save))

(defun drover-codex-rollouts-uninstall (front-end)
  "Stop listing Codex's conversations for FRONT-END.
The backend goes once no front end needs it."
  (setq drover-codex-rollouts--users (delq front-end drover-codex-rollouts--users))
  (unless drover-codex-rollouts--users
    (when (drover-codex-rollouts--current-index)
      (drover-transcript-stop (drover-codex-rollouts--current-index)))
    (remove-hook 'kill-emacs-hook #'drover-codex-rollouts--save)
    (drover-unregister-backend 'codex)))

;; Loaded anew, as reloading drover does, the backend a front end
;; installed takes this file's properties.
(when (eq (drover-backend-property 'codex :sessions) #'drover-codex-rollouts--sessions)
  (drover-codex-rollouts--register))

(provide 'drover-codex-rollouts)
;;; drover-codex-rollouts.el ends here
