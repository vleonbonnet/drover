;;; drover-claude-code-transcripts.el --- Claude Code's closed conversations  -*- lexical-binding: t; -*-

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

;; The `claude-code' backend: the conversations Claude Code recorded,
;; one transcript each under ~/.claude/projects/BUCKET/ID.jsonl, BUCKET
;; the directory it ran in, encoded (see
;; `drover-claude-code-encode-directory').  It lists every conversation,
;; whichever front end ran it, in Emacs or not, as the CLI's own
;; /resume picker does; a conversation a front end follows, such as a
;; claude-code-ide terminal or an agent-shell shell, is that front end's
;; session rather than this backend's (see :recorded in
;; `drover-register-backend'), so that closing it leaves the
;; conversation listed, idle.
;;
;; A transcript is read as the CLI's picker reads it, its head and its
;; tail, a window of `drover-transcript-window' bytes each, and then
;; only what was appended since (see `drover-transcript'): the title
;; the user gave (custom-title), else the one the CLI generated
;; (ai-title), as its desktop app shows it, not the name of the agent
;; it gives a session as its plan is approved (agent-name), else the
;; first prompt; the directory, branch and
;; creation time of its first records; the model, effort and permission
;; mode of its newest turn; its last activity, the time of its newest
;; message, not of the records the CLI appends of its own as the
;; conversation is opened again.  Its subagents' transcripts beside it,
;; under ID/subagents/, are its subagents, each running the model and
;; effort its own transcript tells.
;;
;; It shows as an OpenCode session does: a turn that ended in a reply,
;; or an error of the API, the user did not see is done, or failed (see
;; `drover-transcript-unseen-p'), else idle; its newest reply tells the
;; size of its context and its prompt cache, which expires an hour or
;; five minutes after its last request, as Claude Code asked for it
;; (see `drover-claude-code-transcripts--usage').  A compaction since
;; tells the size of the summary it left, estimated, and no cache: the
;; next request sends the summary, which nothing cached begins with.
;; Front ends following a conversation take those from here
;; (:recorded-run).
;;
;; Claude Code compacts a large conversation of its own as it sits idle,
;; shortly before its prompt cache expires: the summary is no message,
;; and the conversation stays as it stood after its last turn (see
;; `drover-claude-code-transcripts-work-between').
;;
;; Opening a conversation resumes it in a front end (`drover-resume'):
;; claude-code-ide runs \"claude --resume ID\" in a terminal, agent-shell
;; loads it through ACP; the front end it ran in, as its transcript's
;; entry point tells, comes first.  The recap of its last turn is read
;; from its transcript; drover archives it, as Claude Code has no
;; archive; deleting it deletes what the CLI's SDK deletes.
;;
;; Sessions the CLI runs in the background, or in another terminal,
;; `claude agents --json' lists, a command run now and then while a list
;; shows (`drover-claude-code-agents-interval'): their status comes from
;; there.
;;
;; The front ends' backends install this one as they install
;; themselves (`drover-claude-code-transcripts-install').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)
(require 'drover-claude-code)
(require 'drover-transcript)
(require 'drover-command)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 28))
  (load "drover-core" nil t))

(declare-function drover-claude-code-ide-fork "drover-claude-code-ide" (session-id directory))

(defcustom drover-claude-code-history-days nil
  "Days after which a closed Claude Code conversation counts as older.
A conversation idle for longer is listed among the older sessions, which
lists show on demand (see `drover-show-history'); nil lists them all.
Claude Code deletes its transcripts after its own `cleanupPeriodDays',
30 by default."
  :type '(choice (const :tag "List them all" nil) natnum)
  :group 'drover)

(defcustom drover-claude-code-program "claude"
  "The Claude Code CLI, which tells the sessions it runs in the background."
  :type 'string
  :group 'drover)

(defcustom drover-claude-code-agents-interval 30
  "Seconds between two runs of \"claude agents --json\", or nil for none.
It lists the sessions Claude Code runs in the background or in another
terminal, which drover shows working or waiting; it runs only while a
list of drover's shows."
  :type '(choice (const :tag "Never" nil) number)
  :group 'drover)

;;;; State

(defconst drover-claude-code-transcripts--reader-version 7
  "Version of `drover-claude-code-transcripts--read'.
Raised when it reads anew what it read before, so that what an older
one read and kept between Emacs sessions is read again.")

(defvar drover-claude-code-transcripts--index nil
  "The index of Claude Code's transcripts, once installed.")

(defun drover-claude-code-transcripts--current-index ()
  "Return the index of Claude Code's transcripts, or nil before installed.
One made by another version of drover, before its files were loaded
anew, is replaced: its timers stop, and the new one, which reads what
was kept of the files, looks at them from now on if the backend
started."
  (let ((index drover-claude-code-transcripts--index))
    (if (or (null index) (drover-transcript-index-current-p index))
        (progn
          ;; Read by the reader loaded before this one: read again.
          (when index
            (drover-transcript-reread index drover-claude-code-transcripts--reader-version))
          index)
      (drover-stale-layout "index of Claude Code's transcripts")
      (setq drover-claude-code-transcripts--index (drover-claude-code-transcripts--make-index))
      (when (memq 'claude-code drover--started)
        (drover-transcript-start drover-claude-code-transcripts--index))
      drover-claude-code-transcripts--index)))

(defvar drover-claude-code-transcripts--agents (make-hash-table :test #'equal)
  "The sessions \"claude agents\" listed last, by conversation, as plists.")

(defvar drover-claude-code-transcripts--recent nil
  "The conversations listed live at the end of the last look, or nil.
A hash table, kept only while `drover-claude-code-history-days' is set.")

(defvar drover-claude-code-transcripts--users nil
  "The front ends that installed the backend.")

(defvar drover-claude-code-transcripts--agents-job nil
  "The command listing the sessions Claude Code runs.
Its identifier, as `drover-command-run' returned it, or nil.")

(defvar drover-claude-code-transcripts--agents-time 0
  "When \"claude agents\" last ran, as a float time.")

;;;; Reading transcripts

(defun drover-claude-code-transcripts--id (file)
  "Return the conversation transcript FILE records, its base name."
  (file-name-base file))

(defun drover-claude-code-transcripts--prompt (record)
  "Return the prompt of the user RECORD gives, on one line, or nil.
Records of subagents give none."
  (unless (alist-get 'isSidechain record)
    (drover-transcript-text (drover-claude-code--prompt-p record))))

(defun drover-claude-code-transcripts--cut-prompt (beg end)
  "Return the first prompt between BEG and END on a line too long to parse.
A prompt pasting a log may make a line longer than the window it is
read in, which cuts it: its text is read from its start (see
`drover-transcript-string-prefix'), as long as the start tells it is a
prompt of the user, on one line, or nil."
  (save-excursion
    (goto-char beg)
    (let ((found nil))
      (while (and (not found) (search-forward "\"type\":\"user\"" end t))
        (let ((line-beg (line-beginning-position))
              (line-end (min end (line-end-position))))
          (unless (drover-transcript-line)
            (goto-char line-beg)
            (when (search-forward "\"content\":" line-end t)
              (skip-chars-forward " \t" line-end)
              (let* ((blocks (eq (char-after) ?\[))
                     (content (cond ((not blocks) (drover-transcript-string-prefix 2000))
                                    ((and (re-search-forward "\"\\(text\\|tool_result\\)\"" line-end t)
                                          (equal (match-string 1) "text")
                                          (re-search-forward "\"text\":[ \t]*" line-end t))
                                     (drover-transcript-string-prefix 2000)))))
                (setq found (drover-claude-code-transcripts--prompt
                             `((type . "user")
                               (isSidechain . ,(save-excursion
                                                 (goto-char line-beg)
                                                 (and (search-forward "\"isSidechain\":true" line-end t) t)))
                               ;; A compaction's summary is often too long a line.
                               (isCompactSummary . ,(save-excursion
                                                      (goto-char line-beg)
                                                      (and (search-forward "\"isCompactSummary\":true" line-end t)
                                                           t)))
                               (message (content . ,content))))))))
          (goto-char line-end)
          (forward-line 1)))
      found)))

(defun drover-claude-code-transcripts--member (marker key &optional beg end)
  "Return KEY of the last record with MARKER between BEG and END, as text."
  (when-let* ((record (drover-transcript-last marker nil beg end)))
    (drover-transcript-text (alist-get key record))))

(defun drover-claude-code-transcripts--turn-record (beg end &optional sidechain)
  "Return the newest record of a reply of a main turn from BEG to END, or nil.
That is an assistant's record of the conversation, not of a subagent,
of a model the API ran, not one the CLI made up (\"<synthetic>\").
With SIDECHAIN, a subagent's reply counts too, as in the subagent's
own transcript, whose records are all its own."
  (drover-transcript-last
   "\"type\":\"assistant\""
   (lambda (record)
     (and (equal (alist-get 'type record) "assistant")
          (or sidechain (not (alist-get 'isSidechain record)))
          (let ((model (or (alist-get 'requestedModel record)
                           (alist-get 'model (alist-get 'message record)))))
            (and (stringp model) (not (string-prefix-p "<" model))))))
   beg end))

(defun drover-claude-code-transcripts--turn (beg end &optional sidechain)
  "Return (MODEL EFFORT TIME) of the newest main turn from BEG to END, or nil.
With SIDECHAIN, of a subagent's turn too (see
`drover-claude-code-transcripts--turn-record')."
  (when-let* ((record (drover-claude-code-transcripts--turn-record beg end sidechain)))
    (list (or (alist-get 'requestedModel record) (alist-get 'model (alist-get 'message record)))
          (and (stringp (alist-get 'effort record)) (alist-get 'effort record))
          (drover-transcript-time (alist-get 'timestamp record)))))

(defun drover-claude-code-transcripts--tokens (usage key)
  "Return the tokens USAGE, a reply's usage, counts under KEY, or 0."
  (let ((value (alist-get key usage)))
    (if (natnump value) value 0)))

(defun drover-claude-code-transcripts--write-ttl (usage)
  "Return the lifetime of the prompt cache USAGE tells a reply wrote, or nil.
Claude Code asks for a cache of an hour or of five minutes, and the API
counts the tokens written into each apart: 3600 seconds when it wrote
any into the hour's, else 300 when it wrote into the five minutes',
else nil, for a request that wrote nothing."
  (let ((creation (alist-get 'cache_creation usage)))
    (cond ((> (drover-claude-code-transcripts--tokens creation 'ephemeral_1h_input_tokens) 0) 3600)
          ((or (> (drover-claude-code-transcripts--tokens creation 'ephemeral_5m_input_tokens) 0)
               (> (drover-claude-code-transcripts--tokens usage 'cache_creation_input_tokens) 0))
           300))))

(defun drover-claude-code-transcripts--usage-record-p (record &optional sidechain)
  "Return non-nil when RECORD is the reply of a main turn telling its usage.
With SIDECHAIN, a subagent's reply counts too (see
`drover-claude-code-transcripts--turn-record')."
  (and (equal (alist-get 'type record) "assistant")
       (or sidechain (not (alist-get 'isSidechain record)))
       (let ((message (alist-get 'message record)))
         (and (drover-json-object-p message)
              (not (equal (alist-get 'model message) "<synthetic>"))
              (drover-json-object-p (alist-get 'usage message))))))

(defun drover-claude-code-transcripts--requested (record beg end)
  "Return when the request RECORD replies to was sent, as far as BEG to END tell.
The request follows the record its reply's first record has for parent,
the prompt or a tool's result: its time, else that of the reply's first
record, else RECORD's own.  Those records are looked for from BEG to
END."
  (let* ((id (alist-get 'id (alist-get 'message record)))
         (first (and (stringp id)
                     (car (seq-filter (lambda (other)
                                        (equal (alist-get 'id (alist-get 'message other)) id))
                                      (drover-transcript-lines (concat "\"id\":\"" id "\"") nil beg end)))))
         (parent (alist-get 'parentUuid (or first record)))
         (before (and (stringp parent)
                      (seq-find (lambda (other) (equal (alist-get 'uuid other) parent))
                                (drover-transcript-lines (concat "\"uuid\":\"" parent "\"") t beg end)))))
    (or (drover-transcript-time (alist-get 'timestamp before))
        (drover-transcript-time (alist-get 'timestamp first))
        (drover-transcript-time (alist-get 'timestamp record)))))

(defconst drover-claude-code-transcripts--usage-marker "\"cache_read_input_tokens\":"
  "What the line of a reply telling its usage holds, and few others.
Looking for it, rather than for every reply, parses no line of a reply
without usage, nor of a prompt.")

(defun drover-claude-code-transcripts--prompt-tokens (usage)
  "Return the tokens of the prompt USAGE, a reply's usage, counts.
Those uncached, written into the cache and read from it, as the API
counts what the next request sends again."
  (+ (drover-claude-code-transcripts--tokens usage 'input_tokens)
     (drover-claude-code-transcripts--tokens usage 'cache_creation_input_tokens)
     (drover-claude-code-transcripts--tokens usage 'cache_read_input_tokens)))

(defconst drover-claude-code-transcripts--compaction-marker "\"subtype\":\"compact_boundary\""
  "What the record of a compaction holds, and no other.
Within a JSON string its quotes are escaped: a message quoting it does
not hold it.")

(defun drover-claude-code-transcripts--compaction-p (record)
  "Return non-nil when RECORD is the record of a compaction of the conversation.
Claude Code records one as it replaces the conversation with a summary
of it: as the user types /compact, as the context fills up, or of its
own, as the conversation sits idle before its prompt cache expires.
A subagent's tells nothing of the conversation."
  (and (equal (alist-get 'type record) "system")
       (equal (alist-get 'subtype record) "compact_boundary")
       (not (alist-get 'isSidechain record))))

(defun drover-claude-code-transcripts--compacted (record)
  "Return what the compaction RECORD leaves the conversation running on.
The plist `drover-claude-code-transcripts--usage' returns until a reply
tells more: :context, the size of the summary the compaction left, as
Claude Code estimates it (postTokens), with :context-estimated, or nil
without one; no prompt cache, as the next request sends the summary,
which nothing cached begins with; and :compacted, a plist of when the
compaction ran, :start and :end, and :own, non-nil unless the user
asked for it."
  (let* ((metadata (alist-get 'compactMetadata record))
         (metadata (and (drover-json-object-p metadata) metadata))
         (post (alist-get 'postTokens metadata))
         (post (and (natnump post) (> post 0) post))
         (duration (alist-get 'durationMs metadata))
         (end (drover-transcript-time (alist-get 'timestamp record))))
    (list :context post :context-estimated (and post t)
          :cache-tokens nil :cache-prompt-tokens nil :requested nil :cache-ttl nil
          :compacted (and end
                          (list :start (if (and (numberp duration) (> duration 0))
                                           (- end (/ duration 1000.0))
                                         end)
                                :end end
                                :own (not (equal (alist-get 'trigger metadata) "manual")))))))

(defun drover-claude-code-transcripts--usage (beg end)
  "Return what the newest reply of a main turn from BEG to END tells it ran on.
A plist of :context, the tokens of its prompt, uncached, written into
the cache and read from it, as the API counts what the next request
sends again, and :context-estimated, nil; :cache-tokens, those of the
prompt cache, read or written; :cache-prompt-tokens, those of the
prompt; :requested, when its request was sent (see
`drover-claude-code-transcripts--requested'), which starts the cache's
lifetime anew; :cache-ttl, that lifetime, the one the newest request
writing the cache asked for (see
`drover-claude-code-transcripts--write-ttl'), if one is there: a request
reading the cache without writing keeps the lifetime it was written
with; and :compacted, nil.  A compaction newer than any such reply
tells instead what it left (see `drover-claude-code-transcripts--compacted'):
the conversation before it is no longer sent, nor its replies' usage of
any account, nor the cache they wrote.  Nil without either."
  (let* ((boundary (drover-transcript-last-line drover-claude-code-transcripts--compaction-marker
                                                #'drover-claude-code-transcripts--compaction-p
                                                beg end))
         (from (if boundary (car boundary) beg))
         (record (drover-transcript-last drover-claude-code-transcripts--usage-marker
                                         #'drover-claude-code-transcripts--usage-record-p
                                         from end)))
    (cond
     (record
      (let* ((usage (alist-get 'usage (alist-get 'message record)))
             (written (drover-claude-code-transcripts--tokens usage 'cache_creation_input_tokens))
             (read (drover-claude-code-transcripts--tokens usage 'cache_read_input_tokens))
             (prompt (drover-claude-code-transcripts--prompt-tokens usage))
             (ttl (or (drover-claude-code-transcripts--write-ttl usage)
                      (when-let* ((writer (drover-transcript-last
                                           drover-claude-code-transcripts--usage-marker
                                           (lambda (record)
                                             (and (drover-claude-code-transcripts--usage-record-p record)
                                                  (drover-claude-code-transcripts--write-ttl
                                                   (alist-get 'usage (alist-get 'message record)))))
                                           from end)))
                        (drover-claude-code-transcripts--write-ttl
                         (alist-get 'usage (alist-get 'message writer)))))))
        ;; Told whole, nil members too: they replace what an older read
        ;; told, of a compaction among them.  Past a compaction, the
        ;; lifetime of a cache written before it tells nothing.
        (append (list :context prompt :context-estimated nil
                      :cache-tokens (+ written read)
                      :cache-prompt-tokens prompt
                      :requested (drover-claude-code-transcripts--requested record beg end)
                      :compacted nil)
                (and (or ttl boundary) (list :cache-ttl ttl)))))
     (boundary (drover-claude-code-transcripts--compacted (cdr boundary))))))

(defconst drover-claude-code-transcripts--batch-entrypoints '("sdk-cli" "sdk-py")
  "Entry points of Claude Code run by a program rather than a user.
\"claude -p\" and the Python SDK: their replies went to the program.")

(defun drover-claude-code-transcripts--ending (record)
  "Return how the conversation whose newest message is RECORD stands.
A plist of :outcome, `done' when it is a reply ending its turn, `failed'
when it is an error of the API, otherwise `idle': a prompt left without
reply, a tool's result, a reply cut by its terminal closing, the note
of an interruption or of a command such as /exit, after which the user
saw what came before; :ended, its time; and :error, the error's text.
A turn of a program, \"claude -p\" or an SDK, is idle: its reply went to
the program (see `drover-claude-code-transcripts--batch-entrypoints')."
  (let ((message (alist-get 'message record)))
    (append
     (list :outcome
           (cond ((not (equal (alist-get 'type record) "assistant")) 'idle)
                 ((member (alist-get 'entrypoint record) drover-claude-code-transcripts--batch-entrypoints)
                  'idle)
                 ((alist-get 'isApiErrorMessage record) 'failed)
                 ((member (alist-get 'stop_reason message) '(nil "tool_use" "pause_turn")) 'idle)
                 (t 'done))
           :ended (drover-transcript-time (alist-get 'timestamp record)))
     (when (alist-get 'isApiErrorMessage record)
       (let ((content (alist-get 'content message)))
         (list :error (drover-transcript-text
                       (cond ((stringp content) content)
                             ((listp content)
                              (seq-some (lambda (block)
                                          (and (drover-json-object-p block)
                                               (stringp (alist-get 'text block))
                                               (alist-get 'text block)))
                                        content))))))))))

(defun drover-claude-code-transcripts--message-p (record)
  "Return non-nil when RECORD is a message of the conversation, parsed.
That is a prompt, a reply or a tool's result, not what the CLI records
of its own as it opens the conversation again: a note (isMeta), a
record kept for the transcript only, and the reply it makes up for a
prompt left without one; nor the summary a compaction leaves (see
`drover-claude-code--summary-p'), which the CLI writes of its own too,
as the conversation sits idle.  Its API errors are messages: they end
a turn."
  (pcase (alist-get 'type record)
    ("user" (not (or (alist-get 'isMeta record) (alist-get 'queueTranscriptOnly record)
                     (drover-claude-code--summary-p record))))
    ("assistant" (or (alist-get 'isApiErrorMessage record)
                     (not (equal (alist-get 'model (alist-get 'message record)) "<synthetic>"))))))

(defun drover-claude-code-transcripts--last-message-record (beg end)
  "Return the newest message between BEG and END, parsed, or nil.
See `drover-claude-code-transcripts--message-p'."
  (drover-transcript-last
   "\"timestamp\":\""
   (lambda (record)
     (and (drover-claude-code-transcripts--message-p record)
          (drover-transcript-time (alist-get 'timestamp record))))
   beg end))

(defun drover-claude-code-transcripts--activity (beg end)
  "Return the time of the newest message between BEG and END, or nil.
See `drover-claude-code-transcripts--message-p'."
  (when-let* ((record (drover-claude-code-transcripts--last-message-record beg end)))
    (drover-transcript-time (alist-get 'timestamp record))))

(defun drover-claude-code-transcripts--end (beg end)
  "Return how the conversation stands at its newest message from BEG to END.
A plist of its time, :updated, and what
`drover-claude-code-transcripts--ending' tells of it; nil without a
message there.  Messages of subagents do not end the conversation's
turns."
  (when-let* ((record (drover-claude-code-transcripts--last-message-record beg end)))
    (append (list :updated (drover-transcript-time (alist-get 'timestamp record)))
            (unless (alist-get 'isSidechain record)
              (drover-claude-code-transcripts--ending record)))))

(defun drover-claude-code-transcripts--prompted (beg end)
  "Return the time of the user's newest prompt between BEG and END, or nil.
A turn running starts there."
  (when-let* ((record (drover-transcript-last
                       "\"type\":\"user\""
                       (lambda (record)
                         (and (not (alist-get 'isSidechain record))
                              (drover-claude-code-transcripts--message-p record)
                              (drover-claude-code--prompt-p record)))
                       beg end)))
    (drover-transcript-time (alist-get 'timestamp record))))

(defun drover-claude-code-transcripts--turn-told (beg end)
  "Return the model and effort of the newest main turn from BEG to END, or nil.
A plist of :model and :effort, for the lines before a transcript's tail
when the tail holds no reply (see `drover-transcript-look-before-tail-for')."
  (when-let* ((turn (drover-claude-code-transcripts--turn beg end)))
    (list :model (nth 0 turn) :effort (nth 1 turn))))

(defun drover-claude-code-transcripts--read-tail (beg end)
  "Return what the records between BEG and END tell of the conversation's end.
A plist: the newest title the user gave, summary, last prompt,
permission mode, model, effort, branch, prompt's time and activity
there; how its newest message left it (see
`drover-claude-code-transcripts--end') and what its newest reply ran on
\(see `drover-claude-code-transcripts--usage'); members not told are
left out.  Its other titles are read apart (see
`drover-claude-code-transcripts--read-titles')."
  (let ((turn (drover-claude-code-transcripts--turn beg end))
        (ending (drover-claude-code-transcripts--end beg end))
        (data nil))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value)))))
      (add :custom-title (drover-claude-code-transcripts--member "\"type\":\"custom-title\"" 'customTitle beg end))
      (add :summary (drover-claude-code-transcripts--member "\"type\":\"summary\"" 'summary beg end))
      (add :last-prompt (drover-claude-code-transcripts--member "\"type\":\"last-prompt\"" 'lastPrompt beg end))
      (add :mode (drover-claude-code-transcripts--member "\"type\":\"permission-mode\"" 'permissionMode beg end))
      (add :model (nth 0 turn))
      (add :effort (nth 1 turn))
      (add :prompted (drover-claude-code-transcripts--prompted beg end))
      (add :branch (save-excursion
                     (goto-char end)
                     (drover-transcript-sniff "gitBranch" t beg)))
      (add :messages (save-excursion
                       (goto-char beg)
                       (and (re-search-forward "\"type\":\"\\(?:user\\|assistant\\)\"" end t) t))))
    (setq data (drover-claude-code-transcripts--merge
                data (append ending (drover-claude-code-transcripts--usage beg end))))
    (let ((activity (plist-get ending :updated)))
      (when (or activity (nth 2 turn))
        (setq data (plist-put data :updated (if (and activity (nth 2 turn))
                                                (max activity (nth 2 turn))
                                              (or activity (nth 2 turn)))))))
    data))

(defun drover-claude-code-transcripts--merge (data told)
  "Return DATA with every member of TOLD, a plist, put in, nil ones included.
How a conversation stands is told whole: an error of a turn before does
not outlive a reply that ends the next one."
  (while told
    (setq data (plist-put data (pop told) (pop told))))
  data)

(defun drover-claude-code-transcripts--read-head (beg end)
  "Return what the records between BEG and END tell of the conversation's start.
A plist of its directory, creation time, entry point, branch, first
prompt, the title the user gave there, and its newest message's time,
reply's usage and turn's model and effort, which a tail telling none
leaves; members not told are left out."
  (let ((data nil))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value))))
              (sniff (key) (save-excursion (goto-char beg) (drover-transcript-sniff key nil end))))
      (add :cwd (sniff "cwd"))
      (add :created (drover-transcript-time (sniff "timestamp")))
      (add :entrypoint (sniff "entrypoint"))
      (add :branch (sniff "gitBranch"))
      (add :prompt (or (seq-some #'drover-claude-code-transcripts--prompt
                                 (drover-transcript-lines "\"type\":\"user\"" nil beg end))
                       (drover-claude-code-transcripts--cut-prompt beg end)))
      (add :custom-title (drover-claude-code-transcripts--member "\"type\":\"custom-title\"" 'customTitle beg end))
      (add :messages (save-excursion
                       (goto-char beg)
                       (and (re-search-forward "\"type\":\"\\(?:user\\|assistant\\)\"" end t) t))))
    ;; A tail of other records leaves the head's newest message as the
    ;; last activity, and how it left the conversation.
    (drover-claude-code-transcripts--merge
     data (append (drover-claude-code-transcripts--end beg end)
                  (drover-claude-code-transcripts--usage beg end)
                  (drover-claude-code-transcripts--turn-told beg end)))))

(defun drover-claude-code-transcripts--last-message (file size tail old-size)
  "Return the time of the newest message of transcript FILE, of SIZE bytes, or nil.
The current buffer holds what `drover-transcript-insert-ends' inserted
of it with OLD-SIZE, the tail from TAIL: the newest message of the tail,
else of the lines before it, read as needed (see
`drover-transcript-look-before-tail'), else, without OLD-SIZE, of the
head."
  (or (drover-claude-code-transcripts--activity tail (point-max))
      (drover-transcript-look-before-tail file size tail old-size
                                          #'drover-claude-code-transcripts--activity)
      (and (not old-size) (> tail (point-min))
           (drover-claude-code-transcripts--activity (point-min) tail))))

(defun drover-claude-code-transcripts--subagent-turn (beg end)
  "Return the model and effort of a subagent's newest turn from BEG to END, or nil.
A plist of :model and :effort, read in the subagent's own transcript."
  (when-let* ((turn (drover-claude-code-transcripts--turn beg end t)))
    (list :model (nth 0 turn) :effort (nth 1 turn))))

(defun drover-claude-code-transcripts--subagent-context (beg end)
  "Return the context of a subagent's newest reply from BEG to END, or nil.
A plist of :context, the tokens of its prompt (see
`drover-claude-code-transcripts--prompt-tokens'), read in the
subagent's own transcript."
  (when-let* ((record (drover-transcript-last drover-claude-code-transcripts--usage-marker
                                              (lambda (record)
                                                (drover-claude-code-transcripts--usage-record-p record t))
                                              beg end)))
    (list :context (drover-claude-code-transcripts--prompt-tokens
                    (alist-get 'usage (alist-get 'message record))))))

(defconst drover-claude-code-transcripts--subagent-readers
  '((:model . drover-claude-code-transcripts--subagent-turn)
    (:context . drover-claude-code-transcripts--subagent-context))
  "What is read of a subagent's run, each member by the reader that tells it.")

(defun drover-claude-code-transcripts--subagent-read (transcript size)
  "Return what the transcript of a subagent, TRANSCRIPT of SIZE bytes, tells.
A plist of :created, the time of its first record; :updated, that of
its newest message, or :created without one; and what it ran on, the
:model and :effort of its newest turn and the :context of its newest
reply telling its usage, each looked for in the tail, else in the lines
before it, else in the head, and left out when none tells it."
  (with-temp-buffer
    (let* ((tail (drover-transcript-insert-ends transcript size))
           (created (save-excursion
                      (goto-char (point-min))
                      (drover-transcript-time (drover-transcript-sniff "timestamp"))))
           (data (list :created created
                       :updated (or (drover-claude-code-transcripts--last-message transcript size tail nil)
                                    created))))
      (cl-flet ((missing ()
                  (seq-remove (lambda (reader) (plist-member data (car reader)))
                              drover-claude-code-transcripts--subagent-readers)))
        (pcase-dolist (`(,_ . ,reader) drover-claude-code-transcripts--subagent-readers)
          (setq data (append data (funcall reader tail (point-max)))))
        (when-let* ((left (missing)))
          (setq data (append data (drover-transcript-look-before-tail-for
                                   transcript size tail nil (mapcar #'cdr left)))))
        (when (> tail (point-min))
          (pcase-dolist (`(,_ . ,reader) (missing))
            (setq data (append data (funcall reader (point-min) tail))))))
      data)))

(defun drover-claude-code-transcripts--subagents (file &optional old)
  "Return the subagents of the conversation transcript FILE records.
Their transcripts are beside it, under ID/subagents/, each with a
description in a .meta.json; each is a plist of :agent, its name, :title,
:type, the :size and :mtime of its transcript when read, and what
`drover-claude-code-transcripts--subagent-read' read of it: :created,
:updated, and the :model, :effort and :context it ran.  OLD are the
subagents read before: one whose transcript did not change since is
not read again."
  (let ((directory (expand-file-name "subagents" (file-name-sans-extension file))))
    (when (file-directory-p directory)
      (delq nil
            (mapcar (lambda (entry)
                      (let* ((transcript (car entry))
                             (agent (file-name-base transcript))
                             (size (file-attribute-size (cdr entry)))
                             (mtime (float-time (file-attribute-modification-time (cdr entry))))
                             (before (seq-find (lambda (subagent)
                                                 (equal (plist-get subagent :agent) agent))
                                               old)))
                        (if (and before (eql (plist-get before :size) size)
                                 (eql (plist-get before :mtime) mtime))
                            before
                          (let ((meta (drover-read-json (concat (file-name-sans-extension transcript)
                                                                ".meta.json"))))
                            (append
                             (list :agent agent
                                   :title (drover-transcript-text
                                           (alist-get 'description (and (drover-json-object-p meta) meta)))
                                   :type (drover-transcript-text
                                          (alist-get 'agentType (and (drover-json-object-p meta) meta)))
                                   :size size
                                   :mtime mtime)
                             (drover-claude-code-transcripts--subagent-read transcript size))))))
                    (ignore-errors
                      (directory-files-and-attributes directory t "\\`agent-.*\\.jsonl\\'" t)))))))

;;;; Titles

;; Claude Code titles a conversation from its first prompts (ai-title),
;; and its desktop app shows that title.  As a plan is approved, the CLI
;; also names the session after the plan, in kebab case (agent-name),
;; which its terminal's title shows, and records that name as an
;; ai-title too, again at every turn, the ai-title first: read from the
;; newest back, the agent's name comes before each ai-title naming it.
;; The title is the newest ai-title naming no agent.  A long session
;; leaves its generated title megabytes before its tail, further back
;; than a read looks: the transcript is then read whole in the
;; background (`drover-claude-code-transcripts--queue-titles').

(defconst drover-claude-code-transcripts--names-limit 16
  "Agent names kept of a conversation at most, the newest.")

(defconst drover-claude-code-transcripts--title-regexp
  "\"type\":\"\\(?:ai-title\\|agent-name\\)\""
  "Regexp matching a record of a generated title or of an agent's name.")

(defconst drover-claude-code-transcripts--title-keys
  '("\"type\":\"ai-title\"" "\"type\":\"agent-name\"")
  "What the records of generated titles and of agents' names hold, as strings.")

(defun drover-claude-code-transcripts--title-entry (record)
  "Return what RECORD tells of the conversation's titles, or nil.
That is (ai-title . TEXT) for a title the CLI recorded as generated,
\(agent-name . TEXT) for the name of the session's agent, TEXT on one
line, trimmed and cut as titles are."
  (pcase (alist-get 'type record)
    ("ai-title" (when-let* ((text (drover-transcript-text (alist-get 'aiTitle record))))
                  (cons 'ai-title text)))
    ("agent-name" (when-let* ((text (drover-transcript-text (alist-get 'agentName record))))
                    (cons 'agent-name text)))))

(defun drover-claude-code-transcripts--add-names (names more)
  "Return the agent names NAMES, then those of MORE not among them.
At most `drover-claude-code-transcripts--names-limit' of them, the
first: each list comes newest first, MORE older than NAMES."
  (take drover-claude-code-transcripts--names-limit (delete-dups (append names more))))

(defun drover-claude-code-transcripts--titles (beg end names)
  "Return the newest title the CLI generated between BEG and END, and agent names.
The records are read from END back.  The value is (TITLE . FOUND):
TITLE, the text of the newest ai-title naming none of the agents NAMES,
known of newer records, nor of those of the records after it, or nil;
FOUND, the agent names of the records read, the newest first, up to
TITLE or BEG."
  (save-excursion
    (goto-char end)
    (let ((title nil)
          (found nil))
      (while (and (not title)
                  (re-search-backward drover-claude-code-transcripts--title-regexp beg t))
        (pcase (drover-claude-code-transcripts--title-entry (drover-transcript-line))
          (`(agent-name . ,name)
           (unless (member name found)
             (setq found (append found (list name)))))
          (`(ai-title . ,text)
           (unless (or (member text names) (member text found))
             (setq title text))))
        (beginning-of-line))
      (cons title found))))

(defun drover-claude-code-transcripts--title-reader (names known)
  "Return a reader of the lines before a tail looking for a generated title.
For `drover-transcript-look-before-tail-for': a function of BEG and END
returning (:ai-title TITLE) once it found a generated title naming none
of the agents NAMES and KNOWN tell.  NAMES is a cons whose car is the
list of the names found in newer lines, the newest first, which the
reader adds those it finds to, as it reads lines older and older; KNOWN
those read before, of older lines."
  (lambda (beg end)
    (pcase-let ((`(,title . ,found) (drover-claude-code-transcripts--titles
                                     beg end (append (car names) known))))
      (setcar names (drover-claude-code-transcripts--add-names (car names) found))
      (and title (list :ai-title title)))))

(defun drover-claude-code-transcripts--settle-titles (title names size tail grown before)
  "Return what was read of a transcript's titles, a plist.
TITLE is the newest title the CLI generated found in its tail or in the
lines read before it, or nil; NAMES the agent names found there, the
newest first; SIZE the size of the transcript.  The current buffer
holds what `drover-transcript-insert-ends' inserted of it, the tail
from TAIL; with GROWN, the size it was read at before, only what was
appended since, BEFORE being what was read then.  The plist has
:ai-title, the newest generated title known, or nil; :agent-names, the
names of the agents known, the newest first; and :title-unread,
non-nil when a generated title newer than :ai-title may be in lines
not read.

Read anew, without TITLE, the head tells one, the names found anywhere
known; with agent names, the lines between the head and those read
before the tail, if any, are not read.  Grown, TITLE wins, newer than
any before; else the title known stays, unless it names one of the
agents, its ai-title read before the record of the agent's name: then
the lines not read here are to be."
  (if grown
      (let ((names (drover-claude-code-transcripts--add-names names (plist-get before :agent-names)))
            (old (plist-get before :ai-title)))
        (cond (title (list :ai-title title :agent-names names :title-unread nil))
              ((and old (member old names)) (list :ai-title nil :agent-names names :title-unread t))
              (t (list :ai-title old :agent-names names
                       :title-unread (plist-get before :title-unread)))))
    (let* ((head (unless (or title (= tail (point-min)))
                   (drover-claude-code-transcripts--titles (point-min) tail names)))
           (names (drover-claude-code-transcripts--add-names names (cdr head))))
      (list :ai-title (or title (car head))
            :agent-names names
            :title-unread (and (not title) names
                               (> (- size (* 2 drover-transcript-window)) drover-transcript-look-back)
                               t)))))

(defun drover-claude-code-transcripts--read (file attributes old)
  "Return what transcript FILE, of ATTRIBUTES, tells of its conversation.
OLD is the entry read before, or nil: a transcript only appended to
since, the same file grown, is read from where it ended, what its new
records tell replacing what the old ones told.  Otherwise its head and
tail are read.  Its last activity is its newest message's (see
`drover-claude-code-transcripts--message-p'), looked for further back
than the tail when the tail holds none, as its newest turn's model and
reply's usage are, never the file's modification
time, which the CLI's records of its own move as it opens the
conversation again.  Its titles are read as
`drover-claude-code-transcripts--settle-titles' tells.  For the index of
`drover-transcript'."
  (let* ((size (file-attribute-size attributes))
         (before (and old (drover-transcript-data old)))
         ;; A head shorter than its window may lack the first prompt.
         (grown (and before
                     (equal (drover-transcript-inode old) (file-attribute-inode-number attributes))
                     (<= drover-transcript-window (drover-transcript-size old) size)
                     (drover-transcript-size old))))
    (with-temp-buffer
      (let* ((tail (drover-transcript-insert-ends file size grown))
             (data (if grown
                       (copy-sequence before)
                     (drover-claude-code-transcripts--read-head (point-min) (if (= tail (point-min))
                                                                                (point-max)
                                                                              tail)))))
        (pcase-let* ((recent (drover-claude-code-transcripts--read-tail tail (point-max)))
                     (`(,title . ,found) (drover-claude-code-transcripts--titles
                                          tail (point-max) (and grown (plist-get before :agent-names))))
                     ;; The names the title reader adds to as it reads.
                     (names (list found)))
          ;; What the tail does not tell, the lines before it may.
          (setq recent (append recent
                               (drover-transcript-look-before-tail-for
                                file size tail grown
                                (append (unless (plist-get recent :updated) (list #'drover-claude-code-transcripts--end))
                                        ;; A compaction there tells a size, or none.
                                        (unless (plist-member recent :context)
                                          (list #'drover-claude-code-transcripts--usage))
                                        (unless (plist-get recent :model) (list #'drover-claude-code-transcripts--turn-told))
                                        (unless title (list (drover-claude-code-transcripts--title-reader
                                                            names (and grown (plist-get before :agent-names)))))))))
          (setq data (drover-claude-code-transcripts--merge
                      (drover-claude-code-transcripts--merge data recent)
                      (drover-claude-code-transcripts--settle-titles
                       (or title (plist-get recent :ai-title)) (car names) size tail grown before))))
        (setq data (plist-put data :id (drover-claude-code-transcripts--id file)))
        (let ((directory (or (plist-get data :cwd)
                             (drover-claude-code-decode-bucket
                              (file-name-nondirectory (directory-file-name (file-name-directory file)))))))
          (setq data (plist-put data :directory (and directory (file-name-as-directory directory))))
          (setq data (plist-put data :project (drover-transcript-project
                                               (plist-get data :directory)))))
        (unless (plist-get data :updated)
          (setq data (plist-put data :updated (plist-get data :created))))
        (plist-put data :subagents (drover-claude-code-transcripts--subagents
                                    file (plist-get before :subagents)))))))

;;;; Records

(defun drover-claude-code-transcripts--title (data)
  "Return the title of the conversation of DATA, as Claude Code's desktop app does.
That is the title the user gave, else the one the CLI generated, not the
name of the agent it gives the session as its plan is approved (see
`drover-claude-code-transcripts--settle-titles'), else that name, else
its summary, first prompt or last prompt."
  (or (plist-get data :custom-title)
      (plist-get data :ai-title)
      (car (plist-get data :agent-names))
      (plist-get data :summary)
      (plist-get data :prompt)
      (plist-get data :last-prompt)))

(defun drover-claude-code-transcripts--listed-p (data)
  "Return non-nil when the transcript read as DATA records one to list.
One with no message, or naming nothing, as a session that ran a command
and left, the CLI's picker leaves out too."
  (and (plist-get data :messages)
       (drover-claude-code-transcripts--title data)
       t))

(defun drover-claude-code-transcripts--recent-p (data &optional now)
  "Return non-nil when the conversation of DATA is listed live at NOW.
See `drover-claude-code-history-days'."
  (or (null drover-claude-code-history-days)
      (> (or (plist-get data :updated) 0)
         (- (or now (float-time)) (* 86400 drover-claude-code-history-days)))))

(defvar drover-claude-code-transcripts--models (cons nil (make-hash-table :test #'equal))
  "The names of models, by identifier, and the catalog that named them.")

(defun drover-claude-code-transcripts--model-name (id catalog)
  "Return the compact name of the model ID, as CATALOG names it."
  (unless (eq (car drover-claude-code-transcripts--models) catalog)
    (setq drover-claude-code-transcripts--models (cons catalog (make-hash-table :test #'equal))))
  (let ((names (cdr drover-claude-code-transcripts--models)))
    (or (gethash id names)
        (puthash id
                 (let* ((base (drover-claude-code--base-id id))
                        (entry (and catalog (drover-claude-code--catalog-model catalog base)))
                        (name (alist-get 'name entry)))
                   (drover-model-short-name (if name (concat name (substring id (length base))) id)))
                 names))))

(defconst drover-claude-code-transcripts--name 'claude-code-transcripts
  "The name of the index of Claude Code's transcripts, which what was seen keys.")

(defun drover-claude-code-transcripts--unseen (data)
  "Return how the conversation of DATA ended, `done' or `failed', unless seen.
Nil when its newest message ended no turn, or the user saw it since (see
`drover-transcript-unseen-p'), as they do marking it seen, opening it,
or looking at a front end following it."
  (let ((outcome (plist-get data :outcome)))
    (and (memq outcome '(done failed))
         (drover-transcript-unseen-p drover-claude-code-transcripts--name (plist-get data :id)
                                     (plist-get data :ended))
         outcome)))

(defun drover-claude-code-transcripts--status (id &optional data)
  "Return (STATUS REQUESTS DETAIL) of conversation ID, as the CLI runs it.
A session \"claude agents\" lists as working or waiting does so; else,
with DATA, what was read of it, a turn that ended unseen is done, or
failed, its error the detail (see
`drover-claude-code-transcripts--unseen'); else it is idle."
  (let* ((agent (gethash id drover-claude-code-transcripts--agents))
         (state (and agent (or (plist-get agent :state) "")))
         (where (if (equal (plist-get agent :kind) "background")
                    "in the background"
                  "in another terminal")))
    (pcase (drover-claude-code-run-state state)
      ('working
       (list 'working nil (concat (if (equal state "shell") "shell running " "working ") where)))
      ('blocked
       (list 'waiting
             (list (list :kind 'permission
                         :summary (format "waits %s: claude attach %s" where
                                          (or (plist-get agent :short) id))))
             nil))
      (_ (pcase (and data (drover-claude-code-transcripts--unseen data))
           ('done (list 'done nil nil))
           ('failed (list 'failed nil (plist-get data :error)))
           (_ (list 'idle nil (and agent (string-trim (concat state " " where))))))))))

(defcustom drover-claude-code-context-limits '(("\\[1m\\]\\'" . 1000000))
  "Context sizes of Claude's models, as (REGEXP . TOKENS), the first match winning.
REGEXP matches the identifier of a model as a transcript records it,
such as \"claude-opus-5-5\", or \"claude-sonnet-5-5[1m]\" for its
context of a million tokens.  A model none matches has the size Claude
Code's model catalog gives it (its max_input_tokens), else
`drover-claude-code-default-context-limit'.  A conversation whose
context is past that size runs a context of a million tokens, the
largest Claude has."
  :type '(alist :key-type (regexp :tag "Model") :value-type (natnum :tag "Tokens"))
  :group 'drover)

(defcustom drover-claude-code-default-context-limit 200000
  "Context size of a Claude model neither catalog nor table tells.
See `drover-claude-code-context-limits'."
  :type 'natnum
  :group 'drover)

(defvar drover-claude-code-transcripts--limits (cons nil (make-hash-table :test #'equal))
  "The context sizes of models by identifier, and the catalog that told them.")

(defun drover-claude-code-transcripts--context-limit (id tokens catalog)
  "Return the context size of the model ID, which ran a context of TOKENS.
CATALOG is Claude Code's model catalog, or nil.  See
`drover-claude-code-context-limits'."
  (unless (eq (car drover-claude-code-transcripts--limits) catalog)
    (setq drover-claude-code-transcripts--limits (cons catalog (make-hash-table :test #'equal))))
  (let ((limit (with-memoization (gethash (cons id drover-claude-code-context-limits)
                                          (cdr drover-claude-code-transcripts--limits))
                 (or (cdr (seq-find (lambda (entry) (string-match-p (car entry) id))
                                    drover-claude-code-context-limits))
                     (let ((tokens (and catalog
                                        (map-nested-elt (drover-claude-code--catalog-model catalog id)
                                                        '(runtime max_input_tokens)))))
                       (and (natnump tokens) (> tokens 0) tokens))
                     drover-claude-code-default-context-limit))))
    (if (and (natnump tokens) (> tokens limit) (< limit 1000000))
        1000000
      limit)))

(defun drover-claude-code-transcripts--ran-on (data catalog)
  "Return what the conversation of DATA ran on, as arguments of a record.
Keyword arguments of `drover-session-create', those its newest reply
tells: its provider, Anthropic, as OpenCode shows it, the size of its
context and its model's (see `drover-claude-code-transcripts--context-limit'),
the size estimated when a compaction since left it, and its prompt
cache, which expires its lifetime after its request was sent, as the
API keeps a cache for its lifetime from its last use.  CATALOG is
Claude Code's model catalog, or nil."
  (let ((model (plist-get data :model))
        (tokens (plist-get data :context))
        (cached (plist-get data :cache-tokens))
        (requested (plist-get data :requested)))
    (append (and model (drover-transcript-provider "anthropic" "Anthropic"))
            (and (natnump tokens) (> tokens 0)
                 (list :context-tokens tokens
                       :context-estimated (and (plist-get data :context-estimated) t)
                       :context-limit (and model (drover-claude-code-transcripts--context-limit
                                                  model tokens catalog))))
            (and (natnump cached) (> cached 0) requested
                 (let ((ttl (or (plist-get data :cache-ttl) 300)))
                   (list :cache-expires (+ requested ttl) :cache-ttl ttl
                         :cache-tokens cached
                         :cache-prompt-tokens (plist-get data :cache-prompt-tokens)))))))

(defun drover-claude-code-transcripts--catalog (catalog)
  "Return CATALOG as `drover-claude-code-transcripts--record' takes it, read."
  (pcase catalog
    ('none nil)
    ('nil (drover-claude-code-catalog))
    (_ catalog)))

(defun drover-claude-code-transcripts--record (data &optional catalog)
  "Return the `drover-session' of the conversation DATA tells, or nil.
CATALOG names its model, by default the catalog the CLI caches; the
symbol `none' stands for no catalog, so that one listing many looks for
it once."
  (when (drover-claude-code-transcripts--listed-p data)
    (let* ((id (plist-get data :id))
           (status (drover-claude-code-transcripts--status id data))
           (catalog (drover-claude-code-transcripts--catalog catalog))
           (model (plist-get data :model))
           (branch (plist-get data :branch))
           (mode (plist-get data :mode))
           (run (drover-claude-code-transcripts--ran-on data catalog)))
      ;; Not `apply': the constructor's keywords are then parsed at run
      ;; time, which listing thousands of conversations felt.
      (drover-session-create
             :backend 'claude-code :id id
             :engine 'claude-code :engine-id id
             :title (drover-claude-code-transcripts--title data)
             :directory (plist-get data :directory)
             :project (plist-get data :project)
             :branch (and branch (not (equal branch "HEAD")) branch)
             :status (nth 0 status) :requests (nth 1 status) :detail (nth 2 status)
             ;; A turn runs from the newest prompt, a wait from the newest
             ;; message, and a turn ended from its end.
             :since (pcase (nth 0 status)
                      ('idle nil)
                      ('working (or (plist-get data :prompted) (plist-get data :updated)))
                      ((or 'done 'failed) (or (plist-get data :ended) (plist-get data :updated)))
                      (_ (plist-get data :updated)))
             :updated (plist-get data :updated)
             :created (plist-get data :created)
             :agent (and mode (drover-claude-code-mode-agent mode))
             :model (and model (drover-claude-code-transcripts--model-name model catalog))
             :effort (plist-get data :effort)
             :provider (plist-get run :provider)
             :provider-name (plist-get run :provider-name)
             :context-tokens (plist-get run :context-tokens)
             :context-estimated (plist-get run :context-estimated)
             :context-limit (plist-get run :context-limit)
             :cache-expires (plist-get run :cache-expires)
             :cache-ttl (plist-get run :cache-ttl)
             :cache-tokens (plist-get run :cache-tokens)
             :cache-prompt-tokens (plist-get run :cache-prompt-tokens)))))

(defun drover-claude-code-transcripts--subagent-id (id agent)
  "Return the identifier of the subagent AGENT of conversation ID."
  (concat id "/" agent))

(defun drover-claude-code-transcripts--subagent-records (data &optional catalog)
  "Return the records of the subagents of the conversation of DATA.
Each runs the model and effort its own transcript tells, on Anthropic.
CATALOG names its model, as for `drover-claude-code-transcripts--record'."
  (when (drover-claude-code-transcripts--listed-p data)
    (let ((id (plist-get data :id))
          (catalog (drover-claude-code-transcripts--catalog catalog)))
      (mapcar (lambda (subagent)
                (let* ((model (plist-get subagent :model))
                       (run (drover-claude-code-transcripts--ran-on
                             (list :model model :context (plist-get subagent :context))
                             catalog)))
                  (drover-session-create
                   :backend 'claude-code
                   :id (drover-claude-code-transcripts--subagent-id id (plist-get subagent :agent))
                   :parent id
                   :engine 'claude-code
                   :title (or (plist-get subagent :title) (plist-get subagent :agent))
                   :directory (plist-get data :directory)
                   :project (plist-get data :project)
                   :status 'idle
                   :updated (plist-get subagent :updated)
                   :created (plist-get subagent :created)
                   :agent (plist-get subagent :type)
                   :model (and model (drover-claude-code-transcripts--model-name model catalog))
                   :effort (plist-get subagent :effort)
                   :provider (plist-get run :provider)
                   :provider-name (plist-get run :provider-name)
                   :context-tokens (plist-get run :context-tokens)
                   :context-limit (plist-get run :context-limit))))
              (plist-get data :subagents)))))

(defun drover-claude-code-transcripts--records (predicate)
  "Return the records of the conversations PREDICATE keeps, with subagents.
PREDICATE is a function of what was read of a conversation."
  (let ((records nil)
        (catalog (or (drover-claude-code-catalog) 'none)))
    (when (drover-claude-code-transcripts--current-index)
      (drover-transcript-map
       (lambda (_file data)
         (when (funcall predicate data)
           (when-let* ((record (drover-claude-code-transcripts--record data catalog)))
             (push record records)
             (setq records (append (drover-claude-code-transcripts--subagent-records data catalog) records)))))
       (drover-claude-code-transcripts--current-index)))
    records))

(defun drover-claude-code-transcripts--sessions ()
  "Return the conversations listed live, those of the last days.
The backend's :sessions."
  (let ((now (float-time)))
    (drover-claude-code-transcripts--records
     (lambda (data) (drover-claude-code-transcripts--recent-p data now)))))

(defun drover-claude-code-transcripts--history (callback)
  "Call CALLBACK at once with the older conversations.
See `drover-claude-code-history-days'.  The backend's :history."
  (let ((now (float-time)))
    (funcall callback
             (and drover-claude-code-history-days
                  (drover-claude-code-transcripts--records
                   (lambda (data) (not (drover-claude-code-transcripts--recent-p data now))))))))

(defun drover-claude-code-transcripts--file (id)
  "Return the transcript of conversation ID, or nil."
  (when-let* ((index (drover-claude-code-transcripts--current-index)))
    (drover-transcript-keyed-file index id)))

(defun drover-claude-code-transcripts--data (id)
  "Return what was read of conversation ID, or nil."
  (when-let* ((file (drover-claude-code-transcripts--file id)))
    (drover-transcript-lookup (drover-claude-code-transcripts--current-index) file)))

(defun drover-claude-code-transcripts--times (id)
  "Return (CREATED . UPDATED) of conversation ID, as its transcript tells.
UPDATED is the time of its newest message; nil when the transcript was
not read.  The backend's :recorded-times."
  (when-let* ((data (drover-claude-code-transcripts--data id)))
    (cons (plist-get data :created) (plist-get data :updated))))

(defun drover-claude-code-transcripts--recorded-title (id)
  "Return the title of conversation ID, as its transcript tells, or nil.
See `drover-claude-code-transcripts--title'.  The backend's
:recorded-title."
  (when-let* ((data (drover-claude-code-transcripts--data id)))
    (drover-claude-code-transcripts--title data)))

(defun drover-claude-code-transcripts-check (id)
  "Look at the transcript of conversation ID at once, rather than at the next look.
For a front end that knows the CLI just recorded something of it, such
as a title.  Return non-nil when the transcript is to be read again."
  (when-let* ((index (drover-claude-code-transcripts--current-index))
              (file (drover-claude-code-transcripts--file id)))
    (drover-transcript-check index file)))

(defconst drover-claude-code-transcripts--clock-slack 10
  "Seconds by which a transcript's times and this Emacs's may differ.
The CLI dates its records by its own clock, and a busy Emacs reads a
terminal's output late.")

(defun drover-claude-code-transcripts-work-between (id start end)
  "Return what the transcript of conversation ID tells of work START to END.
START and END are float times, when a front end saw the CLI work.  The
value is `turn' when a message was recorded since START, as a prompt is
as its turn starts; `compaction' when none was, and a compaction the
CLI ran of its own, not the user's /compact, ended during the work:
the CLI compacted the conversation as it sat idle, which is no turn;
`quiet' when the transcript tells neither yet; nil when it was never
read.  Times differ by `drover-claude-code-transcripts--clock-slack'
seconds at most."
  (when-let* ((data (drover-claude-code-transcripts--data id)))
    (let* ((slack drover-claude-code-transcripts--clock-slack)
           (from (- start slack))
           (compacted (plist-get data :compacted))
           (ended (plist-get compacted :end)))
      (cond ((>= (or (plist-get data :updated) 0) from) 'turn)
            ((and (plist-get compacted :own) ended (<= from ended (+ end slack))) 'compaction)
            (t 'quiet)))))

(defun drover-claude-code-transcripts--recorded-run (id)
  "Return what conversation ID ran on, as its transcript tells, or nil.
Keyword arguments of `drover-session-create' (see
`drover-claude-code-transcripts--ran-on'), for a front end following it.
The backend's :recorded-run."
  (when-let* ((data (drover-claude-code-transcripts--data id)))
    (drover-claude-code-transcripts--ran-on data (drover-claude-code-catalog))))

(defun drover-claude-code-transcripts--seen-now (id)
  "Return when the user sees conversation ID now, as its transcript dates replies.
Now, or its newest reply's time when later, as a clock ahead of this
one dates it: the reply the user sees is seen."
  (max (float-time) (or (plist-get (drover-claude-code-transcripts--data id) :ended) 0)))

(defun drover-claude-code-transcripts--mark-seen (session)
  "Mark the ended turn of the conversation of SESSION seen.
It is noted for good (see `drover-transcript-mark-seen'): the
conversation idles.  The backend's :mark-seen."
  (let ((id (car (drover-claude-code-transcripts--split (drover-session-id session)))))
    (when (drover-transcript-mark-seen drover-claude-code-transcripts--name id
                                       (drover-claude-code-transcripts--seen-now id))
      (drover-backend-changed 'claude-code id))))

(defun drover-claude-code-transcripts--recorded-seen (id)
  "Note that the user saw conversation ID, which a front end follows, idle.
Noted only when a reply would otherwise show unseen once the front end
lets it go, or the transcript changed since read, which may hold one: so
that a front end reporting the same conversation over and over writes
nothing.  The backend's :recorded-seen."
  (let* ((file (drover-claude-code-transcripts--file id))
         (data (and file (drover-transcript-lookup (drover-claude-code-transcripts--current-index) file))))
    (when (or (null data)
              (drover-claude-code-transcripts--unseen data)
              (drover-transcript-check (drover-claude-code-transcripts--current-index) file))
      (drover-transcript-mark-seen drover-claude-code-transcripts--name id
                                   (drover-claude-code-transcripts--seen-now id)))))

(defun drover-claude-code-transcripts--split (id)
  "Return (CONVERSATION . AGENT) of ID, AGENT nil for a conversation."
  (if (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" id)
      (cons (match-string 1 id) (match-string 2 id))
    (cons id nil)))

(defun drover-claude-code-transcripts--session (id)
  "Return the record of conversation or subagent ID, as :sessions has it, or nil.
Its transcript is looked at again soon when it changed since it was
read.  The backend's :session."
  (pcase-let* ((`(,conversation . ,agent) (drover-claude-code-transcripts--split id))
               (file (drover-claude-code-transcripts--file conversation))
               (data (and file (drover-transcript-lookup (drover-claude-code-transcripts--current-index) file))))
    (when file
      (drover-transcript-check (drover-claude-code-transcripts--current-index) file))
    (when (and data (drover-claude-code-transcripts--recent-p data))
      (if agent
          (seq-find (lambda (record) (equal (drover-session-id record) id))
                    (drover-claude-code-transcripts--subagent-records data))
        (drover-claude-code-transcripts--record data)))))

;;;; Following the transcripts

(defun drover-claude-code-transcripts--changed (file old new)
  "Report that what was read of transcript FILE went from OLD to NEW.
A transcript whose generated title may be in lines not read is read
whole in the background (see `drover-claude-code-transcripts--queue-titles')."
  (when (plist-get new :title-unread)
    (drover-claude-code-transcripts--queue-titles file))
  (let ((id (plist-get (or new old) :id)))
    (drover-backend-changed 'claude-code id)
    (dolist (agent (delete-dups (mapcar (lambda (subagent) (plist-get subagent :agent))
                                        (append (plist-get old :subagents)
                                                (plist-get new :subagents)))))
      (drover-backend-changed 'claude-code (drover-claude-code-transcripts--subagent-id id agent)))))

;;;; Titles further back

(defvar drover-claude-code-transcripts--title-queue nil
  "Transcripts to read whole for their generated titles, the first first.")

(defvar drover-claude-code-transcripts--title-scan nil
  "The transcript read whole for its titles, as (FILE INODE . STOP), or nil.
INODE is the inode it was read at; STOP the function stopping the read.")

(defun drover-claude-code-transcripts--queue-titles (file)
  "Read transcript FILE whole in the background for its generated title.
The titles read of its head and tail were not enough to tell it (see
`drover-claude-code-transcripts--settle-titles').  Transcripts are read
one at a time, in slices that give way to input (see
`drover-transcript-read-lines')."
  (unless (or (member file drover-claude-code-transcripts--title-queue)
              (equal file (car drover-claude-code-transcripts--title-scan)))
    (setq drover-claude-code-transcripts--title-queue
          (append drover-claude-code-transcripts--title-queue (list file)))
    (drover-claude-code-transcripts--next-titles)))

(defun drover-claude-code-transcripts--next-titles ()
  "Begin reading the next transcript queued for its titles, unless one is read."
  (while (and (not drover-claude-code-transcripts--title-scan)
              drover-claude-code-transcripts--title-queue)
    (let* ((file (pop drover-claude-code-transcripts--title-queue))
           (index (drover-claude-code-transcripts--current-index))
           (entry (and index (drover-transcript-entry index file)))
           (entries nil))
      ;; One read again since it was queued may no longer need it.
      (when (and entry (plist-get (drover-transcript-data entry) :title-unread))
        (let ((scan (list file (drover-transcript-inode entry))))
          (setq drover-claude-code-transcripts--title-scan scan)
          (setcdr (cdr scan)
                  (drover-transcript-read-lines
                   (list (cons file 0)) drover-claude-code-transcripts--title-keys
                   (lambda (_file record)
                     (when-let* ((title (drover-claude-code-transcripts--title-entry record)))
                       (push title entries)))
                   (lambda (failure)
                     (when (eq drover-claude-code-transcripts--title-scan scan)
                       (setq drover-claude-code-transcripts--title-scan nil)
                       (unless failure
                         (drover-claude-code-transcripts--titles-read file (cadr scan) entries))
                       (drover-claude-code-transcripts--next-titles))))))))))

(defun drover-claude-code-transcripts--titles-read (file inode entries)
  "Note the titles of transcript FILE, read whole at INODE: ENTRIES.
ENTRIES are what `drover-claude-code-transcripts--title-entry' tells of
its records, the newest first.  Its title is the newest ai-title naming
none of its agents, those of the whole file and those read since.  A
transcript replaced since, or read again since and telling a newer
title, is left as it is."
  (when-let* ((index (drover-claude-code-transcripts--current-index))
              (entry (drover-transcript-entry index file))
              ((equal (drover-transcript-inode entry) inode)))
    (drover-transcript-update
     index file
     (lambda (data)
       (when (plist-get data :title-unread)
         (let* ((names (drover-claude-code-transcripts--add-names
                        (plist-get data :agent-names)
                        (delete-dups (mapcan (lambda (entry)
                                               (and (eq (car entry) 'agent-name) (list (cdr entry))))
                                             entries))))
                (title (seq-some (lambda (entry)
                                   (and (eq (car entry) 'ai-title)
                                        (not (member (cdr entry) names))
                                        (cdr entry)))
                                 entries)))
           (drover-claude-code-transcripts--merge
            (copy-sequence data) (list :ai-title title :agent-names names :title-unread nil))))))))

(defun drover-claude-code-transcripts--queue-unread-titles ()
  "Queue the transcripts kept whose generated titles are still to be read."
  (when-let* ((index (drover-claude-code-transcripts--current-index)))
    (drover-transcript-map (lambda (file data)
                             (when (plist-get data :title-unread)
                               (drover-claude-code-transcripts--queue-titles file)))
                           index)))

(defun drover-claude-code-transcripts--stop-titles ()
  "Stop reading transcripts for their titles, and forget those queued."
  (when-let* ((scan drover-claude-code-transcripts--title-scan))
    (setq drover-claude-code-transcripts--title-scan nil)
    (when (functionp (cddr scan))
      (funcall (cddr scan))))
  (setq drover-claude-code-transcripts--title-queue nil))

(defun drover-claude-code-transcripts--looked ()
  "Follow the end of a look at the transcripts.
Conversations that aged past `drover-claude-code-history-days' move to
the older ones; the sessions Claude Code runs elsewhere are asked for."
  (if (not drover-claude-code-history-days)
      (setq drover-claude-code-transcripts--recent nil)
    (let ((recent (make-hash-table :test #'equal))
          (before drover-claude-code-transcripts--recent)
          (moved nil)
          (now (float-time)))
      (drover-transcript-map (lambda (_file data)
                               (when (drover-claude-code-transcripts--recent-p data now)
                                 (puthash (plist-get data :id) t recent)))
                             (drover-claude-code-transcripts--current-index))
      (when before
        (maphash (lambda (id _) (unless (gethash id recent) (push id moved))) before)
        (maphash (lambda (id _) (unless (gethash id before) (push id moved))) recent))
      (setq drover-claude-code-transcripts--recent recent)
      (when moved
        (dolist (id moved)
          (drover-backend-changed 'claude-code id))
        (when (drover-fetch-wanted-p 'history)
          (drover-history-fetch 'claude-code)))))
  (drover-claude-code-transcripts--ask-agents))

(defun drover-claude-code-transcripts--roots ()
  "Return where Claude Code's transcripts are, for the index."
  (list (list (drover-claude-code-projects-directory) 1 "\\.jsonl\\'")))

(defun drover-claude-code-transcripts--make-index ()
  "Return a new index of Claude Code's transcripts."
  (drover-transcript-index-create 'claude-code-transcripts
                                  #'drover-claude-code-transcripts--roots
                                  #'drover-claude-code-transcripts--read
                                  #'drover-claude-code-transcripts--changed
                                  #'drover-claude-code-transcripts--looked
                                  #'drover-transcript-id
                                  drover-claude-code-transcripts--reader-version))

;;;; Sessions Claude Code runs elsewhere

(defun drover-claude-code-transcripts--list-shown-p ()
  "Return non-nil when a window of a visible frame shows one of drover's views."
  (seq-some (lambda (window) (buffer-local-value 'drover-view-p (window-buffer window)))
            (window-list-1 nil 'nomini 'visible)))

(defun drover-claude-code-transcripts--parse-agents (json)
  "Return the sessions JSON, the output of \"claude agents --json\", lists.
A hash table mapping each conversation to a plist of :kind, :state,
:short, its identifier for \"claude attach\", and :name."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry (let ((value (json-parse-string json :object-type 'alist :array-type 'list
                                                   :null-object nil :false-object nil)))
                     (and (listp value) value)))
      (when-let* (((drover-json-object-p entry))
                  (id (alist-get 'sessionId entry))
                  ((stringp id)))
        (puthash id (list :kind (alist-get 'kind entry)
                          :state (or (alist-get 'status entry) (alist-get 'state entry))
                          :short (alist-get 'id entry) :name (alist-get 'name entry))
                 table)))
    table))

(defun drover-claude-code-transcripts--agents-listed (table)
  "Make TABLE the sessions Claude Code runs elsewhere.
Those that changed are reported."
  (let ((before drover-claude-code-transcripts--agents)
        (changed nil))
    (maphash (lambda (id agent) (unless (equal agent (gethash id before)) (push id changed))) table)
    (maphash (lambda (id _) (unless (gethash id table) (push id changed))) before)
    (setq drover-claude-code-transcripts--agents table)
    (dolist (id changed)
      (drover-backend-changed 'claude-code id))))

(defun drover-claude-code-transcripts--ask-agents (&optional force)
  "Run \"claude agents --json\" to learn what Claude Code runs elsewhere.
Only while a list shows, every `drover-claude-code-agents-interval'
seconds at most, one run at a time; FORCE runs it whether or not a list
shows and the interval passed.  It never waits, not even for the CLI to
start (see `drover-command-run'): the answer is followed as it comes."
  (when-let* ((interval drover-claude-code-agents-interval)
              ((not (drover-command-live-p drover-claude-code-transcripts--agents-job)))
              ((or force
                   (and (> (- (float-time) drover-claude-code-transcripts--agents-time) interval)
                        (drover-claude-code-transcripts--list-shown-p))))
              (program (executable-find drover-claude-code-program)))
    (setq drover-claude-code-transcripts--agents-time (float-time))
    (condition-case err
        (setq drover-claude-code-transcripts--agents-job
              (drover-command-run "claude agents" (list program "agents" "--json")
                                  #'drover-claude-code-transcripts--agents-answered))
      (error (drover-report-failure "claude agents" err program)))))

(defun drover-claude-code-transcripts--agents-answered (result)
  "Follow RESULT, of \"claude agents --json\" as `drover-command-run' tells it.
An answer that failed or was cut changes nothing."
  (when (and (eql (plist-get result :exit) 0)
             (not (plist-get result :truncated)))
    (drover-claude-code-transcripts--agents-listed
     (drover-claude-code-transcripts--parse-agents (plist-get result :output)))))

;;;; Actions

(defun drover-claude-code-transcripts--front-end (data)
  "Return the front end the conversation of DATA ran in, or nil when unknown.
Its entry point tells: an interactive CLI, as claude-code-ide runs, or
the TypeScript SDK, which agent-shell's ACP adapter runs."
  (pcase (plist-get data :entrypoint)
    ("cli" 'claude-code-ide)
    ("sdk-ts" 'agent-shell)))

(defun drover-claude-code-transcripts--elsewhere (id)
  "Return the session of a front end following conversation ID, or nil."
  (when-let* ((session (drover-find-engine-session 'claude-code id)))
    (and (not (eq (drover-session-backend session) 'claude-code)) session)))

(defun drover-claude-code-transcripts--directory (data)
  "Return the directory the conversation of DATA ran in.
Signal a `user-error' when it is gone."
  (let ((directory (plist-get data :directory)))
    (unless (and directory (file-directory-p directory))
      (user-error "The directory of %s is gone: %s" (drover-claude-code-transcripts--title data)
                  (or directory "unknown")))
    directory))

(defun drover-claude-code-transcripts--open (session other-window)
  "Show the conversation of SESSION, resuming it in a front end.
A subagent shows its conversation.  A conversation a front end follows
already shows there, in another window when OTHER-WINDOW is non-nil;
one Claude Code runs elsewhere is attached to there, not here.  The
backend's :open."
  (pcase-let* ((`(,id . ,_) (drover-claude-code-transcripts--split (drover-session-id session)))
               (data (or (drover-claude-code-transcripts--data id)
                         (user-error "No transcript of %s" (drover-session-display-title session)))))
    (if-let* ((followed (drover-claude-code-transcripts--elsewhere id)))
        (drover-backend-call followed :open other-window)
      (when-let* ((agent (gethash id drover-claude-code-transcripts--agents))
                  ((not (eq (car (drover-claude-code-transcripts--status id)) 'idle))))
        (user-error "%s runs elsewhere: attach to it with claude attach %s"
                    (drover-session-display-title session) (or (plist-get agent :short) id)))
      (drover-resume 'claude-code id (drover-claude-code-transcripts--directory data)
                     (drover-claude-code-transcripts--front-end data))
      ;; Resumed, its turn shows: seen.
      (drover-claude-code-transcripts--mark-seen session))))

(defun drover-claude-code-transcripts--respond (session)
  "Show the conversation of SESSION, where what it waits for is answered."
  (drover-claude-code-transcripts--open session nil))

(defun drover-claude-code-transcripts--recap (session callback)
  "Call CALLBACK at once with a recap of the last turn of SESSION.
It is read from the end of its transcript, or of its subagent's (see
`drover-claude-code-recap').  The backend's :recap."
  (pcase-let* ((`(,id . ,agent) (drover-claude-code-transcripts--split (drover-session-id session)))
               (file (drover-claude-code-transcripts--file id))
               (file (and file (if agent
                                   (expand-file-name (concat agent ".jsonl")
                                                     (expand-file-name "subagents"
                                                                       (file-name-sans-extension file)))
                                 file))))
    (funcall callback
             (or (and file (file-readable-p file)
                      (drover-with-reporting "Claude Code recap"
                        (drover-claude-code-recap file (and agent t))))
                 (list :failure "No transcript of this conversation")))))

(defun drover-claude-code-transcripts--preview (session callback)
  "Call CALLBACK with the newest reply of SESSION, from its transcript, or nil."
  (drover-claude-code-transcripts--recap
   session (lambda (recap) (funcall callback (plist-get recap :reply)))))

(defun drover-claude-code-transcripts--running-p (id)
  "Return non-nil when Claude Code runs conversation ID elsewhere.
As far as drover knows."
  (and (gethash id drover-claude-code-transcripts--agents)
       (not (eq (car (drover-claude-code-transcripts--status id)) 'idle))))

(defun drover-claude-code-transcripts--delete (session)
  "Delete the conversation of SESSION for good: what the CLI's SDK deletes.
See `drover-claude-code-session-files'.  The backend's :delete."
  (let* ((id (drover-session-id session))
         (file (drover-claude-code-transcripts--file id)))
    (when (drover-claude-code-transcripts--running-p id)
      (user-error "%s runs elsewhere; stop it first" (drover-session-display-title session)))
    (drover-claude-code-delete-session-files
     (drover-claude-code-session-files id (drover-session-directory session)))
    (when file
      (drover-transcript-check (drover-claude-code-transcripts--current-index) file))))

(defun drover-claude-code-transcripts--fork (session)
  "Fork the conversation of SESSION into a new claude-code-ide terminal.
The backend's :fork."
  (or (drover-claude-code-ide-fork (drover-session-id session)
                                   (drover-claude-code-transcripts--directory
                                    (drover-claude-code-transcripts--data (drover-session-id session))))
      (user-error "Could not locate the new Claude Code buffer")))

(defun drover-claude-code-transcripts--supports (session operation)
  "Return non-nil when OPERATION applies to SESSION.
A subagent opens its conversation and tells its last turn; forking needs
claude-code-ide; deleting, a conversation Claude Code does not run
elsewhere; answering, one waiting there.  The backend's :supports."
  (let ((id (drover-session-id session)))
    (if (cdr (drover-claude-code-transcripts--split id))
        (memq operation '(:open :recap :preview))
      (pcase operation
        (:fork (and (fboundp 'drover-claude-code-ide-fork) (featurep 'drover-claude-code-ide) t))
        (:delete (not (drover-claude-code-transcripts--running-p id)))
        (:respond (eq (drover-session-status session) 'waiting))
        (_ t)))))

;;;; Searching

(defconst drover-claude-code-transcripts--text-starts
  '("\"role\":\"(?:user|assistant)\",\"content\":(?:\"|\\[\\{\"type\":\"text\",\"text\":\")")
  "What begins the text of a prompt or reply in a transcript, for ripgrep.
A message's role, then its content: a string, or a block of text first.
Tool calls, tool results and thinking begin otherwise.")

(defun drover-claude-code-transcripts--search-text (session)
  "Return the first and last prompts of the conversation of SESSION, or nil.
As its transcript tells them, for any backend's session of Claude
Code's conversation.  The backend's :search-text."
  (when-let* ((id (drover-session-engine-id session))
              ((eq (drover-session-engine session) 'claude-code))
              (data (drover-claude-code-transcripts--data id)))
    (delq nil (list (plist-get data :prompt) (plist-get data :last-prompt)))))

(defun drover-claude-code-transcripts--search (query scope callback)
  "Search the transcripts for QUERY said in prompts and replies, for CALLBACK.
Those of the conversations the search offers, as the :wanted of SCOPE
tells, are searched with ripgrep (see `drover-transcript-search'), not
those of subagents; CALLBACK gets matches naming each conversation by
its engine and identifier.  The backend's :search."
  (when-let* ((index (drover-claude-code-transcripts--current-index)))
    (let ((wanted (plist-get scope :wanted))
          (files nil))
      (drover-transcript-map
       (lambda (file data)
         (let ((id (plist-get data :id)))
           (when (and id (plist-get data :messages)
                      (or (not (functionp wanted))
                          (funcall wanted (list :engine 'claude-code :id id))))
             (push (list file id (plist-get data :updated)) files))))
       index)
      (drover-transcript-search
       files drover-claude-code-transcripts--text-starts query
       (lambda (result)
         (funcall callback (if (and (consp result) (consp (car result)))
                               (mapcar (lambda (match) (append (list :engine 'claude-code) match))
                                       result)
                             result)))
       (plist-get scope :limit)))))

;;;; Installing

(defun drover-claude-code-transcripts--start ()
  "Look at the transcripts from now on, and at what Claude Code runs elsewhere.
The first time, note it: replies before count as seen (see
`drover-transcript-seen-since').  The backend's :start."
  (drover-transcript-seen-since drover-claude-code-transcripts--name)
  (drover-transcript-start (drover-claude-code-transcripts--current-index))
  (drover-claude-code-transcripts--queue-unread-titles))

(defun drover-claude-code-transcripts--register ()
  "Register the `claude-code' backend."
  (drover-register-backend
   'claude-code
   :label "Claude Code"
   :recorded '(claude-code)
   :recorded-times #'drover-claude-code-transcripts--times
   :recorded-run #'drover-claude-code-transcripts--recorded-run
   :recorded-title #'drover-claude-code-transcripts--recorded-title
   :recorded-seen #'drover-claude-code-transcripts--recorded-seen
   :caches t
   :start #'drover-claude-code-transcripts--start
   :sessions #'drover-claude-code-transcripts--sessions
   :session #'drover-claude-code-transcripts--session
   :history #'drover-claude-code-transcripts--history
   :search #'drover-claude-code-transcripts--search
   :search-text #'drover-claude-code-transcripts--search-text
   :open #'drover-claude-code-transcripts--open
   :respond #'drover-claude-code-transcripts--respond
   :preview #'drover-claude-code-transcripts--preview
   :recap #'drover-claude-code-transcripts--recap
   :mark-seen #'drover-claude-code-transcripts--mark-seen
   :delete #'drover-claude-code-transcripts--delete
   :fork #'drover-claude-code-transcripts--fork
   :supports #'drover-claude-code-transcripts--supports))

(defun drover-claude-code-transcripts-install (front-end)
  "List Claude Code's conversations, for FRONT-END to resume them.
FRONT-END is a symbol, such as `claude-code-ide'; the backend stays
until every front end that installed it uninstalled it.  Installed
again, as a front end loaded anew installs it, it is registered anew,
so that its registration has this file's properties."
  (unless (drover-claude-code-transcripts--current-index)
    (setq drover-claude-code-transcripts--index (drover-claude-code-transcripts--make-index)))
  (drover-claude-code-transcripts--register)
  (cl-pushnew front-end drover-claude-code-transcripts--users)
  (add-hook 'kill-emacs-hook #'drover-claude-code-transcripts--save)
  (add-hook 'drover-recorded-functions #'drover-claude-code-transcripts--recorded))

(defun drover-claude-code-transcripts--recorded (engine file)
  "Look at FILE at once, placed where Claude Code records, when ENGINE is its.
For `drover-recorded-functions'."
  (when-let* (((eq engine 'claude-code))
              (index (drover-claude-code-transcripts--current-index)))
    (drover-transcript-check index file)))

(defun drover-claude-code-transcripts--save ()
  "Keep what was read of the transcripts, as Emacs exits.
An error is reported, and lets Emacs exit."
  (drover-with-reporting "keeping Claude Code's transcripts"
    (when-let* ((index (drover-claude-code-transcripts--current-index)))
      (drover-transcript-save index t))
    (drover-transcript-seen-save)))

(defun drover-claude-code-transcripts-uninstall (front-end)
  "Stop listing Claude Code's conversations for FRONT-END.
The backend goes once no front end needs it."
  (setq drover-claude-code-transcripts--users (delq front-end drover-claude-code-transcripts--users))
  (unless drover-claude-code-transcripts--users
    (when (drover-claude-code-transcripts--current-index)
      (drover-transcript-stop (drover-claude-code-transcripts--current-index)))
    (drover-command-cancel drover-claude-code-transcripts--agents-job)
    (drover-claude-code-transcripts--stop-titles)
    (remove-hook 'kill-emacs-hook #'drover-claude-code-transcripts--save)
    (remove-hook 'drover-recorded-functions #'drover-claude-code-transcripts--recorded)
    (drover-unregister-backend 'claude-code)))

;; Loaded anew, as reloading drover does, the backend a front end
;; installed takes this file's properties.
(when (eq (drover-backend-property 'claude-code :sessions) #'drover-claude-code-transcripts--sessions)
  (drover-claude-code-transcripts--register)
  (add-hook 'drover-recorded-functions #'drover-claude-code-transcripts--recorded)
  ;; What the reader before read is read again.
  (drover-claude-code-transcripts--current-index))

(provide 'drover-claude-code-transcripts)
;;; drover-claude-code-transcripts.el ends here
