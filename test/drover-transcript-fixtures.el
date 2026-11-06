;;; drover-transcript-fixtures.el --- Transcripts and rollouts for tests  -*- lexical-binding: t; -*-

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

;; Claude Code transcripts and Codex rollouts of synthetic content, in
;; the structure the CLIs record: the record types, members and their
;; order as Claude Code 2.1.316 and Codex 0.166 write them, in the
;; throwaway homes of `drover-test-with-homes'.  Large files are made
;; of records the readers skip, as the API request blobs Claude Code
;; records.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-claude-code-transcripts)
(require 'drover-codex-rollouts)

(defun drover-fixture-time (time)
  "Return float TIME as the CLIs write times, ISO 8601 in UTC."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time t))

(defun drover-fixture-line (&rest members)
  "Return a JSON line of MEMBERS, a plist of keywords and values.
Values are strings, numbers, t, `:false', vectors, or plists for
objects."
  (concat (json-serialize (drover-fixture--object members)) "\n"))

(defun drover-fixture--object (plist)
  "Return PLIST as an alist `json-serialize' takes, its plists objects too."
  (let ((alist nil))
    (while plist
      (let ((key (intern (substring (symbol-name (pop plist)) 1)))
            (value (pop plist)))
        (push (cons key (drover-fixture--value value)) alist)))
    (nreverse alist)))

(defun drover-fixture--value (value)
  "Return VALUE as `json-serialize' takes it."
  (cond ((and (consp value) (keywordp (car value))) (drover-fixture--object value))
        ((vectorp value) (vconcat (mapcar #'drover-fixture--value value)))
        (t value)))

(defun drover-fixture-write (file lines &optional append)
  "Write LINES, strings, to FILE, after what it holds when APPEND is non-nil."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (apply #'insert lines)
      (write-region nil nil file append 'silent))))

;;;; Claude Code

(defun drover-fixture-claude-file (id directory)
  "Return the transcript of conversation ID run in DIRECTORY."
  (expand-file-name (concat id ".jsonl")
                    (expand-file-name (drover-claude-code-encode-directory directory)
                                      (drover-claude-code-projects-directory))))

(cl-defun drover-fixture-claude-user (id directory time text &key (entrypoint "cli") (branch "main") meta
                                         sidechain)
  "Return the record of a prompt TEXT of conversation ID in DIRECTORY at TIME.
ENTRYPOINT and BRANCH are recorded; META marks a note the CLI added,
SIDECHAIN a subagent's prompt, the task it was given."
  (apply #'drover-fixture-line
         :parentUuid :null :isSidechain (if sidechain t :false) :promptId (concat "prompt-" id)
         :type "user" :message (list :role "user" :content text)
         (append (and meta (list :isMeta t))
                 (list :uuid (format "%s-u%d" id (truncate time))
                       :timestamp (drover-fixture-time time)
                       :userType "external" :entrypoint entrypoint :cwd directory :sessionId id
                       :version "2.1.316" :gitBranch branch))))

(cl-defun drover-fixture-claude-assistant (id directory time text &key (model "claude-opus-5-5") effort
                                             (entrypoint "cli") (branch "main") sidechain
                                             (stop "end_turn") usage parent message)
  "Return the record of a reply TEXT of conversation ID in DIRECTORY at TIME.
MODEL and EFFORT ran it; SIDECHAIN marks a subagent's.  It stopped for
STOP, and the API counted USAGE, a plist as `drover-fixture-claude-usage'
makes it, or none.  PARENT is the record it follows, by default the
prompt of the same second; MESSAGE its message's identifier, by
default one of its second: the records of one message share it."
  (apply #'drover-fixture-line
         :parentUuid (or parent (format "%s-u%d" id (truncate time)))
         :isSidechain (if sidechain t :false)
         :message (append (list :model model :id (or message (format "msg_%d" (truncate time))) :type "message"
                                :role "assistant" :content (vector (list :type "text" :text text))
                                :stop_reason (or stop :null))
                          (and usage (list :usage usage)))
         :requestId (format "req_%d" (truncate time)) :requestedModel model :type "assistant"
         :uuid (format "%s-a%d" id (truncate time)) :timestamp (drover-fixture-time time)
         (append (and effort (list :effort effort))
                 (list :userType "external" :entrypoint entrypoint :cwd directory :sessionId id
                       :version "2.1.316" :gitBranch branch))))

(defun drover-fixture-claude-meta (id type key value)
  "Return the record of TYPE Claude Code appends of conversation ID, KEY VALUE."
  (drover-fixture-line :type type key value :sessionId id))

(defun drover-fixture-claude-blob (id bytes)
  "Return records of conversation ID the readers skip, about BYTES long in all.
As the API requests Claude Code records."
  (let ((lines nil)
        (left bytes))
    (while (> left 0)
      (let ((size (min left 200000)))
        (push (drover-fixture-line :type "api-request-blob" :sessionId id
                                   :blob (make-string (max 1 (- size 80)) ?x))
              lines)
        (setq left (- left size))))
    lines))

(cl-defun drover-fixture-claude (id directory &key (start 1790000000.0) (prompt "Fix the login redirect")
                                    (reply "The redirect keeps the page.") ai-title custom-title
                                    (model "claude-opus-5-5") effort (mode "default") (entrypoint "cli")
                                    (branch "main") (filler 0) subagents)
  "Write the transcript of a conversation ID run in DIRECTORY, and return its file.
It starts at START with the records the CLI writes first, a command
note, then PROMPT, REPLY by MODEL at EFFORT, FILLER bytes of request
blobs, then AI-TITLE, CUSTOM-TITLE and the last prompt the CLI appends.
SUBAGENTS is a list of (NAME DESCRIPTION TYPE . REPLY) written beside
it, each a prompt, DESCRIPTION, and a reply, REPLY the keyword
arguments of `drover-fixture-claude-assistant' for it, such as :model,
:effort and :usage."
  (make-directory directory t)
  (let ((file (drover-fixture-claude-file id directory)))
    (drover-fixture-write
     file
     (append
      (list (drover-fixture-claude-meta id "mode" :mode "normal")
            (drover-fixture-claude-meta id "permission-mode" :permissionMode mode)
            (drover-fixture-line :type "file-history-snapshot" :messageId (concat id "-m")
                                 :snapshot (list :messageId (concat id "-m")
                                                 :timestamp (drover-fixture-time start)
                                                 :trackedFileBackups (list :none :false))
                                 :isSnapshotUpdate :false)
            (drover-fixture-claude-user id directory start "<command-name>/model</command-name>"
                                        :entrypoint entrypoint :branch branch :meta t)
            (drover-fixture-claude-user id directory (+ start 1) prompt :entrypoint entrypoint :branch branch)
            (drover-fixture-claude-assistant id directory (+ start 2) reply :model model :effort effort
                                             :entrypoint entrypoint :branch branch))
      (drover-fixture-claude-blob id filler)
      (and ai-title (list (drover-fixture-claude-meta id "ai-title" :aiTitle ai-title)))
      (and custom-title (list (drover-fixture-claude-meta id "custom-title" :customTitle custom-title)))
      (list (drover-fixture-claude-meta id "last-prompt" :lastPrompt prompt))))
    (pcase-dolist (`(,name ,description ,type . ,reply) subagents)
      (let ((base (expand-file-name (concat id "/subagents/" name) (file-name-directory file))))
        (drover-fixture-write (concat base ".jsonl")
                              (list (drover-fixture-claude-user id directory (+ start 3) description
                                                                :sidechain t)
                                    (apply #'drover-fixture-claude-assistant id directory (+ start 3) "Found it."
                                           :sidechain t reply)))
        (drover-fixture-write (concat base ".meta.json")
                              (list (json-serialize `((agentType . ,type) (description . ,description)))))))
    (set-file-times file (seconds-to-time (+ start 3)))
    file))

(defun drover-fixture-claude-append (file &rest lines)
  "Append LINES to the transcript FILE, as the CLI does as the conversation goes on."
  (drover-fixture-write file lines t))

(defun drover-fixture-claude-reopened (id directory time)
  "Return the records the CLI appends to conversation ID in DIRECTORY reopened at TIME.
As Claude Code 2.1 writes them as it resumes a conversation, before
any prompt: its own state again, the reply it makes up for a prompt
left without one, its notes, and a task's notification kept for the
transcript only."
  (let ((stamp (drover-fixture-time time)))
    (list (drover-fixture-line :type "queue-operation" :operation "enqueue" :timestamp stamp
                               :sessionId id :content "<task-notification>done</task-notification>")
          (drover-fixture-line :type "queue-operation" :operation "dequeue" :timestamp stamp :sessionId id)
          (drover-fixture-line :parentUuid :null :isSidechain :false :type "assistant"
                               :message (list :model "<synthetic>" :role "assistant" :type "message"
                                              :content (vector (list :type "text" :text "No response requested.")))
                               :isApiErrorMessage :false :uuid (format "%s-s%d" id (truncate time))
                               :timestamp stamp :userType "external" :entrypoint "cli" :cwd directory
                               :sessionId id :version "2.1.316" :gitBranch "main")
          (drover-fixture-line :parentUuid :null :isSidechain :false :type "system" :subtype "informational"
                               :content "Resumed" :isMeta :false :level "info" :timestamp stamp
                               :uuid (format "%s-i%d" id (truncate time)) :cwd directory :sessionId id)
          (drover-fixture-claude-user id directory time "<local-command-caveat>Caveat</local-command-caveat>"
                                      :meta t)
          (drover-fixture-line :parentUuid :null :isSidechain :false :promptId (concat "prompt-" id)
                               :type "user" :message (list :role "user" :content "<task-notification>done</task-notification>")
                               :origin (list :kind "task-notification") :promptSource "system"
                               :queueTranscriptOnly t :uuid (format "%s-q%d" id (truncate time))
                               :timestamp stamp :userType "external" :entrypoint "cli" :cwd directory
                               :sessionId id :version "2.1.316" :gitBranch "main")
          (drover-fixture-line :type "file-history-snapshot" :messageId (concat id "-r")
                               :snapshot (list :messageId (concat id "-r") :timestamp stamp
                                               :trackedFileBackups (list :none :false))
                               :isSnapshotUpdate :false)
          (drover-fixture-claude-meta id "last-prompt" :lastPrompt "Fix the login redirect")
          (drover-fixture-claude-meta id "ai-title" :aiTitle "Keep the login page")
          (drover-fixture-claude-meta id "mode" :mode "normal")
          (drover-fixture-claude-meta id "permission-mode" :permissionMode "default")
          (drover-fixture-claude-meta id "atis-latch" :atis :false)
          (drover-fixture-line :type "cost-state" :sessionId id :totalCostUSD 0)
          (drover-fixture-line :type "tools-used" :sessionId id :tools []))))

(cl-defun drover-fixture-claude-usage (&key (input 6) (hour 0) (minutes 0) (read 0) (output 120))
  "Return the usage the API counts for a reply, as Claude Code records it.
INPUT tokens of the prompt uncached, HOUR and MINUTES written into the
cache of an hour and of five minutes, READ read from it, and OUTPUT."
  (list :input_tokens input :cache_creation_input_tokens (+ hour minutes) :cache_read_input_tokens read
        :output_tokens output :server_tool_use (list :web_search_requests 0 :web_fetch_requests 0)
        :service_tier "standard"
        :cache_creation (list :ephemeral_1h_input_tokens hour :ephemeral_5m_input_tokens minutes)
        :inference_geo "" :iterations [] :speed "standard"))

(defun drover-fixture-claude-tool-result (id directory time)
  "Return the record of a tool's result of conversation ID in DIRECTORY at TIME."
  (drover-fixture-line
   :parentUuid (format "%s-a%d" id (1- (truncate time))) :isSidechain :false :promptId (concat "prompt-" id)
   :type "user" :message (list :role "user" :content (vector (list :tool_use_id "toolu_1" :type "tool_result"
                                                                   :content "ok")))
   :uuid (format "%s-u%d" id (truncate time)) :timestamp (drover-fixture-time time)
   :userType "external" :entrypoint "cli" :cwd directory :sessionId id :version "2.1.316"
   :gitBranch "main"))

(defun drover-fixture-claude-api-error (id directory time text)
  "Return the record of an error of the API ending a turn of conversation ID at TIME.
TEXT is what the CLI tells of it, in DIRECTORY."
  (drover-fixture-line
   :parentUuid (format "%s-u%d" id (truncate time)) :isSidechain :false
   :message (list :id (format "err_%d" (truncate time)) :container :null :model "<synthetic>"
                  :role "assistant" :stop_reason "stop_sequence" :stop_sequence ""
                  :type "message" :usage (drover-fixture-claude-usage :input 0 :output 0)
                  :content (vector (list :type "text" :text text)))
   :type "assistant" :uuid (format "%s-e%d" id (truncate time)) :timestamp (drover-fixture-time time)
   :error "server_error" :isApiErrorMessage t :userType "external" :entrypoint "cli" :cwd directory
   :sessionId id :version "2.1.316" :gitBranch "main"))

(defun drover-fixture-claude-exit (id directory time)
  "Return the records of /exit typed in conversation ID in DIRECTORY at TIME."
  (list (drover-fixture-claude-user id directory time
                                    "<command-name>/exit</command-name>\n<command-message>exit</command-message>")
        (drover-fixture-claude-user id directory time
                                    "<local-command-stdout>Bye!</local-command-stdout>")))

;;;; Codex

(defun drover-fixture-codex-file (id time &optional archived)
  "Return the rollout of conversation ID started at TIME, ARCHIVED or not."
  (let ((name (format "rollout-%s-%s.jsonl" (format-time-string "%Y-%m-%dT%H-%M-%S" time t) id)))
    (if archived
        (expand-file-name name (expand-file-name "archived_sessions" drover-codex-home))
      (expand-file-name name (expand-file-name (format-time-string "sessions/%Y/%m/%d" time t)
                                               drover-codex-home)))))

(cl-defun drover-fixture-codex (id directory &key (start 1790000000.0) (prompt "Add the export command")
                                   (reply "The export command writes CSV.") (model "gpt-5.5")
                                   (effort "high") (source "cli") parent archived (instructions 2000)
                                   (branch "feature"))
  "Write the rollout of a Codex conversation ID run in DIRECTORY, and return its file.
Its session_meta carries INSTRUCTIONS bytes of base instructions, its
git BRANCH after them; SOURCE is its origin, PARENT the conversation
spawning a subagent.  Then the context Codex adds, PROMPT, the turn's
context with MODEL and EFFORT, a patch and REPLY."
  (make-directory directory t)
  (let ((file (drover-fixture-codex-file id start archived))
        (time (drover-fixture-time start)))
    (drover-fixture-write
     file
     (list
      (drover-fixture-line
       :timestamp time :type "session_meta"
       :payload (list :session_id id :id id :timestamp time :cwd directory :originator "codex_cli_rs"
                      :cli_version "0.166.0"
                      :source (if parent
                                  (list :subagent (list :thread_spawn (list :parent_thread_id parent
                                                                            :depth 1)))
                                source)
                      :model_provider "openai"
                      :base_instructions (list :text (make-string instructions ?i))
                      :git (list :branch branch)))
      (drover-fixture-line :timestamp time :type "response_item"
                           :payload (list :type "message" :role "developer"
                                          :content (vector (list :type "input_text" :text "<permissions>"))))
      (drover-fixture-line :timestamp time :type "response_item"
                           :payload (list :type "message" :role "user"
                                          :content (vector (list :type "input_text"
                                                                 :text "<environment_context>…</environment_context>"))))
      (drover-fixture-line :timestamp time :type "event_msg"
                           :payload (list :type "user_message" :message prompt :images []))
      (drover-fixture-line :timestamp time :type "response_item"
                           :payload (list :type "message" :role "user"
                                          :content (vector (list :type "input_text" :text prompt))))
      (drover-fixture-line :timestamp time :type "turn_context"
                           :payload (list :cwd directory :model model :effort effort :summary "auto"))
      (drover-fixture-line :timestamp time :type "response_item"
                           :payload (list :type "custom_tool_call" :status "completed" :call_id "call_1"
                                          :name "apply_patch"
                                          :input "*** Begin Patch\n*** Update File: src/export.rs\n@@\n-old\n+new\n+more\n*** Add File: src/csv.rs\n+one\n*** End Patch\n"))
      (drover-fixture-line :timestamp time :type "response_item"
                           :payload (list :type "custom_tool_call_output" :call_id "call_1" :output "Done"))
      (drover-fixture-line :timestamp (drover-fixture-time (+ start 5)) :type "event_msg"
                           :payload (list :type "agent_message" :message reply))
      (drover-fixture-line :timestamp (drover-fixture-time (+ start 5)) :type "response_item"
                           :payload (list :type "message" :role "assistant"
                                          :content (vector (list :type "output_text" :text reply))))
      (drover-fixture-line :timestamp (drover-fixture-time (+ start 6)) :type "event_msg"
                           :payload (list :type "token_count" :info :null))))
    (set-file-times file (seconds-to-time (+ start 6)))
    file))

(defun drover-fixture-codex-reopened (id directory time)
  "Return the records Codex may append to conversation ID in DIRECTORY reopened at TIME.
Records of its own, before any prompt: the session's metadata, the
context of the next turn, the state of the world, the settings applied,
the tokens counted and what it tells the model as the developer."
  (let ((stamp (drover-fixture-time time)))
    (list (drover-fixture-line :timestamp stamp :type "session_meta"
                               :payload (list :id id :timestamp stamp :cwd directory
                                              :originator "codex_cli_rs" :cli_version "0.166.0"))
          (drover-fixture-line :timestamp stamp :type "event_msg"
                               :payload (list :type "thread_settings_applied" :model "gpt-5.5"))
          (drover-fixture-line :timestamp stamp :type "response_item"
                               :payload (list :type "message" :role "developer"
                                              :content (vector (list :type "input_text" :text "<permissions>"))))
          (drover-fixture-line :timestamp stamp :type "world_state" :payload (list :cwd directory))
          (drover-fixture-line :timestamp stamp :type "turn_context"
                               :payload (list :cwd directory :model "gpt-5.5" :effort "high"))
          (drover-fixture-line :timestamp stamp :type "event_msg"
                               :payload (list :type "token_count" :info :null)))))

(defun drover-fixture-codex-event (time type &rest members)
  "Return an event of TYPE Codex records at TIME, with MEMBERS, a plist."
  (drover-fixture-line :timestamp (drover-fixture-time time) :type "event_msg"
                       :payload (append (list :type type) members)))

(cl-defun drover-fixture-codex-tokens (time &key (input 30000) (cached 20000) (output 400) window)
  "Return the count of tokens Codex records at TIME of its last request.
INPUT tokens of the prompt, CACHED of them read from the cache, OUTPUT
generated; WINDOW the model's context window, as Codex fills it."
  (drover-fixture-codex-event
   time "token_count"
   :info (append (list :total_token_usage (list :input_tokens (* 3 input) :cached_input_tokens (* 2 cached)
                                                :output_tokens (* 3 output) :reasoning_output_tokens 0
                                                :total_tokens (* 3 (+ input output)))
                       :last_token_usage (list :input_tokens input :cached_input_tokens cached
                                               :output_tokens output :reasoning_output_tokens 0
                                               :total_tokens (+ input output)))
                 (and window (list :model_context_window window)))
   :rate_limits :null))

(defun drover-fixture-codex-name (id name)
  "Append to session_index.jsonl the NAME Codex gave conversation ID."
  (drover-fixture-write (expand-file-name "session_index.jsonl" drover-codex-home)
                        (list (drover-fixture-line :id id :thread_name name
                                                   :updated_at "2026-10-31T10:00:00Z"))
                        t))

(provide 'drover-transcript-fixtures)
;;; drover-transcript-fixtures.el ends here
