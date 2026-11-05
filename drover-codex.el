;;; drover-codex.el --- The Codex engine  -*- lexical-binding: t; -*-

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

;; The `codex' engine: what a new Codex session starts with, read from
;; its config.toml and models cache, and when it last took a prompt,
;; from the rollouts it writes.  Its models come from live sessions
;; only (see `drover-agent-shell'): the CLI has no command listing them.
;; Each model takes its name and reasoning levels from the models cache,
;; and from the levels Codex's ACP agent lists for each model
;; (`drover-codex-acp-models').  The rollouts Codex records of its
;; conversations tell their prompts, replies and patches, of which
;; `drover-codex-recap' tells the last turn.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)
(require 'drover-transcript)

(declare-function drover-codex-rollouts--file "drover-codex-rollouts" (id))

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

(defcustom drover-codex-home (expand-file-name (or (getenv "CODEX_HOME") "~/.codex"))
  "Codex's home directory."
  :type 'directory
  :group 'drover)

(defun drover-codex-recorded-p ()
  "Return non-nil when Codex recorded conversations, archived or not."
  (or (file-directory-p (expand-file-name "sessions" drover-codex-home))
      (file-directory-p (expand-file-name "archived_sessions" drover-codex-home))))

;;;; Defaults

(defun drover-codex-config ()
  "Return Codex's string settings from config.toml, as (KEY . VALUE) pairs.
The keys of the selected `profile' table come before the top-level
ones, so `assoc' finds the value Codex applies.  Only plain strings are
read."
  (let ((file (expand-file-name "config.toml" drover-codex-home))
        (tables nil)
        (table nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Enough TOML for the keys read: table headers, and keys with a
        ;; string value on one line.  Arrays of tables hold none of them.
        (while (not (eobp))
          (cond
           ((looking-at "[ \t]*\\[\\[")
            (setq table :array))
           ((looking-at "[ \t]*\\[[ \t]*\\([^]\n]+?\\)[ \t]*\\]")
            (setq table (replace-regexp-in-string "[\"']" "" (match-string 1))))
           ((looking-at "[ \t]*\\([A-Za-z0-9_-]+\\)[ \t]*=[ \t]*\\(?:\"\\([^\"\n]*\\)\"\\|'\\([^'\n]*\\)'\\)")
            (push (cons (match-string 1) (or (match-string 2) (match-string 3)))
                  (alist-get table tables nil nil #'equal))))
          (forward-line 1)))
      (let* ((top (alist-get nil tables))
             (profile (cdr (assoc "profile" top))))
        (append (and profile (alist-get (concat "profiles." profile) tables nil nil #'equal))
                top)))))

(defun drover-codex--catalog-model (id)
  "Return the entry Codex's models cache keeps for the model ID, or nil."
  (let ((cache (drover-read-json-cached (expand-file-name "models_cache.json" drover-codex-home))))
    (and (drover-json-object-p cache)
         (seq-find (lambda (model) (equal (alist-get 'slug model) id))
                   (alist-get 'models cache)))))

(defun drover-codex-effort (id config)
  "Return the reasoning effort a new Codex session runs model ID at, or nil.
It is CONFIG's `model_reasoning_effort', else the model's default in
Codex's models cache.  The effort is a symbol."
  (when-let* ((level (or (cdr (assoc "model_reasoning_effort" config))
                         (alist-get 'default_reasoning_level (drover-codex--catalog-model id))))
              ((stringp level)))
    (intern level)))

(defun drover-codex-model (id &optional config name)
  "Return the `drover-model' of Codex model ID.
Codex's models cache names it and gives its reasoning levels as
variants, unknown when the cache does not list it; its default variant
is the effort a new session runs it at with CONFIG, Codex's settings
\(see `drover-codex-effort').  NAME names a model the cache does not."
  (let ((entry (drover-codex--catalog-model id))
        (provider (cdr (assoc "model_provider" config))))
    (drover-model-create
     :provider-name (if (member provider '(nil "openai")) "OpenAI" provider)
     :id id
     :name (or (alist-get 'display_name entry) name)
     :variants (mapcar (lambda (level) (list (intern (alist-get 'effort level))))
                       (seq-filter (lambda (level) (stringp (alist-get 'effort level)))
                                   (alist-get 'supported_reasoning_levels entry)))
     :default-variant (drover-codex-effort id config)
     :variants-known (and entry t)
     :engines (delete-dups (cons 'codex (drover-model-engines-for nil id))))))

(defun drover-codex-acp-model-efforts (values)
  "Return the reasoning levels of each model VALUES name.
The value is an alist (ID . LEVELS).  VALUES are the model identifiers
Codex's ACP agent lists apart from its model option, one per model and
level, as \"ID[LEVEL]\"; LEVELS are symbols, in order.  Other values
are ignored."
  (let ((efforts nil))
    (dolist (value values)
      (when (and (stringp value) (string-match "\\`\\([^[]+\\)\\[\\([^]]+\\)\\]\\'" value))
        (let ((id (match-string 1 value))
              (level (intern (match-string 2 value))))
          (if-let* ((entry (assoc id efforts)))
              (unless (memq level (cdr entry))
                (setcdr entry (append (cdr entry) (list level))))
            (push (list id level) efforts)))))
    (nreverse efforts)))

(defun drover-codex-acp-models (options current levels efforts &optional config)
  "Return the models an ACP session of Codex lists.
OPTIONS are the values of the session's model option, in order, as
\(VALUE . NAME), each VALUE a model identifier.  The model the session
runs, CURRENT, has the variants LEVELS, the reasoning levels the session
offers, a list of symbols; the others those EFFORTS, as
`drover-codex-acp-model-efforts' returns them, give, else Codex's
models cache.  CONFIG is Codex's settings, by default those of its
config.toml, which give default variants."
  (let ((config (or config (drover-codex-config))))
    (drover-model-merge-all
     (mapcar (pcase-lambda (`(,value . ,name))
               (let ((model (drover-codex-model value config name))
                     (known (if (equal value current) levels (alist-get value efforts nil nil #'equal))))
                 (when (or known (equal value current))
                   (setf (drover-model-variants model) (mapcar #'list known)
                         (drover-model-variants-known model) t))
                 model))
             options))))

(defun drover-codex-defaults ()
  "Return what a new Codex session starts with, as a plist.
:model is the `drover-model' of config.toml's model, nil when it names
none; :config lets `drover-codex-effort' answer for another model.  This
is the engine's :defaults data."
  (let* ((config (drover-codex-config))
         (id (cdr (assoc "model" config))))
    (list :model (and id (drover-codex-model id config))
          :config config)))

(defun drover-codex--fetch-defaults (_directory callback)
  "Call CALLBACK with the defaults of a new Codex session.
They are read at once; a failure is reported, and CALLBACK not called."
  (when-let* ((defaults (condition-case err
                            (drover-codex-defaults)
                          (error
                           (drover-warn "Could not read Codex's defaults: %s" (drover-error-message err))
                           nil))))
    (funcall callback defaults)))

(defun drover-codex--resolve (defaults agent model variant)
  "Return the options a new Codex session runs, from DEFAULTS.
AGENT, MODEL and VARIANT are the explicit choices, each nil when not
made; DEFAULTS is a plist of `drover-codex-defaults', or nil.  The
model is config.toml's unless chosen, and the variant the effort Codex
gives the model; the agent is the one chosen."
  (let ((model (or model (plist-get defaults :model))))
    (list :agent agent
          :model model
          :variant (or variant
                       (and model
                            (drover-codex-effort (drover-model-id model) (plist-get defaults :config)))))))

(defun drover-codex--last-used ()
  "Return when a Codex session last wrote its rollout, as a float time, or nil.
Rollouts sit under sessions/YEAR/MONTH/DAY of the day their session
started, and a session can outlive its day, so every one is checked."
  (let ((latest nil))
    (dolist (day (ignore-errors
                   (mapcan (lambda (month) (directory-files month t "\\`[0-9]+\\'"))
                           (mapcan (lambda (year) (directory-files year t "\\`[0-9]+\\'"))
                                   (directory-files (expand-file-name "sessions" drover-codex-home)
                                                    t "\\`[0-9]+\\'")))))
      (dolist (entry (ignore-errors
                       (directory-files-and-attributes day nil "\\`rollout-.*\\.jsonl\\'" t)))
        (let ((time (float-time (file-attribute-modification-time (cdr entry)))))
          (when (or (null latest) (> time latest))
            (setq latest time)))))
    latest))

;;;; Rollouts

(defconst drover-codex--rollout-tail 262144
  "Bytes at the end of a rollout `drover-codex-recap' reads.")

(defconst drover-codex--context-regexp
  "\\`[ \t\n]*\\(?:<[a-z_]+\\|# AGENTS\\.md instructions\\)"
  "Regexp matching the text of a user message Codex adds itself.
Such as its environment context and the instructions of AGENTS.md.")

(defun drover-codex-message-text (payload)
  "Return the text of the message PAYLOAD of a rollout, joined, or nil.
Parts Codex adds itself to a user message are left out (see
`drover-codex--context-regexp')."
  (let ((content (alist-get 'content payload)))
    (when-let* ((texts (delq nil (mapcar (lambda (part)
                                           (let ((text (and (consp part) (alist-get 'text part))))
                                             (and (stringp text)
                                                  (not (string-match-p drover-codex--context-regexp text))
                                                  text)))
                                         (and (listp content) content))))
                (text (string-trim (string-join texts "\n")))
                ((not (string-empty-p text))))
      text)))

(defun drover-codex-prompt (record)
  "Return the prompt of the user rollout RECORD gives, or nil.
Codex records a prompt twice, as an event and as a message of the
conversation: either tells it."
  (let ((payload (alist-get 'payload record)))
    (pcase (alist-get 'type record)
      ("event_msg"
       (when (equal (alist-get 'type payload) "user_message")
         (let ((message (alist-get 'message payload)))
           (and (stringp message)
                (not (string-match-p drover-codex--context-regexp message))
                (not (string-blank-p message))
                (string-trim message)))))
      ("response_item"
       (when (and (equal (alist-get 'type payload) "message")
                  (equal (alist-get 'role payload) "user"))
         (drover-codex-message-text payload))))))

(defun drover-codex-reply (record)
  "Return the text of the agent's message rollout RECORD gives, or nil."
  (let ((payload (alist-get 'payload record)))
    (pcase (alist-get 'type record)
      ("event_msg"
       (when (equal (alist-get 'type payload) "agent_message")
         (let ((message (alist-get 'message payload)))
           (and (stringp message) (not (string-blank-p message)) (string-trim message)))))
      ("response_item"
       (when (and (equal (alist-get 'type payload) "message")
                  (equal (alist-get 'role payload) "assistant"))
         (drover-codex-message-text payload))))))

(defun drover-codex--patch-changes (patch)
  "Return the files PATCH, in Codex's apply_patch format, changes.
A list of (FILE ADDITIONS DELETIONS), in its order."
  (let ((changes nil)
        (current nil))
    (dolist (line (split-string patch "\n"))
      (cond ((string-match "\\`\\*\\*\\* \\(?:Add\\|Update\\|Delete\\) File: \\(.+\\)\\'" line)
             (setq current (list (string-trim (match-string 1 line)) 0 0))
             (push current changes))
            ((string-match "\\`\\*\\*\\* Move to: \\(.+\\)\\'" line)
             (when current (setcar current (string-trim (match-string 1 line)))))
            ((not current))
            ((string-prefix-p "+" line) (cl-incf (nth 1 current)))
            ((string-prefix-p "-" line) (cl-incf (nth 2 current)))))
    (nreverse changes)))

(defun drover-codex--changes (record)
  "Return the files the patch of rollout RECORD applies change, or nil."
  (let ((payload (alist-get 'payload record)))
    (when (and (equal (alist-get 'type record) "response_item")
               (member (alist-get 'type payload) '("custom_tool_call" "function_call"))
               (equal (alist-get 'name payload) "apply_patch"))
      (let ((input (or (alist-get 'input payload)
                       (let ((arguments (alist-get 'arguments payload)))
                         (and (stringp arguments)
                              (let ((parsed (ignore-errors (json-parse-string arguments :object-type 'alist))))
                                (or (and (consp parsed) (alist-get 'input parsed)) arguments)))))))
        (and (stringp input) (drover-codex--patch-changes input))))))

(defun drover-codex-recap (file)
  "Return a recap of the last turn rollout FILE records, as `drover-recap' does.
The turn begins at the newest prompt; its reply is the agent's newest
message since, its changes the files its patches changed, and its error
the one Codex reported last.  Only the end of FILE is read."
  (let ((records nil)
        prompt reply error changes)
    (when (file-readable-p file)
      (with-temp-buffer
        (let ((size (file-attribute-size (file-attributes file)))
              (coding-system-for-read 'utf-8-unix))
          (insert-file-contents file nil (max 0 (- size drover-codex--rollout-tail)) size))
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((record (ignore-errors
                                (json-parse-string (buffer-substring-no-properties (point) (line-end-position))
                                                   :object-type 'alist :array-type 'list
                                                   :null-object nil :false-object nil)))
                      ((consp record)))
            (push record records))
          (forward-line 1))))
    (catch 'turn
      (dolist (record records)
        (cond ((setq prompt (drover-codex-prompt record)) (throw 'turn nil))
              ((drover-codex-reply record)
               (unless reply (setq reply (drover-codex-reply record))))
              ((drover-codex--changes record)
               (setq changes (append (drover-codex--changes record) changes)))
              ((and (equal (alist-get 'type record) "event_msg")
                    (member (alist-get 'type (alist-get 'payload record)) '("error" "stream_error")))
               (unless error
                 (setq error (alist-get 'message (alist-get 'payload record))))))))
    (list :prompt prompt
          :reply reply
          :changes (let ((merged nil))
                     (dolist (change changes)
                       (if-let* ((known (assoc (car change) merged)))
                           (setcdr known (list (+ (nth 1 known) (nth 1 change))
                                               (+ (nth 2 known) (nth 2 change))))
                         (push (copy-sequence change) merged)))
                     (nreverse merged))
          :error (and (stringp error) error))))

;;;; Changes

(defconst drover-codex--turn-tail (* 8 1024 1024)
  "Bytes at the end of a rollout looked at first for the last turn's changes.
Past them, the whole rollout is read, the newest prompt being further
back.")

(defun drover-codex--patch-edits (patch)
  "Return the edits PATCH, in Codex's apply_patch format, makes.
A list of (FILE STATUS PATCH), in its order, as
`drover-transcript-changes' takes them: an update's hunks keep the
lines of context Codex placed them by, in their headers, with no line
numbers, which the patch does not tell; a file added has its lines; a
file deleted no patch, its lines unknown."
  (let ((edits nil)
        (current nil))
    (cl-flet ((close ()
                (when current
                  (pcase-let ((`(,file ,status ,kind ,lines) current))
                    (let ((lines (nreverse lines)))
                      (push (list file status
                                  (pcase kind
                                    ('add (drover-transcript-hunk 0 0 1 (length lines) lines))
                                    ('delete "")
                                    (_ (mapconcat (lambda (line) (concat line "\n")) lines ""))))
                            edits)))
                  (setq current nil))))
      (dolist (line (split-string patch "\n"))
        (cond ((string-match "\\`\\*\\*\\* \\(Add\\|Update\\|Delete\\) File: \\(.+\\)\\'" line)
               (close)
               (let ((kind (intern (downcase (match-string 1 line)))))
                 (setq current (list (string-trim (match-string 2 line))
                                     (pcase kind ('add 'added) ('delete 'deleted) (_ 'modified))
                                     kind nil))))
              ((not current))
              ((string-match "\\`\\*\\*\\* Move to: \\(.+\\)\\'" line)
               (setf (nth 1 current) (cons 'renamed (car current))
                     (nth 0 current) (string-trim (match-string 1 line))))
              ((string-prefix-p "***" line)
               (unless (string-prefix-p "*** End of File" line)
                 (close)))
              ((string-prefix-p "@@" line)
               (push line (nth 3 current)))
              ((and (string-match-p "\\`[-+ ]" line))
               (unless (or (eq (nth 2 current) 'add) (nth 3 current))
                 ;; Lines before any header begin a hunk of their own.
                 (push "@@" (nth 3 current)))
               (push line (nth 3 current)))))
      (close))
    (nreverse edits)))

(defun drover-codex--file-changes (changes)
  "Return the edits CHANGES, a map of files to what Codex did to each, made.
As Codex records a patch applied: an alist of file names, as symbols,
to objects of `type' add, with the `content' added, delete, with the
`content' deleted, or update, with its `unified_diff' and the
`move_path' a file moved to.  A list of (FILE STATUS PATCH)."
  (let ((edits nil))
    (pcase-dolist (`(,file . ,change) (and (listp changes) changes))
      (let ((file (if (symbolp file) (symbol-name file) file)))
        (when (and (stringp file) (drover-json-object-p change))
          (let-alist change
            (pcase .type
              ("add" (push (list file 'added (drover-transcript-whole-hunk (if (stringp .content) .content "") t))
                           edits))
              ("delete" (push (list file 'deleted
                                    (drover-transcript-whole-hunk (if (stringp .content) .content "") nil))
                              edits))
              ("update"
               (let ((patch (if (stringp .unified_diff) .unified_diff "")))
                 ;; Headers before the first hunk name the file again.
                 (when-let* ((start (string-match "^@@" patch)))
                   (setq patch (substring patch start)))
                 (push (if (and (stringp .move_path) (not (string-empty-p .move_path)))
                           (list .move_path (cons 'renamed file) patch)
                         (list file 'modified patch))
                       edits))))))))
    (nreverse edits)))

(defun drover-codex--record-edits (record)
  "Return (KIND ID EDITS) of the patch rollout RECORD tells of, or nil.
KIND is `raw' for the apply_patch call, its input parsed, ID its call's;
`applied' for what Codex recorded it applied, a patch_apply_end event
or a file change completed, ID the call's too: those win over the call
they tell of, telling line numbers.  A patch that failed or was
declined changed nothing."
  (let ((payload (alist-get 'payload record)))
    (when (drover-json-object-p payload)
      (pcase (list (alist-get 'type record) (alist-get 'type payload))
        (`("response_item" ,(or "custom_tool_call" "function_call"))
         (when-let* (((drover-codex--changes record))
                     (input (or (alist-get 'input payload)
                                (let ((arguments (alist-get 'arguments payload)))
                                  (and (stringp arguments)
                                       (let ((parsed (ignore-errors
                                                       (json-parse-string arguments :object-type 'alist))))
                                         (or (and (consp parsed) (alist-get 'input parsed)) arguments))))))
                     ((stringp input)))
           (list 'raw (alist-get 'call_id payload) (drover-codex--patch-edits input))))
        (`("event_msg" "patch_apply_end")
         (list 'applied (alist-get 'call_id payload)
               (and (alist-get 'success payload)
                    (member (or (alist-get 'status payload) "completed") '("completed"))
                    (drover-codex--file-changes (alist-get 'changes payload)))))
        (`("event_msg" "item_completed")
         (let ((item (alist-get 'item payload)))
           (when (and (drover-json-object-p item) (equal (alist-get 'type item) "FileChange"))
             (list 'applied (alist-get 'id item)
                   (and (member (or (alist-get 'status item) "completed") '("completed"))
                        (drover-codex--file-changes (alist-get 'changes item)))))))))))

(defun drover-codex-changes (id scope directory callback)
  "Find the files Codex conversation ID changed, and call CALLBACK with them.
SCOPE is `turn', the changes since its newest prompt, or `session',
every change; DIRECTORY is where it ran.  They are the patches it
applied, as its rollout recorded them (see
`drover-codex--record-edits'), gathered by file (see
`drover-transcript-changes').  The rollout is read in slices, and
CALLBACK called once, later, with (:files FILES :notes NOTES), or
\(:failure MESSAGE).  The engine's :changes."
  (let* ((file (or (and (fboundp 'drover-codex-rollouts--file) (drover-codex-rollouts--file id))
                   (drover-codex-rollout-file id)))
         (size (and file (file-attribute-size (file-attributes file)))))
    (if (not size)
        (funcall callback (list :failure "No rollout of this conversation"))
      (cl-labels
          ((scan (from)
             (let ((prompt nil)
                   (patches nil))
               (drover-transcript-read-lines
                (list (cons file (or from 0)))
                '("\"apply_patch\"" "\"patch_apply_end\"" "\"FileChange\"" "\"user_message\"" "\"role\":\"user\"")
                (lambda (_file record)
                  (if (drover-codex-prompt record)
                      (progn (setq prompt t)
                             (when (eq scope 'turn)
                               (setq patches nil)))
                    (when-let* ((patch (drover-codex--record-edits record)))
                      (push patch patches))))
                (lambda (failure)
                  (cond
                   ((and failure (null patches))
                    (funcall callback (list :failure failure)))
                   ((and (eq scope 'turn) (not prompt) from)
                    ;; The newest prompt is further back.
                    (scan nil))
                   (t
                    (let* ((patches (nreverse patches))
                           (applied (delq nil (mapcar (lambda (patch)
                                                        (and (eq (car patch) 'applied) (nth 1 patch)))
                                                      patches)))
                           (edits (mapcan (lambda (patch)
                                            (unless (and (eq (car patch) 'raw) (nth 1 patch)
                                                         (member (nth 1 patch) applied))
                                              (copy-sequence (nth 2 patch))))
                                          patches)))
                      (funcall callback (list :files (drover-transcript-changes edits directory)
                                              :notes (and failure (list failure))))))))))))
        (scan (and (eq scope 'turn) (> size drover-codex--turn-tail)
                   (- size drover-codex--turn-tail)))))))

;;;; Where conversations ran

(defconst drover-codex--meta-limit 1048576
  "Bytes of a rollout read for its first record, which tells its directory.
The record holds the instructions the session started with, tens of
kilobytes.")

(defun drover-codex-rollout-file (id)
  "Return the rollout of Codex conversation ID, archived or not, or nil.
Rollouts are named rollout-TIME-ID.jsonl, under a directory per day."
  (let ((regexp (concat "\\`rollout-.*-" (regexp-quote id) "\\.jsonl\\'")))
    (seq-some (lambda (root)
                (and (file-directory-p root)
                     (car (directory-files-recursively root regexp))))
              (list (expand-file-name "sessions" drover-codex-home)
                    (expand-file-name "archived_sessions" drover-codex-home)))))

(defun drover-codex-session-directory (id)
  "Return the directory Codex conversation ID ran in, or nil.
It is the cwd of the session_meta record that begins its rollout."
  (when-let* ((file (drover-codex-rollout-file id)))
    (with-temp-buffer
      (insert-file-contents file nil 0 drover-codex--meta-limit)
      (goto-char (point-min))
      (let* ((meta (ignore-errors (json-parse-buffer :object-type 'alist :null-object nil
                                                     :false-object nil)))
             (payload (and (consp meta) (alist-get 'payload meta)))
             (cwd (and (consp payload) (alist-get 'cwd payload))))
        (and (stringp cwd) (not (string-empty-p cwd)) (file-name-as-directory cwd))))))

(defun drover-codex--directory (id then else)
  "Call THEN with the directory Codex conversation ID ran in, else ELSE.
The engine's :directory (see `drover-register-engine')."
  (if-let* ((directory (drover-codex-session-directory id)))
      (funcall then directory)
    (funcall else (format "No rollout of Codex conversation %s tells its directory"
                          (drover-bound-line id)))))

;;;; Cloning to another host

(defconst drover-codex--rollout-name-regexp
  "\\`rollout-\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T[^/\\\\]*\\.jsonl\\'"
  "What the name of a rollout is: the day it began, in its name.")

(defun drover-codex--export (id callback)
  "Call CALLBACK with what another host needs to clone conversation ID.
Its rollout, archived or not, to its last whole record, and how its
records spell the directory it ran in.  What Codex keeps elsewhere of
it, its state database's row and its shell snapshots, stays here; Codex
finds a rollout its database does not know by its name, and records
it then.  The engine's :export."
  (funcall callback
           (if-let* ((file (drover-codex-rollout-file id)))
               (list :parts (list (list (file-name-nondirectory file) file (drover-lines-size file)))
                     :cwd (drover-recorded-cwd file))
             (list :failure (format "No rollout of Codex conversation %s" (drover-bound-line id))))))

(defun drover-codex--record-directory (directory)
  "Return DIRECTORY as Codex's records spell the directory it runs in.
Without its trailing slash; on Windows with backslashes and an upper
case drive letter.  The engine's :record-directory."
  (let ((name (directory-file-name (expand-file-name directory))))
    (if (string-match "\\`\\([a-zA-Z]\\):\\(.*\\)\\'" name)
        (concat (upcase (match-string 1 name)) ":"
                (replace-regexp-in-string "/" "\\\\" (match-string 2 name)))
      name)))

(defun drover-codex--import (id parts _data _directory then else)
  "Make conversation ID, of PARTS received, one of this host's.
Its rollout goes among the sessions of the day it began, under its
name, unarchived; one of that conversation here already is not
replaced.  Its records name the directory it runs in, rewritten as it
arrived.  THEN gets ID, ELSE why it could not.  The engine's :import."
  (let* ((name (car (car parts)))
         (day (and (= (length parts) 1) (stringp id) (stringp name)
                   (string-match drover-codex--rollout-name-regexp name)
                   (string-suffix-p (concat "-" id ".jsonl") name)
                   (format "sessions/%s/%s/%s"
                           (match-string 1 name) (match-string 2 name) (match-string 3 name)))))
    (cond
     ((not day)
      (funcall else "not a rollout of that conversation"))
     ((drover-codex-rollout-file id)
      (funcall else "Codex recorded this conversation here already"))
     (t
      (condition-case err
          (let ((target (expand-file-name name (expand-file-name day drover-codex-home))))
            (make-directory (file-name-directory target) t)
            (rename-file (cdr (car parts)) target)
            (drover-run-hook-reporting 'drover-recorded-functions 'codex target)
            (funcall then id))
        (error (funcall else (drover-error-message err))))))))

;;;; Registration

(defun drover-codex--register ()
  "Register the `codex' engine."
  (drover-register-engine 'codex
                          :label "Codex"
                          :defaults #'drover-codex--fetch-defaults
                          :volatile t
                          :resolve #'drover-codex--resolve
                          :last-used #'drover-codex--last-used
                          :directory #'drover-codex--directory
                          :changes #'drover-codex-changes
                          :export #'drover-codex--export
                          :import #'drover-codex--import
                          :record-directory #'drover-codex--record-directory))

(drover-codex--register)

(defun drover-codex-unload-function ()
  "Forget the `codex' engine, and its model sources."
  (drover-unregister-engine 'codex)
  nil)

(provide 'drover-codex)
;;; drover-codex.el ends here
