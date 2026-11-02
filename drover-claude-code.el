;;; drover-claude-code.el --- The Claude Code engine  -*- lexical-binding: t; -*-

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

;; The `claude-code' engine: what a new Claude Code session starts with,
;; read from the CLI's local state as the CLI itself resolves it (start
;; flags, environment, settings layers and its cached model catalog),
;; its models, when it last took a prompt, where it keeps the
;; transcripts and identifiers of its sessions, what their last turns
;; were (`drover-claude-code-recap'), and how to delete one
;; (`drover-claude-code-session-files').  It needs no front end:
;; `drover-claude-code-ide' and `drover-agent-shell' show its sessions.
;;
;; The defaults follow Claude Code's documented precedence for the
;; model, the effort and the permission mode.  The model catalog the
;; CLI caches resolves aliases, names, supported effort levels and the
;; defaults it serves.  The starting permission mode stands for an
;; agent of `drover-claude-code-agent-modes': `plan' while it is plan,
;; none where the built-in default decides, as that differs between
;; front ends.  A top-level `effortLevel' of user settings no longer
;; reaches newer models; the CLI keeps the list of those it does reach
;; internally, so `drover-claude-code-user-effort-models' mirrors it.
;; macOS configuration profiles, merged managed sources and effort pins
;; at launch are not modeled.
;;
;; Claude Code's models come from its live sessions: agent-shell's
;; shells (see `drover-agent-shell') and, while claude-code-ide runs a
;; session, the catalog that session's CLI caches
;; (`drover-claude-code-catalog-models', see `drover-claude-code-ide').
;; `drover-model' remembers what they reported for when none runs.
;; Shells list aliases, which the catalog resolves to the model they
;; stand for, so that a model is one whatever names it, each with the
;; effort levels the catalog gives it (`drover-claude-code-acp-models').

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

(declare-function w32-read-registry "w32fns.c" (root key &optional name))

;;;; Options

(defcustom drover-claude-code-home
  (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))
  "Claude Code's user configuration directory."
  :type 'directory
  :group 'drover)

(defcustom drover-claude-code-managed-directory
  (pcase system-type
    ('windows-nt "C:/Program Files/ClaudeCode")
    ('darwin "/Library/Application Support/ClaudeCode")
    (_ "/etc/claude-code"))
  "Directory of Claude Code's file-based managed settings."
  :type 'directory
  :group 'drover)

(defconst drover-claude-code-effort-levels '(low medium high xhigh max)
  "Claude Code's effort levels, lowest first.")

(defcustom drover-claude-code-user-effort-models
  '("haiku-3-5" "sonnet-3-5" "sonnet-3-7" "haiku-4-5" "sonnet-4" "sonnet-4-5"
    "sonnet-4-6" "sonnet-5" "opus-4" "opus-4-1" "opus-4-5" "opus-4-6" "opus-4-7"
    "opus-4-8" "opus-5" "fable-5" "fable-5-1" "mythos-5" "mythos-5-1")
  "Keys of the models a top-level `effortLevel' of user settings applies to.
It mirrors Claude Code 2.1.296: later models ignore it, and start at
their own default until a level is saved for them.  Keys are
\"FAMILY-VERSION\", a zero minor dropped, such as \"opus-4-5\"."
  :type '(repeat string)
  :group 'drover)

(defcustom drover-claude-code-alias-families
  '(("best" . "opus") ("opusplan" . "sonnet"))
  "Claude Code model aliases selecting a model of another family.
Each entry is (ALIAS . FAMILY); every other alias names its own family.
`opusplan' runs Sonnet outside plan mode."
  :type '(alist :key-type (string :tag "Alias") :value-type (string :tag "Family"))
  :group 'drover)

(defcustom drover-claude-code-agent-modes '(("plan" . "plan") ("build" . "auto"))
  "Agents Claude Code runs as permission modes, as (AGENT . MODE).
Planning and building are permission modes in Claude Code rather than
agents, and its `--agent' refuses a name no agent definition provides."
  :type '(alist :key-type (string :tag "Agent") :value-type (string :tag "Permission mode"))
  :group 'drover)

(defvar drover-claude-code-oauth-catalog nil
  "Ignored: drover no longer asks Anthropic for Claude Code's models.
They come from live sessions, and drover remembers them (see
`drover-remember-models').")

(make-obsolete-variable 'drover-claude-code-oauth-catalog
                        "models come from live sessions, remembered on disk."
                        "0.1.0")

(defvar drover-claude-code-start-flags-function nil
  "Function of no argument returning the flags of new Claude Code sessions.
They are a string of command line flags, such as \"--model opus\", or
nil, and take precedence over settings.  `drover-claude-code-ide' sets
it.")

(defvar drover-claude-code-project-directory-function nil
  "Function of a DIRECTORY returning where a session for it starts, or nil.
Claude Code reads project settings there.  Nil starts in DIRECTORY
itself.  `drover-claude-code-ide' sets it.")

;;;; Agents

(defun drover-claude-code-agent-mode (agent)
  "Return the permission mode Claude Code runs AGENT as, or nil.
See `drover-claude-code-agent-modes'."
  (cdr (assoc agent drover-claude-code-agent-modes)))

;;;; Sessions on disk

(defconst drover-claude-code--uuid-regexp
  "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\'"
  "Regexp matching a whole session identifier, a UUID.")

(defun drover-claude-code-projects-directory ()
  "Return the directory of Claude Code's transcripts, one subdirectory per project."
  (file-name-as-directory (expand-file-name "projects" drover-claude-code-home)))

(defun drover-claude-code-sessions-directory ()
  "Return the directory where Claude Code records its running sessions by PID."
  (file-name-as-directory (expand-file-name "sessions" drover-claude-code-home)))

(defconst drover-claude-code--encoded-limit 200
  "Length past which Claude Code cuts the name of a directory of transcripts.
It then appends a hash of the whole name, as `drover-claude-code--hash'.")

(defun drover-claude-code--utf-16 (name)
  "Return the UTF-16 code units of NAME, as JavaScript counts its characters."
  (mapcan (lambda (char)
            (if (> char #xffff)
                (let ((offset (- char #x10000)))
                  (list (+ #xd800 (ash offset -10)) (+ #xdc00 (logand offset #x3ff))))
              (list char)))
          (string-to-list name)))

(defun drover-claude-code--hash (name)
  "Return the hash Claude Code appends to the encoded NAME it cuts, in base 36.
It is the 32-bit string hash of JavaScript's code, h * 31 + c over the
UTF-16 code units of NAME, as a signed number, made positive."
  (let ((hash 0))
    (dolist (unit (drover-claude-code--utf-16 name))
      (setq hash (logand (+ (* hash 31) unit) #xffffffff)))
    (let ((value (abs (if (>= hash #x80000000) (- hash #x100000000) hash)))
          (digits nil))
      (while (progn (push (aref "0123456789abcdefghijklmnopqrstuvwxyz" (% value 36)) digits)
                    (setq value (/ value 36))
                    (> value 0)))
      (concat digits))))

(defun drover-claude-code--encode (name)
  "Return NAME as Claude Code names a directory of transcripts after it.
Every UTF-16 code unit that is not an ASCII letter or digit becomes
\"-\", so a character beyond the Basic Multilingual Plane becomes two;
a result longer than `drover-claude-code--encoded-limit' is cut there,
followed by \"-\" and a hash of NAME."
  (let ((encoded (mapconcat (lambda (unit)
                              (if (or (<= ?a unit ?z) (<= ?A unit ?Z) (<= ?0 unit ?9))
                                  (string unit)
                                "-"))
                            (drover-claude-code--utf-16 name) "")))
    (if (<= (length encoded) drover-claude-code--encoded-limit)
        encoded
      (concat (substring encoded 0 drover-claude-code--encoded-limit) "-" (drover-claude-code--hash name)))))

(defun drover-claude-code-native-directory (dir)
  "Return DIR, absolute, as Claude Code's process sees its working directory.
Without its trailing slash; on Windows with backslashes and an upper
case drive letter, as Node reports the directory a terminal started it
in, where Emacs may write \"c:/\"."
  (let ((name (directory-file-name dir)))
    (if (string-match "\\`\\([a-zA-Z]\\):\\(.*\\)\\'" name)
        (concat (upcase (match-string 1 name)) ":"
                (replace-regexp-in-string "/" "\\\\" (match-string 2 name)))
      name)))

(defun drover-claude-code-encode-directory (dir)
  "Return the name of the subdirectory of transcripts of sessions in DIR.
Claude Code resolves symbolic links, drops the trailing slash and
encodes the name as `drover-claude-code--encode' does."
  (drover-claude-code--encode
   (drover-claude-code-native-directory (file-truename (expand-file-name dir)))))

(defun drover-claude-code-decode-bucket (encoded)
  "Return the directory ENCODED, a subdirectory name of transcripts, stands for.
Encoding loses characters, so the directory is found by listing the
file system level by level, encoding each name.  Return it with a
trailing slash, or nil."
  (cl-labels
      ((probe (base remaining)
         (if (string-empty-p remaining)
             (file-name-as-directory base)
           (catch 'found
             (dolist (entry (ignore-errors (directory-files base nil nil t)))
               (unless (member entry '("." ".."))
                 (let* ((full (expand-file-name entry base))
                        (encoded (drover-claude-code--encode entry))
                        (size (length encoded)))
                   (when (file-directory-p full)
                     (cond
                      ((string= remaining encoded)
                       (throw 'found (file-name-as-directory full)))
                      ;; Several names may encode alike: a wrong one finds
                      ;; nothing below it, and the next one is tried.
                      ((and (> (length remaining) size)
                            (string= (substring remaining 0 (1+ size)) (concat encoded "-")))
                       (when-let* ((found (probe full (substring remaining (1+ size)))))
                         (throw 'found found))))))))))))
    (cond
     ((string-prefix-p "-" encoded)
      (probe "/" (substring encoded 1)))
     ;; A Windows drive root, "c:/", encodes as "c--".
     ((string-match "\\`\\([a-zA-Z]\\)--" encoded)
      (probe (concat (match-string 1 encoded) ":/") (substring encoded (match-end 0)))))))

(defun drover-claude-code-session-directory (session-id)
  "Return the directory session SESSION-ID ran in, or nil.
It is decoded from the subdirectory of transcripts holding its
transcript."
  (let ((root (drover-claude-code-projects-directory))
        (transcript (concat session-id ".jsonl")))
    (seq-some (lambda (bucket)
                (and (file-directory-p bucket)
                     (file-exists-p (expand-file-name transcript bucket))
                     (drover-claude-code-decode-bucket (file-name-nondirectory bucket))))
              (and (file-directory-p root) (directory-files root t "\\`[^.]")))))

(defun drover-claude-code--directory (session-id then else)
  "Call THEN with the directory session SESSION-ID ran in, else ELSE.
The engine's :directory (see `drover-register-engine')."
  (if-let* ((directory (drover-claude-code-session-directory session-id)))
      (funcall then directory)
    (funcall else (format "No project directory holds Claude Code session %s"
                          (drover-bound-line session-id)))))

(defun drover-claude-code-pid-session-id (pid)
  "Return the identifier of the session the Claude Code process PID runs, or nil.
The CLI records it, by PID, under `drover-claude-code-sessions-directory'."
  (when (integerp pid)
    ;; Not cached: the record changes as the session is resumed or switched.
    (let* ((record (drover-read-json (expand-file-name (format "%d.json" pid)
                                                       (drover-claude-code-sessions-directory))))
           (id (and (drover-json-object-p record) (alist-get 'sessionId record))))
      (and (eql (alist-get 'pid record) pid)
           (stringp id)
           (string-match-p drover-claude-code--uuid-regexp id)
           id))))

(defun drover-claude-code--bucket (directory)
  "Return the subdirectory of transcripts of sessions in DIRECTORY."
  (expand-file-name (drover-claude-code-encode-directory directory)
                    (drover-claude-code-projects-directory)))

(defun drover-claude-code-newest-session-id (directory)
  "Return the identifier of the session last written in DIRECTORY, or nil.
It is the newest transcript of DIRECTORY's subdirectory of transcripts."
  (when-let* ((bucket (drover-claude-code--bucket directory))
              ((file-directory-p bucket))
              (files (directory-files bucket t "\\.jsonl\\'" t)))
    (file-name-base (car (sort files :key (lambda (file) (or (drover-file-mtime file) 0))
                               :reverse t)))))

(defun drover-claude-code-transcript (id dir)
  "Return the transcript file of session ID, started in DIR."
  (expand-file-name (concat id ".jsonl") (drover-claude-code--bucket dir)))

(defun drover-claude-code-session-files (id &optional dir)
  "Return the files of Claude Code session ID, or signal a `user-error'.
The value is (TRANSCRIPT . DIRECTORY): the transcript, in the
subdirectory of transcripts of DIR when given and holding it, else in
the one subdirectory of `drover-claude-code-projects-directory' that
does; and DIRECTORY, the directory of the session's subagent
transcripts and tool results beside it, or nil when there is none.
These are what Claude Code's SDK deletes to delete a session.  ID must
be a session identifier, a UUID: nothing else is ever looked for."
  (unless (and (stringp id) (string-match-p drover-claude-code--uuid-regexp id))
    (user-error "Not a Claude Code session: %S" id))
  (let* ((name (concat id ".jsonl"))
         (root (drover-claude-code-projects-directory))
         (own (and dir (drover-claude-code-transcript id dir)))
         (found (if (and own (file-regular-p own))
                    (list own)
                  (seq-filter #'file-regular-p
                              (mapcar (lambda (bucket) (expand-file-name name bucket))
                                      (and (file-directory-p root)
                                           (seq-filter #'file-directory-p
                                                       (directory-files root t "\\`[^.]"))))))))
    (pcase found
      ('nil (user-error "No transcript of Claude Code session %s" id))
      (`(,transcript)
       (let ((directory (expand-file-name id (file-name-directory transcript))))
         (cons transcript (and (file-directory-p directory)
                               (not (file-symlink-p directory))
                               directory))))
      (_ (user-error "Several transcripts of Claude Code session %s; deleting none" id)))))

(defun drover-claude-code-delete-session-files (files)
  "Delete the FILES of a Claude Code session, its transcript and directory.
FILES are what `drover-claude-code-session-files' returns.  They are
moved to the trash when `delete-by-moving-to-trash' says so."
  (delete-file (car files) t)
  (when (cdr files)
    (delete-directory (cdr files) t t)))

(defconst drover-claude-code--transcript-tail 262144
  "Bytes at the end of a transcript `drover-claude-code-last-turn' reads.")

(defun drover-claude-code-last-turn (file)
  "Return (MODEL-ID . EFFORT) of the newest main turn transcript FILE records.
Only the end of the file is read.  EFFORT is a symbol, nil for a model
without effort.  Nil when no turn is found."
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents file nil (max 0 (- size drover-claude-code--transcript-tail)) size))
      (goto-char (point-max))
      (let (turn)
        ;; Searching the key first skips lines that cannot be turns
        ;; without parsing them; the record itself decides.
        (while (and (not turn) (re-search-backward "\"requestedModel\":\"" nil t))
          (when-let* ((record (ignore-errors
                                (json-parse-string (buffer-substring-no-properties
                                                    (line-beginning-position) (line-end-position))
                                                   :object-type 'alist :null-object nil
                                                   :false-object nil)))
                      ((drover-json-object-p record))
                      ((equal (alist-get 'type record) "assistant"))
                      ((not (alist-get 'isSidechain record)))
                      (model (alist-get 'requestedModel record))
                      ((stringp model)))
            (let ((effort (alist-get 'effort record)))
              (setq turn (cons model (and (stringp effort) (intern effort))))))
          (beginning-of-line))
        turn))))

(defun drover-claude-code--transcript-records (file)
  "Return the records at the end of transcript FILE, newest first.
Only the last `drover-claude-code--transcript-tail' bytes are read; a
line cut by that, or that is not a JSON object, is left out."
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents file nil (max 0 (- size drover-claude-code--transcript-tail)) size))
      (let ((records nil))
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((record (ignore-errors
                                (json-parse-string (buffer-substring-no-properties
                                                    (point) (line-end-position))
                                                   :object-type 'alist :array-type 'list
                                                   :null-object nil :false-object nil)))
                      ((drover-json-object-p record)))
            (push record records))
          (forward-line 1))
        records))))

(defun drover-claude-code--texts (content)
  "Return the texts of message CONTENT, a string or a list of blocks, joined."
  (let ((text (if (stringp content)
                  content
                (string-join (delq nil (mapcar (lambda (block)
                                                 (and (drover-json-object-p block)
                                                      (equal (alist-get 'type block) "text")
                                                      (alist-get 'text block)))
                                               content))
                             "\n"))))
    (setq text (string-trim (or text "")))
    (unless (string-empty-p text) text)))

(defconst drover-claude-code--not-prompt-regexp
  "\\`\\(?:<\\(?:command-\\|local-command-\\|bash-\\)\\|\\[Request interrupted by user\\)"
  "Regexp matching the start of a text of the user that is no prompt.
Commands Claude Code runs and their output, and the note it records
when the user interrupts a turn, \"[Request interrupted by user]\".")

(defun drover-claude-code--prompt-p (record)
  "Return the text RECORD gives as a prompt of the user, or nil.
Tool results, notes Claude Code adds for itself, the commands it runs,
such as /model, and its note of an interruption are no prompt."
  (when (and (equal (alist-get 'type record) "user")
             (not (alist-get 'isMeta record))
             (not (alist-get 'toolUseResult record)))
    (let ((content (alist-get 'content (alist-get 'message record))))
      (unless (and (listp content)
                   (seq-some (lambda (block)
                               (and (drover-json-object-p block)
                                    (equal (alist-get 'type block) "tool_result")))
                             content))
        (when-let* ((text (drover-claude-code--texts content))
                    ((not (string-match-p drover-claude-code--not-prompt-regexp text))))
          text)))))

(defun drover-claude-code--change (record)
  "Return what RECORD's tool result changed, as (FILE ADDITIONS DELETIONS), or nil.
A file written whole adds its lines; an edit adds and deletes those of
its patch."
  (let ((result (alist-get 'toolUseResult record)))
    (when-let* (((drover-json-object-p result))
                (file (alist-get 'filePath result))
                ((stringp file)))
      (if (equal (alist-get 'type result) "create")
          (let ((content (alist-get 'content result)))
            (list file (if (and (stringp content) (not (string-empty-p content))) (length (string-lines content)) 0) 0))
        (let ((additions 0)
              (deletions 0))
          (dolist (hunk (alist-get 'structuredPatch result))
            (dolist (line (and (drover-json-object-p hunk) (alist-get 'lines hunk)))
              (when (stringp line)
                (cond ((string-prefix-p "+" line) (cl-incf additions))
                      ((string-prefix-p "-" line) (cl-incf deletions))))))
          (and (or (> additions 0) (> deletions 0)
                   (assq 'structuredPatch result))
               (list file additions deletions)))))))

(defun drover-claude-code-recap (file)
  "Return a recap of the last turn transcript FILE records, as `drover-recap' does.
The turn begins at the newest prompt of the user; its reply is the
newest text of the main agent since, its error that of an API error
the CLI recorded, and its changes the files its tools wrote or edited,
with the lines they added and deleted.  Subagents' records are left
out.  Only the end of FILE is read: a turn longer than that has no
prompt, unless the CLI recorded the last prompt apart."
  (let (prompt last-prompt reply reply-id error changes)
    (catch 'turn
      (dolist (record (drover-claude-code--transcript-records file))
        (unless (alist-get 'isSidechain record)
          (pcase (alist-get 'type record)
            ("last-prompt"
             (unless last-prompt
               (setq last-prompt (alist-get 'lastPrompt record))))
            ("assistant"
             (let* ((message (alist-get 'message record))
                    (text (drover-claude-code--texts (alist-get 'content message))))
               (cond ((not text))
                     ((alist-get 'isApiErrorMessage record)
                      (unless error (setq error text)))
                     ((not reply)
                      (setq reply text
                            reply-id (alist-get 'id message)))
                     ;; The CLI records each block of a message apart.
                     ((and reply-id (equal reply-id (alist-get 'id message)))
                      (setq reply (concat text "\n" reply))))))
            ("user"
             (if-let* ((text (drover-claude-code--prompt-p record)))
                 (progn (setq prompt text)
                        (throw 'turn nil))
               (when-let* ((change (drover-claude-code--change record)))
                 ;; Walking back in time, pushing puts the oldest first.
                 (push change changes))))))))
    (list :prompt (or prompt (and (stringp last-prompt) (not (string-blank-p last-prompt)) last-prompt))
          :reply reply
          :changes (let ((merged nil))
                     (dolist (change changes)
                       (if-let* ((known (assoc (car change) merged)))
                           (setcdr known (list (+ (nth 1 known) (nth 1 change))
                                               (+ (nth 2 known) (nth 2 change))))
                         (push (copy-sequence change) merged)))
                     (nreverse merged))
          :error error)))

;;;; Settings

(defun drover-claude-code--registry-policy (root)
  "Return the Claude Code policy stored in the Windows registry hive ROOT, or nil."
  (when-let* (((fboundp 'w32-read-registry))
              (json (ignore-errors
                      (w32-read-registry root "SOFTWARE\\Policies\\ClaudeCode" "Settings")))
              ((stringp json)))
    (ignore-errors
      (json-parse-string json :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))

(defun drover-claude-code--managed-file-policy ()
  "Return managed-settings.json with its drop-in directory merged over it.
Drop-ins apply in the order of their names.  Nil when neither exists."
  (let* ((directory drover-claude-code-managed-directory)
         (base (drover-read-json (expand-file-name "managed-settings.json" directory)))
         (drop-ins (delq nil (mapcar #'drover-read-json
                                     (ignore-errors
                                       (directory-files (expand-file-name "managed-settings.d" directory)
                                                        t "\\`[^.].*\\.json\\'"))))))
    (when (or base drop-ins)
      (seq-reduce #'drover-merge-settings drop-ins base))))

(defun drover-claude-code--policy ()
  "Return the managed settings Claude Code enforces, or nil.
Only the first source present applies: the settings the server
manages, the machine's policy, the managed file, then the user's
policy."
  (or (drover-read-json (expand-file-name "remote-settings.json" drover-claude-code-home))
      (drover-claude-code--registry-policy 'HKLM)
      (drover-claude-code--managed-file-policy)
      (drover-claude-code--registry-policy 'HKCU)))

(defun drover-claude-code-settings-layers (dir)
  "Return Claude Code's settings for a session in DIR, highest precedence first.
Each entry is (SOURCE . SETTINGS), SOURCE being `policy', `local',
`project' or `user'.  In the home directory, the project settings are
the user ones, read once as user settings."
  (let* ((project (expand-file-name ".claude" dir))
         (home-p (file-equal-p project drover-claude-code-home)))
    (seq-filter #'cdr
                (list (cons 'policy (drover-claude-code--policy))
                      (unless home-p
                        (cons 'local (drover-read-json (expand-file-name "settings.local.json" project))))
                      (unless home-p
                        (cons 'project (drover-read-json (expand-file-name "settings.json" project))))
                      (cons 'user (drover-read-json
                                   (expand-file-name "settings.json" drover-claude-code-home)))))))

(defun drover-claude-code--flag (name)
  "Return the value of the last option NAME of new sessions' start flags, or nil.
NAME is the option without its dashes, such as \"model\" for --model.
The flags come from `drover-claude-code-start-flags-function'."
  (let ((args (ignore-errors
                (split-string-and-unquote
                 (or (and drover-claude-code-start-flags-function
                          (funcall drover-claude-code-start-flags-function))
                     ""))))
        (option (concat "--" name))
        value)
    (while args
      (let ((arg (pop args)))
        (cond ((equal arg option) (setq value (pop args)))
              ((string-prefix-p (concat option "=") arg)
               (setq value (substring arg (1+ (length option))))))))
    value))

;;;; Models

(defun drover-claude-code-catalog ()
  "Return the model catalog Claude Code last cached, or nil.
Its `config' lists every model's name, short name and effort options,
its `state' the default model and the default effort of each model."
  (let* ((directory (expand-file-name "cache/model-catalog" drover-claude-code-home))
         (file (car (sort (ignore-errors (directory-files directory t "-cc\\.json\\'" t))
                          :key (lambda (file) (or (drover-file-mtime file) 0))
                          :reverse t)))
         (catalog (and file (alist-get 'catalog (drover-read-json-cached file)))))
    (and (drover-json-object-p catalog)
         (equal (alist-get 'surface catalog) "cc")
         catalog)))

(defun drover-claude-code--model-key (id)
  "Return the family-version key of the Claude model ID, a zero minor dropped."
  (when-let* ((key (and id (drover-model-anthropic-key id))))
    (string-remove-suffix "-0" key)))

(defun drover-claude-code--base-id (id)
  "Return model ID without a bracketed context suffix, such as [1m]."
  (replace-regexp-in-string "\\[[^]]*\\]\\'" "" id))

(defun drover-claude-code--effort-levels (entry)
  "Return the effort levels the CLI runs the model of catalog ENTRY at, as symbols.
They are the levels of the entry's `runtime', which the CLI checks an
effort against, else the options of its picker, which may leave some
out, such as max.  Nil for a model without effort."
  (mapcar (lambda (level) (intern (downcase (string-trim level))))
          (seq-filter #'stringp
                      (or (map-nested-elt entry '(runtime effort_levels))
                          (mapcar (lambda (option) (alist-get 'id option))
                                  (map-nested-elt entry '(thinking effort_options)))))))

(defun drover-claude-code--provider-alias-target (family catalog)
  "Return the model CATALOG names for the alias of FAMILY, or nil.
FAMILY is a family alias, such as \"opus\".  Bedrock and Vertex, which
Claude Code reaches when CLAUDE_CODE_USE_BEDROCK or
CLAUDE_CODE_USE_VERTEX is set, may have targets of their own."
  (let ((targets (alist-get (intern family) (map-nested-elt catalog '(config provider_alias_targets))))
        (provider (cond ((drover-getenv "CLAUDE_CODE_USE_BEDROCK") 'bedrock)
                        ((drover-getenv "CLAUDE_CODE_USE_VERTEX") 'vertex))))
    (seq-find #'stringp (list (and provider (map-nested-elt targets (list 'per_provider provider)))
                              (alist-get 'default targets)))))

(defun drover-claude-code--catalog-model (catalog id)
  "Return CATALOG's entry for the model ID, matched exactly or by model key."
  (let ((models (map-nested-elt catalog '(config models)))
        (base (drover-claude-code--base-id id)))
    (or (seq-find (lambda (model) (equal (alist-get 'id model) base)) models)
        (when-let* ((key (drover-claude-code--model-key base)))
          (seq-find (lambda (model) (equal (drover-claude-code--model-key (alist-get 'id model)) key))
                    models)))))

(defun drover-claude-code--resolve-model (name catalog)
  "Return the model identifier the model setting NAME selects.
Full identifiers stay as they are.  An alias takes its
ANTHROPIC_DEFAULT_FAMILY_MODEL environment variable, else the target
CATALOG gives the alias, else CATALOG's model of that family, its main
section first; `default' takes the default CATALOG serves.  A bracketed
suffix such as [1m] is kept.  NAME itself when nothing resolves it."
  (if (or (null name) (string-match-p "claude-" name))
      name
    (let* ((base (drover-claude-code--base-id name))
           (alias (downcase base))
           (family (or (cdr (assoc alias drover-claude-code-alias-families)) alias))
           (models (map-nested-elt catalog '(config models)))
           (family-p (lambda (model)
                       (equal (downcase (or (alist-get 'short_name model) "")) family)))
           (id (if (equal alias "default")
                   (map-nested-elt catalog '(state model))
                 (or (drover-getenv (format "ANTHROPIC_DEFAULT_%s_MODEL" (upcase family)))
                     (drover-claude-code--provider-alias-target family catalog)
                     (alist-get 'id (or (seq-find (lambda (model)
                                                    (and (equal (alist-get 'section model) "main")
                                                         (funcall family-p model)))
                                                  models)
                                        (seq-find family-p models)))))))
      (if (stringp id) (concat id (substring name (length base))) name))))

(defun drover-claude-code-startup-model-id (layers catalog)
  "Return the identifier of the model a new session starts on, or nil.
Claude Code's precedence: --model in the start flags (see
`drover-claude-code-start-flags-function'), ANTHROPIC_MODEL, the first
of the settings LAYERS naming a `model', ANTHROPIC_DEFAULT_MODEL, then
CATALOG's default."
  (drover-claude-code--resolve-model
   (or (drover-claude-code--flag "model")
       (drover-getenv "ANTHROPIC_MODEL")
       (seq-some (lambda (layer) (alist-get 'model (cdr layer))) layers)
       (drover-getenv "ANTHROPIC_DEFAULT_MODEL")
       (map-nested-elt catalog '(state model)))
   catalog))

(defun drover-claude-code-startup-mode (layers)
  "Return the permission mode a new session starts in, or nil.
Claude Code's precedence: --permission-mode in the start flags, then
the first of the settings LAYERS with a `permissions.defaultMode'.
Project and local settings cannot start `auto' nor
`bypassPermissions'."
  (or (drover-claude-code--flag "permission-mode")
      (when-let* ((layer (seq-find (lambda (layer)
                                     (map-nested-elt (cdr layer) '(permissions defaultMode)))
                                   layers))
                  (mode (map-nested-elt (cdr layer) '(permissions defaultMode))))
        ;; Refused there, `auto' leaves the built-in default, which
        ;; differs between front ends, and `bypassPermissions' starts
        ;; the default mode.
        (if (and (memq (car layer) '(project local))
                 (member mode '("auto" "bypassPermissions")))
            (and (equal mode "bypassPermissions") "default")
          mode))))

(defun drover-claude-code--layer-effort (layer key base catalog)
  "Return the effort settings LAYER saves for the model BASE of KEY, or nil.
A level of `modelSettings' wins, the exact identifier before an alias
or another identifier of the model, which CATALOG resolves.  A
top-level `effortLevel' applies to every model, except in user
settings: there only to the models of
`drover-claude-code-user-effort-models' and unknown ones."
  (let* ((settings (cdr layer))
         (entries (seq-filter (lambda (entry) (alist-get 'effortLevel (cdr-safe entry)))
                              (alist-get 'modelSettings settings)))
         (entry (or (seq-find (lambda (entry) (equal (symbol-name (car entry)) base)) entries)
                    (and key
                         (seq-find (lambda (entry)
                                     (equal (drover-claude-code--model-key
                                             (drover-claude-code--resolve-model
                                              (symbol-name (car entry)) catalog))
                                            key))
                                   entries)))))
    (or (alist-get 'effortLevel (cdr entry))
        (and (or (not (eq (car layer) 'user))
                 (null key)
                 (member key drover-claude-code-user-effort-models))
             (alist-get 'effortLevel settings)))))

(defun drover-claude-code--default-effort (entry catalog)
  "Return the effort CATALOG serves as the default of its model ENTRY, or nil.
It is the level CATALOG's state keeps for the model, else the default
of the model's runtime, else the option the CLI's picker recommends."
  (let ((id (alist-get 'id entry)))
    (seq-find #'stringp
              (list (map-nested-elt (seq-find (lambda (model) (equal (alist-get 'id model) id))
                                              (map-nested-elt catalog '(state thinking_by_model)))
                                    '(thinking effort))
                    (map-nested-elt entry '(runtime default_effort))
                    (alist-get 'id (seq-find (lambda (option)
                                               (equal (map-nested-elt option '(badge message)) "Recommended"))
                                             (map-nested-elt entry '(thinking effort_options))))))))

(defun drover-claude-code--clamp-effort (level supported cap)
  "Return LEVEL lowered to CAP, then to the highest SUPPORTED level below it.
LEVEL is a symbol, SUPPORTED a list of symbols, nil when unknown, and
CAP a level name or nil.  A LEVEL outside
`drover-claude-code-effort-levels' is returned as it is."
  (let* ((levels drover-claude-code-effort-levels)
         (cap (and cap (intern cap)))
         (level (if (and (memq level levels) (memq cap levels)
                         (> (seq-position levels level) (seq-position levels cap)))
                    cap
                  level)))
    (if (or (null supported) (not (memq level levels)) (memq level supported))
        level
      (car (last (seq-filter (lambda (candidate)
                               (and (memq candidate supported)
                                    (<= (seq-position levels candidate) (seq-position levels level))))
                             levels))))))

(defun drover-claude-code-effort (id layers catalog)
  "Return the effort a new session runs model ID at, a symbol, or nil.
Claude Code's precedence: CLAUDE_CODE_EFFORT_LEVEL (`auto' being the
model's default), --effort in the start flags, the first of the
settings LAYERS with a level for the model, then CATALOG's default for
it.  The level is lowered to any `maxEffortLevel', and to one the model
supports; a model without effort levels runs none."
  (let* ((base (drover-claude-code--base-id id))
         (key (drover-claude-code--model-key base))
         (entry (drover-claude-code--catalog-model catalog base))
         (supported (drover-claude-code--effort-levels entry))
         (env (drover-getenv "CLAUDE_CODE_EFFORT_LEVEL"))
         (level (cond
                 ((and entry (null supported)) nil)
                 ((and env (not (equal env "auto"))) env)
                 (env (drover-claude-code--default-effort entry catalog))
                 ((drover-claude-code--flag "effort"))
                 ((seq-some (lambda (layer) (drover-claude-code--layer-effort layer key base catalog))
                            layers))
                 (entry (drover-claude-code--default-effort entry catalog)))))
    (when (stringp level)
      (drover-claude-code--clamp-effort
       (intern level)
       supported
       (seq-some (lambda (layer) (alist-get 'maxEffortLevel (cdr layer))) layers)))))

(defun drover-claude-code-model (id layers catalog)
  "Return the `drover-model' of Claude Code model ID.
CATALOG names it and offers its effort levels as variants (see
`drover-claude-code--effort-levels'); its default variant is the effort
LAYERS and CATALOG give it.  A model CATALOG does not list has unknown
variants.  Its engines are `claude-code', then the others of
`drover-model-engines-for' for its identifier: Claude Code runs
whatever model it names, an alias too."
  (let* ((base (drover-claude-code--base-id id))
         (entry (drover-claude-code--catalog-model catalog base)))
    (drover-model-create
     :provider-name "Anthropic"
     :id id
     :name (when-let* ((name (alist-get 'name entry)))
             (concat name (substring id (length base))))
     :variants (mapcar #'list (drover-claude-code--effort-levels entry))
     :default-variant (drover-claude-code-effort id layers catalog)
     :variants-known (and entry t)
     :engines (delete-dups (cons 'claude-code (drover-model-engines-for nil id))))))

(defun drover-claude-code-catalog-models (catalog &optional layers)
  "Return the `drover-model' of every model CATALOG lists, in its order.
CATALOG is what `drover-claude-code-catalog' returns: the CLI's own
list, main section and overflow alike, with each model's effort
levels; LAYERS, the settings layers giving default efforts, as for
`drover-claude-code-model'."
  (delq nil (mapcar (lambda (entry)
                      (when-let* ((id (and (drover-json-object-p entry) (alist-get 'id entry)))
                                  ((stringp id)))
                        (drover-claude-code-model id layers catalog)))
                    (map-nested-elt catalog '(config models)))))

(defun drover-claude-code-acp-models (options current levels backend &optional catalog)
  "Return the models an ACP session of Claude Code lists, each known once.
OPTIONS are the values of the session's model option, in order, as
\(VALUE . NAME): Claude Code's ACP agent lists aliases, such as
\"opus\", \"opus[1m]\" and \"default\", and full identifiers when
settings restrict the models.  Each VALUE is resolved through CATALOG,
by default the one the CLI caches, to the identifier of its model, as
the CLI resolves it (see `drover-claude-code-model'), so that a model
is the same whichever source names it; VALUE becomes the name BACKEND
takes for it (see `drover-model-aliases').  The model the session runs,
CURRENT, a VALUE, has the variants LEVELS, the effort levels the session
offers, a list of symbols; the others those CATALOG lists, unknown when
it does not list them.

\"default\" stands for whichever model is the default: a model it
names another value names too is known by that value, which keeps
naming it once the default changes, and \"default\" not resolved is
left out, as drover's pickers offer the default already."
  (let* ((catalog (or catalog (drover-claude-code-catalog)))
         (default-p (lambda (value) (equal (downcase (drover-claude-code--base-id value)) "default")))
         (entries (delq nil (mapcar (pcase-lambda (`(,value . ,name))
                                      (let ((id (drover-claude-code--resolve-model value catalog)))
                                        (unless (and (funcall default-p value) (equal id value))
                                          (list value id name))))
                                    options)))
         (named (mapcar #'cadr (seq-remove (lambda (entry) (funcall default-p (car entry))) entries)))
         (entries (seq-remove (lambda (entry)
                                (and (funcall default-p (car entry)) (member (cadr entry) named)))
                              entries))
         (current-id (and current (drover-claude-code--resolve-model current catalog)))
         (models (mapcar (pcase-lambda (`(,value ,id ,name))
                           (let ((model (drover-claude-code-model id nil catalog)))
                             (unless (drover-model-name model)
                               (setf (drover-model-name model) name))
                             (unless (equal value id)
                               (setf (drover-model-aliases model)
                                     (list (cons (cons 'claude-code backend) value))))
                             (when (equal id current-id)
                               (setf (drover-model-variants model) (mapcar #'list levels)
                                     (drover-model-variants-known model) t))
                             model))
                         entries)))
    (drover-model-merge-all models)))

;;;; Defaults

(defun drover-claude-code--project-directory (dir)
  "Return the directory a new Claude Code session for DIR starts in.
See `drover-claude-code-project-directory-function'."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (or (and drover-claude-code-project-directory-function
             (funcall drover-claude-code-project-directory-function dir))
        dir)))

(defun drover-claude-code-defaults (dir)
  "Return what a new Claude Code session in DIR starts with, as a plist.
:model is its `drover-model', nil when nothing names one; :agent the
agent of `drover-claude-code-agent-modes' its starting permission mode
stands for, nil when none; :layers and :catalog let
`drover-claude-code-effort' answer for another model.  This is the
engine's :defaults data."
  (let* ((layers (drover-claude-code-settings-layers (drover-claude-code--project-directory dir)))
         (catalog (drover-claude-code-catalog))
         (id (drover-claude-code-startup-model-id layers catalog))
         (mode (drover-claude-code-startup-mode layers)))
    (list :model (and id (drover-claude-code-model id layers catalog))
          :agent (car (rassoc mode drover-claude-code-agent-modes))
          :layers layers
          :catalog catalog)))

(defun drover-claude-code--fetch-defaults (directory callback)
  "Call CALLBACK with the defaults of a new Claude Code session in DIRECTORY.
They are read at once; a failure is reported, and CALLBACK not called."
  (when-let* ((defaults (condition-case err
                            (drover-claude-code-defaults directory)
                          (error
                           (drover-warn "Could not read Claude Code's defaults for %s: %s"
                                        (if (stringp directory) (abbreviate-file-name directory) directory)
                                        (drover-error-message err))
                           nil))))
    (funcall callback defaults)))

(defun drover-claude-code--resolve (defaults agent model variant)
  "Return the options a new Claude Code session runs, from DEFAULTS.
AGENT, MODEL and VARIANT are the explicit choices, each nil when not
made; DEFAULTS is a plist of `drover-claude-code-defaults', or nil.
The model is the startup one unless chosen, the agent the one standing
for the starting permission mode, and the variant the effort Claude
Code gives the model."
  (let ((model (or model (plist-get defaults :model))))
    (list :agent (or agent (plist-get defaults :agent))
          :model model
          :variant (or variant
                       (and model
                            (drover-claude-code-effort (drover-model-id model)
                                                       (plist-get defaults :layers)
                                                       (plist-get defaults :catalog)))))))

(defun drover-claude-code--agents (_directory)
  "Return the agents Claude Code runs as permission modes."
  (mapcar #'car drover-claude-code-agent-modes))

(defun drover-claude-code--last-used ()
  "Return when Claude Code last took a prompt, as a float time, or nil.
Interactive sessions append each prompt to history.jsonl; sessions run
through the SDK, as agent-shell's are, write only their transcripts,
so the transcripts of the sessions front ends follow count too, not
those of every conversation recorded, which only grow when run."
  (let ((times (list (drover-file-mtime (expand-file-name "history.jsonl" drover-claude-code-home)))))
    (dolist (session (drover-sessions))
      (when-let* (((eq (drover-session-engine session) 'claude-code))
                  ((not (drover-backend-property session :recorded)))
                  (id (drover-session-engine-id session))
                  (directory (drover-session-directory session)))
        (push (drover-file-mtime (drover-claude-code-transcript id directory)) times)))
    (when-let* ((times (delq nil times)))
      (apply #'max times))))

;;;; Registration

(defun drover-claude-code--register ()
  "Register the `claude-code' engine."
  (drover-register-engine 'claude-code
                          :label "Claude Code"
                          :defaults #'drover-claude-code--fetch-defaults
                          :volatile t
                          :resolve #'drover-claude-code--resolve
                          :agents #'drover-claude-code--agents
                          :last-used #'drover-claude-code--last-used
                          :directory #'drover-claude-code--directory))

(drover-claude-code--register)

(defun drover-claude-code-unload-function ()
  "Forget the `claude-code' engine.
Its model sources, such as agent-shell's, are their files' to remove."
  (drover-unregister-engine 'claude-code)
  nil)

(provide 'drover-claude-code)
;;; drover-claude-code.el ends here
