;;; drover-conversation.el --- A conversation as turns, from what engines record  -*- lexical-binding: t; -*-

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

;; What a session said, as entries a phone draws at its own width: the
;; user's prompts, the agent's replies and thinking, its tools and what
;; they answered, the patches it applied, its errors.  They are read
;; from the files engines record, Claude Code's transcripts and Codex's
;; rollouts, a line a record: pages from the end, backward, in slices
;; of a few milliseconds from timers, and lines appended since followed
;; by one shared timer looking at the files.
;;
;; An entry is a plist JSON carries: :i, its identifier, the byte offset
;; of its line and, for the second entry of a line and after, a dot and
;; its rank; :y, its kind, `user', `text', `think', `tool', `res',
;; `patch', `err' or `meta'; :t, its time; then what its kind holds.
;; Every text is bounded where it is read, and a line too long to parse
;; is never parsed: its start tells what it was.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)
(require 'drover-transcript)

(declare-function drover-claude-code-session-files "drover-claude-code" (id &optional dir))
(declare-function drover-claude-code--prompt-p "drover-claude-code" (record))
(declare-function drover-claude-code--texts "drover-claude-code" (content))
(declare-function drover-claude-code--edit "drover-claude-code" (record))
(declare-function drover-codex-rollout-file "drover-codex" (id))
(declare-function drover-codex-rollouts--file "drover-codex-rollouts" (id))
(declare-function drover-codex-message-text "drover-codex" (payload))
(declare-function drover-codex--file-changes "drover-codex" (changes))
(declare-function drover-codex--record-edits "drover-codex" (record))

(defvar drover-conversation-window 65536
  "Bytes read at once, backward or forward.")

(defvar drover-conversation-slice 0.008
  "Seconds a slice of reading takes at most.")

(defconst drover-conversation-parse-limit 262144
  "Bytes at most of a line parsed: a longer one is told by its start alone.")

(defconst drover-conversation-scan-limit (* 8 1024 1024)
  "Bytes at most a page reads back.")

(defconst drover-conversation-head 8192
  "Bytes of the start of a line too long to parse read to tell what it was.")

(defconst drover-conversation-page-size 60
  "Entries of a page by default.")

(defconst drover-conversation-page-limit 200
  "Entries at most of a page.")

(defconst drover-conversation-reply-bytes (* 512 1024)
  "Bytes at most of a page's entries, encoded.")

(defvar drover-conversation-follow-interval 1.0
  "Seconds between two looks at the files followed.")

(defconst drover-conversation-bounds
  '((user . 32000) (text . 32000) (think . 4000) (tool . 4000) (res . 8000) (patch . 16000) (err . 4000)
    (meta . 400))
  "Characters at most of the text of each kind of entry.")

;;;; Engines

(defvar drover-conversation-readers
  '((claude-code :transcript drover-conversation--claude-file :entries drover-conversation--claude-entries)
    (codex :transcript drover-conversation--codex-file :entries drover-conversation--codex-entries))
  "How each engine's conversations are read, by engine.
Each is a plist: :transcript, a function of an engine's identifier of a
conversation and the DIRECTORY it ran in returning the file it records
it in, or nil; :entries, a function of a parsed RECORD of it, an alist,
and the session's DIRECTORY, returning its entries, plists without :i.
An engine registered with :transcript and :entries of its own takes
those (see `drover-register-engine').")

(defun drover-conversation--reader (engine property)
  "Return ENGINE's function PROPERTY, :transcript or :entries, or nil."
  (or (and (fboundp 'drover-engine-property)
           (ignore-errors (drover-engine-property engine property)))
      (plist-get (alist-get engine drover-conversation-readers) property)))

(defun drover-conversation-file (session)
  "Return the file SESSION's conversation is recorded in, or nil.
For an engine of `drover-conversation-readers' or one registering its
own reader; agent-shell's sessions are read through their engine."
  (when-let* ((engine (drover-session-engine session))
              (id (drover-session-engine-id session))
              ((stringp id))
              (function (drover-conversation--reader engine :transcript)))
    (let ((file (ignore-errors (funcall function id (drover-session-directory session)))))
      (and (stringp file) (file-regular-p file) file))))

(defun drover-conversation-readable-p (session)
  "Return non-nil when SESSION's conversation reads as entries here."
  (and (drover-session-engine session)
       (drover-conversation--reader (drover-session-engine session) :entries)
       t))

;;;; Entries

(defun drover-conversation--bound (kind text)
  "Return TEXT bounded for an entry of KIND, and its full length when cut.
A value (TEXT . LENGTH), LENGTH nil when whole; a result keeps its head
and its tail."
  (let ((limit (or (alist-get kind drover-conversation-bounds) 4000)))
    (cond
     ((not (stringp text)) (cons "" nil))
     ((<= (length text) limit) (cons text nil))
     ((eq kind 'res)
      (let ((half (/ limit 2)))
        (cons (concat (substring text 0 half) "\n…\n" (substring text (- (length text) half)))
              (length text))))
     (t (cons (concat (substring text 0 limit) "…") (length text))))))

(defun drover-conversation--entry (kind text &rest more)
  "Return an entry of KIND with TEXT, bounded, and MORE, a plist."
  (let ((bounded (drover-conversation--bound kind text)))
    (append (list :y (symbol-name kind) :x (car bounded))
            (and (cdr bounded) (list :cut (cdr bounded)))
            more)))

(defun drover-conversation--relative (file directory)
  "Return FILE relative to DIRECTORY when under it, else FILE."
  (if (and (stringp file) (stringp directory) (file-name-absolute-p file)
           (string-prefix-p (file-name-as-directory (expand-file-name directory)) (expand-file-name file)))
      (file-relative-name file directory)
    file))

(defun drover-conversation--json (value)
  "Return VALUE, parsed JSON, as compact JSON text, bounded."
  (car (drover-conversation--bound
        'tool (condition-case nil
                  (drover-json-text value :null-object nil :false-object :false)
                (error (format "%S" value))))))

(defun drover-conversation--patch (file status patch directory)
  "Return the entry of a PATCH of FILE, in a session of DIRECTORY.
STATUS is `added', `deleted', `modified', or (renamed . FROM)."
  (let ((lines (and (stringp patch) (split-string patch "\n")))
        (additions 0) (deletions 0))
    (dolist (line lines)
      (cond ((string-prefix-p "@@" line))
            ((string-prefix-p "+" line) (cl-incf additions))
            ((string-prefix-p "-" line) (cl-incf deletions))))
    (let* ((kept (if (> (length lines) 400) (seq-take lines 400) lines))
           (hunks (car (drover-conversation--bound 'patch (string-join kept "\n")))))
      (append (list :y "patch" :f (or (drover-conversation--relative file directory) "")
                    :st (if (consp status) "renamed" (symbol-name status))
                    :a additions :d deletions :h hunks)
              (and (consp status) (list :fr (drover-conversation--relative (cdr status) directory)))
              (and (> (length lines) 400) (list :cut (length lines)))))))

(defun drover-conversation--tool-summary (name input directory)
  "Return a line telling what tool NAME does with INPUT, an alist.
Files are named relative to DIRECTORY, the session's, when under it."
  (let ((get (lambda (key) (let ((value (alist-get key input))) (and (stringp value) value)))))
    (or (drover-bound-line
         (pcase name
           ((or "Bash" "shell" "exec_command" "local_shell") (or (funcall get 'command) (funcall get 'cmd)))
           ((or "Read" "Write" "Edit" "MultiEdit" "NotebookEdit")
            (drover-conversation--relative (or (funcall get 'file_path) (funcall get 'notebook_path)) directory))
           ((or "Grep" "Glob") (funcall get 'pattern))
           ("WebFetch" (funcall get 'url))
           ("WebSearch" (funcall get 'query))
           ((or "Task" "Agent") (funcall get 'description))
           (_ (seq-some (lambda (pair) (and (stringp (cdr pair)) (cdr pair))) (and (listp input) input))))
         200)
        "")))

;;;;; Claude Code

(defun drover-conversation--claude-file (id directory)
  "Return the transcript of Claude Code conversation ID, of DIRECTORY, or nil."
  (require 'drover-claude-code)
  (car (ignore-errors (drover-claude-code-session-files id directory))))

(defun drover-conversation--claude-entries (record directory)
  "Return the entries of Claude Code transcript RECORD, of a session of DIRECTORY."
  (let ((time (drover-transcript-time (alist-get 'timestamp record)))
        (type (alist-get 'type record))
        (message (alist-get 'message record)))
    (unless (alist-get 'isSidechain record)
      (mapcar
       (lambda (entry) (append entry (list :t (or time :false))))
       (pcase type
         ("user"
          (let ((content (alist-get 'content message)))
            (cond
             ((or (alist-get 'toolUseResult record)
                  (and (listp content)
                       (seq-some (lambda (block) (and (consp block) (equal (alist-get 'type block) "tool_result")))
                                 content)))
              (append
               (delq nil
                     (mapcar (lambda (block)
                               (when (and (consp block) (equal (alist-get 'type block) "tool_result"))
                                 (let ((text (let ((value (alist-get 'content block)))
                                               (if (stringp value) value (drover-claude-code--texts value)))))
                                   (drover-conversation--entry 'res text :c (or (alist-get 'tool_use_id block) "")
                                                               :er (if (alist-get 'is_error block) t :false)))))
                             (and (listp content) content)))
               (when-let* ((edit (drover-claude-code--edit record)))
                 (list (drover-conversation--patch (nth 1 edit) (nth 2 edit) (nth 3 edit) directory)))))
             ((drover-claude-code--prompt-p record)
              (list (drover-conversation--entry 'user (drover-claude-code--prompt-p record))))
             (t
              (let ((text (and (not (alist-get 'isMeta record)) (drover-claude-code--texts content))))
                (cond
                 ((null text) nil)
                 ((string-prefix-p "[Request interrupted" text)
                  (list (drover-conversation--entry 'meta "interrupted")))
                 ((string-match "<command-name>\\([^<]*\\)</command-name>" text)
                  (list (drover-conversation--entry 'meta (string-trim (match-string 1 text)))))))))))
         ("assistant"
          (if (alist-get 'isApiErrorMessage record)
              (list (drover-conversation--entry 'err (drover-claude-code--texts (alist-get 'content message))))
            (delq nil
                  (mapcar (lambda (block)
                            (when (consp block)
                              (pcase (alist-get 'type block)
                                ("text" (let ((text (alist-get 'text block)))
                                          (and (stringp text) (not (string-blank-p text))
                                               (drover-conversation--entry 'text (string-trim text)))))
                                ("thinking" (let ((text (alist-get 'thinking block)))
                                              (and (stringp text) (not (string-blank-p text))
                                                   (drover-conversation--entry 'think (string-trim text)))))
                                ("tool_use"
                                 (let ((name (or (alist-get 'name block) "tool"))
                                       (input (alist-get 'input block)))
                                   (list :y "tool" :c (or (alist-get 'id block) "") :n (drover-bound-line name 80)
                                         :s (drover-conversation--tool-summary name input directory)
                                         :in (drover-conversation--json input)))))))
                          (let ((content (alist-get 'content message)))
                            (if (listp content) content (list (list (cons 'type "text") (cons 'text content)))))))))
         ("system"
          (when (equal (alist-get 'subtype record) "compact_boundary")
            (list (drover-conversation--entry 'meta "compacted")))))))))

;;;;; Codex

(defun drover-conversation--codex-file (id _directory)
  "Return the rollout of Codex conversation ID, or nil."
  (require 'drover-codex)
  (or (and (fboundp 'drover-codex-rollouts--file) (ignore-errors (drover-codex-rollouts--file id)))
      (drover-codex-rollout-file id)))

(defun drover-conversation--codex-entries (record directory)
  "Return the entries of Codex rollout RECORD, of a session of DIRECTORY.
Codex records a message twice, as an event and as an item of the
conversation: the events tell prompts and replies, the items tools and
reasoning, the events of patches applied what changed."
  (let ((time (drover-transcript-time (alist-get 'timestamp record)))
        (payload (alist-get 'payload record)))
    (when (consp payload)
      (mapcar
       (lambda (entry) (append entry (list :t (or time :false))))
       (pcase (list (alist-get 'type record) (alist-get 'type payload))
         (`("event_msg" "user_message")
          (let ((message (alist-get 'message payload)))
            (and (stringp message) (not (string-blank-p message))
                 (not (string-match-p "\\`[ \t\n]*\\(?:<[a-z_]+\\|# AGENTS\\.md instructions\\)" message))
                 (list (drover-conversation--entry 'user (string-trim message))))))
         (`("event_msg" "agent_message")
          (let ((message (alist-get 'message payload)))
            (and (stringp message) (not (string-blank-p message))
                 (list (drover-conversation--entry 'text (string-trim message))))))
         (`("response_item" "reasoning")
          (when-let* ((texts (delq nil (mapcar (lambda (part) (and (consp part) (alist-get 'text part)))
                                               (let ((summary (alist-get 'summary payload)))
                                                 (and (listp summary) summary)))))
                      (text (string-trim (string-join (seq-filter #'stringp texts) "\n")))
                      ((not (string-empty-p text))))
            (list (drover-conversation--entry 'think text))))
         (`("response_item" ,(and kind (or "function_call" "custom_tool_call")))
          (let* ((name (or (alist-get 'name payload) "tool"))
                 (raw (if (equal kind "function_call") (alist-get 'arguments payload) (alist-get 'input payload)))
                 (input (or (and (stringp raw)
                                 (let ((parsed (ignore-errors (json-parse-string raw :object-type 'alist
                                                                                 :array-type 'list
                                                                                 :null-object nil
                                                                                 :false-object nil))))
                                   (and (consp parsed) (consp (car parsed)) parsed)))
                            raw))
                 (command (and (consp input) (alist-get 'command input))))
            (list (list :y "tool" :c (or (alist-get 'call_id payload) "") :n (drover-bound-line name 80)
                        :s (or (drover-bound-line (cond ((stringp command) command)
                                                        ((and (listp command) (seq-every-p #'stringp command))
                                                         (string-join command " "))
                                                        ((equal name "apply_patch") "apply_patch")
                                                        ((consp input) (drover-conversation--tool-summary name input directory))
                                                        ((stringp input) input))
                                                  200)
                               "")
                        :in (car (drover-conversation--bound 'tool (if (stringp raw) raw
                                                                     (drover-conversation--json raw))))))))
         (`("response_item" ,(or "function_call_output" "custom_tool_call_output"))
          (let* ((output (alist-get 'output payload))
                 (text (cond ((stringp output)
                              (let ((parsed (ignore-errors (json-parse-string output :object-type 'alist))))
                                (if (and (consp parsed) (stringp (alist-get 'output parsed)))
                                    (alist-get 'output parsed)
                                  output)))
                             ((consp output) (or (and (stringp (alist-get 'output output)) (alist-get 'output output))
                                                 (drover-conversation--json output))))))
            (list (drover-conversation--entry 'res text :c (or (alist-get 'call_id payload) "") :er :false))))
         (`("event_msg" "patch_apply_end")
          (when (alist-get 'success payload)
            (mapcar (lambda (edit) (drover-conversation--patch (nth 0 edit) (nth 1 edit) (nth 2 edit) directory))
                    (drover-codex--file-changes (alist-get 'changes payload)))))
         (`("event_msg" ,(or "error" "stream_error"))
          (list (drover-conversation--entry 'err (or (alist-get 'message payload) "error"))))
         (`("event_msg" "turn_aborted")
          (list (drover-conversation--entry 'meta "interrupted")))
         (`(,_ ,(or "compacted" "context_compacted"))
          (list (drover-conversation--entry 'meta "compacted"))))))))

;;;; Lines

(defun drover-conversation--parse (bytes)
  "Return the record BYTES, a line, holds, an alist, or nil."
  (let ((record (ignore-errors
                  (json-parse-string (decode-coding-string bytes 'utf-8-unix t)
                                     :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil))))
    (and (consp record) (consp (car record)) record)))

(defun drover-conversation--long (file offset)
  "Return what the line of FILE at OFFSET, too long to parse, was, a record or nil.
Its start is read, and the kind of record it is and the start of its
text found without parsing it whole."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file nil offset (+ offset drover-conversation-head)))
    (let* ((type (progn (goto-char (point-min)) (drover-transcript-sniff "type")))
           (text (progn (goto-char (point-min))
                        (when (re-search-forward "\"\\(?:text\\|message\\|content\\|output\\)\":[ \t]*\"" nil t)
                          (goto-char (1- (match-end 0)))
                          (drover-transcript-string-prefix 2000)))))
      (list :y "meta" :x (format "a record too long to show (%s)%s" (or type "unknown")
                                 (if text (concat ": " (drover-bound-line text 300)) ""))))))

(defun drover-conversation--line-entries (function bytes file offset directory)
  "Return the entries of the line BYTES of FILE at OFFSET, through FUNCTION.
DIRECTORY is the session's.  A line longer than
`drover-conversation-parse-limit', or BYTES nil for one, is not parsed."
  (let ((entries (if (or (null bytes) (> (length bytes) drover-conversation-parse-limit))
                     (list (drover-conversation--long file offset))
                   (when-let* ((record (drover-conversation--parse bytes)))
                     (condition-case nil
                         (funcall function record directory)
                       (error nil))))))
    (let ((rank 0))
      (mapcar (lambda (entry)
                (prog1 (append (list :i (if (zerop rank) (number-to-string offset) (format "%d.%d" offset rank)))
                               entry)
                  (cl-incf rank)))
              (delq nil entries)))))

(defun drover-conversation-generation (file)
  "Return FILE's generation, which changes when it is replaced: its inode."
  (let ((attributes (file-attributes file)))
    (and attributes (format "%s" (file-attribute-inode-number attributes)))))

;;;; Paging, backward

(defun drover-conversation-page (session before count callback)
  "Read COUNT entries of SESSION's conversation before byte BEFORE.
BEFORE is the start of a line a page began with, or nil for the newest.
CALLBACK gets, once, a plist: :g the file's generation, :sz its size,
:from the offset of the first line read, :more non-nil when lines
precede it, :to where the lines read end, the next one to follow
beginning there, :e the entries, oldest first; or (:failure MESSAGE).  The
file is read backward a window at a time, in slices from timers.
Return a function stopping the reading."
  (let* ((engine (drover-session-engine session))
         (function (and engine (drover-conversation--reader engine :entries)))
         (file (and function (drover-conversation-file session)))
         (stopped nil))
    (if (not file)
        (progn (funcall callback (list :failure (if function "its conversation is not recorded here"
                                                  "its engine records nothing drover reads")))
               #'ignore)
      (let* ((size (file-attribute-size (file-attributes file)))
             (generation (drover-conversation-generation file))
             (end (if before (min size (max 0 before)) (drover-conversation--whole-end file size)))
             (state (list :file file :function function :directory (drover-session-directory session)
                          :wanted (max 1 (min drover-conversation-page-limit
                                              (or count drover-conversation-page-size)))
                          ;; The line being read ends at the newline before END;
                          ;; BUFFER holds the bytes from START to it.
                          :start (max 0 (1- end)) :buffer "" :long nil
                          :entries nil :count 0 :scanned 0 :first end :done (<= end 0))))
        (cl-labels
            ((finish ()
               (unless stopped
                 (setq stopped t)
                 (let ((entries (plist-get state :entries))
                       (from (plist-get state :first)))
                   ;; Within the bound of a reply, the oldest dropped.
                   (while (and (cdr entries)
                               (> (length (json-serialize (vconcat entries) :null-object nil :false-object :false))
                                  drover-conversation-reply-bytes))
                     (setq entries (cdr entries)
                           from (string-to-number (plist-get (car entries) :i))))
                   (drover-with-reporting "conversation"
                     (funcall callback (list :g (or generation :false) :sz size :from from :more (> from 0)
                                             :to end :e (vconcat entries)))))))
             (step ()
               (unless stopped
                 (condition-case err
                     (let ((until (+ (float-time) drover-conversation-slice)))
                       (while (and (not (plist-get state :done)) (< (float-time) until))
                         (drover-conversation--back state))
                       (if (plist-get state :done)
                           (finish)
                         (run-at-time 0 nil (lambda () (drover-with-reporting "conversation" (step))))))
                   (error (setq stopped t)
                          (drover-with-reporting "conversation"
                            (funcall callback (list :failure (drover-error-message err)))))))))
          (step)
          (lambda () (setq stopped t)))))))

(defun drover-conversation--whole-end (file size)
  "Return where the whole lines of FILE end, FILE of SIZE bytes.
That is past its last newline: a last line without one is still
being written."
  (if (<= size 0)
      0
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((from (max 0 (- size drover-conversation-window))))
        (insert-file-contents-literally file nil from size)
        (goto-char (point-max))
        (cond ((eq (char-before) ?\n) size)
              ((search-backward "\n" nil t) (+ from (- (point) (point-min)) 1))
              ;; No newline in the last window: a line too long, cut there.
              ((> from 0) from)
              (t 0))))))

(defun drover-conversation--emit (state offset bytes)
  "Note the line of STATE's file at OFFSET, BYTES, or nil for one too long."
  (let ((entries (drover-conversation--line-entries (plist-get state :function) bytes (plist-get state :file)
                                                    offset (plist-get state :directory))))
    (plist-put state :entries (append entries (plist-get state :entries)))
    (plist-put state :count (+ (plist-get state :count) (length entries)))
    (plist-put state :first offset)
    (when (or (<= offset 0) (>= (plist-get state :count) (plist-get state :wanted)))
      (plist-put state :done t))))

(defun drover-conversation--back (state)
  "Read one window more of STATE's file, backward, noting the lines it completes.
STATE is done once enough entries came, the file's start was reached,
or `drover-conversation-scan-limit' bytes were read."
  (if (> (plist-get state :scanned) drover-conversation-scan-limit)
      (plist-put state :done t)
    (let* ((start (plist-get state :start))
           (from (max 0 (- start drover-conversation-window)))
           (chunk (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally (plist-get state :file) nil from start)
                    (buffer-string)))
           (bytes (if (plist-get state :long) chunk (concat chunk (plist-get state :buffer))))
           (position (length bytes)))
      (plist-put state :scanned (+ (plist-get state :scanned) (- start from)))
      (plist-put state :start from)
      (catch 'more
        (while (not (plist-get state :done))
          (let ((newline (cl-position ?\n bytes :end position :from-end t)))
            (cond
             ((or newline (= from 0))
              (let ((line-start (if newline (1+ newline) 0)))
                (drover-conversation--emit state (+ from line-start)
                                           (and (not (plist-get state :long))
                                                (substring bytes line-start position)))
                (plist-put state :long nil)
                (setq position (or newline 0))
                (unless newline (plist-put state :done t))))
             ((or (plist-get state :long) (> position drover-conversation-parse-limit))
              ;; Its start is further back: the bytes are let go.
              (plist-put state :long t)
              (plist-put state :buffer "")
              (throw 'more nil))
             (t
              (plist-put state :buffer (substring bytes 0 position))
              (throw 'more nil))))))
      (when (and (plist-get state :done) (= from 0) (not (plist-get state :long)))
        (plist-put state :buffer "")))))

;;;; Following, forward

(defvar drover-conversation--follows nil
  "The conversations followed, as plists.
Each holds :file, :offset, :generation, :pending, :callback, :reading.")

(defvar drover-conversation--timer nil
  "The timer looking at the files followed.")

(defun drover-conversation-follow (session from generation callback)
  "Follow SESSION's conversation from byte FROM, telling CALLBACK what is appended.
GENERATION is the file's, as a page told it.  CALLBACK gets (:entries
ENTRIES :to OFFSET) as whole lines are appended, the offset where the
next line begins; or (:reset WHY), once, when the file was replaced or
shrank, or is gone, after which it is no longer followed.  Return a
function stopping it."
  (let* ((engine (drover-session-engine session))
         (function (and engine (drover-conversation--reader engine :entries)))
         (file (and function (drover-conversation-file session)))
         (follow (list :file file :offset from :generation generation :pending nil
                       :callback callback :function function
                       :directory (drover-session-directory session))))
    (cond
     ((not file) (funcall callback (list :reset "its conversation is not recorded here")) #'ignore)
     ((not (equal generation (drover-conversation-generation file)))
      (funcall callback (list :reset "its file was replaced")) #'ignore)
     (t
      (push follow drover-conversation--follows)
      (unless drover-conversation--timer
        (setq drover-conversation--timer
              (run-at-time drover-conversation-follow-interval drover-conversation-follow-interval
                           #'drover-conversation--look)))
      (lambda () (drover-conversation--unfollow follow))))))

(defun drover-conversation--unfollow (follow)
  "Stop following FOLLOW."
  (setq drover-conversation--follows (delq follow drover-conversation--follows))
  (plist-put follow :stopped t)
  (when (and (null drover-conversation--follows) drover-conversation--timer)
    (cancel-timer drover-conversation--timer)
    (setq drover-conversation--timer nil)))

(defun drover-conversation-stop-all ()
  "Stop following every conversation."
  (dolist (follow drover-conversation--follows)
    (plist-put follow :stopped t))
  (setq drover-conversation--follows nil)
  (when drover-conversation--timer
    (cancel-timer drover-conversation--timer)
    (setq drover-conversation--timer nil)))

(defun drover-conversation--look ()
  "Look at the files followed, reading what was appended to them."
  (drover-with-reporting "conversation"
    (dolist (follow (copy-sequence drover-conversation--follows))
      (unless (plist-get follow :reading)
        (let* ((file (plist-get follow :file))
               (attributes (file-attributes file))
               (size (and attributes (file-attribute-size attributes))))
          (cond
           ((or (not attributes)
                (not (equal (drover-conversation-generation file) (plist-get follow :generation)))
                (< size (plist-get follow :offset)))
            (drover-conversation--unfollow follow)
            (funcall (plist-get follow :callback)
                     (list :reset (if attributes "its file was replaced or shrank" "its file is gone"))))
           ((> size (+ (plist-get follow :offset) (length (or (plist-get follow :pending) ""))))
            (drover-conversation--forward follow size))))))))

(defun drover-conversation--forward (follow size)
  "Read FOLLOW's file forward up to SIZE, in slices, giving the whole lines."
  (plist-put follow :reading t)
  (cl-labels
      ((step ()
         (drover-with-reporting "conversation"
           (if (plist-get follow :stopped)
               (plist-put follow :reading nil)
             (let ((until (+ (float-time) drover-conversation-slice))
                   (entries nil))
               (while (and (< (float-time) until)
                           (< (+ (plist-get follow :offset) (length (or (plist-get follow :pending) ""))) size))
                 (let* ((pending (or (plist-get follow :pending) ""))
                        (read-from (+ (plist-get follow :offset) (length pending)))
                        (chunk (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally (plist-get follow :file) nil read-from
                                                                 (min size (+ read-from drover-conversation-window)))
                                 (buffer-string)))
                        (bytes (concat pending chunk))
                        (start 0))
                   (when (zerop (length chunk))
                     (setq size read-from))
                   (while (let ((newline (cl-position ?\n bytes :start start)))
                            (when newline
                              (let ((offset (+ (plist-get follow :offset) start)))
                                (setq entries (append entries
                                                      (drover-conversation--line-entries
                                                       (plist-get follow :function) (substring bytes start newline)
                                                       (plist-get follow :file) offset (plist-get follow :directory))))
                                (setq start (1+ newline))))))
                   (plist-put follow :offset (+ (plist-get follow :offset) start))
                   (let ((rest (substring bytes start)))
                     ;; A line longer than a megabyte is told by its start once whole.
                     (plist-put follow :pending (if (> (length rest) (* 1024 1024))
                                                    (substring rest 0 drover-conversation-parse-limit)
                                                  rest)))))
               (when entries
                 (funcall (plist-get follow :callback) (list :entries entries :to (plist-get follow :offset))))
               (if (< (+ (plist-get follow :offset) (length (or (plist-get follow :pending) ""))) size)
                   (run-at-time 0 nil #'step)
                 (plist-put follow :reading nil)))))))
    (step)))

(provide 'drover-conversation)
;;; drover-conversation.el ends here
