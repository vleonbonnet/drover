;;; drover-prompts.el --- The prompts the user sent, from what engines record  -*- lexical-binding: t; -*-

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

;; The prompts the user sent to agents over a period, of every engine
;; and host, for a review of how the user works with them: what the
;; user asks again and again, and corrects.  Each host reads its own
;; engines, as for usage, and sends its prompts to its peers as they
;; ask (`drover-remote'):
;;
;; - OpenCode keeps each message of the user in its service's database
;;   (`drover-opencode-database'), read only, by time of creation;
;; - Claude Code's transcripts and Codex's rollouts are read backward
;;   from their ends, as `drover-conversation' reads them, until a
;;   record older than the period.
;;
;; Only the user's prompts count: not those of subagents, nor what the
;; engines add as the user, nor the conversations a program ran
;; (\"claude -p\", \"codex exec\").  The configuration leaves out more
;; (`drover-prompts-ignore-regexps', `drover-prompts-ignore-directories'),
;; such as the prompts its own automation sends.  Every text is bounded
;; as it is read.  `drover-prompts-gather' asks every host; agents ask
;; through `drover-anvil''s tool drover-prompts.  This file loads with
;; drover's core alone, as a host answering does.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-transcript)
(require 'drover-conversation)
(require 'drover-claude-code)
(require 'drover-codex)

(defvar drover-opencode-database)
(declare-function drover-remote-prompts-hosts "drover-remote" ())
(declare-function drover-remote-ask-prompts "drover-remote" (name request callback seconds))

;;;; Options

(defcustom drover-prompts-engines '(opencode claude-code codex)
  "The engines whose prompts are read, among `opencode', `claude-code' and `codex'."
  :type '(set (const opencode) (const claude-code) (const codex))
  :group 'drover)

(defcustom drover-prompts-ignore-regexps nil
  "Regexps matching the prompts left out, such as those an automation sends.
A prompt matching one of them, its text trimmed, is not read, such as
\"\\\\`Automated notice: \"."
  :type '(repeat regexp)
  :group 'drover)

(defcustom drover-prompts-ignore-directories nil
  "Directories whose sessions' prompts are left out, those below them too.
As this host spells them, such as \"~/work/automation/\"."
  :type '(repeat directory)
  :group 'drover)

(defcustom drover-prompts-text-limit 4000
  "Characters at most of a prompt read; a longer one is cut, its length told."
  :type 'natnum
  :group 'drover)

(defcustom drover-prompts-limit 5000
  "Prompts at most a host reads for one period; past it, the oldest are left out."
  :type 'natnum
  :group 'drover)

(defcustom drover-prompts-days-limit 31
  "Days at most a period of prompts spans."
  :type 'natnum
  :group 'drover)

(defcustom drover-prompts-timeout 240
  "Seconds `drover-prompts-gather' waits for a host at most."
  :type 'natnum
  :group 'drover)

(defconst drover-prompts-opencode-page 500
  "Messages of OpenCode read in one query.")

(defconst drover-prompts-file-page 100
  "Prompts of a transcript or rollout read in one page.")

(defconst drover-prompts-remote-bytes (* 2 1024 1024)
  "Bytes at most of the prompts a host sends a peer, encoded.")

(defconst drover-prompts--injected-regexp
  (concat "\\`[ \t\n]*\\(?:<[a-z_-]+\\|# AGENTS\\.md instructions\\|Base directory for this skill"
          "\\|This session is being continued\\|Your response above was stopped\\)")
  "Regexp matching the start of a text engines add as the user's.
Context such as a <system-reminder>, a skill's body or the summary a
compaction left: no prompt of the user's.")

;;;; Prompts

;; A prompt is a plist: :engine, a symbol; :id, the engine's identifier
;; of its conversation; :time, a float time; :text, bounded, and :cut,
;; its whole length when cut; :directory and :title of its conversation,
;; when known; :host, the name of the host it is of, nil for this one.

(defun drover-prompts--bound (text)
  "Return TEXT, a prompt's, trimmed and bounded, as (TEXT . LENGTH), or nil.
LENGTH is its whole length when cut, else nil."
  (when (stringp text)
    (let ((text (string-trim (substring-no-properties text))))
      (unless (string-empty-p text)
        (if (length> text drover-prompts-text-limit)
            (cons (concat (substring text 0 drover-prompts-text-limit) "…") (length text))
          (cons text nil))))))

(defvar drover-prompts--truenames (make-hash-table :test #'equal)
  "The true names of the directories looked at, by name as given.
Made anew as each reading starts, so that each directory is resolved
once a reading.")

(defun drover-prompts--names (directory)
  "Return the names of DIRECTORY, as given and as its true name, as directories.
An engine may record a directory by its true name, past a symbolic
link, or as it was given."
  (let* ((given (file-name-as-directory (expand-file-name directory)))
         (true (with-memoization (gethash given drover-prompts--truenames)
                 (file-name-as-directory (file-truename given)))))
    (if (equal given true) (list given) (list given true))))

(defun drover-prompts--under-p (directory roots)
  "Return non-nil when DIRECTORY is one of ROOTS or below one.
Names compare as this host compares them, ignoring case on Windows,
by the names as given and by their true names."
  (and (stringp directory)
       (let ((names (drover-prompts--names directory))
             (ignore-case (memq system-type '(windows-nt ms-dos cygwin))))
         (seq-some (lambda (root)
                     (and (stringp root)
                          (seq-some (lambda (prefix)
                                      (seq-some (lambda (name) (string-prefix-p prefix name ignore-case)) names))
                                    (drover-prompts--names root))))
                   roots))))

(defun drover-prompts--ignored-p (text directory)
  "Return non-nil when the prompt TEXT, of a session in DIRECTORY, is left out."
  (or (drover-prompts--under-p directory drover-prompts-ignore-directories)
      (seq-some (lambda (regexp) (and (stringp regexp) (string-match-p regexp text)))
                drover-prompts-ignore-regexps)))

(defun drover-prompts--make (engine id time text cut directory title)
  "Return the prompt TEXT of ENGINE's conversation ID at TIME, or nil.
Nil when the prompt is left out.  CUT is TEXT's whole length when cut;
DIRECTORY and TITLE are its conversation's, or nil."
  (unless (drover-prompts--ignored-p text directory)
    (list :engine engine :id id :time time :text text :cut cut
          :directory (and (stringp directory) (not (string-empty-p directory))
                          (drover-bound-line directory 1024))
          :title (drover-bound-line title 300))))

(defun drover-prompts--finish (prompts)
  "Return PROMPTS, without those read twice, oldest first, within the limit.
A value (PROMPTS . CUT), CUT how many of the oldest were left out.  A
conversation forked or resumed into another file copies its history: a
prompt of an engine at one time with one text is the same prompt."
  (let ((seen (make-hash-table :test #'equal))
        (kept nil))
    (dolist (prompt prompts)
      (let ((key (list (plist-get prompt :engine) (round (* 1000 (plist-get prompt :time)))
                       (plist-get prompt :text))))
        (unless (gethash key seen)
          (puthash key t seen)
          (push prompt kept))))
    (let* ((sorted (sort kept :key (lambda (prompt) (plist-get prompt :time)) :in-place t))
           (cut (max 0 (- (length sorted) drover-prompts-limit))))
      (cons (nthcdr cut sorted) cut))))

;;;; OpenCode

(defun drover-prompts--opencode-database ()
  "Return OpenCode's database, when it can be read here, else nil."
  (and (boundp 'drover-opencode-database)
       (stringp drover-opencode-database)
       (file-readable-p drover-opencode-database)
       drover-opencode-database))

(defun drover-prompts--opencode-query (db from until)
  "Return a page of the user's messages of DB after FROM, before UNTIL.
FROM is (MILLISECONDS . ID), UNTIL in milliseconds.  Each row is (ID
SESSION CREATED TEXT LENGTH DIRECTORY TITLE PARENT), TEXT cut past
`drover-prompts-text-limit'."
  (sqlite-select
   db
   (format (concat "SELECT m.id, m.session_id, m.time_created, "
                   "substr(json_extract(m.data, '$.text'), 1, %d), length(json_extract(m.data, '$.text')), "
                   "s.directory, s.title, s.parent_id "
                   "FROM session_message m LEFT JOIN session_v2 s ON s.id = m.session_id "
                   "WHERE m.type = 'user' AND (m.time_created > ?1 OR (m.time_created = ?1 AND m.id > ?2)) "
                   "AND m.time_created < ?3 ORDER BY m.time_created, m.id LIMIT %d")
           (1+ drover-prompts-text-limit) drover-prompts-opencode-page)
   (list (car from) (cdr from) until)))

(defun drover-prompts--opencode-row (row)
  "Return the prompt of ROW, a message of the user read, or nil.
Messages of subagents are left out."
  (pcase-let ((`(,_id ,session ,created ,text ,length ,directory ,title ,parent) row))
    (when-let* (((stringp session))
                ((numberp created))
                ((not (and (stringp parent) (not (string-empty-p parent)))))
                (bounded (drover-prompts--bound text)))
      (drover-prompts--make 'opencode session (/ created 1000.0) (car bounded)
                            (and (or (cdr bounded) (and (numberp length) (> length drover-prompts-text-limit)))
                                 (if (numberp length) length (cdr bounded)))
                            directory title))))

(defun drover-prompts--opencode (since until add done)
  "Read OpenCode's prompts between SINCE and UNTIL, in slices from timers.
ADD is called with each prompt, then DONE with nil or why they could
not be read.  Return a function stopping the reading."
  (let ((file (drover-prompts--opencode-database))
        (from (cons (floor (* 1000 since)) ""))
        (until (floor (* 1000 until)))
        (stopped nil))
    (cl-labels
        ((step ()
           (unless stopped
             (let ((rows nil)
                   (failure nil))
               (condition-case err
                   (let ((db (sqlite-open file t)))
                     (unwind-protect
                         (setq rows (drover-prompts--opencode-query db from until))
                       (sqlite-close db)))
                 (error (setq failure (drover-error-message err))))
               (dolist (row rows)
                 (setq from (cons (nth 2 row) (nth 0 row)))
                 (when-let* ((prompt (drover-prompts--opencode-row row)))
                   (funcall add prompt)))
               (cond
                (failure (setq stopped t) (funcall done failure))
                ((length< rows drover-prompts-opencode-page) (setq stopped t) (funcall done nil))
                (t (run-at-time 0.01 nil (lambda () (drover-with-reporting "prompts of OpenCode" (step))))))))))
      (cond
       ((not (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
        (funcall done "this Emacs has no SQLite"))
       ((not file) (funcall done nil))
       (t (step)))
      (lambda () (setq stopped t)))))

;;;; Claude Code and Codex

(defun drover-prompts--claude-prompt (record since until)
  "Return what Claude Code's RECORD tells of the prompts from SINCE to UNTIL.
A list of entries: one of kind \"user\" for a prompt of the period,
with its text :x and time :t, its cut length :c and directory :d; one
of kind \"old\" for a record older than the period, after which nothing
of the period comes; or nil."
  (when-let* ((time (drover-transcript-time (alist-get 'timestamp record))))
    (cond
     ((< time since) (list (list :y "old")))
     ((>= time until) nil)
     ((alist-get 'isSidechain record) nil)
     ((member (alist-get 'entrypoint record) '("sdk-cli" "sdk-py")) nil)
     (t
      (when-let* ((text (drover-claude-code--prompt-p record))
                  ((not (string-match-p drover-prompts--injected-regexp text)))
                  (bounded (drover-prompts--bound text)))
        (list (list :y "user" :x (car bounded) :c (cdr bounded) :t time
                    :d (alist-get 'cwd record))))))))

(defun drover-prompts--codex-prompt (record since until)
  "Return what Codex's RECORD tells of the prompts from SINCE to UNTIL.
As `drover-prompts--claude-prompt'."
  (when-let* ((time (drover-transcript-time (alist-get 'timestamp record))))
    (cond
     ((< time since) (list (list :y "old")))
     ((>= time until) nil)
     (t
      (let ((payload (alist-get 'payload record)))
        (when-let* (((equal (alist-get 'type record) "event_msg"))
                    ((drover-json-object-p payload))
                    ((equal (alist-get 'type payload) "user_message"))
                    (text (alist-get 'message payload))
                    ((stringp text))
                    ((not (string-match-p drover-prompts--injected-regexp text)))
                    (bounded (drover-prompts--bound text)))
          (list (list :y "user" :x (car bounded) :c (cdr bounded) :t time))))))))

(defun drover-prompts--codex-head (file)
  "Return what the first record of the rollout FILE tells, as (DIRECTORY . LISTED).
LISTED is nil for a conversation the user did not have: a subagent's,
one `codex exec' or an MCP client ran."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file nil 0 65536))
    (goto-char (point-min))
    (let* ((end (line-end-position))
           (sniff (lambda (key) (save-excursion (goto-char (point-min)) (drover-transcript-sniff key nil end))))
           (source (funcall sniff "source")))
      (cons (funcall sniff "cwd")
            (not (or (member source '("exec" "mcp"))
                     (funcall sniff "parent_thread_id")
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "\"source\":[ \t]*{" end t))))))))

(defun drover-prompts--recent-files (roots since)
  "Return the files of ROOTS changed since SINCE, the newest first.
Each root is (DIRECTORY DEPTH REGEXP): the files matching REGEXP DEPTH
levels below DIRECTORY."
  (let ((files nil))
    (pcase-dolist (`(,directory ,depth ,regexp) roots)
      (let ((directories (list directory)))
        (dotimes (_ depth)
          (setq directories
                (mapcan (lambda (parent)
                          (and (file-directory-p parent)
                               (seq-filter #'file-directory-p
                                           (directory-files parent t directory-files-no-dot-files-regexp t))))
                        directories)))
        (dolist (parent directories)
          (when (file-directory-p parent)
            (dolist (entry (directory-files-and-attributes parent t regexp t))
              (let ((attributes (cdr entry)))
                (when (and (null (file-attribute-type attributes))
                           (>= (float-time (file-attribute-modification-time attributes)) since))
                  (push (cons (car entry) (float-time (file-attribute-modification-time attributes)))
                        files))))))))
    (mapcar #'car (sort files :key #'cdr :reverse t :in-place t))))

(defun drover-prompts--conversations (since)
  "Return the conversations of Claude Code and Codex changed since SINCE.
Each is a plist: :engine, :id, :file, :directory, and :reader, the
function of a record and the period telling its prompts."
  (append
   (and (memq 'claude-code drover-prompts-engines)
        (mapcar (lambda (file)
                  (list :engine 'claude-code :id (file-name-base file) :file file
                        :reader #'drover-prompts--claude-prompt))
                (drover-prompts--recent-files
                 (list (list (drover-claude-code-projects-directory) 1 "\\.jsonl\\'"))
                 since)))
   (and (memq 'codex drover-prompts-engines)
        (seq-keep (lambda (file)
                    (let ((base (file-name-base file))
                          (head (ignore-errors (drover-prompts--codex-head file))))
                      (when (and (cdr head) (length> base 36))
                        (list :engine 'codex :id (substring base -36) :file file :directory (car head)
                              :reader #'drover-prompts--codex-prompt))))
                  (drover-prompts--recent-files
                   (let ((regexp "\\`rollout-.*\\.jsonl\\'"))
                     (list (list (expand-file-name "sessions" drover-codex-home) 3 regexp)
                           (list (expand-file-name "archived_sessions" drover-codex-home) 0 regexp)))
                   since)))))

(defun drover-prompts--conversation (conversation since until add done)
  "Read the prompts of CONVERSATION from SINCE to UNTIL, a page at a time.
Its file is read backward, from its end.  CONVERSATION is as
`drover-prompts--conversations' makes it.  ADD is called with each
prompt, then DONE with nil or why they could not be read.  Return a
function stopping the reading."
  (let* ((engine (plist-get conversation :engine))
         (id (plist-get conversation :id))
         (reader (plist-get conversation :reader))
         (title (when-let* ((session (drover-find-engine-session engine id)))
                  (drover-session-title session)))
         (function (lambda (record _directory) (funcall reader record since until)))
         (stop nil)
         (stopped nil))
    (cl-labels
        ((page (before)
           (setq stop (drover-conversation-page-file
                       (plist-get conversation :file) function nil before drover-prompts-file-page
                       (lambda (result)
                         (unless stopped
                           (if-let* ((failure (plist-get result :failure)))
                               (progn (setq stopped t) (funcall done failure))
                             (let ((old nil))
                               (seq-doseq (entry (plist-get result :e))
                                 (pcase (plist-get entry :y)
                                   ("old" (setq old t))
                                   ("user"
                                    (when-let* ((prompt (drover-prompts--make
                                                         engine id (plist-get entry :t) (plist-get entry :x)
                                                         (plist-get entry :c)
                                                         (or (plist-get entry :d) (plist-get conversation :directory))
                                                         title)))
                                      (funcall add prompt)))))
                               (if (or old (not (plist-get result :more)))
                                   (progn (setq stopped t) (funcall done nil))
                                 (page (plist-get result :from)))))))))))
      (page nil)
      (lambda ()
        (setq stopped t)
        (when stop (funcall stop))))))

;;;; Reading this host's

(defun drover-prompts--check-period (since until)
  "Signal an error unless SINCE and UNTIL, float times, make a period read."
  (unless (and (numberp since) (numberp until) (< since until))
    (error "No period from %s to %s" since until))
  (when (> (- until since) (* 86400 drover-prompts-days-limit))
    (error "A period of prompts spans %d days at most" drover-prompts-days-limit)))

(defun drover-prompts-collect (since until callback)
  "Read the prompts the user sent this host's sessions from SINCE to UNTIL.
SINCE and UNTIL are float times, the period at most
`drover-prompts-days-limit' days long.  CALLBACK gets, once, a plist:
:prompts, oldest first, as described above; :cut, how many of the
oldest were left out past `drover-prompts-limit'; :failures, a list of
\(ENGINE . MESSAGE) for the engines or conversations that could not be
read.  The files and the database are read in slices from timers.
Return a function stopping the reading."
  (drover-prompts--check-period since until)
  (setq drover-prompts--truenames (make-hash-table :test #'equal))
  (let* ((prompts nil)
         (failures nil)
         (stop nil)
         (stopped nil)
         (sources
          (append
           (and (memq 'opencode drover-prompts-engines)
                (list (cons 'opencode (lambda (add done) (drover-prompts--opencode since until add done)))))
           (mapcar (lambda (conversation)
                     (cons (plist-get conversation :engine)
                           (lambda (add done)
                             (drover-prompts--conversation conversation since until add done))))
                   (drover-prompts--conversations since))))
         (add (lambda (prompt) (push prompt prompts))))
    (cl-labels
        ((next ()
           (unless stopped
             (if (null sources)
                 (progn (setq stopped t)
                        (pcase-let ((`(,kept . ,cut) (drover-prompts--finish prompts)))
                          (funcall callback (list :prompts kept :cut cut :failures (nreverse failures)))))
               (pcase-let ((`(,engine . ,source) (pop sources)))
                 (let ((ended nil))
                   (setq stop (condition-case err
                                  (funcall source add
                                           (lambda (failure)
                                             (unless ended
                                               (setq ended t)
                                               (when failure
                                                 (push (cons engine failure) failures))
                                               (run-at-time 0 nil (lambda ()
                                                                    (drover-with-reporting "prompts" (next)))))))
                                (error (push (cons engine (drover-error-message err)) failures)
                                       (setq ended t)
                                       (run-at-time 0 nil (lambda () (drover-with-reporting "prompts" (next))))
                                       nil)))))))))
      (run-at-time 0 nil (lambda () (drover-with-reporting "prompts" (next))))
      (lambda ()
        (setq stopped t)
        (when stop (funcall stop))))))

;;;; Between hosts

(defun drover-prompts--wire (prompt)
  "Return PROMPT as it travels to a peer."
  (append (list :g (symbol-name (plist-get prompt :engine)) :id (plist-get prompt :id)
                :t (plist-get prompt :time) :x (plist-get prompt :text))
          (and (plist-get prompt :cut) (list :c (plist-get prompt :cut)))
          (and (plist-get prompt :directory) (list :d (plist-get prompt :directory)))
          (and (plist-get prompt :title) (list :ti (plist-get prompt :title)))))

(defun drover-prompts--encode (result)
  "Return RESULT, as `drover-prompts-collect' gives it, as it travels.
Within `drover-prompts-remote-bytes', the oldest prompts left out
first, counted with those cut already."
  (let* ((wires (mapcar #'drover-prompts--wire (plist-get result :prompts)))
         (sizes (mapcar (lambda (wire) (+ 2 (string-bytes (json-serialize wire)))) wires))
         (total (apply #'+ sizes))
         (cut (plist-get result :cut)))
    (while (and wires (> total drover-prompts-remote-bytes))
      (setq total (- total (pop sizes))
            cut (1+ cut))
      (pop wires))
    (list :p (vconcat wires) :cut cut
          :f (vconcat (mapcar (lambda (failure)
                                (list :g (symbol-name (car failure)) :m (drover-bound-line (cdr failure) 300)))
                              (plist-get result :failures))))))

(defun drover-prompts--name (value limit)
  "Return VALUE when a string, a line of LIMIT characters at most, else nil."
  (and (stringp value) (drover-bound-line value limit)))

(defun drover-prompts--decode-engine (value)
  "Return the engine VALUE names, a symbol, or nil when it names none known here.
Nothing is interned."
  (and (stringp value)
       (string-match-p "\\`[a-z][a-z0-9-]\\{0,30\\}\\'" value)
       (intern-soft value)))

(defun drover-prompts--decode-prompt (wire host)
  "Return the prompt WIRE, as a peer HOST sent it, checked and bounded, or nil."
  (when (listp wire)
    (let ((engine (drover-prompts--decode-engine (plist-get wire :g)))
          (id (drover-prompts--name (plist-get wire :id) 200))
          (time (plist-get wire :t))
          (bounded (drover-prompts--bound (plist-get wire :x)))
          (cut (plist-get wire :c)))
      (when (and engine id (numberp time) (< 0 time 1e11) bounded)
        (list :engine engine :id id :time (float time) :text (car bounded)
              :cut (or (cdr bounded) (and (natnump cut) cut))
              :directory (drover-prompts--name (plist-get wire :d) 1024)
              :title (drover-prompts--name (plist-get wire :ti) 300)
              :host host)))))

(defun drover-prompts--decode (reply host)
  "Return REPLY, the prompts a peer HOST sent, checked and bounded, as a plist.
Of :prompts, :cut and :failures, as `drover-prompts-collect' gives them."
  (let ((prompts nil)
        (failures nil)
        (wires (plist-get reply :p))
        (told (plist-get reply :f)))
    (when (vectorp wires)
      (seq-doseq (wire (seq-take wires drover-prompts-limit))
        (when-let* ((prompt (drover-prompts--decode-prompt wire host)))
          (push prompt prompts))))
    (when (vectorp told)
      (seq-doseq (failure (seq-take told 50))
        (when (listp failure)
          (when-let* ((engine (drover-prompts--decode-engine (plist-get failure :g))))
            (push (cons engine (or (drover-prompts--name (plist-get failure :m) 300) "failed")) failures)))))
    (list :prompts (nreverse prompts)
          :cut (let ((cut (plist-get reply :cut))) (if (natnump cut) cut 0))
          :failures (nreverse failures))))

(defun drover-prompts-remote-answer (message reply)
  "Answer a peer's MESSAGE asking this host's prompts, through REPLY, once read.
MESSAGE tells the period, :since and :until, float times."
  (let ((since (plist-get message :since))
        (until (plist-get message :until)))
    (condition-case err
        (drover-prompts-collect
         since until
         (lambda (result) (funcall reply (drover-prompts--encode result))))
      (error (funcall reply nil (drover-error-message err))))))

;;;; Every host's

(defun drover-prompts-gather (since until callback &optional seconds)
  "Read the prompts the user sent from SINCE to UNTIL, of this host and its peers.
Each host reads its own; a peer has SECONDS, by default
`drover-prompts-timeout', to answer.  CALLBACK gets, once, a plist:
:prompts, every host's, oldest first, each with its :host, nil for
this one; :hosts, a list of plists telling how each host answered: its
:host, :label, :status (`ok', `failed', `offline' for a peer not
connected, `old' for one whose drover reads no prompts), :count, :cut,
:failures and :message.  Return a function stopping the reading, after
which CALLBACK is not called."
  (drover-prompts--check-period since until)
  (let* ((seconds (or seconds drover-prompts-timeout))
         ;; By host, nil for this one: (STATUS . PROPERTIES), in order.
         (hosts (list (list nil 'pending :label (drover-host-label nil))))
         (stops nil)
         (finished nil)
         (timer nil))
    (cl-labels
        ((stop ()
           (unless finished
             (setq finished t)
             (when timer (cancel-timer timer))
             (dolist (function stops)
               (drover-with-reporting "prompts" (funcall function)))))
         (settle (host status &rest properties)
           (when-let* ((entry (assoc host hosts))
                       ((eq (nth 1 entry) 'pending)))
             (setcdr entry (cons status (append properties (cddr entry))))
             (unless (seq-some (lambda (entry) (eq (nth 1 entry) 'pending)) hosts)
               (finish))))
         (finish ()
           (unless finished
             (stop)
             (let ((prompts nil)
                   (told nil))
               (pcase-dolist (`(,host ,status . ,properties) hosts)
                 (let ((late (eq status 'pending)))
                   (setq prompts (append (plist-get properties :prompts) prompts))
                   (push (list :host host :label (plist-get properties :label)
                               :status (if late 'failed status)
                               :count (length (plist-get properties :prompts))
                               :cut (or (plist-get properties :cut) 0)
                               :failures (plist-get properties :failures)
                               :message (or (plist-get properties :message)
                                            (and late (format "did not answer within %d seconds" seconds))))
                         told)))
               (drover-with-reporting "prompts"
                 (funcall callback
                          (list :prompts (sort prompts :key (lambda (prompt) (plist-get prompt :time)) :in-place t)
                                :hosts (nreverse told))))))))
      (when (fboundp 'drover-remote-prompts-hosts)
        (pcase-dolist (`(,name ,label ,state) (drover-remote-prompts-hosts))
          (setq hosts (append hosts (list (list name (if (eq state 'ready) 'pending state) :label label))))))
      (setq timer (run-at-time seconds nil (lambda () (drover-with-reporting "prompts" (finish)))))
      (condition-case err
          (push (drover-prompts-collect
                 since until
                 (lambda (result)
                   (settle nil 'ok :prompts (plist-get result :prompts) :cut (plist-get result :cut)
                           :failures (plist-get result :failures))))
                stops)
        (error (settle nil 'failed :message (drover-error-message err))))
      (pcase-dolist (`(,name ,status . ,_) hosts)
        (when (and name (eq status 'pending))
          (drover-remote-ask-prompts
           name (list :since since :until until)
           (lambda (reply error)
             (drover-with-reporting "prompts"
               (if error
                   (settle name 'failed :message error)
                 (let ((result (drover-prompts--decode reply name)))
                   (settle name 'ok :prompts (plist-get result :prompts) :cut (plist-get result :cut)
                           :failures (plist-get result :failures))))))
           seconds)))
      #'stop)))

(provide 'drover-prompts)
;;; drover-prompts.el ends here
