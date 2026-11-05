;;; drover-usage.el --- What agents cost, in tokens and money  -*- lexical-binding: t; -*-

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

;; The tokens agents used, input, output, read from and written into
;; prompt caches, and what they cost, from what each engine records:
;;
;; - OpenCode records the usage and cost of each step in its service's
;;   database (`drover-opencode-database'), which drover reads, read
;;   only, a few hundred messages a slice: what came since the last
;;   look, and once, the history before, newest first.
;; - Claude Code records the usage of each reply in its transcripts,
;;   and Codex the tokens of each request in its rollouts: drover reads
;;   them whole, once, then only what was appended (`drover-transcript',
;;   an index of their own), a few megabytes a slice.  Neither records
;;   a cost: drover prices their tokens (`drover-usage-prices', then
;;   OpenCode's model catalog, then a built-in table).
;;
;; A subscription includes its usage (`drover-usage-billing'): its
;; tokens show, and its cost reads "included".  What was read is kept
;; in `drover-state-directory', checked against the files and the
;; database, so that an Emacs starting reads only what changed.
;;
;; `drover-usage', $ in `drover-dispatch', shows the totals of today,
;; this week, this month or ever, grouped by session, project, harness,
;; provider, model, host or day (`drover-usage-report').  The list
;; shows each session's totals, its subagents' included, in its details
;; and in the column `cost'.  Each host totals its own usage, and sends
;; its totals to its peers as they ask (`drover-remote'): this file
;; loads with drover's core alone, as a host answering does.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)
(require 'drover-transcript)
(require 'drover-claude-code)
(require 'drover-codex)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 18))
  (load "drover-core" nil t))

(defvar drover-opencode-database)
(declare-function opencode-format-tokens "opencode-look" (tokens))
(declare-function opencode-catalog "opencode-session" (kind &optional directory))
(declare-function opencode-catalog-ensure "opencode-session" (kind &optional directory))
(declare-function drover-opencode--call-local "drover-opencode" (function &rest arguments))
(declare-function drover-remote-usage-peers "drover-remote" ())
(declare-function drover-remote-ask-usage "drover-remote" (name request callback))

(defgroup drover-usage nil
  "What agents cost, in tokens and money."
  :group 'drover)

;;;; Options

(defcustom drover-usage-prices nil
  "Prices of models, which drover's own prices and catalogs give way to.
Each entry is (REGEXP . PRICES): a model whose identifier, downcased,
matches REGEXP costs PRICES, a plist of :input, :output, :cache-read
and :cache-write, dollars per million tokens, and :cache-write-1h, the
price of the tokens written into an hour's cache, by default twice the
input's.  The first match wins.  Claude Code and Codex record no cost:
their tokens are priced by this table, then by OpenCode's model
catalog, then by `drover-usage-default-prices'.  OpenCode's own costs,
which its service records, are kept."
  :type '(alist :key-type (regexp :tag "Model")
                :value-type (plist :options ((:input number) (:output number) (:cache-read number)
                                             (:cache-write number) (:cache-write-1h number))))
  :group 'drover-usage)

(defconst drover-usage-default-prices
  '(("claude-\\(?:3-\\)?opus-4-[01]\\|claude-3-opus\\|opus-4-1"
     :input 15 :output 75 :cache-read 1.5 :cache-write 18.75)
    ("opus" :input 5 :output 25 :cache-read 0.5 :cache-write 6.25)
    ("sonnet" :input 3 :output 15 :cache-read 0.3 :cache-write 3.75)
    ("haiku-3\\|claude-3-5-haiku\\|claude-3-haiku" :input 0.8 :output 4 :cache-read 0.08 :cache-write 1)
    ("haiku" :input 1 :output 5 :cache-read 0.1 :cache-write 1.25)
    ("nano" :input 0.05 :output 0.4 :cache-read 0.005 :cache-write 0)
    ("mini" :input 0.25 :output 2 :cache-read 0.025 :cache-write 0)
    ("gpt\\|codex\\|\\`o[0-9]" :input 1.25 :output 10 :cache-read 0.125 :cache-write 0))
  "Prices drover falls back on, as `drover-usage-prices' gives them.
Estimates of the list prices of model families, for a model neither
that option nor OpenCode's catalog prices.")

(defcustom drover-usage-billing
  '((opencode "\\`github-copilot" included)
    (claude-code nil auto)
    (codex nil auto))
  "How the usage of each harness and provider is billed.
Each entry is (HARNESS PROVIDER-REGEXP BILLING), the first match
winning: HARNESS is an engine, such as `opencode', `claude-code' or
`codex', or nil for any; PROVIDER-REGEXP matches the provider's
identifier, or is nil for any; BILLING is `priced', each token paid,
`included', a subscription paying for it, or `auto', for Claude Code
and Codex, which tell how their CLI logs in: Claude Code through a
subscription's account, without an API key nor a gateway of its own
settings, and Codex through ChatGPT, are included.  Usage matching no
entry is priced.  An included usage shows its tokens, its cost
\"included\"."
  :type '(repeat (list (choice (const :tag "Any harness" nil) symbol)
                       (choice (const :tag "Any provider" nil) regexp)
                       (choice (const priced) (const included) (const auto))))
  :group 'drover-usage)

(defcustom drover-usage-week-start 1
  "The day weeks begin on, 0 for Sunday, 1 for Monday."
  :type '(choice (const :tag "Monday" 1) (const :tag "Sunday" 0) natnum)
  :group 'drover-usage)

(defcustom drover-usage-period 'today
  "The period `drover-usage' totals: `today', `week', `month' or `all'."
  :type '(choice (const today) (const week) (const month) (const all))
  :group 'drover-usage)

(defcustom drover-usage-group-by 'project
  "How `drover-usage' groups the usage.
One of `session', the session at the top of its tree, its subagents'
usage with it; `project', `harness', `provider', `model', `host' or
`day'."
  :type '(choice (const session) (const project) (const harness) (const provider)
                 (const model) (const host) (const day))
  :group 'drover-usage)

(defcustom drover-usage-breakdown 'model
  "What each group of `drover-usage' is broken down by, or nil for nothing.
One of the groupings of `drover-usage-group-by'."
  :type '(choice (const :tag "Nothing" nil) (const session) (const project) (const harness)
                 (const provider) (const model) (const host) (const day))
  :group 'drover-usage)

(defcustom drover-usage-sort-by 'cost
  "How `drover-usage' orders its groups: `cost', `tokens' or `name'."
  :type '(choice (const cost) (const tokens) (const name))
  :group 'drover-usage)

(defconst drover-usage-read-chunk (* 4 1024 1024)
  "Bytes of a transcript or rollout read at most in one step of its index.
A longer one is read in several steps, in slices of the index's work.")

(defconst drover-usage-oc-page 400
  "Messages of OpenCode's database read at most in one query.")

(defconst drover-usage-oc-slice 0.008
  "Seconds a slice of reading OpenCode's database takes at most.")

(defconst drover-usage-oc-settle 3600
  "Seconds after which a message of OpenCode not finished counts as settled.")

(defconst drover-usage-save-interval 300
  "Seconds at least between two writes of what was read of OpenCode's usage.")

(defconst drover-usage-remote-rows 4000
  "Rows of totals a host sends a peer at most.")

(defconst drover-usage-remote-interval 60
  "Seconds before the totals a peer sent are asked for again.")

(defconst drover-usage-remote-reading-interval 5
  "Seconds before the totals of a peer still reading its own are asked for again.")

;;;; Days

(defvar drover-usage--days (make-hash-table :test #'eql)
  "Local days by quarter of an hour since the epoch, as YYYYMMDD numbers.
Every time zone's offset is a multiple of a quarter of an hour.")

(defun drover-usage-day (time)
  "Return the local day of TIME, a float time, as a number YYYYMMDD."
  (let ((quarter (floor time 900)))
    (or (gethash quarter drover-usage--days)
        (progn
          (when (> (hash-table-count drover-usage--days) 200000)
            (clrhash drover-usage--days))
          (puthash quarter (string-to-number (format-time-string "%Y%m%d" (* quarter 900)))
                   drover-usage--days)))))

(defun drover-usage--iso-time (string)
  "Return the ISO 8601 time STRING as a float time, or nil.
Times in UTC, as engines write them, are read directly; others through
`drover-transcript-time'."
  (when (stringp string)
    (if (and (>= (length string) 20)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\(?:\\.[0-9]*\\)?Z\\'"
                             string))
        (float-time (encode-time (list (string-to-number (substring string 17 19))
                                       (string-to-number (substring string 14 16))
                                       (string-to-number (substring string 11 13))
                                       (string-to-number (substring string 8 10))
                                       (string-to-number (substring string 5 7))
                                       (string-to-number (substring string 0 4))
                                       nil nil t)))
      (drover-transcript-time string))))

(defun drover-usage--day-time (day)
  "Return the start of DAY, a number YYYYMMDD, as a float time."
  (float-time (encode-time (list 0 0 0 (% day 100) (% (/ day 100) 100) (/ day 10000)))))

(defun drover-usage-period-start (period &optional now)
  "Return the first day of PERIOD at NOW, a number YYYYMMDD, nil for `all'.
PERIOD is `today', `week', beginning on `drover-usage-week-start', or
`month'."
  (let* ((now (or now (float-time)))
         (decoded (decode-time now)))
    (pcase period
      ('today (drover-usage-day now))
      ('week (drover-usage-day (- now (* 86400 (mod (- (decoded-time-weekday decoded) drover-usage-week-start) 7)))))
      ('month (+ (* 10000 (decoded-time-year decoded)) (* 100 (decoded-time-month decoded)) 1))
      (_ nil))))

(defconst drover-usage-period-labels
  '((today . "today") (week . "this week") (month . "this month") (all . "ever"))
  "How each period reads.")

;;;; Text from outside

(defun drover-usage--name (value)
  "Return VALUE, a name read from outside, bounded, or nil when not a string."
  (and (stringp value) (drover-bound-line value 200)))

(defun drover-usage--count (value)
  "Return VALUE when it is a count of tokens, a natural number, else 0."
  (cond ((natnump value) value)
        ((and (floatp value) (>= value 0) (< value 1e15)) (truncate value))
        (t 0)))

(defun drover-usage--money (value)
  "Return VALUE when it is a finite amount of money, else nil."
  (and (numberp value) (or (integerp value) (not (isnan value))) (< (abs value) 1e12) value))

;;;; Reading lines of files

(defun drover-usage--scan (file start size function)
  "Call FUNCTION on each complete line of FILE from byte START, of SIZE bytes.
Lines are read as they are, undecoded, `drover-usage-read-chunk' bytes
or so: FUNCTION is called in a unibyte buffer with the bounds of each
line, point at its start.  Return the byte after the last line read,
START when none is complete: a line being written is read once whole."
  (if (>= start size)
      start
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((end (min size (+ start drover-usage-read-chunk)))
            (last nil))
        (insert-file-contents-literally file nil start end)
        (while (and (not (setq last (save-excursion (goto-char (point-max))
                                                    (and (search-backward "\n" nil t) (point)))))
                    (< end size))
          (let ((more (min size (+ end drover-usage-read-chunk))))
            (goto-char (point-max))
            (insert-file-contents-literally file nil end more)
            (setq end more)))
        (if (not last)
            start
          (goto-char (point-min))
          (while (< (point) last)
            (let ((line-end (line-end-position)))
              (save-excursion (funcall function (point) line-end))
              (goto-char (1+ line-end))))
          ;; The newline at position LAST is the byte LAST - 1 read.
          (+ start last))))))

(defun drover-usage--parse (beg end)
  "Return the JSON object between BEG and END of a unibyte buffer, or nil."
  (let ((object (ignore-errors
                  (json-parse-string (decode-coding-string (buffer-substring-no-properties beg end)
                                                           'utf-8-unix t)
                                     :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil))))
    (and (consp object) (consp (car object)) object)))

;;;; Entries

;; What a transcript or rollout told of its usage is kept per request,
;; as a vector [KEY DAY MODEL INPUT OUTPUT CACHE-READ CACHE-WRITE
;; CACHE-WRITE-1H]: KEY names the request, so that a request recorded
;; in two files, as a forked conversation copies the history of its
;; parent, counts once; INPUT counts the tokens of the prompt neither
;; read from nor written into a cache; OUTPUT, those generated,
;; reasoning included; CACHE-WRITE-1H, those of CACHE-WRITE written
;; into a cache of an hour, which costs more.

(defsubst drover-usage--entry (key day model input output read write hour)
  "Return the entry of a request KEY on DAY by MODEL, of its counts.
INPUT, OUTPUT, READ, WRITE and HOUR are as the commentary above tells."
  (vector key day model input output read write hour))

;;;; Claude Code

(defconst drover-usage--claude-version 1
  "Version of `drover-usage--claude-read'.")

(defvar drover-usage--claude-index nil
  "The index of the usage Claude Code's transcripts record, once made.")

(defun drover-usage--claude-ids (file)
  "Return (CONVERSATION . AGENT) of the transcript FILE.
AGENT is nil but for a subagent's."
  (let ((directory (directory-file-name (file-name-directory file))))
    (if (equal (file-name-nondirectory directory) "subagents")
        (cons (file-name-nondirectory (directory-file-name (file-name-directory directory)))
              (file-name-base file))
      (cons (file-name-base file) nil))))

(defun drover-usage--claude-line (data beg end)
  "Return DATA with the usage of the line between BEG and END taken in.
DATA is what was read of a transcript; a reply's usage becomes an
entry of its :entries, newest first, the records of one message making
one, their newest usage."
  (goto-char beg)
  (if (not (and (search-forward "\"type\":\"assistant\"" end t)
                (progn (goto-char beg) (search-forward "\"usage\":" end t))))
      data
    (let* ((record (drover-usage--parse beg end))
           (message (alist-get 'message record))
           (usage (and (drover-json-object-p message) (alist-get 'usage message)))
           (model (and (drover-json-object-p usage) (alist-get 'model message)))
           (time (and (stringp model) (not (string-prefix-p "<" model))
                      (equal (alist-get 'type record) "assistant")
                      (drover-usage--iso-time (alist-get 'timestamp record)))))
      (when time
        (let* ((id (or (drover-usage--name (alist-get 'id message))
                       (drover-usage--name (alist-get 'uuid record))
                       (format "%s" time)))
               (creation (alist-get 'cache_creation usage))
               (entry (drover-usage--entry
                       id (drover-usage-day time) (drover-usage--name model)
                       (drover-usage--count (alist-get 'input_tokens usage))
                       (drover-usage--count (alist-get 'output_tokens usage))
                       (drover-usage--count (alist-get 'cache_read_input_tokens usage))
                       (drover-usage--count (alist-get 'cache_creation_input_tokens usage))
                       (if (drover-json-object-p creation)
                           (drover-usage--count (alist-get 'ephemeral_1h_input_tokens creation))
                         0)))
               (entries (plist-get data :entries)))
          (if (and entries (equal (aref (car entries) 0) id))
              (setcar entries entry)
            (setq data (plist-put data :entries (cons entry entries))))
          (unless (plist-get data :first)
            (setq data (plist-put data :first time)))
          (unless (plist-get data :directory)
            (when-let* ((cwd (drover-usage--name (alist-get 'cwd record))))
              (setq data (plist-put data :directory (file-name-as-directory cwd)))))))
      data)))

(defun drover-usage--claude-read (file attributes old)
  "Return what transcript FILE, of ATTRIBUTES, tells of its replies' usage.
OLD is the entry read before, or nil: a transcript grown since, the
same file, is read from where the last read ended, at most
`drover-usage-read-chunk' bytes or so at a time, :read-to telling where
it stopped.  For the index of `drover-transcript'."
  (let* ((size (file-attribute-size attributes))
         (before (and old (drover-transcript-data old)))
         (offset (and before
                      (equal (drover-transcript-inode old) (file-attribute-inode-number attributes))
                      (natnump (plist-get before :offset))
                      (<= (plist-get before :offset) size)
                      (plist-get before :offset)))
         (ids (drover-usage--claude-ids file))
         (data (if offset
                   (copy-sequence before)
                 (list :conversation (car ids) :agent (cdr ids) :entries nil)))
         (start (or offset 0))
         (end (drover-usage--scan file start size
                                  (lambda (beg end) (setq data (drover-usage--claude-line data beg end))))))
    (unless (plist-get data :directory)
      (when-let* ((bucket (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory (if (cdr ids)
                                                     (directory-file-name
                                                      (file-name-directory
                                                       (directory-file-name (file-name-directory file))))
                                                   file)))))
                  (directory (drover-claude-code-decode-bucket bucket)))
        (setq data (plist-put data :directory (file-name-as-directory directory)))))
    (setq data (plist-put data :offset end))
    (unless (cdr ids)
      (setq data (plist-put data :subagents
                            (and (file-directory-p (expand-file-name "subagents" (file-name-sans-extension file)))
                                 t))))
    (plist-put data :read-to end)))

(defun drover-usage--claude-key (data)
  "Return the conversation of DATA, or CONVERSATION/AGENT of a subagent's."
  (when-let* ((conversation (plist-get data :conversation)))
    (if-let* ((agent (plist-get data :agent)))
        (concat conversation "/" agent)
      conversation)))

(defun drover-usage--claude-roots ()
  "Return where Claude Code's transcripts are, those of subagents included."
  (let ((projects (drover-claude-code-projects-directory))
        (roots nil))
    (when-let* ((index drover-usage--claude-index))
      (drover-transcript-map (lambda (file data)
                               (when (plist-get data :subagents)
                                 (push (list (expand-file-name "subagents" (file-name-sans-extension file))
                                             0 "\\`agent-.*\\.jsonl\\'")
                                       roots)))
                             index))
    (cons (list projects 1 "\\.jsonl\\'") roots)))

;;;; Codex

(defconst drover-usage--codex-version 1
  "Version of `drover-usage--codex-read'.")

(defvar drover-usage--codex-index nil
  "The index of the usage Codex's rollouts record, once made.")

(defun drover-usage--codex-usage (usage)
  "Return (INPUT OUTPUT CACHED) of USAGE, Codex's count of a request's tokens.
Codex counts the cached tokens among the input's: INPUT leaves them out."
  (let ((input (drover-usage--count (alist-get 'input_tokens usage)))
        (cached (drover-usage--count (alist-get 'cached_input_tokens usage))))
    (list (max 0 (- input cached)) (drover-usage--count (alist-get 'output_tokens usage)) cached)))

(defun drover-usage--codex-line (data beg end)
  "Return DATA with what the line from BEG to END of a rollout tells taken in.
Its conversation, directory, provider and parent, from its metadata;
the model of the next requests, from a turn's context; the tokens of a
request, an entry of :entries, newest first.  Codex counts again the
same totals now and then, as rate limits change: a count whose totals
are those of the one before counts nothing."
  (goto-char beg)
  (cond
   ((search-forward "\"type\":\"token_count\"" end t)
    (let* ((record (drover-usage--parse beg end))
           (payload (alist-get 'payload record))
           (info (and (drover-json-object-p payload) (alist-get 'info payload)))
           (total (and (drover-json-object-p info) (alist-get 'total_token_usage info)))
           (last (and (drover-json-object-p info) (alist-get 'last_token_usage info)))
           (time (drover-usage--iso-time (alist-get 'timestamp record))))
      (when (and time (drover-json-object-p total))
        (let ((totals (drover-usage--codex-usage total))
              (before (plist-get data :total)))
          (unless (equal totals before)
            (let ((counts (if (drover-json-object-p last)
                              (drover-usage--codex-usage last)
                            (if (and before (cl-every #'>= totals before))
                                (cl-mapcar #'- totals before)
                              totals))))
              (setq data (plist-put data :total totals))
              (setq data (plist-put data :entries
                                    (cons (drover-usage--entry
                                           (format "%s|%s" (alist-get 'timestamp record)
                                                   (drover-usage--count (alist-get 'total_tokens total)))
                                           (drover-usage-day time) (or (plist-get data :model) "unknown")
                                           (nth 0 counts) (nth 1 counts) (nth 2 counts) 0 0)
                                          (plist-get data :entries))))
              (unless (plist-get data :first)
                (setq data (plist-put data :first time)))))))))
   ((progn (goto-char beg) (search-forward "\"type\":\"turn_context\"" end t))
    (let* ((payload (alist-get 'payload (drover-usage--parse beg end)))
           (model (and (drover-json-object-p payload) (drover-usage--name (alist-get 'model payload)))))
      (when model
        (setq data (plist-put data :model model)))))
   ((progn (goto-char beg) (search-forward "\"type\":\"session_meta\"" end t))
    (let ((payload (alist-get 'payload (drover-usage--parse beg end))))
      (when (and (drover-json-object-p payload) (not (plist-get data :id)))
        (let ((source (alist-get 'source payload)))
          (setq data (plist-put data :id (drover-usage--name (alist-get 'id payload))))
          (when-let* ((cwd (drover-usage--name (alist-get 'cwd payload))))
            (setq data (plist-put data :directory (file-name-as-directory cwd))))
          (when-let* ((provider (drover-usage--name (alist-get 'model_provider payload))))
            (setq data (plist-put data :provider provider)))
          (when-let* ((parent (and (drover-json-object-p source)
                                   (drover-usage--name
                                    (map-nested-elt source '(subagent thread_spawn parent_thread_id))))))
            (setq data (plist-put data :parent parent))))))))
  data)

(defun drover-usage--codex-read (file attributes old)
  "Return what rollout FILE, of ATTRIBUTES, tells of the tokens of its requests.
OLD is the entry read before, or nil; see `drover-usage--claude-read'."
  (let* ((size (file-attribute-size attributes))
         (before (and old (drover-transcript-data old)))
         (offset (and before
                      (equal (drover-transcript-inode old) (file-attribute-inode-number attributes))
                      (natnump (plist-get before :offset))
                      (<= (plist-get before :offset) size)
                      (plist-get before :offset)))
         (data (if offset (copy-sequence before) (list :entries nil)))
         (end (drover-usage--scan file (or offset 0) size
                                  (lambda (beg end) (setq data (drover-usage--codex-line data beg end))))))
    (setq data (plist-put data :offset end))
    (plist-put data :read-to end)))

(defun drover-usage--codex-roots ()
  "Return where Codex's rollouts are, archived ones included."
  (let ((regexp "\\`rollout-.*\\.jsonl\\'"))
    (list (list (expand-file-name "sessions" drover-codex-home) 3 regexp)
          (list (expand-file-name "archived_sessions" drover-codex-home) 0 regexp))))

;;;; The indexes

(defvar drover-usage--started nil
  "Non-nil once the usage's sources were started.")

(defvar drover-usage--generation 0
  "Count of the changes of the usage known, which caches compare.")

(defvar drover-usage--changed nil
  "Conversations whose usage changed since views were told, as (ENGINE . ID).")

(defvar drover-usage--notify-timer nil
  "Timer telling views of the usage changed.")

(defvar drover-usage-changed-hook nil
  "Hook run once the usage known changed, a second or so after the changes.")

(defun drover-usage--note-change (engine ids)
  "Note that the usage of the conversations IDS of ENGINE changed."
  (cl-incf drover-usage--generation)
  (dolist (id ids)
    (when id
      (push (cons engine id) drover-usage--changed)))
  (unless (timerp drover-usage--notify-timer)
    (setq drover-usage--notify-timer (run-at-time 1 nil #'drover-usage--notify))))

(defun drover-usage--claude-changed (_file old new)
  "Note that the usage read of a transcript went from OLD to NEW."
  (let ((data (or new old)))
    (drover-usage--note-change 'claude-code
                               (delete-dups (list (plist-get data :conversation)
                                                  (drover-usage--claude-key data))))))

(defun drover-usage--codex-changed (_file old new)
  "Note that the usage read of a rollout went from OLD to NEW."
  (drover-usage--note-change 'codex (list (plist-get (or new old) :id))))

(defun drover-usage--make-claude-index ()
  "Return a new index of the usage of Claude Code's transcripts."
  (drover-transcript-index-create 'claude-code-usage #'drover-usage--claude-roots
                                  #'drover-usage--claude-read #'drover-usage--claude-changed
                                  nil #'drover-usage--claude-key drover-usage--claude-version))

(defun drover-usage--make-codex-index ()
  "Return a new index of the usage of Codex's rollouts."
  (drover-transcript-index-create 'codex-usage #'drover-usage--codex-roots
                                  #'drover-usage--codex-read #'drover-usage--codex-changed
                                  nil #'drover-transcript-id drover-usage--codex-version))

(defun drover-usage--index (variable make)
  "Return the index VARIABLE holds, made anew by MAKE when of another layout.
Nil before the sources started.  One made anew is started."
  (let ((index (symbol-value variable)))
    (cond ((null index) nil)
          ((drover-transcript-index-current-p index) index)
          (t (drover-stale-layout "index of usage")
             (set variable (funcall make))
             (drover-transcript-start (symbol-value variable))
             (symbol-value variable)))))

(defun drover-usage--claude-current ()
  "Return the index of the usage of Claude Code's transcripts, or nil."
  (drover-usage--index 'drover-usage--claude-index #'drover-usage--make-claude-index))

(defun drover-usage--codex-current ()
  "Return the index of the usage of Codex's rollouts, or nil."
  (drover-usage--index 'drover-usage--codex-index #'drover-usage--make-codex-index))

;;;; OpenCode

;; OpenCode's service records each step in its database: the message
;; of an assistant, its model and provider, its tokens and the cost the
;; service computed, as its own statistics read them.  Drover reads
;; them read only, by their time of creation, which the database
;; indexes: forward from where it stopped, and once, the history
;; before, newest first.  A step still running has no usage yet: the
;; steps from the first one not finished are read again at each look,
;; until they are, or `drover-usage-oc-settle' seconds old.  What was
;; read is kept as totals by session, day, provider and model.

(cl-defstruct (drover-usage--oc (:constructor drover-usage--oc-make)
                                (:copier nil))
  "What drover read of the usage OpenCode's database records."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-usage--oc) :documentation "The layouts of the types it was made of.")
  (file nil :documentation "The database read, its true name.")
  (forward nil :documentation "Where reading on begins, (TIME . ID): messages after it are new.")
  (back nil :documentation "Where reading the history goes on, (TIME . ID), or t once all read.")
  (meta 0 :documentation "The time of the newest change of a session read, in milliseconds.")
  (sessions (make-hash-table :test #'equal)
            :documentation "Sessions by identifier, as (PARENT DIRECTORY TITLE ROWS).
ROWS are vectors [DAY PROVIDER MODEL INPUT OUTPUT CACHE-READ CACHE-WRITE COST].")
  (live nil :documentation "The steps not settled, as (MESSAGE . STEP).
STEP is as `drover-usage--oc-step' returns it.")
  (checked 0 :documentation "When the sessions were last read, a float time.")
  (timer nil :documentation "Timer of the next slice.")
  (busy nil :documentation "Non-nil while slices read.")
  (again nil :documentation "Non-nil when a look was asked for while slices read.")
  (failure nil :documentation "Why the database could not be read, a line, or nil.")
  (loaded nil :documentation "Non-nil once what was kept was read.")
  (dirty nil :documentation "Non-nil when what was read changed since kept.")
  (saved 0 :documentation "When it was kept last, a float time."))

(drover-note-layout 'drover-usage--oc)

(defvar drover-usage--oc nil
  "What drover read of OpenCode's usage, once its sources started.")

(defconst drover-usage--oc-version 1
  "Version of the format of the file OpenCode's usage is kept in.")

(defun drover-usage--oc-cache-file ()
  "Return the file what was read of OpenCode's usage is kept in."
  (expand-file-name "opencode-usage.eld" drover-state-directory))

(defun drover-usage--oc-database ()
  "Return the true name of OpenCode's database, or nil when it cannot be read."
  (and (boundp 'drover-opencode-database)
       (stringp drover-opencode-database)
       (file-readable-p drover-opencode-database)
       (file-truename drover-opencode-database)))

(defun drover-usage--oc-current ()
  "Return what was read of OpenCode's usage, or nil before its sources started.
One made by another version of drover is made anew."
  (let ((oc drover-usage--oc))
    (cond ((null oc) nil)
          ((drover-layout-current-p oc 'drover-usage--oc) oc)
          (t (drover-stale-layout "usage of OpenCode")
             (setq drover-usage--oc (drover-usage--oc-make))
             (drover-usage--oc-look)
             drover-usage--oc))))

(defun drover-usage--oc-load (oc)
  "Read what was kept of the usage of OpenCode into OC, unless read already."
  (unless (drover-usage--oc-loaded oc)
    (setf (drover-usage--oc-loaded oc) t)
    (condition-case err
        (let ((entries (drover-read-data (drover-usage--oc-cache-file) drover-usage--oc-version :usage)))
          (pcase (car entries)
            (`(:file ,file :forward ,forward :back ,back :meta ,meta)
             (when (and (equal file (drover-usage--oc-file oc))
                        (consp forward) (numberp (car forward))
                        (or (eq back t) (and (consp back) (numberp (car back))))
                        (numberp meta))
               (setf (drover-usage--oc-forward oc) forward
                     (drover-usage--oc-back oc) back
                     (drover-usage--oc-meta oc) meta)
               (let ((sessions (drover-usage--oc-sessions oc)))
                 (pcase-dolist (`(,id ,parent ,directory ,title ,rows) (cdr entries))
                   (when (and (stringp id) (listp rows) (seq-every-p #'vectorp rows))
                     (puthash id (list parent directory title rows) sessions))))))))
      (error (drover-warn "Ignoring what drover kept of OpenCode's usage: %s" (drover-error-message err))))))

(defun drover-usage--oc-save (oc &optional force)
  "Keep what was read of OpenCode's usage in OC, when it changed.
Unless FORCE, not before `drover-usage-save-interval' seconds passed."
  (when (and (drover-usage--oc-dirty oc) (drover-usage--oc-forward oc)
             (or force (> (- (float-time) (drover-usage--oc-saved oc)) drover-usage-save-interval)))
    (setf (drover-usage--oc-dirty oc) nil
          (drover-usage--oc-saved oc) (float-time))
    (let ((entries nil))
      (maphash (lambda (id session) (push (cons id session) entries)) (drover-usage--oc-sessions oc))
      (condition-case err
          (drover-write-data (drover-usage--oc-cache-file) "What drover read of OpenCode's usage"
                             drover-usage--oc-version :usage
                             (cons (list :file (drover-usage--oc-file oc)
                                         :forward (drover-usage--oc-forward oc)
                                         :back (drover-usage--oc-back oc)
                                         :meta (drover-usage--oc-meta oc))
                                   (sort entries :key #'car :in-place t)))
        (error (drover-warn "Could not keep what drover read of OpenCode's usage: %s"
                            (drover-error-message err)))))))

(defconst drover-usage--oc-columns
  (concat "id, session_id, time_created, "
          "json_extract(data, '$.model.providerID', '$.model.id', '$.tokens', '$.cost', "
          "'$.time.completed', '$.error', '$.status')")
  "The columns of the messages read: what a step used, and whether it ended.")

(defun drover-usage--oc-query (db direction from)
  "Return a page of the steps of DB after FROM, (TIME . ID), in DIRECTION.
DIRECTION is `forward', the oldest first, or `back', the newest first,
before FROM."
  (sqlite-select
   db
   (format "SELECT %s FROM session_message WHERE type IN ('assistant', 'compaction') AND %s ORDER BY time_created %s, id %s LIMIT %d"
           drover-usage--oc-columns
           (if (eq direction 'forward)
               "(time_created > ?1 OR (time_created = ?1 AND id > ?2))"
             "(time_created < ?1 OR (time_created = ?1 AND id < ?2))")
           (if (eq direction 'forward) "ASC" "DESC")
           (if (eq direction 'forward) "ASC" "DESC")
           drover-usage-oc-page)
   (list (car from) (cdr from))))

(defun drover-usage--oc-step (row)
  "Return what the step of ROW, a message read, used, or nil.
A list (SESSION TIME SETTLED DAY PROVIDER MODEL INPUT OUTPUT READ WRITE
COST); SETTLED is nil while the step runs."
  (pcase-let* ((`(,_id ,session ,created ,json) row)
               (values (and (stringp json)
                             (ignore-errors (json-parse-string json :object-type 'alist :array-type 'list
                                                               :null-object nil :false-object nil)))))
    (when (and (stringp session) (numberp created) (listp values))
      (pcase-let* ((`(,provider ,model ,tokens ,cost ,completed ,error ,status) values)
                   (time (/ created 1000.0))
                   (cache (and (drover-json-object-p tokens) (alist-get 'cache tokens))))
        (list (drover-usage--name session) time
              (or completed error (member status '("completed" "failed"))
                  (< time (- (float-time) drover-usage-oc-settle)))
              (drover-usage-day time)
              (or (drover-usage--name provider) "unknown")
              (or (drover-usage--name model) "unknown")
              (if (drover-json-object-p tokens) (drover-usage--count (alist-get 'input tokens)) 0)
              (if (drover-json-object-p tokens)
                  (+ (drover-usage--count (alist-get 'output tokens))
                     (drover-usage--count (alist-get 'reasoning tokens)))
                0)
              (if (drover-json-object-p cache) (drover-usage--count (alist-get 'read cache)) 0)
              (if (drover-json-object-p cache) (drover-usage--count (alist-get 'write cache)) 0)
              (or (drover-usage--money cost) 0))))))

(defun drover-usage--add-row (rows step)
  "Return ROWS, vectors of totals by day, provider and model, with STEP added.
STEP is as `drover-usage--oc-step' returns it."
  (pcase-let ((`(,_session ,_time ,_settled ,day ,provider ,model . ,counts) step))
    (let ((row (seq-find (lambda (row) (and (eql (aref row 0) day) (equal (aref row 1) provider)
                                            (equal (aref row 2) model)))
                         rows)))
      (if row
          (progn (cl-loop for count in counts for index from 3
                          do (aset row index (+ (aref row index) count)))
                 rows)
        (cons (apply #'vector day provider model counts) rows)))))

(defun drover-usage--oc-session (oc id)
  "Return the session ID of OC, as (PARENT DIRECTORY TITLE ROWS), made if new."
  (or (gethash id (drover-usage--oc-sessions oc))
      (puthash id (list nil nil nil nil) (drover-usage--oc-sessions oc))))

(defun drover-usage--oc-take (oc step)
  "Take STEP, settled, into the totals of OC; return its session."
  (let ((session (drover-usage--oc-session oc (car step))))
    (setf (nth 3 session) (drover-usage--add-row (nth 3 session) step))
    (setf (drover-usage--oc-dirty oc) t)
    (car step)))

(defun drover-usage--oc-meta-read (oc db)
  "Read what changed of OpenCode's sessions in DB into OC.
Their parents, directories and titles.  Return the sessions read."
  (let ((rows (sqlite-select db "SELECT id, parent_id, directory, title, time_updated FROM session_v2 WHERE time_updated >= ?"
                             (list (drover-usage--oc-meta oc))))
        (ids nil))
    (pcase-dolist (`(,id ,parent ,directory ,title ,updated) rows)
      (when (stringp id)
        (let ((session (drover-usage--oc-session oc id))
              (directory (and (stringp directory) (not (string-empty-p directory))
                              (file-name-as-directory (drover-usage--name directory)))))
          (unless (and (equal (nth 0 session) (drover-usage--name parent))
                       (equal (nth 1 session) directory)
                       (equal (nth 2 session) (drover-usage--name title)))
            (setf (nth 0 session) (drover-usage--name parent)
                  (nth 1 session) directory
                  (nth 2 session) (drover-usage--name title)
                  (drover-usage--oc-dirty oc) t)
            (push id ids))
          (when (and (numberp updated) (> updated (drover-usage--oc-meta oc)))
            (setf (drover-usage--oc-meta oc) updated)))))
    ids))

(defun drover-usage--oc-forward-pass (oc db deadline)
  "Read the steps of DB since OC's forward point, until DEADLINE.
Settled steps from there on are taken in and the point moves past them;
from the first step running, every step is a live one, read again next
time.  Return (DONE . SESSIONS): DONE nil when DEADLINE came first."
  (let ((from (drover-usage--oc-forward oc))
        (live nil)
        (holding nil)
        (sessions nil)
        (done nil))
    (while (and (not done) (< (float-time) deadline))
      (let ((rows (drover-usage--oc-query db 'forward from)))
        (dolist (row rows)
          (when-let* ((step (drover-usage--oc-step row)))
            (if (and (nth 2 step) (not holding))
                (progn (push (drover-usage--oc-take oc step) sessions)
                       ;; Counted once: no longer among the live ones.
                       (setf (drover-usage--oc-live oc)
                             (assoc-delete-all (nth 0 row) (drover-usage--oc-live oc)))
                       (setf (drover-usage--oc-forward oc) (cons (nth 2 row) (nth 0 row))))
              (setq holding t)
              (push (cons (nth 0 row) step) live)
              (push (car step) sessions)))
          (setq from (cons (nth 2 row) (nth 0 row))))
        (when (< (length rows) drover-usage-oc-page)
          (setq done t))))
    (let ((before (mapcar (lambda (entry) (cadr entry)) (drover-usage--oc-live oc))))
      (when done
        (setf (drover-usage--oc-live oc) live))
      (cons done (append before sessions)))))

(defun drover-usage--oc-back-pass (oc db deadline)
  "Read the history of DB before OC's back point, newest first, until DEADLINE.
Return the sessions read."
  (let ((sessions nil))
    (while (and (consp (drover-usage--oc-back oc)) (< (float-time) deadline))
      (let ((rows (drover-usage--oc-query db 'back (drover-usage--oc-back oc))))
        (dolist (row rows)
          (when-let* ((step (drover-usage--oc-step row)))
            (push (drover-usage--oc-take oc step) sessions))
          (setf (drover-usage--oc-back oc) (cons (nth 2 row) (nth 0 row))))
        (when (< (length rows) drover-usage-oc-page)
          (setf (drover-usage--oc-back oc) t
                (drover-usage--oc-dirty oc) t))))
    sessions))

(defun drover-usage--oc-open (oc)
  "Return a connection to OC's database, read only, or nil, noting why not."
  (let ((file (drover-usage--oc-database)))
    (cond
     ((not (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
      (setf (drover-usage--oc-failure oc) "this Emacs has no SQLite")
      nil)
     ((not file)
      (setf (drover-usage--oc-failure oc) "no database to read")
      nil)
     (t
      (unless (equal file (drover-usage--oc-file oc))
        ;; Another database: what was read of the one before is not its.
        (clrhash (drover-usage--oc-sessions oc))
        (setf (drover-usage--oc-file oc) file
              (drover-usage--oc-forward oc) nil
              (drover-usage--oc-back oc) nil
              (drover-usage--oc-live oc) nil
              (drover-usage--oc-meta oc) 0
              (drover-usage--oc-loaded oc) nil))
      (drover-usage--oc-load oc)
      (condition-case err
          (let ((db (sqlite-open file t)))
            (setf (drover-usage--oc-failure oc) nil)
            db)
        (error (setf (drover-usage--oc-failure oc) (drover-error-message err))
               nil))))))

(defun drover-usage--oc-work (oc)
  "Read OpenCode's usage into OC for a slice, then schedule the next one.
From a timer: the history read back to its start, the sessions read and
what is new, until nothing is left."
  (when (drover-layout-current-p oc 'drover-usage--oc)
    (setf (drover-usage--oc-timer oc) nil)
    (let ((more nil)
          (sessions nil))
      (drover-with-reporting "usage of OpenCode"
        (when-let* ((db (drover-usage--oc-open oc)))
          (unwind-protect
              (condition-case err
                  (let ((deadline (+ (float-time) drover-usage-oc-slice)))
                    (unless (drover-usage--oc-forward oc)
                      ;; The history ends an hour ago: what came since is read forward.
                      (let ((start (cons (* 1000 (floor (- (float-time) drover-usage-oc-settle))) "")))
                        (setf (drover-usage--oc-forward oc) start
                              (drover-usage--oc-back oc) start)))
                    (when (> (- (float-time) (drover-usage--oc-checked oc)) 5)
                      (setf (drover-usage--oc-checked oc) (float-time))
                      (setq sessions (drover-usage--oc-meta-read oc db)))
                    (pcase-let ((`(,done . ,read) (drover-usage--oc-forward-pass oc db deadline)))
                      (setq sessions (append read sessions)
                            more (not done)))
                    (unless more
                      (setq sessions (append (drover-usage--oc-back-pass oc db deadline) sessions)
                            more (consp (drover-usage--oc-back oc)))))
                (error (setf (drover-usage--oc-failure oc) (drover-error-message err))))
            (sqlite-close db))))
      (when sessions
        (drover-usage--note-change 'opencode (delete-dups sessions)))
      (cond ((or more (drover-usage--oc-again oc))
             (setf (drover-usage--oc-again oc) nil)
             (setf (drover-usage--oc-timer oc)
                   (run-at-time 0.05 nil #'drover-usage--oc-work oc)))
            (t (setf (drover-usage--oc-busy oc) nil)
               (drover-usage--oc-save oc (not (consp (drover-usage--oc-back oc)))))))))

(defun drover-usage--oc-look (&optional delay)
  "Look at OpenCode's database for new usage after DELAY seconds, or soon."
  (when-let* ((oc (drover-usage--oc-current)))
    (if (drover-usage--oc-busy oc)
        (setf (drover-usage--oc-again oc) t)
      (setf (drover-usage--oc-busy oc) t)
      (setf (drover-usage--oc-timer oc)
            (run-at-time (or delay 0.05) nil #'drover-usage--oc-work oc)))))

(defun drover-usage--oc-sessions-changed ()
  "Look at OpenCode's database soon when OpenCode's sessions changed.
On `drover-changed-hook'."
  (when (and drover-usage--started
             (or (eq drover-changed-sessions t)
                 (seq-some (lambda (key) (eq (car-safe key) 'opencode)) drover-changed-sessions)))
    (drover-usage--oc-look 2)))

;;;; Starting

(defun drover-usage-start ()
  "Start reading the usage engines record, unless started.
Their indexes read what changed since kept, in slices, and follow the
files from then on; OpenCode's database is read when its sessions
change."
  (unless drover-usage--started
    (setq drover-usage--started t)
    (setq drover-usage--claude-index (drover-usage--make-claude-index)
          drover-usage--codex-index (drover-usage--make-codex-index)
          drover-usage--oc (drover-usage--oc-make))
    (drover-transcript-start drover-usage--claude-index)
    (drover-transcript-start drover-usage--codex-index)
    (drover-usage--oc-look)
    (add-hook 'drover-changed-hook #'drover-usage--oc-sessions-changed)
    (add-hook 'kill-emacs-hook #'drover-usage--save-all)))

(defun drover-usage-stop ()
  "Stop reading the usage engines record, and keep what was read."
  (when drover-usage--started
    (setq drover-usage--started nil)
    (remove-hook 'drover-changed-hook #'drover-usage--oc-sessions-changed)
    (remove-hook 'kill-emacs-hook #'drover-usage--save-all)
    (dolist (index (list drover-usage--claude-index drover-usage--codex-index))
      (when (and index (drover-transcript-index-current-p index))
        (drover-transcript-stop index)))
    (when-let* ((oc (drover-usage--oc-current)))
      (when (timerp (drover-usage--oc-timer oc))
        (cancel-timer (drover-usage--oc-timer oc)))
      (drover-usage--oc-save oc t))
    (setq drover-usage--claude-index nil
          drover-usage--codex-index nil
          drover-usage--oc nil)))

(defun drover-usage--save-all ()
  "Keep what was read of the usage, as Emacs exits."
  (drover-with-reporting "keeping the usage"
    (when-let* ((oc (drover-usage--oc-current)))
      (drover-usage--oc-save oc t))))

(defvar drover-usage--starting nil
  "Timer starting the usage's sources, or nil.")

(defun drover-usage--ensure ()
  "Start the usage's sources soon, from a timer, unless started."
  (unless (or drover-usage--started (timerp drover-usage--starting))
    (setq drover-usage--starting
          (run-at-time 0 nil (lambda ()
                               (drover-with-reporting "usage"
                                 (setq drover-usage--starting nil)
                                 (drover-usage-start)))))))

(defun drover-usage-reading-p ()
  "Return non-nil while a source still reads usage it had not read."
  (or (seq-some (lambda (index)
                  (and index (drover-transcript-index-current-p index)
                       (or (drover-transcript-index-queue index)
                           (drover-transcript-index-later index))))
                (list drover-usage--claude-index drover-usage--codex-index))
      (when-let* ((oc (drover-usage--oc-current)))
        (or (drover-usage--oc-busy oc) (consp (drover-usage--oc-back oc))))))

(defun drover-usage-scan ()
  "Read now whatever changed of the usage, for commands and tests.
Every file of the indexes is checked, and OpenCode's database read to
its end."
  (drover-usage-start)
  (dolist (index (list (drover-usage--claude-current) (drover-usage--codex-current)))
    (when index
      ;; Two looks: subagents found in the first are read in the second.
      (drover-transcript-scan index)
      (drover-transcript-scan index)))
  (when-let* ((oc (drover-usage--oc-current)))
    (when (timerp (drover-usage--oc-timer oc))
      (cancel-timer (drover-usage--oc-timer oc)))
    (setf (drover-usage--oc-busy oc) t)
    (let ((drover-usage-oc-slice 3600))
      (drover-usage--oc-work oc))))

;;;; Facts

;; Every source's usage becomes facts: the tokens one session used on
;; one day through one provider and model, with the cost its engine
;; recorded, if any.  Facts are made once per change of the usage, and
;; every total is a sum of them.

(cl-defstruct (drover-usage--fact (:constructor drover-usage--fact-make)
                                  (:copier nil))
  "The tokens a session used on a day, through a provider and a model."
  (host nil :documentation "The host it ran on, nil for this one.")
  (harness nil :documentation "The engine recording it, such as `opencode'.")
  (provider nil :documentation "The provider's identifier, a string.")
  (model nil :documentation "The model's identifier, a string.")
  (directory nil :documentation "Where its session ran, of this host, or nil.")
  (project nil :documentation "The project of another host's, or nil.")
  (id nil :documentation "The session's identifier for its engine.
A subagent of Claude Code's is CONVERSATION/AGENT.")
  (day nil :documentation "The day, a number YYYYMMDD.")
  (input 0 :documentation "Tokens of prompts neither read from nor written into a cache.")
  (output 0 :documentation "Tokens generated, reasoning included.")
  (read 0 :documentation "Tokens read from a cache.")
  (write 0 :documentation "Tokens written into a cache.")
  (hour 0 :documentation "Tokens of WRITE written into a cache of an hour.")
  (cost nil :documentation "What it cost as its engine recorded it, or as its host priced it; nil when not.")
  (billing nil :documentation "How another host found it billed, `priced' or `included', or nil for this one's.")
  (source nil :documentation "Where another host's cost comes from, a line, or nil."))

(cl-defstruct (drover-usage--facts (:constructor drover-usage--facts-make)
                                   (:copier nil))
  "The facts of the usage known, made for one generation."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-usage--facts 'drover-usage--fact)
         :documentation "The layouts of the types it was made of.")
  (generation nil :documentation "The value of `drover-usage--generation' it was made at.")
  (facts nil :documentation "This host's facts.")
  (parents nil :documentation "Parents of sessions, by (ENGINE . ID), a hash table.")
  (titles nil :documentation "Titles the sources tell of sessions, by (ENGINE . ID), a hash table.")
  (totals nil :documentation "Totals of this host's sessions, by (ENGINE . ID), or nil until asked.
Each session's totals hold its subagents'."))

(drover-note-layout 'drover-usage--fact 'drover-usage--facts)

(defvar drover-usage--facts nil
  "The facts of the usage, as last made.")

(defun drover-usage--add-entries (table key-prefix entries)
  "Add ENTRIES, of one session, to TABLE, by KEY-PREFIX and their day and model.
TABLE maps (KEY-PREFIX DAY MODEL) to a vector of counts [INPUT OUTPUT
READ WRITE HOUR]."
  (dolist (entry entries)
    (let* ((key (list key-prefix (aref entry 1) (aref entry 2)))
           (counts (or (gethash key table) (puthash key (make-vector 5 0) table))))
      (dotimes (index 5)
        (aset counts index (+ (aref counts index) (aref entry (+ 3 index))))))))

(defun drover-usage--transcript-facts (index harness provider-of parent-of directory-of id-of)
  "Return the facts of the transcripts of INDEX, recorded by HARNESS.
Each file's data gives its provider, parent, directory and session's
identifier through PROVIDER-OF, PARENT-OF, DIRECTORY-OF and ID-OF.  A
request two files record counts once, for the file whose first request
came first, of two such the one whose session is named first.  Return
\(FACTS . PARENTS), PARENTS an alist (ID . PARENT)."
  (let ((files nil)
        (seen (make-hash-table :test #'equal))
        (table (make-hash-table :test #'equal))
        (where (make-hash-table :test #'equal))
        (parents nil))
    (when index
      (drover-transcript-map (lambda (_file data) (push data files)) index)
      ;; Of two files beginning alike, as a fork from the start, the
      ;; one named first counts, whatever order they were read in.
      (setq files (sort files :key (lambda (data) (list (or (plist-get data :first) 0)
                                                        (or (funcall id-of data) "")))
                        :in-place t))
      (dolist (data files)
        (when-let* ((id (funcall id-of data)))
          (when-let* ((parent (funcall parent-of data)))
            (push (cons id parent) parents))
          (let ((kept nil))
            (dolist (entry (plist-get data :entries))
              (unless (gethash (aref entry 0) seen)
                (puthash (aref entry 0) t seen)
                (push entry kept)))
            (when kept
              (let ((prefix (list id (funcall provider-of data))))
                (puthash prefix (funcall directory-of data) where)
                (drover-usage--add-entries table prefix kept)))))))
    (let ((facts nil))
      (maphash (lambda (key counts)
                 (pcase-let ((`((,id ,provider) ,day ,model) key))
                   (push (drover-usage--fact-make
                          :harness harness :provider provider :model model
                          :directory (gethash (car key) where) :id id :day day
                          :input (aref counts 0) :output (aref counts 1) :read (aref counts 2)
                          :write (aref counts 3) :hour (aref counts 4))
                         facts)))
               table)
      (cons facts parents))))

(defun drover-usage--claude-provider ()
  "Return the provider Claude Code's requests go to, as a string.
A gateway of the CLI's environment or settings, else Anthropic."
  (if (drover-usage--claude-gateway-p) "gateway" "anthropic"))

(defun drover-usage--oc-facts (oc)
  "Return (FACTS PARENTS TITLES) of what was read of OpenCode's usage in OC."
  (let ((facts nil)
        (parents nil)
        (titles nil))
    (when oc
      (cl-flet ((add (id session row)
                  (push (drover-usage--fact-make
                         :harness 'opencode :provider (aref row 1) :model (aref row 2)
                         :directory (nth 1 session) :id id :day (aref row 0)
                         :input (aref row 3) :output (aref row 4) :read (aref row 5)
                         :write (aref row 6) :cost (aref row 7))
                        facts)))
        (maphash (lambda (id session)
                   (when (nth 0 session) (push (cons id (nth 0 session)) parents))
                   (when (nth 2 session) (push (cons id (nth 2 session)) titles))
                   (dolist (row (nth 3 session))
                     (add id session row)))
                 (drover-usage--oc-sessions oc))
        (pcase-dolist (`(,_message . ,step) (drover-usage--oc-live oc))
          (add (car step) (gethash (car step) (drover-usage--oc-sessions oc))
               (apply #'vector (nthcdr 3 step))))))
    (list facts parents titles)))

(defun drover-usage--make-facts ()
  "Return this host's facts as the sources read them now."
  (let* ((parents (make-hash-table :test #'equal))
         (titles (make-hash-table :test #'equal))
         (claude-provider (drover-usage--claude-provider))
         (claude (drover-usage--transcript-facts
                  (drover-usage--claude-current) 'claude-code
                  (lambda (_data) claude-provider)
                  (lambda (data) (and (plist-get data :agent) (plist-get data :conversation)))
                  (lambda (data) (plist-get data :directory))
                  #'drover-usage--claude-key))
         (codex (drover-usage--transcript-facts
                 (drover-usage--codex-current) 'codex
                 (lambda (data) (or (plist-get data :provider) "openai"))
                 (lambda (data) (plist-get data :parent))
                 (lambda (data) (plist-get data :directory))
                 (lambda (data) (plist-get data :id))))
         (oc (drover-usage--oc-facts (drover-usage--oc-current))))
    (pcase-dolist (`(,engine . ,alist) (list (cons 'claude-code (cdr claude)) (cons 'codex (cdr codex))
                                             (cons 'opencode (nth 1 oc))))
      (pcase-dolist (`(,id . ,parent) alist)
        (puthash (cons engine id) parent parents)))
    (pcase-dolist (`(,id . ,title) (nth 2 oc))
      (puthash (cons 'opencode id) title titles))
    (drover-usage--facts-make :generation drover-usage--generation
                              :facts (append (car claude) (car codex) (car oc))
                              :parents parents :titles titles)))

(defun drover-usage--current-facts ()
  "Return the facts of this host's usage, made anew once it changed."
  (let ((facts drover-usage--facts))
    (unless (and (drover-layout-current-p facts 'drover-usage--facts)
                 (eql (drover-usage--facts-generation facts) drover-usage--generation))
      (setq facts (drover-usage--make-facts)
            drover-usage--facts facts))
    facts))

(defun drover-usage--root-in (parents engine id)
  "Return the top of the tree of session ID of ENGINE, PARENTS by (ENGINE . ID).
A cycle of parents ends where it closes."
  (let ((seen nil)
        (parent nil))
    (while (and id parents (not (member id seen))
                (setq parent (gethash (cons engine id) parents)))
      (push id seen)
      (setq id parent))
    id))

;;;; Billing and prices

(defvar drover-usage--logins (make-hash-table :test #'equal)
  "How each engine's CLI logs in, by engine, as (STAMP . INCLUDED).
STAMP is what it was found from, the modification times of the files
read.")

(defun drover-usage--claude-gateway-p ()
  "Return non-nil when Claude Code's requests go elsewhere than to its account.
An API key, a token or a base URL of its environment or settings, or a
program giving it a key."
  (let ((names '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
                 "CLAUDE_CODE_USE_BEDROCK" "CLAUDE_CODE_USE_VERTEX")))
    (or (seq-some (lambda (name) (let ((value (getenv name))) (and value (not (string-empty-p value)))))
                  names)
        (let ((settings (drover-read-json-cached (expand-file-name "settings.json" drover-claude-code-home))))
          (and (drover-json-object-p settings)
               (or (alist-get 'apiKeyHelper settings)
                   (let ((env (alist-get 'env settings)))
                     (and (drover-json-object-p env)
                          (seq-some (lambda (name) (alist-get (intern name) env)) names))))
               t)))))

(defun drover-usage--claude-config-file ()
  "Return the file of Claude Code's global configuration, .claude.json."
  (if (getenv "CLAUDE_CONFIG_DIR")
      (expand-file-name ".claude.json" drover-claude-code-home)
    (expand-file-name ".claude.json" (file-name-directory (directory-file-name drover-claude-code-home)))))

(defun drover-usage--login-included-p (engine)
  "Return non-nil when the CLI of ENGINE logs in through a subscription.
Claude Code, through the account its configuration names, without a
key nor a gateway (see `drover-usage--claude-gateway-p'); Codex,
through ChatGPT.  Of the files read, only that is kept."
  (let* ((file (pcase engine
                 ('claude-code (drover-usage--claude-config-file))
                 ('codex (expand-file-name "auth.json" drover-codex-home))))
         (stamp (and file (drover-file-mtime file)))
         (known (gethash engine drover-usage--logins)))
    (if (and known (equal (car known) stamp))
        (and (cdr known) (not (and (eq engine 'claude-code) (drover-usage--claude-gateway-p))))
      (let ((included
             (and stamp
                  (pcase engine
                    ('claude-code
                     (let ((json (drover-read-json file)))
                       (and (drover-json-object-p json)
                            (drover-json-object-p (alist-get 'oauthAccount json))
                            t)))
                    ('codex
                     (let ((json (drover-read-json file)))
                       (and (drover-json-object-p json)
                            (equal (alist-get 'auth_mode json) "chatgpt")
                            (member (or (cdr (assoc "model_provider" (drover-codex-config))) "openai")
                                    '("openai"))
                            t)))))))
        (puthash engine (cons stamp included) drover-usage--logins)
        (and included (not (and (eq engine 'claude-code) (drover-usage--claude-gateway-p))))))))

(defun drover-usage-billing (harness provider)
  "Return how the usage of HARNESS through PROVIDER is billed.
`included' or `priced', as `drover-usage-billing' tells."
  (let ((rule (seq-find (pcase-lambda (`(,engine ,regexp ,_))
                          (and (or (null engine) (eq engine harness))
                               (or (null regexp) (and (stringp provider) (string-match-p regexp provider)))))
                        drover-usage-billing)))
    (pcase (nth 2 rule)
      ('included 'included)
      ('auto (if (drover-usage--login-included-p harness) 'included 'priced))
      (_ 'priced))))

(defun drover-usage--normal-model (model)
  "Return MODEL's identifier compared with catalogs': downcased, dots as dashes.
A date it ends in, as Claude's identifiers, is left out."
  (let ((model (replace-regexp-in-string "\\." "-" (downcase model))))
    (replace-regexp-in-string "-20[0-9]\\{6\\}\\'" "" (replace-regexp-in-string "\\[.*\\]\\'" "" model))))

(defvar drover-usage--catalog nil
  "OpenCode's model catalog as last found, and its prices by model.
As (CATALOG . TABLE).")

(defun drover-usage--opencode-catalog ()
  "Return OpenCode's model catalog, cached, or nil, fetched for next time.
That of the local service, for the directory of the session of OpenCode
read last, else of the home directory."
  (when (and (featurep 'opencode-session) (fboundp 'drover-opencode--call-local))
    (ignore-errors
      (drover-opencode--call-local
       (lambda ()
         (let ((directory (expand-file-name "~/")))
           (or (opencode-catalog 'model directory)
               (opencode-catalog-ensure 'model directory))))))))

(defun drover-usage--catalog-prices (catalog)
  "Return the prices CATALOG, OpenCode's models, gives, by normalized model.
A table of (PROVIDER . PRICES), the provider Anthropic's or OpenAI's
first, then the others in the catalog's order."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (info (and (listp catalog) catalog))
      (when (drover-json-object-p info)
        (let* ((costs (alist-get 'cost info))
               (base (and (listp costs)
                          (seq-find (lambda (cost) (and (drover-json-object-p cost) (null (alist-get 'tier cost))))
                                    costs)))
               (provider (or (drover-usage--name (alist-get 'providerID info)) ""))
               (id (or (alist-get 'modelID info) (alist-get 'id info))))
          (when (and base (stringp id) (numberp (alist-get 'input base)) (numberp (alist-get 'output base)))
            (let* ((cache (alist-get 'cache base))
                   (prices (list :input (alist-get 'input base) :output (alist-get 'output base)
                                 :cache-read (or (and (drover-json-object-p cache) (drover-usage--money (alist-get 'read cache))) 0)
                                 :cache-write (or (and (drover-json-object-p cache) (drover-usage--money (alist-get 'write cache))) 0)))
                   (key (drover-usage--normal-model id))
                   (entries (gethash key table))
                   (first (string-match-p "\\`\\(?:anthropic\\|openai\\)\\'" provider)))
              (puthash key (if first (cons (cons provider prices) entries)
                             (append entries (list (cons provider prices))))
                       table))))))
    table))

(defun drover-usage-price (harness model)
  "Return (PRICES . SOURCE) of MODEL, run through HARNESS, or nil.
PRICES is a plist as `drover-usage-prices' gives; SOURCE tells where
from, a line: that option, OpenCode's catalog and the provider pricing
it there, or drover's own table."
  (ignore harness)
  (let ((id (downcase (or model ""))))
    (or (when-let* ((entry (seq-find (lambda (entry) (string-match-p (car entry) id)) drover-usage-prices)))
          (cons (cdr entry) "your prices"))
        (when-let* ((catalog (drover-usage--opencode-catalog)))
          (unless (eq (car drover-usage--catalog) catalog)
            (setq drover-usage--catalog (cons catalog (drover-usage--catalog-prices catalog)))
            ;; Totals priced before it arrived are priced again.
            (cl-incf drover-usage--generation))
          (when-let* ((entry (car (gethash (drover-usage--normal-model id) (cdr drover-usage--catalog)))))
            (cons (cdr entry) (format "OpenCode's catalog, %s" (car entry)))))
        (when-let* ((entry (seq-find (lambda (entry) (string-match-p (car entry) id))
                                     drover-usage-default-prices)))
          (cons (cdr entry) "drover's estimate")))))

(defun drover-usage--priced (prices input output read write hour)
  "Return what INPUT, OUTPUT, READ, WRITE and HOUR tokens cost at PRICES."
  (let ((in (or (plist-get prices :input) 0)))
    (/ (+ (* in input)
          (* (or (plist-get prices :output) 0) output)
          (* (or (plist-get prices :cache-read) 0) read)
          (* (or (plist-get prices :cache-write) 0) (- write hour))
          (* (or (plist-get prices :cache-write-1h) (* 2 in)) hour))
       1e6)))

;;;; Totals

(cl-defstruct (drover-usage-sum (:constructor drover-usage-sum-make)
                                (:copier drover-usage-sum-copy))
  "Totals of usage."
  (input 0 :documentation "Tokens of prompts neither read from nor written into a cache.")
  (output 0 :documentation "Tokens generated.")
  (read 0 :documentation "Tokens read from a cache.")
  (write 0 :documentation "Tokens written into a cache.")
  (cost 0 :documentation "What the priced usage cost, in dollars.")
  (priced nil :documentation "Non-nil when some of it was priced.")
  (included nil :documentation "Non-nil when a subscription included some of it.")
  (unpriced nil :documentation "Non-nil when some priced usage had no price."))

(defun drover-usage-sum-tokens (sum)
  "Return the tokens SUM counts, all of them."
  (+ (drover-usage-sum-input sum) (drover-usage-sum-output sum)
     (drover-usage-sum-read sum) (drover-usage-sum-write sum)))

(defun drover-usage--add (sum other)
  "Add the totals OTHER to SUM, and return SUM."
  (cl-incf (drover-usage-sum-input sum) (drover-usage-sum-input other))
  (cl-incf (drover-usage-sum-output sum) (drover-usage-sum-output other))
  (cl-incf (drover-usage-sum-read sum) (drover-usage-sum-read other))
  (cl-incf (drover-usage-sum-write sum) (drover-usage-sum-write other))
  (cl-incf (drover-usage-sum-cost sum) (drover-usage-sum-cost other))
  (when (drover-usage-sum-priced other) (setf (drover-usage-sum-priced sum) t))
  (when (drover-usage-sum-included other) (setf (drover-usage-sum-included sum) t))
  (when (drover-usage-sum-unpriced other) (setf (drover-usage-sum-unpriced sum) t))
  sum)

(defun drover-usage--pricer ()
  "Return a function of a fact returning (BILLING COST SOURCE).
BILLING is `priced' or `included'; COST, in dollars, nil when included
or without a price; SOURCE tells where the cost comes from.  Prices are
looked up once per harness, provider and model."
  (let ((memo (make-hash-table :test #'equal)))
    (lambda (fact)
      (if (drover-usage--fact-billing fact)
          (list (drover-usage--fact-billing fact) (drover-usage--fact-cost fact)
                (drover-usage--fact-source fact))
        (let* ((harness (drover-usage--fact-harness fact))
               (key (list harness (drover-usage--fact-provider fact) (drover-usage--fact-model fact)))
               (how (with-memoization (gethash key memo)
                      (let ((billing (drover-usage-billing harness (drover-usage--fact-provider fact))))
                        (cond ((eq billing 'included) (list 'included nil "included"))
                              ((drover-usage--fact-cost fact) (list 'priced 'recorded "recorded by OpenCode"))
                              (t (let ((price (drover-usage-price harness (drover-usage--fact-model fact))))
                                   (list 'priced (car price) (or (cdr price) "no price")))))))))
          (list (nth 0 how)
                (pcase (nth 1 how)
                  ('nil nil)
                  ('recorded (drover-usage--fact-cost fact))
                  (prices (drover-usage--priced prices (drover-usage--fact-input fact)
                                                (drover-usage--fact-output fact)
                                                (drover-usage--fact-read fact)
                                                (drover-usage--fact-write fact)
                                                (drover-usage--fact-hour fact))))
                (nth 2 how)))))))

(defun drover-usage--fact-sum (fact pricer)
  "Return the totals of FACT, priced by PRICER (see `drover-usage--pricer')."
  (pcase-let ((`(,billing ,cost ,_source) (funcall pricer fact)))
    (drover-usage-sum-make :input (drover-usage--fact-input fact) :output (drover-usage--fact-output fact)
                           :read (drover-usage--fact-read fact) :write (drover-usage--fact-write fact)
                           :cost (or cost 0)
                           :priced (eq billing 'priced)
                           :included (eq billing 'included)
                           :unpriced (and (eq billing 'priced) (null cost)))))

(defun drover-usage-session-totals (session)
  "Return the totals of SESSION's usage ever, its subagents' included, or nil.
A `drover-usage-sum', nil when none is known yet.  A session of another
host is totalled by its host, which sends them as asked."
  (when-let* ((key (drover-usage--session-key session)))
    (if-let* ((host (drover-session-host session)))
        (drover-usage--remote-totals host key)
      (let ((facts (drover-usage--current-facts)))
        (gethash key (or (drover-usage--facts-totals facts)
                         (setf (drover-usage--facts-totals facts) (drover-usage--session-table facts))))))))

(defun drover-usage--session-table (facts)
  "Return the totals of every session of FACTS, by (ENGINE . ID).
Each session's totals hold those of its subagents, at any depth."
  (let ((table (make-hash-table :test #'equal))
        (pricer (drover-usage--pricer))
        (parents (drover-usage--facts-parents facts)))
    (dolist (fact (drover-usage--facts-facts facts))
      (let* ((engine (drover-usage--fact-harness fact))
             (id (drover-usage--fact-id fact))
             (sum (drover-usage--fact-sum fact pricer))
             (seen nil))
        (while (and id (not (member id seen)))
          (push id seen)
          (drover-usage--add (or (gethash (cons engine id) table)
                                 (puthash (cons engine id) (drover-usage-sum-make) table))
                             sum)
          (setq id (gethash (cons engine id) parents)))))
    table))

(defun drover-usage--session-key (session)
  "Return (ENGINE . ID) naming the usage of SESSION, or nil.
Claude Code's subagents are named CONVERSATION/AGENT, as its backend
names them."
  (let ((engine (drover-session-engine session))
        (id (or (drover-session-engine-id session)
                (let ((id (drover-session-id session)))
                  (and (eq (drover-session-backend session) 'claude-code) (stringp id) id)))))
    (when (and engine (stringp id))
      (cons engine id))))

;;;; Formatting

(defun drover-usage-format-cost (sum)
  "Return the cost of SUM as it reads, such as \"$1.23\" or \"included\"."
  (let ((cost (drover-usage-sum-cost sum))
        (priced (drover-usage-sum-priced sum))
        (included (drover-usage-sum-included sum)))
    (concat
     (cond ((not priced) nil)
           ((>= cost 100) (format "$%d" (round cost)))
           ((>= cost 0.995) (format "$%.2f" cost))
           ((> cost 0) (format "$%.2f" (max cost 0.01)))
           (t "$0"))
     (and (drover-usage-sum-unpriced sum) "?")
     (and priced included " + ")
     (and included "included"))))

(defun drover-usage-format-short (sum)
  "Return the cost of SUM in a few characters, for a column."
  (if (drover-usage-sum-priced sum)
      (concat (string-remove-suffix " + included" (drover-usage-format-cost sum))
              (and (drover-usage-sum-included sum) "+"))
    "incl."))

(defun drover-usage-format-tokens (tokens)
  "Return TOKENS, a count, compact, such as 12.3k."
  (if (fboundp 'opencode-format-tokens)
      (opencode-format-tokens tokens)
    (cond ((>= tokens 1000000) (format "%.1fM" (/ tokens 1e6)))
          ((>= tokens 1000) (format "%.1fk" (/ tokens 1e3)))
          (t (format "%d" tokens)))))

(defun drover-usage-format-sum (sum)
  "Return SUM on one line: its cost and its tokens of each kind."
  (format "%s · %s in, %s out, %s cache read, %s cache write"
          (drover-usage-format-cost sum)
          (drover-usage-format-tokens (drover-usage-sum-input sum))
          (drover-usage-format-tokens (drover-usage-sum-output sum))
          (drover-usage-format-tokens (drover-usage-sum-read sum))
          (drover-usage-format-tokens (drover-usage-sum-write sum))))

(defun drover-usage-session-text (session form)
  "Return the usage of SESSION as FORM shows it, or nil.
FORM is `column', its cost in a few characters, `details', its cost
and tokens, both strings, or `value', its totals, a `drover-usage-sum'
\(see `drover-list-usage-function').  The sources start once asked, and
views are told of the totals as they come (see `drover-views-changed')."
  (drover-usage--ensure)
  (when drover-usage--started
    (let ((sum (drover-usage-session-totals session)))
      (pcase form
        ('value sum)
        ('column (and sum (drover-usage-format-short sum)))
        (_ (cond (sum (drover-usage-format-sum sum))
                 ((drover-usage-reading-p) "reading…")))))))

;;;; Telling views

(defun drover-usage--notify ()
  "Tell views of the sessions whose usage changed, and run the change hook.
From a timer.  The sessions are those of the conversations changed and
the parents above them, whose totals hold theirs."
  (drover-with-reporting "usage changes"
    (setq drover-usage--notify-timer nil)
    (let ((changed (delete-dups drover-usage--changed)))
      (setq drover-usage--changed nil)
      (when (and changed (fboundp 'drover-views-changed) drover--index)
        (let* ((facts (drover-usage--current-facts))
               (wanted (make-hash-table :test #'equal))
               (keys nil))
          (dolist (key changed)
            (let ((engine (car key))
                  (id (cdr key))
                  (seen nil))
              (while (and id (not (member id seen)))
                (push id seen)
                (puthash (cons engine id) t wanted)
                (setq id (gethash (cons engine id) (drover-usage--facts-parents facts))))))
          (dolist (session (drover-index-sessions))
            (let ((key (and (not (drover-session-host session)) (drover-usage--session-key session))))
              (when (and key (gethash key wanted))
                (push (drover-session-key session) keys))))
          (drover-views-changed keys))))
    (drover-run-hook-reporting 'drover-usage-changed-hook)))

;;;; Other hosts

;; Each host totals its own usage, priced as it prices it, and sends
;; its totals to a peer asking (`drover-remote', its "usage" request):
;; rows of the dimensions the peer asks for, summed over the others,
;; `drover-usage-remote-rows' at most.  What a peer sends is checked and
;; bounded as it comes, and kept in memory only, as last received.

(defconst drover-usage-dimensions '("session" "day" "harness" "provider" "model" "project")
  "The dimensions a peer may ask a host's totals by.")

(defun drover-usage--project (fact)
  "Return the project of FACT, a name, or nil."
  (or (drover-usage--fact-project fact)
      (when-let* ((directory (drover-usage--fact-directory fact)))
        (or (drover-transcript-project directory)
            (abbreviate-file-name (directory-file-name directory))))))

(defun drover-usage--in-period-p (fact start)
  "Return non-nil when FACT is of a day from START, a day, or START is nil."
  (or (null start)
      (let ((day (drover-usage--fact-day fact)))
        (and day (>= day start)))))

(defun drover-usage--wire-rows (period by)
  "Return this host's totals of PERIOD by the dimensions BY, as they travel.
A list of vectors [ENGINE ID PARENT DAY PROVIDER MODEL PROJECT INPUT
OUTPUT READ WRITE COST BILLING SOURCE TITLE], the dimensions BY leaves
out empty, COST nil when unknown, and the number of rows left out."
  (let* ((facts (drover-usage--current-facts))
         (start (drover-usage-period-start period))
         (pricer (drover-usage--pricer))
         (table (make-hash-table :test #'equal))
         (titles (drover-usage--titles facts)))
    (dolist (fact (drover-usage--facts-facts facts))
      (when (drover-usage--in-period-p fact start)
        (pcase-let* ((`(,billing ,cost ,source) (funcall pricer fact))
                     (engine (drover-usage--fact-harness fact))
                     (session (member "session" by))
                     (id (and session (drover-usage--fact-id fact)))
                     (key (list (and (or session (member "harness" by)) engine)
                                id
                                (and (member "day" by) (drover-usage--fact-day fact))
                                (and (member "provider" by) (drover-usage--fact-provider fact))
                                (and (member "model" by) (drover-usage--fact-model fact))
                                (and (member "project" by) (drover-usage--project fact))
                                billing source))
                     (row (or (gethash key table)
                              (puthash key (vector (and (car key) (symbol-name (car key)))
                                                   (or id "")
                                                   (or (and id (gethash (cons engine id)
                                                                        (drover-usage--facts-parents facts)))
                                                       "")
                                                   (or (nth 2 key) 0) (or (nth 3 key) "") (or (nth 4 key) "")
                                                   (or (nth 5 key) "")
                                                   0 0 0 0 nil (symbol-name billing) source
                                                   (or (and id (gethash (cons engine id) titles)) ""))
                                       table))))
          (cl-incf (aref row 7) (drover-usage--fact-input fact))
          (cl-incf (aref row 8) (drover-usage--fact-output fact))
          (cl-incf (aref row 9) (drover-usage--fact-read fact))
          (cl-incf (aref row 10) (drover-usage--fact-write fact))
          (when cost
            (aset row 11 (+ (or (aref row 11) 0) cost))))))
    (let ((rows (sort (hash-table-values table)
                      :key (lambda (row) (- (+ (aref row 7) (aref row 8) (aref row 9) (aref row 10))))
                      :in-place t)))
      (cons (seq-take rows drover-usage-remote-rows)
            (max 0 (- (length rows) drover-usage-remote-rows))))))

(defun drover-usage-remote-answer (message)
  "Return this host's answer to MESSAGE, a peer's request of its usage.
MESSAGE tells the :period, a string among today, week, month and all,
and :by, a vector of `drover-usage-dimensions'.  The reply is a plist
of :rows, as `drover-usage--wire-rows' makes them, the cost nil as
false, :more, the rows left out, and :reading, true while the sources
still read usage not read before, which a peer then asks for again."
  (let ((period (pcase (plist-get message :period)
                  ("today" 'today) ("week" 'week) ("month" 'month) (_ 'all)))
        (by (seq-filter (lambda (dimension) (member dimension drover-usage-dimensions))
                        (seq-take (let ((by (plist-get message :by))) (if (vectorp by) by [])) 8))))
    (drover-usage-start)
    (pcase-let ((`(,rows . ,more) (drover-usage--wire-rows period by)))
      (list :rows (vconcat (mapcar (lambda (row)
                                     (let ((row (copy-sequence row)))
                                       (dotimes (index (length row))
                                         (unless (aref row index) (aset row index :false)))
                                       row))
                                   rows))
            :more more
            :reading (if (drover-usage-reading-p) t :false)))))

(defun drover-usage--wire-string (value)
  "Return VALUE, a string received, bounded, or nil when empty or not one."
  (and (stringp value) (not (string-empty-p value)) (drover-usage--name value)))

(defun drover-usage--wire-engine (value)
  "Return the engine VALUE, a string received, names, or nil.
Only short names of lowercase letters, digits and dashes are interned."
  (and (stringp value) (string-match-p "\\`[a-z][a-z0-9-]\\{0,31\\}\\'" value) (intern value)))

(defun drover-usage-decode-row (row host)
  "Return the fact the row ROW of HOST's totals tells, or nil.
Every value is checked: what does not fit is left out, or the row."
  (when (and (vectorp row) (>= (length row) 15))
    (let ((engine (drover-usage--wire-engine (aref row 0)))
          (day (aref row 3))
          (counts (mapcar (lambda (index) (let ((value (aref row index)))
                                            (and (natnump value) (< value 1e15) value)))
                          '(7 8 9 10)))
          (billing (pcase (aref row 12) ("included" 'included) ("priced" 'priced))))
      (when (and billing (seq-every-p #'identity counts))
        (drover-usage--fact-make
         :host host :harness engine
         :id (drover-usage--wire-string (aref row 1))
         :day (and (natnump day) (<= 19000101 day 29991231) day)
         :provider (drover-usage--wire-string (aref row 4))
         :model (drover-usage--wire-string (aref row 5))
         :project (drover-usage--wire-string (aref row 6))
         :input (nth 0 counts) :output (nth 1 counts) :read (nth 2 counts) :write (nth 3 counts)
         :cost (and (eq billing 'priced) (drover-usage--money (aref row 11)))
         :billing billing
         :source (or (drover-usage--wire-string (aref row 13)) "its host"))))))

(defvar drover-usage--remote (make-hash-table :test #'equal)
  "What peers sent of their totals, by (HOST . ASKED).
ASKED is the period asked, or `sessions' for each session's totals
ever; each entry a plist of :time, when received, :facts, :parents and
:titles, by (ENGINE . ID), :more, rows left out, :reading, :error, and
:pending, while asked.")

(defvar drover-usage--remote-generation 0
  "Count of the totals received from peers.")

(defun drover-usage--remote-hosts ()
  "Return the peers connected whose drover sends its usage, by name."
  (and (fboundp 'drover-remote-usage-peers)
       (drover-with-reporting "usage of other hosts"
         (drover-remote-usage-peers))))

(defun drover-usage--remote-ask (host asked)
  "Ask HOST for its totals ASKED, a period or `sessions', unless current.
They are current when received less than `drover-usage-remote-interval'
seconds ago, or asked meanwhile; a host still reading its own is asked
again sooner."
  (let* ((key (cons host asked))
         (entry (gethash key drover-usage--remote))
         (age (- (float-time) (or (plist-get entry :time) 0))))
    (unless (or (plist-get entry :pending)
                (< age (if (plist-get entry :reading)
                           drover-usage-remote-reading-interval
                         drover-usage-remote-interval)))
      (puthash key (plist-put (copy-sequence entry) :pending t) drover-usage--remote)
      (drover-remote-ask-usage
       host (list :period (if (eq asked 'sessions) "all" (symbol-name asked))
                  :by (if (eq asked 'sessions) ["session"] (vconcat drover-usage-dimensions)))
       (lambda (reply error)
         (drover-with-reporting "usage of other hosts"
           (drover-usage--remote-received host asked reply error)))))))

(defun drover-usage--remote-received (host asked reply error)
  "Keep REPLY of HOST to the request ASKED, or the ERROR it failed with."
  (let ((key (cons host asked)))
    (if (or error (not (and (consp reply) (keywordp (car reply)))))
        (puthash key (list :time (float-time) :error (drover-bound-line (format "%s" (or error "no reply"))))
                 drover-usage--remote)
      (let ((facts nil)
            (parents (make-hash-table :test #'equal))
            (titles (make-hash-table :test #'equal))
            (rows (plist-get reply :rows)))
        (seq-doseq (row (if (vectorp rows) (seq-take rows drover-usage-remote-rows) []))
          (when-let* ((fact (drover-usage-decode-row row host)))
            (push fact facts)
            (when-let* ((engine (drover-usage--fact-harness fact))
                        (id (drover-usage--fact-id fact)))
              (when-let* ((parent (drover-usage--wire-string (aref row 2))))
                (puthash (cons engine id) parent parents))
              (when-let* ((title (drover-usage--wire-string (aref row 14))))
                (puthash (cons engine id) title titles)))))
        (puthash key (list :time (float-time) :facts facts :parents parents :titles titles
                           :more (and (natnump (plist-get reply :more)) (plist-get reply :more))
                           :reading (eq (plist-get reply :reading) t))
                 drover-usage--remote)))
    (cl-incf drover-usage--remote-generation)
    (when (and (eq asked 'sessions) (fboundp 'drover-views-changed) drover--index)
      (drover-views-changed (delq nil (mapcar (lambda (session)
                                                (and (equal (drover-session-host session) host)
                                                     (drover-session-key session)))
                                              (drover-index-sessions)))))
    (drover-run-hook-reporting 'drover-usage-changed-hook)))

(defun drover-usage--remote-totals (host key)
  "Return the totals of the session KEY, (ENGINE . ID), of HOST ever, or nil.
Its subagents' are included.  HOST is asked for them, as it totals its
sessions."
  (when (member host (drover-usage--remote-hosts))
    (drover-usage--remote-ask host 'sessions))
  (when-let* ((entry (gethash (cons host 'sessions) drover-usage--remote))
              ((plist-get entry :facts)))
    (let ((table (or (plist-get entry :totals)
                     (let ((table (make-hash-table :test #'equal))
                           (pricer (drover-usage--pricer))
                           (parents (plist-get entry :parents)))
                       (dolist (fact (plist-get entry :facts))
                         (let ((engine (drover-usage--fact-harness fact))
                               (id (drover-usage--fact-id fact))
                               (sum (drover-usage--fact-sum fact pricer))
                               (seen nil))
                           (while (and id (not (member id seen)))
                             (push id seen)
                             (drover-usage--add (or (gethash (cons engine id) table)
                                                    (puthash (cons engine id) (drover-usage-sum-make) table))
                                                sum)
                             (setq id (gethash (cons engine id) parents)))))
                       (puthash (cons host 'sessions) (plist-put entry :totals table) drover-usage--remote)
                       table))))
      (gethash key table))))

(defun drover-usage--remote-facts (period)
  "Return the facts of PERIOD the peers sent, asking those not current.
And (HOST . STATE) of each peer asked, STATE a plist of its entry."
  (let ((facts nil)
        (states nil))
    (dolist (host (drover-usage--remote-hosts))
      (drover-usage--remote-ask host period)
      (let ((entry (gethash (cons host period) drover-usage--remote)))
        (push (cons host entry) states)
        (setq facts (append (plist-get entry :facts) facts))))
    ;; Hosts not connected keep what they sent, as of then.
    (maphash (lambda (key entry)
               (when (and (equal (cdr key) period) (not (assoc (car key) states)))
                 (push (cons (car key) (plist-put (copy-sequence entry) :away t)) states)
                 (setq facts (append (plist-get entry :facts) facts))))
             drover-usage--remote)
    (cons facts (nreverse states))))

;;;; Grouping

(defconst drover-usage-grouping-labels
  '((session . "session") (project . "project") (harness . "harness") (provider . "provider")
    (model . "model") (host . "host") (day . "day"))
  "How each grouping of the usage reads.")

(defvar drover-usage-filter nil
  "Which usage `drover-usage' totals, as a plist; nil totals it all.
Its members, each nil for any: :project, :harness, :host, :model and
:provider, the values the report groups by.")

(defun drover-usage--titles (facts)
  "Return the titles of the sessions of FACTS and of the index.
A table by (ENGINE . ID)."
  (let ((titles (copy-hash-table (drover-usage--facts-titles facts))))
    (when drover--index
      (dolist (session (drover-index-sessions))
        (when-let* (((not (drover-session-host session)))
                    (key (drover-usage--session-key session))
                    (title (drover-session-title session)))
          (puthash key title titles))))
    titles))

(defun drover-usage--dimension (fact grouping context)
  "Return (VALUE . LABEL) of FACT for GROUPING, CONTEXT giving roots and titles.
CONTEXT is a plist of :facts, this host's, and :remote, the entries of
peers by host."
  (pcase grouping
    ('session
     (let* ((host (drover-usage--fact-host fact))
            (engine (drover-usage--fact-harness fact))
            (id (drover-usage--fact-id fact))
            (parents (if host
                         (plist-get (cdr (assoc host (plist-get context :remote))) :parents)
                       (drover-usage--facts-parents (plist-get context :facts))))
            (root (drover-usage--root-in parents engine id))
            (title (and root (or (gethash (cons engine root)
                                          (if host
                                              (plist-get (cdr (assoc host (plist-get context :remote))) :titles)
                                            (plist-get context :titles)))
                                 root))))
       (if root
           (cons (list host engine root) (concat title (and host (format " (%s)" (drover-host-label host)))))
         (cons nil "other sessions"))))
    ('project (let ((project (drover-usage--project fact)))
                (cons project (or project "no project"))))
    ('harness (let ((engine (drover-usage--fact-harness fact)))
                (cons engine (drover-usage--engine-label engine))))
    ('provider (let ((provider (drover-usage--fact-provider fact)))
                 (cons provider (or provider "unknown"))))
    ('model (let ((model (drover-usage--fact-model fact)))
              (cons model (or model "unknown"))))
    ('host (let ((host (drover-usage--fact-host fact)))
             (cons host (drover-host-label host))))
    ('day (let ((day (drover-usage--fact-day fact)))
            (cons day (if day
                          (format-time-string "%a %e %b %Y" (drover-usage--day-time day))
                        "some day"))))))

(defun drover-usage--engine-label (engine)
  "Return how ENGINE reads."
  (if (and engine (drover-engine-property engine :label))
      (drover-engine-label engine)
    (pcase engine
      ('opencode "OpenCode") ('claude-code "Claude Code") ('codex "Codex")
      (_ (format "%s" (or engine "other"))))))

(defun drover-usage--filtered-p (fact context)
  "Return non-nil when `drover-usage-filter' keeps FACT, in CONTEXT."
  (cl-loop for (key grouping) in '((:project project) (:harness harness) (:host host)
                                   (:model model) (:provider provider))
           always (let ((wanted (plist-get drover-usage-filter key)))
                    (or (null wanted)
                        (equal wanted (car (drover-usage--dimension fact grouping context)))))))

(defun drover-usage-report (period grouping breakdown)
  "Return the usage of PERIOD grouped by GROUPING, broken down by BREAKDOWN.
A plist of :total, a `drover-usage-sum'; :groups, a list of (VALUE
LABEL SUM ITEMS), ITEMS a list of (VALUE LABEL SUM), sorted as
`drover-usage-sort-by' tells; :sources, a list of (HARNESS PROVIDER
MODEL SOURCE SUM); and :hosts, how each peer's totals stand, as
\(HOST . ENTRY).  BREAKDOWN nil breaks nothing down."
  (let* ((facts (drover-usage--current-facts))
         (start (drover-usage-period-start period))
         (remote (drover-usage--remote-facts period))
         (context (list :facts facts :titles (drover-usage--titles facts) :remote (cdr remote)))
         (pricer (drover-usage--pricer))
         (total (drover-usage-sum-make))
         (groups (make-hash-table :test #'equal))
         (sources (make-hash-table :test #'equal)))
    (dolist (fact (append (drover-usage--facts-facts facts) (car remote)))
      (when (and (drover-usage--in-period-p fact start)
                 (or (null drover-usage-filter) (drover-usage--filtered-p fact context)))
        (pcase-let* ((sum (drover-usage--fact-sum fact pricer))
                     (`(,value . ,label) (drover-usage--dimension fact grouping context))
                     (group (or (gethash value groups)
                                (puthash value (list value label (drover-usage-sum-make)
                                                     (make-hash-table :test #'equal))
                                         groups)))
                     (source-key (list (drover-usage--fact-host fact) (drover-usage--fact-harness fact)
                                       (drover-usage--fact-provider fact) (drover-usage--fact-model fact)
                                       (nth 2 (funcall pricer fact)))))
          (drover-usage--add total sum)
          (drover-usage--add (nth 2 group) sum)
          (drover-usage--add (cdr (or (gethash source-key sources)
                                      (puthash source-key (cons source-key (drover-usage-sum-make)) sources)))
                             sum)
          (when breakdown
            (pcase-let ((`(,item . ,item-label) (drover-usage--dimension fact breakdown context)))
              (drover-usage--add (nth 2 (or (gethash item (nth 3 group))
                                            (puthash item (list item item-label (drover-usage-sum-make))
                                                     (nth 3 group))))
                                 sum))))))
    (list :total total
          :groups (drover-usage--sorted
                   (mapcar (pcase-lambda (`(,value ,label ,sum ,items))
                             (list value label sum (drover-usage--sorted (hash-table-values items))))
                           (hash-table-values groups)))
          :sources (sort (mapcar (pcase-lambda (`(,key . ,sum)) (append key (list sum)))
                                 (hash-table-values sources))
                         :key (lambda (source) (- (drover-usage-sum-tokens (nth 5 source))))
                         :in-place t)
          :hosts (cdr remote))))

(defun drover-usage--sorted (entries)
  "Return ENTRIES, lists (VALUE LABEL SUM ...), sorted.
In the order of `drover-usage-sort-by'."
  (pcase drover-usage-sort-by
    ('name (sort entries :key #'cadr :in-place t))
    ('tokens (sort entries :key (lambda (entry) (- (drover-usage-sum-tokens (nth 2 entry)))) :in-place t))
    (_ (sort entries :key (lambda (entry) (list (- (drover-usage-sum-cost (nth 2 entry)))
                                                (- (drover-usage-sum-tokens (nth 2 entry)))))
             :in-place t))))

(defun drover-usage-totals (&optional period)
  "Return the totals of the usage of PERIOD, by default today, every host's.
A `drover-usage-sum'."
  (plist-get (drover-usage-report (or period 'today) 'host nil) :total))

(provide 'drover-usage)
;;; drover-usage.el ends here
