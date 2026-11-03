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
;; (ai-title), else the first prompt; the directory, branch and
;; creation time of its first records; the model, effort and permission
;; mode of its newest turn; its last activity, the time of its newest
;; message, not of the records the CLI appends of its own as the
;; conversation is opened again.  Its subagents' transcripts beside it,
;; under ID/subagents/, are its subagents.
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
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
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
        index
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
                               (message (content . ,content))))))))
          (goto-char line-end)
          (forward-line 1)))
      found)))

(defun drover-claude-code-transcripts--member (marker key &optional beg end)
  "Return KEY of the last record with MARKER between BEG and END, as text."
  (when-let* ((record (drover-transcript-last marker nil beg end)))
    (drover-transcript-text (alist-get key record))))

(defun drover-claude-code-transcripts--turn (beg end)
  "Return (MODEL EFFORT TIME) of the newest main turn from BEG to END, or nil."
  (when-let* ((record (drover-transcript-last
                       "\"type\":\"assistant\""
                       (lambda (record)
                         (and (equal (alist-get 'type record) "assistant")
                              (not (alist-get 'isSidechain record))
                              (let ((model (or (alist-get 'requestedModel record)
                                               (alist-get 'model (alist-get 'message record)))))
                                (and (stringp model) (not (string-prefix-p "<" model))))))
                       beg end)))
    (list (or (alist-get 'requestedModel record) (alist-get 'model (alist-get 'message record)))
          (and (stringp (alist-get 'effort record)) (alist-get 'effort record))
          (drover-transcript-time (alist-get 'timestamp record)))))

(defun drover-claude-code-transcripts--message-p (record)
  "Return non-nil when RECORD is a message of the conversation, parsed.
That is a prompt, a reply or a tool's result, not what the CLI records
of its own as it opens the conversation again: a note (isMeta), a
record kept for the transcript only, and the reply it makes up for a
prompt left without one.  Its API errors are messages: they end a turn."
  (pcase (alist-get 'type record)
    ("user" (not (or (alist-get 'isMeta record) (alist-get 'queueTranscriptOnly record))))
    ("assistant" (or (alist-get 'isApiErrorMessage record)
                     (not (equal (alist-get 'model (alist-get 'message record)) "<synthetic>"))))))

(defun drover-claude-code-transcripts--activity (beg end)
  "Return the time of the newest message between BEG and END, or nil.
See `drover-claude-code-transcripts--message-p'."
  (when-let* ((record (drover-transcript-last
                       "\"timestamp\":\""
                       (lambda (record)
                         (and (drover-claude-code-transcripts--message-p record)
                              (drover-transcript-time (alist-get 'timestamp record))))
                       beg end)))
    (drover-transcript-time (alist-get 'timestamp record))))

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

(defun drover-claude-code-transcripts--read-tail (beg end)
  "Return what the records between BEG and END tell of the conversation's end.
A plist: the newest title, summary, last prompt, permission mode, model,
effort, branch, prompt's time and activity there; members not told are
left out."
  (let ((turn (drover-claude-code-transcripts--turn beg end))
        (data nil))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value)))))
      (add :custom-title (drover-claude-code-transcripts--member "\"type\":\"custom-title\"" 'customTitle beg end))
      (add :ai-title (drover-claude-code-transcripts--member "\"type\":\"ai-title\"" 'aiTitle beg end))
      (add :summary (drover-claude-code-transcripts--member "\"type\":\"summary\"" 'summary beg end))
      (add :last-prompt (drover-claude-code-transcripts--member "\"type\":\"last-prompt\"" 'lastPrompt beg end))
      (add :mode (drover-claude-code-transcripts--member "\"type\":\"permission-mode\"" 'permissionMode beg end))
      (add :model (nth 0 turn))
      (add :effort (nth 1 turn))
      (add :prompted (drover-claude-code-transcripts--prompted beg end))
      (add :updated (let ((activity (drover-claude-code-transcripts--activity beg end)))
                      (if (and activity (nth 2 turn)) (max activity (nth 2 turn)) (or activity (nth 2 turn)))))
      (add :branch (save-excursion
                     (goto-char end)
                     (drover-transcript-sniff "gitBranch" t beg)))
      (add :messages (save-excursion
                       (goto-char beg)
                       (and (re-search-forward "\"type\":\"\\(?:user\\|assistant\\)\"" end t) t))))
    data))

(defun drover-claude-code-transcripts--read-head (beg end)
  "Return what the records between BEG and END tell of the conversation's start.
A plist of its directory, creation time, entry point, branch, first
prompt, the titles there and its newest message's time; members not
told are left out."
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
      (add :ai-title (drover-claude-code-transcripts--member "\"type\":\"ai-title\"" 'aiTitle beg end))
      ;; A tail of other records leaves the head's newest message as the
      ;; last activity.
      (add :updated (drover-claude-code-transcripts--activity beg end))
      (add :messages (save-excursion
                       (goto-char beg)
                       (and (re-search-forward "\"type\":\"\\(?:user\\|assistant\\)\"" end t) t))))
    data))

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

(defun drover-claude-code-transcripts--subagent-times (transcript size)
  "Return (CREATED . UPDATED) of the subagent whose TRANSCRIPT is SIZE bytes long.
CREATED is the time of its first record, UPDATED that of its newest
message, or CREATED without one."
  (with-temp-buffer
    (let* ((tail (drover-transcript-insert-ends transcript size))
           (created (save-excursion
                      (goto-char (point-min))
                      (drover-transcript-time (drover-transcript-sniff "timestamp")))))
      (cons created (or (drover-claude-code-transcripts--last-message transcript size tail nil)
                        created)))))

(defun drover-claude-code-transcripts--subagents (file &optional old)
  "Return the subagents of the conversation transcript FILE records.
Their transcripts are beside it, under ID/subagents/, each with a
description in a .meta.json; each is a plist of :agent, its name, :title,
:type, :created and :updated, and the :size and :mtime of its transcript
when read.  OLD are the subagents read before: one whose transcript did
not change since is not read again."
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
                                                                ".meta.json")))
                                (times (drover-claude-code-transcripts--subagent-times transcript size)))
                            (list :agent agent
                                  :title (drover-transcript-text
                                          (alist-get 'description (and (drover-json-object-p meta) meta)))
                                  :type (drover-transcript-text
                                         (alist-get 'agentType (and (drover-json-object-p meta) meta)))
                                  :created (car times)
                                  :updated (cdr times)
                                  :size size
                                  :mtime mtime)))))
                    (ignore-errors
                      (directory-files-and-attributes directory t "\\`agent-.*\\.jsonl\\'" t)))))))

(defun drover-claude-code-transcripts--read (file attributes old)
  "Return what transcript FILE, of ATTRIBUTES, tells of its conversation.
OLD is the entry read before, or nil: a transcript only appended to
since, the same file grown, is read from where it ended, what its new
records tell replacing what the old ones told.  Otherwise its head and
tail are read.  Its last activity is its newest message's (see
`drover-claude-code-transcripts--message-p'), looked for further back
than the tail when the tail holds none, never the file's modification
time, which the CLI's records of its own move as it opens the
conversation again.  For the index of `drover-transcript'."
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
        (let* ((recent (drover-claude-code-transcripts--read-tail tail (point-max)))
               (updated (or (plist-get recent :updated)
                            (drover-transcript-look-before-tail
                             file size tail grown #'drover-claude-code-transcripts--activity))))
          (while recent
            (setq data (plist-put data (pop recent) (pop recent))))
          (when updated
            (setq data (plist-put data :updated updated))))
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
  "Return the title of the conversation of DATA, as the CLI's picker titles it."
  (or (plist-get data :custom-title)
      (plist-get data :ai-title)
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

(defun drover-claude-code-transcripts--status (id)
  "Return (STATUS REQUESTS DETAIL) of conversation ID, as the CLI runs it.
Idle but for a session \"claude agents\" listed as working or waiting."
  (if-let* ((agent (gethash id drover-claude-code-transcripts--agents)))
      (let ((where (if (equal (plist-get agent :kind) "background")
                       "in the background"
                     "in another terminal"))
            (state (or (plist-get agent :state) "")))
        (pcase state
          ((or "working" "running" "busy")
           (list 'working nil (concat "working " where)))
          ((or "blocked" "waiting" "needs-input" "needs_input" "input")
           (list 'waiting
                 (list (list :kind 'permission
                             :summary (format "waits %s: claude attach %s" where
                                              (or (plist-get agent :short) id))))
                 nil))
          (_ (list 'idle nil (string-trim (concat state " " where))))))
    (list 'idle nil nil)))

(defun drover-claude-code-transcripts--record (data &optional catalog)
  "Return the `drover-session' of the conversation DATA tells, or nil.
CATALOG names its model, by default the catalog the CLI caches; the
symbol `none' stands for no catalog, so that one listing many looks for
it once."
  (when (drover-claude-code-transcripts--listed-p data)
    (let* ((id (plist-get data :id))
           (status (drover-claude-code-transcripts--status id))
           (model (plist-get data :model))
           (branch (plist-get data :branch))
           (mode (plist-get data :mode)))
      (drover-session-create
       :backend 'claude-code :id id
       :engine 'claude-code :engine-id id
       :title (drover-claude-code-transcripts--title data)
       :directory (plist-get data :directory)
       :project (plist-get data :project)
       :branch (and branch (not (equal branch "HEAD")) branch)
       :status (nth 0 status) :requests (nth 1 status) :detail (nth 2 status)
       ;; A turn runs from the newest prompt, a wait from the newest message.
       :since (pcase (nth 0 status)
                ('idle nil)
                ('working (or (plist-get data :prompted) (plist-get data :updated)))
                (_ (plist-get data :updated)))
       :updated (plist-get data :updated)
       :created (plist-get data :created)
       :agent (and mode (car (rassoc mode drover-claude-code-agent-modes)))
       :model (and model (drover-claude-code-transcripts--model-name
                          model (pcase catalog
                                  ('none nil)
                                  ('nil (drover-claude-code-catalog))
                                  (_ catalog))))
       :effort (plist-get data :effort)))))

(defun drover-claude-code-transcripts--subagent-id (id agent)
  "Return the identifier of the subagent AGENT of conversation ID."
  (concat id "/" agent))

(defun drover-claude-code-transcripts--subagent-records (data)
  "Return the records of the subagents of the conversation of DATA."
  (when (drover-claude-code-transcripts--listed-p data)
    (let ((id (plist-get data :id)))
      (mapcar (lambda (subagent)
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
                 :agent (plist-get subagent :type)))
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
             (setq records (append (drover-claude-code-transcripts--subagent-records data) records)))))
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

(defun drover-claude-code-transcripts--changed (_file old new)
  "Report that what was read of a transcript went from OLD to NEW."
  (let ((id (plist-get (or new old) :id)))
    (drover-backend-changed 'claude-code id)
    (dolist (agent (delete-dups (mapcar (lambda (subagent) (plist-get subagent :agent))
                                        (append (plist-get old :subagents)
                                                (plist-get new :subagents)))))
      (drover-backend-changed 'claude-code (drover-claude-code-transcripts--subagent-id id agent)))))

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
        (when drover-show-history
          (drover-history-fetch 'claude-code)))))
  (drover-claude-code-transcripts--ask-agents))

(defun drover-claude-code-transcripts--roots ()
  "Return where Claude Code's transcripts are, for the index."
  (list (list (drover-claude-code-projects-directory) 1 "\\.jsonl\\'")))

(defconst drover-claude-code-transcripts--reader-version 2
  "Version of `drover-claude-code-transcripts--read'.
Raised when it reads anew what it read before, so that what an older
one read and kept between Emacs sessions is read again.")

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
        (puthash id (list :kind (alist-get 'kind entry) :state (alist-get 'state entry)
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
                     (drover-claude-code-transcripts--front-end data)))))

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
                        (drover-claude-code-recap file)))
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
The backend's :start."
  (drover-transcript-start (drover-claude-code-transcripts--current-index)))

(defun drover-claude-code-transcripts--register ()
  "Register the `claude-code' backend."
  (drover-register-backend
   'claude-code
   :label "Claude Code"
   :recorded '(claude-code)
   :recorded-times #'drover-claude-code-transcripts--times
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
  (add-hook 'kill-emacs-hook #'drover-claude-code-transcripts--save))

(defun drover-claude-code-transcripts--save ()
  "Keep what was read of the transcripts, as Emacs exits.
An error is reported, and lets Emacs exit."
  (drover-with-reporting "keeping Claude Code's transcripts"
    (when-let* ((index (drover-claude-code-transcripts--current-index)))
      (drover-transcript-save index t))))

(defun drover-claude-code-transcripts-uninstall (front-end)
  "Stop listing Claude Code's conversations for FRONT-END.
The backend goes once no front end needs it."
  (setq drover-claude-code-transcripts--users (delq front-end drover-claude-code-transcripts--users))
  (unless drover-claude-code-transcripts--users
    (when (drover-claude-code-transcripts--current-index)
      (drover-transcript-stop (drover-claude-code-transcripts--current-index)))
    (drover-command-cancel drover-claude-code-transcripts--agents-job)
    (remove-hook 'kill-emacs-hook #'drover-claude-code-transcripts--save)
    (drover-unregister-backend 'claude-code)))

;; Loaded anew, as reloading drover does, the backend a front end
;; installed takes this file's properties.
(when (eq (drover-backend-property 'claude-code :sessions) #'drover-claude-code-transcripts--sessions)
  (drover-claude-code-transcripts--register))

(provide 'drover-claude-code-transcripts)
;;; drover-claude-code-transcripts.el ends here
