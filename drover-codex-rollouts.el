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
;; newest turn and the last activity.  A conversation's name is the one
;; session_index.jsonl gave it last, else its first prompt.  Sessions
;; run by `codex exec' are left out, as the CLI's own picker leaves them
;; out, unless `drover-codex-include-non-interactive' says otherwise; a
;; subagent's rollout names its parent.
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

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
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

(defvar drover-codex-rollouts--files (make-hash-table :test #'equal)
  "The rollout of each conversation, by identifier.")

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
      (add :branch (sniff "branch"))
      (add :prompt (drover-codex-rollouts--first-prompt beg end))
      (pcase-let ((`(,model ,effort) (drover-codex-rollouts--turn beg end)))
        (add :model model)
        (add :effort effort))
      (add :messages (drover-codex-rollouts--messages-p beg end)))
    data))

(defun drover-codex-rollouts--read-tail (beg end)
  "Return what the records between BEG and END tell of the conversation's end.
A plist of the newest model, effort and activity there."
  (let ((data nil))
    (cl-flet ((add (key value) (when value (setq data (plist-put data key value)))))
      (pcase-let ((`(,model ,effort) (drover-codex-rollouts--turn beg end)))
        (add :model model)
        (add :effort effort))
      (add :updated (save-excursion
                      (goto-char end)
                      (drover-transcript-time (drover-transcript-sniff "timestamp" t beg))))
      (add :messages (drover-codex-rollouts--messages-p beg end)))
    data))

(defun drover-codex-rollouts--read (file attributes old)
  "Return what rollout FILE, of ATTRIBUTES, tells of its conversation.
OLD is the entry read before, or nil: a rollout only appended to since,
the same file grown past its head, is read from where it ended.  For
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
          (while recent
            (setq data (plist-put data (pop recent) (pop recent)))))
        (setq data (plist-put data :id (drover-codex-rollouts--id file)))
        (setq data (plist-put data :archived (drover-codex-rollouts--archived-file-p file)))
        (let ((directory (plist-get data :cwd)))
          (setq data (plist-put data :directory (and directory (file-name-as-directory directory))))
          (setq data (plist-put data :project (drover-transcript-project (plist-get data :directory)))))
        (unless (plist-get data :updated)
          (setq data (plist-put data :updated (float-time (file-attribute-modification-time attributes)))))
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
          (when (gethash id drover-codex-rollouts--files)
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

(defun drover-codex-rollouts--record (data names models)
  "Return the `drover-session' of the conversation DATA tells, or nil.
NAMES are the names conversations were given; MODELS a hash table
caching the names of models, which Codex's models cache gives."
  (when (drover-codex-rollouts--listed-p data)
    (let* ((id (plist-get data :id))
           (model (plist-get data :model)))
      (drover-session-create
       :backend 'codex :id id
       :engine 'codex :engine-id id
       :parent (plist-get data :parent)
       :title (or (and names (gethash id names)) (plist-get data :prompt) id)
       :directory (plist-get data :directory)
       :project (plist-get data :project)
       :branch (plist-get data :branch)
       :status 'idle
       :updated (plist-get data :updated)
       :created (plist-get data :created)
       :model (and model
                   (with-memoization (gethash model models)
                     (or (alist-get 'display_name (drover-codex--catalog-model model)) model)))
       :effort (plist-get data :effort)
       :archived (and (plist-get data :archived) t)))))

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

(defun drover-codex-rollouts--data (id)
  "Return what was read of conversation ID, or nil."
  (when-let* ((file (gethash id drover-codex-rollouts--files)))
    (drover-transcript-lookup (drover-codex-rollouts--current-index) file)))

(defun drover-codex-rollouts--session (id)
  "Return the record of conversation ID, as :sessions lists it, or nil.
Its rollout is looked at again soon when it changed since it was read.
The backend's :session."
  (let* ((file (gethash id drover-codex-rollouts--files))
         (data (and file (drover-transcript-lookup (drover-codex-rollouts--current-index) file))))
    (when file
      (drover-transcript-check (drover-codex-rollouts--current-index) file))
    (when (and data (not (plist-get data :archived)) (drover-codex-rollouts--recent-p data))
      (drover-codex-rollouts--record data (drover-codex-rollouts--names)
                                     (make-hash-table :test #'equal)))))

;;;; Following the rollouts

(defun drover-codex-rollouts--changed (file old new)
  "Report that what was read of rollout FILE went from OLD to NEW.
A conversation archived moves to another file: the one it has now is
kept."
  (let ((id (plist-get (or new old) :id)))
    (when id
      (cond (new (puthash id file drover-codex-rollouts--files))
            ((equal (gethash id drover-codex-rollouts--files) file)
             (remhash id drover-codex-rollouts--files)))
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

(defun drover-codex-rollouts--make-index ()
  "Return a new index of Codex's rollouts."
  (drover-transcript-index-create 'codex-rollouts
                                  #'drover-codex-rollouts--roots
                                  #'drover-codex-rollouts--read
                                  #'drover-codex-rollouts--changed
                                  #'drover-codex-rollouts--looked))

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
        (drover-resume 'codex id directory)))))

(defun drover-codex-rollouts--recap (session callback)
  "Call CALLBACK at once with a recap of the last turn of SESSION.
It is read from the end of its rollout (see `drover-codex-recap').  The
backend's :recap."
  (let ((file (gethash (drover-session-id session) drover-codex-rollouts--files)))
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
  (let* ((title (drover-session-display-title session))
         (output (generate-new-buffer " *drover codex*")))
    (make-process
     :name (concat "drover-codex-" command)
     :buffer output
     :command (drover-codex-rollouts-command command (drover-session-id session))
     :connection-type 'pipe
     :noquery t
     :sentinel (lambda (process _event)
                 (drover-with-reporting (concat "codex " command)
                   (unless (process-live-p process)
                     (unwind-protect
                         (if (and (eq (process-status process) 'exit)
                                  (zerop (process-exit-status process)))
                             (message "Codex %s %s" done title)
                           (drover-warn "Codex could not %s %s: %s" command title
                                        (if (buffer-live-p output)
                                            (string-trim (with-current-buffer output (buffer-string)))
                                          (process-status process))))
                       (when (buffer-live-p output) (kill-buffer output))
                       (when-let* ((index (drover-codex-rollouts--current-index)))
                         (drover-transcript-look-soon index)))))))))

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
  "Look at the rollouts from now on.  The backend's :start."
  (drover-transcript-start (drover-codex-rollouts--current-index)))

(defun drover-codex-rollouts--register ()
  "Register the `codex' backend."
  (drover-register-backend
   'codex
   :label "Codex"
   :recorded '(codex)
   :start #'drover-codex-rollouts--start
   :sessions #'drover-codex-rollouts--sessions
   :session #'drover-codex-rollouts--session
   :history #'drover-codex-rollouts--history
   :archived #'drover-codex-rollouts--archived
   :open #'drover-codex-rollouts--open
   :preview #'drover-codex-rollouts--preview
   :recap #'drover-codex-rollouts--recap
   :archive #'drover-codex-rollouts--archive
   :unarchive #'drover-codex-rollouts--unarchive
   :delete #'drover-codex-rollouts--delete
   :supports #'drover-codex-rollouts--supports))

(defun drover-codex-rollouts--save ()
  "Keep what was read of the rollouts, as Emacs exits.
An error is reported, and lets Emacs exit."
  (drover-with-reporting "keeping Codex's rollouts"
    (when-let* ((index (drover-codex-rollouts--current-index)))
      (drover-transcript-save index t))))

(defun drover-codex-rollouts-install (front-end)
  "List Codex's conversations, for FRONT-END to resume them.
The backend stays until every front end that installed it uninstalled
it."
  (unless (drover-codex-rollouts--current-index)
    (setq drover-codex-rollouts--index (drover-codex-rollouts--make-index)))
  (unless (assq 'codex drover--backends)
    (drover-codex-rollouts--register))
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

(provide 'drover-codex-rollouts)
;;; drover-codex-rollouts.el ends here
