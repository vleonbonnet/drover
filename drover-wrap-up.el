;;; drover-wrap-up.el --- Have a session clean up after itself, then archive it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; A session the user is done with leaves things behind: worktrees it
;; added, folders of notes it wrote while investigating, a second clone
;; of a repository.  `drover-wrap-up', Z on a session, tells its agent
;; that the user is closing it, so that it removes what it created that
;; does not belong where it is, then archives it and closes its buffer:
;; at once for a session that runs without its buffer, as OpenCode's,
;; once the cleanup's turn ended for a terminal or a shell, which would
;; end it.
;;
;; The prompt (`drover-wrap-up-prompt') goes through the queue of
;; scheduled prompts, which never touches a draft, after the session's
;; own turn when one runs, with the engine's build agent, the plan agent
;; deleting nothing.  It lists what drover attributes to the session: the
;; files its edits created outside any repository, the directories
;; holding only those, and the worktrees added to its repository since
;; it started, a hint for the agent, never a list drover deletes.
;;
;; The agent may keep the session open: by asking a question, or waiting
;; for a permission, by starting its last message with
;; `drover-wrap-up-keyword', or by ending it with a question.  A turn
;; failing or interrupted keeps it open too, its cleanup unfinished.
;; Kept open, the session is restored from the archive, its buffer kept,
;; and a warning tells why.  Restored meanwhile, by the user or another
;; client, it is no longer wrapped up, and a message says so;
;; `drover-wrap-up-under-way-p' tells code that restores sessions which
;; to leave archived.
;;
;; Wrap-ups under way are kept in `drover-state-directory', so that one
;; of a session running without its buffer is followed to its end after
;; Emacs restarts.  A session of another host is wrapped up by its host,
;; which tells this one how it ended.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'format-spec)
(require 'drover-core)
(require 'drover-model)
(require 'drover-links)
(require 'drover-schedule)
(require 'drover-task-git)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 24))
  (load "drover-core" nil t))

(declare-function drover-remote-tell-peer "drover-remote" (name text))

;;;; Options

(defcustom drover-wrap-up-prompt
  "The user asked to close this session.  Before it is archived, clean up what this session created that does not belong where it is, then stop.

Remove:
- git worktrees this session added: remove them with `git worktree remove`, and delete their branches only when merged or holding nothing new;
- directories it made outside an existing repository, such as investigation or scratch folders, and git clones duplicating a repository already on this machine;
- new files it left that belong to no repository and to no existing Org file.

Keep work committed, or meant to be, in an existing repository, edits to existing files, and notes added to existing Org files.  Leave alone the temporary folder your harness made for this session, and anything under the system's temporary directories: they are managed for you.  Delete nothing you are not sure this session created.%r

If something needs the user's decision first, such as uncommitted or unpushed work, an unmerged branch or a path you are unsure of, leave it: ask with your question tool, or make %k alone the first line of your final message and say what needs deciding.  The session then stays open.  Otherwise end with a short list of what you removed.%a"
  "The prompt asking a session to clean up as it is wrapped up.
A `format-spec' template: %k is `drover-wrap-up-keyword'; %r is
`drover-wrap-up-removal-advice' and %a what drover attributes to the
session (see `drover-wrap-up'), each empty or a paragraph of its own
after a blank line; %% is a percent sign."
  :type 'string
  :group 'drover)

(defcustom drover-wrap-up-keyword "DROVER-KEEP-OPEN"
  "The word an agent starts its last message with to keep its session open.
Matched case sensitively, alone on a line or followed by what needs
deciding, markup around it ignored."
  :type 'string
  :group 'drover)

(defcustom drover-wrap-up-removal-advice nil
  "How agents remove files and directories on this host, or nil.
A paragraph of `drover-wrap-up-prompt' (its %r), for a host where they
remove them otherwise than as they would, such as one denying a
recursive rm and offering a command of its own."
  :type '(choice (const :tag "None" nil) string)
  :group 'drover)

(defcustom drover-wrap-up-agent 'build
  "The agent the session runs the cleanup with.
`build', the engine's build agent (see `drover-engine-build-agent'),
which may delete what a plan agent would only ask about; nil, the agent
the session runs."
  :type '(choice (const :tag "The engine's build agent" build)
                 (const :tag "The session's own agent" nil))
  :group 'drover)

(defcustom drover-wrap-up-attribution-timeout 10
  "Seconds at most finding what a session created, before its prompt goes."
  :type 'number
  :group 'drover)

(defcustom drover-wrap-up-lost-seconds 600
  "Seconds a wrap-up waits for its session when its backend no longer lists it.
Past them, it is given up, the session staying archived."
  :type 'number
  :group 'drover)

(defconst drover-wrap-up--version 1
  "Version of the format of the file of wrap-ups.")

(defconst drover-wrap-up--states '(preparing waiting-turn sending archiving watching judging)
  "States of a wrap-up under way, as its file keeps them.")

(defconst drover-wrap-up--hint-limit 40
  "Entries at most of each list the hint gives the agent.")

(defconst drover-wrap-up--directory-limit 200
  "Entries at most a directory may hold to be told as created by a session.")

(defconst drover-wrap-up--tick 30
  "Seconds between two looks at the wrap-ups under way, besides changes.")

(defconst drover-wrap-up--archive-grace 60
  "Seconds a session may take to show archived before it counts as restored.")

(defconst drover-wrap-up--recap-attempts 4
  "Times a session's last reply is asked for before it counts as unread.")

(defvar drover-wrap-up--recap-delay 3
  "Seconds between two asks of a session's last reply that failed.
A service answering its event stream may not take a request yet, as
OpenCode's once refused one the moment a turn ended.")

;;;; The wrap-ups

(cl-defstruct (drover-wrap-up--table (:constructor drover-wrap-up--table-make)
                                     (:copier nil))
  "The wrap-ups under way."
  (stamp (drover-layout 'drover-wrap-up--table)
         :documentation "The layout it was made of, see `drover-layout'.")
  (entries nil :documentation "The wrap-ups under way, plists, oldest first.")
  (loaded nil :documentation "Non-nil once the file was read.")
  (unreadable nil :documentation "Non-nil when the file did not read: it is never written over.")
  (timer nil :documentation "The timer looking at the wrap-ups now and then, while some are under way."))

(drover-note-layout 'drover-wrap-up--table)

(defvar drover-wrap-up--table nil
  "The wrap-ups under way, a `drover-wrap-up--table', or nil before read.")

(defun drover-wrap-up--state ()
  "Return the table of wrap-ups, made anew when another version of drover made it."
  (unless (drover-layout-current-p drover-wrap-up--table 'drover-wrap-up--table)
    (when drover-wrap-up--table
      (drover-stale-layout "the wrap-ups")
      (when (timerp (drover-wrap-up--table-timer drover-wrap-up--table))
        (cancel-timer (drover-wrap-up--table-timer drover-wrap-up--table))))
    (setq drover-wrap-up--table (drover-wrap-up--table-make)))
  drover-wrap-up--table)

(defun drover-wrap-up-file ()
  "Return the file keeping the wrap-ups under way."
  (expand-file-name "wrap-ups.eld" drover-state-directory))

(defun drover-wrap-up--entry-p (entry)
  "Return non-nil when ENTRY is a wrap-up as the file keeps them."
  (and (plistp entry)
       (stringp (plist-get entry :id))
       (plist-get entry :engine) (symbolp (plist-get entry :engine))
       (stringp (plist-get entry :engine-id))
       (plist-get entry :backend) (symbolp (plist-get entry :backend))
       (plist-get entry :session)
       (plist-get entry :front-end) (symbolp (plist-get entry :front-end))
       (stringp (plist-get entry :title))
       (stringp (plist-get entry :directory))
       (memq (plist-get entry :state) drover-wrap-up--states)
       (or (null (plist-get entry :mark)) (numberp (plist-get entry :mark)))
       (or (null (plist-get entry :job)) (stringp (plist-get entry :job)))
       (or (null (plist-get entry :peer)) (stringp (plist-get entry :peer)))
       (or (null (plist-get entry :hint)) (stringp (plist-get entry :hint)))
       (or (null (plist-get entry :archived-at)) (numberp (plist-get entry :archived-at)))
       (numberp (plist-get entry :started))))

(defun drover-wrap-up--load ()
  "Read the wrap-ups once, and return their table.
A file that does not read is told of, and never written over."
  (let ((table (drover-wrap-up--state)))
    (unless (drover-wrap-up--table-loaded table)
      (setf (drover-wrap-up--table-loaded table) t)
      (condition-case err
          (setf (drover-wrap-up--table-entries table)
                (seq-filter #'drover-wrap-up--entry-p
                            (drover-read-data (drover-wrap-up-file) drover-wrap-up--version :wrap-ups)))
        (error (setf (drover-wrap-up--table-unreadable table) t)
               (drover-warn "Wrap-ups not read, and not written over: %s" (drover-error-message err)))))
    table))

(defun drover-wrap-up--entries ()
  "Return the wrap-ups under way, oldest first, read the first time."
  (drover-wrap-up--table-entries (drover-wrap-up--load)))

(defun drover-wrap-up--save ()
  "Write the wrap-ups under way, or remove their file when none is.
Never over a file that did not read; a failure is warned of."
  (let ((table (drover-wrap-up--state)))
    (unless (drover-wrap-up--table-unreadable table)
      (condition-case err
          (if-let* ((entries (drover-wrap-up--table-entries table)))
              (drover-write-data (drover-wrap-up-file) "Sessions being wrapped up"
                                 drover-wrap-up--version :wrap-ups entries)
            (when (file-exists-p (drover-wrap-up-file))
              (delete-file (drover-wrap-up-file))))
        (error (drover-warn "Wrap-ups not saved: %s" (drover-error-message err)))))))

(defun drover-wrap-up--current-p (entry)
  "Return non-nil while ENTRY is under way."
  (and (drover-layout-current-p drover-wrap-up--table 'drover-wrap-up--table)
       (memq entry (drover-wrap-up--table-entries drover-wrap-up--table))
       t))

(defun drover-wrap-up--find (id)
  "Return the wrap-up of the conversation ID under way, or nil."
  (seq-find (lambda (entry) (equal (plist-get entry :id) id)) (drover-wrap-up--entries)))

;;;###autoload
(defun drover-wrap-up-under-way-p (engine id)
  "Return non-nil while the conversation ID of ENGINE is being wrapped up.
ENGINE is a symbol, such as `opencode', and ID the engine's own
identifier of the conversation.  Its session is archived as its cleanup's
turn begins, and a restore meanwhile calls the wrap-up off: code that
restores archived sessions starting a turn leaves this one archived."
  (and (drover-wrap-up--find (format "%s/%s" engine id)) t))

(defun drover-wrap-up--set (entry &rest properties)
  "Set PROPERTIES of ENTRY, a plist of keys and values, and save the wrap-ups."
  (cl-loop for (key value) on properties by #'cddr
           do (setf (plist-get entry key) value))
  (drover-wrap-up--save))

(defun drover-wrap-up--add (entry)
  "Add ENTRY to the wrap-ups under way, save them and follow them."
  (let ((table (drover-wrap-up--load)))
    (setf (drover-wrap-up--table-entries table)
          (append (drover-wrap-up--table-entries table) (list entry)))
    (drover-wrap-up--save)
    (drover-wrap-up--follow)))

(defun drover-wrap-up--drop (entry)
  "Remove ENTRY from the wrap-ups under way, save them, and follow those left."
  (let ((table (drover-wrap-up--state)))
    (setf (drover-wrap-up--table-entries table)
          (delq entry (drover-wrap-up--table-entries table)))
    (drover-wrap-up--save)
    (drover-wrap-up--follow)))

(defun drover-wrap-up--follow ()
  "Follow the sessions and prompts of the wrap-ups under way, if any."
  (let ((table (drover-wrap-up--state)))
    (if (drover-wrap-up--table-entries table)
        (progn
          (add-hook 'drover-changed-hook #'drover-wrap-up--changed)
          (add-hook 'drover-schedule-changed-hook #'drover-wrap-up--schedule-changed)
          (unless (timerp (drover-wrap-up--table-timer table))
            (setf (drover-wrap-up--table-timer table)
                  (run-at-time drover-wrap-up--tick drover-wrap-up--tick #'drover-wrap-up--changed))))
      (remove-hook 'drover-changed-hook #'drover-wrap-up--changed)
      (remove-hook 'drover-schedule-changed-hook #'drover-wrap-up--schedule-changed)
      (when (timerp (drover-wrap-up--table-timer table))
        (cancel-timer (drover-wrap-up--table-timer table))
        (setf (drover-wrap-up--table-timer table) nil)))))

(defun drover-wrap-up--tell (entry format-string &rest arguments)
  "Tell the host that asked for ENTRY, if any, FORMAT-STRING with ARGUMENTS.
The first argument is ENTRY's session, named with this host, such as
\"Fix the login on core\"."
  (when-let* ((peer (plist-get entry :peer))
              ((fboundp 'drover-remote-tell-peer)))
    (drover-with-reporting "wrap-up note"
      (drover-remote-tell-peer peer (apply #'format format-string
                                           (format "%s on %s" (plist-get entry :title) drover-host)
                                           arguments)))))

;;;; Beginning

(defun drover-wrap-up--refusal (session)
  "Return why SESSION cannot be wrapped up here, a line, or nil when it can."
  (let ((engine (drover-session-engine session)))
    (cond
     ((drover-session-parent session) "it is a subagent: wrap up the session above it")
     ((eq (drover-session-status session) 'waiting) "it waits for your answer: answer it first")
     ((drover-session-archived-p session) "it is archived already")
     ((not (drover-links-conversation session)) "its conversation is not known yet")
     ((not (drover-session-local-directory session)) "its directory is not known")
     ((not (file-directory-p (drover-session-local-directory session)))
      (format "its directory %s is gone" (abbreviate-file-name (drover-session-local-directory session))))
     ((not (drover-archivable-p session)) "it cannot be archived")
     ((not (drover-wrap-up--front-end session))
      (format "no front end resumes %s conversations" (drover-engine-label engine))))))

(defun drover-wrap-up--front-end (session)
  "Return the backend delivering SESSION's prompt, a front end, or nil.
SESSION's own when it takes prompts; else the front end resuming its
conversation, loaded if need be (see `drover-load-resumer')."
  (if (drover-supports-p session :submit)
      (drover-session-backend session)
    (car (drover-load-resumer (drover-session-engine session)))))

(defun drover-wrap-up-session (session &optional peer)
  "Wrap SESSION up: have it clean up, then archive it and close its buffer.
See `drover-wrap-up'.  PEER, the name of the host that asked, if any,
hears how it ended.  A session of another host is wrapped up by that
host (its backend's :wrap-up).  Signal a `user-error' telling why
SESSION cannot be wrapped up."
  (let ((title (drover-bound-line (drover-session-display-title session))))
    (if (drover-session-host session)
        (if (drover-supports-p session :wrap-up)
            (drover--call-confirming session :wrap-up #'ignore
                                     (lambda (message)
                                       (drover-with-reporting "wrap-up"
                                         (drover-warn "Could not wrap up %s: %s" title message))))
          (user-error "%s cannot be wrapped up: its host does not wrap up its sessions for this one" title))
      (drover-wrap-up--begin session peer))
    nil))

(defun drover-wrap-up--begin (session peer)
  "Begin wrapping up SESSION, of this host, for PEER, if any; return the wrap-up."
  (let* ((title (drover-bound-line (drover-session-display-title session) 120))
         (key (drover-session-engine-key session))
         (id (format "%s/%s" (car key) (cdr key))))
    (when-let* ((why (drover-wrap-up--refusal session)))
      (user-error "%s cannot be wrapped up: %s" title why))
    (when (drover-wrap-up--find id)
      (user-error "%s is being wrapped up already" title))
    (let ((entry (list :id id :engine (car key) :engine-id (cdr key)
                       :backend (drover-session-backend session) :session (drover-session-id session)
                       :front-end (drover-wrap-up--front-end session)
                       :title (substring-no-properties title)
                       :directory (file-name-as-directory (drover-session-local-directory session))
                       :state 'preparing :hint nil :runs-in-buffer nil :mark nil :active nil
                       :archived nil :archived-at nil :job nil :peer peer :started (float-time) :lost nil)))
      (drover-wrap-up--add entry)
      (drover-wrap-up--attribute
       session (lambda (hint)
                 (drover-with-reporting "wrap-up"
                   (when (and (drover-wrap-up--current-p entry) (eq (plist-get entry :state) 'preparing))
                     (setf (plist-get entry :hint) hint)
                     (drover-wrap-up--proceed entry)))))
      entry)))

(defun drover-wrap-up--busy-p (record)
  "Return non-nil when RECORD runs a turn, or holds prompts not taken yet."
  (or (memq (drover-session-status record) '(working retrying))
      (> (drover-pending-prompts record) 0)))

(defun drover-wrap-up--record (entry)
  "Return the record of ENTRY's session, archived or not, or nil."
  (drover-find-archived (plist-get entry :backend) (plist-get entry :session)))

(defun drover-wrap-up--proceed (entry)
  "Send ENTRY's prompt, or wait for its session's turn to end first."
  (let ((record (drover-wrap-up--record entry)))
    (cond
     ((and record (eq (drover-session-status record) 'waiting))
      (drover-wrap-up--drop entry)
      (drover-message "%s waits for your answer: its wrap-up is called off; Z again once answered"
                      (plist-get entry :title))
      (drover-wrap-up--tell entry "%s waits for an answer: its wrap-up is called off"))
     ((and record (drover-wrap-up--busy-p record))
      (drover-wrap-up--set entry :state 'waiting-turn))
     (t (drover-wrap-up--send entry)))))

;;;; What drover attributes to a session

(defconst drover-wrap-up--system-temporary-directories
  '("/tmp/" "/private/tmp/" "/var/folders/" "/private/var/folders/")
  "Temporary directories of the system whatever the environment says.
macOS's own, where its per-user temporary directories are, and /tmp.")

(defun drover-wrap-up--temporary-directories ()
  "Return the temporary directories of this host, canonical, with a slash.
Those the variable `temporary-file-directory' and the environment name, and
`drover-wrap-up--system-temporary-directories'."
  (delete-dups
   (mapcar (lambda (directory) (file-name-as-directory (drover-task-canonical directory)))
           (seq-filter (lambda (directory) (and (stringp directory) (not (string-empty-p directory))))
                       (append (list temporary-file-directory (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP"))
                               drover-wrap-up--system-temporary-directories)))))

(defun drover-wrap-up--under-p (file directory)
  "Return non-nil when FILE is DIRECTORY or under it, both canonical."
  (let ((directory (file-name-as-directory directory)))
    (or (string-prefix-p directory (file-name-as-directory file) (file-name-case-insensitive-p directory)))))

(defun drover-wrap-up--in-repository-p (file)
  "Return non-nil when FILE is in a Git working tree."
  (and (locate-dominating-file (file-name-directory (directory-file-name file)) ".git") t))

(defun drover-wrap-up--holds-only-p (directory files)
  "Return non-nil when DIRECTORY holds nothing but FILES, a hash table of names.
Its directories are looked into, never through a link, and at most
`drover-wrap-up--directory-limit' entries in all: one holding more, a
link or a file of FILES' not, is not."
  (let ((stack (list directory))
        (seen 0)
        (only t))
    (while (and only stack)
      (let ((entries (condition-case nil
                         (directory-files (pop stack) t directory-files-no-dot-files-regexp t
                                          (1+ drover-wrap-up--directory-limit))
                       (error (setq only nil) nil))))
        (dolist (entry entries)
          (when only
            (setq seen (1+ seen))
            (cond ((> seen drover-wrap-up--directory-limit) (setq only nil))
                  ((file-symlink-p entry) (setq only nil))
                  ((file-directory-p entry) (push entry stack))
                  ((not (gethash (drover-task-canonical entry) files)) (setq only nil)))))))
    only))

(defun drover-wrap-up--own-directories (files session-directory)
  "Return the directories holding nothing but FILES, then the FILES outside them.
FILES are canonical names of files a session created, outside any
repository; a directory is the highest one holding nothing but some of
them, below the home directory, the temporary ones and the root, never
SESSION-DIRECTORY nor one above it.  The value is (DIRECTORIES . REST)."
  (let* ((set (let ((table (make-hash-table :test #'equal)))
                (dolist (file files table) (puthash file t table))))
         (home (file-name-as-directory (drover-task-canonical "~")))
         (temporary (drover-wrap-up--temporary-directories))
         (session-directory (file-name-as-directory (drover-task-canonical session-directory)))
         (checked (make-hash-table :test #'equal))
         (directories nil))
    (cl-flet ((stop-p (directory)
                (or (equal (file-name-as-directory directory) home)
                    (not (drover-wrap-up--under-p directory home))
                    (drover-wrap-up--under-p session-directory directory)
                    (seq-some (lambda (root) (drover-wrap-up--under-p root directory)) temporary)
                    (equal (directory-file-name directory) (directory-file-name (file-name-directory (directory-file-name directory))))))
              (only-p (directory)
                (pcase (gethash directory checked 'unknown)
                  ('unknown (puthash directory (drover-wrap-up--holds-only-p directory set) checked))
                  (value value))))
      (dolist (file files)
        (let ((directory (directory-file-name (file-name-directory file)))
              (best nil))
          (while (and directory (not (stop-p directory)) (only-p directory))
            (setq best directory
                  directory (directory-file-name (file-name-directory directory))))
          (when best
            (push (file-name-as-directory best) directories)))))
    (let ((directories (seq-uniq directories)))
      ;; Only the highest: a directory under another one goes with it.
      (setq directories (seq-remove (lambda (directory)
                                      (seq-some (lambda (other)
                                                  (and (not (equal other directory))
                                                       (drover-wrap-up--under-p directory other)))
                                                directories))
                                    directories))
      (cons (sort directories #'string<)
            (seq-remove (lambda (file) (seq-some (lambda (directory) (drover-wrap-up--under-p file directory))
                                                 directories))
                        files)))))

(defun drover-wrap-up--created-files (session callback)
  "Call CALLBACK with the files SESSION's edits created, outside any repository.
They are canonical names of files still there, but under a temporary
directory, as the engine of SESSION recorded them (its :changes, scope
`session'); nil when it records none."
  (let* ((engine (drover-session-engine session))
         (directory (drover-session-local-directory session))
         (changes (drover-engine-property engine :changes))
         (called nil))
    (cl-flet ((deliver (files)
                (unless called
                  (setq called t)
                  (funcall callback files))))
      (if (not (functionp changes))
          (deliver nil)
        (condition-case nil
            (funcall changes (drover-session-engine-id session) 'session directory
                     (lambda (result)
                       (drover-with-reporting "wrap-up attribution"
                         (let ((temporary (drover-wrap-up--temporary-directories)))
                           (deliver
                            (delete-dups
                             (seq-filter
                              (lambda (file)
                                (and (file-regular-p file)
                                     (not (seq-some (lambda (root) (drover-wrap-up--under-p file root)) temporary))
                                     (not (drover-wrap-up--in-repository-p file))))
                              (mapcar (lambda (file) (drover-task-canonical (expand-file-name (plist-get file :file) directory)))
                                      (seq-filter (lambda (file) (and (eq (plist-get file :status) 'added)
                                                                      (stringp (plist-get file :file))))
                                                  (plist-get result :files))))))))))
          (error (deliver nil)))))))

(defun drover-wrap-up--worktree-created (worktree)
  "Return when WORKTREE, a directory, was added, a float time, or nil.
That is the first entry of its HEAD's log in its Git directory, which Git
writes as it adds the worktree, else when its gitdir file was written."
  (when-let* ((gitdir (ignore-errors (drover--git-directory worktree))))
    (or (let ((log (expand-file-name "logs/HEAD" gitdir)))
          (when (file-readable-p log)
            (with-temp-buffer
              (insert-file-contents log nil 0 1024)
              (goto-char (point-min))
              (when (re-search-forward "> \\([0-9]+\\) [-+][0-9]\\{4\\}" (line-end-position) t)
                (float (string-to-number (match-string 1)))))))
        (when-let* ((attributes (file-attributes (expand-file-name "gitdir" gitdir))))
          (float-time (file-attribute-modification-time attributes))))))

(defun drover-wrap-up--stray-worktree-p (worktree session-directory created others)
  "Return non-nil when WORKTREE, a plist, looks added by a session and left.
That is a linked worktree of the session's repository, not
SESSION-DIRECTORY nor above it, neither bare nor locked nor drover's own
\(a task's marker), added after CREATED, the session's start, where none
of OTHERS, the directories of the other live sessions, runs."
  (let ((path (plist-get worktree :path)))
    (and (not (plist-get worktree :bare))
         (not (plist-get worktree :locked))
         (not (plist-get worktree :prunable))
         (file-directory-p path)
         (not (drover-wrap-up--under-p session-directory path))
         (not (drover-task-git-read-marker path))
         (not (seq-some (lambda (directory) (drover-wrap-up--under-p directory path)) others))
         (when-let* ((added (drover-wrap-up--worktree-created path)))
           (>= added created)))))

(defun drover-wrap-up--added-worktrees (session callback)
  "Call CALLBACK with the worktrees added to SESSION's repository since it started.
They are plists, as `drover-task-git-parse-worktrees' returns them; nil
outside a repository, or when Git fails."
  (let* ((directory (drover-session-local-directory session))
         (created (drover-session-created session))
         (key (drover-session-engine-key session))
         (called nil))
    (cl-flet ((deliver (worktrees)
                (unless called
                  (setq called t)
                  (funcall callback worktrees))))
      (if (not (and directory created (drover-task-git-toplevel directory)))
          (deliver nil)
        (let ((session-directory (file-name-as-directory (drover-task-canonical directory)))
              (others (delq nil (mapcar (lambda (other)
                                          (unless (equal (drover-session-engine-key other) key)
                                            (when-let* ((directory (drover-session-local-directory other)))
                                              (file-name-as-directory (drover-task-canonical directory)))))
                                        (drover--live-sessions)))))
          (drover-task-git directory '("worktree" "list" "--porcelain")
                           (lambda (result)
                             (drover-with-reporting "wrap-up attribution"
                               (deliver
                                (and (plist-get result :ok)
                                     (seq-filter (lambda (worktree)
                                                   (drover-wrap-up--stray-worktree-p
                                                    worktree session-directory created others))
                                                 ;; The first is the main worktree.
                                                 (cdr (drover-task-git-parse-worktrees
                                                       (plist-get result :output))))))))
                           :timeout drover-wrap-up-attribution-timeout))))))

(defun drover-wrap-up--list (heading items)
  "Return HEADING then ITEMS, strings, as a list of the hint, bounded, or nil."
  (when items
    (concat "- " heading ":\n"
            (mapconcat (lambda (item) (concat "  - " item))
                       (seq-take items drover-wrap-up--hint-limit) "\n")
            (if (> (length items) drover-wrap-up--hint-limit)
                (format "\n  - and %d more" (- (length items) drover-wrap-up--hint-limit))
              ""))))

(defun drover-wrap-up-hint (files worktrees session-directory)
  "Return what drover attributes to a session, for its prompt, or an empty string.
FILES are the files its edits created outside any repository, canonical
names; WORKTREES the worktrees added to its repository since it started,
plists; SESSION-DIRECTORY the directory it runs in."
  (pcase-let* ((`(,directories . ,rest) (if files
                                             (drover-wrap-up--own-directories files session-directory)
                                           (cons nil nil)))
               (lists (delq nil
                            (list (drover-wrap-up--list
                                   "worktrees added to its repository since it started"
                                   (mapcar (lambda (worktree)
                                             (concat (abbreviate-file-name (plist-get worktree :path))
                                                     (if-let* ((branch (plist-get worktree :branch)))
                                                         (format " (branch %s)" branch)
                                                       "")))
                                           worktrees))
                                  (drover-wrap-up--list "directories holding only files its edits created"
                                                        (mapcar #'abbreviate-file-name directories))
                                  (drover-wrap-up--list "files its edits created outside any repository"
                                                        (mapcar #'abbreviate-file-name rest))))))
    (if lists
        (concat "What drover attributes to this session, a hint rather than a list to delete:\n"
                (mapconcat #'identity lists "\n"))
      "")))

(defun drover-wrap-up--attribute (session callback)
  "Call CALLBACK once with what drover attributes to SESSION, a string.
See `drover-wrap-up-hint'.  What is not found within
`drover-wrap-up-attribution-timeout' seconds is left out."
  (let ((files nil) (worktrees nil) (pending 2) (done nil) (timer nil)
        (directory (drover-session-local-directory session)))
    (cl-flet ((finish ()
                (unless done
                  (setq done t)
                  (when (timerp timer) (cancel-timer timer))
                  (funcall callback (or (drover-with-reporting "wrap-up attribution"
                                          (drover-wrap-up-hint files worktrees directory))
                                        "")))))
      (setq timer (run-at-time drover-wrap-up-attribution-timeout nil
                               (lambda () (drover-with-reporting "wrap-up attribution" (finish)))))
      (drover-wrap-up--created-files session (lambda (value)
                                               (setq files value)
                                               (when (zerop (cl-decf pending)) (finish))))
      (drover-wrap-up--added-worktrees session (lambda (value)
                                                 (setq worktrees value)
                                                 (when (zerop (cl-decf pending)) (finish)))))))

;;;; Sending

(defun drover-wrap-up-prompt-text (hint)
  "Return the prompt of a wrap-up, HINT telling what drover attributes to it."
  (cl-flet ((paragraph (text)
              (if (and (stringp text) (not (string-blank-p text)))
                  (concat "\n\n" (string-trim text))
                "")))
    (format-spec drover-wrap-up-prompt
                 `((?k . ,drover-wrap-up-keyword)
                   (?r . ,(paragraph drover-wrap-up-removal-advice))
                   (?a . ,(paragraph hint))))))

(defun drover-wrap-up--send (entry)
  "Deliver the prompt of ENTRY through the queue, with the agent chosen."
  (condition-case err
      (let* ((engine (plist-get entry :engine))
             (agent (and (eq drover-wrap-up-agent 'build) (drover-engine-build-agent engine)))
             (target (list :link (drover-links-link (list :engine engine :id (plist-get entry :engine-id)
                                                          :host nil))
                           :backend (plist-get entry :front-end)
                           :directory (plist-get entry :directory)
                           :title (plist-get entry :title)
                           :options (and agent (list :agent agent))))
             (job (drover-schedule-deliver target (drover-wrap-up-prompt-text (plist-get entry :hint))
                                           :hidden t :origin "wrap-up")))
        (drover-wrap-up--set entry :state 'sending :job (plist-get job :id)))
    (error
     (drover-wrap-up--drop entry)
     (drover-warn "Could not wrap up %s: its prompt was not queued: %s"
                  (plist-get entry :title) (drover-error-message err))
     (drover-wrap-up--tell entry "Could not wrap up %s: its prompt was not queued"))))

(defun drover-wrap-up--schedule-changed ()
  "Follow the delivery of the prompts of wrap-ups.
On `drover-schedule-changed-hook'."
  (drover-with-reporting "wrap-up"
    (when (drover-layout-current-p drover-wrap-up--table 'drover-wrap-up--table)
      (dolist (entry (seq-filter (lambda (entry) (eq (plist-get entry :state) 'sending))
                                 (drover-wrap-up--table-entries drover-wrap-up--table)))
        (let* ((id (plist-get entry :job))
               (job (and id (drover-schedule-job id)))
               (title (plist-get entry :title)))
          (pcase (and job (plist-get job :status))
            ('sent
             ;; Out of `sending' first: forgetting the prompt runs this hook again.
             (drover-wrap-up--set entry :state 'archiving :job nil
                                  :mark (or (plist-get job :sent-at) (float-time)))
             (ignore-errors (drover-schedule-forget id))
             (drover-wrap-up--archive entry))
            ((and status (or 'failed 'interrupted 'cancelled))
             (drover-wrap-up--drop entry)
             (ignore-errors (drover-schedule-forget id))
             (drover-warn "Could not wrap up %s: its prompt was %s: %s" title status
                          (or (plist-get job :error) "no reason told"))
             (drover-wrap-up--tell entry "Could not wrap up %s: its prompt was %s" status))
            ('nil
             (when (and id
                        (drover-schedule--queue-p drover-schedule--queue)
                        (drover-schedule--queue-loaded drover-schedule--queue)
                        (not (drover-schedule-job id)))
               (drover-wrap-up--drop entry)
               (drover-warn "Could not wrap up %s: its prompt left the queue of scheduled prompts" title)))))))))

;;;; Archiving

(defun drover-wrap-up--live-record (entry)
  "Return the record of the conversation of ENTRY that took its prompt.
The record of a backend showing the conversation in a live buffer,
such as the front end that resumed it, else the session's own."
  (let ((key (cons (plist-get entry :engine) (plist-get entry :engine-id))))
    (or (seq-find (lambda (session)
                    (and (equal (drover-session-engine-key session) key)
                         (not (drover-session-host session))
                         (drover-session-buffer session)))
                  (drover--live-sessions))
        (drover-wrap-up--record entry))))

(defun drover-wrap-up--archive (entry)
  "Archive the session of ENTRY, which took its prompt, keeping its buffer.
A buffer that runs its agent closes once the verdict is known; one
showing a session that runs elsewhere, at once (see
`drover--archive-finish')."
  (let ((record (drover-wrap-up--live-record entry))
        (title (plist-get entry :title)))
    (if (null record)
        (progn
          (drover-wrap-up--drop entry)
          (drover-warn "%s took its wrap-up prompt, but is no longer listed: it is not archived" title))
      (drover-wrap-up--set entry :state 'archiving
                           :backend (drover-session-backend record)
                           :session (drover-session-id record)
                           :runs-in-buffer (and (drover-backend-property record :runs-in-buffer) t))
      (condition-case err
          (drover--archive-step
           record (lambda (ok &optional value)
                    (drover-with-reporting "wrap-up"
                      (drover-wrap-up--archived entry record ok value))))
        (error
         (drover-wrap-up--drop entry)
         (drover-warn "Could not archive %s, whose cleanup goes on: %s" title (drover-error-message err)))))))

(defun drover-wrap-up--archived (entry record ok value)
  "Go on with ENTRY once RECORD's archive settled: OK, and VALUE as settled.
See `drover--archive-step'."
  (when (and (drover-wrap-up--current-p entry) (eq (plist-get entry :state) 'archiving))
    (let ((title (plist-get entry :title)))
      (if (not ok)
          (progn
            (drover-wrap-up--drop entry)
            (drover-warn "Could not archive %s, whose cleanup goes on: %s" title (or value "refused"))
            (drover-wrap-up--tell entry "Could not archive %s"))
        (drover-wrap-up--set entry :state 'watching :archived-at (float-time))
        (unless (plist-get entry :runs-in-buffer)
          (pcase (drover--archive-finish record value)
            (`(failed . ,message)
             (drover-warn "Could not close the buffer of %s, archived: %s" title message))
            ('kept (drover-message "The unsent input of %s, archived, is in the kill ring" title))))
        (drover-wrap-up--check entry)))))

;;;; Watching the cleanup's turn

(defun drover-wrap-up--changed ()
  "Look at the sessions of the wrap-ups under way.
On `drover-changed-hook', and from a timer now and then."
  (drover-with-reporting "wrap-up"
    (when (drover-layout-current-p drover-wrap-up--table 'drover-wrap-up--table)
      (dolist (entry (copy-sequence (drover-wrap-up--table-entries drover-wrap-up--table)))
        (when (drover-wrap-up--current-p entry)
          (drover-wrap-up--check entry))))))

(defun drover-wrap-up--check (entry)
  "Go on with ENTRY as its session stands."
  (pcase (plist-get entry :state)
    ('waiting-turn (drover-wrap-up--check-turn entry))
    ('watching (drover-wrap-up--watch entry))))

(defun drover-wrap-up--check-turn (entry)
  "Send ENTRY's prompt once its session's own turn ended."
  (let ((record (drover-wrap-up--record entry)))
    (cond
     ((null record)
      (drover-wrap-up--drop entry)
      (drover-message "%s left before its wrap-up began" (plist-get entry :title)))
     ((not (drover-wrap-up--busy-p record)) (drover-wrap-up--proceed entry)))))

(defun drover-wrap-up--ended-p (entry record)
  "Return non-nil when the turn of ENTRY's prompt ended, as RECORD tells.
Out of a turn, RECORD ended it once seen in one since the prompt went,
or when its status changed and it did something since."
  (let ((mark (or (plist-get entry :mark) 0)))
    (and (not (drover-wrap-up--busy-p record))
         (or (plist-get entry :active)
             (and (> (or (drover-session-since record) 0) mark)
                  (> (or (drover-session-updated record) 0) mark))))))

(defun drover-wrap-up--watch (entry)
  "Follow the cleanup's turn of ENTRY, and judge it once it ended."
  (let ((record (drover-wrap-up--record entry))
        (title (plist-get entry :title)))
    (cond
     ((null record) (drover-wrap-up--missing entry))
     ((not (drover-session-archived-p record))
      ;; Restored by the user, or by a prompt sent: no longer wrapped up.
      ;; A backend may tell the archive a moment after it confirmed it.
      (cond
       ((plist-get entry :archived)
        (drover-wrap-up--drop entry)
        (drover-message "The wrap-up of %s is called off: it was restored from the archive" title)
        (drover-wrap-up--tell entry "The wrap-up of %s is called off: it was restored from the archive"))
       ((> (- (float-time) (or (plist-get entry :archived-at) 0)) drover-wrap-up--archive-grace)
        (drover-wrap-up--drop entry)
        (drover-warn "The wrap-up of %s is called off: it never showed archived" title)
        (drover-wrap-up--tell entry "The wrap-up of %s is called off: it never showed archived"))
       ((plist-get entry :lost) (drover-wrap-up--set entry :lost nil))))
     ((and (plist-get entry :runs-in-buffer) (not (drover-session-buffer record)))
      (drover-wrap-up--drop entry)
      (drover-message "The buffer of %s closed before its wrap-up ended; it stays archived" title))
     (t
      (unless (plist-get entry :archived)
        (drover-wrap-up--set entry :archived t :lost nil))
      (cond
       ((eq (drover-session-status record) 'waiting)
        (drover-wrap-up--keep entry record
                              (if-let* ((request (car (drover-session-requests record))))
                                  (format "it %s: %s"
                                          (if (eq (plist-get request :kind) 'question) "asks" "awaits a permission")
                                          (or (drover-bound-line (plist-get request :summary) 200) "no summary"))
                                "it waits for you")))
       ((drover-wrap-up--busy-p record)
        (unless (plist-get entry :active)
          (drover-wrap-up--set entry :active t)))
       ((drover-wrap-up--ended-p entry record)
        (drover-wrap-up--judge entry record)))))))

(defun drover-wrap-up--missing (entry)
  "Note that ENTRY's session is not listed, and give it up once lost too long."
  (let ((title (plist-get entry :title)))
    (cond
     ((plist-get entry :runs-in-buffer)
      (drover-wrap-up--drop entry)
      (drover-message "The buffer of %s closed before its wrap-up ended; it stays archived" title))
     ((null (plist-get entry :lost))
      (drover-wrap-up--set entry :lost (float-time)))
     ((> (- (float-time) (plist-get entry :lost)) drover-wrap-up-lost-seconds)
      (drover-wrap-up--drop entry)
      (drover-warn "Lost track of the wrap-up of %s, no longer listed: it stays archived" title)
      (drover-wrap-up--tell entry "Lost track of the wrap-up of %s: it stays archived")))))

;;;; The verdict

(defun drover-wrap-up--keyword-line (reply)
  "Return the line of REPLY telling `drover-wrap-up-keyword', or nil.
It is what follows the keyword on its line, or the next line that is
not blank, or the keyword alone."
  (when (and (stringp reply) (stringp drover-wrap-up-keyword)
             (not (string-blank-p drover-wrap-up-keyword)))
    (let ((case-fold-search nil)
          (regexp (concat "^[ \t>*_`#]*" (regexp-quote drover-wrap-up-keyword)
                          "\\(?:[^[:alnum:]_-]\\|$\\)[*_` \t:.!-]*\\(.*\\)$")))
      (when (string-match regexp reply)
        (let ((rest (string-trim (match-string 1 reply)))
              (after (substring reply (match-end 0))))
          (or (and (not (string-empty-p rest)) (drover-bound-line rest 200))
              (drover-bound-line after 200)
              drover-wrap-up-keyword))))))

(defun drover-wrap-up--question (reply)
  "Return the last line of REPLY when it asks a question, or nil."
  (when-let* ((lines (and (stringp reply) (split-string reply "\n" t "[ \t]+")))
              (last-line (car (last lines)))
              ((string-match-p "?[*_)\"'»”]*\\'" last-line)))
    (drover-bound-line last-line 200)))

(defun drover-wrap-up--judge (entry record &optional attempt)
  "Read the last reply of ENTRY's session, RECORD, and keep it open or close it.
A reply that could not be read is asked for again, ATTEMPT counting the
asks, `drover-wrap-up--recap-attempts' at most."
  (let ((attempt (or attempt 1)))
    (cl-flet ((judging-p ()
                (and (drover-wrap-up--current-p entry) (eq (plist-get entry :state) 'judging))))
      (drover-wrap-up--set entry :state 'judging)
      (drover-recap record
                    (lambda (recap)
                      (drover-with-reporting "wrap-up"
                        (when (judging-p)
                          (if (and (plist-get recap :failure) (< attempt drover-wrap-up--recap-attempts))
                              (run-at-time drover-wrap-up--recap-delay nil
                                           (lambda ()
                                             (drover-with-reporting "wrap-up"
                                               (when (judging-p)
                                                 (drover-wrap-up--judge entry (or (drover-wrap-up--record entry) record)
                                                                        (1+ attempt))))))
                            (drover-wrap-up--verdict entry (or (drover-wrap-up--record entry) record) recap)))))))))

(defun drover-wrap-up--verdict (entry record recap)
  "Keep ENTRY's session, RECORD, open or close it, as RECAP, its last turn, tells."
  (let ((reply (plist-get recap :reply)))
    (cond
     ((plist-get recap :failure)
      (drover-wrap-up--keep entry record (format "its last reply could not be read: %s"
                                                 (drover-bound-line (plist-get recap :failure) 200))))
     ((drover-wrap-up--keyword-line reply)
      (drover-wrap-up--keep entry record (concat "the agent asks: " (drover-wrap-up--keyword-line reply))))
     ((or (eq (drover-session-status record) 'failed) (plist-get recap :error))
      (drover-wrap-up--keep entry record
                            (concat "its cleanup failed"
                                    (if-let* ((error (or (plist-get recap :error) (drover-session-detail record))))
                                        (concat ": " (drover-bound-line error 200))
                                      ""))))
     ((drover-wrap-up--question reply)
      (drover-wrap-up--keep entry record (concat "the agent asks: " (drover-wrap-up--question reply))))
     (t (drover-wrap-up--close entry record)))))

(defun drover-wrap-up--keep (entry record reason)
  "Keep ENTRY's session, RECORD, open: restore it, for REASON, a line."
  (let ((title (plist-get entry :title)))
    (drover-wrap-up--drop entry)
    (condition-case err
        (progn
          (drover-unarchive-session record)
          (drover-warn "%s stays open: %s" title reason)
          (drover-wrap-up--tell entry "%s stays open: %s" reason))
      (error
       (drover-warn "%s needs you before it closes (%s), but stays archived: %s"
                    title reason (drover-error-message err))
       (drover-wrap-up--tell entry "%s needs you before it closes: %s" reason)))))

(defun drover-wrap-up--close (entry record)
  "Close the buffer of ENTRY's session, RECORD, archived and cleaned up."
  (let ((title (plist-get entry :title)))
    (drover-wrap-up--drop entry)
    (pcase (and (plist-get entry :runs-in-buffer) (drover--close-archived record))
      (`(failed . ,message)
       (drover-warn "Wrapped up %s, but its buffer stays open: %s" title message))
      ('kept (drover-message "Wrapped up %s; its unsent input is in the kill ring" title))
      ((or 'later 'waiting)
       (drover-message "Wrapped up %s; its buffer closes once its turn ends" title))
      (_ (drover-message "Wrapped up %s" title)))
    (drover-wrap-up--tell entry "Wrapped up %s")))

;;;; Commands

(defun drover-wrap-up--kind (session)
  "Return what wrapping SESSION up is, for `drover--bulk-start'."
  (if (drover-session-host session)
      (drover--request-kind session :wrap-up)
    'local))

(defun drover-wrap-up--step (session settle)
  "Wrap SESSION up as `drover-wrap-up-session' does, then SETTLE.
For `drover--bulk-start': another host's session settles once its host
took it, this host's once its wrap-up began."
  (if (drover-session-host session)
      (drover--call-confirming session :wrap-up
                               (lambda () (funcall settle t))
                               (lambda (message) (funcall settle nil message)))
    (drover-wrap-up--begin session nil)
    (funcall settle t)))

;;;###autoload
(defun drover-wrap-up (sessions)
  "Have SESSIONS clean up, then archive them and close their buffers.
SESSIONS is a session or a list.  Each is told the user is closing it,
and asked to remove what it created that does not belong where it is
\(see `drover-wrap-up-prompt'): worktrees it added, folders it made
outside repositories, duplicate clones, stray files; drover lists what
it attributes to it, a hint.  The prompt goes once the session's turn,
if one runs, ended, with the engine's build agent (see
`drover-wrap-up-agent').  The session is then archived, and its buffer
closes: at once for a session running without it, as OpenCode's; once
the cleanup ended for a terminal or a shell.

The agent keeps the session open by asking a question or awaiting a
permission, by starting its last message with
`drover-wrap-up-keyword', or by ending it with a question; a turn
failing or interrupted keeps it open too.  The session is then restored
from the archive, its buffer kept, and a warning says why.

Interactively, SESSIONS are the selected sessions that may be wrapped
up, or the session at point (see `drover--selected-for')."
  (interactive (list (drover--selected-for nil #'drover-wrap-up-p "can be wrapped up")))
  (drover--act sessions #'drover-wrap-up-session "Wrapping up %s"
               :verb "Wrapping up" :step #'drover-wrap-up--step :kind #'drover-wrap-up--kind))

;;;; Across Emacs sessions

(defun drover-wrap-up-restore ()
  "Follow again the wrap-ups under way as Emacs stopped.
As drover first starts, when their file exists (see
`drover--kept-modules').  One whose prompt was not queued yet is given
up, told; one whose prompt waits in the queue goes on as it goes.  The
cleanup of a session running in its buffer ended with Emacs: its
wrap-up is given up, told, the session staying archived; that of a
session running elsewhere is followed to its end."
  (dolist (entry (copy-sequence (drover-wrap-up--entries)))
    (let ((title (plist-get entry :title)))
      (pcase (plist-get entry :state)
        ((or 'preparing 'waiting-turn)
         (drover-wrap-up--drop entry)
         (drover-message "The wrap-up of %s had not begun as Emacs stopped: Z begins it again" title))
        ((or 'archiving 'watching 'judging)
         (if (plist-get entry :runs-in-buffer)
             (progn
               (drover-wrap-up--drop entry)
               (drover-warn "The wrap-up of %s was cut short as Emacs stopped: it stays archived, and z restores it"
                            title))
           (drover-wrap-up--set entry :state 'watching :lost nil))))))
  (drover-wrap-up--follow)
  (drover-wrap-up--changed))

(defun drover-wrap-up-unload-function ()
  "Stop following the wrap-ups; they stay in their file."
  (remove-hook 'drover-changed-hook #'drover-wrap-up--changed)
  (remove-hook 'drover-schedule-changed-hook #'drover-wrap-up--schedule-changed)
  (when (and (drover-layout-current-p drover-wrap-up--table 'drover-wrap-up--table)
             (timerp (drover-wrap-up--table-timer drover-wrap-up--table)))
    (cancel-timer (drover-wrap-up--table-timer drover-wrap-up--table)))
  nil)

(provide 'drover-wrap-up)
;;; drover-wrap-up.el ends here
