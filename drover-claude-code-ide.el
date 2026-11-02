;;; drover-claude-code-ide.el --- Claude Code terminals in drover  -*- lexical-binding: t; -*-

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

;; The `claude-code-ide' backend: the Claude Code sessions of
;; claude-code-ide.el, terminals running the CLI, and the terminals it
;; no longer tracks whose buffer names say they are Claude's (see
;; `drover-claude-code-ide-buffer-regexp').  Sessions run the
;; `claude-code' engine; a session's drover identifier is a token of its
;; buffer, and its engine identifier the conversation the CLI records
;; for the terminal's process.
;;
;; Statuses come from Ghostel's program status, which Claude Code
;; reports through OSC 7501 (or OSC 9;4): a `blocked' record anywhere
;; waits for the user, then an `error' one fails, else the root record
;; tells whether the session works, is done or idles.  A turn that ended
;; while the terminal was in front of the user, or that the user looked
;; at since, is seen and idles.  vterm and eat terminals have no such
;; channel and always idle.
;;
;; The prompt starts sessions with the flags its options stand for,
;; switches a running session's model by typing /model, and pastes
;; prompts once the interface reads input.  The backend also tells the
;; `claude-code' engine the start flags and directory claude-code-ide
;; uses (`drover-claude-code-start-flags-function',
;; `drover-claude-code-project-directory-function'), and learns the
;; conversation of every terminal claude-code-ide starts.  The recap of
;; a session's last turn (:recap) is read from the end of its transcript.
;; While a session runs, the models of the catalog its CLI caches are
;; the `claude-code' engine's, a live source `drover-model' remembers.
;;
;; A session is forked into a new terminal resuming it with
;; --fork-session, compacted by typing /compact, and deleted by closing
;; its terminal, then deleting the files Claude Code keeps of it (see
;; `drover-claude-code-session-files').  claude-code-ide has no archive:
;; drover archives terminals' sessions itself, closing their terminals
;; once no turn runs (:close, :runs-in-buffer), and keeps them listed,
;; while archived, once their terminal is gone, which forking, deleting
;; and recapping need not.
;;
;; A conversation whose terminal closed stays listed, idle, by the
;; `claude-code' backend, which lists Claude Code's transcripts and is
;; installed with this one (see `drover-claude-code-transcripts');
;; opening it resumes it here (`drover-claude-code-ide-resume'), in the
;; directory it ran in.
;;
;; `drover' loads this file once claude-code-ide.el is loaded.

;;; Code:

(require 'drover-core)
(require 'drover-model)
(require 'drover-claude-code)
(require 'drover-claude-code-transcripts)
(require 'project)
(require 'seq)
(require 'subr-x)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 10))
  (load "drover-core" nil t))

;; Drover does not require claude-code-ide, so this file compiles
;; without it: what it uses of claude-code-ide is declared here, and
;; claude-code-ide is loaded only when the backend activates
;; (`drover-claude-code-ide--activate').  The accessors of its session
;; structure are declared by file only, as `check-declare' finds no
;; `cl-defstruct' accessor.
(defvar claude-code-ide-cli-extra-flags)
(declare-function claude-code-ide--get-working-directory "ext:claude-code-ide" ())
(declare-function claude-code-ide--start-session "ext:claude-code-ide" (&optional continue resume))
(declare-function claude-code-ide--touch-session "ext:claude-code-ide" (session))
(declare-function claude-code-ide-mcp--active-sessions "ext:claude-code-ide-mcp" ())
(declare-function claude-code-ide-mcp--mru-session "ext:claude-code-ide-mcp" (project-dir))
(declare-function claude-code-ide-mcp-session-buffer "ext:claude-code-ide-mcp" (session) t)
(declare-function claude-code-ide-mcp-session-process "ext:claude-code-ide-mcp" (session) t)
(declare-function claude-code-ide-mcp-session-project-dir "ext:claude-code-ide-mcp" (session) t)

;; Terminals are optional: none of them is required.
(defvar ghostel-program-status)
(defvar ghostel-buffer-name-function)
(defvar ghostel--pid)
(defvar vterm-buffer-name-string)
(defvar vterm-copy-mode)
(defvar eat-terminal)
(declare-function ghostel-paste-string "ext:ghostel" (string))
(declare-function ghostel-send-string "ext:ghostel" (string))
(declare-function ghostel-send-key "ext:ghostel" (key-name &optional mods))
(declare-function vterm-send-string "ext:vterm" (string &optional paste-p))
(declare-function vterm-send-return "ext:vterm" ())
(declare-function vterm-send-escape "ext:vterm" ())
(declare-function vterm-copy-mode "ext:vterm" (&optional arg))
(declare-function eat-term-send-string "ext:eat" (terminal string))
(declare-function ghostel-cursor-point "ext:ghostel" ())
(declare-function vterm--get-cursor-point "ext:vterm" ())

;;;; Options

(defvar-local drover-claude-code-ide-session-id nil
  "Identifier of the Claude Code session this terminal started, or nil.
Set when drover starts the session with an explicit --session-id, or
when the session's transcript is found after it started.")

(defcustom drover-claude-code-ide-terminal-modes '(ghostel-mode vterm-mode eat-mode)
  "Major modes of the terminals Claude Code sessions run in."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-claude-code-ide-buffer-regexp "\\`\\*claude\\*"
  "Regexp matching the names of the terminal buffers of Claude Code sessions.
A live terminal claude-code-ide.el does not track is a session when its
name matches.  A session's title is its buffer name without the match."
  :type 'regexp
  :group 'drover)

(defcustom drover-claude-code-ide-rename-format "*claude* %s"
  "Format of the buffer name of a renamed session; %s stands for the title."
  :type 'string
  :group 'drover)

(defvar drover-claude-code-ide-input-regexp
  ;; The no-break space is spelled by code point: a literal one would not
  ;; survive `whitespace-cleanup'.
  (concat "^\\(?:❯\\|>" (string #xa0) "\\)")
  "Regexp matching the input caret of Claude Code's interface at a line start.
The CLI draws \">\" and a no-break space, older ones \"❯\"; requiring
the no-break space keeps Markdown quotes of the transcript from
matching.")

(defconst drover-claude-code-ide--ready-timeout 30
  "Seconds a prompt waits for a terminal to read input before it is given up.")

(defconst drover-claude-code-ide--ready-interval 0.25
  "Seconds between two looks at a terminal not reading input yet.")

(defconst drover-claude-code-ide--submit-delay 0.1
  "Seconds between pasting a prompt and submitting it.
The interface takes a return arriving with the paste as part of it.")

(defconst drover-claude-code-ide--model-delay 0.3
  "Seconds the interface is given to switch models before the prompt follows.")

(defconst drover-claude-code-ide--capture-interval 2
  "Seconds between two looks for the conversation of a new terminal.")

(defconst drover-claude-code-ide--capture-timeout 600
  "Seconds drover looks for the conversation of a new terminal.")


(defconst drover-claude-code-ide--preview-lines 40
  "Lines of a terminal a preview shows.")

(defconst drover-claude-code-ide--session-flags-regexp
  "\\(?:\\`\\|[ \t]\\)\\(?:--resume\\|--continue\\|--session-id\\|-[rc]\\)\\(?:[= \t]\\|\\'\\)"
  "Regexp matching start flags that resume, continue or name a session.")

(defconst drover-claude-code-ide--session-id-flag-regexp
  "\\(?:\\`\\|[ \t]\\)--session-id[= \t]+['\"]?\\([[:xdigit:]]\\{8\\}\\(?:-[[:xdigit:]]\\{4\\}\\)\\{3\\}-[[:xdigit:]]\\{12\\}\\)"
  "Regexp matching a --session-id flag; group 1 is its identifier.")

;;;; Terminal buffers

(defvar drover-claude-code-ide--counter 0
  "Count of the tokens given to terminals.")

(defvar-local drover-claude-code-ide--token nil
  "Identifier of this terminal's session in drover, given when first listed.")
;; A token names a session for as long as its buffer lives.
(put 'drover-claude-code-ide--token 'permanent-local t)

(defvar-local drover-claude-code-ide--created nil
  "Time, as a float, at which claude-code-ide created this terminal, or nil.")

(defvar-local drover-claude-code-ide--project nil
  "The project of this terminal, as (DIRECTORY . NAME), NAME nil outside one.")


(defvar-local drover-claude-code-ide--submitting nil
  "Non-nil while text typed in this terminal waits to be submitted.")

(defun drover-claude-code-ide--terminal-p (buffer)
  "Return non-nil when BUFFER is live and in a terminal mode.
See `drover-claude-code-ide-terminal-modes'."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                drover-claude-code-ide-terminal-modes)))

(defun drover-claude-code-ide--live-p (mcp-session)
  "Return the live terminal of claude-code-ide's MCP-SESSION, or nil.
A session whose process died is gone, though claude-code-ide may not
have cleaned it up yet."
  (let ((buffer (claude-code-ide-mcp-session-buffer mcp-session))
        (process (claude-code-ide-mcp-session-process mcp-session)))
    (and (buffer-live-p buffer)
         (or (null process) (process-live-p process))
         buffer)))

(defun drover-claude-code-ide--tracked ()
  "Return the live sessions claude-code-ide tracks, as (BUFFER . MCP-SESSION)."
  (delq nil (mapcar (lambda (mcp-session)
                      (when-let* ((buffer (drover-claude-code-ide--live-p mcp-session)))
                        (cons buffer mcp-session)))
                    (claude-code-ide-mcp--active-sessions))))

(defun drover-claude-code-ide--orphan-p (buffer)
  "Return non-nil when BUFFER is a Claude terminal claude-code-ide does not track.
It is a live terminal whose name matches
`drover-claude-code-ide-buffer-regexp' and whose process runs."
  (and (drover-claude-code-ide--terminal-p buffer)
       (string-match-p drover-claude-code-ide-buffer-regexp (buffer-name buffer))
       (process-live-p (get-buffer-process buffer))))

(defun drover-claude-code-ide--claude-buffer-p (buffer)
  "Return non-nil when BUFFER is the terminal of a Claude Code session."
  (or (assq buffer (drover-claude-code-ide--tracked))
      (drover-claude-code-ide--orphan-p buffer)))

(defun drover-claude-code-ide--buffers ()
  "Return the terminals of Claude Code sessions, as (BUFFER . MCP-SESSION).
MCP-SESSION is claude-code-ide's session, nil for an untracked terminal.
Tracked sessions come first."
  (let ((tracked (drover-claude-code-ide--tracked)))
    (append tracked
            (mapcan (lambda (buffer)
                      (and (not (assq buffer tracked))
                           (drover-claude-code-ide--orphan-p buffer)
                           (list (cons buffer nil))))
                    (buffer-list)))))

(defun drover-claude-code-ide--mcp-session (buffer)
  "Return claude-code-ide's live session of terminal BUFFER, or nil."
  (cdr (assq buffer (drover-claude-code-ide--tracked))))

(defun drover-claude-code-ide--directory (buffer)
  "Return the directory of the session of terminal BUFFER.
It is the project directory of claude-code-ide's session, else the
terminal's directory."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((mcp-session (drover-claude-code-ide--mcp-session buffer)))
          (claude-code-ide-mcp-session-project-dir mcp-session))
        (buffer-local-value 'default-directory buffer)))))

(defun drover-claude-code-ide--project-name (buffer directory)
  "Return the name of the project of DIRECTORY, the directory of BUFFER, or nil.
It is kept in BUFFER: finding a project may walk a slow file system."
  (with-current-buffer buffer
    (let ((cached drover-claude-code-ide--project))
      (if (equal (car cached) directory)
          (cdr cached)
        (let ((name (when-let* ((project (project-current nil directory)))
                      (project-name project))))
          (setq drover-claude-code-ide--project (cons directory name))
          name)))))

(defun drover-claude-code-ide--title (buffer)
  "Return the title of the session of terminal BUFFER.
It is the buffer name without `drover-claude-code-ide-buffer-regexp'."
  (let* ((name (buffer-name buffer))
         (title (if (string-match drover-claude-code-ide-buffer-regexp name)
                    (string-trim (concat (substring name 0 (match-beginning 0))
                                         (substring name (match-end 0))))
                  name)))
    (if (string-empty-p title) name title)))

(defun drover-claude-code-ide--buffer (session)
  "Return the live terminal of SESSION, or nil."
  (let ((token (drover-session-id session)))
    (seq-find (lambda (buffer)
                (equal (buffer-local-value 'drover-claude-code-ide--token buffer) token))
              (buffer-list))))

(defun drover-claude-code-ide--live-buffer (session)
  "Return the live terminal of SESSION, signalling a `user-error' when gone."
  (or (drover-claude-code-ide--buffer session)
      (user-error "%s is gone" (drover-session-display-title session))))

;;;; Session identifiers

(defun drover-claude-code-ide--uuid ()
  "Return a random version 4 UUID, as Claude Code names its sessions."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random #x10000) (random #x10000) (random #x10000) (random #x1000)
          (logior #x8000 (random #x4000))
          (random #x10000) (random #x10000) (random #x10000)))

(defun drover-claude-code-ide-live-session-id (&optional buffer)
  "Return the identifier of the session terminal BUFFER runs, or nil.
BUFFER defaults to the current one.  It is the one the CLI records for
the terminal's process (see `drover-claude-code-pid-session-id'), else
`drover-claude-code-ide-session-id', so that a session resumed,
continued or switched within the CLI is followed; nothing is guessed."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((process (get-buffer-process (current-buffer)))
           (pid (and (process-live-p process)
                     ;; A Ghostel terminal's buffer process may stand in for
                     ;; the program, whose own PID Ghostel keeps.
                     (or (bound-and-true-p ghostel--pid) (process-id process))))
           (id (and (integerp pid) (drover-claude-code-pid-session-id pid))))
      (when id
        (setq-local drover-claude-code-ide-session-id id))
      drover-claude-code-ide-session-id)))

(defun drover-claude-code-ide-current-session-id (&optional buffer)
  "Return the identifier of the session terminal BUFFER runs, or a guess, or nil.
BUFFER defaults to the current one.  It is
`drover-claude-code-ide-live-session-id', else the newest transcript of
the terminal's directory."
  (let ((buffer (or buffer (current-buffer))))
    (or (drover-claude-code-ide-live-session-id buffer)
        (and (drover-claude-code-ide--claude-buffer-p buffer)
             (drover-claude-code-newest-session-id (drover-claude-code-ide--directory buffer))))))

;;;; Learning the session of a new terminal

(defvar drover-claude-code-ide--watchers nil
  "Timers looking for the conversations of new terminals.")

(defun drover-claude-code-ide--transcripts (bucket)
  "Return the names of the transcripts in directory BUCKET."
  (and (file-directory-p bucket)
       (directory-files bucket nil "\\.jsonl\\'")))

(defun drover-claude-code-ide--capture-session-id (buffer directory)
  "Learn the conversation of BUFFER, a terminal Claude Code started in DIRECTORY.
Every `drover-claude-code-ide--capture-interval' seconds, the CLI's
record of the terminal's process is read (see
`drover-claude-code-ide-live-session-id'), and the transcripts of
DIRECTORY are listed: one transcript new since the start is the
session's, never one of several, which concurrent sessions may have
created.  The search stops once the session is known, when the
terminal exits, or after `drover-claude-code-ide--capture-timeout'
seconds."
  (let* ((bucket (expand-file-name (drover-claude-code-encode-directory directory)
                                   (drover-claude-code-projects-directory)))
         (before (drover-claude-code-ide--transcripts bucket))
         (deadline (+ (float-time) drover-claude-code-ide--capture-timeout))
         (timer nil))
    (setq timer
          (run-with-timer
           drover-claude-code-ide--capture-interval drover-claude-code-ide--capture-interval
           (lambda ()
             (condition-case err
                 (when (drover-claude-code-ide--capture-step buffer bucket before deadline)
                   (drover-claude-code-ide--stop-watching timer))
               (error
                (drover-claude-code-ide--stop-watching timer)
                (drover-warn "Cannot learn the session of %s: %s"
                             (if (buffer-live-p buffer) (buffer-name buffer) "a Claude terminal")
                             (drover-error-message err)))))))
    (push timer drover-claude-code-ide--watchers)
    timer))

(defun drover-claude-code-ide--capture-step (buffer bucket before deadline)
  "Look once for the conversation of terminal BUFFER; return non-nil when done.
BUCKET is the directory of its transcripts, BEFORE the transcripts there
when it started, DEADLINE the time to give up at."
  (cond
   ((or (not (buffer-live-p buffer))
        (buffer-local-value 'drover-claude-code-ide-session-id buffer)
        (not (process-live-p (get-buffer-process buffer)))
        (> (float-time) deadline))
    t)
   ((drover-claude-code-ide-live-session-id buffer)
    (drover-backend-changed 'claude-code-ide)
    (drover-models-changed 'claude-code)
    t)
   (t
    (let ((new (seq-difference (drover-claude-code-ide--transcripts bucket) before)))
      (when (= (length new) 1)
        (with-current-buffer buffer
          (setq-local drover-claude-code-ide-session-id (file-name-base (car new))))
        (drover-backend-changed 'claude-code-ide)
        ;; By its first transcript, the CLI refreshed its catalog if stale.
        (drover-models-changed 'claude-code)
        t)))))

(defun drover-claude-code-ide--stop-watching (timer)
  "Cancel TIMER, looking for the conversation of a terminal."
  (cancel-timer timer)
  (setq drover-claude-code-ide--watchers (delq timer drover-claude-code-ide--watchers)))

(defun drover-claude-code-ide--track-terminal (create buffer-name working-dir &rest arguments)
  "Create a Claude terminal with CREATE, and learn its conversation.
CREATE is `claude-code-ide--create-terminal-session', called with
BUFFER-NAME, WORKING-DIR and ARGUMENTS; its value is returned.  A
--session-id in `claude-code-ide-cli-extra-flags' names the
conversation at once; otherwise it is looked for as it starts (see
`drover-claude-code-ide--capture-session-id')."
  (let ((result (apply create buffer-name working-dir arguments)))
    ;; Never let drover break the start of a session.
    (condition-case err
        (when-let* ((buffer (car-safe result))
                    ((buffer-live-p buffer)))
          (let ((flags (or claude-code-ide-cli-extra-flags "")))
            (with-current-buffer buffer
              (setq-local drover-claude-code-ide--created (float-time))
              (when (string-match drover-claude-code-ide--session-id-flag-regexp flags)
                (setq-local drover-claude-code-ide-session-id (downcase (match-string 1 flags)))))
            (unless (buffer-local-value 'drover-claude-code-ide-session-id buffer)
              (drover-claude-code-ide--capture-session-id buffer working-dir)))
          (drover-backend-changed 'claude-code-ide)
          (drover-models-changed 'claude-code))
      (error (drover-warn "Cannot follow the new Claude terminal %s: %s"
                          buffer-name (drover-error-message err))))
    result))

;;;; Statuses

(defvar-local drover-claude-code-ide--entries nil
  "The program status records this terminal's status followed when last noted.")

(defvar-local drover-claude-code-ide--state nil
  "The program status state of this terminal when last noted, or nil.")

(defvar-local drover-claude-code-ide--since nil
  "Time, as a float, at which this terminal's state began, or nil.")

(defvar-local drover-claude-code-ide--activity nil
  "Time, as a float, of the last status change of a turn of this terminal, or nil.
A turn working, waiting for the user, ending, or the terminal leaving
one; not the status the CLI tells as it starts, idle, which opening a
conversation again tells too: the conversation did nothing then.")

(defvar-local drover-claude-code-ide--seen nil
  "Non-nil when the user saw the turn this terminal's state ended.")

(defun drover-claude-code-ide--records ()
  "Return the program status records of the current terminal, newest first."
  (bound-and-true-p ghostel-program-status))

(defun drover-claude-code-ide--governing (records)
  "Return the entries of program status RECORDS a session's status follows.
As Ghostel's mode line: the `blocked' and `error' records, wherever they
are, else the root record, the program's own.  RECORDS are
`ghostel-program-status' entries, (ID . RECORD), newest first."
  (or (seq-filter (lambda (entry) (memq (plist-get (cdr entry) :state) '(blocked error)))
                  records)
      (when-let* ((root (assoc "" records)))
        (list root))))

(defun drover-claude-code-ide--state-of (entries)
  "Return the state the governing ENTRIES stand for, a symbol.
A `blocked' entry wins over an `error' one; without entries, `idle'."
  (let ((states (mapcar (lambda (entry) (plist-get (cdr entry) :state)) entries)))
    (cond ((memq 'blocked states) 'blocked)
          ((memq 'error states) 'error)
          (t (or (car states) 'idle)))))

(defun drover-claude-code-ide--note-status (seen)
  "Note the program status of the current terminal, SEEN by the user or not.
When the records its status follows changed, the state's start is noted
when it changed, the time of a turn's activity too (see
`drover-claude-code-ide--activity'), and the turn is unseen unless SEEN.
SEEN non-nil marks the state seen whatever changed."
  (let ((entries (drover-claude-code-ide--governing (drover-claude-code-ide--records))))
    (unless (equal entries drover-claude-code-ide--entries)
      (let ((state (drover-claude-code-ide--state-of entries))
            (before (or drover-claude-code-ide--state 'idle))
            (now (float-time)))
        (unless (eq state before)
          (setq-local drover-claude-code-ide--since now))
        (when (or (not (eq state 'idle)) (memq before '(working blocked)))
          (setq-local drover-claude-code-ide--activity now))
        (setq-local drover-claude-code-ide--entries entries)
        (setq-local drover-claude-code-ide--state state)
        (setq-local drover-claude-code-ide--seen nil)))
    (when seen
      (setq-local drover-claude-code-ide--seen t))))

(defun drover-claude-code-ide--unseen-p ()
  "Return non-nil when the current terminal's turn ended unseen."
  (and (memq drover-claude-code-ide--state '(done error))
       (not drover-claude-code-ide--seen)))

(defun drover-claude-code-ide--focused-buffers ()
  "Return the buffers in front of the user.
They are those of the selected windows of the frames that have the
focus."
  (delq nil (mapcar (lambda (frame)
                      (and (eq (frame-focus-state frame) t)
                           (window-buffer (frame-selected-window frame))))
                    (frame-list))))

(defun drover-claude-code-ide--focused-p (buffer)
  "Return non-nil when BUFFER is in front of the user.
See `drover-claude-code-ide--focused-buffers'."
  (memq buffer (drover-claude-code-ide--focused-buffers)))

(defun drover-claude-code-ide--status-changed (_id _old _new)
  "Note a change of the program status of the current terminal.
For `ghostel-program-status-functions'."
  (drover-with-reporting "Claude terminal"
    (drover-claude-code-ide--note-status
     (drover-claude-code-ide--focused-p (current-buffer)))
    (when (drover-claude-code-ide--claude-buffer-p (current-buffer))
      (drover-backend-changed 'claude-code-ide))))

(defun drover-claude-code-ide--see (buffer)
  "Mark the turn terminal BUFFER ended seen, if it is not yet."
  (with-current-buffer buffer
    (drover-claude-code-ide--note-status nil)
    (when (drover-claude-code-ide--unseen-p)
      (drover-claude-code-ide--note-status t)
      (drover-backend-changed 'claude-code-ide))))

(defun drover-claude-code-ide--note-focus (&rest _)
  "Mark seen the turns of the terminals in front of the user.
For `window-selection-change-functions', `window-buffer-change-functions'
and `after-focus-change-function'."
  (drover-with-reporting "Claude terminal windows"
    (dolist (buffer (drover-claude-code-ide--focused-buffers))
      ;; Only terminals whose status was ever noted can have a turn to see.
      (when (buffer-local-value 'drover-claude-code-ide--state buffer)
        (drover-claude-code-ide--see buffer)))))

(defun drover-claude-code-ide--request (record)
  "Return the request a `blocked' program status RECORD stands for."
  (let ((kind (plist-get record :kind)))
    (list :kind (if (eq kind 'question) 'question 'permission)
          :summary (or (plist-get record :msg)
                       (plist-get record :title)
                       (symbol-name (or kind 'permission))))))

(defun drover-claude-code-ide--status (buffer)
  "Return the status of terminal BUFFER, as (STATUS REQUESTS DETAIL)."
  (with-current-buffer buffer
    (drover-claude-code-ide--note-status nil)
    (let* ((entries drover-claude-code-ide--entries)
           (records (mapcar #'cdr entries)))
      (pcase drover-claude-code-ide--state
        ('blocked
         (list 'waiting
               (mapcar #'drover-claude-code-ide--request
                       (reverse (seq-filter (lambda (record) (eq (plist-get record :state) 'blocked))
                                            records)))
               nil))
        ('working
         (let ((record (car records)))
           (list 'working nil
                 (or (plist-get record :msg)
                     (when-let* ((progress (plist-get record :progress)))
                       (format "%d%%" progress))))))
        ('done (list (if drover-claude-code-ide--seen 'idle 'done) nil nil))
        ('error
         (if drover-claude-code-ide--seen
             (list 'idle nil nil)
           (let ((record (seq-find (lambda (record) (eq (plist-get record :state) 'error)) records)))
             (list 'failed nil (or (plist-get record :msg) (plist-get record :title))))))
        (_ (list 'idle nil nil))))))

;;;; Sessions

(defvar-local drover-claude-code-ide--last-run nil
  "What the terminal's session ran in its newest turn, as (STAMP . PLIST), or nil.
STAMP is the transcript read, its size and its modification time;
PLIST is what `drover-claude-code-ide--run' returns.")

(defun drover-claude-code-ide--run (id directory)
  "Return what session ID, started in DIRECTORY, ran in its newest turn.
The value is a plist of keyword arguments of `drover-session-create':
:model, the compact name Claude Code's catalog gives the model, else its
identifier, and :effort, the effort of the turn.  Whether that effort
is the model's default the transcript does not tell, nor which provider
served it, as the CLI may reach Claude through a cloud's.  The
transcript is read again only once it changed, so that listing sessions
costs a file status; the current buffer, the session's terminal, keeps
what was read.  Nil without a turn."
  (drover-with-reporting "Claude Code run"
    (when-let* ((file (drover-claude-code-transcript id directory))
                (attributes (file-attributes file)))
      (let ((stamp (list file (file-attribute-size attributes)
                         (file-attribute-modification-time attributes))))
        (unless (equal (car drover-claude-code-ide--last-run) stamp)
          (setq drover-claude-code-ide--last-run
                (cons stamp
                      (when-let* ((turn (drover-claude-code-last-turn file))
                                  ((car turn)))
                        (let ((model (drover-claude-code-model (car turn) nil (drover-claude-code-catalog))))
                          (list :model (drover-model-short-name
                                        (or (drover-model-name model) (drover-model-id model)))
                                :effort (and (cdr turn) (symbol-name (cdr turn)))))))))
        (cdr drover-claude-code-ide--last-run)))))

(defun drover-claude-code-ide--session-of (buffer)
  "Return the `drover-session' of terminal BUFFER."
  (let* ((directory (drover-claude-code-ide--directory buffer))
         (status (drover-claude-code-ide--status buffer)))
    (with-current-buffer buffer
      (unless drover-claude-code-ide--token
        (setq drover-claude-code-ide--token
              (format "terminal-%d" (cl-incf drover-claude-code-ide--counter))))
      (let* ((id (drover-claude-code-ide-live-session-id buffer))
             (times (drover-followed-times 'claude-code id drover-claude-code-ide--created
                                           drover-claude-code-ide--activity)))
        (apply #'drover-session-create
               :backend 'claude-code-ide
               :id drover-claude-code-ide--token
               :title (drover-claude-code-ide--title buffer)
               :directory directory
               :project (drover-claude-code-ide--project-name buffer directory)
               :branch (drover-git-branch directory)
               :status (nth 0 status)
               :requests (nth 1 status)
               :detail (nth 2 status)
               :since drover-claude-code-ide--since
               ;; A conversation resumed is as old as it is until a turn here.
               :updated (cdr times)
               :created (car times)
               :engine 'claude-code
               ;; Never a guess: drover merges sessions of one engine identifier.
               :engine-id id
               (and id directory (drover-claude-code-ide--run id directory)))))))

(defun drover-claude-code-ide--sessions ()
  "Return the sessions of the Claude terminals."
  (mapcar (lambda (entry) (drover-claude-code-ide--session-of (car entry)))
          (drover-claude-code-ide--buffers)))

(defun drover-claude-code-ide--buffer-session ()
  "Return the session of the current terminal, or nil when not Claude's."
  (when (drover-claude-code-ide--claude-buffer-p (current-buffer))
    (drover-claude-code-ide--session-of (current-buffer))))

;;;; Typing in terminals

(defun drover-claude-code-ide--leave-copy-mode ()
  "Leave vterm's copy mode in the current terminal, which input would confuse."
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1)))

(defun drover-claude-code-ide--type (string)
  "Type STRING in the current terminal."
  (cond ((derived-mode-p 'ghostel-mode) (ghostel-send-string string))
        ((derived-mode-p 'vterm-mode)
         (drover-claude-code-ide--leave-copy-mode)
         (vterm-send-string string))
        ((derived-mode-p 'eat-mode) (eat-term-send-string eat-terminal string))
        (t (error "%s is not a terminal drover knows" (buffer-name)))))

(defun drover-claude-code-ide--press (key)
  "Press KEY, `return' or `escape', in the current terminal."
  (cond ((derived-mode-p 'ghostel-mode) (ghostel-send-key (symbol-name key)))
        ((derived-mode-p 'vterm-mode)
         (drover-claude-code-ide--leave-copy-mode)
         (if (eq key 'return) (vterm-send-return) (vterm-send-escape)))
        (t (drover-claude-code-ide--type (if (eq key 'return) "\r" "\e")))))

(defun drover-claude-code-ide--paste (text)
  "Paste TEXT in the current terminal, bracketed, so newlines do not submit it."
  (if (derived-mode-p 'ghostel-mode)
      (ghostel-paste-string text)
    (drover-claude-code-ide--type (concat "\e[200~" text "\e[201~"))))

(defun drover-claude-code-ide--later (buffer delay function)
  "Call FUNCTION in terminal BUFFER after DELAY seconds, unless BUFFER dies.
FUNCTION is called with no argument; what it signals is reported."
  (run-at-time delay nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (condition-case err
                         (funcall function)
                       (error (drover-warn "%s: %s" (buffer-name) (drover-error-message err)))))))))

;;;; Sending

(defun drover-claude-code-ide-ready-p (buffer)
  "Return non-nil when the Claude Code interface in BUFFER reads input.
Its input caret (see `drover-claude-code-ide-input-regexp') shows only
once it does."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (save-excursion
           (goto-char (point-max))
           (re-search-backward drover-claude-code-ide-input-regexp nil t)))
       t))

(defun drover-claude-code-ide--takes-input-p (buffer)
  "Return non-nil when terminal BUFFER reads input and has nothing to submit."
  (and (drover-claude-code-ide-ready-p buffer)
       (not (buffer-local-value 'drover-claude-code-ide--submitting buffer))))

(defun drover-claude-code-ide--submit (buffer text)
  "Paste TEXT in terminal BUFFER, then submit it."
  (with-current-buffer buffer
    (setq-local drover-claude-code-ide--submitting t)
    (condition-case err
        (drover-claude-code-ide--paste text)
      (error (setq-local drover-claude-code-ide--submitting nil)
             (signal (car err) (cdr err))))
    (drover-claude-code-ide--later
     buffer drover-claude-code-ide--submit-delay
     (lambda ()
       (setq-local drover-claude-code-ide--submitting nil)
       (drover-claude-code-ide--press 'return)))))

(defun drover-claude-code-ide-send-text (buffer text)
  "Submit TEXT as a prompt to the Claude Code session of terminal BUFFER.
TEXT is pasted, bracketed, then submitted.  A session not ready yet
gets it once ready (see `drover-claude-code-ide-ready-p'); after 30
seconds, it is given up, with a message saying the prompt is in the
kill ring."
  (let ((name (if (buffer-live-p buffer) (buffer-name buffer) "The Claude terminal"))
        (deadline (+ (float-time) drover-claude-code-ide--ready-timeout))
        (timer nil))
    (cl-flet ((send ()
                (condition-case err
                    (drover-claude-code-ide--submit buffer text)
                  (error (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                                      name (drover-error-message err))))))
      (cond
       ((not (buffer-live-p buffer))
        (drover-warn "%s died before the prompt was sent; it is in the kill ring" name))
       ((drover-claude-code-ide--takes-input-p buffer) (send))
       (t
        ;; Bytes written while the CLI starts reach it in one chunk, the
        ;; return included in the paste: wait for its input box.
        (setq timer
              (run-at-time
               drover-claude-code-ide--ready-interval drover-claude-code-ide--ready-interval
               (lambda ()
                 (drover-with-reporting "Claude terminal"
                   (cond
                    ((not (buffer-live-p buffer))
                     (cancel-timer timer)
                     (drover-warn "%s died before the prompt was sent; it is in the kill ring" name))
                    ((condition-case nil
                         (drover-claude-code-ide--takes-input-p buffer)
                       (error nil))
                     (cancel-timer timer)
                     (send))
                    ((> (float-time) deadline)
                     (cancel-timer timer)
                     (drover-warn "%s was not ready after %ds; the prompt is in the kill ring"
                                  name drover-claude-code-ide--ready-timeout))))))))))))

(defun drover-claude-code-ide--send (session text _token)
  "Submit TEXT as a prompt to SESSION, once it reads input."
  (if-let* ((buffer (drover-claude-code-ide--buffer session)))
      (drover-claude-code-ide-send-text buffer text)
    (drover-warn "%s died before the prompt was sent; it is in the kill ring"
                 (drover-session-display-title session))))

;;;; Prompts sent later

(defun drover-claude-code-ide--cursor ()
  "Return the position of the current terminal's cursor, or nil."
  (cond ((derived-mode-p 'ghostel-mode) (ghostel-cursor-point))
        ((and (derived-mode-p 'vterm-mode) (fboundp 'vterm--get-cursor-point))
         (vterm--get-cursor-point))))

(defun drover-claude-code-ide-input-empty-p (buffer)
  "Return non-nil when the cursor of terminal BUFFER sits in an empty input box.
The box is one line, between two rules, starting with the input caret
\(see `drover-claude-code-ide-input-regexp').  The live cursor tells,
not the caret of an older screen still in the scrollback.  The context
Claude Code attaches to the next prompt, the file or lines selected in
Emacs, shows as a pill, [⧉ In NAME], at the start of the box: it is not
a draft, though what follows it is."
  (with-current-buffer buffer
    (let ((cursor (drover-claude-code-ide--cursor)))
      (and cursor (<= cursor (point-max))
           (save-excursion
             (goto-char cursor)
             (beginning-of-line)
             (and (looking-at (concat drover-claude-code-ide-input-regexp
                                      "\\(?:\\[⧉ [^]\n]+\\]\\)?[ \t]*$"))
                  (progn (forward-line -1) (looking-at "[─━-]\\{3,\\}"))
                  (progn (forward-line 2) (looking-at "[─━-]\\{3,\\}"))))))))

(defun drover-claude-code-ide--blocker (session)
  "Say why the terminal of SESSION cannot take a prompt now, or nil once it can.
It can once its cursor sits in an empty input box, so that no draft is
touched.  The backend's :blocker."
  (let ((buffer (drover-claude-code-ide--buffer session)))
    (cond ((not buffer) "the terminal is gone")
          ((buffer-local-value 'drover-claude-code-ide--submitting buffer)
           "another prompt is being submitted")
          ((not (drover-claude-code-ide-input-empty-p buffer))
           "the cursor is not in an empty input box"))))

(defun drover-claude-code-ide--deliver (session text then else)
  "Paste TEXT in the terminal of SESSION, submit it, then call THEN.
ELSE gets a message instead when the paste fails, or the terminal
exits before the return that submits it.  The backend's :submit."
  (let ((buffer (drover-claude-code-ide--buffer session))
        (failure nil))
    (if (not buffer)
        (funcall else "the terminal is gone")
      (with-current-buffer buffer
        (setq-local drover-claude-code-ide--submitting t)
        (condition-case err
            (drover-claude-code-ide--paste text)
          (error (setq-local drover-claude-code-ide--submitting nil)
                 (setq failure (drover-error-message err)))))
      (if failure
          (funcall else failure)
        (run-at-time
         drover-claude-code-ide--submit-delay nil
         (lambda ()
           (drover-with-reporting "Claude terminal"
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq-local drover-claude-code-ide--submitting nil)))
             (if (not (and (buffer-live-p buffer) (process-live-p (get-buffer-process buffer))))
                 (funcall else "the session exited before its prompt was submitted")
               (with-current-buffer buffer
                 (condition-case err
                     (drover-claude-code-ide--press 'return)
                   (error (setq failure (drover-error-message err)))))
               (if failure (funcall else failure) (funcall then))))))))))

(defun drover-claude-code-ide--apply-options (session options then)
  "Apply explicit OPTIONS to the running SESSION, then call THEN.
A model is switched by typing /model once the interface reads input;
an agent, which Claude Code takes at start only, and an effort without
a model are reported instead."
  (let* ((buffer (drover-claude-code-ide--buffer session))
         (name (if buffer (buffer-name buffer) (drover-session-display-title session)))
         (agent (plist-get options :agent))
         (model (plist-get options :model))
         (variant (plist-get options :variant))
         (pending nil))
    (condition-case err
        (progn
          (when agent
            (if-let* ((mode (drover-claude-code-agent-mode agent)))
                (drover-warn "%s enters %s mode only from its own prompt; keeping its current mode"
                             name mode)
              (drover-warn "%s takes its agent at session start; keeping its current agent instead of %s"
                           name agent)))
          (cond
           (model
            (let ((id (drover-model-id-for model 'claude-code 'claude-code-ide)))
              (cond
               ((null id)
                (drover-warn "%s cannot run %s; keeping its current model"
                             name (drover-model-label model)))
               ((not (and buffer (drover-claude-code-ide--takes-input-p buffer)))
                (drover-warn "%s is not ready for /model; keeping its current model" name))
               (t
                (with-current-buffer buffer
                  (drover-claude-code-ide--type (concat "/model " id))
                  (drover-claude-code-ide--press 'return)
                  ;; The prompt must not reach the interface before it switched.
                  (setq-local drover-claude-code-ide--submitting t))
                (setq pending t)
                (run-at-time drover-claude-code-ide--model-delay nil
                             #'drover-claude-code-ide--model-switched buffer then)))))
           (variant
            (drover-warn "Variant selection requires an explicit model for Claude Code"))))
      (error
       (drover-warn "Could not set the model of %s: %s; sending the prompt anyway"
                    name (drover-error-message err))))
    (unless pending
      (funcall then))))

(defun drover-claude-code-ide--model-switched (buffer then)
  "Let terminal BUFFER take input again after /model, then call THEN.
THEN is called even when BUFFER died meanwhile; what it signals is
reported."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local drover-claude-code-ide--submitting nil)))
  (condition-case err
      (funcall then)
    (error (drover-warn "Could not send the prompt after /model: %s; it is in the kill ring"
                        (drover-error-message err)))))

(defun drover-claude-code-ide--options (session)
  "Return what SESSION runs, as (:model MODEL :variant VARIANT), or nil.
It is what the newest turn of its transcript ran: a model or an effort
changed since shows from the next answer."
  (drover-with-reporting "Claude Code options"
    (when-let* ((buffer (drover-claude-code-ide--buffer session))
                (id (drover-claude-code-ide-current-session-id buffer))
                (file (drover-claude-code-transcript id (drover-session-directory session)))
                ((file-readable-p file))
                (turn (drover-claude-code-last-turn file))
                ((car turn)))
      (list :model (drover-claude-code-model (car turn) nil (drover-claude-code-catalog))
            :variant (cdr turn)))))

(defun drover-claude-code-ide--settable (_session)
  "Return what a running session can change: only its model."
  '(:model))

;;;; Models

(defun drover-claude-code-ide--models (_directory)
  "Return the models of the catalog Claude Code caches, while a session runs.
The CLI keeps its catalog of models on disk and fetches it again once
stale (see `drover-claude-code-catalog'): it is what a running session
offers, effort levels included.  Nil without a live session, or before
the CLI cached a catalog.  Default variants leave out the settings of
directories, as one list stands for every directory."
  (when (drover-claude-code-ide--buffers)
    (drover-claude-code-catalog-models (drover-claude-code-catalog))))

;;;; Starting sessions

(defun drover-claude-code-ide-start (directory &optional flags)
  "Start a Claude Code session in DIRECTORY, and return its terminal buffer.
FLAGS is a string of command line flags, by default
`claude-code-ide-cli-extra-flags'; unless they resume, continue or name
a session already, \"--session-id UUID\" is added, and the buffer's
`drover-claude-code-ide-session-id' set to UUID.  It starts a new
instance even where a session runs."
  (let* ((flags (string-trim (or flags claude-code-ide-cli-extra-flags "")))
         (id (unless (string-match-p drover-claude-code-ide--session-flags-regexp flags)
               (drover-claude-code-ide--uuid)))
         (default-directory (file-name-as-directory (expand-file-name directory)))
         (current-prefix-arg nil)
         (claude-code-ide-cli-extra-flags
          (if id (string-trim (concat flags " --session-id " id)) flags))
         (before (claude-code-ide-mcp--active-sessions)))
    (claude-code-ide--start-session)
    (when-let* ((mcp-session (seq-find (lambda (candidate) (not (memq candidate before)))
                                       (claude-code-ide-mcp--active-sessions)))
                (buffer (claude-code-ide-mcp-session-buffer mcp-session))
                ((buffer-live-p buffer)))
      (when id
        (with-current-buffer buffer
          (setq-local drover-claude-code-ide-session-id id)))
      buffer)))

(defun drover-claude-code-ide-resume (session-id directory)
  "Resume Claude Code session SESSION-ID in DIRECTORY, and return its terminal.
The CLI finds a conversation among those of the directory it starts in,
so it starts in DIRECTORY itself, as the conversation's project, though
claude-code-ide would start in the root of a project around it.  See
`drover-claude-code-ide-start'."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (project-find-functions (list (lambda (_directory) (cons 'transient directory)))))
    (when-let* ((buffer (drover-claude-code-ide-start
                         directory
                         (concat (or claude-code-ide-cli-extra-flags "")
                                 " --resume " (shell-quote-argument session-id)))))
      (with-current-buffer buffer
        (setq-local drover-claude-code-ide-session-id session-id))
      buffer)))

(defun drover-claude-code-ide--start-flags (options base)
  "Return the command line flags a new session started with OPTIONS takes.
OPTIONS is a start-options plist; BASE the flags it starts from.  An
agent of `drover-claude-code-agent-modes' becomes its
--permission-mode, another one --agent; :title becomes --name, the
model --model; the variant's effort, when Claude Code has that level,
comes last as --effort, so that it wins over one in BASE."
  (let* ((agent (plist-get options :agent))
         (mode (and agent (drover-claude-code-agent-mode agent)))
         (title (plist-get options :title))
         (model (plist-get options :model))
         (model-id (and model (drover-model-id-for model 'claude-code 'claude-code-ide)))
         (variant (plist-get options :variant))
         (levels (mapcar #'symbol-name drover-claude-code-effort-levels))
         (effort (seq-find (lambda (candidate) (member candidate levels))
                           (drover-model-effort-candidates model variant)))
         (flags (or base "")))
    (cl-flet ((prepend (flag value)
                (setq flags (string-trim (concat flag " " value " " flags)))))
      (cond (mode (prepend "--permission-mode" mode))
            ((and agent (not (string-empty-p agent)))
             (prepend "--agent" (shell-quote-argument agent))))
      (when (and title (not (string-empty-p title)))
        (prepend "--name" (shell-quote-argument title)))
      ;; Aliases carry brackets, such as "opus[1m]": a shell other than
      ;; Bash refuses an unmatched glob.
      (when (and model-id (not (string-empty-p model-id)))
        (prepend "--model" (shell-quote-argument model-id))))
    (if effort
        (string-trim (concat flags " --effort " effort))
      flags)))

(defun drover-claude-code-ide--create (directory engine options then else)
  "Start a session of ENGINE in DIRECTORY with start OPTIONS, and display it.
THEN gets its `drover-session'; ELSE a message, when ENGINE is not
Claude Code or no terminal starts."
  (if (not (eq engine 'claude-code))
      (funcall else (format "claude-code-ide runs Claude Code, not %s" engine))
    (let (buffer failure)
      (condition-case err
          (setq buffer (drover-claude-code-ide-start
                        directory (drover-claude-code-ide--start-flags
                                   options claude-code-ide-cli-extra-flags)))
        (error (setq failure (drover-error-message err))))
      (cond (failure (funcall else failure))
            ((buffer-live-p buffer) (funcall then (drover-claude-code-ide--session-of buffer)))
            (t (funcall else "Could not locate new Claude Code buffer"))))))

;;;; Actions

(defun drover-claude-code-ide--open (session other-window)
  "Display SESSION's terminal, in another window when OTHER-WINDOW is non-nil."
  (let ((buffer (drover-claude-code-ide--live-buffer session)))
    ;; The most recently used session is the one claude-code-ide picks.
    (when-let* ((mcp-session (drover-claude-code-ide--mcp-session buffer)))
      (claude-code-ide--touch-session mcp-session))
    (if other-window
        (pop-to-buffer buffer t)
      (pop-to-buffer-same-window buffer))))

(defun drover-claude-code-ide--respond (session)
  "Display SESSION's terminal, where its request waits for an answer."
  (drover-claude-code-ide--open session nil))

(defun drover-claude-code-ide--interrupt (session)
  "Stop the running turn of SESSION, pressing escape in its terminal."
  (with-current-buffer (drover-claude-code-ide--live-buffer session)
    (drover-claude-code-ide--press 'escape)))

(defun drover-claude-code-ide--mark-seen (session)
  "Mark the ended turn of SESSION seen."
  (drover-claude-code-ide--see (drover-claude-code-ide--live-buffer session)))

(defun drover-claude-code-ide--preview (session callback)
  "Call CALLBACK with the last lines of SESSION's terminal, or nil."
  (funcall callback
           (when-let* ((buffer (drover-claude-code-ide--buffer session)))
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-max))
                 (skip-chars-backward " \t\n")
                 (let ((end (point)))
                   (forward-line (- 1 drover-claude-code-ide--preview-lines))
                   (let ((text (buffer-substring-no-properties (line-beginning-position) end)))
                     (and (not (string-blank-p text)) text))))))))

(defun drover-claude-code-ide--recap (session callback)
  "Call CALLBACK with a recap of the last turn of SESSION.
It is read from the end of the session's transcript (see
`drover-claude-code-recap'), local and short, so at once; a session
whose terminal is gone, as one archived may be, by its conversation.
The backend's :recap."
  (funcall callback
           (or (drover-with-reporting "Claude Code recap"
                 (when-let* ((id (if-let* ((buffer (drover-claude-code-ide--buffer session)))
                                     (drover-claude-code-ide-current-session-id buffer)
                                   (drover-session-engine-id session)))
                             (file (drover-claude-code-transcript id (drover-session-directory session)))
                             ((file-readable-p file)))
                   (drover-claude-code-recap file)))
               (list :failure "No transcript of this session yet"))))

(defun drover-claude-code-ide--close (buffer)
  "Kill terminal BUFFER, and the Claude Code it runs, without asking.
Deleting and archiving close terminals so.  claude-code-ide cleans its
session up as the buffer goes, as when it stops a session itself.  An
archived session's conversation stays, which the CLI recorded as it
went."
  (drover-kill-buffer buffer))

(defun drover-claude-code-ide--close-session (session)
  "Close the terminal of SESSION without asking, as archiving does.
Drover calls it once no turn runs, as closing ends the turn.  What its
input box holds goes with it: it is the CLI's screen, which drover
cannot tell from the box's placeholder.  The value is nil: nothing goes
to the kill ring.  The backend's :close."
  (when-let* ((buffer (drover-claude-code-ide--buffer session)))
    (drover-claude-code-ide--close buffer)
    nil))

(defun drover-claude-code-ide--delete (session)
  "Delete SESSION for good: close its terminal, then delete its conversation.
The conversation's files are those Claude Code's SDK deletes (see
`drover-claude-code-session-files'), found before the terminal is
closed, so that a session whose transcript is not found keeps its
terminal; the terminal is closed first, as a running CLI would write
its transcript anew.  The backend's :delete."
  (let* ((buffer (drover-claude-code-ide--buffer session))
         (files (drover-claude-code-session-files (drover-session-engine-id session)
                                                  (drover-session-directory session))))
    (when (buffer-live-p buffer)
      (drover-claude-code-ide--close buffer))
    (drover-claude-code-delete-session-files files)
    (drover-backend-changed 'claude-code-ide)))

(defun drover-claude-code-ide-fork (session-id directory)
  "Start a Claude Code session forking session SESSION-ID, of DIRECTORY.
Return its terminal buffer.  It resumes SESSION-ID under a new
identifier, as \=`--fork-session' does, which it is given at start, so
that the buffer's `drover-claude-code-ide-session-id' names the fork
at once.  See `drover-claude-code-ide-start'."
  (let ((fork (drover-claude-code-ide--uuid)))
    (when-let* ((buffer (drover-claude-code-ide-start
                         directory
                         (concat (or claude-code-ide-cli-extra-flags "")
                                 " --resume " (shell-quote-argument session-id)
                                 " --fork-session --session-id " fork))))
      (with-current-buffer buffer
        (setq-local drover-claude-code-ide-session-id fork))
      buffer)))

(defun drover-claude-code-ide--fork (session)
  "Fork SESSION whole into a new session, in a terminal of its own.
The backend's :fork."
  (or (drover-claude-code-ide-fork (drover-session-engine-id session)
                                   (drover-session-directory session))
      (user-error "Could not locate the new Claude Code buffer")))

(defun drover-claude-code-ide--compact (session)
  "Compact the conversation of SESSION, typing /compact in its terminal.
Only once its turn ended, as far as its status tells, and its interface
reads input, as when switching models.  The backend's :compact."
  (let ((buffer (drover-claude-code-ide--live-buffer session))
        (title (drover-session-display-title session)))
    (when (drover-session-active-p session)
      (user-error "%s is in the middle of a turn; compact it once the turn ends" title))
    (unless (drover-claude-code-ide--takes-input-p buffer)
      (user-error "%s does not read input yet" title))
    (with-current-buffer buffer
      (drover-claude-code-ide--type "/compact")
      (drover-claude-code-ide--press 'return))))

(defconst drover-claude-code-ide--terminal-operations
  '(:open :respond :interrupt :mark-seen :preview :rename :compact)
  "Operations that need the session's terminal.")

(defun drover-claude-code-ide--supports (session operation)
  "Return non-nil when OPERATION applies to SESSION.
Those of `drover-claude-code-ide--terminal-operations' need its live
terminal, which a session archived may have lost; forking and deleting
need its conversation, which a terminal not started by drover tells
once Claude Code recorded it.  The backend's :supports."
  (cond ((memq operation drover-claude-code-ide--terminal-operations)
         (and (drover-claude-code-ide--buffer session) t))
        ((memq operation '(:fork :delete))
         (and (drover-session-engine-id session) (drover-session-directory session) t))
        (t t)))

(defun drover-claude-code-ide--new (directory)
  "Start a Claude Code session in DIRECTORY, and display it."
  (or (drover-claude-code-ide-start directory)
      (user-error "Could not locate new Claude Code buffer")))

(defun drover-claude-code-ide--rename (session title)
  "Rename SESSION's terminal after TITLE.
The terminal's title no longer renames it."
  (with-current-buffer (drover-claude-code-ide--live-buffer session)
    (rename-buffer (format drover-claude-code-ide-rename-format title) t)
    (cond ((derived-mode-p 'ghostel-mode) (setq-local ghostel-buffer-name-function nil))
          ((derived-mode-p 'vterm-mode) (setq-local vterm-buffer-name-string nil))))
  (drover-backend-changed 'claude-code-ide))

(defun drover-claude-code-ide-buffer-for-directory (dir)
  "Return the live terminal of a Claude Code session for DIR, or nil.
With several, the most recently used."
  (when-let* ((mcp-session (claude-code-ide-mcp--mru-session dir)))
    (drover-claude-code-ide--live-p mcp-session)))

;;;; The engine's front end

(defun drover-claude-code-ide--cli-flags ()
  "Return the flags claude-code-ide starts Claude Code with."
  claude-code-ide-cli-extra-flags)

(defun drover-claude-code-ide--working-directory (directory)
  "Return the directory claude-code-ide starts a session for DIRECTORY in."
  (let ((default-directory (file-name-as-directory (expand-file-name directory))))
    (claude-code-ide--get-working-directory)))

;;;; Installing

(defun drover-claude-code-ide--register ()
  "Register the `claude-code-ide' backend, and its live model source."
  (drover-register-backend
   'claude-code-ide
   :label "Claude Code IDE"
   :sessions #'drover-claude-code-ide--sessions
   :open #'drover-claude-code-ide--open
   :respond #'drover-claude-code-ide--respond
   :interrupt #'drover-claude-code-ide--interrupt
   :mark-seen #'drover-claude-code-ide--mark-seen
   :preview #'drover-claude-code-ide--preview
   :recap #'drover-claude-code-ide--recap
   :new #'drover-claude-code-ide--new
   :buffer #'drover-claude-code-ide--buffer
   :close #'drover-claude-code-ide--close-session
   :runs-in-buffer t
   :rename #'drover-claude-code-ide--rename
   :delete #'drover-claude-code-ide--delete
   :fork #'drover-claude-code-ide--fork
   :compact #'drover-claude-code-ide--compact
   :supports #'drover-claude-code-ide--supports
   :engines '(claude-code)
   :create #'drover-claude-code-ide--create
   :apply-options #'drover-claude-code-ide--apply-options
   :send #'drover-claude-code-ide--send
   :options #'drover-claude-code-ide--options
   :settable #'drover-claude-code-ide--settable
   :blocker #'drover-claude-code-ide--blocker
   :submit #'drover-claude-code-ide--deliver)
  ;; After live agent-shell shells, whose model identifiers their ACP
  ;; agent takes.
  (drover-register-model-source 'claude-code #'drover-claude-code-ide--models -5 t))

(defun drover-claude-code-ide--terminal-killed ()
  "Report a Claude terminal going away."
  (drover-with-reporting "Claude terminal"
    (when (or drover-claude-code-ide--token
              (drover-claude-code-ide--claude-buffer-p (current-buffer)))
      (drover-backend-changed 'claude-code-ide))))

(defun drover-claude-code-ide--terminal-exited (&rest _)
  "Report a terminal whose program exited."
  (drover-with-reporting "Claude terminal"
    (drover-backend-changed 'claude-code-ide)))

(defun drover-claude-code-ide--setup-terminal ()
  "Let the current terminal tell its session, when Claude's, and report it."
  (drover-with-reporting "Claude terminal"
    (setq-local drover-session-at-point-function #'drover-claude-code-ide--buffer-session)
    (add-hook 'kill-buffer-hook #'drover-claude-code-ide--terminal-killed nil t)
    (drover-backend-changed 'claude-code-ide)))

(defun drover-claude-code-ide--teardown-terminal ()
  "Undo `drover-claude-code-ide--setup-terminal' in the current terminal."
  (when (eq drover-session-at-point-function #'drover-claude-code-ide--buffer-session)
    (kill-local-variable 'drover-session-at-point-function))
  (remove-hook 'kill-buffer-hook #'drover-claude-code-ide--terminal-killed t))

(defun drover-claude-code-ide--hooks ()
  "Return the hooks the backend follows, as (HOOK . FUNCTION)."
  (append (mapcar (lambda (mode)
                    (cons (intern (format "%s-hook" mode)) #'drover-claude-code-ide--setup-terminal))
                  drover-claude-code-ide-terminal-modes)
          '((ghostel-program-status-functions . drover-claude-code-ide--status-changed)
            (ghostel-exit-functions . drover-claude-code-ide--terminal-exited)
            (vterm-exit-functions . drover-claude-code-ide--terminal-exited))))

(defun drover-claude-code-ide--add-hooks (&rest _)
  "Add the backend to the hooks of the terminals that are defined.
A hook is left alone until its package defines it: adding to a hook
customization option not yet defined would replace its default value."
  (drover-with-reporting "Claude terminal hooks"
    (pcase-dolist (`(,hook . ,function) (drover-claude-code-ide--hooks))
      (when (boundp hook)
        (add-hook hook function)))))

(defun drover-claude-code-ide--remove-hooks ()
  "Remove the backend from the hooks of the terminals."
  (pcase-dolist (`(,hook . ,function) (drover-claude-code-ide--hooks))
    ;; `remove-hook' defines an unbound hook, as `add-hook' does.
    (when (boundp hook)
      (remove-hook hook function))))

(defun drover-claude-code-ide--terminals ()
  "Return the live buffers in a terminal mode."
  (seq-filter #'drover-claude-code-ide--terminal-p (buffer-list)))

(defun drover-claude-code-ide--install ()
  "Register the backend and follow terminals, existing and future.
Unless set already, `drover-claude-code-start-flags-function' and
`drover-claude-code-project-directory-function' answer as
claude-code-ide starts sessions."
  (drover-claude-code-ide--register)
  (drover-register-resumer 'claude-code 'claude-code-ide #'drover-claude-code-ide-resume)
  (drover-claude-code-transcripts-install 'claude-code-ide)
  (unless drover-claude-code-start-flags-function
    (setq drover-claude-code-start-flags-function #'drover-claude-code-ide--cli-flags))
  (unless drover-claude-code-project-directory-function
    (setq drover-claude-code-project-directory-function
          #'drover-claude-code-ide--working-directory))
  (advice-add 'claude-code-ide--create-terminal-session :around
              #'drover-claude-code-ide--track-terminal)
  (drover-claude-code-ide--add-hooks)
  ;; Terminal packages may load later.
  (add-hook 'after-load-functions #'drover-claude-code-ide--add-hooks)
  (add-hook 'window-selection-change-functions #'drover-claude-code-ide--note-focus)
  (add-hook 'window-buffer-change-functions #'drover-claude-code-ide--note-focus)
  (add-function :after after-focus-change-function #'drover-claude-code-ide--note-focus)
  (dolist (buffer (drover-claude-code-ide--terminals))
    (with-current-buffer buffer
      (drover-claude-code-ide--setup-terminal))))

(defun drover-claude-code-ide--uninstall ()
  "Undo `drover-claude-code-ide--install'."
  (drover-unregister-backend 'claude-code-ide)
  (drover-unregister-resumer 'claude-code 'claude-code-ide)
  (drover-claude-code-transcripts-uninstall 'claude-code-ide)
  (drover-unregister-model-source 'claude-code #'drover-claude-code-ide--models)
  (when (eq drover-claude-code-start-flags-function #'drover-claude-code-ide--cli-flags)
    (setq drover-claude-code-start-flags-function nil))
  (when (eq drover-claude-code-project-directory-function
            #'drover-claude-code-ide--working-directory)
    (setq drover-claude-code-project-directory-function nil))
  (advice-remove 'claude-code-ide--create-terminal-session
                 #'drover-claude-code-ide--track-terminal)
  (drover-claude-code-ide--remove-hooks)
  (remove-hook 'after-load-functions #'drover-claude-code-ide--add-hooks)
  (remove-hook 'window-selection-change-functions #'drover-claude-code-ide--note-focus)
  (remove-hook 'window-buffer-change-functions #'drover-claude-code-ide--note-focus)
  (remove-function after-focus-change-function #'drover-claude-code-ide--note-focus)
  (mapc #'drover-claude-code-ide--stop-watching drover-claude-code-ide--watchers)
  (dolist (buffer (drover-claude-code-ide--terminals))
    (with-current-buffer buffer
      (drover-claude-code-ide--teardown-terminal))))

(defun drover-claude-code-ide--activate ()
  "Load claude-code-ide, register the backend and follow its terminals.
claude-code-ide is loaded here rather than when this file compiles,
which drover does without claude-code-ide.  `drover' loads this file
once claude-code-ide is loaded; an autoloaded function of this file
may load it first, and claude-code-ide with it."
  (require 'claude-code-ide)
  (drover-claude-code-ide--install))

(drover-claude-code-ide--activate)

(defun drover-claude-code-ide-unload-function ()
  "Forget the `claude-code-ide' backend and stop following terminals."
  (drover-claude-code-ide--uninstall)
  nil)

(provide 'drover-claude-code-ide)
;;; drover-claude-code-ide.el ends here
