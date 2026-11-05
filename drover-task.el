;;; drover-task.el --- Tasks: Org headings worked on by agents, in worktrees  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience, outlines, vc

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

;; A task is an Org heading, worked on by agent sessions in a place of
;; its own: a Git worktree on a branch of its own (`worktree'), the main
;; checkout of a repository (`in-place'), or a directory (`folder').
;; `drover-task-start', n in `drover-dispatch', starts the task of the
;; heading at point, or continues it: it resolves the repository
;; (DROVER_REPO, inherited, else the repository of the Org file, else
;; asked), makes the worktree from a fresh default branch, copies the
;; ignored files `.worktreeinclude' names, runs the repository's setup
;; visibly, writes DROVER_ properties and a LOGBOOK note on the heading,
;; and starts a session there, whose first prompt links the heading.
;; `drover-task-finish', F, merges the branch or opens a pull request,
;; as the repository's settings say (`drover-task-repositories'), then
;; cleans up: a snapshot of the worktree kept as a ref, the worktree
;; removed, the branch deleted once merged, the sessions archived, the
;; heading marked done on the user's word.  `drover-task-sweep', W,
;; lists the worktrees that may go, deleting nothing.
;;
;; Drover keeps no index of tasks: the heading is read when needed,
;; what was read kept in memory until its buffer changes.  A session
;; serves a task when its first prompt links the heading, as tasks
;; start them and `drover-prompt-org-task' did, when it runs in a
;; worktree a task made, whose Git directory says so, or when the user
;; linked it (`drover-task-link-session', I), the one thing drover keeps
;; of tasks in `drover-state-directory'.  `drover-task-mode' has lists
;; show the project of each session's task, its Org category, in a
;; colour of the theme (see `drover-task-palette'), group by task and
;; filter by project.  A task belongs to the host running its sessions:
;; another host starts and finishes it for this one (`drover-remote').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ansi-color)
(require 'ucs-normalize)
(require 'drover-core)
(require 'drover-ui)
(require 'drover-command)
(require 'drover-task-git)
(require 'drover-task-org)
(require 'drover-task-palette)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 20))
  (load "drover-core" nil t))

(declare-function drover-prompt-to-markdown "drover-prompt" (text))
(declare-function drover-prompt-org-task "drover-prompt" ())
(declare-function drover-remote-ask-task "drover-remote" (name request callback &optional seconds))
(declare-function drover-remote-open-started "drover-remote" (name result))
(declare-function sqlite-select "sqlite.c" (db query &optional values return-type))
(declare-function sqlite-open "sqlite.c" (&optional file readonly disable-trusted-schema))
(declare-function sqlite-close "sqlite.c" (db))
(declare-function sqlite-available-p "sqlite.c" ())
(defvar drover-prompt-new-targets)
(defvar drover-prompt-remote-functions)
(defvar drover-opencode-database)

;;;; Options

(defcustom drover-task-repositories nil
  "The repositories tasks work in, as an alist (NAME . PROPERTIES).
NAME is how DROVER_REPO names the repository, such as
\"10gen/secateur\", the same on every host.  PROPERTIES is a plist:

 :checkout   Its main checkout: a directory, or an alist (HOST
             . DIRECTORY) of its checkouts by host, `drover-host'.
 :kind       How tasks work in it: `worktree', on a branch of their
             own, the default, or `in-place', in the main checkout.
 :worktrees  Where its worktrees go: `sibling', beside the main
             checkout, as CHECKOUT-TOPIC; a directory; or nil, under
             `drover-task-worktree-root', as REPOSITORY/TOPIC.
 :branch-prefix
             What its branches begin with, such as \"val.leon/\".
 :target     The branch work merges into; its default branch by
             default.
 :finish     How a task's work ends: `merge', merged into the target
             in the main checkout; `merge-push', merged there, checked
             with :checks, and pushed; `pr', its branch pushed and a
             pull request opened with gh, merged by people, the task
             cleaned up once it is; nil, nothing merged.
 :checks     Shell commands run in the main checkout once merged,
             before pushing: a failure stops the push.
 :setup      Shell commands run in a new worktree, before any session
             starts there: a failure starts none.
 :teardown   Shell commands run in a worktree before it is removed.
 :include    Nil not to copy the ignored files `.worktreeinclude'
             names into new worktrees, which they are by default.

The repository of an Org file not named here is known by its
remote's name, works in worktrees under the root, and merges nothing."
  :type '(alist :key-type (string :tag "Name")
                :value-type (plist :options ((:checkout (choice directory (alist :key-type string :value-type directory)))
                                             (:kind (choice (const worktree) (const in-place)))
                                             (:worktrees (choice (const :tag "Under the root" nil)
                                                                 (const sibling) directory))
                                             (:branch-prefix string)
                                             (:target string)
                                             (:finish (choice (const :tag "Nothing merged" nil)
                                                              (const merge) (const merge-push) (const pr)))
                                             (:checks (repeat string))
                                             (:setup (repeat string))
                                             (:teardown (repeat string))
                                             (:include boolean))))
  :group 'drover)

(defcustom drover-task-worktree-root "~/local/worktrees/"
  "Where tasks' worktrees go, as REPOSITORY/TOPIC, unless their repository says.
A directory, or an alist (HOST . DIRECTORY) by `drover-host', such as a
short root on Windows, whose paths are short."
  :type '(choice directory (alist :key-type string :value-type directory))
  :group 'drover)

(defcustom drover-task-folder-root "~/local/projects/"
  "Where the folders of tasks working in a folder go, unless DROVER_FOLDER says."
  :type 'directory
  :group 'drover)

(defcustom drover-task-fetch-timeout 15
  "Seconds the fetch of a task's base may take, past which it starts from HEAD."
  :type 'number
  :group 'drover)

(defcustom drover-task-setup-timeout 1800
  "Seconds each setup, check or teardown command of a task may take."
  :type 'number
  :group 'drover)

(defcustom drover-task-slug-length 40
  "Characters at most of the name of a task's branch, its key apart."
  :type 'natnum
  :group 'drover)

(defcustom drover-task-harness nil
  "The new session a task starts, an entry of `drover-prompt-new-targets'.
Nil for the first of them installed, as the prompt offers them."
  :type '(choice (const :tag "The prompt's first" nil)
                 (plist :options ((:backend symbol) (:engine symbol) (:feature symbol) (:label string))))
  :group 'drover)

(defcustom drover-task-agent nil
  "The agent a task's sessions run, or nil for the harness's default."
  :type '(choice (const :tag "Default" nil) string)
  :group 'drover)

(defcustom drover-task-prompt-function #'drover-task-prompt
  "Function of a task's plan returning the first prompt of its sessions, in Org.
Its first line links the task's heading, as `[[id:ID][TITLE]]': that
link tells which task a session serves."
  :type 'function
  :group 'drover)

(defcustom drover-task-prompt-days 60
  "Days back the first prompts of OpenCode's sessions are read for their tasks."
  :type 'natnum
  :group 'drover)

(defconst drover-task-links-limit 1000
  "Sessions linked to tasks by hand that drover keeps, those linked last.")

(defconst drover-task--links-version 1
  "Version of the format of the file of the links of sessions to tasks.")

(defconst drover-task-id-regexp
  "\\[\\[id:\\([A-Za-z0-9][A-Za-z0-9_.:-]\\{0,99\\}\\)\\]\\|\\](id:\\([A-Za-z0-9][A-Za-z0-9_.:-]\\{0,99\\}\\))"
  "What a link to a heading looks like in a prompt, in Org or in Markdown.
The first group is the ID in Org, the second in Markdown.")

;;;; Repositories

(defun drover-task-repository (name)
  "Return the entry of `drover-task-repositories' named NAME, or nil."
  (assoc name drover-task-repositories))

(defun drover-task-checkout (entry &optional host)
  "Return the main checkout of the repository of ENTRY on HOST, or nil.
HOST is `drover-host' by default."
  (let* ((checkout (plist-get (cdr entry) :checkout))
         (directory (if (consp checkout)
                        (cdr (assoc (or host drover-host) checkout))
                      checkout)))
    (and (stringp directory) (file-name-as-directory (drover-task-canonical directory)))))

(defun drover-task-repository-of (directory)
  "Return the entry of the repository whose worktree DIRECTORY is in, or nil.
That of `drover-task-repositories' whose checkout here is its main
checkout, else one made for it: named after its remote, of no
settings but its checkout."
  (when-let* ((main (drover-task-git-main-checkout directory)))
    (or (seq-find (lambda (entry) (drover-task-same-file-p (drover-task-checkout entry) main))
                  drover-task-repositories)
        (list (or (drover-task-git-repository-name (drover-task-git-remote-url main))
                  (file-name-nondirectory (directory-file-name main)))
              :checkout (drover-task-canonical main)))))

(defun drover-task--root (setting)
  "Return the directory SETTING, a directory or an alist by host, names here."
  (let ((directory (if (consp setting) (cdr (assoc drover-host setting)) setting)))
    (and (stringp directory) (file-name-as-directory (drover-task-canonical directory)))))

;;;; Names

(defun drover-task-slug (title &optional key)
  "Return the name of a task's branch and worktree, of TITLE and its Jira KEY.
Lower case words of ASCII letters and digits, accents dropped, joined by
dashes, `drover-task-slug-length' characters at most cut between words,
after KEY when given."
  (let* ((title (if key
                    (let ((case-fold-search t))
                      (replace-regexp-in-string (regexp-quote key) " " (or title "")))
                  (or title "")))
         ;; Decomposed, a letter's accent is a character of its own, dropped.
         (title (replace-regexp-in-string "[^[:ascii:]]+" "" (downcase (ucs-normalize-NFKD-string title))))
         (words (split-string (replace-regexp-in-string "[^a-z0-9]+" " " title) " " t))
         (slug ""))
    (while (and words (<= (+ (length slug) 1 (length (car words))) drover-task-slug-length))
      (setq slug (if (string-empty-p slug) (car words) (concat slug "-" (car words)))
            words (cdr words)))
    (when (and (string-empty-p slug) words)
      (setq slug (substring (car words) 0 (min (length (car words)) drover-task-slug-length))))
    (progn
      (cond ((and key (string-empty-p slug)) key)
            (key (concat key "-" slug))
            ((string-empty-p slug) "task")
            (t slug)))))

(defun drover-task--worktree-path (entry checkout slug)
  "Return where the worktree of SLUG goes, of ENTRY's repository at CHECKOUT."
  (let ((setting (plist-get (cdr entry) :worktrees))
        (name (file-name-nondirectory (directory-file-name checkout))))
    (file-name-as-directory
     (cond
      ((eq setting 'sibling)
       (expand-file-name (concat name "-" slug) (file-name-directory (directory-file-name checkout))))
      (setting (expand-file-name slug (expand-file-name name (drover-task-canonical setting))))
      (t (expand-file-name slug (expand-file-name (replace-regexp-in-string "/" "-" (car entry))
                                                  (or (drover-task--root drover-task-worktree-root)
                                                      (drover-task-canonical "~/local/worktrees/")))))))))

;;;; Sessions and tasks

(defvar drover-task--links nil
  "Sessions linked to tasks by hand, a table KEY to (ID . TIME), nil unread.
KEY is the printed engine key of the session, else its key.")

(defvar drover-task--links-warned nil
  "Non-nil once a problem with the file of links was told.")

(defvar drover-task--noted (make-hash-table :test #'equal)
  "Sessions known to serve a task this Emacs session: key to ID.
Those tasks started, whose first prompt links them, and those a search
found; never kept.")

(defvar drover-task--directories (make-hash-table :test #'equal)
  "The task each worktree's Git directory names: GITDIR to (ID . TIME).")

(defvar drover-task--prompts (make-hash-table :test #'eq :weakness 'key)
  "The task the first prompt of each record links, ID or `none', by record.")

(defvar drover-task--oc-prompts (make-hash-table :test #'equal)
  "The task the first prompt of each OpenCode session links, by its identifier.")

(defvar drover-task--oc-read nil
  "When OpenCode's first prompts were read until, in milliseconds, or nil.")

(defun drover-task-links-file ()
  "Return the file of the sessions linked to tasks, in `drover-state-directory'."
  (expand-file-name "task-links.eld" drover-state-directory))

(defun drover-task--link-key (session)
  "Return the key SESSION is linked to a task by: its conversation's or its own."
  (format "%S" (or (drover-session-engine-key session) (drover-session-key session))))

(defun drover-task--links ()
  "Return the table of links, read from its file the first time."
  (or drover-task--links
      (let ((table (make-hash-table :test #'equal)))
        (condition-case err
            (dolist (entry (drover-read-data (drover-task-links-file) drover-task--links-version :links))
              (pcase entry
                (`(,(and key (pred stringp)) ,(and id (pred stringp)) ,(and time (pred numberp)))
                 (puthash key (cons id time) table))))
          (error
           (unless drover-task--links-warned
             (setq drover-task--links-warned t)
             (drover-warn "Could not read the sessions linked to tasks: %s" (drover-error-message err)))))
        (setq drover-task--links table))))

(defun drover-task--save-links ()
  "Write the links, the newest `drover-task-links-limit'."
  (let ((entries nil))
    (maphash (lambda (key entry) (push (list key (car entry) (cdr entry)) entries)) (drover-task--links))
    (setq entries (seq-take (sort entries :key #'caddr :reverse t) drover-task-links-limit))
    (condition-case err
        (drover-write-data (drover-task-links-file) "Sessions linked to tasks"
                           drover-task--links-version :links entries)
      (error (drover-warn "Could not keep the sessions linked to tasks: %s" (drover-error-message err))))))

(defun drover-task-link (session id)
  "Link SESSION to the task of ID, or unlink it with a nil ID, and keep it."
  (let ((key (drover-task--link-key session)))
    (if id
        (puthash key (cons id (float-time)) (drover-task--links))
      (remhash key (drover-task--links)))
    (remhash (drover-session-key session) drover-task--noted)
    (drover-task--changed)
    (drover-task--save-links)
    (drover-task--views-changed)))

(defun drover-task--directory-id (directory)
  "Return the task the worktree of DIRECTORY serves, as its Git directory says."
  (when-let* ((gitdir (drover-git-directory-cached directory))
              ;; A linked worktree's Git directory is in the common one's worktrees/.
              ((string-match-p "/worktrees/[^/]+/\\'" gitdir)))
    (let ((entry (gethash gitdir drover-task--directories))
          (now (float-time)))
      (if (and entry (< (- now (cdr entry)) drover-branch-ttl))
          (car entry)
        (let ((id (let ((file (expand-file-name "drover-task" gitdir)))
                    (and (file-readable-p file)
                         (with-temp-buffer
                           (insert-file-contents file nil 0 512)
                           (and (looking-at "\\([^ \t\n]\\{1,200\\}\\)$") (match-string 1)))))))
          (puthash gitdir (cons id now) drover-task--directories)
          id)))))

(defun drover-task-prompt-id (text)
  "Return the ID of the first heading TEXT links, or nil."
  (and (stringp text)
       (string-match drover-task-id-regexp text)
       (or (match-string 1 text) (match-string 2 text))))

(defun drover-task--prompt-id (session)
  "Return the task the first prompt of SESSION links, or nil.
The engines recording conversations tell it of theirs (:search-text),
and OpenCode's database of its sessions (see `drover-task--oc-read')."
  (let ((cached (gethash session drover-task--prompts)))
    (if cached
        (and (not (eq cached 'none)) cached)
      (let ((id (or (and (eq (drover-session-engine session) 'opencode)
                         (not (drover-session-host session))
                         (gethash (drover-session-engine-id session) drover-task--oc-prompts))
                    (and (not (drover-session-host session))
                         (drover-session-engine session)
                         (seq-some (lambda (backend)
                                     (when-let* ((function (drover-backend-property backend :search-text))
                                                 ((functionp function)))
                                       (drover-task-prompt-id
                                        (car (last (drover-with-reporting "task prompts"
                                                     (funcall function session)))))))
                                   (drover--recording-backends (drover-session-engine session)))))))
        (puthash session (or id 'none) drover-task--prompts)
        id))))

(defvar drover-task--generation 0
  "A count raised whenever what tells sessions' tasks may have changed.")

(defvar drover-task--ids (make-hash-table :test #'eq :weakness 'key)
  "The task of each record, as (GENERATION . ID), by record.
Views ask for it several times per row as they draw.")

(defun drover-task--changed ()
  "Note that the tasks of sessions may have changed."
  (cl-incf drover-task--generation))

(defun drover-task-id-of (session)
  "Return the ID of the heading of the task SESSION serves, or nil.
The task the user linked it to, else the one it was found serving this
Emacs session, else the one its worktree serves, else the one its first
prompt links.  Known of each record until something telling tasks
changes (see `drover-task--changed'), or a minute passed."
  (let ((known (gethash session drover-task--ids)))
    (if (eql (car known) drover-task--generation)
        (cdr known)
      (let ((id (or (car (gethash (drover-task--link-key session) (drover-task--links)))
                    (gethash (drover-session-key session) drover-task--noted)
                    (when-let* ((directory (drover-session-local-directory session)))
                      (drover-task--directory-id directory))
                    (drover-task--prompt-id session))))
        (puthash session (cons drover-task--generation id) drover-task--ids)
        id))))

(defun drover-task-note-session (key id)
  "Note that the session of KEY serves the task ID, this Emacs session."
  (puthash key id drover-task--noted)
  (drover-task--changed))

;;;; What views show of tasks

(defvar drover-task-mode)

(defvar drover-task--infos (make-hash-table :test #'equal)
  "What was read of each task's heading, by ID and ORIG_ID.
Each value is the plist of `drover-task-org-read' with :view, the task
as views show it, or (:missing TIME) for a heading not found then.")

(defvar drover-task--pending nil
  "IDs of the headings to read again, as views asked, in order.")

(defvar drover-task--timer nil
  "The timer reading pending headings, or nil.")

(defconst drover-task-missing-retry 600
  "Seconds before a heading not found is looked for again.")

(defun drover-task-project-face (project)
  "Return the face of PROJECT, its colour in the theme in force."
  (list :foreground (drover-task-project-color project)))

(defun drover-task--make-view (info)
  "Return the task of INFO as views show it (see `drover-session-task')."
  (let ((project (plist-get info :category)))
    (list :id (plist-get info :id)
          :title (plist-get info :title)
          :project project
          :face (and project (drover-task-project-face project)))))

(defun drover-task--schedule ()
  "Read the pending headings soon, while Emacs is idle."
  (unless (timerp drover-task--timer)
    (setq drover-task--timer (run-with-idle-timer 0.3 nil #'drover-task--read-pending))))

(defun drover-task--want (id)
  "Have the heading of ID read soon."
  (unless (member id drover-task--pending)
    (setq drover-task--pending (append drover-task--pending (list id)))
    (drover-task--schedule)))

(defun drover-task-view (id)
  "Return the task of ID as views show it, from what was read, or nil.
A heading not read, or changed since, is read soon, views then told."
  (let ((info (gethash id drover-task--infos)))
    (cond
     ((null info) (drover-task--want id) nil)
     ((plist-member info :missing)
      (when (> (- (float-time) (plist-get info :missing)) drover-task-missing-retry)
        (drover-task--want id))
      nil)
     (t (unless (drover-task-org-fresh-p info)
          (drover-task--want id))
        (plist-get info :view)))))

(defun drover-task--session-task (session)
  "Return the task SESSION serves as views show it, or nil.
The value of `drover-session-task-function' in `drover-task-mode'."
  (when-let* ((id (drover-task-id-of session)))
    (drover-task-view id)))

(defun drover-task--take (id info)
  "Keep INFO, read of the heading of ID; return non-nil when its view changed."
  (let* ((old (gethash id drover-task--infos))
         (view (drover-task--make-view info))
         (info (plist-put (copy-sequence info) :view view)))
    (dolist (name (delete-dups (delq nil (list id (plist-get info :id) (plist-get info :orig-id)))))
      (puthash name info drover-task--infos))
    (not (equal (plist-get old :view) view))))

(defun drover-task-read (marker &optional id)
  "Read the heading at MARKER, keep what it tells, and return it.
ID is the one it was asked by, perhaps its ORIG_ID."
  (let ((info (drover-task-org-read marker)))
    (when (drover-task--take (or id (plist-get info :id)) info)
      (drover-task--views-changed))
    (gethash (or id (plist-get info :id)) drover-task--infos)))

(defun drover-task--read-pending ()
  "Read the pending headings, for 30 ms at most, then tell views what changed.
From an idle timer: the headings of visited files and of files the ID
locations name are read now, opening their files; another is looked
for with ripgrep, one at a time."
  (drover-with-reporting "task headings"
    (setq drover-task--timer nil)
    (let ((deadline (+ (float-time) 0.03))
          (changed nil))
      (while (and drover-task--pending (< (float-time) deadline))
        (let* ((id (pop drover-task--pending))
               (marker (drover-task-org-marker id)))
          (if marker
              (progn
                (when (drover-task--take id (drover-task-org-read marker))
                  (setq changed t))
                (set-marker marker nil))
            (puthash id (list :missing (float-time)) drover-task--infos)
            (drover-task--search id))))
      (when changed
        (drover-task--views-changed))
      (when drover-task--pending
        (drover-task--schedule)))))

(defvar drover-task--searching nil
  "The ID ripgrep looks for now, or nil.")

(defvar drover-task--searches nil
  "IDs waiting for ripgrep, in order.")

(defun drover-task--search (id)
  "Look for the heading of ID in the Org files with ripgrep, one at a time."
  (unless (or (equal id drover-task--searching) (member id drover-task--searches))
    (setq drover-task--searches (append drover-task--searches (list id))))
  (unless drover-task--searching
    (when-let* ((next (pop drover-task--searches)))
      (setq drover-task--searching next)
      (drover-task-org-find
       next
       (lambda (marker)
         (drover-with-reporting "task heading search"
           (setq drover-task--searching nil)
           (when (and marker (drover-task--take next (drover-task-org-read marker)))
             (drover-task--views-changed))
           (when drover-task--searches
             (drover-task--search (pop drover-task--searches)))))))))

(defun drover-task--views-changed ()
  "Have views draw again what they show of tasks."
  (when (and drover-task-mode (fboundp 'drover-view-settings-changed))
    (drover-view-settings-changed)))

(defun drover-task--theme-changed (&rest _)
  "Colour projects anew from the theme now in force, and redraw views."
  (drover-with-reporting "task colours"
    (drover-task-palette-forget)
    (maphash (lambda (id info)
               (unless (plist-member info :missing)
                 (puthash id (plist-put info :view (drover-task--make-view info)) drover-task--infos)))
             drover-task--infos)
    (drover-task--views-changed)))

;;;; First prompts of OpenCode's sessions

(defun drover-task--oc-database ()
  "Return OpenCode's database file, readable, or nil."
  (and (boundp 'drover-opencode-database)
       (stringp drover-opencode-database)
       (file-readable-p drover-opencode-database)
       drover-opencode-database))

(defun drover-task--oc-read ()
  "Read which tasks the first prompts of OpenCode's sessions link, since last.
From its database, read only: the first prompt of each session written
since the last read, back `drover-task-prompt-days' days the first time,
that links a heading.  Views are told of the sessions found."
  (drover-with-reporting "task prompts of OpenCode"
    (when-let* (((fboundp 'sqlite-available-p))
                ((sqlite-available-p))
                (file (drover-task--oc-database)))
      (let ((db (sqlite-open file t))
            (found nil))
        (unwind-protect
            (let* ((since (or drover-task--oc-read
                              (* 1000 (floor (- (float-time) (* 86400 drover-task-prompt-days))))))
                   (rows (sqlite-select
                          db
                          "SELECT m.session_id, json_extract(m.data, '$.text'), m.time_created FROM session_message m WHERE m.type = 'user' AND m.time_created > ?1 AND m.data LIKE '%id:%' AND m.seq = (SELECT min(seq) FROM session_message WHERE session_id = m.session_id AND type = 'user') ORDER BY m.time_created LIMIT 5000"
                          (list since))))
              (pcase-dolist (`(,session ,text ,time) rows)
                (when (numberp time)
                  (setq since (max since time)))
                (when-let* (((stringp session))
                            (id (drover-task-prompt-id
                                 (and (stringp text) (substring text 0 (min (length text) 2000))))))
                  (unless (equal (gethash session drover-task--oc-prompts) id)
                    (puthash session id drover-task--oc-prompts)
                    (setq found t))))
              (setq drover-task--oc-read since))
          (sqlite-close db))
        (when found
          (clrhash drover-task--prompts)
          (drover-task--changed)
          (drover-task--views-changed))))))

;;;; Waiting in a command

(defun drover-task--wait (function &optional seconds what)
  "Call FUNCTION with a callback, wait for it in the command, return its value.
SECONDS, 60 by default, past which a `user-error' tells WHAT did not
answer; \\[keyboard-quit] stops waiting.  Timers and processes run
meanwhile."
  (let ((box (list 'waiting))
        (deadline (+ (float-time) (or seconds 60))))
    (funcall function (lambda (value) (setcar box value)))
    (while (and (eq (car box) 'waiting) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (when (eq (car box) 'waiting)
      (user-error "%s did not answer in time" (or what "Git")))
    (car box)))

(defun drover-task--git-now (directory arguments &optional seconds)
  "Run Git with ARGUMENTS in DIRECTORY, waited for; return its result."
  (drover-task--wait (lambda (callback) (drover-task-git directory arguments callback :timeout (or seconds 60)))
                     (+ 5 (or seconds 60))
                     (concat "git " (car arguments))))

;;;; Running commands visibly

(defun drover-task--commands-buffer (title)
  "Return the buffer telling the commands of the task TITLE, emptied."
  (let ((buffer (get-buffer-create (format "*drover task: %s*" (drover-bound-line title 60)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t)) (erase-buffer)))
    buffer))

(defun drover-task--insert (buffer &rest strings)
  "Insert STRINGS at the end of BUFFER, read only."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (apply #'insert strings))))))

(defun drover-task-run-commands (commands directory environment buffer callback)
  "Run the shell COMMANDS one after the other in DIRECTORY, telling BUFFER.
ENVIRONMENT is a list of VARIABLE=VALUE more.  Each runs in
`shell-file-name' within `drover-task-setup-timeout' seconds, its output
going to BUFFER as it comes.  CALLBACK gets nil once all succeeded,
else why the first failing did, after which none runs."
  (if (null commands)
      (funcall callback nil)
    (let* ((command (car commands))
           (default-directory (file-name-as-directory directory))
           (process-environment (append environment process-environment))
           (timer nil)
           (process nil))
      (drover-task--insert buffer (propertize (concat "$ " command "\n") 'face 'bold))
      (condition-case err
          (progn
            (setq process
                  (make-process
                   :name "drover-task" :noquery t :connection-type 'pipe
                   :command (list shell-file-name shell-command-switch command)
                   :filter (lambda (_process text)
                             (drover-with-reporting "task commands"
                               (drover-task--insert buffer (ansi-color-apply text))))
                   :sentinel (lambda (process _event)
                               (drover-with-reporting "task commands"
                                 (unless (process-live-p process)
                                   (when timer (cancel-timer timer))
                                   (let ((status (process-exit-status process)))
                                     (if (and (eq (process-status process) 'exit) (zerop status))
                                         (drover-task-run-commands (cdr commands) directory environment
                                                                   buffer callback)
                                       (drover-task--insert buffer (format "[exit %s]\n" status))
                                       (funcall callback (format "%s failed (exit %s)"
                                                                 (drover-bound-line command 80) status)))))))))
            (setq timer (run-at-time drover-task-setup-timeout nil
                                     (lambda ()
                                       (drover-with-reporting "task commands"
                                         (when (process-live-p process)
                                           (drover-task--insert buffer "[timed out]\n")
                                           (delete-process process)))))))
        (error (funcall callback (format "%s did not start: %s" (drover-bound-line command 80)
                                         (drover-error-message err))))))))

(defun drover-task--environment (plan)
  "Return the environment of the commands of PLAN: where, for which task."
  (list (concat "DROVER_ROOT_PATH=" (or (plist-get plan :checkout) ""))
        (concat "DROVER_WORKTREE_PATH=" (or (plist-get plan :directory) ""))
        (concat "DROVER_TASK=" (or (plist-get plan :id) ""))
        (concat "DROVER_TASK_NAME=" (or (plist-get plan :slug) ""))))

;;;; Making a task's place

(defun drover-task--put (context key value)
  "Set KEY of the steps' CONTEXT to VALUE."
  (puthash key value context))

(defun drover-task--get (context key)
  "Return KEY of the steps' CONTEXT."
  (gethash key context))

(defun drover-task--step-fetch (context next)
  "Fetch the target of the plan in CONTEXT, then resolve its base, for NEXT."
  (let* ((checkout (drover-task--get context :checkout))
         (base (drover-task--get context :base-choice))
         (target (drover-task--get context :target)))
    (cl-flet ((resolve (ref note)
                (drover-task-git checkout (list "rev-parse" "--verify" "--quiet" (concat ref "^{commit}"))
                                 (lambda (result)
                                   (if-let* ((sha (and (plist-get result :ok) (drover-task-git-line result))))
                                       (progn (drover-task--put context :base sha)
                                              (drover-task--put context :base-ref ref)
                                              (when note (drover-task--put context :base-note note))
                                              (funcall next nil))
                                     (funcall next (format "no commit %s in %s" ref checkout)))))))
      (cond
       ((and (stringp base) (not (eq base 'fresh)))
        (resolve base nil))
       ((eq base 'head) (resolve "HEAD" nil))
       (t
        (drover-task-git checkout (list "fetch" "--quiet" "--no-tags" "origin" target)
                         (lambda (result)
                           (if (plist-get result :ok)
                               (resolve (concat "origin/" target) nil)
                             (resolve "HEAD" (format "fetching origin failed (%s): based on HEAD"
                                                     (drover-task-git-why result)))))
                         :timeout drover-task-fetch-timeout))))))

(defun drover-task--step-carry (context next)
  "Note the uncommitted changes of CONTEXT's main checkout to carry, for NEXT."
  (if (not (drover-task--get context :carry))
      (funcall next nil)
    (drover-task-git (drover-task--get context :checkout) '("stash" "create" "drover: carried into a task")
                     (lambda (result)
                       (if (plist-get result :ok)
                           (progn (drover-task--put context :carried (drover-task-git-line result))
                                  (funcall next nil))
                         (funcall next (concat "noting the uncommitted changes failed: "
                                               (drover-task-git-why result))))))))

(defun drover-task--step-names (context next)
  "Find a branch and a worktree not taken yet for the plan in CONTEXT, for NEXT."
  (let ((checkout (drover-task--get context :checkout))
        (slug (drover-task--get context :slug))
        (prefix (or (drover-task--get context :branch-prefix) ""))
        (entry (drover-task--get context :entry)))
    (named-let try ((n 1))
      (let* ((name (if (= n 1) slug (format "%s-%d" slug n)))
             (branch (concat prefix name))
             (path (drover-task--worktree-path entry checkout name)))
        (if (> n 20)
            (funcall next (format "no free name for %s" slug))
          (drover-task-git checkout (list "rev-parse" "--verify" "--quiet" (concat "refs/heads/" branch))
                           (lambda (result)
                             (if (or (plist-get result :ok) (file-exists-p path))
                                 (try (1+ n))
                               (drover-task--put context :branch branch)
                               (drover-task--put context :worktree path)
                               (drover-task--put context :directory path)
                               (funcall next nil)))))))))

(defun drover-task--step-add (context next)
  "Add the worktree of the plan in CONTEXT, on its new branch, for NEXT."
  (let ((worktree (drover-task--get context :worktree)))
    (make-directory (file-name-directory (directory-file-name worktree)) t)
    (drover-task-git (drover-task--get context :checkout)
                     (list "worktree" "add" "-b" (drover-task--get context :branch)
                           (directory-file-name worktree) (drover-task--get context :base))
                     (lambda (result)
                       (if (not (plist-get result :ok))
                           (funcall next (concat "git worktree add failed: " (drover-task-git-why result)))
                         (drover-task-git-write-marker worktree (drover-task--get context :id) 'running)
                         (drover-task--put context :made t)
                         (funcall next nil)))
                     :timeout 900)))

(defun drover-task--step-lock (context next)
  "Lock the worktree of CONTEXT while its sessions run, for NEXT."
  (drover-task-git (drover-task--get context :checkout)
                   (list "worktree" "lock" "--reason"
                         (drover-bound-line (format "drover: task %s %s" (drover-task--get context :id)
                                                    (drover-task--get context :title))
                                            200)
                         (directory-file-name (drover-task--get context :worktree)))
                   (lambda (result)
                     (unless (plist-get result :ok)
                       (drover-warn "Could not lock %s: %s" (drover-task--get context :worktree)
                                    (drover-task-git-why result)))
                     (funcall next nil))))

(defun drover-task--step-apply-carried (context next)
  "Apply the uncommitted changes carried into the worktree of CONTEXT, for NEXT."
  (if-let* ((stash (drover-task--get context :carried))
            ((not (string-empty-p stash))))
      (drover-task-git (drover-task--get context :worktree) (list "stash" "apply" stash)
                       (lambda (result)
                         (funcall next (unless (plist-get result :ok)
                                         (concat "carrying the uncommitted changes failed: "
                                                 (drover-task-git-why result))))))
    (funcall next nil)))

(defun drover-task--include-patterns (checkout)
  "Return whether CHECKOUT has a .worktreeinclude, the file, or nil."
  (let ((file (expand-file-name ".worktreeinclude" checkout)))
    (and (file-readable-p file) file)))

(defun drover-task--step-include (context next)
  "Copy what `.worktreeinclude' names into the worktree of CONTEXT, for NEXT.
As Claude Code and Codex do: those its patterns, of .gitignore's syntax,
match and Git ignores, in the main checkout, a directory wholly
matched copied whole, nothing overwritten; and of those, only what the
worktree's branch ignores too, so that none may be committed."
  (let* ((checkout (drover-task--get context :checkout))
         (worktree (drover-task--get context :worktree))
         (file (drover-task--include-patterns checkout))
         (properties (cdr (drover-task--get context :entry))))
    (if (or (not file) (and (plist-member properties :include) (not (plist-get properties :include))))
        (funcall next nil)
      (drover-task-git
       checkout (list "ls-files" "--others" "--ignored" "--directory" "--no-empty-directory"
                      (concat "--exclude-from=" file))
       (lambda (result)
         (let ((paths (seq-take (split-string (or (plist-get result :output) "") "\n" t) 500)))
           (if (or (not (plist-get result :ok)) (null paths))
               (funcall next nil)
             (drover-task-git
              worktree (append (list "check-ignore" "--no-index" "--") paths)
              (lambda (ignored)
                (let ((copy (split-string (or (plist-get ignored :output) "") "\n" t))
                      (refused (seq-difference paths (split-string (or (plist-get ignored :output) "") "\n" t))))
                  (when refused
                    (drover-warn "%s: not copied, the task's branch does not ignore them: %s"
                                 (drover-task--get context :title)
                                 (string-join (seq-take refused 5) ", ")))
                  (named-let copy-next ((paths copy))
                    (if (null paths)
                        (funcall next nil)
                      (let* ((path (directory-file-name (car paths)))
                             (target (expand-file-name path worktree)))
                        (if (file-exists-p target)
                            (copy-next (cdr paths))
                          (make-directory (file-name-directory target) t)
                          (drover-task-run
                           checkout (if (file-executable-p "/bin/cp") "/bin/cp" "cp")
                           ;; macOS's own cp clones files on APFS, costing no room.
                           (append (and (eq system-type 'darwin) (file-executable-p "/bin/cp") (list "-c"))
                                   (list "-R" path target))
                           (lambda (copied)
                             (unless (plist-get copied :ok)
                               (drover-warn "Could not copy %s into the worktree: %s" path
                                            (drover-task-git-why copied)))
                             (copy-next (cdr paths)))
                           :timeout drover-task-setup-timeout)))))))))))))))

(defun drover-task--step-submodules (context next)
  "Initialize the submodules of the worktree of CONTEXT, if any, for NEXT."
  (let ((worktree (drover-task--get context :worktree)))
    (if (not (file-exists-p (expand-file-name ".gitmodules" worktree)))
        (funcall next nil)
      (drover-task-git worktree '("submodule" "update" "--init" "--recursive")
                       (lambda (result)
                         (funcall next (unless (plist-get result :ok)
                                         (concat "submodules: " (drover-task-git-why result)))))
                       :timeout drover-task-setup-timeout))))

(defun drover-task--step-setup (context next)
  "Run the setup of the repository in the worktree of CONTEXT, for NEXT.
Visibly unless CONTEXT is hidden, the worktree's marker telling the
outcome."
  (let ((commands (plist-get (cdr (drover-task--get context :entry)) :setup))
        (worktree (drover-task--get context :worktree)))
    (if (null commands)
        (progn (drover-task-git-write-marker worktree (drover-task--get context :id) 'ok)
               (funcall next nil))
      (let ((buffer (drover-task--commands-buffer (drover-task--get context :title))))
        (drover-task--put context :buffer buffer)
        (drover-task--insert buffer (format "Setting up %s for %s\n\n" worktree (drover-task--get context :title)))
        (unless (drover-task--get context :hidden)
          (display-buffer buffer '(nil (inhibit-same-window . t))))
        (drover-task-run-commands
         commands worktree (drover-task--environment (drover-task--plan context)) buffer
         (lambda (why)
           (drover-task-git-write-marker worktree (drover-task--get context :id) (if why 'failed 'ok))
           (drover-task--insert buffer (if why (format "\nSetup failed: %s\n" why) "\nSetup done.\n"))
           (funcall next (and why (concat "setup: " why)))))))))

(defun drover-task--plan (context)
  "Return the plan CONTEXT holds, as a plist."
  (let ((plan nil))
    (maphash (lambda (key value) (setq plan (plist-put plan key value))) context)
    plan))

(defun drover-task--context (plan)
  "Return a context of steps holding PLAN, a plist."
  (let ((context (make-hash-table :test #'eq)))
    ;; The first of a key wins, as for `plist-get'.
    (cl-loop for (key value) on plan by #'cddr
             when (eq (gethash key context 'none) 'none) do (puthash key value context))
    context))

(defun drover-task-prepare (plan callback &optional on-made)
  "Make the place PLAN works in, then call CALLBACK with how it went.
PLAN is a plist (see `drover-task--plan-for').  For a worktree: the
target fetched, the base resolved, uncommitted changes noted if
carried, free names found, the worktree added and locked, its marker
written, ON-MADE called with the plan then, the changes carried
applied, the ignored files copied, submodules initialized, the setup
run.  A folder is made if missing.  CALLBACK gets (PLAN . WHY), WHY nil
on success; the plan tells what was made, :made non-nil once the
worktree exists."
  (let ((context (drover-task--context plan)))
    (pcase (plist-get plan :kind)
      ('worktree
       (drover-task-git-steps
        (list #'drover-task--step-fetch #'drover-task--step-carry #'drover-task--step-names
              #'drover-task--step-add #'drover-task--step-lock
              (lambda (context next)
                (when on-made
                  (drover-with-reporting "task made"
                    (funcall on-made (drover-task--plan context))))
                (funcall next nil))
              #'drover-task--step-apply-carried #'drover-task--step-include
              #'drover-task--step-submodules #'drover-task--step-setup)
        context
        (lambda (why) (funcall callback (cons (drover-task--plan context) why)))))
      ('folder
       (let ((directory (plist-get plan :directory)))
         (condition-case err
             (progn (make-directory directory t)
                    (when on-made (funcall on-made plan))
                    (funcall callback (cons plan nil)))
           (error (funcall callback (cons plan (drover-error-message err)))))))
      (_
       (when on-made (funcall on-made plan))
       (funcall callback (cons plan nil))))))

;;;; Starting sessions

(defun drover-task--harness ()
  "Return the new session tasks start, an entry of `drover-prompt-new-targets'."
  (or drover-task-harness
      (progn (require 'drover-prompt) nil)
      (seq-find (lambda (entry)
                  (let ((feature (plist-get entry :feature)))
                    (or (null feature) (featurep feature) (locate-library (symbol-name feature)))))
                drover-prompt-new-targets)
      (user-error "No new session to start: see `drover-prompt-new-targets'")))

(defun drover-task-prompt (plan)
  "Return the first prompt of the sessions of PLAN, in Org.
Its first line links the task's heading; then where the session works,
and the heading's body."
  (let ((link (format "[[id:%s][%s]]" (plist-get plan :id)
                      (replace-regexp-in-string "[][]" "" (plist-get plan :title))))
        (body (plist-get plan :body)))
    (concat "Work on " link (if-let* ((key (plist-get plan :jira)))
                                (format " (%s)" key)
                              "")
            ".\n\n"
            (pcase (plist-get plan :kind)
              ('worktree
               (format "You work in the worktree %s, on the branch %s, made from %s at %s: commit your work on that branch there, and touch neither the main checkout %s nor its branches.%s"
                       (abbreviate-file-name (plist-get plan :directory)) (plist-get plan :branch)
                       (plist-get plan :base-ref) (substring (or (plist-get plan :base) "") 0
                                                             (min 12 (length (or (plist-get plan :base) ""))))
                       (abbreviate-file-name (or (plist-get plan :checkout) ""))
                       (if (plist-get plan :carried) " The main checkout's uncommitted changes were carried into it." "")))
              ('in-place
               (format "You work in %s, the repository's main checkout." (abbreviate-file-name (plist-get plan :directory))))
              (_ (format "You work in %s." (abbreviate-file-name (plist-get plan :directory)))))
            (if body (concat "\n\nThe task:\n\n" body) ""))))

(defun drover-task-start-session (plan &optional hidden callback)
  "Start a session for PLAN in its directory, sent its first prompt.
Of `drover-task--harness', in no window when HIDDEN.  CALLBACK, if any,
gets the session, or a string telling why it did not start.  The
session is noted as serving the task."
  (let* ((target (drover-task--harness))
         (backend (plist-get target :backend))
         (engine (plist-get target :engine))
         (text (funcall drover-task-prompt-function plan))
         (callback (or callback #'ignore)))
    (when-let* ((feature (plist-get target :feature)))
      (require feature))
    (let ((create (drover-backend-property backend :create)))
      (if (not (and (memq backend (drover-backends)) (functionp create)
                    (memq engine (drover-backend-engines backend))))
          (funcall callback (format "%s starts no %s session" backend engine))
        (let ((started
               (lambda (session)
                 (drover-with-reporting "task session"
                   (drover-task-note-session (drover-session-key session) (plist-get plan :id))
                   (let ((send (drover-backend-property session :send))
                         (markdown (condition-case nil
                                       (progn (require 'drover-prompt) (drover-prompt-to-markdown text))
                                     (error text))))
                     (if (not (functionp send))
                         (funcall callback "its backend takes no prompt")
                       (save-current-buffer
                         (if hidden
                             (save-window-excursion
                               (let ((display-buffer-overriding-action
                                      '(display-buffer-no-window (allow-no-window . t))))
                                 (funcall send session markdown nil)))
                           (funcall send session markdown nil)))
                       (drover-task--views-changed)
                       (funcall callback session))))))
              (failed (lambda (why)
                        (drover-with-reporting "task session"
                          (funcall callback (format "%s" why))))))
          (if hidden
              (save-window-excursion
                (let ((display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t))))
                  (funcall create (plist-get plan :directory) engine
                           (list :title (plist-get plan :title) :agent drover-task-agent :hidden t)
                           started failed)))
            (funcall create (plist-get plan :directory) engine
                     (list :title (plist-get plan :title) :agent drover-task-agent)
                     started failed)))))))

;;;; Planning a start

(defun drover-task--heading-marker ()
  "Return a marker at the heading point is on or under, in Org, or signal."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (when (drover-task-org-refused-p (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
    (user-error "Tasks are not read from this file: see `drover-task-refused-files'"))
  (org-with-wide-buffer
   (when (org-before-first-heading-p)
     (user-error "Point is not under an Org heading"))
   (org-back-to-heading t)
   (point-marker)))

(defun drover-task--ensure-id (marker)
  "Return the ID of the heading at MARKER, made and saved if it has none."
  (require 'org-id)
  (with-current-buffer (marker-buffer marker)
    (or (org-entry-get marker "ID")
        (let ((clean (not (buffer-modified-p))))
          (prog1 (org-with-point-at marker (org-id-get-create))
            (when (and clean buffer-file-name)
              (drover-task-org--save (current-buffer))))))))

(defun drover-task--read-kind (choices default)
  "Read how a task works, among CHOICES, symbols, DEFAULT first."
  (intern (completing-read (format "Work in (default %s): " default)
                           (mapcar #'symbol-name choices) nil t nil nil (symbol-name default))))

(defun drover-task--hosts ()
  "Return the hosts a task may start on, as (LABEL . NAME), this one's nil."
  (cons (cons (format "%s (this host)" drover-host) nil)
        (and drover-prompt-remote-functions
             (mapcar (lambda (host) (cons (cdr host) (car host)))
                     (funcall (plist-get drover-prompt-remote-functions :hosts))))))

(defun drover-task--resolve-repository (info marker ask)
  "Return the repository of the task of INFO at MARKER, an entry, or `folder'.
DROVER_REPO, inherited, names it, one asked for among several; else the
repository of the heading's file; else a folder the heading names;
else it is asked, among `drover-task-repositories' and a folder, and
saved as DROVER_REPO on the heading if the user wishes.  With ASK, the
choice among several is always asked."
  (let* ((repos (plist-get info :repos))
         (file (plist-get info :file))
         (name (cond ((cdr repos)
                      (completing-read "Repository: " repos nil t nil nil (car repos)))
                     (repos (car repos)))))
    (cond
     (name (or (drover-task-repository name)
               (user-error "No repository %s in `drover-task-repositories'" name)))
     ((and file (not ask) (drover-task-repository-of (file-name-directory file))))
     ((and (plist-get info :folder) (not ask)) 'folder)
     (t
      (let* ((names (mapcar #'car drover-task-repositories))
             (own (and file (drover-task-repository-of (file-name-directory file))))
             (folder "A folder")
             (choices (append (and own (list (car own))) names (list folder)))
             (choice (completing-read "Work on this task in: " (delete-dups choices) nil t nil nil
                                      (car choices))))
        (cond
         ((equal choice folder) 'folder)
         ((and own (equal choice (car own))) own)
         (t (prog1 (drover-task-repository choice)
              (when (y-or-n-p (format "Save DROVER_REPO %s on this heading? " choice))
                (drover-task-org-write marker (list (cons "DROVER_REPO" choice))))))))))))

(defun drover-task--plan-for (marker ask)
  "Return the plan starting the task at MARKER, asking what is left to decide.
With ASK, as with a prefix argument, how it works, on which host and
from which base are asked too."
  (let* ((id (drover-task--ensure-id marker))
         (info (drover-task-read marker id))
         (repository (drover-task--resolve-repository info marker ask))
         (host (when ask
                 (let ((hosts (drover-task--hosts)))
                   (when (cdr hosts)
                     (cdr (assoc (completing-read "Host: " (mapcar #'car hosts) nil t nil nil (caar hosts))
                                 hosts))))))
         (plan (list :id (plist-get info :id) :title (plist-get info :title) :jira (plist-get info :jira)
                     :category (plist-get info :category)
                     :body (with-current-buffer (marker-buffer marker)
                             (org-with-point-at marker (drover-task-org-body)))
                     :slug (drover-task-slug (plist-get info :title) (plist-get info :jira))
                     :host host)))
    (if (eq repository 'folder)
        (let ((folder (or (and (not ask) (plist-get info :folder))
                          (file-name-as-directory
                           (read-directory-name "Folder: " nil
                                                (or (plist-get info :folder)
                                                    (expand-file-name (plist-get plan :slug)
                                                                      (drover-task--root drover-task-folder-root))))))))
          (when (and host (not (plist-get info :folder)))
            (user-error "A task in a folder starts on this host"))
          (append (list :kind 'folder :directory (drover-task-canonical folder)) plan))
      (let* ((entry repository)
             (properties (cdr entry))
             (kind (or (plist-get info :kind) (plist-get properties :kind) 'worktree))
             (kind (if ask (drover-task--read-kind '(worktree in-place) kind) kind))
             (checkout (and (null host) (drover-task-checkout entry))))
        (when (and (null host) (not (and checkout (file-directory-p checkout))))
          (user-error "No checkout of %s on this host: see `drover-task-repositories'" (car entry)))
        (let* ((target (or (plist-get properties :target)
                           (and checkout (drover-git-default-branch checkout))
                           "main"))
               (base (if (and ask (eq kind 'worktree))
                         (let ((choice (completing-read
                                        "Base: " (list (format "origin/%s, fetched" target) "HEAD" "A branch…")
                                        nil t nil nil (format "origin/%s, fetched" target))))
                           (cond ((equal choice "HEAD") 'head)
                                 ((equal choice "A branch…") (read-string "Branch or commit: "))
                                 (t 'fresh)))
                       'fresh))
               (carry (and ask (eq kind 'worktree) (y-or-n-p "Carry the main checkout's uncommitted changes? "))))
          (append (list :kind kind :repo (car entry) :entry entry :checkout checkout :target target
                        :branch-prefix (or (plist-get properties :branch-prefix) "")
                        :base-choice base :carry carry
                        :directory (and (eq kind 'in-place) checkout))
                  plan))))))

;;;; Starting

(defun drover-task--attempt-properties (plan &optional host)
  "Return the properties of the heading telling the place of PLAN, run on HOST."
  (if (eq (plist-get plan :kind) 'worktree)
      (list (cons "DROVER_WORKTREE" (abbreviate-file-name (plist-get plan :worktree)))
            (cons "DROVER_BRANCH" (plist-get plan :branch))
            (cons "DROVER_BASE" (plist-get plan :base))
            (cons "DROVER_ATTEMPT_REPO" (plist-get plan :repo))
            (cons "DROVER_HOST" (or host drover-host)))
    (list (cons "DROVER_DIRECTORY" (abbreviate-file-name (plist-get plan :directory)))
          (cons "DROVER_ATTEMPT_REPO" (plist-get plan :repo))
          (cons "DROVER_HOST" (or host drover-host)))))

(defun drover-task--made-note (plan &optional host)
  "Return the LOGBOOK note telling where PLAN works, on HOST."
  (if (eq (plist-get plan :kind) 'worktree)
      (format "Worktree %s on branch %s, from %s at %s, on %s%s."
              (abbreviate-file-name (plist-get plan :worktree)) (plist-get plan :branch)
              (plist-get plan :base-ref) (substring (plist-get plan :base) 0 (min 12 (length (plist-get plan :base))))
              (or host drover-host)
              (if-let* ((note (plist-get plan :base-note))) (concat "; " note) ""))
    (format "Working in %s, on %s." (abbreviate-file-name (plist-get plan :directory)) (or host drover-host))))

(defun drover-task--start-here (marker plan)
  "Start PLAN, the task at MARKER, here: its place, then a session there."
  (let ((title (plist-get plan :title)))
    (message "Starting %s…" title)
    (drover-task-prepare
     plan
     (lambda (outcome)
       (drover-with-reporting "task start"
         (pcase-let ((`(,plan . ,why) outcome))
           (cond
            ((and why (not (plist-get plan :made)) (eq (plist-get plan :kind) 'worktree))
             (drover-warn "Task %s did not start: %s" title why))
            (why
             (drover-task-org-write marker nil (format "Setup failed: %s" why))
             (drover-warn "Task %s: %s; no session started: fix it, then %s continues"
                          title why (substitute-command-keys "\\[drover-task-start]"))
             (when-let* ((buffer (plist-get plan :buffer)))
               (display-buffer buffer)))
            (t
             (drover-task-start-session
              plan nil
              (lambda (session)
                (drover-with-reporting "task start"
                  (if (stringp session)
                      (drover-warn "Task %s: no session started: %s" title session)
                    (drover-message "Task %s started in %s" title
                                    (abbreviate-file-name (plist-get plan :directory))))))))))))
     (lambda (plan)
       (drover-task-org-write marker (drover-task--attempt-properties plan)
                              (drover-task--made-note plan))
       (drover-task-read marker)))))

(defun drover-task--start-there (marker plan)
  "Have the host of PLAN start the task at MARKER: its place, a session."
  (require 'drover-remote)
  (let* ((host (plist-get plan :host))
         (title (plist-get plan :title))
         (request (list :kind "start" :task (plist-get plan :id) :title title
                        :repo (plist-get plan :repo) :mode (symbol-name (plist-get plan :kind))
                        :slug (plist-get plan :slug) :jira (or (plist-get plan :jira) :false)
                        :base (let ((base (plist-get plan :base-choice)))
                                (if (stringp base) base (symbol-name (or base 'fresh))))
                        :carry (if (plist-get plan :carry) t :false)
                        :body (or (plist-get plan :body) :false))))
    (message "Starting %s on %s…" title host)
    (drover-remote-ask-task
     host request
     (lambda (reply error)
       (drover-with-reporting "task start"
         (let* ((field (lambda (key) (drover-task--remote-string (and (listp reply) (plist-get reply key)) 1024)))
                (made (or (funcall field :worktree) (funcall field :directory))))
           (when made
             (let ((there (list :kind (plist-get plan :kind) :worktree (funcall field :worktree)
                                :directory (funcall field :directory) :branch (funcall field :branch)
                                :base (or (funcall field :base) "") :base-ref (or (funcall field :base-ref) "its base")
                                :repo (plist-get plan :repo))))
               (drover-task-org-write marker (drover-task--attempt-properties there host)
                                      (drover-task--made-note there host))
               (drover-task-read marker)))
           (cond
            (error (drover-warn "Task %s did not start on %s: %s" title host error))
            ((funcall field :failure)
             (drover-warn "Task %s on %s: %s" title host (drover-bound-line (funcall field :failure))))
            (t
             (when-let* ((key (plist-get reply :key))
                         ((and (vectorp key) (= (length key) 2)
                               (stringp (aref key 0)) (string-match-p "\\`[a-z][a-z0-9-]\\{0,40\\}\\'" (aref key 0))
                               (or (stringp (aref key 1)) (integerp (aref key 1))))))
               (drover-task-note-session (cons (intern (concat "remote:" host)) (cons (intern (aref key 0)) (aref key 1)))
                                         (plist-get plan :id)))
             (drover-message "Task %s started on %s" title host))))))
     (+ drover-task-setup-timeout 120))))

;;;###autoload
(defun drover-task-start (&optional ask)
  "Start the task of the Org heading at point, or continue it.
A heading whose task works somewhere already continues: one of its
sessions opens, or a new one starts there.  Otherwise its place is made
\(see `drover-task-prepare') and a session started there, sent a prompt
linking the heading.  With a prefix argument ASK, how it works, on which
host and from which base are asked.  From a session serving a task, in
a list or its buffer, that task continues.  `drover-prompt-org-task'
only writes a prompt for the task."
  (interactive "P")
  (drover-start)
  (if (derived-mode-p 'org-mode)
      (let* ((marker (drover-task--heading-marker))
             (id (org-entry-get marker "ID"))
             (info (and id (drover-task-read marker id))))
        (if (and info (plist-get info :attempts) (not ask))
            (drover-task--continue marker info)
          (let ((plan (drover-task--plan-for marker ask)))
            (if (plist-get plan :host)
                (drover-task--start-there marker plan)
              (drover-task--start-here marker plan)))))
    (let* ((session (drover-session-at-point))
           (id (or (drover-task-id-of session) (user-error "This session serves no task")))
           (marker (or (drover-task-org-marker id) (user-error "The heading of task %s is not found" id))))
      (drover-task--continue marker (drover-task-read marker id)))))

;;;; Continuing

(defun drover-task-sessions (info)
  "Return the live sessions serving the task INFO tells of, most recent first."
  (let ((ids (delq nil (list (plist-get info :id) (plist-get info :orig-id)))))
    (sort (seq-filter (lambda (session) (member (drover-task-id-of session) ids))
                      (drover-sessions))
          :key (lambda (session) (- (or (drover-session-updated session) 0))))))

(defun drover-task--attempt-here (info)
  "Return the attempt of INFO to act on, asked among several."
  (let ((attempts (plist-get info :attempts)))
    (if (cdr attempts)
        (let ((choice (completing-read "Attempt: " (mapcar (lambda (attempt) (plist-get attempt :directory)) attempts)
                                       nil t)))
          (seq-find (lambda (attempt) (equal (plist-get attempt :directory) choice)) attempts))
      (car attempts))))

(defun drover-task--plan-of-attempt (info attempt)
  "Return the plan of the task INFO tells of, working as ATTEMPT does."
  (let* ((entry (drover-task-repository (plist-get attempt :repo)))
         (checkout (and entry (drover-task-checkout entry))))
    (list :id (plist-get info :id) :title (plist-get info :title) :jira (plist-get info :jira)
          :slug (drover-task-slug (plist-get info :title) (plist-get info :jira))
          :target (or (plist-get (cdr entry) :target)
                      (and checkout (file-directory-p checkout) (drover-git-default-branch checkout))
                      "main")
          :kind (if (plist-get attempt :worktree) 'worktree
                  (if entry 'in-place 'folder))
          :repo (plist-get attempt :repo) :entry entry
          :checkout checkout
          :directory (and (plist-get attempt :directory)
                          (file-name-as-directory (drover-task-canonical (plist-get attempt :directory))))
          :worktree (and (plist-get attempt :worktree)
                         (file-name-as-directory (drover-task-canonical (plist-get attempt :worktree))))
          :branch (plist-get attempt :branch) :base (plist-get attempt :base)
          :base-ref (or (plist-get attempt :branch) "its base")
          :host (plist-get attempt :host) :pr (plist-get attempt :pr))))

(defun drover-task--continue (marker info)
  "Continue the task INFO tells of, at MARKER: open a session, or start one."
  (let* ((attempt (drover-task--attempt-here info))
         (plan (append (drover-task--plan-of-attempt info attempt)
                       (list :body (with-current-buffer (marker-buffer marker)
                                     (org-with-point-at marker (drover-task-org-body))))))
         (host (plist-get plan :host))
         (here (or (null host) (equal host drover-host)))
         (sessions (drover-task-sessions info))
         (new (format "New session in %s" (abbreviate-file-name (or (plist-get plan :directory) "?"))))
         (setup "Run the setup again, then start a session")
         (missing (and here (plist-get plan :directory) (not (file-directory-p (plist-get plan :directory)))))
         (marker-info (and here (not missing) (plist-get plan :worktree)
                           (drover-task-git-read-marker (plist-get plan :worktree))))
         (choices (append (mapcar (lambda (session)
                                    (cons (format "Open %s" (drover-session-display-title session)) session))
                                  sessions)
                          (list (cons new 'new))
                          (and (eq (plist-get marker-info :setup) 'failed) (list (cons setup 'setup)))))
         (choice (if missing
                     (if (y-or-n-p (format "%s is gone: make it again from its branch %s? "
                                           (plist-get plan :directory) (plist-get plan :branch)))
                         'recreate
                       (user-error "Task %s left as it is" (plist-get info :title)))
                   (cdr (assoc (completing-read (format "Task %s: " (plist-get info :title))
                                                (mapcar #'car choices) nil t nil nil (caar choices))
                               choices)))))
    (pcase choice
      ((pred drover-session-p) (drover-open choice))
      ('new
       (if here
           (drover-task-start-session plan nil
                                      (lambda (session)
                                        (when (stringp session)
                                          (drover-warn "No session started for %s: %s" (plist-get info :title) session))))
         (drover-task--remote-session plan)))
      ('setup
       (let ((context (drover-task--context plan)))
         (drover-task-git-steps (list #'drover-task--step-setup) context
                                (lambda (why)
                                  (drover-with-reporting "task setup"
                                    (if why
                                        (drover-warn "Task %s: %s" (plist-get info :title) why)
                                      (drover-task-start-session plan)))))))
      ('recreate (drover-task--recreate marker plan)))))

(defun drover-task--recreate (marker plan)
  "Make the worktree of PLAN, the task at MARKER, again from its branch."
  (let ((worktree (plist-get plan :worktree))
        (checkout (plist-get plan :checkout)))
    (unless (and checkout (file-directory-p checkout))
      (user-error "No checkout of %s on this host" (plist-get plan :repo)))
    (drover-task-git checkout (list "worktree" "add" "--force" (directory-file-name worktree) (plist-get plan :branch))
                     (lambda (result)
                       (drover-with-reporting "task recreate"
                         (if (not (plist-get result :ok))
                             (drover-warn "Could not make %s again: %s" worktree (drover-task-git-why result))
                           (drover-task-git-write-marker worktree (plist-get plan :id) 'running)
                           (drover-task-org-write marker nil (format "Worktree %s made again from %s."
                                                                     (abbreviate-file-name worktree)
                                                                     (plist-get plan :branch)))
                           (let ((context (drover-task--context plan)))
                             (drover-task-git-steps (list #'drover-task--step-lock #'drover-task--step-include
                                                          #'drover-task--step-submodules #'drover-task--step-setup)
                                                    context
                                                    (lambda (why)
                                                      (drover-with-reporting "task recreate"
                                                        (if why
                                                            (drover-warn "Task %s: %s" (plist-get plan :title) why)
                                                          (drover-task-start-session plan)))))))))
                     :timeout 900)))

(defun drover-task--remote-session (plan)
  "Start a new session of PLAN on its host, in its directory there."
  (require 'drover-remote)
  (let* ((host (plist-get plan :host))
         (target (or (seq-find (lambda (target) (equal (plist-get target :host) host))
                               (funcall (plist-get drover-prompt-remote-functions :targets) 5))
                     (user-error "%s starts no session for this host" host)))
         (text (funcall drover-task-prompt-function plan))
         (result (funcall (plist-get drover-prompt-remote-functions :start)
                          host target (plist-get plan :directory)
                          (progn (require 'drover-prompt) (drover-prompt-to-markdown text))
                          (list :title (plist-get plan :title) :agent drover-task-agent)
                          #'ignore)))
    (when-let* ((key (plist-get result :key)))
      (drover-task-note-session (cons (intern (concat "remote:" host)) key) (plist-get plan :id))
      (drover-remote-open-started host result))))

;;;; Finishing

(defun drover-task--finish-context ()
  "Return the task to finish here, as (MARKER . INFO), or signal."
  (if (derived-mode-p 'org-mode)
      (let* ((marker (drover-task--heading-marker))
             (id (or (org-entry-get marker "ID") (user-error "This heading is no task"))))
        (cons marker (drover-task-read marker id)))
    (let* ((session (drover-session-at-point))
           (id (or (drover-task-id-of session) (user-error "This session serves no task")))
           (marker (or (drover-task-org-marker id)
                       (user-error "The heading of task %s is not found: visit its file" id))))
      (cons marker (drover-task-read marker id)))))

(defun drover-task--snapshot-name (plan)
  "Return the ref the snapshot of PLAN's worktree goes to."
  (concat "refs/drover/snapshots/"
          (replace-regexp-in-string "[^A-Za-z0-9._/-]" "-" (or (plist-get plan :branch) (plist-get plan :slug) "task"))))

;;;###autoload
(defun drover-task-finish ()
  "Finish the task of the Org heading at point, or of the session at point.
As its repository's settings say (:finish of `drover-task-repositories'):
its branch merged into the target in the main checkout, `merge'; merged,
checked and pushed, `merge-push'; pushed and a pull request opened,
`pr', the task cleaned up once the pull request is merged, when
finished again.  Conflicts are looked for first, and stop it.  Then the
task is cleaned up: a snapshot of its worktree kept as a ref, its
teardown run, the worktree unlocked and removed, its branch deleted
once merged, its sessions archived, and the heading marked done if you
said so.  What would be lost, uncommitted changes, is asked about once,
first.  A task of another host is finished by that host."
  (interactive)
  (drover-start)
  (pcase-let* ((`(,marker . ,info) (drover-task--finish-context))
               (attempt (or (drover-task--attempt-here info)
                            (user-error "Task %s works nowhere" (plist-get info :title))))
               (plan (drover-task--plan-of-attempt info attempt))
               (host (plist-get plan :host)))
    (if (and host (not (equal host drover-host)))
        (drover-task--finish-there marker info plan)
      (drover-task--finish-here marker info plan))))

(defun drover-task--dirty-files (worktree)
  "Return the files of WORKTREE with uncommitted changes, waited for."
  (let ((result (drover-task--git-now worktree '("status" "--porcelain=v1" "--untracked-files=all"))))
    (unless (plist-get result :ok)
      (user-error "Git status failed in %s: %s" worktree (drover-task-git-why result)))
    (drover-task-git-parse-status (plist-get result :output))))

(defun drover-task--pr-state (plan)
  "Return the state of PLAN's pull request, a plist (:state :head), waited for."
  (let ((result (drover-task--wait
                 (lambda (callback)
                   (drover-task-run (plist-get plan :worktree) drover-task-gh-program
                                    (list "pr" "view" (plist-get plan :pr) "--json" "state,headRefOid")
                                    callback :timeout 30))
                 40 "gh pr view")))
    (unless (plist-get result :ok)
      (user-error "The pull request's state is unknown: %s" (drover-task-git-why result)))
    (let ((data (json-parse-string (plist-get result :output) :object-type 'plist)))
      (list :state (plist-get data :state) :head (plist-get data :headRefOid)))))

(defun drover-task--finish-here (marker info plan)
  "Finish PLAN, the task INFO tells of at MARKER, on this host."
  (let* ((title (plist-get info :title))
         (entry (plist-get plan :entry))
         (method (and (plist-get plan :worktree) (plist-get (cdr entry) :finish)))
         (worktree (plist-get plan :worktree))
         (exists (and worktree (file-directory-p worktree)))
         (pr-state (and (eq method 'pr) (plist-get plan :pr) exists (drover-task--pr-state plan)))
         (cleanup (or (not (eq method 'pr))
                      (and pr-state (not (equal (plist-get pr-state :state) "OPEN")))))
         (_ (when (and pr-state (equal (plist-get pr-state :state) "OPEN"))
              (user-error "%s's pull request is still open: %s" title (plist-get plan :pr))))
         (_ (when (and pr-state (equal (plist-get pr-state :state) "CLOSED")
                       (not (y-or-n-p (format "%s's pull request was closed unmerged: clean the task up (a snapshot stays)? "
                                              title))))
              (user-error "Task %s left as it is" title)))
         (dirty (and exists cleanup (drover-task--dirty-files worktree)))
         (_ (when (and dirty
                       (not (yes-or-no-p
                             (format "%s has uncommitted changes: %s%s.  They go to %s, then the worktree goes: finish? "
                                     (abbreviate-file-name worktree)
                                     (string-join (seq-take dirty 5) ", ")
                                     (if (> (length dirty) 5) (format " and %d more" (- (length dirty) 5)) "")
                                     (drover-task--snapshot-name plan)))))
              (user-error "Task %s left as it is" title)))
         (done (and cleanup (not (plist-get info :done))
                    (y-or-n-p (format "Then mark %s %s? " title
                                      (with-current-buffer (marker-buffer marker)
                                        (if (member "DONE" org-done-keywords) "DONE" (car org-done-keywords)))))))
         (plan (append (list :dirty dirty :pr-state pr-state :mark-done done :method method
                             :cleanup cleanup :exists exists)
                       plan)))
    (message "Finishing %s…" title)
    (drover-task-finish-plan
     plan nil
     (lambda (outcome)
       (drover-with-reporting "task finish"
         (drover-task--finished marker info plan outcome))))))

(defun drover-task--finished (marker info plan outcome)
  "Note on the heading at MARKER how PLAN, the task of INFO, ended: OUTCOME.
OUTCOME is a plist: :failure, why it stopped; :note, what was done;
:pr, a pull request opened; :removed, non-nil once its worktree went."
  (let ((title (plist-get info :title)))
    (cond
     ((plist-get outcome :failure)
      (when-let* ((note (plist-get outcome :note)))
        (drover-task-org-write marker nil note))
      (drover-warn "Task %s not finished: %s" title (plist-get outcome :failure)))
     ((plist-get outcome :pr)
      (drover-task-org-write marker (list (cons "DROVER_PR" (plist-get outcome :pr)))
                             (format "Pull request %s opened." (plist-get outcome :pr)))
      (drover-task-read marker)
      (drover-message "Task %s: pull request %s; finish it again once merged" title (plist-get outcome :pr)))
     (t
      (drover-task-org-write marker
                             (mapcar (lambda (name) (cons name nil))
                                     '("DROVER_WORKTREE" "DROVER_DIRECTORY" "DROVER_BRANCH" "DROVER_BASE"
                                       "DROVER_HOST" "DROVER_ATTEMPT_REPO" "DROVER_PR"))
                             (plist-get outcome :note))
      (let ((sessions (drover-task-sessions info)))
        (when sessions
          (drover-archive sessions)))
      (when (plist-get plan :mark-done)
        (drover-task-org-mark-done marker))
      (drover-task-read marker)
      (drover-message "Task %s finished: %s" title (or (plist-get outcome :note) "done"))))))

(defun drover-task--merge-message (plan)
  "Return the message of the merge of PLAN's branch."
  (format "Merge %s: %s%s" (plist-get plan :branch) (plist-get plan :title)
          (if-let* ((key (plist-get plan :jira))) (format " (%s)" key) "")))

(defun drover-task--step-target-checked-out (context next)
  "Check the main checkout of CONTEXT is on its target branch, for NEXT."
  (drover-task-git (drover-task--get context :checkout) '("symbolic-ref" "--quiet" "--short" "HEAD")
                   (lambda (result)
                     (let ((branch (drover-task-git-line result))
                           (target (drover-task--get context :target)))
                       (funcall next (unless (equal branch target)
                                       (format "the main checkout %s is on %s, not %s"
                                               (abbreviate-file-name (drover-task--get context :checkout))
                                               (or branch "no branch") target)))))))

(defun drover-task--step-update-target (context next)
  "Fetch the target of CONTEXT, fast-forward the main checkout to it, for NEXT.
A target ahead of origin stays as it is, its commits pushed with the
merge."
  (let ((checkout (drover-task--get context :checkout))
        (target (drover-task--get context :target)))
    (drover-task-git checkout (list "fetch" "--quiet" "--no-tags" "origin" target)
                     (lambda (result)
                       (if (not (plist-get result :ok))
                           (funcall next (concat "fetching origin failed: " (drover-task-git-why result)))
                         (drover-task-git checkout (list "merge" "--ff-only" "--quiet" (concat "origin/" target))
                                          (lambda (merged)
                                            (if (plist-get merged :ok)
                                                (funcall next nil)
                                              ;; Diverged: local commits go with the push.
                                              (drover-task-git checkout (list "merge-base" "--is-ancestor"
                                                                              (concat "origin/" target) target)
                                                               (lambda (ahead)
                                                                 (funcall next (unless (plist-get ahead :ok)
                                                                                 (concat "the main checkout's " target
                                                                                         " and origin's diverged: "
                                                                                         (drover-task-git-why merged)))))))))))
                     :timeout drover-task-fetch-timeout)))

(defun drover-task--step-conflicts (context next)
  "Look for conflicts between the branch of CONTEXT and its target, for NEXT."
  (let ((target (or (drover-task--get context :merge-into) (drover-task--get context :target))))
    (drover-task-git (drover-task--get context :checkout)
                     (list "merge-tree" "--write-tree" "--name-only" "--no-messages"
                           target (drover-task--get context :branch))
                     (lambda (result)
                       (funcall next
                                (pcase (plist-get result :exit)
                                  (0 nil)
                                  (1 (format "%s conflicts with %s in %s" (drover-task--get context :branch) target
                                             (string-join (seq-take (cdr (split-string (or (plist-get result :output) "")
                                                                                       "\n" t))
                                                                    5)
                                                          ", ")))
                                  (_ (concat "git merge-tree failed: " (drover-task-git-why result)))))))))

(defun drover-task--step-ahead (context next)
  "Count the commits of the branch of CONTEXT past its target, for NEXT."
  (drover-task-git (drover-task--get context :checkout)
                   (list "rev-list" "--count" (format "%s..%s" (or (drover-task--get context :merge-into)
                                                                   (drover-task--get context :target))
                                                      (drover-task--get context :branch)))
                   (lambda (result)
                     (if (plist-get result :ok)
                         (progn (drover-task--put context :ahead (string-to-number (or (drover-task-git-line result) "0")))
                                (funcall next nil))
                       (funcall next (concat "git rev-list failed: " (drover-task-git-why result)))))))

(defun drover-task--step-merge (context next)
  "Merge the branch of CONTEXT into its target, in the main checkout, for NEXT."
  (if (zerop (or (drover-task--get context :ahead) 0))
      (progn (drover-task--put context :note-merge "nothing to merge")
             (funcall next nil))
    (drover-task-git (drover-task--get context :checkout)
                     (list "merge" "--no-ff" "-m" (drover-task--merge-message (drover-task--plan context))
                           (drover-task--get context :branch))
                     (lambda (result)
                       (if (not (plist-get result :ok))
                           (funcall next (concat "git merge failed: " (drover-task-git-why result)))
                         (drover-task-git (drover-task--get context :checkout) '("rev-parse" "--short=12" "HEAD")
                                          (lambda (head)
                                            (drover-task--put context :note-merge
                                                              (format "merged %d commit%s into %s as %s"
                                                                      (drover-task--get context :ahead)
                                                                      (if (= 1 (drover-task--get context :ahead)) "" "s")
                                                                      (drover-task--get context :target)
                                                                      (drover-task-git-line head)))
                                            (drover-task--put context :merged t)
                                            (funcall next nil)))))
                     :timeout 300)))

(defun drover-task--step-checks (context next)
  "Run the checks of the repository of CONTEXT in the main checkout, for NEXT."
  (let ((commands (plist-get (cdr (drover-task--get context :entry)) :checks)))
    (if (or (null commands) (not (drover-task--get context :merged)))
        (funcall next nil)
      (let ((buffer (drover-task--commands-buffer (drover-task--get context :title))))
        (drover-task--insert buffer (format "Checking %s once merged\n\n" (drover-task--get context :checkout)))
        (unless (drover-task--get context :hidden)
          (display-buffer buffer '(nil (inhibit-same-window . t))))
        (drover-task-run-commands commands (drover-task--get context :checkout)
                                  (drover-task--environment (drover-task--plan context)) buffer
                                  (lambda (why)
                                    (funcall next (and why (format "%s, %s: checks failed, not pushed: %s"
                                                                   (drover-task--get context :note-merge)
                                                                   "locally" why)))))))))

(defun drover-task--step-push-target (context next)
  "Push the target of CONTEXT, merged, for NEXT."
  (if (not (drover-task--get context :merged))
      (funcall next nil)
    (drover-task-git (drover-task--get context :checkout)
                     (list "push" "origin" (drover-task--get context :target))
                     (lambda (result)
                       (if (plist-get result :ok)
                           (progn (drover-task--put context :note-merge
                                                    (concat (drover-task--get context :note-merge) ", pushed"))
                                  (funcall next nil))
                         (funcall next (format "%s, but pushing failed: %s" (drover-task--get context :note-merge)
                                               (drover-task-git-why result)))))
                     :timeout 300)))

(defun drover-task--step-open-pr (context next)
  "Push the branch of CONTEXT and open its pull request with gh, for NEXT."
  (let ((worktree (drover-task--get context :worktree))
        (branch (drover-task--get context :branch))
        (plan (drover-task--plan context)))
    (drover-task-git worktree (list "push" "--set-upstream" "origin" branch)
                     (lambda (pushed)
                       (if (not (plist-get pushed :ok))
                           (funcall next (concat "pushing the branch failed: " (drover-task-git-why pushed)))
                         (drover-task-run
                          worktree drover-task-gh-program
                          (list "pr" "create" "--base" (drover-task--get context :target) "--head" branch
                                "--title" (concat (plist-get plan :title)
                                                  (if-let* ((key (plist-get plan :jira))) (format " (%s)" key) ""))
                                "--body" (or (plist-get plan :body) (plist-get plan :title)))
                          (lambda (result)
                            (if-let* ((url (and (plist-get result :ok)
                                                (seq-find (lambda (line) (string-match-p "\\`https?://" line))
                                                          (split-string (or (plist-get result :output) "") "\n" t)))))
                                (progn (drover-task--put context :pr url)
                                       (funcall next 'stop))
                              (funcall next (concat "gh pr create failed: " (drover-task-git-why result)))))
                          :timeout 120)))
                     :timeout 300)))

(defun drover-task--step-snapshot (context next)
  "Keep the state of CONTEXT's worktree as a ref, before removing it, for NEXT.
Its branch's commit when clean; else a commit of every file, untracked
ones too, made through a temporary index, the branch's commit its
parent, so that what is discarded can be restored."
  (let* ((worktree (drover-task--get context :worktree))
         (ref (drover-task--snapshot-name (drover-task--plan context)))
         (index (make-temp-name (expand-file-name "drover-snapshot-index-" temporary-file-directory)))
         (env (concat "GIT_INDEX_FILE=" index)))
    (cl-flet ((update (commit)
                (drover-task-git worktree (list "update-ref" ref commit)
                                 (lambda (result)
                                   (when (file-exists-p index) (ignore-errors (delete-file index)))
                                   (if (plist-get result :ok)
                                       (progn (drover-task--put context :snapshot ref) (funcall next nil))
                                     (funcall next (concat "keeping a snapshot failed: " (drover-task-git-why result)))))))
              (env-git (arguments callback)
                (drover-task-run worktree "env" (append (list env drover-task-git-program) arguments) callback)))
      (if (not (drover-task--get context :dirty))
          (update "HEAD")
        (env-git '("read-tree" "HEAD")
                 (lambda (read)
                   (if (not (plist-get read :ok))
                       (funcall next (concat "snapshot: " (drover-task-git-why read)))
                     (env-git '("add" "--all")
                              (lambda (added)
                                (if (not (plist-get added :ok))
                                    (funcall next (concat "snapshot: " (drover-task-git-why added)))
                                  (env-git '("write-tree")
                                           (lambda (tree)
                                             (if-let* ((sha (and (plist-get tree :ok) (drover-task-git-line tree))))
                                                 (drover-task-git worktree (list "commit-tree" sha "-p" "HEAD" "-m"
                                                                                 (format "drover: snapshot of %s"
                                                                                         (drover-task--get context :title)))
                                                                  (lambda (commit)
                                                                    (if-let* ((commit (and (plist-get commit :ok)
                                                                                           (drover-task-git-line commit))))
                                                                        (update commit)
                                                                      (funcall next (concat "snapshot: " (drover-task-git-why commit))))))
                                               (funcall next (concat "snapshot: " (drover-task-git-why tree))))))))))))))))

(defun drover-task--step-teardown (context next)
  "Run the teardown of the repository of CONTEXT in its worktree, for NEXT.
A failure is told, and the cleanup goes on."
  (let ((commands (plist-get (cdr (drover-task--get context :entry)) :teardown)))
    (if (null commands)
        (funcall next nil)
      (let ((buffer (drover-task--commands-buffer (drover-task--get context :title))))
        (drover-task-run-commands commands (drover-task--get context :worktree)
                                  (drover-task--environment (drover-task--plan context)) buffer
                                  (lambda (why)
                                    (when why
                                      (drover-warn "Teardown of %s: %s" (drover-task--get context :title) why))
                                    (funcall next nil)))))))

(defun drover-task--step-remove (context next)
  "Unlock and remove the worktree of CONTEXT, for NEXT; forced only once kept."
  (let ((checkout (drover-task--get context :checkout))
        (worktree (directory-file-name (drover-task--get context :worktree))))
    (drover-task-git checkout (list "worktree" "unlock" worktree)
                     (lambda (_)
                       (drover-task-git checkout (append (list "worktree" "remove")
                                                         (and (drover-task--get context :dirty)
                                                              (drover-task--get context :snapshot)
                                                              (list "--force"))
                                                         (list worktree))
                                        (lambda (result)
                                          (if (plist-get result :ok)
                                              (progn (drover-task--put context :removed t)
                                                     (funcall next nil))
                                            (funcall next (concat "git worktree remove failed: "
                                                                  (drover-task-git-why result)))))
                                        :timeout 300)))))

(defun drover-task--step-delete-branch (context next)
  "Delete the branch of CONTEXT once merged, for NEXT; kept otherwise, as told."
  (let ((checkout (drover-task--get context :checkout))
        (branch (drover-task--get context :branch))
        (pr-state (drover-task--get context :pr-state)))
    (drover-task-git checkout (list "branch" "-d" branch)
                     (lambda (result)
                       (cond
                        ((plist-get result :ok)
                         (drover-task--put context :branch-deleted t)
                         (funcall next nil))
                        ;; Squash-merged: Git cannot tell, its pull request can.
                        ((equal (plist-get pr-state :state) "MERGED")
                         (drover-task-git checkout (list "rev-parse" (concat "refs/heads/" branch))
                                          (lambda (tip)
                                            (if (equal (drover-task-git-line tip) (plist-get pr-state :head))
                                                (drover-task-git checkout (list "branch" "-D" branch)
                                                                 (lambda (deleted)
                                                                   (drover-task--put context :branch-deleted
                                                                                     (plist-get deleted :ok))
                                                                   (funcall next nil)))
                                              (funcall next nil)))))
                        (t (funcall next nil)))))))

(defun drover-task-finish-plan (plan hidden callback)
  "Finish PLAN as its method says, clean it up, call CALLBACK with the outcome.
PLAN is a plan of `drover-task--plan-of-attempt' with :method,
:cleanup, :dirty, the uncommitted files the user agreed to lose, and
:pr-state.  HIDDEN runs the checks in no window.  The outcome is a
plist: :failure, why it stopped, :note, what was done, :pr, a pull
request opened, :removed, non-nil once the worktree went."
  (let* ((context (drover-task--context (append (list :hidden hidden) plan)))
         (method (plist-get plan :method))
         (worktree (plist-get plan :worktree))
         (steps
          (append
           (when (and worktree (plist-get plan :exists))
             (pcase method
               ('merge (list #'drover-task--step-target-checked-out #'drover-task--step-conflicts
                             #'drover-task--step-ahead #'drover-task--step-merge))
               ('merge-push (list #'drover-task--step-target-checked-out #'drover-task--step-update-target
                                  #'drover-task--step-conflicts #'drover-task--step-ahead
                                  #'drover-task--step-merge #'drover-task--step-checks
                                  #'drover-task--step-push-target))
               ('pr (unless (plist-get plan :pr)
                      (list (lambda (context next)
                              (drover-task-git (drover-task--get context :checkout)
                                               (list "fetch" "--quiet" "--no-tags" "origin"
                                                     (drover-task--get context :target))
                                               (lambda (result)
                                                 (drover-task--put context :merge-into
                                                                   (concat "origin/" (drover-task--get context :target)))
                                                 (funcall next (unless (plist-get result :ok)
                                                                 (concat "fetching origin failed: "
                                                                         (drover-task-git-why result)))))
                                               :timeout drover-task-fetch-timeout))
                            #'drover-task--step-conflicts #'drover-task--step-open-pr)))))
           (when (and worktree (plist-get plan :exists) (plist-get plan :cleanup))
             (list #'drover-task--step-snapshot #'drover-task--step-teardown
                   #'drover-task--step-remove #'drover-task--step-delete-branch)))))
    (drover-task-git-steps
     steps context
     (lambda (why)
       (let ((note (string-join
                    (delq nil (list (drover-task--get context :note-merge)
                                    (and (drover-task--get context :snapshot)
                                         (format "snapshot %s" (drover-task--get context :snapshot)))
                                    (and (drover-task--get context :removed)
                                         (format "worktree %s removed" (abbreviate-file-name worktree)))
                                    (and (drover-task--get context :removed)
                                         (if (drover-task--get context :branch-deleted)
                                             (format "branch %s deleted" (drover-task--get context :branch))
                                           (format "branch %s kept" (drover-task--get context :branch))))
                                    (and worktree (not (plist-get plan :exists))
                                         (format "worktree %s already gone" (abbreviate-file-name worktree)))))
                    "; ")))
         (funcall callback
                  (append (and why (list :failure why))
                          (list :note (if (string-empty-p note) "nothing to clean up" note)
                                :removed (drover-task--get context :removed))
                          (and (drover-task--get context :pr) (null why)
                               (list :pr (drover-task--get context :pr))))))))))

(defun drover-task--finish-there (marker info plan)
  "Have the host of PLAN, the task INFO tells of at MARKER, finish it."
  (require 'drover-remote)
  (let* ((host (plist-get plan :host))
         (title (plist-get info :title))
         (done (and (not (plist-get info :done))
                    (y-or-n-p (format "Once finished, mark %s done? " title))))
         (request (list :kind "finish" :task (plist-get info :id) :title title
                        :repo (or (plist-get plan :repo) :false)
                        :worktree (or (plist-get plan :worktree) :false)
                        :directory (or (plist-get plan :directory) :false)
                        :branch (or (plist-get plan :branch) :false)
                        :jira (or (plist-get plan :jira) :false)
                        :pr (or (plist-get plan :pr) :false)
                        :discard :false)))
    (cl-labels ((ask (request)
                  (drover-remote-ask-task
                   host request
                   (lambda (reply error)
                     (drover-with-reporting "task finish"
                       (cond
                        (error (drover-warn "Task %s not finished on %s: %s" title host error))
                        ((plist-get reply :dirty)
                         (if (yes-or-no-p (format "%s has uncommitted changes on %s: %s.  They go to a snapshot, then the worktree goes: finish? "
                                                  title host (plist-get reply :dirty)))
                             (ask (plist-put (copy-sequence request) :discard t))
                           (message "Task %s left as it is" title)))
                        (t (drover-task--finished marker info (plist-put (copy-sequence plan) :mark-done done)
                                                  reply)))))
                   (+ drover-task-setup-timeout 120))))
      (message "Finishing %s on %s…" title host)
      (ask request))))

;;;; Linking sessions by hand

;;;###autoload
(defun drover-task-link-session ()
  "Link a session to a task, or unlink it.
On an Org heading, the session is read among the live ones; on a
session, in a list or its buffer, the task is read among those known.
The link is kept in `drover-state-directory'."
  (interactive)
  (drover-start)
  (if (derived-mode-p 'org-mode)
      (let* ((marker (drover-task--heading-marker))
             (id (drover-task--ensure-id marker))
             (info (drover-task-read marker id))
             (sessions (sort (seq-filter #'drover-session-title (drover-sessions))
                             :key (lambda (session) (- (or (drover-session-updated session) 0)))))
             (choices (mapcar (lambda (session)
                                (cons (format "%s%s" (drover-session-display-title session)
                                              (if (member (drover-task-id-of session)
                                                          (list (plist-get info :id) (plist-get info :orig-id)))
                                                  " (linked)" ""))
                                      session))
                              sessions))
             (session (cdr (assoc (completing-read (format "Link to %s: " (plist-get info :title))
                                                   (mapcar #'car choices) nil t)
                                  choices))))
        (if (member (drover-task-id-of session) (list (plist-get info :id) (plist-get info :orig-id)))
            (progn (drover-task-link session nil)
                   (message "%s no longer linked by hand" (drover-session-display-title session)))
          (drover-task-link session (plist-get info :id))
          (message "%s now serves %s" (drover-session-display-title session) (plist-get info :title))))
    (let* ((session (drover-session-at-point))
           (tasks nil))
      (maphash (lambda (id info)
                 (when (and (not (plist-member info :missing)) (equal id (plist-get info :id)))
                   (push (cons (plist-get info :title) id) tasks)))
               drover-task--infos)
      (unless tasks
        (user-error "No task known yet: link from the task's heading"))
      (let* ((none "No task")
             (choice (completing-read (format "Task of %s: " (drover-session-display-title session))
                                      (cons none (mapcar #'car tasks)) nil t)))
        (drover-task-link session (and (not (equal choice none)) (cdr (assoc choice tasks))))
        (message "%s linked" (drover-session-display-title session))))))

;;;; Sweeping

(defun drover-task--sweep-repositories ()
  "Return the main checkouts of this host whose worktrees a sweep lists."
  (seq-uniq
   (delq nil
         (append (mapcar #'drover-task-checkout drover-task-repositories)
                 (mapcar (lambda (session)
                           (when-let* ((directory (drover-session-local-directory session))
                                       ((drover-git-directory-cached directory)))
                             (drover-task-git-main-checkout directory)))
                         (seq-take (drover-sessions) 500))))
   #'drover-task-same-file-p))

;;;###autoload
(defun drover-task-sweep ()
  "List the worktrees that may go, deleting nothing.
Of the repositories of `drover-task-repositories' and of the sessions
listed, on this host: those Git tells prunable, whose directory is
gone, such as agents' worktrees in the temporary directory; those of
tasks marked done; those of tasks whose heading is not found; and the
others, with the task each serves, and whether it is locked."
  (interactive)
  (drover-start)
  (let* ((checkouts (seq-filter #'file-directory-p (drover-task--sweep-repositories)))
         (left (length checkouts))
         (found nil)
         (buffer (get-buffer-create "*drover worktrees*")))
    (when (zerop left)
      (user-error "No repository to sweep"))
    (message "Listing the worktrees of %d repositories…" left)
    (dolist (checkout checkouts)
      (drover-task-git checkout '("worktree" "list" "--porcelain")
                       (lambda (result)
                         (drover-with-reporting "task sweep"
                           (push (cons checkout (and (plist-get result :ok)
                                                     (cdr (drover-task-git-parse-worktrees (plist-get result :output)))))
                                 found)
                           (when (zerop (cl-decf left))
                             (drover-task--sweep-show buffer (sort found :key #'car))
                             (display-buffer buffer))))))))

(defun drover-task--sweep-verdict (worktree)
  "Return what may become of WORKTREE, of `drover-task-git-parse-worktrees'."
  (let* ((path (plist-get worktree :path))
         (marker (and (file-directory-p path) (drover-task-git-read-marker path)))
         (info (and marker (gethash (plist-get marker :id) drover-task--infos))))
    (cond
     ((plist-get worktree :prunable) "prunable: its directory is gone (git worktree prune)")
     ((not (file-directory-p path)) "its directory is gone")
     ((and marker (plist-get info :done)) (format "its task %s is done: finish it" (plist-get info :title)))
     ((and marker (plist-member info :missing)) (format "its task %s is not found" (plist-get marker :id)))
     (marker (format "task %s" (or (plist-get info :title) (plist-get marker :id))))
     ((plist-get worktree :locked) (format "locked: %s" (plist-get worktree :locked)))
     (t "made by hand or by an agent"))))

(defun drover-task--sweep-show (buffer found)
  "Show in BUFFER the worktrees FOUND, an alist (CHECKOUT . WORKTREES)."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p 'special-mode) (special-mode))
      (insert (propertize "Worktrees that may go: drover deletes none of them.\n\n" 'face 'drover-heading))
      (pcase-dolist (`(,checkout . ,worktrees) found)
        (when worktrees
          (insert (propertize (abbreviate-file-name checkout) 'face 'bold) "\n")
          (dolist (worktree worktrees)
            (insert "  " (abbreviate-file-name (plist-get worktree :path))
                    (if-let* ((branch (plist-get worktree :branch))) (concat "  " branch) "")
                    "\n    " (propertize (drover-task--sweep-verdict worktree) 'face 'drover-dim) "\n"))
          (insert "\n")))
      (goto-char (point-min)))))

;;;; Other hosts

(defconst drover-task--slug-regexp "\\`[A-Za-z0-9][A-Za-z0-9._-]\\{0,99\\}\\'"
  "What the name of a task a peer asks for may be.")

(defun drover-task--remote-string (value &optional limit)
  "Return VALUE, a string a peer sent, bounded to LIMIT characters, or nil."
  (and (stringp value) (not (string-empty-p value))
       (if (and limit (> (length value) limit)) (substring value 0 limit) value)))

(defun drover-task--remote-start (message reply)
  "Start for a peer the task MESSAGE asks, answering through REPLY.
Its repository is one of `drover-task-repositories' with a checkout
here; its place is made as here, in no window, and a session started
there, in none either.  The answer tells where it works."
  (let* ((id (drover-task--remote-string (plist-get message :task) 200))
         (title (drover-task--remote-string (plist-get message :title) 300))
         (entry (drover-task-repository (drover-task--remote-string (plist-get message :repo) 200)))
         (checkout (and entry (drover-task-checkout entry)))
         (mode (pcase (plist-get message :mode) ("worktree" 'worktree) ("in-place" 'in-place)))
         (slug (drover-task--remote-string (plist-get message :slug) 100))
         (base (pcase (plist-get message :base)
                 ("fresh" 'fresh) ("head" 'head)
                 ((and (pred stringp) base) (and (string-match-p "\\`[A-Za-z0-9._/-]\\{1,200\\}\\'" base) base)))))
    (cond
     ((not (and id title)) (funcall reply nil "no task named"))
     ((not entry) (funcall reply nil (format "no repository %s here" (plist-get message :repo))))
     ((not (and checkout (file-directory-p checkout))) (funcall reply nil (format "no checkout of %s here" (car entry))))
     ((not mode) (funcall reply nil "a task works in a worktree or in place here"))
     ((not (and slug (string-match-p drover-task--slug-regexp slug))) (funcall reply nil "no name of a known shape"))
     ((not base) (funcall reply nil "no base of a known shape"))
     (t
      (let* ((properties (cdr entry))
             (target (or (plist-get properties :target) (drover-git-default-branch checkout) "main"))
             (plan (list :id id :title (drover-bound-line title) :jira (drover-task--remote-string (plist-get message :jira) 40)
                         :body (and (stringp (plist-get message :body))
                                    (drover-bound-text (plist-get message :body) 400 drover-task-org-body-limit))
                         :slug slug :kind mode :repo (car entry) :entry entry :checkout checkout :target target
                         :branch-prefix (or (plist-get properties :branch-prefix) "")
                         :base-choice base :carry (eq (plist-get message :carry) t)
                         :directory (and (eq mode 'in-place) checkout) :hidden t)))
        (drover-task-prepare
         plan
         (lambda (outcome)
           (drover-with-reporting "remote task start"
             (pcase-let* ((`(,plan . ,why) outcome)
                          (made (list :worktree (or (plist-get plan :worktree) :false)
                                      :directory (or (plist-get plan :directory) :false)
                                      :branch (or (plist-get plan :branch) :false)
                                      :base (or (plist-get plan :base) :false)
                                      :base-ref (or (plist-get plan :base-ref) :false))))
               (cond
                ((and why (not (plist-get plan :made)) (eq mode 'worktree)) (funcall reply nil why))
                (why (funcall reply (append (list :failure why) made)))
                (t (drover-task-start-session
                    plan t
                    (lambda (session)
                      (if (stringp session)
                          (funcall reply (append (list :failure (concat "no session started: " session)) made))
                        (funcall reply (append (list :key (vector (symbol-name (drover-session-backend session))
                                                                  (drover-session-id session))
                                                     :title (or (drover-session-title session) :false))
                                               made))))))))))))))))

(defun drover-task--remote-finish (message reply)
  "Finish for a peer the task MESSAGE asks, answering through REPLY.
Only a worktree of a repository of `drover-task-repositories' here,
whose marker names the task, is finished, never prompting: uncommitted
changes are told back unless the peer said to discard them."
  (let* ((id (drover-task--remote-string (plist-get message :task) 200))
         (entry (drover-task-repository (drover-task--remote-string (plist-get message :repo) 200)))
         (checkout (and entry (drover-task-checkout entry)))
         (worktree (let ((path (drover-task--remote-string (plist-get message :worktree) 1024)))
                     (and path (file-name-absolute-p path) (file-name-as-directory (drover-task-canonical path)))))
         (directory (drover-task--remote-string (plist-get message :directory) 1024))
         (branch (drover-task--remote-string (plist-get message :branch) 200))
         (marker (and worktree (file-directory-p worktree) (drover-task-git-read-marker worktree))))
    (cond
     ((not id) (funcall reply nil "no task named"))
     ((and (not worktree) directory)
      (funcall reply (list :note (format "nothing to clean up in %s" directory))))
     ((not (and entry checkout (file-directory-p checkout)))
      (funcall reply nil (format "no checkout of %s here" (plist-get message :repo))))
     ((not worktree) (funcall reply nil "no worktree named"))
     ((and (file-directory-p worktree) (not (equal (plist-get marker :id) id)))
      (funcall reply nil (format "%s serves no task %s here" worktree id)))
     ((and (file-directory-p worktree)
           (not (drover-task-same-file-p (drover-task-git-main-checkout worktree) checkout)))
      (funcall reply nil (format "%s is no worktree of %s here" worktree (car entry))))
     (t
      (let* ((exists (file-directory-p worktree))
             (plan (list :id id :title (or (drover-task--remote-string (plist-get message :title) 300) id)
                         :jira (drover-task--remote-string (plist-get message :jira) 40)
                         :repo (car entry) :entry entry :checkout checkout
                         :target (or (plist-get (cdr entry) :target) (drover-git-default-branch checkout) "main")
                         :worktree worktree :directory worktree :branch branch
                         :pr (drover-task--remote-string (plist-get message :pr) 300)
                         :method (plist-get (cdr entry) :finish) :exists exists)))
        (drover-task-git
         (if exists worktree checkout) (if exists '("status" "--porcelain=v1" "--untracked-files=all") '("--version"))
         (lambda (status)
           (drover-with-reporting "remote task finish"
             (let ((dirty (and exists (drover-task-git-parse-status (plist-get status :output)))))
               (if (and dirty (not (eq (plist-get message :discard) t)))
                   (funcall reply (list :dirty (string-join (seq-take dirty 10) ", ")))
                 (let ((finish
                        (lambda (pr-state)
                          (let ((cleanup (or (not (eq (plist-get plan :method) 'pr))
                                             (and pr-state (not (equal (plist-get pr-state :state) "OPEN"))))))
                            (if (and pr-state (equal (plist-get pr-state :state) "OPEN"))
                                (funcall reply nil (format "its pull request is still open: %s" (plist-get plan :pr)))
                              (drover-task-finish-plan
                               (append (list :dirty dirty :pr-state pr-state :cleanup cleanup) plan) t
                               (lambda (outcome)
                                 (funcall reply (append (list :note (or (plist-get outcome :note) :false)
                                                              :removed (if (plist-get outcome :removed) t :false))
                                                        (and (plist-get outcome :failure)
                                                             (list :failure (plist-get outcome :failure)))
                                                        (and (plist-get outcome :pr)
                                                             (list :pr (plist-get outcome :pr))))))))))))
                   (if (and (eq (plist-get plan :method) 'pr) (plist-get plan :pr) exists)
                       (drover-task-run worktree drover-task-gh-program
                                        (list "pr" "view" (plist-get plan :pr) "--json" "state,headRefOid")
                                        (lambda (result)
                                          (let ((data (and (plist-get result :ok)
                                                           (ignore-errors (json-parse-string (plist-get result :output)
                                                                                             :object-type 'plist)))))
                                            (if data
                                                (funcall finish (list :state (plist-get data :state)
                                                                      :head (plist-get data :headRefOid)))
                                              (funcall reply nil (concat "gh pr view failed: "
                                                                         (drover-task-git-why result))))))
                                        :timeout 30)
                     (funcall finish nil)))))))))))))

(defun drover-task-remote-answer (peer access message reply)
  "Answer the request MESSAGE of PEER, of ACCESS, about tasks, through REPLY.
Its :kind is \"start\", for an access starting sessions, `act' or
`full', or \"finish\", for `full' access.  REPLY takes the answer, or
nil and why it is refused."
  (pcase (plist-get message :kind)
    ("start"
     (if (memq access '(act full))
         (drover-task--remote-start message reply)
       (funcall reply nil (format "%s has %s access, which starts no task" peer access))))
    ("finish"
     (if (eq access 'full)
         (drover-task--remote-finish message reply)
       (funcall reply nil (format "%s has %s access, which finishes no task" peer access))))
    (_ (funcall reply nil "unknown request about tasks"))))

;;;; The mode

(defvar drover-task--oc-timer nil
  "The timer reading the first prompts of OpenCode's sessions, or nil.")

(defun drover-task--oc-tick ()
  "Read the first prompts of OpenCode's sessions now and then, while lists show."
  (drover-with-reporting "task prompts of OpenCode"
    ;; Worktrees made or gone meanwhile, by hand or by Git: looked at anew.
    (drover-task--changed)
    (when (or (null drover-task--oc-read)
              (seq-some (lambda (buffer) (and (buffer-local-value 'drover-view-p buffer)
                                              (get-buffer-window buffer t)))
                        (buffer-list)))
      (drover-task--oc-read))))

;;;###autoload
(define-minor-mode drover-task-mode
  "Show the tasks of sessions in drover's lists, and their projects' colours.
Lists then show the project of a session's task, its Org category, in
its colour, group sessions by task (=g) and filter them by project
\(=p).  See `drover-task-start' for what tasks are."
  :global t
  :group 'drover
  (if drover-task-mode
      (progn
        (setq drover-session-task-function #'drover-task--session-task)
        (add-hook 'enable-theme-functions #'drover-task--theme-changed)
        (add-hook 'disable-theme-functions #'drover-task--theme-changed)
        (unless (timerp drover-task--oc-timer)
          (setq drover-task--oc-timer (run-with-timer 2 60 #'drover-task--oc-tick))))
    (when (eq drover-session-task-function #'drover-task--session-task)
      (setq drover-session-task-function nil))
    (remove-hook 'enable-theme-functions #'drover-task--theme-changed)
    (remove-hook 'disable-theme-functions #'drover-task--theme-changed)
    (when (timerp drover-task--oc-timer)
      (cancel-timer drover-task--oc-timer))
    (setq drover-task--oc-timer nil))
  (when (fboundp 'drover-view-settings-changed)
    (drover-view-settings-changed)))

(provide 'drover-task)
;;; drover-task.el ends here
