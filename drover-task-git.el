;;; drover-task-git.el --- Git for tasks: worktrees, merges, snapshots  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: vc, convenience

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

;; What tasks ask of Git, each command run by `drover-command-run', so
;; that Emacs never waits for one: a step calls back with its result,
;; and steps run one after the other (`drover-task-git-steps').  Git
;; never prompts: a fetch or a push that would ask for credentials
;; fails, within its time.  The paths of a host are canonical
;; (`drover-task-canonical'): the same directory spelt otherwise, as
;; Git on Windows spells it /c/Users where Emacs spells it c:/Users, or
;; through a directory another names (`drover-task-path-aliases'), is
;; the same one.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-command)

(declare-function drover-task--changed "drover-task" ())

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 20))
  (load "drover-core" nil t))

(defcustom drover-task-path-aliases nil
  "Directories naming the same place, as an alist (ALIAS . DIRECTORY).
A path under ALIAS is the path under DIRECTORY, such as
\(\"~/Documents/\" . \"~/local/\") where both name one directory."
  :type '(alist :key-type directory :value-type directory)
  :group 'drover)

(defcustom drover-task-git-program "git"
  "The Git program tasks run."
  :type 'string
  :group 'drover)

(defcustom drover-task-gh-program "gh"
  "The GitHub CLI tasks open pull requests with."
  :type 'string
  :group 'drover)

(defconst drover-task-git-environment
  '("GIT_TERMINAL_PROMPT=0" "SSH_ASKPASS_REQUIRE=never" "GCM_INTERACTIVE=never"
    "GIT_EDITOR=true" "GH_PROMPT_DISABLED=1")
  "The environment of the commands tasks run: none may prompt.")

;;;; Paths

(defun drover-task--native-name (file)
  "Return FILE as Emacs names it on this system.
On Windows, the MSYS form /c/... is c:/..., and a drive's letter is
lower case."
  (if (memq system-type '(windows-nt cygwin))
      (let ((file (if (string-match "\\`/\\([a-zA-Z]\\)\\(/\\|\\'\\)" file)
                      (concat (match-string 1 file) ":/" (substring file (match-end 0)))
                    file)))
        (if (string-match "\\`\\([A-Za-z]\\):" file)
            (concat (downcase (match-string 1 file)) (substring file 1))
          file))
    file))

(defun drover-task-canonical (file)
  "Return FILE, a directory or a file, as its canonical name on this host.
Expanded; on Windows the MSYS form /c/... is c:/..., and the drive's
letter is lower case (see `drover-task--native-name'); a path under an
alias of `drover-task-path-aliases' is under the directory it names.  A
directory keeps its trailing slash, or lack of it."
  (when (and (stringp file) (not (string-empty-p file)))
    (let ((file (drover-task--native-name (expand-file-name (drover-task--native-name file)))))
      (pcase-dolist (`(,alias . ,directory) drover-task-path-aliases)
        (let ((alias (file-name-as-directory (expand-file-name alias))))
          (when (or (string-prefix-p alias file) (equal (directory-file-name alias) file))
            (setq file (concat (file-name-as-directory (expand-file-name directory))
                               (substring file (min (length file) (length alias))))))))
      file)))

(defun drover-task-same-file-p (a b)
  "Return non-nil when A and B name the same file or directory on this host."
  (and a b
       (equal (directory-file-name (drover-task-canonical a))
              (directory-file-name (drover-task-canonical b)))))

;;;; Running commands

(defun drover-task-git--command (program arguments)
  "Return the command running PROGRAM with ARGUMENTS in the tasks' environment."
  (if (executable-find "env")
      (append (list "env") drover-task-git-environment (cons program arguments))
    (cons program arguments)))

(cl-defun drover-task-run (directory program arguments callback &key (timeout 120) name)
  "Run PROGRAM with ARGUMENTS in DIRECTORY, then call CALLBACK with its result.
The result is a plist: :exit, :output and :errors once it ran, else
:failure, why it did not, as `drover-command-run' tells; with :ok non-nil
for an exit status of 0.  It runs within TIMEOUT seconds, prompting
for nothing; NAME names it in reports."
  (let ((drover-command-timeout timeout)
        (process-environment (if (executable-find "env")
                                 process-environment
                               (append drover-task-git-environment process-environment))))
    (condition-case err
        (drover-command-run (or name (format "%s %s" (file-name-nondirectory program) (car arguments)))
                            (drover-task-git--command program arguments)
                            (lambda (result)
                              (funcall callback (if (eql (plist-get result :exit) 0)
                                                    (append (list :ok t) result)
                                                  result)))
                            directory)
      (error (funcall callback (list :failure (drover-error-message err)))))))

(cl-defun drover-task-git (directory arguments callback &key (timeout 120))
  "Run Git with ARGUMENTS in DIRECTORY, then call CALLBACK with its result.
See `drover-task-run'."
  (drover-task-run directory drover-task-git-program arguments callback :timeout timeout
                   :name (concat "git " (car arguments))))

(defun drover-task-git-why (result)
  "Return why RESULT, of `drover-task-run', failed, a line."
  (or (plist-get result :failure)
      (let ((errors (string-trim (or (plist-get result :errors) "")))
            (output (string-trim (or (plist-get result :output) ""))))
        (drover-bound-line
         (cond ((not (string-empty-p errors)) (car (last (split-string errors "\n" t))))
               ((not (string-empty-p output)) (car (last (split-string output "\n" t))))
               (t (format "exit status %s" (plist-get result :exit))))
         200))
      "failed"))

(defun drover-task-git-line (result)
  "Return the first line of the output of RESULT, trimmed, or nil."
  (when-let* ((output (plist-get result :output))
              (line (car (split-string output "\n" t "[ \t\r]+"))))
    line))

;;;; Steps

(defun drover-task-git-steps (steps context done)
  "Run STEPS on CONTEXT in order, then call DONE with nil, or why one failed.
Each step is a function of CONTEXT and NEXT, which it calls once: with
nil to go on, a string to stop there, or the symbol `stop' to end
without failure.  CONTEXT is what the steps share, a hash table.  A
step signalling fails, its error told."
  (if (null steps)
      (funcall done nil)
    (let ((called nil))
      (condition-case err
          (funcall (car steps) context
                   (lambda (outcome)
                     (unless called
                       (setq called t)
                       (drover-with-reporting "task step"
                         (cond ((null outcome) (drover-task-git-steps (cdr steps) context done))
                               ((eq outcome 'stop) (funcall done nil))
                               (t (funcall done outcome)))))))
        (error (unless called
                 (setq called t)
                 (funcall done (drover-error-message err))))))))

;;;; Reading repositories

(defun drover-task-git-common-directory (directory)
  "Return the common Git directory of the repository of DIRECTORY, or nil.
Read from the files, without running Git: a linked worktree's names it."
  (when-let* ((gitdir (drover--git-directory directory)))
    (let ((file (expand-file-name "commondir" gitdir)))
      (file-name-as-directory
       (if (file-readable-p file)
           (with-temp-buffer
             (insert-file-contents file)
             (expand-file-name (drover--native-file-name (string-trim (buffer-string))) gitdir))
         gitdir)))))

(defun drover-task-git-toplevel (directory)
  "Return the top of the worktree of DIRECTORY, with a slash, or nil."
  (when-let* ((root (locate-dominating-file directory ".git")))
    (file-name-as-directory (expand-file-name root))))

(defun drover-task-git-main-checkout (directory)
  "Return the main checkout of the repository of DIRECTORY, or nil.
The directory holding its common Git directory, for a repository not
bare."
  (when-let* ((common (drover-task-git-common-directory directory)))
    (let ((common (directory-file-name common)))
      (when (equal (file-name-nondirectory common) ".git")
        (file-name-as-directory (file-name-directory common))))))

(defun drover-task-git-remote-url (directory &optional remote)
  "Return the URL of REMOTE, origin by default, of the repository of DIRECTORY.
Read from its configuration, without running Git; nil without one."
  (when-let* ((common (drover-task-git-common-directory directory))
              (config (expand-file-name "config" common))
              ((file-readable-p config)))
    (with-temp-buffer
      (insert-file-contents config)
      (goto-char (point-min))
      (when (re-search-forward (format "^\\[remote \"%s\"\\]" (regexp-quote (or remote "origin"))) nil t)
        (let ((end (save-excursion (if (re-search-forward "^\\[" nil t) (point) (point-max)))))
          (when (re-search-forward "^[ \t]*url[ \t]*=[ \t]*\\(.+?\\)[ \t]*$" end t)
            (match-string 1)))))))

(defun drover-task-git-repository-name (url)
  "Return the repository URL names: OWNER/NAME on a forge, else NAME."
  (when (stringp url)
    (let ((url (replace-regexp-in-string "\\.git/?\\'" "" (directory-file-name url))))
      (cond
       ((string-match "\\`[a-z+]+://[^/]+/\\(.+\\)\\'" url) (match-string 1 url))
       ((string-match "\\`[^/:@]+@[^:/]+:\\(.+\\)\\'" url) (match-string 1 url))
       (t (file-name-nondirectory url))))))

(defun drover-task-git-marker-file (worktree)
  "Return the file in WORKTREE's Git directory naming its task, or nil."
  (when-let* ((gitdir (drover--git-directory worktree)))
    (expand-file-name "drover-task" gitdir)))

(defun drover-task-git-read-marker (worktree)
  "Return what the marker of WORKTREE tells, a plist (:id :setup), or nil."
  (when-let* ((file (drover-task-git-marker-file worktree))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (let ((lines (split-string (buffer-string) "\n" t "[ \t\r]+")))
        (when (and (car lines) (string-match-p "\\`[^ \t]\\{1,200\\}\\'" (car lines)))
          (list :id (car lines)
                :setup (seq-some (lambda (line)
                                   (and (string-match "\\`setup: \\([a-z]+\\)\\'" line)
                                        (intern (match-string 1 line))))
                                 lines)))))))

(defun drover-task-git-write-marker (worktree id &optional setup)
  "Note in WORKTREE's Git directory that it serves the task ID, its SETUP state.
SETUP is `running', `ok' or `failed'.  The file goes with the worktree's
Git directory as Git removes it, a sweep telling drover's worktrees
from others by it."
  (when-let* ((file (drover-task-git-marker-file worktree)))
    (when (fboundp 'drover-task--changed)
      (drover-task--changed))
    (with-temp-file file
      (insert id "\n")
      (when setup (insert (format "setup: %s\n" setup))))))

(defun drover-task-git--misread-p (worktree spelled)
  "Return non-nil when Git listed WORKTREE prunable for misreading its path.
SPELLED is its path as Git listed it.  On Windows, MSYS2's Git writes
the worktrees it adds as /c/..., which Git for Windows reads as a
directory under the drive's root: it lists such a worktree prunable,
its gitdir file pointing to a location that does not exist, though its
directory, c:/..., holds its .git file."
  (and (plist-get worktree :prunable)
       (memq system-type '(windows-nt cygwin))
       (string-match-p "\\`/[a-zA-Z]/" spelled)
       (file-exists-p (expand-file-name ".git" (plist-get worktree :path)))))

(defun drover-task-git-parse-worktrees (output)
  "Return the worktrees `git worktree list --porcelain' OUTPUT lists, as plists.
Each has :path, :head, :branch, :locked (its reason, or t), :prunable,
:bare and :detached.  A worktree MSYS2's Git added on Windows, which
Git for Windows lists prunable though it is not, is not prunable (see
`drover-task-git--misread-p')."
  (let ((worktrees nil)
        (current nil)
        (spelled nil))
    (dolist (line (append (split-string (or output "") "\n") '("")))
      (cond
       ((string-empty-p line)
        (when current
          (when (drover-task-git--misread-p current spelled)
            (setq current (plist-put current :prunable nil)))
          (push current worktrees))
        (setq current nil))
       ((string-match "\\`worktree \\(.+\\)\\'" line)
        (setq spelled (match-string 1 line)
              current (list :path (file-name-as-directory (drover-task-canonical spelled)))))
       ((string-match "\\`HEAD \\([0-9a-f]+\\)\\'" line)
        (setq current (plist-put current :head (match-string 1 line))))
       ((string-match "\\`branch refs/heads/\\(.+\\)\\'" line)
        (setq current (plist-put current :branch (match-string 1 line))))
       ((string-match "\\`locked\\(?: \\(.*\\)\\)?\\'" line)
        (setq current (plist-put current :locked (or (match-string 1 line) t))))
       ((string-match "\\`prunable\\(?: \\(.*\\)\\)?\\'" line)
        (setq current (plist-put current :prunable (or (match-string 1 line) t))))
       ((equal line "bare") (setq current (plist-put current :bare t)))
       ((equal line "detached") (setq current (plist-put current :detached t)))))
    (nreverse worktrees)))

(defun drover-task-git-parse-status (output)
  "Return the files `git status --porcelain' OUTPUT names, at most 200."
  (seq-take (delq nil (mapcar (lambda (line)
                                (and (> (length line) 3) (substring line 3)))
                              (split-string (or output "") "\n" t)))
            200))

(provide 'drover-task-git)
;;; drover-task-git.el ends here
