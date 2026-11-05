;;; drover-task-org.el --- The Org headings of tasks: found, read and written  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: outlines, convenience

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

;; A task is an Org heading, found by its ID alone: headings move, and
;; ejira, syncing them with Jira, gives a heading the key of its issue
;; as ID once created, keeping the ID before as ORIG_ID; so a heading
;; is found by either.  What drover writes on a heading is properties
;; whose names begin with DROVER_ and notes in its LOGBOOK, nothing
;; else: never ejira's properties, the title, the TODO keyword but on
;; the user's word, the body, nor a child heading, which ejira would
;; take for an issue or a comment.  A file in a linked worktree, a copy
;; of a tracked file such as a repository's todo.org, and the files of
;; `drover-task-refused-files' are never opened nor written.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'drover-core)
(require 'drover-task-git)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 20))
  (load "drover-core" nil t))

(declare-function org-id-locations-load "org-id" ())
(declare-function org-id-files "org-id" ())
(defvar org-id-locations)
(defvar ejira-push-on-save)

(defcustom drover-task-refused-files nil
  "Org files and directories tasks are never read from nor written to.
Such as an old clone of a repository holding a copy of a task file
another checkout tracks.  Files of linked worktrees are refused anyway."
  :type '(repeat file)
  :group 'drover)

(defconst drover-task-org-jira-regexp "\\`[A-Z][A-Z0-9]+-[0-9]+\\'"
  "What a Jira key looks like, as ejira makes it the ID of a heading.")

(defconst drover-task-org-property-regexp "\\`DROVER_[A-Z0-9_]+\\'"
  "What the names of the properties drover writes match.")

(defconst drover-task-org-body-limit 6000
  "Characters of a heading's body a task's prompt quotes at most.")

(defconst drover-task-org-attempts 9
  "Attempts a heading tells at most, the properties of each ending in its number.")

;;;; Files

(defun drover-task-org-linked-worktree-p (file)
  "Return non-nil when FILE is in a linked Git worktree, whose .git is a file."
  (when-let* ((root (locate-dominating-file file ".git")))
    (file-regular-p (expand-file-name ".git" root))))

(defun drover-task-org-refused-p (file)
  "Return non-nil when tasks are never read from FILE nor written to it.
See `drover-task-refused-files'."
  (or (null file)
      (let ((file (drover-task-canonical file)))
        (or (seq-some (lambda (refused)
                        (let ((refused (drover-task-canonical refused)))
                          (or (equal refused file)
                              (string-prefix-p (file-name-as-directory refused) file))))
                      drover-task-refused-files)
            (drover-task-org-linked-worktree-p file)))))

;;;; Finding headings

(defun drover-task-org--position (id)
  "Return the position of the heading of ID in the current buffer, or nil.
Its ID, else its ORIG_ID, the ID ejira replaced."
  (org-with-wide-buffer
   (or (org-find-property "ID" id)
       (org-find-property "ORIG_ID" id))))

(defun drover-task-org--marker-in (buffer id)
  "Return a marker at the heading of ID in BUFFER, or nil."
  (with-current-buffer buffer
    (when-let* (((derived-mode-p 'org-mode))
                (position (drover-task-org--position id)))
      (copy-marker position))))

(defun drover-task-org--visited ()
  "Return the Org buffers visiting files tasks may be in."
  (seq-filter (lambda (buffer)
                (let ((file (buffer-file-name buffer)))
                  (and file
                       (null (buffer-base-buffer buffer))
                       (eq (buffer-local-value 'major-mode buffer) 'org-mode)
                       (not (drover-task-org-refused-p file)))))
              (buffer-list)))

(defun drover-task-org--known-file (id)
  "Return the file `org-id-locations' records for ID, or nil.
It is never looked for anywhere else, nor in the current buffer."
  (require 'org-id)
  (unless org-id-locations
    (ignore-errors (org-id-locations-load)))
  (when (hash-table-p org-id-locations)
    (gethash id org-id-locations)))

(defun drover-task-org-marker (id &optional visited-only)
  "Return a marker at the heading of ID, by its ID or ORIG_ID, or nil.
It is looked for in the file `org-id-locations' records, visited if
need be unless VISITED-ONLY, then in the Org buffers visiting files:
never in refused files (see `drover-task-org-refused-p'), nor by
updating the ID locations, which reads every file."
  (when (and (stringp id) (not (string-empty-p id)))
    (let ((file (drover-task-org--known-file id)))
      (or (and file
               (not (drover-task-org-refused-p file))
               (when-let* ((buffer (or (find-buffer-visiting file)
                                       (and (not visited-only) (file-readable-p file)
                                            (find-file-noselect file t)))))
                 (drover-task-org--marker-in buffer id)))
          (seq-some (lambda (buffer) (drover-task-org--marker-in buffer id))
                    (drover-task-org--visited))))))

(defun drover-task-org-files ()
  "Return the Org files a heading may be searched for in, not refused.
Those `org-id-locations' and `org-agenda-files' name."
  (require 'org-id)
  (seq-uniq
   (seq-filter (lambda (file) (and (stringp file) (not (drover-task-org-refused-p file))
                                   (file-readable-p file)))
               (mapcar #'expand-file-name
                       (append (ignore-errors (org-id-files))
                               (ignore-errors (org-agenda-files t)))))))

(defun drover-task-org-search-regexp (id)
  "Return the regexp, of ripgrep's syntax, of the property lines naming ID."
  (concat "^[ \t]*:(ORIG_)?ID:[ \t]+"
          (replace-regexp-in-string "[^A-Za-z0-9_-]" (lambda (char) (concat "\\\\" char)) id)
          "[ \t]*$"))

(defun drover-task-org-find (id callback)
  "Find the heading of ID and call CALLBACK with a marker at it, or nil.
At once when `drover-task-org-marker' finds it, else once ripgrep looked
for its ID or ORIG_ID in `drover-task-org-files', from a process: the
first file holding it is visited, and searched."
  (if-let* ((marker (drover-task-org-marker id)))
      (funcall callback marker)
    (let ((files (drover-task-org-files)))
      (if (or (null files) (not (executable-find "rg")))
          (funcall callback nil)
        (drover-command-run
         "rg task" (append (list "rg" "--files-with-matches" "--no-messages" "-e"
                                 (drover-task-org-search-regexp id) "--")
                           files)
         (lambda (result)
           (drover-with-reporting "task heading"
             (funcall callback
                      (seq-some (lambda (file)
                                  (and (member file files)
                                       (when-let* ((buffer (or (find-buffer-visiting file)
                                                               (find-file-noselect file t))))
                                         (drover-task-org--marker-in buffer id))))
                                (split-string (or (plist-get result :output) "") "\n" t))))))))))

;;;; Reading headings

(defun drover-task-org-jira-key (id properties)
  "Return the Jira key of the heading of ID with PROPERTIES, or nil.
Its JIRA property, else its ID when ejira made it the key."
  (or (cdr (assoc "JIRA" properties))
      (and (stringp id) (let ((case-fold-search nil)) (string-match-p drover-task-org-jira-regexp id)) id)))

(defun drover-task-org-attempt-suffix (number)
  "Return what the properties of the attempt NUMBER end in: \"\" for the first."
  (if (= number 1) "" (format "_%d" number)))

(defun drover-task-org--attempt (number)
  "Return the attempt NUMBER of the heading at point, from its properties.
Those of the first end in nothing, of the second in _2, and so on."
  (let* ((suffix (drover-task-org-attempt-suffix number))
         (worktree (org-entry-get nil (concat "DROVER_WORKTREE" suffix)))
         (directory (org-entry-get nil (concat "DROVER_DIRECTORY" suffix))))
    (when (or worktree directory)
      (list :number number
            :worktree worktree
            :directory (or directory worktree)
            :branch (org-entry-get nil (concat "DROVER_BRANCH" suffix))
            :host (org-entry-get nil (concat "DROVER_HOST" suffix))
            :base (org-entry-get nil (concat "DROVER_BASE" suffix))
            :repo (org-entry-get nil (concat "DROVER_ATTEMPT_REPO" suffix))
            :pr (org-entry-get nil (concat "DROVER_PR" suffix))))))

(defun drover-task-org--folder-link ()
  "Return the first directory a file link of the entry at point names, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point)))
          (found nil))
      (while (and (not found) (re-search-forward org-link-bracket-re end t))
        (let ((link (match-string-no-properties 1)))
          (when (string-match "\\`file\\(?:\\+[a-z]+\\)?:\\(.+?\\)\\(?:::.*\\)?\\'" link)
            (let ((file (expand-file-name (match-string 1 link))))
              (when (file-directory-p file)
                (setq found (file-name-as-directory file)))))))
      found)))

(defconst drover-task-org-overrides
  '((:finish . "DROVER_FINISH") (:placement . "DROVER_WORKTREES") (:setup . "DROVER_SETUP")
    (:checks . "DROVER_CHECKS") (:teardown . "DROVER_TEARDOWN") (:target . "DROVER_TARGET"))
  "The properties of a heading overriding how its repository works, by key.
DROVER_FINISH is merge-push, merge, pr or none; DROVER_WORKTREES
sibling or default; DROVER_SETUP, DROVER_CHECKS and DROVER_TEARDOWN
shell commands joined by &&; DROVER_TARGET the branch work lands on.")

(defun drover-task-org--overrides ()
  "Return the overrides of the entry at point, inherited, as a plist of strings."
  (let ((overrides nil))
    (pcase-dolist (`(,key . ,property) (reverse drover-task-org-overrides))
      (when-let* ((value (org-entry-get nil property t))
                  ((not (string-blank-p value))))
        (setq overrides (cons key (cons (substring-no-properties (string-trim value)) overrides)))))
    overrides))

(defun drover-task-org-body ()
  "Return the body of the entry at point, its drawers and sub-headings left out.
Bounded to `drover-task-org-body-limit' characters; nil when empty."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (org-end-of-meta-data t)
      (when (< (point) end)
        (let ((text (string-trim
                     (replace-regexp-in-string
                      "^[ \t]*:\\([A-Za-z_-]+\\):[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" ""
                      (buffer-substring-no-properties (point) end)))))
          (unless (string-empty-p text)
            (if (> (length text) drover-task-org-body-limit)
                (concat (substring text 0 drover-task-org-body-limit) "\n…")
              text)))))))

(defun drover-task-org-read (marker)
  "Return what the heading at MARKER tells of its task, as a plist.
:id, its ID, :orig-id, the ID ejira replaced, :title, plain text, its
links shown as their descriptions, :todo, its TODO
keyword, :done, non-nil for a done one, :category, its Org category,
the task's project, :jira, its Jira key (see `drover-task-org-jira-key'),
:ejira, non-nil for a heading ejira syncs, :file, :repos, the
repositories DROVER_REPO names, inherited, :kind, DROVER_KIND,
inherited, a symbol, :folder, DROVER_FOLDER, inherited, else a
directory a file link of its entry names, :goal-checks,
DROVER_GOAL_CHECKS, inherited, the checks its goals run, joined by &&,
:overrides, a plist of what the heading says of how its repository
works, overriding what the repository says, its properties inherited
and unparsed (see `drover-task-org-overrides'), :attempts, the
worktrees or directories it works in, each with its :number, and
:tick, what tells it changed."
  (with-current-buffer (marker-buffer marker)
    (org-with-point-at marker
      (let* ((properties (org-entry-properties nil 'standard))
             (id (cdr (assoc "ID" properties)))
             (todo (org-get-todo-state))
             (repos (org-entry-get nil "DROVER_REPO" t))
             (kind (org-entry-get nil "DROVER_KIND" t))
             (goal-checks (org-entry-get nil "DROVER_GOAL_CHECKS" t))
             (type (cdr (assoc "TYPE" properties)))
             (attempts nil))
        ;; A fan-out may leave gaps, an attempt whose place was never made.
        (cl-loop for n from 1 to drover-task-org-attempts
                 for attempt = (drover-task-org--attempt n)
                 when attempt do (push attempt attempts))
        (list :id id
              :orig-id (cdr (assoc "ORIG_ID" properties))
              ;; Its links as their descriptions, without the buffer's faces nor prefixes.
              :title (org-link-display-format (substring-no-properties (org-get-heading t t t t)))
              :todo todo
              :done (and todo (member todo org-done-keywords) t)
              :category (let ((category (org-get-category)))
                          (and (stringp category) (not (string-empty-p category))
                               (substring-no-properties category)))
              :jira (drover-task-org-jira-key id properties)
              :ejira (and (stringp type) (string-prefix-p "ejira-" type))
              :file (buffer-file-name (or (buffer-base-buffer) (current-buffer)))
              :repos (and repos (split-string repos "[ \t,]+" t))
              :kind (and kind (not (string-blank-p kind)) (intern (downcase (string-trim kind))))
              :folder (or (when-let* ((folder (org-entry-get nil "DROVER_FOLDER" t)))
                            (file-name-as-directory (expand-file-name folder)))
                          (drover-task-org--folder-link))
              :overrides (drover-task-org--overrides)
              :goal-checks (and goal-checks (not (string-blank-p goal-checks))
                                (substring-no-properties (string-trim goal-checks)))
              :attempts (nreverse attempts)
              :tick (cons (current-buffer) (buffer-chars-modified-tick)))))))

(defun drover-task-org-fresh-p (info)
  "Return non-nil when INFO, of `drover-task-org-read', still tells its heading."
  (let ((tick (plist-get info :tick)))
    (and (buffer-live-p (car tick))
         (eql (buffer-chars-modified-tick (car tick)) (cdr tick)))))

;;;; Writing headings

(defun drover-task-org--add-note (text)
  "Add a note TEXT to the LOGBOOK of the heading at point, as Org notes go.
In a drawer whatever `org-log-into-drawer' says: a note after the
metadata would be part of the body, which ejira takes for an issue's
description."
  (let ((org-log-into-drawer (if (stringp org-log-into-drawer) org-log-into-drawer t))
        (lines (split-string (string-trim text) "\n")))
    (save-excursion
      (goto-char (org-log-beginning t))
      (cond ((not (bolp)) (insert "\n"))
            ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
      (if-let* ((item (org-in-item-p)))
          (indent-line-to (let ((struct (save-excursion (goto-char item) (org-list-struct))))
                            (org-list-get-ind (org-list-get-top-point struct) struct)))
        (org-indent-line))
      (insert (org-list-bullet-string "-")
              (org-replace-escapes (cdr (assq 'note org-log-note-headings))
                                   (list (cons "%t" (format-time-string
                                                     (org-time-stamp-format 'long 'inactive)))))
              " \\\\")
      (let ((indent (org-list-item-body-column (line-beginning-position))))
        (dolist (line lines)
          (insert "\n")
          (unless (string-empty-p line)
            (indent-line-to indent)
            (insert line)))))))

(defun drover-task-org--save (buffer)
  "Save BUFFER, as an external change for ejira: no review pops."
  (with-current-buffer buffer
    (dlet ((ejira-push-on-save nil))
      (save-buffer))))

(defun drover-task-org-write (marker properties &optional note)
  "Write PROPERTIES and NOTE on the heading at MARKER, and save its file.
PROPERTIES is an alist (NAME . VALUE), each NAME beginning with
DROVER_, a nil VALUE deleting it; NOTE, a string, goes to its LOGBOOK.
Nothing else of the heading changes.  The file is saved when it had no
unsaved change before, as a change ejira counts but does not review;
otherwise the buffer keeps both, saved when the user saves it.  Signal
an error for a refused file or another property.  Return non-nil when
saved."
  (let ((buffer (marker-buffer marker)))
    (unless (buffer-live-p buffer)
      (error "The task's heading is gone"))
    (when (drover-task-org-refused-p (buffer-file-name (or (buffer-base-buffer buffer) buffer)))
      (error "Tasks are not written in %s" (buffer-name buffer)))
    (pcase-dolist (`(,name . ,_) properties)
      (unless (and (stringp name) (let ((case-fold-search nil))
                                    (string-match-p drover-task-org-property-regexp name)))
        (error "Drover writes no property %s" name)))
    (let* ((base (or (buffer-base-buffer buffer) buffer))
           (clean (not (buffer-modified-p base))))
      (with-current-buffer buffer
        (org-with-point-at marker
          (pcase-dolist (`(,name . ,value) properties)
            (if value
                (org-entry-put nil name (if (stringp value) value (format "%s" value)))
              (org-entry-delete nil name)))
          (when note
            (drover-task-org--add-note note))))
      (when (and clean (buffer-file-name base))
        (drover-task-org--save base)
        t))))

(defun drover-task-org-mark-done (marker)
  "Set the heading at MARKER to its first done keyword, the normal Org way.
As `org-todo' does, its logging included; the file is saved when it
had no unsaved change, as any save, so that ejira offers the issue's
transition through its own review.  Return the keyword."
  (let* ((buffer (marker-buffer marker))
         (base (or (buffer-base-buffer buffer) buffer))
         (clean (not (buffer-modified-p base)))
         (keyword nil))
    (with-current-buffer buffer
      (org-with-point-at marker
        (setq keyword (if (member "DONE" org-done-keywords) "DONE" (car org-done-keywords)))
        (org-todo keyword)))
    (when (and clean (buffer-file-name base))
      (with-current-buffer base (save-buffer)))
    keyword))

(provide 'drover-task-org)
;;; drover-task-org.el ends here
