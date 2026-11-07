;;; drover-review.el --- Reviewing the changes a session made  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, vc

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

;; `drover-review', R in lists and in `drover-dispatch', shows the
;; changes a session made in a buffer of Magit's sections, a file per
;; section and a hunk per section within it, in one of three scopes:
;;
;; - its last turn, and
;; - the whole session, as its engine recorded them: OpenCode's diffs
;;   of the snapshots its service took of the working tree, files Git
;;   does not track included; the edits and writes of Claude Code's
;;   tools, and the patches Codex applied, as their transcripts and
;;   rollouts recorded them, each hunk as its tool numbered it;
;; - the working tree against the commit the session started from: its
;;   commits since, what is not committed, and the files Git does not
;;   track yet, from Git.  The commit is where HEAD was as the session
;;   was created, as the reflog of the worktree tells, else the commit
;;   its branch forked from its remote's default branch, else HEAD.
;;
;; A session of another host is reviewed through its host, which
;; computes the changes with its own backends and sends them, bounded
;; (see `drover-remote').
;;
;; On any line of a diff, or a region of lines, c writes a comment,
;; shown under the line, editable (c again) and removable (k); C writes
;; a note on the whole review.  Comments are kept per conversation, in
;; `drover-state-directory', until sent: C-c C-c composes them into one
;; prompt, each with its file, its lines and the lines quoted, and puts
;; it in drover's prompt, set to go to the session reviewed, to be read
;; and sent; with a prefix argument, it is sent at once.  Comments sent
;; stay, dimmed, for a week (`drover-review-sent-days').
;;
;; A diff of thousands of lines draws in slices of a few milliseconds,
;; from a timer, its first screen at once.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'diff-mode)
(require 'magit-section)
(require 'drover-core)
(require 'drover-model)
(require 'drover-ui)
(require 'drover-command)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 17))
  (load "drover-core" nil t))

(declare-function drover-prompt-edit-for "drover-prompt" (session text))
(declare-function drover-prompt-send-to "drover-prompt" (session text &optional options))
(declare-function org-escape-code-in-string "org-src" (s))
(declare-function magit-diff-working-tree "ext:magit-diff" (&optional rev args files))

;;;; Options

(defcustom drover-review-default-scope 'turn
  "What `drover-review' shows of a session, when it can.
`turn', the changes of its last turn; `session', those of all its
turns; `tree', its working tree against the commit it started from.  A
session whose engine records no changes shows its working tree."
  :type '(choice (const :tag "Last turn" turn)
                 (const :tag "Whole session" session)
                 (const :tag "Working tree" tree))
  :group 'drover)

(defcustom drover-review-context 3
  "Unchanged lines around each hunk of the diffs Git and OpenCode make."
  :type 'natnum
  :group 'drover)

(defcustom drover-review-sent-days 7
  "Days a comment sent stays, dimmed, among a session's comments."
  :type 'number
  :group 'drover)

(defconst drover-review-limit (* 8 1024 1024)
  "Characters of patches a review keeps at most; past them, files show counts.")

(defconst drover-review-file-limit 2000
  "Files a review shows at most.")

(defconst drover-review-line-limit 1000
  "Characters of a line of a diff shown at most.")

(defconst drover-review-chunk-lines 200
  "Lines of a diff drawn at a time, between two looks at the clock.")

(defconst drover-review-slice 0.012
  "Seconds a slice of a review's drawing takes, about.")

(defconst drover-review-untracked-limit 200
  "Untracked files a review of the working tree shows at most.")

(defconst drover-review-untracked-bytes (* 2 1024 1024)
  "Bytes of untracked files a review of the working tree reads at most.")

(defconst drover-review-quote-lines 20
  "Lines of the diff a comment quotes in the prompt at most.")

(defconst drover-review-comment-lines 200
  "Lines of a diff a comment is on at most.")

(defconst drover-review-comment-limit 200
  "Comments kept per session at most, the oldest sent going first.")

(defconst drover-review-session-limit 200
  "Sessions whose comments are kept at most, those changed last.")

(defconst drover-review-scopes
  '((turn . "Last turn") (session . "Whole session") (tree . "Working tree"))
  "The scopes of a review, and their names.")

;;;; Faces

(defface drover-review-added '((t :inherit diff-added :extend t))
  "Face of the lines a change added."
  :group 'drover-faces)

(defface drover-review-removed '((t :inherit diff-removed :extend t))
  "Face of the lines a change removed."
  :group 'drover-faces)

(defface drover-review-context '((t :inherit diff-context :extend t))
  "Face of the unchanged lines around a change."
  :group 'drover-faces)

(defface drover-review-file-heading '((t :inherit diff-file-header :extend t))
  "Face of the heading of a file changed."
  :group 'drover-faces)

(defface drover-review-hunk-heading '((t :inherit diff-hunk-header :extend t))
  "Face of the heading of a hunk."
  :group 'drover-faces)

(defface drover-review-title '((t :inherit magit-section-heading))
  "Face of the title of a review."
  :group 'drover-faces)

(defface drover-review-comment '((t :inherit font-lock-doc-face :extend t))
  "Face of a comment not sent yet."
  :group 'drover-faces)

(defface drover-review-comment-mark '((t :inherit warning))
  "Face of the mark beside a comment, and in the fringe of the lines it is on."
  :group 'drover-faces)

(defface drover-review-sent '((t :inherit shadow :extend t))
  "Face of a comment sent."
  :group 'drover-faces)

;;;; Sections

(defclass drover-review-file-section (magit-section) ()
  "The section of a file a review shows.")

(defclass drover-review-hunk-section (magit-section) ()
  "The section of a hunk of a file a review shows.")

(defclass drover-review-commits-section (magit-section) ()
  "The section of the commits a review of the working tree counts.")

(defclass drover-review-comments-section (magit-section) ()
  "The section of comments the lines of a review do not hold.")

;;;; Which sessions, which scopes

(defun drover-review--engine-changes (session)
  "Return the :changes of the engine of SESSION, a function, or nil."
  (and (drover-session-engine-id session)
       (let ((function (drover-engine-property (drover-session-engine session) :changes)))
         (and (functionp function) function))))

(defun drover-review-scopes-of (session)
  "Return the scopes SESSION may be reviewed in, a list of symbols.
A session of another host may be reviewed in any, its host telling what
it cannot show."
  (if (drover-session-host session)
      (and (drover-backend-property session :review) (mapcar #'car drover-review-scopes))
    (append (and (drover-review--engine-changes session) '(turn session))
            (and (drover-session-local-directory session) '(tree)))))

;;;###autoload
(defun drover-reviewable-p (session)
  "Return non-nil when the changes SESSION made may be reviewed.
See `drover-review'."
  (and (drover-session-p session)
       (drover-review-scopes-of session)
       t))

;;;; Fetching

(defun drover-review-fetch (session scope callback)
  "Fetch the changes SESSION made in SCOPE, and call CALLBACK with them.
SCOPE is a key of `drover-review-scopes'.  The changes are a plist:

 :files     The files changed, each a plist of :file, its name,
            relative to :directory; :status, `added', `deleted',
            `modified' or `renamed', with :from, its former name;
            :additions and :deletions, counts of lines, or nil when
            unknown; :patch, the hunks of a unified diff, or nil when
            unknown; and :binary, non-nil for a binary file.
 :directory The directory files are named relative to, on this host,
            or nil.
 :base      The commit a review of the working tree compares with, and
 :how       why that one, a string.
 :commits   The commits since :base, a list of (HASH . SUBJECT).
 :notes     Lines telling what the review leaves out, or nil.

or (:failure MESSAGE).  Everything is bounded (see
`drover-review--bound').  A session of another host is reviewed by its
host, through its backend's :review; the working tree from Git (see
`drover-review--git'); a turn and a session through its engine's
:changes (see `drover-register-engine').  CALLBACK is called once,
possibly before this returns; an error it signals is reported briefly."
  (let ((called nil))
    (cl-flet ((deliver (review)
                (unless called
                  (setq called t)
                  (drover-with-reporting "review"
                    (funcall callback (drover-review--bound review))))))
      (condition-case err
          (let ((directory (drover-session-local-directory session))
                (changes (drover-review--engine-changes session)))
            (cond
             ((drover-session-host session)
              (if-let* ((function (drover-backend-property session :review))
                        ((functionp function)))
                  (funcall function session scope #'deliver)
                (deliver (list :failure (format "%s tells no changes" (drover-backend-label session))))))
             ((eq scope 'tree)
              (if directory
                  (drover-review--git directory (drover-session-created session) #'deliver)
                (deliver (list :failure "The session has no directory here"))))
             (changes
              (funcall changes (drover-session-engine-id session) scope directory
                       (lambda (result)
                         (deliver (if (eq (car-safe result) :failure)
                                      result
                                    (append (list :directory directory) result))))))
             (t
              (deliver (list :failure
                             (format "%s records no changes of its own: review the working tree, W"
                                     (drover-engine-label (drover-session-engine session))))))))
        (error (deliver (list :failure (drover-error-message err))))))))

(defun drover-review--bound (review)
  "Return REVIEW, as `drover-review-fetch' tells it, bounded.
Past `drover-review-file-limit' files, the others are left out; past
`drover-review-limit' characters of patches, the files after show their
counts alone; names and notes are lines.  The notes tell what was left
out."
  (if-let* ((failure (plist-get review :failure)))
      (list :failure (or (drover-bound-line (format "%s" failure)) "failed"))
    (let* ((files (plist-get review :files))
           (notes (delq nil (mapcar #'drover-bound-line (seq-take (plist-get review :notes) 10))))
           (room drover-review-limit)
           (cut nil)
           (kept nil))
      (when (> (length files) drover-review-file-limit)
        (push (format "%d more files are left out" (- (length files) drover-review-file-limit)) notes)
        (setq files (seq-take files drover-review-file-limit)))
      (dolist (file files)
        (when-let* ((name (drover-bound-line (plist-get file :file) 1024)))
          (let* ((patch (plist-get file :patch))
                 (patch (and (stringp patch) (not (string-empty-p patch)) patch)))
            (when patch
              (if (> (length patch) room)
                  (setq cut (or cut name)
                        patch nil
                        room 0)
                (setq room (- room (length patch)))))
            (push (list :file name
                        :from (drover-bound-line (plist-get file :from) 1024)
                        :status (if (memq (plist-get file :status) '(added deleted modified renamed))
                                    (plist-get file :status)
                                  'modified)
                        :additions (and (natnump (plist-get file :additions)) (plist-get file :additions))
                        :deletions (and (natnump (plist-get file :deletions)) (plist-get file :deletions))
                        :binary (and (plist-get file :binary) t)
                        :patch patch)
                  kept))))
      (when cut
        (push (format "The diff is cut past %d MB: from %s on, files show their counts alone"
                      (/ drover-review-limit 1024 1024) cut)
              notes))
      (list :files (nreverse kept)
            :directory (plist-get review :directory)
            :base (drover-bound-line (plist-get review :base) 64)
            :how (drover-bound-line (plist-get review :how))
            :commits (mapcar (lambda (commit)
                               (cons (or (drover-bound-line (car commit) 64) "")
                                     (or (drover-bound-line (cdr commit)) "")))
                             (seq-take (seq-filter (lambda (commit) (and (consp commit) (stringp (car commit))))
                                                   (plist-get review :commits))
                                       100))
            :notes (nreverse notes)))))

;;;; Patches

(defun drover-review--unquote (name)
  "Return NAME, a file name as Git writes it in a diff, unquoted.
Git quotes a name holding special characters, C-style, its bytes not
ASCII in octal."
  (if (not (string-prefix-p "\"" name))
      name
    (let ((bytes nil)
          (index 1)
          (end (1- (length name))))
      (while (< index end)
        (let ((char (aref name index)))
          (if (/= char ?\\)
              (progn (setq bytes (nconc (nreverse (string-to-list (encode-coding-string (string char) 'utf-8)))
                                        bytes))
                     (cl-incf index))
            (let ((next (and (< (1+ index) end) (aref name (1+ index)))))
              (cond ((and next (<= ?0 next ?7) (< (+ index 3) (length name)))
                     (push (string-to-number (substring name (1+ index) (+ index 4)) 8) bytes)
                     (cl-incf index 4))
                    (t (push (pcase next (?t ?\t) (?n ?\n) (?r ?\r) (?a 7) (?b 8) (?f 12) (?v 11)
                                    (_ (or next ?\\)))
                             bytes)
                       (cl-incf index 2)))))))
      (decode-coding-string (apply #'unibyte-string (nreverse bytes)) 'utf-8))))

(defun drover-review--diff-name (text)
  "Return the file TEXT, after --- or +++ in a diff, names, nil for /dev/null."
  (let ((name (drover-review--unquote (car (split-string text "\t")))))
    (cond ((equal name "/dev/null") nil)
          ((string-match "\\`[abciwo]/" name) (substring name 2))
          (t name))))

(defun drover-review-parse-git (text)
  "Return the files the Git diff TEXT changes.
They are as `drover-review-fetch' tells them, each file's patch its
hunks, without the lines before them."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((files nil))
      (while (re-search-forward "^diff --git \\(.*\\)$" nil t)
        (let* ((header (match-string 1))
               (start (line-beginning-position 2))
               (end (save-excursion
                      (if (re-search-forward "^diff --git " nil t) (match-beginning 0) (point-max))))
               (hunks (save-excursion
                        (if (re-search-forward "^@@" end t) (match-beginning 0) end)))
               (name (and (string-match " \"?b/" header)
                          (drover-review--unquote
                           (let ((rest (substring header (1+ (match-beginning 0)))))
                             (if (string-prefix-p "\"" rest) rest (substring rest 2))))))
               (name (if (string-prefix-p "b/" (or name "")) (substring name 2) name))
               (status 'modified) (from nil) (binary nil)
               (additions 0) (deletions 0))
          (goto-char start)
          (while (< (point) hunks)
            (let ((line (buffer-substring-no-properties (point) (line-end-position))))
              (cond ((string-prefix-p "new file mode" line) (setq status 'added))
                    ((string-prefix-p "deleted file mode" line) (setq status 'deleted))
                    ((string-prefix-p "rename from " line)
                     (setq status 'renamed from (drover-review--unquote (substring line 12))))
                    ((string-prefix-p "rename to " line)
                     (setq name (drover-review--unquote (substring line 10))))
                    ((string-prefix-p "+++ " line)
                     (when-let* ((new (drover-review--diff-name (substring line 4))))
                       (setq name new)))
                    ((string-prefix-p "--- " line)
                     (unless name (setq name (drover-review--diff-name (substring line 4)))))
                    ((string-prefix-p "Binary files " line) (setq binary t))))
            (forward-line 1))
          (goto-char hunks)
          (while (re-search-forward "^[-+]" end t)
            (if (eq (char-before) ?+) (cl-incf additions) (cl-incf deletions)))
          (when name
            (push (list :file name :status status :from from :binary binary
                        :additions additions :deletions deletions
                        :patch (and (< hunks end) (buffer-substring-no-properties hunks end)))
                  files))
          (goto-char end)))
      (nreverse files))))

(defun drover-review--new-file-patch (content)
  "Return the hunk adding CONTENT, a file's text, and its count of lines."
  (let* ((lines (if (string-empty-p content)
                    nil
                  (split-string (string-remove-suffix "\n" content) "\n")))
         (count (length lines)))
    (cons (concat (format "@@ -0,0 +1,%d @@\n" count)
                  (mapconcat (lambda (line) (concat "+" line "\n")) lines "")
                  (and lines (not (string-suffix-p "\n" content)) "\\ No newline at end of file\n"))
          count)))

;;;; The working tree, from Git

(defun drover-review--git-run (directory arguments then)
  "Run Git with ARGUMENTS in DIRECTORY, and call THEN with its result.
THEN gets the output, a string, and the errors, once Git succeeded; or
nil and a message, once it failed or could not run."
  (drover-command-run
   (concat "git " (car arguments)) (cons "git" arguments)
   (lambda (result)
     (drover-with-reporting "review"
       (cond ((plist-get result :failure)
              (funcall then nil (plist-get result :failure)))
             ((eql (plist-get result :exit) 0)
              (funcall then (or (plist-get result :output) "") (plist-get result :errors)
                       (plist-get result :truncated)))
             (t (funcall then nil (or (drover-bound-line (plist-get result :errors))
                                      (format "git %s failed" (car arguments))))))))
   directory))

(defun drover-review--git (directory created callback)
  "Find how the working tree of DIRECTORY changed, and call CALLBACK with it.
The tree is compared with the commit HEAD was at CREATED, a float time,
when the session started, as the reflog of the worktree tells; without
CREATED or a reflog reaching it, with the commit HEAD forked from the
default branch of origin; else with HEAD.  The changes are its commits
since, what is not committed, and the files Git does not track, read
here, `drover-review-untracked-limit' of them at most, of
`drover-review-untracked-bytes' in all.  CALLBACK gets them as
`drover-review-fetch' tells them."
  (let ((notes nil)
        top base how commits files)
    (cl-labels
        ((fail (message) (funcall callback (list :failure message)))
         (toplevel ()
           (drover-review--git-run
            directory '("rev-parse" "--show-toplevel")
            (lambda (output &optional message &rest _)
              (if (not output)
                  (fail (format "Not in a Git repository: %s" (or message directory)))
                (setq top (file-name-as-directory (drover--native-file-name (string-trim output))))
                (if created
                    (started)
                  (forked))))))
         (started ()
           (drover-review--git-run
            directory (list "rev-parse" "--verify" (format "HEAD@{@%d}^{commit}" (truncate created)))
            (lambda (output &optional errors &rest _)
              (if (not output)
                  (forked)
                (setq base (string-trim output)
                      how (if (and (stringp errors) (string-match-p "only goes back" errors))
                              "where HEAD was as far back as the reflog goes"
                            "where HEAD was as the session started"))
                (commits-since)))))
         (forked ()
           (drover-review--git-run
            directory '("merge-base" "HEAD" "refs/remotes/origin/HEAD")
            (lambda (output &rest _)
              (if output
                  (setq base (string-trim output)
                        how "where the branch forked from origin's default branch")
                (setq base "HEAD"
                      how "HEAD: no record of where the session started")
                (push "Only what is not committed: no record of where the session started" notes))
              (commits-since))))
         (commits-since ()
           (if (equal base "HEAD")
               (changes)
             (drover-review--git-run
              directory (list "log" "--format=%h %s" "-n" "100" (concat base "..HEAD"))
              (lambda (output &rest _)
                (dolist (line (and output (split-string output "\n" t)))
                  (push (if (string-match "\\`\\([^ ]+\\) \\(.*\\)\\'" line)
                            (cons (match-string 1 line) (match-string 2 line))
                          (cons line ""))
                        commits))
                (setq commits (nreverse commits))
                (changes)))))
         (changes ()
           (drover-review--git-run
            directory (list "-c" "core.quotePath=false" "diff" "--no-color" "--no-ext-diff"
                            "--find-renames" "--src-prefix=a/" "--dst-prefix=b/"
                            (format "-U%d" drover-review-context) base "--")
            (lambda (output &optional message truncated &rest _)
              (if (not output)
                  (fail message)
                (when truncated
                  (push "Git's diff is cut past a megabyte: its last files are left out" notes))
                (setq files (drover-review-parse-git output))
                (untracked)))))
         (untracked ()
           (drover-review--git-run
            directory '("ls-files" "-z" "--others" "--exclude-standard")
            (lambda (output &rest _)
              (let ((names (and output (split-string output "\0" t)))
                    (room drover-review-untracked-bytes)
                    (added nil))
                (when (> (length names) drover-review-untracked-limit)
                  (push (format "%d more files Git does not track are left out"
                                (- (length names) drover-review-untracked-limit))
                        notes)
                  (setq names (seq-take names drover-review-untracked-limit)))
                (dolist (name names)
                  (let* ((file (expand-file-name name top))
                         (size (file-attribute-size (file-attributes file))))
                    (cond
                     ((not (and size (file-regular-p file)))
                      nil)
                     ((> size room)
                      (push (list :file name :status 'added) added))
                     (t
                      (setq room (- room size))
                      (let ((content (with-temp-buffer
                                       (set-buffer-multibyte nil)
                                       (insert-file-contents-literally file)
                                       (buffer-string))))
                        (if (string-search "\0" content)
                            (push (list :file name :status 'added :binary t) added)
                          (pcase-let ((`(,patch . ,count)
                                       (drover-review--new-file-patch
                                        (decode-coding-string content 'utf-8-unix))))
                            (push (list :file name :status 'added :additions count :deletions 0
                                        :patch patch)
                                  added))))))))
                (funcall callback
                         (list :files (append files (nreverse added))
                               :directory top :base base :how how :commits commits
                               :notes (nreverse notes))))))))
      (toplevel))))

;;;; Comments, kept per conversation

(defconst drover-review--version 1
  "Version of the format of the file of the comments of reviews.")

(defvar drover-review--store nil
  "The comments of reviews, a hash table, or nil until read from their file.
Its keys are those of `drover-review-key', each value a plist of :note,
the note on the whole review, :comments, the comments, oldest first,
and :time, when they last changed.  Each comment is a plist of
:id, a number; :file; :side, `new' or `old', the side of the diff its
lines are numbered on, and :start and :end, those numbers, or nil when
the diff numbers none; :lines, the lines of the diff it is on, with
their prefix; :hunk, the heading of their hunk; :text; :time, when
written; :state, `pending' or `sent'; and :sent, when sent.")

(defvar drover-review--store-file nil
  "The file `drover-review--store' was read from.")

(defvar drover-review--serial 0
  "The identifier of the latest comment.")

(defvar drover-review--save-timer nil
  "Timer keeping the comments, or nil.")

(defvar drover-review--warned nil
  "Non-nil once a problem with the file of comments was reported.")

(defun drover-review--file ()
  "Return the file the comments of reviews are kept in."
  (expand-file-name "review.eld" drover-state-directory))

(defun drover-review-key (session)
  "Return the key the comments on SESSION's changes are kept by.
Its conversation's, (ENGINE . ID), or (HOST ENGINE . ID) for a session
of another host (see `drover-session-engine-key'), so that they follow
the conversation from one front end to another; else its own key."
  (or (drover-session-engine-key session)
      (cons :session (drover-session-key session))))

(defun drover-review--warn (format-string &rest args)
  "Report a problem with the file of comments, once.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless drover-review--warned
    (setq drover-review--warned t)
    (apply #'drover-warn format-string args)))

(defun drover-review--comment-p (comment)
  "Return non-nil when COMMENT is a comment as kept."
  (and (plistp comment)
       (natnump (plist-get comment :id))
       (stringp (plist-get comment :file))
       (stringp (plist-get comment :text))
       (memq (plist-get comment :side) '(new old nil))
       (cl-every (lambda (key) (let ((value (plist-get comment key))) (or (null value) (natnump value))))
                 '(:start :end))
       (proper-list-p (plist-get comment :lines))
       (cl-every #'stringp (plist-get comment :lines))
       (memq (plist-get comment :state) '(pending sent))))

(defun drover-review--store ()
  "Return the comments of reviews, read from their file once."
  (let ((file (drover-review--file)))
    (unless (and drover-review--store (equal drover-review--store-file file))
      (let ((table (make-hash-table :test #'equal)))
        (condition-case err
            (dolist (entry (drover-read-data file drover-review--version :reviews))
              (pcase entry
                (`(,key . ,(and (pred plistp) data))
                 (let ((comments (seq-filter #'drover-review--comment-p (plist-get data :comments))))
                   (dolist (comment comments)
                     (setq drover-review--serial (max drover-review--serial (plist-get comment :id))))
                   (puthash key (list :note (and (stringp (plist-get data :note)) (plist-get data :note))
                                      :comments comments
                                      :time (and (numberp (plist-get data :time)) (plist-get data :time)))
                            table)))))
          (error (drover-review--warn "Ignoring the comments of reviews in %s: %s"
                                      (abbreviate-file-name file) (drover-error-message err))))
        (setq drover-review--store table
              drover-review--store-file file)))
    drover-review--store))

(defun drover-review--entry (key)
  "Return the comments and note kept for KEY, a plist, or nil."
  (gethash key (drover-review--store)))

(defun drover-review-comments (key)
  "Return the comments kept for KEY, oldest first."
  (plist-get (drover-review--entry key) :comments))

(defun drover-review-note (key)
  "Return the note on the whole review kept for KEY, or nil."
  (plist-get (drover-review--entry key) :note))

(defun drover-review--put (key comments note)
  "Keep COMMENTS and NOTE for KEY, and save them soon."
  (let ((table (drover-review--store)))
    (if (or comments note)
        (puthash key (list :note note :comments comments :time (float-time)) table)
      (remhash key table)))
  (drover-review--save-soon))

(defun drover-review--save-soon ()
  "Save the comments of reviews in a moment, and as Emacs exits."
  (add-hook 'kill-emacs-hook #'drover-review--save-at-exit)
  (unless (timerp drover-review--save-timer)
    (setq drover-review--save-timer
          (run-at-time 2 nil (lambda ()
                               (drover-with-reporting "keeping review comments"
                                 (setq drover-review--save-timer nil)
                                 (drover-review-save)))))))

(defun drover-review--prune (comments)
  "Return COMMENTS without those sent past `drover-review-sent-days', bounded."
  (let* ((since (- (float-time) (* 86400 drover-review-sent-days)))
         (kept (seq-remove (lambda (comment)
                             (and (eq (plist-get comment :state) 'sent)
                                  (< (or (plist-get comment :sent) 0) since)))
                           comments)))
    (while (> (length kept) drover-review-comment-limit)
      (setq kept (let ((sent (seq-find (lambda (comment) (eq (plist-get comment :state) 'sent)) kept)))
                   (if sent (delq sent kept) (cdr kept)))))
    kept))

(defun drover-review-save ()
  "Keep the comments of reviews in their file, those of the latest sessions.
Comments sent long ago go (see `drover-review-sent-days'), and so do
those of sessions past `drover-review-session-limit'.  A failure is
reported once."
  (when (timerp drover-review--save-timer)
    (cancel-timer drover-review--save-timer)
    (setq drover-review--save-timer nil))
  (when drover-review--store
    (let ((entries nil))
      (maphash (lambda (key data)
                 (let ((comments (drover-review--prune (plist-get data :comments))))
                   (when (or comments (plist-get data :note))
                     (push (cons key (list :note (plist-get data :note) :comments comments
                                           :time (plist-get data :time)))
                           entries))))
               drover-review--store)
      (setq entries (seq-take (sort entries :key (lambda (entry) (or (plist-get (cdr entry) :time) 0))
                                    :reverse t)
                              drover-review-session-limit))
      (condition-case err
          (if (and (null entries) (not (file-exists-p drover-review--store-file)))
              nil
            (drover-write-data drover-review--store-file "The comments of reviews of sessions"
                               drover-review--version :reviews entries))
        (error (drover-review--warn "Could not keep the comments of reviews in %s: %s"
                                    (abbreviate-file-name drover-review--store-file)
                                    (drover-error-message err)))))))

(defun drover-review--save-at-exit ()
  "Keep the comments of reviews as Emacs exits."
  (drover-with-reporting "keeping review comments"
    (when (timerp drover-review--save-timer)
      (drover-review-save))))

(defun drover-review-add-comment (key comment)
  "Keep COMMENT, a plist as `drover-review--store' describes, for KEY.
It gets an identifier, its time and the state `pending' unless it has
them.  Return it."
  (let ((comment (append comment
                         (list :id (cl-incf drover-review--serial)
                               :time (float-time) :state 'pending))))
    (setq comment (list :id (plist-get comment :id)
                        :file (plist-get comment :file)
                        :side (plist-get comment :side)
                        :start (plist-get comment :start)
                        :end (plist-get comment :end)
                        :lines (plist-get comment :lines)
                        :hunk (plist-get comment :hunk)
                        :text (or (drover-bound-text (plist-get comment :text)
                                                     drover-text-lines drover-text-limit)
                                  "")
                        :time (plist-get comment :time)
                        :state (plist-get comment :state)
                        :sent (plist-get comment :sent)))
    (drover-review--put key (append (drover-review-comments key) (list comment))
                        (drover-review-note key))
    comment))

(defun drover-review-edit-comment (key id text)
  "Set the TEXT of the comment ID kept for KEY; a comment sent is pending again.
An empty TEXT removes it."
  (if (string-blank-p text)
      (drover-review-remove-comment key id)
    (drover-review--put key
                        (mapcar (lambda (comment)
                                  (if (eql (plist-get comment :id) id)
                                      (thread-first comment
                                                    (copy-sequence)
                                                    (plist-put :text (drover-bound-text
                                                                      text drover-text-lines drover-text-limit))
                                                    (plist-put :state 'pending)
                                                    (plist-put :sent nil))
                                    comment))
                                (drover-review-comments key))
                        (drover-review-note key))))

(defun drover-review-remove-comment (key id)
  "Forget the comment ID kept for KEY."
  (drover-review--put key
                      (seq-remove (lambda (comment) (eql (plist-get comment :id) id))
                                  (drover-review-comments key))
                      (drover-review-note key)))

(defun drover-review-set-note (key note)
  "Set the NOTE on the whole review kept for KEY; nil or blank removes it."
  (drover-review--put key (drover-review-comments key)
                      (and note (not (string-blank-p note))
                           (drover-bound-text (string-trim note) drover-text-lines drover-text-limit))))

(defun drover-review-pending (key)
  "Return the comments kept for KEY not sent yet."
  (seq-filter (lambda (comment) (eq (plist-get comment :state) 'pending))
              (drover-review-comments key)))

(defun drover-review-mark-sent (key ids)
  "Mark the comments IDS kept for KEY sent, and the note gone with them."
  (let ((now (float-time)))
    (drover-review--put key
                        (mapcar (lambda (comment)
                                  (if (memql (plist-get comment :id) ids)
                                      (thread-first comment
                                                    (copy-sequence)
                                                    (plist-put :state 'sent)
                                                    (plist-put :sent now))
                                    comment))
                                (drover-review-comments key))
                        nil)))

;;;; The prompt a review sends

(defun drover-review--lines-reference (comment)
  "Return where COMMENT is, such as \"Line 42\", for the prompt."
  (let ((start (plist-get comment :start))
        (end (plist-get comment :end)))
    (cond ((not (or start end)) "At these lines")
          ((eq (plist-get comment :side) 'old)
           (if (eql start end)
               (format "Removed line %d (of the file before the change)" start)
             (format "Removed lines %d-%d (of the file before the change)" start end)))
          ((eql start end) (format "Line %d" end))
          (t (format "Lines %d-%d" start end)))))

(defun drover-review--quote (comment)
  "Return the Org block quoting the lines of COMMENT, indented for a list item."
  (require 'org-src)
  (let* ((lines (plist-get comment :lines))
         (shown (seq-take lines drover-review-quote-lines))
         (left (- (length lines) (length shown)))
         (body (string-join (cons (or (plist-get comment :hunk) "@@") shown) "\n")))
    (concat "  #+begin_src diff\n"
            (replace-regexp-in-string "^" "  " (org-escape-code-in-string body))
            "\n  #+end_src\n"
            (and (> left 0) (format "  (%d more line%s left out.)\n" left (if (= left 1) "" "s"))))))

(defun drover-review-compose (what comments note &optional order)
  "Return the Org text of a prompt sending COMMENTS and NOTE to an agent.
WHAT tells which changes they are on, such as \"in the last turn\".
Comments are grouped by file, the files in ORDER first, a list of names,
the others by name; within a file, by line.  Each tells its lines (see
`drover-review--lines-reference'), quotes them with their hunk's
heading in a diff block, which the prompt sends as a Markdown fence,
then gives its text.  NOTE, if any, ends the prompt."
  (let* ((files (seq-uniq (mapcar (lambda (comment) (plist-get comment :file)) comments)))
         (files (sort files :key (lambda (file)
                                   (if-let* ((index (seq-position order file)))
                                       (format "0%09d" index)
                                     (concat "1" file)))))
         (count (length comments)))
    (concat
     (format "Review of your changes %s%s\n" what
             (if comments
                 (format ": %d comment%s on %d file%s." count (if (= count 1) "" "s")
                         (length files) (if (cdr files) "s" ""))
               "."))
     (mapconcat
      (lambda (file)
        (concat
         "\n`" file "`:\n"
         (mapconcat
          (lambda (comment)
            (concat "- " (drover-review--lines-reference comment) ":\n"
                    (drover-review--quote comment)
                    (replace-regexp-in-string "^" "  " (string-trim (plist-get comment :text)))
                    "\n"))
          (sort (seq-filter (lambda (comment) (equal (plist-get comment :file) file)) comments)
                :key (lambda (comment) (or (plist-get comment :start) (plist-get comment :end) 0)))
          "")))
      files "")
     (and note (concat "\nOverall: " (string-trim note) "\n")))))

;;;; The review buffer

(defvar-local drover-review--session nil
  "The session the review shows the changes of, as last known.")

(defvar-local drover-review--key nil
  "The key of the comments on the session reviewed (see `drover-review-key').")

(defvar-local drover-review--scope nil
  "What the review shows, a key of `drover-review-scopes'.")

(defvar-local drover-review--data nil
  "The changes the review shows, as `drover-review-fetch' told them, or nil.")

(defvar-local drover-review--fetching nil
  "Non-nil while the changes of the review are fetched.")

(defvar-local drover-review--generation 0
  "How many times the review was fetched, so that a stale answer is dropped.")

(defvar-local drover-review--drawing nil
  "What is left to draw of the review, a plist, or nil once drawn.
:files, the files not drawn yet; :file, the one being drawn; :patch and
:position, its patch and how far it was read; :old and :new, the line
numbers there; :hunk, the heading of the hunk there; and links to the
sections being drawn into, :file-section and :hunk-section.")

(defvar-local drover-review--timer nil
  "Timer drawing the rest of the review, or nil.")

(defvar-local drover-review--placed nil
  "The comments shown, a hash table of their identifiers to their overlays.")

(defun drover-review--current-session ()
  "Return the session reviewed, as its backend reports it now if listed."
  (or (and drover-review--session
           (ignore-errors (drover-index-session (drover-session-key drover-review--session))))
      drover-review--session))

(defvar-keymap drover-review-mode-map
  :doc "Keys of a review of a session's changes."
  :parent magit-section-mode-map
  "RET" #'drover-review-visit
  "c" #'drover-review-comment
  "k" #'drover-review-remove
  "C" #'drover-review-edit-note
  "C-c C-c" #'drover-review-send
  "C-c C-k" #'drover-review-discard
  "T" #'drover-review-turn
  "S" #'drover-review-whole-session
  "W" #'drover-review-working-tree
  "M" #'drover-review-magit
  "P" #'drover-review-patch)

(define-derived-mode drover-review-mode magit-section-mode "Drover review"
  "Review the changes a session made, and comment on them.
Files and hunks are sections: TAB folds them, n and p move between
them.  On a line of a diff, or a region of lines, \\[drover-review-comment]
writes a comment, shown under it, or edits the one there;
\\[drover-review-remove] removes it; \\[drover-review-edit-note] writes a note on
the whole review.  \\[drover-review-send] puts the comments not sent and the
note in drover's prompt as one prompt, set to go to the session, to be
read and sent; with a prefix argument, it sends them at once.
\\[drover-review-discard] discards them.  \\[drover-review-turn] shows the last turn,
\\[drover-review-whole-session] the whole session, \\[drover-review-working-tree] the working tree;
\\[drover-review-visit] visits the line at point; \\[drover-review-magit] shows the
working tree in Magit, which stages and discards; \\[drover-review-patch] shows
the review as a patch.

\\{drover-review-mode-map}"
  :group 'drover
  (setq-local revert-buffer-function #'drover-review--revert)
  (setq-local drover-session-at-point-function #'drover-review--current-session)
  (setq-local header-line-format '(:eval (drover-review--header-line)))
  (setq drover-review--placed (make-hash-table)))

(defun drover-review--buffer (session &optional create)
  "Return the review buffer of SESSION, made with CREATE when there is none."
  (let ((key (drover-review-key session)))
    (or (seq-find (lambda (buffer)
                    (and (eq (buffer-local-value 'major-mode buffer) 'drover-review-mode)
                         (equal (buffer-local-value 'drover-review--key buffer) key)))
                  (buffer-list))
        (when create
          (with-current-buffer (generate-new-buffer
                                (format "*drover review: %s*"
                                        (or (drover-bound-line (drover-session-display-title session) 60)
                                            "session")))
            (drover-review-mode)
            (setq drover-review--key key
                  drover-review--session session)
            (current-buffer))))))

;;;###autoload
(defun drover-review (session &optional scope)
  "Review the changes SESSION made, in SCOPE, and comment on them.
SCOPE is `turn', its last turn, `session', all its turns, or `tree', its
working tree against the commit it started from (see
`drover-review-fetch'); by default `drover-review-default-scope', as
far as SESSION can show it.  Interactively, SESSION is the session at
point, and a prefix argument asks for SCOPE.  The review shows in
another window than the session's (see `drover-other-window'), in its
buffer of Magit's sections, `drover-review-mode'."
  (interactive
   (let* ((session (drover-session-at-point))
          (scopes (or (drover-review-scopes-of session)
                      (user-error "The changes of %s cannot be reviewed"
                                  (drover-session-display-title session)))))
     (list session
           (and current-prefix-arg
                (let ((name (completing-read "Review: "
                                             (mapcar (lambda (scope)
                                                       (alist-get scope drover-review-scopes))
                                                     scopes)
                                             nil t)))
                  (car (rassoc name drover-review-scopes)))))))
  (let* ((scopes (or (drover-review-scopes-of session)
                     (user-error "The changes of %s cannot be reviewed"
                                 (drover-session-display-title session))))
         (buffer (drover-review--buffer session t))
         (scope (cond ((memq scope scopes) scope)
                      (scope (user-error "%s cannot show %s" (drover-session-display-title session)
                                         (downcase (alist-get scope drover-review-scopes))))
                      ((buffer-local-value 'drover-review--scope buffer))
                      ((memq drover-review-default-scope scopes) drover-review-default-scope)
                      (t (car scopes)))))
    (with-current-buffer buffer
      (setq drover-review--session session)
      (drover-review--load scope))
    (unless (eq (window-buffer) buffer)
      (let ((window (or (get-buffer-window buffer)
                        (drover-other-window (drover-session-window) buffer))))
        (set-window-buffer window buffer)
        (select-window window)))
    buffer))

(defun drover-review--revert (&rest _)
  "Fetch the changes of the review again, and draw them; `revert-buffer' here."
  (drover-review--load drover-review--scope))

(defun drover-review--load (scope)
  "Fetch the changes of the session reviewed in SCOPE, and draw them.
What showed stays until they arrive."
  (let ((buffer (current-buffer))
        (generation (cl-incf drover-review--generation))
        (session (drover-review--current-session)))
    (setq drover-review--scope scope
          drover-review--fetching t)
    (if (or (null drover-review--data) (zerop (buffer-size)))
        (drover-review--draw-waiting)
      (force-mode-line-update))
    (drover-review-fetch
     session scope
     (lambda (review)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (eql generation drover-review--generation)
             (setq drover-review--data review
                   drover-review--fetching nil)
             (drover-review--draw))))))))

(defun drover-review--draw-waiting ()
  "Draw the review as its changes are fetched."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Fetching the changes of %s…\n"
                                (downcase (alist-get drover-review--scope drover-review-scopes)))
                        'font-lock-face 'drover-dim))))

;;;; Drawing

(defun drover-review--scope-phrase (data)
  "Return which changes DATA, a review, shows, as a phrase for a prompt."
  (pcase drover-review--scope
    ('turn "in the last turn")
    ('session "in this session")
    (_ (if-let* ((base (plist-get data :base))
                 ((not (equal base "HEAD"))))
           (format "in the working tree since commit %s" (substring base 0 (min 12 (length base))))
         "in the working tree, not committed yet"))))

(defun drover-review--counts (files)
  "Return the files FILES are and their lines added and deleted, as a string."
  (let ((additions 0) (deletions 0))
    (dolist (file files)
      (cl-incf additions (or (plist-get file :additions) 0))
      (cl-incf deletions (or (plist-get file :deletions) 0)))
    (format "%d file%s, %s %s" (length files) (if (= (length files) 1) "" "s")
            (propertize (format "+%d" additions) 'font-lock-face 'diff-indicator-added)
            (propertize (format "−%d" deletions) 'font-lock-face 'diff-indicator-removed))))

(defun drover-review--draw ()
  "Draw the review of `drover-review--data' anew, its files in slices.
Point keeps to the section it was in."
  (when (timerp drover-review--timer)
    (cancel-timer drover-review--timer))
  (setq drover-review--timer nil)
  (clrhash drover-review--placed)
  (let* ((inhibit-read-only t)
         (data drover-review--data)
         (session drover-review--session)
         (here (when-let* ((section (magit-current-section)))
                 (cons (magit-section-ident section) (- (point) (oref section start))))))
    (remove-overlays)
    (erase-buffer)
    (setq magit-root-section nil)
    (magit-insert-section (drover-review)
      (insert (propertize (format "Review of %s" (drover-session-display-title session))
                          'font-lock-face 'drover-review-title)
              "\n")
      (insert (propertize (alist-get drover-review--scope drover-review-scopes) 'font-lock-face 'bold))
      (if-let* ((failure (plist-get data :failure)))
          (insert ": " (propertize failure 'font-lock-face 'error) "\n")
        (insert ": " (drover-review--counts (plist-get data :files)))
        (when-let* ((base (plist-get data :base)))
          (insert (propertize (format ", against %s, %s"
                                      (if (equal base "HEAD") base (substring base 0 (min 12 (length base))))
                                      (plist-get data :how))
                              'font-lock-face 'drover-dim)))
        (insert "\n")
        (dolist (note (plist-get data :notes))
          (insert (propertize note 'font-lock-face 'warning) "\n"))
        (unless (plist-get data :files)
          (insert (propertize "No changes.\n" 'font-lock-face 'drover-dim))))
      (insert "\n")
      (drover-review--insert-note)
      (when-let* ((commits (plist-get data :commits)))
        (magit-insert-section (drover-review-commits-section)
          (magit-insert-heading (propertize (format "Commits since %s (%d)"
                                                    (substring (plist-get data :base) 0
                                                               (min 12 (length (plist-get data :base))))
                                                    (length commits))
                                            'font-lock-face 'magit-section-heading))
          (pcase-dolist (`(,hash . ,subject) commits)
            (insert (propertize hash 'font-lock-face 'magit-hash) " " subject "\n"))
          (insert "\n"))))
    (setq drover-review--drawing (list :files (plist-get data :files)))
    (drover-review--draw-some)
    (if here
        (if-let* ((section (magit-get-section (car here))))
            (goto-char (min (+ (oref section start) (cdr here)) (oref section end)))
          (goto-char (point-min)))
      (goto-char (point-min)))
    (when drover-review--drawing
      (drover-review--draw-later))
    (set-buffer-modified-p nil)))

(defun drover-review--insert-note ()
  "Insert the note on the whole review, if any, at point."
  (when-let* ((note (drover-review-note drover-review--key)))
    (let ((start (point)))
      (insert (propertize "Note on the whole review: " 'font-lock-face 'drover-review-comment-mark)
              (propertize note 'font-lock-face 'drover-review-comment)
              "\n\n")
      (put-text-property start (point) 'drover-review-note t))))

(defun drover-review--draw-later ()
  "Draw more of the review from a timer."
  (setq drover-review--timer
        (run-at-time 0.005 nil
                     (let ((buffer (current-buffer)))
                       (lambda ()
                         (drover-with-reporting "review drawing"
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (setq drover-review--timer nil)
                               (when drover-review--drawing
                                 (drover-review--draw-some)
                                 (when drover-review--drawing
                                   (drover-review--draw-later)))))))))))

(defun drover-review--ends (&rest sections)
  "Move the ends of SECTIONS, and of the root, to the end of the buffer."
  (dolist (section (cons magit-root-section sections))
    (when (and section (< (oref section end) (point-max)))
      (set-marker (oref section end) (point-max)))))

(defun drover-review--draw-some ()
  "Draw some of the files left of the review, for `drover-review-slice' seconds.
Lines are appended to the hunk drawn last, hunks to the file drawn last,
each line telling its kind and numbers (`drover-review-line'), so that
a file of thousands of lines draws over several slices."
  (let ((inhibit-read-only t)
        (until (+ (float-time) drover-review-slice))
        (state drover-review--drawing))
    (save-excursion
      (while (and state (< (float-time) until))
        (if (plist-get state :file)
            (drover-review--draw-chunk state)
          (if-let* ((file (pop (plist-get state :files))))
              (drover-review--begin-file state file)
            (setq state nil)
            (drover-review--draw-unplaced)))))
    (setq drover-review--drawing state)
    (set-buffer-modified-p nil)))

(defun drover-review--status-label (file)
  "Return the status of FILE, a file of a review, as its heading tells it."
  (format "%-10s" (if (plist-get file :binary) "binary" (symbol-name (plist-get file :status)))))

(defun drover-review--begin-file (state file)
  "Draw the heading of FILE, a file of a review, and note it in STATE."
  (goto-char (point-max))
  (let* ((magit-insert-section--parent magit-root-section)
         (section (magit-insert-section (drover-review-file-section (plist-get file :file))
                    (magit-insert-heading
                      (propertize (concat (drover-review--status-label file)
                                          (if (plist-get file :from)
                                              (format "%s → %s" (plist-get file :from) (plist-get file :file))
                                            (plist-get file :file))
                                          (if (or (plist-get file :additions) (plist-get file :deletions))
                                              (format "  +%d −%d" (or (plist-get file :additions) 0)
                                                      (or (plist-get file :deletions) 0))
                                            ""))
                                  'font-lock-face 'drover-review-file-heading)))))
    (drover-review--ends)
    (plist-put state :file file)
    (plist-put state :file-section (drover-link section))
    (plist-put state :hunk-section nil)
    (plist-put state :hunk nil)
    (plist-put state :patch (or (plist-get file :patch) ""))
    (plist-put state :position 0)
    (unless (plist-get file :patch)
      (insert (propertize (cond ((plist-get file :binary) "Binary file.\n")
                                ((eq (plist-get file :status) 'deleted) "Deleted.\n")
                                ((eq (plist-get file :status) 'renamed) "Renamed.\n")
                                ((plist-get file :additions) "No patch: only its counts are known.\n")
                                (t "No patch.\n"))
                          'font-lock-face 'drover-dim
                          'magit-section section))
      (drover-review--ends section))))

(defconst drover-review--hunk-regexp
  "\\`@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@"
  "Regexp matching the heading of a hunk, and its first old and new lines.")

(defun drover-review--line (text kind old new)
  "Return the line TEXT of a diff, of KIND, numbered OLD and NEW, to insert.
KIND is the character it begins with.  It is cut to
`drover-review-line-limit' characters."
  (propertize (concat (if (> (length text) drover-review-line-limit)
                          (concat (substring text 0 drover-review-line-limit) "…")
                        text)
                      "\n")
              'font-lock-face (pcase kind
                                (?+ 'drover-review-added)
                                (?- 'drover-review-removed)
                                (_ 'drover-review-context))
              'drover-review-line (vector kind old new)))

(defun drover-review--draw-chunk (state)
  "Draw up to `drover-review-chunk-lines' lines of the file STATE draws.
A new hunk begins a section of its own in the file's; the file done,
the comments on it are shown."
  (goto-char (point-max))
  (let* ((patch (plist-get state :patch))
         (position (plist-get state :position))
         (file-section (drover-linked (plist-get state :file-section)))
         (hunk-section (drover-linked (plist-get state :hunk-section)))
         (count 0)
         (pieces nil))
    (cl-flet ((flush ()
                (when pieces
                  (let ((start (point)))
                    (apply #'insert (nreverse pieces))
                    (put-text-property start (point) 'magit-section hunk-section))
                  (setq pieces nil)
                  (drover-review--ends hunk-section file-section))))
      (while (and (< position (length patch)) (< count drover-review-chunk-lines))
        (let* ((end (or (string-search "\n" patch position) (length patch)))
               (line (substring patch position end)))
          (setq position (min (length patch) (1+ end)))
          (cond
           ((string-prefix-p "@@" line)
            (flush)
            (let ((numbered (string-match drover-review--hunk-regexp line)))
              (plist-put state :old (and numbered (string-to-number (match-string 1 line))))
              (plist-put state :new (and numbered (string-to-number (match-string 2 line)))))
            (plist-put state :hunk line)
            (let ((magit-insert-section--parent file-section))
              (setq hunk-section
                    (magit-insert-section (drover-review-hunk-section line)
                      (magit-insert-heading
                        (propertize (if (> (length line) drover-review-line-limit)
                                        (substring line 0 drover-review-line-limit)
                                      line)
                                    'font-lock-face 'drover-review-hunk-heading)))))
            (plist-put state :hunk-section (drover-link hunk-section))
            (drover-review--ends file-section))
           ((not hunk-section))
           ((string-empty-p line))
           (t
            (let* ((kind (aref line 0))
                   (old (plist-get state :old))
                   (new (plist-get state :new)))
              (pcase kind
                (?+ (push (drover-review--line line kind nil new) pieces)
                    (when new (plist-put state :new (1+ new))))
                (?- (push (drover-review--line line kind old nil) pieces)
                    (when old (plist-put state :old (1+ old))))
                (?\s (push (drover-review--line line kind old new) pieces)
                     (when old (plist-put state :old (1+ old)))
                     (when new (plist-put state :new (1+ new))))
                (?\\ (push (drover-review--line line kind nil nil) pieces))
                ;; The headers of a patch concatenated after another.
                (_ (flush)
                   (setq hunk-section nil)
                   (plist-put state :hunk-section nil))))
            (cl-incf count)))))
      (flush))
    (plist-put state :position position)
    (when (>= position (length patch))
      (let ((name (plist-get (plist-get state :file) :file)))
        (plist-put state :file nil)
        (drover-review--place-file file-section name)))
    (dolist (section (list hunk-section file-section))
      (when section
        (if (oref section hidden)
            (magit-section-hide section)
          (magit-section-maybe-update-visibility-indicator section))))))

;;;; Comments shown

(defun drover-review--line-info (&optional position)
  "Return what the line of a diff at POSITION is, or nil.
A plist of :kind, the character it begins with, :old and :new, its
numbers, either nil, :text, the line, :hunk, the heading of its hunk,
and :file, the name of its file."
  (save-excursion
    (when position (goto-char position))
    (when-let* ((info (get-text-property (line-beginning-position) 'drover-review-line))
                (section (magit-current-section))
                (file (magit-section-parent-value section)))
      (list :kind (aref info 0) :old (aref info 1) :new (aref info 2)
            :text (buffer-substring-no-properties (line-beginning-position) (line-end-position))
            :hunk (oref section value)
            :file file))))

(defun drover-review--format-comment (comment)
  "Return COMMENT as it shows under its lines, after a newline."
  (let* ((sent (eq (plist-get comment :state) 'sent))
         (face (if sent 'drover-review-sent 'drover-review-comment))
         (mark (propertize "▌ " 'font-lock-face (if sent 'drover-review-sent 'drover-review-comment-mark)
                           'face (if sent 'drover-review-sent 'drover-review-comment-mark)))
         (lines (split-string (plist-get comment :text) "\n")))
    (concat "\n"
            (mapconcat (lambda (line) (concat mark (propertize line 'face face)))
                       (if sent
                           (cons (format-time-string "Sent %a %H:%M" (plist-get comment :sent)) lines)
                         lines)
                       "\n"))))

(defun drover-review--show-comment (comment start end)
  "Show COMMENT under the lines from START to END, and mark them in the fringe."
  (let ((overlays nil)
        (id (plist-get comment :id)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((overlay (make-overlay (line-beginning-position) (line-end-position) nil t nil)))
          (overlay-put overlay 'drover-review-comment id)
          (overlay-put overlay 'before-string
                       (propertize " " 'display '(left-fringe filled-rectangle drover-review-comment-mark)))
          (push overlay overlays))
        (forward-line 1)))
    (when overlays
      (overlay-put (car overlays) 'after-string (drover-review--format-comment comment)))
    (puthash id overlays drover-review--placed)))

(defun drover-review--hide-comment (id)
  "Stop showing the comment ID."
  (mapc #'delete-overlay (gethash id drover-review--placed))
  (remhash id drover-review--placed))

(defun drover-review--comment-matches (comment kind old new text)
  "Return non-nil when COMMENT ends on the line of KIND, OLD, NEW and TEXT.
Its last line must read the same, and be numbered the same when both
are numbered."
  (let ((last (car (last (plist-get comment :lines))))
        (number (if (eq (plist-get comment :side) 'old) old new))
        (end (plist-get comment :end)))
    (ignore kind)
    (and last (> (length last) 0) (> (length text) 0)
         (equal (substring last 1) (substring text 1))
         (or (not end) (not number) (= end number)))))

(defun drover-review--place-file (section file)
  "Show the comments on FILE not shown yet on the lines of its SECTION.
A comment goes where its last line reads and is numbered the same,
else where its lines read the same, anywhere in the file."
  (when-let* ((comments (seq-filter (lambda (comment)
                                      (and (equal (plist-get comment :file) file)
                                           (not (gethash (plist-get comment :id) drover-review--placed))))
                                    (drover-review-comments drover-review--key))))
    (let ((lines nil))
      (save-excursion
        (goto-char (oref section start))
        (while (< (point) (oref section end))
          (when-let* ((info (get-text-property (point) 'drover-review-line)))
            (push (list (point) (aref info 0) (aref info 1) (aref info 2)
                        (buffer-substring-no-properties (point) (line-end-position)))
                  lines))
          (forward-line 1)))
      (setq lines (vconcat (nreverse lines)))
      (dolist (comment comments)
        (let* ((count (max 1 (length (plist-get comment :lines))))
               (found (or (cl-loop for index from 0 below (length lines)
                                   for (_ kind old new text) = (aref lines index)
                                   when (drover-review--comment-matches comment kind old new text)
                                   return index)
                          ;; Lines moved: those numbered by line go where they
                          ;; read the same, several in a row.
                          (and (or (cdr (plist-get comment :lines)) (not (plist-get comment :end)))
                               (drover-review--find-lines lines (plist-get comment :lines))))))
          (when found
            (drover-review--show-comment comment
                                         (car (aref lines (max 0 (- found (1- count)))))
                                         (1+ (car (aref lines found))))))))))

(defun drover-review--find-lines (lines wanted)
  "Return the index in LINES of the last of WANTED, read in a row, or nil.
LINES is a vector of (POSITION KIND OLD NEW TEXT); WANTED are texts."
  (let ((count (length wanted)))
    (when (> count 0)
      (cl-loop for index from (1- count) below (length lines)
               when (cl-loop for offset from 0 below count
                             always (equal (nth 4 (aref lines (+ (- index (1- count)) offset)))
                                           (nth offset wanted)))
               return index))))

(defun drover-review--draw-unplaced ()
  "Draw the comments the lines of the review do not hold, at its end."
  (goto-char (point-max))
  (when-let* ((comments (seq-remove (lambda (comment) (gethash (plist-get comment :id) drover-review--placed))
                                    (drover-review-comments drover-review--key))))
    (let ((magit-insert-section--parent magit-root-section))
      (magit-insert-section (drover-review-comments-section)
        (magit-insert-heading
          (propertize (format "Comments on lines this review does not show (%d)" (length comments))
                      'font-lock-face 'magit-section-heading))
        (dolist (comment comments)
          (let ((start (point)))
            (insert (propertize (format "%s, %s:\n" (plist-get comment :file)
                                        (downcase (drover-review--lines-reference comment)))
                                'font-lock-face 'drover-dim))
            (dolist (line (seq-take (plist-get comment :lines) drover-review-quote-lines))
              (insert (drover-review--line line (aref (concat line " ") 0) nil nil)))
            (insert (substring (drover-review--format-comment comment) 1) "\n")
            (put-text-property start (point) 'drover-review-comment (plist-get comment :id))))))
    (drover-review--ends)))

(defun drover-review--comment-ids-at (&optional start end)
  "Return the identifiers of the comments on the lines from START to END.
They default to the line at point."
  (let ((start (or start (line-beginning-position)))
        (end (or end (line-end-position))))
    (seq-uniq (delq nil (cons (get-text-property start 'drover-review-comment)
                              (mapcar (lambda (overlay) (overlay-get overlay 'drover-review-comment))
                                      (overlays-in start (max (1+ start) end))))))))

(defun drover-review--comment (id)
  "Return the comment ID of the session reviewed, or nil."
  (seq-find (lambda (comment) (eql (plist-get comment :id) id))
            (drover-review-comments drover-review--key)))

(defun drover-review--redraw-comment (id)
  "Show the comment ID anew, as it is kept now, where it showed.
One among those the lines do not hold draws the review anew."
  (when (and (not (gethash id drover-review--placed))
             (text-property-any (point-min) (point-max) 'drover-review-comment id))
    (drover-review--draw))
  (when-let* ((overlays (gethash id drover-review--placed))
              (start (overlay-start (car (last overlays))))
              (end (overlay-end (car overlays))))
    (drover-review--hide-comment id)
    (when-let* ((comment (drover-review--comment id)))
      (drover-review--show-comment comment start (1+ end)))))

;;;; Commands

(defun drover-review--check ()
  "Signal a `user-error' outside a review."
  (unless (derived-mode-p 'drover-review-mode)
    (user-error "Not in a review")))

(defun drover-review--region-lines ()
  "Return the lines of a diff the region or point is on, a list of line infos.
See `drover-review--line-info'.  Signal a `user-error' when they are not
lines of one hunk."
  (let* ((start (if (use-region-p) (region-beginning) (point)))
         (end (if (use-region-p) (max start (1- (region-end))) (point)))
         (infos nil))
    (save-excursion
      (goto-char start)
      (while (progn
               (when-let* ((info (drover-review--line-info)))
                 (push (append (list :position (line-beginning-position)) info) infos))
               (and (zerop (forward-line 1)) (<= (point) end)))))
    (setq infos (nreverse infos))
    (unless infos
      (user-error "No line of a diff here"))
    (when (> (length infos) drover-review-comment-lines)
      (user-error "Comment on %d lines at most" drover-review-comment-lines))
    (unless (and (seq-every-p (lambda (info) (equal (plist-get info :file) (plist-get (car infos) :file))) infos)
                 (seq-every-p (lambda (info) (equal (plist-get info :hunk) (plist-get (car infos) :hunk))) infos))
      (user-error "Comment on the lines of one hunk at a time"))
    infos))

(defun drover-review-comment-lines (infos text)
  "Comment TEXT on the lines INFOS tell, and show it; return the comment.
INFOS are what `drover-review--line-info' tells of each, in order, with
their :position.  The comment is numbered on the new side when one of
its lines is there, else on the old side."
  (let* ((news (delq nil (mapcar (lambda (info) (plist-get info :new)) infos)))
         (olds (delq nil (mapcar (lambda (info) (plist-get info :old)) infos)))
         (side (if (or news (not olds)) 'new 'old))
         (numbers (if (eq side 'new) news olds))
         (comment (drover-review-add-comment
                   drover-review--key
                   (list :file (plist-get (car infos) :file)
                         :side side
                         :start (and numbers (apply #'min numbers))
                         :end (and numbers (apply #'max numbers))
                         :lines (mapcar (lambda (info) (plist-get info :text)) infos)
                         :hunk (plist-get (car infos) :hunk)
                         :text text))))
    (let ((inhibit-read-only t))
      (drover-review--show-comment comment (plist-get (car infos) :position)
                                   (1+ (plist-get (car (last infos)) :position))))
    (drover-review--update-header)
    comment))

(defvar-local drover-review--edit-target nil
  "What the comment edited here is for, a plist.
:buffer, its review's; :id, the comment's, or :infos, the lines a new
one is on; :note, non-nil for the note on the whole review.")

(defvar-keymap drover-review-edit-mode-map
  :doc "Keys of the buffer a comment of a review is written in."
  "C-c C-c" #'drover-review-edit-finish
  "C-c C-k" #'drover-review-edit-cancel)

(define-derived-mode drover-review-edit-mode text-mode "Drover comment"
  "Write a comment on a review.
\\[drover-review-edit-finish] keeps it, \\[drover-review-edit-cancel] leaves it.

\\{drover-review-edit-mode-map}"
  :group 'drover
  (setq-local header-line-format
              (substitute-command-keys
               "Comment: \\[drover-review-edit-finish] keeps it, \\[drover-review-edit-cancel] leaves it")))

(defun drover-review--edit (text target)
  "Edit TEXT in a buffer below, for TARGET.
TARGET is a plist, as `drover-review--edit-target' tells it."
  (let ((buffer (get-buffer-create "*drover review comment*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (drover-review-edit-mode)
        (insert (or text ""))
        (setq drover-review--edit-target target)))
    (pop-to-buffer buffer '((display-buffer-below-selected) (window-height . 8)))))

(defun drover-review-comment (&optional text)
  "Comment on the line at point, or the region's, or edit the comment there.
A region always gets a comment of its own; a line holding a comment
has it edited.  The comment is written in a buffer below, unless TEXT
is given."
  (interactive)
  (drover-review--check)
  ;; A region is lines to comment on, whatever comments they hold.
  (let ((ids (unless (use-region-p) (drover-review--comment-ids-at))))
    (cond
     ((and ids text)
      (drover-review-edit-comment drover-review--key (car ids) text)
      (drover-review--redraw-comment (car ids)))
     (ids
      (let ((id (if (cdr ids) (drover-review--choose ids "Edit the comment: ") (car ids))))
        (drover-review--edit (plist-get (drover-review--comment id) :text)
                             (list :buffer (current-buffer) :id id))))
     (t
      (let ((infos (drover-review--region-lines)))
        (deactivate-mark)
        (if text
            (drover-review-comment-lines infos text)
          (drover-review--edit nil (list :buffer (current-buffer) :infos infos))))))))

(defun drover-review--choose (ids prompt)
  "Read one of the comments IDS, with PROMPT, and return its identifier."
  (let* ((choices (mapcar (lambda (id)
                            (cons (or (drover-bound-line (plist-get (drover-review--comment id) :text) 80)
                                      (number-to-string id))
                                  id))
                          ids))
         (choice (completing-read prompt choices nil t)))
    (cdr (assoc choice choices))))

(defun drover-review-edit-note ()
  "Write, or edit, the note on the whole review, which ends its prompt."
  (interactive)
  (drover-review--check)
  (drover-review--edit (drover-review-note drover-review--key)
                       (list :buffer (current-buffer) :note t)))

(defun drover-review-edit-finish ()
  "Keep the comment written here, and go back to the review."
  (interactive)
  (let ((target drover-review--edit-target)
        (text (string-trim (buffer-string))))
    (quit-window t)
    (let ((buffer (plist-get target :buffer)))
      (unless (buffer-live-p buffer)
        (user-error "The review is gone; the comment is in the kill ring"))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (cond
           ((plist-get target :note)
            (drover-review-set-note drover-review--key text)
            (drover-review--draw-note))
           ((plist-get target :id)
            (let ((id (plist-get target :id)))
              (drover-review-edit-comment drover-review--key id text)
              (if (string-blank-p text)
                  (drover-review--hide-comment id)
                (drover-review--redraw-comment id))))
           ((not (string-blank-p text))
            (drover-review-comment-lines (plist-get target :infos) text))))
        (drover-review--update-header)))))

(defun drover-review-edit-cancel ()
  "Leave the comment written here, which goes to the kill ring."
  (interactive)
  (unless (string-blank-p (buffer-string))
    (kill-new (buffer-string)))
  (quit-window t))

(defun drover-review--draw-note ()
  "Draw the note on the whole review anew, where it shows."
  (save-excursion
    (let ((inhibit-read-only t))
      (if-let* ((start (text-property-any (point-min) (point-max) 'drover-review-note t)))
          (progn (goto-char start)
                 (delete-region start (or (next-single-property-change start 'drover-review-note)
                                          (point-max))))
        ;; After the header, which a blank line ends.
        (goto-char (point-min))
        (search-forward "\n\n" nil 'move))
      (drover-review--insert-note))))

(defun drover-review-remove ()
  "Remove the comment on the line at point."
  (interactive)
  (drover-review--check)
  (let ((ids (drover-review--comment-ids-at)))
    (unless ids
      (user-error "No comment here"))
    (let ((id (if (cdr ids) (drover-review--choose ids "Remove the comment: ") (car ids)))
          (inhibit-read-only t))
      (drover-review-remove-comment drover-review--key id)
      (drover-review--hide-comment id)
      (when-let* ((start (text-property-any (point-min) (point-max) 'drover-review-comment id)))
        (delete-region start (or (next-single-property-change start 'drover-review-comment) (point-max))))
      (drover-review--update-header)
      (message "Comment removed"))))

(defun drover-review-discard ()
  "Discard the comments not sent and the note on the review, once confirmed."
  (interactive)
  (drover-review--check)
  (let ((pending (drover-review-pending drover-review--key)))
    (unless (or pending (drover-review-note drover-review--key))
      (user-error "No comment to discard"))
    (when (yes-or-no-p (format "Discard %d comment%s%s? " (length pending) (if (= (length pending) 1) "" "s")
                               (if (drover-review-note drover-review--key) " and the note" "")))
      (drover-review--put drover-review--key
                          (seq-remove (lambda (comment) (eq (plist-get comment :state) 'pending))
                                      (drover-review-comments drover-review--key))
                          nil)
      (drover-review--draw))))

(defun drover-review-send (&optional now)
  "Put the comments not sent and the note in drover's prompt, for the session.
They make one prompt (see `drover-review-compose'), set to go to the
session, to be read and sent; with NOW, interactively a prefix
argument, it is sent at once, as the prompt sends.  The comments are
then marked sent: they stay, dimmed, for `drover-review-sent-days'."
  (interactive "P")
  (drover-review--check)
  (let* ((key drover-review--key)
         (pending (drover-review-pending key))
         (note (drover-review-note key))
         (session (drover-review--current-session)))
    (unless (or pending note)
      (user-error "No comment to send: c comments on a line"))
    (require 'drover-prompt)
    (let ((text (drover-review-compose (drover-review--scope-phrase drover-review--data) pending note
                                       (mapcar (lambda (file) (plist-get file :file))
                                               (plist-get drover-review--data :files)))))
      (if now
          (unless (drover-prompt-send-to session text)
            (user-error "Not sent"))
        (drover-prompt-edit-for session text))
      (drover-review-mark-sent key (mapcar (lambda (comment) (plist-get comment :id)) pending))
      (dolist (buffer (buffer-list))
        (when (and (eq (buffer-local-value 'major-mode buffer) 'drover-review-mode)
                   (equal (buffer-local-value 'drover-review--key buffer) key))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (dolist (comment pending)
                (drover-review--redraw-comment (plist-get comment :id)))
              (drover-review--draw-note)
              (drover-review--update-header)))))
      (when now
        (drover-message "Sent %d comment%s to %s" (length pending) (if (= (length pending) 1) "" "s")
                        (drover-session-display-title session))))))

(defun drover-review--update-header ()
  "Redraw the header line, which counts the comments not sent."
  (force-mode-line-update))

(defun drover-review--header-line ()
  "Return the header line of a review: its scope and the comments not sent."
  (let ((pending (length (drover-review-pending drover-review--key))))
    (concat " " (alist-get drover-review--scope drover-review-scopes)
            (if (> pending 0)
                (substitute-command-keys
                 (format "   %d comment%s to send: \\[drover-review-send]" pending (if (= pending 1) "" "s")))
              "")
            (cond (drover-review--fetching "   fetching…")
                  (drover-review--drawing "   drawing…")
                  (t "")))))

(defun drover-review--switch (scope)
  "Show SCOPE of the session reviewed."
  (drover-review--check)
  (let ((session (drover-review--current-session)))
    (unless (memq scope (drover-review-scopes-of session))
      (user-error "%s cannot show %s" (drover-session-display-title session)
                  (downcase (alist-get scope drover-review-scopes))))
    (drover-review--load scope)))

(defun drover-review-turn ()
  "Review the changes of the last turn of the session."
  (interactive)
  (drover-review--switch 'turn))

(defun drover-review-whole-session ()
  "Review the changes of every turn of the session."
  (interactive)
  (drover-review--switch 'session))

(defun drover-review-working-tree ()
  "Review the working tree of the session against the commit it started from."
  (interactive)
  (drover-review--switch 'tree))

(defun drover-review-visit ()
  "Visit the file of the line at point, at that line as the file is now.
A line removed is visited where it was.  Only files of this host."
  (interactive)
  (drover-review--check)
  (let* ((info (or (drover-review--line-info)
                   (user-error "No line of a diff here")))
         (directory (plist-get drover-review--data :directory)))
    (unless directory
      (user-error "The file is on %s" (or (drover-session-host drover-review--session) "another host")))
    (let ((line (or (plist-get info :new)
                    (save-excursion
                      (let (found)
                        (while (and (not found) (zerop (forward-line -1)))
                          (when-let* ((before (drover-review--line-info)))
                            (when (plist-get before :new)
                              (setq found (1+ (plist-get before :new))))))
                        found))
                    1)))
      (find-file-other-window (expand-file-name (plist-get info :file) directory))
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun drover-review-magit ()
  "Show the working tree against the commit the session started from, in Magit.
Magit stages and discards there."
  (interactive)
  (drover-review--check)
  (let ((base (plist-get drover-review--data :base))
        (directory (plist-get drover-review--data :directory)))
    (unless (and (eq drover-review--scope 'tree) base directory)
      (user-error "Magit shows the working tree: W first"))
    (unless (require 'magit-diff nil t)
      (user-error "Magit is not installed"))
    (let ((default-directory directory))
      (magit-diff-working-tree (unless (equal base "HEAD") base)))))

(defun drover-review--patch-text (data)
  "Return the files of DATA, a review, as one patch."
  (mapconcat (lambda (file)
               (let ((name (plist-get file :file))
                     (from (or (plist-get file :from) (plist-get file :file))))
                 (concat (format "diff --git a/%s b/%s\n" from name)
                         (pcase (plist-get file :status)
                           ('added "--- /dev/null\n")
                           (_ (format "--- a/%s\n" from)))
                         (pcase (plist-get file :status)
                           ('deleted "+++ /dev/null\n")
                           (_ (format "+++ b/%s\n" name)))
                         (or (plist-get file :patch) ""))))
             (plist-get data :files) ""))

(defun drover-review-patch ()
  "Show the changes of the review as one patch, in `diff-mode'."
  (interactive)
  (drover-review--check)
  (let ((text (drover-review--patch-text drover-review--data))
        (directory (or (plist-get drover-review--data :directory) default-directory))
        (buffer (get-buffer-create (format "*drover patch: %s*"
                                           (drover-bound-line (drover-session-display-title
                                                               drover-review--session)
                                                              60)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (setq default-directory directory)
        (diff-mode)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(provide 'drover-review)
;;; drover-review.el ends here
