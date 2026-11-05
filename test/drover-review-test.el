;;; drover-review-test.el --- Tests of reviewing the changes of sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

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

;; The changes of a session from each source: Git repositories made
;; here, transcripts and rollouts written in throwaway homes, OpenCode's
;; diffs through a stub of opencode.el; the review buffer, its sections
;; and comments, kept and read again; the prompt a review composes.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-review)
(require 'drover-prompt)
(require 'drover-opencode)

;;;; Helpers

(defun drover-review-test-wait (predicate &optional seconds)
  "Run timers and processes until PREDICATE returns non-nil, SECONDS at most.
Return its value."
  (let ((deadline (+ (float-time) (or seconds 10)))
        value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.005))
    value))

(defun drover-review-test-fetch (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and a callback, and return what it got, waiting."
  (let ((result 'pending))
    (apply function (append arguments (list (lambda (value) (setq result value)))))
    (drover-review-test-wait (lambda () (not (eq result 'pending))))
    (should-not (eq result 'pending))
    result))

(defun drover-review-test-git (directory &rest arguments)
  "Run Git with ARGUMENTS in DIRECTORY, and return its output, or fail."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (zerop (apply #'call-process "git" nil t nil arguments))
        (error "git %s: %s" (car arguments) (buffer-string)))
      (buffer-string))))

(defmacro drover-review-test-with-store (&rest body)
  "Run BODY with the comments of reviews kept in a fresh state directory."
  (declare (indent 0) (debug t))
  `(drover-test-with-state
     (let ((drover-review--store nil)
           (drover-review--store-file nil)
           (drover-review--save-timer nil)
           (drover-review--warned nil))
       (unwind-protect
           (progn ,@body)
         (when (timerp drover-review--save-timer)
           (cancel-timer drover-review--save-timer))))))

(defun drover-review-test-patch (lines &optional start)
  "Return a hunk of LINES, strings with their prefix, its first lines at START."
  (let ((old (seq-count (lambda (line) (not (string-prefix-p "+" line))) lines))
        (new (seq-count (lambda (line) (not (string-prefix-p "-" line))) lines))
        (start (or start 1)))
    (concat (format "@@ -%d,%d +%d,%d @@ (defun fake ()\n" start old start new)
            (mapconcat (lambda (line) (concat line "\n")) lines ""))))

(defvar drover-review-test-changes nil
  "What the fake engine's :changes tells, by scope, an alist.")

(defvar drover-review-test-scopes nil
  "The scopes the fake engine's :changes was asked for, newest first.")

(defmacro drover-review-test-with-review (changes &rest body)
  "Run BODY with a fake session whose engine tells CHANGES, an alist by scope.
The session is `session' in BODY; its review's buffer is killed after."
  (declare (indent 1) (debug t))
  `(drover-review-test-with-store
     (drover-test-with-engines
       (let* ((drover-review-test-changes ,changes)
              (drover-review-test-scopes nil)
              (session (drover-test-session "s1" :engine 'fake :engine-id "conv-1"
                                            :directory temporary-file-directory)))
         (drover-test-register-engine
          'fake :changes (lambda (_id scope _directory callback)
                           (push scope drover-review-test-scopes)
                           (funcall callback (alist-get scope drover-review-test-changes))))
         (drover-test-with-backend (list session)
           (unwind-protect
               (save-window-excursion ,@body)
             (when-let* ((buffer (drover-review--buffer session)))
               (kill-buffer buffer))))))))

(defun drover-review-test-drawn ()
  "Wait for the review of the current buffer to be fetched and drawn whole."
  (should (drover-review-test-wait (lambda () (and drover-review--data (not drover-review--fetching)
                                                   (not drover-review--drawing))))))

(defun drover-review-test-goto (text)
  "Move point to the start of the line of the diff reading TEXT."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (search-forward text nil t))
      (when (get-text-property (line-beginning-position) 'drover-review-line)
        (setq found t)))
    (should found)
    (beginning-of-line)))

(defun drover-review-test-shown ()
  "Return the texts the comments shown in the current buffer show."
  (sort (delq nil (mapcar (lambda (overlay) (overlay-get overlay 'after-string))
                          (overlays-in (point-min) (point-max))))
        #'string<))

;;;; Patches

(ert-deftest drover-review-test-parse-git ()
  "A Git diff is its files, their status, names and counts, and their hunks."
  (let ((files (drover-review-parse-git
                (concat "diff --git a/src/a.el b/src/a.el\n"
                        "index 1111111..2222222 100644\n"
                        "--- a/src/a.el\n+++ b/src/a.el\n"
                        "@@ -1,3 +1,4 @@\n line\n-old\n+new\n+more\n line\n"
                        "diff --git a/new.txt b/new.txt\nnew file mode 100644\n"
                        "--- /dev/null\n+++ b/new.txt\n@@ -0,0 +1 @@\n+hello\n"
                        "diff --git a/gone.txt b/gone.txt\ndeleted file mode 100644\n"
                        "--- a/gone.txt\n+++ /dev/null\n@@ -1 +0,0 @@\n-bye\n"
                        "diff --git a/old name.el b/new name.el\nsimilarity index 90%\n"
                        "rename from old name.el\nrename to new name.el\n"
                        "diff --git \"a/t\\303\\251\\tst.el\" \"b/t\\303\\251\\tst.el\"\n"
                        "--- \"a/t\\303\\251\\tst.el\"\n+++ \"b/t\\303\\251\\tst.el\"\n@@ -1 +1 @@\n-a\n+b\n"
                        "diff --git a/image.png b/image.png\nindex 1..2 100644\n"
                        "Binary files a/image.png and b/image.png differ\n"))))
    (should (equal (mapcar (lambda (file) (plist-get file :file)) files)
                   '("src/a.el" "new.txt" "gone.txt" "new name.el" "té\tst.el" "image.png")))
    (should (equal (mapcar (lambda (file) (plist-get file :status)) files)
                   '(modified added deleted renamed modified modified)))
    (should (equal (plist-get (nth 3 files) :from) "old name.el"))
    (should (equal (mapcar (lambda (file) (list (plist-get file :additions) (plist-get file :deletions))) files)
                   '((2 1) (1 0) (0 1) (0 0) (1 1) (0 0))))
    (should (equal (plist-get (car files) :patch) "@@ -1,3 +1,4 @@\n line\n-old\n+new\n+more\n line\n"))
    (should (plist-get (nth 5 files) :binary))
    (should-not (plist-get (nth 3 files) :patch))))

(ert-deftest drover-review-test-read-lines ()
  "Lines are read whole across reads, decoded once whole, those of a key only."
  (let* ((file (make-temp-file "drover-lines" nil ".jsonl"))
         (drover-transcript-read-chunk 7)
         (seen nil) (done 'pending))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert "{\"key\":\"été — first\"}\n{\"other\":1}\nnot json key\n{\"key\":\"日本語 last\"}"))
          (drover-transcript-read-lines (list (cons file 0)) '("\"key\"")
                                        (lambda (_file record) (push (alist-get 'key record) seen))
                                        (lambda (failure) (setq done failure)))
          (drover-review-test-wait (lambda () (not (eq done 'pending))))
          (should-not done)
          (should (equal (nreverse seen) '("été — first" "日本語 last")))
          ;; From the middle of the first line: that line is cut, skipped.
          (setq seen nil done 'pending)
          (drover-transcript-read-lines (list (cons file 3)) '("\"key\"")
                                        (lambda (_file record) (push (alist-get 'key record) seen))
                                        (lambda (failure) (setq done failure)))
          (drover-review-test-wait (lambda () (not (eq done 'pending))))
          (should (equal seen '("日本語 last")))
          ;; A file missing is told, the others read.
          (setq seen nil done 'pending)
          (drover-transcript-read-lines (list (cons (concat file ".gone") 0) (cons file 0)) '("\"key\"")
                                        (lambda (_file record) (push (alist-get 'key record) seen))
                                        (lambda (failure) (setq done failure)))
          (drover-review-test-wait (lambda () (not (eq done 'pending))))
          (should (string-match-p "No such file" done))
          (should (= (length seen) 2)))
      (delete-file file))))

;;;; Git

(ert-deftest drover-review-test-git ()
  "The working tree is compared with where HEAD was as the session started."
  (let* ((directory (file-name-as-directory (make-temp-file "drover-review-git" t)))
         (process-environment (append '("GIT_AUTHOR_NAME=T" "GIT_AUTHOR_EMAIL=t@t"
                                        "GIT_COMMITTER_NAME=T" "GIT_COMMITTER_EMAIL=t@t"
                                        "GIT_CONFIG_NOSYSTEM=1" "HOME=/nonexistent")
                                      process-environment)))
    (unwind-protect
        (cl-flet ((commit (time message)
                    (let ((process-environment (cons (format "GIT_COMMITTER_DATE=%d +0000" time)
                                                     process-environment)))
                      (drover-review-test-git directory "add" "-A")
                      (drover-review-test-git directory "commit" "-q" "-m" message))))
          (drover-review-test-git directory "init" "-q" "-b" "main")
          (drover-test-write (expand-file-name "a.el" directory) "one\ntwo\nthree\n")
          (commit 1700000000 "First")
          (drover-test-write (expand-file-name "a.el" directory) "one\n2\nthree\n")
          (commit 1700000200 "Second")
          (drover-test-write (expand-file-name "a.el" directory) "one\n2\nthree\nfour\n")
          (drover-test-write (expand-file-name "new dir/b.txt" directory) "fresh\n")
          (drover-test-write (expand-file-name "blob.bin" directory) "a\0b")
          (let* ((first (string-trim (drover-review-test-git directory "rev-parse" "HEAD~1")))
                 (review (drover-review-test-fetch #'drover-review--git directory 1700000100.0)))
            (should-not (plist-get review :failure))
            (should (equal (plist-get review :base) first))
            (should (string-match-p "as the session started" (plist-get review :how)))
            (should (equal (mapcar #'cdr (plist-get review :commits)) '("Second")))
            (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get review :files))
                           '("a.el" "blob.bin" "new dir/b.txt")))
            (let ((a (car (plist-get review :files))))
              (should (equal (list (plist-get a :additions) (plist-get a :deletions)) '(2 1)))
              (should (string-match-p "^-two\n\\+2\n" (plist-get a :patch))))
            (should (plist-get (nth 1 (plist-get review :files)) :binary))
            (should (equal (plist-get (nth 2 (plist-get review :files)) :patch) "@@ -0,0 +1,1 @@\n+fresh\n"))
            (should (equal (file-truename (plist-get review :directory)) (file-truename directory))))
          ;; Without a time, nor a remote: what is not committed, said so.
          (let ((review (drover-review-test-fetch #'drover-review--git directory nil)))
            (should (equal (plist-get review :base) "HEAD"))
            (should (seq-some (lambda (note) (string-match-p "not committed" note)) (plist-get review :notes)))
            (should-not (plist-get review :commits))
            (should (equal (plist-get (car (plist-get review :files)) :additions) 1)))
          ;; Outside Git, a failure.
          (let ((outside (file-name-as-directory (make-temp-file "drover-review-none" t))))
            (unwind-protect
                (should (string-match-p "Not in a Git repository"
                                        (plist-get (drover-review-test-fetch #'drover-review--git outside nil)
                                                   :failure)))
              (delete-directory outside t))))
      (delete-directory directory t))))

;;;; Transcripts and rollouts

(defun drover-review-test-claude-edit (id directory time file patch)
  "Return the record of an edit of FILE by conversation ID at TIME, of PATCH hunks.
PATCH is a list of (OLD-START OLD-LINES NEW-START NEW-LINES LINES)."
  (drover-fixture-line
   :parentUuid :null :isSidechain :false :type "user"
   :message (list :role "user" :content (vector (list :tool_use_id "toolu_1" :type "tool_result" :content "ok")))
   :uuid (format "%s-e%d" id (truncate time)) :timestamp (drover-fixture-time time)
   :toolUseResult (list :filePath file :oldString "x" :newString "y"
                        :structuredPatch (vconcat (mapcar (lambda (hunk)
                                                            (list :oldStart (nth 0 hunk) :oldLines (nth 1 hunk)
                                                                  :newStart (nth 2 hunk) :newLines (nth 3 hunk)
                                                                  :lines (vconcat (nth 4 hunk))))
                                                          patch)))
   :cwd directory :sessionId id))

(defun drover-review-test-claude-create (id directory time file content &optional sidechain)
  "Return the record of FILE written whole with CONTENT by conversation ID at TIME.
SIDECHAIN marks a subagent's."
  (drover-fixture-line
   :parentUuid :null :isSidechain (if sidechain t :false) :type "user"
   :message (list :role "user" :content (vector (list :tool_use_id "toolu_2" :type "tool_result" :content "ok")))
   :uuid (format "%s-c%d" id (truncate time)) :timestamp (drover-fixture-time time)
   :toolUseResult (list :type "create" :filePath file :content content :structuredPatch [])
   :cwd directory :sessionId id))

(ert-deftest drover-review-test-claude-code ()
  "Claude Code's changes are its tools' edits and writes, its subagents' too."
  (drover-test-with-homes
    (let* ((id "11111111-2222-4333-8444-555555555555")
           (directory (file-name-as-directory (expand-file-name "work" home)))
           (file (drover-fixture-claude-file id directory))
           (agent (expand-file-name "subagents/agent-a1.jsonl" (file-name-sans-extension file))))
      (make-directory directory t)
      (drover-fixture-write
       file
       (list (drover-fixture-claude-user id directory 1790000000.0 "First task")
             (drover-review-test-claude-edit id directory 1790000010.0 (expand-file-name "src/a.el" directory)
                                             '((10 2 10 3 (" keep" "-old" "+new" "+more"))))
             (drover-fixture-claude-user id directory 1790000100.0 "Second task")
             (drover-review-test-claude-create id directory 1790000110.0
                                               (expand-file-name "b.txt" directory) "one\ntwo\n")
             (drover-review-test-claude-edit id directory 1790000120.0 (expand-file-name "src/a.el" directory)
                                             '((1 1 1 1 ("-a" "+b"))))))
      (drover-fixture-write
       agent
       (list (drover-review-test-claude-create id directory 1790000050.0
                                               (expand-file-name "early.txt" directory) "x\n" t)
             (drover-review-test-claude-create id directory 1790000130.0
                                               (expand-file-name "late.txt" directory) "y\n" t)))
      (let ((turn (drover-review-test-fetch #'drover-claude-code-changes id 'turn directory))
            (whole (drover-review-test-fetch #'drover-claude-code-changes id 'session directory)))
        (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get turn :files))
                       '("b.txt" "src/a.el" "late.txt")))
        (should (equal (mapcar (lambda (file) (plist-get file :status)) (plist-get turn :files))
                       '(added modified added)))
        (should (equal (plist-get (nth 1 (plist-get turn :files)) :patch) "@@ -1,1 +1,1 @@\n-a\n+b\n"))
        (should (equal (plist-get (car (plist-get turn :files)) :patch) "@@ -0,0 +1,2 @@\n+one\n+two\n"))
        (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get whole :files))
                       '("src/a.el" "early.txt" "b.txt" "late.txt")))
        ;; Two edits of a file, their hunks one after the other.
        (let ((a (car (plist-get whole :files))))
          (should (equal (plist-get a :patch)
                         "@@ -10,2 +10,3 @@\n keep\n-old\n+new\n+more\n@@ -1,1 +1,1 @@\n-a\n+b\n"))
          (should (equal (list (plist-get a :additions) (plist-get a :deletions)) '(3 2)))))
      ;; The newest prompt far back: the whole transcript is read.
      (let ((drover-claude-code--turn-tail 200))
        (should (equal (mapcar (lambda (file) (plist-get file :file))
                               (plist-get (drover-review-test-fetch #'drover-claude-code-changes id 'turn directory)
                                          :files))
                       '("b.txt" "src/a.el" "late.txt"))))
      (should (plist-get (drover-review-test-fetch #'drover-claude-code-changes
                                                   "99999999-2222-4333-8444-555555555555" 'turn directory)
                         :failure)))))

(ert-deftest drover-review-test-codex ()
  "Codex's changes are the patches it applied, as recorded, else as asked."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (file (drover-fixture-codex "c1" directory)))
      (drover-fixture-write
       file
       (list (drover-fixture-codex-event 1790000010.0 "user_message" :message "Now the import" :images [])
             (drover-fixture-line :timestamp (drover-fixture-time 1790000011.0) :type "response_item"
                                  :payload (list :type "custom_tool_call" :status "completed" :call_id "call_2"
                                                 :name "apply_patch"
                                                 :input "*** Begin Patch\n*** Update File: src/import.rs\n@@ fn main\n-a\n+b\n*** End Patch\n"))
             (drover-fixture-codex-event 1790000012.0 "patch_apply_end" :call_id "call_2" :success t
                                         :status "completed"
                                         :changes (list (intern (expand-file-name "src/import.rs" directory))
                                                        (list :type "update" :move_path :null
                                                              :unified_diff "@@ -4,1 +4,1 @@\n-a\n+b\n")))
             (drover-fixture-line :timestamp (drover-fixture-time 1790000013.0) :type "response_item"
                                  :payload (list :type "custom_tool_call" :status "completed" :call_id "call_3"
                                                 :name "apply_patch"
                                                 :input "*** Begin Patch\n*** Update File: old.rs\n*** Move to: new.rs\n@@\n-x\n+y\n*** Delete File: gone.rs\n*** End Patch\n"))
             (drover-fixture-codex-event 1790000014.0 "patch_apply_end" :call_id "call_4" :success :false
                                         :changes (list (intern "failed.rs") (list :type "add" :content "z\n"))))
       t)
      (let ((turn (drover-review-test-fetch #'drover-codex-changes "c1" 'turn directory))
            (whole (drover-review-test-fetch #'drover-codex-changes "c1" 'session directory)))
        (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get turn :files))
                       '("src/import.rs" "new.rs" "gone.rs")))
        ;; What Codex applied wins over the call, numbered.
        (should (equal (plist-get (car (plist-get turn :files)) :patch) "@@ -4,1 +4,1 @@\n-a\n+b\n"))
        (let ((moved (nth 1 (plist-get turn :files))))
          (should (eq (plist-get moved :status) 'renamed))
          (should (equal (plist-get moved :from) "old.rs"))
          (should (equal (plist-get moved :patch) "@@\n-x\n+y\n")))
        (should (eq (plist-get (nth 2 (plist-get turn :files)) :status) 'deleted))
        ;; The first turn's patch, of the fixture, as Codex was asked.
        (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get whole :files))
                       '("src/export.rs" "src/csv.rs" "src/import.rs" "new.rs" "gone.rs")))
        (should (equal (plist-get (car (plist-get whole :files)) :patch) "@@\n-old\n+new\n+more\n"))
        (should (equal (plist-get (nth 1 (plist-get whole :files)) :patch) "@@ -0,0 +1,1 @@\n+one\n"))))))

(ert-deftest drover-review-test-opencode ()
  "OpenCode's changes are its service's diffs, a folder listed in part told."
  (let ((asked nil))
    (cl-letf (((symbol-function 'opencode-session-diff)
               (lambda (id callback &optional whole _context)
                 (push (list id whole) asked)
                 (funcall callback
                          (if (equal id "ses_bad")
                              '(:failure "Turn range spans a location change")
                            '(((file . "a.el") (patch . "@@ -1 +1 @@\n-a\n+b\n") (additions . 1) (deletions . 1)
                               (status . "modified"))
                              ((file . "big.json") (patch . "") (additions . 9000) (deletions . 0)
                               (status . "added"))
                              ((file . "node_modules/") (patch . "") (additions . 0) (deletions . 0)
                               (status . "added") (more . t))))))))
      (let ((review (drover-review-test-fetch #'drover-opencode--changes "ses_1" 'session "/tmp/")))
        (should (equal asked '(("ses_1" t))))
        (should (equal (mapcar (lambda (file) (plist-get file :file)) (plist-get review :files))
                       '("a.el" "big.json")))
        (should (eq (plist-get (nth 1 (plist-get review :files)) :status) 'added))
        (should (equal (plist-get review :notes) '("The service stopped listing node_modules/ early"))))
      (should (equal (drover-review-test-fetch #'drover-opencode--changes "ses_bad" 'turn nil)
                     '(:failure "Turn range spans a location change"))))))

;;;; The review buffer

(defconst drover-review-test-two-files
  `((turn . (:files ((:file "src/a.el" :status modified :additions 2 :deletions 1
                      :patch ,(concat (drover-review-test-patch '(" one" "-two" "+2" "+2b" " three") 10)
                                      (drover-review-test-patch '(" x" "+y" " z") 40)))
                     (:file "README" :status added :additions 1 :deletions 0
                      :patch "@@ -0,0 +1,1 @@\n+Hello\n"))))
    (session . (:files ((:file "src/a.el" :status modified :additions 1 :deletions 0
                         :patch ,(drover-review-test-patch '(" x" "+y" " z") 40))))))
  "Changes of a fake session, by scope.")

(ert-deftest drover-review-test-buffer ()
  "A review is a section per file and per hunk, its lines numbered."
  (drover-review-test-with-review drover-review-test-two-files
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (should (derived-mode-p 'drover-review-mode))
      (should (eq drover-review--scope 'turn))
      (let ((files (oref magit-root-section children)))
        (should (equal (mapcar (lambda (section) (oref section value))
                               (seq-filter (lambda (section) (cl-typep section 'drover-review-file-section)) files))
                       '("src/a.el" "README")))
        (should (equal (mapcar (lambda (section) (length (oref section children)))
                               (seq-filter (lambda (section) (cl-typep section 'drover-review-file-section)) files))
                       '(2 1))))
      (should (string-match-p "Last turn: 2 files, \\+3 −1" (buffer-string)))
      (drover-review-test-goto "+2b")
      (should (equal (drover-review--line-info) (list :kind ?+ :old nil :new 12 :text "+2b"
                                                      :hunk "@@ -10,3 +10,4 @@ (defun fake ()"
                                                      :file "src/a.el")))
      (drover-review-test-goto "-two")
      (should (equal (plist-get (drover-review--line-info) :old) 11))
      ;; n moves to the next hunk; TAB folds it.
      (drover-review-test-goto " three")
      (magit-section-forward)
      (should (equal (oref (magit-current-section) value) "@@ -40,2 +40,3 @@ (defun fake ()"))
      (magit-section-toggle (magit-current-section))
      (should (oref (magit-current-section) hidden))
      ;; Another scope, from its key.
      (drover-review-whole-session)
      (drover-review-test-drawn)
      (should (eq (car drover-review-test-scopes) 'session))
      (should (string-match-p "Whole session: 1 file, \\+1 −0" (buffer-string)))
      ;; The working tree of a directory outside Git fails on its line.
      (let ((default-directory temporary-file-directory))
        (drover-review-working-tree)
        (drover-review-test-drawn)
        (should (string-match-p "Working tree: " (buffer-string)))))))

(ert-deftest drover-review-test-failure-and-no-scope ()
  "A failure shows on the scope's line; a session without changes to show refuses."
  (drover-review-test-with-review '((turn . (:failure "The service is down")))
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (should (string-match-p "Last turn: The service is down" (buffer-string)))))
  (drover-test-with-engines
    (drover-test-with-backend (list (drover-test-session "s2"))
      (should-not (drover-reviewable-p (drover-test-session "s2")))
      (should-error (drover-review (drover-test-session "s2")) :type 'user-error))))

;;;; Comments

(ert-deftest drover-review-test-comments ()
  "Comments show under their lines, are edited, removed and kept."
  (drover-review-test-with-review drover-review-test-two-files
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (let ((key drover-review--key))
        (should (equal key '(fake . "conv-1")))
        (drover-review-test-goto "+2b")
        (drover-review-comment "Name it better")
        ;; A region of lines, one comment.
        (drover-review-test-goto "-two")
        (set-mark (point))
        (forward-line 2)
        (end-of-line)
        (activate-mark)
        (drover-review-comment "These two\nlines")
        (should (equal (mapcar (lambda (comment) (list (plist-get comment :side) (plist-get comment :start)
                                                       (plist-get comment :end) (plist-get comment :lines)))
                               (drover-review-comments key))
                       '((new 12 12 ("+2b"))
                         (new 11 12 ("-two" "+2" "+2b")))))
        (should (seq-some (lambda (text) (string-match-p "Name it better" text)) (drover-review-test-shown)))
        (should (seq-some (lambda (text) (string-match-p "▌ These two\n▌ lines" text))
                          (drover-review-test-shown)))
        ;; Edited where it shows; two on a line: the one asked.
        (drover-review-test-goto "+y")
        (drover-review-comment "On y")
        (drover-review-comment "On y, better")
        (should (equal (plist-get (car (last (drover-review-comments key))) :text) "On y, better"))
        (should (seq-some (lambda (text) (string-match-p "On y, better" text)) (drover-review-test-shown)))
        (should-not (seq-some (lambda (text) (string-match-p "On y$" text)) (drover-review-test-shown)))
        ;; Removed.
        (drover-review-remove)
        (should (= (length (drover-review-comments key)) 2))
        (should-not (seq-some (lambda (text) (string-match-p "On y" text)) (drover-review-test-shown)))
        ;; Not on a line of a diff.
        (goto-char (point-min))
        (should-error (drover-review-comment "Nowhere") :type 'user-error)
        ;; Kept, and read again.
        (drover-review-set-note key "Run the tests too.")
        (drover-review-save)
        (should (file-exists-p (drover-review--file)))
        (let ((kept (drover-review-comments key)))
          (setq drover-review--store nil)
          (should (equal (drover-review-comments key) kept))
          (should (equal (drover-review-note key) "Run the tests too.")))
        ;; Drawn anew, they show where they were; on a scope without
        ;; their lines, at the end.
        (drover-review--draw)
        (drover-review-test-drawn)
        (should (= (length (drover-review-test-shown)) 2))
        (should (string-match-p "Note on the whole review: Run the tests too." (buffer-string)))
        (drover-review-whole-session)
        (drover-review-test-drawn)
        (should (string-match-p "Comments on lines this review does not show (2)" (buffer-string)))
        (goto-char (point-min))
        (search-forward "Name it better")
        (drover-review-remove)
        (should (= (length (drover-review-comments key)) 1))))))

(ert-deftest drover-review-test-comment-moved ()
  "A comment whose lines moved shows where they read the same."
  (drover-review-test-with-review
      `((turn . (:files ((:file "a.el" :status modified :additions 2 :deletions 0
                          :patch ,(drover-review-test-patch '(" a" "+b" "+c" " d") 5)))))
        (session . (:files ((:file "a.el" :status modified :additions 2 :deletions 0
                             :patch ,(drover-review-test-patch '(" a" "+b" "+c" " d") 50))))))
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (drover-review-test-goto "+b")
      (set-mark (point))
      (forward-line 1)
      (end-of-line)
      (activate-mark)
      (drover-review-comment "Both")
      (drover-review-whole-session)
      (drover-review-test-drawn)
      (should (seq-some (lambda (text) (string-match-p "Both" text)) (drover-review-test-shown)))
      (should-not (string-match-p "does not show" (buffer-string))))))

(ert-deftest drover-review-test-compose ()
  "The prompt a review sends: its comments by file, quoted, then its note."
  (let ((comments (list (list :id 1 :file "src/a.el" :side 'new :start 12 :end 12 :lines '("+2b")
                              :hunk "@@ -10,4 +10,5 @@ (defun fake ()" :text "Name it better")
                        (list :id 2 :file "README" :side 'old :start 3 :end 4 :lines '("-x" "-* y")
                              :hunk "@@ -3,2 +2,0 @@" :text "Why remove these?\nThey matter.")
                        (list :id 3 :file "src/a.el" :side 'new :start 2 :end 3 :lines '(" a" "+b")
                              :hunk "@@ -1,2 +1,3 @@" :text "Fine")
                        (list :id 4 :file "lib.rs" :side 'new :start nil :end nil :lines '("+z")
                              :hunk "@@ fn main" :text "Check"))))
    (should (equal (drover-review-compose "in the last turn" comments "Run the tests too." '("src/a.el" "README"))
                   "Review of your changes in the last turn: 4 comments on 3 files.

`src/a.el`:
- Lines 2-3:
  #+begin_src diff
  @@ -1,2 +1,3 @@
   a
  +b
  #+end_src
  Fine
- Line 12:
  #+begin_src diff
  @@ -10,4 +10,5 @@ (defun fake ()
  +2b
  #+end_src
  Name it better

`README`:
- Removed lines 3-4 (of the file before the change):
  #+begin_src diff
  @@ -3,2 +2,0 @@
  -x
  -* y
  #+end_src
  Why remove these?
  They matter.

`lib.rs`:
- At these lines:
  #+begin_src diff
  @@ fn main
  +z
  #+end_src
  Check

Overall: Run the tests too.
"))
    ;; As sent, in Markdown: fences, the lines as they read.
    (should (string-match-p (regexp-quote "  ```diff\n  @@ -1,2 +1,3 @@\n   a\n  +b\n  ```\n")
                            (drover-prompt-to-markdown
                             (drover-review-compose "in the last turn" comments nil))))
    (should (string-match-p (regexp-quote "  -* y\n")
                            (drover-prompt-to-markdown
                             (drover-review-compose "in the last turn" comments nil))))
    (should (equal (drover-review-compose "in this session" nil "Looks good.")
                   "Review of your changes in this session.\n\nOverall: Looks good.\n"))))

(ert-deftest drover-review-test-send ()
  "Sending puts the comments in the prompt, for the session, or sends them; then they are sent."
  (drover-review-test-with-review drover-review-test-two-files
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (should-error (drover-review-send) :type 'user-error)
      (drover-review-test-goto "+2b")
      (drover-review-comment "Name it better")
      (let ((edited nil) (sent nil))
        (cl-letf (((symbol-function 'drover-prompt-edit-for)
                   (lambda (target text) (setq edited (list (drover-session-id target) text))))
                  ((symbol-function 'drover-prompt-send-to)
                   (lambda (target text) (setq sent (list (drover-session-id target) text)) t)))
          (drover-review-send)
          (should (equal (car edited) "s1"))
          (should (string-prefix-p "Review of your changes in the last turn: 1 comment on 1 file." (cadr edited)))
          (should-not (drover-review-pending drover-review--key))
          (should (eq (plist-get (car (drover-review-comments drover-review--key)) :state) 'sent))
          (should (seq-some (lambda (text) (string-match-p "Sent .*\n▌ Name it better" text))
                            (drover-review-test-shown)))
          ;; Nothing more to send; a new comment, sent at once.
          (should-error (drover-review-send) :type 'user-error)
          (drover-review-test-goto "+y")
          (drover-review-comment "And this")
          (drover-review-send t)
          (should (equal (car sent) "s1"))
          (should (string-match-p "And this" (cadr sent)))
          (should-not (string-match-p "Name it better" (cadr sent)))
          ;; Editing a comment sent makes it pending again.
          (drover-review-test-goto "+2b")
          (drover-review-comment "Name it much better")
          (should (equal (mapcar (lambda (comment) (plist-get comment :text))
                                 (drover-review-pending drover-review--key))
                         '("Name it much better"))))))))

(ert-deftest drover-review-test-send-through-prompt ()
  "The review goes in drover's prompt, set to go to the session reviewed."
  (drover-review-test-with-review drover-review-test-two-files
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (drover-review-test-goto "+2b")
      (drover-review-comment "Name it better")
      (unwind-protect
          (progn
            (drover-review-send)
            (with-current-buffer (drover-prompt-buffer)
              (should (string-match-p "Name it better" (buffer-string)))
              (should (equal (plist-get drover-prompt--chosen-session :key) '(fake . "s1")))))
        (when-let* ((buffer (drover-prompt-buffer)))
          (kill-buffer buffer))))))

(ert-deftest drover-review-test-sent-pruned ()
  "Comments sent long ago go as they are kept, and so do sessions past the limit."
  (drover-review-test-with-store
    (let ((old (- (float-time) (* 86400 (1+ drover-review-sent-days)))))
      (drover-review-add-comment '(fake . "a") (list :file "x" :text "old" :state 'sent :sent old))
      (drover-review-add-comment '(fake . "a") (list :file "x" :text "new"))
      (let ((drover-review-session-limit 1))
        (drover-review-add-comment '(fake . "b") (list :file "y" :text "other"))
        (drover-review-save))
      (setq drover-review--store nil)
      (should-not (drover-review-comments '(fake . "a")))
      (should (equal (mapcar (lambda (comment) (plist-get comment :text)) (drover-review-comments '(fake . "b")))
                     '("other"))))))

(ert-deftest drover-review-test-unreadable-store ()
  "A file of comments that does not read is told once, and not written over silently."
  (drover-review-test-with-store
    (drover-test-write (drover-review--file) "(:version 1 :reviews (((fake . \"a\") :comments (oops")
    (let ((warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (should-not (drover-review-comments '(fake . "a")))
        (should-not (drover-review-comments '(fake . "b")))
        (should (= (length warnings) 1))
        (should (string-match-p "Ignoring the comments" (car warnings)))))))

;;;; Large diffs

(ert-deftest drover-review-test-large-diff-slices ()
  "A diff of thousands of lines draws in slices, each short, the first at once."
  (let* ((lines (cl-loop for index from 1 to 5000 collect (format "+line %d of a long file" index)))
         (changes `((turn . (:files ((:file "big.el" :status added :additions 5000 :deletions 0
                                     :patch ,(drover-review-test-patch lines))
                                    (:file "small.el" :status modified :additions 1 :deletions 0
                                     :patch ,(drover-review-test-patch '("+tail")))))))))
    (drover-review-test-with-review changes
      (let ((slices nil)
            (drover-review-slice 0.002)
            (draw (symbol-function 'drover-review--draw-some)))
        (cl-letf (((symbol-function 'drover-review--draw-some)
                   (lambda ()
                     (let ((start (float-time)))
                       (funcall draw)
                       (push (- (float-time) start) slices)))))
          (with-current-buffer (drover-review session)
            (should drover-review--drawing)
            (should (string-match-p "big.el" (buffer-string)))
            (drover-review-test-drawn)
            (should (> (length slices) 1))
            ;; Generous for a loaded machine: a slice aims at 2 ms here.
            (should (< (apply #'max slices) 0.25))
            (should (= (count-lines (point-min) (point-max)) (+ 5000 3 2 3)))
            (drover-review-test-goto "+line 5000 of")
            (should (equal (plist-get (drover-review--line-info) :new) 5000))
            (should (equal (plist-get (drover-review--line-info) :file) "big.el"))
            (drover-review-test-goto "+tail")
            (should (equal (plist-get (drover-review--line-info) :file) "small.el"))))))))

(ert-deftest drover-review-test-bounded ()
  "A review is bounded: patches past the limit show counts, names are lines."
  (let ((drover-review-limit 100))
    (let ((review (drover-review--bound
                   (list :files (list (list :file "a" :status 'modified :patch (make-string 60 ?x))
                                      (list :file "b" :status 'weird :patch (make-string 60 ?y) :additions 3)
                                      (list :file (concat "c\n" (make-string 2000 ?z)) :status 'added))
                         :notes (list (make-string 500000 ?n))))))
      (should (plist-get (car (plist-get review :files)) :patch))
      (should-not (plist-get (nth 1 (plist-get review :files)) :patch))
      (should (eq (plist-get (nth 1 (plist-get review :files)) :status) 'modified))
      (should (equal (plist-get (nth 2 (plist-get review :files)) :file) "c"))
      (should (seq-some (lambda (note) (string-match-p "cut past" note)) (plist-get review :notes)))
      (should (seq-every-p (lambda (note) (<= (length note) (1+ drover-line-limit))) (plist-get review :notes))))))

(ert-deftest drover-review-test-long-lines ()
  "A line of half a megabyte shows cut, and a comment on it keeps it cut."
  (drover-review-test-with-review
      `((turn . (:files ((:file "min.js" :status modified :additions 1 :deletions 0
                          :patch ,(concat "@@ -1,0 +1,1 @@\n+" (make-string 500000 ?q) "\n"))))))
    (with-current-buffer (drover-review session)
      (drover-review-test-drawn)
      (drover-review-test-goto "+qqq")
      (should (<= (- (line-end-position) (line-beginning-position)) (+ 2 drover-review-line-limit)))
      (let ((comment (drover-review-comment "Minified")))
        (ignore comment))
      (should (<= (length (car (plist-get (car (drover-review-comments drover-review--key)) :lines)))
                  (+ 2 drover-review-line-limit))))))

(ert-deftest drover-review-test-prints-in-proportion ()
  "What a review keeps prints in proportion to its diff and comments."
  (let* ((lines (cl-loop for index from 1 to 2000 collect (format "+line %d" index)))
         (patch (drover-review-test-patch lines))
         (changes `((turn . (:files ((:file "a.el" :status added :additions 2000 :deletions 0 :patch ,patch)))))))
    (drover-review-test-with-review changes
      (with-current-buffer (drover-review session)
        (drover-review-test-drawn)
        (drover-review-test-goto "+line 7")
        (drover-review-comment "One")
        (pcase-dolist (`(,name . ,object) (drover-test-kept))
          (ert-info (name)
            (should (drover-test-printed-length object (+ 65536 (* 4 (length patch)))))))))))

;;;; Keys

(ert-deftest drover-review-test-keys ()
  "R reviews from lists and the menu; the review's keys are its own."
  (should (eq (keymap-lookup drover-session-map "R") #'drover-review))
  (dolist (binding '(("c" . drover-review-comment) ("k" . drover-review-remove)
                     ("C" . drover-review-edit-note) ("C-c C-c" . drover-review-send)
                     ("C-c C-k" . drover-review-discard) ("T" . drover-review-turn)
                     ("S" . drover-review-whole-session) ("W" . drover-review-working-tree)
                     ("RET" . drover-review-visit) ("n" . magit-section-forward)))
    (should (eq (keymap-lookup drover-review-mode-map (car binding)) (cdr binding)))))

(provide 'drover-review-test)
;;; drover-review-test.el ends here
