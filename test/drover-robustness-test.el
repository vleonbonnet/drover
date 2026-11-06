;;; drover-robustness-test.el --- Errors out of commands, text from outside, reloading  -*- lexical-binding: t; -*-

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

;; What made drover hang a running Emacs once: a timer's error carrying
;; the index of sessions, printed whole in the echo area, which every
;; redisplay measured again; and an index made by the version of drover
;; loaded before, read by the accessors of the one loaded after.  The
;; tests check that errors out of commands are reported on one short
;; line, at most once a minute, drover going on; that text from outside,
;; titles and prompts of hundreds of kilobytes, is bounded before
;; anything measures or prints it; and that structures made by another
;; version of drover are made anew rather than misread, down to loading
;; the version before this one and this one after it in one Emacs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-attention)
(require 'drover-list-test)
(require 'drover-transcript-fixtures)
(require 'drover-claude-code-transcripts-test)
(require 'drover-codex-rollouts-test)

(defconst drover-robustness-test-limit 400
  "Characters at most of a message drover shows.
An error's report is `drover-error-length' characters after what it
comes from, and where it was.")

(defmacro drover-robustness-test-messages (&rest body)
  "Run BODY, and return the messages it showed, oldest first.
Errors out of commands are reported, not debugged."
  (declare (indent 0) (debug t))
  `(let ((messages nil)
         (debug-on-error nil)
         (drover--errors-reported (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest arguments)
                  (when format
                    (push (apply #'format-message format arguments) messages))
                  nil)))
       ,@body)
     (nreverse messages)))

(defun drover-robustness-test-bounded-p (messages)
  "Return non-nil when every one of MESSAGES is one short line."
  (seq-every-p (lambda (message)
                 (and (<= (length message) drover-robustness-test-limit)
                      (not (string-search "\n" message))))
               messages))

(defun drover-robustness-test-index (count)
  "Return an index holding COUNT records, as printed of megabytes."
  (let ((index (drover--index-make)))
    (dotimes (n count)
      (let ((id (format "s%d" n)))
        (puthash (cons 'fake id) (drover-test-session id :title (make-string 200 ?t))
                 (drover--index-records index))))
    index))

(defun drover-robustness-test-old-index (count)
  "Return an index as drover d61bfdf made it, holding COUNT records.
It has no stamp, nor the slot of conversations to look at again that
came after: the accessors of today read past its end."
  (let ((records (make-hash-table :test #'equal)))
    (dotimes (n count)
      (let ((id (format "s%d" n)))
        (puthash (cons 'fake id) (drover-test-session id) records)))
    (record 'drover--index records (make-hash-table :test #'equal) count
            (make-hash-table :test #'equal) 0 (make-hash-table :test #'equal)
            (make-hash-table :test #'equal) (make-hash-table :test #'equal)
            (make-hash-table :test #'equal) nil)))

;;;; Errors out of commands

(ert-deftest drover-robustness-test-error-report ()
  "An error is reported on one short line, its data briefly, at most once a minute."
  (let* ((index (drover-robustness-test-index 3000))
         (err (list 'args-out-of-range index 12))
         (messages (drover-robustness-test-messages
                     (drover-report-failure "timer" err)
                     (drover-report-failure "timer" err)
                     (drover-report-failure "timer" (list 'error (make-string 500000 ?x)))
                     (drover-report-failure "timer" (list 'wrong-type-argument 'stringp
                                                        (make-list 100000 'a)))
                     (drover-report-failure "filter" (list 'file-missing "Opening input file"
                                                         "No such file or directory" "/x")))))
    ;; Printed whole, as Emacs reports a timer's error.
    (should (> (length (format "%S" err)) 500000))
    (should (= (length messages) 4))
    (should (drover-robustness-test-bounded-p messages))
    (should (equal (car messages)
                   "Drover timer: args-out-of-range: Args out of range: #s(drover--index …), 12"))
    (should (string-match-p "\\`Drover timer: error: x+…\\'" (nth 1 messages)))
    (should (string-match-p "\\`Drover timer: wrong-type-argument: Wrong type argument: stringp, (a a a a a a a a …)\\'"
                            (nth 2 messages)))
    (should (equal (nth 3 messages)
                   "Drover filter: file-missing: Opening input file: No such file or directory: /x")))
  ;; Once the interval passed, the error is reported again.
  (let ((drover-error-interval 0.05))
    (should (= (length (drover-robustness-test-messages
                         (drover-report-failure "timer" '(error "Again"))
                         (sleep-for 0.1)
                         (drover-report-failure "timer" '(error "Again"))))
               2))))

(ert-deftest drover-robustness-test-quietly ()
  "Work for a user not at this Emacs shows no window and asks nothing, in no dialog box either."
  (let ((use-dialog-box t))
    (drover-quietly
     (should inhibit-interaction)
     (should-not use-dialog-box)
     (should (eq (car display-buffer-overriding-action) 'display-buffer-no-window))))
  (let ((err (should-error (drover-quietly (y-or-n-p "Send it? ")) :type 'inhibited-interaction)))
    (should (equal (drover-error-message err) "it asked a question, which nobody is there to answer"))))

(ert-deftest drover-robustness-test-changes-keep-going ()
  "A function of the change hook failing is reported once; the others and the next changes run."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let* ((index (drover-robustness-test-index 3000))
           (ran nil))
      (drover-index)
      (add-hook 'drover-changed-hook (lambda () (signal 'args-out-of-range (list index 12))))
      (add-hook 'drover-changed-hook (lambda () (push drover-changed-sessions ran)) t)
      (let ((messages (drover-robustness-test-messages
                        (dotimes (_ 3)
                          (drover-backend-changed 'fake "a")
                          (drover-changed-now)))))
        (should (equal ran '(((fake . "a")) ((fake . "a")) ((fake . "a")))))
        (should (equal messages
                       '("Drover drover-changed-hook: args-out-of-range: Args out of range: #s(drover--index …), 12")))))))

(ert-deftest drover-robustness-test-index-failing-made-anew ()
  "The index failing to take changes in is dropped, and views draw everything anew."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let ((ran nil))
      (drover-index)
      (add-hook 'drover-changed-hook (lambda () (push drover-changed-sessions ran)))
      (let ((messages (drover-robustness-test-messages
                        (cl-letf (((symbol-function 'drover--index-apply)
                                   (lambda (&rest _) (signal 'args-out-of-range
                                                             (list (drover-robustness-test-index 2000) 11)))))
                          (drover-backend-changed 'fake "a")
                          (drover-changed-now)))))
        (should (equal ran '(t)))
        (should-not drover--index)
        (should (drover-robustness-test-bounded-p messages))
        (should (string-prefix-p "Drover index of sessions: args-out-of-range" (car messages))))
      (setf (drover-session-status (car drover-test-sessions)) 'working)
      ;; As a view draws.
      (drover-index)
      (drover-backend-changed 'fake "a")
      (drover-changed-now)
      (should (equal (car ran) '((fake . "a"))))
      (should (eq (drover-session-status (drover-index-session '(fake . "a"))) 'working)))))

(ert-deftest drover-robustness-test-timer-never-prints-data ()
  "Emacs's timers never report the data of an error of drover's."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let ((index (drover-robustness-test-index 3000))
          (drover-refresh-delay 0.01)
          (ran 0))
      (drover-index)
      (add-hook 'drover-changed-hook (lambda () (signal 'args-out-of-range (list index 12))))
      (add-hook 'drover-changed-hook (lambda () (cl-incf ran)) t)
      (let ((messages (drover-robustness-test-messages
                        (drover-backend-changed 'fake "a")
                        (sleep-for 0.2)
                        (drover-backend-changed 'fake "a")
                        (sleep-for 0.2))))
        (should (= ran 2))
        (should (= (length messages) 1))
        (should (drover-robustness-test-bounded-p messages))
        (should-not (seq-some (lambda (message) (string-match-p "Error running timer" message))
                              messages))))))

(ert-deftest drover-robustness-test-sentinel ()
  "What a command of drover's answered is followed, its error reported briefly."
  (drover-claude-code-transcripts-test-with
    (let* ((script (expand-file-name "claude" home))
           (drover-claude-code-program script)
           (drover-claude-code-agents-interval 30)
           (drover-claude-code-transcripts--agents-time 0)
           (drover-claude-code-transcripts--agents-job nil)
           (big (drover-robustness-test-index 2000)))
      (drover-test-write script "#!/bin/sh\necho '[]'\n")
      (set-file-modes script #o755)
      (let ((messages
             (drover-robustness-test-messages
               (cl-letf (((symbol-function 'drover-claude-code-transcripts--agents-listed)
                          (lambda (_) (signal 'args-out-of-range (list big 3)))))
                 (drover-claude-code-transcripts--ask-agents t)
                 (should (drover-test-wait-commands))))))
        (should (equal messages
                       '("Drover claude agents: args-out-of-range: Args out of range: #s(drover--index …), 3")))))))

(ert-deftest drover-robustness-test-hooks-of-others ()
  "Drover's functions on the hooks of Emacs never signal, whatever fails within."
  (drover-list-test-with (list (drover-test-session "a"))
    (let ((big (drover-robustness-test-index 2000)))
      (drover-list-test-shown
        (let ((messages
               (drover-robustness-test-messages
                 (cl-letf (((symbol-function 'drover-list--restyle-titles)
                            (lambda () (signal 'wrong-type-argument (list 'stringp big))))
                           ((symbol-function 'drover-list--render)
                            (lambda () (signal 'wrong-type-argument (list 'stringp big)))))
                   (run-hook-with-args 'window-buffer-change-functions (selected-frame))
                   (run-hook-with-args 'window-selection-change-functions (selected-frame))
                   (run-hooks 'enable-theme-functions)
                   (with-current-buffer buffer
                     (run-hooks 'text-scale-mode-hook))))))
          (should messages)
          (should (drover-robustness-test-bounded-p messages)))))))

(ert-deftest drover-robustness-test-transcript-timer ()
  "A transcript failing to read is reported once; the others are read, and looks go on."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (read (drover-transcript-index-read index))
          (drover-transcript-scan-interval 0.05))
      (dolist (id '("a" "b" "c"))
        (drover-fixture-claude id project))
      (setf (drover-transcript-index-read index)
            (lambda (file attributes old)
              (if (string-suffix-p "b.jsonl" file)
                  (signal 'args-out-of-range (list (drover-robustness-test-index 1000) 1))
                (funcall read file attributes old))))
      (let ((messages (drover-robustness-test-messages
                        (drover-transcript-start index)
                        (let ((deadline (+ (float-time) 2)))
                          (while (and (< (drover-transcript-index-looks index) 3)
                                      (< (float-time) deadline))
                            (sleep-for 0.02)))
                        (drover-changed-now))))
        (should (>= (drover-transcript-index-looks index) 3))
        (should (drover-transcript-index-active index))
        (should (equal (drover-claude-code-transcripts-test-ids) '("a" "c")))
        (should (= (length messages) 1))
        (should (string-match-p "\\`Drover reading claude-code-transcripts: args-out-of-range: .*(.*b\\.jsonl)\\'"
                                (car messages)))
        (should (drover-robustness-test-bounded-p messages)))
      (drover-transcript-stop index))))

;;;; Text from outside

(ert-deftest drover-robustness-test-bound-line-and-text ()
  "Lines are their first line that is not blank, cut; texts their first lines and characters."
  (should (equal (drover-bound-line "\n \n  First line  \nsecond") "First line"))
  (should-not (drover-bound-line " \n\t "))
  (should-not (drover-bound-line nil))
  (let ((short "Already bounded"))
    (should (eq (drover-bound-line short) short))
    (should (eq (drover-bound-text short) short)))
  (let* ((huge (concat "Paste: " (make-string 500000 ?x) "\nmore"))
         (start (float-time))
         (line (drover-bound-line huge))
         (text (drover-bound-text huge 3 1000)))
    (should (< (- (float-time) start) 0.01))
    (should (= (length line) (1+ drover-line-limit)))
    (should (string-suffix-p "x…" line))
    (should (= (length text) 1001)))
  (should (equal (drover-bound-text "a\nb\nc\nd" 2) "a\nb…"))
  (should (equal (drover-bound-text "a\nb\nc\nd" 2 nil t) "…c\nd"))
  (should (equal (drover-bound-lines (concat "a\n" (make-string 500 ?y)) 5)
                 (concat "a\n" (make-string drover-line-limit ?y) "…")))
  ;; A tooltip within bounds is itself; a short line with a carriage
  ;; return stays as it is.
  (let ((tooltip "Title\nproject:main\nbuild (Opus 5.5)"))
    (should (eq (drover-bound-lines tooltip) tooltip)))
  (should (equal (drover-bound-lines "a\rb\nc") "a\rb\nc")))

(ert-deftest drover-robustness-test-records-bounded ()
  "Records are bounded as drover takes them in, the backend's own left alone."
  (let* ((title (concat "Fix the build\n" (make-string 500000 ?x)))
         (summary (make-string 500000 ?q))
         (session (drover-test-session "big" :status 'waiting
                                       :requests `((:kind question :summary ,summary))
                                       :detail summary)))
    (setf (drover-session-title session) title)
    (drover-test-with-backend (list session)
      (let ((record (drover-find 'fake "big"))
            (listed (drover-index-session '(fake . "big"))))
        (pcase-dolist (`(,name . ,record) (list (cons "found" record) (cons "listed" listed)))
          (ert-info (name :prefix "Record: ")
            (should (equal (drover-session-title record) "Fix the build"))
            (should (<= (length (drover-session-detail record)) (1+ drover-text-limit)))
            (should (<= (length (plist-get (car (drover-session-requests record)) :summary))
                        (1+ drover-text-limit))))))
      ;; The backend's record is the backend's.
      (should (eq (drover-session-title session) title)))
    (let ((plain (drover-test-session "plain")))
      (should (eq (drover--bound-record plain) plain)))))

(defun drover-robustness-test-older-record (record)
  "Return RECORD as drover d61bfdf made it, without the slots that came after."
  (let ((older (make-record 'drover-session 31 nil)))
    (dotimes (index 32)
      (aset older index (aref record index)))
    older))

(ert-deftest drover-robustness-test-records-of-older-layouts ()
  "A record a backend made with an older drover gets the slots that came since.
Loaded over a drover still running, this one reads the slots that came
after, such as the host of a session, of the records its backends made
before: read past their end, their sessions were reported as failing."
  (let ((old (drover-robustness-test-older-record
              (drover-test-session "old" :status 'waiting :title (concat "Old\n" (make-string 1000 ?x)))))
        (short (drover-robustness-test-older-record (drover-test-session "short" :status 'working))))
    (should (= (length old) 32))
    (drover-test-with-backend (list old short)
      (let ((bounded (drover--bound-record old))
            (whole (drover--bound-record short)))
        (should (= (length bounded) (length (drover-session-create))))
        (should (equal (drover-session-title bounded) "Old"))
        (should (eq (drover-session-status bounded) 'waiting))
        (should-not (drover-session-host bounded))
        (should-not (drover-session-default-branch bounded))
        ;; Complete, though within bounds: a copy, the backend's own left alone.
        (should (= (length whole) (length (drover-session-create))))
        (should (equal (drover-session-title whole) "Session short"))
        (should (= (length short) 32)))
      (let ((messages (drover-robustness-test-messages
                        (should (equal (drover-session-title (drover-find 'fake "short")) "Session short"))
                        (should (equal (sort (mapcar #'drover-session-id (drover-index-sessions)) #'string<)
                                       '("old" "short"))))))
        (should-not messages)))))

(ert-deftest drover-robustness-test-structures-before-use ()
  "Each file defines its structures before code calling their accessors.
Accessors are inlined where code is compiled, or loaded from its
sources: code before the definition, compiled or loaded where another
version of drover runs, would read the slots of that version's
structures.  The accessors of slots the links replaced, defined as
functions, have their setters defined with them."
  (let ((root (file-name-directory (locate-library "drover-core"))))
    (dolist (file (directory-files root t "\\`drover.*\\.el\\'"))
      (let ((forms nil))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t (push (read (current-buffer)) forms))
            (end-of-file nil)))
        (setq forms (nreverse forms))
        (let ((defined (make-hash-table :test #'eq)))
          ;; Where each accessor comes: its structure's definition, or
          ;; the setter of one defined as a function.
          (seq-do-indexed
           (lambda (form index)
             (pcase form
               (`(cl-defstruct ,spec . ,slots)
                (let ((name (if (consp spec) (car spec) spec)))
                  (dolist (slot slots)
                    (unless (stringp slot)
                      (puthash (intern (format "%s-%s" name (if (consp slot) (car slot) slot))) index defined)))))
               (`(gv-define-setter ,name . ,_) (puthash name index defined))))
           forms)
          (seq-do-indexed
           (lambda (form index)
             (let ((seen nil))
               (named-let walk ((tree form))
                 (cond ((eq (car-safe tree) 'quote))
                       ((consp tree) (walk (car tree)) (walk (cdr tree)))
                       ((and (symbolp tree) (> (gethash tree defined -1) index))
                        (push tree seen))))
               ;; The function an accessor is, defined with its setter.
               (when (memq (car-safe form) '(defun defsubst))
                 (setq seen (delq (cadr form) seen)))
               (ert-info ((format "%s, form %d" (file-name-nondirectory file) index))
                 (should-not (delete-dups seen)))))
           forms))))))

(ert-deftest drover-robustness-test-measuring-bounded ()
  "Measuring a text of hundreds of kilobytes in pixels measures its start."
  (let* ((huge (make-string 500000 ?x))
         (start (float-time))
         (width (drover-text-pixel-width huge))
         (cut (drover-truncate-to-pixels huge 40 #'drover-text-pixel-width))
         (extent (drover-text-ink-extent huge)))
    (should (< (- (float-time) start) 0.5))
    (should (>= width 500000))
    (should (>= (cdr extent) width))
    (should (<= (length cut) 41))))

(defun drover-robustness-test-help-echoes ()
  "Return the strings of the property `help-echo' in the current buffer."
  (let ((position (point-min))
        (echoes nil))
    (while (< position (point-max))
      (let ((help (get-text-property position 'help-echo)))
        (when (stringp help)
          (push help echoes)))
      (setq position (next-single-property-change position 'help-echo nil (point-max))))
    echoes))

(defun drover-robustness-test-bounded-help-p (echoes)
  "Return non-nil when each of ECHOES, tooltips, has short lines, not too many."
  (seq-every-p (lambda (help)
                 (let ((lines (split-string help "\n")))
                   (and (<= (length lines) drover-text-lines)
                        (seq-every-p (lambda (line) (<= (length line) (1+ drover-line-limit))) lines))))
               echoes))

(ert-deftest drover-robustness-test-list-of-huge-texts ()
  "A list of sessions titled by texts of hundreds of kilobytes draws in normal time."
  (let ((big (drover-test-session "big" :status 'waiting :since (- drover-test-now 60)
                                  :requests `((:kind question :summary ,(make-string 500000 ?q))))))
    (setf (drover-session-title big) (concat "Investigate " (make-string 500000 ?x)))
    (drover-list-test-with (list big (drover-test-session "other" :status 'idle))
      (drover-list-test-shown
        (let* ((start (float-time))
               (messages (drover-robustness-test-messages (drover-list--render)))
               (drawn (- (float-time) start)))
          (should (< drawn 0.5))
          (should (drover-robustness-test-bounded-p messages)))
        (should (string-match-p "Investigate x+…" (drover-list-test-heading "big")))
        (should (< (length (buffer-string)) 20000))
        (should (drover-robustness-test-bounded-help-p (drover-robustness-test-help-echoes)))
        ;; A change of the session draws its line in place, in time.
        (let ((times nil))
          (dolist (status '(working waiting working waiting))
            (setf (drover-session-status big) status)
            (let ((start (float-time)))
              (drover-backend-changed 'fake "big")
              (drover-changed-now)
              (push (- (float-time) start) times)))
          (should (< (apply #'max times) 0.1)))
        (should (drover-robustness-test-bounded-help-p (drover-robustness-test-help-echoes)))))))

(ert-deftest drover-robustness-test-claude-huge-prompt ()
  "A first prompt of 500 kB on one line titles its conversation, bounded."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (prompt (concat "Investigate the crash " (make-string 500000 ?x) " please")))
      (drover-fixture-claude "big" project :prompt prompt)
      (drover-claude-code-transcripts-test-scan)
      (let ((title (drover-session-title (drover-find 'claude-code "big"))))
        (should (string-prefix-p "Investigate the crash xxx" title))
        (should (= (length title) (1+ drover-transcript-text-limit)))
        (should (string-suffix-p "…" title)))
      ;; A turn appended is followed in time, the list drawn whole and in place.
      (let ((buffer (drover-list--buffer)))
        (unwind-protect
            (with-current-buffer buffer
              (save-window-excursion
                (delete-other-windows)
                (set-window-buffer (selected-window) buffer)
                (let ((messages (drover-robustness-test-messages
                                  (revert-buffer)
                                  (let ((start (float-time)))
                                    (drover-fixture-claude-append
                                     (drover-fixture-claude-file "big" project)
                                     (drover-fixture-claude-user "big" project 1790000100.0 prompt))
                                    (drover-claude-code-transcripts-test-scan)
                                    (should (< (- (float-time) start) 0.1))))))
                  (should (drover-robustness-test-bounded-p messages)))
                (should (drover-robustness-test-bounded-help-p (drover-robustness-test-help-echoes)))))
          (kill-buffer buffer))))))

(ert-deftest drover-robustness-test-codex-huge-prompt ()
  "A first prompt of 500 kB on one line titles its Codex conversation, bounded."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home))
          (id drover-codex-rollouts-test-a))
      (drover-fixture-codex id project :prompt (concat "Port the parser " (make-string 500000 ?y)))
      (drover-codex-rollouts-test-scan)
      (let ((session (drover-find 'codex id)))
        (should (string-prefix-p "Port the parser yyy" (drover-session-title session)))
        (should (= (length (drover-session-title session)) (1+ drover-transcript-text-limit)))
        ;; The tail still tells the model of the newest turn.
        (should (drover-session-model session))))))

(ert-deftest drover-robustness-test-string-prefix ()
  "A JSON string cut by a window is decoded from its start."
  (with-temp-buffer
    (insert "\"caf\\u00e9 \\\"quoted\\\" \\ud83d\\ude00 and more")
    (goto-char (point-min))
    (should (equal (drover-transcript-string-prefix) "café \"quoted\" 😀 and more"))
    (should (equal (drover-transcript-string-prefix 4) "café"))
    (should-not (drover-transcript-string-at)))
  ;; Cut within an escape, or a surrogate pair.
  (dolist (cut '("\"abc\\u00" "\"abc\\" "\"abc\\ud83d"))
    (with-temp-buffer
      (insert cut)
      (goto-char (point-min))
      (should (equal (drover-transcript-string-prefix) "abc"))))
  (with-temp-buffer
    (insert "\"" (make-string 200000 ?z) "\" tail")
    (goto-char (point-min))
    (should (= (length (drover-transcript-string-at)) 200000))
    (should (looking-at " tail"))))

;;;; Structures made by another version of drover

(ert-deftest drover-robustness-test-old-index ()
  "An index made by drover before its stamp is made anew, once told."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let ((old (drover-robustness-test-old-index 2000))
          (ran nil))
      ;; Read by today's accessors, it signals with itself as data.
      (should-error (drover--index-cyclic old) :type 'args-out-of-range)
      (setq drover--index old)
      (add-hook 'drover-changed-hook (lambda () (push drover-changed-sessions ran)))
      (let ((messages (drover-robustness-test-messages
                        (drover-backend-changed 'fake "a")
                        (drover-changed-now)
                        ;; As a view draws.
                        (drover-index)
                        (drover-backend-changed 'fake "a")
                        (drover-changed-now))))
        (should (equal messages '("Drover: index of sessions made by another version of drover, made anew")))
        (should (drover-layout-current-p drover--index 'drover--index))
        (should (equal (mapcar #'drover-session-id (drover-index-sessions)) '("a")))
        ;; Views draw everything once, then follow again.
        (should (equal ran '(((fake . "a")) t)))))))

(ert-deftest drover-robustness-test-types-defined-anew ()
  "A type defined anew with other slots makes the structures of it stale."
  (drover-test-with-backend (list (drover-test-session "a"))
    (let ((drover--layouts (copy-hash-table drover--layouts)))
      (drover-index)
      (should (drover-layout-current-p drover--index 'drover--index))
      ;; As loading a drover whose sessions have one more slot does.
      (puthash 'drover-session (cons '(cl-tag-slot backend id more) (list 'drover-session))
               drover--layouts)
      (should-not (drover-layout-current-p drover--index 'drover--index))
      (let ((messages (drover-robustness-test-messages (drover-index))))
        (should (equal (length messages) 1)))
      (should (drover-layout-current-p drover--index 'drover--index))
      ;; Defining it again with the same slots changes nothing.
      (drover-note-layout 'drover--index)
      (should (drover-layout-current-p drover--index 'drover--index)))))

(ert-deftest drover-robustness-test-old-list-model ()
  "A list drawn by drover before its stamp is drawn whole, once told."
  (drover-list-test-with (list (drover-test-session "a" :status 'working)
                               (drover-test-session "b" :status 'waiting :since drover-test-now))
    (drover-list-test-shown
      ;; The model as drover d61bfdf made it: settings, groups, trees,
      ;; rows, summary, layout and size.
      (setq drover-list--model (record 'drover-list--model nil nil (make-hash-table :test #'equal)
                                       (make-hash-table :test #'equal) nil nil 0))
      (let ((messages (drover-robustness-test-messages
                        (setf (drover-session-status (drover-list-test-find "a")) 'done)
                        (setf (drover-session-since (drover-list-test-find "a")) drover-test-now)
                        (drover-backend-changed 'fake "a")
                        (drover-changed-now))))
        (should (equal messages '("Drover: list of sessions made by another version of drover, made anew")))
        (should (drover-layout-current-p drover-list--model 'drover-list--model))
        (should (equal (drover-list-test-groups) '((waiting "b") (done "a"))))))))

(ert-deftest drover-robustness-test-list-model-of-other-fields ()
  "A list drawn with other text fields, as before the harness's, is drawn whole.
Its rows measure each field of its own: read as this drover's, their
measures would be misplaced, and their summary too short."
  (drover-list-test-with (list (drover-test-session "a" :status 'working)
                               (drover-test-session "b" :status 'waiting :since drover-test-now))
    (drover-list-test-shown
      ;; The summary of a drawing of one text field fewer.
      (setf (drover-list--model-summary drover-list--model)
            (make-vector (- drover-list--measure-count 2) (drover-list--tally-create)))
      (let ((messages (drover-robustness-test-messages
                        (setf (drover-session-status (drover-list-test-find "a")) 'done)
                        (setf (drover-session-since (drover-list-test-find "a")) drover-test-now)
                        (drover-backend-changed 'fake "a")
                        (drover-changed-now))))
        (should (equal messages '("Drover: list of sessions made by another version of drover, made anew")))
        (should (= (length (drover-list--model-summary drover-list--model)) drover-list--measure-count))
        (should (equal (drover-list-test-groups) '((waiting "b") (done "a"))))))))

(ert-deftest drover-robustness-test-old-transcript-index ()
  "An index of transcripts of other slots is replaced, its timers doing nothing."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (old (record 'drover-transcript-index 'claude-code-transcripts #'ignore #'ignore #'ignore)))
      (drover-fixture-claude "a1" project)
      (setq drover-claude-code-transcripts--index old)
      (let ((messages (drover-robustness-test-messages
                        ;; Its timer runs with today's functions.
                        (drover-transcript--work old)
                        (drover-transcript--look old)
                        (drover-sessions))))
        (should (equal messages
                       '("Drover: index of Claude Code's transcripts made by another version of drover, made anew"))))
      (should-not (eq drover-claude-code-transcripts--index old))
      (should (drover-transcript-index-current-p drover-claude-code-transcripts--index))
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-claude-code-transcripts-test-ids) '("a1"))))))

(ert-deftest drover-robustness-test-old-attention ()
  "The sessions the mode line counts, found by another version of drover, are found again."
  (drover-test-with-backend (list (drover-test-session "w" :status 'waiting :since drover-test-now))
    (let ((drover-attention--sessions (make-hash-table :test #'equal))
          (drover-attention--layout nil))
      (puthash '(fake . "old") (record 'drover-session 'fake "old") drover-attention--sessions)
      (should (equal (mapcar #'drover-session-id (drover-attention--current)) '("w"))))))

(defconst drover-robustness-test-old-commit "d61bfdf"
  "The commit of drover before its structures had stamps, which a user ran.")

(ert-deftest drover-robustness-test-reload-over-old-version ()
  "Loading this drover into an Emacs running the version before it does not misread its index.
In an Emacs of its own: drover of `drover-robustness-test-old-commit'
lists 3000 sessions and draws them, then this drover is loaded over it,
and a session changes, as its timers follow."
  (let* ((root (file-name-directory (directory-file-name
                                     (file-name-directory (locate-library "drover-robustness-test")))))
         (root (if (file-exists-p (expand-file-name "drover-core.el" root))
                   root
                 (file-name-directory (locate-library "drover-core")))))
    (skip-unless (and (executable-find "git")
                      (zerop (call-process "git" nil nil nil "-C" root "cat-file" "-e"
                                           (concat drover-robustness-test-old-commit "^{commit}")))))
    (let* ((old (make-temp-file "drover-old" t))
           (state (make-temp-file "drover-reload-state" t))
           (script (make-temp-file "drover-reload" nil ".el"))
           (output (generate-new-buffer " *drover reload*")))
      (unwind-protect
          (progn
            (dolist (file (split-string (with-output-to-string
                                          (call-process "git" nil standard-output nil "-C" root "ls-tree"
                                                        "--name-only" drover-robustness-test-old-commit))
                                        "\n" t))
              (when (string-match-p "\\`drover.*\\.el\\'" file)
                (with-temp-file (expand-file-name file old)
                  (call-process "git" nil t nil "-C" root "show"
                                (format "%s:%s" drover-robustness-test-old-commit file)))))
            (with-temp-file script
              (prin1 `(progn
                        ;; -*- lexical-binding: t -*-
                        (setq load-path (cons ,old load-path)
                              load-prefer-newer t
                              debug-on-error nil)
                        ;; Neither the user's state nor their conversations.
                        (setq drover-state-directory ,(file-name-as-directory state)
                              drover-claude-code-home ,(expand-file-name "claude" state)
                              drover-codex-home ,(expand-file-name "codex" state))
                        (require 'drover)
                        (require 'drover-list)
                        (defvar sessions
                          (let ((list nil))
                            (dotimes (n 3000)
                              (push (drover-session-create :backend 'fake :id (format "s%d" n)
                                                           :title (make-string 100 ?t)
                                                           :status (if (zerop (% n 7)) 'working 'idle)
                                                           :updated (- (float-time) n))
                                    list))
                            list))
                        (drover-register-backend
                         'fake :label "Fake"
                         :sessions (lambda () sessions)
                         :session (lambda (id) (seq-find (lambda (s) (equal (drover-session-id s) id)) sessions)))
                        (setq drover-refresh-delay 0.01)
                        (drover-list)
                        (drover-changed-now)
                        (defvar messages nil)
                        (advice-add 'message :around
                                    (lambda (function format &rest arguments)
                                      (when format
                                        (push (apply #'format-message format arguments) messages))
                                      (apply function format arguments)))
                        ;; This drover, over the one running, its files in
                        ;; any order.
                        (setq load-path (cons ,(directory-file-name root) (delete ,old load-path)))
                        (dolist (file (directory-files ,root t "\\`drover.*\\.el\\'"))
                          (load file nil t))
                        (setf (drover-session-status (nth 5 sessions)) 'done)
                        (drover-backend-changed 'fake "s5")
                        (sleep-for 0.3)
                        (drover-backend-changed 'fake "s6")
                        (sleep-for 0.3)
                        (with-current-buffer (drover-list--buffer)
                          (princ (format "RESULT %S\n"
                                         (list :longest (apply #'max 0 (mapcar #'length messages))
                                               :timer-errors (seq-count (lambda (m) (string-match-p "Error running timer" m))
                                                                        messages)
                                               :index (drover-layout-current-p drover--index 'drover--index)
                                               :model (drover-layout-current-p drover-list--model 'drover-list--model)
                                               :messages (mapcar (lambda (m) (substring m 0 (min 120 (length m))))
                                                                 (reverse messages))))))
                        (kill-emacs 0))
                     (current-buffer)))
            (apply #'call-process "timeout" nil output nil "120"
                   (expand-file-name invocation-name invocation-directory)
                   "-Q" "--batch"
                   (append (mapcan (lambda (directory) (list "-L" directory)) load-path)
                           (list "-l" script)))
            (let* ((text (with-current-buffer output (buffer-string)))
                   (result (and (string-match "^RESULT \\(.*\\)$" text)
                                (car (read-from-string (match-string 1 text))))))
              (should (or result (progn (message "%s" text) nil)))
              (should (<= (plist-get result :longest) drover-robustness-test-limit))
              (should (zerop (plist-get result :timer-errors)))
              (should (plist-get result :index))
              (should (plist-get result :model))
              ;; Told once each, and nothing else: no error reading the
              ;; records of the backend made before, nor running code
              ;; inlined where the structures were others.
              (should (equal (sort (plist-get result :messages) #'string<)
                             '("Drover: index of sessions made by another version of drover, made anew"
                               "Drover: list of sessions made by another version of drover, made anew")))))
        (delete-directory old t)
        (delete-directory state t)
        (delete-file script)
        (kill-buffer output)))))

(provide 'drover-robustness-test)
;;; drover-robustness-test.el ends here
