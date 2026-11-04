;;; drover-start-test.el --- Tests of what drover lists as it starts  -*- lexical-binding: t; -*-

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

;; Drover lists the conversations engines recorded from the time it
;; starts, whichever front end is loaded (`drover-transcript-engines'),
;; and loads a front end to resume one (`drover-load-resumer'): an
;; Emacs restarted lists them before any front end loads.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)

(defconst drover-start-test-codex "019f0000-0000-7000-8000-0000000000c1"
  "The identifier of the Codex conversation of the fixtures.")

(defmacro drover-start-test-with (&rest body)
  "Run BODY with no backend, over the fixtures' homes, as a fresh Emacs.
Neither transcript backend is installed; both are uninstalled after,
for every user BODY installed them for.  `home' is the homes' directory."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-test-with-backend nil
       (let ((drover-claude-code-transcripts--users nil)
             (drover-claude-code-transcripts--index nil)
             (drover-claude-code-transcripts--agents (make-hash-table :test #'equal))
             (drover-claude-code-transcripts--recent nil)
             (drover-claude-code-agents-interval nil)
             (drover-claude-code-history-days nil)
             (drover-codex-rollouts--users nil)
             (drover-codex-rollouts--index nil)
             (drover-codex-rollouts--names nil)
             (drover-codex-program (expand-file-name "no-codex" home))
             (drover-codex-history-days nil)
             (drover-transcript-engines '(claude-code codex)))
         (setq drover--backends nil)
         (unwind-protect
             (progn ,@body)
           (dolist (user drover-claude-code-transcripts--users)
             (drover-claude-code-transcripts-uninstall user))
           (dolist (user drover-codex-rollouts--users)
             (drover-codex-rollouts-uninstall user)))))))

(defun drover-start-test-write-fixtures (home)
  "Write a Claude Code conversation and a Codex one under HOME."
  (let ((project (expand-file-name "work/app/" home)))
    (drover-fixture-claude "a1" project)
    (drover-fixture-codex drover-start-test-codex project)))

(defun drover-start-test-scan ()
  "Read the transcripts and rollouts now, and follow the changes reported."
  (when drover-claude-code-transcripts--index
    (drover-transcript-scan drover-claude-code-transcripts--index))
  (when drover-codex-rollouts--index
    (drover-transcript-scan drover-codex-rollouts--index))
  (drover-changed-now))

(ert-deftest drover-start-test-lists-recorded-conversations ()
  "Starting, drover lists what the engines recorded, without a front end."
  (drover-start-test-with
    (drover-start-test-write-fixtures home)
    (should-not (memq 'claude-code (drover-backends)))
    (drover-start)
    (should (memq 'claude-code (drover-backends)))
    (should (memq 'codex (drover-backends)))
    (should (equal drover-claude-code-transcripts--users '(drover)))
    (should (equal drover-codex-rollouts--users '(drover)))
    ;; Started with the others, as `drover-start' starts every backend.
    (should (memq 'claude-code drover--started))
    (drover-start-test-scan)
    (should (drover-find 'claude-code "a1"))
    (should (drover-find 'codex drover-start-test-codex))
    ;; Starting again installs nothing anew.
    (let ((registered 0))
      (cl-letf (((symbol-function 'drover-claude-code-transcripts--register)
                 (lambda () (cl-incf registered))))
        (drover-start))
      (should (zerop registered)))))

(ert-deftest drover-start-test-front-ends-share ()
  "A front end installing a backend shares it with drover, each a user."
  (drover-start-test-with
    (drover-start-test-write-fixtures home)
    ;; A front end loaded first, as before an Emacs restarted.
    (drover-codex-rollouts-install 'agent-shell)
    (drover-start)
    (should (equal (sort (copy-sequence drover-codex-rollouts--users) #'string<)
                   '(agent-shell drover)))
    ;; The front end going leaves drover's.
    (drover-codex-rollouts-uninstall 'agent-shell)
    (should (memq 'codex (drover-backends)))
    ;; An engine no longer listed goes for drover, and stays for a front end.
    (drover-claude-code-transcripts-install 'claude-code-ide)
    (let ((drover-transcript-engines nil))
      (drover-start))
    (should-not (memq 'codex (drover-backends)))
    (should (memq 'claude-code (drover-backends)))
    (should (equal drover-claude-code-transcripts--users '(claude-code-ide)))
    ;; Listed again, drover installs it again.
    (drover-start)
    (should (memq 'codex (drover-backends)))
    (should (memq 'drover drover-claude-code-transcripts--users))))

(ert-deftest drover-start-test-nothing-recorded ()
  "An engine that recorded nothing is not listed, until it records."
  (drover-start-test-with
    (drover-start)
    (should-not (memq 'claude-code (drover-backends)))
    (should-not (memq 'codex (drover-backends)))
    (should-not drover-claude-code-transcripts--users)
    (drover-fixture-claude "a1" (expand-file-name "work/app/" home))
    (drover-start)
    (should (memq 'claude-code (drover-backends)))
    (should-not (memq 'codex (drover-backends)))
    ;; Archived conversations alone count as recorded.
    (drover-fixture-codex drover-start-test-codex (expand-file-name "work/app/" home) :archived t)
    (drover-start)
    (should (memq 'codex (drover-backends)))))

(ert-deftest drover-start-test-opted-out ()
  "With no engine listed, drover lists nothing recorded as it starts."
  (drover-start-test-with
    (drover-start-test-write-fixtures home)
    (let ((drover-transcript-engines nil))
      (drover-start))
    (should-not (memq 'claude-code (drover-backends)))
    (should-not (memq 'codex (drover-backends)))
    (let ((drover-transcript-engines '(codex)))
      (drover-start))
    (should-not (memq 'claude-code (drover-backends)))
    (should (memq 'codex (drover-backends)))))

(ert-deftest drover-start-test-failure-reported ()
  "An engine failing to install is reported, and the others start."
  (drover-start-test-with
    (drover-start-test-write-fixtures home)
    (let ((reported nil))
      (cl-letf (((symbol-function 'drover-claude-code-transcripts-install)
                 (lambda (_user) (error "Broken")))
                ((symbol-function 'drover-report-failure)
                 (lambda (context _err) (push context reported))))
        (drover-start))
      (should reported)
      (should-not (memq 'claude-code (drover-backends)))
      (should (memq 'codex (drover-backends))))))

;;;; Resuming in a front end not loaded

(defvar drover-start-test-calls nil
  "The conversations the front end of the tests resumed, newest first.")

(defmacro drover-start-test-with-front-end (front-end &rest body)
  "Run BODY with an installed front end FRONT-END, a symbol, not loaded.
Its files are in a fresh directory of `load-path': FRONT-END itself and
drover's backend of it, drover-FRONT-END, which registers a resumer of
the engine `fake' recording its calls in `drover-start-test-calls'.  It loads once, and
is forgotten after."
  (declare (indent 1) (debug t))
  (let ((directory (make-symbol "directory")))
    `(let* ((,directory (make-temp-file "drover-front-end" t))
            (front (symbol-name ,front-end))
            (backend (intern (concat "drover-" front)))
            (load-path (cons ,directory load-path))
            (drover--resumers nil)
            (drover--resumer-properties nil)
            (drover-resume-front-ends nil)
            (drover--front-end-backends (list (list ,front-end backend 'fake)))
            (drover-start-test-calls nil))
       (with-temp-file (expand-file-name (concat front ".el") ,directory)
         (insert (format ";;; -*- lexical-binding: t -*-\n(provide '%s)\n" front)))
       (with-temp-file (expand-file-name (concat "drover-" front ".el") ,directory)
         (insert (format ";;; -*- lexical-binding: t -*-\n(require '%s)\n(drover-register-resumer 'fake '%s (lambda (id directory) (push (list '%s id directory) drover-start-test-calls) (current-buffer)))\n(provide '%s)\n"
                         front front front backend)))
       (unwind-protect
           (progn ,@body)
         (setq features (delq backend (delq ,front-end features)))
         (delete-directory ,directory t)))))


(ert-deftest drover-start-test-resume-loads-front-end ()
  "Opening a conversation no front end loaded resumes loads one that does."
  (drover-start-test-with-front-end 'drover-test-front
    (should-not (drover-resumer 'fake))
    (drover-resume 'fake "x" "/work/")
    (should (featurep 'drover-test-front))
    (should (equal drover-start-test-calls '((drover-test-front "x" "/work/"))))
    ;; Loaded, it resumes without loading again.
    (drover-resume 'fake "y" "/work/")
    (should (equal (car drover-start-test-calls) '(drover-test-front "y" "/work/")))
    ;; Nor does a front end resuming no conversation of the engine load.
    (should-not (drover-load-resumer 'other))))

(ert-deftest drover-start-test-resume-with-loads-front-end ()
  "Resuming in a front end named, as scheduled prompts do, loads it first."
  (drover-start-test-with-front-end 'drover-test-named
    (drover-resume-with 'drover-test-named 'fake "x" "/work/")
    (should (equal drover-start-test-calls '((drover-test-named "x" "/work/"))))))

(ert-deftest drover-start-test-resume-not-installed ()
  "A front end not installed is not loaded, and the conversation does not open."
  (let ((drover--resumers nil)
        (drover--resumer-properties nil)
        (drover-resume-front-ends nil)
        (drover--front-end-backends '((drover-test-absent drover-drover-test-absent fake))))
    (should (equal (cadr (should-error (drover-resume 'fake "x" "/work/") :type 'user-error))
                   "No front end installed resumes fake conversations: install drover-test-absent"))
    (should-not (featurep 'drover-test-absent))))

(ert-deftest drover-start-test-resume-load-fails ()
  "A front end failing to load is reported, and the conversation does not open."
  (drover-start-test-with-front-end 'drover-test-broken
    (with-temp-file (locate-library "drover-drover-test-broken")
      (insert ";;; -*- lexical-binding: t -*-\n(error \"Broken front end\")\n"))
    (let ((warned nil))
      (cl-letf (((symbol-function 'drover-warn) (lambda (&rest args) (push args warned))))
        (should-error (drover-resume 'fake "x" "/work/") :type 'user-error))
      (should warned))))

;;;; An Emacs restarted

(ert-deftest drover-start-test-restart ()
  "An Emacs started anew lists the recorded conversations before any front end.
A batch Emacs of its own loads drover, as a user's init does, and turns
on `drover-attention-mode'; no front end loads."
  (drover-test-with-homes
    (drover-start-test-write-fixtures home)
    (let* ((emacs (expand-file-name invocation-name invocation-directory))
           (form
            `(progn
               (setq load-path ',load-path
                     load-prefer-newer t)
               (setq drover-claude-code-home ,drover-claude-code-home
                     drover-codex-home ,drover-codex-home
                     drover-state-directory ,(expand-file-name "state/" home)
                     drover-claude-code-agents-interval nil
                     drover-codex-program ,(expand-file-name "no-codex" home))
               ;; As the package's autoloads would have it.
               (require 'drover)
               (require 'drover-attention)
               (drover-attention-mode 1)
               (let ((end (+ (float-time) 20)))
                 (while (and (< (float-time) end)
                             (not (and (drover-find 'claude-code "a1")
                                       (drover-find 'codex ,drover-start-test-codex))))
                   (sleep-for 0.05)))
               (prin1 (list :claude (and (drover-find 'claude-code "a1") t)
                            :codex (and (drover-find 'codex ,drover-start-test-codex) t)
                            :front-ends (seq-filter #'featurep '(claude-code-ide agent-shell
                                                                 drover-claude-code-ide
                                                                 drover-agent-shell))
                            :users (list drover-claude-code-transcripts--users
                                         drover-codex-rollouts--users)))
               (terpri)))
           (output (with-temp-buffer
                     (let ((status (call-process emacs nil t nil "-Q" "--batch"
                                                 "--eval" (prin1-to-string form))))
                       (unless (eql status 0)
                         (ert-fail (list status (string-limit (buffer-string) 2000 t)))))
                     (goto-char (point-max))
                     (forward-line -1)
                     (read (current-buffer)))))
      (should (equal output '(:claude t :codex t :front-ends nil :users ((drover) (drover))))))))

(provide 'drover-start-test)
;;; drover-start-test.el ends here
