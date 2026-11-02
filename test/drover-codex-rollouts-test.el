;;; drover-codex-rollouts-test.el --- Tests of Codex's conversations  -*- lexical-binding: t; -*-

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

;; The `codex' backend over rollouts of the fixtures, in the throwaway
;; homes of `drover-test-with-homes'; its index is looked at
;; synchronously, and the Codex CLI is a script of the test's.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)

(defmacro drover-codex-rollouts-test-with (&rest body)
  "Run BODY with the `codex' backend alone, over the fixtures' homes.
`home' is the homes' directory; the Codex CLI is not found."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-test-with-backend nil
       (let ((drover-codex-rollouts--users nil)
             (drover-codex-rollouts--index nil)
             (drover-codex-rollouts--names nil)
             (drover-codex-program (expand-file-name "no-codex" home))
             (drover-codex-include-non-interactive nil)
             (drover-codex-history-days nil)
             (drover--resumers nil)
             (drover--resumer-properties nil)
             (drover-resume-front-ends nil))
         (setq drover--backends nil)
         (drover-codex-rollouts-install 'test)
         (unwind-protect
             (progn ,@body)
           (drover-codex-rollouts-uninstall 'test))))))

(defun drover-codex-rollouts-test-scan ()
  "Look at the rollouts now, and follow the changes reported."
  (drover-transcript-scan drover-codex-rollouts--index)
  (drover-changed-now))

(defun drover-codex-rollouts-test-ids ()
  "Return the identifiers of the sessions drover lists, sorted."
  (sort (mapcar #'drover-session-id (drover-sessions)) #'string<))

(defconst drover-codex-rollouts-test-a "019f0000-0000-7000-8000-00000000000a"
  "A conversation.")

(defconst drover-codex-rollouts-test-b "019f0000-0000-7000-8000-00000000000b"
  "Another conversation.")

(ert-deftest drover-codex-rollouts-test-record ()
  "A rollout tells its conversation: directory, prompt, model and effort, times."
  (drover-codex-rollouts-test-with
    (drover-test-write (expand-file-name "models_cache.json" drover-codex-home)
                       drover-test-codex-models-cache)
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-codex-rollouts-test-scan)
      (let ((session (drover-find 'codex drover-codex-rollouts-test-a)))
        (should (equal (list (drover-session-title session) (drover-session-directory session)
                             (drover-session-branch session) (drover-session-model session)
                             (drover-session-effort session) (drover-session-engine session)
                             (drover-session-engine-id session) (drover-session-status session)
                             (drover-session-created session) (drover-session-updated session))
                       (list "Add the export command" project "feature" "GPT-5.5" "high" 'codex
                             drover-codex-rollouts-test-a 'idle 1790000000.0 1790000006.0)))))))

(ert-deftest drover-codex-rollouts-test-long-session-meta ()
  "A first record longer than the window still tells the session's directory."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-codex drover-codex-rollouts-test-a project
                            :instructions (* 2 drover-transcript-window))
      (drover-codex-rollouts-test-scan)
      (let ((session (drover-find 'codex drover-codex-rollouts-test-a)))
        (should (equal (drover-session-directory session) project))
        (should (eql (drover-session-created session) 1790000000.0))
        ;; Its git branch, past the instructions, is out of the window.
        (should-not (drover-session-branch session))))))

(ert-deftest drover-codex-rollouts-test-names ()
  "The name Codex gave a conversation last is its title, read as it is appended."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-fixture-codex drover-codex-rollouts-test-b project :prompt "Other")
      (drover-fixture-codex-name drover-codex-rollouts-test-a "Export to CSV")
      (drover-codex-rollouts-test-scan)
      (should (equal (drover-session-title (drover-find 'codex drover-codex-rollouts-test-a)) "Export to CSV"))
      (should (equal (drover-session-title (drover-find 'codex drover-codex-rollouts-test-b)) "Other"))
      (let ((changes nil))
        (cl-letf* ((changed (symbol-function 'drover-backend-changed))
                   ((symbol-function 'drover-backend-changed)
                    (lambda (&rest arguments) (push arguments changes) (apply changed arguments))))
          (drover-fixture-codex-name drover-codex-rollouts-test-a "CSV export")
          (drover-codex-rollouts-test-scan))
        ;; Only the conversation renamed is told.
        (should (equal changes `((codex ,drover-codex-rollouts-test-a)))))
      (should (equal (drover-session-title (drover-find 'codex drover-codex-rollouts-test-a)) "CSV export")))))

(ert-deftest drover-codex-rollouts-test-archived ()
  "Rollouts Codex archived are the archived sessions, not listed live."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-fixture-codex drover-codex-rollouts-test-b project :archived t)
      (drover-codex-rollouts-test-scan)
      (should (equal (drover-codex-rollouts-test-ids) (list drover-codex-rollouts-test-a)))
      (drover-set-show-archived t)
      (unwind-protect
          (let ((archived (drover-archived-sessions)))
            (should (equal (mapcar #'drover-session-id archived) (list drover-codex-rollouts-test-b)))
            (should (drover-session-archived (car archived))))
        (drover-set-show-archived nil)))))

(ert-deftest drover-codex-rollouts-test-kept-between-sessions ()
  "A new Emacs finds the conversations it kept, read again or not."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-codex-rollouts-test-scan)
      (drover-transcript-save drover-codex-rollouts--index t)
      (let ((drover-codex-rollouts--index (drover-codex-rollouts--make-index)))
        (drover-transcript-load drover-codex-rollouts--index)
        (drover-index-refresh)
        (should (equal (drover-codex-rollouts-test-ids) (list drover-codex-rollouts-test-a)))
        (should (drover-find 'codex drover-codex-rollouts-test-a))
        (drover-codex-rollouts--recap (drover-find 'codex drover-codex-rollouts-test-a)
                                      (lambda (recap) (should-not (plist-get recap :failure))))
        (drover-codex-rollouts-test-scan)
        (should (drover-find 'codex drover-codex-rollouts-test-a))))))

(ert-deftest drover-codex-rollouts-test-non-interactive-and-subagents ()
  "Sessions `codex exec' ran are left out unless asked; subagents name their parent."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home))
          (exec "019f0000-0000-7000-8000-0000000000e0")
          (child "019f0000-0000-7000-8000-0000000000c0"))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-fixture-codex exec project :source "exec")
      (drover-fixture-codex child project :parent drover-codex-rollouts-test-a)
      (drover-codex-rollouts-test-scan)
      (should (equal (drover-codex-rollouts-test-ids) (list drover-codex-rollouts-test-a child)))
      (should (equal (drover-session-parent (drover-find 'codex child)) drover-codex-rollouts-test-a))
      (let ((drover-codex-include-non-interactive t))
        (drover-index-refresh)
        (should (member exec (drover-codex-rollouts-test-ids)))))))

(ert-deftest drover-codex-rollouts-test-recap ()
  "The recap of a Codex conversation tells its prompt, reply and patches."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home))
          (recap nil))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-codex-rollouts-test-scan)
      (drover-recap (drover-find 'codex drover-codex-rollouts-test-a) (lambda (value) (setq recap value)))
      (should (equal recap '(:prompt "Add the export command" :reply "The export command writes CSV."
                                     :changes (("src/export.rs" 2 1) ("src/csv.rs" 1 0)) :error nil))))))

(ert-deftest drover-codex-rollouts-test-front-end-wins ()
  "A Codex conversation an agent-shell shell follows is the shell's; it opens there."
  (drover-codex-rollouts-test-with
    (let* ((project (expand-file-name "work/" home))
           (shell (drover-session-create :backend 'shell :id "shell-1" :title "Shell"
                                         :engine 'codex :engine-id drover-codex-rollouts-test-a))
           (shells nil)
           (opened nil))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-fixture-codex drover-codex-rollouts-test-b project)
      (drover-test-register-simple 'shell (lambda () shells)
                                   :open (lambda (session _) (push (drover-session-id session) opened)))
      (drover-codex-rollouts-test-scan)
      (let ((record (drover-find 'codex drover-codex-rollouts-test-a)))
        (drover-index)
        (setq shells (list shell))
        (drover-backend-changed 'shell)
        (drover-changed-now)
        (should (drover-index-incremental-p))
        (should (equal (sort (mapcar #'drover-session-id (drover-index-sessions)) #'string<)
                       (list drover-codex-rollouts-test-b "shell-1")))
        (drover-open record)
        (should (equal opened '("shell-1")))))))

(ert-deftest drover-codex-rollouts-test-open-resumes ()
  "Opening a closed Codex conversation resumes it in its directory."
  (drover-codex-rollouts-test-with
    (let ((project (expand-file-name "work/" home))
          (resumed nil))
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-codex-rollouts-test-scan)
      (drover-register-resumer 'codex 'agent-shell (lambda (id directory) (push (list id directory) resumed)))
      (drover-open (drover-find 'codex drover-codex-rollouts-test-a))
      (should (equal resumed `((,drover-codex-rollouts-test-a ,project)))))))

(ert-deftest drover-codex-rollouts-test-archive-without-cli ()
  "Without the Codex CLI, drover archives Codex conversations itself, and cannot delete them."
  (drover-codex-rollouts-test-with
    (drover-test-with-state
      (let ((project (expand-file-name "work/" home)))
        (drover-fixture-codex drover-codex-rollouts-test-a project)
        (drover-codex-rollouts-test-scan)
        (let ((session (drover-find 'codex drover-codex-rollouts-test-a)))
          (should-not (drover-supports-p session :archive))
          (should-not (drover-supports-p session :delete))
          (should (drover-archivable-p session))
          (drover-archive session)
          (should (assoc (cons 'codex drover-codex-rollouts-test-a) (drover--archive)))
          (should-not (drover-sessions))
          (drover-unarchive (drover-find 'codex drover-codex-rollouts-test-a))
          (should (equal (drover-codex-rollouts-test-ids) (list drover-codex-rollouts-test-a))))))))

(ert-deftest drover-codex-rollouts-test-archive-with-cli ()
  "With the Codex CLI, archiving, restoring and deleting are its commands, run without waiting."
  (drover-codex-rollouts-test-with
    (let* ((project (expand-file-name "work/" home))
           (log (expand-file-name "codex.log" home))
           (script (expand-file-name "bin/codex" home))
           (drover-codex-program script))
      (drover-test-write script (format "#!/bin/sh\necho \"$@\" >> %s\n[ \"$1\" != delete ]\n"
                                        (shell-quote-argument log)))
      (set-file-modes script #o755)
      (drover-fixture-codex drover-codex-rollouts-test-a project)
      (drover-codex-rollouts-test-scan)
      (let ((session (drover-find 'codex drover-codex-rollouts-test-a))
            (warnings nil))
        (should (equal (drover-codex-rollouts-command "archive" drover-codex-rollouts-test-a)
                       (list script "archive" drover-codex-rollouts-test-a)))
        (should (drover-supports-p session :archive))
        (cl-letf (((symbol-function 'drover-warn)
                   (lambda (format &rest args) (push (apply #'format-message format args) warnings))))
          (drover-archive session)
          (drover-codex-rollouts--unarchive session)
          (drover-codex-rollouts--delete session)
          (should (drover-test-wait-commands)))
        ;; Not in drover's archive: Codex's.
        (should-not (drover--archive))
        ;; The three ran at once, in any order.
        (should (equal (sort (split-string (with-temp-buffer (insert-file-contents log) (buffer-string))
                                           "\n" t)
                             #'string<)
                       (mapcar (lambda (command) (concat command " " drover-codex-rollouts-test-a))
                               '("archive" "delete" "unarchive"))))
        (should (equal (length warnings) 1))
        (should (string-match-p "\\`Codex could not delete" (car warnings)))))))

(ert-deftest drover-codex-rollouts-test-prints-in-proportion ()
  "The index of rollouts prints in proportion to the conversations it read.
Emacs prints a value whole in an error's data, or as `format' does: an
entry or a record leading back to the index would print it all again."
  (drover-codex-rollouts-test-with
    (dotimes (i 100)
      (drover-fixture-codex (format "019f0000-0000-7000-8000-%012d" i)
                            (expand-file-name (format "work/p%d/" (% i 7)) home)
                            :prompt (format "Prompt %d" i)))
    (drover-codex-rollouts-test-scan)
    (should (= (length (drover-sessions)) 100))
    (drover-test-prints-in-proportion
     100 (list (cons "an entry" (car (hash-table-values (drover-transcript-index-entries
                                                          drover-codex-rollouts--index))))
               (cons "a record" (drover-find 'codex "019f0000-0000-7000-8000-000000000001"))))))

(provide 'drover-codex-rollouts-test)
;;; drover-codex-rollouts-test.el ends here
