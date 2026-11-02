;;; drover-command-test.el --- Tests of commands run without waiting  -*- lexical-binding: t; -*-

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

;; `drover-command-run' through the runner, and directly where no shell
;; is found: scripts of the tests', in a throwaway directory, stand for
;; the CLIs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-command)

(defmacro drover-command-test-with (&rest body)
  "Run BODY with no command running, in a fresh directory bound to `home'.
The runner BODY starts is stopped after it."
  (declare (indent 0) (debug t))
  `(let ((home (file-name-as-directory (make-temp-file "drover-command" t)))
         (drover-command--runner nil)
         (drover-command--jobs (make-hash-table)))
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (id _) (drover-command-cancel id)) drover-command--jobs)
       (drover-command-stop)
       (delete-directory home t))))

(defun drover-command-test-script (file content)
  "Write the shell script CONTENT to FILE, executable, and return FILE."
  (drover-test-write file (concat "#!/bin/sh\n" content))
  (set-file-modes file #o755)
  file)

(defun drover-command-test-run (command &optional directory)
  "Run COMMAND in DIRECTORY with `drover-command-run', and return its result."
  (let* ((result :none)
         (id (drover-command-run "test" command (lambda (answer) (setq result answer)) directory)))
    (should (drover-command-live-p id))
    (should (drover-test-wait-commands))
    (should-not (drover-command-live-p id))
    result))

(defmacro drover-command-test-both (&rest body)
  "Run BODY through the runner, then with commands started directly."
  (declare (indent 0) (debug t))
  `(progn
     (ert-info ("Through the runner")
       (let ((drover-command-shell "/bin/sh"))
         ,@body))
     (drover-command-stop)
     (ert-info ("Directly")
       (let ((drover-command-shell nil))
         ,@body))))

(ert-deftest drover-command-test-result ()
  "A command's output, errors and status come back, its arguments and directory intact."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((script (drover-command-test-script
                   (expand-file-name "bin/tool" home)
                   "pwd -P\nfor a in \"$@\"; do printf '[%s]\\n' \"$a\"; done\necho oops >&2\nexit 3\n"))
          (work (expand-file-name "my work/" home)))
      (make-directory work)
      (drover-command-test-both
        (let ((result (drover-command-test-run
                       (list script "a b" "it's" "$HOME" "\\x" "é😀" "*" "") work)))
          (should (equal (plist-get result :exit) 3))
          (should (equal (plist-get result :output)
                         (concat (file-truename (directory-file-name work))
                                 "\n[a b]\n[it's]\n[$HOME]\n[\\x]\n[é😀]\n[*]\n[]\n")))
          (should (equal (plist-get result :errors) "oops\n"))
          (should-not (plist-get result :truncated)))))))

(ert-deftest drover-command-test-newline ()
  "An argument holding a newline, which the runner's lines cannot carry, is run directly."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((drover-command-shell "/bin/sh"))
      (let ((result (drover-command-test-run (list "/bin/sh" "-c" "printf '%s' \"$0\"" "one\ntwo"))))
        (should (equal (plist-get result :exit) 0))
        (should (equal (plist-get result :output) "one\ntwo"))))))

(ert-deftest drover-command-test-failures ()
  "A program missing, or a directory missing, is a status and its error, not a signal."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((drover-command-shell "/bin/sh"))
      (let ((result (drover-command-test-run (list (expand-file-name "missing" home)))))
        (should (equal (plist-get result :exit) 127))
        (should (string-match-p "missing" (plist-get result :errors))))
      (let ((result (drover-command-test-run (list "/bin/echo" "hi") (expand-file-name "gone/" home))))
        (should (equal (plist-get result :exit) 126))
        (should (string-match-p "gone" (plist-get result :errors)))
        (should (equal (plist-get result :output) ""))))
    (let ((drover-command-shell nil))
      (should-error (drover-command-run "test" (list (expand-file-name "missing" home)) #'ignore))
      (should (zerop (hash-table-count drover-command--jobs))))))

(ert-deftest drover-command-test-bounded ()
  "Output is read up to `drover-command-output-limit' bytes."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((drover-command-output-limit 100))
      (drover-command-test-both
        (let ((result (drover-command-test-run
                       (list "/bin/sh" "-c" "i=0; while [ $i -lt 100 ]; do echo 0123456789; i=$((i+1)); done"))))
          (should (equal (plist-get result :exit) 0))
          (should (<= (length (plist-get result :output)) 101))
          (should (plist-get result :truncated)))))))

(ert-deftest drover-command-test-never-waits ()
  "Commands sleeping two seconds never make Emacs wait, and run side by side.
Not even where starting a process takes a second, as on a managed Mac:
the runner, started once, starts them."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((script (drover-command-test-script (expand-file-name "slow" home) "sleep 2\necho \"$1\"\n"))
          (answers nil)
          (start nil))
      (drover-test-with-slow-starts
        ;; The runner's start takes its second, untimed.
        (drover-command-test-run (list "/bin/echo" "ready"))
        (let ((lag (drover-test-lag
                    (lambda ()
                      (setq start (float-time))
                      (dotimes (i 3)
                        (drover-command-run "test" (list script (number-to-string i))
                                            (lambda (result) (push (plist-get result :output) answers))))
                      (should (drover-test-wait-commands))))))
          (should (< lag 0.05))))
      (should (< (- (float-time) start) 3.5))
      (should (equal (sort answers #'string<) '("0\n" "1\n" "2\n"))))))

(ert-deftest drover-command-test-timeout ()
  "A command past `drover-command-timeout' is stopped, and its caller told."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((script (drover-command-test-script (expand-file-name "stuck" home)
                                              (format "sleep 3\ntouch %s\n"
                                                      (shell-quote-argument
                                                       (expand-file-name "finished" home)))))
          (drover-command-timeout 0.5))
      (drover-command-test-both
        (let ((result (drover-command-test-run (list script))))
          (should (string-match-p "no answer in 0.5 seconds" (plist-get result :failure))))))
    (sleep-for 3.5)
    (should-not (file-exists-p (expand-file-name "finished" home)))))

(ert-deftest drover-command-test-cancel ()
  "A command cancelled is stopped, and its callback never called."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((script (drover-command-test-script (expand-file-name "slow" home)
                                              (format "sleep 1\ntouch %s\n"
                                                      (shell-quote-argument
                                                       (expand-file-name "finished" home)))))
          (called nil))
      (drover-command-test-both
        (let ((id (drover-command-run "test" (list script) (lambda (_) (setq called t)))))
          ;; Before the runner told the process, and after.
          (drover-command-cancel id)
          (should-not (drover-command-live-p id))
          (let ((id (drover-command-run "test" (list script) (lambda (_) (setq called t)))))
            (accept-process-output nil 0.2)
            (drover-command-cancel id))
          (sleep-for 1.5)
          (should-not called)
          (should-not (file-exists-p (expand-file-name "finished" home))))))))

(ert-deftest drover-command-test-runner-ends ()
  "A runner ending fails its commands; the next command starts another."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((result nil)
          (drover-command-shell "/bin/sh"))
      (drover-command-run "test" (list "/bin/sleep" "5") (lambda (answer) (setq result answer)))
      (let ((runner drover-command--runner))
        (should (process-live-p runner))
        (delete-process runner)
        (should (equal result '(:failure "the shell running commands ended")))
        (should (zerop (hash-table-count drover-command--jobs)))
        (should-not (file-exists-p (process-get runner 'drover-command-directory))))
      (should (equal (plist-get (drover-command-test-run (list "/bin/echo" "again")) :output) "again\n")))))

(ert-deftest drover-command-test-environment ()
  "A command runs in the environment of its time: a runner of another ends after its commands."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((drover-command-shell "/bin/sh")
          (first nil)
          (process-environment (cons "DROVER_TEST=one" process-environment)))
      (drover-command-run "test" (list "/bin/sh" "-c" "sleep 0.5; echo $DROVER_TEST")
                          (lambda (answer) (setq first answer)))
      (let ((old drover-command--runner)
            (process-environment (cons "DROVER_TEST=two" process-environment)))
        (should (equal (plist-get (drover-command-test-run (list "/bin/sh" "-c" "echo $DROVER_TEST"))
                                  :output)
                       "two\n"))
        (should-not (eq old drover-command--runner))
        (should (equal (plist-get first :output) "one\n"))
        (let ((deadline (+ (float-time) 5)))
          (while (and (process-live-p old) (< (float-time) deadline))
            (accept-process-output nil 0.02)))
        (should-not (process-live-p old))))))

(ert-deftest drover-command-test-callback-errors ()
  "A callback's error is reported briefly, as out of any command."
  (skip-unless (file-executable-p "/bin/sh"))
  (drover-command-test-with
    (let ((drover-command-shell "/bin/sh")
          (messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) messages))))
        (drover-command-run "failing test" (list "/bin/echo") (lambda (_) (error "Broken")))
        (should (drover-test-wait-commands)))
      (should (equal messages '("Drover failing test: error: Broken"))))))

(provide 'drover-command-test)
;;; drover-command-test.el ends here
