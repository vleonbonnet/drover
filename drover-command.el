;;; drover-command.el --- Commands run without Emacs waiting  -*- lexical-binding: t; -*-

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

;; Backends run CLIs now and then, from timers as from commands, such
;; as "claude agents --json" while a list shows.  A process Emacs starts
;; costs its main thread the whole start: on macOS, `posix_spawn'
;; returns once the program was executed, and the security agents of a
;; managed machine may hold an execution for seconds, the longer the
;; larger the program (Claude Code's is hundreds of megabytes).  Emacs
;; waits as long, however asynchronous the process is afterwards.
;;
;; So drover starts one shell, the runner, once, and has it start the
;; commands: Emacs writes a line to it and reads a line back as each
;; command starts and ends, never waiting for either.  Each command runs
;; in the background of the runner, its output and errors in files of a
;; private directory, of which drover reads a bounded prefix; a command
;; that does not end within `drover-command-timeout' seconds is stopped,
;; and its caller told.  The runner keeps the environment it started
;; with: once `process-environment' changes, the next command starts a
;; new runner, the old one ending after its commands.
;;
;; Where no POSIX shell is found (`drover-command-shell'), commands are
;; processes of Emacs's own, as they used to be.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

(defcustom drover-command-shell
  (and (not (memq system-type '(windows-nt ms-dos))) "/bin/sh")
  "The POSIX shell starting drover's commands, or nil to start them directly.
Started once, it starts every command drover runs, so that Emacs never
waits for a program to start.  Nil, or a shell not found, starts each
command as a process of Emacs's own."
  :type '(choice (const :tag "Start commands directly" nil) file)
  :group 'drover)

(defcustom drover-command-timeout 60
  "Seconds a command drover runs may take before it is stopped."
  :type 'number
  :group 'drover)

(defconst drover-command-output-limit (* 1024 1024)
  "Bytes of a command's output, and of its errors, drover reads at most.")

(defconst drover-command--script
  "dir=$1
drover_run() {
  id=$1; where=$2; shift 2
  (
    if cd \"$where\" 2>\"$dir/$id.err\"; then
      \"$@\" </dev/null >\"$dir/$id.out\" 2>\"$dir/$id.err\" &
      echo \"$id pid $!\"
      wait $!
      echo \"$id exit $?\"
    else
      echo \"$id exit 126\"
    fi
  ) </dev/null &
}
while IFS= read -r line; do
  eval \"$line\"
  jobs >/dev/null 2>&1
done
wait"
  "The runner's script: each line it reads runs `drover_run' on a command.
It answers with a line ID pid PID as the command starts, and a line ID
exit STATUS as it ends, each one write, so that the lines of commands
ending together never mix; a directory it cannot enter is status 126,
its error the command's.  \"jobs\" reaps the commands ended, which some
shells do not do on their own; at the end of its input, the runner
waits for the commands it started.")

(defvar drover-command--runner nil
  "The runner starting commands now, a process, or nil before the first.")

(defvar drover-command--jobs (make-hash-table)
  "The commands running, by identifier, each a plist.
Its members are :name, what it is called in reports; :callback; :timer,
stopping it at its time; :process, its runner, or itself when started
directly; and :pid, once the runner told it.")

(defvar drover-command--serial 0
  "The identifier of the latest command.")

;;;; Running

(defun drover-command-run (name command callback &optional directory)
  "Run COMMAND, a list of strings, in DIRECTORY, and return at once.
NAME, a short string such as \"claude agents\", names it in reports.
CALLBACK is called once, later, with a plist: (:exit STATUS :output
STRING :errors STRING) once the command ran, its exit status, and up to
`drover-command-output-limit' bytes of its output and errors, as UTF-8,
:truncated non-nil when either was cut; else (:failure MESSAGE), when
it could not run, or ran out of `drover-command-timeout'.  DIRECTORY
defaults to the home directory.  Return the command's identifier, for
`drover-command-live-p' and `drover-command-cancel'.  Starting the
runner may signal an error."
  (let* ((directory (expand-file-name (or directory "~/")))
         (id (cl-incf drover-command--serial))
         (runner (and (not (seq-some (lambda (argument) (string-match-p "[\n\r\0]" argument))
                                     (cons directory command)))
                      (not (file-remote-p directory))
                      (drover-command--current-runner))))
    (puthash id (list :name name :callback callback) drover-command--jobs)
    (condition-case err
        (if runner
            (progn
              (drover-command--put id :process runner)
              (process-send-string
               runner (concat (mapconcat #'shell-quote-argument
                                         (append (list "drover_run" (number-to-string id) directory)
                                                 command)
                                         " ")
                              "\n")))
          (drover-command--put id :direct t)
          (drover-command--put id :process (drover-command--start-directly id command directory)))
      (error (remhash id drover-command--jobs)
             (signal (car err) (cdr err))))
    (drover-command--put id :timer (run-at-time drover-command-timeout nil
                                                #'drover-command--time-out id))
    id))

(defun drover-command-live-p (id)
  "Return non-nil while the command ID of `drover-command-run' runs."
  (and id (gethash id drover-command--jobs) t))

(defun drover-command-cancel (id)
  "Stop the command ID of `drover-command-run', if it runs, unannounced."
  (when-let* ((job (and id (gethash id drover-command--jobs))))
    (drover-command--stop job)
    (drover-command--forget id)))

(defun drover-command-stop ()
  "Stop the runner, telling the commands it runs that they failed."
  (when (process-live-p drover-command--runner)
    (delete-process drover-command--runner))
  (setq drover-command--runner nil))

(defun drover-command--put (id property value)
  "Set PROPERTY of the command ID to VALUE."
  (when-let* ((job (gethash id drover-command--jobs)))
    (puthash id (plist-put job property value) drover-command--jobs)))

(defun drover-command--stop (job)
  "Stop the command of JOB, a plist of `drover-command--jobs'.
A command the runner did not tell the process of yet is stopped once it
does (see `drover-command--filter')."
  (let ((process (plist-get job :process)))
    (cond ((plist-get job :pid)
           (ignore-errors (signal-process (plist-get job :pid) 'SIGTERM)))
          ((and (plist-get job :direct) (process-live-p process))
           (delete-process process)))))

(defun drover-command--forget (id)
  "Forget the command ID, returning its plist."
  (when-let* ((job (gethash id drover-command--jobs)))
    (remhash id drover-command--jobs)
    (when (timerp (plist-get job :timer))
      (cancel-timer (plist-get job :timer)))
    job))

(defun drover-command--finish (id result)
  "End the command ID with RESULT, given to its callback."
  (when-let* ((job (drover-command--forget id)))
    (drover-with-reporting (plist-get job :name)
      (funcall (plist-get job :callback) result))))

(defun drover-command--time-out (id)
  "Stop the command ID, which ran out of time, and tell its caller."
  (drover-with-reporting "commands"
    (when-let* ((job (gethash id drover-command--jobs)))
      (drover-command--stop job)
      (drover-command--finish id (list :failure (format "no answer in %s seconds"
                                                        drover-command-timeout))))))

;;;; The runner

(defun drover-command--current-runner ()
  "Return the runner starting commands, started anew if needed, or nil.
Nil without a shell (`drover-command-shell').  A runner started with
another shell or environment than `process-environment' is told to
end, once its commands ended, and another started."
  (when (and drover-command-shell (file-executable-p drover-command-shell))
    (let ((runner drover-command--runner))
      (when (and (process-live-p runner)
                 (not (and (equal (process-get runner 'drover-command-shell) drover-command-shell)
                           (equal (process-get runner 'drover-command-environment)
                                  process-environment))))
        (ignore-errors (process-send-eof runner))
        (setq runner nil))
      (if (process-live-p runner)
          runner
        (setq drover-command--runner (drover-command--start-runner))))))

(defun drover-command--start-runner ()
  "Start a runner, and return it."
  (let* ((directory (make-temp-file "drover-commands-" t))
         (default-directory (file-name-as-directory directory))
         (process
          (condition-case err
              (make-process
               :name "drover-commands"
               :command (list drover-command-shell "-c" drover-command--script "drover-commands"
                              directory)
               :connection-type 'pipe
               :coding 'utf-8-unix
               :noquery t
               :filter #'drover-command--filter
               :sentinel #'drover-command--sentinel)
            (error (delete-directory directory t)
                   (signal (car err) (cdr err))))))
    (set-file-modes directory #o700)
    (process-put process 'drover-command-directory directory)
    (process-put process 'drover-command-shell drover-command-shell)
    (process-put process 'drover-command-environment (copy-sequence process-environment))
    (add-hook 'kill-emacs-hook #'drover-command--clean-up)
    process))

(defun drover-command--clean-up ()
  "Delete the directories of the runners, as Emacs exits without their sentinels.
An error is reported, and lets Emacs exit."
  (drover-with-reporting "commands"
    (dolist (process (process-list))
      (when-let* ((directory (process-get process 'drover-command-directory)))
        (ignore-errors (delete-directory directory t))))))

(defun drover-command--filter (runner output)
  "Follow the lines OUTPUT continues, of RUNNER, as commands start and end."
  (drover-with-reporting "commands"
    (let* ((text (concat (or (process-get runner 'drover-command-pending) "") output))
           (lines (split-string text "\n"))
           (rest (car (last lines))))
      ;; The runner writes short lines only: anything else is not kept.
      (process-put runner 'drover-command-pending (and (< (length rest) 1024) rest))
      (dolist (line (butlast lines))
        (when (string-match "\\`\\([0-9]+\\) \\(pid\\|exit\\) \\([0-9]+\\)\\'" line)
          (let ((id (string-to-number (match-string 1 line)))
                (number (string-to-number (match-string 3 line))))
            (if (equal (match-string 2 line) "pid")
                (if (gethash id drover-command--jobs)
                    (drover-command--put id :pid number)
                  ;; Cancelled or timed out before it started.
                  (ignore-errors (signal-process number 'SIGTERM)))
              (let ((result (and (gethash id drover-command--jobs)
                                 (drover-command--read-result runner id number))))
                (drover-command--delete-files runner id)
                (when result
                  (drover-command--finish id result))))))))))

(defun drover-command--sentinel (runner _event)
  "Tell the commands of RUNNER, which ended, that they failed."
  (drover-with-reporting "commands"
    (unless (process-live-p runner)
      (let ((ids nil))
        (maphash (lambda (id job) (when (eq (plist-get job :process) runner) (push id ids)))
                 drover-command--jobs)
        (dolist (id (sort ids #'<))
          (drover-command--finish id (list :failure "the shell running commands ended"))))
      (when (eq runner drover-command--runner)
        (setq drover-command--runner nil))
      (when-let* ((directory (process-get runner 'drover-command-directory)))
        (ignore-errors (delete-directory directory t))))))

(defun drover-command--files (runner id)
  "Return the files of output and errors of the command ID of RUNNER."
  (let ((directory (process-get runner 'drover-command-directory)))
    (list (expand-file-name (format "%d.out" id) directory)
          (expand-file-name (format "%d.err" id) directory))))

(defun drover-command--delete-files (runner id)
  "Delete the files of the command ID of RUNNER."
  (dolist (file (drover-command--files runner id))
    (ignore-errors (delete-file file))))

(defun drover-command--read (file)
  "Return the start of FILE, as (TEXT . TRUNCATED), \"\" when unreadable."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (ignore-error file-error
        (insert-file-contents file nil 0 drover-command-output-limit))
      (cons (buffer-string)
            (> (or (file-attribute-size (file-attributes file)) 0) drover-command-output-limit)))))

(defun drover-command--read-result (runner id status)
  "Return the result of the command ID of RUNNER, which ended with STATUS."
  (pcase-let ((`(,out ,err) (drover-command--files runner id)))
    (let ((output (drover-command--read out))
          (errors (drover-command--read err)))
      (list :exit status :output (car output) :errors (car errors)
            :truncated (or (cdr output) (cdr errors))))))

;;;; Without a runner

(defun drover-command--start-directly (id command directory)
  "Start COMMAND in DIRECTORY as the command ID, a process of Emacs's own.
The command ends once both it and the pipe of its errors ended, so that
it is told all it wrote."
  (let* ((output (generate-new-buffer " *drover command*" t))
         (errors (generate-new-buffer " *drover command errors*" t))
         (default-directory (file-name-as-directory directory))
         (limit drover-command-output-limit)
         (process nil)
         (stderr nil)
         (filter (lambda (process text)
                   (drover-with-reporting "commands"
                     (when (buffer-live-p (process-buffer process))
                       (with-current-buffer (process-buffer process)
                         (if (< (buffer-size) limit)
                             (insert text)
                           (process-put process 'drover-command-truncated t)))))))
         (sentinel
          (lambda (_process _event)
            (drover-with-reporting "commands"
              (when (and process (not (process-live-p process))
                         stderr (not (process-live-p stderr)))
                (unwind-protect
                    (drover-command--finish
                     id
                     (if (eq (process-status process) 'exit)
                         (list :exit (process-exit-status process)
                               :output (drover-command--buffer-text output limit)
                               :errors (drover-command--buffer-text errors limit)
                               :truncated (or (process-get process 'drover-command-truncated)
                                              (process-get stderr 'drover-command-truncated)))
                       (list :failure (format "%s: %s" (file-name-nondirectory (car command))
                                              (process-status process)))))
                  (when (buffer-live-p output) (kill-buffer output))
                  (when (buffer-live-p errors) (kill-buffer errors))))))))
    (condition-case err
        (progn
          (setq stderr (make-pipe-process :name "drover-command-errors" :buffer errors
                                          :noquery t :coding 'utf-8 :filter filter
                                          :sentinel sentinel))
          (setq process (make-process :name "drover-command" :buffer output :stderr stderr
                                      :command command :connection-type 'pipe :coding 'utf-8
                                      :noquery t :filter filter :sentinel sentinel)))
      (error (when (process-live-p stderr) (delete-process stderr))
             (kill-buffer output)
             (kill-buffer errors)
             (signal (car err) (cdr err))))))

(defun drover-command--buffer-text (buffer limit)
  "Return the first LIMIT characters of BUFFER, or \"\" when it was killed."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (min (point-max) (1+ limit))))
    ""))

(provide 'drover-command)
;;; drover-command.el ends here
