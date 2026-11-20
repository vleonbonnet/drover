;;; drover-headless-test.el --- Tests of drover on a host nobody sits at  -*- lexical-binding: t; -*-

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

;; The health check and the entry points for scripts, on the queue and
;; fake backend of drover-schedule-test.el.  One test starts a second
;; batch Emacs serving a socket of a temporary directory, and asks it
;; for its health as emacsclient would, through `server-eval-at'; the
;; script etc/drover-health-check runs against stub clients.  Nothing
;; talks to the user's Emacs.

;;; Code:

(require 'drover-schedule-test)
(require 'drover-headless)
(require 'server)

(defmacro drover-headless-test-with (&rest body)
  "Run BODY in the queue's test state, the health checks not reaching OpenCode.
The heartbeat of timers is BODY's own, stopped after."
  (declare (indent 0) (debug t))
  `(drover-schedule-test-state
     (let ((drover-health-checks '(emacs queue timers))
           (drover-health--beat nil))
       (unwind-protect (progn ,@body)
         (when drover-health--beat
           (cancel-timer (car drover-health--beat)))))))

;;;; Health

(ert-deftest drover-headless-test-health-ok ()
  "A host in order answers ok on one line, each check told, and arms its heartbeat once."
  (drover-headless-test-with
    (drover-schedule-test-job)
    (let ((line (drover-health)))
      (should (string-match-p "\\`ok: [0-9.]+ ms; emacs answers; queue 1 pending; timers heartbeat armed\\'" line)))
    (let ((timer (car drover-health--beat)))
      (should (memq timer timer-list))
      (should (string-match-p "; timers ticked 0s ago\\'" (drover-health)))
      (should (eq (car drover-health--beat) timer)))
    (let ((report (drover-health-report)))
      (should (eq (plist-get report :status) 'ok))
      (should (equal (mapcar #'car (plist-get report :checks)) '(emacs queue timers)))
      (should (< (plist-get report :elapsed) 50)))))

(ert-deftest drover-headless-test-health-queue ()
  "A queue not restored is degraded, one that does not read failed, none at all ok."
  (drover-headless-test-with
    (let ((drover-health-checks '(queue)))
      (setq drover-schedule--queue nil)
      (should (equal (drover-health--check-queue) '(ok . "no prompts scheduled")))
      (let ((drover-schedule--queue (drover-schedule--queue-make :loaded t)))
        (drover-schedule-test-job)
        (drover-schedule-add-recurring (drover-schedule-test-target) '(:interval 3600) "hourly"))
      (should (equal (drover-health--check-queue)
                     '(degraded . "2 prompts not restored: call drover-schedule-restore")))
      (should (string-prefix-p "degraded: " (drover-health)))
      (drover-schedule-test-reload)
      (should (equal (drover-health--check-queue) '(ok . "2 pending, 1 recurring")))
      (drover-test-write (drover-schedule-file) "(:version 2 :jobs ((:id \"bad\")))")
      (setq drover-schedule--queue nil)
      (should (string-match-p "\\`failed: [0-9.]+ ms; queue failed unreadable: Invalid queue" (drover-health))))))

(ert-deftest drover-headless-test-health-late-prompts ()
  "A prompt due long ago that no timer fires fails the check; one waiting its turn does not."
  (drover-headless-test-with
    (let* ((drover-health-checks '(timers))
           (job (drover-schedule-test-job))
           (id (plist-get job :id)))
      (drover-health)
      (setf (plist-get job :due) (- (float-time) 600))
      ;; Its timer, armed for an hour from now, stands for one re-armed: not late.
      (should (eq (car (drover-health--check-timers)) 'ok))
      (cancel-timer (gethash id (drover-schedule--timers)))
      (should (equal (drover-health--check-timers) '(failed . "1 scheduled prompt late, the oldest by 10 minutes")))
      (remhash id (drover-schedule--timers))
      (should (eq (car (drover-health--check-timers)) 'failed))
      ;; Being delivered, it is not late.
      (puthash id (list :deadline (+ (float-time) 600)) (drover-schedule--active))
      (should (eq (car (drover-health--check-timers)) 'ok))
      (remhash id (drover-schedule--active))
      ;; Within the grace, not yet.
      (setf (plist-get job :due) (- (float-time) 60))
      (should (eq (car (drover-health--check-timers)) 'ok)))))

(ert-deftest drover-headless-test-health-heartbeat ()
  "A heartbeat that stopped ticking fails the check."
  (drover-headless-test-with
    (let ((drover-health-heartbeat 30))
      (should (equal (drover-health--check-timers) '(ok . "heartbeat armed")))
      (setcdr drover-health--beat (- (float-time) 200))
      (should (equal (drover-health--check-timers) '(failed . "the heartbeat timer last ran 3 minutes ago")))
      (drover-health--tick)
      (should (eq (car (drover-health--check-timers)) 'ok))
      ;; A heartbeat cancelled is armed anew.
      (cancel-timer (car drover-health--beat))
      (should (equal (drover-health--check-timers) '(ok . "heartbeat armed"))))))

(ert-deftest drover-headless-test-health-emacs ()
  "A question nobody answers, or a recursive edit, fails Emacs's check, the question named."
  (cl-letf (((symbol-function 'minibuffer-depth) (lambda () 1))
            ((symbol-function 'minibuffer-prompt) (lambda () "Delete the session? ")))
    (should (equal (drover-health--check-emacs)
                   '(failed . "waits on a question in the minibuffer: Delete the session?"))))
  (cl-letf (((symbol-function 'recursion-depth) (lambda () 2)))
    (should (equal (drover-health--check-emacs)
                   '(failed . "in a recursive edit, depth 2, such as the debugger"))))
  (should (equal (drover-health--check-emacs) '(ok . "answers"))))

(ert-deftest drover-headless-test-health-opencode-and-peers ()
  "OpenCode's service not running, or a peer required away, is degraded, not failed."
  (require 'drover-cheap)
  (cl-letf (((symbol-function 'drover-cheap-service-running-p) (lambda () t))
            ((symbol-function 'opencode-service-read-registration) (lambda () '(:pid 4242))))
    (should (equal (drover-health--check-opencode) '(ok . "running, pid 4242"))))
  (cl-letf (((symbol-function 'drover-cheap-service-running-p) #'ignore))
    (should (equal (drover-health--check-opencode) '(degraded . "its service does not run"))))
  (let ((drover-hosts-function (lambda () '("reaver" "core")))
        (drover-host-state-function (lambda (host) (and (equal host "core") "reconnecting, attempt 3")))
        (drover-health-peers '("reaver" "core" "adept")))
    (should (equal (drover-health--check-peers)
                   '(degraded . "core reconnecting, attempt 3; adept not configured")))
    (let ((drover-health-peers '("reaver")))
      (should (equal (drover-health--check-peers) '(ok . "1 connected"))))))

(ert-deftest drover-headless-test-health-never-signals ()
  "A check signalling fails with its error on the line; the worst status wins, those not ok first."
  (drover-headless-test-with
    (cl-letf (((symbol-function 'drover-health--check-queue) (lambda () (error "Disk gone")))
              ((symbol-function 'drover-health--check-emacs) (lambda () '(degraded . "slow"))))
      (should (string-match-p "\\`failed: [0-9.]+ ms; emacs degraded slow; queue failed Disk gone; timers heartbeat armed\\'"
                              (drover-health))))
    (let ((drover-health-checks '(nonesuch)))
      (should (string-match-p "nonesuch failed no such check" (drover-health))))
    (cl-letf (((symbol-function 'drover-health-report) (lambda () (error "Broken"))))
      (should (equal (drover-health) "failed: Broken")))))

;;;; Entry points for scripts

(ert-deftest drover-headless-test-external-schedule ()
  "A script schedules by link or key, once or recurring, hidden by default; failures are one line."
  (drover-headless-test-with
    (drover-schedule-test-session)
    (drover-schedule-test-buffer)
    (let ((line (drover-external-schedule "agent-session:claude-code/fixture-session" "every day 7:00" "brief"
                                          :by "nomad brief")))
      (should (string-match-p "\\`scheduled [0-9A-F-]+ for .+, every day at 07:00, next " line))
      (let ((job (car (drover-schedule--jobs))))
        (should (equal (plist-get job :by) "nomad brief"))
        (should (eq (plist-get job :hidden) t))
        (should (equal (plist-get job :every) '(:days (0 1 2 3 4 5 6) :at (7 . 0))))))
    (drover-external-schedule "fake/fixture" "in 2h" "check" :hidden nil)
    (let ((job (car (drover-schedule--jobs))))
      (should-not (plist-get job :hidden))
      (should (equal (plist-get job :by) "script")))
    (dolist (case '(("fake/nope" "in 1h" "No session here is named fake/nope")
                    ("fake/fixture" "whenever" "cannot read “whenever” as a time")
                    ("fake/fixture" "every day 7:00 Mars/Olympus_Mons" "no time zone Mars/Olympus_Mons here")))
      (should (equal (cadr (should-error (drover-external-schedule (nth 0 case) (nth 1 case) "x") :type 'user-error))
                     (nth 2 case))))
    ;; Asking nothing: a question signals, told on a line.
    (drover-schedule-define-trigger 'asking "asking event" :request (lambda () (y-or-n-p "Really? ") "k"))
    (should (string-match-p "nobody is there to answer"
                            (cadr (should-error (drover-external-schedule "fake/fixture" "after asking event" "x")
                                                :type 'user-error))))
    (should (length= (drover-schedule--jobs) 2))))

(ert-deftest drover-headless-test-external-fire-and-list ()
  "A script fires an event by name, nothing waiting being no error, and lists what is scheduled."
  (drover-headless-test-with
    (drover-schedule-define-trigger 'backup "nightly backup")
    (drover-schedule-add-after-trigger (drover-schedule-test-target) 'backup "check the backup")
    (drover-schedule-add-after-trigger (drover-schedule-test-target) 'backup "and the restore")
    (drover-schedule-test-job)
    (let ((listed (split-string (drover-external-scheduled) "\n")))
      (should (length= listed 3))
      (should (string-match-p "\tpending\t[-0-9]+ [0-9:]+ .*\tScheduler fixture: check\\'" (car listed)))
      (should (string-match-p "\tafter the next nightly backup\t" (cadr listed))))
    (should (equal (drover-external-fire "backup" :text "done at 3:00") "released 2 prompts waiting for backup"))
    (should (equal (drover-external-fire "backup") "released 0 prompts waiting for backup"))
    (should (equal (drover-external-fire "nonesuch") "released 0 prompts: none waits for nonesuch"))
    (drover-schedule-add-after-trigger (drover-schedule-test-target) 'backup "never")
    (should (equal (drover-external-fire "backup" :cancelled "skipped") "cancelled 1 prompt waiting for backup"))
    (should (length= (split-string (drover-external-scheduled t) "\n") 4))
    (dolist (job (drover-schedule--jobs))
      (drover-schedule-cancel-if-pending job))
    (should (equal (drover-external-scheduled) "no prompts scheduled"))))

(defun drover-schedule-cancel-if-pending (job)
  "Cancel JOB unless it went or ended already."
  (when (memq (plist-get job :status) '(pending opening waiting))
    (drover-schedule-cancel job)))

;;;; Through a server, as emacsclient

(defun drover-headless-test--server (socket state)
  "Start a batch Emacs serving SOCKET, drover loaded, its state in STATE; return its process."
  (make-process
   :name "drover-headless-server"
   :buffer (generate-new-buffer " *drover headless server*")
   :noquery t
   :connection-type 'pipe
   :command (append (list (expand-file-name invocation-name invocation-directory) "-Q" "--batch")
                    (mapcan (lambda (directory) (list "-L" directory)) load-path)
                    (list "--eval"
                          (format "%S"
                                  `(progn
                                     (setq drover-state-directory ,state
                                           drover-health-checks '(emacs queue timers)
                                           server-name ,socket)
                                     (require 'drover-headless)
                                     (server-start)
                                     (while t (accept-process-output nil 0.5))))))))

(ert-deftest drover-headless-test-round-trip ()
  "An Emacs with no graphical terminal answers its health through its server, and makes no frame."
  (skip-unless (featurep 'make-network-process '(:family local)))
  (let* ((directory (make-temp-file "dh" t))
         (socket (expand-file-name "s" directory))
         (process (drover-headless-test--server socket (file-name-as-directory (expand-file-name "state" directory))))
         (deadline (+ (float-time) 30))
         answer)
    (unwind-protect
        (progn
          (while (and (not answer) (< (float-time) deadline) (process-live-p process))
            (if (file-exists-p socket)
                (setq answer (ignore-errors (server-eval-at socket '(drover-health))))
              (accept-process-output process 0.1)))
          (should (stringp answer))
          (should (string-match-p "\\`ok: [0-9.]+ ms; emacs answers; queue no prompts scheduled; timers " answer))
          (should (equal (server-eval-at socket '(list (display-graphic-p) (drover-schedule-graphic-terminal)
                                                       (drover-schedule-display-default nil "Brief")
                                                       (length (frame-list))))
                         '(nil nil hidden 1)))
          ;; Its heartbeat armed by the first call, the second tells its age.
          (should (string-match-p "; timers ticked [0-9]+s ago\\'" (server-eval-at socket '(drover-health)))))
      (delete-process process)
      (kill-buffer (process-buffer process))
      (delete-directory directory t))))

;;;; The script of the check

(defun drover-headless-test--check (client &rest arguments)
  "Run etc/drover-health-check with CLIENT, a stub emacsclient script, and ARGUMENTS.
Return (STATUS OUTPUT ARGS), ARGS what the stub was given, a line each."
  (let* ((directory (make-temp-file "drover-check" t))
         (args (expand-file-name "args" directory))
         (stub (drover-test-program (expand-file-name "emacsclient" directory)
                                    (concat "for a in \"$@\"; do printf '%s\\n' \"$a\"; done > '" args "'\n" client)))
         (script (expand-file-name "etc/drover-health-check"
                                   (file-name-parent-directory
                                    (file-name-parent-directory (locate-library "drover-headless-test"))))))
    (unwind-protect
        (with-temp-buffer
          (let* ((process-environment (cons (concat "EMACSCLIENT=" stub) process-environment))
                 (status (apply #'call-process "sh" nil t nil script arguments)))
            (list status (string-trim (buffer-string))
                  (and (file-exists-p args)
                       (with-temp-buffer (insert-file-contents args) (split-string (buffer-string) "\n" t))))))
      (delete-directory directory t))))

(ert-deftest drover-headless-test-check-script ()
  "The script maps Emacs's answer to a script check's status, and fails when Emacs does not answer in time."
  (skip-unless (and (executable-find "sh") (not (memq system-type '(windows-nt ms-dos)))))
  (pcase-let ((`(,status ,output ,args) (drover-headless-test--check "echo '\"ok: 0.4 ms; emacs answers\"'"
                                                                     "-s" "/run/emacs/adun" "-t" "3")))
    (should (= status 0))
    (should (equal output "\"ok: 0.4 ms; emacs answers\""))
    (should (equal (seq-take args 3) '("-s" "/run/emacs/adun" "--timeout=3")))
    (should (equal (nth 3 args) "--eval"))
    (should (string-match-p "(drover-health)" (nth 4 args))))
  (should (= 1 (car (drover-headless-test--check "echo '\"degraded: 0.4 ms; opencode degraded its service does not run\"'"))))
  (should (= 2 (car (drover-headless-test--check "echo '\"failed: 0.4 ms; timers failed 2 late\"'"))))
  (should (= 2 (car (drover-headless-test--check "echo '*ERROR*: Symbol’s function definition is void'; exit 1"))))
  (should (= 2 (car (drover-headless-test--check "echo 'no socket' >&2; exit 1"))))
  (should (= 2 (car (drover-headless-test--check "echo ok" "-t" "x"))))
  ;; Default socket: none given.
  (should (equal (car (nth 2 (drover-headless-test--check "echo '\"ok: 1 ms\"'"))) "--timeout=5"))
  (when (executable-find "timeout")
    (let ((start (float-time)))
      (should (= 2 (car (drover-headless-test--check "sleep 30" "-t" "1"))))
      (should (< (- (float-time) start) 10)))))

(provide 'drover-headless-test)
;;; drover-headless-test.el ends here
