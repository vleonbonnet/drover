;;; drover-headless.el --- Drover on a host nobody sits at  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, processes

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

;; Drover in an Emacs daemon of a host nobody sits at, such as a server
;; of the homelab: what a supervisor and scripts call through
;; emacsclient.
;;
;; `drover-health' answers at once, without waiting on anything, with
;; one line starting "ok:", "degraded:" or "failed:": Emacs waits on no
;; question nobody answers, the queue of scheduled prompts reads and was
;; restored, timers run, and the local OpenCode service runs.  The
;; script etc/drover-health-check calls it through emacsclient with a
;; timeout, and exits as a Nomad or Consul script check expects: 0, 1
;; for a warning, 2 when Emacs failed or did not answer.
;;
;; `drover-external-schedule', `drover-external-fire' and
;; `drover-external-scheduled' schedule a prompt for a session, tell
;; drover an event happened, and list the scheduled prompts, for a
;; script.  They show no window and ask nothing, a question signalling,
;; and fail with one line, which emacsclient prints, exiting 1.
;;
;; Scheduled prompts open no frame on such a host: the default
;; `drover-schedule-display-function' delivers them out of every window
;; when no terminal is graphical (see `drover-schedule').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-schedule)

(declare-function drover-cheap-service-running-p "drover-cheap" ())
(declare-function opencode-service-read-registration "opencode-service" ())

;;;; Options

(defgroup drover-headless nil
  "Drover on a host nobody sits at, through emacsclient."
  :group 'drover
  :prefix "drover-health-")

(defcustom drover-health-checks '(emacs queue timers opencode)
  "What `drover-health' checks, beside Emacs answering at all.

 `emacs'     Emacs waits on no question in the minibuffer, and is in
             no recursive edit, such as the debugger: on a daemon
             nobody answers either.
 `queue'     The queue of scheduled prompts reads, and was restored
             once its file exists (see `drover-schedule-restore').
 `timers'    Timers run: a heartbeat timer ticked lately, and no
             scheduled prompt is late past `drover-health-late'.
 `opencode'  This host's OpenCode service runs, as registered.
 `peers'     The hosts of `drover-health-peers' are connected."
  :type '(set (const emacs) (const queue) (const timers) (const opencode) (const peers))
  :group 'drover-headless)

(defcustom drover-health-peers nil
  "Peers the check `peers' of `drover-health' requires connected, by name."
  :type '(repeat string)
  :group 'drover-headless)

(defcustom drover-health-late 120
  "Seconds a scheduled prompt may stay due before its timer counts as stuck."
  :type 'natnum
  :group 'drover-headless)

(defcustom drover-health-heartbeat 30
  "Seconds between two ticks of the timer `drover-health' looks at.
It is armed by the first call; the check `timers' fails once it has
not ticked for three of these."
  :type 'natnum
  :group 'drover-headless)

;;;; Health

(defvar drover-health--beat nil
  "The heartbeat of timers, (TIMER . LAST), LAST when it last ticked, or nil.")

(defun drover-health--tick ()
  "Note that timers run, for the heartbeat of `drover-health'."
  (when drover-health--beat
    (setcdr drover-health--beat (float-time))))

(defun drover-health--heartbeat ()
  "Return seconds since the heartbeat last ticked, arming it first if need be.
Nil when it was just armed."
  (if (and drover-health--beat (memq (car drover-health--beat) timer-list))
      (- (float-time) (cdr drover-health--beat))
    (setq drover-health--beat
          (cons (run-at-time drover-health-heartbeat drover-health-heartbeat #'drover-health--tick)
                (float-time)))
    nil))

(defun drover-health--check-emacs ()
  "Return how Emacs stands, as (STATUS . LINE): no question, no recursive edit."
  (cond
   ((> (minibuffer-depth) 0)
    (cons 'failed (format "waits on a question in the minibuffer: %s"
                          (or (drover-bound-line (minibuffer-prompt) 120) "no prompt"))))
   ((> (recursion-depth) 0)
    (cons 'failed (format "in a recursive edit, depth %d, such as the debugger" (recursion-depth))))
   (t (cons 'ok "answers"))))

(defun drover-health--queue ()
  "Return the queue of scheduled prompts as it stands, read, or nil before."
  (and (drover-schedule--queue-p drover-schedule--queue)
       (drover-layout-current-p drover-schedule--queue 'drover-schedule--queue)
       (drover-schedule--queue-loaded drover-schedule--queue)
       drover-schedule--queue))

(defun drover-health--check-queue ()
  "Return how the queue of scheduled prompts stands, as (STATUS . LINE)."
  (if-let* ((queue (drover-health--queue)))
      (let* ((jobs (drover-schedule--queue-jobs queue))
             (pending (seq-count (lambda (job) (eq (plist-get job :status) 'pending)) jobs))
             (recurring (seq-count (lambda (job) (and (plist-get job :every) (eq (plist-get job :status) 'pending)))
                                   jobs)))
        (cons 'ok (format "%d pending%s" pending (if (> recurring 0) (format ", %d recurring" recurring) ""))))
    (let ((file (drover-schedule-file)))
      (if (not (file-exists-p file))
          (cons 'ok "no prompts scheduled")
        (condition-case err
            (let ((jobs (drover-schedule--read file)))
              (cons 'degraded (format "%d prompts not restored: call drover-schedule-restore" (length jobs))))
          (error (cons 'failed (format "unreadable: %s" (drover-error-message err)))))))))

(defun drover-health--late-jobs (queue now)
  "Return the prompts of QUEUE due before NOW whose timers do not fire them.
Due by more than `drover-health-late' seconds, pending, not being
delivered, and either no timer armed, or one past its time as long."
  (let ((timers (drover-schedule--queue-timers queue))
        (active (drover-schedule--queue-active queue)))
    (seq-filter (lambda (job)
                  (let ((due (plist-get job :due))
                        (timer (gethash (plist-get job :id) timers)))
                    (and (eq (plist-get job :status) 'pending)
                         (numberp due)
                         (< (+ due drover-health-late) now)
                         (not (gethash (plist-get job :id) active))
                         (or (null timer)
                             (not (memq timer timer-list))
                             (< (+ (float-time (timer--time timer)) drover-health-late) now)))))
                (drover-schedule--queue-jobs queue))))

(defun drover-health--check-timers ()
  "Return how timers stand, as (STATUS . LINE): they tick, no prompt is late."
  (let* ((now (float-time))
         (age (drover-health--heartbeat))
         (late (and-let* ((queue (drover-health--queue))) (drover-health--late-jobs queue now))))
    (cond
     ((and age (> age (* 3 drover-health-heartbeat)))
      (cons 'failed (format "the heartbeat timer last ran %s ago" (drover-schedule--span age))))
     (late
      (cons 'failed (format "%d scheduled prompt%s late, the oldest by %s"
                            (length late) (if (length= late 1) "" "s")
                            (drover-schedule--span (- now (seq-min (mapcar (lambda (job) (plist-get job :due))
                                                                           late)))))))
     (age (cons 'ok (format "ticked %ds ago" (round age))))
     (t (cons 'ok "heartbeat armed")))))

(defun drover-health--check-opencode ()
  "Return how this host's OpenCode service stands, as (STATUS . LINE)."
  (cond
   ((not (and (require 'drover-cheap nil t) (require 'opencode-service nil t)))
    (cons 'degraded "opencode.el is not installed"))
   ((drover-cheap-service-running-p)
    (cons 'ok (format "running, pid %s" (plist-get (opencode-service-read-registration) :pid))))
   (t (cons 'degraded "its service does not run"))))

(defun drover-health--check-peers ()
  "Return how the peers of `drover-health-peers' stand, as (STATUS . LINE)."
  (let* ((known (drover-hosts))
         (down (seq-keep (lambda (name)
                           (cond ((not (member name known)) (format "%s not configured" name))
                                 ((drover-host-state name)
                                  (format "%s %s" name (drover-host-state name)))))
                         drover-health-peers)))
    (if down
        (cons 'degraded (string-join down "; "))
      (cons 'ok (format "%d connected" (length drover-health-peers))))))

(defconst drover-health--functions
  '((emacs . drover-health--check-emacs)
    (queue . drover-health--check-queue)
    (timers . drover-health--check-timers)
    (opencode . drover-health--check-opencode)
    (peers . drover-health--check-peers))
  "The checks of `drover-health-checks', by name.")

(defun drover-health-report ()
  "Return how drover stands, a plist, without waiting on anything.
:status is `ok', `degraded' or `failed', the worst of the checks of
`drover-health-checks'; :checks a list of (NAME STATUS LINE); :elapsed
the milliseconds it took.  A check signalling fails, with its error."
  (let* ((start (float-time))
         (checks (mapcar (lambda (name)
                           (pcase-let ((`(,status . ,line)
                                        (condition-case err
                                            (drover-quietly
                                              (funcall (or (alist-get name drover-health--functions)
                                                           (lambda () (cons 'failed "no such check")))))
                                          (error (cons 'failed (drover-error-message err))))))
                             (list name status (or (drover-bound-line line 200) ""))))
                         drover-health-checks))
         (statuses (mapcar #'cadr checks)))
    (list :status (cond ((memq 'failed statuses) 'failed)
                        ((memq 'degraded statuses) 'degraded)
                        (t 'ok))
          :checks checks
          :elapsed (* 1000 (- (float-time) start)))))

;;;###autoload
(defun drover-health ()
  "Return how drover stands on this host, a line, at once.
It starts with \"ok:\", \"degraded:\" or \"failed:\", then the time it
took and each check of `drover-health-checks', those not ok first.
For a supervisor's check through emacsclient, such as the script
etc/drover-health-check: answering at all tells that Emacs reads its
input; it never signals, nor waits on anything."
  (condition-case err
      (let* ((report (drover-health-report))
             (checks (plist-get report :checks))
             (ordered (append (seq-remove (lambda (check) (eq (nth 1 check) 'ok)) checks)
                              (seq-filter (lambda (check) (eq (nth 1 check) 'ok)) checks))))
        (drover-bound-line
         (format "%s: %.1f ms%s" (plist-get report :status) (plist-get report :elapsed)
                 (mapconcat (lambda (check)
                              (format "; %s%s %s" (car check)
                                      (if (eq (nth 1 check) 'ok) "" (format " %s" (nth 1 check)))
                                      (nth 2 check)))
                            ordered ""))
         1000))
    (error (format "failed: %s" (drover-error-message err)))))

;;;; Entry points for scripts

(defun drover-external--call (function)
  "Call FUNCTION showing no window and asking nobody, for emacsclient.
An error becomes a `user-error' of one bounded line, which emacsclient
prints, exiting with a failure, rather than its data printed whole."
  (condition-case err
      (drover-quietly (funcall function))
    (error (user-error "%s" (or (drover-bound-line (drover-error-message err) 500) "failed")))))

;;;###autoload
(cl-defun drover-external-schedule (session when prompt &key argument (by "script") (hidden t))
  "Schedule PROMPT for SESSION when WHEN tells, for a script; return a line.
SESSION is an agent-session link to its conversation, or its key,
BACKEND/ID (see `drover-schedule-session-named').  WHEN is read as the
user's scheduling reads it: \"in 90m\", \"tomorrow 7:00\", \"every day
7:00\", \"every weekday 9am Europe/Paris\", \"every 30m\", \"when turn
ends\", \"after LABEL\" (see `drover-schedule-when').  ARGUMENT is what
an event taking one concerns, BY names who scheduled it, as lists show
it, and HIDDEN, t by default, delivers it out of every window.  For
emacsclient: nothing shows, nothing asks, a failure is one line."
  (drover-external--call
   (lambda ()
     (let ((job (drover-schedule-for-session (drover-schedule-session-named session) when prompt
                                             (append (and hidden (list :hidden t))
                                                     (and (stringp by) (list :by (drover-bound-line by 100))))
                                             argument)))
       (format "scheduled %s for %s, %s" (plist-get job :id) (plist-get job :title)
               (drover-schedule--when job))))))

;;;###autoload
(cl-defun drover-external-fire (event &key text cancelled argument)
  "Tell drover the event EVENT, a name, happened, for a script; return a line.
The prompts waiting for it are sent, TEXT, what happened, following
each; with CANCELLED, it will not happen, and they are cancelled;
ARGUMENT releases only those waiting for it on that (see
`drover-schedule-fire-event').  An event nothing waits for releases
nothing, and is no error.  For emacsclient: nothing shows, nothing
asks, a failure is one line."
  (drover-external--call
   (lambda ()
     (drover-schedule-load)
     (if-let* ((trigger (drover-schedule-trigger-named event)))
         (let ((count (drover-schedule-fire-event trigger cancelled text argument)))
           (format "%s %d prompt%s waiting for %s" (if cancelled "cancelled" "released")
                   count (if (= count 1) "" "s") trigger))
       (format "released 0 prompts: none waits for %s" (drover-bound-line (format "%s" event) 100))))))

;;;###autoload
(defun drover-external-scheduled (&optional all)
  "Return the prompts scheduled, a line each, the next due first, for a script.
Those pending alone, unless ALL is non-nil.  Each line tells its
identifier, its status, when it goes, its session and its prompt,
bounded."
  (drover-external--call
   (lambda ()
     (drover-schedule-load)
     (let ((jobs (sort (seq-filter (lambda (job) (or all (eq (plist-get job :status) 'pending)))
                                   (drover-schedule--jobs))
                       :key (lambda (job) (or (plist-get job :due) 1.0e+INF)))))
       (if (null jobs)
           "no prompts scheduled"
         (mapconcat (lambda (job)
                      (format "%s\t%s\t%s\t%s: %s" (plist-get job :id) (drover-schedule--status-text job)
                              (drover-schedule--when job) (drover-bound-line (plist-get job :title) 80)
                              (drover-bound-line (drover-schedule--prompt-line (plist-get job :prompt)) 120)))
                    (seq-take jobs 200) "\n"))))))

(defun drover-headless-unload-function ()
  "Stop the heartbeat of `drover-health'."
  (when drover-health--beat
    (cancel-timer (car drover-health--beat))
    (setq drover-health--beat nil))
  nil)

(provide 'drover-headless)
;;; drover-headless.el ends here
