;;; drover-schedule.el --- Prompts sent to the sessions of coding agents later  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, calendar

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

;; A prompt scheduled goes to an existing conversation later: at a time,
;; once the session's running turn ends, or after an event the user's
;; configuration names, such as a restart (`drover-schedule-define-trigger').
;; `drover-schedule-prompt', the prompt action `schedule' of drover's
;; prompt (S-RET in `drover-dispatch'), asks for the session, then when;
;; the time is read locally (`drover-schedule-parse-time'), shown as it
;; is typed, and refused when unreadable or past.  OpenCode session
;; buffers schedule their input the same way (see `drover-opencode').
;;
;; The queue is kept in `drover-state-directory', and survives the
;; session's buffer and Emacs: `drover-schedule-restore' arms its timers
;; again, catching up the prompts that fell due meanwhile, once Emacs
;; runs.  At delivery, the session's front end resumes the conversation
;; if no buffer shows it, in the window `drover-schedule-display-function'
;; gives, applies the agent, model and effort the prompt was scheduled
;; with, as a send does, and submits the prompt once the session can
;; take it without touching a draft (its backend's :blocker and :submit,
;; see `drover-register-backend').  A conversation drover archived is
;; restored first, as a send restores it; one its front end archived,
;; the front end restores as it takes the prompt.  "Sent" means
;; submitted to the front end, not answered.  A submission Emacs
;; stopped in the middle of is kept as interrupted: whether it went out
;; is unknown.
;;
;; `drover-schedule-list' (# in `drover-dispatch') lists the prompts: k
;; cancels one, r retries it or sends it now, RET shows it whole.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'time-date)
(require 'drover-core)
(require 'drover-model)
(require 'drover-links)
(require 'drover-prompt)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 8))
  (load "drover-core" nil t))

(defvar vertico-preselect)

;;;; Options

(defcustom drover-schedule-wait-seconds 600
  "Seconds a scheduled prompt waits for its session to open and take it.
Past them, the prompt fails, kept in the queue to be retried."
  :type 'natnum
  :group 'drover)

(defcustom drover-schedule-candidates '("in 24h" "in 55m" "in 8h" "in 7 days")
  "Times offered when scheduling a prompt, the first being the default.
The end of the running turn comes first while one runs, and the events
of `drover-schedule-define-trigger' last."
  :type '(repeat string)
  :group 'drover)

(defcustom drover-schedule-display-function #'drover-schedule-display-frame
  "Function showing the session a scheduled prompt goes to, as it is delivered.
It is called with the session's live BUFFER, or nil when its front end
must resume it first, and the prompt's TITLE, and returns the window the
session is to show in: the resumed session opens there, with that
window selected."
  :type 'function
  :group 'drover)

(defcustom drover-schedule-former-files nil
  "Queue files of earlier configurations, taken into drover's once.
Their prompts join the queue, those already in it by identifier left
alone; once the queue is saved, each file is renamed with \".migrated\"
appended, and no longer read.  A file that does not read is left where
it is, and told of."
  :type '(repeat file)
  :group 'drover)

(defconst drover-schedule--version 2
  "Version of the queue's file.
Version 1, of earlier configurations, named front ends where version 2
names backends, and printed models whole.")

(defconst drover-schedule--statuses '(pending opening waiting sending sent failed interrupted cancelled)
  "Statuses of a scheduled prompt.")

(defconst drover-schedule--list-buffer "*drover scheduled prompts*"
  "Name of the buffer listing the scheduled prompts.")

;;;; The queue

(cl-defstruct (drover-schedule--queue (:constructor drover-schedule--queue-make)
                                      (:copier nil))
  "The scheduled prompts, and how each is being delivered."
  (stamp (drover-layout 'drover-schedule--queue)
         :documentation "The layout it was made of, see `drover-layout'.")
  (jobs nil :documentation "The prompts, each a plist, newest first.")
  (loaded nil :documentation "Non-nil once the file was read.")
  (timers (make-hash-table :test #'equal)
          :documentation "Timers of the pending prompts, by identifier.")
  (active (make-hash-table :test #'equal)
          :documentation "The deliveries in progress, each a plist, by identifier."))

(drover-note-layout 'drover-schedule--queue)

(defvar drover-schedule--queue nil
  "The queue of scheduled prompts, a `drover-schedule--queue', or nil.")

(defvar drover-schedule--triggers nil
  "Events prompts may wait for, an alist (NAME . PLIST) in registration order.")

(defun drover-schedule--stray-timers ()
  "Cancel the timers of scheduled prompts no queue knows any more."
  (dolist (timer timer-list)
    (when (memq (timer--function timer) '(drover-schedule--run drover-schedule--poll))
      (cancel-timer timer))))

(defun drover-schedule--state ()
  "Return the queue, made anew when another version of drover made it.
The file is read again then, and its timers armed anew."
  (if (drover-layout-current-p drover-schedule--queue 'drover-schedule--queue)
      drover-schedule--queue
    (let ((stale drover-schedule--queue))
      (when stale
        (drover-stale-layout "the queue of scheduled prompts")
        (drover-schedule--stray-timers))
      (setq drover-schedule--queue (drover-schedule--queue-make))
      ;; It had been read: what it held is in the file.
      (when stale
        (drover-with-reporting "scheduled prompts"
          (drover-schedule--start)))
      drover-schedule--queue)))

(defun drover-schedule--jobs ()
  "Return the scheduled prompts."
  (drover-schedule--queue-jobs (drover-schedule--state)))

(defun drover-schedule--timers ()
  "Return the timers of the pending prompts, by identifier."
  (drover-schedule--queue-timers (drover-schedule--state)))

(defun drover-schedule--active ()
  "Return the deliveries in progress, by identifier."
  (drover-schedule--queue-active (drover-schedule--state)))

(defun drover-schedule-file ()
  "Return the file keeping the scheduled prompts."
  (expand-file-name "scheduled-prompts.eld" drover-state-directory))

(defun drover-schedule--job (id)
  "Return the scheduled prompt ID, or nil."
  (seq-find (lambda (job) (equal id (plist-get job :id))) (drover-schedule--jobs)))

(defun drover-schedule-conversation (job)
  "Return the conversation JOB goes to, as `drover-links-parse' returns it."
  (drover-links-parse (string-remove-prefix (concat drover-links-type ":") (plist-get job :link))))

(defun drover-schedule--key (job)
  "Return the engine key, (ENGINE . ID), of the conversation of JOB."
  (map-let (:engine :id) (drover-schedule-conversation job)
    (cons engine id)))

(defun drover-schedule--uuid ()
  "Return a random version 4 UUID, identifying a scheduled prompt."
  (format "%04X%04X-%04X-4%03X-%04X-%04X%04X%04X"
          (random #x10000) (random #x10000) (random #x10000) (random #x1000)
          (logior #x8000 (random #x4000))
          (random #x10000) (random #x10000) (random #x10000)))

;;;; Records

(defun drover-schedule--options-valid-p (options)
  "Return non-nil when OPTIONS are explicit choices delivery can apply."
  (and (listp options)
       (or (null (plist-get options :agent)) (stringp (plist-get options :agent)))
       (or (null (plist-get options :model)) (drover-model-p (plist-get options :model)))
       (symbolp (plist-get options :variant))))

(defun drover-schedule-job-valid-p (job)
  "Return non-nil when JOB is a scheduled prompt as the queue holds them."
  (and (plistp job)
       (stringp (plist-get job :id))
       (stringp (plist-get job :link))
       (string-prefix-p (concat drover-links-type ":") (plist-get job :link))
       (drover-schedule-conversation job)
       (plist-get job :backend) (symbolp (plist-get job :backend))
       (stringp (plist-get job :directory))
       (stringp (plist-get job :title))
       (stringp (plist-get job :prompt)) (not (string-blank-p (plist-get job :prompt)))
       (drover-schedule--options-valid-p (plist-get job :options))
       (cond ((plist-get job :due) (and (numberp (plist-get job :due)) (> (plist-get job :due) 0)))
             ((plist-get job :turn) (eq (plist-get job :turn) t))
             (t (and (stringp (plist-get job :after))
                     (plist-get job :trigger) (symbolp (plist-get job :trigger)))))
       (memq (plist-get job :status) drover-schedule--statuses)))

(defun drover-schedule--model-in (model)
  "Return MODEL, as a file holds it, as a `drover-model', or nil.
Version 2 holds the plist of `drover--model-data'; version 1 a record
printed whole, read through the accessors of the definition loaded."
  (let ((data (cond ((drover--model-data-p model) model)
                    ((and (recordp model) (eq (type-of model) 'drover-model))
                     (ignore-errors (drover--model-data model))))))
    (and data (drover--model-data-p data) (drover--model-from-data data))))

(defun drover-schedule--job-in (job)
  "Return JOB, as a file holds it, as the queue holds it.
Its model becomes a `drover-model', dropped with its variant when it
does not read; a job of version 1 names its front end, which becomes
its backend."
  (if (not (plistp job))
      job
    (let ((copy nil))
      (while job
        (let ((key (pop job))
              (value (pop job)))
          (pcase key
            (:frontend (setq copy (plist-put copy :backend (if (eq value 'ide) 'claude-code-ide value))))
            (:options
             (setq copy (plist-put copy :options
                                   (if-let* (((listp value))
                                             (stored (plist-get value :model)))
                                       (let ((model (drover-schedule--model-in stored)))
                                         (list :agent (plist-get value :agent) :model model
                                               :variant (and model (plist-get value :variant))))
                                     value))))
            (_ (setq copy (plist-put copy key value))))))
      (dolist (key '(:error :sent-at))
        (unless (plist-member copy key)
          (setq copy (plist-put copy key nil))))
      copy)))

(defun drover-schedule--job-out (job)
  "Return JOB as the file holds it: its model as the data it is made of."
  (if-let* ((options (plist-get job :options))
            (model (plist-get options :model)))
      (plist-put (copy-sequence job) :options
                 (plist-put (copy-sequence options) :model (drover--model-data model)))
    job))

(defun drover-schedule--check-jobs (jobs file)
  "Return JOBS, read from FILE, or signal an error when one is not valid.
Each must be a valid prompt (see `drover-schedule-job-valid-p'), and
their identifiers unique."
  (unless (and (proper-list-p jobs)
               (seq-every-p #'drover-schedule-job-valid-p jobs)
               (= (length jobs)
                  (length (delete-dups (mapcar (lambda (job) (plist-get job :id)) jobs)))))
    (error "Invalid queue of scheduled prompts: %s" (abbreviate-file-name file)))
  jobs)

(defun drover-schedule--read (file)
  "Return the prompts FILE holds, of the current version, or nil without FILE.
Signal an error when it does not read."
  (drover-schedule--check-jobs
   (mapcar #'drover-schedule--job-in
           (condition-case err
               (drover-read-data file drover-schedule--version :jobs)
             (error (error "Unreadable queue of scheduled prompts %s: %s"
                           (abbreviate-file-name file) (drover-error-message err)))))
   file))

(defun drover-schedule--read-former (file)
  "Return the prompts FILE, a queue of an earlier configuration, holds.
Signal an error when it does not read."
  (let ((data (with-temp-buffer
                (insert-file-contents file)
                (read (current-buffer)))))
    (unless (and (plistp data) (eql (plist-get data :version) 1))
      (error "Unknown format of %s" (abbreviate-file-name file)))
    (drover-schedule--check-jobs (mapcar #'drover-schedule--job-in (plist-get data :jobs)) file)))

(defun drover-schedule--save ()
  "Write the queue to its file, atomically, or signal an error.
Never before the file was read: a file that did not read is kept."
  (let ((queue (drover-schedule--state)))
    (unless (drover-schedule--queue-loaded queue)
      (error "The queue of scheduled prompts was not read: not writing over it"))
    (drover-write-data (drover-schedule-file) "Scheduled prompts" drover-schedule--version :jobs
                       (mapcar #'drover-schedule--job-out (drover-schedule--queue-jobs queue)))))

(defun drover-schedule--take-former ()
  "Return the prompts of `drover-schedule-former-files', with the files read.
As (JOBS . FILES).  A file that does not read is told of and left out."
  (let ((jobs nil) (files nil))
    (dolist (file drover-schedule-former-files)
      (when (file-exists-p file)
        (condition-case err
            (progn (setq jobs (append jobs (drover-schedule--read-former file)))
                   (push file files))
          (error (drover-warn "Scheduled prompts of %s left where they are: %s"
                              (abbreviate-file-name file) (drover-error-message err))))))
    (cons jobs (nreverse files))))

(defun drover-schedule-load ()
  "Read the queue of scheduled prompts, once, and return it.
The prompts of `drover-schedule-former-files' join it, and the files are
renamed once the queue is saved.  A prompt Emacs stopped while opening
its session is pending again; one it stopped while submitting is
interrupted.  Signal an error when the file does not read: it is then
left as it is, and nothing is scheduled."
  (let ((queue (drover-schedule--state)))
    (unless (drover-schedule--queue-loaded queue)
      (let* ((file (drover-schedule-file))
             (jobs (drover-schedule--read file))
             (former (drover-schedule--take-former))
             (known (mapcar (lambda (job) (plist-get job :id)) jobs)))
        (dolist (job (car former))
          (unless (member (plist-get job :id) known)
            (setq jobs (append jobs (list job)))))
        (dolist (job jobs)
          (pcase (plist-get job :status)
            ((or 'opening 'waiting) (setf (plist-get job :status) 'pending))
            ('sending (setf (plist-get job :status) 'interrupted
                            (plist-get job :error)
                            "Emacs stopped during submission; inspect the session before retrying"))))
        (setf (drover-schedule--queue-jobs queue) jobs
              (drover-schedule--queue-loaded queue) t)
        (add-hook 'drover-changed-hook #'drover-schedule--turn-check)
        (when (or (file-exists-p file) (cdr former))
          (drover-schedule--save))
        (dolist (former-file (cdr former))
          (condition-case err
              (rename-file former-file (concat former-file ".migrated") t)
            (error (drover-warn "Could not rename %s, taken into drover's queue: %s"
                                (abbreviate-file-name former-file) (drover-error-message err)))))))
    queue))

;;;; Timers

(defun drover-schedule--arm (job &optional delay)
  "Arm the timer of JOB, firing when it is due, or after DELAY seconds."
  (let ((id (plist-get job :id))
        (timers (drover-schedule--timers)))
    (when-let* ((timer (gethash id timers)))
      (cancel-timer timer))
    (puthash id (run-at-time (or delay (max 0 (- (plist-get job :due) (float-time))))
                             nil #'drover-schedule--run id)
             timers)))

(defun drover-schedule--start ()
  "Read the queue, then arm the timers of its pending prompts anew.
Overdue prompts go out at once; a prompt waiting for a turn that ended
meanwhile too.  Signal an error when the queue does not read."
  (drover-schedule-load)
  (add-hook 'drover-changed-hook #'drover-schedule--turn-check)
  (let ((timers (drover-schedule--timers)))
    (maphash (lambda (_ timer) (cancel-timer timer)) timers)
    (clrhash timers)
    (dolist (job (drover-schedule--jobs))
      (when (and (eq (plist-get job :status) 'pending) (plist-get job :due))
        (drover-schedule--arm job))))
  ;; A turn may have ended while Emacs was away: its session, once listed idle, gets the prompt.
  (when (seq-some #'drover-schedule--turn-waiting-p (drover-schedule--jobs))
    (drover-start)
    (drover-schedule--turn-check)))

;;;###autoload
(defun drover-schedule-restore ()
  "Restore the scheduled prompts: read the queue and arm its timers.
Prompts that fell due while Emacs did not run go out now.  For a hook
run as Emacs starts, once the session buffers are restored: a queue
that does not read is reported, not signalled."
  (drover-with-reporting "scheduled prompts"
    (drover-schedule--start)))

(defun drover-schedule--status (job status &optional error)
  "Note STATUS and ERROR, a message or nil, of JOB, and save the queue."
  (setf (plist-get job :status) status
        (plist-get job :error) (and error (drover-bound-line error)))
  (drover-schedule--save)
  (drover-schedule--refresh))

;;;; Events prompts wait for

(defun drover-schedule-define-trigger (name label &rest properties)
  "Let prompts wait for event NAME, a symbol, which LABEL names.
LABEL reads after \"after the next\", such as \"Emacs restart\"; the time
\"after LABEL\" is offered when scheduling.  PROPERTIES is a plist of:

 :request  Function of no argument asking for the event to happen, as
           a prompt is scheduled to wait for it, and returning a KEY, a
           string telling what was asked for: `drover-schedule-fire'
           with KEY releases the prompts.  Without it, KEY is NAME's
           name.
 :release  Function of KEY called once no prompt waits for it any more,
           cancelled or sent at once, to withdraw what :request asked.

Defining NAME again replaces its label and properties and keeps its
place."
  (let ((properties (append (list :label label) properties)))
    (if-let* ((entry (assq name drover-schedule--triggers)))
        (setcdr entry properties)
      (setq drover-schedule--triggers (append drover-schedule--triggers
                                              (list (cons name properties))))))
  name)

(defun drover-schedule-undefine-trigger (name)
  "Forget event NAME; the prompts waiting for it go on waiting."
  (setq drover-schedule--triggers (assq-delete-all name drover-schedule--triggers)))

(defun drover-schedule--trigger-label (trigger)
  "Return the label of event TRIGGER, its name when undefined."
  (or (plist-get (alist-get trigger drover-schedule--triggers) :label)
      (symbol-name trigger)))

(defun drover-schedule--waiting-p (job &optional key)
  "Return non-nil when JOB waits for an event, the one of KEY when given."
  (and (eq (plist-get job :status) 'pending)
       (null (plist-get job :due))
       (plist-get job :after)
       (or (null key) (equal key (plist-get job :after)))))

(defun drover-schedule-waiting-count (key)
  "Return how many prompts wait for the event of KEY.
It reads nothing: before the queue was read, none."
  (if (and (drover-schedule--queue-p drover-schedule--queue)
           (drover-layout-current-p drover-schedule--queue 'drover-schedule--queue))
      (seq-count (lambda (job) (drover-schedule--waiting-p job key))
                 (drover-schedule--queue-jobs drover-schedule--queue))
    0))

(defun drover-schedule--request (trigger)
  "Ask for event TRIGGER to happen, and return the key it was asked by."
  (let* ((request (plist-get (alist-get trigger drover-schedule--triggers) :request))
         (key (if (functionp request) (funcall request) (symbol-name trigger))))
    (unless (stringp key)
      (error "The request of %s returned no key" (drover-schedule--trigger-label trigger)))
    key))

(defun drover-schedule--release (job)
  "Withdraw the request JOB made, once no pending prompt waits for it."
  (let ((key (plist-get job :after)))
    (unless (or (null key)
                (seq-some (lambda (other) (drover-schedule--waiting-p other key)) (drover-schedule--jobs)))
      (when-let* ((release (plist-get (alist-get (plist-get job :trigger) drover-schedule--triggers)
                                      :release))
                  ((functionp release)))
        (funcall release key)))))

(defun drover-schedule-fire (key &optional cancelled)
  "Release the prompts waiting for the event of KEY, which happened.
They are sent now; with CANCELLED, the event will not happen, and they
are cancelled instead, CANCELLED telling why when a string, such as
\"dismissed\".  Return how many prompts were released."
  (drover-schedule-load)
  (let ((waiting (seq-filter (lambda (job) (drover-schedule--waiting-p job key)) (drover-schedule--jobs))))
    (when waiting
      (dolist (job waiting)
        (if cancelled
            (setf (plist-get job :status) 'cancelled
                  (plist-get job :error) (format "The %s it waited for was %s"
                                                 (drover-schedule--trigger-label (plist-get job :trigger))
                                                 (if (stringp cancelled) cancelled "called off")))
          (setf (plist-get job :due) (float-time))))
      (drover-schedule--save)
      (unless cancelled
        (mapc #'drover-schedule--arm waiting))
      (drover-schedule--refresh))
    (length waiting)))

;;;; Waiting for a turn to end

(defun drover-schedule--turn-waiting-p (job)
  "Return non-nil when JOB waits for the end of its session's turn."
  (and (eq (plist-get job :status) 'pending)
       (null (plist-get job :due))
       (eq (plist-get job :turn) t)))

(defun drover-schedule--turn-over-p (job sessions)
  "Return non-nil when the turn JOB waits for ended, as drover's SESSIONS tell.
A session listed ended it once out of `drover-active-statuses'.  One
not listed ran no turn drover sees: when its buffer ran the agent
\(:runs-in-buffer), its turn ended with it; a service's session, once
the backend lists every session the service runs (:ready).  A backend
not loaded tells nothing yet."
  (let ((key (drover-schedule--key job)))
    (if-let* ((session (seq-find (lambda (session) (equal (drover-session-engine-key session) key))
                                 sessions)))
        (not (memq (drover-session-status session) drover-active-statuses))
      (let ((backend (plist-get job :backend)))
        (cond ((not (memq backend (drover-backends))) nil)
              ((drover-backend-property backend :runs-in-buffer) t)
              ((functionp (drover-backend-property backend :ready))
               (funcall (drover-backend-property backend :ready)))
              (t t))))))

(defun drover-schedule--sessions ()
  "Return the sessions a scheduled prompt may go to, as drover lists them.
Those drover archived itself, a buffer still showing them, are among
them: delivery restores them first."
  (append (drover-known-sessions) (drover-archived-live-sessions)))

(defun drover-schedule--turn-check ()
  "Send the prompts waiting for a turn that ended, giving them the time now.
On `drover-changed-hook', which runs once backends reported changes."
  (drover-with-reporting "scheduled prompts after a turn"
    (let ((queue (drover-schedule--state)))
      (when-let* ((waiting (and (drover-schedule--queue-loaded queue)
                                (seq-filter #'drover-schedule--turn-waiting-p
                                            (drover-schedule--queue-jobs queue)))))
        (let* ((sessions (drover-schedule--sessions))
               (ended (seq-filter (lambda (job) (drover-schedule--turn-over-p job sessions)) waiting)))
          (when ended
            (dolist (job ended)
              (setf (plist-get job :due) (float-time)))
            (condition-case err
                (drover-schedule--save)
              (error (dolist (job ended)
                       (setf (plist-get job :due) nil))
                     (signal (car err) (cdr err))))
            (dolist (job ended)
              (drover-schedule--arm job 0))
            (drover-schedule--refresh)))))))

;;;; Adding prompts

(defun drover-schedule--new-job (target prompt instruction &rest when)
  "Return a new pending prompt of TARGET, PROMPT and INSTRUCTION.
WHEN is a plist telling when it goes: (:due TIME), (:turn t) or (:after
KEY :trigger TRIGGER)."
  (append (list :id (drover-schedule--uuid))
          when
          (unless (plist-member when :due) (list :due nil))
          (list :prompt prompt :instruction instruction :created (float-time)
                :status 'pending :error nil :sent-at nil)
          (copy-sequence target)))

(defun drover-schedule--push (job &optional undo)
  "Add JOB to the queue and save it, or signal an error.
JOB not valid, or the queue not saved, it is left out, and UNDO, a
function of no argument, called."
  (unless (drover-schedule-job-valid-p job)
    (when undo (funcall undo))
    (error "Invalid scheduled prompt"))
  (let ((queue (drover-schedule-load)))
    (push job (drover-schedule--queue-jobs queue))
    (condition-case err
        (drover-schedule--save)
      (error (setf (drover-schedule--queue-jobs queue) (delq job (drover-schedule--queue-jobs queue)))
             (when undo (funcall undo))
             (signal (car err) (cdr err))))
    job))

(defun drover-schedule--prompt-line (prompt)
  "Return PROMPT on one line, bounded, as messages and the list show it."
  (drover-bound-line (replace-regexp-in-string "[\n\r]+" " " (drover-bound-text prompt 4))))

(defun drover-schedule-add (target time prompt &optional instruction reference-time)
  "Schedule PROMPT for TARGET at TIME, and return the scheduled prompt.
TARGET is what `drover-schedule-target' returns, with the explicit
:options to apply first, if any; INSTRUCTION is what the time was read
from.  TIME must be after REFERENCE-TIME, by default now: a prompt
scheduled a moment ago goes at once."
  (unless (and (time-less-p (or reference-time (current-time)) time)
               (stringp prompt) (not (string-blank-p prompt)))
    (error "Need a future time and a nonempty prompt"))
  (let ((job (drover-schedule--push (drover-schedule--new-job target prompt instruction
                                                              :due (float-time time)))))
    (drover-schedule--arm job)
    (drover-schedule--refresh)
    (drover-message "Scheduled %S for %s — %s" (drover-schedule--prompt-line prompt)
                    (format-time-string "%a %F %R %Z" time) (plist-get job :title))
    job))

(defun drover-schedule-add-after-trigger (target trigger prompt &optional instruction)
  "Schedule PROMPT for TARGET after the next event TRIGGER, asking for it.
See `drover-schedule-add' for TARGET and INSTRUCTION, and
`drover-schedule-define-trigger' for TRIGGER.  Return the prompt."
  (unless (and (stringp prompt) (not (string-blank-p prompt)))
    (error "Need a nonempty prompt"))
  (let* ((key (drover-schedule--request trigger))
         (job (drover-schedule--new-job target prompt instruction :after key :trigger trigger)))
    (drover-schedule--push job (lambda () (drover-schedule--release job)))
    (drover-schedule--refresh)
    (drover-message "Scheduled %S after the next %s — %s" (drover-schedule--prompt-line prompt)
                    (drover-schedule--trigger-label trigger) (plist-get job :title))
    job))

(defun drover-schedule-add-after-turn (target prompt &optional instruction)
  "Schedule PROMPT for TARGET once its session's running turn ends.
See `drover-schedule-add' for TARGET and INSTRUCTION.  Return the prompt."
  (unless (and (stringp prompt) (not (string-blank-p prompt)))
    (error "Need a nonempty prompt"))
  (let ((job (drover-schedule--push (drover-schedule--new-job target prompt instruction :turn t))))
    (drover-schedule--refresh)
    (drover-message "Scheduled %S after the turn of %s" (drover-schedule--prompt-line prompt)
                    (plist-get job :title))
    ;; The turn may have ended while the time was read.
    (drover-schedule--turn-check)
    job))

(defun drover-schedule-enqueue (target chosen text)
  "Schedule TEXT for TARGET when CHOSEN tells, and return the prompt.
CHOSEN is what `drover-schedule-read-when' returns."
  (pcase-let ((`(,when ,input ,read-at) chosen))
    (cond ((eq when 'turn-end) (drover-schedule-add-after-turn target text input))
          ((symbolp when) (drover-schedule-add-after-trigger target when text input))
          (t (drover-schedule-add target when text input read-at)))))

;;;; Reading the time

(defconst drover-schedule-day-parts
  '(("morning" . 9) ("noon" . 12) ("afternoon" . 14) ("evening" . 18) ("night" . 21))
  "Hours the parts of a day stand for.")

(defconst drover-schedule--weekdays
  '(("sunday" . 0) ("sun" . 0) ("monday" . 1) ("mon" . 1) ("tuesday" . 2) ("tues" . 2) ("tue" . 2)
    ("wednesday" . 3) ("wed" . 3) ("thursday" . 4) ("thurs" . 4) ("thur" . 4) ("thu" . 4)
    ("friday" . 5) ("fri" . 5) ("saturday" . 6) ("sat" . 6))
  "Weekday names and abbreviations, with their `decode-time' numbers.")

(defun drover-schedule--clock (text)
  "Return the (HOUR . MINUTE) TEXT names, a time of day, or nil."
  (cond
   ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\(?::\\([0-9]\\{2\\}\\)\\)? ?\\([ap]\\)m\\'" text)
    (let ((hour (string-to-number (match-string 1 text)))
          (minute (if (match-string 2 text) (string-to-number (match-string 2 text)) 0)))
      (when (and (<= 1 hour 12) (< minute 60))
        (cons (+ (% hour 12) (if (equal (match-string 3 text) "p") 12 0)) minute))))
   ((string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" text)
    (let ((hour (string-to-number (match-string 1 text)))
          (minute (string-to-number (match-string 2 text))))
      (when (and (< hour 24) (< minute 60))
        (cons hour minute))))
   ((assoc text drover-schedule-day-parts)
    (cons (cdr (assoc text drover-schedule-day-parts)) 0))))

(defun drover-schedule--at (year month day clock zone)
  "Return the time of CLOCK, (HOUR . MINUTE), on a day in ZONE, or nil.
The day is DAY of MONTH of YEAR; DAY may overflow the month.  A local
time the day does not have is nil."
  (let* ((time (encode-time (list 0 (cdr clock) (car clock) day month year nil -1 zone)))
         (back (decode-time time zone)))
    (when (and (= (decoded-time-hour back) (car clock)) (= (decoded-time-minute back) (cdr clock)))
      time)))

(defun drover-schedule--on-day (day clock now zone)
  "Return the time of CLOCK on DAY, the next one after NOW when DAY recurs.
DAY is a string a DAY of `drover-schedule-parse-time' matched; CLOCK is
\(HOUR . MINUTE).  Nil when that date or time does not exist."
  (let* ((today (decode-time now zone))
         (year (decoded-time-year today))
         (month (decoded-time-month today))
         (date (decoded-time-day today)))
    (cond
     ((equal day "today") (drover-schedule--at year month date clock zone))
     ((equal day "tomorrow") (drover-schedule--at year month (1+ date) clock zone))
     ((assoc day drover-schedule--weekdays)
      (let* ((ahead (mod (- (cdr (assoc day drover-schedule--weekdays)) (decoded-time-weekday today)) 7))
             (time (drover-schedule--at year month (+ date ahead) clock zone)))
        (if (or (null time) (time-less-p now time))
            time
          (drover-schedule--at year month (+ date ahead 7) clock zone))))
     ((string-match "\\`\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)\\(?:/\\([0-9]\\{4\\}\\)\\)?\\'" day)
      (let ((wanted-month (string-to-number (match-string 1 day)))
            (wanted-day (string-to-number (match-string 2 day)))
            (wanted-year (and (match-string 3 day) (string-to-number (match-string 3 day)))))
        (cl-flet ((valid (year)
                    (<= wanted-day (date-days-in-month year wanted-month))))
          (when (<= 1 wanted-month 12)
            (if wanted-year
                (and (valid wanted-year)
                     (drover-schedule--at wanted-year wanted-month wanted-day clock zone))
              ;; February 29 recurs only in leap years.
              (catch 'found
                (dolist (offset (number-sequence 0 8))
                  (when (valid (+ year offset))
                    (let ((time (drover-schedule--at (+ year offset) wanted-month wanted-day clock zone)))
                      (when (or (null time) (time-less-p now time))
                        (throw 'found time))))))))))))))

(defun drover-schedule--normalize (text)
  "Return TEXT in lower case, its blanks single spaces, trimmed."
  (downcase (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))))

(defun drover-schedule--trigger-named (text)
  "Return the event TEXT, normalized, names as \"after [the [next]] LABEL\", or nil."
  (when (string-match "\\`after \\(?:the \\(?:next \\)?\\)?\\(.+\\)\\'" text)
    (let ((label (match-string 1 text)))
      (car (seq-find (lambda (entry)
                       (equal (drover-schedule--normalize (plist-get (cdr entry) :label)) label))
                     drover-schedule--triggers)))))

(defun drover-schedule-parse-time (text now &optional zone)
  "Return when TEXT asks a prompt to be sent, relative to NOW, or nil.
The result is a time, or the name of an event of
`drover-schedule-define-trigger', read \"after [the [next]] LABEL\".
TEXT is \"in N UNIT\" with minutes, hours, days or weeks; \"[on] DAY
\[[at] TIME]\", \"[at] TIME [[on] DAY]\" or \"tonight\", DAY being today,
tomorrow, a weekday or a month/day[/year] date and TIME 9am, 5:30pm,
17:30 or a part of the day of `drover-schedule-day-parts'.  Minutes and
hours elapse; days and weeks keep the time of day across a change of
daylight saving time.  A weekday or date is the next one after NOW, a
day alone means 9:00, a time alone today, or tomorrow once past.  Case
and spacing do not matter; anything else is nil rather than a guess.
An explicit day can be past; the caller refuses it.  ZONE is as for
`decode-time', the local zone when nil."
  (let* ((text (drover-schedule--normalize text))
         (time-re (concat "\\(?:[0-9]\\{1,2\\}\\(?::[0-9]\\{2\\}\\)? ?[ap]m\\|[0-9]\\{1,2\\}:[0-9]\\{2\\}\\|"
                          (regexp-opt (mapcar #'car drover-schedule-day-parts)) "\\)"))
         (day-re (concat "\\(?:today\\|tomorrow\\|" (regexp-opt (mapcar #'car drover-schedule--weekdays))
                         "\\|[0-9]\\{1,2\\}/[0-9]\\{1,2\\}\\(?:/[0-9]\\{4\\}\\)?\\)")))
    (cond
     ((drover-schedule--trigger-named text))
     ((string-match (concat "\\`in \\([0-9]+\\|an?\\) ?"
                            "\\(m\\|mins?\\|minutes?\\|h\\|hrs?\\|hours?\\|d\\|days?\\|w\\|wks?\\|weeks?\\)\\'")
                    text)
      (let ((count (if (string-prefix-p "a" (match-string 1 text)) 1 (string-to-number (match-string 1 text))))
            (unit (aref (match-string 2 text) 0)))
        (pcase unit
          (?m (time-add now (* 60 count)))
          (?h (time-add now (* 3600 count)))
          (_ (let ((decoded (decode-time now zone)))
               (setf (decoded-time-day decoded) (+ (decoded-time-day decoded) (* count (if (eq unit ?w) 7 1)))
                     (decoded-time-dst decoded) -1
                     (decoded-time-zone decoded) zone)
               (encode-time decoded))))))
     ((equal text "tonight") (drover-schedule--on-day "today" '(21 . 0) now zone))
     ((or (string-match (concat "\\`\\(?:on \\)?\\(" day-re "\\)\\(?: \\(?:at \\)?\\(" time-re "\\)\\)?\\'") text)
          (string-match (concat "\\`\\(?:at \\)?\\(?2:" time-re "\\)\\(?: \\(?:on \\)?\\(?1:" day-re "\\)\\)?\\'") text))
      (let ((day (match-string 1 text))
            (clock (if (match-string 2 text) (drover-schedule--clock (match-string 2 text)) '(9 . 0))))
        (cond
         ((null clock) nil)
         (day (drover-schedule--on-day day clock now zone))
         (t (let ((time (drover-schedule--on-day "today" clock now zone)))
              (if (and time (time-less-p now time))
                  time
                (drover-schedule--on-day "tomorrow" clock now zone))))))))))

(defun drover-schedule-describe (when now &optional zone)
  "Describe WHEN, `turn-end', an event or a time, for a reader at NOW in ZONE."
  (pcase when
    ('turn-end "after the running turn")
    ((pred symbolp) (format "after the next %s" (drover-schedule--trigger-label when)))
    (_ (format-time-string (if (= (decoded-time-year (decode-time when zone))
                                  (decoded-time-year (decode-time now zone)))
                               "%a %-d %b %H:%M"
                             "%a %-d %b %Y %H:%M")
                           when zone))))

(defvar drover-schedule--history nil
  "Times read when scheduling prompts.")

(defvar drover-schedule--turn nil
  "Non-nil while reading a time for a session whose turn is running.")

(defvar drover-schedule--default nil
  "The time scheduling sends to with empty input.")

(defvar-local drover-schedule--preview-overlay nil
  "Overlay showing in the minibuffer what the input resolves to.")

(defun drover-schedule--resolve (input now)
  "Return what INPUT asks for at NOW: `turn-end', an event, a time, or nil."
  (if (equal (drover-schedule--normalize input) "when turn ends")
      (and drover-schedule--turn 'turn-end)
    (drover-schedule-parse-time input now)))

(defun drover-schedule--preview-text (input)
  "Return what INPUT resolves to, for display while it is typed."
  (let* ((now (current-time))
         (when (drover-schedule--resolve input now)))
    (cond
     ((null when) (propertize "not a time" 'face 'error))
     ((and (not (symbolp when)) (not (time-less-p now when)))
      (propertize (concat (drover-schedule-describe when now) " is past") 'face 'error))
     (t (concat "→ " (drover-schedule-describe when now))))))

(defun drover-schedule--annotate (candidate)
  "Annotate a time CANDIDATE with the time it resolves to."
  (concat "  " (propertize (drover-schedule--preview-text candidate) 'face 'completions-annotations)))

(defun drover-schedule--preview ()
  "Show after the minibuffer's prompt what the input, or the default, resolves to."
  (drover-with-reporting "scheduling preview"
    (let* ((input (minibuffer-contents-no-properties))
           (text (drover-schedule--preview-text
                  (if (string-blank-p input) drover-schedule--default input))))
      (unless (and drover-schedule--preview-overlay (overlay-buffer drover-schedule--preview-overlay))
        (setq drover-schedule--preview-overlay (make-overlay (point-min) (minibuffer-prompt-end))))
      (overlay-put drover-schedule--preview-overlay 'after-string
                   (concat (propertize "[" 'face 'minibuffer-prompt) text
                           (propertize "] " 'face 'minibuffer-prompt))))))

(defun drover-schedule--candidates (turn)
  "Return the times offered, the end of the running turn first with TURN."
  (append (and turn '("when turn ends"))
          drover-schedule-candidates
          (mapcar (lambda (entry) (concat "after " (plist-get (cdr entry) :label)))
                  drover-schedule--triggers)))

(defun drover-schedule-read-when (&optional turn)
  "Read when to send a prompt; return (WHEN INPUT READ-AT).
WHEN is `turn-end', an event of `drover-schedule-define-trigger' or a
future time, INPUT the text it was read from and READ-AT the time it
was read at.  TURN non-nil offers \"when turn ends\" first, for a
session whose turn runs.  What the input resolves to shows as it is
typed; RET takes the input as typed, never a candidate it completes to.
Input that is not a time, or a time past, is refused and read again."
  (let* ((candidates (drover-schedule--candidates turn))
         (drover-schedule--turn turn)
         (drover-schedule--default (car candidates))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (category . drover-schedule-time)
                                 (annotation-function . drover-schedule--annotate)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action candidates string predicate))))
         (problem nil)
         (result nil))
    (while (not result)
      (let* ((input (string-trim
                     (minibuffer-with-setup-hook
                         (lambda ()
                           ;; RET sends what was typed; a candidate is taken by moving onto it.
                           (setq-local vertico-preselect 'prompt)
                           (add-hook 'post-command-hook #'drover-schedule--preview nil t)
                           ;; The minibuffer is reused: the overlay must not outlive this read.
                           (add-hook 'minibuffer-exit-hook
                                     (lambda () (when drover-schedule--preview-overlay
                                                  (delete-overlay drover-schedule--preview-overlay)))
                                     nil t)
                           (drover-schedule--preview))
                       (completing-read (if problem (format "Send when (%s): " problem) "Send when: ")
                                        table nil nil nil 'drover-schedule--history
                                        drover-schedule--default))))
             (input (if (string-empty-p input) drover-schedule--default input))
             (now (current-time))
             (when (drover-schedule--resolve input now)))
        (cond
         ((null when)
          (setq problem (if (equal (drover-schedule--normalize input) "when turn ends")
                            "no turn is running"
                          (format "cannot read “%s” as a time" (drover-bound-line input 60)))))
         ((and (not (symbolp when)) (not (time-less-p now when)))
          (setq problem (format "%s is past" (drover-schedule-describe when now))))
         (t (setq result (list when input now))))))
    result))

;;;; Scheduling

(defun drover-schedule-read-prompt ()
  "Read the prompt to schedule, nothing being written where it was asked for."
  (let ((text (read-string "Prompt to send: ")))
    (when (string-blank-p text)
      (user-error "Prompt is empty"))
    (string-trim text)))

(defun drover-schedule-target (&optional buffer)
  "Return where a prompt scheduled from BUFFER goes, the current buffer by default.
BUFFER is the buffer of a session, in any front end.  The value is a
plist: :link, an agent-session link to its conversation (see
`drover-links'); :backend, the front end showing it, which delivers
the prompt; :directory, BUFFER's; :title, BUFFER's name.  Signal a
`user-error' when BUFFER shows no session, or one whose conversation's
identifier is not known yet."
  (with-current-buffer (or buffer (current-buffer))
    (let ((session (drover-session-at-point t)))
      (unless (and session (not (drover-session-list-p))
                   (eq (drover-session-buffer session) (current-buffer)))
        (user-error "Run this command in the buffer of an agent session"))
      (let ((conversation (or (drover-links-conversation session)
                              (user-error "%s does not know its conversation's identifier yet"
                                          (drover-session-display-title session)))))
        (list :link (drover-links-link conversation)
              :backend (drover-session-backend session)
              :directory default-directory
              :title (buffer-name))))))

(defun drover-schedule--from-session ()
  "Schedule a prompt, read, for the session of the current buffer.
While its turn runs, its end is offered first."
  (let* ((session (drover-session-at-point t))
         (target (drover-schedule-target))
         (chosen (drover-schedule-read-when
                  (and session (memq (drover-session-status session) '(working retrying)) t))))
    (drover-schedule-enqueue target chosen (drover-schedule-read-prompt))))

(defun drover-schedule--from-prompt ()
  "Schedule drover's prompt for an existing session.
The session is chosen first, then the time, which offers the end of
the session's turn while one runs, whatever its front end.  Its text,
or its active region, goes with the agent, model and effort chosen in
it, and leaves the prompt as a send does; an empty prompt is read
instead.  A prompt with attachments is refused: the queue keeps text."
  (let* ((prompt (current-buffer))
         (content (drover-prompt-content prompt))
         ;; The queue keeps text, which a label alone is.
         (_ (when (plist-get content :attachments)
              (user-error "A prompt with attachments cannot be scheduled: send it, or delete their labels")))
         (session (drover-prompt-read-target "Schedule for session: " :existing-only t :options-menu t))
         (buffer (or (drover-session-buffer session)
                     ;; The queue is this host's, and delivers through this host's front ends.
                     (when (drover-session-host session)
                       (user-error "%s runs on %s: prompts are scheduled on the host running the session"
                                   (drover-session-display-title session)
                                   (drover-host-label (drover-session-host session))))
                     (user-error "%s has no live buffer" (drover-session-display-title session))))
         (target (drover-schedule-target buffer))
         (chosen (drover-schedule-read-when
                  (and (memq (drover-session-status session) '(working retrying)) t)))
         (text (if content (plist-get content :text) (drover-schedule-read-prompt)))
         (options (drover-prompt-options-for session (drover-prompt-explicit-options prompt) t)))
    (drover-schedule-enqueue
     (append target (list :options (and (seq-some (lambda (key) (plist-get options key))
                                                  '(:agent :model :variant))
                                        options)))
     chosen text)
    ;; A blank region read instead leaves the rest of the prompt alone.
    (when (or content (string-blank-p (with-current-buffer prompt (buffer-string))))
      (drover-prompt-consume content prompt))))

;;;###autoload
(defun drover-schedule-prompt ()
  "Schedule the prompt being written for a later turn of its session.
In drover's prompt, the prompt's text, or its region, goes to an
existing session the picker chooses, with the agent, model and effort
chosen in the prompt, and leaves the prompt as a send does; a buffer
defining its own prompt action `schedule', as OpenCode's do, schedules
its input that way; in another session's buffer, the prompt is read.
When it goes is read then (see `drover-schedule-read-when')."
  (interactive)
  (drover-schedule--start)
  (let ((action (alist-get 'schedule drover-prompt-actions)))
    (cond
     ((bound-and-true-p drover-prompt-mode) (drover-schedule--from-prompt))
     ((and action (not (eq action 'drover-schedule-prompt)) (commandp action))
      (call-interactively action))
     (t (drover-schedule--from-session)))))

;;;; Delivering

(defun drover-schedule-display-frame (buffer title)
  "Return the window of a new frame showing the session of a scheduled prompt.
The frame is named after TITLE, and shows BUFFER when non-nil.  The
default `drover-schedule-display-function'."
  (let ((frame (make-frame `((name . ,(format "Scheduled: %s" (drover-bound-line title)))))))
    (when buffer
      (set-window-buffer (frame-selected-window frame) buffer))
    (make-frame-visible frame)
    (frame-selected-window frame)))

(defun drover-schedule--current-run-p (job run)
  "Return non-nil when RUN is the delivery of JOB in progress."
  (eq run (gethash (plist-get job :id) (drover-schedule--active))))

(defun drover-schedule--buffer (job)
  "Return the live buffer of JOB's backend showing its conversation, or nil.
Found by the conversation and backend, never by directory; its process
must run."
  (let ((key (drover-schedule--key job))
        (backend (plist-get job :backend)))
    (seq-some (lambda (session)
                (when-let* (((eq (drover-session-backend session) backend))
                            ((equal (drover-session-engine-key session) key))
                            (buffer (drover-session-buffer session))
                            ((process-live-p (get-buffer-process buffer))))
                  buffer))
              (drover-schedule--sessions))))

(defun drover-schedule--restore (job &optional session)
  "Restore the conversation of JOB from drover's own archive, if it is there.
SESSION is the session JOB's buffer shows, once the conversation was
resumed: signal a `user-error' when it cannot be restored (see
`drover-unarchive-to-send'), so that nothing is sent.  Without SESSION,
before the conversation is resumed, it is restored when a backend
follows it, keeping a buffer archiving was to close; one no backend
follows yet is left archived, for the resumed session to be restored."
  (if session
      (drover-unarchive-to-send session)
    (when-let* ((archived (drover-archived-conversation (drover-schedule--key job))))
      (condition-case nil
          (drover-unarchive-to-send archived)
        (user-error nil)))))

(defun drover-schedule--blocker (job buffer)
  "Say why BUFFER cannot take JOB's prompt without touching a draft, or nil."
  (let ((session (drover-buffer-session buffer)))
    (cond
     ((not (and session (equal (drover-session-engine-key session) (drover-schedule--key job))))
      "the buffer shows another session")
     ((not (process-live-p (get-buffer-process buffer))) "the session process is not running")
     ((functionp (drover-backend-property session :blocker))
      (funcall (drover-backend-property session :blocker) session)))))

(defun drover-schedule--wait-reason (job run)
  "Say what JOB's RUN was still waiting for when its deadline passed."
  (let ((buffer (plist-get run :buffer)))
    (cond ((eq (plist-get job :status) 'sending) "the submission did not complete")
          ((not buffer) "the session never opened")
          ((not (buffer-live-p buffer)) "the session buffer was closed")
          ((drover-schedule--blocker job buffer))
          (t "the session became ready only at the deadline"))))

(defun drover-schedule--finish (job run status &optional error)
  "Finish JOB's RUN with STATUS and ERROR, a message or nil, once.
A RUN finished or cancelled already is left alone."
  (when (drover-schedule--current-run-p job run)
    (when-let* ((timer (plist-get run :timer)))
      (cancel-timer timer))
    (remhash (plist-get job :id) (drover-schedule--active))
    (when (eq status 'sent)
      (setf (plist-get job :sent-at) (float-time)))
    (drover-schedule--status job status error)
    (drover-message "Scheduled prompt %s — %s%s" status (plist-get job :title)
                    (if error (concat ": " error) ""))))

(defmacro drover-schedule--guard (job run status &rest body)
  "Run BODY for JOB's RUN out of a command, finishing it with STATUS if it fails.
BODY runs only while RUN is JOB's delivery in progress."
  (declare (indent 3) (debug t))
  (let ((err (make-symbol "err")))
    `(drover-with-reporting "scheduled prompt"
       (when (drover-schedule--current-run-p ,job ,run)
         (condition-case ,err
             (progn ,@body)
           (error (drover-schedule--finish ,job ,run ,status (drover-error-message ,err))))))))

(defun drover-schedule--attached (job run buffer)
  "Attach BUFFER, showing its session, to JOB's RUN, if it is in progress."
  (when (drover-schedule--current-run-p job run)
    (unless (buffer-live-p buffer)
      (error "The session did not return a live buffer"))
    (setf (plist-get run :buffer) buffer)
    (when-let* ((window (plist-get run :window))
                ((window-live-p window)))
      (set-window-buffer window buffer))
    (drover-schedule--status job 'waiting)))

(defun drover-schedule--open (job run)
  "Resume the conversation of JOB in its backend, in RUN's window.
What the resume calls back with stays scoped to RUN."
  (map-let (:engine :id) (drover-schedule-conversation job)
    (let ((window (plist-get run :window))
          (directory (plist-get job :directory)))
      (unless (file-directory-p directory)
        (error "Session directory is unavailable: %s" directory))
      (with-selected-window (if (window-live-p window) window (selected-window))
        (let ((default-directory directory)
              (current-prefix-arg nil)
              (enable-local-variables :safe)
              (enable-local-eval nil)
              (display-buffer-overriding-action '(display-buffer-same-window))
              (display-buffer-alist nil))
          (drover-resume-with (plist-get job :backend) engine id directory
                              (lambda (buffer)
                                (drover-schedule--guard job run 'failed
                                  (drover-schedule--attached job run buffer)))
                              (lambda (message)
                                (drover-schedule--guard job run 'failed
                                  (drover-schedule--finish job run 'failed message)))))))))

(defun drover-schedule--submit (job run)
  "Hand JOB's prompt to RUN's buffer through its backend's :submit."
  (let* ((buffer (plist-get run :buffer))
         (session (or (drover-buffer-session buffer) (error "The buffer tells no session")))
         (submit (drover-backend-property session :submit)))
    (unless (functionp submit)
      (error "%s takes no scheduled prompt" (drover-backend-label session)))
    (funcall submit session (plist-get job :prompt)
             (lambda ()
               (drover-schedule--guard job run 'interrupted
                 (drover-schedule--finish job run 'sent)))
             (lambda (message &optional not-sent)
               (drover-schedule--guard job run 'interrupted
                 (drover-schedule--finish job run (if not-sent 'failed 'interrupted) message))))))

(defun drover-schedule--send (job run)
  "Submit JOB to RUN's buffer, noting the handoff first.
A conversation drover archived is restored first: when it cannot be,
this signals, nothing sent.  The agent, model and effort JOB was
scheduled with are applied first, as a send applies them; a RUN
cancelled or timed out meanwhile sends nothing."
  (let ((buffer (plist-get run :buffer)))
    (drover-schedule--restore job (drover-buffer-session buffer))
    (drover-schedule--status job 'sending)
    (if-let* ((options (plist-get job :options)))
        (drover-prompt-apply-options
         (drover-buffer-session buffer) options
         (lambda ()
           (drover-schedule--guard job run 'interrupted
             (if (not (buffer-live-p buffer))
                 (drover-schedule--finish job run 'failed "Session buffer was closed before submission")
               (drover-schedule--submit job run)))))
      (drover-schedule--submit job run))))

(defun drover-schedule--poll (job run)
  "Deliver JOB's prompt once RUN's buffer can take it, or fail past the deadline.
From RUN's timer; a RUN no longer in progress stops its timer."
  (drover-with-reporting "scheduled prompt"
    (if (not (drover-schedule--current-run-p job run))
        (when-let* ((timer (plist-get run :timer)))
          (cancel-timer timer))
      (condition-case err
          (let ((buffer (plist-get run :buffer)))
            (cond
             ((> (float-time) (plist-get run :deadline))
              (error "Timed out: %s" (drover-schedule--wait-reason job run)))
             ((eq (plist-get job :status) 'sending) nil)
             ((and buffer (not (buffer-live-p buffer)))
              (error "Session buffer was closed before submission"))
             ((and buffer (null (drover-schedule--blocker job buffer)))
              (drover-schedule--send job run))))
        (error (drover-schedule--finish job run
                                        (if (eq (plist-get job :status) 'sending) 'interrupted 'failed)
                                        (drover-error-message err)))))))

(defun drover-schedule--run (id)
  "Deliver the pending prompt ID, one at a time to a conversation.
From the prompt's timer."
  (drover-with-reporting "scheduled prompt"
    (let ((timers (drover-schedule--timers))
          (active (drover-schedule--active)))
      (when-let* ((timer (gethash id timers)))
        (cancel-timer timer))
      (remhash id timers)
      (when-let* ((job (drover-schedule--job id))
                  ((eq (plist-get job :status) 'pending)))
        (if (seq-some (lambda (other)
                        (and (equal (plist-get job :link) (plist-get other :link))
                             (gethash (plist-get other :id) active)))
                      (drover-schedule--jobs))
            (drover-schedule--arm job 1)
          (let ((run (list :deadline (+ (float-time) drover-schedule-wait-seconds)
                           :window nil :buffer nil :timer nil)))
            (puthash id run active)
            (condition-case err
                (progn
                  (drover-schedule--status job 'opening)
                  (when-let* ((host (plist-get (drover-schedule-conversation job) :host)))
                    (error "Conversations of other hosts cannot be reached yet: %s" host))
                  (drover-links-load-front-end (plist-get job :backend))
                  (drover-schedule--restore job)
                  (let ((buffer (drover-schedule--buffer job)))
                    (setf (plist-get run :window)
                          (funcall drover-schedule-display-function buffer (plist-get job :title)))
                    (if buffer
                        (drover-schedule--attached job run buffer)
                      (drover-schedule--open job run)))
                  (when (drover-schedule--current-run-p job run)
                    (setf (plist-get run :timer) (run-at-time 0.25 0.5 #'drover-schedule--poll job run))))
              (error (drover-schedule--finish job run 'failed (drover-error-message err))))))))))

;;;; The list of scheduled prompts

(defun drover-schedule--when (job)
  "Return when JOB goes, as the list shows it."
  (cond ((plist-get job :due)
         (format-time-string "%F %R %Z" (seconds-to-time (plist-get job :due))))
        ((plist-get job :turn) (format "after the turn of %s" (plist-get job :title)))
        (t (format "after %s" (drover-schedule--trigger-label (plist-get job :trigger))))))

(defun drover-schedule--refresh ()
  "Draw the list of scheduled prompts again, if it exists, without displaying it."
  (when-let* ((buffer (get-buffer drover-schedule--list-buffer)))
    (with-current-buffer buffer
      (setq tabulated-list-entries
            (mapcar (lambda (job)
                      (list (plist-get job :id)
                            (vector (drover-schedule--when job)
                                    (symbol-name (plist-get job :status))
                                    (drover-bound-line (plist-get job :title))
                                    (drover-schedule--prompt-line (plist-get job :prompt)))))
                    (sort (copy-sequence (drover-schedule--jobs))
                          (lambda (a b) (< (or (plist-get a :due) 1.0e+INF)
                                           (or (plist-get b :due) 1.0e+INF))))))
      (tabulated-list-print t))))

(defun drover-schedule--at-point ()
  "Return the scheduled prompt of the row at point, or signal a `user-error'."
  (or (drover-schedule--job (tabulated-list-get-id))
      (user-error "No scheduled prompt on this row")))

(defun drover-schedule-cancel (&optional job)
  "Cancel the scheduled prompt JOB, by default the one at point, before it goes.
The request of an event it waited for is withdrawn with the last prompt
waiting for it."
  (interactive)
  (let* ((job (or job (drover-schedule--at-point)))
         (id (plist-get job :id))
         (run (gethash id (drover-schedule--active)))
         (waiting (drover-schedule--waiting-p job)))
    (unless (memq (plist-get job :status) '(pending opening waiting))
      (user-error "This prompt is already %s" (plist-get job :status)))
    (when-let* ((timer (gethash id (drover-schedule--timers))))
      (cancel-timer timer))
    (remhash id (drover-schedule--timers))
    (if run
        (drover-schedule--finish job run 'cancelled)
      (drover-schedule--status job 'cancelled))
    (when waiting
      (drover-schedule--release job))))

(defun drover-schedule-retry ()
  "Retry the failed, interrupted or cancelled prompt at point now.
A prompt still waiting for an event, or for the end of a turn, is sent
now instead."
  (interactive)
  (let ((job (drover-schedule--at-point)))
    (cond
     ((drover-schedule--waiting-p job)
      (when (y-or-n-p (format "Send now instead of after the %s? "
                              (drover-schedule--trigger-label (plist-get job :trigger))))
        (setf (plist-get job :due) (float-time))
        (drover-schedule--status job 'pending)
        (drover-schedule--arm job 0)
        (drover-schedule--release job)))
     ((drover-schedule--turn-waiting-p job)
      (when (y-or-n-p (format "Send now instead of after the turn of %s? " (plist-get job :title)))
        (setf (plist-get job :due) (float-time))
        (drover-schedule--status job 'pending)
        (drover-schedule--arm job 0)))
     ((not (memq (plist-get job :status) '(failed interrupted cancelled)))
      (user-error "This prompt cannot be retried"))
     ((or (not (eq (plist-get job :status) 'interrupted))
          (y-or-n-p "Delivery was uncertain.  Send the prompt again? "))
      (unless (plist-get job :due)
        (setf (plist-get job :due) (float-time)))
      (drover-schedule--status job 'pending)
      (drover-schedule--arm job 0)))))

(defun drover-schedule--describe-options (options)
  "Describe OPTIONS, the agent, model and effort of a scheduled prompt."
  (string-join (delq nil (list (when-let* ((agent (plist-get options :agent)))
                                 (format "agent %s" agent))
                               (when-let* ((model (plist-get options :model)))
                                 (format "model %s" (drover-model-label model)))
                               (when-let* ((variant (plist-get options :variant)))
                                 (format "effort %s" variant))))
               ", "))

(defun drover-schedule-inspect ()
  "Show the whole prompt at point, and how its delivery went."
  (interactive)
  (let ((job (drover-schedule--at-point)))
    (with-current-buffer (get-buffer-create "*drover scheduled prompt*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n%s\n\nScheduled: %s\nStatus: %s\n%s%s\nPrompt:\n%s\n"
                        (plist-get job :title) (plist-get job :link)
                        (string-join
                         (delq nil (list (when-let* ((due (plist-get job :due)))
                                           (format-time-string "%F %T %Z" (seconds-to-time due)))
                                         (when-let* ((trigger (plist-get job :trigger)))
                                           (format "after the next %s" (drover-schedule--trigger-label trigger)))
                                         (when (plist-get job :turn)
                                           (format "after the turn of %s" (plist-get job :title)))))
                         ", ")
                        (plist-get job :status)
                        (if-let* ((options (plist-get job :options)))
                            (format "Options: %s\n" (drover-schedule--describe-options options))
                          "")
                        (or (plist-get job :error) "") (plist-get job :prompt))))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(defvar-keymap drover-schedule-list-mode-map
  :doc "Keymap of `drover-schedule-list-mode'."
  :parent tabulated-list-mode-map
  "k" #'drover-schedule-cancel
  "r" #'drover-schedule-retry
  "RET" #'drover-schedule-inspect)

(define-derived-mode drover-schedule-list-mode tabulated-list-mode "Scheduled prompts"
  "The scheduled prompts: k cancels, r retries, RET shows the prompt, g refreshes."
  (setq tabulated-list-format [("Time" 32 t) ("Status" 12 t) ("Session" 48 t) ("Prompt" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'drover-schedule--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun drover-schedule-list ()
  "List the scheduled prompts and how their delivery went."
  (interactive)
  (drover-schedule--start)
  (with-current-buffer (get-buffer-create drover-schedule--list-buffer)
    (drover-schedule-list-mode))
  (drover-schedule--refresh)
  (pop-to-buffer drover-schedule--list-buffer))

(defun drover-schedule-unload-function ()
  "Stop the timers of scheduled prompts, which stay in their file."
  (remove-hook 'drover-changed-hook #'drover-schedule--turn-check)
  (when (drover-schedule--queue-p drover-schedule--queue)
    (ignore-errors
      (maphash (lambda (_ timer) (cancel-timer timer)) (drover-schedule--queue-timers drover-schedule--queue))))
  (drover-schedule--stray-timers)
  nil)

(provide 'drover-schedule)
;;; drover-schedule.el ends here
