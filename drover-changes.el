;;; drover-changes.el --- What sessions changed in all their turns  -*- lexical-binding: t; -*-

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

;; What a session changed, all its turns together: the lines added and
;; removed and the files, such as "+120 −14 · 3 files", which the list
;; shows in the column `changes', and the files themselves, which an
;; expanded session lists.  Its engine tells them, as a review of the
;; whole session shows them (the engine's :changes, see
;; `drover-register-engine'): OpenCode's service diffs the snapshots it
;; took of the working tree, Claude Code's and Codex's edits are read
;; from their transcripts and rollouts.  No model is asked.
;;
;; Reading them costs a request or a whole transcript, so they are read
;; once a turn ended, never while a session works, and only for the
;; sessions views show (`drover-prefetch'), `drover-changes-concurrency'
;; at a time, from a timer.  What was read is kept in
;; `drover-state-directory', the newest `drover-changes-limit'
;; conversations, each with the time of its session's last activity it
;; describes: Emacs starting reads again only those that changed since.
;;
;; Records of this host carry the counts, given as drover takes them in
;; (`drover-changes-function', the slot `changes'), so that every view,
;; and the peers this host serves, see them; their recaps carry the
;; files (`drover-changes-files-function', :files).  Another host's
;; sessions carry those their owner read, which a peer asks for the
;; sessions it shows (see `drover-remote', capability "changes").

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 22))
  (load "drover-core" nil t))

(defgroup drover-changes nil
  "What sessions changed in all their turns."
  :group 'drover
  :prefix "drover-changes-")

(defcustom drover-changes-delay 3
  "Seconds a session stays settled before what it changed is read.
A change meanwhile puts it off again, so that a turn ending, its title
renamed and the session marked seen read its changes once."
  :type 'number
  :group 'drover-changes)

(defcustom drover-changes-concurrency 2
  "How many sessions' changes are read at once, at most."
  :type 'natnum
  :group 'drover-changes)

(defcustom drover-changes-timeout 120
  "Seconds an engine is given to tell what a session changed.
Past them, the session is read again only once it changes."
  :type 'number
  :group 'drover-changes)

(defconst drover-changes-limit 1000
  "How many conversations' changes the file keeps, the newest.")

(defconst drover-changes-file-limit 100
  "How many files of a conversation's changes are kept, at most.
Its counts take every file into account.")

(defconst drover-changes-names-limit 8000
  "Characters of the names of a conversation's files kept, at most.
The files past them are counted, not named, so that what is kept of a
conversation stays within a few kilobytes, whatever its files.")

(defconst drover-changes--version 1
  "Version of the format of the file of changes.")

;;;; What a session's changes describe

(defun drover-changes--conversation (session)
  "Return the conversation of SESSION as changes are kept, (ENGINE . ID), or nil.
Its engine and the engine's identifier, so that front ends showing one
conversation share its changes; nil while either is unknown, or for an
identifier neither a string nor an integer."
  (let ((engine (drover-session-engine session))
        (id (drover-session-engine-id session)))
    (and engine (symbolp engine) (or (stringp id) (integerp id))
         (cons engine id))))

(defun drover-changes--reader (session)
  "Return the function reading what SESSION changed, or nil.
That is its engine's :changes, for a session of this host."
  (and (null (drover-session-host session))
       (not (drover-backend-property (drover-session-backend session) :remote))
       (drover-changes--conversation session)
       (let ((function (drover-engine-property (drover-session-engine session) :changes)))
         (and (functionp function) function))))

;;;; Changes kept

;; Each conversation keeps a plist: :updated, the time of last activity
;; of its session as read; :additions and :deletions, the lines, nil
;; when the engine counted none; :count, the files; :files, the first
;; `drover-changes-file-limit' of them, as (FILE ADDITIONS DELETIONS);
;; :time, when read.  An entry is replaced, never changed, so that
;; records given its counts are made once.

(defvar drover-changes--store nil
  "The changes kept, (FILE . TABLE), as read from FILE, or nil.
TABLE maps each engine to a table of its identifiers to entries.")

(defvar drover-changes--warned nil
  "Non-nil once a problem with the file of changes was reported.")

(defvar drover-changes--dirty nil
  "Non-nil when the changes kept changed since written.")

(defvar drover-changes--save-timer nil
  "Timer writing the changes kept, or nil.")

(defun drover-changes-file ()
  "Return the file the changes of conversations are kept in."
  (expand-file-name "changes.eld" drover-state-directory))

(defun drover-changes--warn (format-string &rest args)
  "Report a problem with the file of changes, once.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless drover-changes--warned
    (setq drover-changes--warned t)
    (apply #'drover-warn format-string args)))

(defun drover-changes--count-p (value)
  "Return non-nil when VALUE is a count of lines: a natural number, or nil."
  (or (null value) (natnump value)))

(defun drover-changes--file-valid-p (file)
  "Return non-nil when FILE, read from the file of changes, is one as kept."
  (pcase file
    (`(,(pred stringp) ,(pred drover-changes--count-p) ,(pred drover-changes--count-p)) t)))

(defun drover-changes--entry-valid-p (entry)
  "Return non-nil when ENTRY, read from the file, is one as kept."
  (pcase entry
    (`(,(pred symbolp) ,(or (pred stringp) (pred integerp)) . ,(and plist (pred plistp)))
     (and (or (null (plist-get plist :updated)) (numberp (plist-get plist :updated)))
          (drover-changes--count-p (plist-get plist :additions))
          (drover-changes--count-p (plist-get plist :deletions))
          (natnump (plist-get plist :count))
          (proper-list-p (plist-get plist :files))
          (seq-every-p #'drover-changes--file-valid-p (plist-get plist :files))
          (numberp (plist-get plist :time))))))

(defun drover-changes--put (table conversation entry)
  "Make ENTRY the changes of CONVERSATION in TABLE; nil forgets them."
  (let ((ids (gethash (car conversation) table)))
    (if entry
        (puthash (cdr conversation) entry
                 (or ids (puthash (car conversation) (make-hash-table :test #'equal) table)))
      (when ids (remhash (cdr conversation) ids)))))

(defun drover-changes--read (file)
  "Return the table of changes FILE keeps, empty without one.
One that does not read is reported once, and ignored; it is not written
over until changes are read anew."
  (let ((table (make-hash-table :test #'eq)))
    (condition-case err
        (dolist (entry (drover-read-data file drover-changes--version :changes))
          (when (drover-changes--entry-valid-p entry)
            (let ((plist (cddr entry)))
              (drover-changes--put
               table (cons (car entry) (cadr entry))
               (list :updated (plist-get plist :updated)
                     :additions (plist-get plist :additions)
                     :deletions (plist-get plist :deletions)
                     :count (plist-get plist :count)
                     :files (plist-get (drover-changes--totals
                                        (mapcar (pcase-lambda (`(,name ,added ,deleted))
                                                  (list :file name :additions added :deletions deleted))
                                                (plist-get plist :files)))
                                       :files)
                     :time (plist-get plist :time))))))
      (error
       (drover-changes--warn "Ignoring the changes kept in %s: %s"
                             (abbreviate-file-name file) (drover-error-message err))))
    table))

(defun drover-changes--table ()
  "Return the table of changes, read from its file once.
It is read again when `drover-state-directory' names another file."
  (let ((file (drover-changes-file)))
    (unless (equal (car drover-changes--store) file)
      (setq drover-changes--store (cons file (drover-changes--read file))
            drover-changes--dirty nil))
    (cdr drover-changes--store)))

(defun drover-changes--entry (session)
  "Return the changes kept of the conversation of SESSION, or nil.
A lookup without allocating, as every record listed is looked up."
  (when-let* ((table (cdr drover-changes--store))
              (engine (drover-session-engine session))
              (ids (gethash engine table))
              (id (drover-session-engine-id session)))
    (gethash id ids)))

(defun drover-changes--entry-of (conversation)
  "Return the changes kept of CONVERSATION, (ENGINE . ID), or nil."
  (when-let* ((ids (gethash (car conversation) (drover-changes--table))))
    (gethash (cdr conversation) ids)))

(defun drover-changes--keep (conversation entry)
  "Make ENTRY the changes of CONVERSATION, written soon in their file."
  (drover-changes--put (drover-changes--table) conversation entry)
  (setq drover-changes--dirty t)
  (unless (timerp drover-changes--save-timer)
    (setq drover-changes--save-timer (run-at-time 2 nil #'drover-changes--flush))))

(defun drover-changes--flush ()
  "Write the changes kept in their file, from a timer."
  (drover-with-reporting "keeping changes"
    (setq drover-changes--save-timer nil)
    (drover-changes-save)))

(defun drover-changes-save ()
  "Write the changes kept in their file, when they changed since written.
The newest `drover-changes-limit' are kept.  A failure is reported once."
  (when (and drover-changes--dirty drover-changes--store
             (equal (car drover-changes--store) (drover-changes-file)))
    (setq drover-changes--dirty nil)
    (let ((entries nil))
      (maphash (lambda (engine ids)
                 (maphash (lambda (id entry) (push (cons engine (cons id entry)) entries)) ids))
               (cdr drover-changes--store))
      (setq entries (sort entries :key (lambda (entry) (plist-get (cddr entry) :time))
                          :reverse t :in-place t))
      (dolist (forgotten (nthcdr drover-changes-limit entries))
        (drover-changes--put (cdr drover-changes--store) (cons (car forgotten) (cadr forgotten)) nil))
      (condition-case err
          (drover-write-data (car drover-changes--store) "What conversations changed, read from their engines"
                             drover-changes--version :changes
                             (seq-take entries drover-changes-limit))
        (error (drover-changes--warn "Could not keep the changes in %s: %s"
                                     (abbreviate-file-name (car drover-changes--store))
                                     (drover-error-message err)))))))

(defun drover-changes--totals (files)
  "Return what FILES, as an engine's :changes tells them, changed, as an entry.
A plist of :additions and :deletions, the lines of the files whose
engine counted them, nil when it counted none, :count, the files, and
:files, the first `drover-changes-file-limit' as (FILE ADDITIONS
DELETIONS), each name a line, within `drover-changes-names-limit'
characters together."
  (let ((additions nil) (deletions nil) (count 0) (kept nil) (room drover-changes-names-limit))
    (dolist (file files)
      (when-let* (((listp file))
                  (name (plist-get file :file))
                  ((stringp name)))
        (let ((added (plist-get file :additions))
              (deleted (plist-get file :deletions)))
          (setq added (and (natnump added) added)
                deleted (and (natnump deleted) deleted))
          (when added (setq additions (+ (or additions 0) added)))
          (when deleted (setq deletions (+ (or deletions 0) deleted)))
          (cl-incf count)
          (when (and (<= count drover-changes-file-limit) (= (length kept) (1- count)))
            (let ((name (drover-bound-line (substring-no-properties name))))
              (when (<= (length name) room)
                (setq room (- room (length name)))
                (push (list name added deleted) kept)))))))
    (list :additions additions :deletions deletions :count count :files (nreverse kept))))

;;;; Records and recaps

(defvar drover-changes--copies (make-hash-table :test #'eq :weakness 'key)
  "Records given their changes, each a record taken in mapped to (ENTRY . COPY).
The same record and entry give the same copy, so that records listed
again compare alike, and cost nothing more.")

(defun drover-changes--value (entry)
  "Return what ENTRY tells a record, (ADDITIONS DELETIONS FILES), or nil.
Nil for a session that changed no file."
  (and entry (> (plist-get entry :count) 0)
       (list (plist-get entry :additions) (plist-get entry :deletions) (plist-get entry :count))))

(defun drover-changes--record (record)
  "Return RECORD with what its session changed, or itself.
The function of `drover-changes-function': a copy of RECORD whose slot
`changes' tells the changes kept of its conversation, the same for the
same record and changes; RECORD itself when it tells them already, or
nothing is kept of it."
  (if-let* ((entry (drover-changes--entry record)))
      (let ((made (gethash record drover-changes--copies)))
        (if (eq (car made) entry)
            (cdr made)
          (let ((value (drover-changes--value entry)))
            (if (equal value (drover-session-changes record))
                record
              (let ((copy (copy-sequence record)))
                (setf (drover-session-changes copy) value)
                (puthash record (cons entry copy) drover-changes--copies)
                copy)))))
    record))

(defun drover-changes--files (session)
  "Return the files the turns of SESSION changed, as kept, or nil.
The function of `drover-changes-files-function'."
  (plist-get (drover-changes--entry session) :files))

;;;; Following sessions

(defvar drover-changes--started nil
  "Non-nil once changes are followed, as views first asked for them.")

(defvar drover-changes--wanted (make-hash-table :test #'equal)
  "Conversations whose changes views asked for, mapped to t.
Their changes are read again as their sessions' turns end.")

(defvar drover-changes--active (make-hash-table :test #'equal)
  "Conversations seen at work, mapped to t, until their changes are read again.")

(defvar drover-changes--failed (make-hash-table :test #'equal)
  "Conversations whose changes could not be read, each mapped to a time.
That of their sessions' last activity then: they are read again once
their sessions change.")

(defvar drover-changes--queue (make-hash-table :test #'equal)
  "Conversations whose changes to read, mapped to (TIME . SPEC).
TIME is when, a float time; SPEC, what reading needs, see
`drover-changes--spec'.")

(defvar drover-changes--jobs (make-hash-table :test #'eql)
  "Changes being read, by token, each a plist of :spec and :timer.")

(defvar drover-changes--token 0
  "The token of the last read started.")

(defvar drover-changes--timer nil
  "Timer starting the reads due, or nil.")

(defun drover-changes--spec (session)
  "Return what reading the changes of SESSION needs, a plist, or nil.
Its :conversation, :directory, :updated and :key, the record not kept."
  (when-let* ((conversation (drover-changes--conversation session))
              ((drover-changes--reader session)))
    (list :conversation conversation
          :directory (drover-session-local-directory session)
          :updated (drover-session-updated session)
          :key (drover-session-key session))))

(defun drover-changes--reading-p (conversation)
  "Return non-nil while the changes of CONVERSATION are being read."
  (catch 'found
    (maphash (lambda (_ job)
               (when (equal (plist-get (plist-get job :spec) :conversation) conversation)
                 (throw 'found t)))
             drover-changes--jobs)
    nil))

(defun drover-changes--due-p (session conversation)
  "Return non-nil when the changes of SESSION, of CONVERSATION, are to be read.
It is settled, and what is kept of it describes another time of last
activity, or a turn this Emacs saw it work since; it did not fail to
be read at its time of last activity, and is not being read: once read,
it is looked at again."
  (let ((updated (drover-session-updated session)))
    (and (not (drover-session-active-p session))
         (let ((entry (drover-changes--entry session)))
           (or (null entry)
               (not (equal (plist-get entry :updated) updated))
               (gethash conversation drover-changes--active)))
         (not (equal (gethash conversation drover-changes--failed 'none) updated))
         (not (drover-changes--reading-p conversation)))))

(defun drover-changes--consider (session delay)
  "Read what SESSION changed after DELAY seconds, if due and readable.
Note that it works, while it does: its changes are read again once its
turn ends."
  (when-let* ((spec (drover-changes--spec session)))
    (let ((conversation (plist-get spec :conversation)))
      (when (drover-session-active-p session)
        (puthash conversation t drover-changes--active)
        (remhash conversation drover-changes--queue))
      (when (drover-changes--due-p session conversation)
        (drover-changes--schedule spec delay)))))

;;;###autoload
(defun drover-changes-prefetch (sessions)
  "Read what SESSIONS changed, those of this host, once settled.
The function of `drover-changes-prefetch-function', which
`drover-prefetch' calls with the sessions views show: they are read now
when unknown or stale, and again as their turns end.  It costs nothing
for sessions whose changes are known or being read."
  (drover-changes--start)
  (dolist (session sessions)
    (when-let* (((drover-session-p session))
                (conversation (drover-changes--conversation session))
                ((drover-changes--reader session)))
      (puthash conversation t drover-changes--wanted)
      (drover-changes--consider session 0))))

(defun drover-changes--changed ()
  "Follow the sessions that changed, for `drover-changed-hook'.
Those whose changes views asked for, or kept, are read again once
settled for `drover-changes-delay' seconds."
  (let ((all (eq drover-changed-sessions t)))
    (dolist (key (if all
                     (hash-table-keys (drover--index-records (drover-index)))
                   drover-changed-sessions))
      (when-let* ((session (drover-index-session key))
                  ((or (drover-changes--entry session)
                       (gethash (drover-changes--conversation session) drover-changes--wanted))))
        (drover-changes--consider session drover-changes-delay)))))

(defun drover-changes--schedule (spec delay)
  "Read the changes SPEC describes after DELAY seconds.
A read scheduled already is put off to then."
  (puthash (plist-get spec :conversation) (cons (+ (float-time) delay) spec) drover-changes--queue)
  (drover-changes--arm))

(defun drover-changes--arm ()
  "Arm the timer for the next read due, if one may start."
  (when (timerp drover-changes--timer)
    (cancel-timer drover-changes--timer)
    (setq drover-changes--timer nil))
  (when (< (hash-table-count drover-changes--jobs) (max 1 drover-changes-concurrency))
    (let ((next nil))
      (maphash (lambda (_ entry) (when (or (null next) (< (car entry) next)) (setq next (car entry))))
               drover-changes--queue)
      (when next
        (setq drover-changes--timer
              (run-at-time (max 0 (- next (float-time))) nil #'drover-changes--run))))))

(defun drover-changes--run ()
  "Start the reads due, a few at once, the most recently active first.
From a timer."
  (drover-with-reporting "session changes"
    (setq drover-changes--timer nil)
    (let ((now (float-time))
          (due nil))
      (maphash (lambda (conversation entry)
                 (when (<= (car entry) now) (push (cons conversation (cdr entry)) due)))
               drover-changes--queue)
      (dolist (entry (sort due :key (lambda (entry) (or (plist-get (cdr entry) :updated) 0))
                           :reverse t :in-place t))
        (when (< (hash-table-count drover-changes--jobs) (max 1 drover-changes-concurrency))
          (remhash (car entry) drover-changes--queue)
          ;; As the session stands now, unless it works again.
          (let* ((session (drover-index-session (plist-get (cdr entry) :key)))
                 (spec (if session (drover-changes--spec session) (cdr entry))))
            (unless (or (null spec) (and session (drover-session-active-p session)))
              (drover-changes--read-changes spec))))))
    (drover-changes--arm)))

(defun drover-changes--read-changes (spec)
  "Ask the engine of the conversation SPEC describes what it changed."
  (let* ((conversation (plist-get spec :conversation))
         (function (drover-engine-property (car conversation) :changes))
         (token (cl-incf drover-changes--token)))
    (remhash conversation drover-changes--active)
    (when (functionp function)
      (puthash token (list :spec spec
                           :timer (run-at-time drover-changes-timeout nil #'drover-changes--expire token))
               drover-changes--jobs)
      (condition-case err
          (funcall function (cdr conversation) 'session (plist-get spec :directory)
                   (lambda (result)
                     (drover-with-reporting "session changes"
                       (drover-changes--received token result))))
        (error (drover-changes--received token (list :failure (drover-error-message err))))))))

(defun drover-changes--finish (token)
  "End the read TOKEN, letting the next one start; return its job, or nil."
  (when-let* ((job (gethash token drover-changes--jobs)))
    (when (timerp (plist-get job :timer))
      (cancel-timer (plist-get job :timer)))
    (remhash token drover-changes--jobs)
    (drover-changes--arm)
    job))

(defun drover-changes--expire (token)
  "Give up the read TOKEN, not answered in time, from a timer."
  (drover-with-reporting "session changes"
    (drover-changes--received token (list :failure "no answer"))))

(defun drover-changes--received (token result)
  "Keep RESULT, what the engine told of the read TOKEN.
RESULT is (:files FILES :notes NOTES), as an engine's :changes tells
it, or (:failure MESSAGE): a failure is not asked again until the
session changes.  Records of the conversation are taken in again, and
its sessions read again if they changed meanwhile."
  (when-let* ((job (drover-changes--finish token)))
    (let* ((spec (plist-get job :spec))
           (conversation (plist-get spec :conversation)))
      (if (or (not (listp result)) (plist-get result :failure))
          (puthash conversation (plist-get spec :updated) drover-changes--failed)
        (remhash conversation drover-changes--failed)
        (let ((entry (append (list :updated (plist-get spec :updated))
                             (drover-changes--totals (plist-get result :files))
                             (list :time (float-time))))
              (old (drover-changes--entry-of conversation)))
          (drover-changes--keep conversation entry)
          (unless (equal (drover-changes--value entry) (drover-changes--value old))
            (drover-take-in-again (drover-changes--keys conversation (plist-get spec :key))))))
      (when-let* ((session (drover-index-session (plist-get spec :key))))
        (drover-changes--consider session drover-changes-delay)))))

(defun drover-changes--keys (conversation key)
  "Return the keys of the sessions of CONVERSATION the index holds, and KEY.
Records of several front ends may show one conversation: each tells
what it changed."
  (let ((keys (list key)))
    (dolist (other (gethash conversation (drover--index-engines (drover-index))))
      (cl-pushnew other keys :test #'equal))
    keys))

;;;; Starting and stopping

(defun drover-changes--start ()
  "Follow the changes of sessions, once: views asked for them.
Records are taken in again, given those kept."
  (unless drover-changes--started
    (setq drover-changes--started t)
    (drover-changes--table)
    (setq drover-changes-function #'drover-changes--record
          drover-changes-files-function #'drover-changes--files)
    (add-hook 'drover-changed-hook #'drover-changes--changed)
    (add-hook 'kill-emacs-hook #'drover-changes--kill-emacs)
    (unless (zerop (hash-table-count (drover-changes--table)))
      (drover-backend-changed))))

(defun drover-changes-stop ()
  "Stop following the changes of sessions, writing those kept.
Records no longer carry them once taken in again."
  (when (timerp drover-changes--timer)
    (cancel-timer drover-changes--timer))
  (when (timerp drover-changes--save-timer)
    (cancel-timer drover-changes--save-timer))
  (maphash (lambda (_ job) (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
           drover-changes--jobs)
  (drover-changes-save)
  (setq drover-changes--timer nil
        drover-changes--save-timer nil
        drover-changes--started nil
        drover-changes--store nil
        drover-changes--dirty nil)
  (when (eq drover-changes-function #'drover-changes--record)
    (setq drover-changes-function nil))
  (when (eq drover-changes-files-function #'drover-changes--files)
    (setq drover-changes-files-function nil))
  (dolist (table (list drover-changes--wanted drover-changes--active drover-changes--failed
                       drover-changes--queue drover-changes--jobs drover-changes--copies))
    (clrhash table))
  (remove-hook 'drover-changed-hook #'drover-changes--changed)
  (remove-hook 'kill-emacs-hook #'drover-changes--kill-emacs))

(defun drover-changes--kill-emacs ()
  "Write the changes kept as Emacs exits."
  (drover-with-reporting "keeping changes"
    (drover-changes-save)))

(defun drover-changes-unload-function ()
  "Stop following the changes of sessions."
  (drover-changes-stop)
  nil)

(provide 'drover-changes)
;;; drover-changes.el ends here
