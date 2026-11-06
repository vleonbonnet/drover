;;; drover-changes-test.el --- Tests of what sessions changed  -*- lexical-binding: t; -*-

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

;; A fake engine, `drover-changes-test--changes', tells what the fake
;; backend's sessions changed: at once, later on command, or failing.
;; The other host's sessions are in drover-remote-test.el, which starts
;; a second batch Emacs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-changes)

(defvar drover-changes-test-calls nil
  "What the fake engine was asked, newest first, as (ID SCOPE DIRECTORY).")

(defvar drover-changes-test-answers nil
  "What the fake engine answers, as an alist (ID . RESULT).
RESULT `later' holds the callback in `drover-changes-test-pending'; an
ID missing here changed a.el and b.el.")

(defvar drover-changes-test-pending nil
  "Callbacks the fake engine holds, newest first, as (ID . CALLBACK).")

(defvar drover-changes-test-clock 1800000000.0
  "The time of the last activity the tests give sessions.")

(defconst drover-changes-test-files
  (list (list :file "a.el" :status 'modified :additions 10 :deletions 2 :patch "@@")
        (list :file "b.el" :status 'added :additions 2 :deletions 1 :patch "@@"))
  "What the fake engine tells a session changed, unless told otherwise.")

(defun drover-changes-test--changes (id scope directory callback)
  "Answer what conversation ID changed in SCOPE, run in DIRECTORY, through CALLBACK."
  (push (list id scope directory) drover-changes-test-calls)
  (pcase (alist-get id drover-changes-test-answers nil nil #'equal)
    ('later (push (cons id callback) drover-changes-test-pending))
    ('nil (funcall callback (list :files drover-changes-test-files :notes nil)))
    (answer (funcall callback answer))))

(defun drover-changes-test-session (id &rest properties)
  "Return the fake session ID with PROPERTIES, of the fake engine."
  (apply #'drover-test-session id
         (append properties (list :engine 'fake :engine-id id :directory "/work/"
                                  :updated drover-changes-test-clock))))

(defmacro drover-changes-test-with (sessions &rest body)
  "Run BODY with the fake backend serving SESSIONS, their changes followed.
The state is fresh; the fake engine tells the changes."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend ,sessions
                             (drover-test-with-state
                              (drover-test-with-engines
                               (drover-test-register-engine 'fake :changes #'drover-changes-test--changes)
                               (let ((drover-changes-prefetch-function #'drover-changes-prefetch)
                                     (drover-changes-function nil)
                                     (drover-changes-files-function nil)
                                     (drover-changes-delay 0)
                                     (drover-changes-concurrency 2)
                                     (drover-changes-timeout 60)
                                     (drover-changes--store nil)
                                     (drover-changes--dirty nil)
                                     (drover-changes--save-timer nil)
                                     (drover-changes--warned nil)
                                     (drover-changes--started nil)
                                     (drover-changes--timer nil)
                                     (drover-changes--token 0)
                                     (drover-changes--wanted (make-hash-table :test #'equal))
                                     (drover-changes--active (make-hash-table :test #'equal))
                                     (drover-changes--failed (make-hash-table :test #'equal))
                                     (drover-changes--queue (make-hash-table :test #'equal))
                                     (drover-changes--jobs (make-hash-table :test #'eql))
                                     (drover-changes--copies (make-hash-table :test #'eq :weakness 'key))
                                     (drover-changes-test-calls nil)
                                     (drover-changes-test-answers nil)
                                     (drover-changes-test-pending nil)
                                     (drover-changes-test-clock drover-changes-test-clock)
                                     (kill-emacs-hook nil))
                                 (unwind-protect
                                     (progn ,@body)
                                   (drover-changes-stop)))))))

(defun drover-changes-test-settle (&optional seconds)
  "Run changes and timers until the reads settle, SECONDS at most, 5 by default."
  (let ((end (+ (float-time) (or seconds 5))))
    (drover-changed-now)
    (while (and (< (float-time) end)
                (or (timerp drover--change-timer)
                    (and (> (hash-table-count drover-changes--queue) 0)
                         (< (hash-table-count drover-changes--jobs) drover-changes-concurrency))
                    (and (> (hash-table-count drover-changes--jobs) 0)
                         (not drover-changes-test-pending))))
      (accept-process-output nil 0.01)
      (drover-changed-now))
    (drover-changed-now)))

(defun drover-changes-test-turn (id &rest slots)
  "Change the fake session ID as a turn would: SLOTS set, its activity later.
SLOTS is a plist of slot names as keywords and values."
  (setq drover-test-sessions
        (mapcar (lambda (session)
                  (if (not (equal (drover-session-id session) id))
                      session
                    (let ((copy (copy-sequence session)))
                      (cl-loop for (slot value) on slots by #'cddr
                               do (setf (cl-struct-slot-value 'drover-session
                                                              (intern (substring (symbol-name slot) 1))
                                                              copy)
                                        value))
                      (unless (plist-member slots :updated)
                        (setf (drover-session-updated copy) (cl-incf drover-changes-test-clock)))
                      copy)))
                drover-test-sessions))
  (drover-backend-changed 'fake id))

(defun drover-changes-test-of (id)
  "Return the record of the fake session ID the index holds."
  (drover-index-session (cons 'fake id)))

(defun drover-changes-test-ask (&rest ids)
  "Ask, as a list showing them would, what the fake sessions IDS changed."
  (drover-prefetch (mapcar #'drover-changes-test-of ids) '(changes)))

(defun drover-changes-test-asked ()
  "Return the identifiers the fake engine was asked about, oldest first."
  (reverse (mapcar #'car drover-changes-test-calls)))

;;;; Reading

(ert-deftest drover-changes-test-settled-sessions-read ()
  "Sessions shown are read once settled, all their turns; records and recaps tell it."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done)
                                  (drover-changes-test-session "s2" :status 'working))
    (drover-changes-test-ask "s1" "s2")
    (drover-changes-test-settle)
    ;; Its whole session, where it ran; not the one at work.
    (should (equal drover-changes-test-calls '(("s1" session "/work/"))))
    (should (equal (drover-session-changes (drover-changes-test-of "s1")) '(12 3 2)))
    (should-not (drover-session-changes (drover-changes-test-of "s2")))
    (let ((recap nil))
      (drover-recap (drover-changes-test-of "s1") (lambda (value) (setq recap value)))
      (should (equal (plist-get recap :files) '(("a.el" 10 2) ("b.el" 2 1))))
      ;; The turn's own, as its backend tells them.
      (should (equal (plist-get recap :prompt) "Prompt of s1")))
    ;; Asked again, nothing is read: it is known.
    (drover-changes-test-ask "s1")
    (drover-changes-test-settle)
    (should (equal (drover-changes-test-asked) '("s1")))
    ;; The other's turn ends: it is read then.
    (drover-changes-test-turn "s2" :status 'done)
    (drover-changes-test-settle)
    (should (equal (drover-changes-test-asked) '("s1" "s2")))
    (should (equal (drover-session-changes (drover-changes-test-of "s2")) '(12 3 2)))))

(ert-deftest drover-changes-test-read-again-as-turns-end ()
  "A session is read again once a turn ends, never while it works."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done)
                                  ;; A backend telling no activity.
                                  (drover-changes-test-session "s2" :status 'idle :updated nil))
    (drover-changes-test-ask "s1" "s2")
    (drover-changes-test-settle)
    (should (equal (drover-changes-test-asked) '("s1" "s2")))
    (drover-changes-test-turn "s1" :status 'working)
    (drover-changes-test-turn "s2" :status 'working :updated nil)
    (drover-changes-test-settle)
    (should (equal (drover-changes-test-asked) '("s1" "s2")))
    (setq drover-changes-test-answers
          (list (cons "s1" (list :files (list (list :file "c.el" :additions 1 :deletions 0))))))
    (drover-changes-test-turn "s1" :status 'done)
    (drover-changes-test-turn "s2" :status 'idle :updated nil)
    (drover-changes-test-settle)
    (should (equal (drover-changes-test-asked) '("s1" "s2" "s1" "s2")))
    (should (equal (drover-session-changes (drover-changes-test-of "s1")) '(1 0 1)))
    ;; A session changing nothing tells nothing.
    (setq drover-changes-test-answers (list (cons "s1" (list :files nil))))
    (drover-changes-test-turn "s1" :status 'idle)
    (drover-changes-test-settle)
    (should-not (drover-session-changes (drover-changes-test-of "s1")))))

(ert-deftest drover-changes-test-a-few-at-once-the-newest-first ()
  "Reads run a few at once, the most recently active first."
  (drover-changes-test-with (mapcar (lambda (index)
                                      (drover-changes-test-session (format "s%d" index) :status 'idle
                                                                   :updated (+ drover-changes-test-clock index)))
                                    '(1 2 3 4))
    (setq drover-changes-test-answers (mapcar (lambda (index) (cons (format "s%d" index) 'later)) '(1 2 3 4)))
    (drover-changes-test-ask "s1" "s2" "s3" "s4")
    (drover-changes-test-settle 0.3)
    (should (equal (drover-changes-test-asked) '("s4" "s3")))
    (pcase-dolist (`(,_ . ,callback) drover-changes-test-pending)
      (funcall callback (list :files drover-changes-test-files)))
    (setq drover-changes-test-pending nil)
    (drover-changes-test-settle 0.3)
    (should (equal (drover-changes-test-asked) '("s4" "s3" "s2" "s1")))
    (should (equal (drover-session-changes (drover-changes-test-of "s4")) '(12 3 2)))))

(ert-deftest drover-changes-test-failures-wait-for-a-change ()
  "A session that could not be read, or not in time, is read again once it changes."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done)
                                  (drover-changes-test-session "s2" :status 'done))
    (setq drover-changes-test-answers (list (cons "s1" (list :failure "No transcript"))
                                            (cons "s2" 'later)))
    (let ((drover-changes-timeout 0.05))
      (drover-changes-test-ask "s1" "s2")
      (drover-changes-test-settle 0.3)
      (let ((end (+ (float-time) 2)))
        (while (and (> (hash-table-count drover-changes--jobs) 0) (< (float-time) end))
          (accept-process-output nil 0.01)))
      (should (zerop (hash-table-count drover-changes--jobs)))
      ;; Answered too late, it is dropped.
      (funcall (cdr (pop drover-changes-test-pending)) (list :files drover-changes-test-files)))
    (should-not (drover-session-changes (drover-changes-test-of "s1")))
    (should-not (drover-session-changes (drover-changes-test-of "s2")))
    (drover-changes-test-ask "s1" "s2")
    (drover-changes-test-settle)
    (should (equal (sort (drover-changes-test-asked) #'string<) '("s1" "s2")))
    (setq drover-changes-test-answers nil)
    (drover-changes-test-turn "s1" :status 'idle)
    (drover-changes-test-turn "s2" :status 'idle)
    (drover-changes-test-settle)
    (should (equal (sort (drover-changes-test-asked) #'string<) '("s1" "s1" "s2" "s2")))
    (should (equal (drover-session-changes (drover-changes-test-of "s2")) '(12 3 2)))))

(ert-deftest drover-changes-test-only-what-engines-tell ()
  "Sessions whose engine tells no changes, or of another host, are not read here."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done :engine 'mute)
                                  (drover-changes-test-session "s2" :status 'done :engine-id nil)
                                  (drover-changes-test-session "s3" :status 'done :host "core"))
    (drover-test-register-engine 'mute)
    (let ((records (list (drover-changes-test-of "s1") (drover-changes-test-of "s2")
                         (drover-changes-test-of "s3"))))
      (drover-changes-prefetch records)
      (drover-changes-test-settle 0.2)
      (should-not drover-changes-test-calls)
      (should (seq-every-p (lambda (record) (eq record (drover-index-session (drover-session-key record))))
                           records)))))

(ert-deftest drover-changes-test-bounded ()
  "Thousands of files and names of 500 kB are kept bounded, every file counted."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done))
    (let ((huge (make-string 500000 ?x)))
      (setq drover-changes-test-answers
            (list (cons "s1" (list :files (cons (list :file (concat huge "\nsecond") :additions 1 :deletions 1)
                                                (mapcar (lambda (index)
                                                          (list :file (format "file-%d.el" index)
                                                                :additions 1 :deletions 0))
                                                        (number-sequence 1 3000)))))))
      (drover-changes-test-ask "s1")
      (drover-changes-test-settle)
      (should (equal (drover-session-changes (drover-changes-test-of "s1")) '(3001 1 3001)))
      (let ((files (drover-changes--files (drover-changes-test-of "s1"))))
        (should (<= (length files) drover-changes-file-limit))
        (should (<= (apply #'+ (mapcar (lambda (file) (length (car file))) files))
                    drover-changes-names-limit))
        (should (string-prefix-p "xxx" (car (car files))))
        (should-not (string-search "second" (car (car files)))))
      (setq drover-changes-test-answers nil)
      (drover-test-prints-in-proportion 1 (list (cons "a record" (drover-changes-test-of "s1")))))))

;;;; Kept across restarts

(ert-deftest drover-changes-test-kept-across-restarts ()
  "What was read is kept: Emacs starting again reads only the sessions that changed."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done)
                                  (drover-changes-test-session "s2" :status 'done))
    (drover-changes-test-ask "s1" "s2")
    (drover-changes-test-settle)
    (should (equal (sort (drover-changes-test-asked) #'string<) '("s1" "s2")))
    (drover-changes-stop)
    (should (file-exists-p (drover-changes-file)))
    ;; A new Emacs: the index taken in anew, s2 changed meanwhile.
    (setq drover--index nil
          drover-changes-test-calls nil)
    (drover-changes-test-turn "s2" :status 'idle)
    (drover-changed-now)
    (should-not (drover-session-changes (drover-changes-test-of "s1")))
    (drover-changes-test-ask "s1" "s2")
    (drover-changes-test-settle)
    (should (equal (drover-session-changes (drover-changes-test-of "s1")) '(12 3 2)))
    (should (equal (drover-changes-test-asked) '("s2")))
    (should (equal (drover-session-changes (drover-changes-test-of "s2")) '(12 3 2)))))

(ert-deftest drover-changes-test-newest-kept ()
  "The file keeps the newest conversations, and one that does not read is reported once."
  (drover-changes-test-with (list (drover-changes-test-session "s1" :status 'done))
    (make-directory drover-state-directory t)
    (with-temp-file (drover-changes-file)
      (insert "(:version 1 :changes ((fake \"s1\" :updated nope"))
    (let ((warnings nil))
      (cl-letf (((symbol-function 'drover-warn)
                 (lambda (format &rest arguments) (push (apply #'format-message format arguments) warnings))))
        (drover-changes-test-ask "s1")
        (drover-changes--table)
        (should (= (length warnings) 1))
        (should (string-match-p "Ignoring the changes kept" (car warnings)))))
    ;; Not written over until something was read.
    (drover-changes-save)
    (should (string-suffix-p "nope" (with-temp-buffer
                                      (insert-file-contents (drover-changes-file))
                                      (buffer-string))))
    (drover-changes-test-settle)
    (drover-changes-save)
    (should (equal (mapcar (lambda (entry) (list (car entry) (cadr entry)))
                           (drover-read-data (drover-changes-file) 1 :changes))
                   '((fake "s1"))))
    (let ((drover-changes-limit 2))
      (dolist (id '("t1" "t2" "t3"))
        (drover-changes--keep (cons 'fake id) (list :updated 1.0 :additions 1 :deletions 0 :count 1
                                                    :files '(("a.el" 1 0)) :time (float-time))))
      (drover-changes-save)
      (should (equal (sort (mapcar #'cadr (drover-read-data (drover-changes-file) 1 :changes)) #'string<)
                     '("t2" "t3"))))))

;;;; Asking

(ert-deftest drover-changes-test-prefetch-by-kind ()
  "Views ask what sessions changed as they ask their details, each as its kinds say."
  (drover-test-with-backend (list (drover-test-session "a") (drover-test-session "b" :host "core"))
                            (let* ((local nil)
                                   (elsewhere nil)
                                   (drover-changes-prefetch-function (lambda (sessions) (push (mapcar #'drover-session-id sessions) local))))
                              (drover-test-register-simple 'proxy (list (drover-session-create :backend 'proxy :id "p" :host "core"))
                                                           :prefetch-changes (lambda (sessions)
                                                                               (push (mapcar #'drover-session-id sessions) elsewhere)))
                              (let ((sessions (append drover-test-sessions
                                                      (list (drover-session-create :backend 'proxy :id "p" :host "core")))))
                                (drover-prefetch sessions '(details))
                                (should (equal drover-test-prefetched '(("a" "b"))))
                                (should-not local)
                                (should-not elsewhere)
                                (drover-prefetch sessions '(changes))
                                (should (equal drover-test-prefetched '(("a" "b"))))
                                ;; This host's own, those of another host through their backend.
                                (should (equal local '(("a"))))
                                (should (equal elsewhere '(("p"))))
                                (drover-prefetch sessions)
                                (should (= (length drover-test-prefetched) 2))
                                (should (= (length local) 2))))))

(provide 'drover-changes-test)
;;; drover-changes-test.el ends here
