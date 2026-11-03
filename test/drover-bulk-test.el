;;; drover-bulk-test.el --- Tests of actions on several sessions  -*- lexical-binding: t; -*-

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

;; An action on several sessions runs in slices from a timer, a few
;; requests in flight at most, its progress in the echo area, stoppable,
;; reported once (`drover--bulk-start').  The backend `remote' here
;; confirms its archives later, as OpenCode does: its requests wait in
;; `drover-bulk-test-asked' until the test answers them.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-list)

(defvar drover-bulk-test-asked nil
  "Archives the backend `remote' was asked, newest first, as (ID THEN ELSE).")

(defvar drover-bulk-test-archived nil
  "Identifiers of the sessions of `remote' archived.")

(defvar drover-bulk-test-buffers nil
  "Buffers of the sessions of the test backends, as an alist (ID . BUFFER).")

(defun drover-bulk-test-sessions (backend count &rest properties)
  "Return COUNT sessions of BACKEND, with PROPERTIES; \"s0\" is the newest."
  (cl-loop for index below count
           collect (apply #'drover-session-create :backend backend :id (format "s%d" index)
                          :title (format "Session %d" index) :updated (- 1000.0 index)
                          :engine 'claude-code :engine-id (format "conv-%s-%d" backend index)
                          properties)))

(defun drover-bulk-test-buffer (session)
  "Return the live buffer of SESSION in the tests, or nil."
  (let ((buffer (alist-get (drover-session-id session) drover-bulk-test-buffers nil nil #'equal)))
    (and (buffer-live-p buffer) buffer)))

(defun drover-bulk-test-close (session)
  "Close the buffer of SESSION, as :close does."
  (when-let* ((buffer (drover-bulk-test-buffer session)))
    (drover-kill-buffer buffer)
    nil))

(defmacro drover-bulk-test-with (count &rest body)
  "Run BODY with the backend `remote' serving COUNT sessions, each with a buffer.
`remote' archives its sessions itself, confirming each later; `sessions'
is bound to their records.  The answers and buffers are forgotten after."
  (declare (indent 1) (debug t))
  `(drover-test-with-backend nil
     (drover-test-with-state
       (let* ((drover-bulk-test-asked nil)
              (drover-bulk-test-archived nil)
              (all (drover-bulk-test-sessions 'remote ,count))
              (drover-bulk-test-buffers
               (mapcar (lambda (session)
                         (cons (drover-session-id session)
                               (generate-new-buffer (format " *bulk %s*" (drover-session-id session)))))
                       all))
              (kill-ring nil)
              (interprogram-cut-function nil))
         (setq drover--backends nil)
         (drover-test-register-simple
          'remote (lambda () (seq-remove (lambda (session)
                                           (member (drover-session-id session) drover-bulk-test-archived))
                                         all))
          :session (lambda (id)
                     (unless (member id drover-bulk-test-archived)
                       (seq-find (lambda (session) (equal (drover-session-id session) id)) all)))
          :buffer #'drover-bulk-test-buffer
          :close #'drover-bulk-test-close
          :archive-confirms t
          :archive (lambda (session then else)
                     (push (list (drover-session-id session) then else) drover-bulk-test-asked)))
         (unwind-protect
             (let ((sessions (mapcar (lambda (session) (drover-find 'remote (drover-session-id session)))
                                     all)))
               (ignore sessions)
               ,@body)
           (pcase-dolist (`(,_ . ,buffer) drover-bulk-test-buffers)
             (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun drover-bulk-test-answer (&optional count refuse)
  "Answer the COUNT oldest archives asked and not answered, all by default.
Those whose identifiers REFUSE lists are refused."
  (let ((asked (reverse drover-bulk-test-asked)))
    (setq drover-bulk-test-asked nil)
    (let ((answering (if count (seq-take asked count) asked)))
      (setq drover-bulk-test-asked (reverse (nthcdr (length answering) asked)))
      (pcase-dolist (`(,id ,then ,else) answering)
        (if (member id refuse)
            (funcall else (concat "Refused\n" (make-string 500 ?x)))
          (push id drover-bulk-test-archived)
          (drover-backend-changed 'remote id)
          (funcall then))))))

(defun drover-bulk-test-settle (&optional seconds)
  "Run the slices due, for SECONDS, a tenth by default."
  (let ((end (+ (float-time) (or seconds 0.1))))
    (while (< (float-time) end)
      (accept-process-output nil 0.005))))

(defmacro drover-bulk-test-telling (&rest body)
  "Run BODY with messages and warnings pushed to `messages' and `warnings'."
  (declare (indent 0) (debug t))
  `(let ((messages nil)
         (warnings nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest arguments)
                  (when format-string
                    (push (apply #'format-message format-string arguments) messages))))
               ((symbol-function 'drover-warn)
                (lambda (format-string &rest arguments)
                  (push (apply #'format-message format-string arguments) warnings))))
       ,@body)))

(defun drover-bulk-test-text ()
  "Return the text of the list, without what belongs to one drawing.
Its sections, and the faces font lock gives and takes."
  (let ((text (buffer-string)))
    (remove-list-of-text-properties 0 (length text) '(magit-section face fontified) text)
    text))

(ert-deftest drover-bulk-test-requests-in-flight ()
  "Requests answered later go a few at a time, the next as answers come."
  (drover-bulk-test-with 30
                         (let ((drover-bulk-requests 8))
                           (drover-bulk-test-telling
                            (drover-archive sessions)
                            ;; The command returns once the first are asked.
                            (should (= (length drover-bulk-test-asked) 8))
                            (should drover--bulk-jobs)
                            (should-not messages)
                            (drover-bulk-test-answer 3)
                            (drover-bulk-test-settle)
                            (should (= (length drover-bulk-test-asked) 8))
                            (let ((most 8))
                              (while drover-bulk-test-asked
                                (drover-bulk-test-answer 2)
                                (drover-bulk-test-settle 0.02)
                                (setq most (max most (length drover-bulk-test-asked))))
                              (should (= most 8)))
                            (should (drover-test-wait-actions 2))
                            (should (= (length drover-bulk-test-archived) 30))
                            ;; Every buffer closed, and one report.
                            (should-not (seq-some (lambda (entry) (buffer-live-p (cdr entry))) drover-bulk-test-buffers))
                            (should (equal (seq-filter (lambda (text) (string-prefix-p "Archived" text)) messages)
                                           '("Archived 30 sessions")))
                            (should-not warnings)))))

(ert-deftest drover-bulk-test-failures-reported-once ()
  "Sessions refused are named, a few, the others counted; the rest go on."
  (drover-bulk-test-with 20
                         (drover-bulk-test-telling
                          (drover-archive sessions)
                          (while drover-bulk-test-asked
                            (drover-bulk-test-answer nil '("s1" "s3" "s5" "s7" "s9"))
                            (drover-bulk-test-settle 0.02))
                          (should (drover-test-wait-actions 2))
                          (should (= (length drover-bulk-test-archived) 15))
                          ;; Refused, a buffer stays.
                          (should (buffer-live-p (alist-get "s1" drover-bulk-test-buffers nil nil #'equal)))
                          (should-not (buffer-live-p (alist-get "s2" drover-bulk-test-buffers nil nil #'equal)))
                          (should (= (length warnings) 1))
                          (should (string-match-p
                                   (concat "\\`Archived 15 sessions; 5 failed: Session 1 (Refused), "
                                           "Session 3 (Refused), Session 5 (Refused) and 2 more\\'")
                                   (car warnings))))))

(ert-deftest drover-bulk-test-progress ()
  "A long action tells how far it is, now and then, out of the log."
  (drover-bulk-test-with 20
                         (let ((drover--bulk-progress-delay 0)
                               (drover--bulk-progress-interval 0)
                               (logged nil))
                           (drover-bulk-test-telling
                            (cl-letf* ((message (symbol-function 'message))
                                       ((symbol-function 'message)
                                        (lambda (&rest arguments)
                                          (push message-log-max logged)
                                          (apply message arguments))))
                              (drover-archive sessions)
                              (drover-bulk-test-answer 8)
                              (drover-bulk-test-settle)
                              (let ((progress (seq-filter (lambda (text) (string-prefix-p "Archiving" text)) messages)))
                                (should progress)
                                (should (string-match-p "\\`Archiving 8/20 sessions…  M-x drover-stop-actions stops\\'"
                                                        (car progress))))
                              (should (memq nil logged))
                              (drover-stop-actions)
                              (drover-bulk-test-answer)
                              (should (drover-test-wait-actions 2)))))))

(ert-deftest drover-bulk-test-stopped ()
  "Stopped, an action asks no more; those asked settle, and it says what it left."
  (drover-bulk-test-with 30
                         (drover-bulk-test-telling
                          (drover-archive sessions)
                          (should (= (length drover-bulk-test-asked) 8))
                          (drover-stop-actions)
                          (drover-bulk-test-answer)
                          (should (drover-test-wait-actions 2))
                          (should (= (length drover-bulk-test-archived) 8))
                          (should-not drover-bulk-test-asked)
                          (should (equal (car messages) "Archived 8 sessions; stopped, 22 left as they were"))
                          ;; The buffers of those archived closed, the others' stay.
                          (should-not (buffer-live-p (alist-get "s0" drover-bulk-test-buffers nil nil #'equal)))
                          (should (buffer-live-p (alist-get "s20" drover-bulk-test-buffers nil nil #'equal)))
                          (should-error (drover-stop-actions) :type 'user-error))))

(ert-deftest drover-bulk-test-timeout ()
  "A session whose answer never comes fails, so that the others go on."
  (drover-bulk-test-with 3
                         (let ((drover-bulk-timeout 0.05))
                           (drover-bulk-test-telling
                            (drover-archive sessions)
                            (drover-bulk-test-answer 2)
                            (should (drover-test-wait-actions 3))
                            (should (string-match-p "\\`Archived 2 sessions; failed for Session 2 (no answer after 0.05 seconds)"
                                                    (car warnings)))))))

(ert-deftest drover-bulk-test-idempotent ()
  "A session listed twice, or acted on by an action still running, is acted on once."
  (drover-bulk-test-with 10
                         (drover-bulk-test-telling
                          (drover-archive (append sessions sessions))
                          ;; Archived again while the first archive runs: nothing more asked.
                          (drover-archive sessions)
                          (drover-bulk-test-settle)
                          (should (= (length drover-bulk-test-asked) 8))
                          (while drover-bulk-test-asked
                            (drover-bulk-test-answer)
                            (drover-bulk-test-settle 0.02))
                          (should (drover-test-wait-actions 2))
                          (should (= (length drover-bulk-test-archived) 10))
                          (should (equal (seq-filter (lambda (text) (string-match-p "\\`Archiv\\(?:ed\\|ing 10 sessions already\\)" text))
                                                     messages)
                                         '("Archived 10 sessions" "Archiving 10 sessions already")))
                          ;; Archived already, they are archived without asking.
                          (setq messages nil)
                          (let ((drover-show-archived t))
                            (drover-archive (mapcar (lambda (session)
                                                      (let ((copy (copy-sequence session)))
                                                        (setf (drover-session-archived copy) 1.0)
                                                        copy))
                                                    sessions)))
                          (should-not drover-bulk-test-asked)
                          (should (equal messages '("Archived 10 sessions"))))))

(ert-deftest drover-bulk-test-slices ()
  "Work in Emacs goes in slices: the buffers closed, drover's own archive written once."
  (let ((all (drover-bulk-test-sessions 'shown 40))
        (drover--bulk-slice 0)
        (writes 0)
        (slices 0))
    (drover-test-with-backend nil
      (drover-test-with-state
        (let ((drover-bulk-test-buffers
               (mapcar (lambda (session)
                         (cons (drover-session-id session) (generate-new-buffer " *bulk*")))
                       all)))
          (unwind-protect
              (progn
                (drover-test-register-simple 'shown (lambda () all)
                                             :buffer #'drover-bulk-test-buffer
                                             :close #'drover-bulk-test-close)
                (cl-letf* ((write (symbol-function 'drover-write-data))
                           ((symbol-function 'drover-write-data)
                            (lambda (&rest arguments) (cl-incf writes) (apply write arguments)))
                           (run (symbol-function 'drover--bulk-run))
                           ((symbol-function 'drover--bulk-run)
                            (lambda () (cl-incf slices) (funcall run))))
                  (drover-bulk-test-telling
                   (drover-archive (mapcar (lambda (session) (drover-find 'shown (drover-session-id session)))
                                           all))
                   ;; A slice acts on a few at least, and returns.
                   (should (<= (seq-count (lambda (entry) (not (buffer-live-p (cdr entry))))
                                          drover-bulk-test-buffers)
                               drover--bulk-least))
                   (should drover--bulk-jobs)
                   (should (drover-test-wait-actions 5))
                   (should (>= slices (/ 80 drover--bulk-least)))
                   (should-not (seq-some (lambda (entry) (buffer-live-p (cdr entry))) drover-bulk-test-buffers))
                   (should (= writes 1))
                   (should (= (length (drover--archive)) 40))
                   (should-not (drover-sessions))
                   (should (equal (car messages) "Archived 40 sessions")))))
            (pcase-dolist (`(,_ . ,buffer) drover-bulk-test-buffers)
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest drover-bulk-test-notes-counted ()
  "Of many buffers keeping input or closing later, the report counts them."
  (should (equal (drover--archive-notes '((kept . "A") (closed . "B") (kept . "C")))
                 '("the unsent input of A is in the kill ring" "the unsent input of C is in the kill ring")))
  (should (equal (drover--archive-notes '((kept . "A") (kept . "B") (later . "C") (waiting . "D")
                                          ((failed . "Busy") . "E") ((failed . "Gone") . "F")))
                 '("the unsent input of 2 buffers is in the kill ring" "1 buffer closes once its turn ends"
                   "1 session waits for you: its buffer closes once its turn ends"
                   "2 buffers stay open: Busy"))))

(ert-deftest drover-bulk-test-confirmation-bounded ()
  "A confirmation names a few sessions, and counts the others."
  (let ((asked nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (setq asked prompt) t)))
      (drover--confirm "Interrupt" (drover-bulk-test-sessions 'fake 20)))
    (should (equal asked (concat "Interrupt 20 sessions: Session 0, Session 1, Session 2, Session 3, "
                                 "Session 4 and 15 more? ")))))

(ert-deftest drover-bulk-test-prints-in-proportion ()
  "An action running prints in proportion to its sessions, its callbacks too."
  (drover-bulk-test-with 50
                         (drover-archive sessions)
                         (should drover--bulk-jobs)
                         (drover-test-prints-in-proportion 50 (list (cons "a callback" (nth 1 (car drover-bulk-test-asked)))))
                         (drover-stop-actions)
                         (drover-bulk-test-answer)
                         (should (drover-test-wait-actions 2))))

(ert-deftest drover-bulk-test-one-list-update ()
  "Sessions leaving as answers come are drawn once, in place or whole, as the list drawn anew."
  (drover-bulk-test-with 200
                         (let ((drover-group-by nil)
                               (drover-sort-by 'updated)
                               (drover-list-age-interval nil)
                               (drover-list-cache-interval nil)
                               (drover-list-deferred-delay nil)
                               (drover-refresh-delay 0.5)
                               (list (drover-list--buffer)))
                           (unwind-protect
                               (save-window-excursion
                                 (delete-other-windows)
                                 (set-window-buffer (selected-window) list)
                                 (with-current-buffer list
                                   (drover-list--revert t)
                                   (drover-changed-now)
                                   ;; The list lists every session anew once shown.
                                   (drover-bulk-test-settle)
                                   (dolist (count '(100 100))
                                     (let* ((hooks 0)
                                            (count-hook (lambda () (cl-incf hooks)))
                                            (renders 0)
                                            (render (symbol-function 'drover-list--render)))
                                       (cl-letf (((symbol-function 'drover-list--render)
                                                  (lambda () (cl-incf renders) (funcall render))))
                                         (add-hook 'drover-changed-hook count-hook)
                                         (drover-bulk-test-telling
                                          (drover-archive (seq-take (drover-index-sessions) count))
                                          (while drover-bulk-test-asked
                                            (drover-bulk-test-answer 4)
                                            (drover-bulk-test-settle 0.005))
                                          (should (drover-test-wait-actions 2))
                                          (drover-bulk-test-settle 0.6)))
                                       (should (= hooks 1))
                                       ;; Half leave in place; the other half, emptying it, drawn whole.
                                       (should (= renders (if (drover-index-sessions) 0 1)))
                                       (let ((text (drover-bulk-test-text)))
                                         (drover-list--render)
                                         (should (equal-including-properties text (drover-bulk-test-text))))
                                       (remove-hook 'drover-changed-hook count-hook)))
                                   (should (= (hash-table-count (drover-list--model-rows drover-list--model)) 0))))
                             (kill-buffer list)))))

(provide 'drover-bulk-test)
;;; drover-bulk-test.el ends here
