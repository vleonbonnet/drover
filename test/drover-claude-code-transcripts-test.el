;;; drover-claude-code-transcripts-test.el --- Tests of Claude Code's closed conversations  -*- lexical-binding: t; -*-

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

;; The `claude-code' backend over transcripts of the fixtures, in the
;; throwaway homes of `drover-test-with-homes'; its index is looked at
;; synchronously, `drover-transcript-scan', rather than from timers, and
;; no front end is loaded: the resumers are stubs.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-list)

(defmacro drover-claude-code-transcripts-test-with (&rest body)
  "Run BODY with the `claude-code' backend alone, over the fixtures' homes.
Its index is fresh, the transcripts' directory and drover's state
empty; `home' is the homes' directory, and `index' the index."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
     (drover-test-with-backend nil
       (let ((drover-claude-code-transcripts--users nil)
             (drover-claude-code-transcripts--index nil)
             (drover-claude-code-transcripts--agents (make-hash-table :test #'equal))
             (drover-claude-code-transcripts--recent nil)
             (drover-claude-code-agents-interval nil)
             (drover-claude-code-history-days nil)
             (drover--resumers nil)
             (drover--resumer-properties nil)
             (drover-resume-front-ends nil))
         (setq drover--backends nil)
         (drover-claude-code-transcripts-install 'test)
         (let ((index drover-claude-code-transcripts--index))
           (ignore index)
           (unwind-protect
               (progn ,@body)
             (drover-claude-code-transcripts-uninstall 'test)))))))

(defun drover-claude-code-transcripts-test-scan ()
  "Look at the transcripts now, and follow the changes reported."
  (drover-transcript-scan drover-claude-code-transcripts--index)
  (drover-changed-now))

(defun drover-claude-code-transcripts-test-ids ()
  "Return the identifiers of the sessions drover lists, sorted."
  (sort (mapcar #'drover-session-id (drover-sessions)) #'string<))

(defun drover-claude-code-transcripts-test-at-point (ids)
  "Return the identifiers of the sessions at point on the lines of IDS in the list.
The list is drawn anew, its sessions of collapsed groups too, and
killed after."
  (let ((buffer (drover-list--buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (revert-buffer)
          (drover-list--draw-deferred)
          (mapcar (lambda (id)
                    (goto-char (oref (drover-list--find-session (cons 'claude-code id)) start))
                    (drover-session-id (drover-session-at-point)))
                  ids))
      (kill-buffer buffer))))

(defmacro drover-claude-code-transcripts-test-counting-reads (&rest body)
  "Run BODY, and return the bytes of transcripts it read, as (BYTES . VALUE)."
  (declare (indent 0) (debug t))
  `(let ((bytes 0)
         (insert (symbol-function 'drover-transcript-insert)))
     (cl-letf (((symbol-function 'drover-transcript-insert)
                (lambda (file beg end)
                  (cl-incf bytes (- end beg))
                  (funcall insert file beg end))))
       (let ((value (progn ,@body)))
         (cons bytes value)))))

;;;; Reading transcripts

(ert-deftest drover-claude-code-transcripts-test-record ()
  "A transcript tells its conversation as the CLI's picker reads it."
  (drover-claude-code-transcripts-test-with
    (drover-test-write-claude-catalog)
    (let ((project (expand-file-name "work/app/" home)))
      (drover-fixture-claude "a1" project :ai-title "Keep the login page" :effort "xhigh"
                             :mode "plan" :branch "fix-login")
      (drover-fixture-claude "b2" project :prompt "Bump the SDK\nto 2.0" :model "claude-sonnet-5-5")
      (drover-claude-code-transcripts-test-scan)
      (let ((a (drover-find 'claude-code "a1"))
            (b (drover-find 'claude-code "b2")))
        (should (equal (list (drover-session-title a) (drover-session-directory a)
                             (drover-session-branch a) (drover-session-status a)
                             (drover-session-engine a) (drover-session-engine-id a)
                             (drover-session-model a) (drover-session-effort a) (drover-session-agent a)
                             (drover-session-created a) (drover-session-updated a))
                       (list "Keep the login page" project "fix-login" 'idle 'claude-code "a1"
                             "Opus 5.5" "xhigh" "plan" 1790000000.0 1790000002.0)))
        ;; Without a title, its first prompt's first line: not the
        ;; command note before it.
        (should (equal (drover-session-title b) "Bump the SDK"))
        (should (equal (drover-session-model b) "Sonnet 5.5"))
        (should-not (drover-session-agent b))
        (should-not (drover-session-buffer b))))))

(ert-deftest drover-claude-code-transcripts-test-not-prompts ()
  "Commands, their output and the note of an interruption title no conversation."
  (dolist (text '("<command-name>/plugin</command-name>" "<local-command-stdout>ok</local-command-stdout>"
                  "[Request interrupted by user]" "[Request interrupted by user for tool use]"))
    (should-not (drover-claude-code--prompt-p
                 `((type . "user") (message (content ((type . "text") (text . ,text))))))))
  (should (equal (drover-claude-code--prompt-p
                  '((type . "user") (message (content . "Why was the request interrupted?"))))
                 "Why was the request interrupted?")))

(ert-deftest drover-claude-code-transcripts-test-titles ()
  "The title the user gave wins over the one the CLI generated, the newest of each."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "t1" project :ai-title "Old title" :custom-title "Mine")))
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-session-title (drover-find 'claude-code "t1")) "Mine"))
      (drover-fixture-claude-append file (drover-fixture-claude-meta "t1" "custom-title" :customTitle "Renamed"))
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-session-title (drover-find 'claude-code "t1")) "Renamed")))))

(ert-deftest drover-claude-code-transcripts-test-unlisted ()
  "A transcript of no message, as a session that ran a command and left, is not listed."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude-file "c3" project)))
      (make-directory project t)
      (drover-fixture-write file (list (drover-fixture-claude-meta "c3" "mode" :mode "normal")
                                       (drover-fixture-claude-user "c3" project 1790000000.0
                                                                   "<command-name>/model</command-name>"
                                                                   :meta t)))
      (drover-fixture-claude "d4" project)
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-claude-code-transcripts-test-ids) '("d4"))))))

(ert-deftest drover-claude-code-transcripts-test-large-file ()
  "A large transcript is read by its head and tail, then by what was appended."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "big" project :filler 3000000 :ai-title "Big one"
                                        :prompt "Start the migration")))
      (should (> (file-attribute-size (file-attributes file)) 3000000))
      (let ((read (car (drover-claude-code-transcripts-test-counting-reads
                         (drover-claude-code-transcripts-test-scan)))))
        ;; Its tail holds no message: its last one is looked for before,
        ;; through the blobs, as far as the look back goes.
        (should (<= read (+ (* 2 drover-transcript-window) drover-transcript-look-back))))
      (let ((session (drover-find 'claude-code "big")))
        (should (equal (drover-session-title session) "Big one"))
        (should (eql (drover-session-updated session) 1790000002.0)))
      ;; A turn appended: only what was appended is read, and what the
      ;; head told stays.
      (drover-fixture-claude-append
       file
       (drover-fixture-claude-user "big" project 1790000100.0 "Go on")
       (drover-fixture-claude-assistant "big" project 1790000101.0 "Done." :model "claude-sonnet-5-5")
       (drover-fixture-claude-meta "big" "ai-title" :aiTitle "Migration done"))
      (let ((read (car (drover-claude-code-transcripts-test-counting-reads
                         (drover-claude-code-transcripts-test-scan)))))
        (should (< read 4096)))
      (let ((session (drover-find 'claude-code "big")))
        (should (equal (drover-session-title session) "Migration done"))
        (should (eql (drover-session-updated session) 1790000101.0))
        (should (equal (drover-session-model session) "claude-sonnet-5-5"))
        (should (eql (drover-session-created session) 1790000000.0)))
      ;; Nothing changed: nothing is read, a file status apiece.
      (should (zerop (car (drover-claude-code-transcripts-test-counting-reads
                            (drover-claude-code-transcripts-test-scan)))))
      ;; Replaced by a shorter one, it is read whole again.
      (delete-file file)
      (drover-fixture-claude "big" project :ai-title "Started over")
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-session-title (drover-find 'claude-code "big")) "Started over")))))

(ert-deftest drover-claude-code-transcripts-test-reopened ()
  "A conversation reopened is as old as its newest message, whatever the CLI appends.
The CLI appends records of its own as it resumes a conversation, some
with the time of now, and the file changes: its age and its place in the
sort stay those of its newest message, until a new one moves them."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (start 1790000000.0)
           (reopened (+ start (* 14 86400)))
           (file (drover-fixture-claude "r1" project :start start :ai-title "Keep the login page")))
      (drover-fixture-claude "r2" project :start (+ start 86400))
      (drover-claude-code-transcripts-test-scan)
      (drover-index)
      (apply #'drover-fixture-claude-append file (drover-fixture-claude-reopened "r1" project reopened))
      (set-file-times file (seconds-to-time reopened))
      (drover-claude-code-transcripts-test-scan)
      (cl-flet ((order () (mapcar #'drover-session-id (drover-sort (drover-index-sessions)))))
        (let ((session (drover-index-session '(claude-code . "r1"))))
          (should (eql (drover-session-updated session) (+ start 2)))
          (should (eql (drover-list--age-time session) (+ start 2)))
          (should (eql (drover--sort-time session) (+ start 2)))
          (should (equal (drover-session-title session) "Keep the login page")))
        (should (equal (order) '("r2" "r1")))
        ;; Read whole again, it tells the same.
        (should (eql (plist-get (drover-claude-code-transcripts--read file (file-attributes file) nil)
                                :updated)
                     (+ start 2)))
        ;; A new exchange is activity.
        (drover-fixture-claude-append file
                                      (drover-fixture-claude-user "r1" project (+ reopened 60) "Go on")
                                      (drover-fixture-claude-assistant "r1" project (+ reopened 61) "Done."))
        (drover-claude-code-transcripts-test-scan)
        (let ((session (drover-index-session '(claude-code . "r1"))))
          (should (eql (drover-session-updated session) (+ reopened 61)))
          (should (eql (drover--sort-time session) (+ reopened 61))))
        (should (equal (order) '("r1" "r2")))))))

(ert-deftest drover-claude-code-transcripts-test-reopened-large ()
  "The newest message is looked for before a tail the CLI's own records fill.
Records longer than the tail's window, as the attachments of a turn,
may follow it, read whole or appended."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (start 1790000000.0)
           (file (drover-fixture-claude "l1" project :start start :filler 200000)))
      (drover-fixture-claude-append file (drover-fixture-claude-user "l1" project (+ start 100) "More")
                                    (drover-fixture-claude-assistant "l1" project (+ start 101) "Here."))
      (apply #'drover-fixture-claude-append file
             (drover-fixture-line :type "attachment" :timestamp (drover-fixture-time (+ start 102))
                                  :attachment (list :content (make-string (* 2 drover-transcript-window) ?a)))
             (drover-fixture-claude-reopened "l1" project (+ start 86400)))
      (drover-claude-code-transcripts-test-scan)
      (should (eql (drover-session-updated (drover-find 'claude-code "l1")) (+ start 101)))
      ;; Appended to: what came after the size read is looked at, not before.
      (let ((read (car (drover-claude-code-transcripts-test-counting-reads
                         (apply #'drover-fixture-claude-append file
                                (drover-fixture-line :type "attachment"
                                                     :attachment (list :content (make-string 100000 ?b)))
                                (drover-fixture-claude-reopened "l1" project (+ start 90000)))
                         (drover-claude-code-transcripts-test-scan)))))
        (should (< read 200000)))
      (should (eql (drover-session-updated (drover-find 'claude-code "l1")) (+ start 101))))))

(ert-deftest drover-claude-code-transcripts-test-short-head-read-again ()
  "A transcript read before its first prompt was written is read whole once it grew."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude-file "new" project)))
      (make-directory project t)
      (drover-fixture-write file (list (drover-fixture-claude-meta "new" "mode" :mode "normal")))
      (drover-claude-code-transcripts-test-scan)
      (should-not (drover-claude-code-transcripts-test-ids))
      (drover-fixture-claude-append file (drover-fixture-claude-user "new" project 1790000000.0 "First words"))
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-session-title (drover-find 'claude-code "new")) "First words")))))

(ert-deftest drover-claude-code-transcripts-test-subagents ()
  "Subagents' transcripts beside a conversation are its subagents."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home)))
      (drover-fixture-claude "p1" project :subagents '(("agent-a1" "Audit the parser" "Explore")))
      (drover-claude-code-transcripts-test-scan)
      (let ((subagent (drover-find 'claude-code "p1/agent-a1")))
        (should (equal (list (drover-session-title subagent) (drover-session-parent subagent)
                             (drover-session-agent subagent) (drover-session-directory subagent))
                       (list "Audit the parser" "p1" "Explore" project)))
        (should (equal (drover-session-parent (car (drover-subagents (drover-find 'claude-code "p1")
                                                                     (drover-sessions))))
                       "p1"))
        (should (drover-supports-p subagent :recap))
        (should-not (drover-supports-p subagent :delete))
        (should-not (drover-archivable-p subagent))
        (drover-recap subagent (lambda (recap) (should (equal (plist-get recap :reply) "Found it."))))))))

(ert-deftest drover-claude-code-transcripts-test-gone ()
  "A transcript deleted leaves the list, its backend told."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "g1" project)))
      (drover-fixture-claude "g2" project)
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-claude-code-transcripts-test-ids) '("g1" "g2")))
      (delete-file file)
      (drover-claude-code-transcripts-test-scan)
      (should (equal (drover-claude-code-transcripts-test-ids) '("g2")))
      (should-not (drover-claude-code-transcripts--file "g1")))))

(ert-deftest drover-claude-code-transcripts-test-kept-between-sessions ()
  "What was read is kept, and a new Emacs reads only the transcripts that changed."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "k1" project :ai-title "Kept")))
      (drover-fixture-claude "k2" project)
      (drover-claude-code-transcripts-test-scan)
      (drover-transcript-save index t)
      (should (file-exists-p (drover-transcript-cache-file index)))
      ;; A new Emacs: nothing read but the kept file, until a look.
      (let ((drover-claude-code-transcripts--index (drover-claude-code-transcripts--make-index)))
        (drover-transcript-load drover-claude-code-transcripts--index)
        (should (equal (plist-get (drover-transcript-lookup drover-claude-code-transcripts--index file) :ai-title)
                       "Kept"))
        ;; Its conversations are found, and open from their lines.
        (drover-index-refresh)
        (should (equal (drover-claude-code-transcripts-test-ids) '("k1" "k2")))
        (should (equal (drover-session-title (drover-find 'claude-code "k1")) "Kept"))
        (should (equal (drover-claude-code-transcripts-test-at-point '("k1" "k2")) '("k1" "k2")))
        (drover-fixture-claude-append file (drover-fixture-claude-meta "k1" "ai-title" :aiTitle "Changed"))
        ;; Short, it is read whole again, the other not at all.
        (let ((read (car (drover-claude-code-transcripts-test-counting-reads
                           (drover-claude-code-transcripts-test-scan)))))
          (should (= read (file-attribute-size (file-attributes file)))))
        (should (equal (drover-claude-code-transcripts-test-ids) '("k1" "k2")))
        (should (equal (drover-session-title (drover-find 'claude-code "k1")) "Changed"))
        ;; The other, not read again, is still found, and stays listed as it changes.
        (should (drover-find 'claude-code "k2"))
        (drover-backend-changed 'claude-code "k2")
        (drover-changed-now)
        (should (equal (drover-claude-code-transcripts-test-ids) '("k1" "k2")))
        (should (equal (drover-claude-code-transcripts-test-at-point '("k1" "k2")) '("k1" "k2"))))
      ;; A kept file that does not read is ignored, once said.
      (drover-test-write (drover-transcript-cache-file index) "(:version 1 :files (garbage")
      (let ((index (drover-claude-code-transcripts--make-index))
            (warnings nil))
        (cl-letf (((symbol-function 'drover-warn)
                   (lambda (format &rest args) (push (apply #'format-message format args) warnings))))
          (drover-transcript-load index))
        (should (= (length warnings) 1))
        (should (zerop (hash-table-count (drover-transcript-index-entries index))))))))

(ert-deftest drover-claude-code-transcripts-test-check-first ()
  "A transcript a backend looks at is read before the others, once it changed."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "f1" project)))
      (drover-claude-code-transcripts-test-scan)
      (should-not (drover-transcript-check index file))
      (drover-fixture-claude-append file (drover-fixture-claude-meta "f1" "custom-title" :customTitle "Now"))
      (should (drover-transcript-check index file))
      (should (equal (drover-transcript-index-queue index) (list file)))
      ;; The next slice reads it.
      (setf (drover-transcript-index-active index) t)
      (unwind-protect
          (drover-transcript--work index)
        (setf (drover-transcript-index-active index) nil)
        (when (timerp (drover-transcript-index-timer index))
          (cancel-timer (drover-transcript-index-timer index))))
      (should (equal (plist-get (drover-transcript-lookup index file) :custom-title) "Now")))))

(ert-deftest drover-claude-code-transcripts-test-history-days ()
  "With an age set, conversations idle longer are older sessions, listed on demand."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (now 1790864000.0))
      (drover-fixture-claude "old" project :start (- now (* 20 86400)))
      (drover-fixture-claude "new" project :start (- now 3600))
      (cl-letf* ((float-time (symbol-function 'float-time))
                 ((symbol-function 'float-time) (lambda (&optional time) (if time (funcall float-time time) now))))
        (drover-claude-code-transcripts-test-scan)
        (should (equal (drover-claude-code-transcripts-test-ids) '("new" "old")))
        (let ((drover-claude-code-history-days 7))
          (drover-index-refresh)
          (should (equal (drover-claude-code-transcripts-test-ids) '("new")))
          (should-not (drover-find 'claude-code "old"))
          (drover-set-show-history t)
          (unwind-protect
              (should (equal (sort (mapcar #'drover-session-id (drover-known-sessions)) #'string<)
                             '("new" "old")))
            (drover-set-show-history nil)))))))

;;;; Live front ends win

(defun drover-claude-code-transcripts-test-terminal (id)
  "Return a session of the front end `term' following conversation ID."
  (drover-session-create :backend 'term :id (concat "terminal-" id) :title (concat "Terminal " id)
                         :status 'working :engine 'claude-code :engine-id id))

(ert-deftest drover-claude-code-transcripts-test-front-end-wins ()
  "A conversation a front end follows is its session, listed once; closed, it is the transcript's."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (terminals nil))
      (drover-fixture-claude "x1" project :ai-title "Closed conversation")
      (drover-fixture-claude "x2" project)
      (drover-test-register-simple 'term (lambda () terminals)
                                   :session (lambda (id)
                                              (seq-find (lambda (session) (equal (drover-session-id session) id))
                                                        terminals)))
      (drover-claude-code-transcripts-test-scan)
      (drover-index)
      (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                     '((claude-code . "x1") (claude-code . "x2"))))
      ;; A terminal resumes x1: its session stands for the conversation,
      ;; followed session by session, not drawn anew.
      (setq terminals (list (drover-claude-code-transcripts-test-terminal "x1")))
      (let ((changed nil))
        (add-hook 'drover-changed-hook (lambda () (setq changed drover-changed-sessions)))
        (drover-backend-changed 'term "terminal-x1")
        (drover-changed-now)
        (should (listp changed))
        (should (member '(claude-code . "x1") changed))
        (should (member '(term . "terminal-x1") changed))
        (should (drover-index-incremental-p))
        (should (equal (sort (mapcar #'drover-session-key (drover-index-sessions))
                             (lambda (a b) (string< (format "%s" a) (format "%s" b))))
                       '((claude-code . "x2") (term . "terminal-x1"))))
        (should (equal (sort (mapcar #'drover-session-key (drover-sessions))
                             (lambda (a b) (string< (format "%s" a) (format "%s" b))))
                       '((claude-code . "x2") (term . "terminal-x1"))))
        ;; The transcript changing as the terminal writes it changes nothing listed.
        (drover-backend-changed 'claude-code "x1")
        (drover-changed-now)
        (should-not (drover-index-session '(claude-code . "x1")))
        ;; Closed, the conversation is the transcript's again, idle.
        (setq terminals nil)
        (drover-backend-changed 'term "terminal-x1")
        (drover-changed-now)
        (should (listp changed))
        (should (member '(claude-code . "x1") changed))
        (should (eq (drover-session-status (drover-index-session '(claude-code . "x1"))) 'idle))
        ;; A relisting agrees.
        (drover-index-refresh)
        (should (equal (mapcar #'drover-session-key (drover-index-sessions))
                       '((claude-code . "x2") (claude-code . "x1"))))))))

;;;; Opening

(ert-deftest drover-claude-code-transcripts-test-front-end-reopens ()
  "A front end reopening a conversation shows it as old as its newest message.
Its own session began as of now, but the conversation began and was
last active when its transcript tells, until a turn of the front end;
the conversation's new messages, read as they are recorded, move it, the
front end's session taken again."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (start 1790000000.0)
           (opened (+ start (* 14 86400)))
           (file (drover-fixture-claude "y1" project :start start))
           (open nil)
           (activity nil))
      (drover-fixture-claude "y2" project :start (+ start 86400))
      (cl-flet ((terminal ()
                  (let ((times (drover-followed-times 'claude-code "y1" opened activity)))
                    (drover-session-create :backend 'term :id "terminal-y1" :title "Terminal y1"
                                           :status 'idle :engine 'claude-code :engine-id "y1"
                                           :created (car times) :updated (cdr times))))
                (order () (mapcar #'drover-session-id (drover-sort (drover-index-sessions)))))
        (drover-test-register-simple 'term (lambda () (and open (list (terminal))))
                                     :session (lambda (id) (and open (equal id "terminal-y1") (terminal))))
        (drover-claude-code-transcripts-test-scan)
        (drover-index)
        (setq open t)
        (drover-backend-changed 'term "terminal-y1")
        (drover-changed-now)
        (let ((session (drover-index-session '(term . "terminal-y1"))))
          (should (eql (drover-session-created session) start))
          (should (eql (drover-session-updated session) (+ start 2)))
          (should (eql (drover--sort-time session) (+ start 2))))
        (should (equal (order) '("y2" "terminal-y1")))
        ;; What the CLI appends as it resumes changes nothing.
        (apply #'drover-fixture-claude-append file (drover-fixture-claude-reopened "y1" project opened))
        (set-file-times file (seconds-to-time opened))
        (drover-claude-code-transcripts-test-scan)
        (should (eql (drover-session-updated (drover-index-session '(term . "terminal-y1"))) (+ start 2)))
        (should (equal (order) '("y2" "terminal-y1")))
        ;; A new exchange, recorded, moves it, the front end saying nothing.
        (drover-fixture-claude-append file
                                      (drover-fixture-claude-user "y1" project (+ opened 60) "Go on")
                                      (drover-fixture-claude-assistant "y1" project (+ opened 61) "Done."))
        (drover-claude-code-transcripts-test-scan)
        (let ((session (drover-index-session '(term . "terminal-y1"))))
          (should (eql (drover-session-updated session) (+ opened 61)))
          (should (eql (drover--sort-time session) (+ opened 61))))
        (should (equal (order) '("terminal-y1" "y2")))
        ;; The front end's turn, before its transcript tells it.
        (setq activity (+ opened 300))
        (drover-backend-changed 'term "terminal-y1")
        (drover-changed-now)
        (should (eql (drover-session-updated (drover-index-session '(term . "terminal-y1"))) (+ opened 300)))
        ;; A relisting agrees.
        (drover-index-refresh)
        (should (eql (drover-session-updated (drover-index-session '(term . "terminal-y1"))) (+ opened 300)))))))

(ert-deftest drover-claude-code-transcripts-test-open-resumes ()
  "Opening a closed conversation resumes it, in the front end it ran in first."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (resumed nil))
      (drover-fixture-claude "o1" project)
      (drover-fixture-claude "o2" project :entrypoint "sdk-ts")
      (drover-claude-code-transcripts-test-scan)
      (should-error (drover-open (drover-find 'claude-code "o1")) :type 'user-error)
      (drover-register-resumer 'claude-code 'claude-code-ide
                               (lambda (id directory) (push (list 'ide id directory) resumed)))
      (drover-register-resumer 'claude-code 'agent-shell
                               (lambda (id directory) (push (list 'shell id directory) resumed)))
      (drover-open (drover-find 'claude-code "o1"))
      (drover-open (drover-find 'claude-code "o2"))
      (should (equal (reverse resumed) `((ide "o1" ,project) (shell "o2" ,project))))
      ;; The user's preference wins.
      (setq resumed nil)
      (let ((drover-resume-front-ends '((claude-code agent-shell))))
        (drover-open (drover-find 'claude-code "o1")))
      (should (equal resumed `((shell "o1" ,project))))
      ;; A directory gone cannot be resumed in.
      (delete-directory project t)
      (should-error (drover-open (drover-find 'claude-code "o1")) :type 'user-error))))

(ert-deftest drover-claude-code-transcripts-test-open-followed ()
  "A conversation a front end follows already opens there rather than anew."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (terminal (drover-claude-code-transcripts-test-terminal "f1"))
          (opened nil))
      (drover-fixture-claude "f1" project)
      (drover-claude-code-transcripts-test-scan)
      (let ((record (drover-find 'claude-code "f1")))
        (drover-test-register-simple 'term (list terminal)
                                     :open (lambda (session _) (push (drover-session-id session) opened)))
        (drover-register-resumer 'claude-code 'claude-code-ide (lambda (&rest _) (ert-fail "Resumed")))
        (drover-open record)
        (should (equal opened '("terminal-f1")))))))

(ert-deftest drover-claude-code-transcripts-test-recap-and-preview ()
  "The recap and preview of a closed conversation come from its transcript."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home))
          (recap nil)
          (preview nil))
      (drover-fixture-claude "r1" project :prompt "Explain the cache" :reply "It keeps an hour.")
      (drover-claude-code-transcripts-test-scan)
      (let ((session (drover-find 'claude-code "r1")))
        (drover-recap session (lambda (value) (setq recap value)))
        (drover-preview session (lambda (value) (setq preview value)))
        (should (equal (plist-get recap :prompt) "Explain the cache"))
        (should (equal (plist-get recap :reply) "It keeps an hour."))
        (should (equal preview "It keeps an hour."))))))

;;;; Archiving and deleting

(ert-deftest drover-claude-code-transcripts-test-archive-and-restore ()
  "A closed conversation is archived by drover, and restored, listed again."
  (drover-claude-code-transcripts-test-with
    (drover-test-with-state
      (let ((project (expand-file-name "work/" home)))
        (drover-fixture-claude "z1" project :ai-title "Archive me")
        (drover-fixture-claude "z2" project)
        (drover-claude-code-transcripts-test-scan)
        (drover-index)
        (drover-archive (drover-find 'claude-code "z1"))
        (drover-changed-now)
        (should (equal (mapcar #'drover-session-id (drover-index-sessions)) '("z2")))
        (let ((drover-show-archived t))
          (let ((archived (drover-archived-sessions)))
            (should (equal (mapcar #'drover-session-key archived) '((claude-code . "z1"))))
            (should (drover-unarchivable-p (car archived)))
            (drover-unarchive (car archived))))
        (drover-changed-now)
        (should (equal (sort (mapcar #'drover-session-id (drover-index-sessions)) #'string<) '("z1" "z2")))
        (should-not (drover-session-archived-p (drover-find 'claude-code "z1")))))))

(ert-deftest drover-claude-code-transcripts-test-restore-terminal-gone ()
  "A terminal's conversation drover archived, its terminal gone, is restored from its transcript."
  (drover-claude-code-transcripts-test-with
    (drover-test-with-state
      (let ((project (expand-file-name "work/" home))
            (terminals nil))
        (drover-fixture-claude "w1" project :ai-title "Was in a terminal")
        (setq terminals (list (drover-claude-code-transcripts-test-terminal "w1")))
        (drover-test-register-simple 'term (lambda () terminals))
        (drover-claude-code-transcripts-test-scan)
        (drover-archive (drover-find 'term "terminal-w1"))
        (setq terminals nil)
        (drover-backend-changed 'term)
        (drover-changed-now)
        (let ((drover-show-archived t))
          (let ((archived (car (drover-archived-sessions))))
            ;; The transcript's record, which opens.
            (should (equal (drover-session-key archived) '(claude-code . "w1")))
            (should (drover-supports-p archived :open))
            (drover-unarchive archived)))
        (drover-changed-now)
        (should (equal (mapcar #'drover-session-key (drover-sessions)) '((claude-code . "w1"))))))))

(ert-deftest drover-claude-code-transcripts-test-delete ()
  "Deleting a closed conversation deletes its transcript and its directory."
  (drover-claude-code-transcripts-test-with
    (let* ((project (expand-file-name "work/" home))
           (file (drover-fixture-claude "00000000-0000-4000-8000-000000000001" project
                                        :subagents '(("agent-x" "Look around" "Explore"))))
           (directory (file-name-sans-extension file))
           (delete-by-moving-to-trash nil))
      (drover-claude-code-transcripts-test-scan)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (drover-delete (drover-find 'claude-code "00000000-0000-4000-8000-000000000001")))
      (should-not (file-exists-p file))
      (should-not (file-exists-p directory))
      (drover-claude-code-transcripts-test-scan)
      (should-not (drover-sessions)))))

;;;; Sessions Claude Code runs elsewhere

(defconst drover-claude-code-transcripts-test-agents
  "[{\"id\":\"bg000001\",\"cwd\":\"/work\",\"kind\":\"background\",\"startedAt\":1781710972404,
     \"sessionId\":\"b1\",\"name\":\"Long job\",\"state\":\"blocked\"},
    {\"id\":\"it000002\",\"cwd\":\"/work\",\"kind\":\"interactive\",\"startedAt\":1781710972404,
     \"sessionId\":\"b2\",\"name\":\"In iTerm\",\"state\":\"working\"},
    {\"id\":\"bg000003\",\"cwd\":\"/work\",\"kind\":\"background\",\"startedAt\":1781710972404,
     \"sessionId\":\"b3\",\"name\":\"Finished\",\"state\":\"done\"}]"
  "The output of \"claude agents --json\", as Claude Code 2.1.316 prints it.")

(ert-deftest drover-claude-code-transcripts-test-agents ()
  "Sessions Claude Code runs elsewhere show as it says they stand."
  (drover-claude-code-transcripts-test-with
    (let ((project (expand-file-name "work/" home)))
      (dolist (id '("b1" "b2" "b3"))
        (drover-fixture-claude id project))
      (drover-claude-code-transcripts-test-scan)
      (drover-claude-code-transcripts--agents-listed
       (drover-claude-code-transcripts--parse-agents drover-claude-code-transcripts-test-agents))
      (drover-changed-now)
      (let ((b1 (drover-find 'claude-code "b1"))
            (b2 (drover-find 'claude-code "b2"))
            (b3 (drover-find 'claude-code "b3")))
        (should (eq (drover-session-status b1) 'waiting))
        (should (equal (drover-session-summary b1) "permit: waits in the background: claude attach bg000001"))
        (should (eq (drover-session-status b2) 'working))
        (should (equal (drover-session-detail b2) "working in another terminal"))
        ;; A turn running elsewhere is placed at its prompt, which the
        ;; events of its turn do not move; a wait at the newest message.
        (should (equal (drover-session-since b2) 1790000001.0))
        (should (equal (drover-session-since b1) 1790000002.0))
        (should (eq (drover-session-status b3) 'idle))
        ;; Running elsewhere, it is attached to there, not resumed here
        ;; nor deleted.
        (drover-register-resumer 'claude-code 'claude-code-ide (lambda (&rest _) (ert-fail "Resumed")))
        (let ((err (should-error (drover-open b1) :type 'user-error)))
          (should (string-match-p "claude attach bg000001" (cadr err))))
        (should-not (drover-supports-p b2 :delete))
        (should (drover-supports-p b3 :delete))))))

(ert-deftest drover-claude-code-transcripts-test-agents-command ()
  "\"claude agents --json\" runs without waiting, while a list shows, and its answer is followed."
  (drover-claude-code-transcripts-test-with
    (let* ((script (expand-file-name "claude" home))
           (drover-claude-code-program script)
           (drover-claude-code-agents-interval 30)
           (drover-claude-code-transcripts--agents-time 0)
           (drover-claude-code-transcripts--agents-job nil))
      (drover-test-write script (concat "#!/bin/sh\n[ \"$1 $2\" = \"agents --json\" ] || exit 2\ncat <<'EOF'\n"
                                        drover-claude-code-transcripts-test-agents "\nEOF\n"))
      (set-file-modes script #o755)
      ;; No list shows: nothing runs.
      (drover-claude-code-transcripts--ask-agents)
      (should-not drover-claude-code-transcripts--agents-job)
      (cl-letf (((symbol-function 'drover-claude-code-transcripts--list-shown-p) (lambda () t)))
        (drover-claude-code-transcripts--ask-agents)
        (should (drover-command-live-p drover-claude-code-transcripts--agents-job))
        ;; One at a time.
        (let ((job drover-claude-code-transcripts--agents-job))
          (drover-claude-code-transcripts--ask-agents)
          (should (eq job drover-claude-code-transcripts--agents-job))
          (should (drover-test-wait-commands)))
        (should (equal (plist-get (gethash "b1" drover-claude-code-transcripts--agents) :short) "bg000001"))
        ;; Not again before the interval.
        (drover-claude-code-transcripts--ask-agents)
        (should-not (drover-command-live-p drover-claude-code-transcripts--agents-job))))))

(ert-deftest drover-claude-code-transcripts-test-agents-slow ()
  "A slow \"claude agents\" never makes Emacs wait, nor piles up; its answer updates sessions.
Not even where starting a process takes a second, as on a managed Mac."
  (drover-claude-code-transcripts-test-with
    (let* ((script (expand-file-name "bin/claude" home))
           (count (expand-file-name "runs" home))
           (drover-claude-code-program script)
           (drover-claude-code-agents-interval 0)
           (drover-claude-code-transcripts--agents-time 0)
           (drover-claude-code-transcripts--agents-job nil))
      (drover-fixture-claude "b1" (expand-file-name "work/" home))
      (drover-claude-code-transcripts-test-scan)
      (should (eq (drover-session-status (drover-find 'claude-code "b1")) 'idle))
      (drover-test-write script (format "#!/bin/sh\necho run >> %s\nsleep 2\ncat <<'EOF'\n%s\nEOF\n"
                                        (shell-quote-argument count)
                                        drover-claude-code-transcripts-test-agents))
      (set-file-modes script #o755)
      (cl-letf (((symbol-function 'drover-claude-code-transcripts--list-shown-p) (lambda () t)))
        (drover-test-with-slow-starts
          ;; The runner's start takes its second, untimed.
          (drover-command-run "warming" (list "/bin/echo") #'ignore)
          (should (drover-test-wait-commands))
          (let ((lag (drover-test-lag
                      (lambda ()
                        (drover-claude-code-transcripts--ask-agents)
                        ;; Asked again and again while it runs: still one run.
                        (dotimes (_ 5) (drover-claude-code-transcripts--ask-agents))
                        (should (drover-command-live-p drover-claude-code-transcripts--agents-job))
                        (let ((deadline (+ (float-time) 1)))
                          (while (< (float-time) deadline)
                            (drover-claude-code-transcripts--ask-agents)
                            (accept-process-output nil 0.01)))
                        (should (drover-command-live-p drover-claude-code-transcripts--agents-job))
                        (should (drover-test-wait-commands))))))
            (should (< lag 0.05))))
        (should (equal (with-temp-buffer (insert-file-contents count) (buffer-string)) "run\n"))
        (drover-changed-now)
        (should (eq (drover-session-status (drover-find 'claude-code "b1")) 'waiting))))))

;;;; Encoding directories

(ert-deftest drover-claude-code-transcripts-test-encode-windows ()
  "Directories encode as the CLI encodes them, Windows ones as Node reports them."
  (should (equal (drover-claude-code-native-directory "c:/Users/val/my.proj/") "C:\\Users\\val\\my.proj"))
  (should (equal (drover-claude-code-native-directory "/home/val/proj/") "/home/val/proj"))
  (should (equal (drover-claude-code--encode (drover-claude-code-native-directory "c:/Users/val/my.proj"))
                 "C--Users-val-my-proj"))
  (should (equal (drover-claude-code--encode "/Users/val/.emacs.d") "-Users-val--emacs-d"))
  ;; A character beyond the Basic Multilingual Plane is two UTF-16 units.
  (should (equal (drover-claude-code--encode "/tmp/a😀b") "-tmp-a--b"))
  (should (equal (drover-claude-code--encode "/tmp/é") "-tmp--"))
  ;; Past 200 characters, cut and hashed as JavaScript hashes strings.
  (let ((long (concat "/" (make-string 250 ?a))))
    (should (equal (drover-claude-code--hash long) "feo44x"))
    (should (equal (drover-claude-code--encode long)
                   (concat "-" (make-string 199 ?a) "-feo44x"))))
  (should (equal (drover-claude-code--hash "") "0"))
  (should (equal (drover-claude-code--hash "C:\\Users\\val") "686f3c")))


(ert-deftest drover-claude-code-transcripts-test-prints-in-proportion ()
  "The index of transcripts prints in proportion to the conversations it read.
Emacs prints a value whole in an error's data, or as `format' does: an
entry or a record leading back to the index would print it all again."
  (drover-claude-code-transcripts-test-with
    (drover-test-write-claude-catalog)
    (dotimes (i 100)
      (drover-fixture-claude (format "c%03d" i) (expand-file-name (format "work/p%d/" (% i 7)) home)
                             :prompt (format "Prompt %d" i)
                             :subagents (and (zerop (% i 10)) '(("agent-a1" "Look around" "Explore")))))
    (drover-claude-code-transcripts-test-scan)
    (should (>= (length (drover-sessions)) 100))
    (drover-test-prints-in-proportion
     100 (list (cons "an entry" (car (hash-table-values (drover-transcript-index-entries index))))
               (cons "a record" (drover-find 'claude-code "c001"))))))

(provide 'drover-claude-code-transcripts-test)
;;; drover-claude-code-transcripts-test.el ends here
