;;; drover-prompts-test.el --- Tests of the prompts the user sent  -*- lexical-binding: t; -*-

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

;; The prompts of a period read from OpenCode's database, Claude Code's
;; transcripts and Codex's rollouts in throwaway homes: those of the
;; user alone, within the period, bounded, each once; what travels to
;; a peer and what is taken from one.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-prompts)

(defconst drover-prompts-test--base 1790000000.0
  "The start of the period of the tests.")

(defmacro drover-prompts-test-with (&rest body)
  "Run BODY over fresh homes, every engine read, nothing left out."
  (declare (indent 0) (debug t))
  `(drover-test-with-homes
    (drover-test-with-backend nil
                              (let ((drover-prompts-engines '(opencode claude-code codex))
                                    (drover-prompts-ignore-regexps nil)
                                    (drover-prompts-ignore-directories nil)
                                    (drover-prompts-text-limit 4000)
                                    (drover-prompts-limit 5000))
                                ,@body))))

(defun drover-prompts-test-collect (&optional since until)
  "Return what `drover-prompts-collect' reads from SINCE to UNTIL, once read.
SINCE and UNTIL default to the period of the tests, a day from its base."
  (let ((result nil))
    (drover-prompts-collect (or since drover-prompts-test--base) (or until (+ drover-prompts-test--base 86400))
                            (lambda (value) (setq result value)))
    (with-timeout (10 (error "The prompts were not read in time"))
      (while (not result)
        (accept-process-output nil 0.01)))
    result))

(defun drover-prompts-test-texts (result)
  "Return the texts of the prompts RESULT tells, oldest first."
  (mapcar (lambda (prompt) (plist-get prompt :text)) (plist-get result :prompts)))

;;;; OpenCode

(defun drover-prompts-test-oc-db ()
  "Make OpenCode's database, with the tables and columns the prompts read; return it."
  (make-directory (file-name-directory drover-opencode-database) t)
  (let ((db (sqlite-open drover-opencode-database)))
    (sqlite-execute db "CREATE TABLE session_v2 (id text PRIMARY KEY, project_id text, parent_id text, directory text, title text, time_created integer, time_updated integer)")
    (sqlite-execute db "CREATE TABLE session_message (id text PRIMARY KEY, session_id text NOT NULL, type text NOT NULL, seq integer, time_created integer NOT NULL, time_updated integer, data text NOT NULL)")
    (sqlite-execute db "CREATE INDEX session_message_time_created_idx ON session_message (time_created)")
    db))

(defun drover-prompts-test-oc-session (db id parent directory title)
  "Add the OpenCode session ID of PARENT to DB, in DIRECTORY, titled TITLE."
  (sqlite-execute db "INSERT INTO session_v2 VALUES (?, 'prj', ?, ?, ?, 0, 0)" (list id parent directory title)))

(defun drover-prompts-test-oc-message (db id session type time text)
  "Add the message ID of SESSION to DB, of TYPE, at TIME, saying TEXT."
  (sqlite-execute db "INSERT INTO session_message VALUES (?, ?, ?, 0, ?, ?, ?)"
                  (list id session type (round (* 1000 time)) (round (* 1000 time))
                        (json-serialize `((time . ((created . ,(round (* 1000 time))))) (text . ,text))))))

(ert-deftest drover-prompts-test-opencode ()
  "The user's prompts of the period, of sessions at the top of their trees, oldest first."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (drover-prompts-test-with
    (let ((db (drover-prompts-test-oc-db))
          (base drover-prompts-test--base)
          (work (expand-file-name "work/" home)))
      (unwind-protect
          (progn
            (drover-prompts-test-oc-session db "ses_a" nil work "Fix the login")
            (drover-prompts-test-oc-session db "ses_sub" "ses_a" work "A subagent")
            (drover-prompts-test-oc-message db "msg_1" "ses_a" "user" (- base 10) "Before the period")
            (drover-prompts-test-oc-message db "msg_2" "ses_a" "user" (+ base 20) "  Second prompt  ")
            (drover-prompts-test-oc-message db "msg_3" "ses_a" "user" (+ base 10) "First prompt")
            (drover-prompts-test-oc-message db "msg_4" "ses_a" "assistant" (+ base 11) "A reply")
            (drover-prompts-test-oc-message db "msg_5" "ses_a" "synthetic" (+ base 12) "<system-reminder>")
            (drover-prompts-test-oc-message db "msg_6" "ses_sub" "user" (+ base 13) "The subagent's task")
            (drover-prompts-test-oc-message db "msg_7" "ses_a" "user" (+ base 90000) "After the period"))
        (sqlite-close db))
      (let* ((result (drover-prompts-test-collect))
             (first (car (plist-get result :prompts))))
        (should (equal (drover-prompts-test-texts result) '("First prompt" "Second prompt")))
        (should (equal (plist-get first :engine) 'opencode))
        (should (equal (plist-get first :id) "ses_a"))
        (should (equal (plist-get first :title) "Fix the login"))
        (should (equal (plist-get first :directory) work))
        (should (= (plist-get first :time) (+ base 10)))
        (should (= (plist-get result :cut) 0))
        (should-not (plist-get result :failures))))))

(ert-deftest drover-prompts-test-opencode-pages-and-bounds ()
  "Pages of the database follow one another; a long prompt is cut, its length told."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (drover-prompts-test-with
    (let ((db (drover-prompts-test-oc-db))
          (base drover-prompts-test--base)
          (drover-prompts-text-limit 50))
      (unwind-protect
          (progn
            (drover-prompts-test-oc-session db "ses_a" nil home "Pages")
            (dotimes (index (+ drover-prompts-opencode-page 5))
              ;; Two at each time: the page's end falls between them.
              (drover-prompts-test-oc-message db (format "msg_%04d" index) "ses_a" "user"
                                              (+ base 1 (/ index 2)) (format "Prompt %d" index)))
            (drover-prompts-test-oc-message db "msg_long" "ses_a" "user" (+ base 5000) (make-string 200 ?x)))
        (sqlite-close db))
      (let* ((result (drover-prompts-test-collect))
             (prompts (plist-get result :prompts))
             (long (car (last prompts))))
        (should (length= prompts (+ drover-prompts-opencode-page 6)))
        (should (equal (delete-dups (mapcar (lambda (prompt) (plist-get prompt :text)) prompts))
                       (mapcar (lambda (prompt) (plist-get prompt :text)) prompts)))
        (should (length= (plist-get long :text) 51))
        (should (= (plist-get long :cut) 200))))))

(ert-deftest drover-prompts-test-ignored ()
  "The configuration leaves out prompts by their text and by their session's directory."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (drover-prompts-test-with
    (let* ((db (drover-prompts-test-oc-db))
           (base drover-prompts-test--base)
           (automation (expand-file-name "automation/" home))
           (drover-prompts-ignore-regexps '("\\`Automated notice: "))
           (drover-prompts-ignore-directories (list automation)))
      (unwind-protect
          (progn
            (drover-prompts-test-oc-session db "ses_a" nil home "Mine")
            (drover-prompts-test-oc-session db "ses_b" nil (expand-file-name "below/" automation) "Its")
            (drover-prompts-test-oc-message db "msg_1" "ses_a" "user" (+ base 1) "Automated notice: a request")
            (drover-prompts-test-oc-message db "msg_2" "ses_a" "user" (+ base 2) "Mine to keep")
            (drover-prompts-test-oc-message db "msg_3" "ses_b" "user" (+ base 3) "In the automation's directory"))
        (sqlite-close db))
      (should (equal (drover-prompts-test-texts (drover-prompts-test-collect)) '("Mine to keep"))))))

;;;; Claude Code and Codex

(defconst drover-prompts-test--claude "5a1e2b3c-4d5e-4f60-8a7b-9c0d1e2f3a4b"
  "A Claude Code conversation of the tests.")

(ert-deftest drover-prompts-test-claude-code ()
  "Claude Code's prompts of the period: the user's alone, read from the end back to the period's start."
  (drover-prompts-test-with
    (let* ((base drover-prompts-test--base)
           (id drover-prompts-test--claude)
           (directory (expand-file-name "repo/" home))
           ;; Begun before the period, its first prompt is left out.
           (file (drover-fixture-claude id directory :start (- base 100) :prompt "Before the period")))
      (drover-fixture-claude-append
       file
       (drover-fixture-claude-user id directory (+ base 1) "<command-name>/model</command-name>")
       (drover-fixture-claude-user id directory (+ base 2) "Fix the redirect")
       (drover-fixture-claude-assistant id directory (+ base 3) "Fixed.")
       (drover-fixture-claude-user id directory (+ base 4) "A note of the CLI" :meta t)
       (drover-fixture-claude-user id directory (+ base 5) "A subagent's task" :sidechain t)
       (drover-fixture-claude-user id directory (+ base 6) "<system-reminder>Context</system-reminder>")
       (drover-fixture-claude-user id directory (+ base 7) "Base directory for this skill: /x")
       (drover-fixture-claude-user id directory (+ base 8) "Now add a test")
       (drover-fixture-claude-user id directory (+ base 90000) "After the period"))
      (let* ((result (drover-prompts-test-collect))
             (first (car (plist-get result :prompts))))
        (should (equal (drover-prompts-test-texts result) '("Fix the redirect" "Now add a test")))
        (should (eq (plist-get first :engine) 'claude-code))
        (should (equal (plist-get first :id) id))
        (should (equal (plist-get first :directory) directory))
        (should (= (plist-get first :time) (+ base 2)))))))

(ert-deftest drover-prompts-test-claude-code-programs-and-copies ()
  "A program's conversation tells no prompt; a forked copy of a prompt counts once; old files are not read."
  (drover-prompts-test-with
    (let* ((base drover-prompts-test--base)
           (directory (expand-file-name "repo/" home))
           (program "6b2f3c4d-5e6f-4a70-9b8c-0d1e2f3a4b5c")
           (fork "7c3a4d5e-6f7a-4b80-8c9d-1e2f3a4b5c6d")
           (old "8d4b5e6f-7a8b-4c90-9d0e-2f3a4b5c6d7e"))
      (drover-fixture-claude program directory :start (+ base 1) :prompt "Summarize this diff"
                             :entrypoint "sdk-cli")
      (drover-fixture-claude drover-prompts-test--claude directory :start (+ base 10) :prompt "Shared prompt")
      ;; The fork's copy of the history, then a prompt of its own.
      (let ((file (drover-fixture-claude fork directory :start (+ base 10) :prompt "Shared prompt")))
        (drover-fixture-claude-append file (drover-fixture-claude-user fork directory (+ base 20) "Only in the fork"))
        (set-file-times file (seconds-to-time (+ base 21))))
      (let ((file (drover-fixture-claude old directory :start (+ base 30) :prompt "Unchanged since")))
        ;; Changed last before the period: not read, whatever it holds.
        (set-file-times file (seconds-to-time (- base 1))))
      (should (equal (drover-prompts-test-texts (drover-prompts-test-collect))
                     '("Shared prompt" "Only in the fork"))))))

(ert-deftest drover-prompts-test-codex ()
  "Codex's prompts of the period, those of the user's conversations alone."
  (drover-prompts-test-with
    (let ((base drover-prompts-test--base)
          (directory (expand-file-name "repo/" home))
          (id "0a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4d"))
      (let ((file (drover-fixture-codex id directory :start (+ base 10) :prompt "Add the export command")))
        (drover-fixture-write file (list (drover-fixture-codex-event (+ base 30) "user_message" :message "Now CSV"))
                              t)
        (set-file-times file (seconds-to-time (+ base 31))))
      (drover-fixture-codex "1b2c3d4e-5f6a-4b7c-9d8e-0f1a2b3c4d5e" directory :start (+ base 11)
                            :prompt "Run by codex exec" :source "exec")
      (drover-fixture-codex "2c3d4e5f-6a7b-4c8d-8e9f-1a2b3c4d5e6f" directory :start (+ base 12)
                            :prompt "A subagent's task" :parent id)
      (let* ((result (drover-prompts-test-collect))
             (first (car (plist-get result :prompts))))
        (should (equal (drover-prompts-test-texts result) '("Add the export command" "Now CSV")))
        (should (eq (plist-get first :engine) 'codex))
        (should (equal (plist-get first :id) id))
        (should (equal (plist-get first :directory) directory))))))

(ert-deftest drover-prompts-test-limit-and-period ()
  "Past the limit the oldest prompts are left out, counted; a period is checked."
  (drover-prompts-test-with
    (let ((base drover-prompts-test--base)
          (directory (expand-file-name "repo/" home))
          (id drover-prompts-test--claude)
          (drover-prompts-limit 2))
      (let ((file (drover-fixture-claude id directory :start (+ base 1) :prompt "One")))
        (drover-fixture-claude-append file
                                      (drover-fixture-claude-user id directory (+ base 5) "Two")
                                      (drover-fixture-claude-user id directory (+ base 6) "Three"))
        (set-file-times file (seconds-to-time (+ base 7))))
      (let ((result (drover-prompts-test-collect)))
        (should (equal (drover-prompts-test-texts result) '("Two" "Three")))
        (should (= (plist-get result :cut) 1)))
      (should-error (drover-prompts-collect (+ base 10) base #'ignore))
      (should-error (drover-prompts-collect base (+ base (* 86400 (1+ drover-prompts-days-limit))) #'ignore)))))

;;;; Between hosts

(ert-deftest drover-prompts-test-wire ()
  "What travels is bounded, the oldest left out first; what comes from a peer is checked."
  (drover-prompts-test-with
    (let* ((prompts (list (list :engine 'opencode :id "ses_a" :time 1.0e9 :text "Old" :cut nil
                                :directory "/work/" :title "A")
                          (list :engine 'claude-code :id "x" :time 1.1e9 :text "New" :cut 9000)))
           (wire (drover-prompts--encode (list :prompts prompts :cut 2 :failures '((codex . "unreadable")))))
           (decoded (drover-prompts--decode (json-parse-string (json-serialize wire) :object-type 'plist)
                                            "peer")))
      (should (equal (mapcar (lambda (prompt) (plist-get prompt :text)) (plist-get decoded :prompts))
                     '("Old" "New")))
      (should (equal (plist-get (car (plist-get decoded :prompts)) :host) "peer"))
      (should (equal (plist-get (car (plist-get decoded :prompts)) :directory) "/work/"))
      (should (= (plist-get (cadr (plist-get decoded :prompts)) :cut) 9000))
      (should (= (plist-get decoded :cut) 2))
      (should (equal (plist-get decoded :failures) '((codex . "unreadable"))))
      (let ((drover-prompts-remote-bytes 100))
        (let ((cut (drover-prompts--encode (list :prompts prompts :cut 0 :failures nil))))
          (should (length= (plist-get cut :p) 1))
          (should (equal (plist-get (aref (plist-get cut :p) 0) :x) "New"))
          (should (= (plist-get cut :cut) 1)))))
    ;; Hostile values: bounded, or dropped.
    (let* ((drover-prompts-text-limit 100)
           (decoded (drover-prompts--decode
                     (list :p (vector (list :g "no-such-engine-here" :id "a" :t 1.0e9 :x "x")
                                      (list :g "opencode" :id "b" :t "soon" :x "x")
                                      (list :g "opencode" :id (make-string 500 ?i) :t 1.0e9
                                            :x (make-string 100000 ?x) :d (make-string 5000 ?d) :c -4)
                                      "junk")
                           :cut "many" :f (vector (list :g "codex" :m (make-string 5000 ?m)) 7))
                     "peer"))
           (prompt (car (plist-get decoded :prompts))))
      (should (length= (plist-get decoded :prompts) 1))
      (should (<= (length (plist-get prompt :id)) 201))
      (should (length= (plist-get prompt :text) 101))
      (should (= (plist-get prompt :cut) 100000))
      (should (<= (length (plist-get prompt :directory)) 1025))
      (should (= (plist-get decoded :cut) 0))
      (should (<= (length (cdar (plist-get decoded :failures))) 301)))))

(ert-deftest drover-prompts-test-gather-this-host ()
  "Without peers, the prompts gathered are this host's, its answer told."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (drover-prompts-test-with
    (let ((db (drover-prompts-test-oc-db))
          (base drover-prompts-test--base)
          (result nil))
      (unwind-protect
          (progn
            (drover-prompts-test-oc-session db "ses_a" nil home "Mine")
            (drover-prompts-test-oc-message db "msg_1" "ses_a" "user" (+ base 1) "Here"))
        (sqlite-close db))
      (drover-prompts-gather base (+ base 86400) (lambda (value) (setq result value)))
      (with-timeout (10 (error "Not gathered in time"))
        (while (not result) (accept-process-output nil 0.01)))
      (should (equal (drover-prompts-test-texts result) '("Here")))
      (should (null (plist-get (car (plist-get result :prompts)) :host)))
      (let ((host (car (plist-get result :hosts))))
        (should (length= (plist-get result :hosts) 1))
        (should (null (plist-get host :host)))
        (should (eq (plist-get host :status) 'ok))
        (should (= (plist-get host :count) 1)))
      ;; Stopped, it calls back no more.
      (let ((called nil))
        (funcall (drover-prompts-gather base (+ base 86400) (lambda (_) (setq called t))))
        (accept-process-output nil 0.2)
        (should-not called)))))

(ert-deftest drover-prompts-test-gather-peers ()
  "Each peer is asked or told why not; one answering late is told so, its prompts left out."
  (drover-prompts-test-with
    (let ((base drover-prompts-test--base)
          (asked nil)
          (result nil))
      (cl-letf (((symbol-function 'drover-remote-prompts-hosts)
                 (lambda () '(("core" "Core" ready) ("away" "Away" offline) ("older" "Older" old)
                              ("broken" "Broken" ready) ("slow" "Slow" ready))))
                ((symbol-function 'drover-remote-ask-prompts)
                 (lambda (name request callback _seconds)
                   (push (cons name request) asked)
                   (pcase name
                     ("core" (run-at-time 0.05 nil callback
                                          (list :p (vector (list :g "codex" :id "c1" :t (+ base 5) :x "From Core"))
                                                :cut 0 :f [])
                                          nil))
                     ("broken" (run-at-time 0.05 nil callback nil "it failed there"))))))
        (drover-prompts-gather base (+ base 86400) (lambda (value) (setq result value)) 1)
        (with-timeout (10 (error "Not gathered in time"))
          (while (not result) (accept-process-output nil 0.01))))
      (should (equal (sort (mapcar #'car asked) #'string<) '("broken" "core" "slow")))
      (should (equal (plist-get (cdar asked) :since) base))
      (should (equal (mapcar (lambda (prompt) (list (plist-get prompt :host) (plist-get prompt :text)))
                             (plist-get result :prompts))
                     '(("core" "From Core"))))
      (should (equal (mapcar (lambda (host) (list (plist-get host :host) (plist-get host :label)
                                                  (plist-get host :status)))
                             (plist-get result :hosts))
                     `((nil ,(drover-host-label nil) ok) ("core" "Core" ok) ("away" "Away" offline)
                       ("older" "Older" old) ("broken" "Broken" failed) ("slow" "Slow" failed))))
      (should (equal (plist-get (nth 4 (plist-get result :hosts)) :message) "it failed there"))
      (should (string-match-p "did not answer" (plist-get (nth 5 (plist-get result :hosts)) :message))))))

(provide 'drover-prompts-test)
;;; drover-prompts-test.el ends here
