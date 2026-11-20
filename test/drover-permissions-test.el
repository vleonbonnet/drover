;;; drover-permissions-test.el --- Tests of the permission prompts agents gave  -*- lexical-binding: t; -*-

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

;; The permission prompts of a period read from a ledger in a
;; throwaway directory, as the harnesses' hooks write it: OpenCode's
;; requests with their replies, each once, the sends a plugin passed,
;; Claude Code's prompts with the answers their transcripts tell; what
;; travels to a peer and what is taken from one.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-permissions)

(defconst drover-permissions-test--base 1790000000.0
  "The start of the period of the tests.")

(defun drover-permissions-test--iso (time)
  "Return TIME, a float time, as the ledger writes it."
  (format-time-string "%FT%T.%3NZ" time t))

(defmacro drover-permissions-test-with (&rest body)
  "Run BODY with an empty ledger directory bound to `directory'."
  (declare (indent 0) (debug t))
  `(let* ((directory (file-name-as-directory (make-temp-file "drover-permissions-" t)))
          (drover-permissions-directory directory)
          (drover-permissions-engines '(opencode claude-code))
          (drover-permissions-limit 5000))
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun drover-permissions-test--write (file &rest objects)
  "Append OBJECTS, alists, to FILE, one JSON line each; a string goes as is."
  (with-temp-buffer
    (dolist (object objects)
      (insert (if (stringp object) object (json-serialize object)) "\n"))
    (write-region nil nil file t 'silent)))

(defun drover-permissions-test--line (time &rest fields)
  "Return the ledger line at TIME, seconds after the base, of FIELDS, an alist."
  (append `((v . 1) (time . ,(drover-permissions-test--iso (+ drover-permissions-test--base time))))
          (car fields)))

(defun drover-permissions-test-collect (&optional since until)
  "Return what `drover-permissions-collect' reads from SINCE to UNTIL, once read."
  (let ((result nil))
    (drover-permissions-collect (or since drover-permissions-test--base)
                                (or until (+ drover-permissions-test--base 86400))
                                (lambda (value) (setq result value)))
    (with-timeout (10 (error "The permission prompts were not read in time"))
      (while (not result)
        (accept-process-output nil 0.01)))
    result))

(defun drover-permissions-test--summary (result)
  "Return each prompt RESULT tells as (ENGINE ACTION REPLY)."
  (mapcar (lambda (prompt) (list (plist-get prompt :engine) (plist-get prompt :action) (plist-get prompt :reply)))
          (plist-get result :permissions)))

(ert-deftest drover-permissions-test-opencode ()
  "OpenCode's requests of the period, each once with its reply, and the sends a plugin passed."
  (drover-permissions-test-with
    (let ((file (expand-file-name "opencode.jsonl" directory)))
      ;; The rotated file holds the oldest.
      (drover-permissions-test--write
       (concat file ".1")
       (drover-permissions-test--line 1 '((event . "asked") (session . "ses_a") (request . "per_1") (action . "shell")
                                          (resources . ["git push"]) (directory . "/w/repo")
                                          (rules . [((action . "shell") (resource . "git push*") (effect . "ask"))]))))
      (drover-permissions-test--write
       file
       ;; Two instances of the plugin wrote the same request.
       (drover-permissions-test--line 1 '((event . "asked") (session . "ses_a") (request . "per_1") (action . "shell")))
       (drover-permissions-test--line 2 '((event . "replied") (session . "ses_a") (request . "per_1") (reply . "once")))
       (drover-permissions-test--line 3 '((event . "asked") (session . "ses_a") (request . "per_2") (action . "edit")
                                          (resources . ["AGENTS.md"]) (rules . [((default . t))])))
       (drover-permissions-test--line 4 '((event . "replied") (session . "ses_a") (request . "per_2") (reply . "reject")))
       (drover-permissions-test--line 5 '((event . "asked") (session . "ses_b") (request . "per_3") (action . "slack_send_message")))
       (drover-permissions-test--line 6 '((event . "passed") (session . "ses_b") (call . "c1") (action . "slack_send_message")
                                          (plugin . "slack-send-ask") (reason . "draft #3, as \"send #3\" asked")))
       "not json"
       "{\"time\": \"never\"}"
       (drover-permissions-test--line 90000 '((event . "asked") (session . "ses_c") (request . "per_9") (action . "shell"))))
      (let* ((result (drover-permissions-test-collect))
             (first (car (plist-get result :permissions)))
             (passed (nth 3 (plist-get result :permissions))))
        (should (equal (drover-permissions-test--summary result)
                       '((opencode "shell" "once") (opencode "edit" "reject")
                         (opencode "slack_send_message" "unknown") (opencode "slack_send_message" "passed"))))
        (should (equal (plist-get first :id) "ses_a"))
        (should (equal (plist-get first :resources) '("git push")))
        (should (equal (plist-get first :rules) '("shell git push* ask")))
        (should (equal (plist-get first :directory) "/w/repo"))
        (should (= (plist-get first :time) (+ drover-permissions-test--base 1)))
        (should (equal (plist-get (nth 1 (plist-get result :permissions)) :rules) '("no rule")))
        (should (equal (plist-get passed :plugin) "slack-send-ask"))
        (should (string-match-p "send #3" (plist-get passed :message)))
        (should (= (plist-get result :cut) 0))))))

(ert-deftest drover-permissions-test-claude-code ()
  "Claude Code's prompts, each answered as its transcript tells: allowed, refused, or unknown."
  (drover-permissions-test-with
    (let ((transcript (expand-file-name "s1.jsonl" directory))
          (at (lambda (time) (drover-permissions-test--iso (+ drover-permissions-test--base time)))))
      (drover-permissions-test--write
       transcript
       `((type . "assistant") (timestamp . ,(funcall at 1))
         (message . ((content . [((type . "tool_use") (id . "t1") (name . "Bash") (input . ((command . "ls"))))]))))
       `((type . "user") (timestamp . ,(funcall at 2))
         (message . ((content . [((type . "tool_result") (tool_use_id . "t1") (content . "a b"))]))))
       `((type . "assistant") (timestamp . ,(funcall at 3))
         (message . ((content . [((type . "tool_use") (id . "t2") (name . "Bash") (input . ((command . "rm x"))))]))))
       `((type . "user") (timestamp . ,(funcall at 4))
         (message . ((content . [((type . "tool_result") (tool_use_id . "t2") (is_error . t)
                                  (content . "The user doesn't want to proceed with this tool use. The tool use was rejected."))])))))
      (drover-permissions-test--write
       (expand-file-name "claude-code.jsonl" directory)
       (drover-permissions-test--line 1.5 `((event . "asked") (session . "s1") (action . "Bash") (resources . ["ls"])
                                            (transcript . ,transcript)))
       (drover-permissions-test--line 3.5 `((event . "asked") (session . "s1") (action . "Bash") (resources . ["rm x"])
                                            (transcript . ,transcript)))
       ;; The hook named the call.
       (drover-permissions-test--line 3.6 `((event . "asked") (session . "s1") (action . "Bash") (call . "t1")
                                            (transcript . ,transcript)))
       (drover-permissions-test--line 5 `((event . "asked") (session . "s1") (action . "Write")
                                          (transcript . ,transcript)))
       (drover-permissions-test--line 6 '((event . "asked") (session . "s2") (action . "Bash")
                                          (transcript . "/no/such/transcript.jsonl"))))
      (should (equal (drover-permissions-test--summary (drover-permissions-test-collect))
                     '((claude-code "Bash" "allowed") (claude-code "Bash" "reject") (claude-code "Bash" "allowed")
                       (claude-code "Write" "unknown") (claude-code "Bash" "unknown")))))))

(ert-deftest drover-permissions-test-limit-and-failures ()
  "Past the limit, the oldest are left out and counted; a ledger it cannot read is told, the others read."
  (drover-permissions-test-with
    (let ((file (expand-file-name "opencode.jsonl" directory)))
      (dotimes (i 5)
        (drover-permissions-test--write
         file (drover-permissions-test--line (1+ i) `((event . "asked") (session . "ses_a")
                                                      (request . ,(format "per_%d" i)) (action . ,(format "a%d" i))))))
      (let ((drover-permissions-limit 2))
        (let ((result (drover-permissions-test-collect)))
          (should (equal (mapcar (lambda (prompt) (plist-get prompt :action)) (plist-get result :permissions))
                         '("a3" "a4")))
          (should (= (plist-get result :cut) 3))))
      (cl-letf (((symbol-function 'drover-permissions--claude-code) (lambda (_) (error "Unreadable"))))
        (let ((result (drover-permissions-test-collect)))
          (should (length= (plist-get result :permissions) 5))
          (should (equal (plist-get result :failures) '((claude-code . "Unreadable"))))))
      (should-error (drover-permissions-collect drover-permissions-test--base (+ drover-permissions-test--base (* 40 86400))
                                                #'ignore)))))

(ert-deftest drover-permissions-test-wire ()
  "What travels is bounded; what comes from a peer is checked, the hostile left out."
  (let* ((prompts (list (list :engine 'opencode :id "ses_a" :time 1.0e9 :action "shell" :reply "once"
                              :resources '("git push") :rules '("shell git push* ask") :directory "/w/")
                        (list :engine 'claude-code :id "s1" :time 1.1e9 :action "Bash" :reply "reject"
                              :resources '("rm x") :message "m" :plugin "p")))
         (wire (drover-permissions--encode (list :permissions prompts :cut 2 :failures '((codex . "unreadable")))))
         (decoded (drover-permissions--decode (json-parse-string (json-serialize wire) :object-type 'plist) "core")))
    (should (equal (mapcar (lambda (prompt) (list (plist-get prompt :host) (plist-get prompt :action)
                                                  (plist-get prompt :reply) (plist-get prompt :resources)))
                           (plist-get decoded :permissions))
                   '(("core" "shell" "once" ("git push")) ("core" "Bash" "reject" ("rm x")))))
    (should (equal (plist-get (car (plist-get decoded :permissions)) :rules) '("shell git push* ask")))
    (should (equal (plist-get (cadr (plist-get decoded :permissions)) :plugin) "p"))
    (should (= (plist-get decoded :cut) 2))
    (should (equal (plist-get decoded :failures) '((codex . "unreadable")))))
  (let* ((decoded (drover-permissions--decode
                   (list :w (vector (list :g "opencode" :id "a" :t 1.0e9 :a "shell" :y "granted")
                                    (list :g "no-such-engine-here" :id "a" :t 1.0e9 :a "shell" :y "once")
                                    (list :g "opencode" :id "b" :t "soon" :a "shell" :y "once")
                                    (list :g "opencode" :id (make-string 500 ?i) :t 1.0e9 :a (make-string 500 ?a)
                                          :y "always" :r (vector (make-string 5000 ?r) 7 "two" "3" "4" "5" "6")
                                          :d (make-string 5000 ?d))
                                    "junk")
                         :cut "many")
                   "core"))
         (prompt (car (plist-get decoded :permissions))))
    (should (length= (plist-get decoded :permissions) 1))
    (should (<= (length (plist-get prompt :id)) 201))
    (should (<= (length (plist-get prompt :action)) 121))
    (should (length= (plist-get prompt :resources) 5))
    (should (<= (length (car (plist-get prompt :resources))) 301))
    (should (<= (length (plist-get prompt :directory)) 1025))
    (should (= (plist-get decoded :cut) 0))))

(ert-deftest drover-permissions-test-gather-peers ()
  "Each peer able to is asked for its prompts, by the request permissions."
  (drover-permissions-test-with
    (let ((base drover-permissions-test--base)
          (asked nil)
          (result nil))
      (cl-letf (((symbol-function 'drover-remote-capable-hosts)
                 (lambda (capability)
                   (should (equal capability "permissions"))
                   '(("core" "Core" ready) ("older" "Older" old))))
                ((symbol-function 'drover-remote-ask)
                 (lambda (name op request callback _seconds)
                   (push (list name op request) asked)
                   (run-at-time 0.05 nil callback
                                (list :w (vector (list :g "claude-code" :id "s1" :t (+ base 5) :a "Bash" :y "allowed"))
                                      :cut 0 :f [])
                                nil))))
        (drover-permissions-gather base (+ base 86400) (lambda (value) (setq result value)) 5)
        (with-timeout (10 (error "Not gathered in time"))
          (while (not result) (accept-process-output nil 0.01))))
      (should (equal (mapcar (lambda (entry) (list (car entry) (cadr entry))) asked) '(("core" "permissions"))))
      (should (equal (mapcar (lambda (prompt) (list (plist-get prompt :host) (plist-get prompt :action)))
                             (plist-get result :permissions))
                     '(("core" "Bash"))))
      (should (equal (mapcar (lambda (host) (list (plist-get host :host) (plist-get host :status)))
                             (plist-get result :hosts))
                     '((nil ok) ("core" ok) ("older" old)))))))

(provide 'drover-permissions-test)
;;; drover-permissions-test.el ends here
