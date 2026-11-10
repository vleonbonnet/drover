;;; drover-conversation-test.el --- Tests of conversations read as entries  -*- lexical-binding: t; -*-

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

;; Claude Code's transcripts and Codex's rollouts, in throwaway homes,
;; read as entries: every kind, pages backward whose concatenation is
;; one whole read, windows cutting characters, lines too long to parse,
;; and lines appended followed until the file is replaced.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-transcript-fixtures)
(require 'drover-claude-code)
(require 'drover-codex)
(require 'drover-conversation)

(defconst drover-conversation-test--id "0f6c4b1e-6a0e-4f1c-9b2a-1c2d3e4f5a6b"
  "A Claude Code conversation of the tests.")

(defun drover-conversation-test--session (engine id directory)
  "Return a session of ENGINE's conversation ID in DIRECTORY."
  (drover-session-create :backend 'fake :id id :engine engine :engine-id id :directory directory))

(defun drover-conversation-test--page (session &optional before count)
  "Return the page of SESSION's conversation before BEFORE, COUNT entries."
  (let ((answer nil))
    (drover-conversation-page session before count (lambda (page) (setq answer page)))
    (let ((end (+ (float-time) 10)))
      (while (and (not answer) (< (float-time) end))
        (accept-process-output nil 0.01)))
    answer))

(defun drover-conversation-test--all (session count)
  "Return every entry of SESSION's conversation, read in pages of COUNT."
  (let ((page (drover-conversation-test--page session nil count))
        (entries nil))
    (while page
      (setq entries (append (append (plist-get page :e) nil) entries))
      (setq page (and (plist-get page :more)
                      (drover-conversation-test--page session (plist-get page :from) count))))
    entries))

(defun drover-conversation-test--claude (directory)
  "Write a Claude Code conversation of every kind of record in DIRECTORY; return its file."
  (let* ((id drover-conversation-test--id)
         (file (drover-fixture-claude-file id directory))
         (time 1790000000.0))
    (drover-fixture-write
     file
     (list
      (drover-fixture-claude-user id directory time "Fix the failing test")
      (drover-fixture-line
       :type "assistant" :isSidechain :false :uuid "a1" :timestamp (drover-fixture-time (+ time 1))
       :message (list :role "assistant" :model "claude-opus-5-5" :id "msg_1"
                      :content (vector (list :type "thinking" :thinking "It fails on Windows.")
                                       (list :type "text" :text "Looking at it.")
                                       (list :type "tool_use" :id "toolu_1" :name "Bash"
                                             :input (list :command "make test\necho done")))))
      (drover-fixture-claude-tool-result id directory (+ time 2))
      (drover-fixture-line
       :type "user" :isSidechain :false :uuid "u3" :timestamp (drover-fixture-time (+ time 3))
       :message (list :role "user" :content (vector (list :tool_use_id "toolu_2" :type "tool_result"
                                                          :content "edited")))
       :toolUseResult (list :filePath (expand-file-name "src/a.el" directory) :type "update"
                            :structuredPatch (vector (list :oldStart 1 :oldLines 1 :newStart 1 :newLines 2
                                                           :lines (vector "-old" "+new" "+more")))))
      (drover-fixture-line
       :type "assistant" :isSidechain t :uuid "s1" :timestamp (drover-fixture-time (+ time 4))
       :message (list :role "assistant" :content (vector (list :type "text" :text "a subagent's"))))
      (drover-fixture-claude-api-error id directory (+ time 5) "API Error: overloaded")
      (drover-fixture-claude-user id directory (+ time 6) "[Request interrupted by user]")
      (car (drover-fixture-claude-exit id directory (+ time 7)))
      (drover-fixture-line :type "system" :subtype "compact_boundary" :timestamp (drover-fixture-time (+ time 8)))
      ;; Shown by the boundary alone.
      (drover-fixture-claude-summary id directory (+ time 8))
      (drover-fixture-claude-assistant id directory (+ time 9) "All green.")))
    file))

(ert-deftest drover-conversation-test-claude-code ()
  "A Claude Code transcript reads as entries of every kind, sidechains left out."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (file (drover-conversation-test--claude directory))
           (session (drover-conversation-test--session 'claude-code drover-conversation-test--id directory))
           (page (drover-conversation-test--page session))
           (entries (append (plist-get page :e) nil)))
      (should (equal (drover-conversation-file session) file))
      (should (equal (mapcar (lambda (entry) (plist-get entry :y)) entries)
                     '("user" "think" "text" "tool" "res" "res" "patch" "err" "meta" "meta" "meta" "text")))
      (should (equal (plist-get (nth 0 entries) :x) "Fix the failing test"))
      (should (equal (plist-get (nth 3 entries) :n) "Bash"))
      (should (equal (plist-get (nth 3 entries) :s) "make test"))
      (should (equal (plist-get (nth 4 entries) :c) "toolu_1"))
      (should (equal (plist-get (nth 6 entries) :f) "src/a.el"))
      (should (equal (plist-get (nth 6 entries) :st) "modified"))
      (should (= (plist-get (nth 6 entries) :a) 2))
      (should (= (plist-get (nth 6 entries) :d) 1))
      (should (equal (plist-get (nth 8 entries) :x) "interrupted"))
      (should (equal (plist-get (nth 9 entries) :x) "/exit"))
      (should (equal (plist-get (nth 10 entries) :x) "compacted"))
      (should (numberp (plist-get (nth 0 entries) :t)))
      ;; Two entries of one line: identified by its offset, then a rank.
      (should (string-match-p "\\`[0-9]+\\'" (plist-get (nth 1 entries) :i)))
      (should (equal (plist-get (nth 2 entries) :i) (concat (plist-get (nth 1 entries) :i) ".1")))
      (should (= (plist-get page :from) 0))
      (should-not (plist-get page :more))
      (should (= (plist-get page :to) (plist-get page :sz)))
      (should (equal (plist-get page :g) (drover-conversation-generation file))))))

(ert-deftest drover-conversation-test-not-ascii ()
  "A tool's input not ASCII reads as characters, bounded by them, and serializes again."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (id drover-conversation-test--id)
           (file (drover-fixture-claude-file id directory))
           (long (concat "« " (make-string 5000 ?é) " »"))
           (session (drover-conversation-test--session 'claude-code id directory)))
      (drover-fixture-write
       file
       (list
        (drover-fixture-claude-user id directory 1790000000.0 "Rename it")
        (drover-fixture-line
         :type "assistant" :isSidechain :false :uuid "a1" :timestamp (drover-fixture-time 1790000001.0)
         :message (list :role "assistant" :model "claude-opus-5-5" :id "msg_1"
                        :content (vector (list :type "tool_use" :id "toolu_1" :name "Edit"
                                               :input (list :replace_all :null :file_path "C:\\Users\\é.txt"
                                                            :old_string "a → b" :new_string "a … b"))
                                         (list :type "tool_use" :id "toolu_2" :name "Write"
                                               :input (list :file_path "b.txt" :content long)))))))
      (let* ((page (drover-conversation-test--page session))
             (tools (seq-filter (lambda (entry) (equal (plist-get entry :y) "tool")) (plist-get page :e)))
             (input (plist-get (car tools) :in)))
        (should (= (length tools) 2))
        (should (string-search "\"old_string\":\"a → b\"" input))
        (should (string-search "é.txt" input))
        ;; Cut at characters, never within one's bytes.
        (should (= (length (plist-get (cadr tools) :in))
                   (1+ (alist-get 'tool drover-conversation-bounds))))
        (should (string-suffix-p "é…" (plist-get (cadr tools) :in)))
        ;; As the remote layer sends it.
        (should (json-serialize page :null-object nil :false-object :false))))))

(ert-deftest drover-conversation-test-codex ()
  "A Codex rollout reads as entries, its messages told once."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (id "019a0000-0000-7000-8000-000000000001")
           (file (drover-fixture-codex id directory))
           (session (drover-conversation-test--session 'codex id directory))
           (entries (append (plist-get (drover-conversation-test--page session) :e) nil)))
      (drover-fixture-write file (list (drover-fixture-codex-event 1790000010.0 "patch_apply_end"
                                                                   :call_id "call_1" :success t :status "completed"
                                                                   :changes (list :src/csv.rs (list :type "add" :content "one\n"))))
                            t)
      (setq entries (append (plist-get (drover-conversation-test--page session) :e) nil))
      (should (equal (mapcar (lambda (entry) (plist-get entry :y)) entries)
                     '("user" "tool" "res" "text" "patch")))
      (should (equal (plist-get (nth 0 entries) :x) "Add the export command"))
      (should (equal (plist-get (nth 1 entries) :n) "apply_patch"))
      (should (equal (plist-get (nth 2 entries) :x) "Done"))
      (should (equal (plist-get (nth 3 entries) :x) "The export command writes CSV."))
      (should (equal (plist-get (nth 4 entries) :f) "src/csv.rs"))
      (should (equal (plist-get (nth 4 entries) :st) "added")))))

(ert-deftest drover-conversation-test-pages ()
  "Pages read backward, concatenated, are one whole read, whatever cuts them.
Windows of a few bytes cut lines and characters of several bytes."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (id drover-conversation-test--id)
           (file (drover-fixture-claude-file id directory))
           (session (drover-conversation-test--session 'claude-code id directory)))
      (drover-fixture-write file (mapcar (lambda (i)
                                           (drover-fixture-claude-user id directory (+ 1790000000.0 i)
                                                                       (format "Prompt %d — 日本語 🚀" i)))
                                         (number-sequence 1 300)))
      (let ((whole (drover-conversation-test--all session 200)))
        (should (= (length whole) 300))
        (should (equal (plist-get (car whole) :x) "Prompt 1 — 日本語 🚀"))
        (should (equal (plist-get (car (last whole)) :x) "Prompt 300 — 日本語 🚀"))
        (should (equal (drover-conversation-test--all session 7) whole))
        (let ((drover-conversation-window 37))
          (should (equal (drover-conversation-test--all session 13) whole)))))))

(ert-deftest drover-conversation-test-long-lines ()
  "A line of 500 kB is told by its start, unparsed; texts are bounded; a line being written waits."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (id drover-conversation-test--id)
           (file (drover-fixture-claude-file id directory))
           (session (drover-conversation-test--session 'claude-code id directory)))
      (drover-fixture-write file (list (drover-fixture-claude-user id directory 1790000000.0 "Before")
                                       (drover-fixture-claude-user id directory 1790000001.0
                                                                   (make-string 500000 ?L))
                                       (drover-fixture-claude-user id directory 1790000002.0
                                                                   (make-string 200000 ?m))
                                       (drover-fixture-claude-user id directory 1790000003.0 "After")))
      (let* ((start (float-time))
             (entries (append (plist-get (drover-conversation-test--page session) :e) nil)))
        (should (< (- (float-time) start) 5))
        (should (equal (mapcar (lambda (entry) (plist-get entry :y)) entries) '("user" "meta" "user" "user")))
        (should (string-match-p "too long" (plist-get (nth 1 entries) :x)))
        (should (<= (length (plist-get (nth 1 entries) :x)) 400))
        (should (= (length (plist-get (nth 2 entries) :x)) 32001))
        (should (= (plist-get (nth 2 entries) :cut) 200000)))
      ;; A line still being written is not read until whole.
      (drover-fixture-write file (list "{\"type\":\"user\",\"mess") t)
      (should (= (length (plist-get (drover-conversation-test--page session) :e)) 4)))))

(ert-deftest drover-conversation-test-follow ()
  "Lines appended are followed, whole; a file replaced or shrunk ends it."
  (drover-test-with-homes
    (let* ((directory (file-name-as-directory (expand-file-name "work" home)))
           (id drover-conversation-test--id)
           (file (drover-fixture-claude-file id directory))
           (session (drover-conversation-test--session 'claude-code id directory))
           (events nil)
           (drover-conversation-follow-interval 0.05))
      (drover-fixture-write file (list (drover-fixture-claude-user id directory 1790000000.0 "First")))
      (let* ((page (drover-conversation-test--page session))
             (stop (drover-conversation-follow session (plist-get page :to) (plist-get page :g)
                                               (lambda (event) (push event events)))))
        (cl-flet ((wait (predicate)
                    (let ((end (+ (float-time) 5)))
                      (while (and (not (funcall predicate)) (< (float-time) end))
                        (accept-process-output nil 0.02)))))
          (drover-fixture-claude-append file (drover-fixture-claude-assistant id directory 1790000001.0 "Second"))
          ;; Half a line, then its rest: told once whole.
          (drover-fixture-write file (list "{\"type\":\"user\",\"isSidechain\":false,") t)
          (wait (lambda () events))
          (should (equal (mapcar (lambda (entry) (plist-get entry :x)) (plist-get (car events) :entries))
                         '("Second")))
          (drover-fixture-write file (list "\"message\":{\"role\":\"user\",\"content\":\"Third\"}}\n") t)
          (wait (lambda () (cdr events)))
          (should (equal (mapcar (lambda (entry) (plist-get entry :x)) (plist-get (car events) :entries))
                         '("Third")))
          (should (= (plist-get (car events) :to) (file-attribute-size (file-attributes file))))
          ;; Shrunk: the end.
          (drover-fixture-write file (list (drover-fixture-claude-user id directory 1790000000.0 "New")))
          (wait (lambda () (plist-get (car events) :reset)))
          (should (plist-get (car events) :reset))
          (should-not drover-conversation--follows)
          (should-not drover-conversation--timer)
          (funcall stop))))))

(ert-deftest drover-conversation-test-unread ()
  "An engine recording nothing drover reads tells so."
  (let ((session (drover-conversation-test--session 'opencode "ses_1" "/x/")))
    (should-not (drover-conversation-readable-p session))
    (should (plist-get (drover-conversation-test--page session) :failure))))

(provide 'drover-conversation-test)
;;; drover-conversation-test.el ends here
