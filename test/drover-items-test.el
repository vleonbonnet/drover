;;; drover-items-test.el --- Tests for the lists of items of sessions  -*- lexical-binding: t; -*-

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

;; The lists of items of `drover-items' on the fake backend: kept in
;; their file across a restart, given and read through the tools of
;; `drover-anvil', their links checked, the statuses of headings, the
;; session asking the user for a review; the view, where items go, and
;; the prompt applying the choices, through the queue of scheduled
;; prompts, as a hand-off's.  Org files are temporary, places read with
;; `org-refile-get-location' answered by the tests.

;;; Code:

(require 'ert)
(require 'drover-test-helpers)
;; Their fixtures: the caller of the tools, a session taking prompts.
(require 'drover-anvil-test)
(require 'drover-handoff-test)
(require 'drover-items)
(require 'drover-keys)

;;;; Fixtures

(defconst drover-items-test-org
  "* Projects
** Drover
:PROPERTIES:
:ID: PLACE-DROVER
:END:
** Notes
* Items
** TODO Fix the cache key
:PROPERTIES:
:ID: H-TODO
:END:
** STARTED Port the parser
:PROPERTIES:
:ID: H-STARTED
:END:
** WAITING Hear from the vendor
:PROPERTIES:
:ID: H-WAITING
:END:
** TODO Rotate the keys
SCHEDULED: <2026-12-01 Tue>
:PROPERTIES:
:ID: H-SCHEDULED
:END:
** WAITING Renew the domain
SCHEDULED: <2026-12-02 Wed>
:PROPERTIES:
:ID: H-BLOCKED-SCHEDULED
:END:
** DONE Ship the fix
:PROPERTIES:
:ID: H-DONE
:END:
** CANCELED Drop the cache
:PROPERTIES:
:ID: H-CANCELED
:END:
** A note without a keyword
:PROPERTIES:
:ID: H-NONE
:END:
"
  "An Org file of places and of the headings of items, in every state.")

(defmacro drover-items-test-with-org (&rest body)
  "Run BODY visiting a fresh Org file of `drover-items-test-org', `file'.
The ID locations are the test's, empty; the keywords those of the
fixture: TODO, STARTED and WAITING, then DONE and CANCELED."
  (declare (indent 0) (debug t))
  `(let* ((directory (file-name-as-directory (make-temp-file "drover-items-org" t)))
          (file (expand-file-name "work.org" directory))
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-track-globally t)
          (org-id-locations-file (expand-file-name "id-locations" directory))
          (org-agenda-files nil)
          (org-refile-targets '((org-agenda-files :maxlevel . 3)))
          (org-refile-history nil)
          (drover-handoff-place-history nil)
          (org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELED")))
          (drover-task--infos (make-hash-table :test #'equal))
          (drover-task--noted (make-hash-table :test #'equal))
          (drover-task--links (make-hash-table :test #'equal))
          (drover-items--table nil))
     (unwind-protect
         (progn
           (drover-test-write file drover-items-test-org)
           (with-current-buffer (find-file-noselect file)
             (org-mode))
           ,@body)
       (when-let* ((buffer (find-buffer-visiting file)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory directory t))))

(defmacro drover-items-test-with-tools (&rest body)
  "Run BODY with the tools called by the fake session me, and the Org fixture.
me runs in `directory', its conversation conv-me; other is a session
drover knows, of conversation conv-other."
  (declare (indent 0) (debug t))
  `(drover-items-test-with-org
     (drover-anvil-test-with (list (drover-test-session "me" :engine 'fake :engine-id "conv-me"
                                                        :directory directory :status 'working)
                                   (drover-test-session "other" :engine 'fake :engine-id "conv-other"))
                             "me"
                             ,@body)))

(defun drover-items-test-set (items &rest arguments)
  "Return the answer of drover-session-items-set given ITEMS and ARGUMENTS.
ITEMS is a list of alists, as JSON objects arrive."
  (drover-anvil-test-json (apply #'drover-anvil--tool-items-set (vconcat items) arguments)))

(defun drover-items-test-refusal (items &rest arguments)
  "Return why drover-session-items-set given ITEMS and ARGUMENTS is refused."
  (cadr (should-error (apply #'drover-anvil--tool-items-set (vconcat items) arguments))))

(defun drover-items-test-items (answer)
  "Return the items of ANSWER, of a tool, by number, as (ID . ITEM)."
  (mapcar (lambda (item) (cons (alist-get 'id item) item)) (alist-get 'items answer)))

;;;; The list kept

(ert-deftest drover-items-test-survives-a-restart ()
  "The list is written as it changes: read again from its file, it holds the same items."
  (drover-items-test-with-tools
    (drover-items-test-set '(((title . "Fix the cache key") (status . "open") (link . "[[id:H-TODO][the fix]]")
                              (next . "Measure the hit rate") (note . "Seen on Core\nand on Reaver") (proposal . "session"))
                             ((title . "Decide the retry policy") (status . "decided"))))
    (let* ((key (drover-items--key (drover-index-session '(fake . "me"))))
           (before (copy-tree (drover-items--list key))))
      (should (file-exists-p (drover-items-file)))
      ;; Emacs ends, and starts again.
      (setq drover-items--table nil)
      (let ((after (drover-items--list key)))
        (should (equal (plist-get after :items) (plist-get before :items)))
        (should (equal (mapcar (lambda (item) (plist-get item :id)) (plist-get after :items)) '(1 2)))
        (should (equal (plist-get (car (plist-get after :items)) :note) "Seen on Core\nand on Reaver"))
        (should (equal (plist-get (car (plist-get after :items)) :link) '("id:H-TODO" . "the fix")))
        ;; The review asked is asked still.
        (should (plist-get after :asked))
        (should (drover-items-ask (drover-index-session '(fake . "me")))))
      ;; Another file read, as another version left it: what is not an item is left out.
      (drover-write-data (drover-items-file) "Lists" drover-items--version :lists
                         (list (list :key key :next 3 :updated 1.0 :asked nil :title "Me"
                                     :items (list (list :id 1 :title "Kept" :status 'open)
                                                  (list :id 2 :title "Broken" :status 'unknown)))))
      (setq drover-items--table nil)
      (should (equal (mapcar (lambda (item) (plist-get item :title)) (plist-get (drover-items--list key) :items))
                     '("Kept"))))))

(ert-deftest drover-items-test-unreadable-file ()
  "A file that does not read is told of, and never written over."
  (drover-items-test-with-tools
    (make-directory drover-state-directory t)
    (with-temp-file (drover-items-file) (insert "(:version 99 :lists nil)"))
    (let ((warnings nil))
      (cl-letf (((symbol-function 'drover-warn)
                 (lambda (format-string &rest arguments) (push (apply #'format format-string arguments) warnings))))
        (should (string-match-p "did not read: not writing over it"
                                (drover-items-test-refusal '(((title . "A") (status . "open"))))))
        (should (string-match-p "not read" (car (last warnings))))))
    (should (equal (with-temp-buffer (insert-file-contents (drover-items-file)) (buffer-string))
                   "(:version 99 :lists nil)"))))

;;;; The tools

(ert-deftest drover-items-test-add-update-keep ()
  "Items given are added, numbered; those given an id updated; those not given stay."
  (drover-items-test-with-tools
    (let ((answer (drover-items-test-set '(((title . "First") (status . "open"))
                                           ((title . "Second") (status . "blocked") (next . "Ask") (proposal . "fork"))))))
      (should (equal (alist-get 'outcome answer) "kept"))
      (should (equal (alist-get 'key answer) "fake/me"))
      (should (equal (mapcar #'car (drover-items-test-items answer)) '(1 2)))
      (let ((second (alist-get 2 (drover-items-test-items answer))))
        (should (equal (alist-get 'status second) "blocked"))
        (should (equal (alist-get 'goes second) "fork"))
        (should (equal (alist-get 'chosen_by second) "agent")))
      (should (equal (alist-get 'goes (alist-get 1 (drover-items-test-items answer))) "keep")))
    ;; One updated, one added; the first, not given, stays.
    (let ((answer (drover-items-test-set '(((id . 2) (title . "Second, sharper") (status . "open") (next . ""))
                                           ((title . "Third") (status . "scheduled"))))))
      (let ((items (drover-items-test-items answer)))
        (should (equal (mapcar #'car items) '(1 2 3)))
        (should (equal (alist-get 'title (alist-get 1 items)) "First"))
        (should (equal (alist-get 'title (alist-get 2 items)) "Second, sharper"))
        (should (equal (alist-get 'status (alist-get 2 items)) "open"))
        ;; An empty field clears it; one not given stays.
        (should-not (alist-get 'next (alist-get 2 items)))
        (should (equal (alist-get 'goes (alist-get 2 items)) "fork"))))
    ;; The items as an array in a string, as some clients send them.
    (should (equal (mapcar #'car (drover-items-test-items
                                  (drover-anvil-test-json
                                   (drover-anvil--tool-items-set "[{\"title\": \"Fourth\", \"status\": \"done\"}]"
                                                                 nil :json-false))))
                   '(1 2 3 4)))
    ;; Read back, as the list is kept.
    (let ((answer (drover-anvil-test-json (drover-anvil--tool-items))))
      (should (equal (mapcar #'car (drover-items-test-items answer)) '(1 2 3 4)))
      (should (equal (alist-get 'goes (alist-get 4 (drover-items-test-items answer))) "none")))
    ;; Another session's, by its key, has none.
    (should (equal (alist-get 'count (drover-anvil-test-json (drover-anvil--tool-items "fake/other"))) 0))))

(ert-deftest drover-items-test-refused-items ()
  "A malformed item is refused, named; nothing of the call is kept."
  (drover-items-test-with-tools
    (drover-items-test-set '(((title . "First") (status . "open"))))
    (let ((why (drover-items-test-refusal '(((title . "No status"))
                                            ((title . "Odd") (status . "started"))
                                            ((title . "Unknown") (status . "open") (priority . "high"))
                                            ((id . 9) (title . "Gone") (status . "open"))
                                            ((title . "Where") (status . "open") (proposal . "elsewhere"))
                                            ((status . "open"))
                                            "not an object"))))
      (should (string-prefix-p "Nothing kept: " why))
      (dolist (told '("#1 \"No status\" has no status: one of open, blocked, scheduled, done, decided"
                      "#2 \"Odd\" has the status started"
                      "#3 \"Unknown\" has no field priority"
                      "#4 \"Gone\" updates item 9, which the list does not hold"
                      "#5 \"Where\" proposes elsewhere: one of keep, session, fork, save"
                      "#6 has no title"
                      "#7 is not an object"))
        (should (string-search told why))))
    (should (equal (mapcar #'car (drover-items-test-items (drover-anvil-test-json (drover-anvil--tool-items))))
                   '(1)))))

(ert-deftest drover-items-test-links ()
  "A link that cannot be what it says is refused, each item named; web links and none pass."
  (drover-items-test-with-tools
    (drover-test-write (expand-file-name "notes.txt" directory) "Notes")
    (let ((why (drover-items-test-refusal
                '(((title . "Missing heading") (status . "open") (link . "[[id:NO-SUCH-ID][gone]]"))
                  ((title . "Unknown session") (status . "open") (link . "agent-session:fake/conv-nobody"))
                  ((title . "Missing file") (status . "open") (link . "[[file:no-such-file.org][plan]]"))
                  ((title . "Made up") (status . "open") (link . "sftp://reaver/srv/plan.txt"))
                  ((title . "No type") (status . "open") (link . "the plan"))
                  ((title . "Bad heading") (status . "open") (heading . "NOT-A-HEADING"))
                  ((title . "Web") (status . "open") (link . "https://example.com/plan"))
                  ((title . "None") (status . "open"))))))
      (dolist (told '("#1 \"Missing heading\" link names no heading of ID NO-SUCH-ID"
                      "drover-task-refused-files"
                      "#2 \"Unknown session\" link agent-session:fake/conv-nobody names a session drover does not know"
                      "#3 \"Missing file\" link file:no-such-file.org names a file that does not exist"
                      "#4 \"Made up\" link sftp://reaver/srv/plan.txt has a type Org does not know, sftp"
                      "#5 \"No type\" link the plan names no link type"
                      "#6 \"Bad heading\" heading names no heading of ID NOT-A-HEADING"))
        (should (string-search told why)))
      (should-not (string-search "#7" why))
      (should-not (string-search "#8" why))
      (should-not (drover-items-list-of (drover-index-session '(fake . "me")))))
    ;; What can be what it says is kept.
    (let ((items (drover-items-test-items
                  (drover-items-test-set
                   `(((title . "Heading") (status . "open") (link . "[[id:H-TODO]]"))
                     ((title . "Session") (status . "open") (link . "[known](agent-session:fake/conv-other)"))
                     ((title . "File") (status . "open") (link . "file:notes.txt::*Top"))
                     ((title . "Absolute") (status . "open") (link . ,(expand-file-name "notes.txt" directory)))
                     ((title . "Web") (status . "open") (link . "https://example.com/plan"))
                     ((title . "Mail") (status . "open") (link . "[[mailto:val@example.com][Val]]"))
                     ((title . "None") (status . "open"))
                     ((title . "Its heading") (status . "open") (heading . "[[id:H-WAITING][vendor]]")))))))
      (should (equal (alist-get 'link (alist-get 2 items)) "[[agent-session:fake/conv-other][known]]"))
      (should (equal (alist-get 'link (alist-get 1 items)) "[[id:H-TODO]]"))
      (should-not (alist-get 'link (alist-get 7 items)))
      (should (equal (alist-get 'id (alist-get 'heading (alist-get 8 items))) "H-WAITING")))))

(ert-deftest drover-items-test-heading-statuses ()
  "An item with a heading takes the heading's status, whatever the agent said, and its sessions."
  (drover-items-test-with-tools
    (drover-task-note-session '(fake . "other") "H-STARTED")
    (let* ((cases '(("H-TODO" . "open") ("H-STARTED" . "open") ("H-WAITING" . "blocked")
                    ("H-SCHEDULED" . "scheduled") ("H-BLOCKED-SCHEDULED" . "scheduled")
                    ("H-DONE" . "done") ("H-CANCELED" . "decided") ("H-NONE" . "open")))
           (answer (drover-items-test-set (mapcar (lambda (case)
                                                    `((title . ,(car case)) (status . "blocked") (heading . ,(car case))))
                                                  cases)))
           (items (alist-get 'items answer)))
      (should (equal (mapcar (lambda (item) (cons (alist-get 'title item) (alist-get 'status item))) items)
                     cases))
      ;; A done or decided item goes nowhere.
      (should (equal (mapcar (lambda (item) (alist-get 'goes item)) items)
                     '("keep" "keep" "keep" "keep" "keep" "none" "none" "keep")))
      (should (equal (alist-get 'title (alist-get 'heading (nth 1 items))) "Port the parser"))
      ;; The sessions serving a heading.
      (should (equal (mapcar (lambda (session) (alist-get 'key session)) (alist-get 'sessions (nth 1 items)))
                     '("fake/other")))
      (should-not (alist-get 'sessions (nth 0 items))))
    ;; The heading done in Org, the item is done.
    (with-current-buffer (find-buffer-visiting file)
      (goto-char (point-min))
      (search-forward "TODO Fix the cache key")
      (org-todo "DONE"))
    (should (equal (alist-get 'status (car (alist-get 'items (drover-anvil-test-json (drover-anvil--tool-items)))))
                   "done"))
    ;; The options decide.
    (let ((drover-task-decided-states '("DONE")))
      (should (eq (drover-task-org-status '(:todo "DONE" :done t)) 'decided))
      (should (eq (drover-task-org-status '(:todo "CANCELED" :done t)) 'done)))
    (let ((drover-task-blocked-states '("STARTED")))
      (should (eq (drover-task-org-status '(:todo "STARTED")) 'blocked))
      (should (eq (drover-task-org-status '(:todo "WAITING")) 'open)))))

(ert-deftest drover-items-test-other-hosts ()
  "The lists of another host's sessions are kept by that host: the tools refuse them."
  (drover-items-test-with-tools
    (let ((there (list (drover-anvil-test-peer-session "ses_1" :engine 'opencode :engine-id "ses_1"))))
      (drover-anvil-test-with-peer there
                                   (drover-backend-changed 'remote:core)
                                   (should (string-match-p "runs on core, which keeps its list"
                                                           (drover-items-test-refusal '(((title . "A") (status . "open")))
                                                                                      "remote:core/opencode/ses_1")))
                                   (should (string-match-p "runs on core" (cadr (should-error (drover-anvil--tool-items "remote:core/opencode/ses_1")))))
                                   ;; A link to it is to a session drover knows.
                                   (should (drover-items-test-set '(((title . "There") (status . "open")
                                                                     (link . "agent-session://core/opencode/ses_1")))))
                                   (should (string-match-p "kept on core"
                                                           (cadr (should-error (drover-items (drover-index-session '(remote:core . (opencode . "ses_1"))))
                                                                               :type 'user-error))))))))

;;;; Asking the user

(ert-deftest drover-items-test-review ()
  "A review asked has the session need the user once its turn ends, as a question does; A shows the list."
  (drover-items-test-with-tools
    (let ((key '(fake . "me")))
      ;; Not asked: the list is kept alone.
      (should (equal (alist-get 'review (drover-items-test-set '(((title . "Quiet") (status . "open"))) nil :json-false))
                     "none"))
      (drover-changed-now)
      (should-not (drover-session-ask (drover-index-session key)))
      ;; Asked, while its turn runs: it works on.
      (let ((answer (drover-items-test-set '(((title . "Loud") (status . "open"))))))
        (should (equal (alist-get 'review answer) "asked"))
        (should (alist-get 'asked_since answer)))
      (drover-changed-now)
      (let ((record (drover-index-session key)))
        (should (equal (plist-get (drover-session-ask record) :summary) "where its 2 items go"))
        (should (eq (drover-session-shown-status record) 'working))
        (should-not (drover-session-attention-p record)))
      ;; Its turn ended: it needs the user, its own status kept.
      (drover-anvil-test-set-status "me" 'idle)
      (drover-changed-now)
      (let ((record (drover-index-session key)))
        (should (eq (drover-session-status record) 'idle))
        (should (eq (drover-session-shown-status record) 'waiting))
        (should (drover-session-attention-p record))
        (should (member record (drover-attention-sessions)))
        (should (equal (drover-session-summary record) "asks: where its 2 items go"))
        (should (drover-session-answerable-p record))
        ;; Listed with the sessions waiting for the user, as one asking a question.
        (should (eq (caar (drover-group-trees-by-status (drover-tree (list record)))) 'waiting))
        ;; What waits for the end of its turn does not wait for the user.
        (should-not (drover-session-active-p record))
        (should-not (drover-handoff--busy-p record))
        ;; Peers get the session as its backend tells it: the review is this host's.
        (require 'drover-remote)
        (let ((wire (drover-remote-encode-record key record)))
          (should (equal (plist-get wire :status) "idle"))
          (should-not (plist-get wire :requests)))
        ;; The tools tell agents.
        (should (equal (alist-get 'asks (drover-anvil-test-json (drover-anvil--tool-session "fake/me" :json-false)))
                       "where its 2 items go"))
        ;; A shows the list.
        (save-window-excursion
          (drover-respond record)
          (should (eq (buffer-local-value 'major-mode (window-buffer (selected-window))) 'drover-items-mode))
          (should (equal (buffer-local-value 'drover-items--view-key (window-buffer (selected-window)))
                         '(fake . "conv-me")))))
      ;; Asked again, it is asked since the first time.
      (let ((since (plist-get (drover-session-ask (drover-index-session key)) :since)))
        (drover-items-test-set '(((id . 1) (title . "Quiet") (status . "done"))))
        (drover-changed-now)
        (should (equal (plist-get (drover-session-ask (drover-index-session key)) :since) since))
        (should (equal (plist-get (drover-session-ask (drover-index-session key)) :summary) "where its item goes")))
      ;; A request of its own comes first.
      (let ((drover-anvil-test-requests nil))
        (setq drover-test-sessions
              (cons (let ((copy (copy-sequence (car drover-test-sessions))))
                      (setf (drover-session-status copy) 'waiting
                            (drover-session-requests copy) (list (list :kind 'question :summary "Which one?")))
                      copy)
                    (cdr drover-test-sessions)))
        (drover-backend-changed 'fake "me")
        (drover-changed-now)
        (setq drover-test-calls nil)
        (drover-respond (drover-index-session key))
        (should (equal (car drover-test-calls) '(:respond "me")))))))

(ert-deftest drover-items-test-restored-review ()
  "Restored as drover starts, a review asked has its session need the user again."
  (drover-items-test-with-tools
    (drover-items-test-set '(((title . "Loud") (status . "open"))))
    (drover-anvil-test-set-status "me" 'idle)
    (setq drover-items--table nil)
    (drover-changed-now)
    (should-not (drover-session-ask (drover-index-session '(fake . "me"))))
    (drover-items-restore)
    (drover-changed-now)
    (should (eq (drover-session-shown-status (drover-index-session '(fake . "me"))) 'waiting))))

;;;; The view and applying

(defmacro drover-items-test-with-view (&rest body)
  "Run BODY over the hand-off fixture's session s1, holding a list, the Org fixture visited.
The list's items: 1, to a new session, proposed; 2, done; 3, with a
heading, to a fork, proposed; 4 staying; 5, saved for later, proposed.
Places are read by `org-refile-get-location', which answers the
heading Projects/Notes of the Org fixture, the place offered recorded
in `asked'."
  (declare (indent 0) (debug t))
  `(let ((read-place (symbol-function 'drover-handoff-read-place))
         (asked nil))
     (drover-items-test-with-org
       (drover-handoff-test-with
        ;; The first agenda file holds the targets of the current buffer.
        (setq org-agenda-files (list file))
        (cl-letf (((symbol-function 'drover-handoff-read-place) read-place)
                  ((symbol-function 'org-refile-get-location)
                   (lambda (prompt &optional default-buffer _new-nodes)
                     (push (list prompt (car org-refile-history) default-buffer) asked)
                     (let ((marker (with-current-buffer (find-buffer-visiting file)
                                     (goto-char (point-min))
                                     (re-search-forward "^\\*\\* Notes")
                                     (copy-marker (line-beginning-position)))))
                       (list "work.org/Projects/Notes/" file nil marker)))))
          (drover-items-set (drover-find 'fake "s1")
                            (list (list :title "Cache key" :status 'open :proposal 'session
                                        :link '("https://example.com/cache" . "the bug") :next "Measure it")
                                  (list :title "Old fix" :status 'done)
                                  (list :title "Vendor" :status 'open :heading "H-WAITING" :proposal 'fork)
                                  (list :title "Rename" :status 'open)
                                  (list :title "Benchmark" :status 'open :proposal 'save :note "Later\non"))
                            t)
          (unwind-protect
              (progn ,@body)
            (dolist (buffer (buffer-list))
              (when (eq (buffer-local-value 'major-mode buffer) 'drover-items-mode)
                (kill-buffer buffer)))))))))

(defun drover-items-test-view ()
  "Show the list of s1, and return its view."
  (drover-items (drover-find 'fake "s1")))

(defun drover-items-test-goes ()
  "Return where each item of the view's list goes, by number."
  (mapcar (lambda (item) (cons (plist-get item :id) (drover-items-goes item)))
          (plist-get (drover-items--list drover-items--view-key) :items)))

(ert-deftest drover-items-test-view-rows ()
  "An item a line: where it goes, its number, status, title, link, sessions and next step."
  (drover-items-test-with-view
    (save-window-excursion
      (with-current-buffer (drover-items-test-view)
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "\\`Items of Session s1" text))
          (dolist (row '("new session +1 +open +Cache key +the bug +Measure it"
                         "— +2 +done +Old fix"
                         "fork +3 +blocked +Vendor"
                         "here +4 +open +Rename"
                         "later +5 +open +Benchmark"))
            (should (string-match-p (concat "^" row) text))))
        ;; The columns are named.
        (should (string-match-p "Goes.*#.*Status.*Title.*Link.*Sessions.*Next step"
                                (substring-no-properties header-line-format)))
        ;; Proposals dim, as inherited values do.
        (goto-char (point-min))
        (search-forward "new session")
        (should (eq (get-text-property (match-beginning 0) 'face) 'opencode-setting-inherited))
        ;; Details: what the line cuts or leaves out.
        (should (eq (drover-items--id-at-point) 1))
        (magit-section-show (drover-items--section 5))
        (should (string-match-p "Note +Later\n +on" (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest drover-items-test-view-moves-and-choices ()
  "Keys move between items; left and right change where one goes; one done goes nowhere."
  (drover-items-test-with-view
    (save-window-excursion
      (with-current-buffer (drover-items-test-view)
        (should (eq (drover-items--id-at-point) 1))
        (drover-items-next)
        (should (eq (drover-items--id-at-point) 2))
        (drover-items-next)
        (drover-items-next)
        (should (eq (drover-items--id-at-point) 4))
        (drover-items-previous)
        (should (eq (drover-items--id-at-point) 3))
        ;; From item 4, here: a new session, a fork, later, here again.
        (drover-items-next)
        (dolist (goes '(session fork save keep session))
          (drover-items-goes-next)
          (should (eq (alist-get 4 (drover-items-test-goes)) goes))
          (should (eq (drover-items--id-at-point) 4)))
        (dolist (goes '(keep save fork))
          (drover-items-goes-previous)
          (should (eq (alist-get 4 (drover-items-test-goes)) goes)))
        ;; The user's choice shows highlighted.
        (goto-char (oref (drover-items--section 4) start))
        (should (eq (get-text-property (point) 'face) 'opencode-setting-chosen))
        ;; Done, it goes nowhere.
        (goto-char (oref (drover-items--section 2) start))
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest arguments) (push (apply #'format format-string arguments) messages))))
            (drover-items-goes-next)
            (drover-items-goes-previous))
          (should (equal messages '("Item 2 is done: it goes nowhere" "Item 2 is done: it goes nowhere"))))
        (should-not (alist-get 2 (drover-items-test-goes)))
        ;; A heading done in Org takes its item out of where it went.
        (with-current-buffer (find-buffer-visiting file)
          (goto-char (point-min))
          (search-forward "WAITING Hear from the vendor")
          (org-todo "CANCELED"))
        (revert-buffer)
        (should-not (alist-get 3 (drover-items-test-goes)))))))

(ert-deftest drover-items-test-leave-and-come-back ()
  "Leaving keeps the choices, and the review asked; coming back, and after a restart, they are there."
  (drover-items-test-with-view
    (save-window-excursion
      (let ((view (drover-items-test-view)))
        (with-current-buffer view
          (drover-items-next)
          (drover-items-next)
          (drover-items-next)
          (drover-items-goes-next)
          (drover-items-leave))
        (should-not (get-buffer-window view))
        (should (eq (alist-get 4 (drover-items-test-goes-of "s1")) 'session))
        (should (drover-items-asked (drover-items-list-of (drover-find 'fake "s1"))))
        ;; Back, the same view, the same choices.
        (should (eq (drover-items-test-view) view))
        (should (eq (get-buffer-window view) (selected-window)))
        ;; Emacs ends, and starts again.
        (setq drover-items--table nil)
        (should (eq (alist-get 4 (drover-items-test-goes-of "s1")) 'session))))))

(defun drover-items-test-goes-of (id)
  "Return where each item of the list of the fake session ID goes, by number."
  (mapcar (lambda (item) (cons (plist-get item :id) (drover-items-goes item)))
          (plist-get (drover-items-list-of (drover-find 'fake id)) :items)))

(ert-deftest drover-items-test-apply ()
  "Applying asks each heading's place once, the last offered, and tells the session where each item goes."
  (drover-items-test-with-view
    (save-window-excursion
      (let ((view (drover-items-test-view)))
        (with-current-buffer view
          (drover-items-apply))
        ;; A place for the items leaving without a heading, the one chosen last offered.
        (should (equal (mapcar #'car (reverse asked))
                       '("Item 1, Cache key, its heading goes under" "Item 5, Benchmark, its heading goes under")))
        (should (equal (mapcar #'cadr (reverse asked)) '(nil "work.org/Projects/Notes/")))
        ;; Targets of the current buffer, an Org one.
        (should (eq (nth 2 (car asked)) (find-buffer-visiting file)))
        ;; The place, a heading, has an ID now, the one thing drover wrote.
        (let ((id (with-current-buffer (find-buffer-visiting file)
                    (goto-char (point-min))
                    (re-search-forward "^\\*\\* Notes")
                    (org-entry-get nil "ID"))))
          (should id)
          (let ((text (drover-handoff-test-take)))
            (should (string-prefix-p "I went through the items of this session's list, and said where each goes:\n\n" text))
            (should (string-search (format "- Item 1, \"Cache key\" (open): to a new session.  Its heading goes under `org-id://%s` (the heading \"work.org/Projects/Notes\").\n  Link: [[https://example.com/cache][the bug]]\n  Next step: Measure it\n" id)
                                   text))
            (should (string-search "- Item 3, \"Vendor\" (blocked): to a fork of this session.  Its heading is [[id:H-WAITING][Hear from the vendor]], written already.\n" text))
            (should (string-search "- Item 4, \"Rename\" (open): stays here.\n" text))
            (should (string-search (format "- Item 5, \"Benchmark\" (open): saved for later: start nothing.  Its heading goes under `org-id://%s`" id) text))
            (should (string-search "  Note: Later\n  on" text))
            ;; Done, it goes nowhere: the prompt leaves it out.
            (should-not (string-search "Old fix" text))
            (dolist (told '("`handoff` skill" "`org-add-todo`" "a brief that reads alone" "`drover-task-start`"
                            "`drover-session-fork`" "start nothing" "`drover-session-items-set`" "`review` false"))
              (should (string-search told text)))))
        ;; Out of every window, the view left.
        (should-not (get-buffer-window view))
        (should-not (get-buffer-window drover-handoff-test-buffer))
        ;; The items told of are sent; the review is over.
        (let ((list (drover-items-list-of (drover-find 'fake "s1"))))
          (should-not (drover-items-asked list))
          (should (equal (mapcar (lambda (item) (car (plist-get item :sent))) (plist-get list :items))
                         '(session nil fork nil save))))
        (should (seq-find (lambda (message) (string-match-p "\\`Session s1 is told where 3 items go\\'" message))
                          drover-handoff-test-messages))
        ;; Applied again, nothing leaves anew: nothing is sent.
        (setq asked nil)
        (with-current-buffer view
          (drover-items-apply))
        (should-not asked)
        (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 0.5))
        (should (equal (car drover-handoff-test-messages) "No item of Session s1 leaves it: nothing sent"))))))

(ert-deftest drover-items-test-apply-draft-and-turn ()
  "The prompt never goes over a draft, and waits for a running turn to end."
  (drover-items-test-with-view
    (save-window-excursion
      (setq drover-handoff-test-blocker "the input holds a draft")
      (with-current-buffer (drover-items-test-view)
        (drover-items-apply))
      (should (drover-handoff-test-wait
               (lambda () (eq (plist-get (car (drover-items-test-jobs)) :status) 'waiting))))
      (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 1.2))
      (setq drover-handoff-test-blocker nil)
      (should (string-search "Cache key" (drover-handoff-test-take)))
      (should (drover-handoff-test-wait (lambda () (null (drover-items-test-jobs))))))
    ;; A running turn: once it ended.
    (drover-handoff-test-session :status 'working)
    (with-current-buffer (drover-items-test-view)
      (goto-char (oref (drover-items--section 4) start))
      (drover-items-goes-next)
      (drover-items-apply))
    (let ((job (car (drover-items-test-jobs))))
      (should (eq (plist-get job :turn) t))
      (should (eq (plist-get job :hidden) t)))
    (should (equal (car drover-handoff-test-messages) "Session s1 is told where 1 item goes, once its turn ends"))
    (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 0.5))
    (drover-handoff-test-session :status 'idle)
    (drover-schedule--turn-check)
    (should (string-search "- Item 4, \"Rename\" (open): to a new session." (drover-handoff-test-take)))))

(defun drover-items-test-jobs ()
  "Return the prompts of lists of items the queue holds."
  (seq-filter (lambda (job) (equal (plist-get job :origin) drover-items-origin)) (drover-schedule--jobs)))

;;;; Places of headings

(ert-deftest drover-items-test-places ()
  "A place is read as Org's refile reads one: an ID given, the top of a file, refused files and hosts."
  (drover-items-test-with-org
    (let ((answers nil)
          (offered nil)
          (prompts nil))
      (cl-letf (((symbol-function 'org-refile-get-location)
                 (lambda (prompt &optional _buffer _new)
                   (push prompt prompts)
                   (push (car org-refile-history) offered)
                   (let ((answer (pop answers)))
                     (if (functionp answer) (funcall answer) answer)))))
        ;; A heading with an ID keeps it; the top of a file is named by its path.
        (setq answers (list (list "work.org/Projects/Drover/" file nil
                                  (with-current-buffer (find-buffer-visiting file)
                                    (goto-char (point-min))
                                    (search-forward "** Drover")
                                    (copy-marker (line-beginning-position))))))
        (should (equal (drover-handoff-read-place "Where")
                       (list :uri "org-id://PLACE-DROVER" :name "work.org/Projects/Drover" :file file
                             :id "PLACE-DROVER")))
        (setq answers (list (list "work.org" file nil nil)))
        (should (equal (plist-get (drover-handoff-read-place "Where") :uri)
                       (concat "org-headline://" (string-replace "#" "%23" file))))
        ;; The place chosen last is offered first.
        (should (equal offered '("work.org/Projects/Drover/" nil)))
        (should (equal drover-handoff-place-history '("work.org" "work.org/Projects/Drover/")))
        ;; For another host's agent, a heading alone: asked again, saying why.
        (setq answers (list (list "work.org" file nil nil)
                            (list "work.org/Projects/Drover/" file nil
                                  (with-current-buffer (find-buffer-visiting file)
                                    (goto-char (point-min))
                                    (search-forward "** Drover")
                                    (point))))
              prompts nil)
        (should (equal (plist-get (drover-handoff-read-place "Where" nil t) :id) "PLACE-DROVER"))
        (should (equal (car prompts) "The top of a file has no ID, which another host's agent finds a place by.  Where"))
        ;; A file tasks are never written in.
        (let ((drover-task-refused-files (list directory)))
          (setq answers (list (list "work.org" file nil nil)) prompts nil)
          (should-error (progn
                          (cl-letf (((symbol-function 'org-refile-get-location)
                                     (lambda (prompt &rest _)
                                       (push prompt prompts)
                                       (if (cdr prompts) (user-error "Stopped") (list "work.org" file nil nil)))))
                            (drover-handoff-read-place "Where")))
                        :type 'user-error)
          (should (string-prefix-p "Headings are not written in " (car prompts))))
        ;; A place offered that is gone is dropped, and asked again.
        (setq drover-handoff-place-history '("work.org/Gone/")
              offered nil
              answers (list (lambda () (user-error "Invalid target location"))
                            (list "work.org" file nil nil)))
        (should (drover-handoff-read-place "Where"))
        (should (equal (reverse offered) '("work.org/Gone/" nil)))
        (should (equal drover-handoff-place-history '("work.org")))))))

(ert-deftest drover-items-test-place-id-written ()
  "A heading chosen as a place without an ID gets one, its file saved; nothing else changes."
  (drover-items-test-with-org
    (let* ((buffer (find-buffer-visiting file))
           (marker (with-current-buffer buffer
                     (goto-char (point-min))
                     (search-forward "** Notes")
                     (copy-marker (line-beginning-position))))
           (id (drover-task-org-place-id marker)))
      (should (string-match-p drover-items--id-regexp id))
      (should-not (buffer-modified-p buffer))
      (should (string-search (format "** Notes\n:PROPERTIES:\n:ID:       %s\n:END:\n* Items" id)
                             (with-temp-buffer (insert-file-contents file) (buffer-string))))
      ;; Asked again, the same.
      (should (equal (drover-task-org-place-id marker) id)))))

;;;; The menu and Anvil

(ert-deftest drover-items-test-menu ()
  "The menu shows the list of the session at point, its open items counted, and the review asked."
  (drover-items-test-with-view
    (let ((session (drover-find 'fake "s1")))
      (should (drover-items-shown-p session))
      (should-not (drover-items-shown-p (drover-test-session "none")))
      (cl-letf (((symbol-function 'drover-dispatch-session) (lambda (&rest _) session)))
        (should (equal (substring-no-properties (drover--dispatch-items-description)) "Items 4 review"))))
    (should (eq (keymap-lookup drover-session-map "*") #'drover-items))))

(ert-deftest drover-items-test-registered ()
  "Both tools are registered with Anvil, under their names and arguments."
  (drover-anvil-test-with (list (drover-test-session "a")) nil
                          (drover-anvil-test-with-anvil
                           (let* ((list (json-parse-string
                                         (anvil-server-process-jsonrpc
                                          (json-serialize '((jsonrpc . "2.0") (id . 1) (method . "tools/list")))
                                          drover-anvil-server-id)
                                         :object-type 'alist :array-type 'list))
                                  (tools (alist-get 'tools (alist-get 'result list))))
                             (pcase-dolist (`(,name ,required . ,arguments)
                                            '(("drover-session-items-set" ("items") "items" "key" "review")
                                              ("drover-session-items" nil "key")))
                               (let* ((tool (seq-find (lambda (tool) (equal (alist-get 'name tool) name)) tools))
                                      (schema (alist-get 'inputSchema tool)))
                                 (should tool)
                                 (should (equal (sort (mapcar (lambda (property) (symbol-name (car property)))
                                                              (alist-get 'properties schema))
                                                      #'string<)
                                                arguments))
                                 (should (equal (alist-get 'required schema) required))))
                             (let ((items-set (seq-find (lambda (tool) (equal (alist-get 'name tool) "drover-session-items-set")) tools)))
                               (should (equal (alist-get 'type (alist-get 'items (alist-get 'properties (alist-get 'inputSchema items-set))))
                                              "array"))
                               (should (equal (alist-get 'type (alist-get 'review (alist-get 'properties (alist-get 'inputSchema items-set))))
                                              "boolean"))))
                           ;; Through the dispatcher, the items arrive as JSON makes them.
                           (let ((answer (drover-anvil-test-json
                                          (drover-anvil-test-tool-text
                                           (anvil-server-process-jsonrpc
                                            (drover-anvil-test-call 2 "drover-session-items-set"
                                                                    '((key . "fake/a")
                                                                      (items . [((title . "Through Anvil") (status . "open"))])
                                                                      (review . :json-false)))
                                            drover-anvil-server-id)))))
                             (should (equal (alist-get 'title (car (alist-get 'items answer))) "Through Anvil"))
                             (should (equal (alist-get 'review answer) "none")))
                           (should (string-match-p "has a type Org does not know, sftp"
                                                   (drover-anvil-test-tool-text
                                                    (anvil-server-process-jsonrpc
                                                     (drover-anvil-test-call 3 "drover-session-items-set"
                                                                             '((key . "fake/a")
                                                                               (items . [((title . "Made up") (status . "open")
                                                                                          (link . "sftp://x/y"))])))
                                                     drover-anvil-server-id)))))))

(provide 'drover-items-test)
;;; drover-items-test.el ends here
