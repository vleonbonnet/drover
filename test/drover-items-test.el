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
;; `drover-anvil', their links checked, the statuses of headings and
;; the sessions serving them, a review asked, which a session's row
;; tells; the side list, shown and hidden, the session it follows, its
;; lines and details, where items go, and the prompt applying the
;; choices, through the queue of scheduled prompts, as a hand-off's.
;; Org files are temporary, places read with `org-refile-get-location'
;; answered by the tests.

;;; Code:

(require 'ert)
(require 'drover-test-helpers)
;; Their fixtures: the caller of the tools, a session taking prompts.
(require 'drover-anvil-test)
(require 'drover-handoff-test)
(require 'drover-items)
(require 'drover-keys)
(require 'drover-attention)

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
                              (next . "Measure the hit rate") (note . "Seen on Core\nand on Reaver"))
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
        (should (drover-items-review-since (drover-index-session '(fake . "me")))))
      ;; Another file read, as another version left it: what is not an item is left out,
      ;; and what an agent proposed of where one goes: it stays.
      (drover-write-data (drover-items-file) "Lists" drover-items--version :lists
                         (list (list :key key :next 3 :updated 1.0 :asked nil :title "Me"
                                     :items (list (list :id 1 :title "Kept" :status 'open :proposal 'session)
                                                  (list :id 2 :title "Broken" :status 'unknown)))))
      (setq drover-items--table nil)
      (let ((items (plist-get (drover-items--list key) :items)))
        (should (equal (mapcar (lambda (item) (plist-get item :title)) items) '("Kept")))
        (should-not (plist-member (car items) :proposal))
        (should (eq (drover-items-goes (car items)) 'keep))))))

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
                                           ((title . "Second") (status . "blocked") (next . "Ask"))))))
      (should (equal (alist-get 'outcome answer) "kept"))
      (should (equal (alist-get 'key answer) "fake/me"))
      (should (equal (mapcar #'car (drover-items-test-items answer)) '(1 2)))
      ;; Every item stays in its session until the user says otherwise.
      (dolist (item (alist-get 'items answer))
        (should (equal (alist-get 'goes item) "keep"))
        (should (equal (alist-get 'chosen_by item) "default")))
      (should (equal (alist-get 'status (alist-get 2 (drover-items-test-items answer))) "blocked")))
    ;; Where the user said one goes is told, a subagent as such.
    (let ((list (drover-items-list-of (drover-index-session '(fake . "me")))))
      (setf (plist-get (nth 1 (plist-get list :items)) :goes) 'subagent))
    (let ((second (alist-get 2 (drover-items-test-items (drover-anvil-test-json (drover-anvil--tool-items))))))
      (should (equal (alist-get 'goes second) "subagent"))
      (should (equal (alist-get 'chosen_by second) "user")))
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
        (should (equal (alist-get 'goes (alist-get 2 items)) "subagent"))))
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
                                            ;; An agent says nothing of where an item goes.
                                            ((title . "Where") (status . "open") (proposal . "session"))
                                            ((status . "open"))
                                            "not an object"))))
      (should (string-prefix-p "Nothing kept: " why))
      (dolist (told '("#1 \"No status\" has no status: one of open, blocked, scheduled, done, decided"
                      "#2 \"Odd\" has the status started"
                      "#3 \"Unknown\" has no field priority"
                      "#4 \"Gone\" updates item 9, which the list does not hold"
                      "#5 \"Where\" has no field proposal: an item has title, status, link, next, note, heading, id"
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
      (should-not (alist-get 'archived (car (alist-get 'sessions (nth 1 items)))))
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
                                   (should-not (drover-items-review-since
                                                (drover-index-session '(remote:core . (opencode . "ses_1")))))))))

;;;; The sessions serving a heading

(ert-deftest drover-items-test-archived-sessions ()
  "An item tells the sessions that served its heading and were archived since.
A session that takes an item is wrapped up once it is done, and
archived: by drover, or by its backend, which tells of it once the
archived sessions are fetched."
  (drover-items-test-with-tools
    (let ((there nil))
      (drover-handoff-test-add
       :archived (lambda (callback)
                   (funcall callback
                            (list (drover-test-session "wrapped" :engine 'fake :engine-id "conv-wrapped"
                                                       :archived (- drover-test-now 60) :status 'idle))))
       :archive (lambda (session) (push (drover-session-id session) there)))
      (setq drover-test-sessions
            (append drover-test-sessions
                    (list (drover-test-session "closing" :engine 'fake :engine-id "conv-closing" :status 'idle))))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      ;; One its backend archived, whose first prompt links the heading; one drover archives.
      (drover-task-note-session '(fake . "wrapped") "H-DONE")
      (drover-task-note-session '(fake . "closing") "H-DONE")
      (drover-task-note-session '(fake . "other") "H-DONE")
      (drover-items-test-set '(((title . "Shipped") (status . "open") (heading . "H-DONE"))) nil :json-false)
      (let ((drover-archive-function nil))
        (drover-archive-session (drover-index-session '(fake . "closing"))))
      (setq drover-test-sessions (seq-remove (lambda (session) (equal (drover-session-id session) "closing"))
                                             drover-test-sessions))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      (should-not (seq-find (lambda (session) (equal (drover-session-id session) "wrapped")) (drover-sessions)))
      ;; Read: the archived sessions are fetched for its headings, and told as archived.
      (let* ((answer (drover-anvil-test-json (drover-anvil--tool-items)))
             (sessions (alist-get 'sessions (car (alist-get 'items answer)))))
        (should (memq 'archived drover--fetch-kept))
        (should (equal (mapcar (lambda (session) (alist-get 'key session)) sessions)
                       '("fake/other" "fake/wrapped")))
        (should-not (alist-get 'archived (nth 0 sessions)))
        (should (eq (alist-get 'archived (nth 1 sessions)) t))
        (should (equal (alist-get 'title (nth 1 sessions)) "Session wrapped")))
      ;; The live ones alone serve what waits for them.
      (let ((info (drover-items-heading-info (car (plist-get (drover-items-list-of (drover-index-session '(fake . "me")))
                                                            :items)))))
        (should (equal (mapcar #'drover-session-id (drover-task-sessions info)) '("other")))
        (should (equal (mapcar #'drover-session-id (drover-task-sessions info t)) '("other" "wrapped")))))))

;;;; Reviews

(ert-deftest drover-items-test-review ()
  "A review asked is no request of its session: its row tells it, and A answers its own requests."
  (drover-items-test-with-tools
    (let ((key '(fake . "me")))
      ;; Not asked: the list is kept alone.
      (should (equal (alist-get 'review (drover-items-test-set '(((title . "Quiet") (status . "open"))) nil :json-false))
                     "none"))
      (should-not (drover-items-review-since (drover-index-session key)))
      ;; Asked.
      (let ((answer (drover-items-test-set '(((title . "Loud") (status . "open"))))))
        (should (equal (alist-get 'review answer) "asked"))
        (should (alist-get 'asked_since answer))
        (should (equal (alist-get 'note answer) "The user is asked to review the list")))
      (drover-anvil-test-set-status "me" 'idle)
      (drover-changed-now)
      (let ((record (drover-index-session key)))
        (should (drover-items-review-since record))
        ;; Its session stays as its backend tells it, and does not need the user for it.
        (should (eq (drover-session-status record) 'idle))
        (should-not (drover-session-attention-p record))
        (should-not (member record (drover-attention-sessions)))
        (should-not (drover-session-to-answer record))
        (should-not (equal (drover-session-summary record) "asks: where its 2 items go"))
        ;; The tools tell agents of the session as before.
        (let ((data (drover-anvil-test-json (drover-anvil--tool-session "fake/me" :json-false))))
          (should-not (assq 'asks data))
          (should-not (alist-get 'needs_user data)))
        ;; A answers the session's requests, as ever, and shows no list.
        (setq drover-test-calls nil)
        (save-window-excursion
          (drover-respond record)
          (should (equal (car drover-test-calls) '(:respond "me")))
          (should-not (drover-items--buffer))))
      ;; Asked again, it is asked since the first time.
      (let ((since (drover-items-review-since (drover-index-session key))))
        (drover-items-test-set '(((id . 1) (title . "Quiet") (status . "done"))))
        (should (equal (drover-items-review-since (drover-index-session key)) since))))))

(ert-deftest drover-items-test-review-icon ()
  "While its list waits for a review, a session's row shows the review icon in drover's lists."
  (drover-items-test-with-tools
    (let ((drover-list-review-icon "§")
          (drover-list-age-interval nil)
          (drover-group-by nil)
          (opencode-session-status-animation-interval nil)
          (buffer nil))
      (cl-flet ((row (id)
                  (with-current-buffer buffer
                    (drover-list--refresh-if-needed)
                    (let ((section (drover-list--find-session (cons 'fake id))))
                      (buffer-substring (oref section start) (oref section content))))))
        (unwind-protect
            (progn
              (drover-anvil-test-set-status "me" 'idle)
              (drover-changed-now)
              (setq buffer (drover-list--buffer))
              (with-current-buffer buffer
                (revert-buffer))
              (should-not (string-search "§" (row "me")))
              ;; Asked: the icon, in its face, with its tooltip, on its row alone.
              (drover-items-test-set '(((title . "Loud") (status . "open"))))
              (drover-changed-now)
              (with-current-buffer buffer
                (setq drover-list--stale t))
              (let* ((text (row "me"))
                     (at (string-search "§" text)))
                (should at)
                (should (eq (get-text-property at 'face text) 'drover-list-review))
                (should (equal (get-text-property at 'help-echo text) "Its list of items waits for your review"))
                ;; After its status, before its title.
                (should (< at (string-search "Session me" text))))
              (should-not (string-search "§" (row "other")))
              ;; Applied, or as here read with no review left, it goes.
              (let ((list (drover-items-list-of (drover-index-session '(fake . "me")))))
                (setf (plist-get list :asked) nil)
                (drover-items--changed (plist-get list :key)))
              (drover-changed-now)
              (with-current-buffer buffer
                (setq drover-list--stale t))
              (should-not (string-search "§" (row "me"))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest drover-items-test-restored-review ()
  "Restored as drover starts, the lists are read, and a review asked is told again."
  (drover-items-test-with-tools
    (drover-items-test-set '(((title . "Loud") (status . "open"))))
    (drover-anvil-test-set-status "me" 'idle)
    (setq drover-items--table nil)
    (drover-changed-now)
    ;; Not read yet: rows ask nothing of the file.
    (should-not (drover-items-review-since (drover-index-session '(fake . "me"))))
    (let ((changed nil))
      (add-hook 'drover-changed-hook (lambda () (setq changed drover-changed-sessions)))
      (drover-items-restore)
      (drover-changed-now)
      ;; Its rows are drawn again.
      (should (member '(fake . "me") changed)))
    (should (drover-items-review-since (drover-index-session '(fake . "me"))))
    (should-not (drover-session-attention-p (drover-index-session '(fake . "me"))))))

;;;; The side list and applying

(defmacro drover-items-test-with-view (&rest body)
  "Run BODY over the hand-off fixture's session s1, holding a list, the Org fixture visited.
The list's items: 1, with a link and a next step; 2, done; 3, with a
heading, WAITING; 4; 5, with a note.  Places are read by
`org-refile-get-location', which answers the heading Projects/Notes of
the Org fixture, the place offered recorded in `asked'.  The frame is
160 columns wide, so that both side lists show; the icons are plain
characters, one column wide in every Emacs."
  (declare (indent 0) (debug t))
  `(let ((read-place (symbol-function 'drover-handoff-read-place))
         (size (cons (frame-width) (frame-height)))
         (asked nil)
         (drover-list-side-window-side 'left)
         (drover-list-side-window-width 36)
         (drover-list-side-window-slot 0)
         (drover-list-age-interval nil)
         (opencode-session-status-icons '((waiting . "!") (failed . "✗") (done . "✓")
                                          (retrying . "↻") (working . "◐") (idle . "·")))
         (opencode-session-status-animation-interval nil)
         (opencode-archived-icon "▣")
         (drover-list-review-icon "§")
         (drover-items-destination-icons '((keep . "=") (subagent . "@") (session . "+") (fork . "Y") (save . "~")))
         (drover-items-status-icons '((open . "o") (blocked . "x") (scheduled . "s") (done . "v") (decided . "d"))))
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
                            (list (list :title "Cache key" :status 'open
                                        :link '("https://example.com/cache" . "the bug") :next "Measure it")
                                  (list :title "Old fix" :status 'done)
                                  (list :title "Vendor" :status 'open :heading "H-WAITING")
                                  (list :title "Rename" :status 'open)
                                  (list :title "Benchmark" :status 'open :note "Later\non"))
                            t)
          (unwind-protect
              (progn
                (set-frame-size (selected-frame) 160 50)
                (switch-to-buffer drover-handoff-test-buffer)
                ,@body)
            (dolist (buffer (buffer-list))
              (when (or (eq (buffer-local-value 'major-mode buffer) 'drover-items-mode)
                        (with-current-buffer buffer (derived-mode-p 'drover-list-mode)))
                (kill-buffer buffer)))
            (let ((ignore-window-parameters t))
              (delete-other-windows (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                                              (window-list nil 'nomini))))
            (switch-to-buffer "*scratch*")
            (drover-list--stop-timer 'drover-list--place-timer)
            (set-frame-parameter nil 'drover-list-sidebar-aside nil)
            (set-frame-parameter nil 'drover-list-sidebar-kept nil)
            (set-frame-size (selected-frame) (car size) (cdr size))))))))

(defun drover-items-test-view ()
  "Show the side list of items, select it, and return its buffer."
  (unless (drover-items--side-window)
    (drover-items))
  (select-window (drover-items--side-window))
  (window-buffer))

(defun drover-items-test-lines ()
  "Return the lines of the side list that show, without their properties."
  (with-current-buffer (drover-items--buffer)
    (let ((lines nil))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          ;; Hidden by its section, not by the invisible character its line begins with.
          (unless (seq-some (lambda (overlay) (overlay-get overlay 'invisible)) (overlays-at (point)))
            (push (buffer-substring-no-properties (if (get-text-property (point) 'invisible) (1+ (point)) (point))
                                                  (line-end-position))
                  lines))
          (forward-line 1)))
      (nreverse lines))))

(defun drover-items-test-goes ()
  "Return where each item of the side list's list goes, by number."
  (drover-items-test-goes-of "s1"))

(defun drover-items-test-goes-of (id)
  "Return where each item of the list of the fake session ID goes, by number."
  (mapcar (lambda (item) (cons (plist-get item :id) (drover-items-goes item)))
          (plist-get (drover-items-list-of (drover-find 'fake id)) :items)))

(defun drover-items-test-choose (number goes)
  "Say the item NUMBER of the list of s1 goes to GOES, as the user's choice."
  (with-current-buffer (drover-items--buffer t)
    (let ((drover-items--view-key (drover-items--key (drover-find 'fake "s1"))))
      (drover-items--set-goes (drover-items--find (drover-items--list drover-items--view-key) number) goes))))

(ert-deftest drover-items-test-side-toggle ()
  "The side list shows and hides with one command, on the side opposite to the sidebar's."
  (drover-items-test-with-view
    (let ((origin (selected-window)))
      (should-not (drover-items--side-window))
      (drover-items)
      (let ((window (drover-items--side-window)))
        (should (window-live-p window))
        ;; Shown, not selected, as the sidebar is.
        (should (eq (selected-window) origin))
        (should (equal (buffer-name (window-buffer window)) "*drover items*"))
        (should (eq (window-parameter window 'window-side) 'right))
        ;; The sidebar's width, slot and settings.
        (should (= (window-total-width window) 36))
        (should (eql (window-parameter window 'window-slot) 0))
        (should (window-parameter window 'no-delete-other-windows))
        (should (window-parameter window 'no-other-window))
        (should (eq (window-dedicated-p window) t))
        (should (window-preserved-size window t))
        (should (equal (seq-take (window-fringes window) 2) '(0 0)))
        (with-current-buffer (window-buffer window)
          (should (eq major-mode 'drover-items-mode))
          ;; The sidebar's look: its face, no cursor, no header line of columns.
          (should drover-list-sidebar-look)
          (should (memq 'drover-list (alist-get 'default face-remapping-alist)))
          (should-not cursor-type)
          (should-not header-line-format)))
      ;; Beside the sidebar, each on its side.
      (drover-list-side)
      (should (eq (window-parameter (drover-list--side-window) 'window-side) 'left))
      (should (window-live-p (drover-items--side-window)))
      (should (eq (selected-window) origin))
      ;; Again, it hides; its buffer stays.
      (drover-items)
      (should-not (drover-items--side-window))
      (should (get-buffer "*drover items*"))
      (should (window-live-p (drover-list--side-window)))
      (should (eq (selected-window) origin))
      ;; With a prefix argument, the same key moves there and back.
      (drover-items '(4))
      (should (eq (selected-window) (drover-items--side-window)))
      (drover-items '(4))
      (should (eq (selected-window) origin))
      (drover-items '(4))
      ;; q hides it, as * does there.
      (should (eq (key-binding (kbd "*")) #'drover-items))
      (call-interactively (key-binding (kbd "q")))
      (should-not (drover-items--side-window))
      (should (eq (selected-window) origin))
      ;; The sidebar on the right, it shows on the left.
      (let ((drover-list-side-window-side 'right))
        (drover-items)
        (should (eq (window-parameter (drover-items--side-window) 'window-side) 'left))))))

(ert-deftest drover-items-test-follows-the-session ()
  "The side list shows the list of the session the user is on, and follows as that changes."
  (drover-items-test-with-view
    (let ((origin (selected-window)))
      (setq drover-test-sessions
            (append drover-test-sessions
                    (list (drover-test-session "s2" :engine 'handfake :engine-id "conv-2" :status 'working)
                          (drover-test-session "s3" :engine 'handfake :engine-id "conv-3" :status 'idle))))
      (drover-backend-changed 'fake)
      (drover-changed-now)
      (drover-items-set (drover-find 'fake "s2") (list (list :title "Only one" :status 'blocked)) nil)
      ;; The session of the selected window's buffer.
      (drover-items)
      (should (equal (car (drover-items-test-lines)) "· Session s1"))
      (should (equal (length (drover-items-test-lines)) 7))
      ;; Nothing changed: it stays.
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "· Session s1"))
      ;; In a list of sessions, the session at point.
      (drover-list-side '(4))
      (with-current-buffer (window-buffer (selected-window))
        (goto-char (oref (drover-list--find-session '(fake . "s2")) start))
        (drover-items--follow)
        (should (equal (drover-items-test-lines) '("◐ Session s2" "= x 1 Only one")))
        ;; A session without a list has one line saying so.
        (goto-char (oref (drover-list--find-session '(fake . "s3")) start))
        (drover-items--follow)
        (should (equal (drover-items-test-lines) '("· Session s3" "No items"))))
      ;; From a buffer telling no session, the session at point in the list shown.
      (select-window origin)
      (switch-to-buffer (get-buffer-create "*other notes*"))
      (with-current-buffer (window-buffer (drover-list--side-window))
        (set-window-point (drover-list--side-window) (oref (drover-list--find-session '(fake . "s2")) start)))
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "◐ Session s2"))
      ;; No list of sessions shown either: it keeps what it shows.
      (drover-list-side)
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "◐ Session s2"))
      ;; Back in a session's buffer, that session's.
      (switch-to-buffer drover-handoff-test-buffer)
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "· Session s1"))
      ;; In the side list itself, the session it shows.
      (select-window (drover-items--side-window))
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "· Session s1"))
      (should (equal (drover-session-id (drover-session-at-point)) "s1"))
      ;; Commands and windows changing have it follow, once Emacs is idle.
      (select-window origin)
      (with-current-buffer drover-handoff-test-buffer
        (setq-local drover-session-at-point-function (lambda () (drover-find 'fake "s2"))))
      (should (memq #'drover-items--follow-soon (default-value 'post-command-hook)))
      (drover-items--follow-soon)
      (should (timerp drover-items--follow-timer))
      (drover-items--follow-now)
      (should (equal (car (drover-items-test-lines)) "◐ Session s2"))
      ;; Its list changing draws it again.
      (drover-items-set (drover-find 'fake "s2") (list (list :title "Another" :status 'open)) nil)
      (should (equal (drover-items-test-lines) '("◐ Session s2" "= x 1 Only one" "= o 2 Another")))
      ;; And so does its session changing.
      (setq drover-test-sessions
            (mapcar (lambda (session)
                      (if (equal (drover-session-id session) "s2")
                          (drover-test-session "s2" :engine 'handfake :engine-id "conv-2" :status 'done)
                        session))
                    drover-test-sessions))
      (drover-backend-changed 'fake "s2")
      (drover-changed-now)
      (should (equal (car (drover-items-test-lines)) "✓ Session s2"))
      ;; Killed, it follows nothing more.
      (kill-buffer (drover-items--buffer))
      (should-not (memq #'drover-items--follow-soon (default-value 'post-command-hook)))
      (should-not (memq #'drover-items--on-change drover-changed-hook)))))

(ert-deftest drover-items-test-lines ()
  "One line per item, cut to the window: where it goes, its status, its number, its title, its sessions."
  (drover-items-test-with-view
    (drover-items-test-choose 3 'session)
    (drover-items-test-choose 5 'save)
    (drover-task-note-session '(fake . "s1") "H-WAITING")
    (with-current-buffer (drover-items-test-view)
      ;; The session, the review asked, then a line per item, no header line.
      (should (equal (drover-items-test-lines)
                     '("· Session s1"
                       "§ Waits for your review"
                       "= o 1 Cache key"
                       "  v 2 Old fix"
                       "+ x 3 Vendor ·"
                       "= o 4 Rename"
                       "~ o 5 Benchmark")))
      (should-not header-line-format)
      ;; Point is on the first item, at the start of its row.
      (should (eq (drover-items--id-at-point) 1))
      (should (= (point) (oref (drover-items--section 1) start)))
      (cl-flet ((face-at (number offset)
                  (get-text-property (+ (oref (drover-items--section number) start) offset) 'face)))
        ;; Where an item goes highlights while the session is still to be told; else it dims.
        (should (eq (face-at 3 0) 'opencode-setting-chosen))
        (should (eq (face-at 1 0) 'opencode-setting-inherited))
        ;; The status in its face, the number dimmed, a closed item's title dimmed.
        (should (eq (face-at 3 2) 'drover-items-blocked))
        (should (eq (face-at 2 2) 'drover-items-done))
        (should (eq (face-at 1 4) 'drover-dim))
        (should (eq (face-at 2 6) 'drover-dim))
        (should-not (face-at 1 6)))
      ;; The session serving a heading shows at the end of the line, as lists draw its
      ;; status, and is named.
      (goto-char (oref (drover-items--section 3) start))
      (let ((at (1- (line-end-position))))
        (should (eq (get-text-property at 'face) 'opencode-session-status-idle))
        (should (equal (get-text-property at 'help-echo) "Session s1: Idle"))
        (should (equal (get-text-property (1- at) 'display)
                       `(space :align-to (,(1- (drover-list--line-width)))))))
      ;; A tooltip tells what the icons stand for.
      (should (equal (get-text-property (+ (point) 6) 'help-echo)
                     "3. Vendor\nblocked, goes new session, once applied")))
    ;; Narrower, every item still has one line, its title cut, its sessions kept.
    (let ((drover-list-side-window-width 20))
      (drover-items--hide-side (drover-items--side-window))
      (drover-items-set (drover-find 'fake "s1")
                        (list (list :id 4 :title "Rename every option of the list to its plain name" :status 'open))
                        nil)
      (with-current-buffer (drover-items-test-view)
        (should (= (window-total-width (drover-items--side-window)) 20))
        (let ((lines (drover-items-test-lines))
              (width (drover-list--line-width)))
          (should (equal (length lines) 7))
          (should (equal (nth 4 lines) "+ x 3 Vendor ·"))
          (should (string-match-p "\\`= o 4 Rename .*…\\'" (nth 5 lines)))
          (dolist (line lines)
            (should (<= (string-width line) width))))
        ;; An archived session shows the archive's glyph; more than three, how many more.
        (cl-letf (((symbol-function 'drover-items-heading-sessions)
                   (lambda (item)
                     (and (eql (plist-get item :id) 3)
                          (list (drover-test-session "a" :status 'working)
                                (drover-test-session "b" :status 'waiting)
                                (drover-test-session "c" :status 'idle :archived 1.0)
                                (drover-test-session "d" :status 'idle))))))
          (revert-buffer)
          (should (equal (nth 4 (drover-items-test-lines)) "+ x 3 Vendor ◐!▣+1"))
          (goto-char (oref (drover-items--section 3) start))
          (should (equal (get-text-property (- (line-end-position) 3) 'help-echo) "Session c: archived")))))))

(ert-deftest drover-items-test-details ()
  "TAB shows the details of an item under its line: what its line cuts or leaves out."
  (drover-items-test-with-view
    (drover-task-note-session '(fake . "s1") "H-WAITING")
    (drover-items-test-choose 3 'fork)
    (let ((list (drover-items-list-of (drover-find 'fake "s1"))))
      (setf (plist-get (nth 2 (plist-get list :items)) :place)
            (list :uri "org-id://PLACE-DROVER" :name "work.org/Projects/Drover" :file file :id "PLACE-DROVER")))
    (with-current-buffer (drover-items-test-view)
      (should (eq (keymap-lookup nil "TAB") #'magit-section-toggle))
      ;; Hidden until asked for.
      (should (equal (length (drover-items-test-lines)) 7))
      ;; The first item: its status and where it goes in words, its next step, its link.
      (magit-section-toggle (drover-items--section 1))
      (should (equal (seq-subseq (drover-items-test-lines) 2 13)
                     '("= o 1 Cache key"
                       " Status" " open"
                       " Goes" " here"
                       " Next" " Measure it"
                       " Link" " the bug" " https://example.com/cache"
                       "  v 2 Old fix")))
      ;; Under its title, labels dimmed.
      (goto-char (oref (drover-items--section 1) content))
      (should (equal (get-text-property (1+ (point)) 'display) '(space :align-to (6))))
      (should (eq (get-text-property (+ (point) 2) 'face) 'drover-dim))
      ;; Hidden again.
      (magit-section-toggle (drover-items--section 1))
      (should (equal (length (drover-items-test-lines)) 7))
      ;; Its note, on its lines.
      (magit-section-toggle (drover-items--section 5))
      (should (equal (last (drover-items-test-lines) 3) '(" Note" " Later" " on")))
      ;; Its heading, the sessions serving it, and the place of its heading.
      (magit-section-toggle (drover-items--section 3))
      (should (equal (seq-subseq (member "Y x 3 Vendor ·" (drover-items-test-lines)) 1 11)
                     '(" Status" " blocked"
                       " Goes" " fork, once applied"
                       " Heading" " Hear from the vendor"
                       " Session" " · Session s1"
                       " Place" " work.org/Projects/Drover")))
      ;; Drawn again, what showed shows still, and point stays on its item.
      (goto-char (oref (drover-items--section 3) start))
      (revert-buffer)
      (should (eq (drover-items--id-at-point) 3))
      (should-not (oref (drover-items--section 3) hidden))
      (should-not (oref (drover-items--section 5) hidden))
      (should (oref (drover-items--section 1) hidden))
      ;; A title its line cuts is told whole, wrapped.
      (drover-items-set (drover-find 'fake "s1")
                        (list (list :id 4 :title "Rename every option of the list to its plain name" :status 'open))
                        nil)
      (should-not (member " Title" (drover-items-test-lines)))
      (magit-section-toggle (drover-items--section 4))
      (should (equal (seq-subseq (member " Title" (drover-items-test-lines)) 1 3)
                     '(" Rename every option of the" " list to its plain name"))))))

(ert-deftest drover-items-test-moves-and-choices ()
  "Keys move between items; left and right change where one goes, a subagent among the places."
  (drover-items-test-with-view
    (with-current-buffer (drover-items-test-view)
      (dolist (binding '(("n" . drover-items-next) ("p" . drover-items-previous)
                         ("<right>" . drover-items-goes-next) ("<left>" . drover-items-goes-previous)
                         ("RET" . drover-items-follow) ("C-c C-c" . drover-items-apply)
                         ("g" . revert-buffer) ("q" . drover-items-quit)))
        (should (eq (keymap-lookup nil (car binding)) (cdr binding))))
      (should (eq (drover-items--id-at-point) 1))
      (drover-items-next)
      (should (eq (drover-items--id-at-point) 2))
      (drover-items-next 2)
      (should (eq (drover-items--id-at-point) 4))
      (drover-items-previous)
      (should (eq (drover-items--id-at-point) 3))
      ;; Past the ends, it stays.
      (drover-items-next 5)
      (should (eq (drover-items--id-at-point) 3))
      (drover-items-previous 3)
      (should (eq (drover-items--id-at-point) 3))
      ;; From item 4, here: a subagent, a new session, a fork, later, here again.
      (drover-items-next)
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest arguments) (push (apply #'format format-string arguments) messages))))
          (dolist (goes '(subagent session fork save keep subagent))
            (drover-items-goes-next)
            (should (eq (alist-get 4 (drover-items-test-goes)) goes))
            (should (eq (drover-items--id-at-point) 4)))
          (should (equal (reverse messages)
                         '("Item 4 goes subagent" "Item 4 goes new session" "Item 4 goes fork" "Item 4 goes later"
                           "Item 4 goes here" "Item 4 goes subagent")))
          (dolist (goes '(keep save fork session))
            (drover-items-goes-previous)
            (should (eq (alist-get 4 (drover-items-test-goes)) goes)))))
      ;; Each place has its icon, the choice kept as it is made.
      (should (string-prefix-p "+ o 4 Rename" (nth 5 (drover-items-test-lines))))
      (drover-items-goes-previous)
      (should (string-prefix-p "@ o 4 Rename" (nth 5 (drover-items-test-lines))))
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
      (drover-items-test-choose 3 'fork)
      (with-current-buffer (find-buffer-visiting file)
        (goto-char (point-min))
        (search-forward "WAITING Hear from the vendor")
        (org-todo "CANCELED"))
      (revert-buffer)
      (should-not (alist-get 3 (drover-items-test-goes)))
      (should (equal (nth 4 (drover-items-test-lines)) "  d 3 Vendor"))
      ;; Hidden and shown again, and after a restart, the choices are there.
      (drover-items-quit)
      (should-not (drover-items--side-window))
      (should (eq (alist-get 4 (drover-items-test-goes)) 'subagent))
      (setq drover-items--table nil)
      (should (eq (alist-get 4 (drover-items-test-goes)) 'subagent)))))

(ert-deftest drover-items-test-follow-heading ()
  "RET shows the heading of an item, else opens its link, where the user works."
  (drover-items-test-with-view
    (let ((origin (selected-window))
          (opened nil))
      (with-current-buffer (drover-items-test-view)
        (goto-char (oref (drover-items--section 3) start))
        (drover-items-follow))
      (should (eq (selected-window) origin))
      (should (eq (window-buffer origin) (find-buffer-visiting file)))
      (with-current-buffer (window-buffer origin)
        (should (= (point) (window-point origin)))
        (should (looking-at-p "\\*\\* WAITING Hear from the vendor")))
      ;; The side list stays, on its session.
      (should (window-live-p (drover-items--side-window)))
      (drover-items--follow)
      (should (equal (car (drover-items-test-lines)) "· Session s1"))
      ;; Without a heading, its link.
      (cl-letf (((symbol-function 'org-link-open-from-string) (lambda (link &rest _) (push link opened))))
        (with-current-buffer (drover-items-test-view)
          (goto-char (oref (drover-items--section 1) start))
          (drover-items-follow))
        (should (equal opened '("[[https://example.com/cache][the bug]]")))
        (should (eq (selected-window) origin))
        (with-current-buffer (drover-items-test-view)
          (goto-char (oref (drover-items--section 4) start))
          (should (string-match-p "Item 4 has no heading and no link"
                                  (cadr (should-error (drover-items-follow) :type 'user-error)))))))))

(ert-deftest drover-items-test-prompt ()
  "The default prompt stands alone: it names no skill, and tells every step for each place."
  (drover-items-test-with-view
    (let* ((session (drover-find 'fake "s1"))
           (text (drover-items-prompt-text nil session)))
      (should (string-prefix-p "I chose where these items of the list of this session go:\n\n" text))
      (should-not (string-match-p "skill" text))
      (should-not (string-match-p "%[ik]" text))
      (dolist (told '(;; A subagent, which the agent starts and manages itself.
                      "An item that goes to a subagent:\n\n1. Run the item in a subagent that you start and manage yourself.  Write no Org heading for it.\n2. Brief the subagent so that the brief reads alone.\n3. Answer the questions of the subagent yourself.\n4. Report the result of the subagent in this session.\n5. Update the status of the item with `drover-session-items-set`, `review` false.\n"
                      "An item that goes to a new session or to a fork, or that is saved for later:\n"
                      "1. Write its Org heading with `org-add-todo`, with the place given as its `parent_uri`.  The tool gives the heading its ID.  An item whose heading is written already keeps that heading.\n"
                      "2. Write the body of the heading as a brief that reads alone, for a reader who saw nothing of this conversation.  The brief holds the goal, what is known with paths and links, the decisions already made, the limits, when the item is done, and a report paragraph.\n"
                      ;; The session an item goes to reports to this one, by its key.
                      "3. Write the report paragraph for the session that takes the item: \"When the item is done or blocked, send one message to the session `fake/s1` with `drover-session-send`.  Then set the state of this heading.\"\n"
                      "4. For a new session, call `drover-task-start` with the ID of the heading, `repo` or `directory`, and `agent`.\n"
                      "5. For a fork, call `drover-session-fork` with the ID of the heading as `task`, `agent`, and a `prompt` that names the item and links the heading."
                      ;; The agent chooses whether the session plans or builds.
                      "6. Choose `agent` yourself: `build` when the research is done and the brief says what to build, `plan` when design decisions remain.\n"
                      "7. For an item saved for later, start nothing.\n"
                      "8. Record the heading on its item: call `drover-session-items-set` with the `id`, `title` and `status` of the item, its `heading`, and `review` false.\n"
                      "Done when each item has its heading and its session, or its subagent ran, and your reply holds the link of each heading, `[[id:ID][title]]`, and of each new session, the `link` that the tool returned."))
        (should (string-search told text)))
      (let ((drover-items-prompt "%i -> %k (100%%)"))
        (should (equal (drover-items-prompt-text nil session) " -> fake/s1 (100%)"))))))

(ert-deftest drover-items-test-apply ()
  "Applying asks the place of each heading once, the last offered, and tells the session where each item goes."
  (drover-items-test-with-view
    (drover-items-test-choose 1 'session)
    (drover-items-test-choose 3 'fork)
    (drover-items-test-choose 4 'subagent)
    (drover-items-test-choose 5 'save)
    (let* ((view (drover-items-test-view))
           (origin (drover-list--main-window))
           (windows (window-list)))
      (with-current-buffer view
        (drover-items-apply))
      ;; A place for the items leaving without a heading, the one chosen last offered:
      ;; none for the one whose heading is written, nor for the one a subagent takes.
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
          (should (string-prefix-p "I chose where these items of the list of this session go:\n\n" text))
          (should (string-search (format "- Item 1, \"Cache key\" (open): to a new session.  Its heading goes under `org-id://%s` (the heading \"work.org/Projects/Notes\").\n  Link: [[https://example.com/cache][the bug]]\n  Next step: Measure it\n" id)
                                 text))
          (should (string-search "- Item 3, \"Vendor\" (blocked): to a fork of this session.  Its heading is [[id:H-WAITING][Hear from the vendor]], written already.\n" text))
          ;; A subagent takes it: no place, no heading.
          (should (string-search "- Item 4, \"Rename\" (open): to a subagent of this session.\n" text))
          (should (string-search (format "- Item 5, \"Benchmark\" (open): saved for later.  Its heading goes under `org-id://%s`" id) text))
          (should (string-search "  Note: Later\n  on" text))
          ;; Done, it goes nowhere: the prompt leaves it out.
          (should-not (string-search "Old fix" text))
          (should (string-search "send one message to the session `fake/s1` with `drover-session-send`" text))))
      ;; The prompt went in no window of its own; the side list stays.
      (should (equal (window-list) windows))
      (should (window-live-p (drover-items--side-window)))
      (should (eq (window-buffer origin) drover-handoff-test-buffer))
      ;; The items told of are sent; the review is over, and the side list says so no more.
      (let ((list (drover-items-list-of (drover-find 'fake "s1"))))
        (should-not (drover-items-asked list))
        (should (equal (mapcar (lambda (item) (car (plist-get item :sent))) (plist-get list :items))
                       '(session nil fork subagent save))))
      (should-not (member "§ Waits for your review" (drover-items-test-lines)))
      (with-current-buffer view
        (should (eq (get-text-property (oref (drover-items--section 4) start) 'face) 'opencode-setting-inherited)))
      (should (seq-find (lambda (message) (string-match-p "\\`Session s1 is told where 4 items go\\'" message))
                        drover-handoff-test-messages))
      ;; Applied again, nothing is told anew: nothing is sent.
      (setq asked nil)
      (with-current-buffer view
        (drover-items-apply))
      (should-not asked)
      (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 0.5))
      (should (equal (car drover-handoff-test-messages) "No item of Session s1 is to be told of: nothing sent")))))

(ert-deftest drover-items-test-apply-draft-and-turn ()
  "The prompt never goes over a draft, and waits for a running turn to end."
  (drover-items-test-with-view
    (drover-items-test-choose 1 'subagent)
    (setq drover-handoff-test-blocker "the input holds a draft")
    (with-current-buffer (drover-items-test-view)
      (drover-items-apply))
    (should (drover-handoff-test-wait
             (lambda () (eq (plist-get (car (drover-items-test-jobs)) :status) 'waiting))))
    (should-not (drover-handoff-test-wait (lambda () drover-handoff-test-submitted) 1.2))
    (setq drover-handoff-test-blocker nil)
    (should (string-search "Cache key" (drover-handoff-test-take)))
    (should (drover-handoff-test-wait (lambda () (null (drover-items-test-jobs)))))
    ;; A running turn: once it ended.
    (drover-handoff-test-session :status 'working)
    (drover-items-test-choose 4 'session)
    (with-current-buffer (drover-items-test-view)
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
  "The menu names the side list, with the open items of the session at point, and the review asked."
  (drover-items-test-with-view
    (let ((session (drover-find 'fake "s1")))
      (should (drover-items-shown-p session))
      (should-not (drover-items-shown-p (drover-test-session "none")))
      (cl-letf (((symbol-function 'drover-dispatch-session) (lambda (&rest _) session)))
        (should (equal (substring-no-properties (drover--dispatch-items-description)) "Items 4 review")))
      (cl-letf (((symbol-function 'drover-dispatch-session) (lambda (&rest _) (drover-test-session "none"))))
        (should (equal (drover--dispatch-items-description) "Items"))))
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
                             ;; Their descriptions say what they do, and send no item anywhere.
                             (dolist (name '("drover-session-items-set" "drover-session-items"))
                               (let ((description (alist-get 'description
                                                             (seq-find (lambda (tool) (equal (alist-get 'name tool) name))
                                                                       tools))))
                                 (should (string-match-p "the caller's by default" description))
                                 (dolist (word '("propos" "fork" "subagent" "new session" "hand" "prompt" "rather than"
                                                 "should" "must"))
                                   (should-not (string-match-p word description)))))
                             (let ((items-set (seq-find (lambda (tool) (equal (alist-get 'name tool) "drover-session-items-set")) tools)))
                               (should-not (assq 'proposal (alist-get 'properties
                                                                      (alist-get 'items (alist-get 'properties (alist-get 'inputSchema items-set))))))
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
