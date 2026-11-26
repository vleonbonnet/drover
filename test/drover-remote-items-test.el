;;; drover-remote-items-test.el --- Tests of the items of other hosts' sessions  -*- lexical-binding: t; -*-

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

;; The list of items of a session of another host: a second batch
;; Emacs over 127.0.0.1 (test/drover-remote-agent.el) owns the session
;; and keeps its list, in its own state directory, the headings of its
;; items in an Org file of its own.  This host reads the list, shows it
;; in the side list, says where an item goes, applies the choices with
;; a place read here, and is told as the list changes there.  The Org
;; file of this host is the fixture of drover-items-test.el: both hold
;; the heading of ID PLACE-DROVER, as two hosts reading one Org file do.

;;; Code:

(require 'drover-remote-test)
(require 'drover-items-test)
(require 'drover-remote-items)
(require 'drover-anvil)

(defconst drover-remote-items-test-org
  "* Places
** Drover
:PROPERTIES:
:ID: PLACE-DROVER
:END:
* Items
** WAITING Hear from the vendor
:PROPERTIES:
:ID: H-WAITING
:END:
** DONE Ship the fix
:PROPERTIES:
:ID: H-DONE
:END:
"
  "The Org file of the agent: a place, and the headings of items.")

(defconst drover-remote-items-test-key '(remote:agent fake . "s0")
  "The key here of the agent's session s0, whose list the tests read.")

(defvar drover-remote-items-test-messages nil
  "What drover told the user, messages and warnings, newest first.")

(defmacro drover-remote-items-test-with (environment &rest body)
  "Run BODY as the host `viewer', the agent owning the sessions s0 and s1, idle.
ENVIRONMENT lists what the agent starts with more, as NAME=VALUE: it
grants full access unless told otherwise.  The agent holds the Org file
of `drover-remote-items-test-org'.  What this host kept of other hosts'
lists starts empty, its own state is fresh, and the tools have no
caller.  What drover tells the user is kept in
`drover-remote-items-test-messages'."
  (declare (indent 1) (debug t))
  `(drover-remote-test-with nil
     (drover-test-with-state
       (let ((drover-items--table nil)
             (drover-remote-items--lists (make-hash-table :test #'equal))
             (drover-remote-items--reviews (make-hash-table :test #'equal))
             (drover-remote-items-test-messages nil)
             (drover-anvil-callers nil)
             (drover-anvil-log-file nil)
             (drover-anvil--jobs (make-hash-table :test #'equal)))
         (let ((process-environment (append ,environment
                                            '("DROVER_AGENT_ACCESS=full" "DROVER_AGENT_LINKABLE=1")
                                            process-environment)))
           (drover-remote-test-connect (drover-remote-test-start-agent dir)))
         (drover-remote-test-wait (lambda () (drover-remote--peer-granted (drover-remote--peer "agent"))))
         (drover-remote-test-command "sessions" :n 2)
         (drover-remote-test-command "change" :ids ["s0" "s1"] :status "idle" :now t)
         (drover-remote-test-command "org" :name "there.org" :text drover-remote-items-test-org)
         (drover-remote-test-synced 2 (lambda (session) (eq (drover-session-status session) 'idle)))
         (cl-letf (((symbol-function 'drover-message)
                    (lambda (format-string &rest arguments)
                      (push (apply #'format format-string arguments) drover-remote-items-test-messages)))
                   ((symbol-function 'drover-warn)
                    (lambda (format-string &rest arguments)
                      (push (apply #'format format-string arguments) drover-remote-items-test-messages))))
           ,@body)))))

(defmacro drover-remote-items-test-with-view (&rest body)
  "Run BODY in a frame wide enough for the side list, the buffer of s0 selected.
That buffer tells the agent's session s0, as a mirror of its terminal
does.  The icons are plain characters, as in drover-items-test.el."
  (declare (indent 0) (debug t))
  `(let ((size (cons (frame-width) (frame-height)))
         (shown (generate-new-buffer " *session of the agent*"))
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
     (with-current-buffer shown
       (setq-local drover-session-at-point-function
                   (lambda () (drover-index-session drover-remote-items-test-key))))
     (unwind-protect
         (progn
           (set-frame-size (selected-frame) 160 50)
           (switch-to-buffer shown)
           ,@body)
       (dolist (buffer (buffer-list))
         (when (or (eq (buffer-local-value 'major-mode buffer) 'drover-items-mode)
                   (with-current-buffer buffer (derived-mode-p 'drover-list-mode)))
           (kill-buffer buffer)))
       (let ((ignore-window-parameters t))
         (delete-other-windows (seq-find (lambda (window) (not (window-parameter window 'window-side)))
                                         (window-list nil 'nomini))))
       (switch-to-buffer "*scratch*")
       (kill-buffer shown)
       (drover-list--stop-timer 'drover-list--place-timer)
       (set-frame-parameter nil 'drover-list-sidebar-aside nil)
       (set-frame-parameter nil 'drover-list-sidebar-kept nil)
       (set-frame-size (selected-frame) (car size) (cdr size)))))

(defun drover-remote-items-test-session ()
  "Return this host's record of the agent's session s0."
  (drover-remote-test-session "s0"))

(defun drover-remote-items-test-give (&optional review)
  "Have the agent's session s0 hold a list of three items; REVIEW asks for one.
1, open; 2, done; 3, whose heading waits, and which s1 serves."
  (drover-remote-test-command "task-session" :id "s1" :heading "H-WAITING")
  (drover-remote-test-command "items-set" :id "s0" :review (if review t :false)
                              :items [["Cache key" "open"] ["Old fix" "done"] ["Vendor" "open" "H-WAITING"]]))

(defun drover-remote-items-test-list ()
  "Return the list this host holds of the agent's session s0, or nil."
  (drover-items-list-of (drover-remote-items-test-session)))

(defun drover-remote-items-test-read ()
  "Read the list of the agent's session s0 from the agent, and return it."
  (let ((answer nil))
    (drover-remote-items-read (drover-remote-items-test-session) (lambda (result) (setq answer result)))
    (should (drover-remote-test-wait (lambda () answer)))
    (should-not (plist-get answer :failure))
    (plist-get answer :list)))

(defun drover-remote-items-test-there (&optional id)
  "Return the items of the list of session ID, s0 by default, as the agent keeps it.
Each as (NUMBER TITLE STATUS GOES PLACE SENT), false as nil."
  (mapcar (lambda (item) (mapcar (lambda (field) (if (eq field :false) nil field)) item))
          (plist-get (drover-remote-test-command "items" :id (or id "s0")) :items)))

(defun drover-remote-items-test-asked-there (&optional id)
  "Return since when the list of session ID, s0 by default, waits for a review there."
  (let ((asked (plist-get (drover-remote-test-command "items" :id (or id "s0")) :asked)))
    (and (numberp asked) asked)))

(defun drover-remote-items-test-told (regexp)
  "Return what drover told the user that REGEXP matches, or nil."
  (seq-find (lambda (text) (string-match-p regexp text)) drover-remote-items-test-messages))

(defun drover-remote-items-test-settled ()
  "Wait until no change asked of the agent for the list of s0 is awaited."
  (should (drover-remote-test-wait
           (lambda ()
             (let ((entry (drover-remote-items--entry drover-remote-items-test-key)))
               (and (zerop (or (plist-get entry :pending) 0)) (null (plist-get entry :asked))))))))

(defun drover-remote-items-test-tool (function &rest arguments)
  "Return the answer of the tool FUNCTION called with ARGUMENTS, once it has one.
A tool going through another host answers with a job, which is taken
once it settled; a job that failed signals its message."
  (let ((answer (drover-anvil-test-json (apply function arguments))))
    (when (alist-get 'pending answer)
      (let ((job (alist-get 'job answer)))
        (should (drover-remote-test-wait
                 (lambda () (not (eq (drover-anvil--job-get job :state) 'pending)))))
        (setq answer (drover-anvil-test-json (drover-anvil--tool-job job)))))
    answer))

(defun drover-remote-items-test-ask (request)
  "Ask the agent REQUEST, as a host or a phone does; return (REPLY ERROR)."
  (drover-remote--wait (drover-remote--peer "agent") request 20))

;;;; Reading and showing

(ert-deftest drover-remote-items-test-read-and-drawn ()
  "The list of another host's session is read from it and drawn as this host's own."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give t)
    (drover-remote-items-test-with-view
      (let ((session (drover-remote-items-test-session)))
        ;; Not read yet: nothing of it here.
        (should-not (drover-items-list-of session))
        (should-not (drover-items-p session))
        ;; Shown: asked of its host, without waiting.
        (drover-items)
        (should (equal (drover-items-test-lines) '("· Session s0" "Reading its list on agent…")))
        (should (drover-remote-test-wait #'drover-remote-items-test-list))
        (let* ((list (drover-remote-items-test-list))
               (items (plist-get list :items)))
          (should (equal (plist-get list :host) "agent"))
          (should (equal (plist-get list :key) drover-remote-items-test-key))
          (should (plist-get list :asked))
          (should (equal (mapcar (lambda (item) (plist-get item :id)) items) '(1 2 3)))
          (should (equal (mapcar (lambda (item) (plist-get item :title)) items) '("Cache key" "Old fix" "Vendor")))
          ;; The status its host computed: its heading waits there.
          (should (equal (mapcar #'drover-items-item-status items) '(open done blocked)))
          (should (equal (mapcar #'drover-items-goes items) '(keep nil keep)))
          (should (equal (drover-items-heading-told (nth 2 items)) '("Hear from the vendor" . t)))
          ;; The session serving its heading there is the one this host lists.
          (should (eq (car (drover-items-heading-sessions (nth 2 items))) (drover-remote-test-session "s1")))
          ;; This host reads none of its headings, and writes it in no file.
          (should-not (drover-items-heading-info (nth 2 items)))
          (should-not (file-exists-p (drover-items-file))))
        (should (drover-items-p session))
        ;; Drawn once it answered: the same lines as a list of this host.
        (should (equal (drover-items-test-lines)
                       '("· Session s0"
                         "§ Waits for your review"
                         "= o 1 Cache key"
                         "  v 2 Old fix"
                         "= x 3 Vendor ·")))
        (with-current-buffer (drover-items-test-view)
          (should (eq (drover-items--id-at-point) 1))
          (goto-char (oref (drover-items--section 3) start))
          (should (equal (get-text-property (1- (line-end-position)) 'help-echo) "Session s1: Idle"))
          ;; Its details, as told.
          (magit-section-toggle (drover-items--section 3))
          (should (equal (seq-subseq (member "= x 3 Vendor ·" (drover-items-test-lines)) 1 9)
                         '(" Status" " blocked"
                           " Goes" " here"
                           " Heading" " Hear from the vendor"
                           " Session" " · Session s1")))
          ;; g reads it from its host again.
          (drover-remote-test-command "items-set" :id "s0" :review :false :items [["Later" "open"]])
          (revert-buffer)
          (should (drover-remote-test-wait
                   (lambda () (member "= o 4 Later" (drover-items-test-lines))))))
        ;; A session of that host without a list says so.
        (with-current-buffer shown
          (setq-local drover-session-at-point-function (lambda () (drover-remote-test-session "s1"))))
        (select-window (get-buffer-window shown))
        (drover-items--follow)
        (should (drover-remote-test-wait
                 (lambda () (equal (drover-items-test-lines) '("· Session s1" "No items")))))))))

(ert-deftest drover-remote-items-test-decode ()
  "What a host tells of a list is checked and bounded as it comes."
  (require 'drover-items)
  (let* ((long (make-string 5000 ?x))
         (list (drover-remote-items--decode
                "core"
                (list :k ["opencode" "ses_1"] :ti long :a 1000.5 :u 2000 :more t
                      :items (vector (list :n 1 :ti long :st "open" :nx long :nt (concat long "\n" long)
                                           :ln (vector long long) :h "H-1" :ht long :hf t :g "fork"
                                           :pl (list :uri "org-id://P" :name long :id "bad id")
                                           :sn ["fork" 1500] :c 1100 :u 1200
                                           :ss (vector (list :k ["opencode" "ses_2"] :h :false :ti long
                                                             :st "working" :ar t)
                                                       (list :k "nope" :h "no such host" :ti 4 :st "odd")
                                                       "not a session"))
                                     ;; Not items: no number, a status of none, a number twice.
                                     (list :ti "No number" :st "open")
                                     (list :n 2 :ti "Odd" :st "started")
                                     (list :n 1 :ti "Twice" :st "done")
                                     "not an item"
                                     (list :n 3 :ti "Plain" :st "done" :g "nowhere" :h 12 :sn "x" :pl "y")))
                100))
         (items (plist-get list :items))
         (first (car items)))
    (should (equal (plist-get list :key) '(remote:core opencode . "ses_1")))
    (should (equal (plist-get list :host) "core"))
    ;; Times on this host's clock, its host's being 100 s ahead.
    (should (equal (plist-get list :asked) 900.5))
    (should (equal (plist-get list :updated) 1900))
    (should (equal (plist-get list :stamp) 2000))
    (should (plist-get list :more))
    (should (<= (length (plist-get list :title)) 301))
    (should (equal (mapcar (lambda (item) (plist-get item :id)) items) '(1 3)))
    (should (<= (length (plist-get first :title)) (1+ drover-items-title-limit)))
    (should (<= (length (plist-get first :next)) (1+ drover-items-next-limit)))
    (should (<= (length (plist-get first :note)) (1+ drover-items-note-limit)))
    (should (<= (length (car (plist-get first :link))) (1+ drover-items-link-limit)))
    (should (<= (length (plist-get first :heading-title)) 301))
    (should (eq (plist-get first :goes) 'fork))
    (should (equal (plist-get first :sent) '(fork . 1400)))
    (should (equal (plist-get (plist-get first :place) :uri) "org-id://P"))
    (should-not (plist-get (plist-get first :place) :id))
    (should (plist-get (drover-items--there first) :found))
    ;; The sessions serving its heading: one of that host, by its key here; one no host names.
    (let ((sessions (plist-get (drover-items--there first) :sessions)))
      (should (length= sessions 2))
      (should (equal (plist-get (car sessions) :key) '(remote:core opencode . "ses_2")))
      (should (eq (plist-get (car sessions) :status) 'working))
      (should (plist-get (car sessions) :archived))
      (should-not (plist-get (cadr sessions) :key))
      (should (eq (plist-get (cadr sessions) :status) 'idle))
      ;; Not listed here, they show as told.
      (let ((told (drover-items-heading-sessions first)))
        (should (equal (drover-session-key (car told)) '(remote:core opencode . "ses_2")))
        (should (drover-session-archived (car told)))
        (should (equal (drover-session-display-title (cadr told)) "A session"))))
    (let ((plain (cadr items)))
      (should-not (plist-get plain :goes))
      (should-not (plist-get plain :heading))
      (should-not (plist-get plain :sent))
      (should-not (plist-get plain :place)))
    ;; No list at all.
    (should-not (drover-remote-items--decode "core" "a list" 0))
    (should-not (drover-remote-items--decode "core" (list :k "nope" :items []) 0))
    ;; What a peer gives as items and places.
    (should (eq (drover-remote-items--given "items") 'invalid))
    (should (eq (drover-remote-items--given (make-vector (1+ drover-items-limit) (list :i 1))) 'invalid))
    (should (equal (drover-remote-items--given
                    (vector (list :i 1 :title (make-string 30000 ?t) :status t :id 3 :note "" :unknown ["priority" 7])
                            "not an object"
                            (list :i 3 :malformed t)))
                   (list (list :index 1 :title (make-string drover-remote-items-field-limit ?t) :status t
                               :note "" :id 3 :unknown '("priority"))
                         (list :index 2 :malformed t)
                         (list :index 3 :malformed t))))
    (should (eq (drover-remote-items--places-given "places") 'invalid))
    (should (eq (drover-remote-items--places-given (vector (list :n 1 :id "bad id"))) 'invalid))
    (should (eq (drover-remote-items--places-given (vector (list :n "1" :id "P"))) 'invalid))
    (should (equal (drover-remote-items--places-given (vector (list :n 1 :id "P-1" :name "Notes") (list :n 2 :id :false)))
                   '((1 :id "P-1" :name "Notes") (2 :id nil :name nil))))
    ;; What goes to the list's host of the items an agent gives.
    (should (equal (drover-remote-items--wire-given
                    (list (list :index 1 :title "A" :status "open" :next nil :note :json-false :id 4
                                :link '(not a text) :unknown '("priority"))
                          (list :index 2 :malformed t)))
                   (vector (list :i 1 :title "A" :status "open" :link t :next "" :note "" :id 4 :unknown ["priority"])
                           (list :i 2 :malformed t))))))

(ert-deftest drover-remote-items-test-nothing-loaded ()
  "A host keeping no list answers that it has none, loading nothing to keep one."
  (drover-remote-items-test-with nil
    (cl-flet ((loaded () (eq (plist-get (drover-remote-test-command "feature" :name "drover-items") :loaded) t)))
      ;; Followed as it granted access: no review waits there.
      (should (drover-remote-test-wait (lambda () (gethash "agent" drover-remote-items--reviews))))
      (should (zerop (hash-table-count (gethash "agent" drover-remote-items--reviews))))
      (should-not (loaded))
      ;; A session without a list.
      (should-not (drover-remote-items-test-read))
      (should-not (drover-remote-items-state (drover-remote-items-test-session)))
      (should-not (loaded))
      ;; Its first list given, it keeps and tells it.
      (drover-remote-items-test-give t)
      (should (loaded))
      (should (drover-remote-test-wait (lambda () (drover-items-review-since (drover-remote-items-test-session)))))
      (should (length= (plist-get (drover-remote-items-test-read) :items) 3)))))

;;;; Where an item goes

(ert-deftest drover-remote-items-test-goes ()
  "Where an item goes, said here, reaches its host, which keeps it: both show it."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give)
    (drover-remote-items-test-with-view
      (drover-items)
      (should (drover-remote-test-wait #'drover-remote-items-test-list))
      (with-current-buffer (drover-items-test-view)
        (should (eq (drover-items--id-at-point) 1))
        ;; Three keys at once: each steps from where the one before left it.
        (drover-items-goes-next)
        (should (string-prefix-p "@ o 1 Cache key" (nth 1 (drover-items-test-lines))))
        (drover-items-goes-next)
        (drover-items-goes-next)
        ;; Shown here at once, its host not waited for.
        (should (string-prefix-p "Y o 1 Cache key" (nth 1 (drover-items-test-lines))))
        (should (eq (get-text-property (point) 'face) 'opencode-setting-chosen))
        (should (> (plist-get (drover-remote-items--entry drover-remote-items-test-key) :pending) 0))
        ;; Kept there.
        (should (drover-remote-test-wait
                 (lambda () (equal (nth 3 (car (drover-remote-items-test-there))) "fork"))))
        ;; And back: what shows is what its host keeps.
        (drover-remote-items-test-settled)
        (let ((item (car (plist-get (drover-remote-items-test-list) :items))))
          (should (eq (plist-get item :goes) 'fork))
          (should (eq (drover-items-chosen-by item) 'user)))
        (should (string-prefix-p "Y o 1 Cache key" (nth 1 (drover-items-test-lines))))
        (should-not (file-exists-p (drover-items-file)))
        ;; The place before.
        (drover-items-goes-previous)
        (should (drover-remote-test-wait
                 (lambda () (equal (nth 3 (car (drover-remote-items-test-there))) "session"))))
        (drover-remote-items-test-settled)
        (should (string-prefix-p "+ o 1 Cache key" (nth 1 (drover-items-test-lines))))
        ;; One done goes nowhere: its host is asked nothing.
        (goto-char (oref (drover-items--section 2) start))
        (let ((messages nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest arguments) (push (apply #'format format-string arguments) messages))))
            (drover-items-goes-next))
          (should (equal messages '("Item 2 is done: it goes nowhere"))))
        (should (zerop (plist-get (drover-remote-items--entry drover-remote-items-test-key) :pending))))
      ;; Its host refuses what it cannot keep, whatever this host checked.
      (pcase-let ((`(,_reply ,error)
                   (drover-remote-items-test-ask (list :op "items-goes" :key ["fake" "s0"] :n 2 :goes "fork"))))
        (should (equal error "item 2 is done: it goes nowhere")))
      (should (equal (cadr (drover-remote-items-test-ask (list :op "items-goes" :key ["fake" "s0"] :n 1 :goes "moon")))
                     "no such place for an item"))
      (should (equal (cadr (drover-remote-items-test-ask (list :op "items-goes" :key ["fake" "s0"] :n 9 :goes "fork")))
                     "its list holds no such item"))
      (should (equal (cadr (drover-remote-items-test-ask (list :op "items-goes" :key ["fake" "nope"] :n 1 :goes "fork")))
                     "no such session here")))))

(ert-deftest drover-remote-items-test-read-access ()
  "A host of read access reads the lists of another, and changes none."
  (drover-remote-items-test-with '("DROVER_AGENT_ACCESS=read")
    (drover-remote-items-test-give t)
    (let ((session (drover-remote-items-test-session)))
      (should (length= (plist-get (drover-remote-items-test-read) :items) 3))
      (should (drover-items-review-since session))
      ;; Refused here, before anything is asked.
      (should (string-match-p "agent grants this host read access"
                              (cadr (should-error (drover-remote-items-set-goes session 1 'fork) :type 'user-error))))
      ;; And there, whatever this host checked.
      (dolist (request (list (list :op "items-goes" :key ["fake" "s0"] :n 1 :goes "fork")
                             (list :op "items-apply" :id "viewer-x-1" :key ["fake" "s0"] :places [])
                             (list :op "items-set" :id "viewer-x-2" :key ["fake" "s0"] :items [])
                             (list :op "items-places" :key ["fake" "s0"])))
        (should (equal (cadr (drover-remote-items-test-ask request)) "viewer has read access")))
      (should-not (nth 3 (car (drover-remote-items-test-there)))))))

;;;; Applying

(defmacro drover-remote-items-test-with-places (heading &rest body)
  "Run BODY with the Org fixture of this host, places read answering HEADING.
HEADING is the text of a heading of `drover-items-test-org'; what
`org-refile-get-location' was asked is kept in `asked'.  The agent's
session s0 has a terminal, which takes its prompts."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (drover-remote-test--ptys-p))
     (drover-remote-test-command "terminal" :id "s0")
     (drover-items-test-with-org
       (let ((asked nil)
             (drover-items--table nil))
         (setq org-agenda-files (list file))
         (cl-letf (((symbol-function 'org-refile-get-location)
                    (lambda (prompt &optional _default-buffer _new-nodes)
                      (push prompt asked)
                      (let ((marker (with-current-buffer (find-buffer-visiting file)
                                      (goto-char (point-min))
                                      (re-search-forward (concat "^\\*\\* " (regexp-quote ,heading)))
                                      (copy-marker (line-beginning-position)))))
                        (list (concat "work.org/Projects/" ,heading "/") file nil marker)))))
           ,@body)))))

(ert-deftest drover-remote-items-test-apply ()
  "Applying from here asks the place here, and its host tells its session: the place goes by its Org ID."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give t)
    (drover-remote-items-test-with-places "Drover"
      (drover-remote-items-test-with-view
        (let ((session (drover-remote-items-test-session)))
          (drover-items)
          (should (drover-remote-test-wait #'drover-remote-items-test-list))
          (should (drover-items-review-since session))
          (drover-remote-items-set-goes session 1 'session)
          (drover-remote-items-set-goes session 3 'subagent)
          (drover-remote-items-test-settled)
          (with-current-buffer (drover-items-test-view)
            (drover-items-apply))
          ;; The place of the one heading to write is asked here, a heading alone.
          (should (equal asked '("Item 1, Cache key, its heading goes under")))
          (should (drover-remote-items-test-told "\\`Asking agent to tell Session s0 where its items go…\\'"))
          ;; Its host is not waited for: told once it answered.
          (should (drover-remote-test-wait
                   (lambda () (drover-remote-items-test-told "\\`Session s0 is told where 2 items go\\'"))))
          ;; There, its session got the prompt, the place by its Org ID, which that host found.
          (should (drover-remote-test-wait (lambda () (drover-remote-test-calls "send"))))
          (let ((text (nth 2 (car (drover-remote-test-calls "send")))))
            (should (string-prefix-p "I chose where these items of the list of this session go:\n\n" text))
            (should (string-search "- Item 1, \"Cache key\" (open): to a new session.  Its heading goes under `org-id://PLACE-DROVER` (the heading \"work.org/Projects/Drover\").\n"
                                   text))
            (should (string-search "- Item 3, \"Vendor\" (blocked): to a subagent of this session.\n" text))
            (should-not (string-search "Old fix" text))
            ;; The sessions its items go to report to it, as that host names it.
            (should (string-search "send one message to the session `fake/s0` with `drover-session-send`" text)))
          ;; Kept there: the place, what was sent, the review over.
          (let ((there (drover-remote-items-test-there)))
            (should (equal (nth 4 (nth 0 there)) "org-id://PLACE-DROVER"))
            (should (equal (mapcar (lambda (item) (nth 5 item)) there) '("session" nil "subagent"))))
          (should-not (drover-remote-items-test-asked-there))
          ;; And here: the list as its host keeps it, no review, the side list drawn.
          (drover-remote-items-test-settled)
          (let ((list (drover-remote-items-test-list)))
            (should-not (plist-get list :asked))
            (should (equal (mapcar (lambda (item) (car (plist-get item :sent))) (plist-get list :items))
                           '(session nil subagent)))
            (should (equal (plist-get (plist-get (car (plist-get list :items)) :place) :name)
                           "work.org/Projects/Drover")))
          (should-not (drover-items-review-since session))
          (should-not (member "§ Waits for your review" (drover-items-test-lines)))
          (with-current-buffer (drover-items--buffer)
            (should (eq (get-text-property (oref (drover-items--section 1) start) 'face) 'opencode-setting-inherited)))
          ;; This host wrote nothing of it: no list, no heading.
          (should-not (file-exists-p (drover-items-file)))
          ;; Applied again, nothing is asked nor sent.
          (setq asked nil
                drover-remote-items-test-messages nil)
          (with-current-buffer (drover-items--buffer)
            (drover-items-apply))
          (should-not asked)
          (should (drover-remote-test-wait
                   (lambda () (drover-remote-items-test-told "\\`No item of Session s0 is to be told of: nothing sent\\'"))))
          (should (length= (drover-remote-test-calls "send") 1)))))))

(ert-deftest drover-remote-items-test-apply-refused-place ()
  "A place its host cannot use is refused there, its reason told here; nothing is sent."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give t)
    ;; A heading of this host's file alone, given an ID here: the agent holds none such.
    (drover-remote-items-test-with-places "Notes"
      (drover-remote-items-test-with-view
        (let ((session (drover-remote-items-test-session)))
          (drover-items)
          (should (drover-remote-test-wait #'drover-remote-items-test-list))
          (drover-remote-items-set-goes session 1 'save)
          (drover-remote-items-test-settled)
          (with-current-buffer (drover-items-test-view)
            (drover-items-apply))
          (should (equal asked '("Item 1, Cache key, its heading goes under")))
          (let ((id (with-current-buffer (find-buffer-visiting file)
                      (goto-char (point-min))
                      (re-search-forward "^\\*\\* Notes")
                      (org-entry-get nil "ID"))))
            (should id)
            (should (drover-remote-test-wait
                     (lambda () (drover-remote-items-test-told "is not told where its items go"))))
            (should (equal (drover-remote-items-test-told "is not told")
                           (format "Session s0 is not told where its items go: item 1: agent finds no heading of ID %s in its Org files"
                                   id))))
          ;; Nothing sent there, nothing kept of it: the item is still to be told of, the review asked.
          (drover-remote-test-settle 0.3)
          (should-not (drover-remote-test-calls "send"))
          (let ((first (car (drover-remote-items-test-there))))
            (should (equal (nth 3 first) "save"))
            (should-not (nth 4 first))
            (should-not (nth 5 first)))
          (should (drover-remote-items-test-asked-there))
          (should (drover-items-review-since session))
          (should (member "§ Waits for your review" (drover-items-test-lines)))
          ;; The refusal is in the history of the link there.
          (should (seq-find (lambda (event) (and (equal (aref event 0) "refused")
                                                 (string-match-p "apply the items of a session" (aref event 1))))
                            (plist-get (drover-remote-test-command "history" :peer "viewer") :events))))))))

(ert-deftest drover-remote-items-test-phone ()
  "What a phone asks: the reviews, a list, the places its host offers, an apply with one, or with none."
  (skip-unless (drover-remote-test--ptys-p))
  (drover-remote-items-test-with '("DROVER_AGENT_VIEWERS=viewer" "DROVER_AGENT_ACCESS=act")
    (drover-remote-test-command "terminal" :id "s0")
    (drover-remote-items-test-give t)
    (let ((key ["fake" "s0"]))
      ;; Followed: the sessions whose lists wait for a review.
      (pcase-let ((`(,reply ,error) (drover-remote-items-test-ask (list :op "items" :follow t))))
        (should-not error)
        (should (equal (aref (aref (plist-get reply :reviews) 0) 0) key))
        (should (numberp (aref (aref (plist-get reply :reviews) 0) 1)))
        (should-not (plist-member reply :list)))
      ;; A list: each item with its status, its heading and the sessions serving it.
      (pcase-let ((`(,reply ,error) (drover-remote-items-test-ask (list :op "items" :key key))))
        (should-not error)
        (let* ((list (plist-get reply :list))
               (third (aref (plist-get list :items) 2)))
          (should (equal (plist-get list :k) key))
          (should (numberp (plist-get list :a)))
          (should (equal (mapcar (lambda (item) (plist-get item :n)) (plist-get list :items)) '(1 2 3)))
          (should (equal (plist-get third :st) "blocked"))
          (should (equal (plist-get third :h) "H-WAITING"))
          (should (equal (plist-get third :ht) "Hear from the vendor"))
          (should (eq (plist-get third :hf) t))
          (should (eq (plist-get third :g) :false))
          (should (equal (plist-get (aref (plist-get third :ss) 0) :k) ["fake" "s1"]))))
      ;; A session without a list.
      (should (eq (plist-get (car (drover-remote-items-test-ask (list :op "items" :key ["fake" "s1"]))) :list) :false))
      ;; Where two items go.
      (dolist (change '((1 . "session") (3 . "save")))
        (pcase-let ((`(,reply ,error)
                     (drover-remote-items-test-ask (list :op "items-goes" :key key :n (car change) :goes (cdr change)))))
          (should-not error)
          (should (equal (plist-get (aref (plist-get (plist-get reply :list) :items) (1- (car change))) :g)
                         (cdr change)))))
      ;; Item 3 has its heading: one place is needed, and none given is refused.
      (pcase-let ((`(,reply ,error)
                   (drover-remote-items-test-ask (list :op "items-apply" :id "phone-1" :key key :places []))))
        (should-not error)
        (should (equal (plist-get reply :refused)
                       ["Session s0 needs a place for the heading of item 1: read its list again"])))
      ;; The places its host offers: none chosen there yet, but for the heading of its task.
      (drover-remote-test-command "task-session" :id "s0" :heading "PLACE-DROVER")
      (pcase-let ((`(,reply ,error) (drover-remote-items-test-ask (list :op "items-places" :key key))))
        (should-not error)
        (should (equal (plist-get reply :places) (vector (list :id "PLACE-DROVER" :name "Drover")))))
      ;; Applied with it: the session told, the place named as its host names it.
      (pcase-let ((`(,reply ,error)
                   (drover-remote-items-test-ask (list :op "items-apply" :id "phone-2" :key key
                                                       :places (vector (list :n 1 :id "PLACE-DROVER"))))))
        (should-not error)
        (should (equal (plist-get reply :sent) 1))
        (should (eq (plist-get reply :waits) :false))
        (should (eq (plist-get (plist-get reply :list) :a) :false))
        (should (equal (plist-get (plist-get (aref (plist-get (plist-get reply :list) :items) 0) :pl) :name) "Drover")))
      (should (drover-remote-test-wait (lambda () (drover-remote-test-calls "send"))))
      (should (string-search "Its heading goes under `org-id://PLACE-DROVER` (the heading \"Drover\")."
                             (nth 2 (car (drover-remote-test-calls "send")))))
      ;; Asked again, its answer lost, it is told what was done, and nothing is sent twice.
      (pcase-let ((`(,reply ,error)
                   (drover-remote-items-test-ask (list :op "items-apply" :id "phone-2" :key key
                                                       :places (vector (list :n 1 :id "PLACE-DROVER"))))))
        (should-not error)
        (should (equal (plist-get reply :sent) 1))
        (should-not (plist-member reply :list)))
      (should (length= (drover-remote-test-calls "send") 1))
      ;; The place chosen is offered next time.
      (should (equal (plist-get (car (drover-remote-items-test-ask (list :op "items-places" :key key))) :places)
                     (vector (list :id "PLACE-DROVER" :name "Drover"))))
      ;; A place left to the agent: it goes where the work keeps its tasks.
      (drover-remote-test-command "items-set" :id "s0" :review :false :items [["Rename" "open"]])
      (drover-remote-items-test-ask (list :op "items-goes" :key key :n 4 :goes "fork"))
      (pcase-let ((`(,reply ,error)
                   (drover-remote-items-test-ask (list :op "items-apply" :id "phone-3" :key key
                                                       :places (vector (list :n 4 :id :false))))))
        (should-not error)
        (should (equal (plist-get reply :sent) 1)))
      (should (drover-remote-test-wait (lambda () (length= (drover-remote-test-calls "send") 2))))
      (should (string-search "- Item 4, \"Rename\" (open): to a fork of this session.  Its heading goes where this work keeps its tasks.\n"
                             (nth 2 (car (last (drover-remote-test-calls "send"))))))
      ;; What it asks is checked: an identifier, places of a known shape.
      (should (equal (cadr (drover-remote-items-test-ask (list :op "items-apply" :key key :places []))) "no identifier"))
      (should (equal (cadr (drover-remote-items-test-ask (list :op "items-apply" :id "phone-4" :key key
                                                              :places (vector (list :n 1 :id "bad id")))))
                     "places of no known shape")))))

;;;; Changes told by the owner

(ert-deftest drover-remote-items-test-pushed ()
  "A list changing on its host shows here without being asked for: items given, a status, a review."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give)
    (drover-remote-items-test-with-view
      (let ((session (drover-remote-items-test-session)))
        (drover-items)
        (should (drover-remote-test-wait #'drover-remote-items-test-list))
        (should (length= (drover-items-test-lines) 4))
        (should-not (drover-items-review-since session))
        ;; Its agent gives an item more.
        (drover-remote-test-command "items-set" :id "s0" :review :false :items [["Benchmark" "open"]])
        (should (drover-remote-test-wait
                 (lambda () (equal (last (drover-items-test-lines)) '("= o 4 Benchmark")))))
        ;; A status changes.
        (drover-remote-test-command "items-set" :id "s0" :review :false :items [["Cache key" "blocked" :false 1]])
        (should (drover-remote-test-wait
                 (lambda () (equal (nth 1 (drover-items-test-lines)) "= x 1 Cache key"))))
        ;; A review is asked: the side list says so.
        (drover-remote-test-command "items-set" :id "s0" :review t :items [["Cache key" "open" :false 1]])
        (should (drover-remote-test-wait
                 (lambda () (equal (seq-take (drover-items-test-lines) 3)
                                   '("· Session s0" "§ Waits for your review" "= o 1 Cache key")))))
        (should (drover-items-review-since session))
        ;; Hidden, it is told still, and draws as it shows again.
        (drover-items)
        (should-not (drover-items--side-window))
        (drover-remote-test-command "items-set" :id "s0" :review :false :items [["Hidden" "open"]])
        (should (drover-remote-test-wait
                 (lambda () (length= (plist-get (drover-remote-items-test-list) :items) 5))))
        (drover-items)
        (should (equal (last (drover-items-test-lines)) '("= o 5 Hidden")))
        ;; Its link dropped and back, what changed meanwhile is read.
        (drover-remote-test-command "drop")
        (drover-remote-test-settle 0.2)
        (drover-remote-test-command "items-set" :id "s0" :review :false :items [["After the drop" "open"]])
        (should (drover-remote-test-wait
                 (lambda () (equal (last (drover-items-test-lines)) '("= o 6 After the drop")))))))))

(ert-deftest drover-remote-items-test-rows-without-lists ()
  "A host that never read a list tells its rows the reviews of other hosts, loading nothing more."
  (let ((drover-remote-items--reviews (make-hash-table :test #'equal))
        (drover-list-review-function nil)
        (there (drover-session-create :backend 'remote:core :id '(opencode . "ses_1") :host "core"))
        (here (drover-session-create :backend 'fake :id "s1"))
        (changed nil))
    (cl-letf (((symbol-function 'drover-views-changed) (lambda (keys) (setq changed (append keys changed)))))
      (drover-remote-items--store-reviews "core" (vector (vector ["opencode" "ses_1"] 1100)
                                                         (vector "not a key" 5)
                                                         ["opencode" "ses_2"])
                                          100)
      (should (eq drover-list-review-function #'drover-remote-items--row-review))
      (should (equal changed '((remote:core opencode . "ses_1"))))
      ;; On this host's clock.
      (should (equal (funcall drover-list-review-function there) 1000))
      (should-not (funcall drover-list-review-function here))
      ;; One more, told as its list changes; then none.
      (setq changed nil)
      (let ((peer (drover-remote--peer-make :name "core")))
        (drover-remote-items-on-message peer (list :t "items" :k [["opencode" "ses_2"] "odd"] :a 2100 :u 2100))
        (should (equal changed '((remote:core opencode . "ses_2"))))
        (should (equal (drover-remote-items-review-since
                        (drover-session-create :backend 'remote:core :id '(opencode . "ses_2") :host "core"))
                       2100))
        (drover-remote-items-on-message peer (list :t "items" :k [["opencode" "ses_2"]] :a :false :u 2200))
        (should-not (drover-remote-items-review-since
                     (drover-session-create :backend 'remote:core :id '(opencode . "ses_2") :host "core"))))
      ;; Told anew, those no longer waiting go.
      (setq changed nil)
      (drover-remote-items--store-reviews "core" [] 0)
      (should (equal changed '((remote:core opencode . "ses_1")))))
    ;; A function of another's stays.
    (let ((drover-list-review-function #'ignore))
      (drover-remote-items--tell-rows)
      (should (eq drover-list-review-function #'ignore)))))

(ert-deftest drover-remote-items-test-review-follows-the-conversation ()
  "A session coming to show a conversation whose list waits for a review is told to the followers.
As a terminal resuming a conversation takes the place of the record of
its transcript, under another key."
  (drover-test-with-backend (list (drover-test-session "transcript" :engine 'fake :engine-id "conv"))
    (drover-test-with-state
      (let ((drover-items--table nil)
            (drover-remote-items--followers (make-hash-table :test #'eq))
            (drover-remote-items--told (make-hash-table :test #'equal))
            (drover-remote-items--changed nil)
            (drover-remote-items--timer nil)
            (drover-changed-hook (list #'drover-remote-items--sessions-changed))
            (drover-items-changed-hook (list #'drover-remote-items--list-changed))
            (sent nil))
        (cl-letf (((symbol-function 'drover-remote--served)
                   (lambda (key) (when-let* ((record (drover-index-session key))) (list record))))
                  ((symbol-function 'spyglass-connection-live-p) #'always)
                  ((symbol-function 'drover-remote--send) (lambda (_connection message) (push message sent))))
          (cl-flet ((told ()
                      (when (timerp drover-remote-items--timer)
                        (cancel-timer drover-remote-items--timer)
                        (drover-remote-items--tell))
                      (prog1 (mapcar (lambda (message) (list (plist-get message :k) (numberp (plist-get message :a))))
                                     (reverse sent))
                        (setq sent nil))))
            (puthash 'a-connection t drover-remote-items--followers)
            (drover-changed-now)
            ;; A review asked: told of the session showing its conversation.
            (drover-items-set (drover-find 'fake "transcript") (list (list :title "A" :status 'open)) t)
            (should (equal (told) '(([["fake" "transcript"]] t))))
            ;; Its session changing otherwise tells nothing more.
            (setq drover-test-sessions
                  (list (drover-test-session "transcript" :engine 'fake :engine-id "conv" :status 'working)))
            (drover-backend-changed 'fake "transcript")
            (drover-changed-now)
            (should-not (told))
            ;; Another session comes to show the conversation: its review is told of it.
            (setq drover-test-sessions (list (drover-test-session "terminal" :engine 'fake :engine-id "conv")))
            (drover-backend-changed 'fake)
            (drover-changed-now)
            (should (equal (told) '(([["fake" "terminal"]] t))))
            (drover-backend-changed 'fake)
            (drover-changed-now)
            (should-not (told))
            ;; Over: told so, and nothing after.
            (let ((list (drover-items-list-of (drover-find 'fake "terminal"))))
              (setf (plist-get list :asked) nil)
              (drover-items--changed (plist-get list :key)))
            (should (equal (told) '(([["fake" "terminal"]] nil))))
            (drover-backend-changed 'fake)
            (drover-changed-now)
            (should-not (told))))))))

(ert-deftest drover-remote-items-test-review-icon ()
  "A review asked on another host shows on its session's row here, and goes as it ends there."
  (drover-remote-items-test-with nil
    (let ((drover-list-review-icon "§")
          (drover-list-age-interval nil)
          (opencode-session-status-animation-interval nil)
          (key drover-remote-items-test-key)
          (buffer nil))
      (cl-flet ((row (id)
                  (with-current-buffer buffer
                    (drover-changed-now)
                    (setq drover-list--stale t)
                    (drover-list--refresh-if-needed)
                    (let ((section (drover-list--find-session (cons 'remote:agent (cons 'fake id)))))
                      (buffer-substring (oref section start) (oref section content))))))
        (unwind-protect
            (progn
              (setq buffer (drover-list--buffer))
              (with-current-buffer buffer
                (revert-buffer))
              (should-not (string-search "§" (row "s0")))
              ;; Asked there: told here, though its list was never read here.
              (drover-remote-items-test-give t)
              (should (drover-remote-test-wait (lambda () (drover-items-review-since (drover-index-session key)))))
              (should-not (drover-remote-items-list key))
              (let* ((text (row "s0"))
                     (at (string-search "§" text)))
                (should at)
                (should (eq (get-text-property at 'face text) 'drover-list-review))
                (should (equal (get-text-property at 'help-echo text) "Its list of items waits for your review")))
              (should-not (string-search "§" (row "s1")))
              ;; Its session needs the user no more for it than one of this host.
              (should-not (drover-session-attention-p (drover-index-session key)))
              ;; Told again as that host's remote layer starts anew.
              (clrhash drover-remote-items--reviews)
              (should-not (drover-items-review-since (drover-index-session key)))
              (drover-remote-test-command "restart")
              (should (drover-remote-test-wait (lambda () (drover-items-review-since (drover-index-session key)))))
              (should (string-search "§" (row "s0")))
              ;; Over there, as its choices are applied: gone here.
              (pcase-let ((`(,reply ,error)
                           (drover-remote-items-test-ask (list :op "items-apply" :id "viewer-review-1"
                                                               :key ["fake" "s0"] :places []))))
                (should-not error)
                (should (equal (plist-get reply :sent) 0)))
              (should (drover-remote-test-wait (lambda () (not (drover-items-review-since (drover-index-session key))))))
              (should-not (string-search "§" (row "s0"))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

;;;; The tools

(ert-deftest drover-remote-items-test-tools ()
  "Both tools serve a session of another host, through that host."
  (drover-remote-items-test-with nil
    (drover-remote-items-test-give t)
    ;; Read there.
    (let* ((answer (drover-remote-items-test-tool #'drover-anvil--tool-items "remote:agent/fake/s0"))
           (items (alist-get 'items answer))
           (third (nth 2 items)))
      (should (equal (alist-get 'outcome answer) "read"))
      (should (equal (alist-get 'key answer) "remote:agent/fake/s0"))
      (should (equal (alist-get 'review answer) "asked"))
      (should (equal (alist-get 'count answer) 3))
      (should (equal (mapcar (lambda (item) (alist-get 'status item)) items) '("open" "done" "blocked")))
      (should (equal (mapcar (lambda (item) (alist-get 'goes item)) items) '("keep" "none" "keep")))
      (should (equal (alist-get 'heading third)
                     '((id . "H-WAITING") (title . "Hear from the vendor") (found . t))))
      (should (equal (mapcar (lambda (session) (alist-get 'key session)) (alist-get 'sessions third))
                     '("remote:agent/fake/s1"))))
    ;; Given there: one added, one updated, the others kept.
    (let* ((answer (drover-remote-items-test-tool
                    #'drover-anvil--tool-items-set
                    (vector '((title . "From another host") (status . "open") (next . "Check it there"))
                            '((id . 1) (title . "Cache key, sharper") (status . "blocked")))
                    "remote:agent/fake/s0" :json-false))
           (items (alist-get 'items answer)))
      (should (equal (alist-get 'outcome answer) "kept"))
      (should (equal (mapcar (lambda (item) (alist-get 'id item)) items) '(1 2 3 4)))
      (should (equal (alist-get 'title (car items)) "Cache key, sharper"))
      (should (equal (alist-get 'status (car items)) "blocked"))
      (should (equal (alist-get 'next (nth 3 items)) "Check it there")))
    (let ((there (drover-remote-items-test-there)))
      (should (equal (mapcar #'cadr there) '("Cache key, sharper" "Old fix" "Vendor" "From another host")))
      (should (equal (nth 2 (car there)) "blocked")))
    ;; A review asked from here is asked there.
    (should-not (plist-get (drover-remote-test-command "items-set" :id "s1" :review :false :items [["Quiet" "open"]])
                           :error))
    (should-not (drover-remote-items-test-asked-there "s1"))
    (should (equal (alist-get 'review (drover-remote-items-test-tool
                                       #'drover-anvil--tool-items-set
                                       (vector '((title . "Loud") (status . "open")))
                                       "remote:agent/fake/s1"))
                   "asked"))
    (should (drover-remote-items-test-asked-there "s1"))
    ;; What its host refuses is told as this host's own refusal, each item named, and nothing kept.
    (let ((why (cadr (should-error
                      (drover-remote-items-test-tool
                       #'drover-anvil--tool-items-set
                       (vector '((title . "Made up") (status . "open") (link . "sftp://reaver/srv/plan.txt"))
                               '((title . "Odd") (status . "started"))
                               '((title . "Unknown") (status . "open") (priority . "high"))
                               '((id . 9) (title . "Gone") (status . "open"))
                               '((title . "Missing heading") (status . "open") (heading . "NO-SUCH-ID")))
                       "remote:agent/fake/s0")))))
      (should (string-prefix-p "Nothing kept: " why))
      (dolist (told '("#1 \"Made up\" link sftp://reaver/srv/plan.txt has a type Org does not know, sftp"
                      "#2 \"Odd\" has the status started"
                      "#3 \"Unknown\" has no field priority"
                      "#4 \"Gone\" updates item 9, which the list does not hold"
                      "#5 \"Missing heading\" heading names no heading of ID NO-SUCH-ID"))
        (should (string-search told why))))
    (should (length= (drover-remote-items-test-there) 4))
    ;; A session without a list has none to keep nothing in.
    (drover-remote-test-command "add" :ids ["s2"])
    (drover-remote-test-synced 3)
    (should (equal (alist-get 'note (drover-remote-items-test-tool #'drover-anvil--tool-items "remote:agent/fake/s2"))
                   "This session has no list"))
    (should (string-match-p "agent keeps nothing: Session s2 has no list yet: give its items"
                            (cadr (should-error (drover-remote-items-test-tool
                                                 #'drover-anvil--tool-items-set [] "remote:agent/fake/s2")))))))

;;;; An older peer

(ert-deftest drover-remote-items-test-older-peer ()
  "A host whose drover keeps no list for others is asked nothing, and named, with what to do."
  (drover-remote-items-test-with
      '("DROVER_AGENT_CAPS=actions,bye,grant,mirror,unsent,search,clone,new,task,jobs,opts")
    (should-not (drover-remote--capable-p (drover-remote--peer "agent") "items"))
    (let ((session (drover-remote-items-test-session))
          (why "agent runs an older drover, which keeps no list of items for other hosts: update it there"))
      ;; Its sessions are listed and acted on as before; nothing of their lists is asked.
      (should (equal (drover-remote-items-refusal session) why))
      (should-not (drover-items-review-since session))
      (let ((answer nil))
        (drover-remote-items-read session (lambda (result) (setq answer result)))
        (should (equal answer (list :failure why))))
      ;; The side list says so.
      (drover-remote-items-test-with-view
        (drover-items)
        ;; On as many lines as it takes: nothing of the advice is cut.
        (should (equal (car (drover-items-test-lines)) "· Session s0"))
        (should (length> (drover-items-test-lines) 2))
        (should (equal (string-join (cdr (drover-items-test-lines)) " ") why))
        (with-current-buffer (drover-items-test-view)
          (should (equal (cadr (should-error (drover-items-apply) :type 'user-error)) why))))
      (should (equal (cadr (should-error (drover-remote-items-set-goes session 1 'fork) :type 'user-error)) why))
      ;; Both tools refuse it by name.
      (dolist (refusal (list (cadr (should-error (drover-anvil--tool-items "remote:agent/fake/s0")))
                             (cadr (should-error (drover-anvil--tool-items-set
                                                  (vector '((title . "A") (status . "open")))
                                                  "remote:agent/fake/s0")))))
        (should (equal refusal (concat "remote:agent/fake/s0 runs on agent, which keeps its list: " why)))))))

(provide 'drover-remote-items-test)
;;; drover-remote-items-test.el ends here
