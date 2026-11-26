;;; drover-items.el --- The items a session opened, and where each goes  -*- lexical-binding: t; -*-

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

;; A long session collects items: actions, decisions, follow-ups.  Its
;; agent hands their list to drover, through the tool
;; drover-session-items-set of `drover-anvil', rather than print it in
;; the conversation, which a compaction may lose: drover keeps it for
;; the session's conversation in `drover-items-file', written as it
;; changes, so that it survives a restart or a crash of Emacs.  Each
;; item has a number that stays, a title, a status, a link, a next
;; step, a note, the Org heading it may have, and where the user wants
;; it to go: kept in the session, run in a subagent of the session,
;; given to a new session, to a fork of the session, or saved for
;; later.  Every item stays in its session until the user says
;; otherwise: an agent proposes nothing.  An item with a heading takes
;; its status from the heading (see `drover-task-org-status'), and
;; tells the sessions serving it, those archived since too.
;;
;; `drover-items', * on a session, toggles the side list of items: a
;; side window on the side opposite to the sidebar's, in the sidebar's
;; look (see `drover-list-sidebar-look'), an item a line, icons telling
;; where it goes, its status and the sessions serving its heading.  It
;; shows the list of the session the user is on, and follows as that
;; changes (`drover-items--follow').  Left and right change where the
;; item at point goes; C-c C-c applies the choices: where the heading
;; of each item leaving the session goes is asked, as Org's refile asks
;; a place (`drover-handoff-read-place'), then one prompt tells the
;; session's agent where each item goes (`drover-items-prompt'), as the
;; prompt of a hand-off goes (`drover-handoff-send').  The agent writes
;; the headings and starts the sessions: drover writes no heading.
;;
;; An agent may ask the user to review its list.  The session does not
;; need the user for it: its row shows an icon in drover's lists (see
;; `drover-list-review-function'), and the side list says so, until the
;; choices are applied.  The lists of the sessions of other hosts are
;; their hosts' to keep, and are neither kept nor shown here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'format-spec)
(require 'map)
(require 'org)
(require 'ol)
(require 'magit-section)
(require 'drover-core)
(require 'drover-ui)
(require 'drover-links)
(require 'drover-list)
(require 'drover-task)
(require 'drover-task-org)
(require 'drover-handoff)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 33))
  (load "drover-core" nil t))

;;;; Options

(defcustom drover-items-prompt
  "I chose where these items of the list of this session go:

%i

An item that goes to a subagent:

1. Run the item in a subagent that you start and manage yourself.  Write no Org heading for it.
2. Brief the subagent so that the brief reads alone.
3. Answer the questions of the subagent yourself.
4. Report the result of the subagent in this session.
5. Update the status of the item with `drover-session-items-set`, `review` false.

An item that goes to a new session or to a fork, or that is saved for later:

1. Write its Org heading with `org-add-todo`, with the place given as its `parent_uri`.  The tool gives the heading its ID.  An item whose heading is written already keeps that heading.
2. Write the body of the heading as a brief that reads alone, for a reader who saw nothing of this conversation.  The brief holds the goal, what is known with paths and links, the decisions already made, the limits, when the item is done, and a report paragraph.
3. Write the report paragraph for the session that takes the item: \"When the item is done or blocked, send one message to the session `%k` with `drover-session-send`.  Then set the state of this heading.\"
4. For a new session, call `drover-task-start` with the ID of the heading, `repo` or `directory`, and `agent`.
5. For a fork, call `drover-session-fork` with the ID of the heading as `task`, `agent`, and a `prompt` that names the item and links the heading.  A fork starts from this whole conversation, so only that prompt tells the fork which item is its own.
6. Choose `agent` yourself: `build` when the research is done and the brief says what to build, `plan` when design decisions remain.
7. For an item saved for later, start nothing.
8. Record the heading on its item: call `drover-session-items-set` with the `id`, `title` and `status` of the item, its `heading`, and `review` false.

Done when each item has its heading and its session, or its subagent ran, and your reply holds the link of each heading, `[[id:ID][title]]`, and of each new session, the `link` that the tool returned."
  "The prompt telling a session where the items of its list go.
A `format-spec' template: %i is the items leaving the session, a line
each, telling where it goes and where its heading goes, as Anvil's
org-add-todo takes a place (see `drover-items-apply'); %k the key of
the session, as the tools of its host name it, for the sessions the
items go to to report to (see `drover-handoff-session-key'); %% is a
percent sign.  The default names no skill and tells every step: the
agent is told nothing of where items go before."
  :type 'string
  :group 'drover)

(defcustom drover-items-destination-icons
  '((keep . "\U000F07D9")               ; nf-md-map_marker_outline
    (subagent . "\U000F167A")           ; nf-md-robot_outline
    (session . "\U000F1412")            ; nf-md-chat_plus_outline
    (fork . "\U000F04C1")               ; nf-md-source_fork
    (save . "\U000F00C3"))              ; nf-md-bookmark_outline
  "Icon of each place an item of a session's list may go.
An alist (DESTINATION . ICON), DESTINATION one of
`drover-items-destinations': the item stays in its session (`keep'),
runs in a subagent of it, goes to a new session, to a fork of the
session, or is saved for later.  The side list of items begins each
line with it, in `opencode-setting-chosen' while the session is still to
be told, else in `opencode-setting-inherited'.  Nerd Font icons by
default; any short strings will do."
  :type '(alist :key-type symbol :value-type string)
  :group 'drover)

(defcustom drover-items-status-icons
  '((open . "\U000F0130")               ; nf-md-checkbox_blank_circle_outline
    (blocked . "\U000F0377")            ; nf-md-minus_circle_outline
    (scheduled . "\U000F00F0")          ; nf-md-calendar_clock
    (done . "\U000F05E1")               ; nf-md-check_circle_outline
    (decided . "\U000F029B"))           ; nf-md-gavel
  "Icon of each status of an item of a session's list.
An alist (STATUS . ICON), STATUS one of `drover-items-statuses'.  The
side list of items shows it after where the item goes, in the face of
the status.  Nerd Font icons by default; any short strings will do."
  :type '(alist :key-type symbol :value-type string)
  :group 'drover)

(defface drover-items-blocked '((t :inherit warning :weight normal))
  "Face of the status of an item blocked."
  :group 'drover-faces)

(defface drover-items-scheduled '((t :inherit font-lock-constant-face))
  "Face of the status of an item scheduled."
  :group 'drover-faces)

(defface drover-items-done '((t :inherit success :weight normal))
  "Face of the status of an item done."
  :group 'drover-faces)

(defface drover-items-decided '((t :inherit drover-dim))
  "Face of the status of an item decided."
  :group 'drover-faces)

(defconst drover-items-statuses '(open blocked scheduled done decided)
  "The statuses of items.")

(defconst drover-items-closed-statuses '(done decided)
  "The statuses of items that go nowhere.")

(defconst drover-items-destinations '(keep subagent session fork save)
  "Where an item may go, in the order the keys step through them.
It stays in its session, runs in a subagent of it, goes to a new
session, to a fork of the session, or is saved for later.")

(defconst drover-items--destination-labels
  '((keep . "here") (subagent . "subagent") (session . "new session") (fork . "fork") (save . "later"))
  "How the side list names where an item goes.")

(defconst drover-items-buffer-name "*drover items*"
  "Name of the buffer of the side list of items, which `drover-items' shows.")

(defconst drover-items-archived-fetch-age 300
  "Seconds the archived sessions fetched serve the lists of items as they are.
A list whose items have headings tells the sessions serving them, those
archived too: they are fetched again from each backend whose last fetch
started longer ago (see `drover-keep-fetched').")

(defconst drover-items-limit 100
  "Items a list holds at most.")

(defconst drover-items-lists-limit 200
  "Lists kept at most, those changed last.")

(defconst drover-items-title-limit 200
  "Characters at most of an item's title.")

(defconst drover-items-next-limit 300
  "Characters at most of an item's next step.")

(defconst drover-items-note-lines 20
  "Lines at most of an item's note.")

(defconst drover-items-note-limit 2000
  "Characters at most of an item's note.")

(defconst drover-items-link-limit 1000
  "Characters at most of the target of an item's link.")

(defconst drover-items-origin "session items"
  "How the prompts telling where items go are named in the queue of prompts.")

(defconst drover-items--version 1
  "Version of the format of the file of the lists.")

(defconst drover-items--id-regexp "\\`[A-Za-z0-9][A-Za-z0-9_.:-]\\{0,99\\}\\'"
  "What the Org ID of a heading looks like.")

(defvar drover-items-changed-hook nil
  "Hook run with the key of a conversation whose list changed.")

;;;; The lists

;; A list is a plist: :key, the key of its conversation (see
;; `drover--conversation-key'); :title, its session's, as it was last
;; set; :next, the number its next item takes; :items, oldest first;
;; :asked, when its agent asked the user to review it, nil once the
;; choices were applied; :updated.  An item is a plist: :id, its
;; number; :title; :status, the agent's, or the one its heading told
;; last; :link, (TARGET . DESCRIPTION); :next; :note; :heading, an Org
;; ID; :heading-title, the heading's as read last; :goes, where the
;; user said it goes; :place, where its heading goes, a place of
;; `drover-handoff-read-place'; :sent, (GOES . TIME) once the session
;; was told; :created, :updated.

(cl-defstruct (drover-items--table (:constructor drover-items--table-make)
                                   (:copier nil))
  "The lists of items of the sessions of this host."
  (stamp (drover-layout 'drover-items--table)
         :documentation "The layout it was made of, see `drover-layout'.")
  (lists (make-hash-table :test #'equal)
         :documentation "The lists, a plist each, by the key of their conversation.")
  (loaded nil :documentation "Non-nil once the file was read.")
  (unreadable nil :documentation "Non-nil when the file did not read, and is never written over."))

(drover-note-layout 'drover-items--table)

(defvar drover-items--table nil
  "The lists of items, a `drover-items--table', or nil before they are read.")

(defun drover-items--state ()
  "Return the table of lists, made anew when another version of drover made it."
  (unless (drover-layout-current-p drover-items--table 'drover-items--table)
    (when drover-items--table
      (drover-stale-layout "the lists of items"))
    (setq drover-items--table (drover-items--table-make)))
  drover-items--table)

(defun drover-items-file ()
  "Return the file keeping the lists of items, in `drover-state-directory'."
  (expand-file-name "items.eld" drover-state-directory))

(defun drover-items--key-p (key)
  "Return non-nil when KEY is the key of a conversation, as lists are kept by."
  (and (consp key) (symbolp (car key)) (car key)
       (or (stringp (cdr key)) (integerp (cdr key)))))

(defun drover-items--text-p (value &optional limit)
  "Return non-nil when VALUE is nil or a string of LIMIT characters at most."
  (or (null value) (and (stringp value) (<= (length value) (or limit 4000)))))

(defun drover-items--place-p (place)
  "Return non-nil when PLACE is nil or a place, as the file keeps it."
  (or (null place)
      (and (plistp place)
           (stringp (plist-get place :uri))
           (drover-items--text-p (plist-get place :name) 1000)
           (drover-items--text-p (plist-get place :file) 4000)
           (drover-items--text-p (plist-get place :id) 100))))

(defun drover-items--item-p (item)
  "Return non-nil when ITEM is an item, as the file keeps it."
  (and (plistp item)
       (natnump (plist-get item :id)) (> (plist-get item :id) 0)
       (stringp (plist-get item :title))
       (memq (plist-get item :status) drover-items-statuses)
       (let ((link (plist-get item :link)))
         (or (null link)
             (and (consp link) (stringp (car link)) (drover-items--text-p (cdr link) 1000))))
       (drover-items--text-p (plist-get item :next))
       (drover-items--text-p (plist-get item :note))
       (or (null (plist-get item :heading))
           (and (stringp (plist-get item :heading))
                (string-match-p drover-items--id-regexp (plist-get item :heading))))
       (drover-items--text-p (plist-get item :heading-title) 1000)
       (memq (plist-get item :goes) (cons nil drover-items-destinations))
       (drover-items--place-p (plist-get item :place))
       (let ((sent (plist-get item :sent)))
         (or (null sent) (and (consp sent) (memq (car sent) drover-items-destinations) (numberp (cdr sent)))))))

(defun drover-items--bound (item)
  "Return ITEM with its texts bounded, as drover keeps them.
What an agent proposed of where it goes, which a file of an earlier
drover holds, is left out: an item stays until the user says otherwise."
  (cl-flet ((line (value limit) (and (stringp value) (drover-bound-line value limit))))
    (let ((link (plist-get item :link)))
      (cl-remf item :proposal)
      (plist-put item :title (or (line (plist-get item :title) drover-items-title-limit) ""))
      (plist-put item :next (line (plist-get item :next) drover-items-next-limit))
      (plist-put item :note (and (stringp (plist-get item :note))
                                 (drover-bound-text (plist-get item :note)
                                                    drover-items-note-lines drover-items-note-limit)))
      (plist-put item :heading-title (line (plist-get item :heading-title) 300))
      (plist-put item :link (and link (cons (line (car link) drover-items-link-limit)
                                            (line (cdr link) 200)))))))

(defun drover-items--list-in (entry)
  "Return the list ENTRY, as read from the file, its items checked, or nil.
An item that is not one is left out."
  (when (and (plistp entry)
             (drover-items--key-p (plist-get entry :key))
             (natnump (plist-get entry :next))
             (proper-list-p (plist-get entry :items))
             (or (null (plist-get entry :asked)) (numberp (plist-get entry :asked)))
             (numberp (plist-get entry :updated))
             (drover-items--text-p (plist-get entry :title) 1000))
    (let ((items (mapcar #'drover-items--bound
                         (seq-take (seq-filter #'drover-items--item-p (plist-get entry :items))
                                   drover-items-limit))))
      (plist-put (copy-sequence entry) :items items))))

(defun drover-items-load ()
  "Read the lists of items, once, and return the table.
A file that does not read is told of, and never written over."
  (let ((table (drover-items--state)))
    (unless (drover-items--table-loaded table)
      (setf (drover-items--table-loaded table) t)
      (condition-case err
          (let ((dropped 0))
            (dolist (entry (drover-read-data (drover-items-file) drover-items--version :lists))
              (if-let* ((list (drover-items--list-in entry)))
                  (puthash (plist-get list :key) list (drover-items--table-lists table))
                (cl-incf dropped)))
            (when (> dropped 0)
              (drover-warn "Left out %d list%s of items that %s not read in %s" dropped
                           (if (= dropped 1) "" "s") (if (= dropped 1) "was" "were")
                           (abbreviate-file-name (drover-items-file)))))
        (error (setf (drover-items--table-unreadable table) t)
               (drover-warn "The lists of items were not read, and are not written over: %s"
                            (drover-error-message err)))))
    table))

(defun drover-items--loaded-p ()
  "Return non-nil when the lists were read, by this version of drover."
  (and drover-items--table
       (drover-layout-current-p drover-items--table 'drover-items--table)
       (drover-items--table-loaded drover-items--table)))

(defun drover-items--save ()
  "Write the lists, those changed last, or signal an error.
Never over a file that did not read."
  (let ((table (drover-items--state)))
    (when (drover-items--table-unreadable table)
      (error "The file of the lists of items did not read: not writing over it"))
    (let ((lists nil))
      (maphash (lambda (_key list) (push list lists)) (drover-items--table-lists table))
      (setq lists (sort lists :key (lambda (list) (plist-get list :updated)) :reverse t))
      (dolist (gone (nthcdr drover-items-lists-limit lists))
        (remhash (plist-get gone :key) (drover-items--table-lists table)))
      (drover-write-data (drover-items-file) "Lists of the items of sessions" drover-items--version :lists
                         (seq-take lists drover-items-lists-limit)))))

(defun drover-items--keep ()
  "Write the lists, warning when they cannot be."
  (condition-case err
      (drover-items--save)
    (error (drover-warn "The lists of items were not kept: %s" (drover-error-message err)))))

(defun drover-items--key (session)
  "Return the key SESSION's list is kept by, its conversation's."
  (drover--conversation-key session))

(defun drover-items--list (key)
  "Return the list of the conversation KEY, or nil."
  (gethash key (drover-items--table-lists (drover-items-load))))

(defun drover-items-list-of (session)
  "Return the list of SESSION, a plist, or nil when it has none.
A session of another host has none here: its host keeps it."
  (and session (not (drover-session-host session))
       (drover-items--list (drover-items--key session))))

(defun drover-items-items (list)
  "Return the items of LIST, oldest first."
  (plist-get list :items))

(defun drover-items-asked (list)
  "Return when the user was asked to review LIST, or nil when not now."
  (plist-get list :asked))

(defun drover-items--find (list number)
  "Return the item NUMBER of LIST, or nil."
  (seq-find (lambda (item) (eql (plist-get item :id) number)) (plist-get list :items)))

(defun drover-items-p (session)
  "Return non-nil when SESSION has a list of items here.
Cheap: the menu asks it of the session at point."
  (and (drover-items--loaded-p)
       (drover-items-list-of session)
       t))

;;;; Items

(defun drover-items-heading-info (item)
  "Return what the heading of ITEM tells, of `drover-task-org-read', or nil.
Nil for an item without a heading, or one not found in the buffers
visiting files nor in those the ID locations name."
  (when-let* ((id (plist-get item :heading)))
    (drover-task-info id)))

(defun drover-items-item-status (item)
  "Return the status of ITEM, one of `drover-items-statuses'.
That of its heading, when it has one found (see
`drover-task-org-status'), else the status kept."
  (if-let* ((info (drover-items-heading-info item)))
      (drover-task-org-status info)
    (plist-get item :status)))

(defun drover-items-closed-p (item)
  "Return non-nil when ITEM is done or decided, and goes nowhere."
  (memq (drover-items-item-status item) drover-items-closed-statuses))

(defun drover-items-goes (item)
  "Return where ITEM goes: nil for one done or decided, else a destination.
The user's choice, else it stays (`keep'); see
`drover-items-destinations'."
  (unless (drover-items-closed-p item)
    (or (plist-get item :goes) 'keep)))

(defun drover-items-chosen-by (item)
  "Return who chose where ITEM goes: `user', or `default' while it stays unchosen."
  (if (plist-get item :goes) 'user 'default))

(defvar drover-items--serving nil
  "The sessions serving each heading, a hash table by Org ID, or nil.
Bound while a list is drawn or told, by `drover-items-with-serving':
each item then looks its sessions up, rather than look through every
session drover knows.")

(defun drover-items--serving-table ()
  "Return the sessions serving headings, a hash table by Org ID.
Each value lists them as `drover-task-sessions' does with ALL: the live
ones, most recent first, then those archived and the older ones."
  (let* ((table (make-hash-table :test #'equal))
         (live (drover-sessions))
         (recent (lambda (sessions)
                   (sort sessions :key (lambda (session) (- (or (drover-session-updated session) 0)))))))
    ;; Pushed last first, so that each list ends up in order.
    (dolist (sessions (list (funcall recent (seq-difference (drover-known-sessions live t) live #'eq))
                            (funcall recent live)))
      (dolist (session (reverse sessions))
        (when-let* ((id (drover-task-id-of session)))
          (push session (gethash id table)))))
    table))

(defmacro drover-items-with-serving (&rest body)
  "Run BODY with the sessions serving headings found once for all its items.
See `drover-items-heading-sessions'."
  (declare (indent 0) (debug t))
  `(let ((drover-items--serving (or drover-items--serving (drover-items--serving-table))))
     ,@body))

(defun drover-items-heading-sessions (item)
  "Return the sessions serving the heading of ITEM.
The live ones, most recent first, then those archived since and the
older ones drover knows: a session that took an item is archived once
it is done, and still tells where the item went.  One its backend
archived is known once the archived sessions were fetched (see
`drover-items-want-archived')."
  (when-let* ((info (drover-items-heading-info item)))
    (if drover-items--serving
        (seq-uniq (append (gethash (plist-get info :id) drover-items--serving)
                          (and (plist-get info :orig-id)
                               (gethash (plist-get info :orig-id) drover-items--serving)))
                  #'eq)
      (drover-task-sessions info t))))

(defun drover-items-want-archived (list)
  "Have drover keep the archived sessions fetched, for the headings of LIST.
When an item of LIST has a heading, whose sessions may be archived:
fetched from each backend whose last fetch started more than
`drover-items-archived-fetch-age' seconds ago, and kept from then on,
as a search keeps them (see `drover-keep-fetched').  Return non-nil
while a fetch is awaited."
  (when (seq-some (lambda (item) (plist-get item :heading)) (plist-get list :items))
    (drover-keep-fetched '(archived) drover-items-archived-fetch-age)
    (drover-fetch-pending-p 10)))

(defun drover-items-link-text (item)
  "Return the link of ITEM as Org writes it, [[TARGET][DESCRIPTION]], or nil."
  (when-let* ((link (plist-get item :link)))
    (if (cdr link)
        (format "[[%s][%s]]" (car link) (cdr link))
      (format "[[%s]]" (car link)))))

(defun drover-items--refresh (list)
  "Bring the statuses of the items of LIST with a heading up to date.
As their headings tell them now (see `drover-items-item-status'), and
the titles of those headings; return non-nil when one changed."
  (let ((changed nil))
    (dolist (item (plist-get list :items))
      (when-let* ((info (drover-items-heading-info item)))
        (let ((status (drover-task-org-status info))
              (title (and (stringp (plist-get info :title)) (drover-bound-line (plist-get info :title) 300))))
          (unless (and (eq status (plist-get item :status)) (equal title (plist-get item :heading-title)))
            (setf (plist-get item :status) status
                  (plist-get item :heading-title) title)
            (setq changed t)))))
    changed))

(defun drover-items-find-headings (list callback)
  "Look for the headings of LIST found nowhere yet, then call CALLBACK.
With no argument, at once when every heading is in a buffer visiting
its file or a file the ID locations name, else once ripgrep looked for
the others (see `drover-task-org-find').  The statuses of the items
are brought up to date then, and kept."
  (let* ((ids (seq-uniq (seq-keep (lambda (item)
                                    (and (plist-get item :heading)
                                         (null (drover-items-heading-info item))
                                         (plist-get item :heading)))
                                  (plist-get list :items))))
         (left (length ids))
         (finish (lambda ()
                   (when (drover-items--refresh list)
                     (drover-items--keep))
                   (funcall callback))))
    (if (null ids)
        (funcall finish)
      (dolist (id ids)
        (drover-task-org-find id (lambda (marker)
                                   (when marker
                                     (set-marker marker nil))
                                   (when (zerop (cl-decf left))
                                     (funcall finish))))))))

;;;; Reviews

(defun drover-items--session-keys (key)
  "Return the keys of the live sessions showing the conversation KEY."
  (or (copy-sequence (gethash key (drover--index-engines (drover-index))))
      (and (drover-index-session key) (list key))))

(defun drover-items-review-since (session)
  "Return since when the list of SESSION waits for the user's review, or nil.
The value of `drover-list-review-function', called for every row a list
draws: a lookup, and nil until the lists were read."
  (and (drover-items--loaded-p)
       (not (drover-session-host session))
       (plist-get (gethash (drover--conversation-key session) (drover-items--table-lists drover-items--table))
                  :asked)))

(defun drover-items--changed (key)
  "Note that the list of the conversation KEY changed: tell views and lists.
The rows of its sessions are drawn again, which show whether its review
waits (see `drover-list-review-function')."
  (when-let* ((keys (drover-items--session-keys key)))
    (drover-views-changed keys))
  (drover-run-hook-reporting 'drover-items-changed-hook key))

;;;; Links

(defun drover-items-parse-link (text)
  "Return the link TEXT writes, as (TARGET . DESCRIPTION), or nil when blank.
TEXT is an Org link, [[TARGET][DESCRIPTION]] or [[TARGET]], a Markdown
one, [DESCRIPTION](TARGET), or a target alone."
  (let ((text (and (stringp text) (string-trim text))))
    (cond
     ((or (null text) (string-empty-p text)) nil)
     ((string-match "\\`\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]\\'" text)
      (cons (match-string 1 text) (let ((description (match-string 2 text)))
                                    (and description (not (string-blank-p description)) description))))
     ((string-match "\\`\\[\\([^]]*\\)\\](\\([^) \t\n]+\\))\\'" text)
      (cons (match-string 2 text) (let ((description (match-string 1 text)))
                                    (and (not (string-blank-p description)) description))))
     (t (cons text nil)))))

(defun drover-items-heading-id (text)
  "Return the Org ID TEXT names, or nil.
TEXT is the ID itself, id:ID, or an Org or Markdown link to id:ID."
  (let* ((link (drover-items-parse-link text))
         (target (car link))
         (id (cond ((null target) nil)
                   ((string-prefix-p "id:" target) (substring target 3))
                   ((and (null (cdr link)) (not (string-search ":" target))) target)
                   ((string-match-p drover-items--id-regexp target) target))))
    (when id
      (let ((id (car (split-string id "::"))))
        (and (string-match-p drover-items--id-regexp id) id)))))

(defun drover-items--session-link-known-p (conversation)
  "Return non-nil when drover knows the session CONVERSATION names.
CONVERSATION is a plist of `drover-links-parse'.  A conversation of this
host is known when a session listed or archived shows it; one of
another host, when a session of that host shows it."
  (map-let (:engine :id :host) conversation
    (if (and host (not (drover-host-local-p host)))
        (seq-some (lambda (session)
                    (and (drover-session-host session)
                         (or (string-equal-ignore-case (drover-session-host session) host)
                             (string-equal-ignore-case (drover-host-label (drover-session-host session)) host))
                         (eq (drover-session-engine session) engine)
                         (equal (drover-session-engine-id session) id)))
                  (drover-index-sessions))
      (let ((key (cons engine id)))
        (or (gethash key (drover--index-engines (drover-index)))
            (drover-archived-conversation key))))))

(defun drover-items--link-problem (link directory)
  "Say why LINK cannot be what it says, or nil when it can be.
LINK is (TARGET . DESCRIPTION).  A link to an ID returns (:id ID), the
heading to look for.  A relative file name is of DIRECTORY."
  (let* ((target (car link))
         (type (and (string-match "\\`\\([A-Za-z][A-Za-z0-9+.-]*\\):" target)
                    (match-string 1 target)))
         (path (and type (substring target (1+ (length type))))))
    (cond
     ((member (downcase (or type "")) '("http" "https")) nil)
     ((and (null type) (string-match-p "\\`\\(?:/\\|~\\|\\./\\|\\.\\./\\)" target))
      (drover-items--file-problem target directory))
     ((null type)
      "names no link type: give a link, such as https://… or [[id:ID]]")
     ((equal type "id")
      (let ((id (car (split-string path "::"))))
        (if (string-match-p drover-items--id-regexp id)
            (list :id id)
          (format "%s is no Org ID" (drover-bound-line id 60)))))
     ((equal type drover-links-type)
      (let ((conversation (drover-links-parse path)))
        (cond ((null conversation) "names no conversation: agent-session:ENGINE/ID")
              ((drover-items--session-link-known-p conversation) nil)
              (t "names a session drover does not know"))))
     ((assoc type drover-links-front-end-types)
      (let ((conversation (drover-links-parse path (nth 2 (assoc type drover-links-front-end-types)))))
        (if (and conversation (drover-items--session-link-known-p conversation))
            nil
          "names a session drover does not know")))
     ((string-prefix-p "file" type)
      (if (member type (org-link-types))
          (drover-items--file-problem path directory)
        (format "has a type Org does not know, %s" type)))
     ((member type (org-link-types)) nil)
     (t (format "has a type Org does not know, %s" type)))))

(defun drover-items--file-problem (path directory)
  "Say why the file PATH, of an Org file link, cannot be what it says, or nil.
A relative PATH is of DIRECTORY; a search after :: is left out."
  (let* ((path (car (split-string path "::")))
         (file (expand-file-name path (or directory default-directory))))
    (cond ((file-remote-p file) "names a file of another machine, which drover does not look at")
          ((file-exists-p file) nil)
          (t (format "names a file that does not exist, %s" (abbreviate-file-name file))))))

;;;; Lists agents give

;; The items of a call, as `drover-anvil' takes them: each a plist of
;; :index, its place in the call from 1, and the fields given, as
;; received, :title, :status, :link, :next, :note, :heading and :id, an
;; empty string for an optional field clearing it; :unknown, the names
;; of fields that are none; :malformed, non-nil for an item that is no
;; object.

(defconst drover-items-fields '(:title :status :link :next :note :heading :id)
  "The fields an item is given with.")

(defun drover-items--given-name (given)
  "Return how a refusal names the item GIVEN: its place in the call, and title."
  (let ((title (plist-get given :title)))
    (if (and (stringp title) (not (string-blank-p title)))
        (format "#%d \"%s\"" (plist-get given :index) (drover-bound-line title 40))
      (format "#%d" (plist-get given :index)))))

(defun drover-items--normalize (given list directory)
  "Return GIVEN checked, as (ITEM . PROBLEMS), for LIST, of DIRECTORY.
ITEM is GIVEN's fields made what an item keeps; PROBLEMS, what refuses
it, strings; an ID to look for among headings is (:id ID)."
  (let ((item (list :index (plist-get given :index)))
        (problems nil))
    (cl-flet ((problem (format-string &rest arguments)
                (push (apply #'format format-string arguments) problems))
              (text (key)
                (let ((value (plist-get given key)))
                  (cond ((not (plist-member given key)) 'absent)
                        ((or (null value) (eq value :json-false) (equal value "")) nil)
                        ((stringp value) (string-trim value))
                        (t 'malformed)))))
      (when (plist-get given :malformed)
        (problem "is not an object"))
      (dolist (name (plist-get given :unknown))
        (problem "has no field %s: an item has %s" (drover-bound-line (format "%s" name) 40)
                 (mapconcat (lambda (key) (substring (symbol-name key) 1)) drover-items-fields ", ")))
      (let ((number (plist-get given :id)))
        (when (plist-member given :id)
          (let ((number (cond ((natnump number) number)
                              ((and (stringp number) (string-match-p "\\`[0-9]\\{1,6\\}\\'" number))
                               (string-to-number number)))))
            (cond ((null number) (problem "has an id that is no number"))
                  ((not (drover-items--find list number))
                   (problem "updates item %d, which the list does not hold" number))
                  (t (setq item (plist-put item :id number)))))))
      (pcase (text :title)
        ((or 'absent 'nil) (problem "has no title"))
        ('malformed (problem "has a title that is no text"))
        (title (setq item (plist-put item :title title))))
      (pcase (text :status)
        ((or 'absent 'nil) (problem "has no status: one of %s" (mapconcat #'symbol-name drover-items-statuses ", ")))
        ('malformed (problem "has a status that is no text"))
        (status (if-let* ((symbol (seq-find (lambda (known) (equal (symbol-name known) (downcase status)))
                                            drover-items-statuses)))
                    (setq item (plist-put item :status symbol))
                  (problem "has the status %s: one of %s" (drover-bound-line status 40)
                           (mapconcat #'symbol-name drover-items-statuses ", ")))))
      (dolist (key '(:next :note))
        (pcase (text key)
          ('absent nil)
          ('malformed (problem "has a %s that is no text" (substring (symbol-name key) 1)))
          (value (setq item (plist-put item key value)))))
      (pcase (text :heading)
        ('absent nil)
        ('nil (setq item (plist-put item :heading nil)))
        ('malformed (problem "has a heading that is no text"))
        (heading (if-let* ((id (drover-items-heading-id heading)))
                     (progn (setq item (plist-put item :heading id))
                            (push (list :id id :heading t) problems))
                   (problem "has a heading that is no Org ID: %s" (drover-bound-line heading 60)))))
      (pcase (text :link)
        ('absent nil)
        ('nil (setq item (plist-put item :link nil)))
        ('malformed (problem "has a link that is no text"))
        (text (let* ((link (drover-items-parse-link text))
                     (why (drover-items--link-problem link directory)))
                (setq item (plist-put item :link link))
                (cond ((stringp why)
                       (problem "link %s %s" (drover-bound-line (car link) 60) why))
                      (why (push (list :id (plist-get why :id) :link (car link)) problems)))))))
    (cons item (nreverse problems))))

(defun drover-items--room-problem (list items)
  "Say why LIST cannot hold the new ITEMS, or nil when it can.
Done and decided items no ITEMS update make room, the oldest first."
  (let* ((kept (plist-get list :items))
         (new (seq-count (lambda (item) (not (plist-get item :id))) items))
         (updated (seq-keep (lambda (item) (plist-get item :id)) items))
         (closed (seq-count (lambda (item)
                              (and (memq (plist-get item :status) drover-items-closed-statuses)
                                   (not (memq (plist-get item :id) updated))))
                            kept))
         (over (- (+ (length kept) new) drover-items-limit)))
    (when (> over closed)
      (format "the list holds %d items at most: it has %d, %d done or decided, and %d new would be %d too many"
              drover-items-limit (length kept) closed new (- over closed)))))

(defun drover-items-check (session given callback)
  "Check GIVEN, items an agent gives SESSION's list, and call CALLBACK.
GIVEN are as `drover-anvil' takes them (see `drover-items-fields').
CALLBACK gets (ITEMS . PROBLEMS): ITEMS the items checked, PROBLEMS
what refuses the call, each a string naming its item, nil when none.
At once, or once ripgrep looked for the headings neither the buffers
visiting files nor the ID locations hold (see `drover-task-org-find'):
a link to an ID, or a heading, names a heading that must be found."
  (let* ((list (drover-items-list-of session))
         (directory (drover-session-local-directory session))
         (checked (mapcar (lambda (given) (drover-items--normalize given list directory)) given))
         (problems nil)
         (sought nil))
    (dolist (entry checked)
      (dolist (problem (cdr entry))
        (if (stringp problem)
            (push (format "%s %s" (drover-items--given-name (car entry)) problem) problems)
          (push (cons (car entry) problem) sought))))
    (let ((numbers (seq-keep (lambda (entry) (plist-get (car entry) :id)) checked)))
      (dolist (number (seq-uniq numbers))
        (when (> (seq-count (lambda (other) (eql other number)) numbers) 1)
          (push (format "item %d is given twice" number) problems))))
    (when-let* ((why (drover-items--room-problem list (mapcar #'car checked))))
      (push why problems))
    (let* ((ids (seq-uniq (mapcar (lambda (entry) (plist-get (cdr entry) :id)) sought)))
           (found (make-hash-table :test #'equal))
           (left (length ids))
           (finish (lambda ()
                     (dolist (entry (reverse sought))
                       (let ((id (plist-get (cdr entry) :id)))
                         (unless (gethash id found)
                           (push (format "%s %s names no heading of ID %s in the Org files drover reads (never those of `drover-task-refused-files')"
                                         (drover-items--given-name (car entry))
                                         (if (plist-get (cdr entry) :heading) "heading" "link")
                                         id)
                                 problems))))
                     (funcall callback (cons (mapcar #'car checked) (nreverse problems))))))
      (if (null ids)
          (funcall finish)
        (dolist (id ids)
          (drover-task-org-find id (lambda (marker)
                                     (when marker
                                       (puthash id t found)
                                       (set-marker marker nil))
                                     (when (zerop (cl-decf left))
                                       (funcall finish)))))))))

(defun drover-items--update (item fields now)
  "Give ITEM the FIELDS given, a plist of `drover-items--normalize', at NOW."
  (dolist (key '(:title :status :link :next :note :heading))
    (when (plist-member fields key)
      (setq item (plist-put item key (plist-get fields key)))))
  (when (and (plist-member fields :heading) (null (plist-get fields :heading)))
    (setq item (plist-put item :heading-title nil)))
  (drover-items--bound (plist-put item :updated now)))

(defun drover-items-set (session items review)
  "Keep ITEMS, checked by `drover-items-check', in SESSION's list; return it.
An item with an :id updates the item of that number, the fields it
gives replacing those it had; another is added, its number the next
one.  The items ITEMS do not give stay.  Done and decided items make
room for new ones past `drover-items-limit', the oldest first.  With
REVIEW, the user is asked to review the list, unless asked already:
the rows of SESSION show so (see `drover-items-review-since'), and
SESSION needs the user no more for it.  The file is written before this
returns; signal an error, keeping nothing, when it cannot be."
  (let* ((table (drover-items-load))
         (key (drover-items--key session))
         (old (gethash key (drover-items--table-lists table)))
         (list (if old (copy-tree old) (list :key key :next 1 :items nil :asked nil)))
         (now (float-time))
         (updated (seq-keep (lambda (fields) (plist-get fields :id)) items))
         (added nil))
    (dolist (fields items)
      (if-let* ((number (plist-get fields :id)))
          (setf (plist-get list :items)
                (mapcar (lambda (item)
                          (if (eql (plist-get item :id) number) (drover-items--update item fields now) item))
                        (plist-get list :items)))
        (let ((number (plist-get list :next)))
          (setf (plist-get list :next) (1+ number))
          (push (drover-items--update (list :id number :created now) fields now) added))))
    (let* ((kept (append (plist-get list :items) (nreverse added)))
           (over (- (length kept) drover-items-limit)))
      ;; The oldest done and decided items make room, those updated aside.
      (when (> over 0)
        (let ((dropped (seq-take (seq-filter (lambda (item)
                                               (and (memq (plist-get item :status) drover-items-closed-statuses)
                                                    (not (memq (plist-get item :id) updated))))
                                             kept)
                                 over)))
          (setq kept (seq-remove (lambda (item) (memq item dropped)) kept))))
      (setf (plist-get list :items) kept))
    (when (and review (not (plist-get list :asked)))
      (setf (plist-get list :asked) now))
    (setf (plist-get list :updated) now
          (plist-get list :title) (drover-bound-line (format "%s" (drover-session-display-title session)) 300))
    (puthash key list (drover-items--table-lists table))
    (drover-items--refresh list)
    (condition-case err
        (drover-items--save)
      (error (if old
                 (puthash key old (drover-items--table-lists table))
               (remhash key (drover-items--table-lists table)))
             (signal (car err) (cdr err))))
    (drover-items--changed key)
    list))

;;;; The side list

;; One buffer shows the list of the session the user is on, in a side
;; window on the side opposite to the sidebar's, in the sidebar's look:
;; its face, its highlight for a cursor, its side window's settings,
;; and the functions measuring and cutting its lines in pixels (see
;; `drover-list-sidebar-look').  An item is a section of Magit's, its
;; line its heading and its details its body, hidden until TAB shows
;; them.  A line begins with its glyphs, at the window's edge, as the
;; sidebar's do; the lines of a body begin with an invisible character
;; (`drover-list--line-start'), so that a hidden body draws nothing.

(defclass drover-items-item-section (magit-section) ()
  "The section of an item in the side list of items; its value is its number.")

(defvar-local drover-items--view-key nil
  "The key of the conversation whose list the side list shows, or nil.
Nil while it shows no session, or one of another host.")

(defvar-local drover-items--view-session nil
  "The key of the session the side list shows the list of, or nil.")

(defvar-local drover-items--view-title nil
  "The title of the session the side list shows the list of.")

(defvar-local drover-items--sought nil
  "IDs of headings the side list looked for with ripgrep, a hash table.")

(defvar-local drover-items--width nil
  "The width in pixels lines could take as the side list was drawn last.")

(defvar-local drover-items--stale nil
  "Non-nil when what the side list shows changed while no window showed it.
It is then drawn again as a window shows it.")

(defvar-local drover-items--drawn nil
  "What the side list drew last depends on, as (SERIAL KEYS HEADINGS).
SERIAL is `drover-fetched-serial' then, KEYS the keys of the sessions
drawn, the one whose list it is and those serving headings, HEADINGS
the Org IDs of the headings of its items.  See `drover-items--on-change'.")

(defun drover-items--buffer (&optional create)
  "Return the buffer of the side list of items, or nil.
With CREATE, it is made when missing."
  (or (get-buffer drover-items-buffer-name)
      (when create
        (with-current-buffer (get-buffer-create drover-items-buffer-name)
          (drover-items-mode)
          (current-buffer)))))

(defun drover-items--side ()
  "Return the side of the frame the side list of items shows on.
The one opposite to the sidebar's (see `drover-list-side-window-side')."
  (if (eq drover-list-side-window-side 'right) 'left 'right))

(defun drover-items--side-window (&optional frame)
  "Return the side window showing the side list of items on FRAME, or nil.
FRAME defaults to the selected frame."
  (when-let* ((buffer (drover-items--buffer)))
    (seq-find (lambda (window) (window-parameter window 'window-side))
              (get-buffer-window-list buffer nil (or frame (selected-frame))))))

(defun drover-items--view-record ()
  "Return the session the side list shows the list of, or nil when it is gone."
  (or (and drover-items--view-session (drover-index-session drover-items--view-session))
      (when-let* ((keys (and drover-items--view-key (drover-items--session-keys drover-items--view-key))))
        (drover-index-session (car keys)))))

(defun drover-items--session-at-point ()
  "Return the session the side list shows the list of, or nil.
The value of `drover-session-at-point-function' in the side list."
  (drover-items--view-record))

;;;;; Lines

(defun drover-items--glyph (icons key face)
  "Return the icon ICONS gives KEY, in FACE; \"?\" for a key without one."
  (drover-face (or (alist-get key icons) "?") face))

(defun drover-items--status-face (status)
  "Return the face of STATUS, an item's."
  (pcase status
    ('blocked 'drover-items-blocked)
    ('scheduled 'drover-items-scheduled)
    ('done 'drover-items-done)
    ('decided 'drover-items-decided)
    (_ 'default)))

(defun drover-items--status-glyph (status)
  "Return the icon of STATUS, an item's, in its face.
See `drover-items-status-icons'."
  (drover-items--glyph drover-items-status-icons status (drover-items--status-face status)))

(defun drover-items--goes-glyph (item)
  "Return the icon of where ITEM goes, or nil for one that goes nowhere.
In `opencode-setting-chosen' while the session is still to be told (see
`drover-items--to-send-p'), which applying does, else dimmed, in
`opencode-setting-inherited'.  See `drover-items-destination-icons'."
  (when-let* ((goes (drover-items-goes item)))
    (drover-items--glyph drover-items-destination-icons goes
                         (if (drover-items--to-send-p item) 'opencode-setting-chosen 'opencode-setting-inherited))))

(defun drover-items--goes-words (item)
  "Return where ITEM goes in words, as tooltips and details tell it.
With when its session was told, or that applying is to tell it."
  (let ((goes (drover-items-goes item))
        (sent (plist-get item :sent)))
    (cond ((null goes) (format "nowhere: it is %s" (drover-items-item-status item)))
          ((eq (car sent) goes)
           (format "%s, told %s" (alist-get goes drover-items--destination-labels)
                   (format-time-string "%b %-e %H:%M" (cdr sent))))
          ((drover-items--to-send-p item)
           (format "%s, once applied" (alist-get goes drover-items--destination-labels)))
          (t (alist-get goes drover-items--destination-labels)))))

(defun drover-items--session-glyph (session)
  "Return the glyph of SESSION, which serves an item's heading.
That of its status, as lists draw it, or the archive's glyph for one
archived; its tooltip names it."
  (propertize (if (drover-session-archived session)
                  (opencode-archived-glyph)
                (opencode-session-status-glyph (drover-session-status session)))
              'help-echo (format "%s: %s" (drover-list--one-line (drover-session-display-title session))
                                 (if (drover-session-archived session)
                                     "archived"
                                   (opencode-session-status-label (drover-session-status session))))))

(defconst drover-items--sessions-shown 3
  "Sessions serving a heading whose glyphs a line shows at most.")

(defun drover-items--metrics (items)
  "Return the measures the lines of ITEMS are drawn with, as a plist.
:width, the pixels a line may take; :space, the width of a space;
:char, that of an average character; :goes-cell, :status-cell and
:session-cell, the cells of the icons of where items go, of their
statuses and of the sessions serving them (see `drover-list--cell'), so
that titles line up whatever the icons; :number, the width of the
widest number; :title-start, where titles begin."
  (let* ((space (drover-list--pixel-width " "))
         (goes-cell (drover-list--cell
                     (mapcar (lambda (goes)
                               (drover-items--glyph drover-items-destination-icons goes 'opencode-setting-chosen))
                             drover-items-destinations)))
         (status-cell (drover-list--cell (mapcar #'drover-items--status-glyph drover-items-statuses)))
         (session-cell (drover-list--cell
                        (cons (opencode-archived-glyph)
                              (mapcar #'opencode-session-status-glyph
                                      (seq-uniq (append drover-status-order
                                                        (mapcar #'car opencode-session-status-icons)))))))
         (number (apply #'max 0 (mapcar (lambda (item)
                                          (drover-list--pixel-width (drover-items--number-text item)))
                                        items))))
    (list :width (drover-list--line-width)
          :space space
          :char (drover-list--char-width)
          :goes-cell goes-cell
          :status-cell status-cell
          :session-cell session-cell
          :number number
          :title-start (+ (car goes-cell) (cdr goes-cell) space
                          (car status-cell) (cdr status-cell) space
                          number space))))

(defun drover-items--number-text (item)
  "Return the number of ITEM, dimmed."
  (drover-face (format "%d" (plist-get item :id)) 'drover-dim))

(defun drover-items--help (item)
  "Return the tooltip of the line of ITEM.
Its title, its status, where it goes and its next step."
  (drover-bound-lines
   (string-join (delq nil (list (format "%d. %s" (plist-get item :id) (plist-get item :title))
                                (format "%s, goes %s" (drover-items-item-status item) (drover-items--goes-words item))
                                (and (plist-get item :next) (concat "Next: " (plist-get item :next)))))
                "\n")
   6))

(defun drover-items--line (item metrics)
  "Return the line of ITEM, drawn with METRICS, and whether its title is whole.
As (LINE . WHOLE).  The icon of where it goes, the icon of its status,
its number, dimmed, then its title, cut to the room left as the sidebar
cuts its rows, and at the end of the line the glyphs of the sessions
serving its heading, `drover-items--sessions-shown' at most, then how
many more."
  (let* ((width (plist-get metrics :width))
         (space (plist-get metrics :space))
         (title-start (plist-get metrics :title-start))
         (number (drover-items--number-text item))
         (prefix (concat (drover-list--in-cell (or (drover-items--goes-glyph item) "") (plist-get metrics :goes-cell))
                         " "
                         (propertize (drover-list--in-cell (drover-items--status-glyph (drover-items-item-status item))
                                                           (plist-get metrics :status-cell))
                                     'help-echo (symbol-name (drover-items-item-status item)))
                         " "
                         (drover-list--pad (- (plist-get metrics :number) (drover-list--pixel-width number)))
                         number
                         " "))
         (sessions (drover-items-heading-sessions item))
         (shown (seq-take sessions drover-items--sessions-shown))
         (more (- (length sessions) (length shown)))
         (tail (and sessions
                    (concat (mapconcat (lambda (session)
                                         (drover-list--in-cell (drover-items--session-glyph session)
                                                               (plist-get metrics :session-cell)))
                                       shown "")
                            (if (> more 0) (drover-face (format "+%d" more) 'drover-dim) ""))))
         (tail-width (if tail (drover-list--pixel-width tail) 0))
         (tail-start (- width tail-width))
         ;; The sessions give way to the title in a window too narrow for both.
         (tail (and tail (> tail-start (+ title-start (* 4 (plist-get metrics :char)))) tail))
         (room (- (if tail (- tail-start space) width) title-start))
         (title (let ((title (drover-list--one-line (plist-get item :title))))
                  (if (drover-items-closed-p item) (drover-face title 'drover-dim) title)))
         (cut (drover-list--truncate title room))
         (line (car (drover-list--assemble
                     (append (list (drover-list--text-segment prefix 0)
                                   (drover-list--text-segment cut title-start))
                             (and tail (list (list 'align tail-start)
                                             (drover-list--text-segment tail tail-start))))))))
    (cons (drover-list--put-help line (drover-items--help item)) (eq cut title))))

(defun drover-items--details (item whole)
  "Return the details of ITEM, as a list of (LABEL TEXT LINES FACE).
What its line cuts or leaves out: its title, unless WHOLE, where it
goes and its status in words, its next step, its note, its link, its
heading, the sessions serving it and the place of its heading.  TEXT
takes at most LINES lines, wrapped; FACE, if any, is that of TEXT."
  (let ((info (drover-items-heading-info item))
        (details nil))
    (cl-flet ((add (label text lines &optional face)
                (when (and text (not (string-blank-p text)))
                  (push (list label text lines face) details))))
      (unless whole
        (add "Title" (plist-get item :title) 4))
      (add "Status" (symbol-name (drover-items-item-status item)) 1
           (drover-items--status-face (drover-items-item-status item)))
      (add "Goes" (drover-items--goes-words item) 2)
      (add "Next" (plist-get item :next) 4)
      (add "Note" (plist-get item :note) drover-items-note-lines)
      (add "Link" (when-let* ((link (plist-get item :link)))
                    (if (cdr link) (concat (cdr link) "\n" (car link)) (car link)))
           4)
      (add "Heading" (when-let* ((id (plist-get item :heading)))
                       (if info
                           (plist-get info :title)
                         (concat (or (plist-get item :heading-title) id) ", not found")))
           2 (and (not info) 'drover-dim))
      (dolist (session (drover-items-heading-sessions item))
        (add "Session" (concat (drover-items--session-glyph session) " "
                               (drover-list--one-line (drover-session-display-title session)))
             1))
      (add "Place" (plist-get (plist-get item :place) :name) 2))
    (nreverse details)))

(defun drover-items--insert-details (item metrics whole)
  "Insert the details of ITEM under its title, drawn with METRICS.
As the list draws those of a session it expands: each label followed by
its text, wrapped to the window, or, where that leaves fewer than 30
characters, as in a side window, the texts under their labels.  WHOLE
is non-nil when the line of ITEM shows its title whole.  Every line
begins with `drover-list--line-start'."
  (let* ((x (plist-get metrics :title-start))
         (width (plist-get metrics :width))
         (details (mapcar (pcase-lambda (`(,label . ,rest)) (cons (drover-face label 'drover-dim) rest))
                          (drover-items--details item whole)))
         (values-x (+ x (* 2 (plist-get metrics :char))
                      (apply #'max 0 (mapcar (lambda (detail) (drover-list--pixel-width (car detail))) details))))
         (stacked (< (- width values-x) (* 30 (plist-get metrics :char))))
         (text-x (if stacked x values-x)))
    (cl-flet ((line (&rest parts)
                (apply #'insert drover-list--line-start parts)
                (insert "\n")))
      (pcase-dolist (`(,label ,text ,lines ,face) details)
        (let* ((room (- width text-x))
               (texts (if (eql lines 1)
                          (list (drover-list--truncate (drover-list--one-line text) room))
                        (drover-list--wrap text room lines)))
               (texts (mapcar (lambda (text) (if face (drover-face text face) text)) texts)))
          (if stacked
              (line (drover-list--align x) label)
            (line (drover-list--align x) label (drover-list--align values-x) (or (car texts) ""))
            (setq texts (cdr texts)))
          (dolist (text texts)
            (line (drover-list--align text-x) text)))))))

(defun drover-items--session-line (record title metrics)
  "Return the line naming the session whose list shows, drawn with METRICS.
RECORD is its session, nil once it is gone; TITLE its title, as it was
last shown.  The glyph of its status, as lists draw it, then its title,
as a heading, cut to the window."
  (drover-list--truncate
   (concat (if record
               (concat (drover-list--icon (opencode-session-status-glyph (drover-session-status record))) " ")
             "")
           (drover-face (drover-list--one-line (or title "A session")) (if record 'drover-heading '(drover-dim drover-heading))))
   (plist-get metrics :width)))

(defun drover-items--note-line (text metrics &optional face)
  "Return TEXT as a line of the side list saying something, cut to METRICS.
In FACE, dimmed by default."
  (drover-list--truncate (drover-face text (or face 'drover-dim)) (plist-get metrics :width)))

(defun drover-items--review-line (metrics)
  "Return the line telling that the list waits for the user's review.
Cut to METRICS."
  (drover-list--truncate
   (concat (if (string-empty-p drover-list-review-icon)
               ""
             (concat (drover-list--icon (drover-face drover-list-review-icon 'drover-list-review)) " "))
           (drover-face "Waits for your review" 'drover-list-review))
   (plist-get metrics :width)))

;;;;; Drawing

(defun drover-items--sections ()
  "Return the sections of the items of the side list, in order."
  (and magit-root-section
       (seq-filter (lambda (section) (cl-typep section 'drover-items-item-section))
                   (oref magit-root-section children))))

(defun drover-items--section (number)
  "Return the section of the item NUMBER in the side list, or nil."
  (seq-find (lambda (section) (eql (oref section value) number)) (drover-items--sections)))

(defun drover-items--id-at-point ()
  "Return the number of the item at point in the side list, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (cl-typep section 'drover-items-item-section)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun drover-items--item-at-point ()
  "Return the item at point in the side list, or signal."
  (let ((number (or (drover-items--id-at-point) (user-error "No item at point"))))
    (or (and drover-items--view-key
             (drover-items--find (drover-items--list drover-items--view-key) number))
        (user-error "No item %d" number))))

(defun drover-items--seek (list)
  "Look for the headings of the items of LIST found nowhere yet.
Once for each session shown; each found has the side list drawn again."
  (let ((buffer (current-buffer)))
    (dolist (item (plist-get list :items))
      (when-let* ((id (plist-get item :heading))
                  ((not (gethash id drover-items--sought)))
                  ((null (drover-items-heading-info item))))
        (puthash id t drover-items--sought)
        (drover-task-org-find id (lambda (marker)
                                   (drover-with-reporting "items side list"
                                     (when marker
                                       (set-marker marker nil)
                                       (when (buffer-live-p buffer)
                                         (with-current-buffer buffer
                                           (drover-items--draw)))))))))))

(defun drover-items--draw ()
  "Draw the side list anew for the session it shows, point kept on its item.
The session's line, then, while its list waits for a review, a line
saying so, then a line per item (see `drover-items--line'), its details
under it, hidden unless they showed; a session without a list, or of
another host, has one line saying so."
  (let* ((inhibit-read-only t)
         ;; Drawing, often from a timer, must not end the user's region.
         (deactivate-mark nil)
         (record (drover-items--view-record))
         (list (and drover-items--view-key (drover-items--list drover-items--view-key)))
         (items (plist-get list :items))
         (windows (get-buffer-window-list nil 'nomini t))
         (here (drover-items--id-at-point))
         (starts (mapcar (lambda (window)
                           (cons window (count-lines (point-min) (window-start window))))
                         windows))
         (shown (seq-keep (lambda (section) (and (not (oref section hidden)) (oref section value)))
                          (drover-items--sections))))
    ;; Before measuring, which is for the width the fringes leave.
    (mapc #'drover-list--set-up-side-window windows)
    (drover-list--measure-begin)
    (when (and list (drover-items--refresh list))
      (drover-items--keep))
    (when list
      (drover-items-want-archived list))
    (when record
      (setq drover-items--view-title
            (drover-bound-line (format "%s" (drover-session-display-title record)) 200)))
    (drover-items-with-serving
      (let ((metrics (drover-items--metrics items))
            (keys (and record (list (drover-session-key record)))))
        (setq drover-items--width (plist-get metrics :width)
              drover-items--stale nil)
        ;; Magit's highlights are of the sections that go.
        (setq magit-section-pre-command-section nil
              magit-section-highlight-overlays nil
              magit-section-selection-overlays nil
              magit-section-highlighted-sections nil
              magit-section-focused-sections nil)
        (erase-buffer)
        (setq magit-root-section nil)
        (magit-insert-section (drover-items-root)
          (cond
           ((null drover-items--view-session)
            (insert (drover-items--note-line "No session" metrics) "\n"))
           (t
            (insert (drover-items--session-line record (or drover-items--view-title (plist-get list :title)) metrics)
                    "\n")
            (cond
             ((and record (drover-session-host record))
              (insert (drover-items--note-line (format "Its list is kept on %s"
                                                       (drover-host-label (drover-session-host record)))
                                               metrics)
                      "\n"))
             ((null items)
              (insert (drover-items--note-line "No items" metrics) "\n"))
             (t
              (when (plist-get list :asked)
                (insert (drover-items--review-line metrics) "\n"))
              (dolist (item items)
                (pcase-let ((`(,line . ,whole) (drover-items--line item metrics)))
                  (dolist (session (drover-items-heading-sessions item))
                    (push (drover-session-key session) keys))
                  (magit-insert-section (drover-items-item-section (plist-get item :id)
                                                                   (not (memq (plist-get item :id) shown)))
                    (drover-list--insert-heading line)
                    (drover-items--insert-details item metrics whole)))))))))
        ;; Magit hides the bodies of the sections made hidden once told to.
        (magit-section-show magit-root-section)
        (setq drover-items--drawn
              (list (drover-fetched-serial) keys
                    (seq-keep (lambda (item) (plist-get item :heading)) items)))))
    (let ((position (if-let* ((section (and here (drover-items--section here))))
                        (oref section start)
                      (if-let* ((first (car (drover-items--sections))))
                          (oref first start)
                        (point-min)))))
      (goto-char position)
      (dolist (window windows)
        (set-window-point window position)
        (set-window-start window (save-excursion
                                   (goto-char (point-min))
                                   (forward-line (alist-get window starts 0))
                                   (point))
                          t)))
    (when (eq (window-buffer) (current-buffer))
      (goto-char (window-point)))
    (magit-section-update-highlight t)
    (drover-list--set-highlight-windows)
    (set-buffer-modified-p nil)
    (when list
      (drover-items--seek list))))

(defun drover-items--shown-p ()
  "Return the buffer of the side list of items when a window shows it.
Nil otherwise."
  (when-let* ((buffer (drover-items--buffer))
              ((get-buffer-window buffer 'visible)))
    buffer))

(defun drover-items--refresh-views (key)
  "Draw the side list again when it shows the list of the conversation KEY.
On `drover-items-changed-hook'.  Hidden, it is drawn as it is shown again."
  (when-let* ((buffer (drover-items--buffer))
              ((equal (buffer-local-value 'drover-items--view-key buffer) key)))
    (with-current-buffer buffer
      (if (drover-items--shown-p)
          (drover-items--draw)
        (setq drover-items--stale t)))))

(add-hook 'drover-items-changed-hook #'drover-items--refresh-views)

(defun drover-items--on-change ()
  "Draw the side list again when the sessions it draws changed.
On `drover-changed-hook': the session whose list it shows, a session
serving the heading of one of its items, one that came to serve one,
or the archived sessions drover fetched.  Hidden, it draws as it is
shown again."
  (drover-with-reporting "items side list"
    (when-let* ((buffer (drover-items--buffer)))
      (with-current-buffer buffer
        (if (not (drover-items--shown-p))
            (setq drover-items--stale t)
          (pcase-let ((`(,serial ,keys ,headings) drover-items--drawn))
            (when (or (eq drover-changed-sessions t)
                      (not (eql serial (drover-fetched-serial)))
                      (seq-some (lambda (key)
                                  (or (member key keys)
                                      (when-let* ((headings)
                                                  (session (drover-index-session key)))
                                        (member (drover-task-id-of session) headings))))
                                drover-changed-sessions))
              (drover-items--draw))))))))

(defun drover-items--window-changed (window)
  "Draw the side list of WINDOW again when the room of its lines changed.
Or what it shows, while no window showed it.  On its
`window-size-change-functions' and `window-buffer-change-functions'."
  (drover-with-reporting "items side list"
    (when (buffer-live-p (window-buffer window))
      (with-current-buffer (window-buffer window)
        (when (derived-mode-p 'drover-items-mode)
          (drover-list--set-up-side-window window)
          (when (or drover-items--stale
                    (not (eql drover-items--width (drover-list--line-width))))
            (drover-items--draw)))))))

(defun drover-items--face-changed (&rest _)
  "Measure the texts of the side list anew, and draw it again.
Its faces or a theme changed, which its lines, laid out in pixels,
depend on, and the highlight of its row at point.  On its
`text-scale-mode-hook' and `buffer-face-mode-hook', and on
`enable-theme-functions' and `disable-theme-functions'."
  (drover-with-reporting "items side list"
    (when-let* ((buffer (drover-items--buffer)))
      (with-current-buffer buffer
        (setq drover-list--widths nil)
        (drover-list--remap-highlight)
        (if (drover-items--shown-p)
            (drover-items--draw)
          (setq drover-items--stale t))))))

(defun drover-items--revert (&rest _)
  "Read the headings of the items again, and draw the side list anew.
The value of `revert-buffer-function' in the side list."
  (clrhash drover-items--sought)
  (drover-items--draw))

;;;;; The session the user is on

(defun drover-items--session-on ()
  "Return the session the user is on, or nil when nothing tells one.
The session of the selected window's buffer: its own, or in a list of
sessions the one at point (see `drover-session-at-point-function');
else the session at point in the list of drover shown on the selected
frame, the most recently used of several, as the dispatch menu acts on
it from a buffer telling no session.  The side list of items itself
tells none: it keeps showing what it shows."
  (let ((buffer (window-buffer (selected-window))))
    (unless (eq buffer (drover-items--buffer))
      (or (with-current-buffer buffer
            (drover-session-at-point t))
          (when-let* ((window (drover--shown-list-window)))
            (with-selected-window window
              (drover-session-at-point t)))))))

(defun drover-items--show-session (session)
  "Have the side list, the current buffer, show the list of SESSION.
And draw it."
  (setq drover-items--view-session (drover-session-key session)
        drover-items--view-key (and (not (drover-session-host session)) (drover-items--key session))
        drover-items--view-title (drover-bound-line (format "%s" (drover-session-display-title session)) 200))
  (clrhash drover-items--sought)
  ;; Another list: what showed its details, and where point was, are not its own.
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq magit-root-section nil))
  (drover-items--draw))

(defun drover-items--follow ()
  "Have the side list show the list of the session the user is on.
When it shows in a window, and that session is another than the one it
shows (see `drover-items--session-on').  Not while a minibuffer is
active, whose windows are provisional, as a buffer previewed is."
  (when-let* ((buffer (drover-items--shown-p))
              ((zerop (minibuffer-depth)))
              (session (drover-items--session-on))
              ((not (equal (drover-session-key session)
                           (buffer-local-value 'drover-items--view-session buffer)))))
    (with-current-buffer buffer
      (drover-items--show-session session))))

(defvar drover-items--follow-timer nil
  "Timer having the side list follow the session the user is on, or nil.")

(defun drover-items--follow-now ()
  "Have the side list follow the session the user is on, from its timer."
  (drover-with-reporting "items side list"
    (setq drover-items--follow-timer nil)
    (drover-items--follow)))

(defun drover-items--follow-soon (&rest _)
  "Have the side list follow the session the user is on, once Emacs is idle.
On `post-command-hook', `window-selection-change-functions' and
`window-buffer-change-functions', while the side list lives: the
session is looked for once a burst of commands ended, and only while a
window shows the side list."
  (drover-with-reporting "items side list"
    (when (and (null drover-items--follow-timer) (drover-items--shown-p))
      (setq drover-items--follow-timer (run-with-idle-timer 0.1 nil #'drover-items--follow-now)))))

(defun drover-items--watch ()
  "Follow the user, the sessions and the themes while the side list lives."
  (add-hook 'post-command-hook #'drover-items--follow-soon)
  (add-hook 'window-selection-change-functions #'drover-items--follow-soon)
  (add-hook 'window-buffer-change-functions #'drover-items--follow-soon)
  (add-hook 'drover-changed-hook #'drover-items--on-change)
  (add-hook 'enable-theme-functions #'drover-items--face-changed)
  (add-hook 'disable-theme-functions #'drover-items--face-changed))

(defun drover-items--unwatch ()
  "Stop following the user, the sessions and the themes.
On the `kill-buffer-hook' and `change-major-mode-hook' of the side list."
  (drover-with-reporting "items side list"
    (remove-hook 'post-command-hook #'drover-items--follow-soon)
    (remove-hook 'window-selection-change-functions #'drover-items--follow-soon)
    (remove-hook 'window-buffer-change-functions #'drover-items--follow-soon)
    (remove-hook 'drover-changed-hook #'drover-items--on-change)
    (remove-hook 'enable-theme-functions #'drover-items--face-changed)
    (remove-hook 'disable-theme-functions #'drover-items--face-changed)
    (when (timerp drover-items--follow-timer)
      (cancel-timer drover-items--follow-timer))
    (setq drover-items--follow-timer nil)))

;;;; Commands of the side list

(defun drover-items--move (count)
  "Move COUNT items down the side list, up when negative."
  (let* ((sections (drover-items--sections))
         (current (when-let* ((here (drover-items--id-at-point))) (drover-items--section here)))
         (index (and current (seq-position sections current)))
         (target (cond ((null sections) nil)
                       ((null index) (if (> count 0) (car sections) (car (last sections))))
                       (t (nth (+ index count) sections)))))
    (if (and target (or (null index) (>= (+ index count) 0)))
        (goto-char (oref target start))
      (message (if (> count 0) "No next item" "No previous item")))))

(defun drover-items-next (&optional count)
  "Move to the next item, or COUNT items further."
  (interactive "p" drover-items-mode)
  (drover-items--move (or count 1)))

(defun drover-items-previous (&optional count)
  "Move to the previous item, or COUNT items back."
  (interactive "p" drover-items-mode)
  (drover-items--move (- (or count 1))))

(defun drover-items--set-goes (item goes)
  "Say ITEM goes to GOES, the user's choice, and keep it."
  (let* ((list (drover-items--list drover-items--view-key))
         (now (float-time)))
    (setf (plist-get list :items)
          (mapcar (lambda (other)
                    (if (eql (plist-get other :id) (plist-get item :id))
                        (plist-put (plist-put other :goes goes) :updated now)
                      other))
                  (plist-get list :items)))
    (setf (plist-get list :updated) now)
    (drover-items--keep)
    (drover-items--changed drover-items--view-key)))

(defun drover-items--turn (step)
  "Have the item at point go STEP places further among the destinations."
  (let* ((item (drover-items--item-at-point))
         (goes (drover-items-goes item)))
    (if (null goes)
        (message "Item %d is %s: it goes nowhere" (plist-get item :id) (drover-items-item-status item))
      (let* ((position (seq-position drover-items-destinations goes))
             (next (nth (mod (+ position step) (length drover-items-destinations)) drover-items-destinations)))
        (drover-items--set-goes item next)
        (message "Item %d goes %s" (plist-get item :id) (alist-get next drover-items--destination-labels))))))

(defun drover-items-goes-next ()
  "Have the item at point go to the next place.
Here, a subagent, a new session, a fork, later, then here again."
  (interactive nil drover-items-mode)
  (drover-items--turn 1))

(defun drover-items-goes-previous ()
  "Have the item at point go to the place before.
Later, a fork, a new session, a subagent, here, then later again."
  (interactive nil drover-items-mode)
  (drover-items--turn -1))

(defun drover-items--show-heading (marker)
  "Show the Org heading at MARKER where the user works, and select it.
From a side window, in the window where the user works, as sessions
opened from the sidebar show there."
  (let ((buffer (marker-buffer marker))
        (position (marker-position marker)))
    (set-marker marker nil)
    (when-let* ((window (drover-list--target-window)))
      (select-window window))
    (pop-to-buffer-same-window buffer)
    (goto-char position)
    (org-fold-show-context)))

(defun drover-items-follow ()
  "Show the heading of the item at point, else open its link.
Where the user works: from the side window, in the window beside it."
  (interactive nil drover-items-mode)
  (let ((item (drover-items--item-at-point)))
    (cond
     ((plist-get item :heading)
      (let ((id (plist-get item :heading)))
        (if-let* ((marker (drover-task-org-marker id)))
            (drover-items--show-heading marker)
          (drover-task-org-find id (lambda (marker)
                                     (drover-with-reporting "items side list"
                                       (if (null marker)
                                           (drover-message "No heading of ID %s is found" id)
                                         (drover-items--show-heading marker))))))))
     ((plist-get item :link)
      (when-let* ((window (drover-list--target-window)))
        (select-window window))
      (org-link-open-from-string (drover-items-link-text item)))
     (t (user-error "Item %d has no heading and no link" (plist-get item :id))))))

(defun drover-items-click (event)
  "Select the side list and go to the item of the row clicked.
The mouse drags no region there.  EVENT is the click."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (position (posn-point posn)))
    (when (and (window-live-p window) (null (posn-area posn)) (integerp position))
      (select-window window)
      (goto-char (or (drover-list--row-start position) position)))))

(defun drover-items-quit ()
  "Hide the side list of items; the choices made stay, applied or not."
  (interactive nil drover-items-mode)
  (if (window-parameter nil 'window-side)
      (drover-items--hide-side (selected-window))
    (quit-window)))

(defvar-keymap drover-items-mode-map
  :doc "Keys of the side list of the items of a session.
The mouse drags no region, and a double or triple click does no more
than the click it starts with, as in the sidebar."
  :parent magit-section-mode-map
  "n" #'drover-items-next
  "p" #'drover-items-previous
  "<down>" #'drover-items-next
  "<up>" #'drover-items-previous
  "<remap> <next-line>" #'drover-items-next
  "<remap> <previous-line>" #'drover-items-previous
  "<right>" #'drover-items-goes-next
  "<left>" #'drover-items-goes-previous
  "RET" #'drover-items-follow
  "C-c C-c" #'drover-items-apply
  "q" #'drover-items-quit
  "*" #'drover-items
  "g" #'revert-buffer
  "<mouse-1>" #'drover-items-click
  "<down-mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore)

(define-derived-mode drover-items-mode magit-section-mode "Drover items"
  "Major mode of the side list of the items of a session.
\\<drover-items-mode-map>
It shows the list of the session you are on, in the look of the
sidebar: the session, then an item a line.  Each line begins with the
icon of where the item goes (see `drover-items-destination-icons'),
then the icon of its status (`drover-items-status-icons'), its number
and its title; at its end, the glyphs of the sessions serving its
heading, as lists draw their statuses.
\\[drover-items-next] and \\[drover-items-previous] move between items.
\\[drover-items-goes-next] and \\[drover-items-goes-previous] change where the item at point goes:
it stays here, runs in a subagent of the session, goes to a new
session, to a fork of the session, or is saved for later; one done or
decided goes nowhere.  The icon of where it goes shows highlighted
while the session is still to be told, which \\[drover-items-apply] does,
asking where the heading of each item leaving goes.
\\[magit-section-toggle] shows the details of an item under its line,
\\[drover-items-follow] its heading, else opens its link, \\[revert-buffer] reads the
headings again, and \\[drover-items-quit] hides the side list, the choices kept.

\\{drover-items-mode-map}"
  :group 'drover
  :interactive nil
  (setq-local drover-items--sought (make-hash-table :test #'equal))
  (setq-local revert-buffer-function #'drover-items--revert)
  (setq-local drover-session-at-point-function #'drover-items--session-at-point)
  (setq-local drover-main-window-function #'drover-list--target-window)
  (setq-local magit-section-cache-visibility nil)
  (drover-list-sidebar-look)
  (add-hook 'window-size-change-functions #'drover-items--window-changed nil t)
  (add-hook 'window-buffer-change-functions #'drover-items--window-changed nil t)
  (add-hook 'text-scale-mode-hook #'drover-items--face-changed nil t)
  (add-hook 'buffer-face-mode-hook #'drover-items--face-changed nil t)
  (add-hook 'kill-buffer-hook #'drover-items--unwatch nil t)
  (add-hook 'change-major-mode-hook #'drover-items--unwatch nil t)
  (drover-items--watch))

;;;; Showing and hiding

(defun drover-items--show-side ()
  "Show the side list of items in a side window of the selected frame.
On the side opposite to the sidebar's, with the sidebar's width, slot
and settings (see `drover-list--display-alist'), for the session the
user is on; return the window.  An ordinary window of the frame showing
its buffer stops showing it, so that it is laid out for the side
window's width."
  (drover-start)
  (let ((buffer (drover-items--buffer t))
        (session (drover-items--session-on)))
    (dolist (window (get-buffer-window-list buffer nil (selected-frame)))
      (unless (window-parameter window 'window-side)
        (quit-window nil window)))
    (let ((window (or (drover-items--side-window)
                      (display-buffer-in-side-window
                       buffer (drover-list--display-alist (list :side (drover-items--side))))
                      (user-error "No room for the list of items"))))
      (drover-list--set-up-side-window window)
      (with-current-buffer buffer
        (if (and session (not (equal (drover-session-key session) drover-items--view-session)))
            (drover-items--show-session session)
          (drover-items--draw)))
      window)))

(defun drover-items--hide-side (window)
  "Delete WINDOW, the side window of the side list of items."
  (delete-window window))

;;;###autoload
(defun drover-items (&optional select)
  "Toggle the side list of the items of the session you are on.
Show it, without selecting it, when the selected frame lacks it, and
hide it otherwise, as `drover-list-side' does the sidebar.  With prefix
argument SELECT, select it instead, showing it first if needed; in the
side list, select the window where you work instead, so that the same
key moves back and forth.

It shows in a side window on the side opposite to the sidebar's, in the
sidebar's look, the list its agent gave drover of the session you are
on (see `drover-anvil'): the session of the selected window, its own or
the one at point in a list of sessions, else the session at point in
the list of drover shown on the frame.  It follows as that changes.
See `drover-items-mode' for what it shows and its keys.  A session
without a list has one line saying so, and so has a session of another
host, which keeps its list."
  (interactive "P")
  (let ((window (drover-items--side-window)))
    (cond (select
           (if (and window (eq window (selected-window)))
               (when-let* ((main (drover-list--main-window)))
                 (select-window main))
             (select-window (drover-items--show-side))))
          (window (drover-items--hide-side window))
          (t (drover-items--show-side)))))

;;;; Applying

(defun drover-items--to-send-p (item)
  "Return non-nil when the session is to be told where ITEM goes.
An item leaving it, or going to a subagent of it, unless the session was
told so already, or saved with a heading, which leaves nothing to do."
  (let ((goes (drover-items-goes item)))
    (and (memq goes '(subagent session fork save))
         (not (eq (car (plist-get item :sent)) goes))
         (not (and (eq goes 'save) (plist-get item :heading))))))

(defun drover-items--needs-place-p (item)
  "Return non-nil when the place of the heading of ITEM is to be asked.
An item the session is to be told of, that gets a heading, and has none
yet: one going to a subagent gets none."
  (and (drover-items--to-send-p item)
       (not (eq (drover-items-goes item) 'subagent))
       (not (plist-get item :heading))))

(defun drover-items--place-text (place)
  "Return where a heading goes, PLACE, as the prompt's items tell it."
  (format "under `%s` (%s)" (plist-get place :uri)
          (if (plist-get place :id)
              (format "the heading \"%s\"" (plist-get place :name))
            (format "the top of %s" (abbreviate-file-name (plist-get place :file))))))

(defun drover-items--prompt-item (item)
  "Return the lines of ITEM in the prompt telling where items go."
  (let* ((goes (drover-items-goes item))
         (heading (plist-get item :heading))
         (heading-link (and heading (format "[[id:%s][%s]]" heading
                                            (or (plist-get item :heading-title) (plist-get item :title)))))
         (its-heading (lambda ()
                        (if heading
                            (format "Its heading is %s, written already." heading-link)
                          (format "Its heading goes %s." (drover-items--place-text (plist-get item :place))))))
         (where (pcase goes
                  ('subagent "to a subagent of this session.")
                  ('session (concat "to a new session.  " (funcall its-heading)))
                  ('fork (concat "to a fork of this session.  " (funcall its-heading)))
                  ('save (concat "saved for later.  " (funcall its-heading))))))
    (concat (format "- Item %d, \"%s\" (%s): %s" (plist-get item :id) (plist-get item :title)
                    (drover-items-item-status item) where)
            (if-let* ((link (drover-items-link-text item))) (concat "\n  Link: " link) "")
            (if-let* ((next (plist-get item :next))) (concat "\n  Next step: " next) "")
            (if-let* ((note (plist-get item :note)))
                (concat "\n  Note: " (string-replace "\n" "\n  " note))
              ""))))

(defun drover-items-prompt-text (items session)
  "Return the prompt telling SESSION where ITEMS go.
Made from `drover-items-prompt'."
  (format-spec drover-items-prompt
               `((?i . ,(mapconcat #'drover-items--prompt-item items "\n"))
                 (?k . ,(drover-handoff-session-key session)))))

(defun drover-items--put (key number property value)
  "Set PROPERTY of the item NUMBER of the list of KEY to VALUE, and keep it.
KEY is the key of the list's conversation."
  (let ((list (drover-items--list key)))
    (setf (plist-get list :items)
          (mapcar (lambda (item)
                    (if (eql (plist-get item :id) number) (plist-put item property value) item))
                  (plist-get list :items)))
    (drover-items--keep)))

(defun drover-items-apply ()
  "Apply where the items of the list the side list shows go.
Where the heading of each item leaving the session goes, with no
heading yet, is asked first, as Org's refile asks a place, the place
chosen last offered (see `drover-handoff-read-place'); an item going to
a subagent gets no heading, and no place is asked for it.  Then one
prompt tells the session where each of those items goes and where its
heading goes (see `drover-items-prompt'), as the prompt of a hand-off
goes: out of every window, never over a draft, and once a running turn
ended.  The items it told of are marked sent, and the list no longer
waits for a review.  With no item to tell of, nothing is sent: the
review is over.  Drover writes no heading."
  (interactive nil drover-items-mode)
  (let* ((key (or drover-items--view-key (user-error "No list of items here")))
         (session (or (drover-items--view-record) (user-error "The session of this list is gone")))
         (title (drover-bound-line (format "%s" (drover-session-display-title session)) 80))
         (list (or (drover-items--list key) (user-error "%s has no list of items" title))))
    ;; Refused before any place is asked: a session no prompt reaches.
    (unless (drover-supports-p session :submit)
      (user-error "%s takes no prompt from drover: its harness, %s, cannot be told where its items go"
                  title (drover-backend-label (drover-session-backend session))))
    (drover-schedule-session-target session)
    (drover-items--refresh list)
    (dolist (item (seq-filter #'drover-items--needs-place-p (plist-get list :items)))
      (drover-items--put key (plist-get item :id) :place
                         (drover-handoff-read-place
                          (format "Item %d, %s, its heading goes under" (plist-get item :id)
                                  (drover-bound-line (plist-get item :title) 50))
                          session)))
    (let* ((list (drover-items--list key))
           (now (float-time))
           (sending (seq-filter #'drover-items--to-send-p (plist-get list :items)))
           (waits (and sending
                       (drover-handoff-send session (drover-items-prompt-text sending session)
                                            drover-items-origin))))
      (setf (plist-get list :items)
            (mapcar (lambda (item)
                      (if (memq item sending)
                          (plist-put item :sent (cons (drover-items-goes item) now))
                        item))
                    (plist-get list :items)))
      (setf (plist-get list :asked) nil
            (plist-get list :updated) now)
      (drover-items--keep)
      (drover-items--changed key)
      (if sending
          (drover-message "%s is told where %d item%s go%s%s" title (length sending)
                          (if (cdr sending) "s" "") (if (cdr sending) "" "es")
                          (if waits ", once its turn ends" ""))
        (drover-message "No item of %s is to be told of: nothing sent" title)))))

;;;; Restoring

(defun drover-items-restore ()
  "Read the lists, as drover first starts, and have lists tell their reviews.
The rows of the sessions whose lists wait for the user's review are
drawn again: the backends listed them before the lists were read (see
`drover--kept-modules')."
  (let ((keys nil))
    (maphash (lambda (key list)
               (when (plist-get list :asked)
                 (setq keys (append (drover-items--session-keys key) keys))))
             (drover-items--table-lists (drover-items-load)))
    (drover-views-changed keys)))

(setq drover-list-review-function #'drover-items-review-since)

;; Loaded over the drover that showed each list in a buffer of its own,
;; those buffers go: the side list shows their lists.
(dolist (buffer (buffer-list))
  (when (and (eq (buffer-local-value 'major-mode buffer) 'drover-items-mode)
             (not (equal (buffer-name buffer) drover-items-buffer-name)))
    (kill-buffer buffer)))

(defun drover-items-unload-function ()
  "Stop telling lists of reviews, and following the user.
The lists stay in their file."
  (when (eq drover-list-review-function #'drover-items-review-since)
    (setq drover-list-review-function nil))
  (remove-hook 'drover-items-changed-hook #'drover-items--refresh-views)
  (drover-items--unwatch)
  nil)

(provide 'drover-items)
;;; drover-items.el ends here
