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
;; it to go: kept in the session, given to a new session, to a fork of
;; the session, or saved for later.  An item with a heading takes its
;; status from the heading (see `drover-task-org-status'), and tells the
;; sessions serving it.
;;
;; `drover-items', * on a session, shows its list, an item a line, in
;; the look of drover's lists.  Left and right change where the item at
;; point goes; C-c C-c applies the choices: where the heading of each
;; item leaving the session goes is asked, as Org's refile asks a place
;; (`drover-handoff-read-place'), then one prompt tells the session's
;; agent where each item goes (`drover-items-prompt'), as the prompt of
;; a hand-off goes (`drover-handoff-send').  The agent writes the
;; headings and starts the sessions: drover writes no heading.
;;
;; An agent asking the user to review its list has its session need the
;; user, as a session asking a question does, once its turn ends (see
;; `drover-ask-function'), until the choices are applied: A shows the
;; list.  The lists of the sessions of other hosts are their hosts' to
;; keep, and are neither kept nor shown here.

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
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 32))
  (load "drover-core" nil t))

;;;; Options

(defcustom drover-items-prompt
  "I went through the items of this session's list, and said where each goes:

%i

Load your `handoff` skill if you have one, and follow it.  Without that skill, do these steps for each item that leaves this session:

1. Write its Org heading with `org-add-todo`, with an Org ID, under the place given, its `parent_uri`.  Write its body as a brief that reads alone, for a reader who saw nothing of this conversation: the goal, what is known, the decisions made, the limits, when it is done, and links.  An item whose heading is written already keeps it.
2. For a new session, call `drover-task-start` with the heading's ID.  For a fork of this session, call `drover-session-fork` with the heading's ID as `task`, and a `prompt` that names the item and links the heading: a fork starts from this whole conversation, so only that prompt tells it which item is its own.  For an item saved for later, start nothing.
3. Record the heading on its item: call `drover-session-items-set` with the item's `id`, `title` and `status`, its `heading`, and `review` false.

The items that stay are yours to go on with.  Reply with the link of each heading, `[[id:ID][title]]`, and of each new session."
  "The prompt telling a session where the items of its list go.
A `format-spec' template: %i is the items, a line each, telling where
it goes and where its heading goes, as Anvil's org-add-todo takes a
place (see `drover-items-apply'); %% is a percent sign."
  :type 'string
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

(defconst drover-items-destinations '(keep session fork save)
  "Where an item may go: it stays, a new session, a fork, saved for later.")

(defconst drover-items--destination-labels
  '((keep . "here") (session . "new session") (fork . "fork") (save . "later"))
  "How the view names where an item goes.")

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
;; ID; :heading-title, the heading's as read last; :proposal, where the
;; agent proposes it goes; :goes, where the user said it goes; :place,
;; where its heading goes, a place of `drover-handoff-read-place'; :sent,
;; (GOES . TIME) once the session was told; :created, :updated.

(cl-defstruct (drover-items--table (:constructor drover-items--table-make)
                                   (:copier nil))
  "The lists of items of the sessions of this host."
  (stamp (drover-layout 'drover-items--table)
         :documentation "The layout it was made of, see `drover-layout'.")
  (lists (make-hash-table :test #'equal)
         :documentation "The lists, a plist each, by the key of their conversation.")
  (asks (make-hash-table :test #'equal)
        :documentation "What is asked of each conversation whose list the user is to review, by key.")
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
       (memq (plist-get item :proposal) (cons nil drover-items-destinations))
       (memq (plist-get item :goes) (cons nil drover-items-destinations))
       (drover-items--place-p (plist-get item :place))
       (let ((sent (plist-get item :sent)))
         (or (null sent) (and (consp sent) (memq (car sent) drover-items-destinations) (numberp (cdr sent)))))))

(defun drover-items--bound (item)
  "Return ITEM with its texts bounded, as drover keeps them."
  (cl-flet ((line (value limit) (and (stringp value) (drover-bound-line value limit))))
    (let ((link (plist-get item :link)))
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
                            (drover-error-message err))))
      (maphash (lambda (key _list) (drover-items--ask-update key t)) (drover-items--table-lists table)))
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
        (remhash (plist-get gone :key) (drover-items--table-lists table))
        (remhash (plist-get gone :key) (drover-items--table-asks table)))
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
The user's choice, else the agent's proposal, else it stays (`keep');
see `drover-items-destinations'."
  (unless (drover-items-closed-p item)
    (or (plist-get item :goes) (plist-get item :proposal) 'keep)))

(defun drover-items-chosen-by (item)
  "Return who chose where ITEM goes: `user', `agent', or `default'."
  (cond ((plist-get item :goes) 'user)
        ((plist-get item :proposal) 'agent)
        (t 'default)))

(defun drover-items-heading-sessions (item)
  "Return the live sessions serving the heading of ITEM, most recent first."
  (when-let* ((info (drover-items-heading-info item)))
    (drover-task-sessions info)))

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
                     (drover-items--keep)
                     (drover-items--ask-update (plist-get list :key)))
                   (funcall callback))))
    (if (null ids)
        (funcall finish)
      (dolist (id ids)
        (drover-task-org-find id (lambda (marker)
                                   (when marker
                                     (set-marker marker nil))
                                   (when (zerop (cl-decf left))
                                     (funcall finish))))))))

;;;; What is asked of the user

(defun drover-items--ask-of (list)
  "Return what is asked of the user for LIST, a plist, or nil when nothing."
  (when-let* ((asked (plist-get list :asked)))
    (let ((open (seq-count (lambda (item) (not (memq (plist-get item :status) drover-items-closed-statuses)))
                           (plist-get list :items))))
      (list :kind 'items
            :summary (pcase open
                       (0 "to review its list")
                       (1 "where its item goes")
                       (_ (format "where its %d items go" open)))
            :since asked))))

(defun drover-items--session-keys (key)
  "Return the keys of the live sessions showing the conversation KEY."
  (or (copy-sequence (gethash key (drover--index-engines (drover-index))))
      (and (drover-index-session key) (list key))))

(defun drover-items--ask-update (key &optional quiet)
  "Bring what is asked of the user for the conversation KEY up to date.
The sessions showing it are taken in again when it changed, unless
QUIET."
  (let* ((table (drover-items--state))
         (asks (drover-items--table-asks table))
         (old (gethash key asks))
         (new (drover-items--ask-of (gethash key (drover-items--table-lists table)))))
    (unless (equal old new)
      (if new (puthash key new asks) (remhash key asks))
      (unless quiet
        (when-let* ((keys (drover-items--session-keys key)))
          (drover-take-in-again keys)
          (drover-views-changed keys))))))

(defun drover-items-ask (record)
  "Return what drover asks the user of RECORD's session, or nil.
The value of `drover-ask-function': a review of its list, once its
agent asked for one, until the choices are applied.  A lookup, and the
same plist while the question stands."
  (when-let* ((table drover-items--table)
              ((drover-layout-current-p table 'drover-items--table))
              (asks (drover-items--table-asks table))
              ((> (hash-table-count asks) 0)))
    (gethash (drover--conversation-key record) asks)))

(defun drover-items--changed (key)
  "Note that the list of the conversation KEY changed: tell views and sessions."
  (drover-items--ask-update key)
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
;; received, :title, :status, :link, :next, :note, :heading, :proposal
;; and :id, an empty string for an optional field clearing it; :unknown,
;; the names of fields that are none; :malformed, non-nil for an item
;; that is no object.

(defconst drover-items-fields '(:title :status :link :next :note :heading :proposal :id)
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
      (pcase (text :proposal)
        ('absent nil)
        ('nil (setq item (plist-put item :proposal nil)))
        ('malformed (problem "has a proposal that is no text"))
        (proposal (if-let* ((symbol (seq-find (lambda (known) (equal (symbol-name known) (downcase proposal)))
                                              drover-items-destinations)))
                      (setq item (plist-put item :proposal symbol))
                    (problem "proposes %s: one of %s" (drover-bound-line proposal 40)
                             (mapconcat #'symbol-name drover-items-destinations ", ")))))
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
  (dolist (key '(:title :status :link :next :note :heading :proposal))
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
SESSION needs the user once its turn ends (see `drover-ask-function').
The file is written before this returns; signal an error, keeping
nothing, when it cannot be."
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

;;;; The view

(defclass drover-items-item-section (magit-section) ()
  "The section of an item in the view of a list.")

(defvar-local drover-items--view-key nil
  "The key of the conversation whose list the view shows.")

(defvar-local drover-items--view-session nil
  "The key of the session the view shows the list of.")

(defvar-local drover-items--view-title nil
  "The title of the session the view shows the list of.")

(defvar-local drover-items--sought nil
  "IDs of headings the view looked for with ripgrep, a hash table.")

(defvar-local drover-items--width nil
  "The width in pixels of the window the view was drawn for last.")

(defconst drover-items--columns
  '((goes "Goes") (number "#") (status "Status") (title "Title") (link "Link") (sessions "Sessions")
    (next "Next step"))
  "The columns of the view, in order, with the names its header line gives.")

(defun drover-items--buffer (key &optional title)
  "Return the view of the list of the conversation KEY, or nil.
With TITLE, one is made when missing, named after it."
  (or (seq-find (lambda (buffer)
                  (and (eq (buffer-local-value 'major-mode buffer) 'drover-items-mode)
                       (equal (buffer-local-value 'drover-items--view-key buffer) key)))
                (buffer-list))
      (when title
        (with-current-buffer (generate-new-buffer (format "*drover items: %s*" (drover-bound-line title 60)))
          (drover-items-mode)
          (setq drover-items--view-key key)
          (current-buffer)))))

(defun drover-items--measurer (window)
  "Return a function measuring a string in pixels as the view in WINDOW draws it."
  (let ((remapping face-remapping-alist))
    (lambda (string) (drover-text-pixel-width string remapping window))))

(defun drover-items--layout (items window measure)
  "Return where the columns of ITEMS go in WINDOW, as MEASURE measures text.
An alist (FIELD X WIDTH), in pixels: the goes, number, status and
sessions columns as wide as what they show, the title, link and next
step sharing the rest."
  (let* ((gap (funcall measure "   "))
         (widest (lambda (texts) (apply #'max 0 (mapcar measure texts))))
         (last (apply #'max 1 (mapcar (lambda (item) (plist-get item :id)) items)))
         (widths (list (cons 'goes (funcall widest (cons "Goes" (mapcar (lambda (entry) (concat (cdr entry) " ✓"))
                                                                        drover-items--destination-labels))))
                       (cons 'number (funcall widest (list "#" (format "%d" last))))
                       (cons 'status (funcall widest (cons "Status" (mapcar #'symbol-name drover-items-statuses))))
                       (cons 'sessions (funcall widest (list "Sessions"
                                                             (concat (opencode-session-status-glyph 'working) " 99"))))))
         (total (max 1 (- (window-body-width window t) (funcall measure "  "))))
         (fixed (+ (alist-get 'goes widths) (alist-get 'number widths) (alist-get 'status widths)
                   (alist-get 'sessions widths) (* 6 gap)))
         (rest (max (* 3 (funcall measure "xxxxxx")) (- total fixed)))
         (title (floor (* rest 0.4)))
         (link (floor (* rest 0.25))))
    (push (cons 'title title) widths)
    (push (cons 'link link) widths)
    (push (cons 'next (- rest title link)) widths)
    (let ((x 0)
          (columns nil))
      (pcase-dolist (`(,field ,_) drover-items--columns)
        (let ((width (alist-get field widths)))
          (push (list field x width) columns)
          (setq x (+ x width gap))))
      (nreverse columns))))

(defun drover-items--status-text (status)
  "Return STATUS, an item's, in its face."
  (drover-face (symbol-name status)
               (pcase status
                 ('blocked 'drover-items-blocked)
                 ('scheduled 'drover-items-scheduled)
                 ('done 'drover-items-done)
                 ('decided 'drover-items-decided)
                 (_ 'default))))

(defun drover-items--goes-text (item)
  "Return where ITEM goes, as the view shows it: chosen highlighted, else dimmed."
  (if-let* ((goes (drover-items-goes item)))
      (concat (drover-face (alist-get goes drover-items--destination-labels)
                           (if (eq (drover-items-chosen-by item) 'user) 'opencode-setting-chosen 'opencode-setting-inherited))
              (if (eq (car (plist-get item :sent)) goes) (drover-face " ✓" 'drover-dim) ""))
    (drover-face "—" 'drover-dim)))

(defun drover-items--link-label (item)
  "Return the link of ITEM as the view's line shows it, or nil."
  (when-let* ((link (plist-get item :link)))
    (or (cdr link) (car link))))

(defun drover-items--sessions-text (sessions)
  "Return SESSIONS, those serving an item's heading, as the view's line shows them."
  (when sessions
    (let ((first (car (drover-sort sessions))))
      (concat (opencode-session-status-glyph (drover-session-shown-status first))
              (if (cdr sessions) (format " %d" (length sessions)) "")))))

(defun drover-items--row (item columns measure)
  "Return the line of ITEM in COLUMNS, as MEASURE measures text.
COLUMNS is an alist (FIELD X WIDTH), in pixels."
  (let ((cells `((goes . ,(drover-items--goes-text item))
                 (number . ,(drover-face (format "%d" (plist-get item :id)) 'drover-dim))
                 (status . ,(drover-items--status-text (drover-items-item-status item)))
                 (title . ,(plist-get item :title))
                 (link . ,(drover-items--link-label item))
                 (sessions . ,(drover-items--sessions-text (drover-items-heading-sessions item)))
                 (next . ,(plist-get item :next))))
        (parts nil))
    (pcase-dolist (`(,field ,x ,width) columns)
      (let ((text (or (alist-get field cells) "")))
        (unless (= x 0)
          (push (propertize " " 'display `(space :align-to (,x))) parts))
        (push (drover-truncate-to-pixels (replace-regexp-in-string "[\n\r\t]+" " " text) width measure) parts)))
    (apply #'concat (nreverse parts))))

(defun drover-items--field (label text)
  "Insert the line of the field LABEL of an item's details, telling TEXT.
Nothing when TEXT is nil or empty."
  (when (and text (not (string-empty-p text)))
    (let ((lines (split-string text "\n")))
      (insert "    " (drover-face (format "%-9s" label) 'drover-dim) " " (car lines) "\n")
      (dolist (line (cdr lines))
        (insert "              " line "\n")))))

(defun drover-items--insert-details (item)
  "Insert the details of ITEM, what its line shows cut or not at all."
  (let ((info (drover-items-heading-info item)))
    (drover-items--field "Title" (plist-get item :title))
    (drover-items--field "Note" (plist-get item :note))
    (drover-items--field "Next" (plist-get item :next))
    (drover-items--field "Link" (drover-items-link-text item))
    (drover-items--field "Heading" (when-let* ((id (plist-get item :heading)))
                                     (if info
                                         (format "%s (%s)" (plist-get info :title) id)
                                       (concat (or (plist-get item :heading-title) id)
                                               (drover-face " — not found" 'drover-dim)))))
    (dolist (session (drover-items-heading-sessions item))
      (drover-items--field "Session" (concat (opencode-session-status-glyph (drover-session-shown-status session)) " "
                                             (format "%s" (drover-session-display-title session)))))
    (drover-items--field "Proposed" (and (plist-get item :proposal)
                                         (alist-get (plist-get item :proposal) drover-items--destination-labels)))
    (drover-items--field "Place" (when-let* ((place (plist-get item :place)))
                                   (format "%s (%s)" (plist-get place :name) (plist-get place :uri))))
    (drover-items--field "Sent" (when-let* ((sent (plist-get item :sent)))
                                  (format "to %s, %s ago" (alist-get (car sent) drover-items--destination-labels)
                                          (opencode-format-age (cdr sent)))))))

(defun drover-items--header-line (columns)
  "Return the header line naming COLUMNS, an alist (FIELD X WIDTH)."
  (let ((shift (if display-line-numbers (line-number-display-width t) 0))
        (parts nil))
    (pcase-dolist (`(,field ,x ,_) columns)
      (push (propertize " " 'display `(space :align-to (,(+ shift x)))) parts)
      (push (propertize (string-replace "%" "%%" (cadr (assq field drover-items--columns))) 'face 'drover-list-header)
            parts))
    (apply #'concat (nreverse parts))))

(defun drover-items--seek (list)
  "Look for the headings of the items of LIST found nowhere yet, once a view.
Each found has the view drawn again."
  (let ((buffer (current-buffer)))
    (dolist (item (plist-get list :items))
      (when-let* ((id (plist-get item :heading))
                  ((not (gethash id drover-items--sought)))
                  ((null (drover-items-heading-info item))))
        (puthash id t drover-items--sought)
        (drover-task-org-find id (lambda (marker)
                                   (when marker
                                     (set-marker marker nil)
                                     (when (buffer-live-p buffer)
                                       (with-current-buffer buffer
                                         (drover-items--draw))))))))))

(defun drover-items--draw ()
  "Draw the view anew, point kept on the item it was on."
  (let* ((inhibit-read-only t)
         (list (drover-items--list drover-items--view-key))
         (items (plist-get list :items))
         (window (or (get-buffer-window (current-buffer) t) (selected-window)))
         (measure (drover-items--measurer window))
         (here (drover-items--id-at-point))
         (offset (when-let* ((section (magit-current-section))) (- (point) (oref section start))))
         (shown (and magit-root-section
                     (seq-keep (lambda (section)
                                 (and (cl-typep section 'drover-items-item-section)
                                      (not (oref section hidden))
                                      (oref section value)))
                               (oref magit-root-section children))))
         (columns (drover-items--layout items window measure)))
    (when (and list (drover-items--refresh list))
      (drover-items--keep))
    (setq drover-items--width (window-body-width window t))
    (erase-buffer)
    (setq magit-root-section nil)
    (magit-insert-section (drover-items-root)
      (insert (drover-face (format "Items of %s" (or drover-items--view-title (plist-get list :title) "a session"))
                           'drover-heading)
              (if-let* ((asked (plist-get list :asked)))
                  (drover-face (format "   asks you since %s" (opencode-format-age asked)) 'opencode-session-status-waiting)
                "")
              "\n"
              (drover-face (substitute-command-keys
                            "\\<drover-items-mode-map>\\[drover-items-goes-previous] \\[drover-items-goes-next] where it goes   \\[drover-items-apply] apply   \\[drover-items-leave] leave, your choices kept   \\[drover-items-follow] open its link   \\[magit-section-toggle] details")
                           'drover-dim)
              "\n\n")
      (if (null items)
          (insert "No items.\n")
        (dolist (item items)
          (magit-insert-section (drover-items-item-section (plist-get item :id) t)
            (magit-insert-heading (concat (drover-items--row item columns measure) "\n"))
            (drover-items--insert-details item)))))
    (setq header-line-format (drover-items--header-line columns))
    (dolist (section (oref magit-root-section children))
      (when (and (cl-typep section 'drover-items-item-section) (memq (oref section value) shown))
        (magit-section-show section)))
    (if-let* ((section (and here (drover-items--section here))))
        (goto-char (min (+ (oref section start) (or offset 0)) (1- (oref section end))))
      (if-let* ((first (car (drover-items--sections))))
          (goto-char (oref first start))
        (goto-char (point-min))))
    (set-buffer-modified-p nil)
    (when list
      (drover-items--seek list))))

(defun drover-items--sections ()
  "Return the sections of the view's items, in order."
  (and magit-root-section
       (seq-filter (lambda (section) (cl-typep section 'drover-items-item-section))
                   (oref magit-root-section children))))

(defun drover-items--section (number)
  "Return the section of the item NUMBER in the view, or nil."
  (seq-find (lambda (section) (eql (oref section value) number)) (drover-items--sections)))

(defun drover-items--id-at-point ()
  "Return the number of the item at point in the view, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (cl-typep section 'drover-items-item-section)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun drover-items--item-at-point ()
  "Return the item at point in the view, or signal."
  (let ((number (or (drover-items--id-at-point) (user-error "No item at point"))))
    (or (drover-items--find (drover-items--list drover-items--view-key) number)
        (user-error "No item %d" number))))

(defun drover-items--refresh-views (key)
  "Draw the views of the list of the conversation KEY again.
On `drover-items-changed-hook'."
  (when-let* ((buffer (drover-items--buffer key)))
    (with-current-buffer buffer
      (drover-items--draw))))

(add-hook 'drover-items-changed-hook #'drover-items--refresh-views)

(defun drover-items--window-changed (window)
  "Draw the view of WINDOW again when its width changed.
On `window-size-change-functions'."
  (drover-with-reporting "items view"
    (with-current-buffer (window-buffer window)
      (unless (eql drover-items--width (window-body-width window t))
        (drover-items--draw)))))

(defun drover-items--revert (&rest _)
  "Read the headings of the items again, and draw the view anew.
The value of `revert-buffer-function' in the view."
  (clrhash drover-items--sought)
  (drover-items--draw))

(defun drover-items--view-record ()
  "Return the session the view shows the list of, or nil when it is gone."
  (or (and drover-items--view-session (drover-index-session drover-items--view-session))
      (when-let* ((keys (drover-items--session-keys drover-items--view-key)))
        (drover-index-session (car keys)))))

(defun drover-items--session-at-point ()
  "Return the session the view shows the list of, or nil.
The value of `drover-session-at-point-function' in the view."
  (drover-items--view-record))

;;;; Commands of the view

(defun drover-items--move (forward)
  "Move to the next item, or with FORWARD nil, the previous one."
  (let* ((sections (drover-items--sections))
         (here (drover-items--id-at-point))
         (current (and here (drover-items--section here)))
         (target (cond
                  ((null sections) nil)
                  ((null current) (if forward (car sections) (car (last sections))))
                  (forward (cadr (memq current sections)))
                  ((> (point) (oref current start)) current)
                  (t (cadr (memq current (reverse sections)))))))
    (if target
        (goto-char (oref target start))
      (message (if forward "No next item" "No previous item")))))

(defun drover-items-next ()
  "Move to the next item."
  (interactive nil drover-items-mode)
  (drover-items--move t))

(defun drover-items-previous ()
  "Move to the previous item."
  (interactive nil drover-items-mode)
  (drover-items--move nil))

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
        (drover-items--set-goes item next)))))

(defun drover-items-goes-next ()
  "Have the item at point go to the next place: here, a new session, a fork, later."
  (interactive nil drover-items-mode)
  (drover-items--turn 1))

(defun drover-items-goes-previous ()
  "Have the item at point go to the place before: later, fork, new session, here."
  (interactive nil drover-items-mode)
  (drover-items--turn -1))

(defun drover-items-follow ()
  "Show the heading of the item at point, else open its link."
  (interactive nil drover-items-mode)
  (let ((item (drover-items--item-at-point)))
    (cond
     ((plist-get item :heading)
      (let ((id (plist-get item :heading)))
        (if-let* ((marker (drover-task-org-marker id)))
            (progn (pop-to-buffer (marker-buffer marker))
                   (goto-char marker)
                   (org-fold-show-context)
                   (set-marker marker nil))
          (drover-task-org-find id (lambda (marker)
                                     (if (null marker)
                                         (drover-message "No heading of ID %s is found" id)
                                       (pop-to-buffer (marker-buffer marker))
                                       (goto-char marker)
                                       (set-marker marker nil)))))))
     ((plist-get item :link)
      (org-link-open-from-string (drover-items-link-text item)))
     (t (user-error "Item %d has no heading and no link" (plist-get item :id))))))

(defun drover-items-leave ()
  "Leave the view: the choices made stay, applied or not."
  (interactive nil drover-items-mode)
  (quit-window))

(defvar-keymap drover-items-mode-map
  :doc "Keys of the view of a session's list of items."
  :parent magit-section-mode-map
  "n" #'drover-items-next
  "p" #'drover-items-previous
  "<down>" #'drover-items-next
  "<up>" #'drover-items-previous
  "<right>" #'drover-items-goes-next
  "<left>" #'drover-items-goes-previous
  "RET" #'drover-items-follow
  "C-c C-c" #'drover-items-apply
  "q" #'drover-items-leave
  "g" #'revert-buffer)

(define-derived-mode drover-items-mode magit-section-mode "Drover items"
  "Major mode of the view of the list of items of a session.
\\<drover-items-mode-map>
An item a line: where it goes, its number, its status, its title, its
link, the sessions serving its heading and its next step.
\\[drover-items-next] and \\[drover-items-previous] move between items,
\\[drover-items-goes-next] and \\[drover-items-goes-previous] change where the item at point goes: it stays
here, goes to a new session, to a fork of this session, or is saved
for later; one done or decided goes nowhere.  Where it goes shows
highlighted once you chose it, dimmed while it is the agent's proposal.
\\[drover-items-apply] applies the choices, asking where the heading of
each item leaving goes; \\[drover-items-leave] leaves, the choices kept for later.
\\[drover-items-follow] shows the item's heading, else opens its link;
\\[magit-section-toggle] shows its details; \\[revert-buffer] reads the headings again.

\\{drover-items-mode-map}"
  :group 'drover
  :interactive nil
  (setq-local drover-items--sought (make-hash-table :test #'equal))
  (setq-local revert-buffer-function #'drover-items--revert)
  (setq-local drover-session-at-point-function #'drover-items--session-at-point)
  (setq-local truncate-lines t)
  (setq-local magit-section-cache-visibility nil)
  (drover-remap-default-face 'drover-list)
  (add-hook 'window-size-change-functions #'drover-items--window-changed nil t))

;;;###autoload
(defun drover-items (session)
  "Show the list of items of SESSION, and say where each goes.
Its agent gave the list (see `drover-anvil'); the view, in
`drover-items-mode', shows an item a line, and changes where each goes
with the arrows.  Interactively, SESSION is the session at point, in a
list or in its own buffer.  A session of another host keeps its list
there."
  (interactive (list (drover-session-at-point)))
  (drover-start)
  (unless session
    (user-error "No session at point"))
  (let ((title (drover-bound-line (format "%s" (drover-session-display-title session)) 80)))
    (when (drover-session-host session)
      (user-error "The list of %s is kept on %s, which runs it: show it there"
                  title (drover-host-label (drover-session-host session))))
    (unless (drover-items-list-of session)
      (user-error "%s has no list of items: its agent gives one with the tool drover-session-items-set" title))
    (let* ((key (drover-items--key session))
           (buffer (drover-items--buffer key title)))
      (with-current-buffer buffer
        (setq drover-items--view-session (drover-session-key session)
              drover-items--view-title title)
        (drover-items--draw))
      (pop-to-buffer buffer)
      buffer)))

(defun drover-items-show (session)
  "Show the list SESSION asks the user to review.
The value of `drover-ask-respond-function', which A calls."
  (drover-items session))

;;;; Applying

(defun drover-items--to-send-p (item)
  "Return non-nil when the session is to be told where ITEM goes.
An item leaving it, unless the session was told so already, or saved
with a heading, which leaves nothing to do."
  (let ((goes (drover-items-goes item)))
    (and (memq goes '(session fork save))
         (not (eq (car (plist-get item :sent)) goes))
         (not (and (eq goes 'save) (plist-get item :heading))))))

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
         (place (plist-get item :place))
         (where (pcase goes
                  ('keep "stays here.")
                  ('session (if heading
                                (format "to a new session.  Its heading is %s, written already." heading-link)
                              (format "to a new session.  Its heading goes %s." (drover-items--place-text place))))
                  ('fork (if heading
                             (format "to a fork of this session.  Its heading is %s, written already." heading-link)
                           (format "to a fork of this session.  Its heading goes %s." (drover-items--place-text place))))
                  ('save (format "saved for later: start nothing.  Its heading goes %s."
                                 (drover-items--place-text place))))))
    (concat (format "- Item %d, \"%s\" (%s): %s" (plist-get item :id) (plist-get item :title)
                    (drover-items-item-status item) where)
            (if (eq goes 'keep)
                ""
              (concat (if-let* ((link (drover-items-link-text item))) (concat "\n  Link: " link) "")
                      (if-let* ((next (plist-get item :next))) (concat "\n  Next step: " next) "")
                      (if-let* ((note (plist-get item :note)))
                          (concat "\n  Note: " (string-replace "\n" "\n  " note))
                        ""))))))

(defun drover-items-prompt-text (items)
  "Return the prompt telling where ITEMS go, made from `drover-items-prompt'."
  (format-spec drover-items-prompt
               `((?i . ,(mapconcat #'drover-items--prompt-item items "\n")))))

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
  "Apply where the items of the view's list go.
Where the heading of each item leaving the session goes, with no
heading yet, is asked first, as Org's refile asks a place, the place
chosen last offered (see `drover-handoff-read-place').  Then one prompt
tells the session where each item goes and where its heading goes
\(see `drover-items-prompt'), as the prompt of a hand-off goes: out of
every window, never over a draft, and once a running turn ended.  The
items it told of are marked sent, and the session no longer asks the
user to review its list.  With no item leaving, nothing is sent: the
review is over.  Drover writes no heading."
  (interactive nil drover-items-mode)
  (let* ((key drover-items--view-key)
         (session (or (drover-items--view-record) (user-error "The session of this list is gone")))
         (title (drover-bound-line (format "%s" (drover-session-display-title session)) 80))
         (list (or (drover-items--list key) (user-error "This list is gone"))))
    ;; Refused before any place is asked: a session no prompt reaches.
    (unless (drover-supports-p session :submit)
      (user-error "%s takes no prompt from drover: its harness, %s, cannot be told where its items go"
                  title (drover-backend-label (drover-session-backend session))))
    (drover-schedule-session-target session)
    (drover-items--refresh list)
    (let ((leaving (seq-filter #'drover-items--to-send-p (plist-get list :items))))
      (dolist (item leaving)
        (unless (plist-get item :heading)
          (drover-items--put key (plist-get item :id) :place
                             (drover-handoff-read-place
                              (format "Item %d, %s, its heading goes under" (plist-get item :id)
                                      (drover-bound-line (plist-get item :title) 50))
                              session))))
      (let* ((list (drover-items--list key))
             (now (float-time))
             (open (seq-filter #'drover-items-goes (plist-get list :items)))
             (sending (seq-filter #'drover-items--to-send-p open))
             (waits (and sending
                         (drover-handoff-send session
                                              (drover-items-prompt-text
                                               (seq-filter (lambda (item)
                                                             (or (memq item sending) (eq (drover-items-goes item) 'keep)))
                                                           open))
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
          (drover-message "No item of %s leaves it: nothing sent" title))
        (when (eq (current-buffer) (drover-items--buffer key))
          (quit-window))))))

;;;; Restoring

(defun drover-items-restore ()
  "Read the lists, as drover first starts, and have their sessions ask anew.
The sessions whose lists wait for the user's review need the user
again (see `drover--kept-modules')."
  (let ((table (drover-items-load)))
    (maphash (lambda (key _ask)
               (when-let* ((keys (drover-items--session-keys key)))
                 (drover-take-in-again keys)))
             (drover-items--table-asks table))))

(setq drover-ask-function #'drover-items-ask
      drover-ask-respond-function #'drover-items-show)

(defun drover-items-unload-function ()
  "Stop asking the user of lists; they stay in their file."
  (when (eq drover-ask-function #'drover-items-ask)
    (setq drover-ask-function nil))
  (when (eq drover-ask-respond-function #'drover-items-show)
    (setq drover-ask-respond-function nil))
  (remove-hook 'drover-items-changed-hook #'drover-items--refresh-views)
  nil)

(provide 'drover-items)
;;; drover-items.el ends here
