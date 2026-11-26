;;; drover-remote-items.el --- The items of the sessions of other hosts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, convenience

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

;; The list of the items of a session (see `drover-items') is kept by
;; the host running the session, in its items.eld: its agent gives it
;; there, the headings of its items are in that host's Org files, and
;; the prompt applying the user's choices goes through that host's
;; queue.  This file has another host read it, show it and change it,
;; as its own.
;;
;; The owner answers five requests of its peers telling the capability
;; `items': `items', the list of one of its sessions, and, followed
;; with :follow, the sessions whose lists wait for a review, then a
;; message `items' naming each list as it changes; `items-goes', where
;; the user says an item goes; `items-apply', the choices applied, with
;; the place of each heading, an Org ID this host must find among its
;; own Org files; `items-set', the items an agent of the peer gives;
;; and `items-places', the headings it offers as places to a peer that
;; reads none, such as a phone.  Reading takes `read' access; the
;; others `act', as a prompt sent does.  What a peer asks is checked as
;; it comes, and performed in no window, asking nothing.
;;
;; The viewer keeps what each host told, in memory: the lists it read,
;; by the key here of their session, and the reviews waiting, which
;; rows show.  A list is read as the side list first shows its session,
;; and again as its host tells it changed; a change asked shows at once,
;; and what the host kept once it answers.  Nothing is waited for.
;;
;; Neither end loads `drover-items' before it has a list to tell or to
;; show: a host of no list answers that it has none, and the rows of a
;; host showing none tell the reviews through this file.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-remote)

(declare-function spyglass-connection-clock-offset "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-live-p "ext:spyglass-conn" (connection))
(declare-function drover-items-load "drover-items" ())
(declare-function drover-items-file "drover-items" ())
(declare-function drover-items-list-of "drover-items" (session))
(declare-function drover-items--list "drover-items" (key))
(declare-function drover-items--key "drover-items" (session))
(declare-function drover-items--there-p "drover-items" (key))
(declare-function drover-items--find "drover-items" (list number))
(declare-function drover-items--bound "drover-items" (item))
(declare-function drover-items--refresh "drover-items" (list))
(declare-function drover-items--told-changed "drover-items" (list))
(declare-function drover-items--changed "drover-items" (key))
(declare-function drover-items--put "drover-items" (key number property value))
(declare-function drover-items--session-keys "drover-items" (key))
(declare-function drover-items--serving-table "drover-items" ())
(declare-function drover-items--needs-place-p "drover-items" (item))
(declare-function drover-items--table-lists "drover-items" (cl-x) t)
(declare-function drover-items-heading-told "drover-items" (item))
(declare-function drover-items-heading-sessions "drover-items" (item))
(declare-function drover-items-item-status "drover-items" (item))
(declare-function drover-items-closed-p "drover-items" (item))
(declare-function drover-items-want-archived "drover-items" (list))
(declare-function drover-items-find-headings "drover-items" (list callback))
(declare-function drover-items-set-goes "drover-items" (key number goes))
(declare-function drover-items-tell "drover-items" (session key))
(declare-function drover-items-check "drover-items" (session given callback))
(declare-function drover-items-set "drover-items" (session items review))
(declare-function drover-handoff-note-place "drover-handoff" (place))
(declare-function drover-task-id-of "drover-task" (session))
(declare-function drover-task-info "drover-task" (id))
(declare-function drover-task-org-find "drover-task-org" (id callback))
(declare-function drover-task-org-refused-p "drover-task-org" (file))
(declare-function drover-task-org-place-id "drover-task-org" (marker))
(declare-function drover-task-org-read "drover-task-org" (marker))
(declare-function anvil-org--find-allowed-file "ext:anvil-org" (filename))
(defvar drover-items-limit)
(defvar drover-items-fields)
(defvar drover-items-statuses)
(defvar drover-items-destinations)
(defvar drover-items-title-limit)
(defvar drover-items-next-limit)
(defvar drover-items-note-lines)
(defvar drover-items-note-limit)
(defvar drover-items-link-limit)
(defvar drover-items--id-regexp)
(defvar drover-items--serving)
(defvar drover-items-changed-hook)
(defvar drover-handoff-places)
(defvar drover-list-review-function)

(defconst drover-remote-items-reviews-limit 200
  "Sessions at most whose review waiting a host tells its peers.")

(defconst drover-remote-items-sessions-limit 10
  "Sessions serving its heading a host tells of an item at most.")

(defconst drover-remote-items-places-limit 30
  "Headings at most a host offers a peer as places.")

(defconst drover-remote-items-lists-limit 50
  "Lists of the sessions of other hosts this host keeps at most, those read last.")

(defconst drover-remote-items-field-limit 4000
  "Characters at most of a field of an item an agent gives another host.
More than a list keeps of any field: what the host keeps of it is
bounded there, as for its own agents.")

(defconst drover-remote-items-wait 10
  "Seconds at most a host waits for the archived sessions it fetches.
Before telling a list asked with :wait, whose items tell the sessions
serving their headings, those archived since too.")

(defconst drover-remote-items-timeout 60
  "Seconds a host may take to keep the items given, or to apply choices.
It looks for headings among its Org files, from a process.")

(defun drover-remote-items--load ()
  "Load what keeps and shows lists of items, which this file needs then."
  (require 'drover-items)
  (require 'drover-handoff)
  (require 'drover-task-org))

(defun drover-remote-items--kept-p ()
  "Return non-nil when this host may keep lists: it read them, or has their file."
  (or (featurep 'drover-items)
      (file-exists-p (expand-file-name "items.eld" drover-state-directory))))

;;;; On the owner

(defvar drover-remote-items--followers (make-hash-table :test #'eq :weakness 'key)
  "Connections whose peers follow this host's lists of items.")

(defvar drover-remote-items--changed nil
  "The keys of the lists changed that the followers are not told of yet.")

(defvar drover-remote-items--timer nil
  "The timer telling the followers the lists that changed, or nil.")

(defvar drover-remote-items--told (make-hash-table :test #'equal)
  "Since when the list of each session waits for a review, as last told.
By the key of the session, for those told the followers in a message
`items' (see `drover-remote-items--sessions-changed').")

(defun drover-remote-items--wire-key (key)
  "Return KEY, of a session of this host, as it travels, or nil."
  (when-let* ((id (drover-remote--wire-id (cdr key))))
    (vector (symbol-name (car key)) id)))

(defun drover-remote-items--session (message)
  "Return the session of this host MESSAGE's :key names, as it serves it, or nil."
  (when-let* ((key (drover-remote--key (plist-get message :key))))
    (car (drover-remote--served key))))

(defun drover-remote-items--allowed (peer what)
  "Return why PEER may not WHAT, changing a list of this host's, or nil.
A line; changing a list takes `act' access, as sending a prompt does."
  (let ((access (drover-remote--access peer)))
    (unless (drover-remote--allows-p access "send")
      (drover-remote--refuse-action peer what
                                    (format "%s has %s access" (drover-remote--label peer) access)))))

(defun drover-remote-items--wire-session (session)
  "Return SESSION, serving the heading of an item, as it travels.
:k is its key on its host, :h that host when it is not this one."
  (let* ((host (drover-session-host session))
         (key (if host (drover-session-id session) (drover-session-key session))))
    (list :k (or (and (consp key) (symbolp (car key)) (drover-remote-items--wire-key key)) :false)
          :h (or (drover-remote--plain host) :false)
          :ti (or (drover-remote--plain (drover-bound-line (format "%s" (drover-session-display-title session)) 160))
                  "")
          :st (symbol-name (or (drover-session-status session) 'idle))
          :ar (if (drover-session-archived session) t :false))))

(defun drover-remote-items--wire-item (item)
  "Return ITEM, of a list of this host, as it travels to peers.
With the status this host computes of it, what it found of its heading
and the sessions serving it."
  (let ((told (drover-items-heading-told item))
        (link (plist-get item :link))
        (place (plist-get item :place))
        (sent (plist-get item :sent)))
    (cl-flet ((text (value) (or (drover-remote--plain value) :false)))
      (list :n (plist-get item :id)
            :ti (or (drover-remote--plain (plist-get item :title)) "")
            :st (symbol-name (drover-items-item-status item))
            :ln (if (and link (stringp (car link)))
                    (vector (drover-remote--plain (car link)) (text (cdr link)))
                  :false)
            :nx (text (plist-get item :next))
            :nt (text (plist-get item :note))
            :h (text (plist-get item :heading))
            :ht (text (car told))
            :hf (if (cdr told) t :false)
            :g (if (plist-get item :goes) (symbol-name (plist-get item :goes)) :false)
            :pl (if place
                    (list :uri (or (drover-remote--plain (plist-get place :uri)) "")
                          :name (or (drover-remote--plain (plist-get place :name)) "")
                          :id (text (plist-get place :id)))
                  :false)
            :sn (if (and sent (symbolp (car sent)) (drover-remote--number (cdr sent)))
                    (vector (symbol-name (car sent)) (cdr sent))
                  :false)
            :ss (vconcat (mapcar #'drover-remote-items--wire-session
                                 (seq-take (drover-items-heading-sessions item)
                                           drover-remote-items-sessions-limit)))
            :c (or (drover-remote--number (plist-get item :created)) :false)
            :u (or (drover-remote--number (plist-get item :updated)) :false)))))

(defun drover-remote-items--wire-list (record list &optional more)
  "Return LIST, of the session RECORD of this host, as it travels to peers.
MORE is non-nil while the archived sessions serving its headings are
still fetched: asked again, it may tell more of them."
  (let ((drover-items--serving (and (plist-get list :items) (drover-items--serving-table))))
    (list :k (drover-remote-items--wire-key (drover-session-key record))
          :ti (or (drover-remote--plain (drover-bound-line (format "%s" (drover-session-display-title record)) 300))
                  "")
          :a (or (drover-remote--number (plist-get list :asked)) :false)
          :u (or (drover-remote--number (plist-get list :updated)) 0)
          :more (if more t :false)
          :items (vconcat (mapcar #'drover-remote-items--wire-item (plist-get list :items))))))

(defun drover-remote-items--wire-reviews ()
  "Return the sessions of this host whose lists wait for a review, as they travel.
A vector of [KEY SINCE], `drover-remote-items-reviews-limit' at most."
  (let ((reviews nil)
        (count 0))
    (when (drover-remote-items--kept-p)
      (drover-remote-items--load)
      (maphash (lambda (key list)
                 (when-let* ((asked (drover-remote--number (plist-get list :asked))))
                   (dolist (session (drover-items--session-keys key))
                     (when-let* (((< count drover-remote-items-reviews-limit))
                                 ((drover-remote--served session))
                                 (wire (drover-remote-items--wire-key session)))
                       (cl-incf count)
                       (push (vector wire asked) reviews)))))
               (drover-items--table-lists (drover-items-load))))
    (vconcat (nreverse reviews))))

(defun drover-remote-items--await-archived (then)
  "Call THEN once the archived sessions fetched arrived.
Or after `drover-remote-items-wait' seconds: a backend may never answer."
  (let ((deadline (+ (float-time) drover-remote-items-wait))
        (timer nil))
    (setq timer
          (run-at-time 0.2 0.2
                       (lambda ()
                         (drover-with-reporting "remote items"
                           (when (or (> (float-time) deadline)
                                     (not (drover-fetch-pending-p drover-remote-items-wait)))
                             (cancel-timer timer)
                             (funcall then))))))))

(defun drover-remote-items--on-read (connection message reply)
  "Tell CONNECTION's peer, through REPLY, what MESSAGE asks of this host's lists.
With its :key, the list of that session, false when it has none, once
its headings were looked for; with :wait, once the archived sessions
serving them arrived too.  With :follow, the sessions whose lists wait
for a review, and the peer is told as a list changes, in messages
`items'."
  (condition-case err
      (let* ((follow (eq (plist-get message :follow) t))
             (reviews (and follow (list :reviews (drover-remote-items--wire-reviews))))
             (record (drover-remote-items--session message)))
        (when follow
          (puthash connection t drover-remote-items--followers)
          (add-hook 'drover-items-changed-hook #'drover-remote-items--list-changed)
          (add-hook 'drover-changed-hook #'drover-remote-items--sessions-changed))
        (cond
         ((null (plist-get message :key)) (funcall reply reviews))
         ((not record) (funcall reply nil "no such session here"))
         ((not (drover-remote-items--kept-p)) (funcall reply (append reviews (list :list :false))))
         (t
          (drover-remote-items--load)
          (let ((list (drover-items-list-of record))
                (key (drover-session-key record)))
            (if (null list)
                (funcall reply (append reviews (list :list :false)))
              (let ((pending (drover-items-want-archived list))
                    (tell (lambda ()
                            (drover-with-reporting "remote items"
                              ;; As they are now: the session may have changed meanwhile.
                              (let ((record (or (car (drover-remote--served key)) record)))
                                (funcall reply
                                         (append reviews
                                                 (list :list (drover-remote-items--wire-list
                                                              record (or (drover-items-list-of record) list)
                                                              (drover-fetch-pending-p drover-remote-items-wait))))))))))
                (drover-items-find-headings
                 list (lambda ()
                        (drover-with-reporting "remote items"
                          (if (and pending (eq (plist-get message :wait) t))
                              (drover-remote-items--await-archived tell)
                            (funcall tell)))))))))))
    (error (funcall reply nil (drover-error-message err)))))

(defun drover-remote-items--list-changed (key)
  "Tell the peers following this host's lists that the list of KEY changed.
A tenth of a second later, several changes told once.  For
`drover-items-changed-hook': KEY is the key of a conversation of this
host; a list of another host changing here is that host's to tell."
  (drover-with-reporting "remote items"
    (when (and (> (hash-table-count drover-remote-items--followers) 0)
               (not (drover-items--there-p key)))
      (cl-pushnew key drover-remote-items--changed :test #'equal)
      (unless drover-remote-items--timer
        (setq drover-remote-items--timer (run-at-time 0.1 nil #'drover-remote-items--tell))))))

(defun drover-remote-items--untold (session)
  "Return the key of the list of SESSION when its review is untold, else nil.
SESSION is the key of a session.  Its review is untold when this host
serves SESSION, its list waits for a review, and the followers were not
told so of SESSION."
  (when-let* ((record (car (drover-remote--served session)))
              (key (drover-items--key record))
              (asked (plist-get (drover-items--list key) :asked))
              ((not (equal (gethash session drover-remote-items--told) asked))))
    key))

(defun drover-remote-items--sessions-changed ()
  "Tell the followers the reviews of the sessions that came to show a list.
A list is its conversation's, and the session showing a conversation
changes: a terminal resuming it takes the place of the record of its
transcript, under another key, which the followers were told nothing
of.  For `drover-changed-hook', while a peer follows and lists are kept."
  (drover-with-reporting "remote items"
    (when (and (> (hash-table-count drover-remote-items--followers) 0)
               (featurep 'drover-items))
      (let ((untold nil))
        (if (eq drover-changed-sessions t)
            (maphash (lambda (key list)
                       (when (plist-get list :asked)
                         (when (seq-some #'drover-remote-items--untold (drover-items--session-keys key))
                           (push key untold))))
                     (drover-items--table-lists (drover-items-load)))
          (dolist (session drover-changed-sessions)
            (when-let* ((key (drover-remote-items--untold session)))
              (push key untold))))
        (mapc #'drover-remote-items--list-changed untold)))))

(defun drover-remote-items--tell ()
  "Tell the followers the lists that changed: their sessions, and their reviews.
A message `items' each: :k, the keys of the sessions showing its
conversation that this host serves, :a, since when it waits for a
review, or false, and :u, when it changed last."
  (drover-with-reporting "remote items"
    (setq drover-remote-items--timer nil)
    (let ((keys (nreverse drover-remote-items--changed)))
      (setq drover-remote-items--changed nil)
      (dolist (key keys)
        (let* ((list (drover-items--list key))
               (sessions (vconcat (seq-keep (lambda (session)
                                              (and (drover-remote--served session)
                                                   (drover-remote-items--wire-key session)))
                                            (seq-take (drover-items--session-keys key) 8)))))
          (when (length> sessions 0)
            (let ((message (list :t "items" :k sessions
                                 :a (or (drover-remote--number (plist-get list :asked)) :false)
                                 :u (or (drover-remote--number (plist-get list :updated)) 0))))
              (dolist (session (drover-items--session-keys key))
                (if (plist-get list :asked)
                    (puthash session (plist-get list :asked) drover-remote-items--told)
                  (remhash session drover-remote-items--told)))
              (dolist (connection (hash-table-keys drover-remote-items--followers))
                (if (spyglass-connection-live-p connection)
                    (drover-remote--send connection message)
                  (remhash connection drover-remote-items--followers))))))))))

(defun drover-remote-items--once (state id reply perform)
  "Perform a peer's request of identifier ID once, in STATE, answering REPLY.
PERFORM is called with a function of an outcome and what more to
answer: the outcome, a plist, (:failure WHY) for a refusal, is kept for
the request asked again after its stream ended, which gets it rather
than be performed twice; what more, such as the list, goes to those
waiting now alone."
  (let ((known (and (stringp id) (gethash id (drover-remote--state-outcomes state)))))
    (cond
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((and known (car known)) (drover-remote--new-reply reply (cdr known)))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (drover-remote--begin-outcome state id reply)
      (funcall perform
               (lambda (outcome &optional more)
                 (let ((entry (gethash id (drover-remote--state-outcomes state))))
                   (puthash id (cons t outcome) (drover-remote--state-outcomes state))
                   (dolist (pending (cdr entry))
                     (drover-with-reporting "remote items"
                       (drover-remote--new-reply pending (append outcome more)))))))))))

(defun drover-remote-items--later (finish function &rest arguments)
  "Call FUNCTION with ARGUMENTS, then FINISH, from a timer.
Out of the filter of the stream the request came by.  FINISH is the
function of `drover-remote-items--once': an error tells it why."
  (run-at-time 0 nil
               (lambda ()
                 (drover-with-reporting "remote items"
                   (condition-case err
                       (apply function (append arguments (list finish)))
                     (error (funcall finish (list :failure (or (drover-bound-line (drover-error-message err))
                                                               "failed")))))))))

(defun drover-remote-items--on-goes (peer message reply)
  "Say for PEER where the item MESSAGE names goes, and tell REPLY the list.
MESSAGE's :key names the session, :n the item's number and :goes one of
`drover-items-destinations'.  An item done or decided goes nowhere."
  (condition-case err
      (let ((why (drover-remote-items--allowed peer "say where an item goes"))
            (record (drover-remote-items--session message)))
        (cond
         (why (funcall reply nil why))
         ((not record) (funcall reply nil "no such session here"))
         (t
          (drover-remote-items--load)
          (let* ((key (drover-items--key record))
                 (list (drover-items--list key))
                 (number (plist-get message :n))
                 (goes (car (memq (drover-remote--symbol (plist-get message :goes)) drover-items-destinations)))
                 (item (and list (natnump number) (drover-items--find list number))))
            (when list
              (drover-items--refresh list))
            (cond
             ((not goes) (funcall reply nil "no such place for an item"))
             ((not item) (funcall reply nil "its list holds no such item"))
             ((drover-items-closed-p item)
              (funcall reply nil (format "item %d is %s: it goes nowhere" number (drover-items-item-status item))))
             (t (funcall reply (list :list (drover-remote-items--wire-list
                                            record (drover-quietly (drover-items-set-goes key number goes)))))))))))
    (error (funcall reply nil (drover-error-message err)))))

(defun drover-remote-items--places-given (value)
  "Return the places VALUE gives, received with a request `items-apply'.
An alist (NUMBER . PLACE), PLACE a plist of :id, an Org ID, or nil for a
place left to the agent, and :name; `invalid' when VALUE is none."
  (cond
   ((null value) nil)
   ((not (and (vectorp value) (<= (length value) drover-items-limit))) 'invalid)
   (t
    (let ((places nil)
          (invalid nil))
      (seq-doseq (entry value)
        (let ((number (and (consp entry) (keywordp (car entry)) (plist-get entry :n)))
              (id (and (consp entry) (plist-get entry :id))))
          (if (and (natnump number)
                   (or (memq id '(nil :false))
                       (and (stringp id) (string-match-p drover-items--id-regexp id))))
              (push (cons number (list :id (and (stringp id) id)
                                       :name (drover-remote--line (plist-get entry :name) 1000)))
                    places)
            (setq invalid t))))
      (if invalid 'invalid (nreverse places))))))

(defun drover-remote-items--place (number given marker)
  "Return the place of the heading at MARKER, for the item NUMBER, or why not.
GIVEN is what the peer gave, a plist of :id and :name.  A place as
`drover-handoff-read-place' returns one, or a line: this host found no
such heading, or its agent may not write under it."
  (if (null marker)
      (format "item %d: %s finds no heading of ID %s in its Org files" number (drover-host-label nil)
              (plist-get given :id))
    (let* ((buffer (marker-buffer marker))
           (file (buffer-file-name (or (buffer-base-buffer buffer) buffer))))
      (unwind-protect
          (cond
           ((or (null file) (drover-task-org-refused-p file))
            (format "item %d: %s writes no heading in %s" number (drover-host-label nil)
                    (if file (abbreviate-file-name file) "that buffer")))
           ((and (fboundp 'anvil-org--find-allowed-file)
                 (not (ignore-errors (anvil-org--find-allowed-file (expand-file-name file)))))
            (format "item %d: Anvil's Org tools on %s do not write %s" number (drover-host-label nil)
                    (abbreviate-file-name file)))
           (t
            (let ((id (drover-task-org-place-id marker)))
              (list :uri (concat "org-id://" id)
                    :name (or (plist-get given :name)
                              (drover-bound-line (format "%s" (plist-get (drover-task-org-read marker) :title)) 300)
                              id)
                    :file file :id id))))
        (set-marker marker nil)))))

(defun drover-remote-items--find-places (given then)
  "Look for the headings of GIVEN among this host's Org files, then call THEN.
GIVEN is an alist (NUMBER . PLACE) of places naming an Org ID.  THEN
gets the alist (NUMBER . PLACE) of those found, as items keep places,
and the lines telling why the others are none."
  (let ((left (length given))
        (places nil)
        (problems nil))
    (if (null given)
        (funcall then nil nil)
      (pcase-dolist (`(,number . ,place) given)
        (drover-task-org-find
         (plist-get place :id)
         (lambda (marker)
           (drover-with-reporting "remote items"
             (let ((found (condition-case err
                              (drover-remote-items--place number place marker)
                            (error (format "item %d: %s" number (drover-error-message err))))))
               (if (stringp found)
                   (push found problems)
                 (push (cons number found) places)))
             (when (zerop (cl-decf left))
               (funcall then places (nreverse problems))))))))))

(defun drover-remote-items--apply (peer message finish)
  "Apply for PEER where the items of the list MESSAGE names go, then call FINISH.
MESSAGE's :key names the session, its :places the place of the heading
of each item that needs one (see `drover-items--needs-place-p'): an Org
ID this host must find among its Org files, or none, which leaves the
place to the session's agent.  The session is then told as this host's
own user's choices tell it (see `drover-items-tell').  FINISH is the
function of `drover-remote-items--once'."
  (drover-remote-items--load)
  (let* ((record (drover-remote-items--session message))
         (key (and record (drover-items--key record)))
         (list (and key (drover-items--list key)))
         (given (drover-remote-items--places-given (plist-get message :places)))
         (title (and record (drover-bound-line (format "%s" (drover-session-display-title record)) 80))))
    (cond
     ((not record) (funcall finish (list :failure "no such session here")))
     ((not list) (funcall finish (list :failure (format "%s has no list of items" title))))
     ((eq given 'invalid) (funcall finish (list :failure "places of no known shape")))
     ((not (drover-supports-p record :submit))
      (funcall finish (list :failure (format "%s takes no prompt from drover: its harness, %s, cannot be told where its items go"
                                             title (drover-backend-label (drover-session-backend record))))))
     (t
      (drover-items--refresh list)
      (let* ((needing (seq-filter #'drover-items--needs-place-p (plist-get list :items)))
             (numbers (mapcar (lambda (item) (plist-get item :id)) needing))
             (missing (seq-remove (lambda (number) (assq number given)) numbers)))
        (if missing
            (funcall finish (list :refused (vector (format "%s needs a place for the heading of item%s %s: read its list again"
                                                           title (if (cdr missing) "s" "")
                                                           (mapconcat #'number-to-string missing ", ")))))
          (drover-remote-items--find-places
           (seq-filter (lambda (entry) (and (memq (car entry) numbers) (plist-get (cdr entry) :id))) given)
           (lambda (places problems)
             (if problems
                 (progn
                   (drover-remote--refuse-action peer "apply the items of a session" (car problems))
                   (funcall finish (list :refused (vconcat problems))))
               (condition-case err
                   (progn
                     (dolist (number numbers)
                       (drover-items--put key number :place (alist-get number places)))
                     (dolist (place places)
                       (drover-handoff-note-place (cdr place)))
                     (pcase-let ((`(,count . ,waits) (drover-quietly (drover-items-tell record key))))
                       (drover-remote--event peer 'items "applied the items of %s: told of %d" title count)
                       (funcall finish (list :sent count :waits (if waits t :false))
                                (list :list (drover-remote-items--wire-list record (drover-items--list key))))))
                 (error (funcall finish (list :failure (or (drover-bound-line (drover-error-message err))
                                                           "failed"))))))))))))))

(defun drover-remote-items--given (value)
  "Return the items VALUE gives, received with a request `items-set'.
As `drover-items-check' takes them: each a plist of :index and the
fields given, :unknown, the names of fields that are none, and
:malformed for an item that is no object.  A field's text is cut to
`drover-remote-items-field-limit' characters; a value that is neither a
text nor a number is t, which no field takes.  `invalid' when VALUE is
no array of `drover-items-limit' items at most."
  (cond
   ((null value) nil)
   ((not (and (vectorp value) (<= (length value) drover-items-limit))) 'invalid)
   (t
    (let ((index 0))
      (mapcar
       (lambda (object)
         (cl-incf index)
         (if (not (and (consp object) (keywordp (car object))))
             (list :index index :malformed t)
           (let ((given (list :index index))
                 (unknown (plist-get object :unknown)))
             (dolist (key drover-items-fields)
               (when (plist-member object key)
                 (let ((field (plist-get object key)))
                   (setq given
                         (plist-put given key
                                    (cond ((stringp field)
                                           (string-limit field drover-remote-items-field-limit))
                                          ((integerp field) field)
                                          ((memq field '(nil :false)) nil)
                                          (t t)))))))
             (when (eq (plist-get object :malformed) t)
               (setq given (plist-put given :malformed t)))
             (when (vectorp unknown)
               (when-let* ((names (seq-keep (lambda (name) (drover-remote--line name 40)) (seq-take unknown 20))))
                 (setq given (plist-put given :unknown names))))
             given)))
       value)))))

(defun drover-remote-items--set (peer message finish)
  "Keep for PEER the items MESSAGE gives the list of one of this host's sessions.
MESSAGE's :key names the session, its :items the items, as an agent of
PEER gave them (see `drover-remote-items--given'), and :review, false
to keep the list without asking the user to review it.  They are
checked here, where their links and headings are (see
`drover-items-check'): what refuses them is told, each item named, and
nothing kept.  FINISH is the function of `drover-remote-items--once'."
  (drover-remote-items--load)
  (let* ((record (drover-remote-items--session message))
         (given (drover-remote-items--given (plist-get message :items)))
         (review (not (eq (plist-get message :review) :false)))
         (title (and record (drover-bound-line (format "%s" (drover-session-display-title record)) 80))))
    (cond
     ((not record) (funcall finish (list :failure "no such session here")))
     ((eq given 'invalid) (funcall finish (list :failure "items of no known shape")))
     ((and (null given) (null (drover-items-list-of record)))
      (funcall finish (list :failure (format "%s has no list yet: give its items" title))))
     (t
      (drover-items-check
       record given
       (lambda (result)
         (drover-with-reporting "remote items"
           (condition-case err
               (if (cdr result)
                   (funcall finish (list :refused (vconcat (mapcar (lambda (problem)
                                                                     (drover-bound-line problem 600))
                                                                   (seq-take (cdr result) 200)))))
                 (let ((list (drover-quietly (drover-items-set record (car result) review))))
                   (drover-remote--event peer 'items "gave the list of %s %d item%s" title (length given)
                                         (if (length= given 1) "" "s"))
                   (funcall finish (list :kept (length given))
                            (list :list (drover-remote-items--wire-list record list)))))
             (error (funcall finish (list :failure (or (drover-bound-line (drover-error-message err))
                                                       "failed"))))))))))))

(defun drover-remote-items--places (record)
  "Return the headings this host offers as places for the items of RECORD.
Plists of :id and :name, the likeliest first: the heading of the task
RECORD serves, the places its items have, then the places chosen last
on this host (see `drover-handoff-places')."
  (drover-remote-items--load)
  (let ((places nil)
        (seen nil))
    (cl-flet ((add (id name)
                (when (and (stringp id) (string-match-p drover-items--id-regexp id) (not (member id seen)))
                  (push id seen)
                  (push (list :id id :name (or (drover-bound-line (and (stringp name) name) 300) id)) places))))
      (when-let* (((require 'drover-task nil t))
                  (id (drover-task-id-of record))
                  (info (drover-task-info id)))
        (add (plist-get info :id) (plist-get info :title)))
      (dolist (item (plist-get (drover-items-list-of record) :items))
        (when-let* ((place (plist-get item :place)))
          (add (plist-get place :id) (plist-get place :name))))
      (dolist (place drover-handoff-places)
        (add (plist-get place :id) (plist-get place :name))))
    (seq-take (nreverse places) drover-remote-items-places-limit)))

(defun drover-remote-items--on-places (peer message reply)
  "Tell PEER, through REPLY, the places this host offers for a session's items.
MESSAGE's :key names the session.  For a peer that reads no place as
Org's refile reads one, such as a phone: each is a heading of this
host's Org files, by its Org ID and its name."
  (condition-case err
      (let ((why (drover-remote-items--allowed peer "read the places of headings"))
            (record (drover-remote-items--session message)))
        (cond
         (why (funcall reply nil why))
         ((not record) (funcall reply nil "no such session here"))
         (t (funcall reply
                     (list :places (vconcat (mapcar (lambda (place)
                                                      (list :id (drover-remote--plain (plist-get place :id))
                                                            :name (drover-remote--plain (plist-get place :name))))
                                                    (drover-remote-items--places record))))))))
    (error (funcall reply nil (drover-error-message err)))))

;;;###autoload
(defun drover-remote-items-answer (state peer connection message reply)
  "Answer PEER's request MESSAGE about this host's lists of items, through REPLY.
STATE is the remote layer's, CONNECTION the one MESSAGE came by."
  (pcase (plist-get message :op)
    ("items" (drover-remote-items--on-read connection message reply))
    ("items-goes" (drover-remote-items--on-goes peer message reply))
    ("items-places" (drover-remote-items--on-places peer message reply))
    ((and (or "items-apply" "items-set") op)
     (let ((what (if (equal op "items-apply") "apply the items of a session" "give a session's list its items")))
       (if-let* ((why (drover-remote-items--allowed peer what)))
           (funcall reply nil why)
         (drover-remote-items--once
          state (plist-get message :id) reply
          (lambda (finish)
            (drover-remote-items--later finish
                                        (if (equal op "items-apply")
                                            #'drover-remote-items--apply
                                          #'drover-remote-items--set)
                                        peer message))))))
    (_ (funcall reply nil "unknown request"))))

;;;; On the viewer

(defvar drover-remote-items--lists (make-hash-table :test #'equal)
  "What each host told of the lists of its sessions, by the session's key here.
Each a plist: :list, the list as `drover-remote-items--decode' reads
it, nil for a session without one; :read, when it was told, a float
time, nil until then; :asked, when it was asked, while not answered;
:failure, why its host did not tell it, a line; :pending, how many
changes asked of its host are not answered: the answer to the last
tells the list as its host keeps it then.")

(defvar drover-remote-items--reviews (make-hash-table :test #'equal)
  "The sessions of each host whose lists wait for a review, by the host's name.
Each a hash table of since when, by the session's key there.")

(defun drover-remote-items--offset (peer)
  "Return how far PEER's clock is ahead of this host's, in seconds."
  (or (when-let* ((connection (drover-remote--peer-connection peer)))
        (spyglass-connection-clock-offset connection))
      0))

(defun drover-remote-items--entry (key)
  "Return what the host of the session KEY told of its list, a plist, or nil."
  (gethash key drover-remote-items--lists))

(defun drover-remote-items--note (key &rest properties)
  "Keep PROPERTIES of what the host of the session KEY told of its list.
The lists read longest ago go past `drover-remote-items-lists-limit'."
  (let ((entry (copy-sequence (gethash key drover-remote-items--lists))))
    (cl-loop for (property value) on properties by #'cddr
             do (setq entry (plist-put entry property value)))
    (puthash key entry drover-remote-items--lists)
    (when (> (hash-table-count drover-remote-items--lists) drover-remote-items-lists-limit)
      (let ((oldest nil))
        (maphash (lambda (other entry)
                   (unless (or (equal other key) (> (or (plist-get entry :pending) 0) 0))
                     (when (or (null oldest) (< (or (plist-get entry :read) 0) (cdr oldest)))
                       (setq oldest (cons other (or (plist-get entry :read) 0))))))
                 drover-remote-items--lists)
        (when oldest
          (remhash (car oldest) drover-remote-items--lists))))
    entry))

(defun drover-remote-items-list (key)
  "Return the list its host told of the session KEY, a key here, or nil.
Nil until it was read, and for a session without a list."
  (plist-get (drover-remote-items--entry key) :list))

(defun drover-remote-items-review-since (session)
  "Return since when the list of SESSION, of another host, waits for a review.
As its host told, on this host's clock; nil when it does not.  A lookup:
lists ask it of every row they draw."
  (when-let* ((reviews (gethash (drover-session-host session) drover-remote-items--reviews)))
    (gethash (drover-session-id session) reviews)))

(defun drover-remote-items--row-review (session)
  "Return since when the list of SESSION waits for a review, for a row of a list.
A value of `drover-list-review-function', of this file alone: the lists
of this host's sessions are told by `drover-items', which takes its
place as it loads."
  (and (drover-session-host session)
       (drover-remote-items-review-since session)))

(defun drover-remote-items--tell-rows ()
  "Have the rows of lists tell the reviews other hosts told wait.
When nothing tells them of reviews: without loading `drover-items',
which a host of no list has no use for, and which tells them itself
once loaded (see `drover-items-review-since')."
  (unless (bound-and-true-p drover-list-review-function)
    (setq drover-list-review-function #'drover-remote-items--row-review)))

(defun drover-remote-items-refusal (session &optional action)
  "Return why the list of SESSION, of another host, cannot be read, a line, or nil.
With ACTION, why it cannot be changed so: \"change\", \"give\" or
\"apply\", which sends SESSION a prompt.  Its host is not a peer, runs a
drover that keeps no list for others, named so, is not connected, or
grants no such access."
  (let* ((name (drover-session-host session))
         (peer (drover-remote--peer name))
         (label (if peer (drover-remote--label peer) (drover-host-label name))))
    (cond
     ((not peer) (format "%s is not a peer of this host" label))
     ((not (drover-remote--capable-p peer "items"))
      (format "%s runs an older drover, which keeps no list of items for other hosts: update it there" label))
     ((not (eq (drover-remote--peer-state peer) 'open)) (format "%s is not connected" label))
     ((and action (not (drover-remote--allows-p (or (drover-remote--peer-granted peer) 'read) "send")))
      (format "%s grants this host %s access" label (or (drover-remote--peer-granted peer) "read")))
     ((equal action "apply") (drover-remote--refusal peer session "send")))))

(defun drover-remote-items-state (session)
  "Return why the list of SESSION, of another host, does not show, or nil.
A line: its host is asked for it, or cannot tell it, and why.  Nil once
its host told it, a list or that it has none; it then shows as last
told, whatever became of its host since."
  (let* ((entry (drover-remote-items--entry (drover-session-key session)))
         (label (drover-host-label (drover-session-host session))))
    (cond
     ((plist-get entry :read) nil)
     ((drover-remote-items-refusal session))
     ((plist-get entry :failure) (format "%s did not tell its list: %s" label (plist-get entry :failure)))
     (t (format "Reading its list on %s…" label)))))

(defun drover-remote-items--decode-sessions (name value)
  "Return VALUE, the sessions the host NAME told serve a heading, as plists.
Each of :key, its key here when this host can name it: one of NAME's,
of this host's, or of a peer's; :host, :title, :status and :archived."
  (and (vectorp value)
       (seq-keep
        (lambda (data)
          (when (and (consp data) (keywordp (car data)))
            (let* ((there (drover-remote--key (plist-get data :k)))
                   (told (plist-get data :h))
                   (host (and (stringp told) (string-match-p drover-remote--name-regexp told) told))
                   (other (and host (not (drover-host-local-p host)) (drover-remote--find-peer host))))
              (list :key (cond ((null there) nil)
                               ((null host) (drover-remote-session-key name there))
                               ((drover-host-local-p host) there)
                               (other (drover-remote-session-key (drover-remote--peer-name other) there)))
                    :host (cond ((null host) name)
                                ((drover-host-local-p host) nil)
                                (t host))
                    :title (or (drover-remote--line (plist-get data :ti) 160) "A session")
                    :status (drover-remote--status (plist-get data :st))
                    :archived (eq (plist-get data :ar) t)))))
        (seq-take value drover-remote-items-sessions-limit))))

(defun drover-remote-items--decode-item (name data offset)
  "Return DATA, an item the host NAME told, as an item of `drover-items', or nil.
Its times are moved OFFSET seconds.  Whatever it holds is checked and
bounded; :there holds what NAME found of its heading (see the plist of
lists of `drover-items')."
  (when-let* (((and (consp data) (keywordp (car data))))
              (number (plist-get data :n))
              ((and (natnump number) (< 0 number 1000000)))
              (status (car (memq (drover-remote--symbol (plist-get data :st)) drover-items-statuses))))
    (let* ((link (plist-get data :ln))
           (place (plist-get data :pl))
           (sent (plist-get data :sn))
           (note (plist-get data :nt))
           (heading (let ((id (plist-get data :h)))
                      (and (stringp id) (string-match-p drover-items--id-regexp id) id))))
      (drover-items--bound
       (list :id number
             :title (or (drover-remote--line (plist-get data :ti) drover-items-title-limit) "")
             :status status
             :link (and (vectorp link) (length= link 2)
                        (when-let* ((target (drover-remote--line (aref link 0) drover-items-link-limit)))
                          (cons target (drover-remote--line (aref link 1) 200))))
             :next (drover-remote--line (plist-get data :nx) drover-items-next-limit)
             :note (and (stringp note) (drover-bound-text note drover-items-note-lines drover-items-note-limit))
             :heading heading
             :heading-title (and heading (drover-remote--line (plist-get data :ht) 300))
             :goes (car (memq (drover-remote--symbol (plist-get data :g)) drover-items-destinations))
             :place (and (consp place) (keywordp (car place))
                         (when-let* ((uri (drover-remote--line (plist-get place :uri) 4000)))
                           (let ((id (plist-get place :id)))
                             (list :uri uri
                                   :name (or (drover-remote--line (plist-get place :name) 1000) "")
                                   :id (and (stringp id) (string-match-p drover-items--id-regexp id) id)))))
             :sent (and (vectorp sent) (length= sent 2)
                        (when-let* ((goes (car (memq (drover-remote--symbol (aref sent 0)) drover-items-destinations)))
                                    (time (drover-remote--time (aref sent 1) offset)))
                          (cons goes time)))
             :created (drover-remote--time (plist-get data :c) offset)
             :updated (drover-remote--time (plist-get data :u) offset)
             :there (list :host name
                          :found (eq (plist-get data :hf) t)
                          :sessions (and heading
                                         (drover-remote-items--decode-sessions name (plist-get data :ss)))))))))

(defun drover-remote-items--decode (name data offset)
  "Return DATA, a list the host NAME told, as a list of `drover-items', or nil.
Its times are moved OFFSET seconds, NAME's clock being that far ahead.
:key is the key here of its session, :host NAME, :stamp when it changed
last as NAME told it, and :more non-nil while NAME may tell more of the
sessions serving its headings.  Nil when DATA names no session."
  (when-let* (((and (consp data) (keywordp (car data))))
              (there (drover-remote--key (plist-get data :k))))
    (let ((items (plist-get data :items))
          (seen (make-hash-table :test #'eql)))
      (list :key (drover-remote-session-key name there)
            :host name
            :title (drover-remote--line (plist-get data :ti) 300)
            :asked (drover-remote--time (plist-get data :a) offset)
            :updated (or (drover-remote--time (plist-get data :u) offset) 0)
            :stamp (drover-remote--number (plist-get data :u))
            :more (eq (plist-get data :more) t)
            :items (and (vectorp items)
                        (seq-keep (lambda (data)
                                    (when-let* ((item (drover-remote-items--decode-item name data offset))
                                                ((not (gethash (plist-get item :id) seen))))
                                      (puthash (plist-get item :id) t seen)
                                      item))
                                  (seq-take items drover-items-limit)))))))

(defun drover-remote-items--review (name there since)
  "Note that the list of the session THERE of the host NAME waits since SINCE.
THERE is its key on NAME; SINCE nil when it does not wait.  Its rows
are drawn again when that changed."
  (let ((reviews (or (gethash name drover-remote-items--reviews)
                     (puthash name (make-hash-table :test #'equal) drover-remote-items--reviews))))
    (unless (equal (gethash there reviews) since)
      (if (and since (or (gethash there reviews)
                         (< (hash-table-count reviews) (* 2 drover-remote-items-reviews-limit))))
          (puthash there since reviews)
        (remhash there reviews))
      (drover-remote-items--tell-rows)
      (drover-views-changed (list (drover-remote-session-key name there))))))

(defun drover-remote-items--install (key name data offset)
  "Keep DATA, the list the host NAME told of the session KEY, and show it.
DATA is as received, false for a session without a list; OFFSET is how
far NAME's clock is ahead.  Return the list, nil for none; `invalid'
when DATA is no list, which keeps nothing."
  (let ((list (and (consp data) (drover-remote-items--decode name data offset))))
    (if (and (consp data) (or (null list) (not (equal (plist-get list :key) key))))
        'invalid
      (drover-remote-items--note key :list list :read (float-time) :asked nil :failure nil)
      (drover-remote-items--review name (cdr key) (plist-get list :asked))
      (drover-items--changed key)
      list)))

(defun drover-remote-items-read (session &optional callback wait)
  "Ask the host of SESSION, another host's, for its list of items.
Nothing is waited for.  Once it answered, the list is kept, lists and
the side list showing SESSION are drawn again, and CALLBACK, if any,
gets (:list LIST), LIST nil for a session without one, or (:failure
WHY).  With WAIT, the host answers once the archived sessions serving
the headings of its items arrived there, ten seconds at most; without,
a list telling it may know more of them is asked again so, once."
  (drover-remote-items--load)
  (let* ((key (drover-session-key session))
         (name (drover-session-host session))
         (why (drover-remote-items-refusal session)))
    (if why
        (when callback
          (funcall callback (list :failure why)))
      (let ((peer (drover-remote--peer name)))
        (drover-remote-items--note key :asked (float-time))
        (drover-remote--request
         peer (append (list :op "items" :key (drover-remote--wire-key session))
                      (and wait (list :wait t)))
         (lambda (reply error)
           (drover-with-reporting "remote items"
             (let* ((entry (drover-remote-items--entry key))
                    (busy (> (or (plist-get entry :pending) 0) 0))
                    (list (cond (error nil)
                                ;; A change asked meanwhile shows; its answer tells the list.
                                (busy (plist-get entry :list))
                                (t (drover-remote-items--install key name (plist-get reply :list)
                                                                 (drover-remote-items--offset peer)))))
                    (failure (cond (error error)
                                   ((eq list 'invalid) "it told a list of no known shape"))))
               (cond
                (failure
                 (drover-remote-items--note key :asked nil :failure failure)
                 (drover-items--changed key))
                (busy (drover-remote-items--note key :asked nil)))
               (when (and (not failure) (not wait) (plist-get list :more))
                 (when-let* ((session (drover-index-session key)))
                   (drover-remote-items-read session nil t)))
               (when callback
                 (funcall callback (if failure (list :failure failure) (list :list list)))))))
         (+ (float-time) drover-remote-request-timeout (if wait drover-remote-items-wait 0)))))))

(defun drover-remote-items--answered (key name reply offset)
  "Take REPLY, the answer of the host NAME to a change of the list of KEY.
Its list is kept once no other change asked is awaited.  OFFSET is how
far NAME's clock is ahead.  Return the list kept; nil while another
change is awaited, and for an answer without a list, as one given again
after its stream ended."
  (when (zerop (or (plist-get (drover-remote-items--entry key) :pending) 0))
    (let ((list (and (consp (plist-get reply :list))
                     (drover-remote-items--install key name (plist-get reply :list) offset))))
      (and (not (eq list 'invalid)) list))))

(defun drover-remote-items--read-again (key)
  "Ask its host again for the list of the session KEY, when it is listed still."
  (when-let* ((session (drover-index-session key)))
    (drover-remote-items-read session)))

(defun drover-remote-items--refused (reply)
  "Return why the host answering REPLY refused, lines, or nil when it did not.
Its :refused tells each reason, a line each: a place it cannot use, an
item it does not keep."
  (let ((refused (plist-get reply :refused)))
    (and (vectorp refused)
         (or (seq-keep (lambda (line) (drover-remote--line line 600)) (seq-take refused 200))
             (list "refused")))))

(defun drover-remote-items-set-goes (session number goes)
  "Have the host of SESSION say the item NUMBER of its list goes to GOES.
GOES is one of `drover-items-destinations', the user's choice.  It
shows here at once, and the host is asked without being waited for:
what it keeps shows once it answered the last change asked; a refusal
is warned of, and the list read from there again."
  (drover-remote-items--load)
  (when-let* ((why (drover-remote-items-refusal session "change")))
    (user-error "%s" why))
  (let* ((key (drover-session-key session))
         (name (drover-session-host session))
         (peer (drover-remote--peer name))
         (entry (drover-remote-items--entry key))
         (title (drover-bound-line (format "%s" (drover-session-display-title session)) 60)))
    (when-let* ((item (drover-items--find (plist-get entry :list) number)))
      (plist-put item :goes goes))
    (drover-remote-items--note key :pending (1+ (or (plist-get entry :pending) 0)))
    (drover-items--changed key)
    (drover-remote--request
     peer (list :op "items-goes" :key (drover-remote--wire-key session) :n number :goes (symbol-name goes))
     (lambda (reply error)
       (drover-with-reporting "remote items"
         (let ((pending (max 0 (1- (or (plist-get (drover-remote-items--entry key) :pending) 1)))))
           (drover-remote-items--note key :pending pending)
           (when error
             (drover-warn "Item %d of %s goes where it went: %s" number title error))
           (unless (or (drover-remote-items--answered key name (and (not error) reply)
                                                      (drover-remote-items--offset peer))
                       (> pending 0))
             (drover-remote-items--read-again key))))))))

(defun drover-remote-items-apply (session places callback)
  "Have the host of SESSION apply where the items of its list go.
PLACES is an alist (NUMBER . PLACE) of the place of the heading of each
item that needs one, as `drover-handoff-read-place' returns it: its
Org ID travels, which that host finds the heading by among its own Org
files, and its name; a nil PLACE leaves the place to the session's
agent.  Nothing is waited for: CALLBACK gets (:count COUNT :waits
WAITS) once the host told its session of COUNT items, WAITS non-nil
when the prompt waits for a turn to end, or (:failure WHY), WHY naming
each place it cannot use."
  (drover-remote-items--load)
  (let* ((key (drover-session-key session))
         (name (drover-session-host session))
         (why (drover-remote-items-refusal session "apply")))
    (if why
        (funcall callback (list :failure why))
      (let ((peer (drover-remote--peer name)))
        (drover-remote--request
         peer (list :op "items-apply" :id (drover-remote--action-id peer)
                    :key (drover-remote--wire-key session)
                    :places (vconcat (mapcar (lambda (entry)
                                               (list :n (car entry)
                                                     :id (or (drover-remote--plain (plist-get (cdr entry) :id)) :false)
                                                     :name (or (drover-remote--plain (plist-get (cdr entry) :name))
                                                               :false)))
                                             places)))
         (lambda (reply error)
           (drover-with-reporting "remote items"
             (let ((refused (and (not error) (drover-remote-items--refused reply))))
               (cond
                (error (funcall callback (list :failure error)))
                (refused (funcall callback (list :failure (string-join refused "; "))))
                (t
                 (unless (drover-remote-items--answered key name reply (drover-remote-items--offset peer))
                   (drover-remote-items--read-again key))
                 (funcall callback (list :count (if (natnump (plist-get reply :sent)) (plist-get reply :sent) 0)
                                         :waits (eq (plist-get reply :waits) t))))))))
         (+ (float-time) drover-remote-items-timeout))))))

(defun drover-remote-items--wire-given (given)
  "Return GIVEN, the items an agent gives, as they travel to the list's host.
GIVEN is as `drover-items-check' takes it.  Each item carries :i, its
place in the call, the fields given, an empty text for one cleared and
true for a value that is neither a text nor a number, :unknown and
:malformed."
  (vconcat
   (mapcar (lambda (item)
             (let ((wire (list :i (plist-get item :index))))
               (dolist (key drover-items-fields)
                 (when (plist-member item key)
                   (let ((value (plist-get item key)))
                     (setq wire
                           (nconc wire
                                  (list key
                                        (cond ((stringp value)
                                               (string-limit (substring-no-properties value)
                                                             drover-remote-items-field-limit))
                                              ((integerp value) value)
                                              ((memq value '(nil :json-false :false)) "")
                                              (t t))))))))
               (when (plist-get item :malformed)
                 (setq wire (nconc wire (list :malformed t))))
               (when-let* ((unknown (plist-get item :unknown)))
                 (setq wire (nconc wire (list :unknown (vconcat (mapcar (lambda (name) (format "%s" name))
                                                                        (seq-take unknown 20)))))))
               wire))
           (seq-take given drover-items-limit))))

(defun drover-remote-items-give (session given review callback)
  "Have the host of SESSION keep GIVEN, the items an agent gives its list.
GIVEN is as `drover-items-check' takes it; that host checks it, where
the links and headings of its items are, and keeps it.  REVIEW non-nil
asks its user to review the list.  Nothing is waited for: CALLBACK gets
\(:list LIST), the list as kept, or (:failure WHY), with :problems, what
refuses each item, strings, when that host refused the items."
  (drover-remote-items--load)
  (let* ((key (drover-session-key session))
         (name (drover-session-host session))
         (why (drover-remote-items-refusal session "give")))
    (if why
        (funcall callback (list :failure why))
      (let ((peer (drover-remote--peer name)))
        (drover-remote--request
         peer (list :op "items-set" :id (drover-remote--action-id peer)
                    :key (drover-remote--wire-key session)
                    :items (drover-remote-items--wire-given given)
                    :review (if review t :false))
         (lambda (reply error)
           (drover-with-reporting "remote items"
             (let ((refused (and (not error) (drover-remote-items--refused reply))))
               (cond
                (error (funcall callback (list :failure error)))
                (refused (funcall callback (list :failure (string-join refused "; ") :problems refused)))
                (t
                 (let ((offset (drover-remote-items--offset peer)))
                   (if-let* ((list (drover-remote-items--answered key name reply offset)))
                       (funcall callback (list :list list))
                     ;; Kept there, its answer given again without the list: read.
                     (drover-remote-items-read
                      session (lambda (result) (funcall callback result))))))))))
         (+ (float-time) drover-remote-items-timeout))))))

(defun drover-remote-items--peer-keys (name)
  "Return the keys here of the lists read of the sessions of the host NAME."
  (let ((backend (drover-remote--backend-name name))
        (keys nil))
    (maphash (lambda (key _entry)
               (when (eq (car key) backend)
                 (push key keys)))
             drover-remote-items--lists)
    keys))

(defun drover-remote-items--store-reviews (name value offset)
  "Keep VALUE, the reviews the host NAME told wait, its clock OFFSET ahead.
VALUE is a vector of [KEY SINCE]; the rows of the sessions whose review
began or ended are drawn again."
  (let ((old (gethash name drover-remote-items--reviews))
        (new (make-hash-table :test #'equal))
        (changed nil))
    (when (vectorp value)
      (seq-doseq (entry (seq-take value drover-remote-items-reviews-limit))
        (when-let* (((and (vectorp entry) (length= entry 2)))
                    (there (drover-remote--key (aref entry 0)))
                    (since (drover-remote--time (aref entry 1) offset)))
          (puthash there since new))))
    (maphash (lambda (there since)
               (unless (equal (and old (gethash there old)) since)
                 (push there changed)))
             new)
    (when old
      (maphash (lambda (there _since)
                 (unless (gethash there new)
                   (push there changed)))
               old))
    (puthash name new drover-remote-items--reviews)
    (drover-remote-items--tell-rows)
    (when changed
      (drover-views-changed (mapcar (lambda (there) (drover-remote-session-key name there)) changed)))))

;;;###autoload
(defun drover-remote-items-follow (peer)
  "Follow PEER's lists of items: the reviews waiting now, and each change.
The lists read of its sessions are read again: they may have changed
while it was not connected."
  (let ((name (drover-remote--peer-name peer)))
    (drover-remote--request
     peer (list :op "items" :follow t)
     (lambda (reply error)
       (drover-with-reporting "remote items"
         (unless error
           (drover-remote-items--store-reviews name (plist-get reply :reviews)
                                               (drover-remote-items--offset peer))
           (dolist (key (drover-remote-items--peer-keys name))
             (if-let* ((session (drover-index-session key)))
                 (drover-remote-items-read session)
               (unless (> (or (plist-get (drover-remote-items--entry key) :pending) 0) 0)
                 (remhash key drover-remote-items--lists)))))))
     (+ (float-time) drover-remote-request-timeout))))

;;;###autoload
(defun drover-remote-items-on-message (peer message)
  "Take MESSAGE, PEER's `items' message: a list of its sessions changed.
Its :k names the sessions showing it, its :a since when it waits for a
review, and its :u when it changed.  The rows of those sessions tell
the review; a list read here that changed since is read again."
  (let* ((name (drover-remote--peer-name peer))
         (since (drover-remote--time (plist-get message :a) (drover-remote-items--offset peer)))
         (stamp (drover-remote--number (plist-get message :u)))
         (sessions (plist-get message :k)))
    (when (vectorp sessions)
      (seq-doseq (wire (seq-take sessions 8))
        (when-let* ((there (drover-remote--key wire)))
          (drover-remote-items--review name there since)
          (let* ((key (drover-remote-session-key name there))
                 (entry (drover-remote-items--entry key)))
            ;; While a change asked here is awaited, its answer tells the list.
            (when (and entry
                       (zerop (or (plist-get entry :pending) 0))
                       (not (and stamp (equal stamp (plist-get (plist-get entry :list) :stamp)))))
              (drover-remote-items--read-again key))))))))

(defun drover-remote-items-unload-function ()
  "Stop telling peers of this host's lists, and rows of other hosts' reviews.
The lists stay with the hosts keeping them."
  (remove-hook 'drover-items-changed-hook #'drover-remote-items--list-changed)
  (remove-hook 'drover-changed-hook #'drover-remote-items--sessions-changed)
  (when (timerp drover-remote-items--timer)
    (cancel-timer drover-remote-items--timer))
  (setq drover-remote-items--timer nil
        drover-remote-items--changed nil)
  (when (eq (bound-and-true-p drover-list-review-function) #'drover-remote-items--row-review)
    (setq drover-list-review-function nil))
  nil)

(provide 'drover-remote-items)
;;; drover-remote-items.el ends here
