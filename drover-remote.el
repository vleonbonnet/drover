;;; drover-remote.el --- Sessions of other hosts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools, comm

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

;; Drover on one host lists the sessions of drover on others, each host
;; the authority over the sessions it runs.  `drover-remote-mode' talks
;; to the hosts of `drover-remote-peers' over spyglass connections, on
;; its channel "drover": each host sends the others the sessions its own
;; backends follow, never those it learnt from another host, so that
;; nothing loops; each peer is a backend here, `remote:NAME', whose
;; sessions are the owner's records as last received, their `host' its
;; name and their `origin' the owner's backend.
;;
;; The owner numbers the changes of its sessions: a snapshot, then
;; numbered updates, acknowledged, a session changing many times while
;; its peer lags being sent once.  A peer connecting again asks for the
;; changes since the revision it holds, and gets them, or a snapshot.
;; Either host may dial; when both do, both keep the connection dialed
;; by the host whose name sorts first.  The sessions of a peer not
;; connected stay listed, dimmed, as last received, and out of the
;; mode-line count.  Records are installed in slices from timers, and
;; sent in slices too: no step blocks Emacs long.
;;
;; This first step is read-only: recaps and previews come through the
;; owner, and opening a session shows what the owner tells of it.  See
;; doc/remote.org for the whole design.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 7))
  (load "drover-core" nil t))

;; Spyglass is loaded as the mode starts, not to compile this file.
(defvar spyglass-instance-id)
(defvar spyglass-remote-registration-file)
(declare-function spyglass-server "ext:spyglass-conn" ())
(declare-function spyglass-server-start "ext:spyglass-conn" (&rest arguments))
(declare-function spyglass-server-stop "ext:spyglass-conn" ())
(declare-function spyglass-connect "ext:spyglass-conn" (transport &rest arguments))
(declare-function spyglass-connections "ext:spyglass-conn" ())
(declare-function spyglass-disconnect "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-live-p "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-state "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-transport "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-clock-offset "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-rtt "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-attempts "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-last-recv "ext:spyglass-conn" (connection))
(declare-function spyglass-connection-set-heartbeat "ext:spyglass-conn" (connection interval timeout))
(declare-function spyglass-connection-drop-stream "ext:spyglass-conn" (connection))
(defvar spyglass-error-functions)
(declare-function spyglass-file-transport "ext:spyglass-conn" (&optional file))
(declare-function spyglass-ssh-transport "ext:spyglass-conn" (host &rest arguments))
(declare-function spyglass-register-channel "ext:spyglass-channel" (name handler &rest arguments))
(declare-function spyglass-unregister-channel "ext:spyglass-channel" (name))
(declare-function spyglass-channel-send "ext:spyglass-channel" (connection channel message))
(declare-function spyglass-channel-request "ext:spyglass-channel"
                  (connection channel message callback &optional timeout))
(declare-function spyglass-wire-encode "ext:spyglass-wire" (value &optional depth))
(declare-function spyglass-wire-decode "ext:spyglass-wire" (data &optional depth))
(declare-function spyglass-wire-sanitize-string "ext:spyglass-wire" (string &optional depth))
(declare-function spyglass-wire-random-token "ext:spyglass-wire" ())
(defvar drover-links-remote-function)

;;;; Options

(defgroup drover-remote nil
  "Sessions of other hosts."
  :group 'drover
  :prefix "drover-remote-")

(defcustom drover-remote-peers nil
  "The hosts this Emacs exchanges sessions with, an alist (NAME . PROPERTIES).
NAME is the host's name, its `drover-host' there.  PROPERTIES is a plist:

 :label         How views name it; NAME by default.
 :dial          Non-nil to connect to it; otherwise this Emacs waits for
                it to connect.
 :ssh           The host as SSH names it; NAME by default.
 :program       The SSH program, such as \"myssh\"; by default
                `drover-remote-ssh-program'.
 :args          More arguments of the SSH program, a list of strings.
 :registration  The file where spyglass registers its server there, as
                the host's shell names it; by default
                `spyglass-remote-registration-file'.
 :local         The registration file of a spyglass server of this
                host to connect to instead, without SSH: for another
                Emacs of this host.
 :access        What it may do with this host's sessions: `read', `act'
                or `full'.  This step serves reading alone.
 :aliases       Other names of it, in links.

A host connecting under a name not here is refused, unless
`drover-remote-accept-unknown' says otherwise.  Change it, then call
`drover-remote-reconfigure' or enable `drover-remote-mode' again."
  :type '(alist :key-type (string :tag "Host")
                :value-type (plist :tag "Properties"))
  :group 'drover-remote)

(defcustom drover-remote-ssh-program "ssh"
  "The SSH program reaching peers whose :program is not set."
  :type 'string
  :group 'drover-remote)

(defcustom drover-remote-accept-unknown nil
  "What a host connecting under a name not in `drover-remote-peers' may do.
Nil refuses it; `read', `act' or `full' accept it with that access."
  :type '(choice (const :tag "Refuse" nil)
                 (const :tag "Read" read)
                 (const :tag "Act" act)
                 (const :tag "Full" full))
  :group 'drover-remote)

(defcustom drover-remote-standby-delay 30
  "Seconds a peer stays unreachable before this Emacs dials it again.
When both hosts dial, one connection is kept, the other closed: its
dialer stands by, and dials again once the kept connection has been
gone this long."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-heartbeat '(5 . 20)
  "Heartbeats of the connections to peers, as (INTERVAL . TIMEOUT) in seconds.
A silent peer is dropped after TIMEOUT, and its sessions dimmed."
  :type '(cons (number :tag "Interval") (number :tag "Timeout"))
  :group 'drover-remote)

(defcustom drover-remote-request-timeout 20
  "Seconds a request to a peer, such as a recap, waits for its answer."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-protocol 1
  "The version of drover's protocol between hosts.")

(defconst drover-remote-channel "drover"
  "The spyglass channel drover talks on.")

(defconst drover-remote-batch 100
  "Records at most in one message of a snapshot or an update.")

(defconst drover-remote-window 4
  "Messages of a batch sent to a peer and not acknowledged, at most.")

(defconst drover-remote-tombstones 4096
  "Sessions gone whose revisions the owner keeps, for peers coming back.")

(defvar drover-remote-apply-slice 0.008
  "Seconds a slice installing records received may take.")

(defvar drover-remote-send-slice 0.004
  "Seconds a slice serializing records to send may take.")

(defconst drover-remote--name-regexp "\\`[A-Za-z0-9][A-Za-z0-9._-]\\{0,63\\}\\'"
  "What a host's name may be.")

(defconst drover-remote--symbol-regexp "\\`[a-z][a-z0-9:_-]\\{0,47\\}\\'"
  "What the name of a backend or an engine received may be, to be interned.")

;;;; State

(cl-defstruct (drover-remote--peer (:constructor drover-remote--peer-make)
                                   (:copier nil))
  "A host this Emacs exchanges sessions with."
  (name nil :documentation "Its name.")
  (properties nil :documentation "Its entry of `drover-remote-peers', a plist.")
  (ephemeral nil :documentation "Non-nil for a host accepted though not configured.")
  (backend nil :documentation "The symbol of its proxy backend, remote:NAME.")
  (records nil :documentation "Its sessions here, by the owner's key.")
  (dialed nil :documentation "The connection this Emacs dialed to it, or nil.")
  (candidates nil :documentation "Connections it greeted on, as (CONNECTION . HELLO).")
  (connection nil :documentation "The connection kept, or nil.")
  (state 'waiting :documentation "connecting, open, reconnecting, waiting or closed.")
  (heard nil :documentation "When it was last heard from, a float time.")
  (error nil :documentation "The last error, a line, or nil.")
  (epoch nil :documentation "The owner's epoch of the records held.")
  (rev nil :documentation "The owner's revision the records are at.")
  (stale nil :documentation "Non-nil while its records are stale.")
  (queue nil :documentation "Messages received and not applied, oldest first.")
  (position 0 :documentation "Records of the first message of QUEUE applied.")
  (worker nil :documentation "Timer applying the queue.")
  (snapshot nil :documentation "Hash table of the keys a snapshot in progress listed.")
  (standby nil :documentation "Timer dialing it again.")
  (prefetch nil :documentation "Keys of records whose details to fetch, and its timer.")
  (fetches nil :documentation "Older and archived sessions asked for, as (KIND GENERATION CALLBACK TABLE).
TABLE holds, by source, the records each of the owner's backends answered.")
  (asks nil :documentation "Requests whose stream ended before their answer, to ask again.
Each is (MESSAGE CALLBACK DEADLINE), oldest first.")
  (stats nil :documentation "Counters and measures, a plist."))

(cl-defstruct (drover-remote--sub (:constructor drover-remote--sub-make)
                                  (:copier nil))
  "A peer subscribed to this host's sessions, on one connection."
  (connection nil :documentation "The connection it subscribed on.")
  (peer nil :documentation "Its name.")
  (held nil :documentation "The revision it holds once a snapshot or update was sent.")
  (dirty nil :documentation "Hash table of the keys changed since last sent.")
  (kind nil :documentation "The batch being sent: `snap', `up', or nil.")
  (keys nil :documentation "Keys of the batch not sent yet.")
  (from nil :documentation "The revision the batch goes from.")
  (to nil :documentation "The revision the batch goes to.")
  (first nil :documentation "Non-nil until the batch's first message is sent.")
  (sent 0 :documentation "The last message number sent.")
  (acked 0 :documentation "The last message number acknowledged.")
  (timer nil :documentation "Timer sending what is due.")
  (stats nil :documentation "Counters and measures, a plist."))

(cl-defstruct (drover-remote--state (:constructor drover-remote--state-make)
                                    (:copier nil))
  "Everything the remote layer keeps."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-remote--state 'drover-remote--peer 'drover-remote--sub
                        'drover-session)
         :documentation "The layouts of the types it was made of.")
  (peers (make-hash-table :test #'equal) :documentation "Peers by name.")
  (order nil :documentation "Names of the peers, in the order configured.")
  (hellos (make-hash-table :test #'eq :weakness 'key)
          :documentation "The greeting of each connection's peer, a plist, by connection.")
  (subs (make-hash-table :test #'eq :weakness 'key)
        :documentation "Peers subscribed, by connection.")
  (streams (make-hash-table :test #'eq :weakness 'key)
           :documentation "How many streams each connection had begun or ended, by connection.")
  (epoch nil :documentation "This host's epoch, made anew each time the mode starts.")
  (rev 0 :documentation "This host's last revision.")
  (revs (make-hash-table :test #'equal) :documentation "Revision of each session served, by key.")
  (sent (make-hash-table :test #'equal)
        :documentation "What each session served was last, as (RECORD . PARENT-KEY), by key.")
  (tombs (make-hash-table :test #'equal) :documentation "Revision each key went at, by key.")
  (tomb-order nil :documentation "The tombstones, oldest first, as a queue (HEAD . TAIL).")
  (tomb-count 0 :documentation "How many tombstones are kept.")
  (floor 0 :documentation "The revision before which tombstones were dropped.")
  (unsynced t :documentation "Non-nil when changes went unnoted, no peer subscribed.")
  (started-server nil :documentation "Non-nil when the mode started spyglass's server."))

(drover-note-layout 'drover-remote--peer 'drover-remote--sub 'drover-remote--state)

(defvar drover-remote--state nil
  "The state of the remote layer while `drover-remote-mode' is on.")

(defun drover-remote--current ()
  "Return the state of the remote layer, or nil when it does not run.
A state made by another version of drover is dropped: the mode starts
anew, and peers greet each other again."
  (when drover-remote--state
    (if (drover-layout-current-p drover-remote--state 'drover-remote--state)
        drover-remote--state
      (setq drover-remote--state nil)
      (drover-stale-layout "the state of the remote layer")
      (when (bound-and-true-p drover-remote-mode)
        (drover-remote--start))
      drover-remote--state)))

(defun drover-remote--stat (object key &optional amount)
  "Add AMOUNT, 1 by default, to counter KEY of OBJECT, a peer or a sub."
  (if (drover-remote--peer-p object)
      (setf (drover-remote--peer-stats object)
            (plist-put (drover-remote--peer-stats object) key
                       (+ (or (plist-get (drover-remote--peer-stats object) key) 0) (or amount 1))))
    (setf (drover-remote--sub-stats object)
          (plist-put (drover-remote--sub-stats object) key
                     (+ (or (plist-get (drover-remote--sub-stats object) key) 0) (or amount 1))))))

(defun drover-remote--stat-max (object key value)
  "Keep the largest VALUE seen as KEY of OBJECT, a peer or a sub."
  (if (drover-remote--peer-p object)
      (when (> value (or (plist-get (drover-remote--peer-stats object) key) 0))
        (setf (drover-remote--peer-stats object)
              (plist-put (drover-remote--peer-stats object) key value)))
    (when (> value (or (plist-get (drover-remote--sub-stats object) key) 0))
      (setf (drover-remote--sub-stats object)
            (plist-put (drover-remote--sub-stats object) key value)))))

(defun drover-remote--peer (name)
  "Return the peer called NAME, or nil."
  (when-let* ((state (drover-remote--current)))
    (gethash name (drover-remote--state-peers state))))

(defun drover-remote--peers ()
  "Return the peers, in the order configured."
  (when-let* ((state (drover-remote--current)))
    (delq nil (mapcar (lambda (name) (gethash name (drover-remote--state-peers state)))
                      (drover-remote--state-order state)))))

(defun drover-remote--find-peer (name)
  "Return the peer NAME names, by its name or an alias, ignoring case, or nil."
  (and (stringp name)
       (seq-find (lambda (peer)
                   (seq-some (lambda (candidate)
                               (and (stringp candidate) (string-equal-ignore-case candidate name)))
                             (cons (drover-remote--peer-name peer)
                                   (plist-get (drover-remote--peer-properties peer) :aliases))))
                 (drover-remote--peers))))

(defun drover-remote--label (peer)
  "Return how views name PEER."
  (let ((label (plist-get (drover-remote--peer-properties peer) :label)))
    (if (and (stringp label) (not (string-empty-p label)))
        label
      (drover-remote--peer-name peer))))

(defun drover-remote--access (peer)
  "Return what PEER may do with this host's sessions: `read', `act' or `full'."
  (let ((access (plist-get (drover-remote--peer-properties peer) :access)))
    (if (memq access '(read act full)) access 'read)))

;;;; Records on the wire

(defun drover-remote--number (value)
  "Return VALUE when it is a number JSON carries, else nil."
  (and (numberp value)
       (or (integerp value)
           (not (or (isnan value) (= value 1.0e+INF) (= value -1.0e+INF))))
       (or (floatp value) (<= (abs value) 9007199254740991))
       value))

(defun drover-remote--plain (value)
  "Return VALUE, a string, without properties, or nil when it is not one."
  (and (stringp value) (substring-no-properties value)))

(defun drover-remote--faced (value)
  "Return VALUE, a string, as it travels with its faces, or nil."
  (when (stringp value)
    (if (seq-some #'caddr (object-intervals value))
        (spyglass-wire-encode value 6)
      (substring-no-properties value))))

(defun drover-remote--wire-id (id)
  "Return ID, an identifier of a session, as it travels, or nil when it cannot."
  (cond ((stringp id) (substring-no-properties id))
        ((integerp id) (drover-remote--number id))))

(defun drover-remote-encode-record (key record &optional parent)
  "Return RECORD, the session KEY of this host, as it travels to peers.
PARENT is the key of its parent among this host's sessions, if listed.
A plist JSON carries: the slots of `drover-session' that are set, by
name, times on this host's clock, and the key as K.  Nil when KEY's
identifier cannot travel."
  (when-let* ((id (drover-remote--wire-id (cdr key))))
    ;; Built backwards, reversed at the end.
    (let ((out (list (vector (symbol-name (car key)) id) :k)))
      (cl-flet ((put (name value) (when value (setq out (cons value (cons name out))))))
        (put :title (drover-remote--plain (drover-session-title record)))
        (put :directory (drover-remote--plain (drover-session-directory record)))
        (put :project (drover-remote--plain (drover-session-project record)))
        (put :branch (drover-remote--plain (drover-session-branch record)))
        ;; Read here, where the repository is: the peer never looks at
        ;; its own file system for a session of this host.
        (put :default-branch (and (drover-session-branch record)
                                  (drover-remote--plain
                                   (ignore-errors (drover-git-default-branch (drover-session-directory record))))))
        (let ((parent-id (and parent (drover-remote--wire-id (cdr parent)))))
          (cond (parent-id (put :parent (vector (symbol-name (car parent)) parent-id)))
                ((drover-remote--wire-id (drover-session-parent record))
                 (put :parent (vector (symbol-name (car key))
                                      (drover-remote--wire-id (drover-session-parent record)))))))
        (put :status (and (drover-session-status record) (symbol-name (drover-session-status record))))
        (put :requests
             (let ((requests (seq-filter #'consp (seq-take (drover-session-requests record) 20))))
               (and requests
                    (vconcat (mapcar (lambda (request)
                                       (list :kind (if (eq (plist-get request :kind) 'question)
                                                       "question" "permission")
                                             :summary (or (drover-remote--plain (plist-get request :summary))
                                                          "")))
                                     requests)))))
        (put :since (drover-remote--number (drover-session-since record)))
        (put :updated (drover-remote--number (drover-session-updated record)))
        (put :created (drover-remote--number (drover-session-created record)))
        (put :agent (drover-remote--plain (drover-session-agent record)))
        (put :model (drover-remote--plain (drover-session-model record)))
        (put :effort (drover-remote--plain (drover-session-effort record)))
        (put :effort-default (and (drover-session-effort-default record) t))
        (put :provider (drover-remote--faced (drover-session-provider record)))
        (put :provider-name (drover-remote--plain (drover-session-provider-name record)))
        (put :context-tokens (drover-remote--number (drover-session-context-tokens record)))
        (put :context-limit (drover-remote--number (drover-session-context-limit record)))
        (put :context-tier (pcase (drover-session-context-tier record)
                             (`(,state ,size ,label)
                              (and (memq state '(near over))
                                   (vector (symbol-name state) (or (drover-remote--number size) 0)
                                           (or (drover-remote--plain label) ""))))))
        (put :detail (drover-remote--plain (drover-session-detail record)))
        (put :engine (and (drover-session-engine record) (symbol-name (drover-session-engine record))))
        (put :engine-id (drover-remote--wire-id (drover-session-engine-id record)))
        (put :cache-expires (drover-remote--number (drover-session-cache-expires record)))
        (put :cache-ttl (drover-remote--number (drover-session-cache-ttl record)))
        (put :cache-tokens (drover-remote--number (drover-session-cache-tokens record)))
        (put :cache-prompt-tokens (drover-remote--number (drover-session-cache-prompt-tokens record)))
        (put :agent-display (drover-remote--faced (drover-session-agent-display record)))
        (put :context-estimated (and (drover-session-context-estimated record) t))
        (put :archived (let ((archived (drover-session-archived record)))
                         (if (eq archived t) t (drover-remote--number archived)))))
      (nreverse out))))

(defun drover-remote--symbol (value)
  "Return the symbol VALUE, a string received, names, or nil when it may not.
Only short names of lowercase letters, digits and dashes are interned."
  (and (stringp value)
       (string-match-p drover-remote--symbol-regexp value)
       (intern value)))

(defun drover-remote--id (value)
  "Return VALUE, an identifier received, or nil when it is not one."
  (cond ((and (stringp value) (<= (length value) 1024)) value)
        ((integerp value) value)))

(defun drover-remote--key (value)
  "Return VALUE, a key received as [BACKEND ID], as (BACKEND . ID), or nil."
  (and (vectorp value) (= (length value) 2)
       (when-let* ((backend (drover-remote--symbol (aref value 0)))
                   (id (drover-remote--id (aref value 1))))
         (cons backend id))))

(defun drover-remote--line (value &optional limit)
  "Return VALUE, a string received, as a line of LIMIT characters at most."
  (and (stringp value) (drover-bound-line value limit)))

(defun drover-remote--text (value)
  "Return VALUE, a string received, bounded as a text of several lines, or nil."
  (and (stringp value) (drover-bound-text value)))

(defun drover-remote--faced-value (value)
  "Return VALUE, a string with faces received, sanitized, on one line, or nil."
  (let ((string (if (stringp value)
                    value
                  (and (consp value) (ignore-errors (spyglass-wire-decode value 6))))))
    (and (stringp string)
         (drover-bound-line (spyglass-wire-sanitize-string
                             (if (> (length string) (* 2 drover-line-limit))
                                 (substring string 0 (* 2 drover-line-limit))
                               string)
                             0)))))

(defun drover-remote--time (value offset)
  "Return VALUE, a time of the owner's clock, on this host's, OFFSET behind."
  (when-let* ((time (drover-remote--number value)))
    (- time offset)))

(defconst drover-remote--statuses
  (seq-uniq (append drover-status-order opencode-session-statuses))
  "The statuses a record received may have.")

(defun drover-remote--status (value)
  "Return the status VALUE, a string received, names, else `idle'."
  (or (and (stringp value)
           (seq-find (lambda (status) (string= (symbol-name status) value))
                     (append drover-status-order drover-remote--statuses)))
      'idle))

(defun drover-remote--count (value)
  "Return VALUE when it is a natural number received, else nil."
  (and (natnump value) value))

(defun drover-remote-decode-record (data peer-name backend offset)
  "Return the record DATA, received from the host PEER-NAME, or nil.
DATA is a plist as `drover-remote-encode-record' makes it, parsed as
JSON; whatever it holds is checked and bounded, unknown members
ignored.  The record is of BACKEND, the host's proxy, its identifier the
owner's key; its times are moved OFFSET seconds, the owner's clock
minus this host's.  Nil when DATA names no session."
  (when-let* (((and (consp data) (keywordp (car data))))
              (key (drover-remote--key (plist-get data :k))))
    (let ((origin (car key))
          (parent (plist-get data :parent)))
      (drover--bound-record
       (drover-session-create
        :backend backend :id key :host peer-name :origin origin
        :title (drover-remote--line (plist-get data :title))
        :directory (drover-remote--line (plist-get data :directory) 1024)
        :project (drover-remote--line (plist-get data :project))
        :branch (drover-remote--line (plist-get data :branch))
        :default-branch (drover-remote--line (plist-get data :default-branch))
        :parent (and parent (drover-remote--key parent))
        :status (drover-remote--status (plist-get data :status))
        :requests (let ((requests (plist-get data :requests)))
                    (and (vectorp requests)
                         (delq nil (mapcar (lambda (request)
                                             (and (consp request) (keywordp (car request))
                                                  (list :kind (if (equal (plist-get request :kind) "question")
                                                                  'question 'permission)
                                                        :summary (or (drover-remote--text
                                                                      (plist-get request :summary))
                                                                     ""))))
                                           (seq-take requests 20)))))
        :since (drover-remote--time (plist-get data :since) offset)
        :updated (drover-remote--time (plist-get data :updated) offset)
        :created (drover-remote--time (plist-get data :created) offset)
        :agent (drover-remote--line (plist-get data :agent))
        :model (drover-remote--line (plist-get data :model))
        :effort (drover-remote--line (plist-get data :effort))
        :effort-default (eq (plist-get data :effort-default) t)
        :provider (drover-remote--faced-value (plist-get data :provider))
        :provider-name (drover-remote--line (plist-get data :provider-name))
        :context-tokens (drover-remote--count (plist-get data :context-tokens))
        :context-limit (drover-remote--count (plist-get data :context-limit))
        :context-tier (let ((tier (plist-get data :context-tier)))
                        (and (vectorp tier) (= (length tier) 3)
                             (member (aref tier 0) '("near" "over"))
                             (list (intern (aref tier 0))
                                   (or (drover-remote--count (aref tier 1)) 0)
                                   (or (drover-remote--line (aref tier 2) 20) ""))))
        :detail (drover-remote--text (plist-get data :detail))
        :engine (drover-remote--symbol (plist-get data :engine))
        :engine-id (let ((id (plist-get data :engine-id)))
                     (and (stringp id) (<= (length id) 1024) id))
        :cache-expires (drover-remote--time (plist-get data :cache-expires) offset)
        :cache-ttl (drover-remote--count (plist-get data :cache-ttl))
        :cache-tokens (drover-remote--count (plist-get data :cache-tokens))
        :cache-prompt-tokens (drover-remote--count (plist-get data :cache-prompt-tokens))
        :agent-display (drover-remote--faced-value (plist-get data :agent-display))
        :context-estimated (eq (plist-get data :context-estimated) t)
        :archived (let ((archived (plist-get data :archived)))
                    (if (eq archived t) t (drover-remote--time archived offset))))))))

;;;; Serving this host's sessions

(defun drover-remote--local-p (record)
  "Return non-nil when RECORD is a session of a backend of this host's own.
A host serves only those: nothing learnt from a peer goes to another."
  (and (null (drover-session-host record))
       (not (drover-backend-property (drover-session-backend record) :remote))))

(defun drover-remote--keeper-p (key record)
  "Return non-nil when RECORD, of KEY, is the record listed of its conversation.
Records of several backends showing one conversation are listed once,
as `drover-sessions' merges them: the others are not served."
  (let ((engine-key (drover-session-engine-key record)))
    (or (null engine-key)
        (let ((keys (gethash engine-key (drover--index-engines (drover-index)))))
          (or (null (cdr keys))
              (eq (car key)
                  (drover--preferred-backend (delq nil (mapcar #'drover-index-session keys)))))))))

(defun drover-remote--served (key)
  "Return what this host serves of session KEY, (RECORD . PARENT-KEY), or nil."
  (when-let* ((record (drover-index-session key))
              ((drover-remote--local-p record))
              ((drover-remote--keeper-p key record)))
    (cons record (drover-index-parent key))))

(defun drover-remote--tomb (state key rev)
  "Note in STATE that KEY went at revision REV, dropping the oldest tombstones."
  (puthash key rev (drover-remote--state-tombs state))
  (let ((cell (list (cons key rev)))
        (queue (drover-remote--state-tomb-order state)))
    (if queue
        (progn (setcdr (cdr queue) cell) (setcdr queue cell))
      (setf (drover-remote--state-tomb-order state) (cons cell cell))))
  (cl-incf (drover-remote--state-tomb-count state))
  (while (> (drover-remote--state-tomb-count state) drover-remote-tombstones)
    (let* ((queue (drover-remote--state-tomb-order state))
           (oldest (car (car queue))))
      (setcar queue (cdr (car queue)))
      (unless (car queue) (setf (drover-remote--state-tomb-order state) nil))
      (cl-decf (drover-remote--state-tomb-count state))
      (when (eql (gethash (car oldest) (drover-remote--state-tombs state)) (cdr oldest))
        (remhash (car oldest) (drover-remote--state-tombs state)))
      (setf (drover-remote--state-floor state)
            (max (drover-remote--state-floor state) (cdr oldest))))))

(defun drover-remote--note (state key)
  "Note in STATE what session KEY is now, giving it a revision if it changed.
Return non-nil when it changed: every subscribed peer then has it to
send.  A record changed is one not `eq' to the one noted, unless equal
to it, as a record copied when the index is made anew."
  (let* ((now (drover-remote--served key))
         (sent (gethash key (drover-remote--state-sent state)))
         (same (and now sent (equal (cdr now) (cdr sent))
                    (or (eq (car now) (car sent))
                        (equal-including-properties (car now) (car sent))))))
    (cond
     (same (puthash key now (drover-remote--state-sent state)) nil)
     ((and (null now) (null sent)) nil)
     (t
      (let ((rev (cl-incf (drover-remote--state-rev state))))
        (if now
            (progn (puthash key now (drover-remote--state-sent state))
                   (puthash key rev (drover-remote--state-revs state))
                   (remhash key (drover-remote--state-tombs state)))
          (remhash key (drover-remote--state-sent state))
          (remhash key (drover-remote--state-revs state))
          (drover-remote--tomb state key rev))
        (maphash (lambda (_connection sub) (puthash key t (drover-remote--sub-dirty sub)))
                 (drover-remote--state-subs state))
        t)))))

(defun drover-remote--sync-all (state)
  "Note in STATE every session of this host as it is now, and those gone."
  (let ((records (drover--index-records (drover-index)))
        (gone nil))
    (maphash (lambda (key _record) (drover-remote--note state key)) records)
    (maphash (lambda (key _) (unless (gethash key records) (push key gone)))
             (drover-remote--state-sent state))
    (dolist (key gone) (drover-remote--note state key))
    (setf (drover-remote--state-unsynced state) nil)))

(defun drover-remote--sync-keys (state keys)
  "Note in STATE the sessions KEYS, and those sharing their conversations."
  (let ((index (drover-index))
        (seen (make-hash-table :test #'equal)))
    (dolist (key keys)
      (unless (gethash key seen)
        (puthash key t seen)
        (drover-remote--note state key)
        ;; Which record of a shared conversation is served may change.
        (when-let* ((record (or (drover-index-session key)
                                (car (gethash key (drover-remote--state-sent state)))))
                    (engine-key (drover-session-engine-key record)))
          (dolist (other (gethash engine-key (drover--index-engines index)))
            (unless (gethash other seen)
              (puthash other t seen)
              (drover-remote--note state other))))))))

(defun drover-remote--owner-changed ()
  "Follow the changes of this host's sessions, for the peers subscribed.
For `drover-changed-hook'."
  (drover-with-reporting "remote sessions"
    (when-let* ((state (drover-remote--current)))
      (if (zerop (hash-table-count (drover-remote--state-subs state)))
          (setf (drover-remote--state-unsynced state) t)
        (let ((changed drover-changed-sessions))
          (if (or (eq changed t) (drover-remote--state-unsynced state))
              (drover-remote--sync-all state)
            (drover-remote--sync-keys state changed)))
        (maphash (lambda (_connection sub) (drover-remote--schedule-flush sub))
                 (drover-remote--state-subs state))))))

(defun drover-remote--subscribe (state connection peer message)
  "Serve this host's sessions to PEER, subscribed on CONNECTION with MESSAGE.
STATE is the remote layer's.  MESSAGE tells the epoch and revision it
holds: under this epoch, with the tombstones reaching back that far, it
gets the changes since; else a snapshot."
  (let ((old nil))
    (maphash (lambda (other sub)
               (when (and (not (eq other connection))
                          (equal (drover-remote--sub-peer sub) (drover-remote--peer-name peer)))
                 (push other old)))
             (drover-remote--state-subs state))
    (dolist (other old) (drover-remote--unsubscribe state other)))
  (drover-remote--unsubscribe state connection)
  (when (or (drover-remote--state-unsynced state)
            (zerop (hash-table-count (drover-remote--state-subs state))))
    (drover-remote--sync-all state))
  (let* ((epoch (plist-get message :epoch))
         (held (plist-get message :rev))
         (sub (drover-remote--sub-make :connection connection
                                       :peer (drover-remote--peer-name peer)
                                       :dirty (make-hash-table :test #'equal))))
    (puthash connection sub (drover-remote--state-subs state))
    (if (and (equal epoch (drover-remote--state-epoch state))
             (natnump held)
             (>= held (drover-remote--state-floor state))
             (<= held (drover-remote--state-rev state)))
        (progn
          (drover-remote--stat sub :resumes)
          (setf (drover-remote--sub-held sub) held)
          (maphash (lambda (key rev) (when (> rev held) (puthash key t (drover-remote--sub-dirty sub))))
                   (drover-remote--state-revs state))
          (maphash (lambda (key rev) (when (> rev held) (puthash key t (drover-remote--sub-dirty sub))))
                   (drover-remote--state-tombs state))
          ;; Resynchronized even when nothing changed meanwhile.
          (when (zerop (hash-table-count (drover-remote--sub-dirty sub)))
            (drover-remote--start-batch state sub 'up nil)))
      (drover-remote--stat sub :snapshots)
      (drover-remote--start-batch state sub 'snap
                                  (hash-table-keys (drover-remote--state-sent state))))
    (drover-remote--schedule-flush sub)))

(defun drover-remote--unsubscribe (state connection)
  "Stop serving the peer subscribed on CONNECTION, if any, in STATE."
  (when-let* ((sub (gethash connection (drover-remote--state-subs state))))
    (when (drover-remote--sub-timer sub)
      (cancel-timer (drover-remote--sub-timer sub)))
    (remhash connection (drover-remote--state-subs state))))

(defun drover-remote--start-batch (state sub kind keys)
  "Start sending SUB a batch of KIND, `snap' or `up', of the sessions KEYS.
The batch goes to the current revision of STATE."
  (setf (drover-remote--sub-kind sub) kind
        (drover-remote--sub-keys sub) keys
        (drover-remote--sub-from sub) (drover-remote--sub-held sub)
        (drover-remote--sub-to sub) (drover-remote--state-rev state)
        (drover-remote--sub-first sub) t))

(defun drover-remote--schedule-flush (sub)
  "Send SUB what is due, from a timer."
  (unless (drover-remote--sub-timer sub)
    (let ((link (drover-link sub)))
      (setf (drover-remote--sub-timer sub)
            (run-at-time 0 nil (lambda ()
                                 (let ((sub (drover-linked link)))
                                   (setf (drover-remote--sub-timer sub) nil)
                                   (drover-with-reporting "remote sending"
                                     (drover-remote--flush sub)))))))))

(defun drover-remote--flush (sub)
  "Send SUB what is due, for a slice at most, and come back for the rest.
A batch goes in messages of `drover-remote-batch' records, at most
`drover-remote-window' unacknowledged; the next batch starts once the
last one was acknowledged whole, so that a session changing many times
while the peer lags is sent once."
  (when-let* ((state (drover-remote--current))
              ((eq (gethash (drover-remote--sub-connection sub) (drover-remote--state-subs state)) sub))
              ((spyglass-connection-live-p (drover-remote--sub-connection sub))))
    (let ((start (float-time))
          (collections gcs-done)
          (deadline (+ (float-time) drover-remote-send-slice)))
      (catch 'done
        (while t
          (cond
           ((>= (- (drover-remote--sub-sent sub) (drover-remote--sub-acked sub)) drover-remote-window)
            (throw 'done nil))
           ((drover-remote--sub-kind sub)
            (drover-remote--send-message state sub deadline)
            (when (> (float-time) deadline)
              (drover-remote--schedule-flush sub)
              (throw 'done nil)))
           ((and (drover-remote--sub-held sub)
                 (> (hash-table-count (drover-remote--sub-dirty sub)) 0)
                 (= (drover-remote--sub-sent sub) (drover-remote--sub-acked sub)))
            (let ((keys (hash-table-keys (drover-remote--sub-dirty sub))))
              (clrhash (drover-remote--sub-dirty sub))
              (drover-remote--start-batch state sub 'up keys)))
           (t (throw 'done nil)))))
      ;; A garbage collection is Emacs's, whoever allocated: apart.
      (drover-remote--stat-max sub (if (= collections gcs-done) :slice-max :slice-gc-max)
                               (- (float-time) start)))))

(defun drover-remote--send-message (state sub deadline)
  "Send SUB the next message of its batch, until DEADLINE or a full one.
Records are read from STATE as they are now."
  (let ((records nil) (gone nil) (count 0))
    (while (and (drover-remote--sub-keys sub)
                (< count drover-remote-batch)
                (or (zerop count) (< (float-time) deadline)))
      (let* ((key (pop (drover-remote--sub-keys sub)))
             (served (gethash key (drover-remote--state-sent state)))
             (encoded (and served (drover-remote-encode-record key (car served) (cdr served)))))
        (cond (encoded (push encoded records) (cl-incf count))
              ((not served)
               (when-let* ((id (drover-remote--wire-id (cdr key))))
                 (push (vector (symbol-name (car key)) id) gone))))))
    (let* ((last (null (drover-remote--sub-keys sub)))
           (kind (drover-remote--sub-kind sub))
           (n (cl-incf (drover-remote--sub-sent sub)))
           (message (append (list :t (symbol-name kind) :n n
                                  :epoch (drover-remote--state-epoch state)
                                  :to (drover-remote--sub-to sub)
                                  :recs (vconcat (nreverse records)))
                            (if (eq kind 'snap)
                                (list :first (if (drover-remote--sub-first sub) t :false))
                              (list :from (drover-remote--sub-from sub)
                                    :gone (vconcat (nreverse gone))))
                            (list :last (if last t :false)))))
      (setf (drover-remote--sub-first sub) nil)
      (drover-remote--stat sub :messages)
      (drover-remote--stat sub :records count)
      (spyglass-channel-send (drover-remote--sub-connection sub) drover-remote-channel message)
      (when last
        (setf (drover-remote--sub-held sub) (drover-remote--sub-to sub)
              (drover-remote--sub-kind sub) nil)))))

(defun drover-remote--acked (state connection message)
  "Note in STATE the acknowledgment MESSAGE of the peer on CONNECTION."
  (when-let* ((sub (gethash connection (drover-remote--state-subs state)))
              (n (plist-get message :n))
              ((natnump n)))
    (setf (drover-remote--sub-acked sub) (max (drover-remote--sub-acked sub)
                                              (min n (drover-remote--sub-sent sub))))
    (drover-remote--flush sub)))

;;;; Connections and greetings

(defun drover-remote--send (connection message)
  "Send MESSAGE to CONNECTION's peer on drover's channel."
  (when (spyglass-connection-live-p connection)
    (spyglass-channel-send connection drover-remote-channel message)))

(defun drover-remote--hello (connection &optional ask)
  "Greet CONNECTION's peer; with ASK, asking it to greet back."
  (drover-remote--send connection
                       (list :t "hello" :v drover-remote-protocol :host drover-host
                             :inst spyglass-instance-id
                             :dialed (if (spyglass-connection-transport connection) t :false)
                             :ask (if ask t :false))))

(defun drover-remote--refuse (connection why &optional close)
  "Tell CONNECTION's peer that this host refuses it, because of WHY.
With CLOSE, close the connection too, a second later: closed at once,
the peer writing meanwhile would lose what it had not read, the reason."
  (drover-remote--send connection (list :t "refuse" :why why))
  (when close
    (run-at-time 1 nil (lambda ()
                         (drover-with-reporting "remote refusal"
                           (when (spyglass-connection-live-p connection)
                             (spyglass-disconnect connection)))))))

(defun drover-remote--peer-of (state connection)
  "Return the peer CONNECTION greeted as, in STATE, or nil."
  (when-let* ((hello (gethash connection (drover-remote--state-hellos state))))
    (gethash (plist-get hello :peer) (drover-remote--state-peers state))))

(defun drover-remote--dialer-of (state connection)
  "Return the peer CONNECTION was dialed to, in STATE, or nil."
  (seq-find (lambda (peer) (eq (drover-remote--peer-dialed peer) connection))
            (hash-table-values (drover-remote--state-peers state))))

(defun drover-remote--on-hello (state connection message)
  "Take the greeting MESSAGE of CONNECTION's peer, in STATE."
  (let* ((host (plist-get message :host))
         (version (plist-get message :v))
         (dialer (drover-remote--dialer-of state connection))
         (peer (and (stringp host) (drover-remote--find-peer host))))
    (cond
     ((not (and (integerp version) (= version drover-remote-protocol)))
      (drover-remote--fail dialer (format "%s speaks drover's protocol %s, this host %s"
                                          (drover-bound-line (format "%s" host)) version
                                          drover-remote-protocol))
      (drover-remote--refuse connection (format "protocol %s expected" drover-remote-protocol) t))
     ((not (and (stringp host) (string-match-p drover-remote--name-regexp host)))
      (drover-remote--refuse connection "no host name" t))
     ((drover-host-local-p host)
      (drover-remote--fail dialer (format "%s is this host's name" host))
      (drover-remote--refuse connection (format "%s is this host's name too" host) t))
     ((and dialer (not (eq peer dialer)))
      (drover-remote--fail dialer (format "dialed %s, %s answered" (drover-remote--peer-name dialer) host))
      (drover-remote--refuse connection "not the host dialed" t))
     ((and (null peer) (not drover-remote-accept-unknown))
      (drover-remote--refuse connection (format "%s does not know %s" drover-host host) t))
     (t
      (unless peer
        (setq peer (drover-remote--add-peer state (downcase host)
                                            (list :access drover-remote-accept-unknown) t)))
      (drover-remote--stat peer :hellos)
      ;; JSON's false is `:false', not nil: asked only when told so.
      (when (eq (plist-get message :ask) t)
        (drover-remote--hello connection))
      (puthash connection (list :peer (drover-remote--peer-name peer)
                                :inst (plist-get message :inst)
                                :ours (and (spyglass-connection-transport connection) t)
                                :time (float-time))
               (drover-remote--state-hellos state))
      (setf (alist-get connection (drover-remote--peer-candidates peer) nil 'remove) nil)
      (push (cons connection (gethash connection (drover-remote--state-hellos state)))
            (drover-remote--peer-candidates peer))
      (spyglass-connection-set-heartbeat connection (car drover-remote-heartbeat)
                                         (cdr drover-remote-heartbeat))
      (drover-remote--elect state peer)))))

(defun drover-remote--preferred-ours-p (peer)
  "Return non-nil when the connection kept with PEER is the one this host dials.
It is the one dialed by the host whose name sorts first."
  (string< drover-host (drover-remote--peer-name peer)))

(defun drover-remote--elect (state peer)
  "Choose the connection PEER's sessions come through, in STATE.
A connection of another Emacs of the same host, while one is kept, is
refused; of two connections of the same Emacs, the one dialed by the
host whose name sorts first is kept, and this host closes the other if
it dialed it, standing by."
  (let* ((live (seq-filter (lambda (candidate) (spyglass-connection-live-p (car candidate)))
                           (drover-remote--peer-candidates peer)))
         (current (assq (drover-remote--peer-connection peer) live))
         (ours (drover-remote--preferred-ours-p peer))
         (rank (lambda (candidate)
                 (list (if (eq (and (plist-get (cdr candidate) :ours) t) ours) 0 1)
                       (- (plist-get (cdr candidate) :time)))))
         (instance (plist-get (cdr (or current
                                       (car (seq-sort-by (lambda (c) (- (plist-get (cdr c) :time)))
                                                         #'< live))))
                              :inst))
         (same (seq-filter (lambda (c) (equal (plist-get (cdr c) :inst) instance)) live))
         (best (car (seq-sort (lambda (a b)
                                (let ((ra (funcall rank a)) (rb (funcall rank b)))
                                  (or (< (car ra) (car rb))
                                      (and (= (car ra) (car rb)) (< (cadr ra) (cadr rb))))))
                              same))))
    (setf (drover-remote--peer-candidates peer) live)
    (dolist (candidate live)
      (unless (equal (plist-get (cdr candidate) :inst) instance)
        (drover-remote--refuse (car candidate)
                               (format "another Emacs of %s is connected" (drover-remote--peer-name peer)))))
    (when (and best (not (eq (car best) (drover-remote--peer-connection peer))))
      (drover-remote--use state peer (car best)))
    ;; A connection this host dialed that lost: closed, the dialer standing by.
    (let ((dialed (drover-remote--peer-dialed peer)))
      (when (and best dialed (not (eq dialed (car best)))
                 (assq dialed same))
        (setf (drover-remote--peer-dialed peer) nil)
        (spyglass-disconnect dialed)))))

(defun drover-remote--use (state peer connection)
  "Have PEER's sessions come through CONNECTION, subscribing there.
STATE is the remote layer's."
  (ignore state)
  (drover-remote--reset-queue peer)
  (setf (drover-remote--peer-connection peer) connection
        (drover-remote--peer-heard peer) (float-time)
        (drover-remote--peer-error peer) nil)
  (drover-remote--set-state peer 'open)
  (when (drover-remote--peer-standby peer)
    (cancel-timer (drover-remote--peer-standby peer))
    (setf (drover-remote--peer-standby peer) nil))
  (drover-remote--stat peer :subscriptions)
  (drover-remote--send connection (list :t "sub"
                                        :epoch (drover-remote--peer-epoch peer)
                                        :rev (drover-remote--peer-rev peer)))
  (drover-remote--resume peer))

(defun drover-remote--resume (peer)
  "Ask PEER again, connected anew, what a stream ending lost.
The requests whose stream ended before their answer, the details of the
sessions lists showed, and the older and archived sessions while lists
show them, whose answers come in several messages, as when shown."
  (let ((asks (drover-remote--peer-asks peer)))
    (setf (drover-remote--peer-asks peer) nil)
    (pcase-dolist (`(,message ,callback ,deadline ,timer) asks)
      (cancel-timer timer)
      (drover-remote--request peer message callback deadline)))
  (when (car (drover-remote--peer-prefetch peer))
    (drover-remote--prefetch peer nil))
  (let ((backend (drover-remote--peer-backend peer)))
    (when drover-show-history (drover-history-fetch backend))
    (when drover-show-archived (drover-archived-fetch backend))))

(defun drover-remote--set-state (peer state)
  "Set PEER's STATE, redrawing what tells it when it changes."
  (unless (eq (drover-remote--peer-state peer) state)
    (setf (drover-remote--peer-state peer) state)
    (force-mode-line-update t)))

(defun drover-remote--fail (peer message)
  "Note MESSAGE, an error, as the last of PEER, if any."
  (when peer
    (setf (drover-remote--peer-error peer) (drover-bound-line message))))

(defvar drover-remote--write-grace 0.5
  "Seconds a stream a write failed on has to end, ending as streams do.")

(defun drover-remote--next-stream (state connection)
  "Note in STATE that a stream of CONNECTION began or ended."
  (puthash connection (1+ (gethash connection (drover-remote--state-streams state) 0))
           (drover-remote--state-streams state)))

(defun drover-remote--spyglass-error (connection message)
  "Follow MESSAGE, an error spyglass reports on CONNECTION, one of drover's.
For `spyglass-error-functions'.  A write failing finds its stream gone,
the peer gone or the stream dropped: the stream's end, which follows,
tells it, stale rows and `drover-remote-status', and the subscription
made anew resends what was lost; it is not reported.  Another error is
the peer's last, which they tell.  One leaving its stream open, past
`drover-remote--write-grace' seconds, is unexpected: it is reported on
one line, once in `drover-error-interval' seconds, and the stream of a
write lost is broken, so that a new one resynchronizes."
  (drover-with-reporting "remote connection"
    (when-let* ((state (drover-remote--current))
                (connection)
                (peer (or (drover-remote--dialer-of state connection)
                          (drover-remote--peer-of state connection))))
      (let ((line (drover-bound-line (format "%s" message)))
            (stream (gethash connection (drover-remote--state-streams state) 0)))
        (if (string-prefix-p "Sending failed" line)
            (drover-remote--stat peer :write-failures)
          (drover-remote--fail peer line))
        (when (spyglass-connection-live-p connection)
          (run-at-time drover-remote--write-grace nil
                       (let ((state-link (drover-link state))
                             (peer-link (drover-link peer)))
                         (lambda ()
                           (drover-with-reporting "remote connection"
                             (let ((state (drover-linked state-link))
                                   (peer (drover-linked peer-link)))
                               (when (and (eq state (drover-remote--current))
                                          (eql stream (gethash connection (drover-remote--state-streams state) 0))
                                          (spyglass-connection-live-p connection))
                                 (drover-remote--stat peer :unexpected)
                                 (when (drover--report-due-p (list "remote" (drover-remote--peer-name peer) line))
                                   (drover-warn "%s: %s" (drover-remote--label peer) line))
                                 (when (string-prefix-p "Sending failed" line)
                                   (spyglass-connection-drop-stream connection)))))))))))))

(defun drover-remote--attach (connection)
  "Greet the peer of CONNECTION, whose stream just opened."
  (drover-with-reporting "remote connection"
    (when-let* ((state (drover-remote--current)))
      (drover-remote--next-stream state connection)
      (remhash connection (drover-remote--state-hellos state))
      (when-let* ((peer (drover-remote--dialer-of state connection)))
        (setf (drover-remote--peer-error peer) nil))
      (drover-remote--hello connection))))

(defun drover-remote--detach (connection final)
  "Follow the end of CONNECTION's stream; FINAL when it does not come back."
  (drover-with-reporting "remote connection"
    (when-let* ((state (drover-remote--current)))
      (drover-remote--next-stream state connection)
      (drover-remote--unsubscribe state connection)
      (remhash connection (drover-remote--state-hellos state))
      (dolist (peer (hash-table-values (drover-remote--state-peers state)))
        (setf (drover-remote--peer-candidates peer)
              (assq-delete-all connection (drover-remote--peer-candidates peer)))
        (when (and final (eq connection (drover-remote--peer-dialed peer)))
          (setf (drover-remote--peer-dialed peer) nil))
        (when (eq connection (drover-remote--peer-connection peer))
          (setf (drover-remote--peer-heard peer)
                (max (or (drover-remote--peer-heard peer) 0)
                     (or (ignore-errors (spyglass-connection-last-recv connection)) 0))
                (drover-remote--peer-connection peer) nil)
          (drover-remote--reset-queue peer)
          (drover-remote--elect state peer)
          (unless (drover-remote--peer-connection peer)
            (drover-remote--lost peer)))
        (when (and (not (drover-remote--peer-connection peer))
                   (not (drover-remote--peer-dialed peer)))
          (drover-remote--stand-by peer))))))

(defun drover-remote--lost (peer)
  "Note that PEER is not connected: its sessions stay, stale."
  (drover-remote--set-state peer (if (drover-remote--peer-dialed peer) 'reconnecting 'waiting))
  (drover-remote--set-stale peer (or (drover-remote--peer-heard peer) (float-time))))

(defun drover-remote--stand-by (peer)
  "Dial PEER again once unreachable for `drover-remote-standby-delay' seconds.
Only a peer this host dials, and not when its configuration dropped it."
  (when (and (plist-get (drover-remote--peer-properties peer) :dial)
             (not (eq (drover-remote--peer-state peer) 'closed))
             (not (drover-remote--peer-standby peer)))
    (setf (drover-remote--peer-standby peer)
          (run-at-time drover-remote-standby-delay nil
                       (let ((link (drover-link peer)))
                         (lambda ()
                           (drover-with-reporting "remote standby"
                             (let ((peer (drover-linked link)))
                               (setf (drover-remote--peer-standby peer) nil)
                               (when (and (drover-remote--current)
                                          (eq peer (drover-remote--peer (drover-remote--peer-name peer)))
                                          (not (drover-remote--peer-connection peer))
                                          (not (drover-remote--peer-dialed peer)))
                                 (drover-remote--dial peer))))))))))

(defun drover-remote--transport (peer)
  "Return the spyglass transport reaching PEER."
  (let ((properties (drover-remote--peer-properties peer)))
    (if-let* ((local (plist-get properties :local)))
        (spyglass-file-transport (expand-file-name local))
      (spyglass-ssh-transport (or (plist-get properties :ssh) (drover-remote--peer-name peer))
                              :program (or (plist-get properties :program) drover-remote-ssh-program)
                              :file (or (plist-get properties :registration)
                                        spyglass-remote-registration-file)
                              :args (plist-get properties :args)))))

(defun drover-remote--dial (peer)
  "Connect to PEER, unless a connection to it was dialed already."
  (unless (drover-remote--peer-dialed peer)
    (unless (drover-remote--peer-connection peer)
      (drover-remote--set-state peer 'connecting))
    ;; Its errors come through `drover-remote--spyglass-error'.
    (setf (drover-remote--peer-dialed peer)
          (spyglass-connect (drover-remote--transport peer) :name (drover-remote--peer-name peer)))))

;;;; Sessions of a peer

(defconst drover-remote--report-limit 200
  "Sessions changed at once past which a peer's are all listed again.
The index takes a session reported changed in tens of microseconds, and
compares all of a backend's listed again in a few milliseconds per
thousand: 2511 sessions of a snapshot took 80 ms one by one, 9 listed.")

(defun drover-remote--report (peer keys)
  "Tell drover that PEER's sessions KEYS changed, owner's keys.
Past `drover-remote--report-limit' sessions waiting for the index, those
of earlier reports included, the peer's sessions are listed again
instead, and its sessions reported one by one forgotten: the records of
a peer differ whenever anything of them changed, so listing them again
finds those."
  (let* ((backend (drover-remote--peer-backend peer))
         (waiting (seq-count (lambda (key) (eq (car-safe key) backend)) drover--changed-keys)))
    (cond
     ((null keys))
     ((or (memq backend drover--changed-backends)
          (> (+ waiting (length keys)) drover-remote--report-limit))
      (setq drover--changed-keys (seq-remove (lambda (key) (eq (car-safe key) backend))
                                             drover--changed-keys))
      (drover-backend-changed backend))
     (t (dolist (key keys)
          (drover-backend-changed backend key))))))

(defun drover-remote--set-stale (peer heard)
  "Make PEER's sessions stale, last heard from at HEARD, or fresh for nil."
  (let ((records (drover-remote--peer-records peer))
        (keys nil))
    (unless (eq (and heard t) (and (drover-remote--peer-stale peer) t))
      (setf (drover-remote--peer-stale peer) heard)
      (maphash (lambda (key record)
                 (let ((copy (copy-sequence record)))
                   (setf (drover-session-stale copy) heard)
                   (puthash key copy records)
                   (push key keys)))
               records)
      (drover-remote--report peer keys))))

(defun drover-remote--reset-queue (peer)
  "Drop what PEER sent and was not applied, and a snapshot in progress."
  (when (drover-remote--peer-worker peer)
    (cancel-timer (drover-remote--peer-worker peer)))
  (setf (drover-remote--peer-worker peer) nil
        (drover-remote--peer-queue peer) nil
        (drover-remote--peer-position peer) 0
        (drover-remote--peer-snapshot peer) nil))

(defun drover-remote--receive (peer message)
  "Queue MESSAGE, a snapshot or an update from PEER, to apply in slices."
  (setf (drover-remote--peer-queue peer) (nconc (drover-remote--peer-queue peer) (list message)))
  (drover-remote--schedule-work peer))

(defun drover-remote--schedule-work (peer)
  "Apply PEER's queue from a timer."
  (unless (drover-remote--peer-worker peer)
    (let ((link (drover-link peer)))
      (setf (drover-remote--peer-worker peer)
            (run-at-time 0 nil (lambda ()
                                 (let ((peer (drover-linked link)))
                                   (setf (drover-remote--peer-worker peer) nil)
                                   (drover-with-reporting "remote sessions"
                                     (drover-remote--work peer)))))))))

(defun drover-remote--resubscribe (peer why)
  "Subscribe to PEER again from what it holds, after WHY, a gap in its updates."
  (drover-remote--stat peer :gaps)
  (drover-remote--fail peer why)
  (drover-remote--reset-queue peer)
  (when-let* ((connection (drover-remote--peer-connection peer)))
    (drover-remote--send connection (list :t "sub" :epoch (drover-remote--peer-epoch peer)
                                          :rev (drover-remote--peer-rev peer)))))

(defun drover-remote--work (peer)
  "Apply PEER's queue for a slice, `drover-remote-apply-slice' at most.
Records are installed one at a time; a message applied whole is
acknowledged, and its last tells the revision now held: a snapshot's
last drops the sessions it did not list."
  (when (and (drover-remote--current) (drover-remote--peer-connection peer))
    (let* ((start (float-time))
           (collections gcs-done)
           (deadline (+ start drover-remote-apply-slice))
           (connection (drover-remote--peer-connection peer))
           (offset (or (spyglass-connection-clock-offset connection) 0))
           (records (drover-remote--peer-records peer))
           (changed nil))
      (catch 'done
        (while (drover-remote--peer-queue peer)
          (let* ((message (car (drover-remote--peer-queue peer)))
                 (snap (equal (plist-get message :t) "snap"))
                 (recs (plist-get message :recs)))
            (when (zerop (drover-remote--peer-position peer))
              (cond
               ((not (vectorp recs))
                (drover-remote--resubscribe peer "malformed message")
                (throw 'done nil))
               ((and snap (eq (plist-get message :first) t))
                (setf (drover-remote--peer-snapshot peer) (make-hash-table :test #'equal)))
               (snap
                (unless (drover-remote--peer-snapshot peer)
                  (drover-remote--resubscribe peer "snapshot without its start")
                  (throw 'done nil)))
               ((not (and (equal (plist-get message :epoch) (drover-remote--peer-epoch peer))
                          (eql (plist-get message :from) (drover-remote--peer-rev peer))))
                (drover-remote--resubscribe peer "update out of sequence")
                (throw 'done nil))))
            (while (< (drover-remote--peer-position peer) (length recs))
              (when (and changed (> (float-time) deadline))
                (throw 'done nil))
              (let ((record (drover-remote-decode-record
                             (aref recs (drover-remote--peer-position peer))
                             (drover-remote--peer-name peer) (drover-remote--peer-backend peer)
                             offset)))
                (cl-incf (drover-remote--peer-position peer))
                (when record
                  (when (drover-remote--peer-stale peer)
                    (setf (drover-session-stale record) (drover-remote--peer-stale peer)))
                  (let ((key (drover-session-id record)))
                    (when (drover-remote--peer-snapshot peer)
                      (puthash key t (drover-remote--peer-snapshot peer)))
                    (unless (equal-including-properties record (gethash key records))
                      (puthash key record records)
                      (push key changed)))
                  (drover-remote--stat peer :records))))
            ;; The message is applied whole.
            (pop (drover-remote--peer-queue peer))
            (setf (drover-remote--peer-position peer) 0)
            (unless snap
              (seq-doseq (gone (or (plist-get message :gone) []))
                (when-let* ((key (drover-remote--key gone))
                            ((gethash key records)))
                  (remhash key records)
                  (push key changed))))
            (drover-remote--stat peer :messages)
            (drover-remote--send connection (list :t "ack" :n (plist-get message :n)))
            (when (eq (plist-get message :last) t)
              (when snap
                (let ((listed (drover-remote--peer-snapshot peer))
                      (gone nil))
                  (maphash (lambda (key _) (unless (gethash key listed) (push key gone))) records)
                  (dolist (key gone)
                    (remhash key records)
                    (push key changed))
                  (setf (drover-remote--peer-snapshot peer) nil)
                  (drover-remote--stat peer :snapshots)))
              (setf (drover-remote--peer-epoch peer) (plist-get message :epoch)
                    (drover-remote--peer-rev peer) (plist-get message :to))
              (drover-remote--report peer changed)
              (setq changed nil)
              (drover-remote--set-stale peer nil)))))
      (drover-remote--report peer changed)
      (setf (drover-remote--peer-heard peer) (float-time))
      (drover-remote--stat-max peer (if (= collections gcs-done) :slice-max :slice-gc-max)
                               (- (float-time) start))
      (drover-remote--stat peer :slices)
      (when (drover-remote--peer-queue peer)
        (drover-remote--schedule-work peer)))))

(defun drover-remote--peer-session (peer key)
  "Return PEER's session of the owner's KEY, or nil."
  (gethash key (drover-remote--peer-records peer)))

(defun drover-remote--request (peer message callback &optional deadline once)
  "Ask PEER MESSAGE, a request, and call CALLBACK with (REPLY ERROR) once.
ERROR is a line when it cannot be asked or fails.  A request whose
stream ends before its answer is asked again once PEER is connected
anew (see `drover-remote--resume'), unless ONCE, until DEADLINE, by
default `drover-remote-request-timeout' seconds from now, past which it
fails: a stream dropped and resumed loses no answer."
  (let ((connection (drover-remote--peer-connection peer))
        (deadline (or deadline (+ (float-time) drover-remote-request-timeout)))
        (link (drover-link peer)))
    (if (not (and connection (spyglass-connection-live-p connection)))
        (funcall callback nil (format "%s is not connected" (drover-remote--label peer)))
      (spyglass-channel-request
       connection drover-remote-channel message
       (lambda (reply error)
         (drover-with-reporting "remote request"
           (let ((peer (drover-linked link)))
             (if (and error (not once)
                      (not (spyglass-connection-live-p connection))
                      (< (float-time) deadline)
                      (eq peer (drover-remote--peer (drover-remote--peer-name peer)))
                      (not (eq (drover-remote--peer-state peer) 'closed)))
                 (drover-remote--ask-later peer message callback deadline)
               (funcall callback reply (and error (drover-bound-line (format "%s" error))))))))
       (max 0.1 (- deadline (float-time)))))))

(defun drover-remote--ask-later (peer message callback deadline)
  "Ask PEER MESSAGE again once connected anew, for CALLBACK, until DEADLINE."
  (let ((entry (list message callback deadline nil))
        (link (drover-link peer)))
    (setf (nth 3 entry)
          (run-at-time (max 0 (- deadline (float-time))) nil
                       (lambda ()
                         (drover-with-reporting "remote request"
                           (let ((peer (drover-linked link)))
                             (when (memq entry (drover-remote--peer-asks peer))
                               (setf (drover-remote--peer-asks peer) (delq entry (drover-remote--peer-asks peer)))
                               (funcall callback nil (format "%s did not come back in time"
                                                             (drover-remote--label peer)))))))))
    (setf (drover-remote--peer-asks peer) (append (drover-remote--peer-asks peer) (list entry)))))

(defun drover-remote--drop-asks (peer why)
  "Fail the requests PEER was to be asked again, telling WHY."
  (let ((asks (drover-remote--peer-asks peer)))
    (setf (drover-remote--peer-asks peer) nil)
    (pcase-dolist (`(,_message ,callback ,_deadline ,timer) asks)
      (cancel-timer timer)
      (drover-with-reporting "remote request"
        (funcall callback nil why)))))

(defun drover-remote--wire-key (session)
  "Return the owner's key of SESSION, a session of a peer, as it travels."
  (let ((key (drover-session-id session)))
    (vector (symbol-name (car key)) (cdr key))))

(defun drover-remote--recap (peer session callback)
  "Fetch the recap of SESSION through PEER, and call CALLBACK with it."
  (let ((label (drover-remote--label peer)))
    (drover-remote--request
     peer (list :op "recap" :key (drover-remote--wire-key session))
     (lambda (reply error)
       (let ((recap (and (null error) (plist-get reply :recap))))
         (funcall callback
                  (if (and (consp recap) (keywordp (car recap)))
                      (drover-remote--decode-recap recap)
                    (list :failure (or error (format "%s told no recap" label))))))))))

(defun drover-remote--decode-recap (recap)
  "Return RECAP, as received, as `drover-recap' tells one."
  (let ((out nil))
    (dolist (key '(:prompt :reply :error))
      (when-let* ((text (drover-remote--text (plist-get recap key))))
        (setq out (plist-put out key text))))
    (when-let* ((failure (drover-remote--line (plist-get recap :failure))))
      (setq out (plist-put out :failure failure)))
    (when-let* ((changes (plist-get recap :changes))
                ((vectorp changes)))
      (setq out (plist-put out :changes
                           (delq nil (mapcar (lambda (change)
                                               (and (vectorp change) (= (length change) 3)
                                                    (stringp (aref change 0))
                                                    (list (drover-remote--line (aref change 0) 1024)
                                                          (drover-remote--count (aref change 1))
                                                          (drover-remote--count (aref change 2)))))
                                             (seq-take changes 100))))))
    out))

(defun drover-remote--encode-recap (recap &optional directory)
  "Return RECAP, as `drover-recap' tells one, as it travels.
The files changed under DIRECTORY, the session's, are named relative to
it, here, where they are: the peer names them as received."
  (let ((out nil))
    (dolist (key '(:prompt :reply :error :failure))
      (when-let* ((text (drover-remote--plain (plist-get recap key))))
        (setq out (plist-put out key text))))
    (when-let* ((changes (plist-get recap :changes)))
      (setq out (plist-put out :changes
                           (vconcat (delq nil (mapcar (lambda (change)
                                                        (and (consp change) (stringp (car change))
                                                             (vector (substring-no-properties
                                                                      (drover-relative-file-name (car change) directory))
                                                                     (or (drover-remote--number (nth 1 change)) :false)
                                                                     (or (drover-remote--number (nth 2 change)) :false))))
                                                      (and (listp changes) (seq-take changes 100))))))))
    out))

(defun drover-remote--preview (peer session callback)
  "Fetch where SESSION stands through PEER, and call CALLBACK with it or nil."
  (let ((label (drover-remote--label peer)))
    (drover-remote--request
     peer (list :op "preview" :key (drover-remote--wire-key session))
     (lambda (reply error)
       (funcall callback (if error
                             (format "%s: %s" label error)
                           (drover-remote--text (plist-get reply :text))))))))

(defun drover-remote--prefetch (peer sessions)
  "Ask PEER to fetch what the records of SESSIONS lack, at most once a second."
  (let ((pending (or (car (drover-remote--peer-prefetch peer)) (make-hash-table :test #'equal))))
    (dolist (session sessions)
      (puthash (drover-session-id session) t pending))
    (unless (cdr (drover-remote--peer-prefetch peer))
      (setf (drover-remote--peer-prefetch peer)
            (cons pending
                  (run-at-time 1 nil
                               (let ((link (drover-link peer)))
                                 (lambda ()
                                   (drover-with-reporting "remote prefetch"
                                     (let* ((peer (drover-linked link))
                                            (pending (car (drover-remote--peer-prefetch peer)))
                                            (connection (drover-remote--peer-connection peer)))
                                       (if (not (and connection (spyglass-connection-live-p connection)))
                                           ;; Asked once connected anew.
                                           (setf (drover-remote--peer-prefetch peer) (cons pending nil))
                                         (setf (drover-remote--peer-prefetch peer) nil)
                                         (drover-remote--send
                                          connection
                                          (list :t "pre"
                                                :keys (vconcat (mapcar (lambda (key)
                                                                         (vector (symbol-name (car key)) (cdr key)))
                                                                       (seq-take (hash-table-keys pending)
                                                                                 500))))))))))))))
    (setcar (drover-remote--peer-prefetch peer) pending)))

;;;; Older and archived sessions

(defun drover-remote--serve-fetch (connection kind generation)
  "Send CONNECTION's peer the sessions of KIND, \"history\" or \"archived\".
Each backend of this host keeping them answers when it can, possibly
more than once, and each answer goes as it comes, in messages of
`drover-remote-batch' records, under GENERATION, the peer's; archived,
drover's own archive answers first, as source \"drover\"."
  (let ((operation (if (equal kind "history") :history :archived)))
    (cl-flet ((send (source sessions)
                (let* ((records (seq-filter #'drover-remote--local-p sessions))
                       (chunks (or (seq-partition records drover-remote-batch) (list nil)))
                       (count (length chunks)))
                  (seq-do-indexed
                   (lambda (chunk index)
                     (drover-remote--send
                      connection
                      (list :t "fetched" :kind kind :gen generation :source source
                            :first (if (zerop index) t :false) :last (if (= index (1- count)) t :false)
                            :recs (vconcat (delq nil (mapcar (lambda (record)
                                                               (drover-remote-encode-record
                                                                (drover-session-key record)
                                                                (drover--bound-record record)))
                                                             chunk))))))
                   chunks))))
      (when (equal kind "archived")
        ;; Without what this host's lists fetched, which their backends answer.
        (let ((drover--archived nil))
          (send "drover" (drover-archived-sessions))))
      (dolist (backend (drover-backends))
        (let ((function (drover-backend-property backend operation)))
          (when (and (functionp function) (not (drover-backend-property backend :remote)))
            (drover-with-reporting "remote fetch"
              (funcall function
                       (lambda (sessions)
                         (drover-with-reporting "remote fetch"
                           (when (spyglass-connection-live-p connection)
                             (send (symbol-name backend)
                                   (mapcar #'drover--bound-record
                                           (if (equal kind "archived")
                                               (mapcar (lambda (session)
                                                         (if (drover-session-archived session)
                                                             session
                                                           (let ((copy (copy-sequence session)))
                                                             (setf (drover-session-archived copy) t)
                                                             copy)))
                                                       sessions)
                                             sessions))))))))))))))

(defun drover-remote--fetch (peer kind callback)
  "Ask PEER for its sessions of KIND, \"history\" or \"archived\", for CALLBACK.
CALLBACK, a backend's :history or :archived, is called with all of them
received so far each time an answer of the owner's comes."
  (let* ((entry (assoc kind (drover-remote--peer-fetches peer)))
         (generation (1+ (or (nth 1 entry) 0))))
    (setf (alist-get kind (drover-remote--peer-fetches peer) nil nil #'equal)
          (list generation callback (make-hash-table :test #'equal)))
    ;; Not asked again as such: a peer connected anew is asked for what
    ;; lists show, all of it (see `drover-remote--resume').
    (drover-remote--request peer (list :op "fetch" :kind kind :gen generation)
                            (let ((link (drover-link peer)))
                              (lambda (_reply error)
                                (when error
                                  (drover-remote--fail (drover-linked link)
                                                       (format "fetching %s: %s" kind error)))))
                            nil t)))

(defun drover-remote--fetched (peer message)
  "Take MESSAGE, older or archived sessions PEER sent: call back with all."
  (pcase-let* ((kind (plist-get message :kind))
               (`(,generation ,callback ,table) (cdr (assoc kind (drover-remote--peer-fetches peer)))))
    (when (and callback (eql generation (plist-get message :gen)) (stringp (plist-get message :source)))
      (let* ((offset (or (spyglass-connection-clock-offset (drover-remote--peer-connection peer)) 0))
             (source (plist-get message :source))
             (records (delq nil (mapcar (lambda (data)
                                          (drover-remote-decode-record data (drover-remote--peer-name peer)
                                                                       (drover-remote--peer-backend peer)
                                                                       offset))
                                        (seq-take (or (plist-get message :recs) []) drover-remote-batch)))))
        (when (equal kind "archived")
          (dolist (record records)
            (unless (drover-session-archived record)
              (setf (drover-session-archived record) t))))
        (puthash source (if (eq (plist-get message :first) t)
                            records
                          (append (gethash source table) records))
                 table)
        (when (eq (plist-get message :last) t)
          (let ((seen (make-hash-table :test #'equal))
                (all nil))
            (maphash (lambda (_source records)
                       (dolist (record records)
                         (unless (gethash (drover-session-id record) seen)
                           (puthash (drover-session-id record) t seen)
                           (push record all))))
                     table)
            (funcall callback (nreverse all))))))))

;;;; Messages

(defun drover-remote--handle (connection message)
  "Handle MESSAGE, received on CONNECTION on drover's channel."
  (drover-with-reporting "remote message"
    (when-let* ((state (drover-remote--current))
                ((and (consp message) (keywordp (car message)))))
      (let ((type (plist-get message :t))
            (peer (drover-remote--peer-of state connection)))
        (when (and peer (eq connection (drover-remote--peer-connection peer)))
          (setf (drover-remote--peer-heard peer) (float-time)))
        (cond
         ((equal type "hello") (drover-remote--on-hello state connection message))
         ((equal type "refuse")
          (let ((dialer (drover-remote--dialer-of state connection))
                (why (drover-bound-line (format "%s" (plist-get message :why)))))
            (drover-remote--fail (or dialer peer) (format "refused: %s" why))
            (when dialer
              ;; Another Emacs of this host may be connected there for a
              ;; while yet: stand by, and try again.
              (setf (drover-remote--peer-dialed dialer) nil)
              (spyglass-disconnect connection)
              (drover-remote--stand-by dialer))))
         ((null peer)
          ;; A connection greeted before the state was made anew.
          (unless (plist-get (gethash connection (drover-remote--state-hellos state)) :asked)
            (puthash connection (list :asked t) (drover-remote--state-hellos state))
            (drover-remote--hello connection t)))
         ((equal type "sub") (drover-remote--subscribe state connection peer message))
         ((equal type "ack") (drover-remote--acked state connection message))
         ((equal type "pre") (drover-remote--on-prefetch message))
         ((equal type "fetched")
          (when (eq connection (drover-remote--peer-connection peer))
            (drover-remote--fetched peer message)))
         ((member type '("snap" "up"))
          (when (eq connection (drover-remote--peer-connection peer))
            (drover-remote--receive peer message)))
         ((equal type "bye")
          (when (eq connection (drover-remote--peer-connection peer))
            (drover-remote--close peer (format "%s" (or (plist-get message :why) "bye"))))))))))

(defun drover-remote--on-prefetch (message)
  "Fetch what the records of the sessions MESSAGE names lack, for a peer."
  (let ((sessions nil))
    (seq-doseq (wire (seq-take (or (plist-get message :keys) []) 500))
      (when-let* ((key (drover-remote--key wire))
                  (served (drover-remote--served key)))
        (push (car served) sessions)))
    (when sessions
      (drover-prefetch (nreverse sessions)))))

(defun drover-remote--answer (connection message reply)
  "Answer MESSAGE, a request of CONNECTION's peer, through REPLY."
  (let* ((state (drover-remote--current))
         (peer (and state (drover-remote--peer-of state connection)))
         (key (drover-remote--key (plist-get message :key)))
         (served (and key state (drover-remote--served key))))
    (cond
     ((not peer) (funcall reply nil "not greeted"))
     ((not (memq (drover-remote--access peer) '(read act full))) (funcall reply nil "not allowed"))
     (t
      (pcase (plist-get message :op)
        ("recap"
         (if (not served)
             (funcall reply nil "no such session here")
           (drover-recap (car served)
                         (lambda (recap)
                           (funcall reply (list :recap (or (drover-remote--encode-recap
                                                            recap (drover-session-directory (car served)))
                                                           (list :failure "no recap"))))))))
        ("fetch"
         (let ((kind (plist-get message :kind)))
           (if (not (member kind '("history" "archived")))
               (funcall reply nil "unknown kind")
             (funcall reply (list :ok t))
             (drover-remote--serve-fetch connection kind (plist-get message :gen)))))
        ("preview"
         (if (not served)
             (funcall reply nil "no such session here")
           (condition-case err
               (drover-preview (car served)
                               (lambda (text)
                                 (funcall reply (list :text (or (drover-remote--plain text) :false)))))
             (error (funcall reply nil (drover-error-message err))))))
        (_ (funcall reply nil "unknown request")))))))

(defun drover-remote--request-handler (connection message reply)
  "Answer MESSAGE, a request of CONNECTION's peer, through REPLY, or its error."
  (let ((answered nil))
    (condition-case err
        (drover-remote--answer connection message
                               (lambda (value &optional error)
                                 (unless answered
                                   (setq answered t)
                                   (funcall reply value error))))
      (error
       (unless answered
         (setq answered t)
         (funcall reply nil (drover-error-message err)))))))

;;;; Peers as backends

(defun drover-remote--backend-name (name)
  "Return the symbol of the backend of the peer NAME."
  (intern (concat "remote:" name)))

(defun drover-remote--register (peer)
  "Register PEER's backend, whose sessions are the owner's records."
  (let ((name (drover-remote--peer-name peer)))
    (drover-register-backend
     (drover-remote--peer-backend peer)
     :label (drover-remote--label peer)
     :remote name
     :sessions (lambda ()
                 (when-let* ((peer (drover-remote--peer name)))
                   (hash-table-values (drover-remote--peer-records peer))))
     :session (lambda (key)
                (when-let* ((peer (drover-remote--peer name)))
                  (drover-remote--peer-session peer key)))
     ;; Every change comes reported, branches too, as the owner reads
     ;; them: a refresh lists none again but with FULL.
     :reports-changes t
     :open (lambda (session &optional _other-window)
             (drover-remote-show session))
     :recap (lambda (session callback)
              (if-let* ((peer (drover-remote--peer name)))
                  (drover-remote--recap peer session callback)
                (funcall callback (list :failure (format "%s is not a peer" name)))))
     :preview (lambda (session callback)
                (if-let* ((peer (drover-remote--peer name)))
                    (drover-remote--preview peer session callback)
                  (funcall callback nil)))
     :history (lambda (callback)
                (when-let* ((peer (drover-remote--peer name)))
                  (drover-remote--fetch peer "history" callback)))
     :archived (lambda (callback)
                 (when-let* ((peer (drover-remote--peer name)))
                   (drover-remote--fetch peer "archived" callback)))
     :prefetch (lambda (sessions)
                 (when-let* ((peer (drover-remote--peer name)))
                   (drover-remote--prefetch peer sessions))))))

(defun drover-remote--add-peer (state name properties &optional ephemeral)
  "Add the peer NAME with PROPERTIES to STATE, registering its backend.
EPHEMERAL non-nil for one accepted without configuration."
  (let ((peer (drover-remote--peer-make :name name :properties properties :ephemeral ephemeral
                                        :backend (drover-remote--backend-name name)
                                        :records (make-hash-table :test #'equal))))
    (puthash name peer (drover-remote--state-peers state))
    (setf (drover-remote--state-order state)
          (append (delete name (drover-remote--state-order state)) (list name)))
    (drover-remote--register peer)
    peer))

(defun drover-remote--close (peer why)
  "Close PEER for good, for WHY: its sessions go, its dialer too."
  (drover-remote--fail peer why)
  (drover-remote--set-state peer 'closed)
  (drover-remote--reset-queue peer)
  (drover-remote--drop-asks peer (format "%s closed: %s" (drover-remote--label peer) why))
  (when (drover-remote--peer-standby peer)
    (cancel-timer (drover-remote--peer-standby peer))
    (setf (drover-remote--peer-standby peer) nil))
  (setf (drover-remote--peer-connection peer) nil
        (drover-remote--peer-epoch peer) nil
        (drover-remote--peer-rev peer) nil)
  (let ((keys (hash-table-keys (drover-remote--peer-records peer))))
    (clrhash (drover-remote--peer-records peer))
    (drover-remote--report peer keys)))

(defun drover-remote--remove-peer (state peer why)
  "Remove PEER from STATE, telling it WHY: its sessions and backend go."
  (dolist (candidate (drover-remote--peer-candidates peer))
    (drover-remote--send (car candidate) (list :t "bye" :why why)))
  (when-let* ((dialed (drover-remote--peer-dialed peer)))
    (setf (drover-remote--peer-dialed peer) nil)
    (spyglass-disconnect dialed))
  (drover-remote--close peer why)
  (when (cdr (drover-remote--peer-prefetch peer))
    (cancel-timer (cdr (drover-remote--peer-prefetch peer))))
  (remhash (drover-remote--peer-name peer) (drover-remote--state-peers state))
  (setf (drover-remote--state-order state)
        (delete (drover-remote--peer-name peer) (drover-remote--state-order state)))
  (drover-unregister-backend (drover-remote--peer-backend peer)))

(defun drover-remote--configured ()
  "Return `drover-remote-peers' checked, entries (NAME . PROPERTIES) valid."
  (let ((seen nil) (valid nil))
    (pcase-dolist (`(,name . ,properties) drover-remote-peers)
      (cond
       ((not (and (stringp name) (string-match-p drover-remote--name-regexp name)))
        (drover-warn "drover-remote-peers: %S is not a host name" name))
       ((drover-host-local-p name)
        (drover-warn "drover-remote-peers: %s is this host" name))
       ((member (downcase name) seen))
       (t (push (downcase name) seen)
          (push (cons (downcase name) (and (proper-list-p properties) properties)) valid))))
    (nreverse valid)))

(defun drover-remote-reconfigure ()
  "Follow `drover-remote-peers': add, change and remove peers as it says.
A peer no longer configured is told so and its sessions go; a peer
added is dialed if it says so."
  (interactive)
  (let ((state (or (drover-remote--current)
                   (user-error "Enable `drover-remote-mode' first")))
        (configured (drover-remote--configured)))
    (dolist (peer (drover-remote--peers))
      (unless (or (assoc (drover-remote--peer-name peer) configured)
                  (drover-remote--peer-ephemeral peer))
        (drover-remote--remove-peer state peer (format "%s dropped %s" drover-host
                                                       (drover-remote--peer-name peer)))))
    (setf (drover-remote--state-order state)
          (append (mapcar #'car configured)
                  (seq-remove (lambda (name) (assoc name configured))
                              (drover-remote--state-order state))))
    (pcase-dolist (`(,name . ,properties) configured)
      (let ((peer (gethash name (drover-remote--state-peers state))))
        (if peer
            (progn (setf (drover-remote--peer-properties peer) properties
                         (drover-remote--peer-ephemeral peer) nil)
                   (drover-remote--register peer))
          (setq peer (drover-remote--add-peer state name properties)))
        (when (eq (drover-remote--peer-state peer) 'closed)
          (drover-remote--set-state peer 'waiting))
        ;; Connected already through its dialing, this host dials too when
        ;; its own connection would be the one kept.
        (when (and (plist-get properties :dial)
                   (or (not (drover-remote--peer-connection peer))
                       (drover-remote--preferred-ours-p peer)))
          (drover-remote--dial peer))))))

;;;; How peers stand

(defun drover-remote--last-heard (peer)
  "Return when PEER was last heard from, a float time, or nil."
  (let ((connection (drover-remote--peer-connection peer)))
    (max (or (drover-remote--peer-heard peer) 0)
         (or (and connection (ignore-errors (spyglass-connection-last-recv connection))) 0))))

(defun drover-remote--state-line (peer &optional now)
  "Return how PEER stands at NOW, on one line."
  (let* ((now (or now (float-time)))
         (state (drover-remote--peer-state peer))
         (dialed (drover-remote--peer-dialed peer))
         (heard (drover-remote--last-heard peer))
         (attempts (and dialed (ignore-errors (spyglass-connection-attempts dialed)))))
    (drover-bound-line
     (concat (pcase state
               ('open "connected")
               ('connecting "connecting")
               ('reconnecting "reconnecting")
               ('closed "closed")
               (_ (if (plist-get (drover-remote--peer-properties peer) :dial)
                      "standing by"
                    "waiting for it to connect")))
             (and attempts (> attempts 0) (format ", attempt %d" attempts))
             (and (> heard 0) (not (eq state 'open))
                  (format ", last heard from %s ago" (opencode-format-age heard now)))
             (and (drover-remote--peer-error peer)
                  (concat ": " (drover-remote--peer-error peer)))))))

(defun drover-remote--host-label (host)
  "Return how views name HOST, for `drover-host-label-function'."
  (if-let* ((peer (and host (drover-remote--peer host))))
      (drover-remote--label peer)
    (or host drover-host)))

(defun drover-remote--host-state (host)
  "Return how HOST stands when its sessions are stale, else nil.
For `drover-host-state-function'."
  (when-let* ((peer (drover-remote--peer host))
              ((drover-remote--peer-stale peer)))
    (drover-remote--state-line peer)))

(defun drover-remote--host-names ()
  "Return the names of the peers, in order, for `drover-hosts-function'."
  (when-let* ((state (drover-remote--current)))
    (copy-sequence (drover-remote--state-order state))))

(defun drover-remote-peer-stats (name)
  "Return what the peer NAME counted and measured, a plist, or nil.
Records and messages applied, slices and the longest, snapshots,
subscriptions and gaps; for this host serving it, under :serving."
  (when-let* ((state (drover-remote--current))
              (peer (drover-remote--peer name)))
    (append (list :state (drover-remote--peer-state peer)
                  :sessions (hash-table-count (drover-remote--peer-records peer))
                  :epoch (drover-remote--peer-epoch peer)
                  :rev (drover-remote--peer-rev peer)
                  :stale (and (drover-remote--peer-stale peer) t)
                  :error (drover-remote--peer-error peer)
                  :dialed (and (drover-remote--peer-dialed peer) t)
                  :through (let ((connection (drover-remote--peer-connection peer)))
                             (cond ((null connection) nil)
                                   ((spyglass-connection-transport connection) 'dialed)
                                   (t 'accepted))))
            (drover-remote--peer-stats peer)
            (let ((serving nil))
              (maphash (lambda (_connection sub)
                         (when (equal (drover-remote--sub-peer sub) name)
                           (setq serving (append (list :held (drover-remote--sub-held sub)
                                                       :dirty (hash-table-count (drover-remote--sub-dirty sub)))
                                                 (drover-remote--sub-stats sub)))))
                       (drover-remote--state-subs state))
              (and serving (list :serving serving))))))

(defun drover-remote-owner-stats ()
  "Return how this host serves its sessions, a plist, nil when the mode is off."
  (when-let* ((state (drover-remote--current)))
    (list :epoch (drover-remote--state-epoch state)
          :rev (drover-remote--state-rev state)
          :served (hash-table-count (drover-remote--state-sent state))
          :tombstones (drover-remote--state-tomb-count state)
          :floor (drover-remote--state-floor state)
          :subscribers (hash-table-count (drover-remote--state-subs state)))))

(defun drover-remote-status ()
  "Show how the peers stand: connections, sessions, revisions, errors."
  (interactive)
  (let ((state (or (drover-remote--current) (user-error "`drover-remote-mode' is off")))
        (now (float-time)))
    (with-current-buffer (get-buffer-create "*drover remote*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "This host: %s, epoch %s, revision %d, %d sessions served\n\n"
                        drover-host (drover-remote--state-epoch state) (drover-remote--state-rev state)
                        (hash-table-count (drover-remote--state-sent state))))
        (dolist (peer (drover-remote--peers))
          (let ((connection (drover-remote--peer-connection peer))
                (stats (drover-remote-peer-stats (drover-remote--peer-name peer))))
            (insert (format "%s (%s), %s access\n" (drover-remote--label peer)
                            (drover-remote--peer-name peer) (drover-remote--access peer))
                    (format "  %s\n" (drover-remote--state-line peer now))
                    (format "  %d sessions, revision %s of epoch %s\n"
                            (plist-get stats :sessions) (plist-get stats :rev) (plist-get stats :epoch)))
            (when connection
              (insert (format "  through a connection %s, round trip %s, clock offset %s\n"
                              (if (spyglass-connection-transport connection) "this host dialed" "it dialed")
                              (let ((rtt (spyglass-connection-rtt connection)))
                                (if rtt (format "%.1f ms" (* 1000 rtt)) "unknown"))
                              (let ((offset (spyglass-connection-clock-offset connection)))
                                (if offset (format "%.3f s" offset) "unknown")))))
            (when-let* ((serving (plist-get stats :serving)))
              (insert (format "  served up to revision %s, %d sessions to send\n"
                              (plist-get serving :held) (plist-get serving :dirty))))
            (insert "\n")))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun drover-remote-check ()
  "Check that each peer this host dials is reachable, and tell what fails.
SSH runs as the connection runs it, without a prompt (BatchMode), and
reads spyglass's registration file there."
  (interactive)
  (dolist (peer (drover-remote--peers))
    (let* ((properties (drover-remote--peer-properties peer))
           (name (drover-remote--peer-name peer)))
      (when (and (plist-get properties :dial) (not (plist-get properties :local)))
        (let* ((program (or (plist-get properties :program) drover-remote-ssh-program))
               (file (or (plist-get properties :registration)
                         (bound-and-true-p spyglass-remote-registration-file)
                         "~/.emacs.d/spyglass/server.json"))
               (buffer (generate-new-buffer " *drover remote check*" t)))
          (make-process
           :name "drover-remote-check" :buffer buffer :noquery t :connection-type 'pipe
           :command (append (list program) (plist-get properties :args)
                            (list "-o" "BatchMode=yes" (or (plist-get properties :ssh) name)
                                  (concat "cat " file)))
           :sentinel (lambda (process _event)
                       (unless (process-live-p process)
                         (drover-with-reporting "remote check"
                           (let ((output (with-current-buffer buffer (buffer-string))))
                             (kill-buffer buffer)
                             (if (and (zerop (process-exit-status process))
                                      (string-match-p "\"port\"" output))
                                 (drover-message "%s: reachable, spyglass registered" name)
                               (drover-warn "%s: %s %s failed: %s" name program
                                            (or (plist-get properties :ssh) name)
                                            (or (drover-bound-line output) "no output")))))))))))))

;;;; Showing a session of a peer

(defvar-local drover-remote--shown nil
  "The session of a peer the current buffer shows, as (PEER-NAME . KEY).")

(define-derived-mode drover-remote-session-mode special-mode "Drover remote"
  "A session of another host, as its host tells it: read only."
  (setq-local drover-session-at-point-function #'drover-remote--shown-session))

(defun drover-remote--shown-session ()
  "Return the session the current buffer shows, as its host last told it."
  (when-let* ((shown drover-remote--shown)
              (peer (drover-remote--peer (car shown))))
    (drover-remote--peer-session peer (cdr shown))))

(defun drover-remote-show (session)
  "Show SESSION, a session of another host, as that host tells it, read only.
Its record, then where it stands and its last turn, fetched through its
host.  Opening it there comes in a later step."
  (let* ((host (drover-session-host session))
         (peer (or (drover-remote--peer host) (user-error "No peer %s" host)))
         (buffer (get-buffer-create (format "*drover %s: %s*" (drover-remote--label peer)
                                            (drover-bound-line (drover-session-display-title session) 60)))))
    (with-current-buffer buffer
      (drover-remote-session-mode)
      (setq drover-remote--shown (cons (drover-remote--peer-name peer) (drover-session-id session)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s" (drover-session-display-title session)) 'face 'bold) "\n\n")
        (pcase-dolist (`(,label . ,value)
                       `(("Host" . ,(drover-remote--label peer))
                         ("Status" . ,(opencode-session-status-label (drover-session-status session)))
                         ("Front end" . ,(and (drover-session-origin session)
                                              (symbol-name (drover-session-origin session))))
                         ("Engine" . ,(and (drover-session-engine session)
                                           (symbol-name (drover-session-engine session))))
                         ("Directory" . ,(drover-session-directory session))
                         ("Model" . ,(drover-session-model session))
                         ("Not connected" . ,(and (drover-session-stale session)
                                                  (drover-remote--state-line peer)))))
          (when value (insert (format "%-14s%s\n" (concat label ":") value))))
        (insert "\n")
        (let ((preview-at (point-marker))
              (recap-at (progn (insert "\n") (point-marker))))
          (set-marker-insertion-type recap-at t)
          (cl-flet ((put (marker text)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (let ((inhibit-read-only t))
                            (save-excursion (goto-char marker) (insert text)))))))
            (unless (drover-session-stale session)
              (drover-remote--preview peer session
                                      (lambda (text) (put preview-at (concat (or text "Nothing to show.") "\n"))))
              (drover-remote--recap
               peer session
               (lambda (recap)
                 (put recap-at
                      (if-let* ((failure (plist-get recap :failure)))
                          (format "\nNo recap: %s\n" failure)
                        (concat "\n"
                                (and (plist-get recap :prompt)
                                     (format "Prompt:\n%s\n\n" (plist-get recap :prompt)))
                                (and (plist-get recap :reply)
                                     (format "Reply:\n%s\n" (plist-get recap :reply)))))))))))
        (goto-char (point-min))))
    (pop-to-buffer-same-window buffer)))

(defun drover-remote--open-conversation (conversation)
  "Show CONVERSATION, of another host, as `drover-links-parse' returns it.
For `drover-links-remote-function'."
  (let* ((host (plist-get conversation :host))
         (peer (or (drover-remote--find-peer host)
                   (user-error "No peer %s in `drover-remote-peers'" (drover-bound-line host))))
         (engine-key (cons (drover-remote--peer-name peer)
                           (cons (plist-get conversation :engine) (plist-get conversation :id))))
         (session (seq-find (lambda (session) (equal (drover-session-engine-key session) engine-key))
                            (hash-table-values (drover-remote--peer-records peer)))))
    (cond
     ((not (eq (drover-remote--peer-state peer) 'open))
      (user-error "%s is not connected: %s" (drover-remote--label peer) (drover-remote--state-line peer)))
     ((not session)
      (user-error "%s does not run that conversation now" (drover-remote--label peer)))
     (t (drover-open session)))))

;;;; The mode

(defun drover-remote--start ()
  "Make the state of the remote layer and connect to the peers."
  (let ((state (drover-remote--state-make :epoch (substring (spyglass-wire-random-token) 0 16))))
    (setq drover-remote--state state)
    (spyglass-register-channel drover-remote-channel #'drover-remote--handle
                               :request #'drover-remote--request-handler
                               :attach #'drover-remote--attach
                               :detach #'drover-remote--detach)
    (unless (spyglass-server)
      (spyglass-server-start)
      (setf (drover-remote--state-started-server state) t))
    (add-hook 'drover-changed-hook #'drover-remote--owner-changed)
    (add-hook 'spyglass-error-functions #'drover-remote--spyglass-error)
    (setq drover-hosts-function #'drover-remote--host-names
          drover-host-label-function #'drover-remote--host-label
          drover-host-state-function #'drover-remote--host-state
          drover-links-remote-function #'drover-remote--open-conversation)
    (drover-remote-reconfigure)
    ;; Connections made before: greeted now.
    (dolist (connection (spyglass-connections))
      (when (and (spyglass-connection-live-p connection)
                 (not (memq connection (mapcar #'drover-remote--peer-dialed (drover-remote--peers)))))
        (drover-remote--hello connection t)))
    state))

(defun drover-remote--stop ()
  "Tell the peers this host stops, close what this host dialed, forget them."
  (when-let* ((state drover-remote--state))
    (when (drover-layout-current-p state 'drover-remote--state)
      (dolist (peer (drover-remote--peers))
        (drover-remote--remove-peer state peer (format "%s stopped" drover-host)))
      (maphash (lambda (connection _sub) (drover-remote--unsubscribe state connection))
               (copy-hash-table (drover-remote--state-subs state)))
      (when (drover-remote--state-started-server state)
        (spyglass-server-stop))))
  (when (fboundp 'spyglass-unregister-channel)
    (spyglass-unregister-channel drover-remote-channel))
  (remove-hook 'drover-changed-hook #'drover-remote--owner-changed)
  (remove-hook 'spyglass-error-functions #'drover-remote--spyglass-error)
  (setq drover-hosts-function nil
        drover-host-label-function nil
        drover-host-state-function nil
        drover-links-remote-function nil
        drover-remote--state nil))

;;;###autoload
(define-minor-mode drover-remote-mode
  "Exchange sessions with the hosts of `drover-remote-peers'.
Each host tells the others the sessions its own backends follow, over
spyglass connections; lists here show those of the peers too, under a
backend per peer, their host in a column of its own, dimmed while that
host is not connected.  This step is read-only: recaps and previews
come through the peers, and opening a session shows what its host
tells of it.  Needs spyglass, with its channels."
  :global t
  :group 'drover-remote
  (if drover-remote-mode
      (condition-case err
          (progn
            (unless (require 'spyglass nil t)
              (user-error "`drover-remote-mode' needs spyglass, which is not installed"))
            (unless (featurep 'spyglass-channel)
              (user-error "`drover-remote-mode' needs a spyglass with channels"))
            (when drover-remote--state (drover-remote--stop))
            (drover-start)
            (drover-remote--start))
        (error (setq drover-remote-mode nil)
               (signal (car err) (cdr err))))
    (drover-remote--stop)))

(defun drover-remote-unload-function ()
  "Stop the remote layer before its file is unloaded."
  (when drover-remote-mode (drover-remote-mode -1))
  nil)

(provide 'drover-remote)
;;; drover-remote.el ends here
