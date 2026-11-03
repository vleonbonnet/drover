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
;; A peer acts on the sessions of another host through it: marking them
;; seen, archiving, restoring, interrupting, deleting, renaming,
;; answering their requests and sending them prompts are requests its
;; owner performs through its own backends, as its access allows, the
;; outcome showing here once its records changed; many sessions go in a
;; few requests.  Each peer keeps a history of its link, shown by
;; `drover-remote-status'.  See doc/remote.org for the whole design.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-ui)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 14))
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
(declare-function spyglass-connection-ending "ext:spyglass-conn" (connection))
(declare-function treesit-language-available-p "treesit.c" (language &optional detail))
(declare-function markdown-ts-mode--initialize "markdown-ts-mode" ())
(declare-function opencode-fontify-markdown "opencode-render" (start end &optional loudly))
(defvar markdown-ts-hide-markup)
(defvar markdown-ts-enable-code-block-context-mode)
(defvar markdown-ts-enable-table-mode)
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
 :access        What it may do with this host's sessions: `read', to
                list and read them; `act', to act on them too, but
                delete them; `full', to delete them too.
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

(defcustom drover-remote-grace 5
  "Seconds a peer whose connection ended may take to connect again unnoticed.
When the connection a peer dialed ends, this host waits this long for
it to dial again before dimming its sessions: SSH connects again in
about a second, and a peer restarting its remote layer greets again
at once.  A peer saying its Emacs quits is dimmed at once."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-fallback-delay 10
  "Seconds before this Emacs dials a peer whose own connection to it ended.
Only for a peer this Emacs dials too (:dial), the peer's connection being
the one kept: the peer dials again within seconds, as SSH and spyglass
retry after 0.2, 1, 2 and 5 seconds, so this Emacs dials only when it
did not.  A peer that said its Emacs quits is dialed after
`drover-remote-standby-delay' instead."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-action-timeout 60
  "Seconds an action on a session of a peer waits for the owner's answer.
An action whose connection ends is asked again once connected anew, and
done once whatever: past this, its outcome is reported unknown."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-protocol 1
  "The version of drover's protocol between hosts.
Peers of the same version understand each other; what a version adds is
told in the greeting's capabilities (`drover-remote-capabilities'), so
that a peer running an older drover keeps working without it.")

(defconst drover-remote-capabilities '("actions" "bye" "grant")
  "What this drover adds to its protocol's version, told in its greeting.
\"actions\": it performs actions on its sessions for its peers, and
tells which each session allows; \"bye\": its farewells say why it goes
\(`restart', `quit', `off' or `dropped'), only those for good closing a
peer; \"grant\": it tells each peer the access it grants it.")

(defconst drover-remote-action-batch 100
  "Sessions at most in one request of an action on a peer's sessions.")

(defconst drover-remote-history-size 64
  "Events of its link each peer keeps, the oldest going first.")

(defconst drover-remote-outcomes 256
  "Outcomes of the actions peers asked that this host keeps, by identifier.
An action asked again, its answer lost with its stream, gets its outcome
rather than run twice.")

(defconst drover-remote-prompt-limit 262144
  "Characters at most of a prompt sent to a session of a peer.")

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
  (stats nil :documentation "Counters and measures, a plist.")
  (caps nil :documentation "The capabilities its drover told, strings, or nil for an older one.")
  (granted nil :documentation "The access it grants this host, a symbol, or nil when untold.")
  (history nil :documentation "Events of its link, newest first, as (TIME KIND TEXT).")
  (error-time nil :documentation "When the last error happened, a float time.")
  (bye nil :documentation "Its last farewell, as (KIND WHY TIME), or nil.")
  (grace nil :documentation "Timer dimming its sessions once its connection did not come back.")
  (outbox nil :documentation "Actions to send, newest first, as (ACTION KEY ARGS THEN ELSE).")
  (flusher nil :documentation "Timer sending the actions of OUTBOX.")
  (ops nil :documentation "What the owner allows of each session, a list of strings, by its key.")
  (serial 0 :documentation "The number of the last action sent."))

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
  (started-server nil :documentation "Non-nil when the mode started spyglass's server.")
  (outcomes (make-hash-table :test #'equal)
            :documentation "Actions peers asked, by identifier, as (DONE . VALUE).
DONE nil: VALUE lists the replies awaiting it; t: VALUE is its outcome.")
  (outcome-order nil :documentation "The identifiers of OUTCOMES, newest first.")
  (tasks nil :documentation "Actions peers asked being performed, oldest first.")
  (runner nil :documentation "Timer performing TASKS.")
  (flight 0 :documentation "Operations of TASKS whose backends have not answered.")
  (watch nil :documentation "Timer failing the operations of TASKS not answered in time."))

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

(defun drover-remote--allows-p (access action)
  "Return non-nil when ACCESS, `read', `act' or `full', allows ACTION, a string."
  (pcase access
    ('full t)
    ('act (not (equal action "delete")))
    (_ nil)))

(defun drover-remote--event (peer kind format-string &rest args)
  "Note in PEER's history an event of KIND, a symbol.
FORMAT-STRING and ARGS tell it.  The history keeps
`drover-remote-history-size' events, each on a line."
  (when peer
    (let ((history (cons (list (float-time) kind
                               (drover-bound-line (apply #'format format-string args) 200))
                         (drover-remote--peer-history peer))))
      (when (> (length history) drover-remote-history-size)
        (setcdr (nthcdr (1- drover-remote-history-size) history) nil))
      (setf (drover-remote--peer-history peer) history))))

(defun drover-remote--capable-p (peer capability)
  "Return non-nil when PEER's drover told CAPABILITY, a string."
  (member capability (drover-remote--peer-caps peer)))

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
  "Return VALUE, a string, as it travels with its faces and tooltips, or nil.
Of its properties, only its faces and the strings of its `help-echo'
travel, those bounded to `drover-text-lines' lines: a tooltip made by a
function is this host's to compute."
  (when (stringp value)
    (let* ((value (if (> (length value) (* 2 drover-line-limit))
                      (substring value 0 (* 2 drover-line-limit))
                    value))
           (copy (substring-no-properties value))
           (kept nil))
      (dolist (interval (object-intervals value))
        (let* ((plist (caddr interval))
               (face (plist-get plist 'face))
               (help (plist-get plist 'help-echo))
               (props (append (and face (list 'face face))
                              (and (stringp help)
                                   (list 'help-echo (drover-bound-lines (substring-no-properties help)))))))
          (when props
            (setq kept t)
            (set-text-properties (car interval) (cadr interval) props copy))))
      (if kept (spyglass-wire-encode copy 6) copy))))

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
                         (if (eq archived t) t (drover-remote--number archived))))
        (put :ops (drover-remote--record-ops record)))
      (nreverse out))))

(defun drover-remote--record-ops (record)
  "Return the actions this host performs on RECORD for a peer, a vector of strings.
What its backend supports of it, whatever a peer's access: each peer's
greys out what its access forbids, and this host checks it again."
  (drover-with-reporting "remote operations"
    (vconcat
     (delq nil
           (list (and (drover-supports-p record :interrupt) "interrupt")
                 (and (drover-supports-p record :mark-seen) "mark-seen")
                 (and (drover-archivable-p record) "archive")
                 (and (drover-session-archived-p record) (drover-unarchivable-p record) "unarchive")
                 (and (drover-supports-p record :delete) "delete")
                 (and (drover-supports-p record :rename) "rename")
                 (and (drover-supports-p record :requests) (drover-supports-p record :answer) "answer")
                 (and (drover-supports-p record :submit) "send"))))))

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
  "Return VALUE, a string with faces received, sanitized, on one line, or nil.
Its tooltips, strings, are kept too, bounded to `drover-text-lines' lines."
  (let ((string (if (stringp value)
                    value
                  (and (consp value) (ignore-errors (spyglass-wire-decode value 6))))))
    (when (stringp string)
      (let* ((string (if (> (length string) (* 2 drover-line-limit))
                         (substring string 0 (* 2 drover-line-limit))
                       string))
             (clean (spyglass-wire-sanitize-string string 0)))
        (dolist (interval (object-intervals string))
          (let ((help (plist-get (caddr interval) 'help-echo)))
            (when (and (stringp help) (< (car interval) (length clean)))
              (put-text-property (car interval) (min (cadr interval) (length clean)) 'help-echo
                                 (drover-bound-lines (drover-bound-text (substring-no-properties help)))
                                 clean))))
        (drover-bound-line clean)))))

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
          (drover-remote--event peer 'serve "serving it from revision %s of this host's epoch %s, to %s"
                                held (drover-remote--state-epoch state) (drover-remote--state-rev state))
          (setf (drover-remote--sub-held sub) held)
          (maphash (lambda (key rev) (when (> rev held) (puthash key t (drover-remote--sub-dirty sub))))
                   (drover-remote--state-revs state))
          (maphash (lambda (key rev) (when (> rev held) (puthash key t (drover-remote--sub-dirty sub))))
                   (drover-remote--state-tombs state))
          ;; Resynchronized even when nothing changed meanwhile.
          (when (zerop (hash-table-count (drover-remote--sub-dirty sub)))
            (drover-remote--start-batch state sub 'up nil)))
      (drover-remote--stat sub :snapshots)
      (drover-remote--event peer 'serve "serving it a snapshot of %d sessions: it held %s"
                            (hash-table-count (drover-remote--state-sent state))
                            (cond ((not (stringp epoch)) "nothing")
                                  ((not (equal epoch (drover-remote--state-epoch state)))
                                   (format "epoch %s, this host's being %s" (drover-bound-line epoch 16)
                                           (drover-remote--state-epoch state)))
                                  (t (format "revision %s, out of reach" held))))
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
                             :ask (if ask t :false)
                             :caps (vconcat drover-remote-capabilities)
                             :epoch (or (and drover-remote--state
                                             (drover-remote--state-epoch drover-remote--state))
                                        :false))))

(defun drover-remote--bye (connection kind why)
  "Tell CONNECTION's peer that this host goes, of KIND, because of WHY.
KIND is \"restart\" (the remote layer starts anew, greeting again),
\"quit\" (Emacs exits), \"off\" (the mode is turned off) or \"dropped\"
\(this host's configuration dropped the peer).  A peer running an older
drover closes this host for good on any farewell: it is told only of
the last two, and of a restart, after which it greets again."
  (let* ((state (drover-remote--current))
         (peer (and state (drover-remote--peer-of state connection))))
    (when (or (member kind '("off" "dropped" "restart"))
              (and peer (drover-remote--capable-p peer "bye")))
      (drover-remote--send connection (list :t "bye" :kind kind :why why)))))

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
     ((and peer (or (null dialer) (eq dialer peer)) (eq connection (drover-remote--peer-connection peer))
           (stringp host) (string-equal-ignore-case host (drover-remote--peer-name peer))
           (integerp version) (= version drover-remote-protocol))
      ;; Greeted again on the connection kept: its remote layer started
      ;; anew, and serves no subscription of this host's any more.
      (drover-remote--greeted peer connection message)
      (drover-remote--event peer 'hello "greeted again: its remote layer restarted, epoch %s"
                            (drover-remote--hello-epoch message))
      (when (eq (plist-get message :ask) t)
        (drover-remote--hello connection))
      (drover-remote--resubscribe peer nil))
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
      (drover-remote--greeted peer connection message)
      (drover-remote--event peer 'hello "greeted on a connection %s, instance %s, epoch %s%s"
                            (if dialer "this host dialed" "it dialed")
                            (drover-bound-line (format "%s" (plist-get message :inst)) 16)
                            (drover-remote--hello-epoch message)
                            (if (drover-remote--peer-caps peer) "" ", an older drover: read only"))
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
      ;; Floats: spyglass looks at a connection every half interval,
      ;; which an integer halves to a whole number, 1 to 0, a timer that
      ;; never repeats: a frozen peer then stayed connected for good.
      (spyglass-connection-set-heartbeat connection (float (car drover-remote-heartbeat))
                                         (float (cdr drover-remote-heartbeat)))
      (drover-remote--elect state peer)))))

(defun drover-remote--hello-epoch (message)
  "Return the epoch the greeting MESSAGE tells, on a short line."
  (let ((epoch (plist-get message :epoch)))
    (if (stringp epoch) (drover-bound-line epoch 16) "untold")))

(defun drover-remote--greeted (peer connection message)
  "Take what PEER's greeting MESSAGE on CONNECTION tells of its drover.
Its capabilities, short strings, and this host's access, granted it."
  (let ((caps (plist-get message :caps)))
    (setf (drover-remote--peer-caps peer)
          (and (vectorp caps)
               (seq-filter (lambda (cap) (and (stringp cap) (< (length cap) 32)))
                           (seq-take caps 16)))))
  (when (drover-remote--peer-grace peer)
    (cancel-timer (drover-remote--peer-grace peer))
    (setf (drover-remote--peer-grace peer) nil))
  (setf (drover-remote--peer-bye peer) nil)
  (drover-remote--send connection (list :t "grant" :access (symbol-name (drover-remote--access peer)))))

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
        (drover-remote--event peer 'elect "closed the connection this host dialed: the one it dialed is kept")
        (setf (drover-remote--peer-dialed peer) nil)
        (spyglass-disconnect dialed)))))

(defun drover-remote--use (state peer connection)
  "Have PEER's sessions come through CONNECTION, subscribing there.
STATE is the remote layer's."
  (ignore state)
  (drover-remote--reset-queue peer)
  (setf (drover-remote--peer-connection peer) connection
        (drover-remote--peer-heard peer) (float-time))
  (drover-remote--set-state peer 'open)
  (when (drover-remote--peer-standby peer)
    (cancel-timer (drover-remote--peer-standby peer))
    (setf (drover-remote--peer-standby peer) nil))
  (when (drover-remote--peer-grace peer)
    (cancel-timer (drover-remote--peer-grace peer))
    (setf (drover-remote--peer-grace peer) nil))
  (drover-remote--stat peer :subscriptions)
  (drover-remote--event peer 'sub "kept the connection %s; subscribed holding epoch %s revision %s"
                        (if (spyglass-connection-transport connection) "this host dialed" "it dialed")
                        (or (drover-remote--peer-epoch peer) "none") (or (drover-remote--peer-rev peer) "none"))
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
  "Note MESSAGE, an error, as the last of PEER, if any, and in its history.
The last error stays once connected again, with its time, for
`drover-remote-status' to tell what happened."
  (when peer
    (setf (drover-remote--peer-error peer) (drover-bound-line (format "%s" message))
          (drover-remote--peer-error-time peer) (float-time))
    (drover-remote--event peer 'error "%s" (drover-remote--peer-error peer))))

(defvar drover-remote--error-grace 0.5
  "Seconds a stream an error was told on has to end, ending as streams do.")

(defun drover-remote--next-stream (state connection)
  "Note in STATE that a stream of CONNECTION began or ended."
  (puthash connection (1+ (gethash connection (drover-remote--state-streams state) 0))
           (drover-remote--state-streams state)))

(defun drover-remote--spyglass-error (connection message)
  "Follow MESSAGE, an error spyglass reports on CONNECTION, one of drover's.
For `spyglass-error-functions'.  A write failing finds its stream gone,
the peer gone, or drops it, as spyglass drops a stream whose write
failed: the stream's end, which follows, tells it, stale rows and
`drover-remote-status', and the subscription made anew resends what was
lost; it is counted, not reported.  Another error is the peer's last,
which they tell.  One leaving its stream open, past
`drover-remote--error-grace' seconds, is unexpected: it is reported on
one line, once in `drover-error-interval' seconds."
  (drover-with-reporting "remote connection"
    (when-let* ((state (drover-remote--current))
                (connection)
                (peer (or (drover-remote--dialer-of state connection)
                          (drover-remote--peer-of state connection))))
      (let ((line (drover-bound-line (format "%s" message)))
            (stream (gethash connection (drover-remote--state-streams state) 0)))
        (if (string-prefix-p "Sending failed" line)
            (progn (drover-remote--stat peer :write-failures)
                   (drover-remote--event peer 'write "%s" line))
          (drover-remote--fail peer line)
          (when (spyglass-connection-live-p connection)
            (run-at-time drover-remote--error-grace nil
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
                                     (drover-warn "%s: %s" (drover-remote--label peer) line))))))))))))))

(defun drover-remote--attach (connection)
  "Greet the peer of CONNECTION, whose stream just opened."
  (drover-with-reporting "remote connection"
    (when-let* ((state (drover-remote--current)))
      (drover-remote--next-stream state connection)
      (remhash connection (drover-remote--state-hellos state))
      (when-let* ((peer (or (drover-remote--dialer-of state connection)
                            (drover-remote--peer-of state connection))))
        (drover-remote--event peer 'open "stream open, a connection %s"
                              (if (spyglass-connection-transport connection) "this host dialed" "it dialed")))
      (drover-remote--hello connection))))

(defun drover-remote--ending (connection)
  "Return how CONNECTION's last stream ended, as spyglass tells it, or nil."
  (and (fboundp 'spyglass-connection-ending)
       (ignore-errors (spyglass-connection-ending connection))))

(defun drover-remote--quitting-p (peer)
  "Return non-nil when PEER said lately that its Emacs quits or its mode stops."
  (pcase (drover-remote--peer-bye peer)
    (`(,kind ,_why ,time) (and (member kind '("quit" "off")) (< (- (float-time) time) 60)))))

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
        (let ((theirs (and (eq connection (drover-remote--peer-connection peer))
                           (not (spyglass-connection-transport connection)))))
          (when (or (eq connection (drover-remote--peer-connection peer))
                    (eq connection (drover-remote--peer-dialed peer)))
            (let ((ending (drover-remote--ending connection)))
              (drover-remote--event peer 'drop "stream ended (%s)%s%s"
                                    (if (spyglass-connection-transport connection)
                                        "this host dialed" "it dialed")
                                    (if ending (concat ": " ending) "")
                                    (if final ", for good" ""))))
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
              (if (and theirs (not (drover-remote--quitting-p peer)) (> drover-remote-grace 0))
                  (drover-remote--wait-grace peer)
                (drover-remote--lost peer))))
          (when (and (not (drover-remote--peer-connection peer))
                     (not (drover-remote--peer-dialed peer)))
            (drover-remote--stand-by peer (and theirs (not (drover-remote--quitting-p peer))
                                               drover-remote-fallback-delay))))))))

(defun drover-remote--wait-grace (peer)
  "Give PEER, whose connection to this host ended, `drover-remote-grace' seconds.
Meanwhile its sessions stay as they are, the peer reconnecting; past
them, its sessions are dimmed, as `drover-remote--lost' does."
  (drover-remote--set-state peer 'reconnecting)
  (drover-remote--event peer 'grace "waiting %ss for it to connect again" drover-remote-grace)
  (unless (drover-remote--peer-grace peer)
    (setf (drover-remote--peer-grace peer)
          (run-at-time drover-remote-grace nil
                       (let ((link (drover-link peer)))
                         (lambda ()
                           (drover-with-reporting "remote grace"
                             (let ((peer (drover-linked link)))
                               (setf (drover-remote--peer-grace peer) nil)
                               (when (and (drover-remote--current)
                                          (eq peer (drover-remote--peer (drover-remote--peer-name peer)))
                                          (or (not (drover-remote--peer-connection peer))
                                              (equal (car (drover-remote--peer-bye peer)) "restart"))
                                          (not (eq (drover-remote--peer-state peer) 'closed)))
                                 (drover-remote--event peer 'grace "not back after %ss: its sessions dimmed"
                                                       drover-remote-grace)
                                 (drover-remote--lost peer))))))))))

(defun drover-remote--lost (peer)
  "Note that PEER is not connected: its sessions stay, stale."
  (when (drover-remote--peer-grace peer)
    (cancel-timer (drover-remote--peer-grace peer))
    (setf (drover-remote--peer-grace peer) nil))
  (drover-remote--set-state peer (if (drover-remote--peer-dialed peer) 'reconnecting 'waiting))
  (drover-remote--set-stale peer (or (drover-remote--peer-heard peer) (float-time))))

(defun drover-remote--stand-by (peer &optional delay)
  "Dial PEER again once unreachable for DELAY seconds.
DELAY is `drover-remote-standby-delay' by default.  Only a peer this
host dials, and not when its configuration dropped it."
  (when (and (plist-get (drover-remote--peer-properties peer) :dial)
             (not (eq (drover-remote--peer-state peer) 'closed))
             (not (drover-remote--peer-standby peer)))
    (let ((delay (or delay drover-remote-standby-delay)))
      (drover-remote--event peer 'standby "dialing it in %ss unless it connects first" delay)
      (setf (drover-remote--peer-standby peer)
            (run-at-time delay nil
                         (let ((link (drover-link peer)))
                           (lambda ()
                             (drover-with-reporting "remote standby"
                               (let ((peer (drover-linked link)))
                                 (setf (drover-remote--peer-standby peer) nil)
                                 (when (and (drover-remote--current)
                                            (eq peer (drover-remote--peer (drover-remote--peer-name peer)))
                                            (not (drover-remote--peer-connection peer))
                                            (not (drover-remote--peer-dialed peer)))
                                   (drover-remote--dial peer)))))))))))

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
    (drover-remote--event peer 'dial "dialing it%s"
                          (let ((properties (drover-remote--peer-properties peer)))
                            (if (plist-get properties :local)
                                " on this host"
                              (format " through %s %s"
                                      (or (plist-get properties :program) drover-remote-ssh-program)
                                      (or (plist-get properties :ssh) (drover-remote--peer-name peer))))))
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
  "Subscribe to PEER again from what it holds, after WHY, a gap in its updates.
WHY nil: PEER greeted again, its remote layer having started anew."
  (when why
  (drover-remote--stat peer :gaps)
    (drover-remote--fail peer why))
  (drover-remote--reset-queue peer)
  (drover-remote--set-state peer 'open)
  (when-let* ((connection (drover-remote--peer-connection peer)))
    (drover-remote--stat peer :subscriptions)
    (drover-remote--event peer 'sub "subscribed again, holding epoch %s revision %s"
                          (or (drover-remote--peer-epoch peer) "none") (or (drover-remote--peer-rev peer) "none"))
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
              (let* ((data (aref recs (drover-remote--peer-position peer)))
                     (record (drover-remote-decode-record
                              data (drover-remote--peer-name peer) (drover-remote--peer-backend peer)
                              offset)))
                (cl-incf (drover-remote--peer-position peer))
                (when record
                  (drover-remote--note-ops peer (drover-session-id record) data)
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
                  (remhash key (drover-remote--peer-ops peer))
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
                    (remhash key (drover-remote--peer-ops peer))
                    (push key changed))
                  (setf (drover-remote--peer-snapshot peer) nil)
                  (drover-remote--stat peer :snapshots)
                  (drover-remote--event peer 'snapshot "took a snapshot of %d sessions, epoch %s%s"
                                        (hash-table-count records)
                                        (drover-bound-line (format "%s" (plist-get message :epoch)) 16)
                                        (if (and (drover-remote--peer-epoch peer)
                                                 (not (equal (drover-remote--peer-epoch peer)
                                                             (plist-get message :epoch))))
                                            (format ", its epoch was %s" (drover-remote--peer-epoch peer))
                                          ""))))
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

(defconst drover-remote--actions
  '("interrupt" "mark-seen" "archive" "unarchive" "delete" "rename" "answer" "send")
  "The actions on its sessions a host performs for its peers.")

(defun drover-remote--note-ops (peer key data)
  "Note what the owner allows of its session KEY, as DATA, its record, tells.
PEER keeps it beside the record; nil when DATA tells nothing, as an
older drover's records."
  (let ((ops (plist-get data :ops)))
    (if (vectorp ops)
        (puthash key (seq-filter (lambda (op) (member op drover-remote--actions)) (seq-take ops 16))
                 (drover-remote--peer-ops peer))
      (remhash key (drover-remote--peer-ops peer)))))

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

;;;; Acting on a peer's sessions

(defconst drover-remote--operations
  '((:interrupt . "interrupt") (:mark-seen . "mark-seen") (:archive . "archive")
    (:unarchive . "unarchive") (:delete . "delete") (:rename . "rename")
    (:respond . "answer") (:send . "send"))
  "The operations of a peer's proxy forwarded to the owner, by action.")

(defun drover-remote--refusal (peer &optional session action)
  "Return why PEER takes no ACTION on SESSION now, a line, or nil when it does.
Without SESSION and ACTION, why it takes no action at all."
  (cond
   ((not (drover-remote--capable-p peer "actions"))
    (format "%s runs an older drover, which takes no action from other hosts: update it"
            (drover-remote--label peer)))
   ((not (eq (drover-remote--peer-state peer) 'open))
    (format "%s is not connected" (drover-remote--label peer)))
   ((and action (not (drover-remote--allows-p (or (drover-remote--peer-granted peer) 'read) action)))
    (format "%s grants this host %s access" (drover-remote--label peer)
            (or (drover-remote--peer-granted peer) "read")))
   ((and session action
         (let ((ops (gethash (drover-session-id session) (drover-remote--peer-ops peer) 'unknown)))
           (and (listp ops) (not (member action ops)))))
    (format "%s does not %s it" (drover-remote--label peer) action))))

(defun drover-remote--supports (name session operation)
  "Return non-nil when the peer NAME takes OPERATION on SESSION now.
For the proxy's :supports: the operations forwarded need a connected
peer whose drover acts for others, an access allowing them, and the
owner's leave for SESSION; the others, recaps and previews, apply."
  (let ((action (alist-get operation drover-remote--operations)))
    (or (null action)
        (when-let* ((peer (drover-remote--peer name)))
          (not (drover-remote--refusal peer session action))))))

(defun drover-remote--unsupported (name session operation)
  "Return why the peer NAME takes no OPERATION on SESSION, for :unsupported."
  (when-let* ((action (alist-get operation drover-remote--operations)))
    (if-let* ((peer (drover-remote--peer name)))
        (drover-remote--refusal peer session action)
      (format "%s is not a peer" name))))

(defun drover-remote--act (name action session args then else)
  "Have the peer NAME perform ACTION on SESSION, with ARGS, a plist.
THEN is called with no argument once the owner did, ELSE with a line
once it refused or failed, at most one of them once.  Actions asked
together go in few requests, the next time Emacs is idle: acting on
thousands of sessions costs a few requests."
  (let ((peer (drover-remote--peer name)))
    (if-let* ((refusal (if peer (drover-remote--refusal peer session action) (format "%s is not a peer" name))))
        (funcall else refusal)
      (push (list action (drover-session-id session) args then else) (drover-remote--peer-outbox peer))
      (unless (drover-remote--peer-flusher peer)
        (setf (drover-remote--peer-flusher peer)
              (run-at-time 0 nil (let ((link (drover-link peer)))
                                   (lambda ()
                                     (drover-with-reporting "remote actions"
                                       (let ((peer (drover-linked link)))
                                         (setf (drover-remote--peer-flusher peer) nil)
                                         (drover-remote--send-actions peer)))))))))))

(defun drover-remote--fail-outbox (peer why)
  "Fail the actions of PEER not sent yet, telling WHY."
  (let ((outbox (drover-remote--peer-outbox peer)))
    (setf (drover-remote--peer-outbox peer) nil)
    (when (drover-remote--peer-flusher peer)
      (cancel-timer (drover-remote--peer-flusher peer))
      (setf (drover-remote--peer-flusher peer) nil))
    (pcase-dolist (`(,_action ,_key ,_args ,_then ,else) outbox)
      (drover-with-reporting "remote actions"
        (funcall else why)))))

(defun drover-remote--action-id (peer)
  "Return a new identifier of an action asked of PEER, unique across restarts."
  (format "%s-%s-%d" drover-host (substring (or (drover-remote--state-epoch (drover-remote--current)) "x") 0 8)
          (cl-incf (drover-remote--peer-serial peer))))

(defun drover-remote--send-actions (peer)
  "Send PEER the actions of its outbox: one request per action and arguments.
Each carries `drover-remote-action-batch' sessions at most, and an
identifier the owner answers again, rather than act twice, when the
request is asked again after its stream ended."
  (let ((outbox (reverse (drover-remote--peer-outbox peer)))
        (groups nil))
    (setf (drover-remote--peer-outbox peer) nil)
    (dolist (entry outbox)
      (let ((group (assoc (cons (nth 0 entry) (nth 2 entry)) groups)))
        (if group
            (push entry (cdr group))
          (push (list (cons (nth 0 entry) (nth 2 entry)) entry) groups))))
    (pcase-dolist (`((,action . ,args) . ,entries) (nreverse groups))
      (dolist (chunk (seq-partition (nreverse entries) drover-remote-action-batch))
        (drover-remote--send-chunk peer action args chunk)))))

(defun drover-remote--send-chunk (peer action args entries)
  "Ask PEER to perform ACTION with ARGS on the sessions of ENTRIES, of its outbox."
  (let ((id (drover-remote--action-id peer))
        (label (drover-remote--label peer)))
    (drover-remote--stat peer :actions-sent (length entries))
    (drover-remote--request
     peer (append (list :op "act" :id id :action action
                        :keys (vconcat (mapcar (lambda (entry)
                                                 (let ((key (nth 1 entry)))
                                                   (vector (symbol-name (car key)) (cdr key))))
                                               entries)))
                  (and args (list :args args)))
     (lambda (reply error)
       (let ((out (and (null error) (plist-get reply :out))))
         (seq-do-indexed
          (lambda (entry index)
            (let ((outcome (and (vectorp out) (< index (length out)) (aref out index))))
              (drover-with-reporting "remote actions"
                (cond ((eq outcome t) (funcall (nth 3 entry)))
                      (error (funcall (nth 4 entry) (format "%s: %s" label error)))
                      ((stringp outcome) (funcall (nth 4 entry) (drover-bound-line outcome)))
                      (t (funcall (nth 4 entry) (format "%s told no outcome" label)))))))
          entries)))
     (+ (float-time) drover-remote-action-timeout))))

(defun drover-remote--confirming (name action)
  "Return the operation of the peer NAME's proxy performing ACTION, a string.
It takes a session, THEN and ELSE, as the operations that confirm do."
  (lambda (session &optional then else)
    (drover-remote--act name action session nil (or then #'ignore)
                        (or else (lambda (message)
                                   (drover-warn "Could not %s %s: %s" action
                                                (drover-session-display-title session) message))))))

(defun drover-remote--rename (name session title)
  "Have the peer NAME rename SESSION to TITLE."
  (drover-remote--act name "rename" session (list :title (drover-bound-line title)) #'ignore
                      (lambda (message)
                        (drover-warn "Could not rename %s: %s" (drover-session-display-title session) message))))

(defun drover-remote--send-prompt (name session text _token &optional _attachments)
  "Have the peer NAME submit TEXT as a prompt of SESSION, for the proxy's :send.
The owner submits it through its backend, as it delivers a scheduled
prompt; attachments, files of this host, do not travel."
  (let ((title (drover-session-display-title session)))
    (if (> (length text) drover-remote-prompt-limit)
        (drover-warn "The prompt for %s is longer than %d characters, which go to another host at most; it is in the kill ring"
                     title drover-remote-prompt-limit)
      (drover-remote--act name "send" session (list :text (substring-no-properties text))
                          (lambda () (drover-message "%s took the prompt" title))
                          (lambda (message)
                            (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                                         title message))))))

(defun drover-remote--wait (peer message timeout)
  "Ask PEER MESSAGE and wait for its answer, TIMEOUT seconds at most.
Return (REPLY ERROR).  Only for a command that must answer from it:
Emacs reads processes and runs timers meanwhile, and \\[keyboard-quit] stops it."
  (let ((answer nil)
        (deadline (+ (float-time) timeout)))
    (drover-remote--request peer message (lambda (reply error) (setq answer (list reply error)))
                            deadline t)
    (while (and (not answer) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (or answer (list nil (format "%s did not answer in %ss" (drover-remote--label peer) timeout)))))

(defun drover-remote--decode-requests (data)
  "Return the requests DATA, received, tells, checked and bounded, oldest first."
  (cl-flet ((strings (value limit)
              (and (vectorp value)
                   (delq nil (mapcar (lambda (item) (drover-remote--line item 200)) (seq-take value limit))))))
    (and (vectorp data)
         (delq nil
               (mapcar (lambda (request)
                         (when-let* (((and (consp request) (keywordp (car request))))
                                     (id (drover-remote--line (plist-get request :id) 200)))
                           (list :id id
                                 :kind (if (equal (plist-get request :kind) "permission") 'permission 'question)
                                 :title (or (drover-remote--line (plist-get request :title)) "Request")
                                 :detail (drover-remote--text (plist-get request :detail))
                                 :choices (seq-filter (lambda (choice) (member choice '("once" "always" "reject")))
                                                      (strings (plist-get request :choices) 3))
                                 :fields (delq nil
                                               (mapcar (lambda (field)
                                                         (when-let* (((and (consp field) (keywordp (car field))))
                                                                     (key (drover-remote--line (plist-get field :key) 64)))
                                                           (list :key key
                                                                 :type (let ((type (plist-get field :type)))
                                                                         (if (member type '("string" "multiselect" "boolean"
                                                                                            "number" "integer"))
                                                                             type "string"))
                                                                 :title (or (drover-remote--line (plist-get field :title)) key)
                                                                 :options (strings (plist-get field :options) 50)
                                                                 :custom (eq (plist-get field :custom) t))))
                                                       (seq-take (or (and (vectorp (plist-get request :fields))
                                                                          (plist-get request :fields))
                                                                     [])
                                                                 20))))))
                       (seq-take data 20))))))

(defun drover-remote--encode-requests (requests)
  "Return REQUESTS, as a backend's :requests tells them, as they travel."
  (vconcat
   (mapcar (lambda (request)
             (list :id (format "%s" (plist-get request :id))
                   :kind (symbol-name (or (plist-get request :kind) 'question))
                   :title (or (drover-remote--plain (plist-get request :title)) "")
                   :detail (or (drover-remote--plain (plist-get request :detail)) :false)
                   :choices (vconcat (plist-get request :choices))
                   :fields (vconcat (mapcar (lambda (field)
                                              (list :key (or (plist-get field :key) "")
                                                    :type (or (plist-get field :type) "string")
                                                    :title (or (drover-remote--plain (plist-get field :title)) "")
                                                    :options (vconcat (plist-get field :options))
                                                    :custom (if (plist-get field :custom) t :false)))
                                            (plist-get request :fields)))))
           (seq-take (seq-filter #'consp requests) 20))))

(defun drover-remote--read-answer (request host)
  "Read the answer to REQUEST, of a session of HOST.
As the action \"answer\" sends it: return a plist of :request and
:decision, for a permission, or :answer, for a question, a vector of
[KEY VALUE]."
  (let ((title (format "%s (%s)" (plist-get request :title) host)))
    (if (eq (plist-get request :kind) 'permission)
        (list :request (plist-get request :id)
              :decision (completing-read (format-prompt (concat title (and (plist-get request :detail)
                                                                           (concat "\n" (drover-bound-lines (plist-get request :detail) 6) "\n")))
                                                        "once")
                                         (or (plist-get request :choices) '("once" "always" "reject"))
                                         nil t nil nil "once"))
      (list :request (plist-get request :id)
            :answer
            (vconcat
             (delq nil
                   (mapcar (lambda (field)
                             (let* ((prompt (format "%s · %s: " title (plist-get field :title)))
                                    (options (plist-get field :options))
                                    (value (pcase (plist-get field :type)
                                             ("multiselect" (vconcat (completing-read-multiple prompt options)))
                                             ("boolean" (if (y-or-n-p (concat (string-remove-suffix ": " prompt) "? "))
                                                            t :false))
                                             ((or "number" "integer") (read-number prompt))
                                             (_ (if options
                                                    (completing-read prompt options nil (not (plist-get field :custom)))
                                                  (read-string prompt))))))
                               (and value (not (equal value "")) (vector (plist-get field :key) value))))
                           (plist-get request :fields))))))))

(defun drover-remote--respond (name session)
  "Answer the oldest request of SESSION, a session of the peer NAME, here.
For the proxy's :respond.  What the request asks comes from its owner,
waited for a few seconds; the answer is read here, then the owner gives
it through its backend, its records then telling the request answered."
  (let* ((peer (or (drover-remote--peer name) (user-error "%s is not a peer" name)))
         (label (drover-remote--label peer)))
    (when-let* ((refusal (drover-remote--refusal peer session "answer")))
      (user-error "%s" refusal))
    (pcase-let ((`(,reply ,error) (drover-remote--wait
                                   peer (list :op "requests" :key (drover-remote--wire-key session)) 10)))
      (when error
        (user-error "%s: %s" label error))
      (let ((requests (drover-remote--decode-requests (plist-get reply :requests))))
        (unless requests
          (user-error "%s has no request pending on %s" (drover-session-display-title session) label))
        (let ((answer (drover-remote--read-answer (car requests) label))
              (title (drover-session-display-title session)))
          (drover-remote--act name "answer" session answer
                              (lambda () (drover-message "Answered %s on %s" title label))
                              (lambda (message) (drover-warn "Could not answer %s: %s" title message))))))))

;;;; Performing a peer's actions

(defconst drover-remote--id-regexp "\\`[A-Za-z0-9._-]\\{1,96\\}\\'"
  "What the identifier of an action a peer asks may be.")

(defun drover-remote--refuse-action (peer action why)
  "Note that this host refused PEER's ACTION because of WHY; return WHY.
A refusal is told once per `drover-error-interval' by peer and action."
  (drover-remote--event peer 'refused "refused to %s: %s" action why)
  (when (drover--report-due-p (list "remote refusal" (drover-remote--peer-name peer) action))
    (drover-warn "Refused %s's request to %s: %s" (drover-remote--label peer) action why))
  why)

(defun drover-remote--check-args (action args)
  "Return ARGS of ACTION, received, checked and bounded, as a plist.
A string tells why they do not fit."
  (let ((args (and (consp args) (keywordp (car args)) args)))
    (pcase action
      ("rename" (let ((title (drover-remote--line (plist-get args :title))))
                  (if title (list :title title) "no title")))
      ("send" (let ((text (plist-get args :text)))
                (if (and (stringp text) (not (string-blank-p text))
                         (<= (length text) drover-remote-prompt-limit))
                    (list :text text)
                  "no prompt, or one too long")))
      ("answer"
       (let ((request (drover-remote--line (plist-get args :request) 200))
             (decision (plist-get args :decision))
             (answer (plist-get args :answer)))
         (cond
          ((not request) "no request")
          ((plist-get args :dismiss) (list :request request :dismiss t))
          ((member decision '("once" "always" "reject")) (list :request request :decision decision))
          ((vectorp answer)
           (list :request request
                 :answer (delq nil
                               (mapcar (lambda (pair)
                                         (and (vectorp pair) (= (length pair) 2)
                                              (stringp (aref pair 0)) (<= (length (aref pair 0)) 64)
                                              (let ((value (aref pair 1)))
                                                (cond
                                                 ((stringp value) (cons (aref pair 0) (drover-bound-text value 20 4096)))
                                                 ((and (vectorp value) (seq-every-p #'stringp value))
                                                  (cons (aref pair 0)
                                                        (vconcat (mapcar (lambda (text) (drover-bound-text text 20 4096))
                                                                         (seq-take value 50)))))
                                                 ((or (memq value '(t :false)) (drover-remote--number value))
                                                  (cons (aref pair 0) value))))))
                                       (seq-take answer 20)))))
          (t "no answer"))))
      (_ nil))))

(defun drover-remote--on-act (state peer message reply)
  "Perform the action MESSAGE asks for PEER, answering through REPLY, in STATE.
The answer is (:out OUTCOMES), one per session asked, t or a line.  An
action whose identifier this host performs or performed already is
answered with its outcome rather than performed twice."
  (let* ((id (plist-get message :id))
         (action (plist-get message :action))
         (keys (plist-get message :keys))
         (args (drover-remote--check-args action (plist-get message :args)))
         (outcomes (drover-remote--state-outcomes state))
         (known (and (stringp id) (gethash id outcomes))))
    (cond
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((not (member action drover-remote--actions))
      (funcall reply nil (drover-remote--refuse-action peer (drover-bound-line (format "%s" action) 20)
                                                       "unknown action")))
     ((not (and (vectorp keys) (<= 1 (length keys) drover-remote-action-batch)))
      (funcall reply nil "no sessions, or too many"))
     ((not (drover-remote--allows-p (drover-remote--access peer) action))
      (funcall reply nil (drover-remote--refuse-action
                          peer action (format "%s has %s access" (drover-remote--label peer)
                                              (drover-remote--access peer)))))
     ((stringp args) (funcall reply nil (drover-remote--refuse-action peer action args)))
     ((and known (car known)) (funcall reply (list :out (cdr known))))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (puthash id (list nil reply) outcomes)
      (push id (drover-remote--state-outcome-order state))
      (let ((order (drover-remote--state-outcome-order state)))
        (when (> (length order) drover-remote-outcomes)
          (let ((old (nthcdr drover-remote-outcomes order)))
            (setcdr (nthcdr (1- drover-remote-outcomes) order) nil)
            ;; Those still being performed are kept until done.
            (dolist (gone old)
              (when (car (gethash gone outcomes))
                (remhash gone outcomes))))))
      (drover-remote--stat peer :actions-done (length keys))
      (drover-remote--event peer 'act "asked to %s %d session%s" action (length keys)
                            (if (= (length keys) 1) "" "s"))
      (setf (drover-remote--state-tasks state)
            (append (drover-remote--state-tasks state)
                    (list (list :id id :action action :args args
                                :keys (vconcat (mapcar #'drover-remote--key keys))
                                :out (make-vector (length keys) nil)
                                :pending (make-vector (length keys) nil)
                                :next 0 :left (length keys)
                                :deadline (+ (float-time) drover-remote-action-timeout)))))
      (drover-remote--run-soon state)))))

(defun drover-remote--run-soon (state)
  "Perform the actions of STATE's tasks from a timer."
  (unless (drover-remote--state-runner state)
    (setf (drover-remote--state-runner state)
          (run-at-time 0 nil (let ((link (drover-link state)))
                               (lambda ()
                                 (drover-with-reporting "remote actions"
                                   (let ((state (drover-linked link)))
                                     (setf (drover-remote--state-runner state) nil)
                                     (when (eq state (drover-remote--current))
                                       (drover-remote--run state))))))))))

(defun drover-remote--owner-record (key action)
  "Return this host's record of its session KEY for ACTION, or nil.
A session listed, else one of the older or archived sessions fetched,
else drover's own archive; for restoring or deleting, a record of the
backend's own archive is made from KEY, the backends acting by
identifier."
  (when-let* ((key)
              (backend (car key))
              ((memq backend (drover-backends)))
              ((not (drover-backend-property backend :remote))))
    (or (drover-index-session key)
        (seq-find (lambda (record) (equal (drover-session-key record) key))
                  (append (drover--fetched-sessions 'history) (drover--fetched-sessions 'archived)))
        (and (member action '("unarchive" "delete"))
             (or (seq-find (lambda (record) (equal (drover-session-key record) key))
                           (let ((drover--archived nil)) (drover-archived-sessions)))
                 (drover-session-create :backend backend :id (cdr key)
                                        :archived (and (equal action "unarchive") t)))))))

(defun drover-remote--perform (action args record settle)
  "Perform ACTION with ARGS on RECORD, this host's, then call SETTLE.
SETTLE takes t, or nil and a line.  Nothing shows a window nor asks."
  (cl-flet ((then () (funcall settle t))
            (else (message) (funcall settle nil (or (drover-bound-line (format "%s" message)) "refused"))))
    (pcase action
      ((or "interrupt" "mark-seen")
       (let ((operation (intern (concat ":" action))))
         (if (drover-supports-p record operation)
             (drover--call-confirming record operation #'then #'else)
           (else (format "cannot %s it" action)))))
      ("archive"
       (if (drover-session-archived-p record)
           (then)
         (drover--archive-step record (lambda (ok &optional value)
                                        (if (not ok)
                                            (else value)
                                          (drover--archive-finish record value)
                                          (then))))))
      ("unarchive"
       (if (not (drover-session-archived-p record))
           (then)
         (drover--unarchive-step record (lambda (ok &optional value) (if ok (then) (else value))))))
      ("delete" (drover--delete-step record (lambda (ok &optional value) (if ok (then) (else value)))))
      ("rename" (drover-backend-call record :rename (plist-get args :title)) (then))
      ("send"
       (if (not (drover-supports-p record :submit))
           (else "it takes no prompt from another host")
         (drover-unarchive-to-send record)
         (drover-backend-call record :submit (plist-get args :text) #'then
                              (lambda (message &rest _) (else message)))))
      ("answer"
       (if (not (and (drover-supports-p record :answer)))
           (else "its requests are answered on its host")
         (drover-backend-call record :answer (plist-get args :request)
                              (cond ((plist-get args :dismiss) (list :dismiss t))
                                    ((plist-get args :decision) (list :decision (plist-get args :decision)))
                                    (t (list :answer (plist-get args :answer))))
                              #'then #'else)))
      (_ (else "unknown action")))))

(defun drover-remote--run (state)
  "Perform for a slice the actions of STATE's tasks, and come back for the rest.
Operations whose backends answer later are `drover-bulk-requests' in
flight at most; a task is answered once all its sessions settled, or
failed past its deadline."
  (let ((deadline (+ (float-time) drover-remote-send-slice))
        (display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t)))
        (link (drover-link state)))
    (catch 'done
      (dolist (task (copy-sequence (drover-remote--state-tasks state)))
        (when (> (float-time) (plist-get task :deadline))
          (dotimes (index (length (plist-get task :out)))
            (when (aref (plist-get task :pending) index)
              (aset (plist-get task :pending) index nil)
              (cl-decf (drover-remote--state-flight state)))
            (unless (aref (plist-get task :out) index)
              (aset (plist-get task :out) index "its backend did not answer in time")))
          (plist-put task :left 0)
          (plist-put task :next (length (plist-get task :keys))))
        (while (and (< (plist-get task :next) (length (plist-get task :keys)))
                    (< (drover-remote--state-flight state) (max 1 drover-bulk-requests)))
          (when (> (float-time) deadline)
            (throw 'done nil))
          (let* ((index (plist-get task :next))
                 (key (aref (plist-get task :keys) index))
                 (record (drover-remote--owner-record key (plist-get task :action))))
            (plist-put task :next (1+ index))
            (cl-incf (drover-remote--state-flight state))
            (aset (plist-get task :pending) index t)
            (let ((settle (lambda (ok &optional message)
                            (when (aref (plist-get task :pending) index)
                              (aset (plist-get task :pending) index nil)
                              (let ((state (drover-linked link)))
                                (cl-decf (drover-remote--state-flight state))
                                (unless (aref (plist-get task :out) index)
                                  (aset (plist-get task :out) index (if ok t (or message "failed")))
                                  (plist-put task :left (1- (plist-get task :left))))
                                (when (eq state (drover-remote--current))
                                  (drover-remote--run-soon state)))))))
              (if (not record)
                  (funcall settle nil "no such session here")
                (condition-case err
                    (drover-remote--perform (plist-get task :action) (plist-get task :args) record settle)
                  (error (funcall settle nil (drover-error-message err))))))))
        (when (<= (plist-get task :left) 0)
          (drover-remote--finish-task state task))))
    (when (seq-some (lambda (task) (< (plist-get task :next) (length (plist-get task :keys))))
                    (drover-remote--state-tasks state))
      (drover-remote--run-soon state))
    ;; Answers late fail at their deadline.
    (when (and (drover-remote--state-tasks state) (not (drover-remote--state-watch state)))
      (setf (drover-remote--state-watch state)
            (run-at-time (max 0.1 (- (apply #'min (mapcar (lambda (task) (plist-get task :deadline))
                                                          (drover-remote--state-tasks state)))
                                     (float-time)))
                         nil (lambda ()
                               (drover-with-reporting "remote actions"
                                 (let ((state (drover-linked link)))
                                   (setf (drover-remote--state-watch state) nil)
                                   (when (and (eq state (drover-remote--current))
                                              (drover-remote--state-tasks state))
                                     (drover-remote--run state))))))))))

(defun drover-remote--finish-task (state task)
  "Answer TASK, done, to whoever asked it, and keep its outcome in STATE."
  (setf (drover-remote--state-tasks state) (delq task (drover-remote--state-tasks state)))
  (let* ((out (vconcat (mapcar (lambda (outcome) (or outcome "failed")) (plist-get task :out))))
         (entry (gethash (plist-get task :id) (drover-remote--state-outcomes state)))
         (replies (cdr entry)))
    (puthash (plist-get task :id) (cons t out) (drover-remote--state-outcomes state))
    (dolist (reply replies)
      (drover-with-reporting "remote actions"
        (funcall reply (list :out out))))))

(defun drover-remote--on-requests (peer key reply)
  "Answer through REPLY what the pending requests of session KEY ask, for PEER."
  (let ((record (drover-remote--owner-record key "answer")))
    (cond
     ((not (drover-remote--allows-p (drover-remote--access peer) "answer"))
      (funcall reply nil (drover-remote--refuse-action peer "answer"
                                                       (format "%s has %s access" (drover-remote--label peer)
                                                               (drover-remote--access peer)))))
     ((not record) (funcall reply nil "no such session here"))
     ((not (drover-supports-p record :requests)) (funcall reply nil "its requests are answered on its host"))
     (t (drover-backend-call record :requests
                             (lambda (requests)
                               (drover-with-reporting "remote requests"
                                 (if (eq (car-safe requests) :failure)
                                     (funcall reply nil (drover-bound-line (format "%s" (cadr requests))))
                                   (funcall reply (list :requests (drover-remote--encode-requests requests)))))))))))

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
                                          (when-let* ((record (drover-remote-decode-record
                                                               data (drover-remote--peer-name peer)
                                                               (drover-remote--peer-backend peer)
                                                               offset)))
                                            (drover-remote--note-ops peer (drover-session-id record) data)
                                            record))
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
        (condition-case err
            (drover-remote--dispatch state connection peer type message)
          (error
           ;; Kept in the history too, such as a recursion too deep.
           (drover-remote--fail (or peer (drover-remote--dialer-of state connection))
                                (format "handling %s: %s" (drover-bound-line (format "%s" type) 20)
                                        (drover-error-message err)))
           (signal (car err) (cdr err))))))))

(defun drover-remote--dispatch (state connection peer type message)
  "Handle MESSAGE of TYPE from PEER, received on CONNECTION, in STATE."
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
   ((equal type "grant")
    (let ((access (plist-get message :access)))
      (setf (drover-remote--peer-granted peer)
            (and (member access '("read" "act" "full")) (intern access)))))
   ((equal type "bye") (drover-remote--on-bye peer connection message))))

(defun drover-remote--on-bye (peer connection message)
  "Take PEER's farewell MESSAGE on CONNECTION.
A restart or a quit leaves its sessions, the peer coming back; turned
off or dropping this host, it closes PEER for good, its sessions gone,
as an older drover's farewells do."
  (let ((kind (plist-get message :kind))
        (why (drover-bound-line (format "%s" (or (plist-get message :why) "bye")))))
          (when (eq connection (drover-remote--peer-connection peer))
      (drover-remote--event peer 'bye "said bye%s: %s" (if (stringp kind) (concat ", " kind) "") why)
      (pcase kind
        ("restart"
         (setf (drover-remote--peer-bye peer) (list kind why (float-time)))
         (drover-remote--wait-grace peer))
        ("quit"
         (setf (drover-remote--peer-bye peer) (list kind why (float-time))))
        (_ (drover-remote--close peer why))))))

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
        ("act" (drover-remote--on-act state peer message reply))
        ("requests" (drover-remote--on-requests peer key reply))
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
                   (drover-remote--prefetch peer sessions)))
     ;; Actions, which the owner performs and confirms.
     :supports (lambda (session operation) (drover-remote--supports name session operation))
     :unsupported (lambda (session operation) (drover-remote--unsupported name session operation))
     :batches t
     :interrupt (drover-remote--confirming name "interrupt")
     :interrupt-confirms t
     :mark-seen (drover-remote--confirming name "mark-seen")
     :mark-seen-confirms t
     :archive (drover-remote--confirming name "archive")
     :archive-confirms t
     :unarchive (drover-remote--confirming name "unarchive")
     :unarchive-confirms t
     :delete (drover-remote--confirming name "delete")
     :delete-confirms t
     :rename (lambda (session title) (drover-remote--rename name session title))
     :respond (lambda (session) (drover-remote--respond name session))
     :send (lambda (session text token &optional attachments)
             (drover-remote--send-prompt name session text token attachments)))))

(defun drover-remote--add-peer (state name properties &optional ephemeral)
  "Add the peer NAME with PROPERTIES to STATE, registering its backend.
EPHEMERAL non-nil for one accepted without configuration."
  (let ((peer (drover-remote--peer-make :name name :properties properties :ephemeral ephemeral
                                        :backend (drover-remote--backend-name name)
                                        :records (make-hash-table :test #'equal)
                                        :ops (make-hash-table :test #'equal))))
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
  (clrhash (drover-remote--peer-ops peer))
  (drover-remote--fail-outbox peer (format "%s closed: %s" (drover-remote--label peer) why))
  (let ((keys (hash-table-keys (drover-remote--peer-records peer))))
    (clrhash (drover-remote--peer-records peer))
    (drover-remote--report peer keys)))

(defun drover-remote--remove-peer (state peer why &optional kind)
  "Remove PEER from STATE, telling it WHY: its sessions and backend go.
KIND is the farewell (see `drover-remote--bye'), \"dropped\" by default."
  (dolist (candidate (drover-remote--peer-candidates peer))
    (drover-remote--bye (car candidate) (or kind "dropped") why))
  (dolist (timer (list (drover-remote--peer-grace peer) (drover-remote--peer-flusher peer)))
    (when timer (cancel-timer timer)))
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
             (and (drover-remote--peer-error peer) (not (eq state 'open))
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
subscriptions and gaps; for this host serving it, under :serving, among
its counts, :held, the revision of this host the peer holds, which each
change of a session raises while a peer is subscribed, not a count of
sessions, and :dirty, how many sessions changed since are still to send."
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
            (insert (format "  %s\n" (or (drover-remote--refusal peer)
                                         (format "takes actions, granting this host %s access"
                                                 (drover-remote--peer-granted peer)))))
            (when-let* ((error (drover-remote--peer-error peer)))
              (insert (format "  last error, %s ago: %s\n"
                              (opencode-format-age (or (drover-remote--peer-error-time peer) now) now)
                              error)))
            (when-let* ((history (drover-remote--peer-history peer)))
              (insert "  history, newest first:\n")
              (pcase-dolist (`(,time ,kind ,text) (seq-take history 30))
                (insert (format "    %s %-9s %s\n" (format-time-string "%H:%M:%S" time) kind text))))
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
  "A session of another host, as its host tells it: read only.
\\[revert-buffer] shows it anew, as its host tells it now; the keys of
drover's sessions act on it through its host."
  (setq-local drover-session-at-point-function #'drover-remote--shown-session)
  (setq-local revert-buffer-function #'drover-remote--revert)
  (when (bound-and-true-p opencode-remap-headings)
    (dolist (face '(markdown-ts-heading-1 markdown-ts-heading-2 markdown-ts-heading-3
                                          markdown-ts-heading-4 markdown-ts-heading-5 markdown-ts-heading-6
                                          markdown-ts-setext-heading))
      (face-remap-add-relative face 'opencode-heading))))

(defun drover-remote--shown-session ()
  "Return the session the current buffer shows, as its host last told it."
  (when-let* ((shown drover-remote--shown)
              (peer (drover-remote--peer (car shown))))
    (drover-remote--peer-session peer (cdr shown))))

(defun drover-remote--revert (&rest _)
  "Show the session of the current buffer anew, for `revert-buffer-function'."
  (if-let* ((session (drover-remote--shown-session)))
      (drover-remote-show session t)
    (user-error "Its host no longer lists this session")))

(defun drover-remote--markdown-ready-p ()
  "Return non-nil when Markdown can be fontified as OpenCode's buffers fontify it."
  (and (require 'markdown-ts-mode nil t)
       (require 'opencode-render nil t)
       (fboundp 'markdown-ts-mode--initialize)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'markdown)
       (treesit-language-available-p 'markdown-inline)))

(defun drover-remote--markdown (text)
  "Return TEXT, Markdown, fontified as OpenCode's session buffers show it.
Its markup hidden goes, its code blocks are fontified by their
languages; of its properties, only faces, tooltips and displays that
are strings stay.  TEXT plain when Markdown cannot be fontified here."
  (or (and (stringp text)
           (drover-remote--markdown-ready-p)
           (condition-case nil
               (with-temp-buffer
                 (insert text)
                 (setq-local markdown-ts-hide-markup t
                             markdown-ts-enable-code-block-context-mode nil
                             markdown-ts-enable-table-mode nil)
                 (let ((delay-mode-hooks t))
                   (markdown-ts-mode--initialize))
                 (setq-local font-lock-defaults
                             '(nil nil nil nil
                                   (font-lock-fontify-syntactically-function . opencode-fontify-markdown)))
                 (setq-local font-lock-fontify-syntactically-function #'opencode-fontify-markdown)
                 (font-lock-mode 1)
                 (font-lock-ensure)
                 (let ((pieces nil)
                       (position (point-min)))
                   (while (< position (point-max))
                     (let ((next (next-property-change position nil (point-max))))
                       (unless (get-text-property position 'invisible)
                         (let* ((piece (buffer-substring-no-properties position next))
                                (face (get-text-property position 'face))
                                (help (get-text-property position 'help-echo))
                                (display (get-text-property position 'display)))
                           (set-text-properties
                            0 (length piece)
                            (append (and face (list 'face face 'font-lock-face face))
                                    (and (stringp help) (list 'help-echo (drover-bound-line help)))
                                    (and (stringp display) (list 'display (substring-no-properties display))))
                            piece)
                           (push piece pieces)))
                       (setq position next)))
                   (apply #'concat (nreverse pieces))))
             (error nil)))
      text))

(defun drover-remote--section (title)
  "Return the heading of a section of a shown session, TITLE, on its line."
  (concat "\n" (drover-face title 'drover-heading) "\n"))

(defun drover-remote--field (label value)
  "Return the line of a shown session telling VALUE, under LABEL, or nil."
  (when (and value (not (equal value "")))
    (concat (drover-face (format "%-11s" label) 'drover-dim) value "\n")))

(defun drover-remote--details (peer session)
  "Return what SESSION, of PEER, is, as its view's first lines show it."
  (let ((status (drover-session-status session)))
    (concat
     (opencode-session-status-glyph status) " "
     (drover-face (drover-bound-line (format "%s" (drover-session-display-title session))) 'drover-heading)
     "\n\n"
     (drover-remote--field "Host" (concat (drover-remote--label peer)
                                          (and (drover-session-stale session)
                                               (drover-face (concat "  " (drover-remote--state-line peer))
                                                            'drover-host-disconnected))))
     (drover-remote--field "Status" (drover-face (opencode-session-status-label status)
                                                 (opencode-session-status-face status)))
     (drover-remote--field "Harness" (concat (drover-harness-glyph (drover-session-harness session)) " "
                                             (drover-harness-name (drover-session-harness session))))
     (drover-remote--field "Agent" (or (drover-session-agent-display session) (drover-session-agent session)))
     (drover-remote--field "Model" (string-join
                                    (delq nil (list (drover-session-model session)
                                                    (and (drover-session-effort session)
                                                         (drover-face (drover-session-effort session)
                                                                      (if (drover-session-effort-default session)
                                                                          'opencode-setting-inherited
                                                                        'opencode-setting-chosen)))))
                                    " "))
     (drover-remote--field "Directory" (drover-session-directory session))
     (drover-remote--field "Branch" (drover-session-branch session))
     (mapconcat (lambda (request)
                  (drover-remote--field (if (eq (plist-get request :kind) 'question) "Asks" "Permit")
                                        (drover-face (drover-bound-line (plist-get request :summary))
                                                     'opencode-session-status-waiting)))
                (drover-session-requests session) "")
     (when-let* ((refusal (drover-remote--refusal peer)))
       (drover-remote--field "Read only" (drover-face refusal 'drover-dim))))))

(defun drover-remote--recap-text (recap)
  "Return RECAP, as received, as the view of a session shows it."
  (if-let* ((failure (plist-get recap :failure)))
      (concat (drover-remote--section "Last turn") (drover-face failure 'drover-dim) "\n")
    (concat
     (and (plist-get recap :prompt)
          (concat (drover-remote--section "Last prompt")
                  (drover-remote--markdown (plist-get recap :prompt)) "\n"))
     (and (plist-get recap :changes)
          (concat (drover-remote--section "Files changed")
                  (mapconcat (lambda (change)
                               (concat (car change)
                                       (and (nth 1 change) (drover-face (format "  +%d" (nth 1 change)) 'success))
                                       (and (nth 2 change) (drover-face (format " -%d" (nth 2 change)) 'error))
                                       "\n"))
                             (plist-get recap :changes) "")))
     (and (plist-get recap :error)
          (concat (drover-remote--section "Error")
                  (drover-face (plist-get recap :error) 'error) "\n")))))

(defun drover-remote-show (session &optional again)
  "Show SESSION, a session of another host, as that host tells it, read only.
What it is, its last reply, Markdown fontified as OpenCode's buffers
show it, its last prompt and the files its last turn changed, fetched
through its host.  Its turn ended unseen is marked seen there, as
viewing it there would.  AGAIN non-nil shows it anew in the current
window, without taking another."
  (let* ((host (drover-session-host session))
         (peer (or (drover-remote--peer host) (user-error "No peer %s" host)))
         (buffer (get-buffer-create (format "*drover %s: %s*" (drover-remote--label peer)
                                            (drover-bound-line (drover-session-display-title session) 60)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'drover-remote-session-mode)
        (drover-remote-session-mode))
      (setq drover-remote--shown (cons (drover-remote--peer-name peer) (drover-session-id session)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (drover-remote--details peer session))
        (let ((preview-at (point-marker))
              (recap-at (point-marker)))
          (set-marker-insertion-type recap-at t)
          (cl-flet ((put (marker text)
                      (when (and (buffer-live-p buffer) text)
                        (with-current-buffer buffer
                          (let ((inhibit-read-only t))
                            (save-excursion (goto-char marker) (insert text)))))))
            (unless (drover-session-stale session)
              (drover-remote--preview peer session
                                      (lambda (text)
                                        (put preview-at
                                             (concat (drover-remote--section "Last reply")
                                                     (if text (drover-remote--markdown text)
                                                       (drover-face "Nothing to show." 'drover-dim))
                                                     "\n"))))
              (drover-remote--recap peer session
                                    (lambda (recap) (put recap-at (drover-remote--recap-text recap)))))))
        (goto-char (point-min))))
    (when (and (memq (drover-session-status session) '(done failed))
               (not (drover-remote--refusal peer session "mark-seen")))
      (drover-remote--act (drover-remote--peer-name peer) "mark-seen" session nil #'ignore
                          (let ((link (drover-link peer)))
                            (lambda (message)
                              (drover-remote--event (drover-linked link) 'error "marking seen: %s" message)))))
    (unless again
      (pop-to-buffer-same-window buffer))
    buffer))

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
    (add-hook 'kill-emacs-hook #'drover-remote--kill-emacs)
    (setq drover-hosts-function #'drover-remote--host-names
          drover-host-label-function #'drover-remote--host-label
          drover-host-state-function #'drover-remote--host-state
          drover-links-remote-function #'drover-remote--open-conversation)
    (drover-remote-reconfigure)
    ;; Connections made before, by a remote layer of this Emacs gone
    ;; since: their peers are told it restarted, as they still serve it
    ;; and follow its subscription to them, and are greeted again.
    (dolist (connection (spyglass-connections))
      (when (and (spyglass-connection-live-p connection)
                 (not (memq connection (mapcar #'drover-remote--peer-dialed (drover-remote--peers)))))
        (drover-remote--bye connection "restart" (format "%s restarted its remote layer" drover-host))
        (drover-remote--hello connection t)))
    state))

(defun drover-remote--kill-emacs ()
  "Tell the peers this Emacs quits, so that they keep its sessions, dimmed.
For `kill-emacs-hook'."
  (drover-with-reporting "remote quit"
    (when-let* ((state (drover-remote--current)))
      (dolist (peer (drover-remote--peers))
        (dolist (candidate (drover-remote--peer-candidates peer))
          (when (spyglass-connection-live-p (car candidate))
            (drover-remote--bye (car candidate) "quit" (format "%s's Emacs quits" drover-host))))))))

(defun drover-remote--stop (&optional kind)
  "Tell the peers this host stops, close what this host dialed, forget them.
KIND is the farewell, \"off\" by default, or \"restart\" when the mode
starts again at once: its peers keep this host's sessions meanwhile."
  (when-let* ((state drover-remote--state))
    (when (drover-layout-current-p state 'drover-remote--state)
      (dolist (peer (drover-remote--peers))
        (drover-remote--remove-peer state peer (format "%s %s" drover-host
                                                       (if (equal kind "restart") "restarted" "stopped"))
                                    (or kind "off")))
      (when (drover-remote--state-runner state)
        (cancel-timer (drover-remote--state-runner state)))
      (when (drover-remote--state-watch state)
        (cancel-timer (drover-remote--state-watch state)))
      (maphash (lambda (connection _sub) (drover-remote--unsubscribe state connection))
               (copy-hash-table (drover-remote--state-subs state)))
      (when (drover-remote--state-started-server state)
        (spyglass-server-stop))))
  (when (fboundp 'spyglass-unregister-channel)
    (spyglass-unregister-channel drover-remote-channel))
  (remove-hook 'drover-changed-hook #'drover-remote--owner-changed)
  (remove-hook 'spyglass-error-functions #'drover-remote--spyglass-error)
  (remove-hook 'kill-emacs-hook #'drover-remote--kill-emacs)
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
tells of it.  Needs spyglass, with its channels, one dropping a stream
whose write failed, as those offering `spyglass-reconnect' do."
  :global t
  :group 'drover-remote
  (if drover-remote-mode
      (condition-case err
          (progn
            (unless (require 'spyglass nil t)
              (user-error "`drover-remote-mode' needs spyglass, which is not installed"))
            (unless (featurep 'spyglass-channel)
              (user-error "`drover-remote-mode' needs a spyglass with channels"))
            ;; Drover counts on spyglass to drop a stream a write failed on.
            (unless (fboundp 'spyglass-reconnect)
              (user-error "`drover-remote-mode' needs a newer spyglass, one dropping a stream whose write failed"))
            (when drover-remote--state (drover-remote--stop "restart"))
            (drover-start)
            (drover-remote--start))
        (error (setq drover-remote-mode nil)
               (signal (car err) (cdr err))))
    (drover-remote--stop)))

;; Loaded anew, as reloading drover does, the peers' backends take
;; this file's properties.
(when (bound-and-true-p drover-remote-mode)
  (drover-with-reporting "remote reload"
    (mapc #'drover-remote--register (drover-remote--peers))))

(defun drover-remote-unload-function ()
  "Stop the remote layer before its file is unloaded."
  (when drover-remote-mode (drover-remote-mode -1))
  nil)

(provide 'drover-remote)
;;; drover-remote.el ends here
