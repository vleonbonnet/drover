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
;; few requests.  Opening a session of another host whose host runs it
;; in a terminal or a shell mirrors that terminal here, through
;; spyglass: its owner shares it with this host alone, taking its input
;; as its access allows, resuming a recorded conversation in its front
;; end first; other sessions show as their host tells them.  The
;; search of sessions searches what was said in a peer's sessions there,
;; with the peer's own backends, its matches coming as they are found.
;; A conversation is cloned to another host, which takes its files from
;; its owner in pieces and owns the clone (`drover-clone').
;; A peer this host cannot reach is tried less and less often, and at
;; once as the network changes or the machine wakes.  Each peer keeps a history
;; of its link, shown by `drover-remote-status'.  See doc/remote.org for
;; the whole design.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-ui)
(require 'drover-model)
(require 'drover-selection)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 24))
  (load "drover-core" nil t))

(autoload 'drover-wrap-up-session "drover-wrap-up")

;; Spyglass is loaded as the mode starts, not to compile this file.
(defvar spyglass-instance-id)
(defvar spyglass-remote-registration-file)
(declare-function spyglass-server "ext:spyglass-conn" ())
(declare-function drover-review-fetch "drover-review" (session scope callback))
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
(declare-function spyglass-connection-set-resume "ext:spyglass-conn" (connection seconds))
(declare-function drover-conversation-page "drover-conversation" (session before count callback))
(declare-function drover-conversation-follow "drover-conversation" (session from generation callback))
(declare-function drover-conversation-readable-p "drover-conversation" (session))
(declare-function drover-conversation-stop-all "drover-conversation" ())
(declare-function drover-conversation-file "drover-conversation" (session))
(declare-function drover-schedule-load "drover-schedule" ())
(declare-function drover-schedule-add "drover-schedule" (target time prompt &optional instruction reference-time))
(declare-function drover-schedule-add-after-turn "drover-schedule" (target prompt &optional instruction))
(declare-function drover-schedule-deliver "drover-schedule" (target prompt &rest properties))
(declare-function drover-schedule-target "drover-schedule" (&optional buffer))
(declare-function drover-schedule-add-after-trigger "drover-schedule" (target trigger prompt &optional instruction))
(declare-function drover-schedule-cancel "drover-schedule" (&optional job))
(declare-function drover-schedule-retry-job "drover-schedule" (job &optional now))
(declare-function drover-schedule-triggers "drover-schedule" ())
(declare-function drover-schedule--jobs "drover-schedule" ())
(declare-function drover-schedule--job "drover-schedule" (id))
(declare-function drover-schedule--trigger-label "drover-schedule" (trigger))
(declare-function drover-schedule-conversation "drover-schedule" (job))
(declare-function drover-links-conversation "drover-links" (session))
(declare-function drover-links-link "drover-links" (conversation))
(defvar drover-schedule-changed-hook)
(declare-function drover-remote-push-start "drover-remote-push" (&optional restart))
(declare-function drover-remote-push-stop "drover-remote-push" ())
(declare-function drover-remote-push-available-p "drover-remote-push" ())
(declare-function drover-remote-push-on-request "drover-remote-push" (peer message reply))
(declare-function drover-remote-push-on-watch "drover-remote-push" (peer message reply))
(declare-function drover-remote-push-foreground "drover-remote-push" (peer on))
(declare-function drover-remote-push-detached "drover-remote-push" (peer))
(declare-function drover-remote-push-status "drover-remote-push" (peer))
(defvar drover-remote-push-endpoints)
(defvar spyglass-error-functions)
(defvar spyglass-share-state-functions)
(defvar spyglass-mirror-state-functions)
(defvar spyglass-input-function)
(defvar spyglass-key-commands)
(defvar spyglass-mirror-mode-line-function)
(declare-function spyglass-default-input "ext:spyglass-owner" (kind value))
(declare-function comint-interrupt-subjob "comint" ())
(declare-function spyglass-share "ext:spyglass-owner" (&optional buffer &rest arguments))
(declare-function spyglass-share-of "ext:spyglass-owner" (buffer))
(declare-function spyglass-share-id "ext:spyglass-owner" (share))
(declare-function spyglass-shares "ext:spyglass-owner" ())
(declare-function spyglass-share-viewers "ext:spyglass-owner" (share))
(declare-function spyglass-share-buffer "ext:spyglass-owner" (share))
(declare-function spyglass-connection-peer-id "ext:spyglass-conn" (connection))
(declare-function spyglass-unshare "ext:spyglass-owner" (buffer-or-share))
(declare-function spyglass-open "ext:spyglass-mirror" (connection share-id &rest arguments))
(declare-function spyglass-close "ext:spyglass-mirror" (mirror-or-buffer))
(declare-function spyglass-mirror-state "ext:spyglass-mirror" (mirror-or-buffer))
(declare-function spyglass-mirror-closed-reason "ext:spyglass-mirror" (mirror-or-buffer))
(declare-function spyglass-mirror-connection "ext:spyglass-mirror" (mirror-or-buffer))
(declare-function spyglass-mirror-set-closed-reason "ext:spyglass-mirror" (mirror-or-buffer reason))
(declare-function spyglass-reconnect "ext:spyglass-conn" (connection &optional fresh))
(declare-function spyglass-connection-set-reconnect-delays "ext:spyglass-conn" (connection delays))
(declare-function spyglass-connection-next-attempt "ext:spyglass-conn" (connection))
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

 :label         How views name it unless `drover-host-labels' names it;
                NAME by default.
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
 :directories   Where its conversations go when cloned here: an alist
                (THEIRS . OURS) of directories, a directory of its
                under THEIRS cloned under OURS, the first matching;
                otherwise one under its home goes under this host's
                home.  See `drover-clone'.
 :viewer        Non-nil for a host owning no sessions, as a phone: this
                Emacs never subscribes to it, lists it in no view, and
                clones nothing to it.  A host whose drover tells the
                capability \"viewer\" is one too.
 :heartbeat     Heartbeats of its connections, (INTERVAL . TIMEOUT) in
                seconds, instead of `drover-remote-heartbeat': a phone
                on a mobile network may be given longer.
 :resume        Seconds a connection it made waits for it to come back
                once its stream ended, its mirrors resuming, instead of
                spyglass's `spyglass-heartbeat-timeout'.
 :push-hold     Seconds of input in this Emacs during which pushes of
                what is new to it wait, instead of
                `drover-remote-push-hold'; a hold its phone asks for
                when it registers takes precedence.

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

(defcustom drover-remote-backoff '(0.2 1 2 5 10 30 60 120 240 300)
  "Seconds before each attempt to reach again a peer this Emacs dials.
The Nth attempt after its connection broke waits the Nth delay, those
after the last its delay: a peer switched off for the night is tried
every five minutes, not every thirty seconds, each attempt through SSH
starting a process.  The delays start over as the network changes or
the machine wakes from sleep (see `drover-remote-lookout-interval'), and
when `drover-remote-check' is called."
  :type '(repeat number)
  :group 'drover-remote)

(defcustom drover-remote-lookout-interval 15
  "Seconds between two looks at the network, for peers backing off.
A change of this host's network addresses, or a gap between two looks
far longer than this, as after sleep, has peers still unreachable tried
again at once, their delays starting over."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-open-timeout 10
  "Seconds opening a peer's terminal waits for its owner's answer."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-resume-timeout 60
  "Seconds opening a peer's conversation waits for its owner to resume it.
Its owner starts its front end, as opening it there would, then shares
its terminal."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-unshare-delay 60
  "Seconds a terminal this host shared stays shared once no peer mirrors it.
A peer opening it again meanwhile mirrors it at once."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-protocol 1
  "The version of drover's protocol between hosts.
Peers of the same version understand each other; what a version adds is
told in the greeting's capabilities (`drover-remote-capabilities'), so
that a peer running an older drover keeps working without it.")

(defvar drover-remote--acting-peer nil
  "The name of the peer whose action this host performs, while it does.")

(defconst drover-remote-capabilities
  '("actions" "bye" "grant" "mirror" "opencode" "unsent" "search" "clone" "review" "new" "usage" "task"
    "push" "opts" "dirs" "conv" "jobs" "queued" "changes" "wrap-up")
  "What this drover adds to its protocol's version, told in its greeting.
\"actions\": it performs actions on its sessions for its peers, and
tells which each session allows; \"bye\": its farewells say why it goes
\(`restart', `quit', `off' or `dropped'), only those for good closing a
peer; \"grant\": it tells each peer the access it grants it;
\"mirror\": it shares the terminals and shells of its sessions with the
peer opening them, resuming a conversation first when asked;
\"opencode\": it relays its OpenCode service to its peers, as their
access allows, for their opencode.el to open its sessions in full
buffers; \"unsent\": it takes what a buffer the peer mirrors held unsent
as it closed, as archiving closes it, for the peer's kill ring;
\"search\": it searches what was said in its sessions for its peers,
with its own backends, sending the matches as they come; \"clone\": it
exports its conversations for its peers to clone, and takes clones of
theirs from peers of full access; \"review\": it tells the changes its
sessions made, as `drover-review' shows them, for its peers to review;
\"new\": it starts sessions for its peers of act or full access, in
directories of its own they choose among those it lists, on its own
engines' agents and models, which it tells them; \"push\": it pushes
a phone, a peer owning no sessions, what needs the user while it is not
connected, encrypted, through the endpoint and with the key the phone
gave it (see drover-remote-push.el); \"opts\": it tells a session's
agent, model and effort and those its engine offers (request `opts'),
applies others to it (action `options') or before a prompt (`send'
with options), and lists the directories its sessions ran in (request
`dirs'), for a phone to start sessions in; \"conv\": it tells what
was said in a session as entries, read from what its engine records,
pages from the end (request `cv') and lines appended as they come
\(request `cvs', messages `cvd' and `cve', `cvc' stopping it); \"jobs\":
it tells its queue of scheduled prompts as it changes (request and
message `jobs'), the events a prompt may wait for (`triggers'), and
schedules, cancels and retries prompts for its peers (`schedule',
`unschedule', `retry'), delivering them out of every window;
\"task\": it starts
tasks in its own repositories for its peers of act or full access, and
finishes them for those of full access (see `drover-task'); \"queued\":
the answer to an action `send' tells, by session, why a prompt its
session could not take now waits in its queue of prompts (:queued),
rather than say it went in, and it sends such a peer no message `note'
of it; \"changes\": it reads what its sessions changed in all their
turns from its engines (see `drover-changes'), for those a peer shows
\(message `chg', the keys of the sessions), and tells them in their
records (:changes, the lines added and removed and the files) and the
files in their recaps (:files); \"wrap-up\": it wraps its sessions up
for its peers (action `wrap-up', see `drover-wrap-up'), telling the
peer that asked how each ended in a message `note'.")

(defconst drover-remote-action-batch 100
  "Sessions at most in one request of an action on a peer's sessions.")

(defconst drover-remote-capabilities-limit 64
  "Capabilities at most taken of a peer's greeting.
Above the number `drover-remote-capabilities' holds: a peer's greeting
was once cut to 16, and the capabilities told last went unseen.")

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

(defconst drover-remote--share-prefix "drover/"
  "What the identifiers of the terminals this host shares begin with.")

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
  (settable nil :documentation "Which of its options the owner can change of each session, by its key.
Each a list of :agent, :model and :variant; a key absent when the owner
told nothing of it.")
  (serial 0 :documentation "The number of the last action sent.")
  (mirrors nil :documentation "The buffers mirroring its sessions' terminals here, by the owner's key."))

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
  (watch nil :documentation "Timer failing the operations of TASKS not answered in time.")
  (asked (make-hash-table :test #'eq :weakness 'key)
         :documentation "Greetings asked of each connection's peer and not received, by connection.")
  (shares (make-hash-table :test #'equal)
          :documentation "Terminals this host shares with peers, by share identifier.
Each a plist: :buffer, :key, the session's, :viewers, the peers that
asked, as (LINK . NAME), LINK their connection's `drover-link', and
:timer, unsharing it once no peer mirrors it.")
  (network nil :documentation "This host's network addresses at the last look, and when, as (TIME . ADDRESSES)."))

(drover-note-layout 'drover-remote--peer 'drover-remote--sub 'drover-remote--state)

(defvar drover-remote--state nil
  "The state of the remote layer while `drover-remote-mode' is on.")

(defvar drover-remote--jobs-followers (make-hash-table :test #'eq :weakness 'key)
  "Connections whose peers follow this host's queue of scheduled prompts.")

(defvar drover-remote--jobs-timer nil
  "Timer telling the followers the queue changed, coalescing its changes.")

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
        (drover-remote--start t))
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
    (drover-host-label (drover-remote--peer-name peer)
                       (if (and (stringp label) (not (string-empty-p label)))
                           label
                         (drover-remote--peer-name peer)))))

(defun drover-remote--access (peer)
  "Return what PEER may do with this host's sessions: `read', `act' or `full'."
  (let ((access (plist-get (drover-remote--peer-properties peer) :access)))
    (if (memq access '(read act full)) access 'read)))

(defun drover-remote--viewer-p (peer)
  "Return non-nil when PEER owns no sessions, as a phone.
So its configuration says (:viewer), or its drover told it (the
capability \"viewer\"): this host never subscribes to it, nor lists it."
  (or (and (plist-get (drover-remote--peer-properties peer) :viewer) t)
      (and (drover-remote--capable-p peer "viewer") t)))

(defun drover-remote--heartbeat (peer)
  "Return PEER's heartbeats, (INTERVAL . TIMEOUT) in seconds.
Its :heartbeat when that makes one, else `drover-remote-heartbeat'."
  (let ((own (plist-get (drover-remote--peer-properties peer) :heartbeat)))
    (if (and (consp own) (numberp (car own)) (numberp (cdr own)) (< 0 (car own) (cdr own)))
        own
      drover-remote-heartbeat)))

(defun drover-remote--set-resume (peer connection)
  "Give CONNECTION, which PEER made, the resume window of PEER's :resume.
Only a connection the peer dialed waits for it to come back; a spyglass
without resume windows of connections keeps its own."
  (let ((resume (plist-get (drover-remote--peer-properties peer) :resume)))
    (when (and (numberp resume) (> resume 0)
               (not (spyglass-connection-transport connection))
               (fboundp 'spyglass-connection-set-resume))
      (spyglass-connection-set-resume connection resume))))

(defun drover-remote--allows-p (access action)
  "Return non-nil when ACCESS, `read', `act' or `full', allows ACTION, a string."
  (pcase access
    ('full t)
    ('act (not (equal action "delete")))
    (_ nil)))

(defun drover-remote--event (peer kind format-string &rest args)
  "Note in PEER's history an event of KIND, a symbol.
FORMAT-STRING and ARGS tell it.  The history keeps
`drover-remote-history-size' events, each on a line, as (TIME KIND
TEXT COUNT FIRST): an event as the last one, of its kind and text, as
each failed attempt to reach a peer away for the night, is counted
there, COUNT times since FIRST, rather than push the others out."
  (when peer
    (let* ((text (drover-bound-line (apply #'format format-string args) 200))
           (now (float-time))
           (last (car (drover-remote--peer-history peer))))
      (if (and last (eq (nth 1 last) kind) (equal (nth 2 last) text))
          (setcar (drover-remote--peer-history peer)
                  (list now kind text (1+ (or (nth 3 last) 1)) (or (nth 4 last) (nth 0 last))))
        (let ((history (cons (list now kind text 1 now) (drover-remote--peer-history peer))))
          (when (> (length history) drover-remote-history-size)
            (setcdr (nthcdr (1- drover-remote-history-size) history) nil))
          (setf (drover-remote--peer-history peer) history))))))

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
        ;; Written here, by this host's model: peers never summarize
        ;; another host's sessions.
        (put :brief (drover-remote--plain (drover-session-brief record)))
        (put :brief-recap (drover-remote--plain (drover-session-brief-recap record)))
        (put :brief-session (drover-remote--plain (drover-session-brief-session record)))
        ;; Read here, from this host's engines (see `drover-changes').
        (put :changes (drover-remote--encode-totals (drover-session-changes record)))
        (put :ops (drover-remote--record-ops record))
        (put :set (drover-remote--record-settable record)))
      (nreverse out))))

(defun drover-remote--encode-totals (changes)
  "Return CHANGES, a record's slot `changes', as it travels, or nil.
A vector [ADDITIONS DELETIONS FILES], an unknown count false."
  (pcase changes
    (`(,additions ,deletions ,(and (pred natnump) files))
     (vector (or (drover-remote--number additions) :false)
             (or (drover-remote--number deletions) :false)
             files))))

(defun drover-remote--decode-totals (value)
  "Return VALUE, a record's changes as received, as its slot `changes', or nil.
See `drover-remote--encode-totals'."
  (when-let* (((vectorp value))
              ((= (length value) 3))
              (files (drover-remote--count (aref value 2)))
              ((> files 0)))
    (list (drover-remote--count (aref value 0)) (drover-remote--count (aref value 1)) files)))

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
                 (and (drover-supports-p record :submit) "send")
                 (and (drover-supports-p record :apply-options) "options")
                 (and (drover-wrap-up-p record) "wrap-up"))))))

(defconst drover-remote--options
  '(("agent" . :agent) ("model" . :model) ("variant" . :variant))
  "The options of a session a record tells its owner can change, as named there.")

(defun drover-remote--settable (record)
  "Return which of :agent, :model and :variant RECORD's backend can change now.
Nil when its backend applies no options; all three when it applies them
and tells nothing more (see `drover-register-backend', :settable)."
  (cond ((not (drover-supports-p record :apply-options)) nil)
        ((drover-supports-p record :settable)
         (drover-with-reporting "remote options" (drover-backend-call record :settable)))
        (t '(:agent :model :variant))))

(defun drover-remote--record-settable (record)
  "Return which of its options this host can change of RECORD for a peer, or nil.
A vector of names among \"agent\", \"model\" and \"variant\", possibly
empty; nil when RECORD's backend applies no options, which its record's
`ops' tell already."
  (when (drover-supports-p record :apply-options)
    (let ((settable (drover-remote--settable record)))
      (vconcat (delq nil (mapcar (lambda (option)
                                   (and (memq (cdr option) settable) (car option)))
                                 drover-remote--options))))))

(defun drover-remote--symbol (value)
  "Return the symbol VALUE, a string received, names, or nil when it may not.
Only short names of lowercase letters, digits and dashes are interned."
  (and (stringp value)
       (let ((case-fold-search nil))
         (string-match-p drover-remote--symbol-regexp value))
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

(defun drover-remote--seconds (value)
  "Return VALUE when it is a positive number of seconds received, else nil.
A duration may be a float: OpenCode's prompt caches live 3600.0."
  (and (drover-remote--number value) (> value 0) value))

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
        :cache-ttl (drover-remote--seconds (plist-get data :cache-ttl))
        :cache-tokens (drover-remote--count (plist-get data :cache-tokens))
        :cache-prompt-tokens (drover-remote--count (plist-get data :cache-prompt-tokens))
        :agent-display (drover-remote--faced-value (plist-get data :agent-display))
        :context-estimated (eq (plist-get data :context-estimated) t)
        :archived (let ((archived (plist-get data :archived)))
                    (if (eq archived t) t (drover-remote--time archived offset)))
        :brief (drover-remote--line (plist-get data :brief))
        :brief-recap (let ((recap (plist-get data :brief-recap)))
                       (and (stringp recap) (drover-bound-text recap drover-brief-recap-lines)))
        :changes (drover-remote--decode-totals (plist-get data :changes))
        :brief-session (let ((story (plist-get data :brief-session)))
                         (and (stringp story)
                              (drover-bound-text story drover-brief-recap-lines drover-brief-session-limit))))))))

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
  (let ((records nil) (gone nil) (count 0)
        ;; What each record allows reads drover's archive: its file is
        ;; looked at once a message, not once a record.
        (drover--archive-checking (or drover--archive-checking t)))
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

(defun drover-remote-tell-peer (name text)
  "Tell the peer NAME TEXT, a line its user sees, as a message `note'.
Noted in its history too; nothing is told while it is not connected.
For what this host does for a peer and ends later, such as wrapping a
session up (see `drover-wrap-up')."
  (when-let* ((peer (drover-remote--peer name))
              (connection (drover-remote--peer-connection peer))
              (text (drover-bound-line text 300)))
    (drover-remote--event peer 'note "told: %s" text)
    (drover-remote--send connection (list :t "note" :text text))))

(defun drover-remote--hello (connection &optional ask)
  "Greet CONNECTION's peer; with ASK, asking it to greet back."
  (when (and ask drover-remote--state)
    (let ((asked (drover-remote--state-asked drover-remote--state)))
      (puthash connection (1+ (gethash connection asked 0)) asked)))
  (drover-remote--send connection
                       (list :t "hello" :v drover-remote-protocol :host drover-host
                             :inst spyglass-instance-id
                             :dialed (if (spyglass-connection-transport connection) t :false)
                             :ask (if ask t :false)
                             :caps (drover-remote--capabilities)
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
         (asked (gethash connection (drover-remote--state-asked state) 0))
         (answer (and (> asked 0) (not (eq (plist-get message :ask) t))))
         (dialer (drover-remote--dialer-of state connection))
         (peer (and (stringp host) (drover-remote--find-peer host))))
    (when answer
      (if (> asked 1)
          (puthash connection (1- asked) (drover-remote--state-asked state))
        (remhash connection (drover-remote--state-asked state))))
    (cond
     ((and peer (or (null dialer) (eq dialer peer)) (eq connection (drover-remote--peer-connection peer))
           (stringp host) (string-equal-ignore-case host (drover-remote--peer-name peer))
           (integerp version) (= version drover-remote-protocol)
           ;; Not the greeting this host asked for, which may come once
           ;; the connection is kept.
           (not answer))
      ;; Greeted again on the connection kept: its remote layer started
      ;; anew, and serves no subscription of this host's any more.
      ;; Greeted back first: what follows needs the greeting there.
      (when (eq (plist-get message :ask) t)
        (drover-remote--hello connection))
      (drover-remote--greeted peer connection message)
      (drover-remote--event peer 'hello "greeted again: its remote layer restarted, epoch %s"
                            (drover-remote--hello-epoch message))
      (unless (drover-remote--viewer-p peer)
        (drover-remote--resubscribe peer nil)
        (drover-remote--reopen-mirrors peer)))
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
      ;; Greeted back first: what follows needs the greeting there.
      (when (eq (plist-get message :ask) t)
        (drover-remote--hello connection))
      (drover-remote--greeted peer connection message)
      (drover-remote--event peer 'hello "greeted on a connection %s, instance %s, epoch %s%s"
                            (if dialer "this host dialed" "it dialed")
                            (drover-bound-line (format "%s" (plist-get message :inst)) 16)
                            (drover-remote--hello-epoch message)
                            (if (drover-remote--peer-caps peer) "" ", an older drover: read only"))
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
      (let ((heartbeat (drover-remote--heartbeat peer)))
        (spyglass-connection-set-heartbeat connection (float (car heartbeat)) (float (cdr heartbeat))))
      (drover-remote--set-resume peer connection)
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
                           (seq-take caps drover-remote-capabilities-limit)))))
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
  (if (drover-remote--viewer-p peer)
      (drover-remote--event peer 'sub "kept the connection %s: a viewer, owning no sessions, not subscribed to"
                            (if (spyglass-connection-transport connection) "this host dialed" "it dialed"))
    (drover-remote--stat peer :subscriptions)
    (drover-remote--event peer 'sub "kept the connection %s; subscribed holding epoch %s revision %s"
                          (if (spyglass-connection-transport connection) "this host dialed" "it dialed")
                          (or (drover-remote--peer-epoch peer) "none") (or (drover-remote--peer-rev peer) "none"))
    (drover-remote--send connection (list :t "sub"
                                          :epoch (drover-remote--peer-epoch peer)
                                          :rev (drover-remote--peer-rev peer)))
    (drover-remote--resume peer)
    (drover-remote--reopen-mirrors peer)
    (drover-remote--oc-resume peer)))

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
        (cond
         ((string-prefix-p "Sending failed" line)
          (drover-remote--stat peer :write-failures)
          (drover-remote--event peer 'write "%s" line))
         ;; A mirror's: input its owner refused, a key it does not take.
         ((string-prefix-p drover-remote--share-prefix line)
          (drover-remote--event peer 'mirror "%s" line)
          (when (drover--report-due-p (list "remote mirror" (drover-remote--peer-name peer) line))
            (drover-message "%s: %s" (drover-remote--label peer) line)))
         (t
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
                                     (drover-warn "%s: %s" (drover-remote--label peer) line)))))))))))))))

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
    (drover-remote--oc-unrelay connection)
    (drover-remote--oc-drop connection)
    (drover-remote--search-unserve connection)
    (drover-remote--search-drop connection)
    (drover-remote--clone-drop connection)
    (drover-remote--cv-drop connection)
    (remhash connection drover-remote--jobs-followers)
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
          (when (and (eq connection (drover-remote--peer-connection peer))
                     (featurep 'drover-remote-push))
            (drover-remote-push-detached peer))
          (when (eq connection (drover-remote--peer-connection peer))
            (setf (drover-remote--peer-heard peer)
                  (max (or (drover-remote--peer-heard peer) 0)
                       (or (ignore-errors (spyglass-connection-last-recv connection)) 0))
                  (drover-remote--peer-connection peer) nil)
            (drover-remote--reset-queue peer)
            (drover-remote--elect state peer)
            (unless (drover-remote--peer-connection peer)
              (cond
               ;; Nothing of it is listed here to dim.
               ((drover-remote--viewer-p peer) (drover-remote--set-state peer 'waiting))
               ((and theirs (not (drover-remote--quitting-p peer)) (> drover-remote-grace 0))
                (drover-remote--wait-grace peer))
               (t (drover-remote--lost peer)))))
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
    (let ((connection (spyglass-connect (drover-remote--transport peer)
                                        :name (drover-remote--peer-name peer))))
      (drover-remote--back-off connection)
      (setf (drover-remote--peer-dialed peer) connection))))

(defun drover-remote--back-off (connection)
  "Have CONNECTION, dialed, wait `drover-remote-backoff' between its attempts.
A spyglass predating delays of a connection's own keeps its own."
  (when (fboundp 'spyglass-connection-set-reconnect-delays)
    (condition-case err
        (spyglass-connection-set-reconnect-delays connection drover-remote-backoff)
      (error (drover-warn "drover-remote-backoff: %s" (drover-error-message err))))))

(defun drover-remote--retry-now (peer why)
  "Have PEER, which this host dials and waits to try again, tried now.
Its delays start over; WHY, a line, goes to its history.  Return
non-nil when it was waiting."
  (when-let* ((dialed (drover-remote--peer-dialed peer))
              ((not (spyglass-connection-live-p dialed)))
              ((fboundp 'spyglass-connection-next-attempt))
              (next (spyglass-connection-next-attempt dialed))
              ((> next 0.5)))
    (drover-remote--event peer 'retry "%s: trying it now rather than in %s" why (drover-remote--duration next))
    (spyglass-reconnect dialed t)
    t))

(defun drover-remote--network ()
  "Return this host's network interfaces and addresses, to compare."
  (ignore-errors (sort (mapcar (lambda (interface) (format "%s" interface)) (network-interface-list t))
                       #'string<)))

(defun drover-remote--look-out ()
  "Try the peers backing off again at once, if the network changed or Emacs slept.
From a timer every `drover-remote-lookout-interval' seconds: timers do
not run while the machine sleeps, so a gap far longer than the interval
tells it woke."
  (drover-with-reporting "remote lookout"
    (when-let* ((state (drover-remote--current)))
      (let* ((now (float-time))
             (last (drover-remote--state-network state))
             (addresses (drover-remote--network))
             (why (cond ((null last) nil)
                        ((> (- now (car last)) (* 3 drover-remote-lookout-interval))
                         (format "this host woke after %s" (drover-remote--duration (- now (car last)))))
                        ((not (equal addresses (cdr last))) "this host's network changed"))))
        (setf (drover-remote--state-network state) (cons now addresses))
        (when why
          (dolist (peer (drover-remote--peers))
            (drover-remote--retry-now peer why)))))))

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
          (drover-backend-changed backend key))))
    (drover-remote--seen-mirrors peer keys)
    (drover-remote--close-archived-mirrors peer keys)
    (drover-remote--oc-archived peer keys)))

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
                  (drover-remote--forget-ops peer key)
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
                    (drover-remote--forget-ops peer key)
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
  '("interrupt" "mark-seen" "archive" "unarchive" "delete" "rename" "answer" "send" "options"
    "wrap-up")
  "The actions on its sessions a host performs for its peers.")

(defun drover-remote--note-ops (peer key data)
  "Note what the owner allows of its session KEY, as DATA, its record, tells.
That is the actions it performs on it, and which of its options it can
change.  PEER keeps them beside the record; each nil when DATA tells
nothing of it, as an older drover's records."
  (let ((ops (plist-get data :ops))
        (set (plist-get data :set))
        (settable (drover-remote--peer-settable peer)))
    (if (vectorp ops)
        (puthash key (seq-filter (lambda (op) (member op drover-remote--actions)) (seq-take ops 16))
                 (drover-remote--peer-ops peer))
      (remhash key (drover-remote--peer-ops peer)))
    (when settable
      (if (vectorp set)
          (puthash key (delete-dups (delq nil (mapcar (lambda (name)
                                                        (and (stringp name)
                                                             (cdr (assoc name drover-remote--options))))
                                                      (seq-take set 8))))
                   settable)
        (remhash key settable)))))

(defun drover-remote--forget-ops (peer key)
  "Forget what the owner allows of its session KEY, gone from PEER's records."
  (remhash key (drover-remote--peer-ops peer))
  (when-let* ((settable (drover-remote--peer-settable peer)))
    (remhash key settable)))

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

(defun drover-remote--decode-files (value)
  "Return VALUE, files a session changed as received, as `drover-recap' tells them.
A list of (FILE ADDITIONS DELETIONS), a hundred at most, each checked."
  (and (vectorp value)
       (delq nil (mapcar (lambda (change)
                           (and (vectorp change) (= (length change) 3)
                                (stringp (aref change 0))
                                (list (drover-remote--line (aref change 0) 1024)
                                      (drover-remote--count (aref change 1))
                                      (drover-remote--count (aref change 2)))))
                         (seq-take value 100)))))

(defun drover-remote--encode-files (files directory)
  "Return FILES, files a session changed, as they travel.
FILES are (FILE ADDITIONS DELETIONS), as `drover-recap' tells them; those
under DIRECTORY, the session's, are named relative to it, here, where
they are: the peer names them as received."
  (vconcat (delq nil (mapcar (lambda (change)
                               (and (consp change) (stringp (car change))
                                    (vector (substring-no-properties
                                             (drover-relative-file-name (car change) directory))
                                            (or (drover-remote--number (nth 1 change)) :false)
                                            (or (drover-remote--number (nth 2 change)) :false))))
                             (and (listp files) (seq-take files 100))))))

(defun drover-remote--decode-recap (recap)
  "Return RECAP, as received, as `drover-recap' tells one."
  (let ((out nil))
    (dolist (key '(:prompt :reply :error))
      (when-let* ((text (drover-remote--text (plist-get recap key))))
        (setq out (plist-put out key text))))
    (when-let* ((failure (drover-remote--line (plist-get recap :failure))))
      (setq out (plist-put out :failure failure)))
    (dolist (key '(:changes :files))
      (when-let* ((files (drover-remote--decode-files (plist-get recap key))))
        (setq out (plist-put out key files))))
    out))

(defun drover-remote--encode-recap (recap &optional directory)
  "Return RECAP, as `drover-recap' tells one, as it travels.
The files changed under DIRECTORY, the session's, are named relative to
it, here, where they are: the peer names them as received."
  (let ((out nil))
    (dolist (key '(:prompt :reply :error :failure))
      (when-let* ((text (drover-remote--plain (plist-get recap key))))
        (setq out (plist-put out key text))))
    (dolist (key '(:changes :files))
      (when-let* ((files (plist-get recap key)))
        (setq out (plist-put out key (drover-remote--encode-files files directory)))))
    out))

;;;; Reviews

(defconst drover-remote--review-limit (* 2 1024 1024)
  "Characters of patches a review sent to a peer holds at most.")

(defconst drover-remote--review-timeout 120
  "Seconds a peer's review of a session waits for the owner's answer.
The owner may read a long conversation whole first.")

(defun drover-remote--review-older (label)
  "Return why the peer LABEL names tells no changes: it runs an older drover."
  (format "%s runs an older drover, which tells no changes: update it there" label))

(defun drover-remote--review (peer session scope callback)
  "Fetch the changes SESSION made in SCOPE through PEER, for CALLBACK.
CALLBACK gets them as `drover-review-fetch' tells them, the owner
computing them with its own backends; a peer running an older drover
tells none, which the failure says."
  (let ((label (drover-remote--label peer)))
    (if (and (drover-remote--peer-caps peer) (not (drover-remote--capable-p peer "review")))
        (funcall callback (list :failure (drover-remote--review-older label)))
      (drover-remote--request
       peer (list :op "review" :key (drover-remote--wire-key session) :scope (symbol-name scope))
       (lambda (reply error)
         (let ((review (and (null error) (plist-get reply :review))))
           (funcall callback
                    (cond ((and (consp review) (keywordp (car review)))
                           (drover-remote--decode-review review))
                          ((and error (string-match-p "unknown request" error))
                           (list :failure (drover-remote--review-older label)))
                          (t (list :failure (or error (format "%s told no changes" label))))))))
       (+ (float-time) drover-remote--review-timeout)))))

(defun drover-remote--encode-review (review)
  "Return REVIEW, as `drover-review-fetch' tells it, as it travels.
Its patches are cut past `drover-remote--review-limit' characters, the
files after showing their counts, which a note tells."
  (if-let* ((failure (plist-get review :failure)))
      (list :failure (drover-remote--plain failure))
    (let ((room drover-remote--review-limit)
          (cut nil))
      (list :files (vconcat
                    (mapcar (lambda (file)
                              (let ((patch (plist-get file :patch)))
                                (when (and patch (> (length patch) room))
                                  (setq cut (or cut (plist-get file :file)) patch nil room 0))
                                (when patch (setq room (- room (length patch))))
                                (vector (drover-remote--plain (plist-get file :file))
                                        (or (drover-remote--plain (plist-get file :from)) :false)
                                        (symbol-name (plist-get file :status))
                                        (or (drover-remote--number (plist-get file :additions)) :false)
                                        (or (drover-remote--number (plist-get file :deletions)) :false)
                                        (if (plist-get file :binary) t :false)
                                        (or (drover-remote--plain patch) :false))))
                            (plist-get review :files)))
            :base (or (drover-remote--plain (plist-get review :base)) :false)
            :how (or (drover-remote--plain (plist-get review :how)) :false)
            :commits (vconcat (mapcar (lambda (commit) (vector (car commit) (cdr commit)))
                                      (plist-get review :commits)))
            :notes (vconcat (append (plist-get review :notes)
                                    (and cut (list (format "The diff sent is cut past %d MB: from %s on, files show their counts alone"
                                                           (/ drover-remote--review-limit 1024 1024) cut)))))))))

(defun drover-remote--decode-review (review)
  "Return REVIEW, as received, as `drover-review-fetch' tells one.
Every value is checked and bounded: names and notes to lines, patches
to `drover-remote--review-limit' characters in all, files, notes and
commits in number."
  (if-let* ((failure (plist-get review :failure)))
      (list :failure (or (drover-remote--line failure) "failed"))
    (let ((room drover-remote--review-limit)
          (files nil))
      (seq-doseq (file (seq-take (let ((files (plist-get review :files))) (and (vectorp files) files)) 2000))
        (when (and (vectorp file) (= (length file) 7) (stringp (aref file 0)))
          (let ((patch (and (stringp (aref file 6)) (aref file 6))))
            (when (and patch (> (length patch) room))
              (setq patch nil room 0))
            (when patch (setq room (- room (length patch))))
            (push (list :file (drover-remote--line (aref file 0) 1024)
                        :from (drover-remote--line (aref file 1) 1024)
                        :status (cdr (assoc (aref file 2) '(("added" . added) ("deleted" . deleted)
                                                            ("modified" . modified) ("renamed" . renamed))))
                        :additions (drover-remote--count (aref file 3))
                        :deletions (drover-remote--count (aref file 4))
                        :binary (eq (aref file 5) t)
                        :patch patch)
                  files))))
      (list :files (seq-filter (lambda (file) (plist-get file :file)) (nreverse files))
            :base (drover-remote--line (plist-get review :base) 64)
            :how (drover-remote--line (plist-get review :how))
            :commits (delq nil (mapcar (lambda (commit)
                                         (and (vectorp commit) (= (length commit) 2)
                                              (stringp (aref commit 0)) (stringp (aref commit 1))
                                              (cons (drover-remote--line (aref commit 0) 64)
                                                    (or (drover-remote--line (aref commit 1)) ""))))
                                       (seq-take (let ((commits (plist-get review :commits)))
                                                   (and (vectorp commits) commits))
                                                 100)))
            :notes (delq nil (mapcar #'drover-remote--line
                                     (seq-take (let ((notes (plist-get review :notes)))
                                                 (and (vectorp notes) notes))
                                               10)))))))

(defun drover-remote--on-review (served message reply)
  "Tell the changes the session SERVED made, as MESSAGE asks, through REPLY."
  (let ((scope (cdr (assoc (plist-get message :scope) '(("turn" . turn) ("session" . session) ("tree" . tree))))))
    (cond ((not served) (funcall reply nil "no such session here"))
          ((not scope) (funcall reply nil "unknown scope"))
          (t
           (require 'drover-review)
           (drover-review-fetch (car served) scope
                                (lambda (review)
                                  (funcall reply (list :review (drover-remote--encode-review review)))))))))

(defun drover-remote--preview (peer session callback)
  "Fetch where SESSION stands through PEER, and call CALLBACK with it or nil."
  (let ((label (drover-remote--label peer)))
    (drover-remote--request
     peer (list :op "preview" :key (drover-remote--wire-key session))
     (lambda (reply error)
       (funcall callback (if error
                             (format "%s: %s" label error)
                           (drover-remote--text (plist-get reply :text))))))))

(defun drover-remote--prefetch (peer sessions &optional kind)
  "Ask PEER to fetch what the records of SESSIONS lack, at most once a second.
KIND is `details', the default, for what its backends fetch, such as
the size of a context, or `changes', for what the sessions changed,
which a peer telling the capability \"changes\" reads (message `chg').
The keys pending are sent together once the second passed."
  (let ((pending (or (car (drover-remote--peer-prefetch peer)) (make-hash-table :test #'equal)))
        (kind (or kind 'details)))
    (dolist (session sessions)
      (let ((kinds (gethash (drover-session-id session) pending)))
        ;; An older drover's pending keys hold t: what it fetches.
        (puthash (drover-session-id session) (cl-adjoin kind (if (listp kinds) kinds '(details)))
                 pending)))
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
                                         (drover-remote--send-prefetch peer connection pending))))))))))
    (setcar (drover-remote--peer-prefetch peer) pending)))

(defun drover-remote--send-prefetch (peer connection pending)
  "Send PEER, on CONNECTION, the keys of PENDING to fetch, by kind.
PENDING maps the owner's keys to the kinds asked, as
`drover-remote--prefetch' notes them: message `pre' for the details,
`chg' for the changes, which only a peer telling the capability
\"changes\" is sent; 500 keys at most each."
  (let ((details nil)
        (changes nil))
    (maphash (lambda (key kinds)
               (let ((kinds (if (listp kinds) kinds '(details))))
                 (when (memq 'details kinds) (push key details))
                 (when (memq 'changes kinds) (push key changes))))
             pending)
    (cl-flet ((send (type keys)
                (when keys
                  (drover-remote--send
                   connection
                   (list :t type
                         :keys (vconcat (mapcar (lambda (key) (vector (symbol-name (car key)) (cdr key)))
                                                (seq-take keys 500))))))))
      (send "pre" details)
      (when (drover-remote--capable-p peer "changes")
        (send "chg" changes)))))

;;;; Acting on a peer's sessions

(defconst drover-remote--operations
  '((:interrupt . "interrupt") (:mark-seen . "mark-seen") (:archive . "archive")
    (:unarchive . "unarchive") (:delete . "delete") (:rename . "rename")
    (:respond . "answer") (:send . "send") (:submit . "send")
    (:requests . "answer") (:answer . "answer") (:apply-options . "options")
    (:wrap-up . "wrap-up"))
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
   ((and (equal action "options") (not (drover-remote--capable-p peer "opts")))
    (format "%s runs an older drover, which sets no options for other hosts: update it"
            (drover-remote--label peer)))
   ((and action (not (drover-remote--allows-p (or (drover-remote--peer-granted peer) 'read) action)))
    (format "%s grants this host %s access" (drover-remote--label peer)
            (or (drover-remote--peer-granted peer) "read")))
   ((and session action
         (let ((ops (gethash (drover-session-id session) (drover-remote--peer-ops peer) 'unknown)))
           (and (listp ops) (not (member action ops)))))
    (format "%s does not %s" (drover-remote--label peer)
            (if (equal action "wrap-up") "wrap it up" (concat action " it"))))))

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
once it refused or failed, at most one of them once; for ACTION `send',
THEN gets a line instead when the owner queued the prompt, telling why
it waits.  Actions asked
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
       (let ((out (and (null error) (plist-get reply :out)))
             (queued (and (null error) (equal action "send") (plist-get reply :queued))))
         (seq-do-indexed
          (lambda (entry index)
            (let ((outcome (and (vectorp out) (< index (length out)) (aref out index)))
                  (why (and (vectorp queued) (< index (length queued))
                            (drover-remote--line (aref queued index) 300))))
              (drover-with-reporting "remote actions"
                (cond ((and (eq outcome t) why) (funcall (nth 3 entry) why))
                      ((eq outcome t) (funcall (nth 3 entry)))
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
prompt, or queues it while the session cannot take it, which is told;
attachments, files of this host, do not travel."
  (let ((title (drover-session-display-title session))
        (label (drover-host-label name)))
    (if (> (length text) drover-remote-prompt-limit)
        (drover-warn "The prompt for %s is longer than %d characters, which go to another host at most; it is in the kill ring"
                     title drover-remote-prompt-limit)
      (drover-remote--act name "send" session (list :text (substring-no-properties text))
                          (lambda (&optional queued)
                            (if queued
                                (drover-message "%s on %s queued the prompt: %s" title label queued)
                              (drover-message "%s took the prompt" title)))
                          (lambda (message)
                            (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                                         title message))))))

(defun drover-remote--submit (name session text then else)
  "Have the peer NAME submit TEXT as a prompt of SESSION, then call THEN, or ELSE.
For the proxy's :submit: the owner submits it through its backend, as it
delivers a scheduled prompt, and THEN is called once it did, with a line
telling why the prompt waits when the owner queued it instead, ELSE with
a line once it refused or failed."
  (if (> (length text) drover-remote-prompt-limit)
      (funcall else (format "the prompt is longer than %d characters, which go to another host at most"
                            drover-remote-prompt-limit))
    (drover-remote--act name "send" session (list :text (substring-no-properties text)) then else)))

(defun drover-remote--apply-options-there (name session options then &optional else)
  "Have the peer NAME apply OPTIONS to SESSION, then call THEN once.
For the proxy's :apply-options: OPTIONS, explicit and filtered for
SESSION already, go as the action `options', a model as the peer named
it, [PROVIDER, ID].  ELSE receives a refusal or failure string.  Without
ELSE, a failure is reported and THEN runs for legacy callers."
  (let* ((agent (plist-get options :agent))
         (model (plist-get options :model))
         (variant (plist-get options :variant))
         (args (append
                (and (stringp agent) (not (string-empty-p agent))
                     (list :agent (substring-no-properties (drover-bound-line agent 200))))
                (and (drover-model-p model) (drover-model-id model)
                     (list :model (vector (or (drover-model-provider-id model) :false)
                                          (substring-no-properties (drover-model-id model)))))
                (and variant (symbolp variant) (list :variant (symbol-name variant))))))
    (if (null args)
        (funcall then)
      (drover-remote--act name "options" session args
                          (lambda (&rest _) (funcall then))
                          (lambda (message)
                            (if else (funcall else message)
                              (drover-warn "%s did not apply the options: %s"
                                           (drover-session-display-title session) message)
                              (funcall then)))))))

(defun drover-remote--settable-there (name session)
  "Return which of :agent, :model and :variant the peer NAME can change of SESSION.
For the proxy's :settable, as the owner's record told; all three when
it told nothing, as an older drover's records."
  (let ((settable (when-let* ((peer (drover-remote--peer name)))
                    (drover-remote--peer-settable peer))))
    (if settable
        (gethash (drover-session-id session) settable '(:agent :model :variant))
      '(:agent :model :variant))))

(defun drover-remote--fetch-requests (name session callback)
  "Fetch from the peer NAME what the pending requests of SESSION ask.
For the proxy's :requests: CALLBACK gets them, as the owner's backend
tells them, checked and bounded, or (:failure MESSAGE).  Nothing waits."
  (let ((peer (drover-remote--peer name)))
    (if-let* ((refusal (if peer (drover-remote--refusal peer session "answer") (format "%s is not a peer" name))))
        (funcall callback (list :failure refusal))
      (drover-remote--request
       peer (list :op "requests" :key (drover-remote--wire-key session))
       (lambda (reply error)
         (funcall callback (if error
                               (list :failure error)
                             (drover-remote--decode-requests (plist-get reply :requests)))))))))

(defun drover-remote--answer-request (name session id answer then else)
  "Have the peer NAME answer the request ID of SESSION with ANSWER.
For the proxy's :answer, ANSWER as the backends' :answer takes it; THEN
is called once the owner answered, ELSE with a line."
  (let ((args (cond
               ((plist-get answer :dismiss) (list :request id :dismiss t))
               ((plist-get answer :decision) (list :request id :decision (plist-get answer :decision)))
               (t (list :request id
                        :answer (vconcat (mapcar (lambda (pair) (vector (car pair) (cdr pair)))
                                                 (plist-get answer :answer))))))))
    (drover-remote--act name "answer" session args then else)))

(defun drover-remote--silence (peer)
  "Return how long PEER has been silent, in seconds, when it seems frozen.
Nil while it is not.  A peer answers the pings spyglass sends once it
has been silent its heartbeat interval (see `drover-remote--heartbeat'),
looked at every half interval: silent twice the interval, it is
stopped, busy for long, or frozen in a question, though its connection
is dropped only past the heartbeat's timeout.  Silent while this Emacs
could hear it, as spyglass counts it (`spyglass-connection-silence'):
right after a command kept Emacs, the peer's answer waits unread, and
the wait for it gave up at once."
  (when-let* ((connection (drover-remote--peer-connection peer))
              ((spyglass-connection-live-p connection))
              (silent (ignore-errors
                        (if (fboundp 'spyglass-connection-silence)
                            (spyglass-connection-silence connection)
                          (- (float-time) (spyglass-connection-last-recv connection))))))
    (and (> silent (* 2 (car (drover-remote--heartbeat peer)))) silent)))

(defun drover-remote--await (peer done timeout)
  "Wait in a command until DONE, a function of no argument, returns non-nil.
Emacs reads processes and runs timers meanwhile.  Return t once DONE
did, else why the wait stopped, a line: TIMEOUT seconds passed; PEER,
which DONE waits for, fell silent (see `drover-remote--silence'), as a
peer frozen in a question stays until its connection is dropped; or a
key was typed, which stops the wait without quitting, the key read
next.  Past a second, the echo area tells what it waits for."
  (let* ((start (float-time))
         (deadline (+ start timeout))
         (label (drover-remote--label peer))
         (told nil)
         (why nil)
         (done-p nil))
    (while (not (or (setq done-p (funcall done)) why))
      (setq why (cond ((>= (float-time) deadline)
                       (format "%s did not answer in %ss" label timeout))
                      ((when-let* ((silent (drover-remote--silence peer)))
                         (format "%s has not been heard from for %ds" label (round silent))))
                      ((input-pending-p) (format "stopped waiting for %s" label))))
      (unless why
        (when (and (not told) (> (- (float-time) start) 1))
          (setq told t)
          (let ((message-log-max nil))
            (message "Waiting for %s... (any key stops waiting)" label)))
        (accept-process-output nil 0.02)))
    (when told
      (let ((message-log-max nil))
        (message nil)))
    (if done-p t why)))

(defun drover-remote--wait (peer message timeout)
  "Ask PEER MESSAGE and wait for its answer, TIMEOUT seconds at most.
Return (REPLY ERROR).  Only for a command that must answer from it:
Emacs reads processes and runs timers meanwhile, and the wait stops
early when PEER falls silent or a key is typed (see
`drover-remote--await')."
  (let ((answer nil))
    (drover-remote--request peer message (lambda (reply error) (setq answer (list reply error)))
                            (+ (float-time) timeout) t)
    (let ((outcome (drover-remote--await peer (lambda () answer) timeout)))
      (or answer (list nil outcome)))))

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
                              (lambda ()
                                (drover-message "Answered %s on %s" title label)
                                (drover-note-handed-off session 'answer 'drover))
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

(defun drover-remote--check-options (args required)
  "Return the options ARGS of an action carry, checked: (:agent :model :variant).
:model is (PROVIDER . ID), PROVIDER nil for a model known by its bare
identifier.  A string tells why they do not fit; with REQUIRED, when
none is given."
  (let ((agent (plist-get args :agent))
        (model (plist-get args :model))
        (variant (plist-get args :variant)))
    (cond
     ((not (or (memq agent '(nil :false)) (drover-remote--line agent 200)))
      "an agent of no known shape")
     ((not (or (memq model '(nil :false))
               (and (vectorp model) (= (length model) 2)
                    (or (eq (aref model 0) :false) (drover-remote--line (aref model 0) 200))
                    (drover-remote--line (aref model 1) 200))))
      "a model of no known shape")
     ((not (or (memq variant '(nil :false)) (drover-remote--symbol variant)))
      "a variant of no known shape")
     ((and required (memq agent '(nil :false)) (memq model '(nil :false)) (memq variant '(nil :false)))
      "no options")
     (t (list :agent (and (stringp agent) (drover-remote--line agent 200))
              :model (and (vectorp model)
                          (cons (and (stringp (aref model 0)) (drover-remote--line (aref model 0) 200))
                                (drover-remote--line (aref model 1) 200)))
              :variant (and (stringp variant) (drover-remote--symbol variant)))))))

(defun drover-remote--check-args (action args)
  "Return ARGS of ACTION, received, checked and bounded, as a plist.
A string tells why they do not fit."
  (let ((args (and (consp args) (keywordp (car args)) args)))
    (pcase action
      ("rename" (let ((title (drover-remote--line (plist-get args :title))))
                  (if title (list :title title) "no title")))
      ("send" (let ((text (plist-get args :text))
                    (options (drover-remote--check-options args nil)))
                (cond
                 ((not (and (stringp text) (not (string-blank-p text))
                            (<= (length text) drover-remote-prompt-limit)))
                  "no prompt, or one too long")
                 ((stringp options) options)
                 (t (append (list :text text) options)))))
      ("options" (drover-remote--check-options args t))
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
The answer is (:out OUTCOMES), one per session asked, t or a line, and
:queued for prompts queued (see `drover-remote--finish-task').  An
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
     ((and known (car known))
      ;; An outcome kept by an older drover is its vector alone.
      (funcall reply (if (vectorp (cdr known)) (list :out (cdr known)) (cdr known))))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (drover-remote--begin-outcome state id reply)
      (drover-remote--stat peer :actions-done (length keys))
      (drover-remote--event peer 'act "asked to %s %d session%s" action (length keys)
                            (if (= (length keys) 1) "" "s"))
      (setf (drover-remote--state-tasks state)
            (append (drover-remote--state-tasks state)
                    (list (list :id id :action action :args args
                                :peer (drover-remote--peer-name peer)
                                :keys (vconcat (mapcar #'drover-remote--key keys))
                                :out (make-vector (length keys) nil)
                                :queued (make-vector (length keys) nil)
                                :pending (make-vector (length keys) nil)
                                :next 0 :left (length keys)
                                :deadline (+ (float-time) drover-remote-action-timeout)))))
      (drover-remote--run-soon state)))))

(defun drover-remote--begin-outcome (state id reply)
  "Note in STATE that the request ID, a peer's, is being performed for REPLY.
Its outcome is kept once done, for the last `drover-remote-outcomes'
requests, those still being performed kept until done."
  (let ((outcomes (drover-remote--state-outcomes state)))
    (puthash id (list nil reply) outcomes)
    (push id (drover-remote--state-outcome-order state))
    (let ((order (drover-remote--state-outcome-order state)))
      (when (> (length order) drover-remote-outcomes)
        (let ((old (nthcdr drover-remote-outcomes order)))
          (setcdr (nthcdr (1- drover-remote-outcomes) order) nil)
          (dolist (gone old)
            (when (car (gethash gone outcomes))
              (remhash gone outcomes))))))))

(defun drover-remote--queue-prompt (record args peer-name)
  "Queue the prompt ARGS carry for RECORD, when it cannot take it now.
For a peer PEER-NAME sending it, ARGS holding :text and its options.  A
session running its agent in its buffer (:runs-in-buffer) while its turn
runs, or whose buffer cannot take a prompt now without touching what its
input holds (its backend's :blocker), gets it from this host's queue of
prompts (`drover-schedule'), saved there: after its turn, as a prompt
scheduled \"when turn ends\" here is, else once it can take it.  Never
typed over a turn, nor lost.  Return (queued . WHY) when queued, WHY a
line telling why it waits and until when, which the answer to the peer
carries (capability \"queued\"), and a peer of an older drover is told in
a message \"note\"; a string saying why when it must wait but cannot be
queued, its conversation not known yet; nil when the session takes it
now, or drover-schedule is not there."
  (let* ((buffer (drover-session-buffer record))
         (running (and (drover-backend-property record :runs-in-buffer)
                       (drover--running-p record)))
         (blocker (and buffer (not running)
                       (functionp (drover-backend-property record :blocker))
                       (drover-with-reporting "remote send"
                         (drover-backend-call record :blocker)))))
    (when (and (or running (stringp blocker)) (require 'drover-schedule nil t))
      (require 'drover-links)
      (let* ((peer (and peer-name (drover-remote--peer peer-name)))
             (title (drover-session-display-title record))
             (blocker (and (stringp blocker) (or (drover-bound-line blocker 200) "it is busy")))
             (why (if running "its turn runs" blocker))
             (conversation (drover-links-conversation record)))
        (if (not (and peer conversation))
            (format "%s, and its conversation is not known yet to queue the prompt" why)
          (let ((target (drover-remote--schedule-target record conversation args peer))
                (instruction (format "sent from %s" (drover-remote--label peer)))
                (text (plist-get args :text)))
            (drover-quietly
              (if running
                  (drover-schedule-add-after-turn target text instruction)
                (drover-schedule-deliver target text :origin instruction)))
            (drover-remote--event peer 'act "queued its prompt for %s: %s" title why)
            (when-let* (((not (drover-remote--capable-p peer "queued")))
                        (connection (drover-remote--peer-connection peer)))
              (drover-remote--send connection
                                   (list :t "note"
                                         :text (if running
                                                   (format "%s runs a turn on %s: the prompt goes once it ends, queued there"
                                                           title drover-host)
                                                 (format "%s on %s cannot take a prompt now (%s): it goes once it can, queued there"
                                                         title drover-host blocker)))))
            (cons 'queued
                  (if running
                      "its turn runs: the prompt goes once it ends"
                    (format "it cannot take a prompt now (%s): the prompt goes once it can" blocker)))))))))

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
SETTLE takes t, or nil and a line, or `queued' and a line telling why a
prompt sent waits, queued.  Nothing shows a window nor asks."
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
       (let* ((options (or (plist-get args :agent) (plist-get args :model) (plist-get args :variant)))
              ;; Its turn running, the prompt waits for it to end, in this
              ;; host's queue of prompts, saved: never typed over a turn nor lost.
              (queued (and (drover-supports-p record :submit)
                           (drover-remote--queue-prompt record args drover-remote--acting-peer))))
         (cond
          ((not (drover-supports-p record :submit))
           (else "it takes no prompt from another host"))
          ((stringp queued) (else queued))
          (queued (funcall settle 'queued (cdr queued)))
          ((not options)
           (drover-unarchive-to-send record)
           (drover-backend-call record :submit (plist-get args :text) #'then
                                (lambda (message &rest _) (else message))))
          (t
           ;; Its options first, as the prompt applies them before sending.
           (drover-remote--apply-options
            record args
            (lambda (_ok &optional _message)
              (drover-unarchive-to-send record)
              (drover-backend-call record :submit (plist-get args :text) #'then
                                   (lambda (message &rest _) (else message)))))))))
      ("wrap-up"
       (condition-case err
           (progn (drover-wrap-up-session record drover-remote--acting-peer)
                  (then))
         (error (else (drover-error-message err)))))
      ("options"
       (if (not (drover-supports-p record :apply-options))
           (else "its options are set on its host")
         (drover-remote--apply-options record args
                                       (lambda (ok &optional message)
                                         (if ok (then) (else message))))))
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
flight at most, the next sent as answers come; a task is answered once
all its sessions settled, or failed past its deadline."
  (let ((deadline (+ (float-time) drover-remote-send-slice))
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
                                  (when-let* (((eq ok 'queued))
                                              (queued (plist-get task :queued)))
                                    (aset queued index (or message "queued")))
                                  (plist-put task :left (1- (plist-get task :left))))
                                (when (eq state (drover-remote--current))
                                  (drover-remote--run-soon state)))))))
              (if (not record)
                  (funcall settle nil "no such session here")
                (condition-case err
                    (let ((drover-remote--acting-peer (plist-get task :peer)))
                      (drover-quietly
                        (drover-remote--perform (plist-get task :action) (plist-get task :args) record settle)))
                  (error (funcall settle nil (drover-error-message err))))))))
        (when (<= (plist-get task :left) 0)
          (drover-remote--finish-task state task))))
    ;; Back at once only when the slice ran out, a request free to go:
    ;; with all in flight, each answer brings the runner back.  A timer
    ;; due again and again keeps Emacs from reading any process, the
    ;; very answers awaited among them (see `wait_reading_process_output').
    (when (and (< (drover-remote--state-flight state) (max 1 drover-bulk-requests))
               (seq-some (lambda (task) (< (plist-get task :next) (length (plist-get task :keys))))
                         (drover-remote--state-tasks state)))
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

(defun drover-remote--apply-options (record args settle)
  "Apply the options ARGS carry to RECORD, this host's, then call SETTLE.
ARGS hold :agent, :model, (PROVIDER . ID), and :variant.  Explicit
choices are validated against the owner's offerings.  SETTLE takes t,
or nil and a line telling why a choice could not apply,
once its backend applied them; nothing shows a window nor asks."
  (let* ((engine (drover-session-engine record))
         (asked (plist-get args :model))
         (model (and asked engine (drover-remote--told-model engine (car asked) (cdr asked))))
         (runs (or (null model) (drover-model-id-for model engine)))
         (settled nil))
    (cl-flet ((settle (ok &optional message)
                (unless settled
                  (setq settled t)
                  (funcall settle ok message))))
      (if (not (drover-supports-p record :apply-options))
          (settle nil "its options are set on its host")
        (condition-case err
            (save-window-excursion
              (drover-quietly
                (require 'drover-prompt)
                (drover-backend-call
                 record :apply-options
                 (drover-prompt-options-for record
                                            (list :agent (plist-get args :agent) :model model
                                                  :variant (plist-get args :variant)))
                 (lambda ()
                   (drover-with-reporting "remote options"
                     (let* ((now (and (drover-supports-p record :options)
                                      (drover-backend-call record :options)))
                            (current (plist-get now :model)))
                       (cond
                        ((and asked (not runs))
                         (settle nil (format "%s cannot run %s" engine (cdr asked))))
                        ((and model (drover-model-p current)
                              (not (equal (drover-model-key current) (drover-model-key model))))
                         (settle nil (format "applied, but it runs %s" (drover-model-label current))))
                        (t (settle t))))))
                 (lambda (reason) (settle nil reason)))))
          (error (settle nil (drover-error-message err))))))))

(defun drover-remote--on-opts (peer key reply)
  "Tell PEER, through REPLY, what session KEY runs and what it may run instead.
Its agent, model and variant now, which of them it may change, and its
engine's agents, models and defaults in its directory, as `catalogs'
tells them."
  (let ((record (drover-remote--owner-record key "options")))
    (cond
     ((not (drover-remote--allows-p (drover-remote--access peer) "options"))
      (funcall reply nil (drover-remote--refuse-action peer "read options"
                                                       (format "%s has %s access" (drover-remote--label peer)
                                                               (drover-remote--access peer)))))
     ((not record) (funcall reply nil "no such session here"))
     ((not (drover-session-engine record)) (funcall reply nil "its engine is not known"))
     (t
      (let* ((engine (drover-session-engine record))
             (directory (or (drover-session-directory record) (expand-file-name "~/")))
             (now (and (drover-supports-p record :options)
                       (drover-with-reporting "remote options" (drover-backend-call record :options))))
             (settable (drover-remote--settable record))
             (model (plist-get now :model))
             (variant (plist-get now :variant)))
        (drover-remote--engine-defaults
         (list engine) directory
         (lambda (answers)
           (funcall reply
                    (list :e (symbol-name engine)
                          :d directory
                          :cur (list :a (or (drover-remote--plain (drover-session-agent record)) :false)
                                     :m (if (drover-model-p model) (drover-remote--tell-model engine model) :false)
                                     :v (if (and variant (symbolp variant)) (symbol-name variant) :false))
                          :set (vconcat (delq nil (mapcar (lambda (option)
                                                            (and (memq option '(:agent :model :variant))
                                                                 (substring (symbol-name option) 1)))
                                                          settable)))
                          :cat (drover-with-reporting "remote options"
                                 (drover-remote--engine-catalog engine directory
                                                                (cdr (assq engine answers)))))))))))))

(defvar drover-remote-conversation-functions nil
  "Functions called with a peer and a `cvd' or `cve' message it sent.
A view of a conversation of the peer it follows (request `cvs') takes
its entries there, as they are appended, and its end.")

(defconst drover-remote-cv-streams 4
  "Conversations at most one connection follows: a fifth stops the oldest.")

(defconst drover-remote-cv-streams-total 16
  "Conversations at most this host follows for its peers.")

(defconst drover-remote-cv-piece (* 256 1024)
  "Bytes at most of the entries of one message of a conversation followed.")

(defvar drover-remote--cv-streams (make-hash-table :test #'eq :weakness 'key)
  "Conversations followed for peers, by connection.
Each value is a list of (ID . STOP), newest first.")

(defun drover-remote--on-cv (key message reply)
  "Answer through REPLY a page of what was said in session KEY, as MESSAGE asks.
MESSAGE's :before, the start of the first line of a page told before,
and :n, how many entries.  A session whose engine records nothing
drover reads answers an error beginning with \"none:\"."
  (require 'drover-conversation)
  (let ((record (and key (drover-remote--owner-record key "cv")))
        (before (plist-get message :before))
        (count (plist-get message :n)))
    (cond
     ((not record) (funcall reply nil "no such session here"))
     ((not (drover-conversation-readable-p record))
      (funcall reply nil "none: its engine records nothing drover reads"))
     (t
      (drover-conversation-page
       record (and (natnump before) before) (and (natnump count) count)
       (lambda (page)
         (if-let* ((failure (plist-get page :failure)))
             (funcall reply nil (concat "none: " (drover-bound-line failure)))
           (funcall reply (append page (list :eng (symbol-name (drover-session-engine record))))))))))))

(defun drover-remote--cv-send (connection id entries to)
  "Send CONNECTION's peer ENTRIES of the conversation it follows as ID, up to TO.
In pieces of `drover-remote-cv-piece' bytes at most, the last telling
TO, where the next line begins."
  (let ((piece nil) (bytes 0))
    (cl-flet ((flush (last)
                (when (or piece last)
                  (drover-remote--send connection
                                       (list :t "cvd" :id id :e (vconcat (nreverse piece))
                                             :to (if last to :false)))
                  (setq piece nil bytes 0))))
      (dolist (entry entries)
        (let ((size (length (json-serialize entry :null-object nil :false-object :false))))
          (when (and piece (> (+ bytes size) drover-remote-cv-piece))
            (flush nil))
          (push entry piece)
          (setq bytes (+ bytes size))))
      (flush t))))

(defun drover-remote--on-cvs (peer connection key message reply)
  "Follow session KEY's conversation for PEER on CONNECTION, as MESSAGE asks.
MESSAGE's :id names the stream, :from is where the lines to send begin,
:g the generation of the file a page told.  REPLY answers at once; the
entries appended come in `cvd' messages, and the stream's end in `cve',
as when the file is replaced."
  (require 'drover-conversation)
  (let* ((id (plist-get message :id))
         (from (plist-get message :from))
         (generation (plist-get message :g))
         (record (and key (drover-remote--owner-record key "cv")))
         (streams (gethash connection drover-remote--cv-streams))
         (total (apply #'+ (mapcar #'length (hash-table-values drover-remote--cv-streams)))))
    (cond
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((not (natnump from)) (funcall reply nil "no offset"))
     ((not record) (funcall reply nil "no such session here"))
     ((not (drover-conversation-readable-p record))
      (funcall reply nil "none: its engine records nothing drover reads"))
     ((and (>= total drover-remote-cv-streams-total) (null streams))
      (funcall reply nil (drover-remote--refuse-action peer "follow a conversation" "this host follows as many as it may")))
     (t
      (when (>= (length streams) drover-remote-cv-streams)
        (drover-remote--cv-stop connection (car (car (last streams))) "a newer one is followed"))
      (let* ((link (drover-link connection))
             (stop (drover-conversation-follow
                    record from (and (stringp generation) generation)
                    (lambda (event)
                      (drover-with-reporting "remote conversation"
                        (let ((connection (drover-linked link)))
                          (when (and connection (spyglass-connection-live-p connection))
                            (if-let* ((why (plist-get event :reset)))
                                (progn
                                  (drover-remote--cv-forget connection id)
                                  (drover-remote--send connection (list :t "cve" :id id :why why :reset t)))
                              (drover-remote--cv-send connection id (plist-get event :entries)
                                                      (plist-get event :to))))))))))
        (puthash connection (cons (cons id stop) (gethash connection drover-remote--cv-streams))
                 drover-remote--cv-streams)
        (funcall reply (list :ok t :sz (or (file-attribute-size
                                            (file-attributes (or (drover-conversation-file record) "")))
                                           0))))))))

(defun drover-remote--cv-forget (connection id)
  "Forget the stream ID of CONNECTION, without stopping it."
  (puthash connection (assoc-delete-all id (gethash connection drover-remote--cv-streams))
           drover-remote--cv-streams))

(defun drover-remote--cv-stop (connection id why)
  "Stop following the conversation ID for CONNECTION's peer; tell it WHY, if any."
  (when-let* ((entry (assoc id (gethash connection drover-remote--cv-streams))))
    (drover-remote--cv-forget connection id)
    (funcall (cdr entry))
    (when (and why (spyglass-connection-live-p connection))
      (drover-remote--send connection (list :t "cve" :id id :why why :reset :false)))))

(defun drover-remote--cv-drop (connection)
  "Stop following the conversations of CONNECTION, whose stream ended."
  (dolist (entry (gethash connection drover-remote--cv-streams))
    (funcall (cdr entry)))
  (remhash connection drover-remote--cv-streams))

(defvar drover-remote-jobs-functions nil
  "Functions called with a peer and a `jobs' message it sent.
A view of the peer's queue of scheduled prompts it follows (request
`jobs' with :follow) takes them there as they change.")

(defconst drover-remote-jobs-limit 200
  "Scheduled prompts at most a host tells its peers, the newest.")

(defun drover-remote--wire-job (job)
  "Return the scheduled prompt JOB as it travels to peers."
  (let* ((conversation (ignore-errors (drover-schedule-conversation job)))
         (record (and conversation
                      (seq-find (lambda (session)
                                  (and (eq (drover-session-backend session) (plist-get job :backend))
                                       (equal (drover-session-engine-key session)
                                              (cons (plist-get conversation :engine) (plist-get conversation :id)))))
                                (drover-index-sessions))))
         (options (plist-get job :options))
         (model (plist-get options :model))
         (prompt (or (plist-get job :prompt) "")))
    (list :id (plist-get job :id)
          :k (if-let* ((record) (id (drover-remote--wire-id (drover-session-id record))))
                 (vector (symbol-name (drover-session-backend record)) id)
               :false)
          :ln (or (drover-remote--plain (plist-get job :link)) :false)
          :ti (or (drover-bound-line (plist-get job :title)) "")
          :p (if (> (length prompt) 2000) (substring prompt 0 2000) prompt)
          :pn (length prompt)
          :o (list :a (or (plist-get options :agent) :false)
                   :m (if (drover-model-p model)
                          (vector (or (drover-model-provider-id model) :false) (or (drover-model-id model) ""))
                        :false)
                   :v (if (and (plist-get options :variant) (symbolp (plist-get options :variant)))
                          (symbol-name (plist-get options :variant))
                        :false))
          :w (cond ((plist-get job :due) (list :at (plist-get job :due)))
                   ((plist-get job :turn) (list :turn t))
                   (t (list :after (drover-schedule--trigger-label (plist-get job :trigger))
                            :tr (if (symbolp (plist-get job :trigger)) (symbol-name (plist-get job :trigger)) ""))))
          :st (symbol-name (plist-get job :status))
          :er (or (drover-remote--plain (plist-get job :error)) :false)
          :c (or (drover-remote--number (plist-get job :created)) :false)
          :sa (or (drover-remote--number (plist-get job :sent-at)) :false)
          :by (or (plist-get job :by) :false))))

(defun drover-remote--wire-jobs ()
  "Return this host's scheduled prompts as they travel, the newest first."
  (vconcat (mapcar #'drover-remote--wire-job
                   (seq-take (sort (copy-sequence (drover-schedule--jobs))
                                   (lambda (a b) (> (or (plist-get a :created) 0) (or (plist-get b :created) 0))))
                             drover-remote-jobs-limit))))

(defun drover-remote--on-jobs (connection message reply)
  "Tell CONNECTION's peer, through REPLY, this host's scheduled prompts.
With MESSAGE's :follow, the peer is told them again as they change, in
`jobs' messages."
  (require 'drover-schedule)
  (condition-case err
      (progn
        (drover-schedule-load)
        (when (eq (plist-get message :follow) t)
          (puthash connection t drover-remote--jobs-followers)
          (add-hook 'drover-schedule-changed-hook #'drover-remote--jobs-changed))
        (funcall reply (list :jobs (drover-remote--wire-jobs))))
    (error (funcall reply nil (drover-error-message err)))))

(defun drover-remote--jobs-changed ()
  "Tell the peers following the queue it changed, half a second later.
For `drover-schedule-changed-hook'."
  (drover-with-reporting "remote jobs"
    (unless drover-remote--jobs-timer
      (setq drover-remote--jobs-timer
            (run-at-time 0.5 nil
                         (lambda ()
                           (drover-with-reporting "remote jobs"
                             (setq drover-remote--jobs-timer nil)
                             (when (> (hash-table-count drover-remote--jobs-followers) 0)
                               (let ((jobs (drover-remote--wire-jobs)))
                                 (dolist (connection (hash-table-keys drover-remote--jobs-followers))
                                   (if (spyglass-connection-live-p connection)
                                       (drover-remote--send connection (list :t "jobs" :jobs jobs))
                                     (remhash connection drover-remote--jobs-followers))))))))))))

(defun drover-remote--on-triggers (reply)
  "Tell a peer, through REPLY, the events a prompt may wait for here."
  (require 'drover-schedule)
  (funcall reply (list :tr (vconcat (mapcar (lambda (trigger)
                                              (list :n (symbol-name (car trigger))
                                                    :l (or (drover-bound-line (format "%s" (cdr trigger)) 80) "")))
                                            (seq-take (drover-schedule-triggers) 32))))))

(defun drover-remote--on-schedule (state peer key message reply)
  "Schedule for PEER the prompt MESSAGE describes for session KEY, in STATE.
MESSAGE's :text, then :at, an absolute time of this host's clock in
the future, :turn, the end of the session's turn, or :trigger, an
event's name, and :o, the options to apply first.  Its :id has it
scheduled once, asked again after a stream ended.  It is delivered by
this host's queue as its own prompts are, out of every window; REPLY
gets the prompt scheduled, or why it was not."
  (require 'drover-schedule)
  (require 'drover-links)
  (let* ((id (plist-get message :id))
         (known (and (stringp id) (gethash id (drover-remote--state-outcomes state)))))
    (cond
     ((not (drover-remote--allows-p (drover-remote--access peer) "send"))
      (funcall reply nil (drover-remote--refuse-action peer "schedule a prompt"
                                                       (format "%s has %s access" (drover-remote--label peer)
                                                               (drover-remote--access peer)))))
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((and known (car known)) (drover-remote--new-reply reply (cdr known)))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (let* ((record (car (and key (drover-remote--served key))))
             (text (plist-get message :text))
             (at (drover-remote--number (plist-get message :at)))
             (turn (eq (plist-get message :turn) t))
             (trigger (and (stringp (plist-get message :trigger))
                           (drover-remote--symbol (plist-get message :trigger))))
             (options (let ((o (plist-get message :o)))
                        (if (and (consp o) (keywordp (car o)))
                            (drover-remote--check-options (list :agent (plist-get o :a) :model (plist-get o :m)
                                                                :variant (plist-get o :v))
                                                          nil)
                          (list :agent nil :model nil :variant nil))))
             (conversation (and record (drover-links-conversation record)))
             (why (cond
                   ((not record) "no such session here")
                   ((not (drover-supports-p record :submit)) "it takes no prompt from another host")
                   ((not (and (stringp text) (not (string-blank-p text))
                              (<= (length text) drover-remote-prompt-limit)))
                    "no prompt, or one too long")
                   ((stringp options) options)
                   ((not conversation) "its conversation is not known yet")
                   ((not (or at turn trigger)) "no time, turn nor event")
                   ((and at (<= at (float-time))) "a time already past")
                   ((and trigger (not (assq trigger (drover-schedule-triggers))))
                    (format "no event %s here" trigger)))))
        (if why
            (funcall reply nil (drover-remote--refuse-action peer "schedule a prompt" why))
          (drover-remote--begin-outcome state id reply)
          (let* ((target (drover-remote--schedule-target record conversation options peer))
                 (instruction (format "scheduled from %s" (drover-remote--label peer)))
                 (answer (condition-case err
                             (let* ((job (drover-quietly
                                          (cond (at (drover-schedule-add target (seconds-to-time at) text instruction))
                                                (turn (drover-schedule-add-after-turn target text instruction))
                                                (t (drover-schedule-add-after-trigger target trigger text instruction))))))
                               (drover-remote--event peer 'schedule "scheduled a prompt for %s" (plist-get job :title))
                               (list :job (drover-remote--wire-job job)))
                           (error (list :failure (or (drover-bound-line (drover-error-message err)) "failed"))))))
            (let ((entry (gethash id (drover-remote--state-outcomes state))))
              (puthash id (cons t answer) (drover-remote--state-outcomes state))
              (dolist (pending (cdr entry))
                (drover-with-reporting "remote schedule"
                  (drover-remote--new-reply pending answer)))))))))))

(defun drover-remote--schedule-target (record conversation options peer)
  "Return where a prompt PEER schedules for RECORD goes, for `drover-schedule'.
CONVERSATION is RECORD's (see `drover-links-conversation'); OPTIONS hold
:agent, :model, (PROVIDER . ID), and :variant, as a peer tells them.
The prompt resumes the session in no window when it has none."
  (let* ((engine (drover-session-engine record))
         (asked (plist-get options :model))
         (model (and asked engine (drover-remote--told-model engine (car asked) (cdr asked)))))
    (list :link (drover-links-link conversation)
          :backend (drover-session-backend record)
          :directory (or (drover-session-directory record) (expand-file-name "~/"))
          :title (or (drover-session-title record) (format "%s" (cdr (drover-session-id record))))
          :options (and (or (plist-get options :agent) model (plist-get options :variant))
                        (list :agent (plist-get options :agent) :model model
                              :variant (and model (plist-get options :variant))))
          :by (drover-remote--peer-name peer)
          :hidden t)))

(defun drover-remote--peer-job (peer message reply action)
  "Return the scheduled prompt MESSAGE's :job names, for PEER's ACTION, or nil.
Nil when PEER may not, or there is no such prompt, REPLY told why."
  (require 'drover-schedule)
  (let ((id (plist-get message :job)))
    (cond
     ((not (drover-remote--allows-p (drover-remote--access peer) "send"))
      (funcall reply nil (drover-remote--refuse-action peer action
                                                       (format "%s has %s access" (drover-remote--label peer)
                                                               (drover-remote--access peer))))
      nil)
     ((not (and (stringp id) (< (length id) 100)))
      (funcall reply nil "no scheduled prompt named")
      nil)
     ((progn (ignore-errors (drover-schedule-load)) (drover-schedule--job id)))
     (t (funcall reply nil "no such scheduled prompt here") nil))))

(defun drover-remote--on-unschedule (peer message reply)
  "Cancel for PEER the scheduled prompt MESSAGE names, answering through REPLY.
One cancelled already is answered done."
  (when-let* ((job (drover-remote--peer-job peer message reply "cancel a scheduled prompt")))
    (condition-case err
        (progn
          (unless (eq (plist-get job :status) 'cancelled)
            (drover-quietly
              (drover-schedule-cancel job)))
          (funcall reply (list :ok t)))
      (error (funcall reply nil (drover-error-message err))))))

(defun drover-remote--on-retry (peer message reply)
  "Retry for PEER the scheduled prompt MESSAGE names, answering through REPLY.
With MESSAGE's :now, one waiting for an event or a turn is sent now."
  (when-let* ((job (drover-remote--peer-job peer message reply "retry a scheduled prompt")))
    (condition-case err
        (progn
          (drover-quietly
            (drover-schedule-retry-job job (eq (plist-get message :now) t)))
          (funcall reply (list :ok t)))
      (error (funcall reply nil (drover-error-message err))))))

(defconst drover-remote-dirs-limit 300
  "Directories at most a host tells a peer its sessions ran in.")

(defun drover-remote--on-dirs (peer message reply)
  "Tell PEER, through REPLY, the directories this host's sessions ran in.
Newest first, MESSAGE's :n at most, with their projects, when a session
ran there last and how many: the directories of its sessions listed,
of the older and archived ones fetched, and the projects project.el
knows.  No file names, nothing of what they hold."
  (if-let* ((why (drover-remote--new-allowed peer "list directories")))
      (funcall reply nil why)
    (let ((limit (min drover-remote-dirs-limit (or (drover-remote--count (plist-get message :n))
                                                   drover-remote-dirs-limit)))
          (table (make-hash-table :test #'equal)))
      (dolist (record (append (drover-index-sessions)
                              (drover--fetched-sessions 'history) (drover--fetched-sessions 'archived)))
        (when-let* (((drover-remote--local-p record))
                    (directory (drover-session-directory record))
                    ((stringp directory))
                    ((file-name-absolute-p directory)))
          (let* ((key (file-name-as-directory directory))
                 (entry (gethash key table))
                 (time (or (drover-session-updated record) (drover-session-created record) 0)))
            (puthash key (list (or (car entry) (drover-session-project record))
                               (max (or (cadr entry) 0) time)
                               (1+ (or (caddr entry) 0)))
                     table))))
      (when (and (featurep 'project) (fboundp 'project-known-project-roots))
        (dolist (root (seq-take (ignore-errors (project-known-project-roots)) 1000))
          (when (and (stringp root) (file-name-absolute-p (expand-file-name root)))
            (let ((key (file-name-as-directory (expand-file-name root))))
              (unless (gethash key table)
                (puthash key (list nil 0 0) table))))))
      (funcall reply
               (list :dirs
                     (vconcat
                      (mapcar (pcase-lambda (`(,directory ,project ,time ,count))
                                (list :d (drover-remote--plain directory)
                                      :n (or (drover-remote--plain (and (stringp project) project))
                                             (file-name-nondirectory (directory-file-name directory)))
                                      :t (or (drover-remote--number time) 0)
                                      :c count))
                              (seq-take (sort (mapcar (lambda (key) (cons key (gethash key table)))
                                                      (hash-table-keys table))
                                              (lambda (a b) (> (nth 2 a) (nth 2 b))))
                                        limit))))))))

(defun drover-remote--finish-task (state task)
  "Answer TASK, done, to whoever asked it, and keep its outcome in STATE.
The answer is (:out OUTCOMES), and :queued, a line or false by session,
when a prompt sent waits in this host's queue rather than went in."
  (setf (drover-remote--state-tasks state) (delq task (drover-remote--state-tasks state)))
  (let* ((out (vconcat (mapcar (lambda (outcome) (or outcome "failed")) (plist-get task :out))))
         (queued (plist-get task :queued))
         (answer (append (list :out out)
                         (and (vectorp queued) (seq-some #'identity queued)
                              (list :queued (vconcat (mapcar (lambda (why) (or why :false)) queued))))))
         (entry (gethash (plist-get task :id) (drover-remote--state-outcomes state)))
         (replies (cdr entry)))
    (puthash (plist-get task :id) (cons t answer) (drover-remote--state-outcomes state))
    (dolist (reply replies)
      (drover-with-reporting "remote actions"
        (funcall reply answer)))))

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

;;;; Sharing terminals with peers

(defun drover-remote--mirrors-p ()
  "Return non-nil when spyglass shares terminals with some peers only.
Its shares for some peers and delays of a connection's own came
together: drover mirrors terminals only with them."
  (and (fboundp 'spyglass-connection-next-attempt) (fboundp 'spyglass-share)))

(defun drover-remote--terminal (record)
  "Return the terminal or shell of RECORD's conversation, as (BUFFER . KIND).
KIND is \"shell\" for a comint buffer, as agent-shell's, \"terminal\"
otherwise.  The buffer is RECORD's, or that of another record of its
conversation, as the terminal of a front end resuming a conversation an
engine records; only of a backend whose buffers run their agents
\(:runs-in-buffer): an OpenCode session's buffer shows a service's
session, which a mirror would not serve.  Nil when none."
  (let* ((engine-key (drover-session-engine-key record))
         (others (and engine-key
                      (delq nil (mapcar #'drover-index-session
                                        (gethash engine-key (drover--index-engines (drover-index))))))))
    (seq-some (lambda (candidate)
                (when-let* (((drover-backend-property candidate :runs-in-buffer))
                            (buffer (drover-session-buffer candidate)))
                  (cons buffer (if (with-current-buffer buffer (derived-mode-p 'comint-mode))
                                   "shell" "terminal"))))
              (cons record others))))

(defun drover-remote--resumable-p (record)
  "Return non-nil when RECORD's conversation may be resumed here in a front end.
A conversation an engine records, Claude Code's or Codex's, which a
front end resumes as opening it here does; not one archived."
  (when-let* ((engine (drover-session-engine record)))
    (and (drover-session-engine-id record)
         (not (drover-session-archived-p record))
         (drover-supports-p record :open)
         (or (drover-resumer-front-ends engine)
             (seq-some (lambda (entry) (memq engine (cddr entry))) drover--front-end-backends))
         t)))

(defun drover-remote--share-id (key)
  "Return the identifier this host shares the terminal of session KEY under."
  (format "%s%s/%s" drover-remote--share-prefix (car key) (cdr key)))

(defun drover-remote--share-viewer (id connection)
  "Return the name of CONNECTION's peer, when it asked to mirror the share ID.
The peer is known by the connection it asked on, which keeps its
streams, or by its greeting there: spyglass's mirrors, resuming as a
stream comes back, ask before the peers greet each other again."
  (when-let* ((state (drover-remote--current))
              (entry (gethash id (drover-remote--state-shares state))))
    (or (cdr (seq-find (lambda (viewer) (eq (drover-linked (car viewer)) connection))
                       (plist-get entry :viewers)))
        (when-let* ((peer (drover-remote--peer-of state connection))
                    ((rassoc (drover-remote--peer-name peer) (plist-get entry :viewers))))
          (drover-remote--peer-name peer)))))

(defun drover-remote--share-visible-p (id connection)
  "Return non-nil when CONNECTION's peer may see and open the share ID.
A peer that asked for it, by its connection or its greeting there (see
`drover-remote--share-viewer'), or, for a share a remote layer before
this one made, the Emacs instances that mirrored it then, which open it
again as their streams come back, before greeting this layer."
  (or (drover-remote--share-viewer id connection)
      (when-let* ((state (drover-remote--current))
                  (entry (gethash id (drover-remote--state-shares state)))
                  (instance (ignore-errors (spyglass-connection-peer-id connection))))
        (member instance (plist-get entry :instances)))))

(defun drover-remote--share-instances (share)
  "Return the instances of the Emacs subscribed to SHARE, connected or not.
From the keys of its subscriptions, INSTANCE/ID."
  (delete-dups (mapcar (lambda (key) (if (string-match "/[^/]*\\'" key) (substring key 0 (match-beginning 0)) key))
                       (spyglass-share-viewers share))))

(defun drover-remote--adopt-shares (state)
  "Have STATE, a remote layer made anew, keep the terminals shared before it.
Their buffers live and their viewers subscribed, as a remote layer
restarting or reloading leaves them: each is shared on as this layer
shares, visible to the instances mirroring it, whose access applies
once they greet it, and unshared as this layer unshares; a share of a
buffer gone is dropped."
  (when (fboundp 'spyglass-shares)
    (dolist (share (spyglass-shares))
      (let ((id (spyglass-share-id share)))
        (when (string-prefix-p drover-remote--share-prefix id)
          (let* ((rest (substring id (length drover-remote--share-prefix)))
                 (slash (string-search "/" rest))
                 (backend (and slash (drover-remote--symbol (substring rest 0 slash))))
                 (key (and backend (cons backend (substring rest (1+ slash)))))
                 (buffer (spyglass-share-buffer share)))
            (if (not (and key (buffer-live-p buffer)))
                (spyglass-unshare share)
              (puthash id (list :buffer buffer :key key
                                :title (when-let* ((record (drover-index-session key)))
                                         (drover-remote--plain (drover-session-display-title record)))
                                :viewers nil
                                :instances (drover-remote--share-instances share))
                       (drover-remote--state-shares state))
              (drover-remote--reshare buffer id)
              (unless (spyglass-share-viewers share)
                (drover-remote--unshare-later state id)))))))))

(defun drover-remote--reshare (buffer id)
  "Share BUFFER under ID as this remote layer shares terminals with peers."
  (spyglass-share buffer :id id
                  :peers (lambda (connection) (and (drover-remote--share-visible-p id connection) t))
                  :read-only (lambda (connection) (drover-remote--share-refuses-p id connection))
                  :input #'drover-remote--share-input
                  :size-policy 'owner)
  (when (with-current-buffer buffer (derived-mode-p 'comint-mode))
    (drover-remote--shell-keys buffer)))

(defun drover-remote--share-refuses-p (id connection)
  "Return non-nil when the share ID takes no input from CONNECTION's peer.
A peer of `read' access watches; `act' and `full' type, as configured
now.  A peer an earlier remote layer shared it with is known once it
greeted this one."
  (let* ((name (or (drover-remote--share-viewer id connection)
                   (when-let* (((drover-remote--share-visible-p id connection))
                               (state (drover-remote--current))
                               (peer (drover-remote--peer-of state connection)))
                     (drover-remote--peer-name peer))))
         (peer (and name (drover-remote--peer name))))
    (not (and peer (drover-remote--allows-p (drover-remote--access peer) "type")))))

(defun drover-remote--share (state peer connection key buffer)
  "Share BUFFER, the terminal of this host's session KEY, with PEER on CONNECTION.
STATE is the remote layer's.  Return the share's identifier.  The share
is visible to the peers that asked alone, takes their input as their
access allows, and is sized by the viewer's window while no window of
this host shows it, else by this host's (spyglass's `owner' policy).
A buffer someone else shared is left as it is."
  (let* ((existing (spyglass-share-of buffer))
         (id (if existing (spyglass-share-id existing) (drover-remote--share-id key)))
         (shares (drover-remote--state-shares state))
         (entry (gethash id shares))
         (name (drover-remote--peer-name peer)))
    (if (and existing (not entry))
        id
      ;; A share of that name left on another buffer is a stale one of drover's.
      (when-let* ((other (seq-find (lambda (share) (equal (spyglass-share-id share) id)) (spyglass-shares)))
                  ((not (eq other existing))))
        (spyglass-unshare other))
      (let ((viewers (seq-remove (lambda (viewer)
                                   (let ((other (drover-linked (car viewer))))
                                     (or (eq other connection)
                                         (eq (spyglass-connection-state other) 'closed))))
                                 (plist-get entry :viewers))))
        (puthash id (list :buffer buffer :key key
                          :title (or (when-let* ((record (drover-index-session key)))
                                       (drover-remote--plain (drover-session-display-title record)))
                                     (plist-get entry :title))
                          :viewers (cons (cons (drover-link connection) name) viewers)
                          :instances (plist-get entry :instances)
                          :timer (plist-get entry :timer))
                 shares)
        (drover-remote--reshare buffer id)
        (unless (spyglass-share-viewers (spyglass-share-of buffer))
          (drover-remote--unshare-later state id))
        id))))

(defconst drover-remote--shell-keys
  '(("RET" . "RET") ("<return>" . "RET") ("M-RET" . "M-RET") ("M-<return>" . "M-RET"))
  "Keys a peer's mirror of a shell sends, and the shell's keys they stand for.
The shell's own command for each runs, as its owner's key would: RET
submits through a front end's own command, such as agent-shell's
`agent-shell-submit', which `comint-send-input' alone does not.  The
arrows up and down recall the input history, as spyglass's comint
input does: the owner's own history keys may be bound to anything else,
as a configuration moving between links binds them.")

(defun drover-remote--shell-keys (buffer)
  "Let peers' mirrors of BUFFER, a shell, run its own commands for its keys.
Its bindings of `drover-remote--shell-keys', as they are now, and the
interrupt a mirror sends, interrupting its session as drover's interrupt
does, without asking (`drover-remote--shell-interrupt'); see
`spyglass-key-commands'."
  (with-current-buffer buffer
    (setq-local spyglass-key-commands
                (cons (cons "C-c" #'drover-remote--shell-interrupt)
                      (delq nil (mapcar (lambda (entry)
                                          (let ((command (save-excursion
                                                           (goto-char (point-max))
                                                           (key-binding (kbd (cdr entry))))))
                                            (and (commandp command) (symbolp command)
                                                 (cons (car entry) command))))
                                        drover-remote--shell-keys))))))

(defun drover-remote--shell-interrupt ()
  "Interrupt the session of the current buffer, a shell, for a peer's mirror.
Through its backend, as drover's interrupt does, without asking; else
through the shell's own command interrupting, comint's by default."
  (let ((session (drover-buffer-session (current-buffer))))
    (if (and session (drover-supports-p session :interrupt))
        (drover--call-confirming session :interrupt #'ignore
                                 (lambda (message)
                                   (drover-message "Could not interrupt %s: %s"
                                                   (drover-session-display-title session) message)))
      (let ((command (key-binding (kbd "C-c C-c"))))
        (if (commandp command)
            (call-interactively command)
          (comint-interrupt-subjob))))))

(defun drover-remote--share-input (kind value)
  "Apply a peer's input of KIND and VALUE in the current buffer, shared with it.
As the buffer's input function, or spyglass's default, applies it, no
window of this host's taken meanwhile."
  (drover-quietly
    (funcall (or spyglass-input-function #'spyglass-default-input) kind value)))

(defun drover-remote--unsent (buffer text)
  "Tell the peers mirroring BUFFER the TEXT it held unsent as it closes.
For `drover-unsent-functions': a user typing in a mirror typed there,
and finds it in the kill ring there, as archiving here puts it in this
host's.  Only peers telling \"unsent\" hear it."
  (drover-with-reporting "remote unsent"
    (when-let* ((state (drover-remote--current)))
      (maphash (lambda (_id entry)
                 (when (eq (plist-get entry :buffer) buffer)
                   (let ((key (plist-get entry :key))
                         (sent nil))
                     (pcase-dolist (`(,link . ,name) (plist-get entry :viewers))
                       (let ((connection (drover-linked link))
                             (peer (drover-remote--peer name)))
                         (when (and peer connection (not (member name sent))
                                    (spyglass-connection-live-p connection)
                                    (drover-remote--capable-p peer "unsent"))
                           (push name sent)
                           (drover-remote--send
                            connection
                            (list :t "unsent" :key (vector (symbol-name (car key)) (cdr key))
                                  :title (or (plist-get entry :title) "")
                                  :text (substring-no-properties
                                         text 0 (min (length text) drover-remote-prompt-limit))))))))))
               (drover-remote--state-shares state)))))

(defun drover-remote--on-unsent (peer message)
  "Keep in the kill ring what a buffer of PEER's mirrored here held unsent.
MESSAGE tells it, as that buffer closed there, as archiving closes it."
  (let* ((text (plist-get message :text))
         (key (drover-remote--key (plist-get message :key)))
         (record (and key (drover-remote--peer-session peer key)))
         (title (or (and record (drover-bound-line (drover-session-display-title record) 80))
                    (let ((title (drover-remote--line (plist-get message :title) 80)))
                      (and title (not (string-empty-p title)) title))
                    "its session")))
    (when (and (stringp text) (not (string-blank-p text)))
      (drover-keep-unsent (list (substring text 0 (min (length text) drover-remote-prompt-limit))))
      (drover-remote--event peer 'mirror "kept in the kill ring the unsent input of %s" title)
      (drover-message "%s: the unsent input of %s is in the kill ring" (drover-remote--label peer) title))))

(defun drover-remote--unshare-later (state id)
  "Unshare ID, a terminal STATE shares, once no peer mirrored it for a while.
See `drover-remote-unshare-delay'."
  (when-let* ((entry (gethash id (drover-remote--state-shares state))))
    (when (plist-get entry :timer)
      (cancel-timer (plist-get entry :timer)))
    (puthash id (plist-put (copy-sequence entry) :timer
                           (run-at-time drover-remote-unshare-delay nil
                                        (lambda ()
                                          (drover-with-reporting "remote sharing"
                                            (drover-remote--unshare-idle id)))))
             (drover-remote--state-shares state))))

(defun drover-remote--unshare-idle (id)
  "Unshare ID, a terminal this host shares, unless a peer mirrors it."
  (when-let* ((state (drover-remote--current))
              (entry (gethash id (drover-remote--state-shares state))))
    (puthash id (plist-put (copy-sequence entry) :timer nil) (drover-remote--state-shares state))
    (let* ((buffer (plist-get entry :buffer))
           (share (and (buffer-live-p buffer) (spyglass-share-of buffer))))
      (cond ((not (and share (equal (spyglass-share-id share) id)))
             (remhash id (drover-remote--state-shares state)))
            ((not (spyglass-share-viewers share))
             (spyglass-unshare share))))))

(defun drover-remote--share-event (share event _data)
  "Follow EVENT of SHARE, if this host shares it with peers.
For `spyglass-share-state-functions': a terminal no peer mirrors any
more is unshared a while later, one a peer mirrors again stays."
  (drover-with-reporting "remote sharing"
    (when-let* ((state (drover-remote--current))
                (id (spyglass-share-id share))
                (entry (gethash id (drover-remote--state-shares state))))
      (pcase event
        ('subscribed
         (when (plist-get entry :timer)
           (cancel-timer (plist-get entry :timer))
           (puthash id (plist-put (copy-sequence entry) :timer nil) (drover-remote--state-shares state))))
        ('unsubscribed
         (unless (spyglass-share-viewers share)
           (drover-remote--unshare-later state id)))
        ('unshared
         (when (plist-get entry :timer)
           (cancel-timer (plist-get entry :timer)))
         (when (buffer-live-p (plist-get entry :buffer))
           (with-current-buffer (plist-get entry :buffer)
             (kill-local-variable 'spyglass-key-commands)))
         (remhash id (drover-remote--state-shares state)))))))

(defun drover-remote--unshare-all (&optional state)
  "Unshare the terminals this host shares with peers, STATE's and any left.
Those left are shares of a remote layer made anew without stopping, as
reloading a drover whose structures changed does."
  (when state
    (maphash (lambda (_id entry)
               (when (plist-get entry :timer) (cancel-timer (plist-get entry :timer))))
             (drover-remote--state-shares state)))
  (when (fboundp 'spyglass-shares)
    (dolist (share (spyglass-shares))
      (when (string-prefix-p drover-remote--share-prefix (spyglass-share-id share))
        (spyglass-unshare share)))))

(defun drover-remote--mirror-answer (state peer connection key terminal)
  "Share TERMINAL, (BUFFER . KIND) of session KEY, with PEER on CONNECTION.
STATE is the remote layer's.  Return the answer to the peer's request:
the share's identifier, its kind, and whether the peer may type."
  (let ((id (drover-remote--share state peer connection key (car terminal))))
    (drover-remote--event peer 'share "sharing the %s %s" (cdr terminal) (buffer-name (car terminal)))
    (list :share id :kind (cdr terminal)
          :input (if (drover-remote--allows-p (drover-remote--access peer) "type") t :false))))

(defun drover-remote--finish-outcome (state id answer)
  "Answer ANSWER to whoever asked the request ID, done, and keep it in STATE."
  (let ((entry (gethash id (drover-remote--state-outcomes state))))
    (unless (car entry)
      (puthash id (cons t answer) (drover-remote--state-outcomes state))
      (dolist (reply (cdr entry))
        (drover-with-reporting "remote mirror"
          (funcall reply answer))))))

(defun drover-remote--on-mirror (state peer connection message reply)
  "Answer PEER's request MESSAGE on CONNECTION to mirror a session's terminal.
STATE is the remote layer's; REPLY gets the answer: the share to open,
else why there is none (:none) and whether it may be resumed.  With
:resume, a conversation without a terminal is resumed in its front end
first, as opening it here would, without a window, for a peer that may
act on it; the request's identifier has it resumed once, asked again
after a stream ended."
  (let* ((key (drover-remote--key (plist-get message :key)))
         (id (plist-get message :id))
         (record (and key (car (drover-remote--served key))))
         (terminal (and record (drover-remote--terminal record)))
         (known (and (stringp id) (gethash id (drover-remote--state-outcomes state)))))
    (cond
     ((not record) (funcall reply nil "no such session here"))
     (terminal (funcall reply (drover-remote--mirror-answer state peer connection key terminal)))
     ((and known (car known)) (funcall reply (cdr known)))
     (known (setcdr known (cons reply (cdr known))))
     ((not (eq (plist-get message :resume) t))
      (funcall reply (list :none "it has no terminal here"
                           :resumable (if (drover-remote--resumable-p record) t :false))))
     ((not (drover-remote--allows-p (drover-remote--access peer) "resume"))
      (funcall reply nil (drover-remote--refuse-action
                          peer "resume" (format "%s has %s access" (drover-remote--label peer)
                                                (drover-remote--access peer)))))
     ((not (drover-remote--resumable-p record))
      (funcall reply (list :none "it has no terminal here, and no front end resumes it" :resumable :false)))
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     (t
      (drover-remote--begin-outcome state id reply)
      (drover-remote--resume-terminal state peer connection key record id)))))

(defun drover-remote--resume-terminal (state peer connection key record id)
  "Resume RECORD, session KEY, in its front end, then share its terminal.
For PEER on CONNECTION, which asked it under the identifier ID, STATE's
outcome answered once its terminal shows, or past
`drover-remote-resume-timeout' seconds.  It is opened as here, from a
timer rather than the stream's filter, without a window: a terminal
needing one for its peer goes to spyglass's parking frame."
  (drover-remote--event peer 'resume "resuming %s, to share its terminal"
                        (drover-session-display-title record))
  (let ((state-link (drover-link state))
        (peer-link (drover-link peer))
        (connection-link (drover-link connection))
        (deadline (+ (float-time) drover-remote-resume-timeout))
        (done nil))
    (cl-labels ((finish (answer)
                  (unless done
                    (setq done t)
                    (drover-remote--finish-outcome (drover-linked state-link) id answer)))
                (poll ()
                  (drover-with-reporting "remote resume"
                    (let ((state (drover-linked state-link)))
                      (cond
                       ((or done (not (eq state (drover-remote--current))))
                        (setq done t))
                       ((drover-remote--terminal (or (drover-index-session key) record))
                        (finish (drover-remote--mirror-answer
                                 state (drover-linked peer-link) (drover-linked connection-link) key
                                 (drover-remote--terminal (or (drover-index-session key) record)))))
                       ((> (float-time) deadline)
                        (finish (list :none "its front end showed no terminal in time" :resumable t)))
                       (t (run-at-time 0.2 nil #'poll)))))))
      (run-at-time 0 nil
                   (lambda ()
                     (drover-with-reporting "remote resume"
                       (condition-case err
                           (save-window-excursion
                             (save-current-buffer
                               (drover-quietly
                                 (drover-backend-call record :open nil))))
                         (error (finish (list :none (drover-error-message err) :resumable :false))))
                       (poll)))))))

;;;; Older and archived sessions

(defun drover-remote--serve-fetch (connection kind generation)
  "Send CONNECTION's peer the sessions of KIND, \"history\" or \"archived\".
Each backend of this host keeping them answers when it can, possibly
more than once, and each answer goes as it comes, in messages of
`drover-remote-batch' records, under GENERATION, the peer's; archived,
drover's own archive answers first, as source \"drover\"."
  (let ((operation (if (equal kind "history") :history :archived)))
    (cl-flet ((send (source sessions)
                (let* ((drover--archive-checking (or drover--archive-checking t))
                       (records (seq-filter #'drover-remote--local-p sessions))
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

;;;; Searching what was said

(defcustom drover-remote-search-timeout 60
  "Seconds a peer searches what was said in its sessions for this host, at most.
Past them it stops, its matches so far kept, and this host stops
waiting for it."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-search-keys 20000
  "Sessions of a peer at most a search here names there.
The search of what was said in a peer's sessions searches those the
search offers here, named by their keys: the first ones, in the list's
order, past this many.")

(defconst drover-remote-search-limit 200
  "Matches at most a peer sends for one search.")

(defconst drover-remote-search-runs 2
  "Searches at most a peer runs at once for this host.
One for the minibuffer's search, one for `drover-search-text': a third
stops the oldest.")

(defconst drover-remote-search-snippet 240
  "Characters at most of the snippet of a match a peer sends.")

(defvar drover-remote--searches (make-hash-table :test #'equal)
  "The searches of peers' sessions running here, by identifier.
Each a plist: :peer, its name, :connection, a link to the connection
asked, :callback, the backend's callback, and :timer, giving up.")

(defvar drover-remote--searching (make-hash-table :test #'eq :weakness 'key)
  "The searches this host runs for its peers, by connection, then identifier.
Each a plist: :sources, the backends searching and their cancelling
functions, an alist, :left, the matches still taken, :failed, the
backends that failed, as (LABEL . MESSAGE), :timer, ending it,
:start, when it began, and :peer, the peer's name.")

(defvar drover-remote--search-serial 0
  "The number of the last search of a peer's sessions asked here.")

;;;;; Here, searching a peer's sessions

(defun drover-remote--search (name query scope callback)
  "Search what was said in the sessions of the peer NAME for QUERY, for CALLBACK.
The proxy's :search: the peer runs its own backends' searches, scoped
to its sessions SCOPE's search offers here (see :sessions in
`drover-register-backend'), named by their keys, and to the older and
archived ones when they show, and sends its matches as they come.
Return the function telling it to stop; nil, searching nothing, when
the peer is not connected, runs a drover that does not search for
others, or the search offers none of its sessions."
  (when-let* ((peer (drover-remote--peer name))
              ((eq (drover-remote--peer-state peer) 'open))
              ((drover-remote--capable-p peer "search"))
              (connection (drover-remote--peer-connection peer))
              (offered (plist-get scope :sessions))
              ((functionp offered))
              (sessions (seq-take (funcall offered (drover-remote--peer-backend peer))
                                  drover-remote-search-keys)))
    (let* ((id (format "%s-%s-%d" drover-host
                       (substring (or (drover-remote--state-epoch (drover-remote--current)) "x") 0 8)
                       (cl-incf drover-remote--search-serial)))
           (engines (make-hash-table :test #'equal))
           (project (plist-get (plist-get scope :filter) :project)))
      (dolist (session sessions)
        (when-let* ((engine (drover-session-engine session))
                    (engine-id (drover-remote--wire-id (drover-session-engine-id session))))
          (puthash (vector (symbol-name engine) engine-id) t engines)))
      (puthash id (list :peer name :connection (drover-link connection) :callback callback
                        :timer (run-at-time (+ drover-remote-search-timeout 5) nil
                                            #'drover-remote--search-expired id))
               drover-remote--searches)
      (drover-remote--stat peer :searches)
      (drover-remote--request
       peer (list :op "search" :id id
                  :q (substring query 0 (min (length query) 256))
                  :keys (vconcat (delq nil (mapcar (lambda (session)
                                                     (let ((key (drover-session-id session)))
                                                       (when-let* ((wire (drover-remote--wire-id (cdr key))))
                                                         (vector (symbol-name (car key)) wire))))
                                                   sessions)))
                  :engines (vconcat (hash-table-keys engines))
                  :history (if (plist-get scope :history) t :false)
                  :archived (if (plist-get scope :archived) t :false)
                  :project (if (stringp project) project :false)
                  :limit (min drover-remote-search-limit
                              (or (plist-get scope :limit) drover-remote-search-limit)))
       (lambda (_reply error)
         (when error
           (drover-remote--search-end id (list :failure error))))
       nil t)
      (lambda () (drover-remote--search-cancel id)))))

(defun drover-remote--search-end (id result)
  "End the search ID of a peer's sessions here, giving its callback RESULT.
RESULT is `done' or (:failure MESSAGE); nothing when it ended already."
  (when-let* ((entry (gethash id drover-remote--searches)))
    (remhash id drover-remote--searches)
    (when (timerp (plist-get entry :timer))
      (cancel-timer (plist-get entry :timer)))
    (drover-with-reporting "remote search"
      (funcall (plist-get entry :callback) result))))

(defun drover-remote--search-expired (id)
  "Give up the search ID of a peer's sessions, whose peer did not finish it."
  (drover-with-reporting "remote search"
    (when-let* ((entry (gethash id drover-remote--searches)))
      (drover-remote--search-tell-stop id entry)
      (drover-remote--search-end id (list :failure "it did not finish in time")))))

(defun drover-remote--search-tell-stop (id entry)
  "Tell the peer of ENTRY, the search ID, to stop it, if still connected."
  (let ((connection (drover-linked (plist-get entry :connection))))
    (when (and connection (spyglass-connection-live-p connection))
      (drover-remote--send connection (list :t "src" :id id)))))

(defun drover-remote--search-cancel (id)
  "Stop the search ID of a peer's sessions: its callback is not called again."
  (drover-with-reporting "remote search"
    (when-let* ((entry (gethash id drover-remote--searches)))
      (remhash id drover-remote--searches)
      (when (timerp (plist-get entry :timer))
        (cancel-timer (plist-get entry :timer)))
      (drover-remote--search-tell-stop id entry))))

(defun drover-remote--decode-match (data name backend)
  "Return the match DATA of the peer NAME, received, for drover's search, or nil.
It names a session of BACKEND, the peer's proxy, by the owner's key, or
a conversation of the peer by its engine and identifier; its snippet
and position are bounded."
  (when (and (consp data) (keywordp (car data)))
    (let ((snippet (or (drover-remote--line (plist-get data :s) drover-remote-search-snippet) ""))
          (position (let ((position (plist-get data :p)))
                      (cond ((integerp position) position)
                            ((stringp position) (drover-remote--line position 128))))))
      (if-let* ((key (drover-remote--key (plist-get data :k))))
          (list :key (cons backend key) :snippet snippet :position position)
        (when-let* ((engine (drover-remote--symbol (plist-get data :e)))
                    (id (drover-remote--id (plist-get data :i))))
          (list :engine engine :id id :host name :snippet snippet :position position))))))

(defun drover-remote--on-search-matches (peer message)
  "Give the search MESSAGE names the matches PEER found."
  (when-let* ((id (plist-get message :id))
              (entry (and (stringp id) (gethash id drover-remote--searches)))
              ((equal (plist-get entry :peer) (drover-remote--peer-name peer))))
    (let* ((matches (plist-get message :m))
           (decoded (and (vectorp matches)
                         (delq nil (mapcar (lambda (data)
                                             (drover-remote--decode-match
                                              data (drover-remote--peer-name peer)
                                              (drover-remote--peer-backend peer)))
                                           (seq-take matches drover-remote-search-limit))))))
      (when decoded
        (drover-remote--stat peer :search-matches (length decoded))
        (drover-with-reporting "remote search"
          (funcall (plist-get entry :callback) decoded))))))

(defun drover-remote--on-search-end (peer message)
  "End the search MESSAGE names, as PEER finished it, or failed it."
  (when-let* ((id (plist-get message :id))
              (entry (and (stringp id) (gethash id drover-remote--searches)))
              ((equal (plist-get entry :peer) (drover-remote--peer-name peer))))
    ;; The search names the peer, its backend, before why.
    (let ((why (drover-remote--line (plist-get message :err))))
      (drover-remote--search-end id (if why (list :failure why) 'done)))))

(defun drover-remote--search-drop (connection)
  "Fail the searches of peers' sessions asked on CONNECTION, whose stream ended."
  (let ((ended nil))
    (maphash (lambda (id entry)
               (when (eq (drover-linked (plist-get entry :connection)) connection)
                 (push (cons id entry) ended)))
             drover-remote--searches)
    (pcase-dolist (`(,id . ,entry) ended)
      (drover-remote--search-end
       id (list :failure (format "the link to %s ended"
                                 (or (when-let* ((peer (drover-remote--peer (plist-get entry :peer))))
                                       (drover-remote--label peer))
                                     (plist-get entry :peer))))))))

;;;;; There, searching this host's sessions for a peer

(defun drover-remote--search-sources ()
  "Return the backends of this host's own searching what was said."
  (seq-filter (lambda (backend)
                (and (functionp (drover-backend-property backend :search))
                     (not (drover-backend-property backend :remote))))
              (drover-backends)))

(defun drover-remote--wire-match (match)
  "Return MATCH, of a backend of this host's, as it travels, or nil.
It names its session by its key, or by its engine and identifier."
  (when (listp match)
    (let* ((key (plist-get match :key))
           (wire-id (and (consp key) (symbolp (car key)) (drover-remote--wire-id (cdr key))))
           (engine (plist-get match :engine))
           (id (drover-remote--wire-id (plist-get match :id)))
           (snippet (drover-remote--plain (plist-get match :snippet)))
           (position (let ((position (plist-get match :position)))
                       (cond ((integerp position) (drover-remote--number position))
                             ((stringp position) (substring-no-properties (drover-bound-line position 128))))))
           (rest (append (list :s (or (and snippet (drover-bound-line snippet drover-remote-search-snippet)) ""))
                         (and position (list :p position)))))
      (cond (wire-id (append (list :k (vector (symbol-name (car key)) wire-id)) rest))
            ((and engine (symbolp engine) id) (append (list :e (symbol-name engine) :i id) rest))))))

(defun drover-remote--search-wanted (message)
  "Return the :wanted of a search of this host's sessions MESSAGE asks.
It wants the sessions the peer's search offers there, named by their
keys and their engines' identifiers, as MESSAGE lists them."
  (let ((keys (make-hash-table :test #'equal))
        (engines (make-hash-table :test #'equal)))
    (seq-doseq (wire (seq-take (or (plist-get message :keys) []) drover-remote-search-keys))
      (when-let* ((key (drover-remote--key wire)))
        (puthash key t keys)))
    (seq-doseq (wire (seq-take (or (plist-get message :engines) []) drover-remote-search-keys))
      (when-let* ((key (drover-remote--key wire)))
        (puthash key t engines)))
    (lambda (match)
      (let ((key (plist-get match :key))
            (engine (plist-get match :engine))
            (id (plist-get match :id)))
        (or (and (consp key) (gethash key keys) t)
            (and engine id (gethash (cons engine id) engines) t))))))

(defun drover-remote--on-search (peer connection message reply)
  "Search this host's sessions for PEER, on CONNECTION, as MESSAGE asks.
REPLY answers at once; the matches follow, in messages \"srm\", then
its end, \"sre\".  Each backend of this host's searching what was said
searches the sessions MESSAGE names, and the older and archived ones
when it says they show there, at most `drover-remote-search-timeout'
seconds, `drover-remote-search-limit' matches in all."
  (let ((id (plist-get message :id))
        (query (plist-get message :q)))
    (cond
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "malformed search"))
     ((not (and (stringp query) (not (string-blank-p query))))
      (funcall reply nil "nothing to search for"))
     (t
      (let* ((query (substring-no-properties query 0 (min (length query) 256)))
             (table (or (gethash connection drover-remote--searching)
                        (puthash connection (make-hash-table :test #'equal) drover-remote--searching)))
             (limit (let ((limit (plist-get message :limit)))
                      (if (natnump limit) (min limit drover-remote-search-limit) drover-remote-search-limit)))
             (project (drover-remote--line (plist-get message :project)))
             (scope (list :filter (and project (list :project project))
                          :history (eq (plist-get message :history) t)
                          :archived (eq (plist-get message :archived) t)
                          :limit limit
                          :wanted (drover-remote--search-wanted message)))
             (link (drover-link connection))
             (run (list :sources nil :left limit :failed nil :start (float-time)
                        :peer (drover-remote--peer-name peer))))
        ;; The oldest give way, told so.
        (let ((runs (sort (hash-table-keys table)
                          :key (lambda (id) (plist-get (gethash id table) :start)))))
          (while (>= (length runs) drover-remote-search-runs)
            (let ((old (pop runs)))
              (drover-remote--search-stop-run connection old "replaced")
              (drover-remote--send connection (list :t "sre" :id old :err "another search replaced it")))))
        (puthash id run table)
        (drover-remote--stat peer :searches-served)
        (funcall reply (list :ok t))
        (plist-put run :timer (run-at-time drover-remote-search-timeout nil
                                           (lambda ()
                                             (drover-with-reporting "remote search"
                                               (when-let* ((connection (drover-linked link)))
                                                 (drover-remote--search-finish connection id))))))
        (dolist (backend (drover-remote--search-sources))
          ;; Listed first, so that a backend answering at once, before it
          ;; returns, ends its part.
          (let ((source (list backend)))
            (plist-put run :sources (cons source (plist-get run :sources)))
            (let ((cancel (condition-case err
                              (funcall (drover-backend-property backend :search) query scope
                                       (drover-remote--search-receiver connection id backend))
                            (error (drover-remote--search-failed
                                    run backend (drover-error-message err))
                                   nil))))
              (cond ((not (memq source (plist-get run :sources)))
                     (when (functionp cancel)
                       (drover-with-reporting "remote search" (funcall cancel))))
                    ((functionp cancel) (setcdr source cancel))
                    (t (plist-put run :sources (delq source (plist-get run :sources))))))))
        (drover-remote--search-maybe-finish connection id))))))

(defun drover-remote--search-failed (run backend message)
  "Note in RUN that BACKEND failed to search, telling MESSAGE, and drop it."
  (plist-put run :failed (cons (cons (drover-backend-label backend)
                                     (or (drover-bound-line (format "%s" message) drover-error-length) "failed"))
                               (plist-get run :failed)))
  (plist-put run :sources (assq-delete-all backend (plist-get run :sources))))

(defun drover-remote--search-receiver (connection id backend)
  "Return the function BACKEND reports to, searching for the peer of CONNECTION.
The search is ID; what comes once it ended is dropped."
  (let ((link (drover-link connection)))
    (lambda (result)
      (drover-with-reporting "remote search"
        (when-let* ((connection (drover-linked link))
                    (table (gethash connection drover-remote--searching))
                    (run (gethash id table)))
          (cond
           ((eq result 'done)
            (plist-put run :sources (assq-delete-all backend (plist-get run :sources)))
            (drover-remote--search-maybe-finish connection id))
           ((and (consp result) (eq (car result) :failure))
            (drover-remote--search-failed run backend (cadr result))
            (drover-remote--search-maybe-finish connection id))
           ((listp result)
            (let ((matches nil))
              (dolist (match result)
                (when (> (plist-get run :left) 0)
                  (when-let* ((wire (drover-remote--wire-match match)))
                    (plist-put run :left (1- (plist-get run :left)))
                    (push wire matches))))
              (when (and matches (spyglass-connection-live-p connection))
                (drover-remote--send connection (list :t "srm" :id id :m (vconcat (nreverse matches)))))
              (when (<= (plist-get run :left) 0)
                (drover-remote--search-finish connection id))))))))))

(defun drover-remote--search-maybe-finish (connection id)
  "End the search ID of the peer of CONNECTION once no backend searches."
  (when-let* ((table (gethash connection drover-remote--searching))
              (run (gethash id table)))
    (unless (plist-get run :sources)
      (drover-remote--search-finish connection id))))

(defun drover-remote--search-stop-run (connection id why)
  "Stop the search ID run for the peer of CONNECTION, its backends too.
WHY, a symbol's name or nil, is noted in the peer's measures."
  (when-let* ((table (gethash connection drover-remote--searching))
              (run (gethash id table)))
    (remhash id table)
    (when (timerp (plist-get run :timer))
      (cancel-timer (plist-get run :timer)))
    (pcase-dolist (`(,backend . ,cancel) (plist-get run :sources))
      (when (functionp cancel)
        (drover-with-reporting (format "search of %s" backend)
          (funcall cancel))))
    (plist-put run :sources nil)
    (when-let* ((peer (drover-remote--peer (plist-get run :peer))))
      (drover-remote--stat-max peer :search-max (- (float-time) (plist-get run :start)))
      (when why
        (drover-remote--stat peer :searches-stopped)))
    run))

(defun drover-remote--search-finish (connection id)
  "End the search ID run for the peer of CONNECTION, telling it how it went."
  (when-let* ((run (drover-remote--search-stop-run connection id nil)))
    (when (spyglass-connection-live-p connection)
      (drover-remote--send connection
                           (append (list :t "sre" :id id)
                                   (when-let* ((failed (plist-get run :failed)))
                                     (list :err (drover-bound-line
                                                 (mapconcat (pcase-lambda (`(,label . ,message))
                                                              (format "%s: %s" label message))
                                                            (reverse failed) "; ")
                                                 drover-error-length))))))))

(defun drover-remote--on-search-stop (connection message)
  "Stop the search MESSAGE names, its peer, of CONNECTION, typing past it."
  (when-let* ((id (plist-get message :id))
              ((stringp id)))
    (drover-remote--search-stop-run connection id "stopped")))

(defun drover-remote--search-unserve (connection)
  "Stop the searches run for the peer of CONNECTION, whose stream ended."
  (when-let* ((table (gethash connection drover-remote--searching)))
    (dolist (id (hash-table-keys table))
      (drover-remote--search-stop-run connection id nil))
    (remhash connection drover-remote--searching)))

;;;; Cloning a conversation to another host

(declare-function drover-engine-property "drover-model" (engine property))
(declare-function drover-engine-label "drover-model" (engine))
(declare-function drover-links-open "drover-links" (conversation &optional front-end))
(declare-function drover-links-parse "drover-links" (path &optional engine))

(defcustom drover-remote-clone-limit (* 1024 1024 1024)
  "Bytes at most of a conversation cloned to or from another host."
  :type 'natnum
  :group 'drover-remote)

(defcustom drover-remote-clone-timeout 900
  "Seconds a clone between hosts may take, its transfer included."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-clone-piece 262144
  "Bytes of a conversation one request of a clone carries.")

(defconst drover-remote-clone-window 4
  "Requests of a clone's pieces awaiting their answers at once.")

(defconst drover-remote-clone-idle 300
  "Seconds a conversation exported for a peer waits for its next request.
It is dropped past them.")

(defconst drover-remote-clone-parts 10001
  "Files at most of a conversation cloned.")

(defconst drover-remote-clone-origins 1000
  "Clones this host received whose origins it keeps, the newest.")

(defconst drover-remote--clone-engines '(claude-code codex opencode)
  "The engines whose conversations hosts clone, their files named after them.")

(defvar drover-remote--exports (make-hash-table :test #'equal)
  "Conversations exported for peers to clone, by identifier.
Each a plist: :peer, the peer's name, :parts, a vector of (NAME SOURCE
SIZE), SOURCE a file of this host or bytes, :served, the bytes sent,
and :timer, dropping it once unused.")

(defvar drover-remote--clones (make-hash-table :test #'equal)
  "Clones this host receives, by the identifier of their export.
See `drover-remote--clone-receive'.")

(defvar drover-remote--clone-serial 0
  "The number of the last conversation exported for a peer.")

;;;;; Directories

(defun drover-remote--clone-home-relative (directory)
  "Return DIRECTORY relative to this host's home, or nil when not under it."
  (when (stringp directory)
    (let ((home (file-name-as-directory (expand-file-name "~")))
          (directory (file-name-as-directory (expand-file-name directory))))
      (and (string-prefix-p home directory (memq system-type '(windows-nt ms-dos cygwin darwin)))
           (substring directory (length home))))))

(defun drover-remote--clone-slashed (directory)
  "Return DIRECTORY with forward slashes and a trailing one, for comparing.
DIRECTORY may be another host's, which keeps its spelling: only its
separators change, where `file-name-as-directory' would spell it as
this host does, a drive's letter lower case on Windows.  An empty
DIRECTORY is the current one, ./, as that function has it."
  (let ((slashed (replace-regexp-in-string "\\\\" "/" directory)))
    (cond ((string-empty-p slashed) "./")
          ((string-suffix-p "/" slashed) slashed)
          (t (concat slashed "/")))))

(defun drover-remote--clone-map (peer directory home)
  "Return where a conversation of PEER run in DIRECTORY goes here, or nil.
DIRECTORY is PEER's, HOME the same relative to PEER's home, or nil.
PEER's :directories map it, their first prefix of it; else, under this
host's home, HOME.  The value is (DIRECTORY . EXISTS), EXISTS non-nil
when that directory is here."
  (let* ((theirs (and (stringp directory) (drover-remote--clone-slashed directory)))
         (mapped
          (or (and theirs
                   (seq-some (pcase-lambda (`(,prefix . ,ours))
                               (when (and (stringp prefix) (stringp ours))
                                 (let ((prefix (drover-remote--clone-slashed prefix)))
                                   (and (string-prefix-p prefix theirs t)
                                        (expand-file-name (substring theirs (length prefix))
                                                          (expand-file-name ours))))))
                             (plist-get (drover-remote--peer-properties peer) :directories)))
              (and (stringp home)
                   (not (string-match-p "\\(?:\\`\\|/\\)\\.\\.\\(?:/\\|\\'\\)" home))
                   (not (file-name-absolute-p home))
                   (expand-file-name home "~")))))
    (when mapped
      (let ((mapped (file-name-as-directory mapped)))
        (cons mapped (file-directory-p mapped))))))

;;;;; There, exporting this host's conversation

(defun drover-remote--clone-engine (engine property &optional loaded)
  "Return ENGINE's PROPERTY for clones, :export or :import, or nil.
Only for the engines hosts clone the conversations of: Claude Code's
and Codex's, their engines' (see `drover-register-engine'), their files
loaded unless LOADED; OpenCode's, this host's service's, which this
layer relays (see `drover-remote--oc-export')."
  (cond
   ((eq engine 'opencode)
    (and (drover-remote--serves-opencode-p)
         (pcase property
           (:export #'drover-remote--oc-export)
           (:import #'drover-remote--oc-import))))
   ((memq engine drover-remote--clone-engines)
    (unless (or loaded (and (fboundp 'drover-engine-property) (drover-engine-property engine property)))
      (require (intern (concat "drover-" (symbol-name engine))) nil t))
    (and (fboundp 'drover-engine-property)
         (drover-engine-property engine property)))))

(defun drover-remote--engine-label (engine)
  "Return how views name ENGINE, a symbol or nil."
  (cond ((null engine) "Its")
        ((fboundp 'drover-engine-label) (drover-engine-label engine))
        (t (symbol-name engine))))

(defun drover-remote--clone-part-name-p (name)
  "Return non-nil when NAME may be the name of a part of a clone.
A relative file name of slashes, none of its components empty, \".\"
or \"..\"."
  (and (stringp name) (< 0 (length name) 512)
       (not (string-match-p "[\\\\:\0]\\|\\`[/~]" name))
       (not (seq-some (lambda (part) (member part '("" "." "..")))
                      (split-string name "/")))))

(defun drover-remote--clone-link (host engine id)
  "Return the link naming conversation ID of ENGINE on HOST."
  (format "agent-session://%s/%s/%s" host engine id))

(defun drover-remote--export (record peer-name callback)
  "Export the conversation of RECORD, of this host, for PEER-NAME to clone.
CALLBACK gets what the peer needs, a plist as it travels, or nil and
why not, a line, once.  Its engine gathers the files (its :export);
they stay where they are, read as the peer asks for them."
  (let* ((engine (drover-session-engine record))
         (id (drover-session-engine-id record))
         (export (and engine (drover-remote--clone-engine engine :export))))
    (cond
     ((not (drover-remote--local-p record))
      (funcall callback nil "it is not a conversation of this host"))
     ((not (and (stringp id) (functionp export)))
      (funcall callback nil (format "%s conversations are not cloned" (drover-remote--engine-label engine))))
     ((drover-session-parent record)
      (funcall callback nil "a subagent is not cloned alone"))
     (t
      (funcall export id
               (lambda (result)
                 (drover-with-reporting "clone export"
                   (if (eq (car-safe result) :failure)
                       (funcall callback nil (drover-bound-line (format "%s" (cadr result))))
                     (drover-remote--export-register record peer-name result callback)))))))))

(defun drover-remote--export-register (record peer-name result callback)
  "Keep RESULT, the export of RECORD's conversation, for PEER-NAME; tell CALLBACK."
  (let ((parts nil)
        (total 0)
        (why nil))
    (dolist (part (plist-get result :parts))
      (pcase part
        ((and `(,name :bytes ,bytes) (guard (and (drover-remote--clone-part-name-p name) (stringp bytes))))
         (let ((bytes (if (multibyte-string-p bytes) (encode-coding-string bytes 'utf-8) bytes)))
           (push (list name 'bytes bytes (length bytes)) parts)))
        ((and `(,name ,file ,size)
              (guard (and (drover-remote--clone-part-name-p name) (stringp file) (natnump size))))
         (if (file-readable-p file)
             (push (list name 'file file size) parts)
           (setq why (format "a file it cannot read: %s" (drover-bound-line name 80)))))
        (_ (setq why "a part it cannot send"))))
    (setq parts (nreverse parts))
    (dolist (part parts)
      (setq total (+ total (nth 3 part))))
    (cond
     (why (funcall callback nil why))
     ((> (length parts) drover-remote-clone-parts)
      (funcall callback nil (format "more than %d files" drover-remote-clone-parts)))
     ((> total drover-remote-clone-limit)
      (funcall callback nil (format "%s, more than `drover-remote-clone-limit'" (file-size-human-readable total))))
     (t
      (let* ((xid (format "%s-%s-%d" drover-host
                          (substring (or (drover-remote--state-epoch (drover-remote--current)) "x") 0 8)
                          (cl-incf drover-remote--clone-serial)))
             (engine (drover-session-engine record))
             (id (drover-session-engine-id record))
             (directory (drover-session-directory record)))
        (puthash xid (list :peer peer-name :served 0 :parts (vconcat parts)
                           :timer (run-at-time drover-remote-clone-idle nil #'drover-remote--export-drop xid))
                 drover-remote--exports)
        (funcall callback
                 (list :x xid :engine (symbol-name engine) :id id
                       :title (or (drover-remote--plain (drover-session-title record)) "")
                       :directory (or (drover-remote--plain directory) :false)
                       :home (or (drover-remote--clone-home-relative directory) :false)
                       :cwd (or (drover-remote--plain (plist-get result :cwd)) :false)
                       :origin (drover-remote--clone-link drover-host engine id)
                       :size total
                       :parts (vconcat (mapcar (lambda (part) (vector (car part) (nth 3 part))) parts)))
                 nil))))))

(defun drover-remote--export-drop (xid)
  "Forget the conversation exported as XID."
  (drover-with-reporting "clone export"
    (when-let* ((entry (gethash xid drover-remote--exports)))
      (remhash xid drover-remote--exports)
      (when (timerp (plist-get entry :timer))
        (cancel-timer (plist-get entry :timer))))))

(defun drover-remote--on-clone-export (peer key reply)
  "Export the conversation of this host's session KEY for PEER to clone it.
REPLY answers with what PEER needs, its parts named and sized.  A peer
of any access may: it reads the conversation anyway."
  (if-let* ((record (drover-remote--owner-record key "clone")))
      (drover-remote--export record (drover-remote--peer-name peer)
                             (lambda (description why)
                               (if description
                                   (progn
                                     (drover-remote--event peer 'clone "exported %s for it to clone"
                                                           (drover-bound-line (plist-get description :title) 60))
                                     (funcall reply description))
                                 (funcall reply nil why))))
    (funcall reply nil "no such session here")))

(defun drover-remote--on-clone-piece (peer message reply)
  "Answer through REPLY the piece of a conversation exported that MESSAGE asks.
The export must be PEER's: X its identifier, I its part, O the offset,
N the bytes, `drover-remote-clone-piece' at most; they go in base64."
  (let* ((xid (plist-get message :x))
         (entry (and (stringp xid) (gethash xid drover-remote--exports)))
         (index (plist-get message :i))
         (offset (plist-get message :o))
         (count (plist-get message :n))
         (part (and entry (natnump index) (< index (length (plist-get entry :parts)))
                    (aref (plist-get entry :parts) index))))
    (cond
     ((not (and entry (equal (plist-get entry :peer) (drover-remote--peer-name peer))))
      (funcall reply nil "no such export here"))
     ((not (and part (natnump offset) (natnump count) (<= count drover-remote-clone-piece)
                (<= (+ offset count) (nth 3 part))))
      (funcall reply nil "no such piece"))
     (t
      (let ((bytes (drover-remote--export-read part offset count)))
        (if (not (= (length bytes) count))
            (funcall reply nil "its file is shorter than when exported")
          (when (timerp (plist-get entry :timer))
            (cancel-timer (plist-get entry :timer)))
          (plist-put entry :timer (run-at-time drover-remote-clone-idle nil #'drover-remote--export-drop xid))
          (plist-put entry :served (+ (plist-get entry :served) count))
          (funcall reply (list :d (base64-encode-string bytes t)))))))))

(defun drover-remote--export-read (part offset count)
  "Return COUNT bytes of PART of an export from OFFSET, a unibyte string."
  (pcase-let ((`(,_name ,kind ,source ,_size) part))
    (if (eq kind 'bytes)
        (substring source offset (+ offset count))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally source nil offset (+ offset count))
        (buffer-string)))))

;;;;; Here, receiving a clone

(defun drover-remote--clone-rewrite (carry chunk pairs final)
  "Return CHUNK, after CARRY, its NEEDLES replaced, as (OUT . CARRY).
PAIRS is a list of (NEEDLE . REPLACEMENT), unibyte strings, the needle
found first replaced first.  The end of CHUNK that may begin a needle is
held back as the new CARRY, unless FINAL: a needle cut between two
pieces is found whole."
  (let* ((text (concat carry chunk))
         (keep (if final 0 (1- (apply #'max (mapcar (lambda (pair) (length (car pair))) pairs)))))
         (start 0)
         (out nil))
    (while (let ((found nil))
             (dolist (pair pairs)
               (when-let* ((at (string-search (car pair) text start)))
                 (when (or (null found) (< at (car found)))
                   (setq found (cons at pair)))))
             (when found
               (push (substring text start (car found)) out)
               (push (cddr found) out)
               (setq start (+ (car found) (length (cadr found))))
               t)))
    (let ((safe (max start (- (length text) keep))))
      (push (substring text start safe) out)
      (cons (apply #'concat (nreverse out)) (substring text safe)))))

(defun drover-remote--clone-pairs (engine cwd directory)
  "Return what the records of a clone of ENGINE have replaced, for DIRECTORY.
CWD is how the records spell the directory it ran in there, as JSON
writes it; the value is a list of (NEEDLE . REPLACEMENT), unibyte, or
nil when nothing is."
  (when-let* (((stringp cwd))
              ((not (string-empty-p cwd)))
              (spell (drover-remote--clone-engine engine :record-directory))
              (ours (funcall spell directory))
              (needle (encode-coding-string (concat "\"cwd\":\"" cwd "\"") 'utf-8))
              (replacement (encode-coding-string (concat "\"cwd\":" (json-serialize ours)) 'utf-8))
              ((not (equal needle replacement))))
    (list (cons needle replacement))))

(defun drover-remote--clone-description (data)
  "Return DATA, a description of a conversation to clone received, checked.
A plist of :x, :engine, a symbol, :id, :title, :directory and :home of
the host it comes from, :cwd, :origin, :size and :parts, a list of
\(NAME SIZE), or a line telling why it does not fit."
  (let* ((xid (plist-get data :x))
         (engine (drover-remote--symbol (plist-get data :engine)))
         (id (plist-get data :id))
         (parts (plist-get data :parts))
         (checked (and (vectorp parts) (<= (length parts) drover-remote-clone-parts)
                       (mapcar (lambda (part)
                                 (and (vectorp part) (= (length part) 2)
                                      (drover-remote--clone-part-name-p (aref part 0))
                                      (natnump (aref part 1))
                                      (list (aref part 0) (aref part 1))))
                               parts)))
         (total (apply #'+ (mapcar (lambda (part) (or (cadr part) 0)) checked))))
    (cond
     ((not (and (stringp xid) (string-match-p drover-remote--id-regexp xid))) "no export")
     ((not (memq engine drover-remote--clone-engines)) "not a conversation hosts clone")
     ((not (and (stringp id) (string-match-p "\\`[A-Za-z0-9._-]\\{1,128\\}\\'" id))) "no conversation")
     ((or (null checked) (memq nil checked)) "files it cannot take")
     ((> total drover-remote-clone-limit)
      (format "%s, more than `drover-remote-clone-limit'" (file-size-human-readable total)))
     (t (list :x xid :engine engine :id id
              :title (or (drover-remote--line (plist-get data :title)) "")
              :directory (drover-remote--line (plist-get data :directory) 1024)
              :home (drover-remote--line (plist-get data :home) 1024)
              :cwd (drover-remote--line (plist-get data :cwd) 4096)
              :origin (drover-remote--line (plist-get data :origin) 1024)
              :size total :parts checked)))))

(defun drover-remote--clone-receive (peer description directory callback)
  "Receive from PEER the conversation DESCRIPTION describes, as a clone here.
DESCRIPTION is as `drover-remote--clone-description' returns it,
DIRECTORY this host's directory the clone runs in.  The parts come in
pieces, `drover-remote-clone-window' requests awaiting answers at once,
staged in a temporary directory, the records' spelling of the directory
rewritten as they arrive (see `drover-remote--clone-pairs'); then the
engine makes them a conversation of this host (its :import).  CALLBACK
gets its identifier here, or nil and why not, once, within
`drover-remote-clone-timeout' seconds.  Return the clone's identifier,
the export's."
  (let* ((xid (plist-get description :x))
         (engine (plist-get description :engine))
         (state (list :peer (drover-remote--peer-name peer) :description description
                      :directory directory :callback callback
                      :parts (vconcat (plist-get description :parts))
                      :index 0 :next 0 :written 0 :flight 0 :carry ""
                      :pending (make-hash-table :test #'eql)
                      :pairs (drover-remote--clone-pairs engine (plist-get description :cwd) directory)
                      :staging (make-temp-file "drover-clone-" t)
                      :files nil :received 0 :start (float-time)
                      :timer (run-at-time drover-remote-clone-timeout nil #'drover-remote--clone-fail
                                          xid "it took too long"))))
    (puthash xid state drover-remote--clones)
    (drover-remote--event peer 'clone "receiving %s, %s"
                          (drover-bound-line (plist-get description :title) 60)
                          (file-size-human-readable (plist-get description :size)))
    (drover-remote--clone-pump xid)
    xid))

(defun drover-remote--clone-staged (state)
  "Return the file of STATE staging its current part."
  (expand-file-name (format "%d" (plist-get state :index)) (plist-get state :staging)))

(defun drover-remote--clone-pump (xid)
  "Ask for the next pieces of the clone XID, or end it once all arrived."
  (drover-with-reporting "clone"
    (when-let* ((state (gethash xid drover-remote--clones))
                (peer (drover-remote--peer (plist-get state :peer))))
      (let ((parts (plist-get state :parts)))
        (if (>= (plist-get state :index) (length parts))
            (drover-remote--clone-import xid)
          (pcase-let ((`(,_name ,size) (aref parts (plist-get state :index))))
            (unless (file-exists-p (drover-remote--clone-staged state))
              (let ((coding-system-for-write 'no-conversion))
                (write-region "" nil (drover-remote--clone-staged state) nil 'silent)))
            (if (and (= (plist-get state :written) size) (= (plist-get state :flight) 0))
                (drover-remote--clone-next xid)
              (while (and (eq state (gethash xid drover-remote--clones))
                          (< (plist-get state :flight) drover-remote-clone-window)
                          (< (plist-get state :next) size))
                (let* ((offset (plist-get state :next))
                       (count (min drover-remote-clone-piece (- size offset)))
                       (index (plist-get state :index)))
                  (plist-put state :next (+ offset count))
                  (plist-put state :flight (1+ (plist-get state :flight)))
                  (drover-remote--request
                   peer (list :op "clone-piece" :x xid :i index :o offset :n count)
                   (lambda (reply error)
                     (drover-remote--clone-arrived xid index offset count reply error))
                   (+ (float-time) drover-remote-clone-timeout) t))))))))))

(defun drover-remote--clone-arrived (xid index offset count reply error)
  "Take the piece of part INDEX of the clone XID from OFFSET, COUNT bytes.
REPLY carries it, else ERROR tells why not."
  (drover-with-reporting "clone"
    (when-let* ((state (gethash xid drover-remote--clones))
                ((= index (plist-get state :index))))
      (let ((bytes (and (null error) (stringp (plist-get reply :d))
                        (ignore-errors (base64-decode-string (plist-get reply :d))))))
        (cond
         (error (drover-remote--clone-fail xid error))
         ((not (and bytes (= (length bytes) count)))
          (drover-remote--clone-fail xid "a piece did not arrive whole"))
         (t
          (plist-put state :flight (1- (plist-get state :flight)))
          (puthash offset bytes (plist-get state :pending))
          (drover-remote--clone-write state)
          (drover-remote--clone-pump xid)))))))

(defun drover-remote--clone-write (state)
  "Write the pieces of STATE's current part that follow what it wrote, in order."
  (let* ((pending (plist-get state :pending))
         (part (aref (plist-get state :parts) (plist-get state :index)))
         (rewrite (and (plist-get state :pairs) (string-suffix-p ".jsonl" (car part))))
         (file (drover-remote--clone-staged state))
         (coding-system-for-write 'no-conversion)
         (write-region-inhibit-fsync t))
    (while-let ((bytes (gethash (plist-get state :written) pending)))
      (remhash (plist-get state :written) pending)
      (plist-put state :written (+ (plist-get state :written) (length bytes)))
      (plist-put state :received (+ (plist-get state :received) (length bytes)))
      (let ((out (if rewrite
                     (let ((cut (drover-remote--clone-rewrite (plist-get state :carry) bytes
                                                              (plist-get state :pairs)
                                                              (= (plist-get state :written) (cadr part)))))
                       (plist-put state :carry (cdr cut))
                       (car cut))
                   bytes)))
        (unless (string-empty-p out)
          (write-region out nil file t 'silent))))))

(defun drover-remote--clone-next (xid)
  "Go on with the next part of the clone XID, its current one staged whole."
  (when-let* ((state (gethash xid drover-remote--clones)))
    (let ((part (aref (plist-get state :parts) (plist-get state :index))))
      (plist-put state :files (cons (cons (car part) (drover-remote--clone-staged state))
                                    (plist-get state :files)))
      (plist-put state :index (1+ (plist-get state :index)))
      (plist-put state :next 0)
      (plist-put state :written 0)
      (plist-put state :carry "")
      (drover-remote--clone-pump xid))))

(defun drover-remote--clone-end (xid)
  "Forget the clone XID: its staged files, its timer; its peer drops its export."
  (when-let* ((state (gethash xid drover-remote--clones)))
    (remhash xid drover-remote--clones)
    (when (timerp (plist-get state :timer))
      (cancel-timer (plist-get state :timer)))
    (ignore-errors (delete-directory (plist-get state :staging) t))
    (when-let* ((peer (drover-remote--peer (plist-get state :peer)))
                (connection (drover-remote--peer-connection peer))
                ((spyglass-connection-live-p connection)))
      (drover-remote--send connection (list :t "clone-end" :x xid)))
    state))

(defun drover-remote--clone-fail (xid why)
  "End the clone XID, which failed because of WHY."
  (drover-with-reporting "clone"
    (when-let* ((state (drover-remote--clone-end xid)))
      (when-let* ((peer (drover-remote--peer (plist-get state :peer))))
        (drover-remote--event peer 'clone "the clone of %s failed: %s"
                              (drover-bound-line (plist-get (plist-get state :description) :title) 60) why))
      (funcall (plist-get state :callback) nil (drover-bound-line (format "%s" why))))))

(defun drover-remote--clone-import (xid)
  "Have the engine of the clone XID, all of it staged, make it this host's."
  (when-let* ((state (gethash xid drover-remote--clones)))
    (let* ((description (plist-get state :description))
           (engine (plist-get description :engine))
           (import (drover-remote--clone-engine engine :import)))
      (if (not (functionp import))
          (drover-remote--clone-fail xid (format "this host clones no %s conversation"
                                                 (drover-remote--engine-label engine)))
        (let ((done nil))
          (condition-case err
              (funcall import (plist-get description :id) (reverse (plist-get state :files))
                       description (plist-get state :directory)
                       (lambda (id)
                         (unless done
                           (setq done t)
                           (drover-with-reporting "clone"
                             (when-let* ((state (drover-remote--clone-end xid)))
                               (drover-remote--clone-note engine id (plist-get description :origin)
                                                          (plist-get description :title))
                               (when-let* ((peer (drover-remote--peer (plist-get state :peer))))
                                 (drover-remote--event peer 'clone "cloned %s here, %s in %.1fs"
                                                       (drover-bound-line (plist-get description :title) 60)
                                                       (file-size-human-readable (plist-get state :received))
                                                       (- (float-time) (plist-get state :start))))
                               (funcall (plist-get state :callback) id nil)))))
                       (lambda (why)
                         (unless done
                           (setq done t)
                           (drover-remote--clone-fail xid why))))
            (error (drover-remote--clone-fail xid (drover-error-message err)))))))))

(defun drover-remote--clone-drop (connection)
  "Fail the clones received from the peer of CONNECTION, whose stream ended."
  (let ((ended nil))
    (maphash (lambda (xid state)
               (when-let* ((peer (drover-remote--peer (plist-get state :peer))))
                 (when (memq (drover-remote--peer-connection peer) (list nil connection))
                   (push xid ended))))
             drover-remote--clones)
    (dolist (xid ended)
      (drover-remote--clone-fail xid "the link ended"))))

;;;;; Where clones come from

(defun drover-remote--clone-origins-file ()
  "Return the file keeping where the clones this host received come from."
  (expand-file-name "clones.eld" drover-state-directory))

(defun drover-remote--clone-origins ()
  "Return where the clones this host received come from, newest first.
A list of (ENGINE ID ORIGIN TIME TITLE), ORIGIN a link; nil when the
file does not read."
  (let ((file (drover-remote--clone-origins-file)))
    (when (file-readable-p file)
      (let ((value (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents file)
                       (read (current-buffer))))))
        (and (listp value)
             (seq-filter (lambda (entry)
                           (and (consp entry) (symbolp (nth 0 entry)) (stringp (nth 1 entry))
                                (stringp (nth 2 entry))))
                         value))))))

(defun drover-remote--clone-note (engine id origin title)
  "Note that conversation ID of ENGINE here is a clone of ORIGIN, a link.
TITLE is the original's.  The newest `drover-remote-clone-origins' are kept."
  (when (stringp origin)
    (let ((entries (cons (list engine id origin (float-time) title)
                         (seq-remove (lambda (entry) (and (eq (nth 0 entry) engine) (equal (nth 1 entry) id)))
                                     (drover-remote--clone-origins))))
          (file (drover-remote--clone-origins-file)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (let ((print-length nil) (print-level nil))
          (insert ";; Where the clones this host received come from, newest first.\n")
          (prin1 (seq-take entries drover-remote-clone-origins) (current-buffer))
          (insert "\n"))))))

(defun drover-remote-clone-origin (session)
  "Return the link to the conversation SESSION's was cloned from, or nil.
For a session of this host cloned from another host."
  (when-let* (((null (drover-session-host session)))
              (engine (drover-session-engine session))
              (id (drover-session-engine-id session)))
    (nth 2 (seq-find (lambda (entry) (and (eq (nth 0 entry) engine) (equal (nth 1 entry) id)))
                     (drover-remote--clone-origins)))))

;;;###autoload
(defun drover-remote-open-origin (session)
  "Open the conversation SESSION, a clone this host received, was cloned from.
Interactively, SESSION is the session at point."
  (interactive (list (drover-session-at-point)))
  (require 'drover-links)
  (let ((origin (or (drover-remote-clone-origin session)
                    (user-error "%s is not a clone this host received"
                                (drover-session-display-title session)))))
    (drover-links-open (or (drover-links-parse (string-remove-prefix "agent-session:" origin))
                           (user-error "Not a link: %s" origin)))))

;;;;; Answering peers

(defun drover-remote--on-clone-where (peer message reply)
  "Tell PEER, through REPLY, where a clone run in MESSAGE's directory goes here.
Only for a peer of full access, which may clone here, never a viewer."
  (if (or (drover-remote--viewer-p peer) (not (eq (drover-remote--access peer) 'full)))
      (funcall reply nil (drover-remote--refuse-action peer "clone" (format "%s has %s access, not full"
                                                                            (drover-remote--label peer)
                                                                            (drover-remote--access peer))))
    (pcase-let ((`(,directory . ,exists)
                 (drover-remote--clone-map peer (drover-remote--line (plist-get message :directory) 1024)
                                           (drover-remote--line (plist-get message :home) 1024))))
      (funcall reply (list :directory (or directory :false) :exists (if exists t :false))))))

(defun drover-remote--on-clone (peer message reply)
  "Receive from PEER a clone of the conversation MESSAGE describes, run here.
REPLY answers once it is a conversation of this host, with its
identifier, or tells why not.  Only for a peer of full access: a clone
writes this host's files and makes a conversation of its."
  (let ((description (drover-remote--clone-description message))
        (directory (drover-remote--line (plist-get message :target) 1024)))
    (cond
     ((drover-remote--viewer-p peer)
      (funcall reply nil (drover-remote--refuse-action peer "clone" (format "%s owns no conversations"
                                                                            (drover-remote--label peer)))))
     ((not (eq (drover-remote--access peer) 'full))
      (funcall reply nil (drover-remote--refuse-action peer "clone" (format "%s has %s access, not full"
                                                                            (drover-remote--label peer)
                                                                            (drover-remote--access peer)))))
     ((stringp description) (funcall reply nil description))
     ((not (and directory (file-name-absolute-p directory) (file-directory-p directory)))
      (funcall reply nil (format "no directory %s here" (or directory "named"))))
     ((gethash (plist-get description :x) drover-remote--clones)
      (funcall reply nil "it is being cloned here already"))
     (t
      (drover-remote--clone-receive peer description (file-name-as-directory directory)
                                    (lambda (id why)
                                      (if id
                                          (funcall reply (list :id id))
                                        (funcall reply nil why))))))))

;;;;; The command

(defun drover-remote--clone-targets (session)
  "Return the hosts the conversation of SESSION may be cloned to now.
An alist (NAME . PEER), PEER nil for this host: this host for another
host's session, whose engine clones here; for this host's, the peers
connected, cloning, that grant this host full access."
  (let ((engine (drover-session-engine session))
        (id (drover-session-engine-id session)))
    (when (and (memq engine drover-remote--clone-engines) (stringp id) (not (drover-session-parent session)))
      (if-let* ((host (drover-session-host session)))
          (when-let* ((peer (drover-remote--peer host))
                      ((eq (drover-remote--peer-state peer) 'open))
                      ((drover-remote--capable-p peer "clone"))
                      ((drover-remote--clone-engine engine :import t)))
            (list (cons drover-host nil)))
        (when (drover-remote--clone-engine engine :export t)
          (delq nil (mapcar (lambda (peer)
                              (and (eq (drover-remote--peer-state peer) 'open)
                                   (not (drover-remote--viewer-p peer))
                                   (drover-remote--capable-p peer "clone")
                                   (eq (drover-remote--peer-granted peer) 'full)
                                   (cons (drover-remote--peer-name peer) peer)))
                            (drover-remote--peers))))))))

(defun drover-remote--clone-why-not (session)
  "Return why the conversation of SESSION is cloned nowhere now, a line."
  (let ((engine (drover-session-engine session))
        (host (drover-session-host session)))
    (cond
     ((not (memq engine drover-remote--clone-engines))
      (format "%s conversations are not cloned" (drover-remote--engine-label engine)))
     ((not (stringp (drover-session-engine-id session))) "Its conversation is not known yet")
     ((drover-session-parent session) "A subagent is not cloned alone")
     (host
      (let ((peer (drover-remote--peer host)))
        (cond ((not (and peer (eq (drover-remote--peer-state peer) 'open))) (format "%s is not connected" host))
              ((not (drover-remote--capable-p peer "clone"))
               (format "%s runs an older drover, which clones nothing: update it" (drover-remote--label peer)))
              (t (format "This host clones no %s conversation" (drover-remote--engine-label engine))))))
     ((not (drover-remote--peers)) "No other host is configured")
     (t "No connected host clones here, granting this host full access"))))

(defun drover-remote--clone-wait (done progress)
  "Wait in the command until DONE, a cons, holds its value in its car.
PROGRESS returns a line telling how far it went, shown every second.
Return non-nil once done, nil when \\[keyboard-quit] stopped waiting."
  (let ((shown 0))
    (condition-case nil
        (progn
          (while (not (car done))
            (when (> (- (float-time) shown) 1)
              (setq shown (float-time))
              (drover-message "%s" (funcall progress)))
            (accept-process-output nil 0.02))
          t)
      (quit nil))))

(defun drover-remote--clone-progress (title host bytes size)
  "Return how a clone of TITLE to HOST goes, BYTES of SIZE sent."
  (format "Cloning %s to %s... %s of %s (C-g: go on in the background)"
          (drover-bound-line title 40) host
          (file-size-human-readable (or bytes 0)) (file-size-human-readable (or size 0))))

(defun drover-remote--clone-here (session)
  "Clone SESSION, of another host, here, asking in which directory.
Its host exports the conversation, this host takes it in pieces and its
engine makes it a conversation of this host, which opens once done; a
wait stopped with \\[keyboard-quit] goes on in the background."
  (let* ((peer (drover-remote--peer (drover-session-host session)))
         (label (drover-remote--label peer))
         (title (drover-session-display-title session)))
    (drover-message "Asking %s for %s..." label (drover-bound-line title 40))
    (pcase-let* ((`(,reply ,error)
                  (drover-remote--wait peer (list :op "clone-export" :key (drover-remote--wire-key session))
                                       drover-remote-clone-timeout))
                 (description (if error
                                  (user-error "%s did not export %s: %s" label (drover-bound-line title 40) error)
                                (drover-remote--clone-description reply))))
      (when (stringp description)
        (user-error "%s exported %s, which this host cannot take: %s" label (drover-bound-line title 40)
                    description))
      (let* ((xid (plist-get description :x))
             (mapped (drover-remote--clone-map peer (plist-get description :directory)
                                               (plist-get description :home)))
             (directory (condition-case quit
                            (read-directory-name (format "Clone %s into: " (drover-bound-line title 40))
                                                 (and (cdr mapped) (car mapped)) nil t)
                          (quit (when-let* ((connection (drover-remote--peer-connection peer)))
                                  (drover-remote--send connection (list :t "clone-end" :x xid)))
                                (signal (car quit) (cdr quit)))))
             (done (list nil)))
        (unless (file-directory-p directory)
          (user-error "No directory %s" directory))
        (drover-remote--clone-receive peer description (file-name-as-directory (expand-file-name directory))
                                      (lambda (id why) (setcar done (if id (list :id id) (list :failure why)))))
        (drover-remote--clone-finish
         done (lambda () (drover-remote--clone-progress
                          title drover-host (plist-get (gethash xid drover-remote--clones) :received)
                          (plist-get description :size)))
         (lambda (id)
           (drover-message "Cloned %s here, from %s" (drover-bound-line title 40) label)
           (require 'drover-links)
           (drover-links-open (list :engine (plist-get description :engine) :id id :host nil)))
         (lambda (why) (format "Could not clone %s here: %s" (drover-bound-line title 40) why)))))))

(defun drover-remote--clone-finish (done progress then else)
  "Wait for DONE, telling PROGRESS; then call THEN with the clone's identifier.
On failure, warn with what ELSE returns of why.  A wait stopped with
\\[keyboard-quit] reports the outcome once it comes, without opening."
  (if (drover-remote--clone-wait done progress)
      (pcase (car done)
        (`(:id ,id) (funcall then id))
        (`(:failure ,why) (drover-message "%s" (funcall else why))))
    (drover-message "The clone goes on in the background")
    (let ((timer nil))
      (setq timer (run-at-time 1 1 (lambda ()
                                     (drover-with-reporting "clone"
                                       (when (car done)
                                         (cancel-timer timer)
                                         (pcase (car done)
                                           (`(:id ,_) (drover-message "%s" (funcall progress)))
                                           (`(:failure ,why) (drover-warn "%s" (funcall else why))))))))))))

(defun drover-remote--clone-there (session peer)
  "Clone SESSION, of this host, to PEER, asking in which of its directories.
PEER suggests one; this host exports the conversation, PEER takes it in
pieces and its engine makes it a conversation of PEER's, which opens
here once done, through PEER, as opening any of its sessions does."
  (let* ((label (drover-remote--label peer))
         (title (drover-session-display-title session))
         (directory (drover-session-directory session)))
    (pcase-let* ((`(,where ,error)
                  (drover-remote--wait peer (list :op "clone-where" :directory (or directory :false)
                                                  :home (or (drover-remote--clone-home-relative directory) :false))
                                       drover-remote-open-timeout))
                 (suggested (and (null error) (stringp (plist-get where :directory))
                                 (drover-remote--line (plist-get where :directory) 1024))))
      (when error
        (user-error "%s takes no clone: %s" label error))
      (let* ((target (string-trim (read-string (format "Clone %s into, on %s: " (drover-bound-line title 40) label)
                                               suggested)))
             (described (list nil))
             (done (list nil)))
        (when (string-empty-p target)
          (user-error "A clone needs a directory"))
        (drover-remote--export session (drover-remote--peer-name peer)
                               (lambda (description why) (setcar described (or description why))))
        (unless (drover-remote--clone-wait described (lambda () (format "Exporting %s..." (drover-bound-line title 40))))
          (user-error "Stopped exporting %s" (drover-bound-line title 40)))
        (when (stringp (car described))
          (user-error "Could not export %s: %s" (drover-bound-line title 40) (car described)))
        (let ((description (car described)))
          (drover-remote--request
           peer (append (list :op "clone" :target target) description)
           (lambda (reply error)
             (setcar done (if error
                              (list :failure error)
                            (list :id (drover-remote--line (plist-get reply :id) 128)))))
           (+ (float-time) drover-remote-clone-timeout) t)
          (drover-remote--clone-finish
           done (lambda () (drover-remote--clone-progress
                            title label (plist-get (gethash (plist-get description :x) drover-remote--exports) :served)
                            (plist-get description :size)))
           (lambda (id)
             (drover-remote--clone-open-there peer (drover-session-engine session) id title))
           (lambda (why) (format "Could not clone %s to %s: %s" (drover-bound-line title 40) label why))))))))

(defun drover-remote--clone-open-there (peer engine id title)
  "Open PEER's conversation ID of ENGINE, a clone of TITLE, once PEER lists it.
Waited for in the command, `drover-remote-open-timeout' seconds at most
\(see `drover-remote--await')."
  (let ((label (drover-remote--label peer))
        (found nil))
    (drover-remote--await peer
                          (lambda ()
                            (setq found (seq-find (lambda (record)
                                                    (and (eq (drover-session-engine record) engine)
                                                         (equal (drover-session-engine-id record) id)))
                                                  (hash-table-values (drover-remote--peer-records peer)))))
                          drover-remote-open-timeout)
    (if found
        (progn (drover-message "Cloned %s to %s" (drover-bound-line title 40) label)
               (drover-open found))
      (drover-message "Cloned %s to %s: %s" (drover-bound-line title 40) label
                      (drover-remote--clone-link (drover-remote--peer-name peer) engine id)))))

(defun drover-remote--clone (session check)
  "Clone the conversation of SESSION to another host, asking which.
With CHECK, return non-nil when there is a host to clone it to now.
For `drover-clone-function'."
  (let ((targets (drover-remote--clone-targets session)))
    (cond
     (check targets)
     ((null targets) (user-error "%s" (drover-remote--clone-why-not session)))
     (t
      (let* ((name (completing-read (format-prompt "Clone %s to host" (caar targets)
                                                   (drover-bound-line (drover-session-display-title session) 40))
                                    (mapcar #'car targets) nil t nil nil (caar targets)))
             (target (assoc name targets)))
        (unless target
          (user-error "No host %s to clone to" name))
        (if (cdr target)
            (drover-remote--clone-there session (cdr target))
          (drover-remote--clone-here session)))))))

;;;; New sessions on peers

;; The prompt's new sessions on another host: what a peer starts, its
;; engines' agents, models and defaults, are the peer's, asked of it
;; (`catalogs'), and so are the directories completed there (`ls',
;; names alone); the peer starts the session through its own backend,
;; without a window, hands it the prompt, and answers its key (`new'),
;; which opens here once its records list it.  A peer of `act' access
;; may start sessions, as it may send prompts and resume conversations.

(declare-function drover-new-session "drover-keys" (directory backend &optional elsewhere))
(defvar drover-prompt-new-targets)
(defvar drover-prompt-remote-functions)
(defvar drover-new-session-elsewhere-function)

(defcustom drover-remote-new-timeout 60
  "Seconds a session started on another host may take to start there.
Its host answers once its backend made the session and took its
prompt: a front end starting a terminal, or a service fetching its
catalogs first, may take a while."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-catalog-timeout 10
  "Seconds this host waits for its engines' defaults, telling a peer its catalogs.
An engine whose defaults did not come by then tells the models it
remembers, and no agent, since reading more would wait for them."
  :type 'number
  :group 'drover-remote)

(defcustom drover-remote-catalog-ttl 300
  "Seconds what a peer told of the sessions it starts is used as it is.
Past them it is still used, and asked again meanwhile."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote-catalog-models 1000
  "Models of an engine at most a host tells its peers.")

(defconst drover-remote-ls-limit 2000
  "Entries of a directory at most a host looks at, listing it for a peer.
It tells the subdirectories among them alone, by name.")

(defconst drover-remote-listing-ttl 30
  "Seconds the subdirectories a peer listed are used without asking again.")

(defconst drover-remote-catalog-failed-ttl 10
  "Seconds a peer that did not tell its catalogs is not asked again.")

(defvar drover-remote--catalogs (make-hash-table :test #'equal)
  "What peers told of the sessions they start, by (NAME LOCAL DIRECTORY).
LOCAL is a directory of this host whose counterpart there was asked,
DIRECTORY one there whose engines' catalogs were, either nil; (NAME nil
nil) holds what NAME told last of what it starts and of its home.  Each
entry is a plist: :time, when told or failed, :data, the answer
decoded, :failed, why it failed, :waiting, the callbacks awaiting the
answer asked.")

(defvar drover-remote--listings (make-hash-table :test #'equal)
  "Subdirectories of peers' directories by (NAME . DIRECTORY), as (TIME . NAMES).")

(defvar drover-remote--told-models (make-hash-table :test #'equal)
  "Models this host told its peers of, by (ENGINE PROVIDER-ID . ID).
A session a peer starts on one of them runs this host's record of it,
with the names its front ends take for it.")

;;;;; There, starting sessions for peers

(defun drover-remote--target-label (backend engine)
  "Return how a new session of BACKEND running ENGINE reads, unlabelled."
  (let ((label (drover-backend-label backend))
        (engine-label (drover-remote--engine-label engine)))
    (if (string= label engine-label)
        (format "%s session" label)
      (format "%s session (%s)" label engine-label))))

(defvar drover-remote--located (make-hash-table :test #'eq)
  "Whether the features of front ends not loaded are installed, by feature.
Each is (TIME . FOUND), looked for again ten minutes later: looking a
library up in `load-path' costs milliseconds a time.")

(defun drover-remote--installed-p (feature)
  "Return non-nil when FEATURE is loaded, or `locate-library' finds it."
  (or (featurep feature)
      (let ((known (gethash feature drover-remote--located)))
        (if (and known (< (- (float-time) (car known)) 600))
            (cdr known)
          (let ((found (and (locate-library (symbol-name feature)) t)))
            (puthash feature (cons (float-time) found) drover-remote--located)
            found)))))

(defun drover-remote--startable ()
  "Return the sessions this host starts for its peers, as plists.
Each is (:backend BACKEND :engine ENGINE :label LABEL :feature FEATURE):
the new sessions this host's prompt offers (`drover-prompt-new-targets')
whose front end is installed, then those of the other backends
registered that start sessions, one per engine."
  (let ((targets nil))
    (cl-flet ((add (backend engine label feature)
                (unless (seq-find (lambda (target) (and (eq (plist-get target :backend) backend)
                                                         (eq (plist-get target :engine) engine)))
                                  targets)
                  (push (list :backend backend :engine engine :label label :feature feature) targets))))
      (dolist (entry (and (boundp 'drover-prompt-new-targets) drover-prompt-new-targets))
        (let ((backend (plist-get entry :backend))
              (engine (plist-get entry :engine))
              (feature (plist-get entry :feature)))
          (when (and backend engine (symbolp backend) (symbolp engine) (symbolp feature)
                     (or (null feature) (drover-remote--installed-p feature))
                     ;; Registered, its backend tells what it starts.
                     (or (not (memq backend (drover-backends)))
                         (memq engine (drover-with-reporting "remote targets"
                                        (drover-backend-engines backend)))))
            (add backend engine
                 (or (drover-remote--plain (plist-get entry :label))
                     (drover-remote--target-label backend engine))
                 feature))))
      (dolist (backend (drover-backends))
        (when (and (functionp (drover-backend-property backend :create))
                   (not (drover-backend-property backend :remote)))
          (dolist (engine (drover-with-reporting "remote targets" (drover-backend-engines backend)))
            (when (symbolp engine)
              (add backend engine (drover-remote--target-label backend engine) nil))))))
    (nreverse targets)))

(defun drover-remote--wire-model (model)
  "Return MODEL, a `drover-model', as it travels to a peer."
  (list :p (or (drover-remote--plain (drover-model-provider-id model)) :false)
        :pn (or (drover-remote--plain (drover-model-provider-name model)) :false)
        :id (or (drover-remote--plain (drover-model-id model)) "")
        :n (or (drover-remote--plain (drover-model-name model)) :false)
        :v (vconcat (mapcar (lambda (variant)
                              (vector (symbol-name (car variant))
                                      (or (drover-remote--plain (plist-get (cdr variant) :effort)) :false)))
                            (seq-take (seq-filter (lambda (variant) (and (consp variant) (symbolp (car variant))))
                                                  (drover-model-variants model))
                                      16)))
        :dv (if (and (drover-model-default-variant model) (symbolp (drover-model-default-variant model)))
                (symbol-name (drover-model-default-variant model))
              :false)
        :vk (if (drover-model-variants-known model) t :false)
        :en (vconcat (mapcar #'symbol-name (seq-take (seq-filter #'symbolp (drover-model-engines model)) 8)))))

(defun drover-remote--tell-model (engine model)
  "Note that this host told its peers of MODEL, of ENGINE.
Return MODEL as it travels."
  (when (> (hash-table-count drover-remote--told-models) (* 8 drover-remote-catalog-models))
    (clrhash drover-remote--told-models))
  (puthash (cons engine (drover-model-key model)) model drover-remote--told-models)
  (drover-remote--wire-model model))

(defun drover-remote--engine-catalog (engine directory answer)
  "Return what this host tells of ENGINE's new sessions in DIRECTORY.
ANSWER is (DEFAULTS) once ENGINE's :defaults answered, else nil: an
engine whose defaults did not come tells only the models it remembers,
since reading its catalogs would wait for them, unless its defaults
read local files alone (:volatile).  It tells its agents, its models
and what a session started without a choice runs."
  (let* ((ready (or (consp answer) (drover-engine-property engine :volatile)))
         (resolved (drover-with-reporting "remote catalogs"
                     (drover-engine-resolve engine (car-safe answer) nil nil nil)))
         (agents (when-let* ((function (and ready (drover-engine-property engine :agents))))
                   (drover-with-reporting "remote catalogs" (funcall function directory))))
         (models (drover-with-reporting "remote catalogs"
                   (if ready (drover--engine-models engine directory) (drover-remembered-models engine))))
         (model (plist-get resolved :model))
         (variant (plist-get resolved :variant)))
    (list :e (symbol-name engine)
          :agents (vconcat (mapcar #'substring-no-properties
                                   (seq-take (seq-filter (lambda (agent) (and (stringp agent) (< (length agent) 200)))
                                                         agents)
                                             100)))
          :models (vconcat (mapcar (lambda (model) (drover-remote--tell-model engine model))
                                   (seq-take (seq-filter #'drover-model-p models) drover-remote-catalog-models)))
          :default (list :a (or (drover-remote--plain (plist-get resolved :agent)) :false)
                         :m (if (drover-model-p model) (drover-remote--tell-model engine model) :false)
                         :v (if (and variant (symbolp variant)) (symbol-name variant) :false)))))

(defun drover-remote--engine-defaults (engines directory callback)
  "Call CALLBACK once with what ENGINES' :defaults deliver for DIRECTORY.
Its argument is an alist (ENGINE . ANSWER), ANSWER (DEFAULTS) for an
engine whose defaults came, nil for one whose did not within
`drover-remote-catalog-timeout' seconds or which has none."
  (let ((answers (mapcar #'list engines))
        (left (length engines))
        (done nil)
        (timer nil))
    (cl-labels ((finish ()
                  (unless done
                    (setq done t)
                    (when timer (cancel-timer timer))
                    (funcall callback answers)))
                (take (engine defaults)
                  (let ((cell (assq engine answers)))
                    (unless (or done (cdr cell))
                      (setcdr cell (list defaults))
                      (when (zerop (cl-decf left))
                        (finish))))))
      (setq timer (run-at-time drover-remote-catalog-timeout nil
                               (lambda () (drover-with-reporting "remote catalogs" (finish)))))
      (dolist (engine engines)
        (let ((function (drover-engine-property engine :defaults)))
          (if (not function)
              (progn (setcdr (assq engine answers) nil)
                     (when (zerop (cl-decf left)) (finish)))
            (condition-case nil
                (funcall function directory
                         (lambda (defaults)
                           (drover-with-reporting "remote catalogs"
                             (take engine defaults))))
              (error (when (zerop (cl-decf left)) (finish)))))))
      (when (null engines)
        (finish)))))

(defun drover-remote--new-allowed (peer action)
  "Return why PEER's access refuses ACTION, a line told PEER, or nil."
  (unless (drover-remote--allows-p (drover-remote--access peer) "new")
    (drover-remote--refuse-action peer action (format "%s has %s access, which starts no session"
                                                      (drover-remote--label peer)
                                                      (drover-remote--access peer)))))

(defun drover-remote--on-catalogs (peer message reply)
  "Tell PEER, through REPLY, what this host starts for it, as MESSAGE asks.
What it starts, by backend and engine, and this host's home; with
:local, a directory of PEER's, where its counterpart is here, as
`drover-remote--clone-map' finds it; with :directory, one of this host's,
the agents, models and defaults of its engines there.  For a peer that
may start sessions here."
  (if-let* ((why (drover-remote--new-allowed peer "list what it starts")))
      (funcall reply nil why)
    (let* ((targets (drover-remote--startable))
           (local (drover-remote--line (plist-get message :local) 1024))
           (where (and local (drover-remote--clone-map
                              peer local (drover-remote--line (plist-get message :home) 1024))))
           (asked (drover-remote--line (plist-get message :directory) 1024))
           (directory (and asked (file-name-absolute-p asked)
                           (file-name-as-directory (expand-file-name asked))))
           (answer (list :home (file-name-as-directory (expand-file-name "~"))
                         :where (or (car where) :false)
                         :exists (if (cdr where) t :false)
                         :targets (vconcat (mapcar (lambda (target)
                                                     (list :b (symbol-name (plist-get target :backend))
                                                           :e (symbol-name (plist-get target :engine))
                                                           :l (plist-get target :label)))
                                                   targets)))))
      (cond
       ((not asked) (funcall reply answer))
       ((not (and directory (file-directory-p directory)))
        (funcall reply (append answer (list :directory (or directory asked) :missing t))))
       (t
        (drover-remote--engine-defaults
         (delete-dups (mapcar (lambda (target) (plist-get target :engine)) targets))
         directory
         (lambda (answers)
           (funcall reply
                    (append answer
                            (list :directory directory
                                  :engines (vconcat (delq nil (mapcar (pcase-lambda (`(,engine . ,answer))
                                                                        (drover-with-reporting "remote catalogs"
                                                                          (drover-remote--engine-catalog
                                                                           engine directory answer)))
                                                                      answers)))))))))))))

(defun drover-remote--on-ls (peer message reply)
  "Tell PEER, through REPLY, the subdirectories of the directory MESSAGE names.
Their names alone, sorted, of the first `drover-remote-ls-limit'
entries: nothing of what files hold.  For a peer that may start
sessions here, in a directory it chooses."
  (let* ((asked (drover-remote--line (plist-get message :directory) 1024))
         (directory (and asked (file-name-absolute-p asked)
                         (file-name-as-directory (expand-file-name asked)))))
    (cond
     ((when-let* ((why (drover-remote--new-allowed peer "list directories")))
        (funcall reply nil why)
        t))
     ((not (and directory (file-directory-p directory)))
      (funcall reply (list :directory (or directory asked :false) :missing t)))
     (t
      (let* ((entries (condition-case nil
                          (directory-files directory nil nil t drover-remote-ls-limit)
                        (error nil)))
             (names (seq-filter (lambda (name)
                                  (and (not (member name '("." "..")))
                                       (<= (length name) 255)
                                       (file-directory-p (expand-file-name name directory))))
                                entries)))
        (funcall reply (list :directory directory
                             :dirs (vconcat (sort names #'string<))
                             :truncated (if (>= (length entries) drover-remote-ls-limit) t :false))))))))

(defun drover-remote--told-model (engine provider id)
  "Return this host's model PROVIDER's ID for ENGINE, as a peer names it.
This host's record of it when told to peers or remembered, else one
made from its names, its variants unknown."
  (let ((key (cons provider id)))
    (or (gethash (cons engine key) drover-remote--told-models)
        (seq-find (lambda (model) (equal (drover-model-key model) key))
                  (drover-with-reporting "remote new" (drover-remembered-models engine)))
        (drover-model-create :provider-id provider :id id :variants-known nil
                             :engines (drover-model-engines-for provider id)))))

(defun drover-remote--new-args (message)
  "Return what the request MESSAGE asks to start, checked, as a plist.
A line tells why it cannot be started here."
  (let* ((backend (drover-remote--symbol (plist-get message :b)))
         (engine (drover-remote--symbol (plist-get message :e)))
         (target (and backend engine
                      (seq-find (lambda (target) (and (eq (plist-get target :backend) backend)
                                                      (eq (plist-get target :engine) engine)))
                                (drover-remote--startable))))
         (asked (drover-remote--line (plist-get message :directory) 1024))
         (directory (and asked (file-name-absolute-p asked)
                         (file-name-as-directory (expand-file-name asked))))
         (text (plist-get message :text))
         (model (plist-get message :model))
         (agent (drover-remote--line (plist-get message :agent) 200)))
    (cond
     ((not target)
      (format "this host starts no %s session" (drover-bound-line (format "%s/%s" (plist-get message :b)
                                                                          (plist-get message :e))
                                                                  80)))
     ((not (and directory (file-directory-p directory)))
      (format "no directory %s here" (or asked "named")))
     ((not (or (memq text '(nil :false))
               (and (stringp text) (<= (length text) drover-remote-prompt-limit))))
      "a prompt too long")
     ((not (or (memq model '(nil :false))
               (and (vectorp model) (= (length model) 2)
                    (or (eq (aref model 0) :false) (drover-remote--line (aref model 0) 200))
                    (drover-remote--line (aref model 1) 200))))
      "a model of no known shape")
     (t
      (list :target target :directory directory
            :text (and (stringp text) (not (string-blank-p text)) text)
            :agent (and agent (not (string-empty-p agent)) agent)
            :model (and (vectorp model)
                        (drover-remote--told-model engine
                                                   (and (stringp (aref model 0))
                                                        (drover-remote--line (aref model 0) 200))
                                                   (drover-remote--line (aref model 1) 200)))
            :variant (drover-remote--symbol (plist-get message :variant))
            :title (let ((title (drover-remote--line (plist-get message :title))))
                     (and title (not (string-empty-p title)) title)))))))

(defun drover-remote--new-reply (reply answer)
  "Answer ANSWER, the outcome of a `new', through REPLY: a failure as an error."
  (if-let* ((why (plist-get answer :failure)))
      (funcall reply nil why)
    (funcall reply answer)))

(defun drover-remote--on-new (state peer message reply)
  "Start for PEER the session MESSAGE asks, answering through REPLY once started.
STATE is the remote layer's.  The request's identifier has it started
once, asked again after its stream ended."
  (let* ((id (plist-get message :id))
         (known (and (stringp id) (gethash id (drover-remote--state-outcomes state)))))
    (cond
     ((when-let* ((why (drover-remote--new-allowed peer "start a session")))
        (funcall reply nil why)
        t))
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "no identifier"))
     ((and known (car known)) (drover-remote--new-reply reply (cdr known)))
     (known (setcdr known (cons reply (cdr known))))
     (t
      (let ((args (drover-remote--new-args message)))
        (if (stringp args)
            (funcall reply nil (drover-remote--refuse-action peer "start a session" args))
          (drover-remote--begin-outcome state id reply)
          (drover-remote--start-new state peer id args)))))))

(defun drover-remote--new-send (session text)
  "Hand TEXT to the backend of SESSION, just started, as the prompt does.
Its :send waits for SESSION to be ready.  Return why it did not take
it, a line, or nil."
  (let ((send (drover-backend-property session :send)))
    (if (not (functionp send))
        "its backend takes no prompt"
      (condition-case err
          (save-window-excursion
            (save-current-buffer
              (drover-quietly
                (funcall send session text nil)
                nil)))
        (error (drover-error-message err))))))

(defun drover-remote--start-new (state peer id args)
  "Start for PEER the session ARGS describe, answering request ID once started.
From a timer: its front end is required, its engine's defaults fetched,
then its backend's :create starts it in no window (a Ghostel terminal
goes to spyglass's parking frame once a peer mirrors it), and its
prompt is handed to it.  STATE's outcome of ID is its key, title and
engine's identifier, or why it did not start, within
`drover-remote-new-timeout' seconds."
  (let* ((target (plist-get args :target))
         (backend (plist-get target :backend))
         (engine (plist-get target :engine))
         (directory (plist-get args :directory))
         (state-link (drover-link state))
         (peer-link (drover-link peer))
         (timer nil)
         (done nil))
    (drover-remote--event peer 'new "starting a %s in %s" (plist-get target :label) directory)
    (cl-labels ((finish (answer)
                  (unless done
                    (setq done t)
                    (when timer (cancel-timer timer))
                    (let ((state (drover-linked state-link))
                          (peer (drover-linked peer-link)))
                      (drover-remote--event peer 'new "%s"
                                            (if (plist-get answer :failure)
                                                (format "did not start a %s: %s" (plist-get target :label)
                                                        (plist-get answer :failure))
                                              (format "started %s" (plist-get answer :title))))
                      (let ((entry (gethash id (drover-remote--state-outcomes state))))
                        (unless (car entry)
                          (puthash id (cons t answer) (drover-remote--state-outcomes state))
                          (dolist (reply (cdr entry))
                            (drover-with-reporting "remote new"
                              (drover-remote--new-reply reply answer))))))))
                (fail (why)
                  (finish (list :failure (or (drover-bound-line (format "%s" why)) "failed"))))
                (started (session)
                  (drover-with-reporting "remote new"
                    (if (not (drover-session-p session))
                        (fail "its backend told no session")
                      (let* ((key (drover-session-key session))
                             (unsent (and (plist-get args :text)
                                          (drover-remote--new-send session (plist-get args :text)))))
                        (finish (list :key (vector (symbol-name (car key)) (drover-remote--wire-id (cdr key)))
                                      :title (or (drover-remote--plain (drover-session-display-title session)) "")
                                      :engine (symbol-name engine)
                                      :eid (or (drover-remote--wire-id (drover-session-engine-id session)) :false)
                                      :unsent (or unsent :false)))))))
                (create (defaults)
                  (unless done
                    (condition-case err
                        (save-window-excursion
                          (save-current-buffer
                            (drover-quietly
                              (let* ((resolved (drover-selection-resolve
                                                engine args
                                                (drover-engine-resolve engine defaults (plist-get args :agent) nil nil)
                                                (drover--engine-models engine directory) t))
                                     (failure (plist-get resolved :failure)))
                                (if failure (fail failure)
                                  (funcall (drover-backend-property backend :create)
                                           directory engine
                                           (append (plist-get resolved :options)
                                                   (list :title (plist-get args :title) :defaults defaults :hidden t))
                                           #'started
                                           (lambda (why) (drover-with-reporting "remote new" (fail why)))))))))
                      (error (fail (drover-error-message err)))))))
      (setq timer (run-at-time drover-remote-new-timeout nil
                               (lambda () (drover-with-reporting "remote new" (fail "it did not start in time")))))
      (run-at-time 0 nil
                   (lambda ()
                     (drover-with-reporting "remote new"
                       (condition-case err
                           (progn
                             (when-let* ((feature (plist-get target :feature)))
                               (require feature))
                             (if (not (and (memq backend (drover-backends))
                                           (functionp (drover-backend-property backend :create))
                                           (memq engine (drover-backend-engines backend))))
                                 (fail (format "%s starts no %s session here" (drover-backend-label backend)
                                               (drover-remote--engine-label engine)))
                               (drover-remote--engine-defaults
                                (list engine) directory
                                (lambda (answers) (create (car (alist-get engine answers)))))))
                         (error (fail (drover-error-message err))))))))))

;;;;; Here, starting sessions on peers

(defun drover-remote--new-refusal (peer)
  "Return why PEER starts no session for this host now, a line, or nil."
  (if (not peer)
      "Not a peer"
    (let ((label (drover-remote--label peer))
          (granted (or (drover-remote--peer-granted peer) 'read)))
      (cond
       ((drover-remote--viewer-p peer) (format "%s owns no sessions" label))
       ((not (eq (drover-remote--peer-state peer) 'open)) (format "%s is not connected" label))
       ((not (drover-remote--capable-p peer "new"))
        (format "%s runs an older drover, which starts no session for other hosts: update it there" label))
       ((not (drover-remote--allows-p granted "new"))
        (format "%s grants this host %s access, which starts no session" label granted))))))

(defun drover-remote--new-peers ()
  "Return the peers starting sessions for this host now."
  (seq-remove #'drover-remote--new-refusal (drover-remote--peers)))

(defun drover-remote--decode-model (data)
  "Return the `drover-model' DATA, a model a peer told, describes, or nil."
  (when-let* (((and (consp data) (keywordp (car data))))
              (id (drover-remote--line (plist-get data :id) 200))
              ((not (string-empty-p id))))
    (let ((variants (plist-get data :v))
          (engines (plist-get data :en)))
      (drover-model-create
       :provider-id (drover-remote--line (plist-get data :p) 200)
       :provider-name (drover-remote--line (plist-get data :pn) 200)
       :id id
       :name (drover-remote--line (plist-get data :n) 200)
       :variants (and (vectorp variants)
                      (delq nil (mapcar (lambda (pair)
                                          (when-let* (((and (vectorp pair) (= (length pair) 2)))
                                                      (name (drover-remote--symbol (aref pair 0))))
                                            (cons name (and (stringp (aref pair 1))
                                                            (list :effort (drover-remote--line (aref pair 1) 48))))))
                                        (seq-take variants 16))))
       :default-variant (drover-remote--symbol (plist-get data :dv))
       :variants-known (eq (plist-get data :vk) t)
       :engines (and (vectorp engines)
                     (delete-dups (delq nil (mapcar #'drover-remote--symbol (seq-take engines 8)))))))))

(defun drover-remote--decode-catalog (data)
  "Return DATA, what a peer told of the sessions it starts, checked and bounded.
A plist: :home, :where and :exists, :directory and :missing as told;
:targets, plists (:backend :engine :label); :engines, an alist (ENGINE
:agents NAMES :models MODELS :default (:agent :model :variant))."
  (when (and (consp data) (keywordp (car data)))
    (let ((targets (plist-get data :targets))
          (engines (plist-get data :engines)))
      (list :home (drover-remote--line (plist-get data :home) 1024)
            :where (drover-remote--line (plist-get data :where) 1024)
            :exists (eq (plist-get data :exists) t)
            :directory (drover-remote--line (plist-get data :directory) 1024)
            :missing (eq (plist-get data :missing) t)
            :targets (and (vectorp targets)
                          (delq nil (mapcar (lambda (target)
                                              (when-let* (((and (consp target) (keywordp (car target))))
                                                          (backend (drover-remote--symbol (plist-get target :b)))
                                                          (engine (drover-remote--symbol (plist-get target :e))))
                                                (list :backend backend :engine engine
                                                      :label (or (drover-remote--line (plist-get target :l) 80)
                                                                 (format "%s session" backend)))))
                                            (seq-take targets 32))))
            :engines (and (vectorp engines)
                          (delq nil (mapcar (lambda (entry)
                                              (when-let* (((and (consp entry) (keywordp (car entry))))
                                                          (engine (drover-remote--symbol (plist-get entry :e))))
                                                (let ((agents (plist-get entry :agents))
                                                      (models (plist-get entry :models))
                                                      (default (plist-get entry :default)))
                                                  (list engine
                                                        :agents (and (vectorp agents)
                                                                     (delete-dups
                                                                      (delq nil (mapcar (lambda (agent) (drover-remote--line agent 200))
                                                                                        (seq-take agents 100)))))
                                                        :models (and (vectorp models)
                                                                     (delq nil (mapcar #'drover-remote--decode-model
                                                                                       (seq-take models drover-remote-catalog-models))))
                                                        :default (and (consp default) (keywordp (car default))
                                                                      (list :agent (drover-remote--line (plist-get default :a) 200)
                                                                            :model (drover-remote--decode-model (plist-get default :m))
                                                                            :variant (drover-remote--symbol (plist-get default :v))))))))
                                            (seq-take engines 16))))))))

(defun drover-remote--catalog-forget (name)
  "Forget what the peer NAME told of the sessions it starts, and its directories."
  (dolist (table (list drover-remote--catalogs drover-remote--listings))
    (dolist (key (hash-table-keys table))
      (when (equal (car key) name)
        (remhash key table)))))

(defun drover-remote--catalog-ask (peer local directory callback)
  "Ask PEER what it starts, for LOCAL and DIRECTORY, then call CALLBACK.
CALLBACK takes (DATA ERROR).
See `drover-remote--catalogs'.  An answer asked already is waited for
rather than asked again."
  (let* ((name (drover-remote--peer-name peer))
         (key (list name local directory))
         (entry (gethash key drover-remote--catalogs)))
    (if (plist-get entry :waiting)
        (puthash key (plist-put (copy-sequence entry) :waiting (cons callback (plist-get entry :waiting)))
                 drover-remote--catalogs)
      (puthash key (plist-put (copy-sequence entry) :waiting (list callback)) drover-remote--catalogs)
      (drover-remote--request
       peer (list :op "catalogs" :local (or local :false)
                  :home (or (and local (drover-remote--clone-home-relative local)) :false)
                  :directory (or directory :false))
       (lambda (reply error)
         (let* ((data (and (null error) (drover-remote--decode-catalog reply)))
                (entry (gethash key drover-remote--catalogs))
                (waiting (plist-get entry :waiting)))
           (if data
               (progn
                 (puthash key (list :time (float-time) :data data) drover-remote--catalogs)
                 (unless (equal key (list name nil nil))
                   (puthash (list name nil nil)
                            (append (list :time (float-time)
                                          :data (list :targets (plist-get data :targets) :home (plist-get data :home)))
                                    (let ((waiting (plist-get (gethash (list name nil nil) drover-remote--catalogs)
                                                              :waiting)))
                                      (and waiting (list :waiting waiting))))
                            drover-remote--catalogs)))
             (puthash key (list :time (float-time) :data (plist-get entry :data)
                                :failed (or error "it told nothing"))
                      drover-remote--catalogs))
           (dolist (callback (reverse waiting))
             (drover-with-reporting "remote catalogs"
               (funcall callback data error)))))
       (+ (float-time) drover-remote-catalog-timeout drover-remote-request-timeout)
       t))))

(defun drover-remote-catalog (name &optional local directory wait)
  "Return what the peer NAME told of the sessions it starts, or nil.
Without LOCAL nor DIRECTORY, what it starts and its home; with LOCAL, a
directory of this host, where its counterpart there is; with DIRECTORY,
one of NAME's, its engines' agents, models and defaults there.  See
`drover-remote--decode-catalog'.  An answer older than
`drover-remote-catalog-ttl' is asked again, and returned meanwhile; one
missing is asked, and with WAIT, seconds, waited for that long at most,
else nil.  Nil for a peer starting no session for this host."
  (when-let* ((peer (drover-remote--peer name))
              ((not (drover-remote--new-refusal peer))))
    (let* ((key (list name local directory))
           (entry (gethash key drover-remote--catalogs))
           (age (and entry (plist-get entry :time) (- (float-time) (plist-get entry :time))))
           (fresh (and age (< age (if (plist-get entry :failed)
                                      drover-remote-catalog-failed-ttl
                                    drover-remote-catalog-ttl)))))
      (unless (or fresh (plist-get entry :waiting))
        (drover-remote--catalog-ask peer local directory #'ignore))
      (when (and wait (not (plist-get entry :data)) (not fresh))
        (drover-remote--await peer (lambda () (not (plist-get (gethash key drover-remote--catalogs) :waiting)))
                              wait))
      (plist-get (gethash key drover-remote--catalogs) :data))))

(defun drover-remote--there (peer local)
  "Return PEER's directory LOCAL, one of this host's, maps to, or nil.
PEER's :directories (THEIRS . OURS) map it, the first OURS a prefix of
it."
  (when (stringp local)
    (let ((ours (drover-remote--clone-slashed (expand-file-name local))))
      (seq-some (pcase-lambda (`(,theirs . ,here))
                  (when (and (stringp theirs) (stringp here))
                    (let ((prefix (drover-remote--clone-slashed (expand-file-name here))))
                      (and (string-prefix-p prefix ours (memq system-type '(windows-nt ms-dos cygwin darwin)))
                           (concat (drover-remote--clone-slashed theirs) (substring ours (length prefix)))))))
                (plist-get (drover-remote--peer-properties peer) :directories)))))

(defun drover-remote-default-directory (name local &optional wait)
  "Return the directory of the peer NAME a session of LOCAL starts in there.
NAME's :directories here map LOCAL, else NAME's own map it back, else
the same directory under NAME's home as LOCAL under this host's, else
NAME's home; nil when unknown.  NAME is asked, waited for WAIT seconds
at most when it has not told yet."
  (when-let* ((peer (drover-remote--peer name)))
    (or (drover-remote--there peer local)
        (let ((data (drover-remote-catalog name local nil wait)))
          (or (plist-get data :where) (plist-get data :home)))
        (plist-get (drover-remote-catalog name) :home))))

(defun drover-remote--recent-directories (peer)
  "Return the directories PEER's sessions run in, the latest active first."
  (let ((directories nil))
    (dolist (session (sort (seq-filter #'drover-session-directory
                                       (hash-table-values (drover-remote--peer-records peer)))
                           :key (lambda (session) (- (drover--time (or (drover-session-since session)
                                                                      (drover-session-updated session)))))))
      (let ((directory (file-name-as-directory (drover-session-directory session))))
        (unless (member directory directories)
          (push directory directories))))
    (seq-take (nreverse directories) 50)))

(defun drover-remote--subdirectories (peer directory)
  "Return the names of the subdirectories of PEER's DIRECTORY, as PEER lists them.
Asked and waited for a few seconds at most, the answer kept
`drover-remote-listing-ttl' seconds; nil when PEER does not answer."
  (let* ((key (cons (drover-remote--peer-name peer) directory))
         (cached (gethash key drover-remote--listings)))
    (if (and cached (< (- (float-time) (car cached)) drover-remote-listing-ttl))
        (cdr cached)
      (pcase-let* ((`(,reply ,error) (drover-remote--wait peer (list :op "ls" :directory directory) 3))
                   (dirs (and (null error) (plist-get reply :dirs)))
                   (names (and (vectorp dirs)
                               (delq nil (mapcar (lambda (name)
                                                   (and (stringp name) (<= (length name) 255)
                                                        (not (string-match-p "[/\\\0]" name))
                                                        (not (member name '("." "..")))
                                                        name))
                                                 (seq-take dirs drover-remote-ls-limit))))))
        (unless error
          (puthash key (cons (float-time) names) drover-remote--listings))
        names))))

(defvar drover-remote--directory-history nil
  "Directories of peers read, the latest first.")

(defun drover-remote--directory-table (peer recent)
  "Return a completion table of PEER's directories, RECENT ones first.
It completes the last component of a directory as file names complete,
on the subdirectories PEER lists; an input without a slash completes on
RECENT directories."
  (lambda (string predicate action)
    (let ((start (let ((position (string-match-p "[/\\][^/\\]*\\'" string)))
                   (if position (1+ position) 0))))
      (pcase action
        ('metadata '(metadata (category . drover-remote-directory)
                              (display-sort-function . identity)
                              (cycle-sort-function . identity)))
        (`(boundaries . ,suffix)
         `(boundaries ,start . ,(or (string-match-p "[/\\]" suffix) (length suffix))))
        (_
         (if (zerop start)
             (complete-with-action action recent string predicate)
           (let ((parent (substring string 0 start)))
             (completion-table-with-context
              parent (mapcar (lambda (name) (concat name "/")) (drover-remote--subdirectories peer parent))
              (substring string start) predicate action))))))))

(defun drover-remote--peer-absolute-p (directory)
  "Return non-nil when DIRECTORY, a peer's, is absolute on some system.
Rooted, as /x or \\x, under a drive, as c:/x or c:\\x, or under a home,
as ~/x: the peer takes it by its own rules, which this host's may not
be, as macOS's are not Windows's."
  (string-match-p "\\`\\(?:[/\\~]\\|[a-zA-Z]:[/\\]\\)" directory))

(defun drover-remote--peer-directory-name (directory)
  "Return DIRECTORY, a peer's, as a directory name, with a trailing slash.
Spelt as given, but for the backslashes of a name under a drive, a
Windows one, which become slashes; this host's `file-name-as-directory'
would spell it by this host's rules."
  (let ((name (if (string-match-p "\\`[a-zA-Z]:" directory)
                  (replace-regexp-in-string "\\\\" "/" directory)
                directory)))
    (if (string-suffix-p "/" name) name (concat name "/"))))

(defun drover-remote-read-directory (name prompt &optional default)
  "Read with PROMPT a directory of the peer NAME, starting from DEFAULT.
It completes on the directories NAME lists, names alone, and on those
its sessions run in.  Return it, with a trailing slash.  An absolute
name on any system is taken, NAME's being unknown here: NAME refuses
one that is not its."
  (let* ((peer (or (drover-remote--peer name) (user-error "%s is not a peer" name)))
         (recent (drover-remote--recent-directories peer))
         (directory (string-trim (completing-read prompt (drover-remote--directory-table peer recent)
                                                  nil nil default 'drover-remote--directory-history))))
    (unless (and (not (string-empty-p directory)) (drover-remote--peer-absolute-p directory))
      (user-error "%s is not a directory of %s" (if (string-empty-p directory) "Nothing" directory)
                  (drover-remote--label peer)))
    (drover-remote--peer-directory-name directory)))

(defun drover-remote--new (peer target directory text options callback)
  "Have PEER start the session TARGET describes in DIRECTORY, its, with TEXT.
OPTIONS are the start options (:agent :model :variant :title); TEXT is
nil for no prompt.  CALLBACK gets, once, what PEER answers, decoded: a
plist (:key KEY :title TITLE :engine ENGINE :eid ID :unsent WHY), or
\(:failure WHY)."
  (let* ((label (drover-remote--label peer))
         (model (plist-get options :model))
         (variant (plist-get options :variant)))
    (drover-remote--request
     peer (list :op "new" :id (drover-remote--action-id peer)
                :b (symbol-name (plist-get target :backend)) :e (symbol-name (plist-get target :engine))
                :directory directory
                :text (if (stringp text) (substring-no-properties text) :false)
                :agent (or (drover-remote--plain (plist-get options :agent)) :false)
                :model (if (drover-model-p model)
                           (vector (or (drover-model-provider-id model) :false) (drover-model-id model))
                         :false)
                :variant (if (and variant (symbolp variant)) (symbol-name variant) :false)
                :title (or (drover-remote--plain (plist-get options :title)) :false))
     (lambda (reply error)
       (funcall callback
                (let ((key (and (null error) (drover-remote--key (plist-get reply :key)))))
                  (cond
                   (error (list :failure error))
                   ((not key) (list :failure (format "%s told no session" label)))
                   (t (list :key key
                            :title (drover-remote--line (plist-get reply :title))
                            :engine (drover-remote--symbol (plist-get reply :engine))
                            :eid (drover-remote--id (plist-get reply :eid))
                            :unsent (drover-remote--line (plist-get reply :unsent))))))))
     (+ (float-time) drover-remote-new-timeout))))

(defun drover-remote-start (name target directory text options later)
  "Start on the peer NAME the session TARGET describes, in DIRECTORY, with TEXT.
TARGET is a plist (:backend :engine :label), as the peer tells it;
OPTIONS the start options (:agent :model :variant :title); TEXT a
prompt, or nil.  Waited for in the command, `drover-remote-new-timeout'
seconds at most: return what NAME answered, (:key KEY :title TITLE
:engine ENGINE :eid ID :unsent WHY), or signal a `user-error' telling
why it did not start.  \\[keyboard-quit] stops waiting: return nil,
LATER being called, from a timer, with the answer once it comes, or
\(:failure WHY)."
  (let* ((peer (or (drover-remote--peer name) (user-error "%s is not a peer" name)))
         (label (drover-remote--label peer))
         (what (format "%s on %s" (or (plist-get target :label) "session") label))
         (box (list nil nil)))
    (when-let* ((why (drover-remote--new-refusal peer)))
      (user-error "%s" why))
    (drover-remote--new peer target directory text options
                        (lambda (result)
                          (if (cadr box)
                              (run-at-time 0 nil (lambda ()
                                                   (drover-with-reporting "remote new"
                                                     (funcall later result))))
                            (setcar box result))))
    (if (drover-remote--clone-wait box (lambda () (format "Starting a new %s... (C-g: go on in the background)" what)))
        (let ((result (car box)))
          (drover-message "")
          (when-let* ((why (plist-get result :failure)))
            (user-error "No new %s: %s" what why))
          result)
      (setf (cadr box) t)
      (drover-message "Starting a new %s in the background" what)
      nil)))

(defun drover-remote--started-record (peer result)
  "Return PEER's record of the session RESULT, its `new' answer, names, or nil."
  (let ((key (plist-get result :key))
        (engine (plist-get result :engine))
        (eid (plist-get result :eid)))
    (or (drover-remote--peer-session peer key)
        (and engine eid
             (seq-find (lambda (record)
                         (and (eq (drover-session-engine record) engine)
                              (equal (drover-session-engine-id record) eid)))
                       (hash-table-values (drover-remote--peer-records peer)))))))

(defun drover-remote-open-started (name result &optional hidden)
  "Open the session the peer NAME started, as RESULT, its answer, names it.
Once NAME's records list it, waited for in the command,
`drover-remote-open-timeout' seconds at most (see
`drover-remote--await'): an OpenCode session in a
full buffer through NAME, a terminal or a shell mirrored; with HIDDEN
non-nil, it is not opened.  Return its record, or nil when not listed
in time, which the echo area tells."
  (let* ((peer (or (drover-remote--peer name) (user-error "%s is not a peer" name)))
         (record nil))
    (drover-remote--await peer (lambda () (setq record (drover-remote--started-record peer result)))
                          drover-remote-open-timeout)
    (when-let* ((why (plist-get result :unsent)))
      (drover-warn "%s started %s, which did not take the prompt: %s; it is in the kill ring"
                   (drover-remote--label peer) (or (plist-get result :title) "a session") why))
    (if (not record)
        (progn (drover-message "%s started %s; it opens from the list once listed"
                               (drover-remote--label peer) (or (plist-get result :title) "a session"))
               nil)
      (drover-changed-now)
      (unless hidden
        (drover-open (or (drover-index-session (drover-session-key record)) record)))
      record)))

(defun drover-remote--prompt-targets (&optional wait)
  "Return the new sessions of other hosts the prompt offers, as plists.
Each is (:host NAME :host-label LABEL :backend :engine :label), of the
peers starting sessions for this host, in the order configured, as each
told; one that has not told yet is asked, and waited for WAIT seconds."
  (mapcan (lambda (peer)
            (let ((name (drover-remote--peer-name peer)))
              (mapcar (lambda (target)
                        (append (list :host name :host-label (drover-remote--label peer)) target))
                      (plist-get (drover-remote-catalog name nil nil wait) :targets))))
          (drover-remote--new-peers)))

(defun drover-remote--prompt-hosts ()
  "Return the peers starting sessions for this host, as (NAME . LABEL)."
  (mapcar (lambda (peer) (cons (drover-remote--peer-name peer) (drover-remote--label peer)))
          (drover-remote--new-peers)))

(defun drover-remote--prompt-label (name)
  "Return how views name the peer NAME."
  (if-let* ((peer (drover-remote--peer name)))
      (drover-remote--label peer)
    name))

(defconst drover-remote--prompt-functions
  (list :targets #'drover-remote--prompt-targets
        :hosts #'drover-remote--prompt-hosts
        :label #'drover-remote--prompt-label
        :catalog #'drover-remote-catalog
        :default-directory #'drover-remote-default-directory
        :read-directory #'drover-remote-read-directory
        :start #'drover-remote-start
        :open #'drover-remote-open-started)
  "How the prompt starts sessions on other hosts.
See `drover-prompt-remote-functions'.")

(defun drover-remote--report-later (title)
  "Return the function reporting a session of TITLE started in the background."
  (lambda (result)
    (if-let* ((why (plist-get result :failure)))
        (drover-warn "%s did not start: %s" title why)
      (drover-message "%s started: %s" title (or (plist-get result :title) "")))))

(defun drover-remote-new-session (&optional session)
  "Start a session, without a prompt, on a host chosen among this one and peers.
The peers are those starting sessions for this host; the host of
SESSION, the session at point, comes first.  On this host, as
`drover-new-session' does; on a peer, of a harness it starts, in a
directory read there (see `drover-remote-read-directory'), SESSION's
when of that peer, opened here once started.  For
`drover-new-session-elsewhere-function'."
  (interactive (list (drover-session-at-point t)))
  (let* ((hosts (drover-remote--prompt-hosts))
         (here (format "%s (this host)" drover-host))
         (candidates (append (mapcar (lambda (host) (cons (cdr host) (car host))) hosts)
                             (list (cons here nil))))
         (session-host (and session (drover-session-host session) (assoc (drover-session-host session) hosts)))
         (default (if session-host (cdr session-host) here))
         (choice (completing-read (format-prompt "Start a session on" default)
                                  (mapcar #'car candidates) nil t nil nil default))
         (name (cdr (assoc choice candidates))))
    (if (not name)
        (let ((current-prefix-arg nil))
          (call-interactively #'drover-new-session))
      (let* ((label (drover-remote--prompt-label name))
             (targets (or (plist-get (drover-remote-catalog name nil nil drover-remote-open-timeout) :targets)
                          (user-error "%s tells no session it starts" label)))
             (labels (mapcar (lambda (target) (cons (plist-get target :label) target)) targets))
             (target (cdr (assoc (completing-read (format "New session on %s: " label) labels nil t)
                                 labels)))
             (directory (drover-remote-read-directory
                         name (format "Directory on %s: " label)
                         (if (and session (equal (drover-session-host session) name)
                                  (drover-session-directory session))
                             (file-name-as-directory (drover-session-directory session))
                           (drover-remote-default-directory name default-directory drover-remote-open-timeout))))
             (what (format "%s on %s" (plist-get target :label) label))
             (result (drover-remote-start name target directory nil nil (drover-remote--report-later what))))
        (when result
          (drover-remote-open-started name result))))))

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
   ((equal type "chg") (drover-remote--on-prefetch message '(changes)))
   ((equal type "fetched")
    (when (eq connection (drover-remote--peer-connection peer))
      (drover-remote--fetched peer message)))
   ((member type '("snap" "up"))
    (when (eq connection (drover-remote--peer-connection peer))
      (drover-remote--receive peer message)))
   ((equal type "grant")
    (let ((access (plist-get message :access)))
      (setf (drover-remote--peer-granted peer)
            (and (member access '("read" "act" "full")) (intern access)))
      ;; What it starts, asked now for the prompt's targets.
      (drover-remote--catalog-forget (drover-remote--peer-name peer))
      (when (and (eq connection (drover-remote--peer-connection peer))
                 (not (drover-remote--new-refusal peer)))
        (drover-remote--catalog-ask peer nil nil #'ignore))))
   ((equal type "bye") (drover-remote--on-bye peer connection message))
   ((equal type "cvc") (drover-remote--cv-stop connection (plist-get message :id) nil))
   ((member type '("cvd" "cve"))
    (drover-run-hook-reporting 'drover-remote-conversation-functions peer message))
   ((equal type "jobs")
    (drover-run-hook-reporting 'drover-remote-jobs-functions peer message))
   ((equal type "fg")
    (when (featurep 'drover-remote-push)
      (drover-remote-push-foreground peer (eq (plist-get message :on) t))))
   ((equal type "unsent") (drover-remote--on-unsent peer message))
   ((equal type "note")
    (when-let* ((text (drover-remote--line (plist-get message :text) 300)))
      (drover-remote--event peer 'note "%s" text)
      (drover-message "%s" text)))
   ((equal type "ocd") (drover-remote--on-oc-data message))
   ((equal type "oce") (drover-remote--on-oc-end message))
   ((equal type "occ") (drover-remote--on-oc-cancel connection message))
   ((equal type "srm") (drover-remote--on-search-matches peer message))
   ((equal type "sre") (drover-remote--on-search-end peer message))
   ((equal type "src") (drover-remote--on-search-stop connection message))
   ((equal type "clone-end")
    (let ((xid (plist-get message :x)))
      (when-let* (((stringp xid))
                  (entry (gethash xid drover-remote--exports))
                  ((equal (plist-get entry :peer) (drover-remote--peer-name peer))))
        (drover-remote--export-drop xid))))))

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

(defun drover-remote--on-prefetch (message &optional kinds)
  "Fetch what the records of the sessions MESSAGE names lack, for a peer.
KINDS are what to fetch, as `drover-prefetch' takes them: the details
their backends fetch, for a message `pre', what they changed for a
message `chg'."
  (let ((sessions nil))
    (seq-doseq (wire (seq-take (or (plist-get message :keys) []) 500))
      (when-let* ((key (drover-remote--key wire))
                  (served (drover-remote--served key)))
        (push (car served) sessions)))
    (when sessions
      (drover-prefetch (nreverse sessions) (or kinds '(details))))))

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
        ("mirror"
         (if (drover-remote--mirrors-p)
             (drover-remote--on-mirror state peer connection message reply)
           (funcall reply nil "this host's spyglass shares no terminal: update it")))
        ("requests" (drover-remote--on-requests peer key reply))
        ("oc" (drover-remote--on-oc peer message reply))
        ("ocs" (drover-remote--on-oc-stream peer connection message reply))
        ("search" (drover-remote--on-search peer connection message reply))
        ("clone-export" (drover-remote--on-clone-export peer key reply))
        ("clone-piece" (drover-remote--on-clone-piece peer message reply))
        ("clone-where" (drover-remote--on-clone-where peer message reply))
        ("clone" (drover-remote--on-clone peer message reply))
        ("catalogs" (drover-remote--on-catalogs peer message reply))
        ("ls" (drover-remote--on-ls peer message reply))
        ("new" (drover-remote--on-new state peer message reply))
        ("usage" (drover-remote--on-usage message reply))
        ("task" (drover-remote--on-task peer message reply))
        ("opts" (drover-remote--on-opts peer key reply))
        ("jobs" (drover-remote--on-jobs connection message reply))
        ("triggers" (drover-remote--on-triggers reply))
        ("schedule" (drover-remote--on-schedule state peer key message reply))
        ("unschedule" (drover-remote--on-unschedule peer message reply))
        ("retry" (drover-remote--on-retry peer message reply))
        ("cv" (drover-remote--on-cv key message reply))
        ("cvs" (drover-remote--on-cvs peer connection key message reply))
        ("dirs" (drover-remote--on-dirs peer message reply))
        ("push" (if (featurep 'drover-remote-push)
                    (drover-remote-push-on-request peer message reply)
                  (funcall reply nil "this host pushes nothing")))
        ("watch" (if (featurep 'drover-remote-push)
                     (drover-remote-push-on-watch peer message reply)
                   (funcall reply nil "this host pushes nothing")))
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
        ("review" (drover-remote--on-review served message reply))
        ("preview"
         (if (not served)
             (funcall reply nil "no such session here")
           (condition-case err
               (drover-preview (car served)
                               (lambda (text)
                                 (funcall reply (list :text (or (drover-remote--plain text) :false)))))
             (error (funcall reply nil (drover-error-message err))))))
        (_ (funcall reply nil "unknown request")))))))

;;;; Tasks

(declare-function drover-task-remote-answer "drover-task" (peer access message reply))

(defun drover-remote--on-task (peer message reply)
  "Answer PEER's MESSAGE about a task of this host, through REPLY.
This host starts and finishes its own tasks (see
`drover-task-remote-answer'), as PEER's access allows."
  (if (not (locate-library "drover-task"))
      (funcall reply nil "this host has no tasks")
    (require 'drover-task)
    (condition-case err
        (drover-task-remote-answer (drover-remote--peer-name peer) (drover-remote--access peer) message
                                   (lambda (value &optional why)
                                     (drover-with-reporting "remote task"
                                       (when why
                                         (drover-remote--refuse-action peer "act on a task" why))
                                       (funcall reply value why))))
      (error (funcall reply nil (drover-error-message err))))))

(defun drover-remote-ask-task (name request callback &optional seconds)
  "Ask the peer NAME REQUEST about one of its tasks, for CALLBACK.
REQUEST is a plist, its :kind \"start\" or \"finish\" (see
`drover-task-remote-answer'); CALLBACK gets (REPLY ERROR) once, within
SECONDS, by default `drover-remote-request-timeout'.  It is asked once,
not again after a stream dropped: a task is not started twice."
  (let ((peer (drover-remote--peer name)))
    (cond
     ((not peer) (funcall callback nil (format "%s is not a peer" name)))
     ((not (eq (drover-remote--peer-state peer) 'open))
      (funcall callback nil (format "%s is not connected" (drover-remote--label peer))))
     ((not (drover-remote--capable-p peer "task"))
      (funcall callback nil (format "%s runs an older drover, which has no tasks: update it there"
                                    (drover-remote--label peer))))
     (t (drover-remote--request peer (append (list :op "task") request) callback
                                (+ (float-time) (or seconds drover-remote-request-timeout)) t)))))

;;;; Usage

(declare-function drover-usage-remote-answer "drover-usage" (message))

(defun drover-remote--on-usage (message reply)
  "Answer a peer's MESSAGE asking this host's totals of usage, through REPLY.
This host totals its own (see `drover-usage-remote-answer')."
  (require 'drover-usage)
  (condition-case err
      (funcall reply (drover-usage-remote-answer message))
    (error (funcall reply nil (drover-error-message err)))))

(defun drover-remote-usage-peers ()
  "Return the names of the peers connected whose drover sends its usage."
  (mapcar #'drover-remote--peer-name
          (seq-filter (lambda (peer)
                        (and (eq (drover-remote--peer-state peer) 'open)
                             (drover-remote--capable-p peer "usage")))
                      (drover-remote--peers))))

(defun drover-remote-ask-usage (name request callback)
  "Ask the peer NAME for its totals of usage REQUEST tells, for CALLBACK.
REQUEST is a plist of :period and :by (see `drover-usage-remote-answer');
CALLBACK gets (REPLY ERROR) once."
  (if-let* ((peer (drover-remote--peer name)))
      (drover-remote--request peer (append (list :op "usage") request) callback nil t)
    (funcall callback nil (format "%s is not a peer" name))))

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
             (drover-remote-open session))
     ;; A mirror of its terminal, or its OpenCode buffer, while open here.
     :buffer (lambda (session)
               (when-let* ((peer (drover-remote--peer name)))
                 (or (drover-remote--mirror-buffer peer (drover-session-id session))
                     (drover-remote--oc-buffer name (drover-session-id session)))))
     :recap (lambda (session callback)
              (if-let* ((peer (drover-remote--peer name)))
                  (drover-remote--recap peer session callback)
                (funcall callback (list :failure (format "%s is not a peer" name)))))
     :preview (lambda (session callback)
                (if-let* ((peer (drover-remote--peer name)))
                    (drover-remote--preview peer session callback)
                  (funcall callback nil)))
     ;; The changes of a session, computed by its owner.
     :review (lambda (session scope callback)
               (if-let* ((peer (drover-remote--peer name)))
                   (drover-remote--review peer session scope callback)
                 (funcall callback (list :failure (format "%s is not a peer" name)))))
     :history (lambda (callback)
                (when-let* ((peer (drover-remote--peer name)))
                  (drover-remote--fetch peer "history" callback)))
     :archived (lambda (callback)
                 (when-let* ((peer (drover-remote--peer name)))
                   (drover-remote--fetch peer "archived" callback)))
     :prefetch (lambda (sessions)
                 (when-let* ((peer (drover-remote--peer name)))
                   (drover-remote--prefetch peer sessions)))
     ;; What they changed, read by their owner, which their records tell.
     :prefetch-changes (lambda (sessions)
                         (when-let* ((peer (drover-remote--peer name)))
                           (drover-remote--prefetch peer sessions 'changes)))
     ;; What was said, searched there by its own backends.
     :search (lambda (query scope callback) (drover-remote--search name query scope callback))
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
     ;; The owner wraps it up, and tells how it ended.
     :wrap-up (drover-remote--confirming name "wrap-up")
     :wrap-up-confirms t
     :rename (lambda (session title) (drover-remote--rename name session title))
     :respond (lambda (session) (drover-remote--respond name session))
     :send (lambda (session text token &optional attachments)
             (drover-remote--send-prompt name session text token attachments))
     ;; Options the owner applies, before a prompt.
     :apply-options (lambda (session options then &optional else)
                      (drover-remote--apply-options-there name session options then else))
     :settable (lambda (session) (drover-remote--settable-there name session))
     ;; Without asking, as agents act (see `drover-anvil').
     :submit (lambda (session text then else) (drover-remote--submit name session text then else))
     :requests (lambda (session callback) (drover-remote--fetch-requests name session callback))
     :answer (lambda (session id answer then else)
               (drover-remote--answer-request name session id answer then else)))))

(defun drover-remote--add-peer (state name properties &optional ephemeral)
  "Add the peer NAME with PROPERTIES to STATE, registering its backend.
EPHEMERAL non-nil for one accepted without configuration."
  (let ((peer (drover-remote--peer-make :name name :properties properties :ephemeral ephemeral
                                        :backend (drover-remote--backend-name name)
                                        :records (make-hash-table :test #'equal)
                                        :ops (make-hash-table :test #'equal)
                                        :settable (make-hash-table :test #'equal)
                                        :mirrors (make-hash-table :test #'equal))))
    (puthash name peer (drover-remote--state-peers state))
    (setf (drover-remote--state-order state)
          (append (delete name (drover-remote--state-order state)) (list name)))
    ;; A viewer has no sessions to list.
    (unless (plist-get properties :viewer)
      (drover-remote--register peer))
    (drover-remote--adopt-mirrors peer)
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
  (when (drover-remote--peer-settable peer)
    (clrhash (drover-remote--peer-settable peer)))
  (drover-remote--catalog-forget (drover-remote--peer-name peer))
  (drover-remote--fail-outbox peer (format "%s closed: %s" (drover-remote--label peer) why))
  (drover-remote--oc-close peer)
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
                   (cond
                    ((not (plist-get properties :viewer)) (drover-remote--register peer))
                    ((memq (drover-remote--peer-backend peer) (drover-backends))
                     ;; Configured a viewer now: what it listed goes.
                     (drover-remote--close peer "a viewer now")
                     (drover-unregister-backend (drover-remote--peer-backend peer)))))
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
         (attempts (and dialed (ignore-errors (spyglass-connection-attempts dialed))))
         (next (and dialed (fboundp 'spyglass-connection-next-attempt)
                    (ignore-errors (spyglass-connection-next-attempt dialed)))))
    (drover-bound-line
     (concat (and (drover-remote--viewer-p peer) "viewer, ")
             (pcase state
               ('open "connected")
               ('connecting "connecting")
               ('reconnecting "reconnecting")
               ('closed "closed")
               (_ (if (plist-get (drover-remote--peer-properties peer) :dial)
                      "standing by"
                    "waiting for it to connect")))
             (and attempts (> attempts 0) (format ", attempt %d" attempts))
             (and next (not (eq state 'open))
                  (format ", next in %s" (drover-remote--duration next)))
             (and (> heard 0) (not (eq state 'open))
                  (format ", last heard from %s ago" (opencode-format-age heard now)))
             (and (drover-remote--peer-error peer) (not (eq state 'open))
                  (concat ": " (drover-remote--peer-error peer)))))))

(defun drover-remote--duration (seconds)
  "Return SECONDS, a duration, on a few characters: 4s, 2m 30s, 1h 5m."
  (let ((seconds (max 0 (round seconds))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm %02ds" (/ seconds 60) (% seconds 60)))
          (t (format "%dh %02dm" (/ seconds 3600) (/ (% seconds 3600) 60))))))

(defun drover-remote--host-label (host)
  "Return how views name HOST, for `drover-host-label-function'."
  (when-let* ((peer (and host (or (drover-remote--peer host) (drover-remote--find-peer host)))))
    (drover-remote--label peer)))

(defun drover-remote--host-state (host)
  "Return how HOST stands when its sessions are stale, else nil.
For `drover-host-state-function'."
  (when-let* ((peer (drover-remote--peer host))
              ((drover-remote--peer-stale peer)))
    (drover-remote--state-line peer)))

(defun drover-remote--host-names ()
  "Return the names of the peers, in order, for `drover-hosts-function'.
Viewers, owning no sessions, are left out: no group, column nor filter."
  (when-let* ((state (drover-remote--current)))
    (seq-remove (lambda (name)
                  (when-let* ((peer (gethash name (drover-remote--state-peers state))))
                    (drover-remote--viewer-p peer)))
                (drover-remote--state-order state))))

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
            (when-let* (((featurep 'drover-remote-push))
                        (line (drover-remote-push-status peer)))
              (insert (format "  %s\n" line)))
            (let ((mirrors nil))
              (maphash (lambda (key _buffer)
                         (when-let* ((buffer (drover-remote--mirror-buffer peer key)))
                           (push (format "%s (%s)" (buffer-name buffer) (spyglass-mirror-state buffer)) mirrors)))
                       (copy-hash-table (drover-remote--peer-mirrors peer)))
              (when mirrors
                (insert (format "  mirrored here: %s\n" (string-join (nreverse mirrors) ", ")))))
            (let ((shared nil))
              (maphash (lambda (id entry)
                         (when (rassoc (drover-remote--peer-name peer) (plist-get entry :viewers))
                           (push id shared)))
                       (drover-remote--state-shares state))
              (when shared
                (insert (format "  terminals of this host it may mirror: %s\n"
                                (string-join (sort shared #'string<) ", ")))))
            (insert (format "  %s\n" (or (drover-remote--refusal peer)
                                         (format "takes actions, granting this host %s access"
                                                 (drover-remote--peer-granted peer)))))
            (when-let* ((error (drover-remote--peer-error peer)))
              (insert (format "  last error, %s ago: %s\n"
                              (opencode-format-age (or (drover-remote--peer-error-time peer) now) now)
                              error)))
            (when-let* ((history (drover-remote--peer-history peer)))
              (insert "  history, newest first:\n")
              (dolist (event (seq-take history 30))
                (pcase-let ((`(,time ,kind ,text ,count ,first) event))
                  (insert (format "    %s %-9s %s%s\n" (format-time-string "%H:%M:%S" time) kind text
                                  (if (and count (> count 1))
                                      (format " (%d times since %s)" count
                                              (format-time-string "%H:%M:%S" first))
                                    ""))))))
            (insert "\n")))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun drover-remote-check ()
  "Check that each peer this host dials is reachable, and tell what fails.
SSH runs as the connection runs it, without a prompt (BatchMode), and
reads spyglass's registration file there.  A peer backing off, still
unreachable, is tried again at once, its delays starting over (see
`drover-remote-backoff')."
  (interactive)
  (dolist (peer (drover-remote--peers))
    (drover-remote--retry-now peer "checked by the user")
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

(defvar-local drover-remote--note nil
  "Why the current buffer shows a session of a peer as its host tells it, or nil.
Rather than mirror its terminal: a line.")

(defvar-local drover-remote--mirror-kind nil
  "What the current buffer mirrors of a session of a peer, \"terminal\" or \"shell\".
Nil in a buffer that mirrors none.")

(defvar-local drover-remote--seen-sent 0
  "When the session the current mirror shows was last marked seen from here.")

(defvar-local drover-remote--reopens 0
  "Times the current mirror was opened anew since it was last live.")

(defvar-local drover-remote--killed-there nil
  "Non-nil once the owner killed the terminal the current mirror showed.
Its reason (\"killed\") then makes way for a line saying so.")

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

(defun drover-remote-show (session &optional again note)
  "Show SESSION, a session of another host, as that host tells it, read only.
What it is, its last reply, Markdown fontified as OpenCode's buffers
show it, its last prompt and the files its last turn changed, fetched
through its host.  Its turn ended unseen is marked seen there, as
viewing it there would.  AGAIN non-nil shows it anew in the current
window, without taking another.  NOTE, a line, tells why its terminal
is not mirrored instead; shown anew, the note stays."
  (let ((buffer (drover-remote--show-buffer session again note)))
    (unless again
      (pop-to-buffer-same-window buffer))
    buffer))

(defun drover-remote--show-buffer (session &optional again note)
  "Return the buffer showing SESSION, of another host, as its host tells it.
Made or shown anew, its details fetched through its host, but displayed
in no window: see `drover-remote-show' for AGAIN and NOTE."
  (let* ((host (drover-session-host session))
         (peer (or (drover-remote--peer host) (user-error "No peer %s" host)))
         (buffer (get-buffer-create (format "*drover %s: %s*" (drover-remote--label peer)
                                            (drover-bound-line (drover-session-display-title session) 60)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'drover-remote-session-mode)
        (drover-remote-session-mode))
      (setq drover-remote--shown (cons (drover-remote--peer-name peer) (drover-session-id session)))
      (unless again
        (setq drover-remote--note note))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (drover-remote--details peer session))
        (when drover-remote--note
          (insert (drover-remote--field "Terminal" (drover-face drover-remote--note 'drover-dim))))
        (let ((preview-at (point-marker))
              (recap-at (point-marker)))
          (set-marker-insertion-type recap-at t)
          (cl-flet ((put (marker text)
                      (when (and (buffer-live-p buffer) text)
                        (with-current-buffer buffer
                          (let ((inhibit-read-only t))
                            (save-excursion (goto-char marker) (insert text))
                            (set-buffer-modified-p nil))))))
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
        (set-buffer-modified-p nil)
        (goto-char (point-min))))
    (drover-remote--seen-here peer session)
    buffer))

(defun drover-remote--seen-here (peer session)
  "Mark SESSION of PEER seen there if its turn ended unseen."
  (when (and (memq (drover-session-status session) '(done failed))
             (not (drover-remote--refusal peer session "mark-seen")))
    (drover-remote--act (drover-remote--peer-name peer) "mark-seen" session nil #'ignore
                        (let ((link (drover-link peer)))
                          (lambda (message)
                            (drover-remote--event (drover-linked link) 'error "marking seen: %s" message))))))

;;;; Mirrors of a peer's terminals

(defun drover-remote--mirror-p (buffer)
  "Return non-nil when BUFFER mirrors the terminal of a peer's session.
Its mode is read first: reading a variable in a buffer where it is not
local searches every variable local there, hundreds in an Org buffer,
and the lists look for the mirror of every session of a peer they draw."
  (and (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'spyglass-mirror-mode)
       (buffer-local-value 'drover-remote--mirror-kind buffer)
       (local-variable-p 'spyglass--mirror buffer)))

(defun drover-remote--mirror-buffers (name key)
  "Return the live buffers mirroring the peer NAME's session KEY, newest first.
Found by what they show (`drover-remote--shown'), which they keep
whatever remote layer made them: a layer made anew, as reloading a
drover whose structures changed makes it, knows none of them."
  (seq-filter (lambda (buffer)
                (and (drover-remote--mirror-p buffer)
                     (equal (buffer-local-value 'drover-remote--shown buffer) (cons name key))))
              (buffer-list)))

(defun drover-remote--mirror-buffer (peer key)
  "Return the live buffer mirroring the terminal of PEER's session KEY, or nil.
The one PEER holds, else one of the buffers mirroring it (see
`drover-remote--mirror-buffers'), which PEER adopts: a live mirror
first.  One session has one mirror: other buffers of it are
duplicates, which `drover-remote--adopt-mirrors' closes."
  (let ((buffer (gethash key (drover-remote--peer-mirrors peer))))
    (if (buffer-live-p buffer)
        buffer
      (remhash key (drover-remote--peer-mirrors peer))
      (let* ((buffers (drover-remote--mirror-buffers (drover-remote--peer-name peer) key))
             (buffer (or (seq-find (lambda (buffer) (not (eq (spyglass-mirror-state buffer) 'closed)))
                                   buffers)
                         (car buffers))))
        (when buffer
          (puthash key buffer (drover-remote--peer-mirrors peer)))
        buffer))))

(defun drover-remote--adopt-mirrors (peer)
  "Have PEER hold the buffers mirroring its sessions, made before it was.
As a remote layer is made anew, its peers knowing no mirror: each
session's mirror is adopted, a live one first, and opened anew if it
closed meanwhile; another buffer of the same session, closed and shown
in no window, is killed, a duplicate, which a remote layer that knew
none of its mirrors made as the session was opened again."
  (let ((name (drover-remote--peer-name peer))
        (keys nil))
    (dolist (buffer (buffer-list))
      (when (drover-remote--mirror-p buffer)
        (let ((shown (buffer-local-value 'drover-remote--shown buffer)))
          (when (equal (car-safe shown) name)
            (cl-pushnew (cdr shown) keys :test #'equal)))))
    (dolist (key keys)
      (let ((kept (drover-remote--mirror-buffer peer key)))
        (dolist (other (drover-remote--mirror-buffers name key))
          (unless (or (eq other kept)
                      (not (eq (spyglass-mirror-state other) 'closed))
                      (get-buffer-window other t))
            (drover-remote--event peer 'mirror "closed %s, a duplicate of %s"
                                  (buffer-name other) (buffer-name kept))
            (drover-kill-buffer other)))))
    (when keys
      (drover-remote--reopen-mirrors peer))))

(defun drover-remote--why-no-mirror (peer session)
  "Return why SESSION of PEER opens as its host tells it, not mirrored, or nil.
A line: an OpenCode session that cannot open in a full buffer here (see
`drover-remote--why-no-opencode'); a peer not connected, or running a
drover or spyglass that shares no terminal; a session archived."
  (let ((label (drover-remote--label peer)))
    (cond
     ((eq (drover-session-origin session) 'opencode)
      (or (drover-remote--why-no-opencode peer)
          (format "An OpenCode session of %s, which opens in an OpenCode buffer" label)))
     ((drover-session-archived-p session) "Archived: it runs no terminal")
     ((not (eq (drover-remote--peer-state peer) 'open))
      (format "%s is not connected: its terminal opens once it is" label))
     ((not (drover-remote--mirrors-p))
      "This host's spyglass mirrors no terminal of drover's: update it")
     ((not (drover-remote--capable-p peer "mirror"))
      (format "%s runs an older drover, which shares no terminal: update it there" label)))))

(defun drover-remote-open (session)
  "Open SESSION, of another host: its terminal mirrored, else its view.
The proxy's :open.  An OpenCode session opens in a full opencode.el
buffer, talking to its owner's service through the owner (see
`drover-remote--open-opencode').  The owner shares its session's
terminal or shell
with this host, which mirrors it in a buffer taking input as the
owner's access allows, sized by the window showing it while the owner
shows it in none; a conversation without one, Claude Code's or
Codex's, is resumed there first, as opening it there would, when this
host may act on it.  Otherwise it shows as its host tells it (see
`drover-remote-show'), saying why.  Killing the mirror leaves the
session running there."
  (let* ((peer (or (drover-remote--peer (drover-session-host session))
                   (user-error "No peer %s" (drover-session-host session))))
         (buffer (drover-remote--mirror-buffer peer (drover-session-id session))))
    (if (and (eq (drover-session-origin session) 'opencode)
             (or (drover-remote--oc-buffer (drover-remote--peer-name peer) (drover-session-id session))
                 (not (drover-remote--why-no-opencode peer))))
        (drover-remote--open-opencode peer session)
      (drover-remote--open-other peer session buffer))))

(defun drover-remote--open-other (peer session buffer)
  "Open SESSION of PEER, not opened in opencode.el: mirrored, else its view.
BUFFER is its mirror, if any.  See `drover-remote-open'."
  (cond
   ((and buffer (not (eq (spyglass-mirror-state buffer) 'closed)))
    (pop-to-buffer-same-window buffer)
    (drover-remote--seen-here peer session)
    buffer)
   ((when-let* ((waiting (drover-remote--opening-buffer (drover-remote--peer-name peer)
                                                        (drover-session-id session))))
      (pop-to-buffer-same-window waiting)
      waiting))
   ((when-let* ((why (drover-remote--why-no-mirror peer session)))
      (drover-remote-show session nil why)))
   (t (drover-remote--open-mirror peer session buffer))))

(defvar-local drover-remote--opening nil
  "In a buffer waiting for a peer to share a session's terminal, (NAME . KEY).
NAME is the peer's, KEY its key of the session.")

(define-derived-mode drover-remote-opening-mode special-mode "Drover remote"
  "A terminal of another host being opened.
Its mirror takes the windows of this buffer once its host shares it;
killing the buffer gives up opening it.  The keys of drover's sessions
act on its session through its host."
  (setq-local drover-session-at-point-function #'drover-remote--shown-session))

(defun drover-remote--opening-buffer (name key)
  "Return the buffer waiting for the peer NAME to share session KEY's terminal."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'drover-remote--opening buffer) (cons name key)))
            (buffer-list)))

(defun drover-remote--open-mirror (peer session buffer)
  "Ask PEER for SESSION's terminal and mirror it, in BUFFER if one closed.
The command waits for nothing: BUFFER, else a buffer named as the
mirror is, shows at once what is awaited, and is returned.  PEER's
answer, given within `drover-remote-open-timeout' seconds, or
`drover-remote-resume-timeout' when PEER may resume the session first,
puts the mirror in the windows showing it, else the session as its host
tells it, saying why it has no terminal (see `drover-remote-show').
Killing the waiting buffer meanwhile gives up opening it, PEER
unsharing it later as for any mirror closed."
  (let* ((name (drover-remote--peer-name peer))
         (key (drover-session-id session))
         (label (drover-remote--label peer))
         (title (drover-bound-line (drover-session-display-title session) 60))
         (resume (drover-remote--allows-p (or (drover-remote--peer-granted peer) 'read) "resume"))
         (reuse (and buffer (buffer-live-p buffer) buffer))
         (waiting (or reuse (generate-new-buffer (format "*%s@%s*" title label))))
         (link (drover-link peer))
         (asked (format "Waiting for %s to share the terminal of %s%s..." label title
                        (if resume ", which it resumes first if none runs it" ""))))
    (if reuse
        (drover-remote--mirror-closed-why reuse asked)
      (with-current-buffer waiting
        (drover-remote-opening-mode)
        (setq drover-remote--shown (cons name key))
        (let ((inhibit-read-only t))
          (insert (drover-face asked 'drover-dim) "\n"))
        (set-buffer-modified-p nil)))
    (with-current-buffer waiting
      (setq drover-remote--opening (cons name key)))
    (drover-remote--event peer 'mirror "asking for the terminal of %s" title)
    ;; Shown first: an answer coming at once, as a stream found gone
    ;; gives, takes the windows it shows in.
    (pop-to-buffer-same-window waiting)
    (drover-remote--request
     peer (list :op "mirror" :key (drover-remote--wire-key session)
                :resume (if resume t :false)
                :id (drover-remote--action-id peer))
     (lambda (reply error)
       (drover-with-reporting "remote mirror"
         (when (buffer-live-p waiting)
           (with-current-buffer waiting
             (setq drover-remote--opening nil))
           (when-let* ((peer (drover-linked link)))
             (drover-remote--opened
              peer (or (drover-remote--peer-session peer key) session) waiting reuse
              (cond
               (error (format "Its terminal did not open: %s" error))
               ((stringp (plist-get reply :share)) reply)
               (t (format "%s: %s%s" label
                          (or (drover-remote--line (plist-get reply :none) 200) "it has no terminal")
                          (if (and (not resume) (eq (plist-get reply :resumable) t))
                              (format "; %s grants this host %s access, which resumes nothing" label
                                      (or (drover-remote--peer-granted peer) "read"))
                            "")))))))))
     (+ (float-time) (if resume drover-remote-resume-timeout drover-remote-open-timeout))
     t)
    (window-buffer (selected-window))))

(defun drover-remote--opened (peer session waiting reuse answer)
  "Show in the windows of WAITING what opening SESSION's terminal gave.
PEER is SESSION's host; ANSWER its answer telling the share to mirror,
a plist, else why there is none, a line, shown with the session as its
host tells it.  WAITING is the buffer that waited: REUSE, a mirror that
closed, opened anew in place, else a buffer that goes once replaced."
  (let* ((windows (get-buffer-window-list waiting nil t))
         (shown (if (stringp answer)
                    (drover-remote--show-buffer session nil answer)
                  (unless reuse
                    ;; The mirror takes the name the waiting buffer had.
                    (with-current-buffer waiting
                      (rename-buffer (generate-new-buffer-name " *drover opening*"))))
                  (drover-remote--attach-mirror peer session answer reuse))))
    (unless (eq shown waiting)
      (dolist (window windows)
        (set-window-buffer window shown))
      (if reuse
          (when (stringp answer)
            (drover-remote--mirror-closed-why reuse answer))
        (drover-kill-buffer waiting)))
    ;; The session as its host tells it was marked seen as it was made.
    (when (and windows (not (stringp answer)))
      (drover-remote--seen-here peer session))
    shown))

(defun drover-remote--attach-mirror (peer session reply &optional buffer)
  "Mirror the terminal of SESSION, of PEER, as REPLY, its owner's answer, tells.
In BUFFER when it is a mirror that closed, so that its windows show
the new one; else in a buffer named after SESSION and PEER.  Return the
mirror's buffer, a session buffer: the keys of drover's sessions act
there on SESSION, through PEER."
  (let* ((label (drover-remote--label peer))
         (title (drover-bound-line (drover-session-display-title session) 60))
         (reuse (and buffer (buffer-live-p buffer)
                     (eq (spyglass-mirror-state buffer) 'closed)
                     buffer))
         (kind (if (equal (plist-get reply :kind) "shell") "shell" "terminal"))
         (mirror (spyglass-open (drover-remote--peer-connection peer) (plist-get reply :share)
                                :buffer-name (format "*%s@%s*" title label)
                                :buffer reuse :title title)))
    (with-current-buffer mirror
      (setq-local drover-remote--shown (cons (drover-remote--peer-name peer) (drover-session-id session)))
      (setq-local drover-session-at-point-function #'drover-remote--shown-session)
      (setq-local drover-remote--mirror-kind kind)
      (setq-local spyglass-mirror-mode-line-function #'drover-remote--mirror-mode-line)
      (add-hook 'kill-buffer-hook #'drover-remote--mirror-killed nil t))
    (puthash (drover-session-id session) mirror (drover-remote--peer-mirrors peer))
    (drover-remote--event peer 'mirror "mirroring the %s of %s%s" kind title
                          (if (eq (plist-get reply :input) t) "" ", read only"))
    mirror))

(defun drover-remote--mirror-killed ()
  "Forget the mirror of the buffer being killed: its session runs on there.
For `kill-buffer-hook'; spyglass closes the mirror."
  (drover-with-reporting "remote mirror"
    (when-let* ((shown drover-remote--shown)
                (peer (drover-remote--peer (car shown))))
      (when (eq (gethash (cdr shown) (drover-remote--peer-mirrors peer)) (current-buffer))
        (remhash (cdr shown) (drover-remote--peer-mirrors peer))))))

(defconst drover-remote--reopen-delays '(1 2 5 10 30)
  "Seconds before each attempt to open anew a mirror its owner stopped sharing.
As many attempts as delays: a peer whose remote layer restarts, or
reloads, shares its terminals again at the first.")

(defun drover-remote--mirror-closed-why (buffer why)
  "Have BUFFER, a mirror that closed, tell WHY, a line, not spyglass's reason."
  (when (and (buffer-live-p buffer) (fboundp 'spyglass-mirror-set-closed-reason))
    (spyglass-mirror-set-closed-reason buffer why)))

(defun drover-remote--reopen (peer key buffer)
  "Open anew in BUFFER the mirror of PEER's session KEY, on its connection now.
For a mirror its connection lost for good, one on a connection no
longer kept, or one its owner stopped sharing, as its remote layer
restarted or reloaded (\"unshared\", \"no such share\"): PEER is asked
for the terminal again, which it shares while it runs, at most once per
`drover-remote--reopen-delays', each failure tried again after the
next.  The owner telling the session runs no terminal any more, the
mirror says so: the terminal there was closed.  Not one closed here, nor
one whose terminal was killed there (see
`drover-remote--mirror-state')."
  (let ((connection (drover-remote--peer-connection peer))
        (session (drover-remote--peer-session peer key))
        (label (drover-remote--label peer)))
    (when (and connection session (buffer-live-p buffer)
               (eq (drover-remote--peer-state peer) 'open)
               (drover-remote--capable-p peer "mirror")
               (< (buffer-local-value 'drover-remote--reopens buffer) (length drover-remote--reopen-delays)))
      (let ((state (spyglass-mirror-state buffer))
            (reason (spyglass-mirror-closed-reason buffer)))
        (when (if (eq state 'closed)
                  (not (or (member reason '("killed" "closed here"))
                           (buffer-local-value 'drover-remote--killed-there buffer)))
                (not (eq (spyglass-mirror-connection buffer) connection)))
          (unless (eq state 'closed)
            (spyglass-close buffer))
          (let ((attempt (with-current-buffer buffer (cl-incf drover-remote--reopens))))
            (drover-remote--event peer 'mirror "opening anew the mirror of %s (attempt %d)" (buffer-name buffer) attempt)
            (drover-remote--mirror-closed-why buffer (format "opening it anew, %s sharing it again" label))
            (let ((link (drover-link peer)))
              (drover-remote--request
               peer (list :op "mirror" :key (drover-remote--wire-key session) :resume :false)
               (lambda (reply error)
                 (let ((peer (drover-linked link)))
                   (cond
                    ((not (buffer-live-p buffer)))
                    ((and (null error) (stringp (plist-get reply :share)))
                     (when (eq (spyglass-mirror-state buffer) 'closed)
                       (drover-remote--attach-mirror peer (or (drover-remote--peer-session peer key) session)
                                                     reply buffer)))
                    ((null error)
                     ;; The owner answered: no terminal runs it any more.
                     (drover-remote--event peer 'mirror "the terminal of %s is gone there: %s"
                                           (buffer-name buffer) (or (plist-get reply :none) "no terminal"))
                     (drover-remote--mirror-closed-why buffer (format "the terminal on %s was closed" label)))
                    ((< attempt (length drover-remote--reopen-delays))
                     (drover-remote--event peer 'mirror "could not open anew the mirror of %s: %s"
                                           (buffer-name buffer) error)
                     (run-at-time (nth attempt drover-remote--reopen-delays) nil
                                  (lambda ()
                                    (drover-with-reporting "remote mirror"
                                      (drover-remote--reopen (drover-linked link) key buffer)))))
                    (t
                     (drover-remote--event peer 'mirror "gave up opening anew the mirror of %s: %s"
                                           (buffer-name buffer) error)
                     (drover-remote--mirror-closed-why
                      buffer (format "%s did not share its terminal again: %s" label error))))))))))))))

(defun drover-remote--reopen-mirrors (peer)
  "Open anew the mirrors of PEER that need it, from a timer.
See `drover-remote--reopen'."
  (when (> (hash-table-count (drover-remote--peer-mirrors peer)) 0)
    (run-at-time 0.2 nil
                 (let ((link (drover-link peer)))
                   (lambda ()
                     (drover-with-reporting "remote mirror"
                       (let ((peer (drover-linked link)))
                         (when (eq peer (drover-remote--peer (drover-remote--peer-name peer)))
                           (maphash (lambda (key buffer) (drover-remote--reopen peer key buffer))
                                    (copy-hash-table (drover-remote--peer-mirrors peer)))))))))))

(defun drover-remote--mirror-state (buffer state)
  "Follow STATE, the new state of BUFFER, if it mirrors a peer's terminal.
For `spyglass-mirror-state-functions': its history tells it.  A mirror
its owner stopped sharing, as its remote layer restarted or reloaded,
or whose connection closed for good, is opened anew (see
`drover-remote--reopen'); one whose terminal was killed there says the
terminal was closed.  Either is killed once its session is archived,
as `drover-remote--close-archived-mirror' tells.  A buffer its peer
does not hold, made by a remote layer before this one, is adopted,
unless the peer holds another mirror of the session."
  (drover-with-reporting "remote mirror"
    (when-let* (((buffer-live-p buffer))
                ((buffer-local-value 'drover-remote--mirror-kind buffer))
                ;; Closed here, as killing it closes it: nothing to follow.
                ((not (and (eq state 'closed)
                           (equal (spyglass-mirror-closed-reason buffer) "closed here"))))
                (shown (buffer-local-value 'drover-remote--shown buffer))
                (peer (drover-remote--peer (car shown)))
                ((eq (drover-remote--mirror-buffer peer (cdr shown)) buffer)))
      (pcase state
        ('live (with-current-buffer buffer
                 (setq drover-remote--reopens 0 drover-remote--killed-there nil)))
        ('closed
         (let ((reason (spyglass-mirror-closed-reason buffer)))
           (drover-remote--event peer 'mirror "the mirror of %s closed: %s" (buffer-name buffer) reason)
           (cond
            ((equal reason "killed")
             (with-current-buffer buffer (setq drover-remote--killed-there t))
             (drover-remote--close-archived-mirror peer (cdr shown) buffer)
             (drover-remote--mirror-closed-why
              buffer (format "the terminal on %s was closed" (drover-remote--label peer))))
            (t
             (drover-remote--close-archived-mirror peer (cdr shown) buffer)
             (when (buffer-live-p buffer)
               (run-at-time (car drover-remote--reopen-delays) nil
                            (let ((link (drover-link peer))
                                  (key (cdr shown)))
                              (lambda ()
                                (drover-with-reporting "remote mirror"
                                  (drover-remote--reopen (drover-linked link) key buffer))))))))))))))

(defun drover-remote--close-archived-mirror (peer key buffer)
  "Kill BUFFER, the mirror of PEER's session KEY, once its owner archived it.
As archiving a session here closes its buffer: the owner closed its
own, at once or once its turn ended, which closed the mirror, and its
record says archived, or is gone from the sessions it lists, as
archived sessions go.  The mirror closed for whatever reason but being
killed here: told \"killed\", or, its link down meanwhile, asking for a
share gone there (\"no such share\").  What was typed and not sent comes
from the owner (see `drover-remote--on-unsent').  A mirror the owner
closed otherwise, its session still listed, stays readable, saying why;
so does every mirror of a peer whose sessions are not known yet, as
after it closed for good, which empties them, until it tells them."
  (when (and (buffer-live-p buffer)
             (eq (spyglass-mirror-state buffer) 'closed)
             (not (equal (spyglass-mirror-closed-reason buffer) "closed here"))
             (eq (drover-remote--peer-state peer) 'open)
             (drover-remote--peer-epoch peer)
             (let ((record (drover-remote--peer-session peer key)))
               (or (null record) (drover-session-archived-p record))))
    (drover-remote--event peer 'mirror "closed the mirror of %s: its session is gone there" (buffer-name buffer))
    (drover-kill-buffer buffer)))

(defun drover-remote--close-archived-mirrors (peer keys)
  "Kill the mirrors of PEER's sessions KEYS whose owner archived them.
See `drover-remote--close-archived-mirror'."
  (when (> (hash-table-count (drover-remote--peer-mirrors peer)) 0)
    (dolist (key keys)
      (when-let* ((buffer (drover-remote--mirror-buffer peer key)))
        (drover-remote--close-archived-mirror peer key buffer)))))

(defun drover-remote--mirror-mode-line ()
  "Return what the mode line of a mirror of a peer's session tells of it.
For `spyglass-mirror-mode-line-function', after the mirror's mode name:
the session's status, harness, model, effort and context, as the peer
last told them."
  (when-let* ((session (drover-remote--shown-session)))
    (let ((status (drover-session-status session))
          (tokens (drover-session-context-tokens session))
          (limit (drover-session-context-limit session)))
      (drover-bound-line
       (string-join
        (delq nil
              (list (concat " " (opencode-session-status-glyph status) " "
                            (drover-face (opencode-session-status-label status)
                                         (opencode-session-status-face status)))
                    (concat (drover-harness-glyph (drover-session-harness session)) " "
                            (drover-harness-name (drover-session-harness session)))
                    (and (drover-session-model session)
                         (concat (drover-session-model session)
                                 (and (drover-session-effort session)
                                      (concat " " (drover-face (drover-session-effort session)
                                                               (if (drover-session-effort-default session)
                                                                   'opencode-setting-inherited
                                                                 'opencode-setting-chosen))))))
                    (and (natnump tokens) (natnump limit) (> limit 0)
                         (format "%d%%" (/ (* 100 tokens) limit)))))
        "  ")
       200))))

(defun drover-remote--seen-mirrors (peer keys)
  "Mark seen those of PEER's sessions KEYS the selected window mirrors.
Their turns ended there while the user looked here: at most once a
second each."
  (when (> (hash-table-count (drover-remote--peer-mirrors peer)) 0)
    (let* ((buffer (window-buffer (selected-window)))
           (shown (buffer-local-value 'drover-remote--shown buffer)))
      (when (and shown
                 (buffer-local-value 'drover-remote--mirror-kind buffer)
                 (equal (car shown) (drover-remote--peer-name peer))
                 (member (cdr shown) keys)
                 (frame-focus-state)
                 (> (- (float-time) (buffer-local-value 'drover-remote--seen-sent buffer)) 1))
        (when-let* ((session (drover-remote--peer-session peer (cdr shown)))
                    ((memq (drover-session-status session) '(done failed))))
          (with-current-buffer buffer (setq drover-remote--seen-sent (float-time)))
          (drover-remote--seen-here peer session))))))

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

;;;; OpenCode services of peers

;; A peer's OpenCode session opens here as a full opencode.el buffer,
;; talking to the peer's service through its drover: opencode.el's
;; requests to the service registered under the peer's name go to the
;; relay relay://NAME, which carries them on drover's channel to the
;; owner, which performs them against its own service, with its own
;; password, as the peer's access allows, and answers.  Its event
;; stream goes the same way, its bytes relayed as the owner's service
;; writes them.  So the password never leaves the owner, and the owner,
;; the truth of its sessions, decides what a peer may do to them.

(declare-function opencode-service-register "opencode-service" (name &rest arguments))
(declare-function opencode-service-unregister "opencode-events" (service))
(declare-function opencode-service-get "opencode-common" (name))
(declare-function opencode-svc-closed "opencode-common" (service))
(declare-function opencode-svc-name "opencode-common" (service))
(declare-function opencode-current-service "opencode-common" ())
(declare-function opencode-call-with-service "opencode-common" (service function &rest arguments))
(declare-function opencode-session-open-by-id "opencode-session" (id &rest arguments))
(declare-function opencode-events-reopen "opencode-events" ())
(declare-function opencode-service-ensure "opencode-service" (callback &optional errback))
(declare-function opencode-service-authorization "opencode-service" (connection))
(declare-function opencode-transport-register-relay "opencode-transport" (name function))
(declare-function opencode-transport-unregister-relay "opencode-transport" (name))
(declare-function opencode-transport-relay-answer "opencode-transport" (exchange status body &optional headers))
(declare-function opencode-transport-relay-chunk "opencode-transport" (exchange bytes))
(declare-function opencode-transport-relay-end "opencode-transport" (exchange))
(declare-function opencode-transport-relay-fail "opencode-transport" (exchange reason message &optional status body))
(declare-function opencode-transport-exchange-filter "opencode-transport" (exchange))
(declare-function opencode-transport-request "opencode-transport" (method url &rest arguments))
(declare-function opencode-transport-cancel "opencode-transport" (exchange))
(declare-function opencode-transport-failure-reason "opencode-transport" (failure))
(declare-function opencode-transport-failure-status "opencode-transport" (failure))
(declare-function opencode-transport-failure-body "opencode-transport" (failure))
(declare-function opencode-transport-failure-describe "opencode-transport" (failure))
(defvar opencode-session-status)
(defvar opencode-state-updated-functions)

(defcustom drover-remote-opencode-timeout 120
  "Seconds a request to a peer's OpenCode service waits for its owner's answer."
  :type 'number
  :group 'drover-remote)

(defconst drover-remote--opencode-headers '(:accept :content-type :x-opencode-directory)
  "The headers of an OpenCode request a peer relays, as keywords.
Never its credentials: the owner adds its own.")

(defvar drover-remote--oc-streams (make-hash-table :test #'equal)
  "Event streams of peers' OpenCode services relayed here, by identifier.
Each is (PEER-NAME CONNECTION EXCHANGE), links (`drover-link') to the
connection the stream was asked on and to opencode.el's exchange.")

(defvar drover-remote--oc-buffers (make-hash-table :test #'equal)
  "The OpenCode buffers of peers' sessions here, by (PEER-NAME . KEY).
KEY is the owner's.")

(defvar drover-remote--oc-relayed (make-hash-table :test #'eq :weakness 'key)
  "The event streams this host relays to peers, by connection.
Each a hash table of links (`drover-link') to the exchanges with this
host's service, by the identifier the peer gave.")

(defvar drover-remote--oc-serial 0
  "The number of the last event stream relayed here.")

(defun drover-remote--opencode-here-p ()
  "Return non-nil when this host's opencode.el reaches services through relays."
  (and (fboundp 'opencode-service-register) (fboundp 'opencode-transport-register-relay)
       (fboundp 'opencode-call-with-service)))

(defun drover-remote--serves-opencode-p ()
  "Return non-nil when this host relays its OpenCode service to its peers.
That is when opencode.el, loaded, reaches its service through sockets."
  (and (featurep 'opencode-service) (fboundp 'opencode-transport-request)
       (fboundp 'opencode-service-ensure)))

(defun drover-remote--pushes-p ()
  "Return non-nil when this host pushes to phones: endpoints set, crypto and curl."
  (and (featurep 'drover-remote-push)
       drover-remote-push-endpoints
       (drover-remote-push-available-p)))

(defun drover-remote--capabilities ()
  "Return the capabilities this host tells its peers, as a vector of strings."
  (vconcat (seq-remove (lambda (capability)
                         (or (and (equal capability "mirror") (not (drover-remote--mirrors-p)))
                             (and (equal capability "opencode") (not (drover-remote--serves-opencode-p)))
                             (and (equal capability "push") (not (drover-remote--pushes-p)))))
                       drover-remote-capabilities)))

;;;;; Here, viewing a peer's sessions

(defun drover-remote--oc-service (peer)
  "Return PEER's OpenCode service in opencode.el, registering it when needed.
It goes by PEER's name, its requests going through the relay of that
name, which carries them to PEER."
  (let* ((name (drover-remote--peer-name peer))
         (label (drover-remote--label peer))
         (service (opencode-service-get name)))
    (opencode-transport-register-relay name (drover-remote--oc-relay name))
    (if (and service (not (opencode-svc-closed service)))
        service
      (opencode-service-register
       name :label label
       :connect (lambda (callback errback)
                  (let ((peer (drover-remote--peer name)))
                    (if (and peer (eq (drover-remote--peer-state peer) 'open)
                             (drover-remote--capable-p peer "opencode"))
                        (funcall callback (list :url (concat "relay://" name) :id (concat "drover:" name)))
                      (funcall errback (format "%s is not connected" label)))))))))

(defun drover-remote--oc-relay (name)
  "Return the relay carrying OpenCode requests to the peer NAME."
  (lambda (exchange method path headers body)
    (drover-with-reporting "remote OpenCode"
      (drover-remote--oc-carry name exchange method path headers body))))

(defun drover-remote--oc-fail-later (exchange reason message)
  "Fail EXCHANGE for REASON with MESSAGE from a timer, after it is relayed."
  (run-at-time 0 nil (lambda ()
                       (drover-with-reporting "remote OpenCode"
                         (opencode-transport-relay-fail exchange reason message)))))

(defun drover-remote--oc-headers (headers)
  "Return the HEADERS of an OpenCode request a peer relays, as a plist."
  (let ((out nil))
    (pcase-dolist (`(,name . ,value) headers)
      (let ((key (intern (concat ":" (downcase name)))))
        (when (and (memq key drover-remote--opencode-headers) (stringp value) (< (length value) 4096))
          (setq out (plist-put out key value)))))
    out))

(defun drover-remote--oc-carry (name exchange method path headers body)
  "Carry EXCHANGE to the peer NAME's service: METHOD on PATH with HEADERS and BODY.
Return the function abandoning it, as relays do."
  (let* ((peer (drover-remote--peer name))
         (connection (and peer (drover-remote--peer-connection peer))))
    (cond
     ((not (and connection (eq (drover-remote--peer-state peer) 'open)))
      (drover-remote--oc-fail-later exchange 'refused (format "%s is not connected" name))
      nil)
     ((not (drover-remote--capable-p peer "opencode"))
      (drover-remote--oc-fail-later exchange 'refused
                                    (format "%s runs an older drover, which relays no OpenCode service"
                                            (drover-remote--label peer)))
      nil)
     ((opencode-transport-exchange-filter exchange)
      (drover-remote--oc-open-stream peer connection exchange path))
     (t
      (drover-remote--oc-send peer connection exchange method path headers body)))))

(defun drover-remote--oc-send (peer connection exchange method path headers body)
  "Send PEER, on CONNECTION, EXCHANGE's request.
It is METHOD on PATH with HEADERS and BODY; its answer settles
EXCHANGE.  A request whose stream ends before its answer fails, retried
by opencode.el only when it asked for something: the owner may have
performed one that changes something."
  (let ((label (drover-remote--label peer))
        (asks (memq method '(get head))))
    (drover-remote--stat peer :oc-requests)
    (spyglass-channel-request
     connection drover-remote-channel
     (list :op "oc" :m (symbol-name method) :p path
           :h (or (drover-remote--oc-headers headers) (make-hash-table))
           :b (if body (decode-coding-string body 'utf-8) :false))
     (lambda (reply error)
       (drover-with-reporting "remote OpenCode"
         (cond
          ((null error)
           (let ((status (plist-get reply :st))
                 (text (plist-get reply :b)))
             (if (integerp status)
                 (opencode-transport-relay-answer exchange status (if (stringp text) text ""))
               (opencode-transport-relay-fail exchange 'protocol (format "%s answered nothing" label)))))
          ((string-match-p "ended" error)
           (opencode-transport-relay-fail exchange (if asks 'closed 'truncated)
                                          (format "the link to %s ended%s" label
                                                  (if asks "" "; it may have performed it"))))
          ((string-match-p "in time" error)
           (opencode-transport-relay-fail exchange 'timeout (format "%s did not answer in time" label)))
          (t (opencode-transport-relay-fail exchange 'refused (format "%s: %s" label error))))))
     drover-remote-opencode-timeout)
    nil))

(defun drover-remote--oc-open-stream (peer connection exchange path)
  "Ask PEER, on CONNECTION, for the event stream EXCHANGE opens at PATH.
Return the function closing it."
  (let* ((name (drover-remote--peer-name peer))
         (label (drover-remote--label peer))
         (id (format "%s-%d" (or (and drover-remote--state (drover-remote--state-epoch drover-remote--state))
                                 "here")
                     (cl-incf drover-remote--oc-serial)))
         (link (drover-link connection)))
    (puthash id (list name link (drover-link exchange)) drover-remote--oc-streams)
    (drover-remote--stat peer :oc-streams)
    (spyglass-channel-request
     connection drover-remote-channel (list :op "ocs" :id id :p path)
     (lambda (reply error)
       (drover-with-reporting "remote OpenCode"
         (let ((status (plist-get reply :st)))
           (when (or error (not (eql status 200)))
             (when (gethash id drover-remote--oc-streams)
               (remhash id drover-remote--oc-streams)
               (if (integerp status)
                   (opencode-transport-relay-answer exchange status (or (plist-get reply :b) ""))
                 (opencode-transport-relay-fail exchange 'refused (format "%s: %s" label error))))))))
     drover-remote-request-timeout)
    (lambda ()
      (drover-with-reporting "remote OpenCode"
        (when (gethash id drover-remote--oc-streams)
          (remhash id drover-remote--oc-streams)
          (let ((connection (drover-linked link)))
            (when (spyglass-connection-live-p connection)
              (drover-remote--send connection (list :t "occ" :id id)))))))))

(defun drover-remote--on-oc-data (message)
  "Give the stream MESSAGE names the bytes its owner's service wrote."
  (when-let* ((id (plist-get message :id))
              (entry (and (stringp id) (gethash id drover-remote--oc-streams)))
              (text (plist-get message :d))
              ((stringp text)))
    (opencode-transport-relay-chunk (drover-linked (nth 2 entry)) (encode-coding-string text 'utf-8 t))))

(defun drover-remote--on-oc-end (message)
  "End the stream MESSAGE names, as its owner's service ended it or refused it."
  (when-let* ((id (plist-get message :id))
              (entry (and (stringp id) (gethash id drover-remote--oc-streams))))
    (remhash id drover-remote--oc-streams)
    (let ((why (plist-get message :err)))
      (if (stringp why)
          (opencode-transport-relay-fail (drover-linked (nth 2 entry)) 'closed (drover-bound-line why))
        (opencode-transport-relay-end (drover-linked (nth 2 entry)))))))

(defun drover-remote--oc-drop (connection)
  "End the streams of peers' OpenCode services asked on CONNECTION.
Its stream ended: opencode.el opens them anew, through the connection
kept."
  (let ((ended nil))
    (maphash (lambda (id entry)
               (when (eq (drover-linked (nth 1 entry)) connection)
                 (push (cons id entry) ended)))
             drover-remote--oc-streams)
    (pcase-dolist (`(,id ,_name ,_link ,exchange) ended)
      (remhash id drover-remote--oc-streams)
      (opencode-transport-relay-fail (drover-linked exchange) 'closed "the link to its host ended"))))

(defun drover-remote--oc-resume (peer)
  "Have PEER's OpenCode service, if registered here, follow PEER again at once.
PEER is connected anew: opencode.el would wait for its backoff.  A
service closed as PEER went for good, or as the remote layer stopped,
which reloading drover does, is registered again, so that the buffers
of its sessions still open follow them anew rather than refuse every
request."
  (when-let* (((drover-remote--opencode-here-p))
              (name (drover-remote--peer-name peer))
              (service (opencode-service-get name)))
    (run-at-time 0 nil (lambda ()
                         (drover-with-reporting "remote OpenCode"
                           (if (opencode-svc-closed service)
                               (drover-remote--oc-reopen name)
                             (opencode-call-with-service service #'opencode-events-reopen)))))))

(defun drover-remote--oc-reopen (name)
  "Register again the OpenCode service of the peer NAME, closed here.
Only while NAME is connected and relays its service; its buffers then
close again once their sessions are archived there."
  (when-let* ((peer (drover-remote--peer name))
              ((eq (drover-remote--peer-state peer) 'open))
              ((drover-remote--capable-p peer "opencode"))
              (service (opencode-service-get name))
              ((opencode-svc-closed service)))
    (drover-remote--oc-service peer)
    (when (seq-some (lambda (shown) (equal (car shown) name))
                    (hash-table-keys drover-remote--oc-buffers))
      (add-hook 'opencode-state-updated-functions #'drover-remote--oc-updated))
    (drover-remote--event peer 'opencode "followed its OpenCode service again")))

(defun drover-remote--oc-close (peer)
  "Close PEER's OpenCode service here, PEER gone for good.
Its buffers stay, saying so, until PEER connects again
\(see `drover-remote--oc-resume')."
  (when-let* (((drover-remote--opencode-here-p))
              (service (opencode-service-get (drover-remote--peer-name peer))))
    (opencode-service-unregister service)
    (opencode-transport-unregister-relay (drover-remote--peer-name peer))))

(defun drover-remote--oc-buffer (name key)
  "Return the live OpenCode buffer of the session KEY of the peer NAME, or nil."
  (let ((buffer (gethash (cons name key) drover-remote--oc-buffers)))
    (if (buffer-live-p buffer)
        buffer
      (remhash (cons name key) drover-remote--oc-buffers)
      nil)))

(defun drover-remote--why-no-opencode (peer)
  "Return why PEER's OpenCode sessions open as their host tells them, or nil."
  (let ((label (drover-remote--label peer)))
    (cond
     ((not (drover-remote--opencode-here-p))
      "This host's opencode.el talks to one service only: update it")
     ((not (eq (drover-remote--peer-state peer) 'open))
      (format "%s is not connected: its OpenCode session opens once it is" label))
     ((not (drover-remote--capable-p peer "opencode"))
      (format "%s runs an older drover, which relays no OpenCode service: update it there" label)))))

(defun drover-remote--open-opencode (peer session)
  "Open SESSION, an OpenCode session of PEER, in a full opencode.el buffer.
The buffer talks to PEER's service through PEER, which performs what
it asks as its access allows.  Return the buffer when one is open
already; otherwise it shows once the session's info arrives."
  (let* ((name (drover-remote--peer-name peer))
         (key (drover-session-id session))
         (existing (drover-remote--oc-buffer name key)))
    (if existing
        (progn
          ;; Its service closed here as the remote layer stopped: follow it again.
          (drover-remote--oc-reopen name)
          (pop-to-buffer-same-window existing)
          existing)
      (let ((service (drover-remote--oc-service peer))
            (window (selected-window))
            (title (drover-bound-line (drover-session-display-title session) 60)))
        (drover-message "Opening %s's session %s..." (drover-remote--label peer) title)
        (opencode-call-with-service
         service #'opencode-session-open-by-id (cdr key)
         :no-display t
         :callback (lambda (_info)
                     (drover-with-reporting "remote OpenCode"
                       (let ((buffer (current-buffer)))
                         (drover-remote--oc-adopt name key buffer)
                         (drover-message "")
                         (if (window-live-p window)
                             (with-selected-window window (pop-to-buffer-same-window buffer))
                           (pop-to-buffer-same-window buffer)))))
         :else (lambda (error)
                 (drover-warn "%s's session %s did not open: %s" (drover-remote--label peer) title
                              (drover-bound-line (format "%s" (if (fboundp 'opencode-error-describe)
                                                                  (opencode-error-describe error)
                                                                error))))))
        nil))))

(declare-function opencode-error-describe "opencode-http" (error))

(defun drover-remote--oc-adopt (name key buffer)
  "Make BUFFER, opencode.el's, the buffer of the session KEY of the peer NAME.
drover's keys act there on that session through NAME, as in its mirrors."
  (with-current-buffer buffer
    (setq-local drover-remote--shown (cons name key))
    (setq-local drover-session-at-point-function #'drover-remote--shown-session)
    (add-hook 'kill-buffer-hook #'drover-remote--oc-killed nil t))
  (puthash (cons name key) buffer drover-remote--oc-buffers)
  (add-hook 'opencode-state-updated-functions #'drover-remote--oc-updated)
  (when-let* ((peer (drover-remote--peer name)))
    (drover-remote--event peer 'opencode "opened the OpenCode session %s"
                          (drover-bound-line (format "%s" (cdr key)) 40))
    (drover-backend-changed (drover-remote--peer-backend peer) key)))

(defun drover-remote--oc-killed ()
  "Forget the OpenCode buffer being killed, of a peer's session.
For `kill-buffer-hook'."
  (drover-with-reporting "remote OpenCode"
    (when-let* ((shown drover-remote--shown))
      (when (eq (gethash shown drover-remote--oc-buffers) (current-buffer))
        (remhash shown drover-remote--oc-buffers)
        (when-let* ((peer (drover-remote--peer (car shown))))
          (drover-backend-changed (drover-remote--peer-backend peer) (cdr shown)))))))

(declare-function opencode-session-archived-p "opencode-session" (&optional id))

(defun drover-remote--oc-archived (peer keys)
  "Close the OpenCode buffers of PEER's sessions KEYS its owner archived.
See `drover-remote--oc-close-archived'."
  (let ((name (drover-remote--peer-name peer)))
    (dolist (key keys)
      (when-let* ((buffer (drover-remote--oc-buffer name key)))
        (drover-remote--oc-close-archived peer key buffer)))))

(defun drover-remote--oc-close-archived (peer key buffer)
  "Close BUFFER, of PEER's OpenCode session KEY, once its owner archived it.
The owner's record says archived, or is gone, its owner's drover listing
archived sessions apart, while the service says archived too.  As
archiving an OpenCode session closes its buffer here: at once, a turn
running or not, its session living in the service, and input typed at
its prompt and not sent going to the kill ring, the user told."
  (let ((record (drover-remote--peer-session peer key)))
    (when (and (buffer-live-p buffer)
               (if record
                   (drover-session-archived-p record)
                 (with-current-buffer buffer (opencode-session-archived-p))))
      (let* ((process (get-buffer-process buffer))
             (unsent (and process (with-current-buffer buffer
                                    (buffer-substring-no-properties (process-mark process) (point-max)))))
             (title (buffer-name buffer)))
        (drover-remote--event peer 'opencode "closed the OpenCode session %s: archived there"
                              (drover-bound-line (format "%s" (cdr key)) 40))
        (when (drover-keep-unsent (list unsent))
          (drover-message "%s" (drover--archive-note 'kept title)))
        (drover-kill-buffer buffer)))))

(defun drover-remote--oc-updated (id)
  "Close the buffer of the peer's OpenCode session ID once archived there.
For `opencode-state-updated-functions', which runs for that peer's
service: the service may tell after the owner's record does."
  (drover-with-reporting "remote OpenCode"
    (when-let* (((and id (fboundp 'opencode-service-remote-p) (opencode-service-remote-p)))
                (name (opencode-svc-name (opencode-current-service)))
                (peer (drover-remote--peer name)))
      (let ((found nil))
        (maphash (lambda (shown buffer)
                   (when (and (equal (car shown) name) (equal (cdr-safe (cdr shown)) id))
                     (push (cons (cdr shown) buffer) found)))
                 drover-remote--oc-buffers)
        (pcase-dolist (`(,key . ,buffer) found)
          (drover-remote--oc-close-archived peer key buffer))))))

;;;;; There, relaying this host's service

(defun drover-remote--oc-refusal (access method path)
  "Return why a peer of ACCESS may not send METHOD on PATH, a line, or nil.
`read' reads; `act' does everything but deleting a session or a
project; `full' everything.  Paths are the service's API's."
  (cond
   ((not (and (stringp path) (< (length path) 8192)
              (string-match-p "\\`/api/[^[:space:]]*\\'" path)))
    "not a path of the OpenCode API")
   ((not (memq method '(get head post put patch delete)))
    "not a method of the OpenCode API")
   ((memq method '(get head)) nil)
   ((eq access 'read) "this host grants it read access")
   ((and (eq access 'act) (eq method 'delete)
         (string-match-p "\\`/api/\\(?:session\\|project\\)/[^/?]+/?\\(?:\\?.*\\)?\\'" path))
    "this host grants it act access, which deletes nothing")
   ((memq access '(act full)) nil)
   (t "not allowed")))

(defun drover-remote--oc-refuse (reply why)
  "Answer through REPLY that the request is refused, WHY: a 403, as services do."
  (funcall reply (list :st 403 :b (drover-json-text (list :message why :_tag "Forbidden")))))

(defun drover-remote--oc-service-then (reply function)
  "Call FUNCTION with this host's OpenCode connection, else answer REPLY a 503.
The local service, whatever the buffers here."
  (opencode-call-with-service
   nil #'opencode-service-ensure
   (lambda (connection)
     (drover-with-reporting "remote OpenCode"
       (funcall function connection)))
   (lambda (why)
     (drover-with-reporting "remote OpenCode"
       (funcall reply (list :st 503 :b (drover-json-text (list :message (drover-bound-line why)))))))))

(defun drover-remote--oc-request-headers (connection message)
  "Return the headers of the request MESSAGE asks this host's service.
CONNECTION is that service's: its credentials, and those of MESSAGE's
headers a peer may send."
  (let ((headers (list (cons "Authorization" (opencode-service-authorization connection))))
        (asked (plist-get message :h)))
    (dolist (key drover-remote--opencode-headers)
      (let ((value (and (consp asked) (plist-get asked key))))
        (when (and (stringp value) (< (length value) 4096) (not (string-match-p "[\r\n]" value)))
          (push (cons (substring (symbol-name key) 1) value) headers))))
    headers))

(defun drover-remote--on-oc (peer message reply)
  "Perform the OpenCode request MESSAGE of PEER against this host's service.
REPLY answers with its status and body; a refusal of PEER's access is
a 403."
  (let* ((method (and (stringp (plist-get message :m))
                      (car (member (intern-soft (plist-get message :m)) '(get head post put patch delete)))))
         (path (plist-get message :p))
         (body (plist-get message :b))
         (why (drover-remote--oc-refusal (drover-remote--access peer) method path)))
    (drover-remote--stat peer :oc-served)
    (cond
     ((not (drover-remote--serves-opencode-p))
      (funcall reply nil "this host relays no OpenCode service"))
     (why
      (drover-remote--event peer 'refused "OpenCode %s %s: %s" (or method "?")
                            (drover-bound-line (format "%s" path) 80) why)
      (drover-remote--oc-refuse reply why))
     ((and body (not (stringp body)) (not (eq body :false)))
      (funcall reply nil "malformed body"))
     (t
      (drover-remote--oc-service-then
       reply
       (lambda (connection)
         (opencode-transport-request
          method (concat (plist-get connection :url) path)
          :headers (drover-remote--oc-request-headers connection message)
          :body (and (stringp body) (encode-coding-string body 'utf-8))
          :timeout drover-remote-opencode-timeout
          :then (lambda (text)
                  (drover-with-reporting "remote OpenCode"
                    (funcall reply (list :st 200 :b (if (stringp text) text "")))))
          :else (lambda (failure)
                  (drover-with-reporting "remote OpenCode"
                    (if (eq (opencode-transport-failure-reason failure) 'http)
                        (funcall reply (list :st (opencode-transport-failure-status failure)
                                             :b (or (opencode-transport-failure-body failure) "")))
                      (funcall reply nil (drover-bound-line
                                          (opencode-transport-failure-describe failure)))))))))))))

(defconst drover-remote--oc-piece 65536
  "Bytes of an event stream at most in one message to a peer.
A large event, a long tool output, goes in pieces, so that neither end
parses one message of megabytes at once.")

(defun drover-remote--oc-cut (bytes)
  "Return how many of BYTES, UTF-8, to send now, whole characters, or 0.
All that ends with a line when it is short; otherwise a piece of
`drover-remote--oc-piece' bytes at most, never cutting a character."
  (let ((length (length bytes)))
    (if (<= length drover-remote--oc-piece)
        (let ((end (cl-position ?\n bytes :from-end t)))
          (if end (1+ end) 0))
      (let ((cut drover-remote--oc-piece))
        ;; Back over the continuation bytes of a character, 10xxxxxx.
        (while (and (> cut 0) (= (logand (aref bytes cut) #xC0) #x80))
          (setq cut (1- cut)))
        cut))))

(defun drover-remote--oc-flush (connection id pending)
  "Send CONNECTION's peer the bytes of the stream ID that PENDING holds, a cons.
In pieces of whole characters: a piece ends with a line, or is a piece
of a long one."
  (let ((cut nil))
    (while (and (car pending) (> (setq cut (drover-remote--oc-cut (car pending))) 0))
      (let ((bytes (car pending)))
        (setcar pending (substring bytes cut))
        (drover-remote--send connection (list :t "ocd" :id id
                                              :d (decode-coding-string (substring bytes 0 cut) 'utf-8)))))))

(defun drover-remote--on-oc-stream (peer connection message reply)
  "Relay to PEER, on CONNECTION, the event stream of this host's service.
MESSAGE asks for it; REPLY answers at once, the stream's bytes follow,
then its end."
  (let ((id (plist-get message :id))
        (path (plist-get message :p)))
    (cond
     ((not (drover-remote--serves-opencode-p))
      (funcall reply nil "this host relays no OpenCode service"))
     ((not (and (stringp id) (string-match-p drover-remote--id-regexp id)))
      (funcall reply nil "malformed stream"))
     ((not (equal path "/api/event"))
      (drover-remote--oc-refuse reply "only the event stream streams"))
     (t
      (drover-remote--stat peer :oc-streams-served)
      (drover-remote--oc-service-then
       reply
       (lambda (service)
         (let* ((link (drover-link connection))
                (pending (list nil))
                (table (or (gethash connection drover-remote--oc-relayed)
                           (puthash connection (make-hash-table :test #'equal) drover-remote--oc-relayed))))
           (cl-flet ((ended (why)
                       (drover-with-reporting "remote OpenCode"
                         (let ((connection (drover-linked link)))
                           (when-let* ((table (gethash connection drover-remote--oc-relayed)))
                             (remhash id table))
                           (when (spyglass-connection-live-p connection)
                             (drover-remote--oc-flush connection id pending)
                             (drover-remote--send connection (append (list :t "oce" :id id)
                                                                     (and why (list :err why)))))))))
             (puthash id
                      (drover-link
                       (opencode-transport-request
                       'get (concat (plist-get service :url) path)
                       :headers (list (cons "Authorization" (opencode-service-authorization service))
                                      (cons "Accept" "text/event-stream"))
                       :filter (lambda (_exchange bytes)
                                 (drover-with-reporting "remote OpenCode"
                                   (let ((connection (drover-linked link)))
                                     (setcar pending (concat (car pending) bytes))
                                     (when (spyglass-connection-live-p connection)
                                       (drover-remote--oc-flush connection id pending)))))
                       :then (lambda (_) (ended nil))
                       :else (lambda (failure)
                               (ended (drover-bound-line (opencode-transport-failure-describe failure))))))
                      table)
             (funcall reply (list :st 200))))))))))

(defun drover-remote--on-oc-cancel (connection message)
  "Close the stream of this host's service MESSAGE names, CONNECTION's peer gone."
  (when-let* ((table (gethash connection drover-remote--oc-relayed))
              (id (plist-get message :id))
              (exchange (and (stringp id) (gethash id table))))
    (remhash id table)
    (opencode-transport-cancel (drover-linked exchange))))

(defun drover-remote--oc-unrelay (connection)
  "Close the streams of this host's service relayed on CONNECTION, which ended."
  (when-let* ((table (gethash connection drover-remote--oc-relayed)))
    (remhash connection drover-remote--oc-relayed)
    (maphash (lambda (_id exchange) (opencode-transport-cancel (drover-linked exchange))) table)))

;;;;; Clones of OpenCode sessions, through this host's service

(defconst drover-remote-oc-clone-timeout 300
  "Seconds this host's service may take to export or import a session to clone.")

(defun drover-remote--oc-failure-message (failure)
  "Return the message the service's body of FAILURE tells, a line, or nil."
  (when-let* ((body (opencode-transport-failure-body failure))
              ((stringp body))
              (data (ignore-errors (json-parse-string body :object-type 'plist :null-object nil
                                                      :false-object nil)))
              ((plistp data)))
    (drover-bound-line (format "%s" (or (plist-get data :message) (plist-get data :_tag) "")))))

(defun drover-remote--oc-clone-request (method path body then else)
  "Ask this host's service METHOD of PATH with BODY, JSON bytes or nil.
THEN gets the answer's body, as bytes; ELSE the failure's status, nil
when the service did not answer, and a line."
  (if (not (drover-remote--serves-opencode-p))
      (funcall else nil "this host's opencode.el reaches no service through sockets: update it")
    (opencode-call-with-service
     nil #'opencode-service-ensure
     (lambda (connection)
       (drover-with-reporting "OpenCode clone"
         (opencode-transport-request
          method (concat (plist-get connection :url) path)
          :headers (append (list (cons "Authorization" (opencode-service-authorization connection)))
                           (and body (list (cons "Content-Type" "application/json"))))
          :body body
          :timeout drover-remote-oc-clone-timeout
          :as (lambda ()
                (let ((text (buffer-substring-no-properties (point) (point-max))))
                  (if (multibyte-string-p text) (encode-coding-string text 'utf-8) text)))
          :then (lambda (bytes)
                  (drover-with-reporting "OpenCode clone"
                    (funcall then bytes)))
          :else (lambda (failure)
                  (drover-with-reporting "OpenCode clone"
                    (funcall else
                             (and (eq (opencode-transport-failure-reason failure) 'http)
                                  (opencode-transport-failure-status failure))
                             (or (drover-remote--oc-failure-message failure)
                                 (drover-bound-line (opencode-transport-failure-describe failure)))))))))
     (lambda (why)
       (drover-with-reporting "OpenCode clone"
         (funcall else nil (drover-bound-line (format "%s" why))))))))

(defun drover-remote--oc-export (id callback)
  "Call CALLBACK with what another host needs to clone OpenCode session ID.
This host's service's export of it, through the route of the fork: the
session and its settled messages, their attachments inline; not the
sessions of its subagents, nor the snapshots of the files its turns
changed.  For the engine's :export (see `drover-remote--clone-engine')."
  (drover-remote--oc-clone-request
   'get (format "/api/experimental/session/%s/export" (url-hexify-string id)) nil
   (lambda (bytes)
     (funcall callback (list :parts (list (list "export.json" :bytes bytes)))))
   (lambda (status message)
     (funcall callback
              (list :failure (if (eql status 404)
                                 (format "%s (a service of the fork exports sessions)" message)
                               message))))))

(defun drover-remote--oc-import-body (bytes directory)
  "Return the request importing the export BYTES as a session run in DIRECTORY.
The export's info and messages, as they are, and the location; parsed
and serialized again only when the export is not shaped as the fork
writes it, its info first."
  (let ((location (json-serialize `((directory . ,directory)))))
    (if (and (string-prefix-p "{\"data\":{\"info\":" bytes)
             (string-match "}}[ \t\r\n]*\\'" bytes))
        (concat "{" (substring bytes (length "{\"data\":{") (match-beginning 0))
                ",\"location\":" location "}")
      (let ((data (alist-get 'data (json-parse-string (decode-coding-string bytes 'utf-8)
                                                      :object-type 'alist
                                                      :null-object :null :false-object :false))))
        (json-serialize `((info . ,(alist-get 'info data))
                          (messages . ,(or (alist-get 'messages data) []))
                          (location (directory . ,directory))))))))

(defun drover-remote--oc-clone-directory (directory)
  "Return DIRECTORY as a service takes a location: absolute, its drive upcased."
  (let ((name (directory-file-name (expand-file-name directory))))
    (if (string-match "\\`\\([a-z]\\):" name)
        (concat (upcase (match-string 1 name)) (substring name 1))
      name)))

(defun drover-remote--oc-import (id parts _data directory then else)
  "Make OpenCode session ID, of PARTS received, a session of this host's service.
The service imports the export, run in DIRECTORY, under its identifier:
a session of that identifier here already is not replaced.  THEN gets
ID, ELSE why it could not.  For the engine's :import (see
`drover-remote--clone-engine')."
  (let ((file (cdr (assoc "export.json" parts))))
    (if (not (and file (= (length parts) 1)))
        (funcall else "no export of an OpenCode session")
      (pcase-let ((`(,body . ,error)
                   (condition-case err
                       (cons (drover-remote--oc-import-body
                              (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally file)
                                (buffer-string))
                              (drover-remote--oc-clone-directory directory))
                             nil)
                     (error (cons nil (drover-error-message err))))))
        (if error
            (funcall else error)
          (drover-remote--oc-clone-request
           'post "/api/experimental/session/import" body
           (lambda (_bytes) (funcall then id))
           (lambda (status message)
             (funcall else (pcase status
                             (409 "this host's OpenCode service has this session already")
                             (404 (format "%s (a service of the fork imports sessions)" message))
                             (_ message))))))))))

;;;; The mode

(defun drover-remote--start (&optional restart)
  "Make the state of the remote layer and connect to the peers.
RESTART non-nil tells that the layer ran before in this Emacs: phones
are then told what changed while it was stopped (see
`drover-remote-push-start')."
  (let ((state (drover-remote--state-make :epoch (substring (spyglass-wire-random-token) 0 16))))
    (setq drover-remote--state state)
    (spyglass-register-channel drover-remote-channel #'drover-remote--handle
                               :request #'drover-remote--request-handler
                               :attach #'drover-remote--attach
                               :detach #'drover-remote--detach)
    (unless (spyglass-server)
      (spyglass-server-start)
      (setf (drover-remote--state-started-server state) t))
    ;; Terminals a remote layer before this one shared, restarting or
    ;; made anew as drover reloads, stay shared: their mirrors resume.
    (drover-remote--adopt-shares state)
    (add-hook 'drover-changed-hook #'drover-remote--owner-changed)
    (require 'drover-remote-push)
    (drover-remote-push-start restart)
    (add-hook 'spyglass-error-functions #'drover-remote--spyglass-error)
    (add-hook 'spyglass-share-state-functions #'drover-remote--share-event)
    (add-hook 'drover-unsent-functions #'drover-remote--unsent)
    (add-hook 'spyglass-mirror-state-functions #'drover-remote--mirror-state)
    (setf (drover-remote--state-network state) (cons (float-time) (drover-remote--network)))
    ;; One, whatever remote layer made it before.
    (cancel-function-timers #'drover-remote--look-out)
    (run-at-time drover-remote-lookout-interval drover-remote-lookout-interval #'drover-remote--look-out)
    (add-hook 'kill-emacs-hook #'drover-remote--kill-emacs)
    (setq drover-hosts-function #'drover-remote--host-names
          drover-host-label-function #'drover-remote--host-label
          drover-host-state-function #'drover-remote--host-state
          drover-links-remote-function #'drover-remote--open-conversation
          drover-clone-function #'drover-remote--clone
          drover-prompt-remote-functions drover-remote--prompt-functions
          drover-new-session-elsewhere-function #'drover-remote-new-session)
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
      ;; Restarting, the terminals stay shared for the layer to come.
      (if (equal kind "restart")
          (maphash (lambda (_id entry)
                     (when (plist-get entry :timer) (cancel-timer (plist-get entry :timer))))
                   (drover-remote--state-shares state))
        (drover-remote--unshare-all state))
      (maphash (lambda (connection _sub) (drover-remote--unsubscribe state connection))
               (copy-hash-table (drover-remote--state-subs state)))
      (when (drover-remote--state-started-server state)
        (spyglass-server-stop))))
  (dolist (connection (hash-table-keys drover-remote--oc-relayed))
    (drover-remote--oc-unrelay connection))
  (dolist (connection (hash-table-keys drover-remote--searching))
    (drover-remote--search-unserve connection))
  (dolist (id (hash-table-keys drover-remote--searches))
    (drover-remote--search-end id (list :failure "the remote layer stopped")))
  (dolist (xid (hash-table-keys drover-remote--clones))
    (drover-remote--clone-fail xid "the remote layer stopped"))
  (mapc #'drover-remote--export-drop (hash-table-keys drover-remote--exports))
  (dolist (connection (hash-table-keys drover-remote--cv-streams))
    (drover-remote--cv-drop connection))
  (clrhash drover-remote--jobs-followers)
  (when drover-remote--jobs-timer
    (cancel-timer drover-remote--jobs-timer)
    (setq drover-remote--jobs-timer nil))
  (remove-hook 'drover-schedule-changed-hook #'drover-remote--jobs-changed)
  (remove-hook 'opencode-state-updated-functions #'drover-remote--oc-updated)
  (when (fboundp 'spyglass-unregister-channel)
    (spyglass-unregister-channel drover-remote-channel))
  (remove-hook 'drover-changed-hook #'drover-remote--owner-changed)
  (when (featurep 'drover-remote-push)
    (drover-remote-push-stop))
  (remove-hook 'spyglass-error-functions #'drover-remote--spyglass-error)
  (cancel-function-timers #'drover-remote--look-out)
  (remove-hook 'spyglass-share-state-functions #'drover-remote--share-event)
  (remove-hook 'drover-unsent-functions #'drover-remote--unsent)
  (remove-hook 'spyglass-mirror-state-functions #'drover-remote--mirror-state)
  (remove-hook 'kill-emacs-hook #'drover-remote--kill-emacs)
  (setq drover-hosts-function nil
        drover-host-label-function nil
        drover-host-state-function nil
        drover-links-remote-function nil
        drover-clone-function nil
        drover-prompt-remote-functions nil
        drover-new-session-elsewhere-function nil
        drover-remote--state nil))

;;;###autoload
(define-minor-mode drover-remote-mode
  "Exchange sessions with the hosts of `drover-remote-peers'.
Each host tells the others the sessions its own backends follow, over
spyglass connections; lists here show those of the peers too, under a
backend per peer, their host in a column of its own, dimmed while that
host is not connected.  Recaps and previews come through the peers,
drover's actions on their sessions are performed by them as their
access allows, and opening a session mirrors its terminal, or shows
what its host tells of it.  Needs spyglass, with its channels, one
dropping a stream whose write failed, as those offering
`spyglass-reconnect' do; mirrors need one sharing with some peers only."
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
            (let ((restart (and drover-remote--state t)))
              (when restart (drover-remote--stop "restart"))
              (drover-start)
              (drover-remote--start restart)))
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
