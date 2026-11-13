;;; drover-attention.el --- Sessions that need you  -*- lexical-binding: t; -*-

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

;; Keeping up with sessions without opening their list:
;;
;; - `drover-attention-mode' counts the sessions that need you in the
;;   global status (see `global-status-mode'), as one entry: a robot
;;   (`drover-attention-glyph') and the count, in the state of the most
;;   urgent of them.  It counts what lists show under `drover-filter' and
;;   `drover-show-subagents', and every subagent waiting for you in a
;;   tree the filter keeps.  mouse-1 on it runs `drover-next', mouse-3
;;   chooses a session.
;; - `drover-next' lines up the sessions to attend to among those lists
;;   show: those whose prompt cache expires soonest first, whether they
;;   need you or not, then those that need you, then the idle ones with
;;   a buffer, then the other idle ones, each as the list shows them
;;   (`drover-visit-order'), a subagent waiting for you through the top
;;   of its tree, and visits the first, or the second when the first
;;   shows already; each press after it visits the next of that queue,
;;   the last followed by the first, until a command other than
;;   `drover-next' runs, as `undo' chains.
;; - `drover-select' chooses any session with completion, whatever the
;;   filter, grouped by status, those needing you first, in the order
;;   `drover-next' visits them among themselves.

;;; Code:

(require 'drover)
(require 'drover-core)
(require 'drover-ui)
(require 'global-status)
(require 'opencode-look)
(require 'seq)
(require 'subr-x)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 27))
  (load "drover-core" nil t))

(defcustom drover-attention-echo t
  "Whether `drover-next' tells the place in the round of the session it visits.
Such as \"2/5\" in the echo area: the buffer visited shows the session."
  :type 'boolean
  :group 'drover)

;;;; The count

(defun drover-attention--state (status)
  "Return the state of the global status for a count whose most urgent is STATUS.
A session waiting on a request, or whose turn ended unseen, waits on
you; one whose turn failed is an alert.  The other statuses count only
when added to `drover-attention-statuses': a turn under way is busy,
an idle session quiet."
  (pcase status
    ('failed 'alert)
    ((or 'waiting 'done) 'waiting)
    ((or 'working 'retrying) 'busy)
    (_ 'quiet)))

(defvar drover-attention--queue nil
  "Keys of the sessions the round of `drover-next' visits, in order, or nil.
They are the sessions of `drover-attention--round-sessions' at the
round's first press, a tree once, in the order then; nil when no round
runs.")

(defvar drover-attention--position 0
  "Position in `drover-attention--queue' of the session last visited.")

(defvar drover-attention--depth 0
  "The `recursion-depth' of the last press of `drover-next' in its round.")

(defun drover-attention--describe (session &optional now)
  "Return one line describing SESSION, for lists of sessions.
It is cut to `drover-line-limit' characters, its summary on one line.
A session of another host names it first; one whose prompt cache lives
at NOW, the current time by default, tells how long it has left (see
`drover-session-shown-cache-fraction')."
  (let ((location (drover-session-location session))
        (summary (drover-session-summary session))
        (host (drover-session-host session))
        (cache (and (drover-session-shown-cache-fraction session now)
                    (drover-session-cache-remaining session now))))
    (drover-bound-line
     (concat (opencode-session-status-glyph (drover-session-status session)) " "
             (and host (concat (propertize (drover-host-label host) 'face 'drover-dim) ": "))
             (format "%s" (drover-session-display-title session))
             (and location (concat "  " (propertize location 'face 'drover-dim)))
             (and cache (concat "  " (propertize (concat "cache " (opencode-format-duration cache))
                                                 'face 'drover-dim)))
             (and summary (concat "  " (string-replace "\n" " " (drover-bound-line summary))))))))

(defconst drover-attention--help-sessions 20
  "Sessions the tooltip of the count names at most.")

(defun drover-attention--help (sessions &optional now)
  "Return the help text of the mode line count of SESSIONS.
SESSIONS come in the order `drover-next' visits them among themselves,
the order the text lists them in (see `drover-attention-order'), their
caches as they stand at NOW.  It names `drover-attention--help-sessions'
sessions at most, and how many more: Emacs measures a tooltip whole to
show it.  While `drover-filter' is set, a line names it: the count, the
sessions listed and those `drover-next' visits are those it keeps.
Clicking it runs `drover-next', whose round may begin with an idle
session whose cache expires sooner, and goes on to idle sessions: the
last line says so."
  (let ((count (length sessions))
        (filter (drover-filter-description nil now)))
    (concat "Sessions that need you, in the order of `drover-next':"
            (mapconcat (lambda (session)
                         (concat "\n  " (drover-attention--describe session now)))
                       (seq-take sessions drover-attention--help-sessions)
                       "")
            (and (> count drover-attention--help-sessions)
                 (format "\n  … and %d more" (- count drover-attention--help-sessions)))
            (and filter (concat "\n\nFilter: " filter))
            "\n\nmouse-1: visit live caches first, then these, then idle sessions"
            "\nmouse-3: choose a session")))

(defvar drover-attention--sessions nil
  "The sessions that need the user, as a hash table by key, or nil.
They are those of `drover-attention-sessions', kept current as
sessions change: a change of one session looks at that session and
those above it, not at every session, or at its whole tree while
`drover-filter' is set (see `drover-attention--follow').")

(defvar drover-attention--layout nil
  "The layout of the records of `drover-attention--sessions', or nil.
As `drover-layout' made it when they were found: records made by
another version of drover are found again (see
`drover-attention--current-p').")

(defvar drover-attention--filter nil
  "The `drover-filter' the sessions of `drover-attention--sessions' follow.
Sessions are found again under another one (see
`drover-attention--current-p'), however it was set.")

(defun drover-attention--current-p ()
  "Return non-nil when the sessions needing the user are current.
Those are `drover-attention--sessions', current when of the layout now
and found under the filter now; otherwise forget them, so that they are
found again."
  (or (and (drover-layout-matches-p drover-attention--layout)
           (equal drover-attention--filter drover-filter))
      (setq drover-attention--sessions nil)))

(defun drover-attention--current (&optional now)
  "Return the sessions that need the user, in the order to visit them at NOW.
See `drover-attention-order'."
  (unless (drover-attention--current-p)
    (drover-attention--recount))
  (let ((sessions nil))
    (when drover-attention--sessions
      (maphash (lambda (_ session) (push session sessions)) drover-attention--sessions))
    (drover-attention-order (drover-sort sessions) now)))

(defun drover-attention--help-echo ()
  "Return the tooltip of the count, from the sessions needing you now.
It is made when shown, however many the sessions are, rather than on
every change."
  (drover-with-reporting "mode line tooltip"
    (let ((now (float-time)))
      (drover-attention--help (drover-attention--current now) now))))

(defun drover-attention--show (sessions)
  "Show the count of SESSIONS, the sessions that need the user, or hide it.
The entry `drover' of the global status shows their number, in the
state of the most urgent status among them (see
`drover-attention--state'); its tooltip lists them in the order
`drover-next' visits them, when shown (see
`drover-attention--help-echo').  The global status draws mode lines
again only when the number or the state changed."
  (if sessions
      (global-status-set 'drover (length sessions)
                         (drover-attention--state
                          (car (seq-sort-by #'drover-status-rank #'<
                                            (delete-dups (mapcar #'drover-session-status sessions)))))
                         #'drover-attention--help-echo)
    (global-status-clear 'drover)))

(defun drover-attention--define (&optional icon)
  "Define the entry `drover' of the global status: its ICON and clicks.
ICON defaults to `drover-attention-glyph'."
  (global-status-define 'drover :icon (or icon drover-attention-glyph) :order 60
                        :mouse-1 #'drover-next :mouse-3 #'drover-select))

(defun drover-attention--active-below-p (key)
  "Return non-nil when session KEY or a subagent below it is mid-turn.
That is `drover-tree-active-p' of its tree, through the index."
  (let ((stack (list key))
        (active nil))
    (while (and stack (not active))
      (let* ((key (pop stack))
             (session (drover-index-session key)))
        (if (and session (drover-session-active-p session))
            (setq active t)
          (setq stack (append (drover-index-children key) stack)))))
    active))

(defun drover-attention--needs-p (key session)
  "Return non-nil when SESSION, of KEY in the index, needs the user and shows.
See `drover-surfaced-sessions': it is no subagent, or a subagent
`drover-show-subagents' shows, at the top of its tree or not, or waits
for the user, and its host, if another, is connected.  It does not
tell what `drover-filter' keeps, which depends on the whole tree (see
`drover-attention--follow-trees')."
  (and (drover-session-attention-p session)
       (not (drover-session-stale session))
       (or (eq (drover-session-status session) 'waiting)
           (null (drover-session-parent session))
           (pcase drover-show-subagents
             ('all t)
             ('none nil)
             (_ (drover-attention--active-below-p key))))))

(defun drover-attention--recount ()
  "Find again every session that needs the user, from the index."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (session (seq-filter #'drover-session-attention-p
                                 (drover-surfaced-sessions (drover-index-sessions))))
      (puthash (drover-session-key session) session table))
    (setq drover-attention--sessions table
          drover-attention--layout (drover-layout 'drover-session)
          drover-attention--filter drover-filter)))

(defun drover-attention--tree-surfaced (root)
  "Return the sessions of the tree of ROOT that drover points the user to.
They are those `drover-surfaced-sessions' finds among the sessions of
that tree alone, under `drover-filter', as a hash table from their keys
to their records.  It walks the tree in the index, which nests it as
`drover-tree' does, rather than making trees, filtering and sorting
them (see `drover-view-trees'): it runs whenever a session of the tree
changes while a filter is set, and a tree of a fan-out holds hundreds.
Going up, a session is kept when it or one below it matches, active
when it or one kept below it is mid-turn, and shown by the filter when
kept with no session above matching, but the top, which explains no
parent (see `drover--filter-trees'); going down, a subagent shows when
its parent does, it is kept, and `drover-show-subagents' or the filter
shows it, as `drover-tree-visible' says.  The top of a tree kept shows,
but a subagent there, its parent not listed, which shows as the mode
says.  And every subagent waiting in a tree kept surfaces, but not a
session of a host not connected."
  (let ((nodes (make-hash-table :test #'equal))
        (surfaced (make-hash-table :test #'equal)))
    (cl-labels ((up (key matched-above top)
                  (let* ((session (drover-index-session key))
                         (match (drover-filter-match-p session))
                         (kept match)
                         (active (drover-session-active-p session)))
                    (dolist (child (drover-index-children key))
                      (pcase-let ((`(,child-kept . ,child-active) (up child (or matched-above match) nil)))
                        (when child-kept
                          (setq kept t)
                          (when child-active
                            (setq active t)))))
                    (puthash key (list session kept active (and kept (not (or matched-above top)))) nodes)
                    (cons kept active)))
                (shown-p (key)
                  (pcase-let ((`(,_ ,kept ,active ,filtered) (gethash key nodes)))
                    (and kept
                         (or filtered
                             (pcase drover-show-subagents
                               ('all t)
                               ('none nil)
                               (_ active))))))
                (surface (key session)
                  (unless (drover-session-stale session)
                    (puthash key session surfaced)))
                (down (key)
                  (surface key (car (gethash key nodes)))
                  (dolist (child (drover-index-children key))
                    (when (shown-p child)
                      (down child)))))
      (when (car (up root nil t))
        (when (or (null (drover-session-parent (drover-index-session root)))
                  (shown-p root))
          (down root))
        (maphash (lambda (key node)
                   (when (eq (drover-session-status (car node)) 'waiting)
                     (surface key (car node))))
                 nodes)))
    surfaced))

(defun drover-attention--follow-trees (keys)
  "Bring the sessions that need the user up to date with KEYS, under a filter.
While `drover-filter' is set, the change of a session may change what
lists draw of its whole tree: a match keeps the top of its tree and the
way to it, and a session coming to match above a match hides it again
under `none' (see `drover-view-trees').  So each tree holding a session
of KEYS is looked at again whole (see `drover-attention--tree-surfaced'),
but a tree none of whose sessions needs the user nor was counted, as
while sessions work.  A session gone is forgotten; the tree it left
holds its parent, which the index tells changed too.  Return nil, for
the caller to find every session again, when the top of a tree has no
record in the index, which never links a session to a parent it does
not hold; t otherwise."
  (let ((roots (make-hash-table :test #'equal)))
    (dolist (key keys)
      (if (drover-index-session key)
          (puthash (drover-index-root key) t roots)
        (remhash key drover-attention--sessions)))
    (catch 'recount
      (maphash
       (lambda (root _)
         (unless (drover-index-session root)
           (throw 'recount nil))
         (let ((sessions (drover-index-tree-sessions root)))
           (when (seq-some (lambda (session)
                             (or (drover-session-attention-p session)
                                 (gethash (drover-session-key session) drover-attention--sessions)))
                           sessions)
             (let ((surfaced (drover-attention--tree-surfaced root)))
               (dolist (session sessions)
                 (let ((key (drover-session-key session)))
                   (if (and (drover-session-attention-p session) (gethash key surfaced))
                       (puthash key session drover-attention--sessions)
                     (remhash key drover-attention--sessions))))))))
       roots)
      t)))

(defun drover-attention--follow (keys)
  "Bring the sessions that need the user up to date with the change of KEYS.
A session changing may change whether those above it show, so those
are looked at again too; while `drover-filter' is set, its whole tree
is (see `drover-attention--follow-trees').  Return nil when the
sessions must be found again, every one; t otherwise."
  (if (drover-filter-active-p)
      (drover-attention--follow-trees keys)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (key keys)
        (while (and key (not (gethash key seen)))
          (puthash key t seen)
          (let ((session (drover-index-session key)))
            (if (and session (drover-attention--needs-p key session))
                (puthash key session drover-attention--sessions)
              (remhash key drover-attention--sessions)))
          (setq key (drover-index-parent key)))))
    t))

(defun drover-attention--update (&optional keys)
  "Count the sessions needing the user again, and show the count.
With KEYS, the sessions changed, look at those alone, and those above
them, or their trees while `drover-filter' is set, when the index can
tell.  Changes come many times a second while sessions work, most of
them leaving the count as it was; the global status draws mode lines
again only when the count or its state changed.  Its tooltip is a
function, made when shown, so that the sessions it lists change
nothing."
  (unless (and keys (drover-attention--current-p) drover-attention--sessions (drover-index-incremental-p)
               (drover-attention--follow keys))
    (drover-attention--recount))
  (drover-attention--show (hash-table-values drover-attention--sessions)))

(defun drover-attention--changed ()
  "Follow a change of sessions, those `drover-changed-sessions' tells."
  (drover-with-reporting "mode line"
    (let ((changed drover-changed-sessions))
      (cond ((eq changed t) (drover-attention--update))
            (changed (drover-attention--update changed))))))

;;;###autoload
(define-minor-mode drover-attention-mode
  "Show in the global status how many sessions need you.
The entry `drover' of the global status (see `global-status-mode')
shows `drover-attention-glyph' and the count of the sessions whose
status is one of `drover-attention-statuses', in the state of the most
urgent of them, and nothing when no session needs you.  It counts the
sessions lists show under `drover-filter' and `drover-show-subagents',
and every subagent waiting for you in a tree the filter keeps, which
blocks its parent (see `drover-surfaced-sessions').  Its tooltip lists
them in the order `drover-next' visits them among themselves; clicking
it runs `drover-next', which visits live caches first, then them, then
idle sessions, and clicking it with the right button chooses a session
\(`drover-select').  Enabling the mode starts the backends."
  :global t
  :group 'drover
  (if drover-attention-mode
      (progn
        (drover-attention--define)
        (add-hook 'drover-changed-hook #'drover-attention--changed)
        (drover-start)
        (drover-attention--update))
    (remove-hook 'drover-changed-hook #'drover-attention--changed)
    (global-status-remove 'drover)
    (drover-attention--end-round)
    (setq drover-attention--sessions nil)))

;;;; Visiting the next session

(defun drover-attention--window ()
  "Return the window `drover-next' shows sessions in.
It is `drover-session-window', but from a side window, such as the
prompt's, where no session shows: the window the user came from, the
one of its frame used most recently that may show a session (see
`drover--session-window-p'), if any."
  (let ((window (drover-session-window)))
    (or (and (window-parameter window 'window-side)
             (car (sort (seq-filter #'drover--session-window-p
                                    (window-list (window-frame window) 'nomini))
                        (lambda (a b) (> (window-use-time a) (window-use-time b))))))
        window)))

(defun drover-attention--place (session)
  "Return SESSION's title, after its place in the round when told.
See `drover-attention-echo'."
  (let ((title (drover-bound-line (format "%s" (drover-session-display-title session)) 60)))
    (if drover-attention-echo
        (format "%d/%d %s" (1+ drover-attention--position) (length drover-attention--queue) title)
      title)))

(defun drover-attention--visit (session position window)
  "Display SESSION, at POSITION in the round, in WINDOW, and select it.
Its place in the round is told (see `drover-attention-echo'); a session
its backend fails to open is told on one line, after that place, and
the next press goes on past it."
  (setq drover-attention--position position
        drover-attention--depth (recursion-depth))
  (unless (eq window (selected-window))
    (select-window window))
  (condition-case err
      (progn
        (drover-open session)
        (when drover-attention-echo
          (message "%d/%d" (1+ position) (length drover-attention--queue))))
    (error
     (drover-message "%s did not open: %s"
                     (drover-attention--place session)
                     (drover-error-message err)))))

(defun drover-attention--targets (sessions)
  "Return the sessions to visit for SESSIONS, needing the user, in order.
Each is the `drover-attention-target' of some of SESSIONS, once, in the
place of the first of them, so that a subagent waiting is answered from
the top of its tree, which comes once however many of its tree need the
user."
  (let ((targets nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((target (drover-attention-target session))
             (key (drover-session-key target)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push target targets))))
    (nreverse targets)))

(defun drover-attention--transient-command-p (command)
  "Return non-nil when COMMAND is one of Transient's own, which its menus run.
Such as `transient-quit-one', or `transient-undefined', which a menu
runs for a key it does not bind."
  (and (symbolp command)
       (string-prefix-p "transient-" (symbol-name command))
       (equal (file-name-base (or (symbol-file command 'defun) "")) "transient")))

(defun drover-attention--chains-p (command event)
  "Return non-nil when COMMAND, run by EVENT, goes on with `drover-next''s round.
COMMAND is `drover-next', however run: from a key, the mode line, the
menu or `execute-extended-command'.  Or it is the menu opened to press
it, `drover-dispatch', or one of Transient's commands, which the menu
runs as it shows, closes or refuses a key (see
`drover-attention--transient-command-p').  Or it runs on no intent of
the user's: EVENT presses a mouse button, whose release runs the
command of the click, such as the mode line's, or moves the mouse, or
switches frames, windows or focus.  Or no command ran, COMMAND being
nil, as when the command loop starts."
  (or (memq command '(nil drover-next drover-dispatch))
      (drover-attention--transient-command-p command)
      (memq 'down (event-modifiers event))
      (memq (car-safe event) '(mouse-movement switch-frame select-window focus-in focus-out move-frame))))

(defun drover-attention--end-round ()
  "End the round of `drover-next': its next press lines sessions up anew."
  (setq drover-attention--queue nil
        drover-attention--position 0)
  (remove-hook 'post-command-hook #'drover-attention--after-command))

(defun drover-attention--after-command ()
  "End the round of `drover-next' unless the command just run goes on with it.
On `post-command-hook' while a round runs: `this-command' is then the
command run, the one `execute-extended-command' ran or a menu's suffix,
and `last-command-event' the event that ran it (see
`drover-attention--chains-p').  Commands run in a minibuffer or another
recursive edit deeper than the last press are those of a command reading
its arguments, judged as it ends, such as `execute-extended-command'."
  (drover-with-reporting "drover-next"
    (unless (or (> (recursion-depth) drover-attention--depth)
                (drover-attention--chains-p this-command last-command-event))
      (drover-attention--end-round))))

(defun drover-attention--round-sessions (&optional unfiltered)
  "Return the sessions a round of `drover-next' lines up, most urgent first.
They are those of `drover-surfaced-sessions' that need the user, and
those at the top of their trees in the middle of no turn, idle: a
session working has nothing to answer until its turn ends, when it
comes to need the user, and a subagent is answered through its parent.
They are among those `drover-filter' keeps, but with UNFILTERED."
  (drover-sort (seq-filter (lambda (session)
                             (or (drover-session-attention-p session)
                                 (and (null (drover-session-parent session))
                                      (not (drover-session-active-p session)))))
                           (drover-surfaced-sessions nil unfiltered))))

(defun drover-attention--shows-p (window session)
  "Return non-nil when WINDOW shows SESSION already.
WINDOW shows SESSION's buffer (see `drover-session-buffer'), or a buffer
telling SESSION as its own (see `drover-session-at-point-function'),
such as the view of a session of another host, or its conversation in
another front end.  The session at point of a list is merely its row,
and does not count."
  (let ((buffer (window-buffer window)))
    (or (eq buffer (drover-session-buffer session))
        (when-let* ((shown (with-current-buffer buffer
                             (and (not (drover-session-list-p))
                                  (drover-session-at-point t)))))
          (or (equal (drover-session-key shown) (drover-session-key session))
              (when-let* ((engine (drover-session-engine-key session)))
                (equal (drover-session-engine-key shown) engine)))))))

(defun drover-attention--start (window)
  "Begin a round of `drover-next', visiting its first session in WINDOW.
The round's queue is the sessions of `drover-attention--round-sessions',
in the order as it stands (see `drover-visit-order'), a tree once (see
`drover-attention--targets').  When WINDOW shows its first already (see
`drover-attention--shows-p'), the round begins with the second, the
first coming last, whatever the first's status; alone, it is visited
again.  With none to visit, the echo area says so, and how many the
filter hides, if it hides some."
  (let ((targets (drover-attention--targets (drover-visit-order (drover-attention--round-sessions)))))
    (if (null targets)
        (let ((hidden (and (drover-filter-active-p)
                           (length (drover-attention--targets (drover-attention--round-sessions t))))))
          (drover-attention--end-round)
          (if (and hidden (> hidden 0))
              (message "No session to visit; the filter hides %d" hidden)
            (message "No session to visit")))
      (setq drover-attention--queue (mapcar #'drover-session-key targets))
      (add-hook 'post-command-hook #'drover-attention--after-command)
      (let ((position (if (and (cdr targets) (drover-attention--shows-p window (car targets))) 1 0)))
        (drover-attention--visit (nth position targets) position window)))))

(defun drover-attention--advance (window)
  "Visit the session after the one last visited in the round, in WINDOW.
The first follows the last; sessions gone since the round began are
passed over, silently.  Return nil, visiting none, when every session
of the round is gone."
  (let ((sessions (make-hash-table :test #'equal))
        (count (length drover-attention--queue)))
    (dolist (session (drover-sessions))
      (puthash (drover-session-key session) session sessions))
    (seq-some (lambda (step)
                (let ((position (% (+ drover-attention--position step) count)))
                  (when-let* ((session (gethash (nth position drover-attention--queue) sessions)))
                    (drover-attention--visit session position window)
                    t)))
              (number-sequence 1 count))))

;;;###autoload
(defun drover-next ()
  "Visit the next session to attend to, in the selected window.
A first press lines up the sessions and visits the first, or the
second when the first shows already, whatever its status, the first
then coming last.  First come those whose prompt cache lives, whether
they need you or not, the one to expire soonest first, so that a turn
sent to it reuses it; then those that need you; then the idle ones
with a buffer; then the other idle ones, which their backends may
resume to show them.  Each comes in the order the list and the sidebar
show them, as `drover-group-by' and `drover-sort-by' arrange them (see
`drover-visit-order').  Sessions in the middle of a turn are left out,
and so are those `drover-filter' hides, and subagents, but one waiting
for you in a tree the filter keeps, which is visited through the top
of its tree, where you answer it and whose cache it ranks by, the tree
coming once.

Each press after it visits the next session of that queue, the first
again after the last, for as long as nothing but this command runs
between presses, as `undo' chains: from a key of its own, the mode line
or `drover-dispatch', whose menu is part of the press (see
`drover-attention--chains-p').  Any other command ends the round, and
the next press lines sessions up anew.  What happens to sessions
meanwhile leaves the queue as it is: statuses changing, caches renewed
or expiring, sessions coming to need you.  A session gone is passed
over; one that fails to open is told on one line, and the next press
goes on past it.  With every session in the middle of a turn or
hidden by the filter, there is none to visit, and the echo area says
so, and how many the filter hides.  It tells the place in the
round otherwise, such as 2/5 (see `drover-attention-echo').  From a
side window, such as the prompt's, sessions show in the window you
came from."
  (interactive)
  (let ((window (drover-attention--window)))
    (unless (and drover-attention--queue
                 (drover-attention--advance window))
      (drover-attention--start window))))

;;;; Choosing a session

(defun drover-attention--names (sessions)
  "Return an alist (NAME . SESSION) of SESSIONS, in order, NAMEs unique.
NAME is the session's title; titles shared by several sessions are
followed by the session's identifier, and a counter if still needed."
  (let ((titles (make-hash-table :test #'equal))
        (taken (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (cl-incf (gethash (format "%s" (drover-session-display-title session)) titles 0)))
    (mapcar (lambda (session)
              (let* ((title (format "%s" (drover-session-display-title session)))
                     (base (if (> (gethash title titles) 1)
                               (format "%s (%s)" title (drover-session-id session))
                             title))
                     (name base)
                     (counter 1))
                (while (gethash name taken)
                  (setq counter (1+ counter)
                        name (format "%s <%d>" base counter)))
                (puthash name t taken)
                (cons name session)))
            sessions)))

(defun drover-attention--pad (string width)
  "Return STRING padded with spaces to WIDTH columns."
  (concat string (make-string (max 0 (- width (string-width string))) ?\s)))

(defun drover-attention--annotator (candidates)
  "Return the annotation function of CANDIDATES, an alist (NAME . SESSION).
Annotations line up in columns: status glyph, project and branch, age
of the status or of the last activity, and why the session has its
status."
  (let* ((width (apply #'max 0 (mapcar (lambda (candidate) (string-width (car candidate)))
                                       candidates)))
         (location-width (apply #'max 0 (mapcar (lambda (candidate)
                                                  (string-width (or (drover-session-location (cdr candidate)) "")))
                                                candidates)))
         (now (float-time)))
    (lambda (name)
      (when-let* ((session (cdr (assoc name candidates))))
        (let ((summary (drover-session-summary session)))
          (concat (drover-attention--pad "" (- width (string-width name)))
                  "  " (opencode-session-status-glyph (drover-session-status session)) " "
                  (propertize (drover-attention--pad (or (drover-session-location session) "")
                                                     location-width)
                              'face 'drover-dim)
                  "  "
                  (propertize (string-pad (opencode-format-age (or (drover-session-since session)
                                                                 (drover-session-updated session))
                                                             now)
                                          3 nil t)
                              'face 'drover-dim)
                  (and summary (concat "  " (propertize summary 'face 'completions-annotations)))))))))

(defun drover-read-session (prompt sessions)
  "Read one of SESSIONS with completion, PROMPT first, and return it.
Candidates keep the order of SESSIONS, grouped by status label, and
are annotated with each session's status, location, age and summary.
Their completion category is `drover-session'."
  (let* ((candidates (drover-attention--names sessions))
         (names (mapcar #'car candidates))
         (annotate (drover-attention--annotator candidates))
         (group (lambda (name transform)
                  (if transform
                      name
                    (opencode-session-status-label (drover-session-status (cdr (assoc name candidates)))))))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata (category . drover-session)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (group-function . ,group)
                                 (annotation-function . ,annotate))
                    (complete-with-action action names string predicate))))
         (name (completing-read prompt table nil t)))
    (or (cdr (assoc name candidates))
        (user-error "No session %s" name))))

;;;###autoload
(defun drover-select (&optional other-window)
  "Choose a session with completion and display it.
Sessions are those lists show, under `drover-show-subagents', and the
subagents waiting for you, whatever it says, as lists would show them
without `drover-filter': a name is looked for wherever the session is
\(see `drover-surfaced-sessions').  Those that need you come first, in
the order `drover-next' visits them (see `drover-attention-order'),
those the filter hides after those it keeps, then the others, most
urgent first.  Completion groups them by status.  With a prefix
argument OTHER-WINDOW, display it in another window."
  (interactive "P")
  (let* ((surfaced (drover-sort (drover-surfaced-sessions nil t)))
         (sessions (append (drover-attention-order
                            (seq-filter #'drover-session-attention-p surfaced))
                           (seq-remove #'drover-session-attention-p surfaced))))
    (unless sessions
      (user-error "No sessions"))
    (drover-open (drover-read-session "Session: " sessions) other-window)))

;;;; Catching up

(defcustom drover-catch-up-statuses '(done failed)
  "Statuses of the sessions `drover-mark-all-seen' marks seen."
  :type '(repeat symbol)
  :group 'drover)

;;;###autoload
(defun drover-mark-all-seen (&optional older-than)
  "Mark seen every session whose turn ended unseen, after confirmation.
Those are the sessions with a status in `drover-catch-up-statuses' whose
backend can mark them seen.  With a numeric prefix argument OLDER-THAN,
only sessions whose status began at least that many hours ago."
  (interactive "P")
  (let* ((hours (and older-than (prefix-numeric-value older-than)))
         (limit (and hours (- (float-time) (* hours 3600))))
         (sessions (seq-filter
                    (lambda (session)
                      (and (memq (drover-session-status session) drover-catch-up-statuses)
                           (drover-supports-p session :mark-seen)
                           (or (null limit)
                               (let ((since (drover-session-since session)))
                                 (and since (<= since limit))))))
                    (drover-sessions))))
    (cond
     ((null sessions) (message "No session to mark seen"))
     ((y-or-n-p (format "Mark %d session%s seen%s? "
                        (length sessions) (if (cdr sessions) "s" "")
                        (if hours (format " (ended %d hour%s ago or more)" hours (if (= hours 1) "" "s")) "")))
      ;; As a list: each is marked whatever happens to the others.
      (drover-mark-seen sessions)))))

(defun drover-attention-unload-function ()
  "Disable `drover-attention-mode' before unloading."
  (drover-attention-mode -1)
  (drover-attention--end-round)
  nil)

;; Reloaded with the mode on, count anew: the sessions counted before may
;; follow other rules.  A drover older than the global status showed the
;; count in `global-mode-string' and measured it on font changes: undo both.
(when (bound-and-true-p drover-attention-mode)
  (drover-with-reporting "mode line"
    (when (and (consp global-mode-string) (memq 'drover-attention-mode-line global-mode-string))
      (setq global-mode-string (remq 'drover-attention-mode-line global-mode-string)))
    (when (fboundp 'drover-attention--fonts-changed)
      (remove-hook 'after-setting-font-hook #'drover-attention--fonts-changed)
      (remove-hook 'enable-theme-functions #'drover-attention--fonts-changed))
    (drover-attention--define)
    (drover-attention--update)))

(provide 'drover-attention)
;;; drover-attention.el ends here
