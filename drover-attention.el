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
;;   mode line, as one count after a robot (`drover-attention-glyph'),
;;   in the face of the most urgent of them, as other indicators of the
;;   mode line show a count: a symbol, a space and the number.  It counts what lists show
;;   under `drover-show-subagents', and every subagent waiting for you.
;;   mouse-1 on it visits the first of them, mouse-3 chooses one.
;; - `drover-next' visits the session that needs you whose prompt cache
;;   expires soonest, or, when no cache is live, the first the list
;;   shows, a subagent waiting for you through the top of its tree;
;;   pressed again while you are still on that session, it walks
;;   through the others, in the order of the first press
;;   (`drover-attention-order'), those coming to need you meanwhile
;;   next, waiting for a session still opening rather than pass it,
;;   then starts a new round, as it does once that order is
;;   `drover-attention-round-lifetime' old.
;; - `drover-select' chooses any session with completion, grouped by
;;   status, those needing you first, in the order `drover-next' visits
;;   them.

;;; Code:

(require 'drover)
(require 'drover-core)
(require 'drover-ui)
(require 'opencode-look)
(require 'seq)
(require 'subr-x)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 17))
  (load "drover-core" nil t))

(defcustom drover-attention-echo t
  "Whether `drover-next' tells the place in the cycle of the session it visits.
Such as \"2/5\" in the echo area: the buffer visited shows the session."
  :type 'boolean
  :group 'drover)

(defcustom drover-attention-round-lifetime 300
  "Seconds a round of `drover-next' keeps the order of its first press.
A press once they passed begins a new round, in the order as it then
stands, even on the session last visited: meanwhile caches expire, and
the sessions visited may need you again.  Sessions coming to need you
during a round join it, after the session visited.  nil keeps a round
however old."
  :type '(choice (const :tag "As long as you stay on its sessions" nil)
                 (number :tag "Seconds"))
  :group 'drover)

(defcustom drover-attention-open-patience 10
  "Seconds `drover-next' waits for a session its backend fetches before showing.
Pressed again before the session visited shows, while the window shows
what it did, `drover-next' says it is still opening rather than visit
the next session, which the one fetched would then cover as it shows.
Once they passed, the open is taken to have failed, its backend saying
why, and the round goes on.  nil never waits."
  :type '(choice (const :tag "Never" nil)
                 (number :tag "Seconds"))
  :group 'drover)

;;;; Mode line

(defvar drover-attention--cycle nil
  "Keys of the sessions `drover-next' walks through in its round.
They are those needing the user at its first press, in the order then,
and, after the session visited, those that came to need the user since.")

(defvar drover-attention--position 0
  "Position in `drover-attention--cycle' of the session last visited.")

(defvar drover-attention--started nil
  "When `drover-next' fixed the order of its round, as `float-time' tells, or nil.")

(defvar drover-attention--visited nil
  "Where `drover-next' last left the user, as (KEY WINDOW BUFFER BEFORE OPENING).
KEY is the session visited, WINDOW the window it was shown in, BUFFER
the buffer WINDOW showed once the session's backend had opened it, and
BEFORE the buffers WINDOW showed before, as `drover-attention--before'
tells them then.  OPENING is when the visit was made, as `float-time'
tells, when the session's backend was still fetching it, else nil.  nil
before the first visit.")

(defvar drover-attention-mode-line nil
  "The mode line construct of `drover-attention-mode', or nil.
It is computed when sessions change, so that drawing the mode line
costs nothing.")
;; Without it, the mode line drops the string's faces and mouse bindings.
(put 'drover-attention-mode-line 'risky-local-variable t)

(defvar-keymap drover-attention-mode-line-map
  :doc "Mouse bindings of the session counts in the mode line."
  "<mode-line> <mouse-1>" #'drover-next
  "<mode-line> <mouse-3>" #'drover-select)

(defvar drover-attention--mode-string-origin nil
  "What `global-mode-string' was before the mode made it a list holding its count.
nil when it was already a list, `empty' when it was nil, `string' when
it was a string; disabling the mode restores it.")

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
SESSIONS come in the order `drover-next' visits them, the order the
text lists them in (see `drover-attention-order'), their caches as they
stand at NOW.  It names `drover-attention--help-sessions' sessions at
most, and how many more: Emacs measures a tooltip whole to show it."
  (let ((count (length sessions)))
    (concat "Sessions that need you, in the order of `drover-next':"
            (mapconcat (lambda (session)
                         (concat "\n  " (drover-attention--describe session now)))
                       (seq-take sessions drover-attention--help-sessions)
                       "")
            (and (> count drover-attention--help-sessions)
                 (format "\n  … and %d more" (- count drover-attention--help-sessions)))
            "\n\nmouse-1: visit them in this order\nmouse-3: choose a session")))

(defvar drover-attention--pads (make-hash-table :test #'equal)
  "Room the glyph of the mode line needs, measured.
Keys are (GLYPH FACE FONT), FONT being `drover-attention--font-key';
values are (LEFT . GAP), as `drover-attention--measure' returns them.
The count is computed whenever sessions change, so the glyph is
measured once per font and face; a change of fonts empties the table.")

(defun drover-attention--font-key ()
  "Return what the fonts of the mode line of the selected frame depend on.
Those are its font and the frame's, not the frame: frames of the same
fonts draw glyphs alike, and a frame deleted leaves nothing behind."
  (list (face-font 'mode-line) (frame-parameter nil 'font)))

(defun drover-attention--measure (glyph face)
  "Return the room GLYPH in FACE needs in the mode line, as (LEFT . GAP).
LEFT is how far left of its start its ink reaches, 0 when it does not;
GAP the pixels from its advance to the count after it, when its ink
reaches past its advance further than a space, else nil, a space being
enough (see `drover-text-ink-extent').  Both are measured over the face
`mode-line', in the fonts of the selected frame.  The Nerd Fonts but
their Mono variants draw icons up to half a cell past their advance,
which a space of the same font covers, but not always a space of a
proportional one."
  (let* ((string (propertize glyph 'face (list face 'mode-line)))
         (extent (drover-text-ink-extent string))
         (overflow (- (cdr extent) (drover-text-pixel-width string)))
         (space (drover-text-pixel-width (propertize " " 'face (list face 'mode-line)))))
    (cons (max 0 (car extent)) (and (> overflow space) overflow))))

(defvar drover-attention--sessions nil
  "The sessions that need the user, as a hash table by key, or nil.
They are those of `drover-attention-sessions', kept current as
sessions change: a change of one session looks at that session and
those above it, not at every session.")

(defvar drover-attention--layout nil
  "The layout of the records of `drover-attention--sessions', or nil.
As `drover-layout' made it when they were found: records made by
another version of drover are found again (see
`drover-attention--current-p').")

(defun drover-attention--current-p ()
  "Return non-nil when the sessions needing the user are of the layout now.
Those are `drover-attention--sessions'; otherwise forget them, so that
they are found again."
  (or (drover-layout-matches-p drover-attention--layout)
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

(defun drover-attention--help-echo (&rest _)
  "Return the tooltip of the mode line count, from the sessions needing you now.
It is made when shown, however many the sessions are, rather than on
every change."
  (drover-with-reporting "mode line tooltip"
    (let ((now (float-time)))
      (drover-attention--help (drover-attention--current now) now))))

(defun drover-attention--format (sessions)
  "Return the mode line count of SESSIONS, the sessions that need the user.
It is `drover-attention-glyph', a space and their number, in the face
of the most urgent status among them, or nil when SESSIONS is empty.
The space widens, and room is left before the glyph, only where the
glyph's ink would reach the count or what comes before it (see
`drover-attention--measure'); the tooltip lists the sessions needing
the user in the order `drover-next' visits them, when shown (see
`drover-attention--help-echo')."
  (when sessions
    (let* ((face (opencode-session-status-face
                  (car (seq-sort-by #'drover-status-rank #'<
                                    (delete-dups (mapcar #'drover-session-status sessions))))))
           (glyph drover-attention-glyph)
           (key (list glyph face (drover-attention--font-key)))
           (room (or (gethash key drover-attention--pads)
                     (puthash key (drover-attention--measure glyph face) drover-attention--pads))))
      (propertize
       (concat (if (> (car room) 0)
                   (propertize " " 'display `(space :width (,(car room))))
                 "")
               glyph
               (if (cdr room)
                   (propertize " " 'display `(space :width (,(cdr room))))
                 " ")
               (number-to-string (length sessions)))
       'face face
       'help-echo #'drover-attention--help-echo
       'mouse-face 'mode-line-highlight
       'local-map drover-attention-mode-line-map))))

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
for the user, and its host, if another, is connected."
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
          drover-attention--layout (drover-layout 'drover-session))))

(defun drover-attention--follow (keys)
  "Bring the sessions that need the user up to date with the change of KEYS.
A session changing may change whether those above it show, so those
are looked at again too."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (key keys)
      (while (and key (not (gethash key seen)))
        (puthash key t seen)
        (let ((session (drover-index-session key)))
          (if (and session (drover-attention--needs-p key session))
              (puthash key session drover-attention--sessions)
            (remhash key drover-attention--sessions)))
        (setq key (drover-index-parent key))))))

(defun drover-attention--update (&optional keys)
  "Compute the mode line count again, and redraw mode lines if it changed.
With KEYS, the sessions changed, look at those alone, and those above
them, when the index can tell.  Changes come many times a second while
sessions work, most of them leaving the count as it was; forcing every
mode line of every frame to be drawn again costs tens of milliseconds
with many windows, so only a count that reads otherwise forces it.  Its
tooltip is a function, made when shown: the string compares alike
whatever the sessions it lists."
  (if (and keys (drover-attention--current-p) drover-attention--sessions (drover-index-incremental-p))
      (drover-attention--follow keys)
    (drover-attention--recount))
  (let ((string (drover-attention--format (hash-table-values drover-attention--sessions))))
    (unless (equal-including-properties string drover-attention-mode-line)
      (setq drover-attention-mode-line string)
      (force-mode-line-update t))))

(defun drover-attention--fonts-changed (&rest _)
  "Measure the glyphs of the mode line again, the fonts having changed.
For `after-setting-font-hook' and `enable-theme-functions'."
  (drover-with-reporting "mode line"
    (clrhash drover-attention--pads)
    (drover-attention--update)))

(defun drover-attention--changed ()
  "Follow a change of sessions, those `drover-changed-sessions' tells."
  (drover-with-reporting "mode line"
    (let ((changed drover-changed-sessions))
      (cond ((eq changed t) (drover-attention--update))
            (changed (drover-attention--update changed))))))

(defun drover-attention--show ()
  "Add the counts to `global-mode-string'."
  (unless (and (listp global-mode-string)
               (memq 'drover-attention-mode-line global-mode-string))
    (setq drover-attention--mode-string-origin
          (cond ((null global-mode-string)
                 ;; A list starting with a symbol is a conditional.
                 (setq global-mode-string (list ""))
                 'empty)
                ((stringp global-mode-string)
                 (setq global-mode-string (list global-mode-string))
                 'string)))
    (setq global-mode-string (append global-mode-string '(drover-attention-mode-line)))))

(defun drover-attention--hide ()
  "Remove the counts from `global-mode-string', restoring what it was."
  (when (listp global-mode-string)
    (setq global-mode-string (remq 'drover-attention-mode-line global-mode-string))
    (pcase drover-attention--mode-string-origin
      ('empty (when (equal global-mode-string '(""))
                (setq global-mode-string nil)))
      ('string (when (and (= (length global-mode-string) 1) (stringp (car global-mode-string)))
                 (setq global-mode-string (car global-mode-string))))))
  (setq drover-attention--mode-string-origin nil))

;;;###autoload
(define-minor-mode drover-attention-mode
  "Show in the mode line how many sessions need you.
The mode line holds one count of the sessions whose status is one of
`drover-attention-statuses' after `drover-attention-glyph' and a space,
in the face of the most urgent of them, and nothing when no session
needs you.  It begins with the glyph: parting it from what comes before
is the mode line's to do.  It
counts the sessions lists show under `drover-show-subagents', and every
subagent waiting for you, which blocks its parent (see
`drover-surfaced-sessions').  Its tooltip lists them in the order
`drover-next' visits them; clicking it visits the first, then the
next (`drover-next'), and clicking it
with the right button chooses one (`drover-select').  Enabling the mode
starts the backends."
  :global t
  :group 'drover
  (if drover-attention-mode
      (progn
        (drover-attention--show)
        (add-hook 'drover-changed-hook #'drover-attention--changed)
        (add-hook 'after-setting-font-hook #'drover-attention--fonts-changed)
        (add-hook 'enable-theme-functions #'drover-attention--fonts-changed)
        (drover-start)
        (drover-attention--update))
    (remove-hook 'drover-changed-hook #'drover-attention--changed)
    (remove-hook 'after-setting-font-hook #'drover-attention--fonts-changed)
    (remove-hook 'enable-theme-functions #'drover-attention--fonts-changed)
    (drover-attention--hide)
    (setq drover-attention-mode-line nil
          drover-attention--sessions nil
          drover-attention--cycle nil
          drover-attention--started nil
          drover-attention--visited nil)
    (force-mode-line-update t)))

;;;; Visiting the next session

(defvar drover-prompt-mode)

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

(defun drover-attention--visit (session position window)
  "Display SESSION, at POSITION in the cycle, in WINDOW, and select it.
Where it shows is remembered even when its backend fails to open it, so
that the next press goes on past it; the failure is told on one line,
as the place in the cycle is.  A session its backend fetches first, to
show it later, is remembered as opening (see
`drover-attention--opening-p')."
  (setq drover-attention--position position)
  (unless (eq window (selected-window))
    (select-window window))
  (let ((shown (window-buffer window))
        (failure nil)
        (returned nil))
    (unwind-protect
        (condition-case err
            (progn (drover-open session)
                   (setq returned t))
          (error (setq failure err)))
      (let* ((selected (selected-window))
             (buffer (window-buffer selected)))
        (setq drover-attention--visited
              (list (drover-session-key session) selected buffer (drover-attention--before selected)
                    (and returned
                         (eq selected window)
                         (eq buffer shown)
                         (drover-attention--fetched-p session)
                         (float-time))))))
    (cond
     (failure
      (drover-message "%s did not open: %s"
                      (drover-attention--place session)
                      (drover-error-message failure)))
     (drover-attention-echo
      (message "%d/%d" (1+ position) (length drover-attention--cycle))))))

(defun drover-attention--place (session)
  "Return SESSION's title, after its place in the cycle when told.
See `drover-attention-echo'."
  (let ((title (drover-bound-line (format "%s" (drover-session-display-title session)) 60)))
    (if drover-attention-echo
        (format "%d/%d %s" (1+ drover-attention--position) (length drover-attention--cycle) title)
      title)))

(defun drover-attention--fetched-p (session)
  "Return non-nil when SESSION, just opened, shows once its backend fetched it.
Its backend tells the buffers of its sessions, and SESSION has none yet."
  (and (functionp (drover-backend-property session :buffer))
       (not (drover-session-buffer session))))

(defun drover-attention--opening-p (window)
  "Return non-nil when the session last visited, in WINDOW, is still opening.
Its backend fetches it to show it later (see `drover-attention--visit'):
the visit was made less than `drover-attention-open-patience' seconds
ago, the session has no buffer yet, and WINDOW shows what the visit
left there, the user still on it (see `drover-attention--continues-p')."
  (pcase drover-attention--visited
    (`(,key ,visited ,left ,_ ,opening . ,_)
     (and (numberp opening)
          (numberp drover-attention-open-patience)
          (< (- (float-time) opening) drover-attention-open-patience)
          (eq window visited)
          (eq (window-buffer window) left)
          (drover-attention--continues-p window)
          (when-let* ((session (drover-index-session key)))
            (not (drover-session-buffer session)))))))

(defun drover-attention--before (window)
  "Return the buffers WINDOW showed before its buffer, the latest first.
They are those of `window-prev-buffers', which showing another buffer
in WINDOW and coming back changes, but not a menu shown and gone."
  (mapcar #'car (window-prev-buffers window)))

(defun drover-attention--session-buffer-p (buffer key)
  "Return non-nil when BUFFER is the session of KEY's own.
It is the session's buffer (see `drover-session-buffer'), or one that
tells that session as its own, such as the view of a session of another
host (see `drover-session-at-point-function'); the session at point of
a list is merely its row, and does not count."
  (or (when-let* ((session (drover-index-session key)))
        (eq buffer (drover-session-buffer session)))
      (with-current-buffer buffer
        (and drover-session-at-point-function
             (not (drover-session-list-p))
             (when-let* ((session (funcall drover-session-at-point-function)))
               (equal (drover-session-key session) key))))))

(defun drover-attention--shows-p (window key)
  "Return non-nil when WINDOW shows the session of KEY.
See `drover-attention--session-buffer-p'."
  (drover-attention--session-buffer-p (window-buffer window) key))

(defun drover-attention--left-p (window buffer before)
  "Return non-nil when BUFFER, in WINDOW, is where the last visit left the user.
BEFORE is the list of buffers WINDOW showed before BUFFER, the latest
first.  BUFFER is the session visited's own (see
`drover-attention--session-buffer-p'), however reached since; or, in
the window of the visit and while the session has no buffer of its
own, its backend fetching it first or having failed to open it, the
buffer the visit left there, as long as the user stayed on it: BEFORE,
without the buffer WINDOW shows, is then as it was."
  (pcase drover-attention--visited
    (`(,key ,visited ,left ,left-before . ,_)
     (or (drover-attention--session-buffer-p buffer key)
         (and (eq window visited)
              (eq buffer left)
              (not (when-let* ((session (drover-index-session key)))
                     (drover-session-buffer session)))
              (let ((shown (window-buffer window)))
                (equal (remq shown before) (remq shown left-before))))))))

(defun drover-attention--prompt-p (buffer)
  "Return non-nil when BUFFER is drover's prompt."
  (and (boundp 'drover-prompt-mode)
       (buffer-local-value 'drover-prompt-mode buffer)))

(defun drover-attention--stale-p (&optional now)
  "Return non-nil when the order of the round is too old to go on with at NOW.
See `drover-attention-round-lifetime'; a round of unknown age is."
  (and drover-attention-round-lifetime
       (or (not (numberp drover-attention--started))
           (>= (- (or now (float-time)) drover-attention--started)
               drover-attention-round-lifetime))))

(defun drover-attention--continues-p (window)
  "Return non-nil when `drover-next' goes on with its round in WINDOW.
WINDOW is where it shows sessions.  The round goes on while its order is
recent enough (see `drover-attention-round-lifetime') and the user is
still where the last visit left them (see `drover-attention--left-p'):
on the session visited, or the buffer the visit showed, or on the
prompt they went to from there, in that window.  From a side window,
such as the prompt's, WINDOW is the one they came from (see
`drover-attention--window').  Commands run meanwhile do not matter, a
menu opened to press the key again nor an answer typed to the session:
having left the session for another buffer starts a new round, and so
does the prompt reached from another buffer."
  (and drover-attention--cycle
       drover-attention--visited
       (not (drover-attention--stale-p))
       (let ((buffer (window-buffer window))
             (before (drover-attention--before window)))
         (or (drover-attention--left-p window buffer before)
             (and (drover-attention--prompt-p buffer)
                  before
                  (drover-attention--left-p window (car before) (cdr before)))))))

(defun drover-attention--join ()
  "Let the sessions newly needing the user join the round, after the one visited.
They are the visits to make now (see `drover-attention--visits') that
the round does not hold yet, in the order as it stands, such as a
session whose turn ended since the first press: they come next, rather
than after a round the user may not finish."
  (let ((round (make-hash-table :test #'equal)))
    (dolist (key drover-attention--cycle)
      (puthash key t round))
    (when-let* ((new (seq-keep (lambda (visit)
                                 (let ((key (drover-session-key (car visit))))
                                   (unless (gethash key round)
                                     key)))
                               (drover-attention--visits
                                (drover-attention-order (drover-attention-sessions))))))
      (let ((after (1+ drover-attention--position)))
        (setq drover-attention--cycle (append (seq-take drover-attention--cycle after)
                                              new
                                              (nthcdr after drover-attention--cycle)))))))

(defun drover-attention--following ()
  "Return the session after the last one visited, and its position, as a cons.
The sessions that came to need the user since the first press come
first (see `drover-attention--join'); sessions gone since are skipped.
Return nil when none is left after it, the round being over."
  (drover-attention--join)
  (let ((sessions (drover-sessions)))
    (seq-some (lambda (position)
                (let ((key (nth position drover-attention--cycle)))
                  (when-let* ((session (seq-find (lambda (session)
                                                   (equal (drover-session-key session) key))
                                                 sessions)))
                    (cons session position))))
              (number-sequence (1+ drover-attention--position)
                               (1- (length drover-attention--cycle))))))

(defun drover-attention--visits (sessions)
  "Return the visits of SESSIONS, needing the user, in the order to make them.
Each is (SESSION . WAITING): SESSION is the session to show for some of
SESSIONS, their `drover-attention-target', once, in the place of the
first of them, so that a subagent waiting is answered from the top of
its tree, which comes once however many of its tree need the user;
WAITING is non-nil when one of them waits for the user."
  (let ((visits nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((target (drover-attention-target session))
             (key (drover-session-key target))
             (visit (gethash key seen)))
        (unless visit
          (setq visit (list target))
          (puthash key visit seen)
          (push visit visits))
        (when (eq (drover-session-status session) 'waiting)
          (setcdr visit t))))
    (nreverse visits)))

(defun drover-attention--start (window)
  "Begin a round of `drover-next', showing its first session in WINDOW.
The round visits the sessions needing the user in the order as it
stands, a tree once (see `drover-attention--visits').  When WINDOW shows
the first already and others need the user, the round begins with the
second, unless the first waits for the user: shown, a session whose
turn ended was seen, even one its backend cannot mark seen, as one of
another host, which would otherwise come first again each time, but a
request still waits for its answer."
  (let* ((visits (drover-attention--visits (drover-attention-order (drover-attention-sessions))))
         (first (car visits))
         (position (if (and (cdr visits)
                            (not (cdr first))
                            (drover-attention--shows-p window (drover-session-key (car first))))
                       1
                     0)))
    (setq drover-attention--cycle (mapcar (lambda (visit) (drover-session-key (car visit))) visits)
          drover-attention--started (float-time))
    (if visits
        (drover-attention--visit (car (nth position visits)) position window)
      (setq drover-attention--visited nil)
      (message "No session needs you"))))

;;;###autoload
(defun drover-next ()
  "Visit the first session that needs you, in the selected window.
First come the sessions whose prompt cache lives, the one to expire
soonest first, so that a turn sent to it reuses it; then the others,
in the order the list and the sidebar show them, as `drover-group-by'
and `drover-sort-by' arrange them (see `drover-attention-order').  A
subagent waiting for you is visited through the top of its tree, where
you answer it and whose cache it ranks by, the tree coming once.
Pressed again while you are still on the session visited, whatever was
done in it meanwhile and however the command is reached, from
`drover-dispatch' or a key of its own, or from the prompt you went to
from it, it visits the following session, skipping those gone.  The
order is the one of the first press, so that sessions changing status
meanwhile, as the ones visited do, and caches expiring or renewed by a
turn keep their place; sessions that came to need you since, as one
whose turn just ended, come next, in the order as it stands.  Pressed
again before a session its backend fetches shows, it waits for it,
saying so, rather than visit the next one, which the session fetched
would cover once it shows (see `drover-attention-open-patience').  A
session that fails to open is told on one line; the next press goes on
past it.  After the last, once you left the session for another buffer,
or once the order is `drover-attention-round-lifetime' old, a new round
begins, in the order as it then stands, past the session shown if it
comes first and does not wait for you.  From a side window, such as the
prompt's, sessions show in the window you came from."
  (interactive)
  (let ((window (drover-attention--window)))
    (cond
     ((drover-attention--opening-p window)
      (when-let* ((session (drover-index-session (car drover-attention--visited))))
        (drover-message "%s is still opening" (drover-attention--place session))))
     ((when-let* ((following (and (drover-attention--continues-p window)
                                  (drover-attention--following))))
        (drover-attention--visit (car following) (cdr following) window)
        t))
     (t (drover-attention--start window)))))

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
subagents waiting for you, whatever it says (see
`drover-surfaced-sessions'): those that need you first, in the order
`drover-next' visits them (see `drover-attention-order'), then the
others, most urgent first.  Completion groups them by status.  With a
prefix argument OTHER-WINDOW, display it in another window."
  (interactive "P")
  (let* ((surfaced (drover-sort (drover-surfaced-sessions)))
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
  nil)

(provide 'drover-attention)
;;; drover-attention.el ends here
