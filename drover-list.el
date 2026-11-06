;;; drover-list.el --- The list of agent sessions  -*- lexical-binding: t; -*-

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

;; `drover-list' lists every agent session in a Magit-like buffer, in an
;; ordinary window; `drover-list-side' lists them in a buffer of its
;; own, the sidebar, in a side window beside your work, so that both
;; may show at once, each laid out for its own width.  Each keeps its
;; point; the list its expanded sessions and collapsed groups, the
;; sidebar folding nothing (`drover-list-sidebar-mode'); both list the
;; sessions `drover-filter' keeps, grouped as `drover-group-by' says and
;; sorted as `drover-sort-by' says, by default ungrouped, the sessions
;; running turns first, then the most recently active, none moving as
;; a turn streams; the View menu, \\`V', changes all three, for both.
;; In the list, each group is a section that \\`TAB' collapses, and each
;; session a section whose body recaps its last turn: what it waits for, the
;; error that ended it, the prompt and the agent's reply, as its backend
;; tells them (`drover-recap'), fetched as it expands, then the files all
;; its turns changed (`drover-changes'), and what its line leaves out or
;; cuts.  The keys of
;; `drover-session-map' act on the session at point; the digits open the
;; sessions at the top of their trees by number, 0 the last
;; (`drover-list-open-numbered').  The buffer shows
;; sessions, and how many each group holds; settings and keys are the
;; View menu's to show, how many sessions need you the mode line's
;; (`drover-attention-mode').
;;
;; A subagent is listed under its parent, its glyph under its parent's
;; title, and a parent and its subagents stay together: they go in the
;; group of the most urgent session among them.  Subagents are sections
;; of the group, like their parent, rather than sections inside their
;; parent's: Magit hides the whole body of a section, and showing a
;; parent's details must neither hide nor reveal its subagents.  Which
;; subagents show, all, the active ones or none, `drover-show-subagents'
;; says; whether shown or not, a session whose subagents work or wait
;; for you tells so beside its status (`opencode-subagent-icons').  The
;; colour of a title tells where its session is in Emacs: in a buffer
;; no window shows (`opencode-session-open'), in a window
;; (`opencode-session-shown'), in the window you work in
;; (`opencode-session-active'); as windows change, only the titles whose
;; place changed take another face.  Glyphs and faces are opencode.el's
;; (opencode-look.el), which its mode line and pickers draw with too.
;;
;; Working glyphs are animated (`opencode-session-status-animations'): a
;; timer changes the display of their characters alone, every frame as
;; wide as the widest, while a displayed list shows one, the same frame
;; as opencode.el's mode lines at a time.  Every glyph takes the room
;; its font draws it in, which an icon font may draw wider than the
;; advance it gives it, so that no text is drawn on an icon.
;;
;; What a line shows depends on the width of the window, measured in
;; pixels in the face `drover-list', variable pitch by default.
;; `drover-list-fields' names the fields a line may show, by priority,
;; with the room each needs: the widest windows show every field as
;; aligned columns, narrower ones drop the fields of least priority,
;; and the narrowest, such as a side window, show two lines per session:
;; status, title and age, and what the session waits for under them.
;; The layout is computed anew whenever the width changes, among the
;; fields the user did not hide (`drover-list-hidden-fields').  Columns
;; are named in the header line, aligned to them.
;;
;; Besides the session's own fields, a line may show what it runs: its
;; model and effort, its provider, and its context, as a short bar of
;; how much of the model's context it fills and its size; and what its
;; turns changed.  The context costs a request per session to some
;; backends, the changes a request or a transcript read: the list asks
;; for them (`drover-prefetch') only for the sessions in sight.
;;
;; A session whose provider still holds its prompt cache shows how much
;; of the cache's lifetime is left: as a short bar and the time left in
;; columns, as a line under its line, from the left, in two lines.  Only
;; the bar and the time left are redrawn as the cache runs out.  A
;; subagent shows none: the user writes to its parent only, whose cache
;; is the one telling what the next turn costs.
;;
;; An active region selects the rows of sessions it touches, those
;; shown, and Magit highlights them; a group's heading selects nothing:
;; marking seen, interrupting, archiving, unarchiving and deleting act
;; on all of them, copying (`kill-ring-save', M-w) copies their Org
;; links, and the prompt quotes them with their last replies.
;;
;; With `drover-show-archived', the list shows archived sessions too,
;; dimmed, after `opencode-archived-icon', in a group of their own when
;; grouped by status.
;;
;; A buffer follows the changes backends report, but only while it is
;; displayed: a hidden list notes which sessions changed, and draws
;; them when it is next shown.  A change draws only what it changes, in
;; place: the lines of the sessions changed, a session moving to its new
;; place, coming or going, so that thousands of sessions cost no more
;; than a few, and the trees a change makes are made once for both
;; lists; a list is drawn whole when its columns or its settings change.
;; The sessions of a collapsed group are drawn once it is expanded,
;; searched or copied, or a few at a time while Emacs is idle.
;; A redraw keeps point on the same session, even when the session moved
;; to another group, keeps every section expanded or collapsed as it
;; was, and an active region active, on the same sessions.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'eieio)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'magit-section)
(require 'drover)
(require 'opencode-look)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 22))
  (load "drover-core" nil t))

;; A row's tree and section, a tree's group and a group's section were
;; slots once, read by the accessors of their structures, which leave
;; their compiler macros on these names.  Loaded over such a drover from
;; its sources, or compiled in an Emacs that ran one, the code below
;; would read the slots holding their links where these functions
;; follow them (`drover-link').
(eval-and-compile
  (dolist (function '(drover-list--row-tree drover-list--row-section
                      drover-list--tree-group drover-list--group-container))
    (function-put function 'compiler-macro nil)))

(defgroup drover-list nil
  "The list of agent sessions."
  :group 'drover
  :prefix "drover-list-")

(defface drover-list '((t :inherit variable-pitch))
  "Face of the text of the session list.
It inherits `variable-pitch', which fits more in a line; inherit
`fixed-pitch' instead for a monospaced list."
  :group 'drover-faces)

(defface drover-list-header '((t :inherit drover-list))
  "Face of the names of the columns, in the header line of the session list.
It goes over the face `header-line'; inheriting `drover-list', it draws
the names in the font of the columns they name."
  :group 'drover-faces)

(define-obsolete-face-alias 'drover-list-current 'opencode-session-active "0.1.0")

(defconst drover-list-buffer-name "*drover sessions*"
  "Name of the buffer of the list of `drover-list', in an ordinary window.")

(defconst drover-list-sidebar-buffer-name "*drover sidebar*"
  "Name of the buffer of the list of `drover-list-side', in a side window.
It is a buffer of its own, so that the sidebar and the list in an
ordinary window may show at once, each laid out for its width, with its
own point and its own sessions expanded.")

(defcustom drover-list-display-action
  '((display-buffer-reuse-window
     display-buffer-same-window
     drover-list-display-in-main-window))
  "Action by which `drover-list' displays its buffer in an ordinary window.
The default shows the list in the window of the selected frame already
showing it, else in the selected window, else, from a window that may
not show it, such as the sidebar's, in the window where you work, as
sessions opened from the sidebar show there (see
`drover-list-display-in-main-window').  See `display-buffer' for the
form of an action."
  :type 'sexp
  :group 'drover-list)

(defcustom drover-list-other-window-display-action
  '((display-buffer-reuse-window drover-list-display-in-other-window)
    (inhibit-same-window . t))
  "Action by which `drover-list' displays its buffer given a prefix argument.
The default shows the list in another window of the selected frame
already showing it, else in another window than the one
`drover-list-display-action' would choose (see
`drover-list-display-in-other-window').  See `display-buffer' for the
form of an action."
  :type 'sexp
  :group 'drover-list)

(defcustom drover-list-side-window-side 'left
  "Side of the frame on which `drover-list-side' shows the sidebar."
  :type '(choice (const left) (const right))
  :group 'drover-list)

(defcustom drover-list-side-window-width 36
  "Total width of the side window of `drover-list-side', in columns."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-side-window-slot 0
  "Slot of the sidebar among the side windows of its side.
See `display-buffer-in-side-window'."
  :type 'integer
  :group 'drover-list)

(defcustom drover-list-collapsed-groups '((status . idle))
  "Groups collapsed until expanded, as a list of (GROUPING . VALUE).
GROUPING is a value of `drover-group-by', VALUE that of the group, such
as (status . idle) for the idle sessions when grouping by status.  Once
a group is expanded or collapsed, the list remembers it: each list, the
sidebar and the list in an ordinary window, its own."
  :type '(repeat (cons symbol sexp))
  :group 'drover-list)

(defcustom drover-list-age-interval 60
  "Seconds between redraws of a displayed list, keeping ages current.
A value of nil disables these redraws: the list then redraws only when
sessions change.  The timer runs only while a list is displayed, Emacs
having focus or not."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-list)

(defcustom drover-list-cache-interval 5
  "Seconds between updates of the prompt cache bars of a displayed list.
Only the bars are redrawn.  A value of nil leaves them as last drawn.
The timer runs only while a displayed list shows a live cache, Emacs
having focus or not."
  :type '(choice (const :tag "Never" nil) (number :tag "Seconds"))
  :group 'drover-list)

(defcustom drover-list-cache-bar-width 5
  "Width of the short bar of the prompt cache field, in characters.
A character counts as wide as the letter x in the face `drover-list'."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-context-bar-width 5
  "Width of the short bar of the context field, in characters.
A character counts as wide as the letter x in the face `drover-list'."
  :type 'natnum
  :group 'drover-list)

(defcustom drover-list-deferred-delay 1
  "Seconds Emacs is idle before a list draws the sessions it left for later.
Drawing the list draws the sessions of its first screens at once, and
the others, those of collapsed groups and those past the first screens,
often most of them, only when they are expanded, searched, copied or
scrolled to, or once Emacs has been idle this long, so that other
packages searching the buffer find them.  A value of nil draws them
only when needed."
  :type '(choice (const :tag "Only when needed" nil) (number :tag "Seconds"))
  :group 'drover-list)

;; The pace of every animated icon, opencode.el's mode line's too.
(drover--adopt-option 'drover-list-animation-interval 'opencode-session-status-animation-interval)
(define-obsolete-variable-alias 'drover-list-animation-interval
  'opencode-session-status-animation-interval "0.2.0")

;; The pace of every animated icon while Emacs has no focus, opencode.el's
;; mode line's too.
(drover--adopt-option 'drover-list-animation-unfocused-interval
                      'opencode-session-status-animation-unfocused-interval)
(define-obsolete-variable-alias 'drover-list-animation-unfocused-interval
  'opencode-session-status-animation-unfocused-interval "0.2.0")

(defconst drover-list--former-fields
  '(((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (location :priority 8 :min 10 :max 30 :label "Project")
     (branch :priority 9 :min 6 :max 24 :label "Branch")
     (agent :priority 10 :min 5 :max 14 :label "Agent")
     (model :priority 5 :min 8 :max 22 :label "Model")
     (provider :priority 7 :min 2 :max 12 :label "Provider")
     (context :priority 6 :min 9 :label "Context")
     (created :priority 11 :min 3 :align right :label "Created")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :narrow icon :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :narrow icon :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (cost :priority 13 :min 4 :align right :label "Cost")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :narrow icon :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :narrow icon :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (cost :priority 13 :min 4 :align right :label "Cost")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (goal :priority 2 :min 3 :max 16 :narrow line :label "Goal")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :narrow icon :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :narrow icon :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (cost :priority 13 :min 4 :align right :label "Cost")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary"))
    ((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
     (goal :priority 2 :min 3 :max 16 :narrow line :label "Goal")
     (attempt :priority 2 :min 2 :max 20 :narrow line :label "Attempt")
     (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
     (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
     (location :priority 9 :min 10 :max 30 :narrow icon :label "Project")
     (branch :priority 10 :min 6 :max 24 :label "Branch")
     (agent :priority 11 :min 5 :max 14 :label "Agent")
     (model :priority 6 :min 8 :max 22 :label "Model")
     (provider :priority 8 :min 2 :max 12 :label "Provider")
     (context :priority 7 :min 9 :narrow icon :label "Context")
     (created :priority 12 :min 3 :align right :label "Created")
     (cost :priority 13 :min 4 :align right :label "Cost")
     (age :priority 1 :min 3 :align right :narrow line :label "Age")
     (cache :priority 3 :min 9 :narrow background :label "Cache")
     (summary :priority 4 :min 12 :narrow below :label "Summary")))
  "The former standard values of `drover-list-fields'.
Before the harness's field, then before the context's pie in two lines,
then before the cost, then before the pill of a task's project in two
lines, then before the goal, then before the attempt, then before the
changes.")

;; Loaded over a drover of a former standard value, which `defcustom'
;; would keep, the list takes the new one.
(when (and (boundp 'drover-list-fields) (member drover-list-fields drover-list--former-fields))
  (makunbound 'drover-list-fields))

(defcustom drover-list-fields
  '((title :priority 0 :min 12 :max 40 :narrow line :label "Session")
    (goal :priority 2 :min 3 :max 16 :narrow line :label "Goal")
    (attempt :priority 2 :min 2 :max 20 :narrow line :label "Attempt")
    (host :priority 2 :min 4 :max 16 :narrow line :label "Host")
    (harness :priority 5 :min 2 :max 14 :narrow line :label "Harness")
    (location :priority 9 :min 10 :max 30 :narrow icon :label "Project")
    (branch :priority 10 :min 6 :max 24 :label "Branch")
    (agent :priority 11 :min 5 :max 14 :label "Agent")
    (model :priority 6 :min 8 :max 22 :label "Model")
    (provider :priority 8 :min 2 :max 12 :label "Provider")
    (context :priority 7 :min 9 :narrow icon :label "Context")
    (changes :priority 8 :min 4 :align right :label "Changes")
    (created :priority 12 :min 3 :align right :label "Created")
    (cost :priority 13 :min 4 :align right :label "Cost")
    (age :priority 1 :min 3 :align right :narrow line :label "Age")
    (cache :priority 3 :min 9 :narrow background :label "Cache")
    (summary :priority 4 :min 12 :narrow below :label "Summary"))
  "Fields a line of the session list may show, in the order of columns.
Each entry is (FIELD . PROPERTIES).  FIELD is one of:

 title     The status icon and the title; it is always shown.
 goal      The goal the session works under, if any (see
           `drover-list-goal-function'): its glyph, the iterations
           run of their limit, and how it stands.
 attempt   The attempt the session works in, of a task worked on in
           several places, such as a fan-out's (see `drover-fanout'):
           its number, and the harness and model it was given.
 host      The host of a session of another host, and how long ago it
           was last heard from while it is not connected; in two lines,
           an icon too (see `drover-host-icon'), and in the sidebar,
           its icon alone, its tooltip naming the host and telling how
           it stands; nothing for this host's, so that a list of them
           has no such column.
 harness   What runs the session (see `drover-harnesses'): its icon and
           name, its icon alone where room is short, as in two lines.
 location  The project, as views show it (see
           `drover-session-view-project'): that of the task the
           session serves in its colour, after its pill, which alone
           shows in two lines, in a cell, and nothing for another.
 branch    The Git branch, greyed out when it is the default branch
           of its repository (see `drover-session-default-branch-p').
 agent     The agent, as @name, in the agent's colour.
 model     The model, compact, and the effort it runs, dimmed when it
           is the model's default.
 provider  The provider serving the model, often as an icon.
 context   The size of the context, and a short bar of how much of the
           model's context it fills, in a warning face near a
           long-context price tier; in two lines, a pie filling by
           eighths (see `drover-context-pie-icons').
 changes   What the session changed in all its turns, as its engine
           recorded them: the lines added and removed and the files,
           such as \"+120 −14 · 3 files\" (see `drover-changes'), read
           once its turns end; nothing until read.  Its expanded
           session lists the files.
 created   How long ago the session was created.
 cost      What the session cost, its subagents included, as the
           engines recorded it (see `drover-usage'); nothing until
           its usage is read.
 age       How long ago it last did something, or since when it needs
           you.
 cache     How much of the lifetime of its prompt cache is left: a
           short bar and the time left; nothing for a subagent.
 summary   What it waits for, or one line on its status.

PROPERTIES is a plist of:

 :priority  Rank of the field, 0 first; fields of a lower rank show
            only when every field of a higher one does.
 :min       Fewest characters the field needs to be shown.  Fields of
            a short text, the ages, context, cache and provider, are
            never cut, and need the room of their text at least; the
            harness needs that of its icon.
 :max       Most characters it takes before being cut; nil lets it
            take what is left of the line.
 :align     `right' to align the field on the right of its column.
 :narrow    Where the field goes when the list is too narrow for
            columns: `line', on the line of the title, flush right,
            the harness by its icon alone and the host with its icon,
            alone in the sidebar,
            icons in cells lining them up, the harness's last; `icon',
            as an icon alone, in such a cell before those, such as the
            context's pie, a field the columns need not show for the
            list to use them; `below', on a second line, under the
            title, for the sessions that need you; `background', as
            the background of the line; nil, not shown.
 :label     The name of the field's column in the header line.

Characters count as wide as the letter x in the face `drover-list'.
The list shows the fields with something to show, by priority, as long
as their :min fit in the window, but those of
`drover-list-hidden-fields': the summary and the prompt cache, which
come and go with turns, count as shown whenever a session listed may
show them, and the title's room counts from the glyph of a session
alone, so that a turn changes no field shown.  It uses columns when
those include every field the narrow layout places but as an icon,
else the narrow layout, which shows all of those.  Columns are named in
the header line, and are at least as wide as their name, room allowing.
On a line too narrow for every column, the summary gives up room first,
then the names of the columns of short texts, then the title, then the
other fields in proportion; the model keeps its effort, cutting its
name, and the harness its icon, leaving its name out.
In two lines, the title's line ends with the texts placed on it, such
as the host's name, but in the sidebar, and the age, then the context's
pie, the host's icon and the harness's, in this order, each in a cell
as wide as the widest the rows show, so that icons line up; a cell is
kept while a row listed shows something in it, the line of a row
without leaving it empty.  The sidebar's columns place the host's icon
and the harness's so too, after the age and the prompt cache."
  :type '(repeat (cons symbol plist))
  :group 'drover-list)

;; Loaded over a drover hiding the cost by default, which `defcustom'
;; would keep, the list shows it.
(when (and (boundp 'drover-list-hidden-fields) (equal drover-list-hidden-fields '(cost)))
  (makunbound 'drover-list-hidden-fields))

(defcustom drover-list-hidden-fields nil
  "Fields of `drover-list-fields' the list never shows.
The list chooses among the other fields by the room they take; the
title always shows.  The View menu, \\<drover-session-map>\\[drover-view] in the list,
changes it for the session, with one switch per field."
  :type '(set (const :tag "Goal" goal)
              (const :tag "Attempt" attempt)
              (const :tag "Host" host)
              (const :tag "Harness" harness)
              (const :tag "Project" location)
              (const :tag "Branch" branch)
              (const :tag "Agent" agent)
              (const :tag "Model and effort" model)
              (const :tag "Provider" provider)
              (const :tag "Context" context)
              (const :tag "Changes" changes)
              (const :tag "Creation" created)
              (const :tag "Cost" cost)
              (const :tag "Age" age)
              (const :tag "Prompt cache" cache)
              (const :tag "Summary" summary))
  :group 'drover-list)

(defconst drover-list-field-descriptions
  '((title . "The status of the session, the subagents at work under it, and its title")
    (goal . "The goal the session works under: iterations run of their limit, and how it stands")
    (attempt . "The attempt of its task the session works in, and the model it was given")
    (host . "The host the session runs on, if another; and since when it is not connected")
    (harness . "What runs the session: its front end, or the CLI that recorded it")
    (location . "The project the session works in")
    (branch . "The branch the session works on, greyed out when its repository's default one")
    (agent . "The agent the session runs")
    (model . "The model the session runs, and its effort: dimmed when the model's default")
    (provider . "The provider serving the model")
    (context . "The size of the session's context, and how much of the model's it fills")
    (changes . "What the session changed in all its turns: lines added and removed, and files")
    (created . "How long ago the session was created")
    (cost . "What the session cost, its subagents included: included when a subscription pays for it")
    (age . "How long ago the session last did something, or since when it needs you")
    (cache . "How long the provider keeps the conversation cached: the next turn sent before then costs less")
    (summary . "What the session waits for, or how its turn stands"))
  "What each field of `drover-list-fields' shows.
The header line's tooltips tell it.")

(defconst drover-list--gap 2
  "Characters between two columns.")

(defconst drover-list--margin 2
  "Characters before the sessions at the top of a tree.")

(defconst drover-list--max-depth 3
  "Deepest nesting level indented further than the one above it.
Deeper subagents line up with that level, so that titles keep room in a
narrow window.")

;;;; The model of the list

;; A drawing keeps what it drew: the groups, their trees, the rows of
;; their sessions, the summary of the rows the layout follows, and the
;; counts of each group.  A change of a few sessions then redraws their
;; lines in place, when it moves no tree (see `drover-list--follow'),
;; and a redraw reuses the rows of the sessions that did not change
;; (`drover-list--reuse').

;; The structures come before the code using them: their accessors are
;; inlined where code is compiled, or loaded from its sources, and those
;; of another version, loaded before, would read other slots.

;; A row leads to its tree and a tree to its group, which leads to all its
;; trees and rows, and rows and groups lead to their sections, which lead
;; to the root section and from there to every section of the buffer:
;; held as they are, what prints of any of them, or of a closure holding
;; one, grew with the square of the sessions shown, or faster.  They are
;; held through links (`drover-link').

(cl-defstruct (drover-list--row (:constructor drover-list--row-create)
                                (:copier nil))
  "A session as its heading shows it, before the layout."
  (session nil :documentation "The `drover-session' shown.")
  (depth nil :documentation "Depth of the session in its tree.")
  (indent nil :documentation "Position of the status glyph, in pixels.")
  (icons nil :documentation "The glyphs before the title, and a space.")
  (title-start nil :documentation "Position of the title, in pixels.")
  (title nil :documentation "The title, without the glyph.")
  (state nil :documentation "Where the session is in Emacs, see `drover-list--title-state'.")
  (texts nil :documentation "Alist (FIELD . TEXT) of the other fields.")
  (cache nil :documentation "Fraction of the prompt cache's lifetime left, or nil.")
  (caching nil :documentation "Non-nil when it may show a prompt cache, see `drover-list--caching-p'.")
  (inputs nil :documentation "What the row was made of, see `drover-list--row-inputs'.")
  (measures nil :documentation "What the layout needs of it, see `drover-list--row-measures'.")
  (heading nil :documentation "Its heading as drawn, see `drover-list--heading', or nil.")
  (tree-link nil :documentation "A link to the `drover-list--tree' showing it, see `drover-list--row-tree'.")
  (section-link nil :documentation "A link to its section once drawn, see `drover-list--row-section'."))

(defun drover-list--row-tree (row)
  "Return the `drover-list--tree' showing ROW, or nil."
  (drover-linked (drover-list--row-tree-link row)))

(gv-define-setter drover-list--row-tree (tree row)
  (macroexp-let2 nil tree tree
    `(progn (setf (drover-list--row-tree-link ,row) (drover-link ,tree)) ,tree)))

(defun drover-list--row-section (row)
  "Return the section of ROW, once drawn, or nil."
  (drover-linked (drover-list--row-section-link row)))

(gv-define-setter drover-list--row-section (section row)
  (macroexp-let2 nil section section
    `(progn (setf (drover-list--row-section-link ,row) (drover-link ,section)) ,section)))

(cl-defstruct (drover-list--tree (:constructor drover-list--tree-create)
                                 (:copier nil))
  "A tree of sessions the list shows, under the session at its top."
  (root nil :documentation "The key of the session at the top.")
  (node nil :documentation "The tree as drawn: its visible sessions, sorted.")
  (full nil :documentation "The whole tree, as `drover-tree' made it.")
  (cut nil :documentation "The tree as `drover-filter' cuts it, or nil without a filter.")
  (serial nil :documentation "The place of its top session in the sessions listed.")
  (group-link nil :documentation "A link to the `drover-list--group' showing it, see `drover-list--tree-group'.")
  (position nil :documentation "Its place among the trees of its group.")
  (shown nil :documentation "The rows of its visible sessions, in the order drawn."))

(defun drover-list--tree-group (tree)
  "Return the `drover-list--group' showing TREE, or, while it is placed, its value."
  (drover-linked (drover-list--tree-group-link tree)))

(gv-define-setter drover-list--tree-group (group tree)
  (macroexp-let2 nil group group
    `(progn (setf (drover-list--tree-group-link ,tree) (drover-link ,group)) ,group)))

(cl-defstruct (drover-list--group (:constructor drover-list--group-create)
                                  (:copier nil))
  "A group of trees the list shows."
  (value nil :documentation "(GROUPING . VALUE), or nil when sessions are not grouped.")
  (container-link nil :documentation "A link to its section, see `drover-list--group-container'.")
  (trees nil :documentation "Its trees, in the order drawn, a vector.")
  (head nil :documentation "Its most urgent tree, which places it among groups.")
  (counts nil :documentation "How many sessions of each status it shows, see `drover-list--count-row'.")
  (size 0 :documentation "How many sessions it shows.")
  (heading-text nil :documentation "Its heading as drawn."))

(defun drover-list--group-container (group)
  "Return the section of GROUP, the root section without groups, or nil."
  (drover-linked (drover-list--group-container-link group)))

(gv-define-setter drover-list--group-container (section group)
  (macroexp-let2 nil section section
    `(progn (setf (drover-list--group-container-link ,group) (drover-link ,section)) ,section)))

(cl-defstruct (drover-list--model (:constructor drover-list--model-create)
                                  (:copier nil))
  "What the list drew, to draw again only what changes."
  ;; First, whatever the version: see `drover-layout-current-p'.
  (stamp (drover-layout 'drover-list--model 'drover-list--group 'drover-list--tree
                        'drover-list--row 'drover-node 'drover-session)
         :documentation "The layouts of the types it was made of, as `drover-layout' tells them.")
  (settings nil :documentation "The settings it was drawn with, see `drover-list--settings'.")
  (groups nil :documentation "The `drover-list--group's, in their order.")
  (trees nil :documentation "Hash table of the trees drawn, by the key of their top session.")
  (rows nil :documentation "Hash table of the rows drawn, by session key.")
  (summary nil :documentation "The summary of the rows, see `drover-list--summarize'.")
  (layout nil :documentation "The layout of the rows, see `drover-list--layout-of'.")
  (size 0 :documentation "How many sessions it shows."))

(drover-note-layout 'drover-list--row 'drover-list--tree 'drover-list--group 'drover-list--model)

(defvar-local drover-list--model nil
  "The `drover-list--model' of the last drawing, or nil.")

;;;; Sections

(defvar drover-list--model)

(defvar-local drover-list--folds t
  "Non-nil when the current list folds its sections.
Its sessions then expand to recap their last turn, its groups collapse,
and every line begins with `drover-list--line-start'.  The sidebar
folds nothing (see `drover-list-sidebar-mode'): its lines begin with
their glyphs, at the window's edge.")

(defvar-keymap drover-list-group-section-map
  :doc "Keys on the heading of a group of sessions."
  "RET" #'magit-section-toggle
  ;; Magit's heading map binds these to toggle, which the sidebar's
  ;; remapping misses when its window is not selected.
  "<double-mouse-1>" #'drover-list-mouse-toggle-section
  "<double-mouse-2>" #'drover-list-mouse-toggle-section
  "<left-margin> <mouse-1>" #'drover-list-mouse-toggle-section)

(defun drover-list-mouse-toggle-section (event)
  "Toggle the section clicked, as Magit's `magit-mouse-toggle-section' does.
In the sidebar, which folds nothing, do nothing: its window need not be
selected, and the remapping of a command bound where a click lands then
misses the sidebar's keymap, which remaps Magit's command away.  EVENT
is the click."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (unless (and (window-live-p window)
                 (not (buffer-local-value 'drover-list--folds (window-buffer window))))
      (magit-mouse-toggle-section event))))

(defclass drover-list-group-section (magit-section)
  ((keymap :initform 'drover-list-group-section-map))
  "A group of sessions; the value is (GROUPING . VALUE).")

(defvar drover-list--no-keymap nil
  "The keymap of session sections: none, their lines taking that of headings.")

(defclass drover-list-session-section (magit-section)
  ;; Named, so that Magit does not look one up by the section's type,
  ;; formatting and interning two names per section drawn.
  ((keymap :initform 'drover-list--no-keymap))
  "One session; the value is its `drover-session-key'.
Its subagents follow it in its group, as sections of their own.")

(defun drover-list--session-section (section)
  "Return SECTION or its ancestor showing a session, or nil."
  (while (and section (not (object-of-class-p section 'drover-list-session-section)))
    (setq section (oref section parent)))
  section)

(defun drover-list--group-section (section)
  "Return SECTION or its ancestor showing a group of sessions, or nil."
  (while (and section (not (object-of-class-p section 'drover-list-group-section)))
    (setq section (oref section parent)))
  section)

(defun drover-list--groups ()
  "Return the group sections of the current list."
  (and magit-root-section
       (seq-filter (lambda (section) (object-of-class-p section 'drover-list-group-section))
                   (oref magit-root-section children))))

(defun drover-list--containers ()
  "Return the sections holding the sessions of the current list.
They are its groups, or its root section when sessions are not grouped."
  (and magit-root-section
       (or (drover-list--groups) (list magit-root-section))))

(defun drover-list--session-sections (container)
  "Return the session sections CONTAINER holds, a group or the root section."
  (seq-filter (lambda (section) (object-of-class-p section 'drover-list-session-section))
              (oref container children)))

(defun drover-list--container-id (container)
  "Return what identifies CONTAINER, a group or the root section, in a redraw.
It is `root' for the root section, else the value of the group."
  (if (eq container magit-root-section)
      'root
    (cons 'group (oref container value))))

(defun drover-list--find-container (id)
  "Return the section holding sessions identified by ID, or nil.
ID is a value of `drover-list--container-id'.  The root section holds
sessions only while they are not grouped."
  (if (eq id 'root)
      (and magit-root-section
           (null (drover-list--groups))
           (drover-list--session-sections magit-root-section)
           magit-root-section)
    (seq-find (lambda (section) (equal (oref section value) (cdr id)))
              (drover-list--groups))))

(defun drover-list--find-session (key)
  "Return the section showing the session whose key is KEY, or nil."
  (when-let* ((model drover-list--model)
              (row (gethash key (drover-list--model-rows model))))
    (drover-list--row-section row)))

(defun drover-list--session-at-point ()
  "Return the current record of the session at point, or nil.
The record comes from the backend, not from the last redraw, so actions
see the session as it stands now."
  (when-let* ((section (drover-list--session-section (magit-current-section)))
              (key (oref section value)))
    (drover-find (car key) (cdr key))))

;;;; Visibility

(defvar-local drover-list--visibility nil
  "Hash table of the visibility, `show' or `hide', of past sections.
Groups are keyed by (group GROUPING . VALUE), sessions by a cons of
`session' and their key: a session keeps its visibility when it moves
to another group.")

(defun drover-list--visibility-key (section)
  "Return the key of SECTION in `drover-list--visibility', or nil."
  (cond ((object-of-class-p section 'drover-list-session-section)
         (cons 'session (oref section value)))
        ((object-of-class-p section 'drover-list-group-section)
         (cons 'group (oref section value)))))

(defun drover-list--remember-visibility ()
  "Record the visibility of every section of the current list."
  (when magit-root-section
    (magit-map-sections
     (lambda (section)
       (when-let* ((key (drover-list--visibility-key section)))
         (puthash key (if (oref section hidden) 'hide 'show) drover-list--visibility))))))

(defun drover-list--forget-visibility (sessions &optional indexed)
  "Forget the visibility of sessions other than SESSIONS.
Sessions come and go, groups are few: only sessions are forgotten.
With INDEXED non-nil, SESSIONS are those of the index, which tells
which sessions it holds without a table of them."
  (let ((keys (if indexed
                  (drover--index-records (drover-index))
                (let ((keys (make-hash-table :test #'equal :size (length sessions))))
                  (dolist (session sessions)
                    (puthash (drover-session-key session) t keys))
                  keys))))
    (maphash (lambda (key _)
               (when (and (eq (car key) 'session)
                          (not (gethash (cdr key) keys)))
                 (remhash key drover-list--visibility)))
             drover-list--visibility)))

(defun drover-list--cached-visibility (section)
  "Return the recorded visibility of SECTION, or nil.
For `magit-section-set-visibility-hook'."
  (drover-with-reporting "list visibility"
    (when-let* ((key (drover-list--visibility-key section)))
      (gethash key drover-list--visibility))))

;;;; Measuring

(define-hash-table-test 'drover-list--text
  #'equal-including-properties #'sxhash-equal-including-properties)

(defvar-local drover-list--widths nil
  "Widths of texts measured, as (CONTEXT TABLE KNOWN PEERS).
CONTEXT is what widths depend on besides the text: a copy of the face
remapping of the buffer, which `face-remap-add-relative' changes in
place, and the frame measuring it.  TABLE maps texts, with their
properties, to their widths in pixels for the current drawing, and
conses (extent . TEXT) to their extents (see `drover-list--extent');
KNOWN is the table of the previous drawing in the same context, or nil;
PEERS the tables of the other lists measuring in the same context, as
the sidebar and the list in an ordinary window of one frame do.")

(defun drover-list--window ()
  "Return the window the current list is drawn for.
It is the selected window when it shows the list, else another window
showing it, else the selected window."
  (if (eq (window-buffer) (current-buffer))
      (selected-window)
    (or (get-buffer-window nil 0) (selected-window))))

(defun drover-list--measure-context ()
  "Return what the widths of texts depend on besides the texts themselves.
That is a copy of the face remapping of the buffer but for the highlight
of the row at point, which
`face-remap-add-relative' changes in place, the frame measuring them,
the fonts of the faces lines are drawn in, so that a face given another
font measures anew, and the functions measuring."
  (let ((frame (window-frame (drover-list--window))))
    ;; The sidebar's highlight, a background, measures nothing: the
    ;; lists measure alike with it.
    (list (copy-tree (assq-delete-all 'magit-section-highlight (copy-sequence face-remapping-alist)))
          frame
          (mapcar (lambda (face) (face-font face frame))
                  '(default drover-list variable-pitch fixed-pitch header-line bold))
          (drover-list--measurer))))

(defun drover-list--measurer ()
  "Return the functions measuring texts, which tests replace."
  (cons (symbol-function 'drover-list--pixel-width) (symbol-function 'drover-list--extent)))

(defvar-local drover-list--measuring 0
  "Generation of the widths of `drover-list--widths'.
It changes when texts measure otherwise than before, as with another
font: what was measured before, such as rows kept from one drawing to
the next, is measured anew.")

(defun drover-list--measure-begin ()
  "Start a drawing: widths measured for the last one stay known.
Laying text out costs more than the rest of a redraw, so widths are
reused while the faces and fonts stay the same, and taken from the
other lists measuring alike, which draw mostly the same texts."
  (let* ((context (drover-list--measure-context))
         (same (equal (car drover-list--widths) context)))
    (unless same
      (cl-incf drover-list--measuring))
    (setq drover-list--widths
          (list context
                (make-hash-table :test 'drover-list--text)
                (and same (nth 1 drover-list--widths))
                (drover-list--peer-widths context)))))

(defun drover-list--peer-widths (context)
  "Return the tables of widths of the other lists measuring in CONTEXT."
  (let ((peers nil))
    (dolist (buffer (drover-list--buffers))
      (unless (eq buffer (current-buffer))
        (when-let* ((widths (buffer-local-value 'drover-list--widths buffer))
                    ((equal (car widths) context)))
          (dolist (table (list (nth 1 widths) (nth 2 widths)))
            (when table
              (push table peers))))))
    peers))

(defun drover-list--measure (string)
  "Return the width in pixels of STRING, laid out as the list draws it."
  (drover-text-pixel-width string face-remapping-alist (drover-list--window)))

(defun drover-list--pixel-width (string)
  "Return the width in pixels of STRING drawn in the current buffer.
The width is that of the fonts of `drover-list--window' with the face
remapping of the buffer, which draws its text in the face `drover-list'.
On a text terminal, a pixel is a character cell."
  (if (zerop (length string))
      0
    (drover-list--memo string #'drover-list--measure string)))

(defvar drover-list--keep-widths t
  "Non-nil when `drover-list--memo' keeps what it computes.
Nil while texts measured once only are measured, such as the candidates
of a cut.")

(defun drover-list--memo (key compute argument)
  "Return what is known of KEY in `drover-list--widths', else COMPUTE of ARGUMENT.
COMPUTE is a function of one argument, whose value is then known for
the current drawing, unless `drover-list--keep-widths' is nil.  KEY is
compared with its text properties.  Nothing is allocated when KEY is
known: a drawing asks for tens of thousands of widths."
  (unless (and drover-list--widths
               (let ((measurer (nth 3 (car drover-list--widths))))
                 (and (eq (car measurer) (symbol-function 'drover-list--pixel-width))
                      (eq (cdr measurer) (symbol-function 'drover-list--extent)))))
    (drover-list--measure-begin))
  (pcase-let ((`(,_ ,table ,known ,peers) drover-list--widths))
    (or (gethash key table)
        (let ((value (and known (gethash key known))))
          (while (and peers (not value))
            (setq value (gethash key (pop peers))))
          (unless value
            (setq value (funcall compute argument)))
          (if drover-list--keep-widths (puthash key value table) value)))))

(defun drover-list--char-width ()
  "Return the width in pixels of an average character, the letter x."
  (drover-list--pixel-width "x"))

(defun drover-list--extent (string)
  "Return how far STRING reaches drawn in the current buffer, as (LEFT . RIGHT).
See `drover-text-ink-extent': RIGHT is the further of its advance and
its ink, LEFT how far left of its start its ink reaches, in the fonts
of `drover-list--window' with the face remapping of the buffer.
Extents are kept as widths are, in `drover-list--widths'."
  (if (zerop (length string))
      (cons 0 0)
    (drover-list--memo (cons 'extent string) #'drover-list--measure-extent string)))

(defun drover-list--measure-extent (string)
  "Return how far STRING reaches drawn in the current buffer, measured anew."
  (drover-text-ink-extent string face-remapping-alist (drover-list--window)))

(defun drover-list--line-width ()
  "Return the width in pixels that lines of the list may take.
Like `window-max-chars-per-line', leave out line numbers, and the
column Emacs keeps for the glyph telling a line is cut when the window
has no fringe for it (see `drover-list--truncation-width')."
  (let* ((window (drover-list--window))
         (fringes (window-fringes window))
         (width (window-body-width window t)))
    (when (buffer-local-value 'display-line-numbers (window-buffer window))
      (setq width (- width (with-selected-window window (line-number-display-width t)))))
    (if (and (display-graphic-p (window-frame window))
             overflow-newline-into-fringe
             (not (eq (car fringes) 0))
             (not (eq (cadr fringes) 0)))
        width
      (- width (max (window-font-width window) (drover-list--truncation-width window))))))

(defun drover-list--truncation-width (window)
  "Return the width in pixels of the glyph WINDOW shows at the end of a cut line.
Without a right fringe, Emacs keeps that much room at the right edge of
every line of a window that truncates lines, and a line reaching into
it shows the glyph, $ unless the display table says otherwise, in the
default face as the buffer remaps it: the face `drover-list', whose $ may
be wider than its average character, which `window-font-width' gives.
0 on a frame showing no such glyph."
  (if (frame-parameter (window-frame window) 'no-special-glyphs)
      0
    (let* ((table (or (window-display-table window)
                      (buffer-local-value 'buffer-display-table (window-buffer window))
                      standard-display-table))
           (glyph (and table (display-table-slot table 'truncation)))
           (glyph (and (or (natnump glyph) (consp glyph)) glyph))
           (char (if glyph (glyph-char glyph) ?$))
           (face (and glyph (glyph-face glyph))))
      (drover-list--pixel-width (if face (propertize (string char) 'face face) (string char))))))

(defun drover-list--kept (string)
  "Return where the tail of STRING to keep whole begins, or nil without one.
The tail is what the property `drover-list-keep' marks at its end, such
as the effort after a model."
  (let ((end (length string)))
    (when (and (> end 0) (get-text-property (1- end) 'drover-list-keep string))
      (let ((start (or (previous-single-property-change end 'drover-list-keep string) 0)))
        (and (> start 0) start)))))

(defun drover-list--kept-head (string)
  "Return where the head of STRING to keep whole ends, or nil without one.
The head is what the property `drover-list-keep' marks at its start,
not at its end, such as the icon of a harness before its name: cut,
STRING is its head alone (see `drover-list--compact')."
  (let ((end (length string)))
    (when (and (> end 0)
               (get-text-property 0 'drover-list-keep string)
               (not (get-text-property (1- end) 'drover-list-keep string)))
      (next-single-property-change 0 'drover-list-keep string end))))

(defun drover-list--compact (string)
  "Return STRING as a line too short for it shows it: its head kept, if any.
See `drover-list--kept-head'; STRING itself without one."
  (if-let* ((head (drover-list--kept-head string)))
      (substring string 0 head)
    string))

(defun drover-list--truncate (string pixels)
  "Return STRING cut to at most PIXELS wide, ending in an ellipsis if cut.
The ellipsis takes the properties of the text before it.  A tail of
STRING marked to keep (see `drover-list--kept') stays whole, its head
being cut, room allowing; a head marked to keep (see
`drover-list--kept-head') is all that shows, without an ellipsis.
Return \"\" when not even the ellipsis fits, or the head.
What a string is cut to is known as its widths are, so that drawing
the same text in the same room again measures nothing."
  (let ((pixels (max 0 pixels)))
    (if (<= (drover-list--pixel-width string) pixels)
        string
      (drover-list--memo (list 'cut pixels string) #'drover-list--cut-cell (cons string pixels)))))

(defun drover-list--cut-cell (cell)
  "Return the string of CELL, (STRING . PIXELS), cut to fit PIXELS."
  (drover-list--cut (car cell) (cdr cell)))

(defun drover-list--cut (string pixels)
  "Return STRING, wider than PIXELS, cut to fit, as `drover-list--truncate' does.
The candidates measured on the way are not kept: what STRING is cut to
in PIXELS is, by `drover-list--truncate'."
  (let ((keep (drover-list--kept string))
        (head (drover-list--kept-head string))
        (measure (lambda (candidate)
                   (let ((drover-list--keep-widths (eq candidate string)))
                     (drover-list--pixel-width candidate)))))
    (if head
        (let ((compact (drover-list--compact string)))
          (if (<= (drover-list--pixel-width compact) pixels) compact ""))
      (or (and keep
               (let* ((tail (concat " " (substring string keep)))
                      (head (drover-truncate-to-pixels (string-trim-right (substring string 0 keep))
                                                       (- pixels (drover-list--pixel-width tail))
                                                       measure)))
                 (unless (string-empty-p head)
                   (concat head tail))))
          (drover-truncate-to-pixels string pixels measure)))))

(defvar drover-list--aligns (make-hash-table :test #'eql)
  "Spaces reaching a position, as `drover-list--align' made them, by position.
Lines draw thousands of them at a few positions, and insert copies.")

(defun drover-list--align (x)
  "Return a space reaching X pixels from the left of the text area.
The same string for the same X: it is copied where it goes, never
changed."
  (or (gethash x drover-list--aligns)
      (progn
        (when (> (hash-table-count drover-list--aligns) 4096)
          (clrhash drover-list--aligns))
        (puthash x (propertize " " 'display `(space :align-to (,x))) drover-list--aligns))))

(defconst drover-list--line-start (propertize " " 'invisible t)
  "The character each line of a session's section begins with.
Magit hides the body of a collapsed section under an invisible overlay,
yet Emacs still draws a `display' property found where invisible text
begins: a body beginning with an aligned space, as every line of the
list but group headings would, drew that space on the next visible
line, which then began where the body would have.  An invisible
character without a display property begins every line of a session
instead, so that a hidden body, or a hidden group of sessions, draws
nothing.")

(defun drover-list--now ()
  "Return the current time, as a float.
The list reads the clock through it, so that tests may stop the clock."
  (float-time))

;;;; Fields

(defconst drover-list--text-fields
  '(goal attempt host harness location branch agent model provider context changes created cost age summary)
  "The fields of `drover-list-fields' a row holds a text for.")

(defun drover-list--one-line (string)
  "Return STRING, or \"\" for nil, on one line.
It is cut to `drover-line-limit' characters, ending in an ellipsis: a
line laid out in pixels costs as much as its text is long.  A string
on one line already is returned itself: rows take thousands of texts."
  (if (and (stringp string)
           (<= (length string) drover-line-limit)
           (not (string-match-p "[\n\r\t]\\|\\` \\| \\'" string)))
      string
    (let* ((string (format "%s" (or string "")))
           (cut (> (length string) (* 2 drover-line-limit)))
           (line (string-trim (replace-regexp-in-string
                               "[\n\r\t]+" " "
                               (if cut (substring string 0 (* 2 drover-line-limit)) string)))))
      (cond ((> (length line) drover-line-limit)
             (concat (string-trim-right (substring line 0 drover-line-limit)) "…"))
            (cut (concat line "…"))
            (t line)))))

(defun drover-list--age-time (session)
  "Return the time from which the age of SESSION counts, or nil.
That is its last activity (see `drover-session-last-activity'): a
session needing the user ages from when its status began, the others
from their last activity, what the order of last activity places them
by, but for running sessions, whose activity is now."
  (drover-session-last-activity session))

(autoload 'drover-usage-session-text "drover-usage")

(defvar drover-list-usage-function #'drover-usage-session-text
  "Function of a session and a form returning what the list shows of its usage.
The form is `column', for the field `cost', `details', for its
details, both a string or nil, or `value', what the others are made of,
compared with `equal' to tell a row to make anew; nil shows no usage.")

(defun drover-list--usage (session form)
  "Return the usage of SESSION as FORM shows it, or nil.
See `drover-list-usage-function'.  Nil without the function, or for
FORM `column' and `value' while the field `cost' is hidden."
  (and drover-list-usage-function
       (or (eq form 'details) (drover-list-field-enabled-p 'cost))
       (drover-with-reporting "usage"
         (funcall drover-list-usage-function session form))))

(defvar drover-list-goal-function nil
  "Function of a session and a form returning what the list shows of its goal.
The form is `column', for the field `goal', a string or nil, or
`value', what it is made of, compared with `equal' to tell a row to
make anew; nil shows no goal.  `drover-goal-mode' sets it.")

(defun drover-list--goal (session form)
  "Return the goal of SESSION as FORM shows it, or nil.
See `drover-list-goal-function'.  Nil without the function, or while
the field `goal' is hidden."
  (and drover-list-goal-function
       (drover-list-field-enabled-p 'goal)
       (drover-with-reporting "goal"
         (funcall drover-list-goal-function session form))))

(defun drover-list--attempt-text (session &optional any)
  "Return the attempt SESSION works in as its column shows it, or nil.
Its number, then its label, of the task SESSION serves (see
`drover-task--attempt-view'); nil while the field `attempt' is hidden,
unless ANY."
  (when-let* (((or any (drover-list-field-enabled-p 'attempt)))
              (task (drover-session-task session))
              (number (plist-get task :attempt))
              ((natnump number)))
    (let ((label (plist-get task :attempt-label)))
      (concat (drover-face (format "#%d" number) 'bold)
              (if (stringp label) (concat " " (drover-list--one-line label)) "")))))

(defun drover-list--field-spec (field)
  "Return the properties of FIELD in `drover-list-fields', or nil."
  (alist-get field drover-list-fields))

;;;; Glyphs

;; A glyph takes the room its font draws it in, its cell, which may be
;; wider than its advance: icon fonts draw icons past their advance
;; (see `drover-text-ink-extent'), and text after a glyph must not be
;; drawn under its ink.

(defun drover-list--pad (pixels)
  "Return a space PIXELS wide, or \"\" when PIXELS is not positive."
  (if (> pixels 0)
      (propertize " " 'display `(space :width (,pixels)))
    ""))

(defun drover-list--cell (strings)
  "Return the cell in which each of STRINGS may be drawn, as (LEFT . RIGHT).
A string drawn in the cell begins LEFT pixels into it, and the cell ends
RIGHT pixels past that: the ink of none of STRINGS leaves it, as
`drover-list--extent' measures it."
  (let ((left 0)
        (right 0))
    (dolist (string strings)
      (pcase-let ((`(,ink-left . ,ink-right) (drover-list--extent string)))
        (setq left (max left ink-left)
              right (max right ink-right))))
    (cons left right)))

(defun drover-list--in-cell (string cell)
  "Return STRING drawn in CELL, a value of `drover-list--cell'.
Spaces as wide as needed come before and after it."
  (concat (drover-list--pad (car cell))
          string
          (drover-list--pad (- (cdr cell) (drover-list--pixel-width string)))))

(defun drover-list--icon (string)
  "Return STRING in a cell of its own, so that no text is drawn on its ink."
  (drover-list--in-cell string (drover-list--cell (list string))))

(defun drover-list--frames (status)
  "Return the frames of the glyph of STATUS, measured, or nil.
The value is (STRINGS WIDTHS): the frames of
`opencode-session-status-animations' in the face of STATUS, and their
widths in pixels.  It is nil when icons do not animate,
`opencode-session-status-animation-interval' being nil, and when STATUS
has no frame."
  (when-let* ((strings (opencode-session-status-frames status)))
    (list strings (mapcar #'drover-list--pixel-width strings))))

(defun drover-list--status-glyph (status metrics &optional still)
  "Return the glyph of STATUS in the status cell of METRICS, animated as they say.
Every status glyph takes the room of the one drawn widest, so that
titles line up whatever the status (see `drover-list--metrics').

When METRICS hold frames for STATUS, the glyph is animated, unless
STILL: one character displays the frame of the time of METRICS, the
next one a space reaching the end of the cell, so that what follows
never moves.  Their properties `drover-list-frame' and
`drover-list-frame-pad' name STATUS: from frame to frame, only their
display changes."
  (let ((glyph (opencode-session-status-glyph status))
        (cell (plist-get metrics :status-cell)))
    (if-let* ((frames (and (not still) (alist-get status (plist-get metrics :frames)))))
        (pcase-let* ((`(,strings ,widths ,width) frames)
                     (index (opencode-session-status-frame-index (length strings) (plist-get metrics :now))))
          (concat (drover-list--pad (car cell))
                  (propertize (if (string-empty-p glyph)
                                  (drover-face " " (opencode-session-status-face status))
                                (substring glyph 0 1))
                              'display (nth index strings) 'drover-list-frame status)
                  (propertize " " 'display `(space :width (,(- width (nth index widths))))
                              'drover-list-frame-pad status)))
      (drover-list--in-cell glyph cell))))

(defun drover-list--subagents-help (subagents)
  "Return the tooltip naming SUBAGENTS, active sessions, by what they do.
It tells how many do each, which their glyphs do not."
  (let* ((waiting (seq-filter (lambda (session) (eq (drover-session-status session) 'waiting))
                              subagents))
         (working (seq-difference subagents waiting #'eq))
         (line (lambda (sessions doing)
                 (when sessions
                   (format "%d subagent%s %s: %s" (length sessions) (if (cdr sessions) "s" "") doing
                           (mapconcat (lambda (session)
                                        (drover-list--one-line (drover-session-display-title session)))
                                      sessions ", "))))))
    (drover-bound-lines (string-join (delq nil (list (funcall line waiting "waiting for you")
                                                     (funcall line working "working")))
                                     "\n")
                        2)))

(defun drover-list--indicator (subagents metrics)
  "Return the glyphs telling that SUBAGENTS are in the middle of a turn, or nil.
SUBAGENTS are the active subagents of a session, at any depth (see
`drover-tree-active-subagents').  The glyph of `opencode-subagent-icons'
for those waiting for the user comes first, then that for those working
or waiting to retry, a space between, each in the subagent cell of
METRICS.  They stay still and tell no number: their tooltip names the
subagents and tells how many."
  (when subagents
    (let* ((waiting (seq-some (lambda (session) (eq (drover-session-status session) 'waiting))
                              subagents))
           (working (seq-some (lambda (session) (not (eq (drover-session-status session) 'waiting)))
                              subagents))
           (cell (plist-get metrics :subagent-cell)))
      (propertize (mapconcat (lambda (kind) (drover-list--in-cell (opencode-subagent-glyph kind) cell))
                             (append (and waiting '(waiting)) (and working '(working)))
                             " ")
                  'help-echo (drover-list--subagents-help subagents)))))

(defun drover-list--archived-glyph (session now)
  "Return the glyph telling that SESSION is archived, in a cell of its own, or nil.
See `opencode-archived-icon'.  Its tooltip tells how long ago, at NOW,
when known."
  (when-let* ((archived (drover-session-archived session)))
    (propertize (drover-list--icon (opencode-archived-glyph))
                'help-echo (if (numberp archived)
                               (format "Archived %s ago" (opencode-format-age archived now))
                             "Archived"))))

(defun drover-list--icons (session metrics)
  "Return what comes before the title of SESSION: its glyphs and a space.
The glyph of its status comes first, still for a session of a host not
connected, whose status may be past, then, when subagents of SESSION are
in the middle of a turn, the glyphs telling so, then, when SESSION is
archived, the glyph telling so; one space separates each.  METRICS give
the cells of glyphs, the frames of animated ones, the active subagents
of each session and the time."
  (let ((indicator (drover-list--indicator
                    (and (plist-get metrics :active)
                         (gethash (drover-session-key session) (plist-get metrics :active)))
                    metrics))
        (archived (drover-list--archived-glyph session (plist-get metrics :now))))
    (concat (drover-list--status-glyph (drover-session-status session) metrics
                                       (drover-session-stale session))
            " "
            (and indicator (concat indicator " "))
            (and archived (concat archived " ")))))

(defun drover-list--title-face (state)
  "Return the face of a title whose session is in STATE, or nil.
STATE is a value of `drover-list--title-state', which names the places
of `opencode-session-place-face'."
  (opencode-session-place-face state))

(defun drover-list--title-text (session depth state)
  "Return the title of SESSION, at DEPTH in its tree, as its line shows it.
A subagent at the top of its tree, its parent not listed, starts with
an arrow; an idle or archived session is dimmed, and so is one of a
host not connected; the title of a
session in STATE, open in a buffer, is in the face of that state (see
`drover-list--title-face'), over the dimming.  The title, not the
arrow, carries the session's key as its property `drover-list-title',
so that its face may change in place."
  (let* ((title (drover-list--one-line (drover-session-display-title session)))
         (faces (delq nil (list (drover-list--title-face state)
                                (and (or (eq (drover-session-status session) 'idle)
                                         (drover-session-archived session)
                                         (drover-session-stale session))
                                     'drover-dim)))))
    (concat (if (and (zerop depth) (drover-session-parent session))
                (drover-face "↳ " 'drover-dim)
              "")
            (propertize (if faces (drover-face title (if (cdr faces) faces (car faces))) title)
                        'drover-list-title (drover-session-key session)))))

(defun drover-list--bar (fraction pixels face)
  "Return a short bar PIXELS wide, FRACTION of it full in FACE.
It is two stretched spaces, the full part and the trough, the trough
in `opencode-cache-trough'."
  (let ((full (round (* pixels (max 0 (min 1 fraction))))))
    (concat (drover-face (propertize " " 'display `(space :width (,full))) face)
            (drover-face (propertize " " 'display `(space :width (,(- pixels full))))
                         'opencode-cache-trough))))

(defun drover-list--model-text (session)
  "Return the model of SESSION and the effort it runs, as its column shows them.
The effort is in `opencode-setting-chosen' when chosen, in
`opencode-setting-inherited' when it is the model's default; its tooltip
says which.  A column too narrow cuts the model's name and keeps the
effort, which the property `drover-list-keep' marks (see
`drover-list--truncate')."
  (let* ((model (drover-session-model session))
         (effort (drover-session-effort session))
         (default (drover-session-effort-default session)))
    (string-join
     (delq nil
           (list (and model (drover-face (drover-list--one-line model) 'drover-dim))
                 (and effort
                      (propertize (drover-face (drover-list--one-line effort)
                                               (if default 'opencode-setting-inherited 'opencode-setting-chosen))
                                  'help-echo (format "Effort: %s, %s" effort
                                                     (if default "the model's default" "chosen"))
                                  'drover-list-keep (and model t)))))
     " ")))

(defun drover-list--provider-text (session)
  "Return the provider of SESSION as its column shows it, or nil.
That is its compact display, often an icon, in a cell of its own, else
its name; the tooltip names it."
  (when-let* ((text (or (drover-session-provider session) (drover-session-provider-name session))))
    (propertize (drover-list--icon (drover-face (drover-list--one-line text) 'drover-dim))
                'help-echo (concat "Provider: " (or (drover-session-provider-name session) text)))))

(defun drover-list--harness-text (session)
  "Return the harness of SESSION as its column shows it.
Its icon, in a cell of its own, then its name; the icon is marked to
keep (see `drover-list--kept-head'), so that a column too narrow for the
name, and a line of two, show the icon alone.  The tooltip tells what
runs SESSION (see `drover-session-harness-description').  The sessions
of a harness, an engine and a host share one text, made once as widths
are measured (see `drover-list--memo'): thousands of rows show a few."
  (drover-list--memo (list 'harness drover-harnesses opencode-provider-icons (drover-session-harness session)
                           (drover-session-engine session) (drover-session-host session))
                     #'drover-list--make-harness-text session))

(defun drover-list--make-harness-text (session)
  "Return the harness of SESSION as its column shows it, made anew.
See `drover-list--harness-text'."
  (let ((harness (drover-session-harness session)))
    (propertize (concat (propertize (drover-list--icon (drover-harness-glyph harness)) 'drover-list-keep t)
                        " "
                        (drover-face (drover-list--one-line (drover-harness-name harness)) 'drover-harness))
                'help-echo (drover-bound-lines
                            (concat "Harness: " (drover-session-harness-description session)) 1))))

(defun drover-list--context-text (session char)
  "Return the context of SESSION as its column shows it, or nil when unknown.
A short bar of how much of the model's context it fills comes first,
`drover-list-context-bar-width' characters CHAR pixels wide, then the
size, after a \"~\" when the service estimated it after a compaction.
Near a long-context price tier, the size is in `opencode-context-near'
and the bar's full part in its inverse video, past it in
`opencode-context-over'.  Without the model's context size, an empty
space takes the bar's place, so that sizes line up.  In two lines, the
context shows as a pie alone (see `drover-list--context-pie')."
  (when-let* ((tokens (drover-session-context-tokens session)))
    (let* ((fraction (drover-session-context-fraction session))
           (pixels (* drover-list-context-bar-width char))
           (face (opencode-context-face (drover-session-context-tier session))))
      (propertize
       (concat (if fraction
                   (drover-list--bar fraction pixels (if face (list '(:inverse-video t) face) 'opencode-context-bar))
                 (drover-list--pad pixels))
               " "
               (drover-face (concat (and (drover-session-context-estimated session) "~")
                                    (opencode-format-tokens tokens))
                            (or face 'drover-dim)))
       'help-echo (concat "Context: " (drover-session-context-description session))))))

(defun drover-list--branch-text (session)
  "Return the branch of SESSION as its column shows it, or nil.
The default branch of its repository (see
`drover-session-default-branch-p') is greyed out, in the face of a
default (`opencode-setting-inherited'), as the model's default effort
is: a column of \"main\" on most lines reads past it, and an empty
cell would not tell which branch it stands for.  Another branch shows
plain, the session working away from it."
  (when-let* ((branch (drover-session-branch session)))
    (let ((line (drover-list--one-line branch)))
      (if (drover-session-default-branch-p session)
          (drover-face line 'opencode-setting-inherited)
        line))))

(defun drover-list--changes-line (additions deletions files)
  "Return ADDITIONS, DELETIONS and FILES, what a session changed, as a line.
Such as \"+120 −14 · 3 files\", the counts of lines in the faces
`drover-added' and `drover-removed', each left out when nil, the files
dimmed.  Nil when FILES is not a positive number."
  (when (and (natnump files) (> files 0))
    (let ((counts (delq nil (list (and (natnump additions) (drover-face (format "+%d" additions) 'drover-added))
                                  (and (natnump deletions) (drover-face (format "−%d" deletions) 'drover-removed)))))
          (files (drover-face (format "%d file%s" files (if (= files 1) "" "s")) 'drover-dim)))
      (if counts
          (concat (string-join counts " ") (drover-face " · " 'drover-dim) files)
        files))))

(defun drover-list--changes-text (session)
  "Return what SESSION changed in all its turns as its column shows it, or nil.
See `drover-session-changes' and `drover-list--changes-line'."
  (pcase (drover-session-changes session)
    (`(,additions ,deletions ,files) (drover-list--changes-line additions deletions files))))

(defun drover-list--host-age (session now)
  "Return how long ago the host of SESSION was last heard from at NOW, or nil.
Nil while the host is connected, or for a session of this host."
  (when-let* ((stale (drover-session-stale session))
              ((numberp stale)))
    (opencode-format-age stale now)))

(defun drover-list--host-help-echo (_window object position)
  "Return the tooltip of the host drawn at POSITION in OBJECT.
It names the host and tells how it stands, as when shown.  For
`help-echo'."
  (drover-with-reporting "list tooltip"
    (when-let* ((host (and (or (bufferp object) (stringp object))
                           (get-text-property position 'drover-list-host object))))
      (drover-bound-lines
       (concat "Host: " (drover-host-label host)
               (when-let* ((state (drover-host-state host)))
                 (concat "\nNot connected: " state)))))))

(defun drover-list--host-text (session now)
  "Return the host of SESSION as its column shows it at NOW, or nil if this host's.
The host's label, dimmed, then, while it is not connected, how long ago
it was last heard from; the tooltip tells how it stands.  In two lines,
its icon shows besides (see `drover-list--host-icon')."
  (when-let* ((host (drover-session-host session)))
    (propertize (drover-face (concat (drover-list--one-line (drover-host-label host))
                                     (when-let* ((age (drover-list--host-age session now)))
                                       (concat " " age)))
                             'drover-dim)
                'drover-list-host host
                'help-echo #'drover-list--host-help-echo)))

(defvar drover-list--drawn)

(defun drover-list--host-icon-help-echo (_window object position)
  "Return the tooltip of the host's icon drawn at POSITION in OBJECT.
It names the host and tells whether it is connected, and since when it
is not, as \"core — disconnected for 5m\", then how it stands, if not
connected.  In the list's buffer, the session drawn there tells whether
it is connected, as the icon shows it.  For `help-echo'."
  (drover-with-reporting "list tooltip"
    (when-let* ((host (and (or (bufferp object) (stringp object))
                           (get-text-property position 'drover-list-host object))))
      (let* ((session (and (bufferp object)
                           (with-current-buffer object
                             (when-let* ((section (drover-list--session-section
                                                   (magit-section-at position))))
                               (and drover-list--drawn
                                    (gethash (oref section value) drover-list--drawn))))))
             (state (drover-host-state host))
             (disconnected (if session (drover-session-stale session) state))
             (age (and session (drover-list--host-age session (drover-list--now)))))
        (drover-bound-lines
         (concat (drover-list--one-line (drover-host-label host)) " — "
                 (cond (age (concat "disconnected for " age))
                       (disconnected "disconnected")
                       (t "connected"))
                 (and disconnected state (concat "\n" state))))))))

(defun drover-list--host-icon (host disconnected)
  "Return the icon telling that a session runs on HOST, another host, in its cell.
That of `drover-host-icon', dimmed, or, when DISCONNECTED, in the face
`drover-host-disconnected', of the same width, its tooltip naming HOST
and telling how it stands (see `drover-list--host-icon-help-echo').
The sessions of a host share one, made once as widths are measured (see
`drover-list--memo')."
  (let ((disconnected (and disconnected t)))
    (drover-list--memo (list 'host-icon drover-host-icon host disconnected)
                       (lambda (host)
                         (propertize (drover-list--icon
                                      (drover-face drover-host-icon
                                                   (if disconnected 'drover-host-disconnected 'drover-dim)))
                                     'drover-list-host host
                                     'help-echo #'drover-list--host-icon-help-echo))
                       host)))

(defun drover-list--context-help-echo (window object position)
  "Return the tooltip of the context's pie drawn at POSITION in OBJECT.
It tells what the context of the session there fills, as the context's
column does; OBJECT is the list's buffer, else WINDOW shows it.  For
`help-echo'."
  (drover-with-reporting "list tooltip"
    (with-current-buffer (cond ((bufferp object) object)
                               ((window-live-p window) (window-buffer window))
                               (t (current-buffer)))
      (when-let* ((section (drover-list--session-section (magit-section-at position)))
                  (session (and drover-list--drawn (gethash (oref section value) drover-list--drawn)))
                  (description (drover-session-context-description session)))
        (concat "Context: " description)))))

(defun drover-list--context-pie (session)
  "Return the pie of how much of its context SESSION fills, in its cell, or nil.
See `drover-context-pie-glyph': in the face of the long-context price
tier it nears or passes, else dimmed, its tooltip telling the context's
size and the model's.  Every pie is drawn in the cell of all of them,
so that its width does not change as the context grows.  Nil without
the model's context size.  Pies of the same eighth and face are one,
made once as widths are measured (see `drover-list--memo')."
  (when-let* ((fraction (drover-session-context-fraction session))
              (glyph (drover-context-pie-glyph fraction)))
    (let ((face (or (opencode-context-face (drover-session-context-tier session)) 'drover-dim)))
      (drover-list--memo (list 'pie drover-context-pie-icons glyph face)
                         (lambda (glyph)
                           (propertize
                            (drover-list--in-cell
                             (drover-face glyph face)
                             (drover-list--cell (mapcar (lambda (icon) (drover-face icon 'drover-dim))
                                                        drover-context-pie-icons)))
                            'help-echo #'drover-list--context-help-echo))
                         glyph))))

(defun drover-list--field-icon (field row)
  "Return what FIELD of ROW shows in two lines as an icon alone, or \"\".
The host, for a session of another host whose host the row shows, its
icon (see `drover-list--host-icon'); the context, its pie (see
`drover-list--context-pie'); a field of an icon and a name, such as the
harness, its icon (see `drover-list--compact'); any other its text."
  (let ((text (or (alist-get field (drover-list--row-texts row)) "")))
    (or (pcase field
          ('host (and (not (string-empty-p text))
                      (let ((session (drover-list--row-session row)))
                        (drover-list--host-icon (drover-session-host session)
                                                (drover-session-stale session)))))
          ('context (drover-list--context-pie (drover-list--row-session row)))
          ('location (if-let* ((head (drover-list--kept-head text)))
                         (substring text 0 head)
                       ""))
          (_ (drover-list--compact text)))
        "")))

(defun drover-list--location-text (session project)
  "Return PROJECT, that of SESSION, as its column shows it, or nil without one.
The project of the task SESSION serves is in its colour, after the
pill of `drover-project-icon', in a cell of its own, which two lines
and the sidebar show alone (see `drover-list--field-icon'); another
project is dimmed.  The texts of a project and its face are made once
as widths are measured (see `drover-list--memo')."
  (when project
    (let* ((task (drover-session-task session))
           (face (and (equal (plist-get task :project) project) (plist-get task :face))))
      (if face
          (drover-list--memo (list 'project drover-project-icon project face)
                             #'drover-list--make-project-text (cons project face))
        (drover-face (drover-list--one-line project) 'drover-dim)))))

(defun drover-list--make-project-text (project)
  "Return PROJECT, (NAME . FACE), as its column shows it, made anew.
See `drover-list--location-text'."
  (let ((name (drover-list--one-line (car project)))
        (face (cdr project)))
    (propertize (concat (propertize (drover-list--icon (drover-face drover-project-icon face))
                                    'drover-list-keep t)
                        " "
                        (drover-face name face))
                'help-echo (concat "Project: " name))))

(defun drover-list--text (field session group metrics)
  "Return the text of FIELD for SESSION, or \"\" when it has none.
GROUP is the group showing SESSION, as (GROUPING . VALUE), or nil: a
group of a project or a branch names it, so its sessions do not repeat
it, nor do those of a group of a host or of a harness.  METRICS give
the time ages count up to and the width of a character.  The prompt cache has no
text: it is drawn apart."
  (let ((now (plist-get metrics :now)))
    (or (pcase field
          ('goal (drover-list--goal session 'column))
          ('attempt (drover-list--attempt-text session))
          ('host (unless (eq (car group) 'host)
                   (drover-list--host-text session now)))
          ('harness (unless (eq (car group) 'harness)
                      (drover-list--harness-text session)))
          ('location (let ((project (drover-session-view-project session)))
                       (unless (equal group (cons 'project project))
                         (drover-list--location-text session project))))
          ('branch (unless (equal group (cons 'branch (drover-session-branch session)))
                     (drover-list--branch-text session)))
          ('agent (drover-list--one-line (drover-agent-label session)))
          ('model (drover-list--model-text session))
          ('provider (drover-list--provider-text session))
          ('context (drover-list--context-text session (plist-get metrics :char)))
          ('changes (drover-list--changes-text session))
          ('created (drover-face (opencode-format-age (drover-session-created session) now) 'drover-dim))
          ('cost (drover-list--usage session 'column))
          ('age (drover-face (opencode-format-age (drover-list--age-time session) now) 'drover-dim))
          ('summary (drover-list--one-line (drover-session-summary session))))
        "")))

(defun drover-list--metrics (now &optional active places)
  "Return the measures lines of the list are drawn with at time NOW.
The value is a plist of :now, :char, the width of an average character,
:gap, the room between two columns, :space, the width of a space,
:margin, where rows place the sessions at the top of a tree, :lead,
where lines draw them: the margin in a list that folds, none in the
sidebar (see `drover-list--folds'), :folds, whether the list folds,
:width, the width lines may take, :status-cell and :subagent-cell,
the cells of the status glyphs and of the glyphs of active subagents
\(see `drover-list--cell'), :frames, an alist (STATUS STRINGS WIDTHS WIDTH)
of the animated status glyphs, their frames and widths (see
`drover-list--frames') and the right of the status cell, :active,
ACTIVE, the table of the active subagents of each session (see
`drover-tree-active-subagents'), or nil, and :places, PLACES, where
buffers are shown (see `drover-list--places'), or nil, and :caching,
the backends that may report prompt caches (see
`drover-list--caching').  The status cell
holds every status glyph and frame, so that titles line up whatever
the status; the subagent cell both glyphs of `opencode-subagent-icons'.
Widths are in pixels; the lines of a list that folds leave room for the
ellipsis Magit adds to collapsed headings.  Rows depend on the margin,
not on where lines draw them, so that the sidebar and the other list
share them (see `drover-list--peer-rows')."
  (let* ((char (drover-list--char-width))
         (statuses (seq-uniq (append drover-status-order (mapcar #'car opencode-session-status-icons))))
         (frames (delq nil (mapcar (lambda (status)
                                     (when-let* ((frames (drover-list--frames status)))
                                       (cons status frames)))
                                   '(working retrying))))
         (status-cell (drover-list--cell
                       (append (mapcar #'opencode-session-status-glyph statuses)
                               (mapcan (lambda (entry) (copy-sequence (nth 1 entry))) frames)))))
    (let ((metrics (list :now now
                         :char char
                         :gap (* drover-list--gap char)
                         :space (drover-list--pixel-width " ")
                         :margin (* drover-list--margin char)
                         :lead (if drover-list--folds (* drover-list--margin char) 0)
                         :folds drover-list--folds
                         :width (- (drover-list--line-width)
                                   (if drover-list--folds
                                       (drover-list--pixel-width (truncate-string-ellipsis))
                                     0))
                         :status-cell status-cell
                         :subagent-cell (drover-list--cell (mapcar #'opencode-subagent-glyph
                                                                   '(waiting working)))
                         :frames (mapcar (lambda (entry) (append entry (list (cdr status-cell)))) frames)
                         :active active
                         :places places
                         :caching (drover-list--caching))))
      (append metrics (list :stamp (drover-list--metrics-stamp metrics))))))

(defun drover-list--metrics-stamp (metrics)
  "Return what rows depend on in METRICS and the settings, besides their session.
A row made with the same stamp, from the same record and in the same
place, is the same row; see `drover-list--inputs'."
  (list (plist-get metrics :char) (plist-get metrics :margin)
        (plist-get metrics :status-cell) (plist-get metrics :subagent-cell)
        (mapcar (lambda (entry) (list (car entry) (nth 2 entry) (nth 3 entry)))
                (plist-get metrics :frames))
        opencode-session-status-icons opencode-subagent-icons opencode-session-status-animations
        opencode-archived-icon opencode-session-status-animation-interval drover-list-context-bar-width
        ;; Printed with each row, so a digest of the harnesses, not them.
        (md5 (prin1-to-string (list drover-harnesses opencode-provider-icons drover-harness-default-icon
                                    drover-host-icon drover-context-pie-icons)))
        drover-list--text-fields (plist-get metrics :caching)
        ;; Last: `drover-list--peers-of' compares the others.
        drover-list--measuring))

(defvar drover-list--reuse nil
  "Rows a drawing may reuse, by session key, or nil.
`drover-list--row' returns the row of this table made of the same
inputs rather than make it anew: most rows of a list stay the same from
one drawing to the next.")

(defun drover-list--inputs (session depth indent group metrics state)
  "Return what the row of SESSION is made of, as a vector.
That is SESSION itself, the record, its DEPTH, INDENT and GROUP, its
active subagents and STATE, what changes with time, its ages, whether
its prompt cache is live, the frame of its animated glyph, the age of
its archiving and since when its host is not connected, the stamp of
METRICS (see `drover-list--metrics-stamp'), its usage, which its
cost shows (see `drover-list-usage-function'), the task it serves,
whose project its project shows (see `drover-session-task'), and its
goal (see `drover-list-goal-function')."
  (let* ((now (plist-get metrics :now))
         (status (drover-session-status session))
         (archived (drover-session-archived session))
         (frames (and (not (drover-session-stale session))
                      (alist-get status (plist-get metrics :frames)))))
    (vector session depth indent group
            (and (plist-get metrics :active)
                 (gethash (drover-session-key session) (plist-get metrics :active)))
            state
            (opencode-format-age (drover-list--age-time session) now)
            (opencode-format-age (drover-session-created session) now)
            (and (drover-list--cache-fraction session now) t)
            (and frames (opencode-session-status-frame-index (length (car frames)) now))
            (and (numberp archived) (opencode-format-age archived now))
            (drover-list--host-age session now)
            (plist-get metrics :stamp)
            (drover-list--usage session 'value)
            (drover-session-task session)
            (drover-list--goal session 'value))))

(defun drover-list--same-inputs-p (a b &optional any-stamp)
  "Return non-nil when row inputs A and B are the same.
Records and subagents are compared as objects: the index replaces the
record of a session that changed.  With ANY-STAMP, their stamps are not
compared: the caller compared them."
  (and (eq (aref a 0) (aref b 0))
       (let ((subagents-a (aref a 4))
             (subagents-b (aref b 4)))
         (while (and subagents-a subagents-b (eq (car subagents-a) (car subagents-b)))
           (setq subagents-a (cdr subagents-a)
                 subagents-b (cdr subagents-b)))
         (and (null subagents-a) (null subagents-b)))
       (let ((index 1)
             (end (length a))
             (same (= (length a) (length b))))
         (while (and same (< index end))
           (unless (or (= index 4) (and any-stamp (= index 12))
                       (equal (aref a index) (aref b index)))
             (setq same nil))
           (setq index (1+ index)))
         same)))

(defvar drover-list--peer-rows nil
  "Rows of the other lists a drawing may copy, or nil.
A list of (ROWS . STAMP), one per other list measuring alike: ROWS its
rows by session key, STAMP the stamp of its drawing (see
`drover-list--metrics-stamp').  The sidebar and the list in an ordinary
window make most rows alike, whatever their layouts: a row one of them
made, of the same inputs, the other copies, with its texts and
measures, rather than make and measure it again.")

(defun drover-list--peers-of (stamp)
  "Return the rows of the other lists whose rows the current one may copy.
Those are the lists measuring as it does, whose drawing's stamp is
STAMP but for the generation of measures, which is each list's own.
See `drover-list--peer-rows'."
  (let ((context (car drover-list--widths))
        (peers nil))
    (dolist (buffer (drover-list--buffers))
      (unless (eq buffer (current-buffer))
        (when-let* ((model (buffer-local-value 'drover-list--model buffer))
                    ((drover-layout-current-p model 'drover-list--model))
                    (widths (buffer-local-value 'drover-list--widths buffer))
                    ((equal (car widths) context))
                    (peer-stamp (plist-get (drover-list--model-layout model) :stamp))
                    ((equal (butlast peer-stamp) (butlast stamp))))
          (push (cons (drover-list--model-rows model) peer-stamp) peers))))
    peers))

(defun drover-list--peer-row (key inputs)
  "Return a copy of a row of `drover-list--peer-rows' of session KEY, or nil.
It is a row made of INPUTS, in another list's drawing, which the copy
takes its place in: it keeps the texts and measures, not what belongs
to that list, its heading, tree and section."
  (seq-some (pcase-lambda (`(,rows . ,stamp))
              (when-let* ((row (gethash key rows))
                          (peer-inputs (drover-list--row-inputs row))
                          ((equal (aref peer-inputs 12) stamp))
                          ((drover-list--same-inputs-p peer-inputs inputs t)))
                (let ((copy (copy-sequence row)))
                  (setf (drover-list--row-inputs copy) inputs
                        (drover-list--row-heading copy) nil
                        (drover-list--row-tree copy) nil
                        (drover-list--row-section copy) nil)
                  copy)))
            drover-list--peer-rows))

(defun drover-list--row (session depth indent group metrics)
  "Return the row of SESSION at DEPTH in its tree, its glyph INDENT pixels in.
GROUP is the group showing it, as (GROUPING . VALUE), or nil; METRICS
are the measures of `drover-list--metrics'.  A row of
`drover-list--reuse' made of the same inputs is returned as it is; one
of another list, copied (see `drover-list--peer-rows')."
  (let* ((state (drover-list--title-state session (plist-get metrics :places)))
         (inputs (and (plist-get metrics :stamp)
                      (drover-list--inputs session depth indent group metrics state)))
         (old (and inputs drover-list--reuse
                   (gethash (drover-session-key session) drover-list--reuse))))
    (if (and old (drover-list--row-inputs old)
             (drover-list--same-inputs-p (drover-list--row-inputs old) inputs))
        old
      (or
       (and inputs drover-list--peer-rows
            (drover-list--peer-row (drover-session-key session) inputs))
       (let ((icons (drover-list--icons session metrics)))
         (drover-list--row-create
          :session session :depth depth :indent indent :icons icons
          :title-start (+ indent (drover-list--pixel-width icons))
          :title (drover-list--title-text session depth state)
          :state state
          :texts (mapcar (lambda (field) (cons field (drover-list--text field session group metrics)))
                         drover-list--text-fields)
          :cache (drover-list--cache-fraction session (plist-get metrics :now))
          :caching (drover-list--caching-p session metrics)
          :inputs inputs))))))

(defun drover-list--tree-rows (nodes group metrics &optional depth indent)
  "Return the rows of the trees NODES at DEPTH, subagents after their parent.
Their glyphs begin INDENT pixels in, by default the margin.  The glyph
of a subagent stands under its parent's title, whatever glyphs come
before the title, down to `drover-list--max-depth'; deeper, under its
parent's glyph.  GROUP is the group showing them, as (GROUPING . VALUE),
or nil; METRICS are the measures of `drover-list--metrics'."
  (nreverse (drover-list--push-rows nodes group metrics (or depth 0)
                                    (or indent (plist-get metrics :margin)) nil)))

(defun drover-list--push-rows (nodes group metrics depth indent rows)
  "Return ROWS, a list, with the row of each session of the trees NODES pushed.
They are pushed in order, so that the last comes first.  DEPTH, INDENT,
GROUP and METRICS are as for `drover-list--tree-rows'."
  (dolist (node nodes)
    (let ((row (drover-list--row (drover-node-session node) depth indent group metrics)))
      (push row rows)
      (when (drover-node-children node)
        (setq rows (drover-list--push-rows (drover-node-children node) group metrics (1+ depth)
                                           (if (< depth drover-list--max-depth)
                                               (drover-list--row-title-start row)
                                             indent)
                                           rows)))))
  rows)

;;;; Summaries of rows

;; The layout depends on rows through a few maxima: the widest text of
;; each field, the furthest title.  With thousands of rows, a change of
;; one row must not measure them all again: a summary counts the rows
;; of each width, per field, so that a row changed takes its widths out
;; and puts its new ones in, and the maxima follow.

(defconst drover-list--measure-count (+ 3 (* 2 (length drover-list--text-fields)))
  "How many measures a row has, see `drover-list--measures'.")

(defun drover-list--text-index (field)
  "Return the index of the width of the text of FIELD among a row's measures."
  (+ 3 (cl-position field drover-list--text-fields)))

(defun drover-list--kept-index (field)
  "Return the index of the room FIELD needs to keep its tail whole, in measures."
  (+ (drover-list--text-index field) (length drover-list--text-fields)))

(defun drover-list--measures (row)
  "Return what the layout needs of ROW, as a vector, measured once.
Its elements are: where its title begins, where it ends, 1 when it
shows a prompt cache or may show one (see `drover-list--caching-p'),
then for each of `drover-list--text-fields' the width of its text, nil
when empty, then for each the width an ellipsis and the tail to keep
whole take (see `drover-list--kept'), or the head to keep (see
`drover-list--kept-head'), nil without either, but for the fields
showing another icon in two lines (see `drover-list--iconic-fields'),
the width of that icon.  Widths are in pixels."
  (or (drover-list--row-measures row)
      (setf (drover-list--row-measures row)
            (let ((measures (make-vector drover-list--measure-count nil))
                  (start (drover-list--row-title-start row))
                  (count (length drover-list--text-fields)))
              (aset measures 0 start)
              (aset measures 1 (+ start (drover-list--pixel-width (drover-list--row-title row))))
              (aset measures 2 (and (or (drover-list--row-cache row) (drover-list--row-caching row)) 1))
              (cl-loop for field in drover-list--text-fields
                       for index from 3
                       do (let ((text (alist-get field (drover-list--row-texts row))))
                            (unless (or (null text) (string-empty-p text))
                              (aset measures index (drover-list--pixel-width text))
                              (when-let* ((keep (drover-list--kept text)))
                                (aset measures (+ index count)
                                      (drover-list--pixel-width
                                       (concat (truncate-string-ellipsis) " " (substring text keep)))))
                              (when (drover-list--kept-head text)
                                (aset measures (+ index count)
                                      (drover-list--pixel-width (drover-list--compact text)))))))
              (dolist (field (drover-list--iconic-fields))
                (let ((icon (drover-list--field-icon field row)))
                  (unless (string-empty-p icon)
                    (aset measures (drover-list--kept-index field) (drover-list--pixel-width icon)))))
              measures))))

(defun drover-list--tally-create ()
  "Return an empty tally: how many times each number counts, and the largest."
  (cons (make-hash-table :test #'eql) 0))

(defun drover-list--tally-add (tally value delta)
  "Count VALUE DELTA more times in TALLY, a negative DELTA counting it less."
  (let* ((table (car tally))
         (count (+ (gethash value table 0) delta)))
    (if (> count 0)
        (puthash value count table)
      (remhash value table))
    (cond ((and (> delta 0) (cdr tally) (> value (cdr tally)))
           (setcdr tally value))
          ((and (< delta 0) (<= count 0) (eql value (cdr tally)))
           ;; Found again when next asked.
           (setcdr tally nil)))))

(defun drover-list--tally-max (tally)
  "Return the largest number TALLY counts, 0 when it counts none."
  (or (cdr tally)
      (setcdr tally (let ((largest 0))
                      (maphash (lambda (value _) (setq largest (max largest value))) (car tally))
                      largest))))

(defun drover-list--tally-any-p (tally)
  "Return non-nil when TALLY counts a number."
  (> (hash-table-count (car tally)) 0))

(defun drover-list--summary-create ()
  "Return the summary of no row: a vector of tallies, one per measure."
  (let ((summary (make-vector drover-list--measure-count nil)))
    (dotimes (index drover-list--measure-count)
      (aset summary index (drover-list--tally-create)))
    summary))

(defun drover-list--summary-add (summary row delta)
  "Count the measures of ROW DELTA times in SUMMARY: 1 adds it, -1 takes it out."
  (let ((measures (drover-list--measures row)))
    (dotimes (index drover-list--measure-count)
      (when-let* ((value (aref measures index)))
        (drover-list--tally-add (aref summary index) value delta)))))

(defun drover-list--summarize (rows)
  "Return the summary of ROWS, for the layout."
  (let ((summary (drover-list--summary-create)))
    (dolist (row rows)
      (drover-list--summary-add summary row 1))
    summary))

(defun drover-list--has-p (field summary)
  "Return non-nil when a row of SUMMARY has something to show for FIELD.
What comes and goes with a session's turns counts whether it shows now
or not, so that a turn changes no field the list shows: the summary for
any row, as any session may come to need the user, and the prompt
cache for a row that may show one (see `drover-list--caching-p')."
  (pcase field
    ('title t)
    ('summary (drover-list--tally-any-p (aref summary 1)))
    ('cache (drover-list--tally-any-p (aref summary 2)))
    (_ (drover-list--tally-any-p (aref summary (drover-list--text-index field))))))

;;;; Layout

(defun drover-list--cache-cell-width (metrics)
  "Return the width of the prompt cache field, in pixels, per METRICS.
It holds the short bar, a space and the longest time left it shows."
  (+ (* drover-list-cache-bar-width (plist-get metrics :char))
     (plist-get metrics :space)
     (drover-list--pixel-width (drover-face "59m" 'drover-dim))))

(defun drover-list--label-width (field)
  "Return the width in pixels of the name of FIELD's column, 0 without one.
It is measured over the face `header-line', in which the header line
draws it."
  (if-let* ((label (plist-get (drover-list--field-spec field) :label)))
      (drover-list--pixel-width (propertize label 'face '(drover-list-header header-line)))
    0))

(defun drover-list--whole-p (field)
  "Return non-nil when FIELD is drawn whole or not at all, never cut.
Those are the fields of a short text: the ages, cut, would read as
another age, and the context and cache, a bar and a number, and the
changes, counts, would mislead; the provider is an icon.  Their
columns may give up only the room of their names."
  (memq field '(cache context changes created age provider)))

(defun drover-list--compact-p (field)
  "Return non-nil when FIELD shows its head alone where room is short.
That is the harness, its icon without its name (see
`drover-list--kept-head'): its column, cut, is as wide as the icon."
  (eq field 'harness))

(defun drover-list--iconic-fields ()
  "Return the fields that show in two lines as an icon other than their head.
The host, as `drover-host-icon', the context, as its pie, and the
project, as the pill of a task's project alone, nothing for another:
see `drover-list--field-icon'."
  '(host context location))

(defun drover-list--icon-alone-p (field metrics)
  "Return non-nil when FIELD shows as its icon alone in the list of METRICS.
That is the host in the sidebar, which folds nothing (see
`drover-list--folds') and keeps its lines compact: in columns as in two
lines, the host's icon, without its name, nor since when it is not
connected, which its tooltip tells, and its face shows (see
`drover-list--host-icon'); its column is as wide as the icon."
  (and (eq field 'host) (not (plist-get metrics :folds))))

(defun drover-list--icon-p (field)
  "Return non-nil when FIELD shows in two lines as an icon alone, in a cell.
That is a field showing its head alone (see `drover-list--compact-p')
or another icon (see `drover-list--iconic-fields')."
  (or (drover-list--compact-p field) (memq field (drover-list--iconic-fields))))

(defun drover-list--content-width (field summary metrics)
  "Return the width in pixels FIELD takes to show the rows of SUMMARY in full.
METRICS are the measures of lines.  That of the title counts from the
margin, indentation and glyph included."
  (pcase field
    ('title (- (drover-list--tally-max (aref summary 1)) (plist-get metrics :margin)))
    ('cache (drover-list--cache-cell-width metrics))
    (_ (drover-list--tally-max (aref summary (if (drover-list--icon-alone-p field metrics)
                                                 (drover-list--kept-index field)
                                               (drover-list--text-index field)))))))

(defun drover-list--narrowest (entry summary metrics)
  "Return the fewest pixels the field of ENTRY needs for the rows of SUMMARY.
ENTRY is an entry of `drover-list-fields', METRICS the measures of
lines.  It is the room of its :min characters, and for a field never
cut (see `drover-list--whole-p'), the room of what it shows at least;
for a field keeping a tail whole, such as the model its effort, the
room of an ellipsis and that tail.  That of the title does not count
the glyphs before it."
  (let* ((field (car entry))
         (least (* (plist-get metrics :char) (or (plist-get (cdr entry) :min) 0))))
    (cond ((drover-list--icon-alone-p field metrics)
           (drover-list--content-width field summary metrics))
          ((drover-list--whole-p field)
           (max least (drover-list--content-width field summary metrics)))
          ((eq field 'title) least)
          (t (max least (drover-list--tally-max (aref summary (drover-list--kept-index field))))))))

(defun drover-list--natural (field summary metrics)
  "Return the width in pixels the column of FIELD wants for the rows of SUMMARY.
It is as wide as what it shows, see `drover-list--content-width', and
at least as wide as its name in the header line, but a field showing
its icon alone (see `drover-list--icon-alone-p'), as wide as the icon;
METRICS are the measures of lines."
  (max (if (drover-list--icon-alone-p field metrics) 0 (drover-list--label-width field))
       (drover-list--content-width field summary metrics)))

(defun drover-list--title-prefix (summary metrics)
  "Return the room, in pixels, before the titles of SUMMARY, from the margin.
METRICS are the measures of lines."
  (- (drover-list--tally-max (aref summary 0)) (plist-get metrics :margin)))

(defun drover-list--least-prefix (metrics)
  "Return the room in pixels before the title of a session alone, from the margin.
That is the room of the status glyph, in its cell, and a space, as the
title of a session at the top of a tree, without active subagents,
begins; METRICS are the measures of lines.  Subagents shown under a
session, and the glyphs telling that they work, come and go with turns:
the fields shown do not depend on them."
  (let ((cell (plist-get metrics :status-cell)))
    (+ (car cell) (cdr cell) (plist-get metrics :space))))

(defun drover-list--choose (summary metrics)
  "Return the layout of the rows of SUMMARY in lines of the width of METRICS.
The value is (LAYOUT . FIELDS): LAYOUT is `columns' or `narrow', and
FIELDS the entries of `drover-list-fields' shown, in their order.  The
fields with something to show for the rows (see `drover-list--has-p')
are taken by priority, as long as the fewest pixels each needs fit (see
`drover-list--narrowest'), the title's counted from the glyph of a
session alone (see `drover-list--least-prefix'), and the title always;
the fields of `drover-list-hidden-fields' never.  Columns are used when
they include every field the narrow layout places, but those it shows
as an icon alone; else the narrow layout shows those fields, which need
no room of their own but on the title's line, where they give way to
the title.  So the fields shown
change with the width, the settings and the sessions listed, not with
what a session's turn changes: whether it needs the user, its prompt
cache, its subagents at work."
  (let* ((gap (plist-get metrics :gap))
         (present (seq-filter (lambda (entry)
                                (and (drover-list--has-p (car entry) summary)
                                     (or (eq (car entry) 'title)
                                         (not (memq (car entry) drover-list-hidden-fields)))))
                              drover-list-fields))
         (used (+ (plist-get metrics :lead) (drover-list--least-prefix metrics)))
         (chosen nil))
    (catch 'full
      (dolist (entry (seq-sort-by (lambda (entry) (or (plist-get (cdr entry) :priority) 99)) #'<
                                  present))
        (let* ((title (eq (car entry) 'title))
               (need (+ (drover-list--narrowest entry summary metrics) (if title 0 gap))))
          (if (or title (<= (+ used need) (plist-get metrics :width)))
              (setq used (+ used need)
                    chosen (cons entry chosen))
            (throw 'full nil)))))
    (let ((narrow (seq-filter (lambda (entry)
                                (pcase (plist-get (cdr entry) :narrow)
                                  ((guard (eq (car entry) 'title)) t)
                                  ;; An icon alone shows only for the rows showing one.
                                  ('icon (> (drover-list--tally-max
                                             (aref summary (drover-list--kept-index (car entry))))
                                            0))
                                  (place place)))
                              present)))
      (if (seq-every-p (lambda (entry)
                         (or (memq entry chosen) (eq (plist-get (cdr entry) :narrow) 'icon)))
                       narrow)
          (cons 'columns (seq-filter (lambda (entry) (memq entry chosen)) present))
        (cons 'narrow narrow)))))

(defun drover-list--give (widths floors indices deficit)
  "Take up to DEFICIT pixels from WIDTHS at INDICES, down to FLOORS.
WIDTHS and FLOORS are vectors; WIDTHS changes in place.  Each column
gives in proportion to its room above its floor, the pixels left by
rounding going to the largest remainders, so that no more is taken
than DEFICIT: a column given a pixel too few would cut its text, while
the line had room.  Return the deficit left."
  (let* ((gives (mapcar (lambda (index) (max 0 (- (aref widths index) (aref floors index)))) indices))
         (slack (apply #'+ gives)))
    (if (or (<= slack 0) (<= deficit 0))
        deficit
      (let* ((cut (min deficit slack))
             (shares (cl-mapcar (lambda (index give)
                                  (list index (/ (* cut give) slack) (% (* cut give) slack)))
                                indices gives))
             (left (- cut (apply #'+ (mapcar #'cadr shares)))))
        (dolist (share (sort shares (lambda (a b) (> (nth 2 a) (nth 2 b)))))
          (let ((take (+ (nth 1 share) (if (> left 0) 1 0))))
            (setq left (1- left))
            (aset widths (car share) (- (aref widths (car share)) take))))
        (- deficit cut)))))

(defun drover-list--column-widths (fields summary metrics)
  "Return the widths in pixels of the columns of FIELDS for the rows of SUMMARY.
FIELDS are entries of `drover-list-fields'; METRICS give the width of
lines.  A column is as wide as what it shows and its name, up to its
:max.  When the line is too narrow for that, room is given up, each
column down to the fewest pixels it needs (see
`drover-list--narrowest'), in turn by: the fields without a :max, of a
long text, such as the summary; the names of the columns of short
texts, which are never cut (see `drover-list--whole-p'); the name of
the harness, whose icon stays (see `drover-list--compact-p'), its
column then as wide as the icon; the title; the other fields, in
proportion to what they would lose.  So the ages and the effort show
whole while the summary or the title has room to give, the project and
model whole while the title has, and no column is narrowed while the
line has room.  The title gives up room down to its fewest pixels from
the glyph of a session alone, as `drover-list--choose' counts them: the
titles of subagents, further in, are cut more.  A column no row has
anything for yet, the summary's kept for the sessions that will need
the user (see `drover-list--has-p'), gives up all its room."
  (let* ((prefix (drover-list--title-prefix summary metrics))
         (least (drover-list--least-prefix metrics))
         (room (- (plist-get metrics :width) (plist-get metrics :lead)
                  (* (plist-get metrics :gap) (max 0 (1- (length fields))))))
         (widths (make-vector (length fields) 0))
         (floors (make-vector (length fields) 0))
         (passes (list nil nil nil nil nil)))
    (seq-do-indexed
     (lambda (entry index)
       (let* ((field (car entry))
              (extra (if (eq field 'title) prefix 0))
              (max (plist-get (cdr entry) :max))
              (natural (drover-list--natural field summary metrics))
              (want (if max (min natural (+ extra (* (plist-get metrics :char) max))) natural)))
         (aset widths index want)
         ;; Empty, a column lends all its room until its text comes.
         (aset floors index (if (zerop (drover-list--content-width field summary metrics))
                                0
                              (min want (+ (if (eq field 'title) least 0)
                                           (drover-list--narrowest entry summary metrics)))))
         (push index (nth (cond ((drover-list--whole-p field) 1)
                                ((null max) 0)
                                ((drover-list--compact-p field) 2)
                                ((eq field 'title) 3)
                                (t 4))
                          passes))))
     fields)
    (let ((deficit (- (apply #'+ (append widths nil)) room))
          (wants (copy-sequence widths)))
      (dolist (indices passes)
        (setq deficit (drover-list--give widths floors (nreverse indices) deficit)))
      ;; Cut, a compact field shows its head alone: the rest of its
      ;; column would stay blank.
      (seq-do-indexed (lambda (entry index)
                        (when (and (drover-list--compact-p (car entry))
                                   (< (aref widths index) (aref wants index)))
                          (aset widths index (aref floors index))))
                      fields))
    (append widths nil)))

(defun drover-list--layout-of (summary metrics)
  "Return METRICS with the layout of the rows of SUMMARY added.
Add :layout, `columns' or `narrow', and :fields, the fields shown; in
columns, :columns, a list of (FIELD X WIDTH ALIGN) giving where each
field begins and how wide it is, in pixels, the first at the lead of
METRICS; in the narrow layout, :edges, see `drover-list--edges'.  In
the sidebar, which folds nothing, the columns of the fields showing
an icon alone, the host's and the harness's, come after those of short
texts (see `drover-list--compact-after-whole'), as in two lines."
  (pcase-let* ((`(,layout . ,fields) (drover-list--choose summary metrics))
               (columns nil))
    (when (and (eq layout 'columns) (not (plist-get metrics :folds)))
      (setq fields (drover-list--compact-after-whole fields metrics)))
    (when (eq layout 'columns)
      (let ((x (plist-get metrics :lead)))
        (cl-mapc (lambda (entry width)
                   (push (list (car entry) x width (plist-get (cdr entry) :align)) columns)
                   (setq x (+ x width (plist-get metrics :gap))))
                 fields (drover-list--column-widths fields summary metrics))))
    (append (list :layout layout :fields (mapcar #'car fields) :columns (nreverse columns)
                  :edges (and (eq layout 'narrow) (drover-list--edges (mapcar #'car fields) summary)))
            metrics)))

(defun drover-list--compact-after-whole (fields metrics)
  "Return FIELDS, those showing an icon alone after those of short texts.
FIELDS are entries of `drover-list-fields', METRICS the measures of the
sidebar's lines.  Those showing their head alone, such as the harness's
icon (see `drover-list--compact-p'), and those showing their icon alone
there, the host's (see `drover-list--icon-alone-p'), in their order,
come right after the last of FIELDS of a short text (see
`drover-list--whole-p'), the age and the prompt cache: in the sidebar,
the host's icon and the harness's follow them, as in two lines, where
they end the title's line, the summary's column after them, which its
texts of every width would move."
  (let* ((compact (seq-filter (lambda (entry)
                                (or (drover-list--compact-p (car entry))
                                    (drover-list--icon-alone-p (car entry) metrics)))
                              fields))
         (others (seq-difference fields compact #'eq))
         (last-whole (seq-reduce (lambda (last entry) (if (drover-list--whole-p (car entry)) entry last))
                                 others nil)))
    (if (or (null compact) (null last-whole))
        fields
      (let ((tail (memq last-whole others)))
        (append (seq-take others (- (length others) (length tail)))
                (list last-whole) compact (cdr tail))))))

(defun drover-list--edges (fields summary)
  "Return the cells ending the title's line in two lines, for the rows of SUMMARY.
FIELDS are the fields shown.  The value is a list of (FIELD . WIDTH),
from the right edge leftwards: first each field of FIELDS placed on
the title's line that shows an icon alone there (see
`drover-list--icon-p'), the harness's last, the host's before it, then
each placed as an icon alone, the context's pie, each in a cell as wide
as the widest icon the rows show, in pixels.  So icons line up from
line to line, whatever stands before them, the ages of other widths; a
line without one keeps its cell empty, and the cell is left out while
no row shows one."
  (let ((edges nil)
        (place (lambda (field) (plist-get (drover-list--field-spec field) :narrow))))
    (dolist (field (append (seq-filter (lambda (field) (eq (funcall place field) 'icon)) fields)
                           (seq-filter (lambda (field)
                                         (and (drover-list--icon-p field)
                                              (eq (funcall place field) 'line)))
                                       fields)))
      (let ((width (drover-list--tally-max (aref summary (drover-list--kept-index field)))))
        (when (> width 0)
          (push (cons field width) edges))))
    edges))

(defun drover-list--layout (rows metrics)
  "Return METRICS with the layout of ROWS added, see `drover-list--layout-of'."
  (drover-list--layout-of (drover-list--summarize rows) metrics))

(defun drover-list--layout-key (layout)
  "Return what drawing a heading depends on in LAYOUT, but the time.
Two layouts of the same key draw rows alike: a heading drawn for one is
the heading for the other."
  (list (plist-get layout :layout) (plist-get layout :fields) (plist-get layout :columns)
        (plist-get layout :edges) (plist-get layout :width) (plist-get layout :margin) (plist-get layout :lead)
        (plist-get layout :folds) (plist-get layout :char)
        (plist-get layout :gap) (plist-get layout :space) (plist-get layout :stamp)))

;;;; Lines

(defun drover-list--assemble (segments &optional split)
  "Return the string of SEGMENTS, and where they lie, as (STRING . SPEC).
Each segment is (align X), a space reaching X pixels, or (text STRING
WIDTH X), STRING, WIDTH pixels wide, beginning X pixels in.  With SPLIT,
each space is two, the first, of no width, ready to stretch: SPEC then
lists each segment as (OFFSET split FROM TO), where FROM and TO bound
the space, or (OFFSET text X WIDTH STRING), OFFSET being where it
begins in STRING.  A last space reaching SPLIT, a number, closes the
line, so that a bar may fill all of it."
  (let ((x 0) (parts nil) (spec nil) (offset 0))
    (dolist (segment (if (numberp split) (append segments (list (list 'align split))) segments))
      (pcase segment
        (`(align ,to)
         (if split
             (progn
               (push (list offset 'split x (max x to)) spec)
               (push (propertize " " 'display '(space :width (0))) parts)
               (push (drover-list--align to) parts)
               (setq offset (+ offset 2)))
           (push (drover-list--align to) parts)
           (setq offset (1+ offset)))
         (setq x (max x to)))
        (`(text ,string ,width ,start)
         (push (list offset 'text start width string) spec)
         (push string parts)
         (setq offset (+ offset (length string))
               x (+ start width)))))
    (cons (apply #'concat (nreverse parts)) (nreverse spec))))

(defun drover-list--text-segment (string x)
  "Return the segment of STRING beginning X pixels in, for `drover-list--assemble'."
  (list 'text string (drover-list--pixel-width string) x))

(defun drover-list--title-segments (row room &optional shift)
  "Return the segments of the glyphs and title of ROW, the title ROOM pixels wide.
They begin SHIFT pixels further right than the row's indentation,
further left when SHIFT is negative.  Glyphs at the start of the line
need no space before them: the line begins with them, so that the
cursor at the start of a session is on its glyph."
  (let* ((shift (or shift 0))
         (indent (+ shift (drover-list--row-indent row)))
         (title (drover-list--truncate (drover-list--row-title row) room)))
    (append (and (> indent 0) (list (list 'align indent)))
            (list (drover-list--text-segment (drover-list--row-icons row) indent)
                  (drover-list--text-segment title (+ shift (drover-list--row-title-start row)))))))

(defconst drover-list--cache-fill '((:inverse-video t) opencode-cache-bar)
  "Face of the full part of the short bar of a prompt cache.
It fills the bar with the foreground of `opencode-cache-bar', in inverse
video, so that the highlight of the current line or of the region,
which sets only a background, does not hide it.")

(defun drover-list--cache-line-face ()
  "Return the face drawing a prompt cache under a line, in two lines.
It underlines the line, at its descent, in the foreground of
`opencode-cache-bar': the text keeps its colours, and the highlights of
the current line and of the region, which set only a background, leave
it visible.  The colour is that of the selected frame, read as the list
is drawn, which a theme enabled draws anew."
  `(:underline (:color ,(or (face-foreground 'opencode-cache-bar nil t) 'foreground-color)
                :position t)))

(defun drover-list--cache-label (remaining)
  "Return the time left of a prompt cache, REMAINING seconds, as a field shows it.
It is text, as copyable as the rest of the line, which the cache timer
replaces as time passes: its characters tell their part in the
property `drover-list-cache', as `label'."
  (propertize (drover-face (opencode-format-duration remaining) 'drover-dim)
              'drover-list-cache 'label 'help-echo #'drover-list--cache-help-echo))

(defun drover-list--cache-cell (fraction remaining layout)
  "Return the prompt cache field: a bar FRACTION full, REMAINING seconds left.
The value is a string displayed as the full part of the bar, its empty
part, a space and the time left, in text (see
`drover-list--cache-label'); LAYOUT gives the measures.  Each character
tells its part in the property `drover-list-cache', so that only they
change as time passes.  The tooltip tells how the cache stands when
shown, see `drover-list--cache-help-echo'."
  (let* ((bar (* drover-list-cache-bar-width (plist-get layout :char)))
         (full (round (* bar fraction))))
    (concat (propertize (drover-face " " drover-list--cache-fill)
                        'display `(space :width (,full))
                        'drover-list-cache 'full 'help-echo #'drover-list--cache-help-echo)
            (propertize (drover-face " " 'opencode-cache-trough)
                        'display `(space :width (,(- bar full)))
                        'drover-list-cache 'trough 'help-echo #'drover-list--cache-help-echo)
            (propertize " " 'drover-list-cache 'space 'help-echo #'drover-list--cache-help-echo)
            (drover-list--cache-label remaining))))

(defun drover-list--column-line (row layout &optional prefix)
  "Return the heading line of ROW in the columns of LAYOUT, and what it shows.
The value is (LINE CACHE WHOLE): CACHE is non-nil when the line shows a
prompt cache field; WHOLE lists the fields it shows whole, or for which
it has nothing to show, `title' among them: not those cut, nor showing
an icon alone (see `drover-list--details').  Each cell begins with a
space aligned to its column, so that columns line up whatever the
widths of the characters before them.  LINE begins with PREFIX, if any.
Its parts are joined once: a list drawn whole makes thousands of
lines."
  (let ((parts (and prefix (list prefix)))
        (cache nil)
        (whole nil)
        (session (drover-list--row-session row)))
    (pcase-dolist (`(,field ,x ,width ,align) (plist-get layout :columns))
      (pcase field
        ('title
         ;; As `drover-list--title-segments' places them.
         (let* ((shift (- x (plist-get layout :margin)))
                (title (drover-list--row-title row))
                (shown (drover-list--truncate title (- (+ x width) shift (drover-list--row-title-start row)))))
           (when (> (+ shift (drover-list--row-indent row)) 0)
             (push (drover-list--align (+ shift (drover-list--row-indent row))) parts))
           (push (drover-list--row-icons row) parts)
           (push shown parts)
           (when (eq shown title)
             (push 'title whole))))
        ('cache
         (push 'cache whole)
         (when-let* ((fraction (drover-list--row-cache row)))
           (setq cache t)
           (push (drover-list--align x) parts)
           (push (drover-list--cache-cell
                  fraction (drover-session-cache-remaining session (plist-get layout :now))
                  layout)
                 parts)))
        (_
         (let* ((full (or (alist-get field (drover-list--row-texts row)) ""))
                (text (if (drover-list--icon-alone-p field layout)
                          (drover-list--field-icon field row)
                        (drover-list--truncate full width))))
           (when (or (string-empty-p full) (eq text full))
             (push field whole))
           (unless (string-empty-p text)
             (push (drover-list--align (if (eq align 'right)
                                           (- (+ x width) (drover-list--pixel-width text))
                                         x))
                   parts)
             (push text parts))))))
    (list (apply #'concat (nreverse parts)) cache whole)))

(defun drover-list--shift (layout)
  "Return how far right of their rows' places the lines of LAYOUT draw, in pixels.
Rows place the sessions at the top of a tree at the margin; lines draw
them at the lead, the window's edge in the sidebar: there the shift is
negative."
  (- (plist-get layout :lead) (plist-get layout :margin)))

(defun drover-list--line-prefix (layout)
  "Return what each line of a session begins with in LAYOUT.
That is `drover-list--line-start' in a list that folds, and nothing in
the sidebar, which hides no line."
  (if (plist-get layout :folds) drover-list--line-start ""))

(defun drover-list--narrow-line (row layout &optional prefix)
  "Return the heading of ROW in the narrow layout of LAYOUT, and what it shows.
The value is (HEADING SPEC WHOLE).  HEADING is one or two lines: the glyph,
the title and the fields placed on its line, flush right, as they show
where room is short (see `drover-list--compact'), those showing an icon
alone last, in the cells of :edges of LAYOUT, so that they line up (see
`drover-list--edges' and `drover-list--field-icon'), then, for a session
that needs the user, the fields placed below, under the title; it
begins with PREFIX, if any, its second line with the line prefix of
LAYOUT.  SPEC tells where the first line's segments lie, from
`drover-list--assemble', past PREFIX, when a prompt cache colours its
background, else nil.  WHOLE lists the fields HEADING shows whole, or
has nothing to show for, `title' among them: not those cut, given up
for the title, shown as an icon alone or as a background, nor those
below while not shown (see `drover-list--details')."
  (let* ((session (drover-list--row-session row))
         (width (plist-get layout :width))
         (space (plist-get layout :space))
         (shift (drover-list--shift layout))
         (title-start (+ shift (drover-list--row-title-start row)))
         (texts (drover-list--row-texts row))
         (narrow (lambda (place)
                   (seq-filter (lambda (field)
                                 (eq (plist-get (drover-list--field-spec field) :narrow) place))
                               (remq 'title (plist-get layout :fields)))))
         (edges (plist-get layout :edges))
         (right nil)
         (whole nil)
         (end width))
    ;; The cells at the edge, then the other fields on the title's line,
    ;; flush right, from the right edge leftwards.
    (pcase-dolist (`(,field . ,cell) edges)
      (let ((text (drover-list--field-icon field row))
            (start (- end cell)))
        (when (string-empty-p (or (alist-get field texts) ""))
          (push field whole))
        (when (> start (+ title-start space))
          (unless (string-empty-p text)
            (setq right (append (list (list 'align start) (drover-list--text-segment text start))
                                right)))
          (setq end (- start space)))))
    (dolist (field (reverse (funcall narrow 'line)))
      ;; A field showing its head in a cell shows nothing else, nor
      ;; does the host in the sidebar; elsewhere, it shows its name
      ;; besides its icon.
      (let* ((full (or (alist-get field texts) ""))
             (iconic (and (assq field edges)
                          (or (drover-list--compact-p field)
                              (drover-list--icon-alone-p field layout))))
             (text (if iconic "" (drover-list--compact full))))
        (cond ((string-empty-p full)
               (cl-pushnew field whole))
              ((not (string-empty-p text))
               (let ((start (- end (drover-list--pixel-width text))))
                 (when (> start (+ title-start space))
                   (setq right (append (list (list 'align start) (drover-list--text-segment text start))
                                       right)
                         end (- start space))
                   (when (eq text full)
                     (push field whole))))))))
    (let* ((cache (and (memq 'cache (funcall narrow 'background)) (drover-list--row-cache row)))
           (title (drover-list--row-title row))
           (line (drover-list--assemble
                  (append (drover-list--title-segments row (- end title-start) shift)
                          right)
                  (and cache width)))
           (below-fields (funcall narrow 'below))
           (below (and (drover-session-attention-p session)
                       (string-join (seq-remove #'string-empty-p
                                                (mapcar (lambda (field) (or (alist-get field texts) ""))
                                                        below-fields))
                                    "  ")))
           (below-line (and below (not (string-empty-p below))
                            (let ((faced (drover-face below 'drover-dim)))
                              (cons faced (drover-list--truncate faced (- width title-start)))))))
      (when (eq (drover-list--truncate title (- end title-start)) title)
        (push 'title whole))
      (dolist (field below-fields)
        (when (or (string-empty-p (or (alist-get field texts) ""))
                  (and below-line (eq (car below-line) (cdr below-line))))
          (push field whole)))
      (unless (drover-list--row-cache row)
        (push 'cache whole))
      (list (concat prefix
                    (car line)
                    (when below-line
                      (concat "\n" (drover-list--line-prefix layout) (drover-list--align title-start)
                              (cdr below-line))))
            (and cache (cdr line))
            whole))))

;;;; The prompt cache

(defvar-local drover-list--drawn nil
  "Hash table of the sessions drawn, by key, as records of the last drawing.")

(defvar-local drover-list--title-states nil
  "Hash table of the place of each session drawn, by key, as titles show it.
Each value is a value of `drover-list--title-state'.")

(defvar drover-list--session-buffers (make-hash-table :test #'eq :weakness 'key)
  "Buffers of the sessions lists drew in them, as keys.
Killing one of them changes the place of its session; killing another
buffer, as `with-temp-buffer' does all the time, costs a lookup.")

(defvar-local drover-list--caches nil
  "Prompt caches drawn, live when last drawn, as plists.
Each has :session, the record drawn, :start, a marker where its heading
begins, which text inserted there, rows drawn before it, leaves after
it, :kind, `columns' for a field or `narrow' for a background,
:spec, the segments of a background line from `drover-list--assemble',
:width, the width a background fills, :bar, the width of a field's bar,
in pixels, and :overlay, the overlay of a background.  The time left a
field shows is text, which changes length as time passes: positions
after it are kept by markers.")

(defun drover-list--cache-fraction (session now)
  "Return what the prompt cache of SESSION shows of its lifetime left at NOW.
It is `drover-session-shown-cache-fraction': nil for a subagent, whose
bar would only stand beside the one the user's next turn reuses."
  (drover-session-shown-cache-fraction session now))

(defun drover-list--caching-p (session metrics)
  "Return non-nil when SESSION may show a prompt cache, live or not.
That is a session at the top of its tree, whose cache shows (see
`drover-list--cache-fraction'), of a backend of the :caching of METRICS,
which may report caches: its cache is live from one turn's answers to
some minutes after, and the list keeps its room meanwhile."
  (and (not (drover-session-parent session))
       (memq (drover-session-backend session) (plist-get metrics :caching))
       t))

(defun drover-list--cache-help (session now)
  "Return the line telling how the prompt cache of SESSION stands at NOW."
  (concat "Prompt cache: " (or (drover-session-cache-description session now) "expired")))

(defun drover-list--cache-help-echo (window object position)
  "Return the tooltip of the prompt cache drawn at POSITION in OBJECT.
OBJECT is the list's buffer, else WINDOW shows it.  The tooltip tells
how the cache stands when it shows, see
`drover-session-cache-description', after the text of the property
`drover-list-help' there, if any: in two lines, where the cache is the
background of the line, that of the session.  For `help-echo'."
  (drover-with-reporting "list tooltip"
    (with-current-buffer (cond ((bufferp object) object)
                               ((window-live-p window) (window-buffer window))
                               (t (current-buffer)))
      (when-let* ((section (drover-list--session-section (magit-section-at position)))
                  (session (and drover-list--drawn (gethash (oref section value) drover-list--drawn))))
        (string-join (delq nil (list (get-text-property position 'drover-list-help)
                                     (drover-list--cache-help session (drover-list--now))))
                     "\n")))))

(defun drover-list--fitting (string pixels)
  "Return how many characters of STRING, from its start, fit in PIXELS."
  (let ((low 0)
        (high (length string)))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (<= (drover-list--pixel-width (substring string 0 middle)) pixels)
            (setq low middle)
          (setq high (1- middle)))))
    low))

(defun drover-list--put-display (position value)
  "Make VALUE the display of the character at POSITION, unless it is already.
Putting a property changes the buffer even when the value is the same,
which has Emacs lay out the windows showing it again."
  (unless (equal (get-text-property position 'display) value)
    (put-text-property position (1+ position) 'display value)))

(defun drover-list--draw-background (entry fraction)
  "Colour the line of ENTRY, from the left, over FRACTION of its width.
FRACTION nil colours nothing.  The bar ends at a space, stretched to
the pixel, or between two characters of text.  What is drawn already
is left as it is."
  (let* ((start (plist-get entry :start))
         (overlay (plist-get entry :overlay))
         (x (and fraction (round (* fraction (plist-get entry :width)))))
         (end start)
         (done (not x)))
    (dolist (item (plist-get entry :spec))
      (pcase item
        (`(,offset split ,from ,to)
         (let ((width 0))
           (unless done
             (cond ((>= x to)
                    (setq width (- to from)
                          end (+ start offset 2)))
                   ((> x from)
                    (setq width (- x from)
                          end (+ start offset 1)
                          done t))
                   (t (setq done t))))
           (drover-list--put-display (+ start offset) `(space :width (,(max 0 width))))))
        (`(,offset text ,from ,width ,string)
         (unless done
           (if (>= x (+ from width))
               (setq end (+ start offset (length string)))
             ;; The text as shown now: an animated glyph may show
             ;; another frame than when the line was drawn.
             (setq end (+ start offset
                          (drover-list--fitting
                           (buffer-substring (+ start offset) (+ start offset (length string)))
                           (- x from)))
                   done t))))))
    (unless (and (eq (overlay-buffer overlay) (current-buffer))
                 (= (overlay-start overlay) start)
                 (= (overlay-end overlay) end))
      (move-overlay overlay start end))))

(defun drover-list--draw-field (entry fraction remaining)
  "Draw the prompt cache field of ENTRY, FRACTION full, REMAINING seconds left.
FRACTION nil draws nothing in its place.  The bar changes its display
alone; the time left, text, is replaced when it reads otherwise, in the
properties it had, the section it belongs to included."
  (let* ((start (plist-get entry :start))
         (bar (plist-get entry :bar))
         (full (if fraction (round (* bar fraction)) 0))
         (end (save-excursion (goto-char start) (line-end-position)))
         (label nil))
    (let ((position start))
      (while (setq position (text-property-not-all position end 'drover-list-cache nil))
        (pcase (get-text-property position 'drover-list-cache)
          ('label (setq label (or label position)))
          (part (drover-list--put-display
                 position
                 (pcase part
                   ('full `(space :width (,full)))
                   ('trough `(space :width (,(if fraction (- bar full) 0))))
                   ('space (if fraction nil '(space :width (0))))))))
        (setq position (1+ position))))
    (when label
      (let* ((label-end (next-single-property-change label 'drover-list-cache nil end))
             (text (if fraction (opencode-format-duration remaining) "")))
        (unless (equal (buffer-substring-no-properties label label-end) text)
          (let ((properties (text-properties-at label)))
            (save-excursion
              ;; Inserted before the old text goes, so that a marker
              ;; right after the label stays after the new one.
              (goto-char label)
              (insert (apply #'propertize text properties))
              (delete-region (point) (+ (point) (- label-end label))))))))))

(defun drover-list--drop-caches ()
  "Forget the prompt caches drawn in the current list, their overlays and markers.
The buffer keeps overlays its text no longer holds, empty where it was
erased, until they are deleted."
  (dolist (entry drover-list--caches)
    (when-let* ((overlay (plist-get entry :overlay)))
      (delete-overlay overlay))
    (set-marker (plist-get entry :start) nil))
  (setq drover-list--caches nil))

(defun drover-list--draw-caches (&optional now ranges)
  "Redraw the prompt caches of the current list as they stand at NOW.
NOW defaults to the current time.  Only the caches change: the display
of their bars, and the text of the time left.  A cache gone out shows
nothing, and is forgotten.  With RANGES, the parts of the buffer
windows show, as (START . END), a cache still live elsewhere is left as
it was drawn, as `drover-list--draw-frames' leaves animated glyphs."
  (when (drover-list--current-p)
    (let ((now (or now (drover-list--now)))
          (ranges (or ranges t))
          (inhibit-read-only t)
          (deactivate-mark nil))
      (with-silent-modifications
        (setq drover-list--caches
              (seq-filter
               (lambda (entry)
                 (let* ((session (plist-get entry :session))
                        (fraction (drover-list--cache-fraction session now)))
                   (when (or (not fraction) (drover-list--in-ranges-p (plist-get entry :start) ranges))
                     (if (eq (plist-get entry :kind) 'narrow)
                         (drover-list--draw-background entry fraction)
                       (drover-list--draw-field entry fraction
                                                (drover-session-cache-remaining session now))))
                   (unless fraction
                     (when-let* ((overlay (plist-get entry :overlay)))
                       (delete-overlay overlay))
                     (set-marker (plist-get entry :start) nil))
                   fraction))
               drover-list--caches))))))

;;;; Animated glyphs

(defvar-local drover-list--frames nil
  "Frames of the animations of the last drawing, from `drover-list--metrics'.
An alist (STATUS STRINGS WIDTHS WIDTH): the frames of the glyph of
STATUS, their widths and the right of the cell they are drawn in.")

(defvar-local drover-list--animated nil
  "Animated glyphs drawn, as a list of (POSITION . KIND).
POSITION is a marker at the character displaying the frame, the next
one its pad: the time left of a prompt cache before it may change
length.  KIND is their animation, of `opencode-session-status-animations'.")

(defun drover-list--collect-animated (&optional start end)
  "Return the animated glyphs of the current list, between START and END.
They default to the whole buffer.  The value is a list of (POSITION .
KIND), see `drover-list--animated'."
  (let ((position (or start (point-min)))
        (end (or end (point-max)))
        (found nil))
    (while (setq position (text-property-not-all position end 'drover-list-frame nil))
      ;; Advancing, as the marks of a prompt cache: a row drawn in place
      ;; before a glyph beginning its line goes before it.
      (push (cons (copy-marker position t) (get-text-property position 'drover-list-frame)) found)
      (setq position (1+ position)))
    (nreverse found)))

(defun drover-list--in-ranges-p (position ranges)
  "Return non-nil when RANGES is t, or POSITION lies in one of RANGES.
RANGES is a list of (START . END), the parts of the buffer windows show."
  (or (eq ranges t)
      (seq-some (lambda (range) (<= (car range) position (cdr range))) ranges)))

(defun drover-list--draw-frames (&optional now ranges)
  "Show the frames of the animated glyphs of the current list at NOW.
NOW defaults to the current time.  Only the display of their two
characters changes, and only where the frame did: neither the text nor
the sections.  Glyphs out of sight, in a collapsed group, are left as
they are until they show; so are those out of RANGES, the parts of the
buffer windows show, as (START . END), when given: changing the text
of a buffer that the selected window does not show has Emacs lay out
every line of the windows showing it again."
  (when (drover-list--current-p)
    (when opencode-session-status-animation-interval
      (let ((now (or now (drover-list--now)))
            (ranges (or ranges t))
            (inhibit-read-only t))
        (with-silent-modifications
          (pcase-dolist (`(,position . ,kind) drover-list--animated)
            (pcase-let ((`(,strings ,widths ,width) (alist-get kind drover-list--frames)))
              (when strings
                (let* ((index (opencode-session-status-frame-index (length strings) now))
                       (frame (nth index strings)))
                  (unless (or (eq (get-text-property position 'display) frame)
                              (not (drover-list--in-ranges-p position ranges))
                              (invisible-p position))
                    (put-text-property position (1+ position) 'display frame)
                    (put-text-property (1+ position) (+ position 2) 'display
                                       `(space :width (,(- width (nth index widths)))))))))))))))

;;;; Drawing

(defvar drover-list-mode-map)

(defvar-local drover-list--stale nil
  "Non-nil when the hidden list must be drawn whole once shown.
That is when anything may have changed, such as the settings, or more
sessions changed than drawing them in place is worth; otherwise the
list notes which (see `drover-list--pending').")

(defvar-local drover-list--pending nil
  "What changed while the list was hidden, to draw once it is shown.
A list (KEYS BODIES TIMES): the keys of the sessions changed, those
whose details are to be drawn again, as `drover-list--follow' takes
them, and non-nil when time passed, ages reading otherwise.  A hidden
list draws nothing, and catches up in place when shown.")

(defconst drover-list--pending-limit 64
  "Most sessions a list draws in place before drawing whole.
Past this many, drawing them in place costs about what drawing the
list whole does: a hidden list notes as many before drawing whole once
shown, and a list shown draws whole a change of more, such as the
first look at thousands of transcripts.")

(defvar-local drover-list--width nil
  "Width, in pixels, of the lines the list was last drawn for.")

(defvar-local drover-list--layout nil
  "Layout of the last drawing, `columns' or `narrow'.")

(defvar-local drover-list--fields nil
  "Fields of `drover-list-fields' the last drawing shows, in their order.")

(defun drover-list--model-label (session)
  "Return the model of SESSION and its effort as one plain string, or nil."
  (let ((label (string-join (delq nil (list (drover-session-model session) (drover-session-effort session)))
                            " ")))
    (unless (string-empty-p label) label)))

(defun drover-list--help (session &optional subagents)
  "Return the tooltip of SESSION: its full title and details.
SUBAGENTS, its active subagents, are named after them.  It is bounded
\(see `drover-bound-lines')."
  (drover-bound-lines
   (string-join
    (delq nil (list (drover-list--one-line (drover-session-display-title session))
                    (drover-session-location session)
                    (let ((agent (drover-session-agent session))
                          (model (drover-list--model-label session)))
                      (cond ((and agent model) (format "%s (%s)" agent model))
                            (t (or agent model))))
                    (when-let* ((summary (drover-session-summary session)))
                      (drover-list--one-line summary))
                    (and subagents (drover-list--subagents-help subagents))))
    "\n")
   10))

(defun drover-list--put-help (string help &optional start)
  "Give HELP as their tooltip to the characters of STRING without one, in place.
From START, 0 by default.  The glyphs of active subagents keep their
own.  Return STRING."
  (let ((position (or start 0)))
    (while (< position (length string))
      (let ((next (next-single-property-change position 'help-echo string (length string))))
        (unless (get-text-property position 'help-echo string)
          (put-text-property position next 'help-echo help string))
        (setq position next)))
    string))

(defun drover-list--wrap (text pixels lines)
  "Return TEXT as at most LINES lines at most PIXELS wide, as a list.
Lines break between words, and within a word wider than a line; blank
lines are dropped, and spaces within a line count as one.  When TEXT
takes more lines, the last one ends in an ellipsis."
  (let ((result nil)
        (count 0)
        (more nil)
        (pixels (max pixels (drover-list--char-width))))
    (cl-flet ((add (line)
                (if (>= count lines)
                    (throw 'full (setq more t))
                  (push line result)
                  (cl-incf count))))
      (catch 'full
        (dolist (paragraph (split-string (or text "") "\n"))
          (let ((words (split-string paragraph "[ \t\r]+" t))
                (line nil))
            (while words
              (let ((candidate (if line (concat line " " (car words)) (car words))))
                (cond ((<= (drover-list--pixel-width candidate) pixels)
                       (setq line candidate
                             words (cdr words)))
                      (line
                       (add line)
                       (setq line nil))
                      (t
                       ;; A word wider than a line: what fits, then the rest.
                       (let ((fit (max 1 (drover-list--fitting (car words) pixels))))
                         (add (substring (car words) 0 fit))
                         (setcar words (substring (car words) fit)))))))
            (when line
              (add line))))))
    (when (and more result)
      (setcar result (drover-list--truncate (concat (car result) (truncate-string-ellipsis)) pixels)))
    (nreverse result)))

(defvar-local drover-list--recaps nil
  "Recaps of the last turns of sessions, as a hash table by session key.
Each value is a plist: :updated, the stamp of the session the recap was
asked at (see `drover-list--recap-stamp'); :recap, the recap of
`drover-recap', when one
arrived; :pending, a token while a fetch runs, an answer counting only
when its token is still there; :fetched, when the last fetch began.")

(defvar-local drover-list--recap-timer nil
  "Timer drawing the list again, once a recap asked too soon may be fetched.")

(defvar drover-list--recap-asking nil
  "Non-nil while the list asks for a recap.
A recap arriving then is drawn by the asking, not by a redraw.")

(defconst drover-list--recap-interval 2
  "Seconds at least between two fetches of the recap of one session.
A session at work changes many times a second, and each change made
its recap stale: an expanded session fetches its recap again at most
this often while it changes, and once more after.")

(defun drover-list--recap-stamp (session)
  "Return what the recap of SESSION was asked at, to tell when it is stale.
Its last activity, and what it changed, which is read once its turn
ended: the recap tells the files (see `drover-recap')."
  (cons (drover-session-updated session) (drover-session-changes session)))

(defun drover-list--recap-entry (session)
  "Return what the list knows of the recap of SESSION, fetching it when stale.
The value is a plist of :recap, the recap last known, or nil, :known,
non-nil once a recap arrived, and :loading, non-nil while a fetch
runs.  A recap is fresh while the session's last activity and changes
are those it was asked at (see `drover-list--recap-stamp'); a stale one
is fetched again in the background, at most every
`drover-list--recap-interval' seconds, and the list drawn again once it
arrives.  Nil when the backend of SESSION tells no recap."
  (when (drover-supports-p session :recap)
    (unless drover-list--recaps
      (setq drover-list--recaps (make-hash-table :test #'equal)))
    (let* ((key (drover-session-key session))
           (updated (drover-list--recap-stamp session))
           (entry (gethash key drover-list--recaps))
           (fresh (and (plist-member entry :recap)
                       (equal (plist-get entry :updated) updated))))
      (unless (or fresh (plist-get entry :pending))
        (let ((wait (and (plist-get entry :fetched)
                         (- (+ (plist-get entry :fetched) drover-list--recap-interval)
                            (drover-list--now)))))
          (if (and wait (> wait 0))
              (drover-list--recap-later wait key)
            (drover-list--fetch-recap session)
            (setq entry (gethash key drover-list--recaps)))))
      (list :recap (plist-get entry :recap)
            :known (and (plist-member entry :recap) t)
            :loading (and (plist-get entry :pending) t)))))

(defvar-local drover-list--recap-deferred nil
  "Keys of the sessions whose recap is to be fetched once the timer runs.")

(defun drover-list--recap-later (delay key)
  "Draw the details of session KEY again in DELAY seconds, fetching its recap.
One timer draws those of every session put off meanwhile."
  (cl-pushnew key drover-list--recap-deferred :test #'equal)
  (unless (timerp drover-list--recap-timer)
    (let ((buffer (current-buffer)))
      (setq drover-list--recap-timer
            (run-at-time delay nil
                         (lambda ()
                           (drover-with-reporting "list recap"
                             (when (buffer-live-p buffer)
                               (let ((keys (buffer-local-value 'drover-list--recap-deferred buffer)))
                                 (with-current-buffer buffer
                                   (setq drover-list--recap-timer nil
                                         drover-list--recap-deferred nil))
                                 (drover-list--update-sessions buffer nil keys))))))))))

(defun drover-list--fetch-recap (session)
  "Ask the backend of SESSION for its recap, for the current list.
The recap is kept for the last activity SESSION has now.  Arriving
later than the asking, it draws the list again if SESSION's details
show; an answer to a fetch the list forgot, as a refresh does, is
dropped."
  (let* ((buffer (current-buffer))
         (key (drover-session-key session))
         (updated (drover-list--recap-stamp session))
         (token (list 'recap))
         (fetched (drover-list--now))
         (entry (gethash key drover-list--recaps)))
    (puthash key (append (list :pending token :fetched fetched)
                         (and (plist-member entry :recap)
                              (list :updated (plist-get entry :updated) :recap (plist-get entry :recap))))
             drover-list--recaps)
    (let ((drover-list--recap-asking t))
      (drover-recap session
                    (lambda (recap)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when (and drover-list--recaps
                                     (eq (plist-get (gethash key drover-list--recaps) :pending) token))
                            (puthash key (list :updated updated :recap recap :fetched fetched)
                                     drover-list--recaps)
                            (unless drover-list--recap-asking
                              (drover-list--recap-arrived key))))))))))

(defun drover-list--recap-arrived (key)
  "Draw the current list again if the details of the session KEY show."
  (when-let* ((section (drover-list--find-session key))
              ((not (oref section hidden)))
              ((drover-list--line-shown-p (oref section start))))
    (drover-list--update-sessions (current-buffer) nil (list key))))

(defun drover-list--forget-recaps (sessions)
  "Forget the recaps of sessions other than SESSIONS."
  (when drover-list--recaps
    (let ((keys (make-hash-table :test #'equal)))
      (dolist (session sessions)
        (puthash (drover-session-key session) t keys))
      (maphash (lambda (key _)
                 (unless (gethash key keys)
                   (remhash key drover-list--recaps)))
               drover-list--recaps))))

(defconst drover-list--former-recap-lines
  '(((summary . 4) (request . 6) (error . 6) (prompt . 3) (reply . 8) (changes . 2)))
  "The former standard values of `drover-list-recap-lines'.
Before the changes listed their files, a line each.")

;; Loaded over a drover of a former standard value, which `defcustom'
;; would keep, the details take the new one.
(when (and (boundp 'drover-list-recap-lines) (member drover-list-recap-lines drover-list--former-recap-lines))
  (makunbound 'drover-list-recap-lines))

(defcustom drover-list-recap-lines
  '((summary . 4) (request . 6) (error . 6) (prompt . 3) (reply . 8) (changes . 100))
  "Lines each part of the details of an expanded session takes at most.
The details recap the session's last turn: the recap a model wrote of
it (see `drover-summary-mode'), what it waits for, the error that ended
it, the user's prompt and the agent's reply; then the files all its
turns changed, a line each.  A part cut short ends in an ellipsis, the
files in how many more there are; the whole reply shows in the session
itself, or in a preview, the whole changes in a review.  An alist (PART
. LINES); a part of 0 lines is left out."
  :type '(alist :key-type (choice (const :tag "Recap written by a model" summary)
                                  (const :tag "What the session waits for" request)
                                  (const :tag "Error" error)
                                  (const :tag "Prompt" prompt)
                                  (const :tag "Reply" reply)
                                  (const :tag "Files changed" changes))
                :value-type natnum)
  :group 'drover-list)

(defun drover-list--recap-lines (part)
  "Return the lines PART of the details takes at most."
  (or (alist-get part drover-list-recap-lines) (if (eq part 'changes) 100 3)))

(defun drover-list--file-line (change directory)
  "Return CHANGE, a file a session changed, as a line of its details.
CHANGE is (FILE ADDITIONS DELETIONS), as `drover-recap' tells it: the
name, relative to DIRECTORY when under it, then the lines added and
removed, kept whole when the line is cut (see `drover-list--kept').
Files under DIRECTORY, one of this host, are named relative to it;
those of another host's session come so named by it."
  (let ((name (drover-list--one-line (drover-relative-file-name (car change) directory)))
        (counts (delq nil (list (and (natnump (nth 1 change))
                                     (drover-face (format "+%d" (nth 1 change)) 'drover-added))
                                (and (natnump (nth 2 change))
                                     (drover-face (format "−%d" (nth 2 change)) 'drover-removed))))))
    (if counts
        (concat name "  " (propertize (string-join counts " ") 'drover-list-keep t))
      name)))

(defun drover-list--changes-lines (session recap whole)
  "Return the lines of the details telling what SESSION changed, or nil.
The files all its turns changed, from RECAP's :files, a line each (see
`drover-list--file-line'), after their counts unless WHOLE, the fields
its line shows whole, holds `changes'; without them, the counts its
record tells, unless its line shows them whole; without those, the
files its last turn changed, as RECAP's :changes tells them, after
their counts.  At most the lines `drover-list-recap-lines' gives the
part `changes', the last telling how many more files there are."
  (let* ((directory (drover-session-local-directory session))
         (totals (drover-session-changes session))
         (session-files (seq-filter #'consp (plist-get recap :files)))
         (files (or session-files (and (null totals) (seq-filter #'consp (plist-get recap :changes)))))
         (limit (drover-list--recap-lines 'changes)))
    (when (> limit 0)
      (if (null files)
          (unless (memq 'changes whole)
            (when-let* ((line (drover-list--changes-text session)))
              (list line)))
        (let* ((count (if session-files (max (length files) (or (nth 2 totals) 0)) (length files)))
               (head (if session-files
                         (unless (memq 'changes whole)
                           (drover-list--changes-line (nth 0 totals) (nth 1 totals) count))
                       (let ((additions (delq nil (mapcar #'cadr files)))
                             (deletions (delq nil (mapcar #'caddr files))))
                         (concat (drover-list--changes-line (and additions (apply #'+ additions))
                                                            (and deletions (apply #'+ deletions))
                                                            count)
                                 (drover-face " in the last turn" 'drover-dim)))))
               (room (- limit (if head 1 0)))
               (shown (min (length files) (if (> count room) (max 0 (1- room)) count)))
               (lines (mapcar (lambda (change) (drover-list--file-line change directory))
                              (seq-take files shown))))
          (append (and head (list head))
                  lines
                  (and (> count shown) (> room 0)
                       (list (drover-face (format "and %d more file%s" (- count shown)
                                                  (if (= (- count shown) 1) "" "s"))
                                          'drover-dim)))))))))

(defun drover-list--hidden-facts (session layout whole)
  "Return what the line of SESSION does not show of it, as (LABEL VALUE FACE).
Those are the fields of `drover-list-fields' its line, in LAYOUT,
leaves out, or shows cut or as an icon alone: those but WHOLE, the
fields it shows whole (see `drover-list--heading'), such as the model
in two lines; and what its line cannot tell: the directory of a session
without a project, the task it serves, and why its host is not
connected."
  (let ((now (plist-get layout :now))
        (facts nil))
    (cl-flet ((fact (field label value &optional face)
                (when (and value (not (memq field whole))
                           (not (string-empty-p (drover-list--one-line value))))
                  (push (list label value face) facts))))
      (unless (drover-session-project session)
        ;; Another host's, as it names it: not this host's to abbreviate.
        (fact 'directory "Directory" (if-let* ((directory (drover-session-local-directory session)))
                                         (abbreviate-file-name directory)
                                       (drover-session-directory session))))
      (fact 'goal "Goal" (and drover-list-goal-function
                              (drover-with-reporting "goal"
                                (funcall drover-list-goal-function session 'column))))
      (fact 'attempt "Attempt" (drover-list--attempt-text session t))
      (when-let* ((host (drover-session-host session)))
        (fact 'host "Host" (drover-host-label host))
        (fact 'host-state "Not connected" (drover-host-state host) 'drover-dim))
      (fact 'harness "Harness" (drover-session-harness-description session))
      (fact 'location "Project" (drover-session-view-project session))
      (fact 'task "Task" (plist-get (drover-session-task session) :title))
      (fact 'branch "Branch" (drover-session-branch session))
      (fact 'agent "Agent" (drover-agent-label session))
      (fact 'model "Model" (let ((text (drover-list--model-text session)))
                             (unless (string-empty-p text) text)))
      (fact 'provider "Provider" (drover-session-provider-name session))
      (fact 'context "Context" (when-let* ((context (drover-session-context-description session)))
                                 (string-replace "\n" "; " context))
            (pcase (car (drover-session-context-tier session))
              ('over 'opencode-context-over)
              ('near 'opencode-context-near)))
      (fact 'created "Created" (when-let* ((created (drover-session-created session)))
                                 (concat (opencode-format-age created now) " ago")))
      (fact 'age "Age" (when-let* ((time (drover-list--age-time session)))
                         (concat (opencode-format-age time now) " ago")))
      (fact 'cache "Prompt cache" (and (not (drover-session-parent session))
                                       (drover-session-cache-description session now))))
    (nreverse facts)))

(defun drover-list--details (session layout whole)
  "Return the details of SESSION, as a list of (LABEL TEXT LINES FACE).
They recap its last turn, from what the session tells and the recap
its backend fetches (see `drover-list--recap-entry'): its title, when
its line cuts it, the recap a model wrote of it, if any (see
`drover-session-brief-recap'), what it waits for, the error that ended
or holds it, or how it stands, the user's last prompt and the agent's
reply; then the files all its turns changed (see
`drover-list--changes-lines'), and what its line leaves out in LAYOUT.
WHOLE lists the fields its line shows whole (see
`drover-list--heading'): what it shows whole is not repeated.  TEXT
takes at most LINES lines, wrapped; LINES nil keeps it to one, cut; a
list TEXT is its lines, each cut.  FACE, if any, is that of TEXT."
  (let* ((status (drover-session-status session))
         (entry (drover-list--recap-entry session))
         (recap (plist-get entry :recap))
         (failure (plist-get recap :failure))
         (summary (memq 'summary whole))
         (problem (and (memq status '(failed retrying))
                       (or (and (eq status 'failed) (plist-get recap :error))
                           (drover-session-detail session))))
         (details nil))
    (cl-flet ((add (label text part &optional face)
                (when (and text (not (string-blank-p text))
                           (not (and part (zerop (drover-list--recap-lines part)))))
                  (push (list label text (and part (drover-list--recap-lines part)) face) details))))
      (unless (memq 'title whole)
        (add "Title" (drover-session-display-title session) nil))
      (add "Summary" (let ((brief (drover-session-brief session))
                           (recap (drover-session-brief-recap session)))
                       ;; Its line is the summary its row shows, if any.
                       (if (or (null brief) summary)
                           recap
                         (string-join (delq nil (list brief recap)) "\n")))
           'summary)
      (dolist (request (drover-session-requests session))
        (add (if (eq (plist-get request :kind) 'question) "Question" "Permission")
             (plist-get request :summary) 'request 'opencode-session-status-waiting))
      ;; Not what the line shows whole already, the same text.
      (unless (and summary problem
                   (eq (drover-list--one-line problem) problem)
                   (equal (drover-session-summary session) problem))
        (add (if (eq status 'retrying) "Retrying" "Error") problem 'error (opencode-session-status-face status)))
      (unless (or summary problem (drover-session-brief session) (drover-session-requests session))
        (add "Status" (drover-session-detail session) nil (opencode-session-status-face status)))
      (add "Prompt" (plist-get recap :prompt) 'prompt)
      (add "Reply" (plist-get recap :reply) 'reply)
      (cond ((and (plist-get entry :loading) (not (plist-get entry :known)))
             (add "Last turn" "loading…" nil 'drover-dim))
            (failure (add "Last turn" (concat "not available: " failure) nil 'drover-dim))
            ((and (plist-get entry :known)
                  (not (or (plist-get recap :prompt) (plist-get recap :reply) (plist-get recap :changes))))
             (add "Last turn" "nothing to recap" nil 'drover-dim)))
      (when-let* ((lines (drover-list--changes-lines session recap whole)))
        (push (list "Changes" lines nil nil) details)))
    (append (nreverse details)
            (mapcar (pcase-lambda (`(,label ,value ,face)) (list label value nil face))
                    (drover-list--hidden-facts session layout whole)))))

(defun drover-list--body-x (row layout)
  "Return where the details of ROW begin in LAYOUT, under its title, in pixels."
  (+ (drover-list--row-title-start row)
     (if-let* ((column (assq 'title (plist-get layout :columns))))
         (- (nth 1 column) (plist-get layout :margin))
       (drover-list--shift layout))))

(defun drover-list--row-whole (row layout)
  "Return the fields the line of ROW shows whole in LAYOUT, or names otherwise.
Those its heading shows whole (see `drover-list--heading'), and the
field its group names, such as the project of a group of a project,
which its line leaves out."
  (let ((whole (nth 4 (drover-list--heading row layout)))
        (group (and (drover-list--row-inputs row) (aref (drover-list--row-inputs row) 3))))
    (if-let* ((field (pcase (car group)
                       ('host 'host)
                       ('harness 'harness)
                       ('project 'location)
                       ('branch 'branch))))
        (cons field whole)
      whole)))

(defun drover-list--insert-details (row layout)
  "Insert the details of the session of ROW, under its title, in LAYOUT.
Each label is followed by its text, wrapped to the window and aligned
after the widest label; when that leaves little room, as in a side
window, the texts go under their labels instead.  Every line begins
with the line prefix of LAYOUT (see `drover-list--line-prefix').  What
its line shows whole is left out (see `drover-list--row-whole')."
  (let* ((session (drover-list--row-session row))
         (x (drover-list--body-x row layout))
         (width (plist-get layout :width))
         (details (progn
                    (drover-list--prefetch-changes session)
                    (mapcar (pcase-lambda (`(,label . ,rest))
                              (cons (drover-face label 'drover-dim) rest))
                            (drover-list--details session layout (drover-list--row-whole row layout)))))
         (values-x (+ x (plist-get layout :gap)
                      (apply #'max 0 (mapcar (lambda (detail) (drover-list--pixel-width (car detail)))
                                             details))))
         (stacked (< (- width values-x) (* 20 (plist-get layout :char))))
         (text-x (if stacked x values-x))
         (prefix (drover-list--line-prefix layout)))
    (cl-flet ((line (&rest parts)
                (apply #'insert prefix parts)
                (insert "\n")))
      (pcase-dolist (`(,label ,text ,lines ,face) details)
        (let* ((room (- width text-x))
               (texts (cond ((listp text)
                             (mapcar (lambda (line) (drover-list--truncate line room)) text))
                            (lines (drover-list--wrap text room lines))
                            (t (list (drover-list--truncate (drover-list--one-line text) room)))))
               (texts (mapcar (lambda (text) (if face (drover-face text face) text)) texts)))
          (if stacked
              (line (drover-list--align x) label)
            (line (drover-list--align x) label (drover-list--align values-x) (or (car texts) ""))
            (setq texts (cdr texts)))
          (dolist (text texts)
            (line (drover-list--align text-x) text)))))))

(defun drover-list--row-key (row)
  "Return the key of the session of ROW."
  (drover-session-key (drover-list--row-session row)))

(defun drover-list--row-subagents (row layout)
  "Return the active subagents of the session of ROW, from its inputs or LAYOUT."
  (if-let* ((inputs (drover-list--row-inputs row)))
      (aref inputs 4)
    (and (plist-get layout :active)
         (gethash (drover-list--row-key row) (plist-get layout :active)))))

(defun drover-list--heading (row layout)
  "Return the heading of ROW in LAYOUT, as (TEXT KIND SPEC KEY WHOLE).
TEXT is its line or lines, beginning with the line prefix of LAYOUT
\(see `drover-list--line-prefix'), the tooltip of the session on the
characters without one of their own; KIND is `columns' when it shows a
prompt cache field, `narrow' when a prompt cache colours its first
line, else nil; SPEC tells where the segments of that line lie, see
`drover-list--assemble'.  KEY is the
layout key the heading was drawn for (see `drover-list--layout-key'):
the heading is kept in ROW, and drawn again only for another layout.
WHOLE lists the fields TEXT shows whole, or has nothing to show for,
which the details leave out (see `drover-list--details')."
  (let ((key (or (plist-get layout :key) (drover-list--layout-key layout)))
        (heading (drover-list--row-heading row)))
    (if (and heading (eq (nth 3 heading) key))
        heading
      (setf (drover-list--row-heading row)
            (let* ((session (drover-list--row-session row))
                   (prefix (drover-list--line-prefix layout))
                   ;; Their text is a string of their own, given its
                   ;; tooltips in place: each copy of a heading copies the
                   ;; properties of its runs.
                   (drawn (if (eq (plist-get layout :layout) 'columns)
                              (let ((line (drover-list--column-line row layout prefix)))
                                (list (nth 0 line) (and (nth 1 line) 'columns) nil (nth 2 line)))
                            (let ((heading (drover-list--narrow-line row layout prefix)))
                              (list (nth 0 heading) (and (nth 1 heading) 'narrow) (nth 1 heading)
                                    (nth 2 heading)))))
                   (help (drover-list--help session (drover-list--row-subagents row layout)))
                   (text (car drawn))
                   (start (length prefix)))
              (if (eq (nth 1 drawn) 'narrow)
                  ;; The cache colours the line: its tooltip tells of it
                  ;; too, as it stands when shown.
                  (progn (put-text-property start (length text) 'drover-list-help help text)
                         (drover-list--put-help text #'drover-list--cache-help-echo start))
                (drover-list--put-help text help start))
              (list text (nth 1 drawn) (nth 2 drawn) key (nth 3 drawn)))))))

(defun drover-list--note-row (row heading start layout)
  "Note ROW, drawn with HEADING from START, in the tables of the list.
Those are the sessions drawn and where they are, and the prompt cache
the heading shows, if any, in `drover-list--caches', LAYOUT giving its
measures.  START is where the heading begins."
  (let* ((session (drover-list--row-session row))
         (key (drover-session-key session))
         (start (+ start (length (drover-list--line-prefix layout)))))
    (puthash key session drover-list--drawn)
    (puthash key (drover-list--row-state row) drover-list--title-states)
    (when (drover-list--row-state row)
      (puthash (drover-session-buffer session) t drover-list--session-buffers))
    (when (nth 1 heading)
      ;; Rows drawn in place before this one go before its marks: in
      ;; the sidebar, without a line prefix, they begin where it does.
      (push (list :session session :start (copy-marker start t) :kind (nth 1 heading) :spec (nth 2 heading)
                  :width (plist-get layout :width)
                  :bar (* drover-list-cache-bar-width (plist-get layout :char))
                  :overlay (and (eq (nth 1 heading) 'narrow)
                                (let ((overlay (make-overlay start start nil t nil)))
                                  (overlay-put overlay 'face (drover-list--cache-line-face))
                                  ;; Under the highlights of the current section and
                                  ;; the region, whose backgrounds leave its underline.
                                  (overlay-put overlay 'priority -50)
                                  overlay)))
            drover-list--caches))))

(defun drover-list--insert-details-of (key)
  "Insert the details of the session KEY, as the list knows it now.
The body of a collapsed section is drawn once expanded, maybe long
after the drawing: from the row the list holds then."
  (when-let* ((model drover-list--model)
              (row (gethash key (drover-list--model-rows model))))
    (drover-list--insert-details row (drover-list--model-layout model))))

(defun drover-list--insert-heading (text)
  "Insert TEXT, a heading in faces, as `magit-insert-heading' inserts it.
Magit concatenates its arguments and looks for faces in them, which
costs thousands of headings a copy and two scans for nothing."
  (insert text)
  (unless (bolp)
    (insert ?\n))
  (when (fboundp 'magit-maybe-make-margin-overlay)
    (magit-maybe-make-margin-overlay))
  (oset magit-insert-section--current content
        (if magit-section-inhibit-markers (point) (point-marker))))

(defun drover-list--insert-session (row layout)
  "Insert the section of the session of ROW, in LAYOUT.
Its heading is its line; its body, drawn once expanded, its details,
as `drover-list--insert-details' draws them, in a list that folds; the
sidebar's sessions have none.  A prompt cache it shows is noted in
`drover-list--caches'."
  (let* ((heading (drover-list--heading row layout))
         (key (drover-list--row-key row))
         (folds (plist-get layout :folds))
         (start (point)))
    (magit-insert-section section (drover-list-session-section key folds)
      (setf (drover-list--row-section row) section)
      (drover-list--insert-heading (car heading))
      (when folds
        (drover-list--insert-session-body key)))
    (drover-list--note-row row heading start layout)))

(defun drover-list--insert-session-body (key)
  "Insert the details of session KEY as the body of the section being inserted.
As `magit-insert-section-body' inserts a body: at once in a section
shown, once expanded in a collapsed one, whose washer then draws it,
`drover-list--wash-session'.  That washer is the same for every
session, where Magit's makes two closures per section, of thousands."
  (if (oref magit-insert-section--current hidden)
      (oset magit-insert-section--current washer #'drover-list--wash-session)
    (drover-list--insert-details-of key)))

(defun drover-list--wash-session ()
  "Draw the details of the collapsed session section Magit expands.
The washer of `drover-list--insert-session-body': it does what the
washer `magit-insert-section-body' makes does, for the section
`magit-section--opportunistic-wash' washes, its current section."
  (let* ((section magit-insert-section--current)
         (lineage (magit-section-lineage section t)))
    (dolist (ancestor lineage)
      (set-marker-insertion-type (oref ancestor end) t))
    (drover-list--insert-details-of (oref section value))
    (dolist (ancestor lineage)
      (set-marker-insertion-type (oref ancestor end) nil))
    (magit-section--set-section-properties section)
    (magit-section-maybe-remove-heading-map section)
    (magit-section-maybe-remove-visibility-indicator section)))

(defun drover-list--count (size)
  "Return SIZE, the number of rows of a group, dimmed and in parentheses."
  (drover-face (format " (%d)" size) 'drover-dim))

(defun drover-list--count-row (counts row delta)
  "Count the session of ROW DELTA times in COUNTS, a hash table by status.
Archived sessions count apart, as `archived'."
  (let* ((session (drover-list--row-session row))
         (key (if (drover-session-archived session) 'archived (drover-session-status session))))
    (puthash key (+ (gethash key counts 0) delta) counts)))

(defun drover-list--status-counts (counts)
  "Return how many sessions need the user, per status, as one string, or nil.
COUNTS is a hash table of the sessions of each status, see
`drover-list--count-row'; archived sessions never count."
  (when-let* ((parts (delq nil
                           (mapcar (lambda (status)
                                     (let ((count (gethash status counts 0)))
                                       (when (> count 0)
                                         (concat (drover-list--icon (opencode-session-status-glyph status))
                                                 (drover-face (format "%d" count)
                                                              (opencode-session-status-face status))))))
                                   drover-attention-statuses))))
    (string-join parts " ")))

(defun drover-list--group-icon (group trees)
  "Return the icon of GROUP, (GROUPING . VALUE), of TREES, and a space, or nil.
TREES is a sequence of `drover-list--tree'.  A group of a provider has
the icon its sessions show for it, when they show one rather than its
name: the first session showing one, so that a long group is not read
whole.  A group of a harness has the harness's icon."
  (pcase group
    (`(harness . ,(and harness (pred symbolp)))
     (concat (drover-list--icon (drover-face (drover-harness-icon harness) 'drover-heading)) " "))
    (`(provider . ,(and name (pred stringp)))
     (when-let* ((icon (catch 'icon
                         (seq-doseq (tree trees)
                           (dolist (row (drover-list--tree-shown tree))
                             (let ((session (drover-list--row-session row)))
                               (when-let* (((equal (drover-session-provider-name session) name))
                                           (icon (drover-session-provider session)))
                                 (throw 'icon icon)))))
                         nil))
                 ((not (equal icon name))))
       (concat (drover-list--icon (drover-face (drover-list--one-line icon) 'drover-heading)) " ")))))

(defun drover-list--group-heading (group layout)
  "Return the heading of GROUP, a `drover-list--group', fit to LAYOUT."
  (pcase-let ((`(,grouping . ,value) (drover-list--group-value group)))
    (drover-list--truncate
     (concat (and (memq grouping '(provider harness))
                  (drover-list--group-icon (drover-list--group-value group)
                                           (drover-list--group-trees group)))
             (drover-face (drover-group-label grouping value)
                          (cond ((eq grouping 'status) (list (opencode-session-status-face value) 'drover-heading))
                                ((and (null value) (not (memq grouping '(updated host)))) '(italic drover-heading))
                                (t 'drover-heading)))
             (drover-list--count (drover-list--group-size group))
             (unless (eq grouping 'status)
               (when-let* ((counts (drover-list--status-counts (drover-list--group-counts group))))
                 (concat "  " counts))))
     (plist-get layout :width))))

(defvar drover-list--drawing nil
  "Non-nil while the list is drawn whole: what is drawn is gathered after.")

(defvar drover-list--rows-left nil
  "Rows a drawing draws yet, before it leaves the others for later, or nil.
Nil draws every row of the groups shown.")

(defvar-local drover-list--undrawn nil
  "Groups of the list some trees of which are drawn later, as keys, or nil.
A list draws at once the rows of a few screens, and the others while
Emacs is idle, or as searching, copying or scrolling to them needs
them, as a list that folds draws the rows of its collapsed groups (see
`drover-list--deferred-p').  A weak hash table.")

(defun drover-list--first-rows ()
  "Return how many rows a list draws at once, before the others.
Those of two windows' height, the tallest showing it, at least those of
the frame's: rows take a line or two, and the window may grow."
  (let ((window (drover-list--window)))
    (* 2 (max (window-body-height window) (frame-height (window-frame window))))))

(defun drover-list--insert-group (group layout)
  "Insert the section of GROUP, a `drover-list--group', in LAYOUT.
Without a value, its rows go alone, in the current section.  The rows
of a collapsed group are drawn later, as Magit draws a collapsed body:
once expanded, searched or copied, or a few at a time while Emacs is
idle (see `drover-list--defer').  So are the rows past the first
screens, drawing the list whole (see `drover-list--rows-left'), its
trees drawn coming first."
  (let ((value (drover-list--group-value group)))
    (cl-flet ((rows ()
                (let ((left nil))
                  (seq-doseq (tree (drover-list--group-trees group))
                    (if (or left (and drover-list--rows-left (<= drover-list--rows-left 0)))
                        (setq left t)
                      (dolist (row (drover-list--tree-shown tree))
                        (drover-list--insert-session row layout))
                      (when drover-list--rows-left
                        (cl-decf drover-list--rows-left (length (drover-list--tree-shown tree))))))
                  (when left
                    (unless drover-list--undrawn
                      (setq drover-list--undrawn (make-hash-table :test #'eq :weakness 'key)))
                    (puthash group t drover-list--undrawn))
                  (not left))))
      (if (null value)
          (progn (setf (drover-list--group-container group) magit-insert-section--parent)
                 (rows))
        (magit-insert-section section (drover-list-group-section
                                       value (and (plist-get layout :folds)
                                                  (member value drover-list-collapsed-groups)
                                                  t))
          (setf (drover-list--group-container group) section
                (drover-list--group-heading-text group) (drover-list--group-heading group layout))
          (magit-insert-heading (drover-list--group-heading-text group))
          (magit-insert-section-body
            (if drover-list--drawing
                ;; A group drawn in part ends as its last trees come.
                (when (rows)
                  (insert "\n"))
              ;; Magit draws a body from where the section ends, as its
              ;; heading did; the rows drawn while idle come before.
              (let ((content (save-excursion (goto-char (oref section start)) (line-beginning-position 2))))
                (drover-list--draw-trees group nil)
                (insert "\n")
                (set-marker (oref section content) content)))))))))

(defun drover-list--draw-trees (group count)
  "Draw the trees of GROUP, a `drover-list--group', not drawn yet, COUNT at most.
COUNT nil draws them all.  Its trees drawn come first, those left after
them: they are drawn at point, where the rows drawn end.  Return how
many trees are left."
  (let* ((section (drover-list--group-container group))
         (layout (drover-list--model-layout drover-list--model))
         (start (point))
         (left 0))
    ;; Inserted as the list is, with positions, turned into markers once
    ;; drawn.
    (let ((magit-section-inhibit-markers t)
          ;; Pushed, not appended to thousands of siblings each.
          (magit-section-insert-in-reverse t)
          (magit-insert-section--parent section))
      (seq-doseq (tree (drover-list--group-trees group))
        (unless (drover-list--row-section (car (drover-list--tree-shown tree)))
          (if (and count (<= count 0))
              (cl-incf left)
            (dolist (row (drover-list--tree-shown tree))
              (drover-list--insert-session row layout))
            (when count
              (cl-decf count))))))
    (drover-list--use-markers section)
    (oset section children
          (mapcan (lambda (tree)
                    (when-let* ((first (drover-list--row-section (car (drover-list--tree-shown tree)))))
                      (cons first (mapcar #'drover-list--row-section (cdr (drover-list--tree-shown tree))))))
                  (append (drover-list--group-trees group) nil)))
    (drover-list--drawn-later start (point))
    left))

(defun drover-list--drawn-later (start end)
  "Take in the rows drawn between START and END after the list was drawn.
Their animated glyphs animate, their prompt caches are drawn."
  (setq drover-list--animated (append drover-list--animated (drover-list--collect-animated start end)))
  (drover-list--draw-caches)
  (drover-list--update-timers))

(defun drover-list--model-group (section)
  "Return the `drover-list--group' whose section is SECTION, or nil."
  (and drover-list--model
       (seq-find (lambda (group) (eq (drover-list--group-container group) section))
                 (drover-list--model-groups drover-list--model))))

(defun drover-list--deferred-p (group)
  "Return non-nil when trees of GROUP, a `drover-list--group', are not drawn.
Those drawn come first.  They are those of a collapsed group, which
Magit draws as it expands, or of a group drawn in part (see
`drover-list--undrawn')."
  (when-let* ((container (drover-list--group-container group)))
    (or (and (drover-list--group-value group) (oref container washer) t)
        (and drover-list--undrawn (gethash group drover-list--undrawn) t))))

(defun drover-list--draw-group (group)
  "Draw the rows of GROUP, a `drover-list--group', if they are not all drawn.
A collapsed group stays collapsed."
  (when (drover-list--deferred-p group)
    (let ((section (drover-list--group-container group))
          (inhibit-read-only t)
          (deactivate-mark nil))
      (if (oref section washer)
          (save-excursion
            (magit-section--opportunistic-wash section))
        (while (drover-list--deferred-p group)
          (drover-list--draw-some group)))
      (when (oref section hidden)
        (magit-section-hide section))
      (set-buffer-modified-p nil))))

(defun drover-list--draw-deferred (&optional start end)
  "Draw the rows of the collapsed groups of the current list not drawn yet.
With START and END, those of the groups between them only.  Searching
and copying need their text."
  (when (drover-list--current-p)
    (drover-with-reporting "list drawing"
      (when drover-list--model
        (dolist (group (drover-list--model-groups drover-list--model))
          (when (and (drover-list--deferred-p group)
                     (or (null start)
                         (let ((section (drover-list--group-container group)))
                           (and (< (oref section start) end) (> (oref section end) start)))))
            (drover-list--draw-group group)))))))

(defun drover-list--draw-undrawn ()
  "Draw the rows the current list left for later past its first screens.
Not those of its collapsed groups, which are drawn as they expand: see
`drover-list--draw-deferred' for those."
  (when (and drover-list--undrawn drover-list--model (drover-list--current-p))
    (drover-with-reporting "list drawing"
      (dolist (group (drover-list--model-groups drover-list--model))
        (when (gethash group drover-list--undrawn)
          (drover-list--draw-group group))))))

(defun drover-list--draw-near-end ()
  "Draw the rows left for later that a window of the current list nears.
For `post-command-hook': a command scrolling or moving a window of the
list to two screens of where a group's rows left for later go, as at
the end of an ungrouped list, has them drawn, rather than show the
list end early until Emacs is idle."
  (when drover-list--undrawn
    (drover-with-reporting "list drawing"
      (when-let* ((model drover-list--model)
                  (windows (get-buffer-window-list nil 'nomini t))
                  (reach (apply #'max (mapcar (lambda (window)
                                                (save-excursion
                                                  (goto-char (window-start window))
                                                  (forward-line (* 2 (window-body-height window)))
                                                  (point)))
                                              windows))))
        (dolist (group (drover-list--model-groups model))
          (when-let* (((gethash group drover-list--undrawn))
                      (section (drover-list--group-container group))
                      ((<= (oref section end) reach)))
            (drover-list--draw-group group)))))))

(defconst drover-list--deferred-chunk 50
  "Trees drawn at a time while Emacs is idle, between two looks at input.")

(defun drover-list--draw-some (group)
  "Draw some of the trees of GROUP, a `drover-list--group', not drawn yet.
They go after those drawn, inside the collapsed group, or the group
drawn in part; once none is left, the group is drawn as a
drawing draws it."
  (let* ((section (drover-list--group-container group))
         (root magit-root-section)
         (collapsed (oref section washer))
         (inhibit-read-only t)
         (deactivate-mark nil))
    (save-excursion
      (goto-char (oref section end))
      (let ((left (drover-list--draw-trees group drover-list--deferred-chunk)))
        (when (zerop left)
          (when (drover-list--group-value group)
            (insert "\n"))
          (oset section washer nil)
          (when drover-list--undrawn
            (remhash group drover-list--undrawn)))
        (dolist (ancestor (if (eq section root) (list root) (list section root)))
          (when (< (oref ancestor end) (point))
            (set-marker (oref ancestor end) (point))))
        (when (and (zerop left) (not (eq section root)))
          (magit-section--set-section-properties section)
          (magit-section-maybe-remove-visibility-indicator section))))
    ;; The rows of a group shown show as they come.
    (when collapsed
      (if (oref section hidden)
          (magit-section-hide section)
        (magit-section-show section)))
    (set-buffer-modified-p nil)))

(defvar-local drover-list--deferred-timer nil
  "Idle timer drawing the rows of collapsed groups, or nil.")

(defun drover-list--draw-idle (buffer)
  "Draw in BUFFER the trees of collapsed groups not drawn yet, while Emacs is idle.
They are drawn a few at a time, and drawing stops as input comes, until
Emacs is idle again.  A hidden list draws them once shown again."
  (drover-with-reporting "list drawing"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq drover-list--deferred-timer nil)
        (when (and drover-list--model (drover-list--visible-p buffer))
          (let ((groups (seq-filter #'drover-list--deferred-p (drover-list--model-groups drover-list--model))))
            (while (and groups (not (input-pending-p)))
              (drover-list--draw-some (car groups))
              (unless (drover-list--deferred-p (car groups))
                (pop groups)))
            (when groups
              (drover-list--defer))))))))

(defun drover-list--defer ()
  "Draw the rows of the collapsed groups of the current list once Emacs is idle.
So the buffer holds every session for other packages searching it, as
Occur or Consult, a moment after it was drawn."
  (when (and drover-list-deferred-delay
             (not (timerp drover-list--deferred-timer))
             drover-list--model
             (seq-some #'drover-list--deferred-p (drover-list--model-groups drover-list--model)))
    (setq drover-list--deferred-timer
          (run-with-idle-timer drover-list-deferred-delay nil #'drover-list--draw-idle (current-buffer)))))

(defun drover-list--flow (parts width)
  "Return PARTS, strings, laid out in lines WIDTH pixels wide, as one string.
Parts are separated by three spaces, and one that does not fit on a
line starts the next; a part wider than a line is cut."
  (let ((separator "   ")
        (lines nil)
        (line nil))
    (dolist (part parts)
      (let ((joined (if line (concat line separator part) part)))
        (if (or (null line) (<= (drover-list--pixel-width joined) width))
            (setq line joined)
          (push line lines)
          (setq line part))))
    (when line
      (push line lines))
    (mapconcat (lambda (line) (concat (drover-list--truncate line width) "\n"))
               (nreverse lines) "")))

(defun drover-list--insert-empty (sessions layout)
  "Insert why the list shows none of SESSIONS, every session known.
When there are SESSIONS, `drover-filter' left them all out; the View
menu tells which filter and clears it.  Lines fit the width of LAYOUT."
  (magit-insert-section (drover-list-empty)
    (let ((width (plist-get layout :width)))
      (cond
       ((null (drover-backends))
        (insert (drover-list--flow '("No agent backend is registered.") width)
                (drover-list--flow '("Load an agent front end, such as opencode.el, to see its sessions.")
                                   width)))
       ((and sessions (drover-filter-active-p))
        (insert (drover-list--flow '("No session matches the filter.") width)))
       (t (insert "No sessions.\n"))))))

(defun drover-list--line-numbers-width ()
  "Return the width in pixels of the line numbers of the list's window, or 0.
Positions aligned to in the text count from past the line numbers,
those of the header line from the text area's edge."
  (let ((window (drover-list--window)))
    (if (buffer-local-value 'display-line-numbers (window-buffer window))
        (with-selected-window window (line-number-display-width t))
      0)))

(defun drover-list--header-line (layout)
  "Return the header line naming the columns of LAYOUT, or nil in two lines.
Each name, the :label of its field in `drover-list-fields', begins with
a space aligned to its column as the cells of lines do, past the line
numbers, which the header line lacks; the name of a column aligned
right is aligned right.  A name wider than its column is cut; a column
of an icon alone (see `drover-list--icon-alone-p') has none.  The
tooltip of a name tells what its column shows, from
`drover-list-field-descriptions'."
  (when (eq (plist-get layout :layout) 'columns)
    (let ((shift (drover-list--line-numbers-width))
          (segments nil))
      (pcase-dolist (`(,field ,x ,width ,align) (plist-get layout :columns))
        (when-let* (((not (drover-list--icon-alone-p field layout)))
                    (label (plist-get (drover-list--field-spec field) :label))
                    ;; Measured over the face of the header line, which draws it.
                    (cut (drover-list--truncate (propertize label 'face '(drover-list-header header-line))
                                                width))
                    ((not (string-empty-p cut))))
          (let* ((label-width (drover-list--pixel-width cut))
                 (start (+ shift (if (eq align 'right) (- (+ x width) label-width) x)))
                 (text (propertize (string-replace "%" "%%" (substring-no-properties cut))
                                   'face 'drover-list-header
                                   'help-echo (alist-get field drover-list-field-descriptions))))
            (setq segments (append segments (list (list 'align start)
                                                  (list 'text text label-width start)))))))
      (and segments (car (drover-list--assemble segments))))))

(defvar-local drover-list--prefetched nil
  "Keys of the sessions the list asked their backends to complete, or nil.
A hash table, made anew by each drawing that may show what backends
fetch on demand, nil when the drawing shows none of it.  The groups
whose sessions were all asked for are in it too, as (group . VALUE).
See `drover-list--prefetch'.")

(defvar-local drover-list--prefetch-kinds nil
  "What the list asks for the sessions in sight, as `drover-prefetch' takes it.
See `drover-list--prefetch-wanted-p'.")

(defun drover-list--prefetch ()
  "Ask backends to fetch what the sessions in sight lack, once per drawing.
Those are the sessions of the expanded groups, not yet asked for since
the list was drawn: a group expanded after the drawing has its sessions
asked for then, as `post-command-hook' runs this.  A group asked for is
not looked at again, so that a command costs nothing in a long list.
What is asked is `drover-list--prefetch-kinds'.  See `drover-prefetch'."
  (when (drover-list--current-p)
    (drover-with-reporting "list prefetch"
      (when (and drover-list--prefetched drover-list--model)
        (let ((sessions nil))
          (dolist (group (drover-list--model-groups drover-list--model))
            (let ((section (drover-list--group-container group))
                  (id (cons 'group (drover-list--group-value group))))
              (unless (or (gethash id drover-list--prefetched)
                          (and (drover-list--group-value group) (oref section hidden)))
                (puthash id t drover-list--prefetched)
                (seq-doseq (tree (drover-list--group-trees group))
                  (dolist (row (drover-list--tree-shown tree))
                    (let ((key (drover-list--row-key row)))
                      (unless (gethash key drover-list--prefetched)
                        (puthash key t drover-list--prefetched)
                        (push (drover-list--row-session row) sessions))))))))
          (when sessions
            (drover-prefetch (nreverse sessions) drover-list--prefetch-kinds)))))))

(defun drover-list--prefetch-wanted-p ()
  "Return what the list asks for the sessions in sight, or nil for nothing.
A list of the kinds `drover-prefetch' takes: `details', for the
context, which costs a request per session, unless the user hides it:
in columns, and in two lines when `drover-list-fields' places it there,
as its pie; `changes', for what sessions changed, unless the user hides
its column, as an expanded session lists it anyway.  Whether a row has
a context or changes to show yet does not count: the rows have none
until they are fetched."
  (let ((context (assq 'context drover-list-fields)))
    (delq nil (list (and context
                         (not (memq 'context drover-list-hidden-fields))
                         (or (eq drover-list--layout 'columns)
                             (plist-get (cdr context) :narrow))
                         'details)
                    (and (assq 'changes drover-list-fields)
                         (not (memq 'changes drover-list-hidden-fields))
                         'changes)))))

(defun drover-list--prefetch-changes (session)
  "Ask what SESSION changed, which its details list, unless asked already.
Asked with the sessions in sight while the list shows its column (see
`drover-list--prefetch-wanted-p'), else as it expands."
  (unless (and drover-list--prefetched
               (memq 'changes drover-list--prefetch-kinds)
               (gethash (drover-session-key session) drover-list--prefetched))
    (drover-prefetch (list session) '(changes))))

(defun drover-list--settings ()
  "Return the settings a drawing depends on, besides sessions and width."
  (list drover-group-by drover-sort-by drover-sort-descending drover-sort-activity-tolerance
        (copy-tree drover-filter)
        drover-show-subagents drover-show-history drover-show-archived
        drover-list-hidden-fields (copy-tree drover-list-fields)
        drover-status-order drover-attention-statuses
        (drover-list--caching)))

(defun drover-list--caching ()
  "Return the backends that may report prompt caches now.
See `drover-backend-caches-p'."
  (seq-filter #'drover-backend-caches-p (drover-backends)))

(defun drover-list--before-p (a b)
  "Return non-nil when tree A comes before tree B in the order of urgency.
That is the order views sort trees in, sort after sort, each keeping
the order of the one before where trees rank alike: by what each tree
shows, by what the filter leaves of it, by the whole tree, and by the
order its top session was listed in.  It is total: no two trees rank
alike, so that where a changed tree goes among the others is known
from its neighbours alone."
  ;; Allocating nothing: a change in a group of thousands of trees
  ;; compares the changed one with each of them.
  (let ((order (or (drover-list--node-order (drover-list--tree-node a) (drover-list--tree-node b))
                   (drover-list--node-order (drover-list--tree-cut a) (drover-list--tree-cut b))
                   (drover-list--node-order (drover-list--tree-full a) (drover-list--tree-full b)))))
    (if order
        (eq order 'before)
      (< (drover-list--tree-serial a) (drover-list--tree-serial b)))))

(defun drover-list--node-order (x y)
  "Return `before' or `after' as tree X ranks before or after tree Y.
Nil when they rank alike, or either is nil."
  (and x y
       (cond ((drover--node-before-p x y) 'before)
             ((drover--node-before-p y x) 'after))))

(defun drover-list--drawn-before-p (a b)
  "Return non-nil when tree A is drawn before tree B in their group.
They are sorted as `drover-sort-trees' sorts them, by
`drover-sort-by', trees alike keeping the order of urgency, see
`drover-list--before-p'."
  (let ((field drover-sort-by)
        (descending drover-sort-descending))
    (if (eq field 'urgency)
        (if descending (drover-list--before-p b a) (drover-list--before-p a b))
      (let ((key-a (drover--sort-key field (drover-list--tree-node a) 0))
            (key-b (drover--sort-key field (drover-list--tree-node b) 0)))
        (cond ((drover--sort-key-less-p key-a key-b descending) t)
              ((drover--sort-key-less-p key-b key-a descending) nil)
              (t (drover-list--before-p a b)))))))

(defun drover-list--head (group)
  "Return the most urgent tree of GROUP, a `drover-list--group'."
  (let ((head nil))
    (seq-doseq (tree (drover-list--group-trees group))
      (when (or (null head) (drover-list--before-p tree head))
        (setq head tree)))
    head))

(defvar drover-list--sharing nil
  "What every list following one change shares, or nil.
While `drover-list--on-change' brings the lists up to date, a plist of
:now, the time they draw, :trees, a hash table of the trees
`drover-list--plan-trees' made, by the key of their top session, and
what `drover-list--shared' made: the sidebar and the list in an
ordinary window list the same sessions and place the same trees, each
with rows of its own layout.")

(defun drover-list--shared (key make)
  "Return what MAKE, a function of no argument, returns, made once per change.
While every list follows one change (see `drover-list--sharing'), the
value is kept under KEY for the lists after the first."
  (if-let* ((cell (plist-member drover-list--sharing key)))
      (cadr cell)
    (let ((value (funcall make)))
      (when drover-list--sharing
        (plist-put drover-list--sharing key value))
      value)))

(defun drover-list--shared-now ()
  "Return the time lists draw at, the same for every list following a change."
  (or (plist-get drover-list--sharing :now) (drover-list--now)))

(defun drover-list--shared-places ()
  "Return `drover-list--places', made once for every list following a change."
  (drover-list--shared :places #'drover-list--places))

(defun drover-list--arrange (sessions now)
  "Return the trees of SESSIONS as a view draws them at NOW.
The value is (SESSIONS TREES ACTIVE VIEW ARRANGED): TREES as
`drover-tree' makes them; ACTIVE their active subagents (see
`drover-tree-active-subagents'), from every session, so that a parent
tells its subagents at work whether or not they are shown; VIEW what
`drover--view-trees' leaves of them, and ARRANGED the groups
`drover-view-arrange' puts those in."
  (let* ((trees (drover-tree sessions))
         (view (drover--view-trees nil trees)))
    (list sessions trees (drover-tree-active-subagents trees) view
          (drover-view-arrange (car view) now))))

(defun drover-list--build (sessions now &optional indexed)
  "Return the model of the list of SESSIONS at NOW, not drawn yet.
INDEXED non-nil tells that SESSIONS are those of the index (see
`drover-index-sessions'), whose order it keeps.  Rows of the last
drawing made of the same inputs are reused (see `drover-list--row').
The trees are made once for every list drawing SESSIONS while they
follow a change (see `drover-list--shared')."
  (let* ((arranged (drover-list--shared :arranged (lambda () (drover-list--arrange sessions now))))
         (arranged (if (eq (car arranged) sessions) arranged (drover-list--arrange sessions now)))
         (trees (nth 1 arranged))
         ;; Changes drawn in place change it: each drawing its own.
         (active (copy-hash-table (nth 2 arranged)))
         (view (nth 3 arranged))
         (filtered (drover-filter-active-p))
         (metrics (drover-list--metrics now active (drover-list--shared-places)))
         (old drover-list--model)
         (model (drover-list--model-create
                 :settings (drover-list--settings)
                 :trees (make-hash-table :test #'equal :size (length (cdr view)))
                 :rows (make-hash-table :test #'equal :size (length sessions))))
         (serials (make-hash-table :test #'equal :size (length sessions)))
         (fulls (make-hash-table :test #'equal))
         (cuts (make-hash-table :test #'equal))
         (summary (drover-list--summary-create))
         (drover-list--reuse (and old (drover-list--model-rows old)))
         (drover-list--peer-rows (drover-list--peers-of (plist-get metrics :stamp))))
    ;; Sessions of the index keep its serial numbers, which trees drawn
    ;; in place later compare with.
    (let ((index 0)
          (indexed (and indexed (drover--index-serials (drover-index)))))
      (dolist (session sessions)
        (let ((key (drover-session-key session)))
          (puthash key (if indexed (gethash key indexed) (cl-incf index)) serials))))
    (dolist (node trees)
      (puthash (drover-session-key (drover-node-session node)) node fulls))
    (dolist (node (cdr view))
      (puthash (drover-session-key (drover-node-session node)) node cuts))
    (setf (drover-list--model-groups model)
          (mapcar
           (pcase-lambda (`(,value . ,nodes))
             (let ((group (drover-list--group-create :value value
                                                     :counts (make-hash-table :test #'eq)))
                   (position -1))
               (setf (drover-list--group-trees group)
                     (vconcat
                      (mapcar
                       (lambda (node)
                         (let* ((root (drover-session-key (drover-node-session node)))
                                (tree (drover-list--tree-create
                                       :root root :node node :full (gethash root fulls)
                                       :cut (and filtered (gethash root cuts))
                                       :serial (gethash root serials)
                                       :group-link (drover-link group) :position (cl-incf position)
                                       :shown (drover-list--tree-rows (list node) value metrics))))
                           (puthash root tree (drover-list--model-trees model))
                           (dolist (row (drover-list--tree-shown tree))
                             ;; A row reused belongs to this drawing, drawn or not.
                             (setf (drover-list--row-tree row) tree
                                   (drover-list--row-section row) nil)
                             (puthash (drover-list--row-key row) row (drover-list--model-rows model))
                             (drover-list--summary-add summary row 1)
                             (drover-list--count-row (drover-list--group-counts group) row 1)
                             (cl-incf (drover-list--group-size group))
                             (cl-incf (drover-list--model-size model)))
                           tree))
                       nodes)))
               (setf (drover-list--group-head group) (drover-list--head group))
               group))
           (nth 4 arranged)))
    (let* ((layout (drover-list--layout-of summary metrics))
           (key (drover-list--layout-key layout))
           (old-key (and old (plist-get (drover-list--model-layout old) :key))))
      ;; The key of the last layout, when the same, keeps the headings
      ;; of the rows reused.
      (setf (drover-list--model-summary model) summary
            (drover-list--model-layout model)
            (append (list :key (if (equal key old-key) old-key key)) layout)))
    model))

(defun drover-list--insert-model (model sessions)
  "Insert the sections of MODEL at point, SESSIONS being every session known."
  (let ((layout (drover-list--model-layout model)))
    (magit-insert-section (drover-list)
      ;; Every tree the filter keeps has its root shown, so no row means
      ;; no session at all, or none the filter keeps.
      (if (zerop (drover-list--model-size model))
          (drover-list--insert-empty sessions layout)
        (dolist (group (drover-list--model-groups model))
          (drover-list--insert-group group layout))))))

(defun drover-list--insert ()
  "Insert every section of the list at point, from what backends report now.
The model of the drawing goes to `drover-list--model'."
  (let* ((now (drover-list--shared-now))
         (indexed (not (or drover-show-history drover-show-archived)))
         (all (drover-list--shared :sessions
                                   (lambda () (if indexed (drover-index-sessions) (drover-known-sessions)))))
         (model (drover-list--build all now indexed))
         (layout (drover-list--model-layout model)))
    (setq drover-list--model model
          drover-list--layout (plist-get layout :layout)
          drover-list--fields (plist-get layout :fields)
          drover-list--frames (plist-get layout :frames)
          header-line-format (and (> (drover-list--model-size model) 0)
                                  (drover-list--header-line layout)))
    (drover-list--forget-visibility all indexed)
    (drover-list--forget-recaps all)
    (setq drover-list--undrawn nil)
    ;; Sections pushed, not appended to thousands of siblings each.
    (let ((magit-section-inhibit-markers t)
          (magit-section-insert-in-reverse t)
          (drover-list--drawing t)
          ;; A few screens at once, the other rows later: thousands of
          ;; rows cost hundreds of milliseconds, garbage collected mostly.
          (drover-list--rows-left (drover-list--first-rows)))
      (drover-list--insert-model model all))
    (drover-list--use-markers)))

(defun drover-list--use-markers (&optional section)
  "Turn the positions of every section into markers, or those under SECTION.
Sections are inserted with positions, which inserting text does not
move, since a marker moves on every insertion before it: thousands of
sections inserted with markers cost each insertion a walk over all of
them.  Once drawn, lines change in place, which markers follow.  Starts
advance on insertion and the other positions do not, as Magit makes
them."
  (when magit-root-section
    (magit-map-sections
     (lambda (section)
       (unless (markerp (oref section start))
         (oset section start (copy-marker (oref section start) t)))
       (when-let* ((content (oref section content))
                   ((not (markerp content))))
         (oset section content (copy-marker content)))
       (unless (markerp (oref section end))
         (oset section end (copy-marker (oref section end)))))
     section)))

(defvar magit--ellipses-sections)
(defvar magit-section--opened-sections)

(defun drover-list--release-markers ()
  "Detach the markers of the sections of the current list, before it is erased.
Markers detached cost later insertions nothing, while those of sections
no longer drawn would until collected.  Magit forgets those sections,
whose positions are gone: the ellipses it painted and the sections
Isearch opened."
  (dolist (variable '(magit--ellipses-sections magit-section--opened-sections))
    (when (boundp variable)
      (set variable nil)))
  (setq magit--context-menu-section nil)
  (when magit-root-section
    (let ((markers nil))
      (magit-map-sections
       (lambda (section)
         (dolist (position (list (oref section start) (oref section content) (oref section end)))
           (when (markerp position)
             (push position markers)))))
      ;; Detaching a marker finds it from the newest of the buffer: the
      ;; newest first, each is found at once.
      (dolist (marker markers)
        (set-marker marker nil)))))

;;;; Keeping point

(defun drover-list--anchor (position)
  "Return what is at POSITION, to find it again after a redraw.
The value is a plist of :key, the key of the session there, :offset,
the line within its section, :container, the identifier of the group
there or of the root section holding the session (see
`drover-list--container-id'), :index, the position of the session
among the sessions of its container, :line, the line number, and
:char, the position within the line.  Positions in a line count
characters, not columns: with variable pitch and aligned cells, columns
say little."
  (save-excursion
    (goto-char position)
    (let* ((section (magit-section-at))
           (session (drover-list--session-section section))
           (container (if session
                          (oref session parent)
                        (drover-list--group-section section))))
      (list :key (and session (oref session value))
            :offset (and session (- (line-number-at-pos) (line-number-at-pos (oref session start))))
            :container (and container (drover-list--container-id container))
            :index (and session
                        (seq-position (drover-list--session-sections container) session))
            :line (line-number-at-pos)
            :char (- (point) (line-beginning-position))))))

(defun drover-list--move-in-line (char)
  "Move point CHAR characters into its line, or to its end."
  (goto-char (min (+ (line-beginning-position) char) (line-end-position))))

(defun drover-list--line-shown-p (position)
  "Return non-nil when the line of POSITION shows, in a section not collapsed.
Its first character tells nothing: every line of a session begins with
an invisible one in a list that folds (see `drover-list--line-start').
Its newline belongs to the body of the section collapsed around it, if
any."
  (not (invisible-p (save-excursion (goto-char position) (line-end-position)))))

(defun drover-list--visible-start (section)
  "Return the start of SECTION, or of its outermost collapsed ancestor."
  (let ((visible section))
    (while-let ((parent (oref section parent)))
      (when (oref parent hidden)
        (setq visible parent))
      (setq section parent))
    (oref visible start)))

(defun drover-list--resolve (anchor)
  "Return the position of ANCHOR, a value of `drover-list--anchor'.
Prefer the same session, wherever it moved, then the session that took
its place in its group, or in the list when sessions are not grouped,
then the same line."
  (save-excursion
    (let* ((key (plist-get anchor :key))
           (session (and key (drover-list--find-session key)))
           (container-id (plist-get anchor :container))
           (container (and (not session) container-id
                           (drover-list--find-container container-id))))
      (cond
       (session
        (let ((start (drover-list--visible-start session)))
          (goto-char start)
          ;; In a collapsed group, the group heading stands for the session.
          (when (= start (oref session start))
            (forward-line (plist-get anchor :offset))
            (unless (and (< (point) (oref session end))
                         (drover-list--line-shown-p (point))
                         (eq (drover-list--session-section (magit-section-at)) session))
              (goto-char start))
            (drover-list--move-in-line (plist-get anchor :char)))))
       (container
        (let ((sessions (drover-list--session-sections container))
              (index (plist-get anchor :index)))
          (goto-char (drover-list--visible-start
                      (if (and index sessions)
                          (nth (min index (1- (length sessions))) sessions)
                        container)))))
       (t
        (goto-char (point-min))
        (forward-line (1- (plist-get anchor :line)))
        (when (and (eobp) (not (bobp)))
          (forward-line -1))
        (if-let* ((section (magit-section-at))
                  (_ (not (drover-list--line-shown-p (point)))))
            (goto-char (drover-list--visible-start section))
          (drover-list--move-in-line (plist-get anchor :char)))))
      (point))))

;;;; Refreshing

(defun drover-list--render ()
  "Redraw the current list from what backends report now.
Point, in the buffer and in every window showing it, stays on the
same session, and window starts on the same line.  An active region
stays active, its mark on the same session too, so that it selects the
same sessions: a list redraws whenever a session changes, many times a
second during a turn.  The sidebar's side windows get its settings
first, which the width depends on, and windows scrolled sideways
scroll back (see `drover-list--unscroll')."
  (drover-list--set-up-side-windows)
  (drover-list--unscroll)
  (let* ((windows (get-buffer-window-list nil nil t))
         (anchor (drover-list--anchor (point)))
         (mark (and (region-active-p) (drover-list--anchor (mark))))
         (placements (mapcar (lambda (window)
                               (list window
                                     (drover-list--anchor (window-point window))
                                     (line-number-at-pos (window-start window))))
                             windows))
         (inhibit-read-only t)
         ;; Redrawing, often from a timer, must not end the user's region.
         (deactivate-mark nil))
    (drover-list--drop-stale)
    (drover-list--remember-visibility)
    (drover-list--drop-caches)
    (pcase-dolist (`(,marker . ,_) drover-list--animated)
      (set-marker marker nil))
    (drover-list--release-markers)
    (setq magit-section-pre-command-section nil
          magit-section-highlight-overlays nil
          magit-section-selection-overlays nil
          magit-section-highlighted-sections nil
          magit-section-focused-sections nil
          drover-list--caches nil
          drover-list--drawn (make-hash-table :test #'equal)
          drover-list--title-states (make-hash-table :test #'equal)
          drover-list--width (drover-list--line-width)
          drover-list--stale nil
          drover-list--pending nil)
    (drover-list--measure-begin)
    (erase-buffer)
    (save-excursion
      (drover-list--insert))
    ;; Sections exist now; showing the root applies their visibility.
    (magit-section-show magit-root-section)
    (goto-char (drover-list--resolve anchor))
    (pcase-dolist (`(,window ,window-anchor ,start-line) placements)
      (set-window-point window (drover-list--resolve window-anchor))
      (set-window-start window
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point))
                        t))
    (when (eq (window-buffer) (current-buffer))
      (goto-char (window-point)))
    (when mark
      (set-marker (mark-marker) (drover-list--resolve mark)))
    (drover-list--draw-caches)
    (setq drover-list--animated (drover-list--collect-animated)
          drover-list--prefetch-kinds (drover-list--prefetch-wanted-p)
          drover-list--prefetched (and drover-list--prefetch-kinds
                                       (make-hash-table :test #'equal)))
    (drover-list--prefetch)
    (magit-section-update-highlight t)
    (set-buffer-modified-p nil)
    (push (current-buffer) magit-section--refreshed-buffers)
    (drover-list--defer)
    (drover-list--update-timers)))

;;;; Drawing in place

;; A change of a few sessions, the common case while a turn runs,
;; redraws their lines in place, as long as it moves no line: each
;; session changed is placed again within its tree, which is made anew
;; from the index (`drover-index-tree-sessions'); its tree must keep its
;; shape and group, and come between the same neighbours, in its group
;; and among groups, and the layout must stay the same.  Otherwise the
;; list is drawn anew.  The lines then drawn are those a drawing anew
;; would draw, so that the two are the same, which the tests check.

(defun drover-list--replace-text (section start end text)
  "Replace the text of SECTION from START to END with TEXT, starting at START.
TEXT goes in before the old text goes, so that what begins where it
ends, as the next section, follows it.  The start of SECTION, and of
those of its ancestors beginning there too, stay at START."
  (let* ((start (if (markerp start) (marker-position start) start))
         (starting (seq-filter (lambda (ancestor) (= (oref ancestor start) start))
                               (magit-section-lineage section t)))
         (length (- end start)))
    (goto-char start)
    (insert text)
    (delete-region (point) (+ (point) length))
    (dolist (ancestor starting)
      (set-marker (oref ancestor start) start))))

(defun drover-list--forget-marks (start end)
  "Forget the prompt caches and animated glyphs drawn between START and END."
  (setq drover-list--caches
        (seq-remove (lambda (entry)
                      (let ((position (plist-get entry :start)))
                        (when (and (<= start position) (< position end))
                          (when-let* ((overlay (plist-get entry :overlay))) (delete-overlay overlay))
                          (set-marker position nil)
                          t)))
                    drover-list--caches)
        drover-list--animated
        (seq-remove (lambda (entry)
                      (when (and (<= start (car entry)) (< (car entry) end))
                        (set-marker (car entry) nil)
                        t))
                    drover-list--animated)))

(defun drover-list--redraw-row (row heading body)
  "Draw ROW in place of the section it shares with the row it replaces.
HEADING is its heading (see `drover-list--heading'), or nil to keep
the text drawn;
with BODY non-nil, its details too: drawn again if shown, else made to
be drawn once expanded, as a drawing anew leaves a collapsed section."
  (let* ((section (drover-list--row-section row))
         (key (drover-list--row-key row))
         (start (marker-position (oref section start)))
         (layout (drover-list--model-layout drover-list--model)))
    (drover-list--forget-marks start (oref section end))
    (when heading
      ;; Magit ends a heading with a newline.
      (drover-list--replace-text section start (oref section content) (concat (car heading) "\n")))
    (when (and body (plist-get layout :folds))
      (let ((content (oref section content)))
        (unless (and (oref section hidden) (oref section washer))
          (delete-region content (oref section end))
          (goto-char content)
          (let ((magit-insert-section--current section)
                (magit-insert-section--parent section))
            (drover-list--insert-session-body key))
          (set-marker (oref section end) (point))
          ;; Without details, its heading has no keymap of a heading,
          ;; as when drawn anew.
          (unless (magit-section-content-p section)
            (remove-text-properties start (magit--eol-position start) '(keymap nil))))))
    (magit-section--set-section-properties section)
    (magit-section-maybe-add-heading-map section)
    (magit-section-maybe-update-visibility-indicator section)
    (drover-list--note-row row (drover-list--row-heading row) start layout)
    (setq drover-list--animated
          (append drover-list--animated
                  (drover-list--collect-animated start (oref section content))))))

(defun drover-list--redraw-group (group text)
  "Draw TEXT as the heading of GROUP, a `drover-list--group', in place."
  (let* ((section (drover-list--group-container group))
         (start (marker-position (oref section start)))
         (keymap (oref section keymap)))
    (drover-list--replace-text section start (oref section content) (concat text "\n"))
    (add-text-properties start (oref section content)
                         `(magit-section ,section
                                         ,@(and (symbol-value keymap) (list 'keymap (symbol-value keymap)))))
    (magit-section-maybe-add-heading-map section)
    (magit-section-maybe-update-visibility-indicator section)
    (setf (drover-list--group-heading-text group) text)))

(defun drover-list--follow (keys &optional bodies)
  "Draw in place what the change of the sessions KEYS changes, if it can.
The sessions are those of `drover-changed-sessions', as the index has
them now.  BODIES lists sessions whose details are drawn again even if
they did not change, such as one whose recap arrived.  Return nil,
drawing nothing, when that change cannot be drawn in place: the list
must then be drawn anew, as when the sidebar's side window had other
settings, which change the width."
  (drover-list--set-up-side-windows)
  (let ((model drover-list--model))
    (and model
         (not drover-list--stale)
         (> (drover-list--model-size model) 0)
         (not drover-show-history)
         (not drover-show-archived)
         (eql drover-list--width (drover-list--line-width))
         (equal (drover-list--model-settings model) (drover-list--settings))
         (drover-index-incremental-p)
         (catch 'redraw
           (drover-list--follow-1 model keys bodies)
           t))))

(defun drover-list--plan-trees (root now)
  "Return the tree of session ROOT as views arrange it at NOW, from the index.
The value is that of `drover-list--arrange' for the sessions of the
tree.  Made once per change for every list, see `drover-list--sharing'."
  (let ((shared (plist-get drover-list--sharing :trees)))
    (or (and shared (gethash root shared))
        (let ((value (drover-list--arrange (drover-index-tree-sessions root) now)))
          (when shared
            (puthash root value shared))
          value))))

(defun drover-list--plan (root now places layout rows-table)
  "Return the tree of session ROOT as the list would draw it now, or nil.
The value is a `drover-list--tree' of its rows, not drawn, its group
slot holding the value of the group it goes in; nil when ROOT tops no
tree any more, or the filter leaves none of it.  Rows of ROWS-TABLE
made of the same inputs are reused; NOW, PLACES and LAYOUT give the
measures.  The active subagents LAYOUT holds follow the tree.  Throw
`redraw' for a tree in a group the list cannot place."
  (when (and (drover-index-session root) (null (drover-index-parent root)))
    (pcase-let ((`(,_ ,trees ,active ,view ,arranged) (drover-list--plan-trees root now)))
      (unless (and trees (null (cdr trees))
                   (equal (drover-session-key (drover-node-session (car trees))) root))
        (throw 'redraw nil))
      (let ((global (plist-get layout :active)))
        (when global
          (dolist (session (drover-tree-sessions trees))
            (let ((key (drover-session-key session)))
              (if-let* ((subagents (gethash key active)))
                  (puthash key subagents global)
                (remhash key global)))))
        (when arranged
          (when (or (cdr arranged) (cdr (cdar arranged))
                    (null (drover-list--group-rank (caar arranged))))
            (throw 'redraw nil))
          (let* ((value (caar arranged))
                 (node (cadar arranged))
                 (metrics (append (list :now now :active active :places places) layout)))
            (drover-list--tree-create
             :root root :node node :full (car trees)
             :cut (and (drover-filter-active-p) (car (cdr view)))
             :serial (drover-index-serial root) :group-link (drover-link value)
             :shown (let ((drover-list--reuse rows-table))
                      (drover-list--tree-rows (list node) value metrics)))))))))

(defun drover-list--group-rank (value)
  "Return where the group of VALUE goes among groups, or nil when unknown.
For groups of a status, a time or a host, a number; for groups placed by
their most urgent tree, and the one group of sessions not grouped, t."
  (pcase (car value)
    ('status (seq-position (append drover-status-order '(archived)) (cdr value)))
    ('updated (seq-position drover-activity-buckets (cdr value)))
    ('host (drover-host-rank (cdr value)))
    (_ t)))

(defun drover-list--group-before-p (a b)
  "Return non-nil when group A, a `drover-list--group', comes before group B."
  (let ((rank-a (drover-list--group-rank (drover-list--group-value a)))
        (rank-b (drover-list--group-rank (drover-list--group-value b))))
    (if (numberp rank-a)
        (< rank-a rank-b)
      (drover-list--before-p (drover-list--group-head a) (drover-list--group-head b)))))

(defun drover-list--place (vector tree)
  "Return VECTOR, of trees in the order drawn, with TREE in its place."
  (let ((low 0)
        (high (length vector)))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (drover-list--drawn-before-p (aref vector middle) tree)
            (setq low (1+ middle))
          (setq high middle))))
    (vconcat (substring vector 0 low) (vector tree) (substring vector low))))

(defun drover-list--follow-1 (model keys bodies)
  "Draw in MODEL the change of the sessions KEYS, or throw `redraw'.
BODIES are as for `drover-list--follow'.  Each tree a session changed
in is made anew; one that keeps its rows and its place has the lines
that changed drawn again in place, the others leave their place and
are drawn in the new one.  Groups come and go likewise."
  (let* ((now (drover-list--shared-now))
         (places (drover-list--shared-places))
         (rows-table (drover-list--model-rows model))
         (trees-table (drover-list--model-trees model))
         (layout (drover-list--model-layout model))
         (drover-list--peer-rows (drover-list--peers-of (plist-get layout :stamp)))
         (roots nil))
    (dolist (key (append keys bodies))
      (when (drover-index-session key)
        (cl-pushnew (drover-index-root key) roots :test #'equal))
      (when-let* ((row (gethash key rows-table)))
        (cl-pushnew (drover-list--tree-root (drover-list--row-tree row)) roots :test #'equal)))
    (let ((plans nil)
          (touched nil))
      ;; Each plan is (OLD NEW OLD-ROWS IN-PLACE): a tree keeping its
      ;; group and rows stays the same object, so that its neighbours
      ;; tell whether it moved.
      (dolist (root roots)
        (let* ((old (gethash root trees-table))
               (new (drover-list--plan root now places layout rows-table))
               (old-rows (and old (drover-list--tree-shown old)))
               (same (and old new
                          (equal (drover-list--tree-group new)
                                 (drover-list--group-value (drover-list--tree-group old)))
                          (= (length old-rows) (length (drover-list--tree-shown new)))
                          (cl-every (lambda (a b)
                                      (and (equal (drover-list--row-key a) (drover-list--row-key b))
                                           (eql (drover-list--row-depth a) (drover-list--row-depth b))))
                                    old-rows (drover-list--tree-shown new)))))
          (when old
            (cl-pushnew (drover-list--tree-group old) touched))
          (when same
            (setf (drover-list--tree-node old) (drover-list--tree-node new)
                  (drover-list--tree-full old) (drover-list--tree-full new)
                  (drover-list--tree-cut old) (drover-list--tree-cut new)
                  (drover-list--tree-shown old) (drover-list--tree-shown new)
                  (drover-list--tree-group new) (drover-list--tree-group old)
                  new old))
          (push (list old new old-rows same) plans)))
      (setq plans (nreverse plans))
      (pcase-let ((`(,touched . ,vectors) (drover-list--follow-model model plans touched now)))
        (drover-list--follow-buffer model plans touched vectors bodies)))))

(defun drover-list--follow-model (model plans touched now)
  "Take the trees of PLANS into MODEL at NOW: groups, order, rows, counts, layout.
TOUCHED lists the groups the old trees of PLANS were in; the groups the
new ones go in, made if need be, join it.  Return (TOUCHED . VECTORS):
VECTORS is an alist of the trees of each group before.  Throw `redraw'
when the groups would come in another order, or the layout change."
  (let ((rows-table (drover-list--model-rows model))
        (trees-table (drover-list--model-trees model))
        (summary (drover-list--model-summary model))
        (layout (drover-list--model-layout model))
        (removed (mapcar #'car plans))
        (vectors (mapcar (lambda (group) (cons group (drover-list--group-trees group)))
                         (drover-list--model-groups model))))
    (dolist (group touched)
      (setf (drover-list--group-trees group)
            (cl-remove-if (lambda (tree) (memq tree removed)) (drover-list--group-trees group))))
    (pcase-dolist (`(,old ,_ ,old-rows ,_) plans)
      (when old
        (remhash (drover-list--tree-root old) trees-table)
        (let ((group (drover-list--tree-group old)))
          (dolist (row old-rows)
            (remhash (drover-list--row-key row) rows-table)
            (drover-list--summary-add summary row -1)
            (drover-list--count-row (drover-list--group-counts group) row -1)
            (cl-decf (drover-list--group-size group))
            (cl-decf (drover-list--model-size model))))))
    (pcase-dolist (`(,_ ,new ,_ ,same) plans)
      (when new
        (let ((group (if same
                         (drover-list--tree-group new)
                       (let ((value (drover-list--tree-group new)))
                         (or (seq-find (lambda (group) (equal (drover-list--group-value group) value))
                                       (drover-list--model-groups model))
                             (drover-list--group-create :value value :trees []
                                                        :counts (make-hash-table :test #'eq)))))))
          (cl-pushnew group touched)
          (setf (drover-list--tree-group new) group
                (drover-list--group-trees group) (drover-list--place (drover-list--group-trees group) new))
          (puthash (drover-list--tree-root new) new trees-table)
          (dolist (row (drover-list--tree-shown new))
            (setf (drover-list--row-tree row) new)
            (puthash (drover-list--row-key row) row rows-table)
            (drover-list--summary-add summary row 1)
            (drover-list--count-row (drover-list--group-counts group) row 1)
            (cl-incf (drover-list--group-size group))
            (cl-incf (drover-list--model-size model))))))
    (when (zerop (drover-list--model-size model))
      (throw 'redraw nil))
    (dolist (group touched)
      (let ((position -1))
        (seq-doseq (tree (drover-list--group-trees group))
          (setf (drover-list--tree-position tree) (cl-incf position))))
      ;; The head stays unless it changed or left: then the trees that
      ;; came may come before it.  Looking at every tree of a group of
      ;; thousands, at every change, cost most of a change.
      (let ((head (drover-list--group-head group)))
        (setf (drover-list--group-head group)
              (if (and head (not (memq head removed)))
                  (let ((best head))
                    (pcase-dolist (`(,_ ,new) plans)
                      (when (and new (eq (drover-list--tree-group new) group)
                                 (drover-list--before-p new best))
                        (setq best new)))
                    best)
                (drover-list--head group)))))
    ;; Groups come and go, in their place; those staying keep theirs.
    (let ((groups (seq-remove (lambda (group) (zerop (drover-list--group-size group)))
                              (drover-list--model-groups model))))
      (dolist (group touched)
        (unless (or (zerop (drover-list--group-size group)) (memq group groups))
          (let ((before nil)
                (after groups))
            (while (and after (drover-list--group-before-p (car after) group))
              (push (pop after) before))
            (setq groups (append (nreverse before) (list group) after)))))
      (cl-loop for (a b) on groups while b
               unless (drover-list--group-before-p a b)
               do (throw 'redraw nil))
      (setf (drover-list--model-groups model) groups))
    (unless (equal (drover-list--layout-key (drover-list--layout-of summary layout))
                   (plist-get layout :key))
      (throw 'redraw nil))
    (plist-put layout :now now)
    (cons touched vectors)))

(defun drover-list--stable-p (old-vector new-vector plans)
  "Return non-nil when the trees of a group staying in place keep their order.
OLD-VECTOR and NEW-VECTOR are the trees of the group before and after
the change of PLANS.  Those staying are the trees no plan changed, and
those changed in place, which are the same objects before and after."
  (let ((moving (mapcan (pcase-lambda (`(,old ,new ,_ ,same))
                          (unless same (delq nil (list old new))))
                        plans)))
    (cl-flet ((staying (vector)
                (cl-remove-if (lambda (tree) (memq tree moving)) (append vector nil))))
      (let ((before (staying old-vector))
            (after (staying new-vector)))
        (and (= (length before) (length after))
             (cl-every #'eq before after))))))

(defun drover-list--live-p (section)
  "Return non-nil when SECTION is still drawn."
  (let ((start (oref section start)))
    (or (not (markerp start)) (marker-buffer start))))

(defun drover-list--forget-gone-sections ()
  "Make Magit forget the sections no longer drawn, which it may remember."
  (setq magit-section-pre-command-section
        (and magit-section-pre-command-section
             (drover-list--live-p magit-section-pre-command-section)
             magit-section-pre-command-section)
        magit-section-highlighted-sections
        (seq-filter #'drover-list--live-p magit-section-highlighted-sections)
        magit-section-focused-sections
        (seq-filter #'drover-list--live-p magit-section-focused-sections))
  (dolist (variable '(magit--ellipses-sections magit-section--opened-sections))
    (when (boundp variable)
      (set variable (seq-filter #'drover-list--live-p (symbol-value variable))))))

(defun drover-list--delete-rows (rows)
  "Delete the sections of ROWS, consecutive, from the buffer."
  (let* ((first (drover-list--row-section (car rows)))
         (last (drover-list--row-section (car (last rows))))
         (start (marker-position (oref first start)))
         (end (marker-position (oref last end))))
    (drover-list--forget-marks start end)
    (delete-region start end)
    (dolist (row rows)
      (let ((section (drover-list--row-section row)))
        (dolist (position (list (oref section start) (oref section content) (oref section end)))
          (when (markerp position)
            (set-marker position nil)))))))

(defun drover-list--delete-section (section)
  "Delete SECTION, its text and its markers, from the buffer."
  (let ((start (marker-position (oref section start)))
        (end (marker-position (oref section end))))
    (drover-list--forget-marks start end)
    (delete-region start end)
    (magit-map-sections
     (lambda (child)
       (dolist (position (list (oref child start) (oref child content) (oref child end)))
         (when (markerp position)
           (set-marker position nil))))
     section)))

(defun drover-list--apply-visibility (section)
  "Give SECTION, inserted, the visibility its slot tells, as a drawing does."
  (if (oref section hidden)
      (magit-section-hide section)
    (magit-section-show section)))

(defun drover-list--insert-rows (tree container position layout)
  "Insert the sections of the rows of TREE in CONTAINER at POSITION, in LAYOUT."
  (goto-char position)
  (let ((magit-insert-section--parent container))
    (dolist (row (drover-list--tree-shown tree))
      (drover-list--insert-session row layout)
      (let ((section (drover-list--row-section row)))
        (setq drover-list--animated
              (append drover-list--animated
                      (drover-list--collect-animated (oref section start) (oref section content))))
        (unless (and (not (eq container magit-root-section)) (oref container hidden))
          (drover-list--apply-visibility section)))))
  ;; A container ending here ends after them.
  (when (< (oref container end) (point))
    (set-marker (oref container end) (point)))
  (when (< (oref magit-root-section end) (point))
    (set-marker (oref magit-root-section end) (point))))

(defun drover-list--follow-buffer (model plans touched vectors bodies)
  "Draw in the buffer the change of PLANS that MODEL took in.
TOUCHED lists the groups the change touched, VECTORS their trees
before it, as an alist; BODIES are as for `drover-list--follow'.  Point,
the mark and the windows showing the list keep their places as a
drawing anew keeps them."
  (let* ((layout (drover-list--model-layout model))
         (root magit-root-section)
         (groups (drover-list--model-groups model))
         (grouped (drover-list--group-value (car groups)))
         (windows (get-buffer-window-list nil nil t))
         (anchor (drover-list--anchor (point)))
         (mark (and (region-active-p) (drover-list--anchor (mark))))
         (placements (mapcar (lambda (window)
                               (list window
                                     (drover-list--anchor (window-point window))
                                     (line-number-at-pos (window-start window))))
                             windows))
         (inhibit-read-only t)
         (deactivate-mark nil)
         (fresh nil)
         (gone nil))
    ;; Trees in place in a group whose order changed move as the others.
    (dolist (group touched)
      (unless (drover-list--stable-p (or (alist-get group vectors) [])
                                     (drover-list--group-trees group) plans)
        (dolist (plan plans)
          (when (and (nth 3 plan) (eq (drover-list--tree-group (nth 1 plan)) group))
            (setf (nth 3 plan) nil)))))
    ;; Rows of trees in place take the sections of those they replace.
    (pcase-dolist (`(,_ ,new ,old-rows ,same) plans)
      (when same
        (cl-mapc (lambda (row old-row)
                   (unless (eq row old-row)
                     (setf (drover-list--row-section row) (drover-list--row-section old-row))))
                 (drover-list--tree-shown new) old-rows)))
    (save-excursion
      ;; Trees leaving their place, and groups emptied, go first.
      (pcase-dolist (`(,old ,_ ,old-rows ,same) plans)
        (when (and old (not same) old-rows (drover-list--row-section (car old-rows)))
          (drover-list--delete-rows old-rows)))
      (dolist (group touched)
        (when (and (zerop (drover-list--group-size group)) (drover-list--group-container group)
                   (drover-list--group-value group))
          (drover-list--delete-section (drover-list--group-container group))
          (setf (drover-list--group-container group) nil)))
      ;; Rows of trees leaving their place have no section until drawn.
      (pcase-dolist (`(,_ ,new ,_ ,same) plans)
        (when (and new (not same))
          (dolist (row (drover-list--tree-shown new))
            (setf (drover-list--row-section row) nil))))
      ;; Groups coming, whole, in their place.
      (let ((next nil))
        (dolist (group (reverse groups))
          (unless (drover-list--group-container group)
            (goto-char (if next
                           (oref (drover-list--group-container next) start)
                         (oref root end)))
            (let ((magit-insert-section--parent root))
              (drover-list--insert-group group layout))
            (when (< (oref root end) (point))
              (set-marker (oref root end) (point)))
            (let ((container (drover-list--group-container group)))
              (drover-list--apply-visibility container)
              (seq-doseq (tree (drover-list--group-trees group))
                (dolist (row (drover-list--tree-shown tree))
                  (push row fresh)))
              (setq drover-list--animated
                    (append drover-list--animated
                            (drover-list--collect-animated (oref container start) (oref container end))))))
          (setq next group)))
      ;; Trees coming, each before the next one drawn in its group.  In
      ;; a group whose trees are drawn but the last ones, the trees
      ;; drawn come first: one coming among those left is left too.
      (dolist (group touched)
        (when-let* (((> (drover-list--group-size group) 0))
                    (container (drover-list--group-container group)))
          (let ((next nil)
                (deferred (drover-list--deferred-p group))
                (trees (drover-list--group-trees group)))
            (cl-loop for index from (1- (length trees)) downto 0
                     do (let* ((tree (aref trees index))
                               (row (car (drover-list--tree-shown tree)))
                               (next-section (and next (drover-list--row-section
                                                        (car (drover-list--tree-shown next))))))
                          (unless (or (drover-list--row-section row)
                                      (and deferred (not next-section)))
                            (drover-list--insert-rows
                             tree container
                             (cond (next-section (oref next-section start))
                                   ((drover-list--group-value group) (1- (oref container end)))
                                   (t (oref container end)))
                             layout)
                            (dolist (row (drover-list--tree-shown tree))
                              (push row fresh)))
                          (setq next tree)))
            (oset container children
                  (mapcan (lambda (tree)
                            (when (drover-list--row-section (car (drover-list--tree-shown tree)))
                              (mapcar #'drover-list--row-section (drover-list--tree-shown tree))))
                          (append trees nil))))))
      (when grouped
        (oset root children (mapcar #'drover-list--group-container groups)))
      ;; The list begins where the buffer does, whatever came before
      ;; what began it: its start advances on insertion, as Magit makes
      ;; starts.
      (set-marker (oref root start) (point-min))
      ;; Trees in place: the lines that changed.
      (pcase-dolist (`(,_ ,new ,old-rows ,same) plans)
        (when (and same (drover-list--row-section (car old-rows)))
          (cl-mapc (lambda (row old-row)
                     (let* ((heading (drover-list--heading row layout))
                            (changed (not (and (drover-list--row-heading old-row)
                                               (equal-including-properties
                                                (car heading) (car (drover-list--row-heading old-row))))))
                            (body (or (not (eq row old-row)) (member (drover-list--row-key row) bodies))))
                       (when (or changed body)
                         (drover-list--redraw-row row (and changed heading) body))
                       (when (not (eq row old-row))
                         (push row fresh))))
                   (drover-list--tree-shown new) old-rows)))
      ;; What tells of them: group headings.
      (dolist (group touched)
        (when-let* (((drover-list--group-value group))
                    ((> (drover-list--group-size group) 0))
                    (text (drover-list--group-heading group layout))
                    ((not (equal-including-properties text (drover-list--group-heading-text group)))))
          (drover-list--redraw-group group text))))
    (drover-list--forget-gone-sections)
    ;; Sessions gone are forgotten.
    (pcase-dolist (`(,_ ,_ ,old-rows ,_) plans)
      (dolist (row old-rows)
        (let ((key (drover-list--row-key row)))
          (unless (gethash key (drover-list--model-rows model))
            (push key gone)
            (remhash key drover-list--drawn)
            (remhash key drover-list--title-states)
            (remhash (cons 'session key) drover-list--visibility)
            (when drover-list--recaps
              (remhash key drover-list--recaps))))))
    (goto-char (drover-list--resolve anchor))
    (pcase-dolist (`(,window ,window-anchor ,start-line) placements)
      (set-window-point window (drover-list--resolve window-anchor))
      (set-window-start window
                        (save-excursion
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point))
                        t))
    (when (eq (window-buffer) (current-buffer))
      (goto-char (window-point)))
    (when mark
      (set-marker (mark-marker) (drover-list--resolve mark)))
    (drover-list--draw-caches)
    ;; What the sessions drawn lack is asked for, if they show.
    (when drover-list--prefetched
      (let ((sessions nil))
        (dolist (row fresh)
          (let ((group (drover-list--tree-group (drover-list--row-tree row))))
            (unless (and (drover-list--group-value group)
                         (oref (drover-list--group-container group) hidden))
              (puthash (drover-list--row-key row) t drover-list--prefetched)
              (push (drover-list--row-session row) sessions))))
        (when sessions
          (drover-prefetch sessions drover-list--prefetch-kinds))))
    (magit-section-update-highlight t)
    (set-buffer-modified-p nil)
    (push (current-buffer) magit-section--refreshed-buffers)
    (ignore gone)
    (drover-list--defer)
    (drover-list--update-timers)))

(defun drover-list--visible-p (buffer)
  "Return non-nil when a window of a visible or iconified frame shows BUFFER."
  (get-buffer-window buffer 0))

;;;; Lists, the windows showing them, and the focus

;; Timers ticking several times a second, and hooks running on every
;; change of the selected window, find the lists from what drover noted
;; as lists came and went and as windows changed: looking at every
;; buffer, as `match-buffers' does, cost them more than their drawing.

(defvar drover-list--live (match-buffers '(derived-mode . drover-list-mode))
  "The list buffers, oldest first.
`drover-list-mode' adds its buffer; killing it or changing its mode
takes it out (`drover-list--forget').  Loaded over a drover that kept
none, the lists of that drover are found once.")

(defun drover-list--buffers ()
  "Return the live list buffers, oldest first."
  (if (seq-every-p #'buffer-live-p drover-list--live)
      drover-list--live
    (setq drover-list--live (seq-filter #'buffer-live-p drover-list--live))))

(defun drover-list--forget ()
  "Take the current buffer out of the list buffers.
For `kill-buffer-hook' and `change-major-mode-hook'."
  (setq drover-list--live (remq (current-buffer) drover-list--live)))

(defvar drover-list--windows nil
  "The windows found showing lists, as an alist (BUFFER . WINDOWS).
They are found again as windows change, lists draw and timers start or
stop (`drover-list--find-windows'), in every frame, visible or not;
what uses them checks that each still shows its list in a visible
frame (`drover-list--showing').")

(defun drover-list--find-windows ()
  "Find the windows showing lists, in `drover-list--windows'."
  (setq drover-list--windows
        (let ((found nil))
          (dolist (buffer (drover-list--buffers))
            (when-let* ((windows (get-buffer-window-list buffer nil t)))
              (push (cons buffer windows) found)))
          (nreverse found))))

(defun drover-list--showing ()
  "Return the lists that windows of visible frames show, with what they show.
The value is an alist (BUFFER . RANGES), RANGES the parts of BUFFER
the windows show, as (START . END), as they were last displayed.  The
windows are those found last (`drover-list--windows'): it looks at no
other window, nor at any buffer."
  (let ((showing nil))
    (pcase-dolist (`(,buffer . ,windows) drover-list--windows)
      (let ((ranges nil))
        (dolist (window windows)
          (when (and (window-live-p window)
                     (eq (window-buffer window) buffer)
                     (eq (frame-visible-p (window-frame window)) t))
            (push (cons (window-start window) (window-end window)) ranges)))
        (when ranges
          (push (cons buffer ranges) showing))))
    (nreverse showing)))

(defun drover-list--shown-buffers ()
  "Return the live list buffers a window of a visible or iconified frame shows."
  (drover-list--find-windows)
  (let ((shown nil))
    (pcase-dolist (`(,buffer . ,windows) drover-list--windows)
      (when (seq-some (lambda (window) (frame-visible-p (window-frame window))) windows)
        (push buffer shown)))
    (nreverse shown)))

(defvar drover-list--focused t
  "Non-nil unless Emacs lost focus, as `drover-list--focus-state' told last.
Another frame of Emacs having focus, it has: only the pace of the
animated glyphs follows it.")

(defun drover-list--focus-state ()
  "Return non-nil unless Emacs is known to have lost focus.
See `opencode-session-status-focused-p'."
  (opencode-session-status-focused-p))

(defvar drover-list--focus-timer nil
  "Timer looking at the focus once its events settled, or nil.")

(defun drover-list--focus-changed ()
  "Look at the focus again, once the events moving it settled.
Moving from one frame to another, Emacs may tell that the first lost
focus before telling that the other has it.  For
`after-focus-change-function'."
  (drover-with-reporting "list focus"
    (unless (timerp drover-list--focus-timer)
      (setq drover-list--focus-timer (run-at-time 0 nil #'drover-list--refocus)))))

(defun drover-list--refocus ()
  "Follow the focus of Emacs, once its events settled.
Losing focus, the animated glyphs the lists show move at the pace of
`opencode-session-status-animation-unfocused-interval'; regaining it,
they show the frame of the time at once, rather than at the next tick,
and move at the pace of `opencode-session-status-animation-interval'.
The other timers keep theirs."
  (drover-with-reporting "list focus"
    (setq drover-list--focus-timer nil)
    (let ((focused (drover-list--focus-state)))
      (unless (eq focused drover-list--focused)
        (setq drover-list--focused focused)
        (drover-list--find-windows)
        (when focused
          (let ((now (drover-list--now)))
            (pcase-dolist (`(,buffer . ,ranges) (drover-list--showing))
              (with-current-buffer buffer
                (drover-list--draw-frames now ranges)))))
        (drover-list--update-timers)))))

(defun drover-list--drop-stale ()
  "Drop what another version of drover drew of the current list; non-nil if so.
Its model and rows, of other slots, would be misread: the list is to
be drawn whole."
  (when (and drover-list--model
             (or (not (drover-layout-current-p drover-list--model 'drover-list--model))
                 ;; Rows measure each text field: those of another
                 ;; version's fields would be misread too.
                 (not (eql (length (drover-list--model-summary drover-list--model))
                           drover-list--measure-count))))
    (setq drover-list--model nil
          drover-list--drawn nil
          drover-list--pending nil
          drover-list--stale t)
    (drover-stale-layout "list of sessions")
    t))

(defun drover-list--current-p ()
  "Return non-nil unless the current list was drawn by another version of drover.
Otherwise it is drawn whole, now if it shows, else once it shows (see
`drover-list--drop-stale')."
  (not (when (drover-list--drop-stale)
         (when (drover-list--visible-p (current-buffer))
           (drover-list--render))
         t)))

(defun drover-list--update (buffer)
  "Redraw list BUFFER if it is displayed, else mark it stale."
  (with-current-buffer buffer
    (if (drover-list--visible-p buffer)
        (drover-list--render)
      (setq drover-list--stale t
            drover-list--pending nil))))

(defun drover-list--note-pending (keys bodies times)
  "Note that the sessions KEYS changed while the current list is hidden.
BODIES are as for `drover-list--follow', TIMES non-nil when time passed.
They are drawn in place once the list shows; past
`drover-list--pending-limit' sessions, the list is drawn whole then."
  (unless drover-list--stale
    (pcase-let* ((`(,old-keys ,old-bodies ,old-times) drover-list--pending)
                 (keys (seq-union old-keys keys))
                 (bodies (seq-union old-bodies bodies)))
      (if (> (+ (length keys) (length bodies)) drover-list--pending-limit)
          (setq drover-list--stale t
                drover-list--pending nil)
        (setq drover-list--pending (list keys bodies (or old-times times)))))))

(defun drover-list--draw-whole-p (keys bodies)
  "Return non-nil when the change of KEYS and BODIES is drawn whole, not in place.
BODIES are as for `drover-list--follow'.  A session that changed costs
about a row to draw in place, so that past `drover-list--pending-limit'
of them drawing whole costs about the same.  Sessions leaving cost
little in place, the trees they leave too, as when archiving a
selection takes hundreds away: while more leave than change, the
change is drawn in place, unless as many leave as the list holds rows,
which drawing whole empties at once."
  (let* ((rows (and drover-list--model (drover-list--model-rows drover-list--model)))
         (gone 0)
         (changed (length bodies)))
    (dolist (key keys)
      (if (drover-index-session key) (cl-incf changed) (cl-incf gone)))
    (if (> gone changed)
        (and (> gone drover-list--pending-limit)
             (>= gone (if rows (hash-table-count rows) 0)))
      (> changed drover-list--pending-limit))))

(defun drover-list--update-sessions (buffer keys &optional bodies)
  "Bring list BUFFER up to date with the change of the sessions KEYS.
Draw it in place if displayed, and anew when it cannot; otherwise note
them, to draw once it shows.  BODIES are as for `drover-list--follow'.
Older and archived sessions, which the index does not hold, draw the
list anew."
  (with-current-buffer buffer
    (cond ((not (drover-list--current-p)))
          ((not (drover-list--visible-p buffer))
           (drover-list--note-pending keys bodies nil))
          ((or drover-show-history drover-show-archived)
           (drover-list--render))
          ((not (or keys bodies)))
          ((drover-list--draw-whole-p keys bodies)
           (drover-list--render))
          ((drover-list--follow keys bodies))
          (t (drover-list--render)))))

(defun drover-list--on-change ()
  "Bring every list up to date with the backends.
For `drover-changed-hook': the sessions `drover-changed-sessions' tells
are drawn in place, when they can be, in each list shown; the trees
they make are made once for every list (see `drover-list--sharing')."
  (let ((changed drover-changed-sessions)
        (drover-list--sharing (list :now (drover-list--now)
                                    :trees (make-hash-table :test #'equal))))
    (dolist (buffer (drover-list--buffers))
      (if (eq changed t)
          (drover-list--update buffer)
        (drover-list--update-sessions buffer changed)))))

(defun drover-list--refresh-if-needed ()
  "Bring the current list up to date, now that it shows.
Redraw it if it is stale or was drawn for another width; otherwise draw
in place what changed while it was hidden, and bring the faces of
titles and the timers up to date.  The sidebar's side windows get its
settings first (see `drover-list--set-up-side-windows'), and windows
scrolled sideways scroll back (see `drover-list--unscroll')."
  (when (drover-list--current-p)
    (drover-list--set-up-side-windows)
    (drover-list--unscroll)
    (if (or drover-list--stale (not (eql drover-list--width (drover-list--line-width))))
        (drover-list--render)
      (pcase-let ((`(,keys ,bodies ,times) drover-list--pending))
        (setq drover-list--pending nil)
        (if times
            (drover-list--refresh-times keys bodies)
          (when (or keys bodies)
            (drover-list--update-sessions (current-buffer) keys bodies))))
      (drover-list--restyle-titles)
      (drover-list--defer)
      (drover-list--update-timers))))

(defun drover-list--unscroll ()
  "Scroll back the windows showing the current list that are scrolled sideways.
Lines are laid out for the width of the window, from its left edge: a
window scrolled sideways shows them cut on the left, filling part of
it.  `auto-hscroll-mode' is off in lists, so that point at the end of
a line filling the window does not scroll it, but a window may have
been scrolled before, or by a command."
  (dolist (window (get-buffer-window-list nil 'nomini t))
    (unless (zerop (window-hscroll window))
      (set-window-hscroll window 0))))

(defun drover-list--on-window-change (window)
  "Bring the list WINDOW shows up to date, now that WINDOW shows it.
For `window-buffer-change-functions' and `window-size-change-functions',
which run in the list's buffer: the layout follows the width."
  (drover-with-reporting "list windows"
    (let ((buffer (window-buffer window)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (derived-mode-p 'drover-list-mode)
            (drover-list--refresh-if-needed)))))))

(defun drover-list--on-face-change ()
  "Redraw the list, whose text changed size, or mark it stale if hidden.
For `text-scale-mode-hook' and `buffer-face-mode-hook': lines are laid
out in pixels, measured in the faces of the buffer."
  (drover-with-reporting "list faces"
    (when (derived-mode-p 'drover-list-mode)
      (if (drover-list--visible-p (current-buffer))
          (drover-list--render)
        (setq drover-list--stale t)))))

(defun drover-list--revert (&optional ignore-auto _noconfirm)
  "Fetch older and archived sessions again if shown, redraw, list them anew.
The recaps of expanded sessions are fetched anew too.  Sessions are
listed anew once the list shows (see `drover-list--relist-soon').
Rows of sessions that did not change are kept, and texts measured
anew only when fonts changed; with IGNORE-AUTO nil, as with a prefix
argument to \\[revert-buffer], every row is made and measured anew.
For `revert-buffer-function'."
  (when drover-show-history
    (drover-history-fetch))
  (when drover-show-archived
    (drover-archived-fetch))
  (unless ignore-auto
    ;; Nothing drawn before is kept: rows are made and measured anew.
    (setq drover-list--widths nil
          drover-list--model nil))
  (setq drover-list--recaps nil)
  (drover-list--render)
  (drover-list--relist-soon))

(defvar drover-list--relist-timer nil
  "Timer listing the sessions anew after a refresh, or nil.")

(defun drover-list--relist-soon ()
  "List the sessions anew once the list shows, drawing what changed in place.
Backends report what changes, but for details such as a branch: a
refresh lists every session anew, for every view (see
`drover-index-refresh').  With thousands of sessions that takes as long
as drawing, so the list is drawn first, from what was reported."
  (unless (timerp drover-list--relist-timer)
    (setq drover-list--relist-timer
          (run-at-time 0 nil (lambda ()
                               (setq drover-list--relist-timer nil)
                               (drover-with-reporting "list refresh"
                                 (drover-index-refresh t)))))))

(defun drover-list--refresh-times (&optional keys bodies)
  "Draw again the rows of the current list whose times read otherwise now.
Those are their ages, and whether their prompt caches live, and the
trees that go to another group of last activity: rows of sessions that
did not change are drawn in place, as the time passes.  The sessions
KEYS and BODIES, as for `drover-list--follow', are drawn with them."
  (when-let* ((model drover-list--model))
    (let ((now (drover-list--now)))
      (when (eq drover-group-by 'updated)
        (dolist (group (drover-list--model-groups model))
          (seq-doseq (tree (drover-list--group-trees group))
            (unless (eq (drover-tree-activity-bucket (drover-list--tree-node tree) now)
                        (cdr (drover-list--group-value group)))
              (push (drover-list--tree-root tree) keys)))))
      (maphash (lambda (key row)
                 (let ((inputs (drover-list--row-inputs row))
                       (session (drover-list--row-session row)))
                   (unless (and inputs
                                (equal (aref inputs 6)
                                       (opencode-format-age (drover-list--age-time session) now))
                                (equal (aref inputs 7)
                                       (opencode-format-age (drover-session-created session) now))
                                (eq (aref inputs 8)
                                    (and (drover-list--cache-fraction session now) t))
                                (equal (aref inputs 10)
                                       (let ((archived (drover-session-archived session)))
                                         (and (numberp archived) (opencode-format-age archived now))))
                                (equal (aref inputs 11) (drover-list--host-age session now)))
                     (push key keys))))
               (drover-list--model-rows model))
      (when (or keys bodies)
        (drover-list--update-sessions (current-buffer) keys bodies)))))

;;;; Timers

(defvar drover-list--age-timer nil
  "Timer redrawing displayed lists, so that ages stay current.")

(defvar drover-list--cache-timer nil
  "Timer redrawing the prompt caches of displayed lists.")

(defvar drover-list--frame-timer nil
  "Timer showing the next frames of the animated glyphs of displayed lists.")

(defvar drover-list--frame-timer-pace nil
  "Seconds between the ticks of `drover-list--frame-timer', or nil.")

(defvar drover-list--age-due nil
  "When the age timer is to tick next, as a float time, or nil before it ran.
A timer started again, once a list shows again, ticks then, or a second
later when that time passed.")

(defun drover-list--age-tick ()
  "Keep displayed lists current; stop when none is shown.
What may have changed unreported, as a branch, is listed anew; the rows
whose ages read otherwise are drawn again in place.  A hidden list notes
that time passed, and draws those rows once shown."
  (drover-with-reporting "list timer"
    (setq drover-list--age-due (and drover-list-age-interval
                                    (+ (float-time) drover-list-age-interval)))
    (drover-index-refresh)
    (dolist (buffer (drover-list--buffers))
      (with-current-buffer buffer
        (if (drover-list--visible-p buffer)
            (drover-list--refresh-times)
          (drover-list--note-pending nil nil t))))
    (drover-list--update-timers)))

(defun drover-list--cache-tick ()
  "Redraw the prompt caches lists show; stop when none shows a live one."
  (drover-with-reporting "list timer"
    (let ((showing (drover-list--showing))
          (now (drover-list--now)))
      (pcase-dolist (`(,buffer . ,ranges) showing)
        (when (buffer-local-value 'drover-list--caches buffer)
          (with-current-buffer buffer
            (drover-list--draw-caches now ranges))))
      (unless (seq-some (lambda (entry) (buffer-local-value 'drover-list--caches (car entry)))
                        showing)
        (drover-list--update-timers)))))

(defun drover-list--frame-tick ()
  "Show the next frames of the glyphs lists show; stop when none animates.
It looks at the windows found showing lists, not at every buffer nor
window, and changes only the glyphs those windows show.  A tick due as
its timer stopped, the glyphs standing still while Emacs has no focus,
draws nothing."
  (drover-with-reporting "list timer"
    (let ((showing (and (drover-list--frame-pace) (drover-list--showing)))
          (now (drover-list--now)))
      (pcase-dolist (`(,buffer . ,ranges) showing)
        (when (buffer-local-value 'drover-list--animated buffer)
          (with-current-buffer buffer
            (drover-list--draw-frames now ranges))))
      (unless (seq-some (lambda (entry) (buffer-local-value 'drover-list--animated (car entry)))
                        showing)
        (drover-list--update-timers)))))

(defun drover-list--stop-timer (symbol)
  "Cancel the timer in the variable SYMBOL, and forget it."
  (when (timerp (symbol-value symbol))
    (cancel-timer (symbol-value symbol)))
  (set symbol nil))

(defun drover-list--keep-timer (symbol interval needed function &optional first)
  "Keep the timer in SYMBOL running FUNCTION every INTERVAL seconds while NEEDED.
Start it if needed and not running, FIRST seconds from now, INTERVAL by
default; stop it when INTERVAL or NEEDED is nil."
  (if (and interval needed)
      (unless (timerp (symbol-value symbol))
        (set symbol (run-at-time (or first interval) interval function)))
    (drover-list--stop-timer symbol)))

(defun drover-list--frame-pace ()
  "Return the seconds between two frames the lists show now, or nil for none.
See `opencode-session-status-frame-pace': slower while Emacs has no
focus, as `drover-list--focused' tells."
  (opencode-session-status-frame-pace drover-list--focused))

(defun drover-list--update-timers ()
  "Run the timers lists shown need, and only those.
Every timer pauses while no window of a visible frame shows a list:
ages need redraws while a list shows; prompt caches while a list shown
shows a live one; animated glyphs while a list shown shows one, at the
pace of `drover-list--frame-pace', slower or not at all while Emacs has
no focus.  Caches and glyphs change only a few characters: their
display, and the text of a cache's time left, never the sections."
  (drover-list--find-windows)
  (let ((showing (drover-list--showing))
        (now (float-time))
        (pace (drover-list--frame-pace)))
    (drover-list--keep-timer 'drover-list--age-timer drover-list-age-interval showing
                             #'drover-list--age-tick
                             (and drover-list-age-interval drover-list--age-due
                                  (max 1 (- drover-list--age-due now))))
    (when (and (timerp drover-list--age-timer) (not drover-list--age-due))
      (setq drover-list--age-due (+ now drover-list-age-interval)))
    (drover-list--keep-timer 'drover-list--cache-timer drover-list-cache-interval
                             (seq-some (lambda (entry) (buffer-local-value 'drover-list--caches (car entry)))
                                       showing)
                             #'drover-list--cache-tick)
    (unless (eql pace drover-list--frame-timer-pace)
      (drover-list--stop-timer 'drover-list--frame-timer))
    (drover-list--keep-timer 'drover-list--frame-timer pace
                             (seq-some (lambda (entry) (buffer-local-value 'drover-list--animated (car entry)))
                                       showing)
                             #'drover-list--frame-tick
                             (and pace (opencode-session-status-frame-delay (drover-list--now))))
    (setq drover-list--frame-timer-pace (and (timerp drover-list--frame-timer) pace))))

;;;; Windows

(defun drover-list--side-window (&optional frame)
  "Return the side window showing the sidebar on FRAME, or nil.
FRAME defaults to the selected frame."
  (when-let* ((buffer (get-buffer drover-list-sidebar-buffer-name)))
    (seq-find (lambda (window) (window-parameter window 'window-side))
              (get-buffer-window-list buffer nil (or frame (selected-frame))))))

(defun drover-list--main-window (&optional frame)
  "Return the window of FRAME where the user works beside the lists, or nil.
It is the most recently used window outside side windows showing no
list, the sidebar's nor the other's, preferring one not dedicated to
its buffer.  FRAME defaults to the selected frame."
  (let (best best-dedicated)
    (dolist (window (window-list (or frame (selected-frame)) 'nomini))
      (unless (or (window-parameter window 'window-side)
                  (with-current-buffer (window-buffer window)
                    (derived-mode-p 'drover-list-mode)))
        (if (window-dedicated-p window)
            (when (or (null best-dedicated)
                      (> (window-use-time window) (window-use-time best-dedicated)))
              (setq best-dedicated window))
          (when (or (null best) (> (window-use-time window) (window-use-time best)))
            (setq best window)))))
    (or best best-dedicated)))

(defun drover-list--target-window ()
  "Return the main window when the current list shows in a side window, else nil.
It is the list's `drover-main-window-function': from a side window,
sessions open in the main window; from an ordinary one, as usual."
  (let ((window (if (eq (window-buffer) (current-buffer))
                    (selected-window)
                  (get-buffer-window nil))))
    (and window (window-parameter window 'window-side)
         (drover-list--main-window (window-frame window)))))

(defun drover-list--may-show-p (window)
  "Return non-nil when WINDOW may be given the list of `drover-list' to show.
Side windows, such as the sidebar's, minibuffer windows and windows
dedicated to their buffer may not."
  (and (window-live-p window)
       (not (window-minibuffer-p window))
       (not (window-parameter window 'window-side))
       (not (window-dedicated-p window))))

(defun drover-list--home-window ()
  "Return the window `drover-list' shows its list in by default, or nil.
It is the selected window when it may show the list, else the window
where you work, such as from the sidebar (see `drover-list--main-window'),
when that one may."
  (if (drover-list--may-show-p (selected-window))
      (selected-window)
    (when-let* ((main (drover-list--main-window)))
      (and (drover-list--may-show-p main) main))))

(defun drover-list-display-in-main-window (buffer alist)
  "Display BUFFER in the window where you work, and return that window.
It is an action function for `display-buffer', for the list of
`drover-list' shown from a window that may not show it, such as the
sidebar's: the list takes the window where you work, as sessions
opened from the sidebar do, rather than a window split from it.  That
window is the most recently used of the selected frame outside side
windows, showing no list and not dedicated to its buffer; without one,
or when it is the selected window and ALIST has a non-nil
`inhibit-same-window' entry, return nil."
  (when-let* ((window (drover-list--main-window))
              ((drover-list--may-show-p window))
              ((not (and (eq window (selected-window))
                         (cdr (assq 'inhibit-same-window alist))))))
    (window--display-buffer buffer window 'reuse alist)))

(defun drover-list-display-in-other-window (buffer alist)
  "Display BUFFER in another window than the list's default one, and return it.
It is an action function for `display-buffer', for the list of
`drover-list' given a prefix argument.  The default window is the
selected one, or, from a window that may not show the list, such as the
sidebar's, the window where you work (see `drover-list--home-window').
The other window is chosen as `drover-other-window' chooses one for a
session: the one used last, else one split from the default window.
Return nil when there is none, ALIST being the action's alist."
  (when-let* ((home (drover-list--home-window)))
    (let* ((windows (window-list (window-frame home) 'nomini))
           (window (ignore-error user-error (drover-other-window home))))
      (when window
        (window--display-buffer buffer window (if (memq window windows) 'reuse 'window) alist)))))

;;;; Where sessions are

;; The colour of a title tells where its session is in Emacs.  Windows
;; and the selection change far more often than sessions, so a change
;; of windows gives new faces to the titles whose place changed, in
;; place, without drawing the list anew; buffers opening or closing are
;; changes their backends report, and redraw it.

(defvar drover-list--restyle-timer nil
  "Timer giving titles their faces, once a session buffer killed is gone.")

(defun drover-list--view-window-p (window)
  "Return non-nil when WINDOW shows one of drover's views."
  (buffer-local-value 'drover-view-p (window-buffer window)))

(defun drover-list--active-window ()
  "Return the window the user works in, or nil.
It is the selected window, but a list's or the minibuffer's: then the
window selected before the minibuffer, else the window of the frame
used most recently that shows no list."
  (let ((window (selected-window)))
    (when (window-minibuffer-p window)
      (setq window (or (minibuffer-selected-window) window)))
    (if (not (or (window-minibuffer-p window) (drover-list--view-window-p window)))
        window
      (let (best)
        (dolist (other (window-list (window-frame window) 'nomini))
          (unless (or (drover-list--view-window-p other)
                      (and best (<= (window-use-time other) (window-use-time best))))
            (setq best other)))
        best))))

(defun drover-list--places ()
  "Return where buffers are shown now, as (ACTIVE . SHOWN).
ACTIVE is the buffer of `drover-list--active-window', or nil; SHOWN a
hash table of the buffers shown in windows of visible frames, as keys."
  (let ((shown (make-hash-table :test #'eq))
        (active (drover-list--active-window)))
    (dolist (window (window-list-1 nil 'nomini 'visible))
      (puthash (window-buffer window) t shown))
    (cons (and active (window-buffer active)) shown)))

(defun drover-list--title-state (session places)
  "Return where SESSION is in Emacs, given PLACES, for its title, or nil.
PLACES is a value of `drover-list--places'.  The value is `active' when
the buffer of SESSION is that of the window the user works in, `shown'
when a window of a visible frame shows it, `open' when it has a buffer
no window shows, and nil without a buffer."
  (when-let* ((places)
              (buffer (drover-session-buffer session)))
    (cond ((eq buffer (car places)) 'active)
          ((gethash buffer (cdr places)) 'shown)
          (t 'open))))

(defun drover-list--title-run (key)
  "Return where the title of the session KEY lies, as (KEY START END), or nil.
The title is found by its property `drover-list-title' in the session's
heading, its ellipsis included when cut."
  (when-let* ((section (drover-list--find-session key))
              (start (text-property-not-all (oref section start) (oref section content)
                                            'drover-list-title nil)))
    (list key start (next-single-property-change start 'drover-list-title nil
                                                 (oref section content)))))

(defun drover-list--title-faces (face state)
  "Return FACE, the face of a title, with the face of STATE in place of its own.
FACE is a value of the property `face'; the face of a state comes
first, over the others, as `drover-list--title-text' puts it."
  (let* ((others (seq-remove (lambda (face)
                               (memq face '(opencode-session-active opencode-session-shown
                                                                    opencode-session-open)))
                             (ensure-list face)))
         (faces (delq nil (cons (drover-list--title-face state) others))))
    (if (cdr faces) faces (car faces))))

(defun drover-list--reface (start end state)
  "Give the title between START and END the face of STATE, in place.
Every character of a title has the same faces, the ellipsis of a cut
title included."
  (let ((face (drover-list--title-faces (get-text-property start 'face) state)))
    (put-text-property start end 'face face)
    (put-text-property start end 'font-lock-face face)))

(defun drover-list--restyle-titles ()
  "Give the titles of the current list the faces of where their sessions are now.
Only the titles whose session changed place change, in place: the list
is not drawn anew, but when a face would change the width of a title,
as a bold one would, whose line was laid out in the other one."
  (when (drover-list--current-p)
    (when (and drover-list--drawn drover-list--title-states)
      (let ((places (drover-list--places))
            (changed (make-hash-table :test #'equal)))
        (maphash (lambda (key _)
                   (let* ((session (gethash key drover-list--drawn))
                          (state (drover-list--title-state session places)))
                     (unless (eq state (gethash key drover-list--title-states))
                       (puthash key state changed))
                     (when state
                       (puthash (drover-session-buffer session) t drover-list--session-buffers))))
                 drover-list--drawn)
        (unless (zerop (hash-table-count changed))
          (let ((runs (delq nil (mapcar #'drover-list--title-run (hash-table-keys changed)))))
            (if (seq-some (pcase-lambda (`(,key ,start ,end))
                            (let ((text (buffer-substring start end)))
                              (/= (drover-list--pixel-width text)
                                  (drover-list--pixel-width
                                   (propertize text 'face (drover-list--title-faces
                                                           (get-text-property 0 'face text)
                                                           (gethash key changed)))))))
                          runs)
                (drover-list--render)
              (let ((inhibit-read-only t)
                    (deactivate-mark nil))
                (with-silent-modifications
                  (pcase-dolist (`(,key ,start ,end) runs)
                    (drover-list--reface start end (gethash key changed)))))
              (maphash (lambda (key state)
                         (puthash key state drover-list--title-states)
                         ;; Its row no longer tells what its line shows:
                         ;; made anew the next time it is drawn.
                         (when-let* ((row (and drover-list--model
                                               (gethash key (drover-list--model-rows drover-list--model)))))
                           (setf (drover-list--row-state row) state
                                 (drover-list--row-inputs row) nil
                                 (drover-list--row-heading row) nil)))
                       changed))))))))

(defun drover-list--windows-changed (&optional _frame)
  "Give the titles of displayed lists the faces of where their sessions are.
For `window-buffer-change-functions' and
`window-selection-change-functions'."
  (drover-with-reporting "list windows"
    (when (timerp drover-list--restyle-timer)
      (cancel-timer drover-list--restyle-timer))
    (setq drover-list--restyle-timer nil)
    (dolist (buffer (drover-list--shown-buffers))
      (with-current-buffer buffer
        (drover-list--restyle-titles)))
    ;; A list hidden, or shown, pauses or runs the timers.
    (drover-list--update-timers)))

(defun drover-list--buffer-killed ()
  "Give titles new faces once the session buffer being killed is gone.
For `kill-buffer-hook', which runs while the buffer still lives."
  (drover-with-reporting "list windows"
    (when (gethash (current-buffer) drover-list--session-buffers)
      (remhash (current-buffer) drover-list--session-buffers)
      (unless (timerp drover-list--restyle-timer)
        (setq drover-list--restyle-timer (run-at-time 0 nil #'drover-list--windows-changed))))))

(defun drover-list--theme-changed (&rest _)
  "Measure text anew and redraw lists, a theme having been enabled or disabled.
A theme may change fonts, which widths depend on, the colour of
`opencode-cache-bar', which the lines of prompt caches are drawn in,
and the backgrounds the sidebar's highlight is made of (see
`drover-list--remap-highlight').  For `enable-theme-functions' and
`disable-theme-functions'."
  (drover-with-reporting "list theme"
    (dolist (buffer (drover-list--buffers))
      (with-current-buffer buffer
        (setq drover-list--widths nil))
      (drover-list--update buffer))
    (drover-list--rehighlight)))

(defun drover-list--watch ()
  "Follow sessions, windows and the focus while a list buffer lives.
Magit's selection in lists is drover's then (see
`drover-list--region-advice')."
  (add-hook 'drover-changed-hook #'drover-list--on-change)
  (add-hook 'window-buffer-change-functions #'drover-list--windows-changed)
  (add-hook 'window-selection-change-functions #'drover-list--windows-changed)
  (add-hook 'window-buffer-change-functions #'drover-list--place-soon)
  (add-hook 'window-configuration-change-hook #'drover-list--place-soon)
  (add-hook 'kill-buffer-hook #'drover-list--buffer-killed)
  (add-hook 'enable-theme-functions #'drover-list--theme-changed)
  (add-hook 'disable-theme-functions #'drover-list--theme-changed)
  (advice-add 'magit-region-sections :around #'drover-list--region-advice)
  (unless (advice-function-member-p #'drover-list--focus-changed after-focus-change-function)
    (setq drover-list--focused (drover-list--focus-state))
    (add-function :after after-focus-change-function #'drover-list--focus-changed)))

(defun drover-list--on-kill ()
  "Stop following sessions, windows and the focus once the last list is killed.
The list killed draws no recap later, and a list whose mode changes,
as a mode applied again, keeps no overlay of the caches it drew.  For
`kill-buffer-hook' and `change-major-mode-hook'."
  (drover-with-reporting "list"
    (drover-list--forget)
    (drover-list--drop-caches)
    (when (timerp drover-list--recap-timer)
      (cancel-timer drover-list--recap-timer))
    (when (timerp drover-list--deferred-timer)
      (cancel-timer drover-list--deferred-timer))
    (unless (seq-some (lambda (buffer) (not (eq buffer (current-buffer))))
                      (drover-list--buffers))
      (remove-hook 'drover-changed-hook #'drover-list--on-change)
      (remove-hook 'window-buffer-change-functions #'drover-list--windows-changed)
      (remove-hook 'window-selection-change-functions #'drover-list--windows-changed)
      (remove-hook 'window-buffer-change-functions #'drover-list--place-soon)
      (remove-hook 'window-configuration-change-hook #'drover-list--place-soon)
      (remove-hook 'kill-buffer-hook #'drover-list--buffer-killed)
      (remove-hook 'enable-theme-functions #'drover-list--theme-changed)
      (remove-hook 'disable-theme-functions #'drover-list--theme-changed)
      (advice-remove 'magit-region-sections #'drover-list--region-advice)
      (remove-function after-focus-change-function #'drover-list--focus-changed)
      (setq drover-list--windows nil)
      (drover-list--stop-timer 'drover-list--focus-timer)
      (drover-list--stop-timer 'drover-list--restyle-timer)
      (drover-list--stop-timer 'drover-list--place-timer)
      (drover-list--stop-timer 'drover-list--relist-timer)
      (drover-list--stop-timer 'drover-list--age-timer)
      (drover-list--stop-timer 'drover-list--cache-timer)
      (drover-list--stop-timer 'drover-list--frame-timer))))

;;;; Selection

;; An active region selects the rows of sessions it touches, those
;; shown: the heading of a group selects nothing, and neither do the
;; sessions of a collapsed group, which do not show.  So a region
;; sweeping over a group's heading never acts on thousands of sessions
;; the user does not see.  A region within the lines of one session
;; selects nothing, as Magit's within one section: it is text to copy.
;; Magit highlights the selection it reads from `magit-region-sections',
;; which, in a list, returns drover's (`drover-list--region-advice').

(defun drover-list--hidden-around (section)
  "Return the outermost collapsed section holding SECTION, or nil."
  (let ((hidden nil))
    (while-let ((parent (oref section parent)))
      (when (oref parent hidden)
        (setq hidden parent))
      (setq section parent))
    hidden))

(defun drover-list--selected-sections ()
  "Return the session sections the active region selects, in order, or nil.
They are those of the rows the region touches, from its beginning to
its end, both included, that show: neither a group's heading nor the
sessions of a collapsed group.  A region of some text within the lines
of one session selects nothing; an empty one selects the session it is
on, but as a click leaves it."
  (when (and (region-active-p) magit-root-section)
    (let* ((start (region-beginning))
           (end (region-end))
           (first (drover-list--session-section (magit-section-at start))))
      (unless (if (= start end)
                  (eq this-command #'mouse-drag-region)
                (and first (eq first (drover-list--session-section (magit-section-at end)))))
        (let ((position start)
              (sections nil))
          (while (<= position end)
            (let* ((section (or (magit-section-at position) magit-root-section))
                   (hidden (drover-list--hidden-around section))
                   (session (and (not hidden) (drover-list--session-section section)))
                   (next (cond (hidden (oref hidden end))
                               (session (push session sections)
                                        (oref session end))
                               (t (save-excursion
                                    (goto-char position)
                                    (line-beginning-position 2))))))
              (setq position (max (1+ position) next))))
          (nreverse sections))))))

(defun drover-list--region-advice (function &optional condition multiple)
  "Return the sections the region selects, in a list as drover selects them.
Around `magit-region-sections', which FUNCTION is, called with CONDITION
and MULTIPLE outside drover's lists: Magit highlights what it returns,
and shows the region as other buffers do when it returns nothing.  In a
list, CONDITION and MULTIPLE apply as Magit applies them, MULTIPLE
asking for two sessions at least."
  (if (derived-mode-p 'drover-list-mode)
      (drover-with-reporting "list selection"
        (let ((sections (drover-list--selected-sections)))
          (and sections
               (or (not multiple) (cdr sections))
               (or (not condition)
                   (seq-every-p (lambda (section) (magit-section-match condition section))
                                sections))
               sections)))
    (funcall function condition multiple)))

(defun drover-list--selected-sessions ()
  "Return the current records of the sessions the region selects, or nil.
For `drover-selected-sessions-function'.  Records come from the
backends, as at point; a session gone since the list was drawn is left
out."
  (when-let* ((sections (drover-list--selected-sections)))
    (let ((known nil))
      (delq nil (mapcar (lambda (section)
                          (let ((key (oref section value)))
                            (if (functionp (drover-backend-property (car key) :session))
                                (drover-find (car key) (cdr key))
                              ;; Its backend lists every session to find one:
                              ;; once for all.
                              (unless known
                                (setq known (make-hash-table :test #'equal))
                                (dolist (session (drover-known-sessions))
                                  (puthash (drover-session-key session) session known)))
                              (gethash key known))))
                        sections)))))

(defun drover-list-copy (&optional plain)
  "Copy Org links to the selected sessions, one per line, or the region.
The sessions are those the region selects, else the session at point;
each is copied as `drover-session-link' makes its link, or by its title
without one.  A region selecting no session, which shows as a region
does in other buffers, is copied as text, as `kill-ring-save' does, and
so is any region with prefix argument PLAIN."
  (interactive "P" drover-list-mode)
  (if (and (use-region-p) (or plain (null (drover-list--selected-sections))))
      ;; The sessions of collapsed groups are copied as Emacs copies
      ;; hidden text: drawn first, the region ending after them.
      (let ((start (copy-marker (region-beginning)))
            (end (copy-marker (region-end) t)))
        (drover-list--draw-deferred start end)
        (kill-ring-save start end)
        (set-marker start nil)
        (set-marker end nil))
    (let* ((sessions (drover-selected-sessions))
           (links (mapcar #'drover-session-link sessions))
           (missing (seq-count #'null links)))
      (kill-new (string-join (cl-mapcar (lambda (session link)
                                          (or link (drover-list--one-line
                                                    (drover-session-display-title session))))
                                        sessions links)
                             "\n"))
      (deactivate-mark)
      (message "%s"
               (let ((title (drover-list--one-line (drover-session-display-title (car sessions)))))
                 (cond ((and (null (cdr sessions)) (zerop missing)) (format "Copied a link to %s" title))
                       ((null (cdr sessions))
                        (format "Copied the title of %s: no Org link type stores sessions" title))
                       ((zerop missing) (format "Copied links to %d sessions" (length sessions)))
                       (t (format "Copied links to %d sessions, %d by title: no Org link type stores %s"
                                  (length sessions) missing (if (= missing 1) "it" "them")))))))))

;;;; Commands

(defun drover-list--goto-first-session ()
  "Move point to the first session shown, if any."
  (when-let* ((container (car (drover-list--containers)))
              (target (or (car (drover-list--session-sections container))
                          (and (not (eq container magit-root-section)) container))))
    (goto-char (drover-list--visible-start target))))

(defun drover-list-reveal (session)
  "Show the row of SESSION in drover's list, and select it.
In the list shown on the selected frame, the one used last of several,
the sidebar included, else in the list `drover-list' shows.  A group
collapsed around the row expands.  Signal a `user-error' when the list
does not show SESSION, filtered out or a subagent it hides."
  (let ((window (or (drover--shown-list-window)
                    (progn (drover-list) (selected-window)))))
    (select-window window)
    (drover-list--refresh-if-needed)
    (let* ((key (drover-session-key session))
           (section (or (drover-list--find-session key)
                        (progn (drover-list--draw-deferred)
                               (drover-list--find-session key)))))
      (unless section
        (user-error "The list does not show %s" (drover-session-display-title session)))
      (let ((parent (oref section parent)))
        (while parent
          (when (oref parent hidden)
            (magit-section-show parent))
          (setq parent (oref parent parent))))
      (goto-char (oref section start)))))

(defun drover-list--buffer (&optional side)
  "Return the buffer of the list, creating it if needed.
With SIDE non-nil, the sidebar's, which `drover-list-side' shows, in
`drover-list-sidebar-mode'; otherwise the one `drover-list' shows in an
ordinary window."
  (let* ((name (if side drover-list-sidebar-buffer-name drover-list-buffer-name))
         (mode (if side #'drover-list-sidebar-mode #'drover-list-mode))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      ;; A sidebar an older drover made folds: it folds no more.
      (unless (eq major-mode mode)
        (funcall mode)))
    buffer))

(defun drover-list--show (window)
  "Bring the list WINDOW shows up to date; on a first showing, go to a session."
  (with-current-buffer (window-buffer window)
    (let ((new (null drover-list--width)))
      (with-selected-window window
        (drover-list--refresh-if-needed)
        (when new
          (drover-list--goto-first-session))))))

;;;###autoload
(defun drover-list (&optional other-window)
  "Show the list of every agent session in an ordinary window, and select it.
\\<drover-list-mode-map>It lists the sessions `drover-filter' keeps, grouped as
`drover-group-by' says and sorted as `drover-sort-by' says; see
`drover-list-mode' for the keys.

A window of the frame already showing the list is selected.  Otherwise
the list takes the selected window, or, from a window that may not show
it, such as the sidebar's, the window where you work, as sessions opened
there do, rather than splitting a window; \\[drover-list-quit] in the list brings
back the buffer it replaced (see `drover-list-display-action').  With
prefix argument OTHER-WINDOW, it shows in another window instead (see
`drover-list-other-window-display-action').

The sidebar of `drover-list-side' is a list of its own: both may show
at once, each laid out for its width, with its own point, but side by
side, where the sidebar steps aside until the list leaves (see
`drover-list--place-sidebar')."
  (interactive "P")
  (drover-start)
  (let ((buffer (drover-list--buffer)))
    (pop-to-buffer buffer (if other-window
                              drover-list-other-window-display-action
                            drover-list-display-action))
    ;; Before drawing, for the width the sidebar leaves.
    (drover-list--place-sidebar (selected-frame))
    (with-current-buffer buffer
      ;; Asked for, so drawn anew whatever its state.
      (setq drover-list--stale t))
    (drover-list--show (selected-window))))

(defconst drover-list--side-window-parameters
  '((no-delete-other-windows . t)
    (no-other-window . t))
  "Window parameters of the sidebar's side window.")

(defun drover-list--display-alist (&optional place)
  "Return the action alist displaying the sidebar in a side window.
PLACE is a plist of :side, :slot and :width, the place the sidebar had
before it stepped aside (see `drover-list--place-sidebar'); without
one, the options say."
  `((side . ,(or (plist-get place :side) drover-list-side-window-side))
    (slot . ,(or (plist-get place :slot) drover-list-side-window-slot))
    (window-width . ,(or (plist-get place :width) drover-list-side-window-width))
    (preserve-size . (t . nil))
    ;; Strongly dedicated, so that `switch-to-buffer' never replaces it.
    (dedicated . t)
    (window-parameters . ,drover-list--side-window-parameters)))

(defun drover-list--show-side (&optional place)
  "Show the sidebar in a side window of the selected frame and return the window.
An ordinary window of the frame showing the sidebar's buffer stops
showing it, so that it is laid out for the side window's width; the
list of `drover-list' stays where it is.  PLACE, if any, is where the
sidebar was before it stepped aside (see `drover-list--display-alist')."
  (drover-start)
  (let ((buffer (drover-list--buffer t)))
    (dolist (window (get-buffer-window-list buffer nil (selected-frame)))
      (unless (window-parameter window 'window-side)
        (quit-window nil window)))
    (let ((window (or (drover-list--side-window)
                      (display-buffer-in-side-window buffer (drover-list--display-alist place))
                      (user-error "No room for the drover list"))))
      ;; Before drawing, which is for the width the fringes leave.
      (drover-list--set-up-side-window window)
      (drover-list--show window)
      window)))

;;;###autoload
(defun drover-list-side (&optional select)
  "Toggle the sidebar, a list of agent sessions in a side window.
Show it, without selecting it, when the selected frame lacks it, and
hide it otherwise.  With prefix argument SELECT, select it instead,
showing it first if needed; in the side window, select the window where
you work instead, so that the same key moves back and forth.  From the
side window, sessions open in the window where you work.  The sidebar
is a list of its own, beside the one `drover-list' shows, if any, which
folds nothing (see `drover-list-sidebar-mode'), in a side window
without fringes.

While the list of `drover-list' shows beside it, the sidebar steps
aside, and comes back once the list leaves (see
`drover-list--place-sidebar'); shown with this command meanwhile, it
stays beside the list, until the list leaves; hidden with it, it does
not come back."
  (interactive "P")
  (let ((window (drover-list--side-window))
        (aside (frame-parameter nil 'drover-list-sidebar-aside)))
    (cond (select
           (if (and window (eq window (selected-window)))
               (when-let* ((main (drover-list--main-window)))
                 (select-window main))
             (select-window (drover-list--show-side-asked aside))))
          (window (drover-list--hide-side window))
          (t (drover-list--show-side-asked aside)))))

(defun drover-list--show-side-asked (aside)
  "Show the sidebar as asked, where it was before stepping aside, ASIDE, if any.
Asked for beside the list, it stays there, until the list leaves."
  (set-frame-parameter nil 'drover-list-sidebar-aside nil)
  (let ((window (drover-list--show-side aside)))
    (set-frame-parameter nil 'drover-list-sidebar-kept
                         (drover-list--beside-p (window-frame window)
                                                (window-parameter window 'window-side)))
    window))

(defun drover-list--hide-side (window)
  "Delete WINDOW, the sidebar's, as asked: it does not come back by itself."
  (let ((frame (window-frame window)))
    (set-frame-parameter frame 'drover-list-sidebar-aside nil)
    (set-frame-parameter frame 'drover-list-sidebar-kept nil)
    (delete-window window)))

(defun drover-list-quit ()
  "Hide the list: delete its side window, or quit its ordinary window."
  (interactive nil drover-list-mode)
  (if (window-parameter nil 'window-side)
      (drover-list--hide-side (selected-window))
    (quit-window)))

;;;; The sidebar beside the list

;; The sidebar beside the list of `drover-list' shows the same sessions
;; twice, side by side: it steps aside while the list shows next to it,
;; and comes back once the list leaves, where it was.  Where it was is
;; a parameter of the frame, as is whether the user asked for it there.

(defun drover-list--beside-p (frame side)
  "Return non-nil when a window of FRAME shows the list at its SIDE.
That is a window, not a side window, showing the buffer of
`drover-list' whose SIDE edge is that of the frame's main window: the
sidebar, on SIDE, would be next to it."
  (when-let* ((buffer (get-buffer drover-list-buffer-name))
              (edge (pcase side ('left 0) ('top 1) ('right 2) ('bottom 3)))
              (main (window-main-window frame)))
    (let ((at (nth edge (window-pixel-edges main))))
      (and (seq-some (lambda (window)
                       (and (not (window-parameter window 'window-side))
                            (= (nth edge (window-pixel-edges window)) at)))
                     (get-buffer-window-list buffer 'nomini frame))
           t))))

(defun drover-list--place-sidebar (frame)
  "Have the sidebar of FRAME step aside next to the list, or come back.
The sidebar shown, the list beside it, its window goes, its place, side,
slot and width, kept in the frame's `drover-list-sidebar-aside'; once
no window shows the list beside where it was, as the list's window
shows another buffer, is deleted or moved, or the list is buried, it
comes back there.  Shown by `drover-list-side' beside the list, it stays
until the list leaves (`drover-list-sidebar-kept'); hidden by it, it no
longer comes back."
  (let ((side-window (drover-list--side-window frame))
        (aside (frame-parameter frame 'drover-list-sidebar-aside)))
    (cond
     (side-window
      (if (drover-list--beside-p frame (window-parameter side-window 'window-side))
          (unless (frame-parameter frame 'drover-list-sidebar-kept)
            (set-frame-parameter frame 'drover-list-sidebar-aside
                                 (list :side (window-parameter side-window 'window-side)
                                       :slot (window-parameter side-window 'window-slot)
                                       :width (window-total-width side-window)))
            (delete-window side-window))
        (set-frame-parameter frame 'drover-list-sidebar-kept nil)))
     (aside
      (unless (drover-list--beside-p frame (plist-get aside :side))
        (set-frame-parameter frame 'drover-list-sidebar-aside nil)
        (when (buffer-live-p (get-buffer drover-list-sidebar-buffer-name))
          (with-selected-frame frame
            (drover-list--show-side aside))))))))

(defvar drover-list--place-timer nil
  "Timer placing the sidebars once windows settled, or nil.")

(defun drover-list--place-sidebars ()
  "Have the sidebar of each frame step aside next to the list, or come back.
See `drover-list--place-sidebar'."
  (drover-with-reporting "sidebar place"
    (setq drover-list--place-timer nil)
    (when (get-buffer drover-list-sidebar-buffer-name)
      (dolist (frame (frame-list))
        (when (and (frame-live-p frame)
                   (or (frame-parameter frame 'drover-list-sidebar-aside)
                       (drover-list--side-window frame)))
          (drover-list--place-sidebar frame))))))

(defun drover-list--place-soon (&rest _)
  "Place the sidebars once the windows changing now settled.
For `window-buffer-change-functions' and
`window-configuration-change-hook', which run many times as windows
change: the sidebars are placed once, after."
  (drover-with-reporting "sidebar place"
    (unless (or (timerp drover-list--place-timer)
                (not (get-buffer drover-list-sidebar-buffer-name)))
      (setq drover-list--place-timer (run-at-time 0 nil #'drover-list--place-sidebars)))))

(defun drover-list-display ()
  "Show the session at point in another window, the list staying selected.
From a side window, that is the window where you work, where RET opens
it; from an ordinary window, another window, as
`drover-open-other-window' chooses it."
  (interactive nil drover-list-mode)
  (let ((session (drover-session-at-point)))
    (save-selected-window
      (drover-open session (not (drover-list--target-window))))
    (drover-list--restyle-titles)))

;;;; Moving between sessions

;; Commands of the dispatch menu's List group, which moves point in a
;; list from wherever the menu opened (see `drover--dispatch-navigate').
;; The list binds none of them: Magit's n, p, M-n and M-p move there.

(defun drover-list--check ()
  "Signal a `user-error' unless the current buffer is drover's list."
  (unless (derived-mode-p 'drover-list-mode)
    (user-error "Not in drover's list of sessions")))

(defun drover-list--reachable-sessions ()
  "Return the session sections of the current list point may reach, in order.
Those of collapsed groups are left out: their heading stands for them.
The sessions left for later past the first screens are drawn first."
  (drover-list--draw-undrawn)
  (mapcan (lambda (container)
            (unless (and (not (eq container magit-root-section)) (oref container hidden))
              (drover-list--session-sections container)))
          (drover-list--containers)))

(defun drover-list--step (sections current count)
  "Return the section COUNT steps away among SECTIONS, and whether it stops.
SECTIONS are in the order of the buffer, CURRENT the one of them point
is in, or nil.  A step goes to the next section, or back to the
previous one when COUNT is negative; from outside SECTIONS, the first
step goes to the first section after point, or the last before it.  The
value is (SECTION . CLAMPED), or nil without SECTIONS; CLAMPED is
non-nil when the steps would have left SECTIONS, SECTION being then the
first or the last."
  (when sections
    (let* ((point (point))
           (here (seq-position sections current #'eq))
           (index (cond (here (+ here count))
                        ((> count 0)
                         (+ (or (cl-position-if (lambda (section) (> (oref section start) point)) sections)
                                (length sections))
                            count -1))
                        (t
                         (+ (or (cl-position-if (lambda (section) (< (oref section start) point))
                                                sections :from-end t)
                                -1)
                            count 1))))
           (clamped (max 0 (min index (1- (length sections))))))
      (cons (nth clamped sections) (/= clamped index)))))

(defun drover-list-next-session (&optional count)
  "Move to the next session of the list, over group headings.
With COUNT, move that many sessions, backwards when negative.  Sessions
in collapsed groups are passed over."
  (interactive "p" drover-list-mode)
  (drover-list--check)
  (let ((count (or count 1)))
    (pcase (drover-list--step (drover-list--reachable-sessions)
                              (drover-list--session-section (magit-current-section))
                              count)
      ('nil (message "No session"))
      (`(,section . ,clamped)
       (goto-char (oref section start))
       (when clamped
         (message (if (< count 0) "First session" "Last session")))))))

(defun drover-list-previous-session (&optional count)
  "Move to the previous session of the list, over group headings.
With COUNT, move that many sessions, forwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list-next-session (- (or count 1))))

(defun drover-list-next-group (&optional count)
  "Move to the first session of the next group of the list.
The heading of a collapsed group stands for its sessions.  With COUNT,
move that many groups, backwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list--check)
  (let ((groups (drover-list--groups))
        (count (or count 1)))
    (unless groups
      (user-error "Sessions are not grouped"))
    (pcase-let ((`(,group . ,clamped)
                 (drover-list--step groups (drover-list--group-section (magit-current-section)) count)))
      (goto-char (oref (or (and (not (oref group hidden)) (car (drover-list--session-sections group)))
                           group)
                       start))
      (when clamped
        (message (if (< count 0) "First group" "Last group"))))))

(defun drover-list-previous-group (&optional count)
  "Move to the first session of the previous group of the list.
With COUNT, move that many groups, forwards when negative."
  (interactive "p" drover-list-mode)
  (drover-list-next-group (- (or count 1))))

(defun drover-list-first-session ()
  "Move to the first session of the list."
  (interactive nil drover-list-mode)
  (drover-list--check)
  (if-let* ((section (car (drover-list--reachable-sessions))))
      (goto-char (oref section start))
    (message "No session")))

(defun drover-list-last-session ()
  "Move to the last session of the list.
Sessions in collapsed groups are passed over."
  (interactive nil drover-list-mode)
  (drover-list--check)
  (if-let* ((section (car (last (drover-list--reachable-sessions)))))
      (goto-char (oref section start))
    (message "No session")))

;;;; Opening sessions by number

;; The digits open the sessions at the top of their trees, counted as
;; the list shows them, 0 the last: the list binds them, and so does
;; the dispatch menu's List group.  Subagents are not counted, so that
;; a number stays with its session as subagents come and go.

(defun drover-list--numbered-tree (number)
  "Return the tree whose top session is numbered NUMBER in the current list.
The top sessions of the trees are numbered from 1 in the order the list
shows them, but those of collapsed groups; NUMBER 0 is the last of them.
The value is (TREE . GROUP), GROUP the `drover-list--group' showing
TREE, or nil when no tree has that number."
  (when-let* ((model drover-list--model))
    (let ((left number)
          (last nil))
      (catch 'found
        (dolist (group (drover-list--model-groups model))
          (when-let* ((container (drover-list--group-container group))
                      ((or (eq container magit-root-section) (not (oref container hidden))))
                      (trees (drover-list--group-trees group))
                      (count (length trees))
                      ((> count 0)))
            (cond ((zerop number)
                   (setq last (cons (aref trees (1- count)) group)))
                  ((<= left count)
                   (throw 'found (cons (aref trees (1- left)) group)))
                  (t (setq left (- left count))))))
        last))))

(defun drover-list--numbered-section (number)
  "Return the section of the session numbered NUMBER in the current list, or nil.
See `drover-list--numbered-tree'.  Its group is drawn first when the
list left its rows for later."
  (when (and drover-list--model (drover-list--current-p))
    (pcase (drover-list--numbered-tree number)
      (`(,tree . ,group)
       (let ((row (car (drover-list--tree-shown tree))))
         (or (drover-list--row-section row)
             (progn (drover-list--draw-group group)
                    (drover-list--row-section row))))))))

(defun drover-list--typed-number ()
  "Return the digit of the key that ran the current command, as a number.
For a command not run by a digit, such as through \\[execute-extended-command]
or a click on its entry in a menu, read the number instead."
  (let ((event last-command-event))
    (if (and (characterp event) (<= ?0 event ?9))
        (- event ?0)
      (read-number "Open session number (0 for the last): "))))

(defun drover-list-open-numbered (number)
  "Open the session numbered NUMBER in the list, as RET there opens it.
The sessions at the top of their trees are numbered from 1, in the order
the list shows them; their subagents are not numbered, nor the sessions
of collapsed groups.  NUMBER 0 is the last of them.  Point goes to the
session's row, then the session opens: from a side window, in the
window where you work.  Interactively, NUMBER is the digit typed: the
list binds 1 to 9 and 0, as does the List group of `drover-dispatch'."
  (interactive (list (drover-list--typed-number)) drover-list-mode)
  (drover-list--check)
  (let ((section (or (drover-list--numbered-section number)
                     (if (zerop number)
                         (user-error "No session")
                       (user-error "No session numbered %d" number)))))
    (goto-char (oref section start))
    (drover-open-at-point)))

;;;; Reordering for display

;; Emacs lays out every line of a window showing a buffer that changed,
;; unless the selected window shows it: an animated glyph costs that
;; many times a second.  Reordering bidirectional text is a quarter of
;; it, and does nothing to a list without right-to-left text, as most
;; are.

(defconst drover-list--right-to-left "[\u202b\u202e\u2067]\\|\\cR"
  "What makes a list's lines need reordering for display.
Strong right-to-left characters (the category R: Hebrew, Arabic, and
the embeddings and overrides RLE and RLO), and the isolate RLI.")

(defvar-local drover-list--reordering-tick nil
  "The `buffer-chars-modified-tick' of the list when it was last looked at.
See `drover-list--reorder'.")

(defun drover-list--reorder (&optional _window)
  "Reorder the list's lines for display only while it holds right-to-left text.
Without such text, reordering changes nothing on the screen, and costs
redisplay a quarter of its time.  The text is looked at again once it
changed, not as the display of characters does, as animated glyphs do.
For `pre-redisplay-functions'."
  (drover-with-reporting "list display"
    (let ((tick (buffer-chars-modified-tick)))
      (unless (eql tick drover-list--reordering-tick)
        (setq drover-list--reordering-tick tick)
        (setq bidi-display-reordering
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (and (re-search-forward drover-list--right-to-left nil t) t))))))))

;;;; The mode

(defun drover-list--mode-line-process ()
  "Return how the list lists sessions, for the mode line.
It tells the grouping, the sort when not by urgency, which subagents
show when not the active ones, whether older and archived sessions
show, and whether a filter leaves sessions out."
  (concat (if drover-group-by
              (concat " by " (alist-get drover-group-by drover-grouping-labels))
            " ungrouped")
          (unless (and (eq drover-sort-by 'urgency) (not drover-sort-descending))
            (concat ", " (alist-get drover-sort-by drover-sort-labels)
                    (if drover-sort-descending " ↓" " ↑")))
          (pcase drover-show-subagents
            ('all " +subagents")
            ('none " -subagents"))
          (and drover-show-history " +older")
          (and drover-show-archived " +archived")
          (and (drover-filter-active-p) " filtered")))

;; The keys of `drover-session-map' come first, so they win over those
;; of `magit-section-mode-map' and its parent `special-mode-map'.  The
;; only one they share is g, which refreshes either way: `drover-refresh'
;; reverts the buffer, as `revert-buffer' would.  RET on a group heading
;; toggles it, through the group sections' own keymap.  Copying, M-w
;; wherever the user binds it, copies sessions rather than their lines.
;; The digits open sessions by number, rather than show Magit's levels
;; of sections, which mean little in a list two levels deep.
(defvar-keymap drover-list-mode-map
  :doc "Keymap of `drover-list-mode'."
  :parent (make-composed-keymap (list drover-session-map magit-section-mode-map))
  "RET" #'drover-open-at-point
  "C-o" #'drover-list-display
  "q" #'drover-list-quit
  "1" #'drover-list-open-numbered
  "2" #'drover-list-open-numbered
  "3" #'drover-list-open-numbered
  "4" #'drover-list-open-numbered
  "5" #'drover-list-open-numbered
  "6" #'drover-list-open-numbered
  "7" #'drover-list-open-numbered
  "8" #'drover-list-open-numbered
  "9" #'drover-list-open-numbered
  "0" #'drover-list-open-numbered
  "<remap> <kill-ring-save>" #'drover-list-copy)

(define-derived-mode drover-list-mode magit-section-mode "Drover"
  "Major mode of the list of agent sessions.
\\<drover-list-mode-map>
The list shows the sessions the filter keeps, each subagent under its
parent, grouped and sorted as the View menu, \\[drover-view], says.
\\[magit-section-toggle] collapses a group or shows the recap of a
session's last turn, \\[drover-open-at-point] opens the session at point and
\\[drover-list-display] shows it, the list staying selected.  The digits 1
to 9 open the first nine sessions at the top of their trees, 0 the last
one, subagents and collapsed groups not counted: see
`drover-list-open-numbered'.

What a line shows depends on the window's width: see
`drover-list-fields'.  In columns, the header line names them; hover
a name to read what its column shows.  The sidebar, shown by
`drover-list-side', is a list of its own in a side window, which may
show beside this one, in `drover-list-sidebar-mode', which folds
nothing: each keeps its point; from it, sessions open in the window
where you work.  The
colour of a title tells where its session is: in a buffer no window
shows, in a window, or in the window you work in, the faces
`opencode-session-open', `opencode-session-shown' and
`opencode-session-active'.  The keys of `drover-session-map'
act on the session at point; \\[drover-view] opens the View menu, whose
=u chooses which subagents show, and \\[drover-refresh]
redraws the list.  Text is drawn in the face
`drover-list'.

An active region selects the rows of sessions it touches, those shown,
as the list highlights them: a group's heading selects nothing, nor do
the sessions of a collapsed group.  Marking seen, interrupting,
archiving, unarchiving and deleting act on every selected session,
\\[drover-list-copy] copies their links, and the prompt quotes them
with their last replies (see `drover-prompt-dwim').

\\{drover-list-mode-map}"
  :group 'drover-list
  :interactive nil
  (setq-local drover-view-p t)
  (setq-local drover-list--visibility (make-hash-table :test #'equal))
  (setq-local drover-session-at-point-function #'drover-list--session-at-point)
  (setq-local drover-selected-sessions-function #'drover-list--selected-sessions)
  (setq-local drover-main-window-function #'drover-list--target-window)
  (setq-local revert-buffer-function #'drover-list--revert)
  (setq-local mode-line-process '(:eval (drover-list--mode-line-process)))
  (setq-local cursor-in-non-selected-windows nil)
  ;; Lines fit the window: point at the end of a full one must not
  ;; scroll the window sideways (see `drover-list--unscroll').
  (setq-local auto-hscroll-mode nil)
  ;; Magit's cache keys a session by its group too, and never forgets;
  ;; `drover-list--visibility' replaces it.
  (setq-local magit-section-cache-visibility nil)
  (drover-remap-default-face 'drover-list)
  (add-hook 'magit-section-set-visibility-hook #'drover-list--cached-visibility nil t)
  (add-hook 'window-buffer-change-functions #'drover-list--on-window-change nil t)
  (add-hook 'window-size-change-functions #'drover-list--on-window-change nil t)
  (add-hook 'text-scale-mode-hook #'drover-list--on-face-change nil t)
  (add-hook 'buffer-face-mode-hook #'drover-list--on-face-change nil t)
  (add-hook 'kill-buffer-hook #'drover-list--on-kill nil t)
  (add-hook 'change-major-mode-hook #'drover-list--on-kill nil t)
  (add-hook 'post-command-hook #'drover-list--prefetch nil t)
  (add-hook 'post-command-hook #'drover-list--draw-near-end nil t)
  (add-hook 'pre-redisplay-functions #'drover-list--reorder nil t)
  ;; Isearch finds the sessions of collapsed groups, and opens them.
  (add-hook 'isearch-mode-hook #'drover-list--draw-deferred nil t)
  (setq drover-list--live (append (remq (current-buffer) drover-list--live) (list (current-buffer))))
  (drover-list--watch))

;; Loaded over an older drover, the lists it made follow this one.
(when (drover-list--buffers)
  (drover-list--watch))

;;;; The sidebar

;; The sidebar shows a line per session, or two, beside your work: it
;; folds nothing, so that no key, no click and no indicator expands a
;; session or collapses a group, its lines need no invisible character
;; before them, and its window no fringes.

(defvar-keymap drover-list-sidebar-mode-map
  :doc "Keymap of `drover-list-sidebar-mode'.
The commands folding sections do nothing: the sidebar folds nothing.
A click on a session's row opens it, as RET does there; the mouse
drags no region, and a double or triple click does no more than the
click it starts with."
  :parent drover-list-mode-map
  "<mouse-1>" #'drover-list-sidebar-click
  "<down-mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<triple-mouse-1>" #'ignore
  "<remap> <magit-section-toggle>" #'ignore
  "<remap> <magit-section-cycle>" #'ignore
  "<remap> <magit-section-cycle-global>" #'ignore
  "<remap> <magit-mouse-toggle-section>" #'ignore
  "<remap> <drover-list-mouse-toggle-section>" #'ignore
  "<remap> <magit-section-show-level-1>" #'ignore
  "<remap> <magit-section-show-level-2>" #'ignore
  "<remap> <magit-section-show-level-3>" #'ignore
  "<remap> <magit-section-show-level-4>" #'ignore
  "<remap> <magit-section-show-level-1-all>" #'ignore
  "<remap> <magit-section-show-level-2-all>" #'ignore
  "<remap> <magit-section-show-level-3-all>" #'ignore
  "<remap> <magit-section-show-level-4-all>" #'ignore
  "<remap> <next-line>" #'drover-list-next-row
  "<remap> <previous-line>" #'drover-list-previous-row)

;; The sidebar shows no cursor: the highlight of the row at point tells
;; where it is, and point stays at the start of a row.  A column means
;; nothing there, and moving by lines took the column of the end of the
;; line: `posn-at-point' places a character displayed as a space at the
;; end of its line while `bidi-display-reordering' is nil, which
;; `drover-list--reorder' sets, and the sidebar's lines begin with one.

(defun drover-list--row-start-p (position)
  "Return non-nil when POSITION begins a row: a session's heading or a group's."
  (when-let* ((section (get-text-property position 'magit-section)))
    (and (not (eq section magit-root-section))
         (= position (oref section start)))))

(defun drover-list--next-row-start (position count)
  "Return the start of the COUNTth row after the one at POSITION, or nil.
Backwards for a negative COUNT; nil when there are not that many rows."
  (save-excursion
    (goto-char position)
    (let ((step (if (< count 0) -1 1))
          (left (abs count)))
      (while (and (> left 0)
                  (zerop (forward-line step))
                  (not (and (> step 0) (eobp))))
        (when (drover-list--row-start-p (point))
          (setq left (1- left))))
      (and (zerop left) (point)))))

(defun drover-list--row-start (position)
  "Return the start of the row of the sidebar at POSITION, or nil without rows.
That of the row a line of POSITION belongs to, else of the row above,
else of the first row."
  (save-excursion
    (goto-char position)
    (beginning-of-line)
    (if (drover-list--row-start-p (point))
        (point)
      (or (drover-list--next-row-start (point) -1)
          (drover-list--next-row-start (point) 1)))))

(defun drover-list-next-row (&optional count)
  "Move to the start of the next row of the sidebar, a session or a heading.
With COUNT, move that many rows, backwards when negative.  The sidebar
moves by rows rather than lines: point stays at the start of a row,
where the highlight shows it."
  (interactive "^p" drover-list-sidebar-mode)
  (let* ((count (or count 1))
         (start (or (drover-list--row-start (point)) (point))))
    (if-let* ((target (drover-list--next-row-start start count)))
        (goto-char target)
      (goto-char start)
      (signal (if (< count 0) 'beginning-of-buffer 'end-of-buffer) nil))))

(defun drover-list-previous-row (&optional count)
  "Move to the start of the previous row of the sidebar.
With COUNT, move that many rows, forwards when negative."
  (interactive "^p" drover-list-sidebar-mode)
  (drover-list-next-row (- (or count 1))))

(defun drover-list-sidebar-click (event)
  "Open the session of the row of the sidebar clicked, as RET does there.
The sidebar's window is selected and point goes to the row, so that the
session opens where RET at the row opens it, in the window where you
work.  A click on a group's heading, or below the rows, does nothing:
the sidebar folds nothing.  EVENT is the click."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (position (posn-point posn)))
    (when (and (window-live-p window) (null (posn-area posn)) (integerp position)
               (with-current-buffer (window-buffer window)
                 (and (derived-mode-p 'drover-list-sidebar-mode)
                      (save-excursion
                        (goto-char position)
                        (drover-session-at-point t)))))
      (select-window window)
      (goto-char (or (drover-list--row-start position) position))
      (drover-open-at-point))))

(defun drover-list--sidebar-settle ()
  "Bring point back to the start of its row of the sidebar, and the mark too.
After any command but those of a search going on: a click, a scroll or
a search ended leaves point anywhere in a row.  A region then goes from
the start of a row to the start of another, selecting both and those
between, as it shows.  For the sidebar's `post-command-hook'."
  (drover-with-reporting "sidebar point"
    (unless (bound-and-true-p isearch-mode)
      (when-let* ((start (drover-list--row-start (point))))
        (unless (= start (point))
          (goto-char start)))
      (when-let* ((mark (and mark-active (mark t)))
                  (start (drover-list--row-start mark)))
        (unless (= start mark)
          (set-marker (mark-marker) start))))))

;; The row at point stands out a little more than in Magit's buffers:
;; without a cursor, its highlight alone tells where point is.

(defface drover-list-sidebar-highlight '((t))
  "Face of the row at point in the sidebar, over `magit-section-highlight'.
What it leaves unspecified comes from `magit-section-highlight', its
background taken further from the default one, see
`drover-list-sidebar-highlight-contrast'."
  :group 'drover-list)

(defcustom drover-list-sidebar-highlight-contrast 1.4
  "How far the row at point stands out in the sidebar, from the default background.
Its background is that of `magit-section-highlight' this many times as
far from the default background, colour by colour, as the theme draws
them: 1 is Magit's own.  The face `drover-list-sidebar-highlight' sets it
otherwise."
  :type 'number
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'drover-list--rehighlight)
           (drover-list--rehighlight)))
  :group 'drover-list)

(defun drover-list--rehighlight ()
  "Draw the highlight of the row at point anew in every sidebar."
  (dolist (buffer (drover-list--buffers))
    (with-current-buffer buffer
      (when (derived-mode-p 'drover-list-sidebar-mode)
        (drover-list--remap-highlight)))))

(defvar-local drover-list--highlight-cookie nil
  "Cookie of the remapping of the highlight, see `drover-list--remap-highlight'.")

(defun drover-list--highlight-background ()
  "Return the background of the row at point in the sidebar, or nil.
That of `magit-section-highlight', taken
`drover-list-sidebar-highlight-contrast' times as far from the default
background; nil when either is not a colour."
  (let ((default (color-name-to-rgb (or (face-background 'default nil t) "")))
        (highlight (color-name-to-rgb (or (face-background 'magit-section-highlight nil t) ""))))
    (when (and default highlight)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (base color)
                                  (max 0.0 (min 1.0 (+ base (* drover-list-sidebar-highlight-contrast
                                                               (- color base))))))
                                default highlight)
                     '(2))))))

(defun drover-list--remap-highlight ()
  "Draw the highlight of the row at point in the current sidebar as the theme says.
Over `magit-section-highlight', the face `drover-list-sidebar-highlight',
then the background of `drover-list--highlight-background': a theme
enabled or disabled draws it anew (see `drover-list--theme-changed')."
  (when drover-list--highlight-cookie
    (face-remap-remove-relative drover-list--highlight-cookie))
  (setq drover-list--highlight-cookie
        (apply #'face-remap-add-relative 'magit-section-highlight 'drover-list-sidebar-highlight
               (when-let* ((background (drover-list--highlight-background)))
                 (list (list :background background))))))

(defun drover-list--sidebar-window-p (window)
  "Return non-nil when WINDOW is a side window showing the sidebar."
  (and (window-live-p window)
       (window-parameter window 'window-side)
       (with-current-buffer (window-buffer window)
         (derived-mode-p 'drover-list-sidebar-mode))))

(defun drover-list--set-up-side-window (window)
  "Give WINDOW, if a side window showing the sidebar, the sidebar's settings.
They are those `drover-list--display-alist' displays it with, which a
window shown by an older drover may lack: no fringes, strongly
dedicated, its width preserved and `drover-list--side-window-parameters'.
Only what differs is set, so that a window already set up does not
change.  The fringes are set on the window alone, not persistently: a
window showing another buffer, or the sidebar shown in an ordinary
window, takes the fringes of its buffer, as any window does."
  (when (drover-list--sidebar-window-p window)
    (unless (equal (seq-take (window-fringes window) 2) '(0 0))
      (set-window-fringes window 0 0))
    (unless (eq (window-dedicated-p window) t)
      (set-window-dedicated-p window t))
    (pcase-dolist (`(,parameter . ,value) drover-list--side-window-parameters)
      (unless (equal (window-parameter window parameter) value)
        (set-window-parameter window parameter value)))
    (unless (window-preserved-size window t)
      (window-preserve-size window t t))))

(defun drover-list--set-up-side-windows ()
  "Give the side windows showing the current list, if the sidebar, its settings.
As it draws, before measuring the width they leave (see
`drover-list--set-up-side-window')."
  (unless drover-list--folds
    (dolist (window (get-buffer-window-list nil 'nomini t))
      (drover-list--set-up-side-window window))))

(defun drover-list--sidebar-shown (window)
  "Give WINDOW the sidebar's settings as it shows the sidebar, if a side window.
For the sidebar's `window-buffer-change-functions'."
  (drover-with-reporting "sidebar window"
    (drover-list--set-up-side-window window)))

(define-derived-mode drover-list-sidebar-mode drover-list-mode "Drover"
  "Major mode of the sidebar, the list of agent sessions of a side window.
\\<drover-list-sidebar-mode-map>
It is `drover-list-mode' folding nothing: a session shows its line, or
two, without details, a group every session it holds, whatever
`drover-list-collapsed-groups' says, and \\[magit-section-toggle] does
nothing.  Its lines begin with their glyphs, and its side window has no
fringes.

It shows no cursor: the row at point stands out (see
`drover-list-sidebar-highlight-contrast'), and point stays at the start
of a row, \\[drover-list-next-row] and \\[drover-list-previous-row] moving by rows.  A click on a
session's row opens it, as \\[drover-open-at-point] does there; one on a group's heading
does nothing, and the mouse drags no region.

\\{drover-list-sidebar-mode-map}"
  :group 'drover-list
  :interactive nil
  (setq-local drover-list--folds nil)
  ;; Nothing folds: no section is hidden, nor tells it could be.
  (setq-local magit-section-visibility-indicators nil)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil)
  (drover-list--remap-highlight)
  (remove-hook 'magit-section-set-visibility-hook #'drover-list--cached-visibility t)
  (add-hook 'post-command-hook #'drover-list--sidebar-settle nil t)
  (add-hook 'window-buffer-change-functions #'drover-list--sidebar-shown nil t))

(defun drover-list-unload-function ()
  "Give Magit back its own selection, before `unload-feature' unloads the lists."
  (advice-remove 'magit-region-sections #'drover-list--region-advice)
  nil)

(provide 'drover-list)
;;; drover-list.el ends here
