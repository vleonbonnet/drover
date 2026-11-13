;;; drover-prompt.el --- One prompt for every agent session  -*- lexical-binding: t; -*-

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

;; The prompt: an Org buffer, `*drover*', where the user writes to an
;; agent, gathering context from the buffers worked in
;; (`drover-prompt-dwim'), or from drover's list the sessions selected,
;; quoted each on a line telling what the list knows of it
;; (`drover-prompt-quote-sessions'), then sends it to a running session
;; or a new one of any backend and engine (`drover-prompt-submit').  The
;; buffer is in `drover-prompt-mode', which defines drover's prompt
;; actions (see `drover-prompt-actions'): send, clear, attach,
;; copy-block, open-link, and the choice of agent, model, effort and
;; directory the next session runs.
;;
;; The prompt knows no backend: it sends through their prompt
;; operations (:create :check :apply-options :send :options :settable,
;; see `drover-register-backend') and resolves what a new session would
;; run through the engines (see `drover-register-engine').  With an
;; active region, only the region is sent, and the rest stays.
;;
;; Choices made in the prompt are explicit options: only they are
;; applied to a running session.  The header shows what a new session
;; would run, resolved by the engine that took a prompt last from the
;; defaults the prompt holds for each engine: those of slow engines are
;; fetched when the prompt starts or changes directory, those of
;; engines reading local files whenever the prompt gains context.  A
;; prompt set to go to a running session shows what that session runs
;; instead.  OpenCode's default model is the one chosen last for a
;; session, not the one a prompt went to last (see
;; `drover-opencode--resolve').
;;
;; Scheduling, Org links and the like belong to the user's
;; configuration, which uses `drover-prompt-content',
;; `drover-prompt-consume', `drover-prompt-read-target',
;; `drover-prompt-explicit-options', `drover-prompt-options-for' and
;; `drover-prompt-apply-options'.
;;
;; Front ends put a prompt of a session in it to be edited, such as one
;; withdrawn before delivery, with `drover-prompt-edit-for', which also
;; chooses that session as where the prompt goes: sending skips the
;; picker until the prompt is sent or cleared, or the session forgotten
;; (`drover-prompt-forget-target').
;; Its attachments come with it, each label carrying its own
;; (`drover-prompt-attachment-label'), and go with the prompt to the
;; sessions of their backend (see :send in `drover-register-backend').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'org)
(require 'org-element)
(require 'org-src)
(require 'drover-core)
(require 'drover-model)
(require 'drover-selection)
(require 'drover-ui)
(require 'opencode-look)
(require 'drover-keys)
(require 'drover-claude-code)
(require 'drover-codex)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 29))
  (load "drover-core" nil t))

(declare-function org-id-get-create "org-id" (&optional force))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function project-files "project" (project &optional dirs))

;;;; Options

(defface drover-prompt-attachment '((t :inherit org-code))
  "Face of the label of an attachment in the prompt.
See `drover-prompt-attachment-label'."
  :group 'drover)

(defcustom drover-prompt-buffer-name "*drover*"
  "Name of the prompt buffer."
  :type 'string
  :group 'drover)

(defcustom drover-prompt-display-action
  '((display-buffer-in-side-window) (side . bottom) (slot . 0) (window-height . 14))
  "How the prompt buffer is displayed, as an action of `display-buffer'.
A frame already showing the prompt is raised instead."
  :type 'sexp
  :group 'drover)

(defcustom drover-prompt-display-new t
  "Whether a session the prompt's send starts shows in a window.
Nil starts it in no window, as a task's sessions start, the windows
staying as they were but for the prompt's: the list and the mode line
follow it, and one of another host is not opened here."
  :type 'boolean
  :group 'drover)

(defcustom drover-prompt-mode-languages
  '((sh-mode . "bash") (bash-ts-mode . "bash") (lisp-interaction-mode . "emacs-lisp")
    (vterm-mode . "console") (eat-mode . "console") (ghostel-mode . "console")
    (agent-shell-mode . "markdown") (opencode-session-mode . "markdown"))
  "Languages of the code blocks quoting buffers of these major modes.
Each entry is (MODE . LANGUAGE).  Modes not listed take their name
without \"-mode\" or \"-ts-mode\"; see `drover-prompt-language'.
Terminal modes map to \"console\" because Org would otherwise start
them, and their process, to fontify the block."
  :type '(alist :key-type (symbol :tag "Major mode") :value-type (string :tag "Language"))
  :group 'drover)

(defcustom drover-prompt-new-targets
  '((:backend opencode :engine opencode :feature opencode :label "OpenCode session")
    (:backend claude-code-ide :engine claude-code :feature claude-code-ide
              :label "Claude Code IDE session")
    (:backend agent-shell :engine opencode :feature agent-shell :label "agent-shell session (OpenCode)")
    (:backend agent-shell :engine claude-code :feature agent-shell
              :label "agent-shell session (Claude Code)")
    (:backend agent-shell :engine codex :feature agent-shell :label "agent-shell session (Codex)"))
  "New sessions the prompt offers to send to, after the running ones.
Each entry is a plist: :backend, the backend starting the session with
its :create operation; :engine, the engine it runs, one of the
backend's :engines; :feature, the feature to require first; :label,
the candidate's text.  An entry shows when FEATURE is installed, as
`locate-library' finds it; picking it requires FEATURE, after which the
backend must be registered and run ENGINE."
  :type '(repeat (plist :options ((:backend symbol) (:engine symbol)
                                  (:feature symbol) (:label string))))
  :group 'drover)

(defcustom drover-prompt-default-engine 'opencode
  "Engine whose defaults the prompt shows before any engine took a prompt."
  :type 'symbol
  :group 'drover)

(defcustom drover-prompt-require-features '(opencode)
  "Features required, if installed, when a prompt starts.
Loading them registers their backends and engines, so the prompt knows
their defaults; opencode.el's loads drover's OpenCode backend."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-prompt-task-format "Work on %s. Investigate fully and provide a plan."
  "Prompt `drover-prompt-org-task' writes for an Org task.
%s stands for a link to the task."
  :type 'string
  :group 'drover)

(defcustom drover-prompt-task-agent "plan"
  "Agent `drover-prompt-org-task' picks for an Org task, or nil for none."
  :type '(choice (const :tag "None" nil) string)
  :group 'drover)

(defvar drover-prompt-org-headings-start-tasks nil
  "Whether `drover-prompt-dwim' on an Org heading started a task prompt.
It no longer does, whatever this says: `drover-prompt-dwim' always
quotes the line at point or the region, and `drover-prompt-org-task', n
in `drover-dispatch', starts a prompt for the task from anywhere in
its entry.")
(make-obsolete-variable
 'drover-prompt-org-headings-start-tasks
 "`drover-prompt-dwim' always quotes; `drover-prompt-org-task', n in `drover-dispatch', starts task prompts."
 "0.1.0")

(defvar drover-prompt-quote-timeout nil
  "Ignored: `drover-prompt-quote-sessions' no longer waits for replies.
It quotes each session on a line telling what the list knows of it.")
(make-obsolete-variable 'drover-prompt-quote-timeout
                        "sessions are quoted without their last replies." "0.2.0")

(defvar drover-prompt-quote-reply-lines nil
  "Ignored: `drover-prompt-quote-sessions' no longer quotes last replies.")
(make-obsolete-variable 'drover-prompt-quote-reply-lines
                        "sessions are quoted without their last replies." "0.2.0")

(defcustom drover-prompt-host-timeout 15
  "Seconds the prompt waits for another host to tell what it starts.
Its agents, models and defaults in a directory, or where a directory of
this host is there; \[keyboard-quit] stops waiting."
  :type 'number
  :group 'drover)

(defvar drover-prompt-quote-reply-characters nil
  "Ignored: `drover-prompt-quote-sessions' no longer quotes last replies.")
(make-obsolete-variable 'drover-prompt-quote-reply-characters
                        "sessions are quoted without their last replies." "0.2.0")

;;;; State of the prompt buffer

(defvar-local drover-prompt-picked-agent nil
  "Agent the user chose for the prompt, a string, or nil for the default.")

(defvar-local drover-prompt-picked-model nil
  "Model the user chose for the prompt, a `drover-model', or nil for the default.")

(defvar-local drover-prompt-picked-provider nil
  "Serving provider chosen for the next prompt.
Nil leaves the provider to the destination's defaults.")

(defvar-local drover-prompt-model-selection nil
  "Non-nil when the chosen model and effort are provider-independent choices.")

(defvar-local drover-prompt--sending nil
  "Non-nil while this prompt is waiting for a send to settle.")

(defvar-local drover-prompt-picked-variant nil
  "Variant the user chose for the prompt, a symbol, or nil for the default.")

(defvar-local drover-prompt-title nil
  "Title of the new session the prompt starts, or nil to let it choose.")

(defvar-local drover-prompt-directory-picked-p nil
  "Non-nil when the user chose the directory of the prompt.
The directory is the buffer's `default-directory'.  Unless chosen, the
prompt's directory follows the buffer it was started from.")

(defvar-local drover-prompt--default-directory nil
  "Directory the prompt inherited from the buffer it was started from.")

(defvar-local drover-prompt-host nil
  "Host a new session of the prompt starts on, a peer's name, or nil for this one.
Chosen in the target picker, with one of its new sessions, or with the
directory, \[universal-argument] before its key; its directory there is
`drover-prompt-host-directory', the buffer's `default-directory' staying
this host's, for its links and attachments.")

(defvar-local drover-prompt-host-directory nil
  "Directory of `drover-prompt-host' a new session of the prompt starts in.")

(defvar drover-prompt-context-functions nil
  "Functions naming where context quoted from a buffer comes from.
Each is called with no argument in the buffer, point where the context
was taken, and returns the line quoted context begins with, such as
\"From Slack message LINK\", or nil to let the next one tell.  For
buffers whose lines name nothing an agent can open, where the fallback
is \"From buffer NAME line N\"; a file's buffer, and a session's own,
are named by drover.")

(defvar drover-prompt-remote-functions nil
  "How the prompt starts sessions on other hosts, or nil when it cannot.
`drover-remote-mode' sets it, a plist of functions:

 :targets   Of an optional WAIT, seconds, returning the new sessions
            of other hosts offered, plists (:host NAME :host-label
            LABEL :backend BACKEND :engine ENGINE :label LABEL).
 :hosts     Returning the hosts starting sessions, as (NAME . LABEL).
 :label     Of a host's NAME, returning how views name it.
 :catalog   Of a host's NAME, a directory of this host LOCAL, one of
            NAME's DIRECTORY and a WAIT, seconds, returning what NAME
            told of what it starts there: :engines, an alist (ENGINE
            :agents :models :default), :where, LOCAL's counterpart.
 :default-directory
            Of a host's NAME, a directory of this host and a WAIT,
            returning the directory there a session of it starts in.
 :read-directory
            Of a host's NAME, a PROMPT and a DEFAULT, reading one of
            its directories.
 :start     Of a host's NAME, a TARGET, a DIRECTORY there, a TEXT, the
            start OPTIONS and a function LATER, starting the session
            there, waited for: returning its host's answer, nil when
            the wait was stopped, LATER getting it then, or signaling
            a `user-error'.
 :open      Of a host's NAME, that answer and HIDDEN, opening the
            session, unless HIDDEN is non-nil, and returning its
            record once listed, else nil.

The functions answer at once, asking a host what it has not told yet;
once a host answered, `drover-remote-mode' calls
`drover-prompt-hosts-told', so that a target picker open shows it.")

(defvar-local drover-prompt--defaults nil
  "Defaults data of the engines, as an alist (ENGINE . DEFAULTS).
DEFAULTS is what the engine's :defaults function delivered for the
prompt's directory (see `drover-register-engine').")

(defvar-local drover-prompt--last-engine nil
  "Engine that took a prompt last, as `drover-last-used-engine' told, or nil.")

(defvar-local drover-prompt--generations nil
  "Count of the defaults requests of each engine, as an alist (ENGINE . N).
Defaults answering an earlier request are dropped.")

(defvar-local drover-prompt--last-point nil
  "Marker at the point the user left the prompt at, while it was hidden.
Its insertion type is t, so context added there one block after the
other comes in order.")

(defvar-local drover-prompt--picker nil
  "Non-nil in the minibuffer of `drover-prompt-read-target'.")

(defvar-local drover-prompt--minibuffer-overlay nil
  "Overlay showing, in the target picker's minibuffer, what the prompt runs.")

(defvar-local drover-prompt--picker-update nil
  "Function making the target picker's candidates anew, in its minibuffer, or nil.
Called with no argument in the minibuffer, it reads the new sessions of
other hosts again, as hosts tell them, and takes the archived sessions
in while `drover-prompt--archived-shown' says so.")

(defvar-local drover-prompt--archived-offered nil
  "Non-nil where the target picker offers the archived sessions.
See `drover-prompt-toggle-archived'.")

(defvar-local drover-prompt--archived-shown nil
  "Non-nil while the target picker's minibuffer offers the archived sessions.")

(defvar-local drover-prompt--archived-serial nil
  "The `drover-fetched-serial' at which the picker last took archived sessions in.")

(defvar drover-prompt--pickers nil
  "The minibuffers of the target pickers open.
While one is, `drover-prompt--archived-changed' is on
`drover-changed-hook', and `drover-prompt--pickers-changed' on
`drover-models-changed-hook'.")

(defconst drover-prompt-archived-fetch-age 60
  "Seconds the archived sessions fetched serve the target picker as they are.
Shown there, they are fetched again from each backend whose last fetch
started longer ago, those kept meanwhile offered until they arrive.")

(defconst drover-prompt-archived-fetch-timeout 10
  "Seconds at most the target picker says archived sessions are being fetched.")

(defvar-local drover-prompt--chosen-session nil
  "Session the prompt goes to without the picker, or nil.
A plist (:key KEY :title TITLE :session SESSION): KEY is the session's
`drover-session-key', TITLE how the header names it, SESSION its
`drover-session' when chosen.  Set by `drover-prompt-edit-for'; see
`drover-prompt-read-target'.")

;;;; The mode

(defconst drover-prompt--actions
  '((send . drover-prompt-submit)
    (clear . drover-prompt-erase)
    (attach . drover-prompt-insert-attachment)
    (agent . drover-prompt-choose-agent)
    (model . drover-prompt-choose-model)
    (provider . drover-prompt-choose-provider)
    (effort . drover-prompt-choose-effort)
    (directory . drover-prompt-choose-directory)
    (copy-block . drover-prompt-copy-src-block)
    (open-link . org-open-at-point)
    (schedule . drover-schedule-prompt))
  "Prompt actions `drover-prompt-mode' defines, as (ACTION . COMMAND).")

(autoload 'drover-schedule-prompt "drover-schedule" nil t)

(defvar-keymap drover-prompt-mode-map
  :doc "Keymap of `drover-prompt-mode', empty: prompt keys go through `drover-dispatch'.")

(defun drover-prompt--remember-point ()
  "Remember point in `drover-prompt--last-point', from `post-command-hook'.
A window's point is lost with the window, so context added while the
prompt is hidden goes where the user last left point instead."
  (drover-with-reporting "prompt"
    (if (markerp drover-prompt--last-point)
        (set-marker drover-prompt--last-point (point))
      (setq drover-prompt--last-point (point-marker))
      (set-marker-insertion-type drover-prompt--last-point t))))

(define-minor-mode drover-prompt-mode
  "Minor mode of drover's prompt buffer.
It defines the prompt actions of the buffer (see `drover-prompt-actions'):
`send', `schedule', `clear', `attach', `copy-block', `open-link',
`agent', `model', `effort' and `directory', and remembers the point the
user leaves the buffer at.  Other packages define more actions from
`drover-prompt-mode-hook'.  It binds no key: the prompt keys are in
`drover-dispatch'."
  :lighter " Drover"
  :keymap drover-prompt-mode-map
  :group 'drover
  (if drover-prompt-mode
      (progn
        (pcase-dolist (`(,action . ,command) drover-prompt--actions)
          (drover-prompt-define action command))
        (add-hook 'post-command-hook #'drover-prompt--remember-point nil t))
    (pcase-dolist (`(,action . ,command) drover-prompt--actions)
      ;; Actions redefined since, by the user or another package, stay.
      (when (eq (alist-get action drover-prompt-actions) command)
        (drover-prompt-define action nil)))
    (remove-hook 'post-command-hook #'drover-prompt--remember-point t)))

;;;; Defaults of the engines

(defun drover-prompt--request-defaults (engine)
  "Ask ENGINE for its defaults for the directory of the current prompt.
The answer is kept when it answers the latest request of ENGINE, the
prompt buffer is still live and its directory unchanged."
  (when-let* ((function (drover-engine-property engine :defaults)))
    (let ((buffer (current-buffer))
          (directory default-directory)
          (generation (1+ (alist-get engine drover-prompt--generations 0))))
      (setf (alist-get engine drover-prompt--generations) generation)
      (drover-with-reporting "engine defaults"
        (funcall function directory
                 (lambda (defaults)
                   ;; Often called back from a process.
                   (drover-with-reporting "engine defaults"
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (and (eql (alist-get engine drover-prompt--generations) generation)
                                    (equal default-directory directory))
                           (setf (alist-get engine drover-prompt--defaults) defaults)
                           (force-mode-line-update)))))))))))

(defun drover-prompt--refresh-volatile ()
  "Read again which engine took a prompt last and the volatile engines' defaults.
Volatile engines read local files only, so this runs whenever the
prompt gains context or the target picker opens."
  (setq drover-prompt--last-engine (drover-last-used-engine))
  (dolist (engine (drover-engines))
    (when (drover-engine-property engine :volatile)
      (drover-prompt--request-defaults engine)))
  (force-mode-line-update))

(defun drover-prompt--schedule-refresh ()
  "Refresh the volatile defaults of the current prompt once the command returns.
So the prompt shows at once, its header following."
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (drover-with-reporting "prompt defaults"
                         (drover-prompt--refresh-volatile))))))))

(defun drover-prompt--fetch-defaults ()
  "Ask every engine for its defaults for the directory of the current prompt.
Engines that are not volatile may answer later, as their defaults come
from a service: the header follows when they do.  The volatile ones are
refreshed after the command.  The features of
`drover-prompt-require-features' are loaded first, so that their
engines are registered."
  (drover-prompt--schedule-refresh)
  (dolist (feature drover-prompt-require-features)
    (drover-with-reporting "prompt"
      (require feature nil t)))
  (dolist (engine (drover-engines))
    (unless (drover-engine-property engine :volatile)
      (drover-prompt--request-defaults engine))))

(defun drover-prompt--reset (directory)
  "Start the current prompt afresh, inheriting DIRECTORY.
Every choice is forgotten, the session chosen to send to included, and
the defaults are fetched again."
  (setq default-directory (file-name-as-directory (expand-file-name directory))
        drover-prompt--default-directory default-directory
        drover-prompt-directory-picked-p nil
        drover-prompt-host nil
        drover-prompt-host-directory nil
        drover-prompt-picked-agent nil
        drover-prompt-picked-model nil
        drover-prompt-picked-provider nil
        drover-prompt-model-selection nil
        drover-prompt-picked-variant nil
        drover-prompt-title nil
        drover-prompt--chosen-session nil)
  (drover-prompt--fetch-defaults))

(defun drover-prompt--begin (directory)
  "Begin a new prompt from DIRECTORY in the current, empty prompt buffer.
The agent, model, variant and directory chosen while the buffer was
empty stay, since they were chosen for this prompt; DIRECTORY is
inherited unless a directory was chosen.  A session chosen to send to
goes, having been chosen for the text that left."
  (setq drover-prompt-title nil
        drover-prompt--chosen-session nil)
  (if drover-prompt-directory-picked-p
      (drover-prompt--schedule-refresh)
    (setq default-directory (file-name-as-directory (expand-file-name directory))
          drover-prompt--default-directory default-directory)
    (drover-prompt--fetch-defaults)))

;;;; What the prompt runs

(defun drover-prompt--remote (key &rest arguments)
  "Call the function KEY of `drover-prompt-remote-functions' with ARGUMENTS.
Return nil when there is none, other hosts out of reach."
  (when-let* ((function (plist-get drover-prompt-remote-functions key)))
    (apply function arguments)))

(defun drover-prompt--host-label (host)
  "Return how views name HOST, a peer's name."
  (or (drover-prompt--remote :label host) host))

(defun drover-prompt--host-targets (host &optional wait)
  "Return the new sessions HOST starts, as `drover-prompt-remote-functions' tells.
A host not told yet is waited for WAIT seconds."
  (seq-filter (lambda (target) (equal (plist-get target :host) host))
              (drover-prompt--remote :targets wait)))

(defun drover-prompt--host-engines (host directory &optional wait)
  "Return what HOST told of its engines in DIRECTORY, there, as an alist.
Each entry is (ENGINE :agents NAMES :models MODELS :default OPTIONS);
nil until told, asked meanwhile, and waited for WAIT seconds."
  (and host directory
       (plist-get (drover-prompt--remote :catalog host nil directory wait) :engines)))

(defun drover-prompt--host-resolve (host directory engine agent model variant)
  "Return the resolved options a new session of ENGINE on HOST in DIRECTORY runs.
AGENT, MODEL and VARIANT are the explicit choices; what is not chosen
is what HOST told a session started without a choice runs, the variant
of a model chosen its default."
  (let* ((entry (alist-get engine (drover-prompt--host-engines host directory)))
         (default (plist-get entry :default))
         (options (list :agent agent :model model :variant variant
                        :provider drover-prompt-picked-provider :selection drover-prompt-model-selection)))
    (or (plist-get (drover-selection-resolve engine options default (plist-get entry :models) t) :effective)
        options)))

(defun drover-prompt--host-default-directory (host &optional buffer wait)
  "Return where a new session of the prompt BUFFER starts on HOST, or nil.
BUFFER's directory there, when HOST is its host, else what HOST's
directories map BUFFER's to (see `drover-prompt-remote-functions'),
told already or waited for WAIT seconds."
  (with-current-buffer (or buffer (current-buffer))
    (if (and drover-prompt-host-directory (equal drover-prompt-host host))
        drover-prompt-host-directory
      (drover-prompt--remote :default-directory host default-directory wait))))

(defun drover-prompt--runs-p (model engine)
  "Return non-nil when ENGINE can run MODEL, a `drover-model'.
It can when it has an identifier for it (see `drover-model-id-for')."
  (and (drover-model-id-for model engine) t))

(defun drover-prompt-explicit-options (&optional buffer)
  "Return the options chosen in the prompt BUFFER, as an options plist.
BUFFER defaults to the prompt buffer.  The plist is (:agent AGENT
:model MODEL :variant VARIANT), each nil when not chosen, all of them
without a prompt buffer.  Independent choices add :selection t and
an explicitly chosen serving provider adds :provider."
  (let ((buffer (or buffer (drover-prompt-buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (append (list :agent (and drover-prompt-picked-agent
                                    (not (string-empty-p drover-prompt-picked-agent))
                                    drover-prompt-picked-agent)
                        :model drover-prompt-picked-model
                        :variant drover-prompt-picked-variant)
                  (and drover-prompt-picked-provider (list :provider drover-prompt-picked-provider))
                  (and drover-prompt-model-selection '(:selection t))))
      (list :agent nil :model nil :variant nil))))

(defun drover-prompt--resolve (target current options &optional buffer)
  "Resolve OPTIONS for TARGET, whose running options are CURRENT.
TARGET is a session or a new-target plist.  BUFFER supplies defaults
and the directory for a new session.  Return the result of
`drover-selection-resolve', including its incompatibility reason.
The engine's models are read only when OPTIONS choose a model, a
provider or an effort, which `drover-selection-resolve' checks against
them: the target picker resolves every candidate, and Claude Code's
models, made from every live shell's, took 30 ms each time."
  (let* ((session (and (drover-session-p target) target))
         (engine (if session (drover-session-engine session) (plist-get target :engine)))
         (host (if session (drover-session-host session) (plist-get target :host)))
         (buffer (or buffer (drover-prompt-buffer)))
         (directory (or (and session (drover-session-directory session))
                        (and buffer (if host (drover-prompt--host-default-directory host buffer)
                                      (buffer-local-value 'default-directory buffer))) default-directory))
         (entry (and host (alist-get engine (drover-prompt--host-engines host directory))))
         (chosen (seq-some (lambda (key) (plist-get options key)) '(:model :provider :variant)))
         (models (cond (host (plist-get entry :models))
                       (chosen (drover--engine-models engine directory))))
         (defaults (and buffer (alist-get engine (buffer-local-value 'drover-prompt--defaults buffer))))
         (current (if session current
                    (if host (plist-get entry :default)
                      (drover-engine-resolve engine defaults (plist-get options :agent) nil nil))))
         (settable (if session
                       (and (drover-supports-p session :apply-options)
                            (if (drover-supports-p session :settable)
                                (drover-backend-call session :settable) '(:agent :model :variant)))
                     t))
         (resolved (drover-selection-resolve engine options current models settable)))
    (when (and (not session) (not host) (not (plist-get resolved :failure)))
      (let ((native (plist-get resolved :options)))
        (setq resolved (plist-put resolved :effective
                                  (drover-engine-resolve engine defaults (plist-get native :agent)
                                                         (plist-get native :model) (plist-get native :variant))))))
    resolved))

(defun drover-prompt-options-for (session options &optional _warn)
  "Return OPTIONS resolved for SESSION, or signal why they cannot apply.
The optional _WARN is accepted for callers of the former best-effort
filter.  Explicit choices are never discarded."
  (let* ((current (and (drover-supports-p session :options) (drover-backend-call session :options)))
         (resolved (drover-prompt--resolve session current options)))
    (when-let* ((failure (plist-get resolved :failure)))
      (user-error "%s: %s" (drover-session-display-title session) failure))
    (plist-get resolved :options)))

(defun drover-prompt--once (function)
  "Return a function of no argument calling FUNCTION the first time only."
  (let ((called nil))
    (lambda ()
      (unless called
        (setq called t)
        (funcall function)))))

(defun drover-prompt--apply (session options then &optional else)
  "Apply resolved OPTIONS to SESSION, then call THEN once on success.
ELSE receives a failure string; without ELSE a failure is reported."
  (let* ((apply (drover-backend-property session :apply-options))
         (settled nil)
         (success (lambda ()
                    (drover-with-reporting "prompt options"
                      (unless settled (setq settled t) (funcall then)))))
         (failure (lambda (reason)
                    (drover-with-reporting "prompt options"
                      (unless settled
                        (setq settled t)
                        (if else (funcall else reason)
                          (drover-warn "%s; prompt not sent" reason)))))))
    (if (not (functionp apply))
        (if (seq-some (lambda (key) (plist-get options key)) '(:agent :model :variant))
            (funcall failure "This session cannot apply prompt options")
          (funcall success))
      (condition-case err
          (funcall apply session options success failure)
        (error
         (funcall failure (format "Could not apply the options to %s: %s"
                                  (drover-session-display-title session) (drover-error-message err))))))))

(defun drover-prompt-apply-options (session options then &optional else)
  "Apply explicit OPTIONS to the running SESSION, then call THEN once.
OPTIONS are resolved by `drover-prompt-options-for'.  An incompatible
choice or a failed switch calls ELSE with a message, or reports the
failure when ELSE is nil, without calling THEN."
  (if (null session)
      (funcall then)
    (condition-case err
        (drover-prompt--apply session (drover-prompt-options-for session options t) then else)
      (error (if else (funcall else (drover-error-message err))
               (drover-warn "%s; prompt not sent" (drover-error-message err)))))))

(defun drover-prompt-engine-options (engine agent model variant &optional buffer)
  "Return the resolved options a new session of ENGINE would run.
AGENT, MODEL and VARIANT are the explicit choices; the engine resolves
them with the defaults the prompt BUFFER holds for it, BUFFER
defaulting to the current one.  See `drover-engine-resolve'."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (or model variant drover-prompt-picked-provider) drover-prompt-model-selection)
        (let* ((options (append (list :agent agent :model model :variant variant :selection t)
                                (and drover-prompt-picked-provider (list :provider drover-prompt-picked-provider))))
               (resolved (drover-prompt--resolve (list :engine engine) nil options (current-buffer))))
          (or (plist-get resolved :effective) options))
      (drover-engine-resolve engine (alist-get engine drover-prompt--defaults) agent model variant))))

(defun drover-prompt--display-engine (&optional buffer)
  "Return the engine whose new session the prompt BUFFER describes.
It is the engine that took a prompt last, else
`drover-prompt-default-engine', unless the model chosen cannot run
there: then the first engine that can.  BUFFER defaults to the current
one."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((last (or drover-prompt--last-engine drover-prompt-default-engine))
           (engines (and drover-prompt-picked-model
                         (drover-model-engines drover-prompt-picked-model)))
           ;; On another host, among the engines it starts.
           (there (and drover-prompt-host
                       (delete-dups (mapcar (lambda (target) (plist-get target :engine))
                                            (drover-prompt--host-targets drover-prompt-host)))))
           (runs (if (and there engines) (seq-filter (lambda (engine) (memq engine engines)) there) there)))
      (cond
       (there (if (memq last runs) last (or (car runs) (car there))))
       ((or (null engines) (memq last engines)) last)
       (t (car engines))))))

(defun drover-prompt--chosen-target (&optional buffer)
  "Return the running session the prompt BUFFER is set to go to, or nil.
BUFFER defaults to the current one.  The session is the one
`drover-prompt-edit-for' chose, as its backend tells it now when it
finds a session by identifier (:session), else as it was chosen; nil
when no session was chosen, or a session without a record."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((chosen drover-prompt--chosen-session)
                (session (plist-get chosen :session)))
      (or (and (drover-backend-property (drover-session-backend session) :session)
               (drover-find (drover-session-backend session) (drover-session-id session)))
          session))))

(defun drover-prompt--chosen-entry-options (session buffer)
  "Return what the running SESSION runs once sent the prompt BUFFER.
The value is an entry plist (see `drover-prompt--entry-options'),
from what SESSION's backend tells it runs now (:options), nil when it
tells nothing."
  (drover-prompt--entry-options session
                                (and (drover-supports-p session :options)
                                     (drover-backend-call session :options))
                                buffer))

(defun drover-prompt-inherited-options (&optional buffer)
  "Return the resolved options a session would run without any choice.
For a prompt `drover-prompt-edit-for' set to go to a running session,
they are what that session runs now: its model and variant, as its
backend tells them, the agent unknown.  Otherwise those of a new
session of the engine that took a prompt last, else of
`drover-prompt-default-engine', on another host what it told such a
session runs.  BUFFER is the prompt buffer, the current one by default.
The pickers' \"Default\" entry names them."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((drover-prompt--chosen-target)
      (let* ((session (drover-prompt--chosen-target))
             (current (and (drover-supports-p session :options)
                           (drover-backend-call session :options))))
        (list :agent nil :model (plist-get current :model) :variant (plist-get current :variant))))
     (drover-prompt-host
      (plist-get (alist-get (drover-prompt--display-engine)
                            (drover-prompt--host-engines drover-prompt-host drover-prompt-host-directory))
                 :default))
     (t
      (let ((engine (or drover-prompt--last-engine drover-prompt-default-engine)))
        (drover-engine-resolve engine (alist-get engine drover-prompt--defaults) nil nil nil))))))

(defun drover-prompt-effective-options (&optional buffer)
  "Return what the prompt BUFFER shows it runs, as a display plist.
BUFFER defaults to the current one.  The plist holds the resolved
:agent, :model and :variant, whether each was chosen
\(:agent-explicit-p, :model-explicit-p, :variant-explicit-p), the
:directory and whether it was chosen (:directory-explicit-p).  The
resolved values are for display only: only the choices are applied.
A prompt `drover-prompt-edit-for' set to go to a running session shows
what that session runs once sent it, as its entry in the target picker
does: its own model and variant, the choices applying to it; its agent
is the one chosen, else unknown.  Otherwise it shows what a new session
would run, on another host (`drover-prompt-host') what that host told:
its :directory is then the host's, named by :host-label."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((session (drover-prompt--chosen-target))
           (host (and (not session) drover-prompt-host))
           (entry (and session (drover-prompt--chosen-entry-options session (current-buffer))))
           (resolved (cond
                      (session nil)
                      (host (drover-prompt--host-resolve host drover-prompt-host-directory
                                                         (drover-prompt--display-engine)
                                                         drover-prompt-picked-agent
                                                         drover-prompt-picked-model
                                                         drover-prompt-picked-variant))
                      (t (drover-prompt-engine-options (drover-prompt--display-engine)
                                                       drover-prompt-picked-agent
                                                       drover-prompt-picked-model
                                                       drover-prompt-picked-variant)))))
      (list :directory (if host
                           drover-prompt-host-directory
                         (or default-directory drover-prompt--default-directory))
            :host-label (and host (drover-prompt--host-label host))
            :directory-explicit-p (or (and host t) drover-prompt-directory-picked-p)
            :agent (if session drover-prompt-picked-agent (plist-get resolved :agent))
            :agent-explicit-p (and drover-prompt-picked-agent t)
            :provider drover-prompt-picked-provider
            :provider-explicit-p (and drover-prompt-picked-provider t)
            :model (or (plist-get (if session entry resolved) :model) drover-prompt-picked-model)
            :model-explicit-p (if session
                                  (plist-get entry :model-explicit-p)
                                (and drover-prompt-picked-model t))
            :variant (or (plist-get (if session entry resolved) :variant) drover-prompt-picked-variant)
            :variant-explicit-p (if session
                                    (plist-get entry :variant-explicit-p)
                                  (and drover-prompt-picked-variant t))))))

(defun drover-prompt-option-display (value explicit-p)
  "Return VALUE as a string in the face of a chosen or an inherited setting.
The face is `opencode-setting-chosen' when EXPLICIT-P, else
`opencode-setting-inherited'."
  (propertize (format "%s" value)
              'face (if explicit-p 'opencode-setting-chosen 'opencode-setting-inherited)))

(defun drover-prompt--directory-label (options)
  "Return the directory of display OPTIONS as the header shows it, or nil.
One of another host's is named after its host, as that host spells it."
  (when-let* ((directory (plist-get options :directory)))
    (if-let* ((host (plist-get options :host-label)))
        (format "%s: %s" host directory)
      (abbreviate-file-name directory))))

(defun drover-prompt--header-option (value explicit-p)
  "Return VALUE in brackets for the header, its face telling EXPLICIT-P, or nil."
  (when value
    (concat " [" (drover-prompt-option-display value explicit-p) "]")))

(defun drover-prompt--minibuffer-option (label value explicit-p)
  "Return LABEL and VALUE for the target picker's minibuffer, or nil.
The face of VALUE tells EXPLICIT-P.  Nil when VALUE is."
  (when value
    (concat (propertize (format " [%s: " label) 'face 'minibuffer-prompt)
            (drover-prompt-option-display value explicit-p)
            (propertize "]" 'face 'minibuffer-prompt))))

;;;; The header

(defun drover-prompt--menu-key (command)
  "Return the key of COMMAND in `drover-dispatch', such as \"RET\", or nil."
  (when-let* ((suffix (ignore-errors (transient-get-suffix 'drover-dispatch command)))
              (key (plist-get (cdr suffix) :key)))
    (key-description (kbd key))))

(defvar-keymap drover-prompt-target-header-map
  :doc "Keys of the session the header names as where the prompt goes."
  "<header-line> <mouse-1>" #'drover-prompt-forget-target)

(defun drover-prompt--header-target ()
  "Return the header's part naming the session the prompt goes to, or nil.
A click on the session's name forgets it."
  (when-let* ((title (plist-get drover-prompt--chosen-session :title)))
    (concat "  |  to ["
            (propertize (string-replace "%" "%%" (format "%s" title))
                        'face 'opencode-setting-chosen
                        'mouse-face 'mode-line-highlight
                        'help-echo "mouse-1: forget it, and choose where the prompt goes when sent"
                        'keymap drover-prompt-target-header-map)
            "]")))

(defun drover-prompt--menu-send-target ()
  "Return the title of the session the prompt goes to, or nil.
For `drover-dispatch-send-target-functions': Send in the menu names the
session `drover-prompt-edit-for' chose, as the header does, in the
prompt and wherever Send sends the prompt (see `drover-prompt-target')."
  (when-let* ((buffer (if drover-prompt-mode (current-buffer) (drover-prompt-target 'send)))
              ((buffer-local-value 'drover-prompt-mode buffer))
              (title (plist-get (buffer-local-value 'drover-prompt--chosen-session buffer) :title)))
    (format "%s" title)))

(add-hook 'drover-dispatch-send-target-functions #'drover-prompt--menu-send-target)

(defun drover-prompt--header-line ()
  "Return the header line of the prompt: its keys and what it runs.
The keys are those of `drover-dispatch', after the name of its key (see
`drover-dispatch-key-description'); scheduling shows when the buffer
defines it.  The session chosen to send to, if any, follows them."
  (let* ((options (drover-prompt-effective-options))
         (dispatch (or (drover-dispatch-key-description t)
                       (substitute-command-keys "\\[drover-dispatch]")))
         (model (plist-get options :model))
         (variant (plist-get options :variant))
         (key (lambda (command label &optional prefix)
                (when-let* ((key (drover-prompt--menu-key command)))
                  (concat (and prefix (concat dispatch " ")) key " " label)))))
    (concat
     (mapconcat #'identity
                (delq nil (list (funcall key #'drover-prompt-send "send" t)
                                (and (drover-prompt-defines-p (current-buffer) 'schedule)
                                     (funcall key #'drover-prompt-schedule "schedule" t))
                                (funcall key #'drover-prompt-attach "attach" t)))
                "  ")
     (drover-prompt--header-target)
     "  |  "
     (funcall key #'drover-prompt-directory "cwd" t)
     (drover-prompt--header-option (drover-prompt--directory-label options)
                                   (plist-get options :directory-explicit-p))
     "  " (funcall key #'drover-prompt-agent "agent")
     (drover-prompt--header-option (plist-get options :agent) (plist-get options :agent-explicit-p))
     "  " (funcall key #'drover-prompt-model "model")
     (drover-prompt--header-option (and model (or (drover-model-name model) (drover-model-id model)))
                                   (plist-get options :model-explicit-p))
     "  " (funcall key #'drover-prompt-provider "provider")
     (drover-prompt--header-option (or (plist-get options :provider) (and model (drover-model-provider-name model)))
                                   (plist-get options :provider-explicit-p))
     "  " (funcall key #'drover-prompt-effort "effort")
     (drover-prompt--header-option (and variant (symbol-name variant))
                                   (plist-get options :variant-explicit-p)))))

;;;; The buffer

(defun drover-prompt-buffer (&optional create)
  "Return the prompt buffer, or nil when there is none.
With CREATE, create it when missing, in `org-mode' and
`drover-prompt-mode', its header line showing the keys and what a new
session would run."
  (let ((buffer (get-buffer drover-prompt-buffer-name)))
    (if (or buffer (not create))
        buffer
      (setq buffer (get-buffer-create drover-prompt-buffer-name))
      (with-current-buffer buffer
        (org-mode)
        (drover-prompt-mode 1)
        (setq header-line-format '(:eval (drover-prompt--header-line))))
      buffer)))

(defun drover-prompt--display (buffer)
  "Show the prompt BUFFER and select its window.
A frame already showing it is raised; otherwise BUFFER is displayed
with `drover-prompt-display-action'."
  (if-let* ((window (get-buffer-window buffer t)))
      (progn
        (select-frame-set-input-focus (window-frame window))
        (select-window window))
    (when-let* ((window (display-buffer buffer drover-prompt-display-action)))
      (select-window window))))

(defun drover-prompt-prepared-buffer ()
  "Return the prompt buffer when it holds a prompt to send, else nil.
It holds one when `drover-prompt-content' finds something to send: the
active region, else the buffer, has more than blanks.  It is
`drover-prepared-prompt-function', so that Send sends that prompt from
any buffer, as from the prompt, the prompt staying where it is."
  (when-let* ((buffer (drover-prompt-buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((region (use-region-p)))
        (save-excursion
          (goto-char (if region (region-beginning) (point-min)))
          ;; The blanks `string-trim' trims there.
          (and (re-search-forward "[^ \t\n\r]" (if region (region-end) (point-max)) t)
               buffer))))))

(setq drover-prepared-prompt-function #'drover-prompt-prepared-buffer)

;;;###autoload
(defun drover-prompt-default-buffer ()
  "Return the prompt buffer, creating it undisplayed when missing.
A new one begins a prompt in the current buffer's directory, as
`drover-prompt-toggle' would.  It is the default of
`drover-default-prompt-buffer-function', so the agent, model, effort
and directory actions choose what the next prompt runs from any
buffer."
  (or (drover-prompt-buffer)
      (let ((directory default-directory)
            (buffer (drover-prompt-buffer t)))
        (with-current-buffer buffer
          (drover-prompt--reset directory))
        buffer)))

(defun drover-prompt--ensure-blank-line ()
  "Make sure a blank line precedes point, unless point is at the beginning."
  (let ((before (buffer-substring-no-properties (max (point-min) (- (point) 2)) (point))))
    (cond ((bobp))
          ((string= before "\n\n"))
          ((string-suffix-p "\n" before) (insert "\n"))
          (t (insert "\n\n")))))

;;;###autoload
(defun drover-prompt-dwim ()
  "Open the prompt, or add to it the context at point.
The first call from a buffer shows the prompt, begun in that buffer's
directory, with a link to the line at point and the active region
quoted in a code block; later calls add such a block at the point the
user left the prompt at.  In drover's own lists, `*drover sessions*'
and the sidebar, the context is the sessions the region selects, else
the one at point, each linked on a line telling how it stands (see
`drover-prompt-quote-sessions'); everywhere else, in session buffers
and the lists of other front ends too, it is the line or the region.
An Org heading line is quoted as any other line:
`drover-prompt-org-task' starts a prompt for an Org task.  Signal a
`user-error' in the prompt buffer itself."
  (interactive)
  (when drover-prompt-mode
    (user-error "Already in the prompt"))
  (drover-prompt-add-context))

(defun drover-prompt-add-context (&optional text)
  "Open the prompt, or add to it the context at point.
As `drover-prompt-dwim' does, which refuses the prompt buffer first.
The context is the active region, else the line at point, whole even
where fields, such as a Comint prompt, divide it, formatted by
`drover-prompt-format-context'; TEXT, when given, is quoted in their
place, linked to the line at point as they are.  In drover's own lists,
those setting `drover-view-p', it is the sessions selected, or the one
at point, each on a line telling how it stands, by
`drover-prompt-quote-sessions'; a buffer telling a session at point,
such as a session buffer, or a list of another front end, gives its
line or region as any buffer does, a session's own buffer linked to the
session rather than to a line.  It goes at point in the prompt's
window, or while the prompt is hidden where the user left point."
  (if-let* ((drover-view-p)
            (sessions (drover-selected-sessions t)))
      (drover-prompt-quote-sessions sessions)
    (let* ((source (current-buffer))
           ;; The whole line: `line-beginning-position' stops at fields,
           ;; such as a Comint prompt's.
           (region (cond (text)
                         ((use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end)))
                         (t (buffer-substring-no-properties (pos-bol) (pos-eol)))))
           (block (drover-prompt-format-context source region (drover-prompt-language major-mode))))
      (drover-prompt--add block default-directory))))

(defun drover-prompt--add (text directory &optional quietly)
  "Add TEXT to the prompt, and show the prompt.
An empty prompt begins in DIRECTORY with TEXT.  Otherwise TEXT goes at
point in the prompt's window, or while the prompt is hidden where the
user left point, after a blank line.  The prompt's window is selected,
unless QUIETLY, as when TEXT arrives after the command that asked for
it: the user may be elsewhere by then."
  (let ((buffer (drover-prompt-buffer t)))
    (with-current-buffer buffer
      (if (zerop (buffer-size))
          (progn
            (drover-prompt--begin directory)
            (insert text))
        ;; Another engine may have taken a prompt since this one began.
        (drover-prompt--schedule-refresh)
        (if-let* ((window (get-buffer-window buffer)))
            (with-selected-window window
              (drover-prompt--ensure-blank-line)
              (insert text))
          (goto-char (if (markerp drover-prompt--last-point) drover-prompt--last-point (point-max)))
          (drover-prompt--ensure-blank-line)
          (insert text))))
    (if quietly
        (or (get-buffer-window buffer t)
            (display-buffer buffer drover-prompt-display-action))
      (drover-prompt--display buffer))))

;;;###autoload
(defun drover-prompt-toggle ()
  "Show or hide the prompt, adding nothing to it.
A shown prompt, or the current buffer when it is the prompt, is hidden;
otherwise the prompt is shown, created empty when missing.  An empty
prompt begins in the current buffer's directory."
  (interactive)
  (let* ((buffer (drover-prompt-buffer))
         (window (and buffer (get-buffer-window buffer t)))
         (directory default-directory))
    (cond
     ((or window (eq (current-buffer) buffer))
      (when window
        (delete-window window)))
     (buffer
      (with-current-buffer buffer
        (if (zerop (buffer-size))
            (drover-prompt--begin directory)
          (drover-prompt--schedule-refresh)))
      (drover-prompt--display buffer))
     (t
      (setq buffer (drover-prompt-buffer t))
      (with-current-buffer buffer
        (drover-prompt--reset directory))
      (drover-prompt--display buffer)))))

(defun drover-prompt--send-keys ()
  "Return the keys sending the prompt, such as \"C-\\=' RET\", or nil."
  (when-let* ((key (drover-prompt--menu-key #'drover-prompt-send)))
    (concat (or (drover-dispatch-key-description t) (substitute-command-keys "\\[drover-dispatch]"))
            " " key)))

;;;###autoload
(defun drover-prompt-edit-for (session text)
  "Put TEXT in the prompt, to be edited and sent to SESSION, and show it.
The prompt shows as `drover-prompt-toggle' shows it, point after TEXT.
An empty prompt begins in SESSION's directory with TEXT; a draft stays,
TEXT following it after a blank line, which the echo area tells.
SESSION becomes where the prompt goes: sending it, or scheduling it,
skips the picker, until it is sent, cleared, or the session forgotten
\(`drover-prompt-forget-target'); the header names it.  Nothing is sent.
Front ends call this to have a prompt edited in the prompt, as
drover's OpenCode backend does for the prompts opencode.el withdraws.
The labels of its attachments, made by `drover-prompt-attachment-label',
carry them in TEXT: they go with the prompt to sessions of their
backend."
  (let* ((buffer (drover-prompt-buffer t))
         (title (drover-session-display-title session))
         (draft nil)
         (end nil))
    (with-current-buffer buffer
      (setq draft (not (string-blank-p (buffer-string))))
      (if draft
          (progn
            (drover-prompt--schedule-refresh)
            (goto-char (point-max))
            (drover-prompt--ensure-blank-line))
        (erase-buffer)
        (drover-prompt--begin (or (drover-session-local-directory session) default-directory)))
      (insert text)
      (setq end (point)
            drover-prompt--chosen-session (list :key (drover-session-key session) :title title
                                                :session session))
      (force-mode-line-update))
    (drover-prompt--display buffer)
    (if-let* ((window (get-buffer-window buffer t)))
        (set-window-point window end)
      (with-current-buffer buffer (goto-char end)))
    (let ((keys (drover-prompt--send-keys)))
      (drover-message "%s%s"
                      (if draft "Added after the prompt being written; " "")
                      (if keys
                          (format "%s sends it to %s" keys title)
                        (format "it goes to %s" title))))))

;;;###autoload
(defun drover-prompt-send-to (session text &optional options then)
  "Send TEXT, in Org, to SESSION at once, as the prompt sends it.
The prompt is left as it is.  TEXT goes to the kill ring, then as
Markdown (see `drover-prompt-to-markdown') through SESSION's backend,
its checks first: when they cancel, or discard the text, nothing is
sent.  OPTIONS, an explicit options plist, are checked and applied
before sending, as the prompt's choices are.  A session drover archived
is restored first (see `drover-unarchive-to-send'); one that cannot be
signals a `user-error'.  Return non-nil when the checks allow delivery
to proceed.  THEN, a function of one argument, is called once the
backend took the text, after OPTIONS were applied, with non-nil when
SESSION has its turn, nil when the text waits on a request SESSION
awaits (see `drover-prompt--hands-off-p').  Front ends call this to send
a prompt of their own making, as a review of a session's changes does
\(see `drover-review')."
  (let* ((markdown (drover-prompt-to-markdown text))
         (target (drover-session-buffer session))
         (check (drover-backend-property session :check))
         (token (and (functionp check)
                     (funcall check session markdown
                              (and options (drover-prompt-options-for session options))))))
    (kill-new text)
    (pcase token
      ('cancel
       (message "Not sent to %s; the prompt is in the kill ring"
                (drover-session-display-title session))
       nil)
      ('discard
       (message "Discarded; the prompt is in the kill ring")
       nil)
      (_
       (drover-unarchive-to-send session)
       (cl-flet ((deliver ()
                   (when (and (drover-prompt--deliver session target markdown token) then)
                     (funcall then (drover-prompt--hands-off-p session token)))))
         (if options
             (drover-prompt-apply-options
              session options
              (lambda ()
                (drover-with-reporting "prompt after options"
                  (deliver))))
           (deliver)))
       t))))

(defun drover-prompt-forget-target ()
  "Forget the session the prompt goes to: sending it asks where again.
The session is the one `drover-prompt-edit-for' chose; the text stays."
  (interactive)
  (let ((buffer (drover-prompt--prompt-buffer)))
    (with-current-buffer buffer
      (if (not drover-prompt--chosen-session)
          (message "The prompt goes where the picker says when sent")
        (setq drover-prompt--chosen-session nil)
        (force-mode-line-update)
        (message "The prompt asks where it goes when sent")))))

;;;###autoload
(defun drover-prompt-org-task ()
  "Start a prompt for the Org task point is in, from anywhere in its entry.
The task is the heading point is on or under, found with the buffer
widened, so that an entry narrowed to its body, or folded, is found
too.  The prompt replaces the prompt's text, after asking when some is
being written: `drover-prompt-task-format' with an `id:' link to the
task, the task's file saved so that the link resolves.  It picks the
agent `drover-prompt-task-agent' and titles the new session after the
heading; picks made on an empty prompt stay, a replaced draft's go."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (when drover-prompt-mode
    (user-error "Already in the prompt"))
  (let ((task (org-with-wide-buffer
               (when (org-before-first-heading-p)
                 (user-error "Point is not under an Org heading"))
               (org-back-to-heading t)
               (point))))
    (let ((draft (drover-prompt-buffer)))
      (when (and draft
                 (not (string-blank-p (with-current-buffer draft (buffer-string))))
                 (not (y-or-n-p "Replace the prompt being written with this task? ")))
        (user-error "Prompt kept")))
    (require 'org-id)
    (let* ((id (org-with-wide-buffer
                (goto-char task)
                (org-id-get-create)))
           (heading (org-with-wide-buffer
                     (goto-char task)
                     (org-get-heading t t t t)))
           (text (format drover-prompt-task-format (format "[[id:%s][%s]]" id heading)))
           (directory default-directory)
           (buffer (drover-prompt-buffer t)))
      ;; The new ID must reach the file for the link to resolve.
      (when (and buffer-file-name (buffer-modified-p))
        (save-buffer))
      (with-current-buffer buffer
        (if (string-blank-p (buffer-string))
            (drover-prompt--begin directory)
          (drover-prompt--reset directory))
        (erase-buffer)
        (insert text)
        (setq drover-prompt-title heading)
        (when drover-prompt-task-agent
          (setq drover-prompt-picked-agent drover-prompt-task-agent)))
      (drover-prompt--display buffer))))

;;;; Quoting sessions

(declare-function drover-list-session-facts "drover-list" (session &optional now quote))

(defun drover-prompt-quote-sessions (sessions)
  "Add SESSIONS to the prompt, each on a line telling how it stands.
The links are made from the current buffer by `drover-session-link'.
See `drover-prompt-format-sessions' for the quote."
  ;; The facts are the list's, which quoting from a view has loaded.
  (require 'drover-list)
  (drover-prompt--add (drover-prompt-format-sessions sessions (mapcar #'drover-session-link sessions))
                      (or (drover-session-local-directory (car sessions)) default-directory))
  (message "Quoted %s" (if (cdr sessions)
                           (format "%d sessions" (length sessions))
                         (drover-session-display-title (car sessions)))))

(defun drover-prompt--quote-text (value)
  "Return VALUE, a string or nil, on one line without its properties."
  (string-trim (replace-regexp-in-string "[\n\r\t]+" " "
                                         (substring-no-properties (format "%s" (or value ""))))))

(defun drover-prompt--quote-status (session)
  "Return how SESSION stands, as a quote tells an agent."
  (let ((summary (drover-prompt--quote-text (drover-session-summary session))))
    (concat (pcase (drover-session-status session)
              ('waiting "waiting for an answer")
              (status (symbol-name status)))
            (and (not (string-empty-p summary)) (concat ": " summary)))))

(defun drover-prompt--quote-facts (session)
  "Return what the list knows of SESSION on one line, its facts labelled.
Such as \"Host: core; Branch: main; Age: 2h ago\", from
`drover-list-session-facts', or \"\" without any."
  (mapconcat (pcase-lambda (`(,_field ,label ,value ,_face))
               (concat label ": " (drover-prompt--quote-text value)))
             (drover-list-session-facts session nil t)
             "; "))

(defun drover-prompt-format-sessions (sessions links)
  "Return the Org text quoting SESSIONS for an agent to read.
LINKS are Org links to SESSIONS, in their order, nil for a session
without one.  A session is told on one line: its link, else its title,
how it stands, then what the list knows of it (see
`drover-prompt--quote-facts').  A session alone follows its heading on
its line; several are the items of a list under theirs."
  (cl-flet ((line (session link)
              (let ((facts (drover-prompt--quote-facts session)))
                (concat (or link (drover-prompt--quote-text (drover-session-display-title session)))
                        " — " (drover-prompt--quote-status session)
                        (and (not (string-empty-p facts)) (concat " — " facts))
                        "\n"))))
    (concat
     (if (cdr sessions)
         (concat "Agent sessions:\n"
                 (mapconcat (lambda (pair) (concat "- " (line (car pair) (cdr pair))))
                            (cl-mapcar #'cons sessions links)
                            ""))
       (concat "Agent session: " (line (car sessions) (car links))))
     "\n")))

;;;; Text

(defun drover-prompt-format-context (buffer region language)
  "Return the Org context of BUFFER at point: a link, then REGION quoted.
The link names the file and line at point; in the buffer of a session,
the session, by its link (see `drover-session-link'), else its title;
otherwise what `drover-prompt-context-functions' tell, else the buffer
and line.  REGION, a string or nil, goes in a code
block of LANGUAGE, escaped so that no line of it ends the block or opens
a heading, without the blank lines around it, and from a terminal
without trailing spaces."
  (with-current-buffer buffer
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (session (and (not file) (drover-prompt--buffer-own-session buffer)))
           (header (cond
                    (file (format "From [[file:%s::%d][%s:%d]]" file line (buffer-name) line))
                    (session
                     ;; Its lines are those of a rendering, which the
                     ;; agent cannot open: the session itself can be.
                     (format "From agent session %s"
                             (or (drover-session-link session)
                                 (format "\"%s\"" (replace-regexp-in-string
                                                   "[\n\r\t]+" " "
                                                   (format "%s" (drover-session-display-title session)))))))
                    ((drover-prompt--context-header))
                    (t (format "From buffer %s line %d" (buffer-name) line))))
           (terminal (derived-mode-p 'vterm-mode 'term-mode 'eat-mode 'ghostel-mode 'comint-mode))
           ;; Whitespace after the last newline goes too, as when a
           ;; selection runs into the next line's indentation.
           (region (and region
                        (replace-regexp-in-string "\\`\\([ \t]*\n\\)+\\|\\(\n[ \t]*\\)+\\'" "" region)))
           ;; Terminals pad lines to their width.
           (region (if (and region terminal)
                       (replace-regexp-in-string "[ \t]+$" "" region)
                     region)))
      (concat header
              (if region
                  (format ":\n#+begin_src %s\n%s\n#+end_src\n\n"
                          language (org-escape-code-in-string region))
                "\n\n")))))

(defun drover-prompt--context-header ()
  "Return the line `drover-prompt-context-functions' name this buffer by, or nil.
A function failing is reported and passed over; what one returns is
taken on one line."
  (when-let* ((header (run-hook-wrapped 'drover-prompt-context-functions
                                        (lambda (function)
                                          (let ((value (drover-with-reporting "context header"
                                                         (save-excursion (funcall function)))))
                                            (and (stringp value) (not (string-blank-p value)) value))))))
    (drover-bound-line (replace-regexp-in-string "[\n\r\t]+" " " header))))

(defun drover-prompt--buffer-own-session (buffer)
  "Return the session whose own buffer BUFFER is, or nil.
A list of another front end tells the session of the row at point too,
but its lines are not that session's conversation."
  (when-let* ((session (drover-buffer-session buffer))
              ((eq (drover-session-buffer session) buffer)))
    session))

(defun drover-prompt-language (mode)
  "Return the language of code blocks quoting a buffer in major MODE.
It is that of `drover-prompt-mode-languages', else MODE without
\"-ts-mode\" or \"-mode\"."
  (or (alist-get mode drover-prompt-mode-languages)
      (replace-regexp-in-string "\\(-ts\\)?-mode\\'" "" (symbol-name mode))))

(defun drover-prompt--fence (content)
  "Return a backtick fence longer than any run of backticks in CONTENT.
CommonMark closes a fence only on a run at least as long as the one
opening it, so nothing inside CONTENT ends it early."
  (let ((length 3)
        (start 0))
    (while (string-match "`+" content start)
      (setq length (max length (1+ (- (match-end 0) (match-beginning 0))))
            start (match-end 0)))
    (make-string length ?`)))

(defun drover-prompt-to-markdown (text)
  "Return Org TEXT with its code and example blocks as Markdown fences.
Block contents are unescaped and lose their common indentation, as an
Org export does, then are indented to the block's column, so a block
in a list item stays in it; a fence is longer than any run of
backticks in its content.  Everything else is kept as it is."
  (with-temp-buffer
    (insert text)
    ;; Org's way to parse a buffer: no hooks, no startup visibility,
    ;; indentation or numbering modes, whose properties would remain.
    (let ((org-mode-hook nil)
          (org-inhibit-startup t))
      (org-mode))
    ;; From the last block up, so that the positions of the others hold.
    (dolist (block (nreverse (org-element-map (org-element-parse-buffer)
                                 '(src-block example-block) #'identity)))
      (let* ((start (org-element-post-affiliated block))
             ;; The newline and blank lines after the block stay.
             (end (save-excursion
                    (goto-char (org-element-end block))
                    (skip-chars-backward " \t\n")
                    (point)))
             (pad (make-string (save-excursion (goto-char start) (current-indentation)) ?\s))
             (value (org-element-property :value block))
             (value (if (org-src-preserve-indentation-p block)
                        value
                      (org-remove-indentation value)))
             (value (if (or (string-empty-p value) (string-suffix-p "\n" value))
                        value
                      (concat value "\n")))
             (fence (drover-prompt--fence value))
             (markdown (concat fence (or (org-element-property :language block) "")
                               "\n" value fence)))
        (delete-region start end)
        (goto-char start)
        (insert (replace-regexp-in-string "^." (lambda (line) (concat pad line)) markdown t t))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun drover-prompt-attachment-label (label backend data)
  "Return LABEL as the label of an attachment of BACKEND, DATA, in the prompt.
LABEL is text, such as a file's name, which the prompt sends as text;
the attachment goes with it to a session of BACKEND, a backend's name,
whose :send receives DATA (see `drover-register-backend'), and to no
other: there the label is text alone.  The label keeps it as long as
any of it stays, deleting it whole drops it.  Front ends give the
labels of a prompt's attachments to `drover-prompt-edit-for' in its
text."
  (propertize label
              'drover-prompt-attachment (cons backend data)
              'font-lock-face 'drover-prompt-attachment
              'help-echo (format "Attached for %s sessions; deleting the label drops it"
                                 (drover-backend-label backend))))

(defun drover-prompt--attachments (start end)
  "Return the attachments of the labels between START and END, in order.
Each is (BACKEND . DATA), as `drover-prompt-attachment-label' made it."
  (let ((attachments nil)
        (pos start))
    (while (< pos end)
      (let ((attachment (get-text-property pos 'drover-prompt-attachment))
            (next (next-single-property-change pos 'drover-prompt-attachment nil end)))
        (when (and (consp attachment) (not (memq attachment attachments)))
          (push attachment attachments))
        (setq pos next)))
    (nreverse attachments)))

(defun drover-prompt-content (&optional buffer)
  "Return what the prompt BUFFER would send, or nil when it is blank.
BUFFER defaults to the prompt buffer.  The value is a plist: :org, the
Org text, trimmed; :text, it in Markdown; :start and :end, markers
around it in BUFFER; :region, non-nil when it is the active region
rather than the whole buffer; :attachments, those of its labels, each
\(BACKEND . DATA) (see `drover-prompt-attachment-label').  Nil too
without a prompt buffer."
  (let ((buffer (or buffer (drover-prompt-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((region (use-region-p))
               (start (if region (region-beginning) (point-min)))
               (end (if region (region-end) (point-max)))
               (org (string-trim (buffer-substring-no-properties start end))))
          (unless (string-empty-p org)
            (list :org org
                  :text (drover-prompt-to-markdown org)
                  :start (copy-marker start)
                  :end (copy-marker end)
                  :region (and region t)
                  :attachments (drover-prompt--attachments start end))))))))

(defun drover-prompt--hide (buffer)
  "Delete the windows showing the prompt BUFFER, but the last of a frame."
  (dolist (window (get-buffer-window-list buffer nil t))
    (when (and (window-live-p window) (eq (window-deletable-p window) t))
      (delete-window window))))

(defun drover-prompt--close (buffer)
  "Kill the prompt BUFFER and delete the windows that showed it.
A window that is the last of its frame stays."
  (drover-prompt--hide buffer)
  (kill-buffer buffer))

(defun drover-prompt-consume (content &optional buffer)
  "Remove CONTENT, as `drover-prompt-content' returned it, from the prompt BUFFER.
BUFFER defaults to the prompt buffer.  A region is deleted and the mark
deactivated, the buffer and its window kept, unless only blanks remain;
otherwise the prompt is done with: its buffer is killed and its window
deleted.  The session chosen to send to, if any, is forgotten either
way."
  (let ((buffer (or buffer (drover-prompt-buffer))))
    (when (buffer-live-p buffer)
      ;; The session chosen to send to was for what left.
      (with-current-buffer buffer
        (when drover-prompt--chosen-session
          (setq drover-prompt--chosen-session nil)
          (force-mode-line-update)))
      (unless (and (plist-get content :region)
                   (with-current-buffer buffer
                     (delete-region (plist-get content :start) (plist-get content :end))
                     (deactivate-mark)
                     (not (string-blank-p (buffer-string)))))
        (drover-prompt--close buffer)))))

;;;; Prompt actions

(defun drover-prompt--prompt-buffer ()
  "Return the prompt buffer a command acts on: the current one, else the prompt.
Signal a `user-error' when there is none."
  (cond (drover-prompt-mode (current-buffer))
        ((drover-prompt-buffer))
        (t (user-error "No prompt"))))

(defun drover-prompt--deliver (session buffer text token &optional attachments)
  "Send TEXT with TOKEN through the backend of SESSION.
BUFFER is the live buffer SESSION had when it was chosen, or nil for a
new session; a SESSION whose BUFFER died meanwhile is reported, and so
is a backend failing to send.  ATTACHMENTS, the prompt's, each
\(BACKEND . DATA), go with it when of SESSION's backend, given to its
:send; the others cannot, which is told, their labels going as text.
This may run from a process filter or a timer, so it signals nothing.
Return non-nil once the backend took TEXT."
  (let* ((title (drover-session-display-title session))
         (send (drover-backend-property session :send))
         (backend (drover-session-backend session))
         (own (mapcar #'cdr (seq-filter (lambda (attachment) (eq (car attachment) backend))
                                        attachments)))
         (others (seq-remove (lambda (attachment) (eq (car attachment) backend)) attachments)))
    (cond
     ((and buffer (not (buffer-live-p buffer)))
      (drover-warn "%s died before the prompt was sent; it is in the kill ring" title))
     ((not (functionp send))
      (drover-warn "%s takes no prompt; it is in the kill ring" title))
     (t
      (when others
        (drover-warn "%d attachment%s of %s went to %s as text only"
                     (length others) (if (cdr others) "s" "")
                     (drover-backend-label (car (car others))) title))
      (condition-case err
          (progn
            (save-current-buffer
              (if own
                  (funcall send session text token own)
                (funcall send session text token)))
            t)
        (error
         (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                      title (drover-error-message err))
         nil))))))

(defun drover-prompt--sent (session new)
  "Note that the prompt's send handed SESSION its turn, NEW when it started it.
See `drover-handed-off-functions', which run once the command ended:
the prompt's window goes, and a new session shows, as it ends.  Other
sends, such as `drover-prompt-send-to', a scheduled prompt's or a
task's, note nothing: the user did not send them now."
  (drover-note-handed-off session (if new 'start 'prompt) 'drover))

(defun drover-prompt--hands-off-p (session token)
  "Return non-nil when a prompt sent to SESSION with TOKEN hands SESSION its turn.
TOKEN is what SESSION's :check returned.  The prompt does, unless the
backend says it waits on a request SESSION awaits (see :waits in
`drover-register-backend')."
  (not (and (drover-supports-p session :waits)
            (drover-with-reporting "backend waits"
              (drover-backend-call session :waits token)))))

(defun drover-prompt--send-to-session (session content buffer)
  "Send CONTENT of the prompt BUFFER to the running SESSION.
Its backend's checks run first, for the options about to be applied:
when they cancel, nothing is sent and the prompt stays; when they
discard the prompt, nothing is sent and CONTENT leaves the prompt, in
the kill ring.  A session drover archived is restored then (see
`drover-unarchive-to-send'): when it cannot be, a `user-error' says
why, nothing is sent and the prompt stays.  Otherwise the options are
applied, the text sent, and CONTENT leaves the prompt; SESSION has its
turn then, unless the prompt waits on a request SESSION awaits (see
`drover-prompt--hands-off-p')."
  (let* ((text (plist-get content :text))
         (target (drover-session-buffer session))
         (explicit (drover-prompt-explicit-options buffer))
         (check (drover-backend-property session :check))
         (token (and (functionp check)
                     (funcall check session text (drover-prompt-options-for session explicit)))))
    (pcase token
      ('cancel
       (message "Not sent to %s; the prompt is in the kill ring"
                (drover-session-display-title session)))
      ('discard
       (drover-prompt-consume content buffer)
       (message "Discarded; the prompt is in the kill ring"))
      (_
       (drover-unarchive-to-send session)
       (let ((finish (drover-prompt--begin-send buffer))
             (start (and (buffer-live-p target)
                         (with-current-buffer target (without-restriction (point-max))))))
         (drover-prompt-apply-options
          session explicit
          (lambda ()
            (unwind-protect
                (when (drover-prompt--deliver session target text token (plist-get content :attachments))
                  (when (drover-prompt--unchanged-p content buffer)
                    (drover-prompt-consume content buffer))
                  (when (drover-prompt--hands-off-p session token)
                    (drover-prompt--sent session nil)))
              (funcall finish)))
          (lambda (reason)
            (funcall finish)
            (drover-warn "%s; prompt not sent" reason)))
         ;; Everything before START was said before this prompt.
         (drover-note-sent-text session text target start))))))

(defun drover-prompt--begin-send (buffer)
  "Mark prompt BUFFER as sending and return a function clearing that state."
  (with-current-buffer buffer
    (when drover-prompt--sending (user-error "This prompt is already being sent"))
    (let ((token (list t)))
      (setq drover-prompt--sending token)
      (lambda ()
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq drover-prompt--sending token)
              (setq drover-prompt--sending nil))))))))

(defun drover-prompt--hiding (hidden function)
  "Call FUNCTION and return its value, in no new window when HIDDEN is non-nil.
HIDDEN shows the buffers FUNCTION displays in no window, and restores
the windows it changes otherwise, as a task's hidden session starts."
  (if (not hidden)
      (funcall function)
    (save-window-excursion
      (let ((display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t))))
        (funcall function)))))

(defun drover-prompt--send-to-new (target content buffer)
  "Start the new session TARGET describes and send it CONTENT of the prompt BUFFER.
TARGET is an entry of `drover-prompt-new-targets'.  Its feature is
loaded; its backend, which must be registered and run its engine,
starts the session in BUFFER's directory with the options chosen there,
in no window when `drover-prompt-display-new' is nil.  CONTENT leaves
the prompt once delivered, if it is still unchanged."
  (let* ((backend (plist-get target :backend))
         (engine (plist-get target :engine))
         (feature (plist-get target :feature))
         (label (or (plist-get target :label)
                    (format "%s session" (drover-engine-label engine))))
         (hidden (not drover-prompt-display-new)))
    (when feature
      (require feature))
    (let ((create (drover-backend-property backend :create)))
      (unless (and (memq backend (drover-backends))
                   (functionp create)
                   (memq engine (drover-backend-engines backend)))
        (user-error "%s starts no %s session"
                    (drover-backend-label backend) (drover-engine-label engine)))
      (let* ((resolved (drover-prompt--resolve target nil (drover-prompt-explicit-options buffer) buffer))
             (_ (when-let* ((why (plist-get resolved :failure))) (user-error "%s" why)))
             (options (with-current-buffer buffer
                        (append (plist-get resolved :options)
                                (list :title drover-prompt-title
                                      :defaults (alist-get engine drover-prompt--defaults))
                                (and hidden (list :hidden t)))))
             (text (plist-get content :text))
             (settled nil)
             (finish (drover-prompt--begin-send buffer))
             ;; Asked now, in the command; what is accepted applies once the session started.
             (started (drover-note-sent-text nil text)))
        (condition-case err
            (drover-prompt--hiding
             hidden
             (lambda ()
               (funcall create (buffer-local-value 'default-directory buffer) engine options
                        (lambda (session)
                          ;; THEN may come later, from a process or a timer.
                          (unless settled
                            (setq settled t)
                            (funcall finish)
                            (when (drover-prompt--hiding
                                   hidden
                                   (lambda ()
                                     (drover-prompt--deliver session nil text nil (plist-get content :attachments))))
                              (when (drover-prompt--unchanged-p content buffer)
                                (drover-prompt-consume content buffer))
                              (drover-prompt--sent session t)
                              (funcall started session))))
                        (lambda (reason)
                          (unless settled
                            (setq settled t)
                            (funcall finish)
                            (drover-warn "No new %s: %s; the prompt stays" label reason))))))
          ((error quit) (funcall finish) (signal (car err) (cdr err))))))))

(defun drover-prompt--unchanged-p (content buffer)
  "Return non-nil when the prompt BUFFER still holds CONTENT as it was taken."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (or (not (plist-get content :region))
                  (<= (point-min) (plist-get content :start) (plist-get content :end) (point-max)))
              (equal (string-trim (if (plist-get content :region)
                                      (buffer-substring-no-properties (plist-get content :start) (plist-get content :end))
                                    (buffer-substring-no-properties (point-min) (point-max))))
                     (plist-get content :org))))))

(defun drover-prompt--host-offers-p (host directory engine model)
  "Return non-nil unless HOST told it runs no MODEL with ENGINE in DIRECTORY.
Its catalogs not told, it may."
  (let ((entry (alist-get engine (drover-prompt--host-engines host directory))))
    (or (null entry)
        (null (plist-get entry :models))
        (and (seq-find (lambda (offered) (equal (drover-model-key offered) (drover-model-key model)))
                       (plist-get entry :models))
             t))))

(defun drover-prompt--send-to-host (target content buffer)
  "Start on another host the session TARGET describes, sending it CONTENT.
CONTENT is the prompt BUFFER's.  TARGET is a new session of another
host the target picker offers.  It starts in BUFFER's directory there,
read first unless BUFFER's new sessions start on that host already
\(`drover-prompt-host'), defaulting to what that host's directories map
BUFFER's `default-directory' to; the host and directory become
BUFFER's, kept for the next prompt as a directory chosen is.  The host
starts it through its own backend, with the options chosen in BUFFER
and CONTENT's text, waited for: CONTENT then leaves the prompt and the
session opens here, an OpenCode session in a full buffer through its
host, a terminal or a shell mirrored, unless `drover-prompt-display-new'
is nil.  A model, provider or effort chosen is resolved against what
the host offers in that directory, waited for
`drover-prompt-host-timeout' seconds at most when not told yet.  When it
does not start, a `user-error' says why and the prompt
stays.  Attachments, files of this host, go as their labels in the text."
  (let* ((host (plist-get target :host))
         (label (or (plist-get target :host-label) (drover-prompt--host-label host)))
         (engine (plist-get target :engine))
         (what (format "%s on %s" (plist-get target :label) label))
         (directory (or (and (equal (buffer-local-value 'drover-prompt-host buffer) host)
                             (buffer-local-value 'drover-prompt-host-directory buffer))
                        (drover-prompt--remote
                         :read-directory host (format "Start a new %s in: " what)
                         (drover-prompt--host-default-directory host buffer drover-prompt-host-timeout)))))
    (with-current-buffer buffer
      (setq drover-prompt-host host
            drover-prompt-host-directory directory)
      (force-mode-line-update))
    ;; A model, provider or effort chosen is resolved against what the
    ;; host offers in that directory, which it may not have told yet.
    (when (and (seq-some (lambda (key) (plist-get (drover-prompt-explicit-options buffer) key))
                         '(:model :provider :variant))
               (not (drover-prompt--host-engines host directory drover-prompt-host-timeout)))
      (user-error "%s did not tell its models in %s; the prompt stays" label directory))
    (let* ((resolved (drover-prompt--resolve target nil (drover-prompt-explicit-options buffer) buffer))
           (_ (when-let* ((why (plist-get resolved :failure))) (user-error "%s" why)))
           (options (append (plist-get resolved :options)
                            (list :title (buffer-local-value 'drover-prompt-title buffer))))
           (model (plist-get options :model))
           (attachments (plist-get content :attachments)))
      (when model
        (unless (drover-prompt--runs-p model engine)
          (user-error "The %s runs no %s; choose another model" what (drover-model-label model)))
        (unless (drover-prompt--host-offers-p host directory engine model)
          (user-error "%s offers no %s to %s there; choose another model"
                      label (drover-model-label model) (drover-engine-label engine))))
      (when attachments
        (drover-warn "%d attachment%s went to %s as text only"
                     (length attachments) (if (cdr attachments) "s" "") label))
      (let ((finish (drover-prompt--begin-send buffer))
            (settled nil))
        (condition-case err
            (when-let* ((result (drover-prompt--remote
                                 :start host target directory (plist-get content :text) options
                                 (lambda (result)
                                   (drover-with-reporting "remote prompt"
                                     (unless settled
                                       (setq settled t)
                                       (funcall finish)
                                       (if-let* ((why (plist-get result :failure)))
                                           (drover-warn "No new %s: %s; the prompt stays" what why)
                                         (when (drover-prompt--unchanged-p content buffer)
                                           (drover-prompt-consume content buffer))
                                         (drover-message "%s started %s" label
                                                         (or (plist-get result :title) "a session")))))))))
              (unless settled
                (setq settled t)
                (funcall finish)
                (when (drover-prompt--unchanged-p content buffer)
                  (drover-prompt-consume content buffer))
                (when-let* ((record (drover-prompt--remote :open host result (not drover-prompt-display-new))))
                  (drover-prompt--sent record t))))
          ((error quit) (funcall finish) (signal (car err) (cdr err))))))))

(defun drover-prompt-submit ()
  "Send the prompt to a session chosen among the running ones and new ones.
In the picker, \\<drover-prompt-archived-map>\\[drover-prompt-toggle-archived] offers the archived sessions too, which it
restores first (see `drover-prompt-toggle-archived').
With an active region, only the region is sent.  The text, in Org, is
put in the kill ring, then converted to Markdown.  To a running session,
the options chosen in the prompt must all apply first; to a new
session, they are its
start options.  The text sent leaves the prompt, unless nothing was
sent: a check the user cancelled keeps it, region included, one the
user told to discard it removes it all the same.  This is
the prompt action `send'."
  (interactive)
  (let* ((buffer (drover-prompt--prompt-buffer))
         (_ (when (buffer-local-value 'drover-prompt--sending buffer)
              (user-error "This prompt is already being sent")))
         (content (or (drover-prompt-content buffer)
                      (user-error "Prompt is empty"))))
    (kill-new (plist-get content :org))
    (let ((target (with-current-buffer buffer
                    (drover-prompt-read-target "Send to session: " :archived t))))
      (cond
       ((drover-session-p target) (drover-prompt--send-to-session target content buffer))
       ((plist-get target :host) (drover-prompt--send-to-host target content buffer))
       (t (drover-prompt--send-to-new target content buffer))))))

(defun drover-prompt-erase ()
  "Erase the text of the prompt, region or not, and close its window.
The buffer and the agent, model, effort and directory chosen stay, for
the next prompt, which `drover-prompt-toggle' shows again; the session
chosen to send to goes with the text.  A window that is the last of its
frame stays.  This is the prompt action `clear'."
  (interactive)
  (let ((buffer (drover-prompt--prompt-buffer)))
    (with-current-buffer buffer
      (deactivate-mark)
      (erase-buffer)
      (setq drover-prompt--chosen-session nil))
    (drover-prompt--hide buffer)))

(defun drover-prompt--attach-candidates (buffer)
  "Return the buffers and project files `drover-prompt-insert-attachment' offers.
Buffers come first, most recently used first, without the prompt
BUFFER, minibuffers and internal buffers; files are those of the
project of BUFFER's directory, named from the project directory's own
name.  Each candidate carries its (TYPE . OBJECT) as its
`drover-prompt-attach' property, TYPE being `buffer' or `file'; a file
named like a buffer is marked as a file."
  (require 'project)
  (let* ((buffers (delq nil (mapcar (lambda (candidate)
                                      (let ((name (buffer-name candidate)))
                                        (unless (or (eq candidate buffer) (minibufferp candidate)
                                                    (string-prefix-p " " name))
                                          (propertize name
                                                      'drover-prompt-attach (cons 'buffer candidate)
                                                      'multi-category (cons 'buffer name)))))
                                    (buffer-list))))
         (project (let ((default-directory (buffer-local-value 'default-directory buffer)))
                    (project-current nil)))
         (base (and project (file-name-parent-directory (project-root project))))
         (taken (let ((table (make-hash-table :test #'equal)))
                  (dolist (name buffers table)
                    (puthash name t table))))
         (files (and project
                     (mapcar (lambda (file)
                               (let ((name (file-relative-name file base)))
                                 (propertize (if (gethash name taken) (concat name " (file)") name)
                                             'drover-prompt-attach (cons 'file file)
                                             'multi-category (cons 'file file))))
                             (project-files project)))))
    (append buffers files)))

(defun drover-prompt-insert-attachment ()
  "Insert at point the context of a buffer or of a project file.
Buffers come first, most recently used first, then the files of the
project of the prompt's directory.  A buffer's whole text is quoted as
`drover-prompt-dwim' quotes a region; a file is named by a link, without
its text, which agents read themselves.  An active region is refused,
being prompt text already.  This is the prompt action `attach'."
  (interactive)
  (unless drover-prompt-mode
    (user-error "Not in the prompt"))
  (when (use-region-p)
    (user-error "The region is prompt text already; deactivate it to attach a buffer or file"))
  (let* ((candidates (drover-prompt--attach-candidates (current-buffer)))
         (choice (completing-read "Attach buffer or file: "
                                  (lambda (string predicate action)
                                    (if (eq action 'metadata)
                                        '(metadata (category . multi-category))
                                      (complete-with-action action candidates string predicate)))
                                  nil t))
         (attachment (get-text-property 0 'drover-prompt-attach
                                        (or (car (member choice candidates))
                                            (user-error "Nothing to attach"))))
         (block (pcase attachment
                  (`(buffer . ,buffer)
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char (point-min))
                       (drover-prompt-format-context
                        buffer (buffer-substring-no-properties (point-min) (point-max))
                        (drover-prompt-language major-mode)))))
                  (`(file . ,file)
                   (format "From [[file:%s][%s]]\n\n"
                           (abbreviate-file-name file) (file-name-nondirectory file))))))
    (drover-prompt--ensure-blank-line)
    (insert block)))

(defun drover-prompt-copy-src-block ()
  "Copy the contents of the code or example block at point to the kill ring.
The block's escaping and common indentation are removed, as
`drover-prompt-to-markdown' removes them.  This is the prompt action
`copy-block'."
  (interactive)
  (let ((block (org-element-context)))
    (while (and block (not (memq (org-element-type block) '(src-block example-block))))
      (setq block (org-element-parent block)))
    (unless block
      (user-error "Point is not in a code or example block"))
    (let* ((value (org-element-property :value block))
           (value (if (org-src-preserve-indentation-p block)
                      value
                    (org-remove-indentation value))))
      (kill-new value)
      (message "Copied %d lines" (length (split-string (string-trim-right value "\n") "\n"))))))

;;;; Choosing what the prompt runs

(defun drover-prompt--options-buffer ()
  "Return the prompt buffer the choice of an option applies to.
It is the current buffer in the prompt, else the prompt buffer, created
undisplayed when missing, as from the target picker's option menu."
  (cond (drover-prompt-mode (current-buffer))
        ((drover-prompt-buffer))
        (t (drover-prompt-default-buffer))))

(defun drover-prompt--minibuffer-info (buffer)
  "Return what the prompt BUFFER runs, as the target picker shows it, or nil."
  (let* ((options (drover-prompt-effective-options buffer))
         (model (plist-get options :model))
         (variant (plist-get options :variant))
         (info (concat
                (drover-prompt--minibuffer-option "cwd" (drover-prompt--directory-label options)
                                                  (plist-get options :directory-explicit-p))
                (drover-prompt--minibuffer-option "agent" (plist-get options :agent)
                                                  (plist-get options :agent-explicit-p))
                (when model
                  (concat (propertize " [model: " 'face 'minibuffer-prompt)
                          (drover-prompt-option-display (or (drover-model-name model) (drover-model-id model))
                                                        (plist-get options :model-explicit-p))
                          (when variant
                            (concat "/" (drover-prompt-option-display
                                         (symbol-name variant) (plist-get options :variant-explicit-p))))
                          (propertize "]" 'face 'minibuffer-prompt)))
                (drover-prompt--minibuffer-option
                 "provider" (or (plist-get options :provider)
                                (and model (or (drover-model-provider-name model) (drover-model-provider-id model))))
                 (plist-get options :provider-explicit-p)))))
    (unless (string-empty-p info)
      info)))

(defun drover-prompt--remove-minibuffer-overlay ()
  "Delete the overlay of the current minibuffer showing what the prompt runs."
  (drover-with-reporting "prompt"
    (when (overlayp drover-prompt--minibuffer-overlay)
      (delete-overlay drover-prompt--minibuffer-overlay))
    (setq drover-prompt--minibuffer-overlay nil)))

(defun drover-prompt--archived-info ()
  "Return how the target picker of the current minibuffer offers archived sessions.
Nil while it shows none; else, for its minibuffer, whether they are
being fetched."
  (when drover-prompt--archived-shown
    (concat (propertize " [archived: " 'face 'minibuffer-prompt)
            (if (drover-fetch-pending-p drover-prompt-archived-fetch-timeout) "fetching…" "shown")
            (propertize "]" 'face 'minibuffer-prompt))))

(defun drover-prompt--update-minibuffer (buffer)
  "Show, after the prompt of the current minibuffer, what the prompt BUFFER runs.
And whether the archived sessions show (see
`drover-prompt-toggle-archived').  An overlay shows it rather than the
prompt's text, which completion interfaces such as Vertico keep
positions in.  BUFFER may be nil, or dead."
  (let ((info (concat (and (buffer-live-p buffer) (drover-prompt--minibuffer-info buffer))
                      (drover-prompt--archived-info))))
    (if (string-empty-p info)
        (drover-prompt--remove-minibuffer-overlay)
      (unless (and (overlayp drover-prompt--minibuffer-overlay)
                   (overlay-buffer drover-prompt--minibuffer-overlay))
        (setq drover-prompt--minibuffer-overlay
              (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end) nil t t)))
      (overlay-put drover-prompt--minibuffer-overlay 'after-string info))))

(defun drover-prompt--refresh-completion ()
  "Make the completion interface of the minibuffer compute its candidates anew.
The minibuffer is the current buffer.  Vertico keeps the candidates
while the input is unchanged, so a choice changing which new sessions
show would not show until the next key; resetting `vertico--input' as
Vertico does when the minibuffer opens updates them after the command."
  (when (local-variable-p 'vertico--input)
    (setq-local vertico--input t)))

(defun drover-prompt--refresh-display (buffer)
  "Show the choices just made in the prompt BUFFER.
They show in its header and, while the target picker is open, in the
picker's minibuffer and candidates.  Only BUFFER's header and mode
lines are drawn again: forcing every window's costs tens of
milliseconds with many windows."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (force-mode-line-update)))
  (when-let* ((window (active-minibuffer-window))
              (minibuffer (window-buffer window))
              ((buffer-local-value 'drover-prompt--picker minibuffer)))
    (with-current-buffer minibuffer
      (drover-prompt--update-minibuffer buffer)
      (drover-prompt--refresh-completion))))

(defun drover-prompt--options-host (buffer)
  "Return the host supplying the prompt BUFFER's catalogs, nil for this one."
  (with-current-buffer buffer
    (if-let* ((session (drover-prompt--chosen-target)))
        (drover-session-host session)
      drover-prompt-host)))

(defun drover-prompt--host-catalogs (buffer)
  "Return what the host of the prompt BUFFER told of its engines, waited for."
  (with-current-buffer buffer
    (let* ((session (drover-prompt--chosen-target))
           (host (drover-prompt--options-host buffer))
           (directory (if session (drover-session-directory session) drover-prompt-host-directory)))
      (or (drover-prompt--host-engines host directory drover-prompt-host-timeout)
          (user-error "%s did not tell its agents and models in %s"
                      (drover-prompt--host-label host) directory)))))

(defun drover-prompt-choose-agent ()
  "Choose the agent the prompt runs, among those the engines offer.
With two agents, switch to the other without asking.  New sessions of
another host (see `drover-prompt-host') choose among that host's
agents.  This is the prompt action `agent'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (host (drover-prompt--options-host buffer))
         (directory (if host
                        (buffer-local-value 'drover-prompt-host-directory buffer)
                      (buffer-local-value 'default-directory buffer)))
         (names (if host
                    (delete-dups (mapcan (lambda (entry) (copy-sequence (plist-get (cdr entry) :agents)))
                                         (drover-prompt--host-catalogs buffer)))
                  (drover-agent-names directory)))
         (current (plist-get (drover-prompt-effective-options buffer) :agent))
         (other (and (= (length names) 2) (member current names)
                     (car (remove current names))))
         (agent (or other
                    (drover-read-agent (plist-get (drover-prompt-inherited-options buffer) :agent)
                                       directory (and host (lambda () names))))))
    (with-current-buffer buffer
      (setq drover-prompt-picked-agent agent))
    (when other
      (message "Agent: %s" other))
    (drover-prompt--refresh-display buffer)))

(defun drover-prompt-choose-model ()
  "Choose the model the prompt runs, among those the engines offer.
Provider and effort choices are kept.  New sessions of another host
choose among that host's models.  This is the prompt action `model'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (host (drover-prompt--options-host buffer))
         (models (lambda ()
                   (drover-selection-models (drover-prompt--selection-catalogs buffer)
                                            (buffer-local-value 'drover-prompt-picked-provider buffer)
                                            (buffer-local-value 'drover-prompt-picked-variant buffer))))
         (inherited (plist-get (drover-prompt-inherited-options buffer) :model))
         (default (and inherited (drover-model-create :id (drover-selection-id inherited)
                                                      :name (drover-model-name inherited))))
         (model (drover-read-model default
                                   (if host
                                       (buffer-local-value 'drover-prompt-host-directory buffer)
                                     (buffer-local-value 'default-directory buffer))
                                   models)))
    (with-current-buffer buffer
      (setq drover-prompt-picked-model model
            drover-prompt-model-selection t))
    (drover-prompt--refresh-display buffer)))

(defun drover-prompt--selection-catalogs (buffer)
  "Return the catalogs for selections in prompt BUFFER, kept by engine."
  (if (drover-prompt--options-host buffer)
      (mapcar (lambda (entry) (cons (car entry) (plist-get (cdr entry) :models)))
              (drover-prompt--host-catalogs buffer))
    (drover-model-catalogs (buffer-local-value 'default-directory buffer))))

(defun drover-prompt-choose-provider ()
  "Choose the serving provider for the next prompt, independently of its model."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (model (buffer-local-value 'drover-prompt-picked-model buffer))
         (logical (buffer-local-value 'drover-prompt-model-selection buffer))
         (id (and model (drover-selection-id model)))
         (candidates (cons '("Default" . nil)
                           (drover-selection-providers
                            (drover-prompt--selection-catalogs buffer) id
                            (buffer-local-value 'drover-prompt-picked-variant buffer))))
         (choice (completing-read "Provider: " (drover--completion-table candidates 'drover-provider)
                                  nil t nil nil "Default")))
    (with-current-buffer buffer
      (setq drover-prompt-picked-provider (cdr (assoc choice candidates)))
      (when (and model (not logical))
        (setq drover-prompt-picked-model (drover-model-create :id id :name (drover-model-name model))))
      (setq drover-prompt-model-selection t))
    (drover-prompt--refresh-display buffer)))

(defun drover-prompt-choose-effort ()
  "Choose the variant, the reasoning effort, of the prompt's model.
The model is the one chosen, else the one a new session would run.
This is the prompt action `effort'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (picked (buffer-local-value 'drover-prompt-picked-model buffer))
         (base (or picked (plist-get (drover-prompt-inherited-options buffer) :model)))
         (id (and base (drover-selection-id base)))
         (model (or (seq-find (lambda (model) (equal (drover-model-id model) id))
                              (drover-selection-models
                               (drover-prompt--selection-catalogs buffer)
                               (buffer-local-value 'drover-prompt-picked-provider buffer)))
                    base)))
    (cond
     ((not model) (message "Choose a model first"))
     ((not (drover-model-variants-known model))
      (message "The efforts of %s are unknown until a session runs it" (drover-model-label model)))
     ((not (drover-model-variants model)) (message "No variants for %s" (drover-model-label model)))
     (t
      (let ((variant (drover-read-variant model)))
        (with-current-buffer buffer
          (setq drover-prompt-picked-variant variant)
          (when (and picked (not drover-prompt-model-selection))
            (setq drover-prompt-picked-provider
                  (or drover-prompt-picked-provider (drover-model-provider-id picked))
                  drover-prompt-picked-model (drover-model-create :id id :name (drover-model-name picked))))
          (setq drover-prompt-model-selection t))
        (drover-prompt--refresh-display buffer))))))

(defun drover-prompt--read-host (buffer)
  "Read the host new sessions of the prompt BUFFER start on; nil for this one.
The candidates are this host and the other hosts starting sessions for
it (see `drover-prompt-remote-functions'), BUFFER's first."
  (let* ((hosts (drover-prompt--remote :hosts))
         (here (format "%s (this host)" drover-host))
         (candidates (cons (cons here nil) (mapcar (lambda (host) (cons (cdr host) (car host))) hosts)))
         (default (or (car (rassoc (buffer-local-value 'drover-prompt-host buffer) candidates)) here)))
    (unless hosts
      (user-error "No other host starts sessions for this one"))
    (cdr (assoc (completing-read (format-prompt "New sessions start on" default)
                                 (mapcar #'car candidates) nil t nil nil default)
                candidates))))

(defun drover-prompt-choose-directory (&optional elsewhere)
  "Choose the directory a new session of the prompt starts in.
The engines' defaults are read again for it, as a project's settings
may change any of them.  With a prefix argument, ELSEWHERE, choose the
host first, this one or another host starting sessions for it (see
`drover-remote-mode'): the directory is then one of that host's,
completed there, which `drover-prompt-host' and
`drover-prompt-host-directory' keep, and its agents, models and
defaults those that host tells.  New sessions of another host choose
among its directories.  This is the prompt action `directory'."
  (interactive "P")
  (let* ((buffer (drover-prompt--options-buffer))
         (host (if elsewhere
                   (drover-prompt--read-host buffer)
                 (buffer-local-value 'drover-prompt-host buffer))))
    (if host
        (let ((directory (drover-prompt--remote
                          :read-directory host
                          (format "Working directory on %s: " (drover-prompt--host-label host))
                          (drover-prompt--host-default-directory host buffer drover-prompt-host-timeout))))
          (with-current-buffer buffer
            (setq drover-prompt-host host
                  drover-prompt-host-directory directory))
          ;; Its catalogs there, asked now for the header.
          (drover-prompt--remote :catalog host nil directory nil))
      (let ((directory (file-name-as-directory
                        (expand-file-name
                         (read-directory-name "Working directory: "
                                              (buffer-local-value 'default-directory buffer) nil t)))))
        (with-current-buffer buffer
          (setq default-directory directory
                drover-prompt-directory-picked-p t
                drover-prompt-host nil
                drover-prompt-host-directory nil)
          (drover-prompt--fetch-defaults)
          ;; An open target picker annotates before a scheduled refresh runs.
          (drover-prompt--refresh-volatile))))
    (drover-prompt--refresh-display buffer)))

(transient-define-prefix drover-prompt-options-menu ()
  "Choose what the prompt runs: its agent, model, effort and directory."
  [["Prompt runs"
    ("a" "Agent" drover-prompt-choose-agent)
    ("m" "Model" drover-prompt-choose-model)
    ("P" "Provider" drover-prompt-choose-provider)
    ("e" "Effort" drover-prompt-choose-effort)
    ("d" "Directory" drover-prompt-choose-directory)]]
  drover--menu-close
  (interactive)
  (transient-setup 'drover-prompt-options-menu nil nil
                   :display-action drover-menu-display-action))

(defvar-keymap drover-prompt-target-map
  :doc "Keys of the minibuffer choosing where the prompt goes.
The key of `drover-dispatch' opens `drover-prompt-options-menu' there."
  "<remap> <drover-dispatch>" #'drover-prompt-options-menu)

(defvar-keymap drover-prompt-archived-map
  :doc "Keys of the minibuffer choosing where the prompt goes, from the prompt.
See `drover-prompt-toggle-archived'."
  "C-z" #'drover-prompt-toggle-archived)

;;;; Choosing where the prompt goes

(defcustom drover-prompt-remote-limit 50
  "Sessions of other hosts the prompt offers to send to, at most.
Those shown in a buffer here come first, then the latest active.
Their hosts submit the prompt through their backends; it goes as text,
attachments being this host's files."
  :type 'natnum
  :group 'drover)

(defun drover-prompt--buffer-ranks ()
  "Return the ranks of the buffers, and of the sessions they belong to.
The value is (BUFFERS . SESSIONS).  BUFFERS is a hash table of the
live buffers to their rank, the current buffer 0, then the others from
1 in the order of `buffer-list'.  SESSIONS is a hash table of the keys
of the sessions buffers belong to (see `drover-buffer-session') to the
least rank of those buffers.  A list of sessions belongs to none, its
session at point being only where its point is."
  (let ((buffers (make-hash-table :test #'eq))
        (sessions (make-hash-table :test #'equal))
        (rank 0))
    (dolist (buffer (buffer-list))
      (puthash buffer (cl-incf rank) buffers))
    (puthash (current-buffer) 0 buffers)
    (dolist (buffer (buffer-list))
      (when-let* (((buffer-local-value 'drover-session-at-point-function buffer))
                  ((not (with-current-buffer buffer (drover-session-list-p))))
                  (session (drover-with-reporting "session of a buffer"
                             (drover-buffer-session buffer))))
        (let ((key (drover-session-key session))
              (rank (gethash buffer buffers)))
          (when (< rank (gethash key sessions most-positive-fixnum))
            (puthash key rank sessions)))))
    (cons buffers sessions)))

(defun drover-prompt--target-sessions ()
  "Return the running sessions the prompt may go to, with their buffers.
The value is a list of (SESSION . BUFFER): the sessions whose backend
takes prompts (:send) and that have a live buffer.  Those drover
archived itself are among them, a buffer still showing them: sent a
prompt, they are restored first (see `drover-unarchive-to-send').  The
sessions of other hosts connected that take prompts there, their
owners submitting them, are among them too, as (SESSION),
`drover-prompt-remote-limit' at most.

A session comes as its buffer used last does: the current buffer's
first, then in the order of `buffer-list'.  Its buffers are those
belonging to it (see `drover-prompt--buffer-ranks'), as its review,
and for a session of this host its own (`drover-session-buffer').
Another host's session is shown here by buffers belonging to it, its
mirror, its OpenCode buffer or its view, which all tell it: asking its
backend for its buffer too would only search them again.  The sessions
of other hosts shown in no buffer here come last, the latest active
first; of those of other hosts, the ones shown here are offered first.

The sessions are those of the index (see `drover-index-sessions'), as
backends last reported them: listing every backend's anew took longer
than the rest of the picker."
  (pcase-let* ((`(,buffers . ,shown) (drover-prompt--buffer-ranks))
               (local nil)
               (remote nil))
    (cl-flet ((rank-of (session buffer)
                (min (or (and buffer (gethash buffer buffers)) most-positive-fixnum)
                     (gethash (drover-session-key session) shown most-positive-fixnum))))
      (dolist (session (append (drover-index-sessions) (drover-archived-live-sessions)))
        (if (drover-session-host session)
            (when (and (not (drover-session-stale session))
                       (not (drover-session-archived session))
                       (drover-supports-p session :send))
              (push (cons (rank-of session nil) (list session)) remote))
          (when-let* (((drover-supports-p session :send))
                      (buffer (drover-session-buffer session)))
            (push (cons (rank-of session buffer) (cons session buffer)) local)))))
    (let* ((remote (seq-take (sort (nreverse remote)
                                   :key (lambda (entry)
                                          (if (< (car entry) most-positive-fixnum)
                                              (vector 0 (car entry))
                                            ;; A session telling no activity comes last,
                                            ;; never before the latest active ones.
                                            (let ((session (cadr entry)))
                                              (vector 1 (- (or (drover-session-since session)
                                                               (drover-session-updated session)
                                                               0)))))))
                             drover-prompt-remote-limit))
           (unshown-p (lambda (entry) (= (car entry) most-positive-fixnum))))
      (mapcar #'cdr (append (sort (nconc (nreverse local) (seq-remove unshown-p remote)) :key #'car)
                            (seq-filter unshown-p remote))))))


(defun drover-prompt--new-targets ()
  "Return the entries of `drover-prompt-new-targets' whose feature is installed."
  (seq-filter (lambda (entry)
                (let ((feature (plist-get entry :feature)))
                  (or (null feature) (featurep feature) (locate-library (symbol-name feature)))))
              drover-prompt-new-targets))

(defun drover-prompt--entry-options (target current buffer)
  "Return what TARGET runs after the choices of prompt BUFFER apply.
CURRENT holds a running session's options.  Incompatible destinations
carry :failure instead of advertising settings they cannot apply."
  (if (not (buffer-live-p buffer)) current
    (let* ((options (drover-prompt-explicit-options buffer))
           (resolved (drover-prompt--resolve target current options buffer))
           (effective (plist-get resolved :effective)))
      (cond
       ((plist-get resolved :failure) resolved)
       ((and (drover-session-p target)
             (not (seq-some (lambda (key) (plist-get options key)) '(:model :provider :variant)))) current)
       (t
        (list :model (plist-get effective :model)
              :model-explicit-p (and (or (plist-get options :model) (plist-get options :provider)) t)
              :variant (plist-get effective :variant)
              :variant-explicit-p (and (plist-get options :variant) t)))))))

(defun drover-prompt--options-label (options)
  "Return the model and variant of entry OPTIONS as the header shows them, or nil."
  (when-let* ((model (plist-get options :model)))
    (concat (drover-prompt-option-display (drover-model-label model)
                                          (plist-get options :model-explicit-p))
            (when-let* ((variant (plist-get options :variant)))
              (concat (propertize " / " 'face 'shadow)
                      (drover-prompt-option-display (symbol-name variant)
                                                    (plist-get options :variant-explicit-p)))))))

(defun drover-prompt--target-label (target)
  "Return the backend of TARGET, and its engine when its name differs."
  (let* ((session (and (drover-session-p target) target))
         (backend (if session (drover-session-backend session) (plist-get target :backend)))
         (engine (if session (drover-session-engine session) (plist-get target :engine)))
         (label (if (and (not session) (plist-get target :host) (not (memq backend (drover-backends))))
                    (symbol-name backend)
                  (drover-backend-label backend))))
    (if (or (null engine) (string= (drover-engine-label engine) label))
        label
      (format "%s (%s)" label (drover-engine-label engine)))))

(defun drover-prompt--candidates (sessions targets &optional archived)
  "Return the candidates of the target picker, as an alist (NAME . ENTRY).
SESSIONS are the running sessions, as (SESSION . BUFFER), named after
their buffer, those of other hosts, without one, after their titles
and hosts; TARGETS the entries of `drover-prompt-new-targets' to
offer, then the new sessions of other hosts, each with its :host and
:host-label; ARCHIVED, last, the archived sessions offered, as
SESSIONS, those without a buffer named after their titles, and their
hosts for other hosts', dimmed.  ENTRY is a plist: :target, the
session or new-target entry; :current, what a session runs now (see
:options); :directory, where it runs or would start, and :host-label,
the host whose directory it is, for another's; :archived, non-nil for
an archived session.  Each NAME carries its target as its
`drover-prompt-target' property."
  (let ((taken (make-hash-table :test #'equal))
        (prompt (drover-prompt-buffer))
        candidates)
    (cl-flet* ((add (base entry)
                 (let ((name base)
                       (counter 1))
                   (while (gethash name taken)
                     (setq counter (1+ counter)
                           name (format "%s <%d>" base counter)))
                   (puthash name t taken)
                   (push (cons (propertize name 'drover-prompt-target (plist-get entry :target)) entry)
                         candidates)))
               (add-session (session buffer &optional archived)
                 (let* ((title (drover-bound-line (format "%s" (drover-session-display-title session)) 60))
                        (host (drover-session-host session))
                        (name (cond (buffer (buffer-name buffer))
                                    (host (format "%s @%s" title (drover-host-label host)))
                                    (t title))))
                   (add (if archived (drover-face name 'drover-dim) name)
                        (list :target session
                              :current (and (drover-supports-p session :options)
                                            (drover-with-reporting "session options"
                                              (drover-backend-call session :options)))
                              :directory (or (drover-session-local-directory session)
                                             (and buffer (buffer-local-value 'default-directory buffer)))
                              :archived archived)))))
      (pcase-dolist (`(,session . ,buffer) sessions)
        (add-session session buffer))
      (dolist (target targets)
        (if-let* ((host (plist-get target :host)))
            (add (format "[New %s on %s…]" (or (plist-get target :label) (plist-get target :engine))
                         (plist-get target :host-label))
                 (list :target target
                       :directory (and prompt (drover-prompt--host-default-directory host prompt))
                       :host-label (plist-get target :host-label)))
          (add (format "[New %s…]" (or (plist-get target :label) (plist-get target :engine)))
               (list :target target
                     :directory (and prompt (buffer-local-value 'default-directory prompt))))))
      (pcase-dolist (`(,session . ,buffer) archived)
        (add-session session buffer t)))
    (nreverse candidates)))

(defun drover-prompt--host-untold-p (target buffer)
  "Return non-nil when TARGET waits for its host to tell what it offers.
TARGET is a candidate of the target picker.  It does for a new session
of another host that has not told yet where a session of the prompt
BUFFER starts there, or what its engines offer in that directory: both
are asked meanwhile, and the host's answer shows the picker's candidates
again (see `drover-prompt-hosts-told')."
  (when-let* (((not (drover-session-p target)))
              (host (plist-get target :host)))
    (let ((directory (and (buffer-live-p buffer) (drover-prompt--host-default-directory host buffer))))
      (not (and directory (drover-prompt--host-engines host directory))))))

(defun drover-prompt--annotator (candidates)
  "Return the annotation function of the target picker's CANDIDATES.
An annotation shows the status of a session, after it the archive's
glyph for an archived one, its backend and engine, the model and
variant it would run with the prompt's choices, computed anew each time
so that choices made in the picker show, or that its host has not told
yet what it offers, its directory and why it has its status."
  (let ((width (apply #'max 0 (mapcar (lambda (candidate) (string-width (car candidate)))
                                      candidates))))
    (lambda (name)
      (when-let* ((entry (cdr (assoc name candidates))))
        (let* ((target (plist-get entry :target))
               (session (and (drover-session-p target) target))
               (prompt (drover-prompt-buffer))
               (options (if (drover-prompt--host-untold-p target prompt)
                            (propertize "what it runs not told yet" 'face 'drover-dim)
                          (drover-prompt--options-label
                           (drover-prompt--entry-options target (plist-get entry :current) prompt))))
               (directory (plist-get entry :directory))
               (summary (and session (drover-session-summary session))))
          (concat (make-string (max 0 (- width (string-width name))) ?\s)
                  "  " (if session (opencode-session-status-glyph (drover-session-status session)) " ")
                  (if (plist-get entry :archived) (concat " " (opencode-archived-glyph)) "")
                  " " (propertize (format "[%s]" (drover-prompt--target-label target))
                                  'face 'drover-dim)
                  (and options (concat "  " options))
                  (and directory
                       (concat "  " (propertize (drover-prompt--directory-label
                                                 (list :directory directory
                                                       :host-label (plist-get entry :host-label)))
                                                'face 'drover-dim)))
                  (and summary
                       (concat "  " (propertize summary 'face 'completions-annotations)))))))))

(defun drover-prompt--target-predicate (candidates)
  "Return the predicate keeping CANDIDATES that can honor the prompt's choices.
A new session of another host that has not told yet what it offers is
kept, as it may (see `drover-prompt--host-untold-p'): sending to it
waits for what it offers."
  (lambda (candidate)
    (let* ((name (if (consp candidate) (car candidate) candidate))
           (entry (cdr (assoc name candidates)))
           (target (plist-get entry :target))
           (prompt (drover-prompt-buffer))
           (options (drover-prompt-explicit-options prompt)))
      (or (drover-prompt--host-untold-p target prompt)
          (not (plist-get (drover-prompt--resolve target (plist-get entry :current) options prompt)
                          :failure))))))

(defun drover-prompt--setup-picker (keymaps &optional update archived)
  "Set up the minibuffer of the target picker, with KEYMAPS over its own keys.
KEYMAPS is a keymap, a list of them, or nil for none.  UPDATE, a
function of no argument making the candidates anew (see
`drover-prompt--picker-update'), is called as what the picker offers
changes: the models engines offer (see `drover-models-changed'), what
other hosts tell (see `drover-prompt-hosts-told'), the archived sessions
fetched.  ARCHIVED non-nil offers the archived sessions (see
`drover-prompt-toggle-archived'); the picker opens without them.  The
minibuffer shows what the prompt runs."
  (when keymaps
    (use-local-map (make-composed-keymap keymaps (current-local-map))))
  (setq-local drover-prompt--picker t)
  (setq-local drover-prompt--picker-update update)
  (setq-local drover-prompt--archived-offered (and archived t))
  (setq-local drover-prompt--archived-shown nil)
  (add-hook 'minibuffer-exit-hook #'drover-prompt--remove-minibuffer-overlay nil t)
  (when update
    (add-hook 'minibuffer-exit-hook #'drover-prompt--picker-exit nil t)
    (unless (memq (current-buffer) drover-prompt--pickers)
      (push (current-buffer) drover-prompt--pickers))
    (add-hook 'drover-changed-hook #'drover-prompt--archived-changed)
    (add-hook 'drover-models-changed-hook #'drover-prompt--pickers-changed))
  (drover-prompt--update-minibuffer (drover-prompt-buffer)))

(defvar drover--archive-checking)

(defun drover-prompt--archived-targets (sessions)
  "Return the archived sessions the prompt may go to, as (SESSION . BUFFER).
SESSIONS, as `drover-prompt--target-sessions' returns them, are offered
already: they and their conversations are left out.  The others are
the archived sessions lists would show (see `drover-archived-sessions'),
of this host and of the other hosts connected, that may be sent a
prompt (see `drover-archived-takes-prompt-p'), but subagents: the
latest archived first.  BUFFER is the live buffer of a session of this
host, else nil: its backend opens it as it sends."
  (let ((drover--archive-checking (or drover--archive-checking t))
        (seen (make-hash-table :test #'equal)))
    (pcase-dolist (`(,session . ,_) sessions)
      (puthash (drover-session-key session) t seen)
      (when-let* ((key (drover-session-engine-key session)))
        (puthash (cons 'engine key) t seen)))
    (mapcar (lambda (session)
              (cons session (and (not (drover-session-host session)) (drover-session-buffer session))))
            (sort (seq-filter (lambda (session)
                                (and (not (drover-session-parent session))
                                     (not (drover-session-stale session))
                                     (drover-archived-takes-prompt-p session)))
                              (drover--not-seen (drover-archived-sessions) seen))
                  :key (lambda (session)
                         (let ((time (drover-session-archived session)))
                           (- (if (numberp time) time 0))))))))

(defun drover-prompt-toggle-archived ()
  "Show or hide the archived sessions among where the prompt may go.
In the picker `drover-prompt-submit' opens, which hides them at first.
They come last, dimmed, after the archive's glyph: the archived
sessions of this host and of the other hosts connected that may be sent
a prompt (see `drover-archived-takes-prompt-p'), but subagents, the
latest archived first.  Showing them fetches them from each backend
whose last fetch started more than `drover-prompt-archived-fetch-age'
seconds ago, and keeps them fetched from then on, as a search does (see
`drover-keep-fetched'); the minibuffer says so while they are fetched,
and those arriving join the candidates.  A session chosen is restored
from the archive as it is sent the prompt."
  (interactive)
  (unless drover-prompt--archived-offered
    (user-error "This picker offers no archived session"))
  (let ((minibuffer (current-buffer)))
    (setq drover-prompt--archived-shown (not drover-prompt--archived-shown))
    (when drover-prompt--archived-shown
      (drover-keep-fetched '(archived) drover-prompt-archived-fetch-age)
      (when (drover-fetch-pending-p drover-prompt-archived-fetch-timeout)
        ;; A backend may never answer: the minibuffer stops saying it fetches.
        (run-with-timer drover-prompt-archived-fetch-timeout nil
                        #'drover-prompt--picker-refresh minibuffer)))
    (drover-prompt--picker-refresh minibuffer)))

(defun drover-prompt--picker-refresh (minibuffer)
  "Make the candidates of the target picker in MINIBUFFER anew, and show them.
They take the archived sessions in, or leave them out, as the picker
shows them or not; its minibuffer says how they are fetched.  Nothing
happens once MINIBUFFER is gone, or no longer a picker."
  (drover-with-reporting "prompt picker"
    (when (and (buffer-live-p minibuffer)
               (buffer-local-value 'drover-prompt--picker-update minibuffer))
      (with-current-buffer minibuffer
        (setq drover-prompt--archived-serial (drover-fetched-serial))
        (funcall drover-prompt--picker-update)
        (drover-prompt--update-minibuffer (drover-prompt-buffer))
        (drover--refresh-completion minibuffer)))))

(defun drover-prompt--pickers-changed (&rest _)
  "Make the candidates of the target pickers open anew, and show them.
On `drover-models-changed-hook' while a picker is open, and as other
hosts tell what they offer (see `drover-prompt-hosts-told')."
  (drover-with-reporting "prompt picker"
    (dolist (minibuffer (copy-sequence drover-prompt--pickers))
      (if (buffer-live-p minibuffer)
          (drover-prompt--picker-refresh minibuffer)
        (drover-prompt--picker-forget minibuffer)))))

(defun drover-prompt-hosts-told ()
  "Show the target pickers' candidates anew, a host having told what it offers.
`drover-remote-mode' calls it once another host answered what it
starts, where and with which models: a new session of that host the
picker kept while it had not told (see `drover-prompt--host-untold-p')
is resolved then, and hidden when it cannot honor the prompt's choices,
and the new sessions of a host that told them since the picker opened
join it."
  (when drover-prompt--pickers
    (drover-prompt--pickers-changed)))

(defun drover-prompt--archived-changed ()
  "Take the archived sessions fetched anew in the pickers showing them.
On `drover-changed-hook' while a picker is open: a picker showing them,
whose archived sessions were taken in before those fetched changed (see
`drover-fetched-serial'), makes its candidates anew."
  (drover-with-reporting "prompt picker"
    (dolist (minibuffer (copy-sequence drover-prompt--pickers))
      (cond ((not (buffer-live-p minibuffer)) (drover-prompt--picker-forget minibuffer))
            ((and (buffer-local-value 'drover-prompt--archived-shown minibuffer)
                  (not (eql (buffer-local-value 'drover-prompt--archived-serial minibuffer)
                            (drover-fetched-serial))))
             (drover-prompt--picker-refresh minibuffer))))))

(defun drover-prompt--picker-forget (minibuffer)
  "Stop following what the target picker in MINIBUFFER offers.
The last picker forgotten takes `drover-prompt--archived-changed' off
`drover-changed-hook', and `drover-prompt--pickers-changed' off
`drover-models-changed-hook'."
  (setq drover-prompt--pickers (delq minibuffer drover-prompt--pickers))
  (unless drover-prompt--pickers
    (remove-hook 'drover-changed-hook #'drover-prompt--archived-changed)
    (remove-hook 'drover-models-changed-hook #'drover-prompt--pickers-changed)))

(defun drover-prompt--picker-exit ()
  "Forget the target picker of the current minibuffer.
On `minibuffer-exit-hook', locally: the next picker hides the archived
sessions."
  (drover-with-reporting "prompt picker"
    (setq drover-prompt--archived-shown nil
          drover-prompt--archived-offered nil
          drover-prompt--picker-update nil)
    (drover-prompt--picker-forget (current-buffer))))

(defun drover-prompt--preselected (sessions)
  "Return the session of SESSIONS the prompt was set to go to, or nil.
SESSIONS are as `drover-prompt--target-sessions' returns them.  A
session the prompt was set to go to that is no longer among them, its
buffer gone or its backend taking no prompt, is forgotten, and the
echo area says so."
  (when-let* ((prompt (drover-prompt-buffer))
              (target (buffer-local-value 'drover-prompt--chosen-session prompt)))
    (or (car (seq-find (lambda (entry) (equal (drover-session-key (car entry)) (plist-get target :key)))
                       sessions))
        (progn
          (with-current-buffer prompt
            (setq drover-prompt--chosen-session nil)
            (force-mode-line-update))
          (drover-message "%s no longer takes prompts; choose where the prompt goes"
                          (plist-get target :title))
          nil))))

(defun drover-prompt-read-target (prompt &rest keys)
  "Read where to send the prompt, with PROMPT.
The candidates are the running sessions whose backend takes prompts
and that have a live buffer, and those of other hosts, in the order of
`drover-prompt--target-sessions': the current buffer's first, then the
most recently used, another host's session by the buffers showing it
here, those of other hosts shown in none last.  Each is annotated with
its backend, what it runs and its status.  Then come the new sessions
of `drover-prompt-new-targets' whose feature is installed and whose
engine can run the model chosen, then the new sessions other hosts
start (see `drover-remote-mode').
Return the `drover-session' chosen, or for a new session its entry of
`drover-prompt-new-targets', a plist (:backend BACKEND :engine ENGINE
...), or for one of another host a plist (:host NAME :host-label LABEL
:backend BACKEND :engine ENGINE :label LABEL).  A session
`drover-prompt-edit-for' set the prompt to go to is returned without
asking, while it takes prompts.  KEYS are:

 :existing-only  Non-nil to offer the running sessions only; signal a
                 `user-error' when there is none.
 :options-menu   Non-nil to offer `drover-prompt-options-menu' even
                 without new sessions.
 :archived       Non-nil to offer the archived sessions too, after the
                 others, once \\<drover-prompt-archived-map>\\[drover-prompt-toggle-archived] shows them (see
                 `drover-prompt-toggle-archived').

Where new sessions are offered, or with :options-menu, the key of
`drover-dispatch' opens `drover-prompt-options-menu'.

The models of each engine and directory are made once while it reads
\(see `drover-keeping-models'): what each candidate would run is
resolved as it is filtered and as it is annotated."
  (drover-keeping-models
    (drover-prompt--read-target prompt keys)))

(defun drover-prompt--read-target (prompt keys)
  "Read where to send the prompt, with PROMPT, as KEYS tell.
See `drover-prompt-read-target'."
  (let* ((existing-only (plist-get keys :existing-only))
         (sessions (drover-prompt--target-sessions))
         (prompt-buffer (drover-prompt-buffer)))
    (or
     (drover-prompt--preselected sessions)
     (progn
       (when (and existing-only (null sessions))
         (user-error "No running session takes prompts"))
       ;; New sessions are annotated with the engines' defaults.
       (when (and prompt-buffer (not existing-only))
         (with-current-buffer prompt-buffer
           (drover-prompt--refresh-volatile)))
       (let* ((targets (unless existing-only
                         ;; Other hosts' told already, or soon.
                         (append (drover-prompt--new-targets) (drover-prompt--remote :targets 2))))
              (candidates (or (drover-prompt--candidates sessions targets)
                              (user-error "No running session takes prompts, and none can start")))
              (names (mapcar #'car candidates))
              (annotate (drover-prompt--annotator candidates))
              (predicate (drover-prompt--target-predicate candidates))
              (table (lambda (string predicate action)
                       (if (eq action 'metadata)
                           `(metadata (category . drover-target)
                                      (display-sort-function . identity)
                                      (cycle-sort-function . identity)
                                      (annotation-function . ,(lambda (name) (funcall annotate name))))
                         (complete-with-action action names string predicate))))
              (update (lambda ()
                        ;; Other hosts' as told now, those told since joining.
                        (setq targets (unless existing-only
                                        (append (drover-prompt--new-targets) (drover-prompt--remote :targets)))
                              candidates (drover-prompt--candidates
                                          sessions targets
                                          (and drover-prompt--archived-shown
                                               (drover-prompt--archived-targets sessions)))
                              names (mapcar #'car candidates)
                              annotate (drover-prompt--annotator candidates)
                              predicate (drover-prompt--target-predicate candidates))))
              (archived (plist-get keys :archived))
              (keymaps (delq nil (list (and (or targets (plist-get keys :options-menu)) drover-prompt-target-map)
                                       (and archived drover-prompt-archived-map))))
              (choice (minibuffer-with-setup-hook (lambda () (drover-prompt--setup-picker keymaps update archived))
                        (completing-read prompt table
                                         (lambda (candidate) (funcall predicate candidate))
                                         t))))
         (or (plist-get (cdr (assoc choice candidates)) :target)
             (user-error "No session %s" choice)))))))

(provide 'drover-prompt)
;;; drover-prompt.el ends here
