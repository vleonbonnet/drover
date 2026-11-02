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
;; (`drover-prompt-dwim'), or from a list of sessions the sessions
;; selected, quoted with their last replies
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
;; engines reading local files whenever the prompt gains context.
;;
;; Scheduling, Org links and the like belong to the user's
;; configuration, which uses `drover-prompt-content',
;; `drover-prompt-consume', `drover-prompt-read-target',
;; `drover-prompt-explicit-options', `drover-prompt-options-for' and
;; `drover-prompt-apply-options'.

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
(require 'drover-ui)
(require 'opencode-look)
(require 'drover-keys)
(require 'drover-claude-code)
(require 'drover-codex)

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

(declare-function org-id-get-create "org-id" (&optional force))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function project-files "project" (project &optional dirs))

;;;; Options

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

(defcustom drover-prompt-quote-timeout 10
  "Seconds `drover-prompt-quote-sessions' waits for the replies it quotes.
The prompt gains the quote once every session's reply arrived, or once
this time passed, those late left without a reply."
  :type 'number
  :group 'drover)

(defcustom drover-prompt-quote-reply-lines 20
  "Lines of a last reply `drover-prompt-quote-sessions' quotes at most.
A longer reply is cut, and a note tells how many lines it goes on for."
  :type 'natnum
  :group 'drover)

(defcustom drover-prompt-quote-reply-characters 2000
  "Characters of a last reply `drover-prompt-quote-sessions' quotes at most.
It bounds the quote of replies of few long lines, as
`drover-prompt-quote-reply-lines' does of many lines."
  :type 'natnum
  :group 'drover)

;;;; State of the prompt buffer

(defvar-local drover-prompt-picked-agent nil
  "Agent the user chose for the prompt, a string, or nil for the default.")

(defvar-local drover-prompt-picked-model nil
  "Model the user chose for the prompt, a `drover-model', or nil for the default.")

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

;;;; The mode

(defconst drover-prompt--actions
  '((send . drover-prompt-submit)
    (clear . drover-prompt-erase)
    (attach . drover-prompt-insert-attachment)
    (agent . drover-prompt-choose-agent)
    (model . drover-prompt-choose-model)
    (effort . drover-prompt-choose-effort)
    (directory . drover-prompt-choose-directory)
    (copy-block . drover-prompt-copy-src-block)
    (open-link . org-open-at-point))
  "Prompt actions `drover-prompt-mode' defines, as (ACTION . COMMAND).")

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
`send', `clear', `attach', `copy-block', `open-link', `agent', `model',
`effort' and `directory', and remembers the point the user leaves the
buffer at.  Other packages define more actions from
`drover-prompt-mode-hook', such as `schedule'.  It binds no key: the
prompt keys are in `drover-dispatch'."
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
Every choice is forgotten, and the defaults are fetched again."
  (setq default-directory (file-name-as-directory (expand-file-name directory))
        drover-prompt--default-directory default-directory
        drover-prompt-directory-picked-p nil
        drover-prompt-picked-agent nil
        drover-prompt-picked-model nil
        drover-prompt-picked-variant nil
        drover-prompt-title nil)
  (drover-prompt--fetch-defaults))

(defun drover-prompt--begin (directory)
  "Begin a new prompt from DIRECTORY in the current, empty prompt buffer.
The agent, model, variant and directory chosen while the buffer was
empty stay, since they were chosen for this prompt; DIRECTORY is
inherited unless a directory was chosen."
  (setq drover-prompt-title nil)
  (if drover-prompt-directory-picked-p
      (drover-prompt--schedule-refresh)
    (setq default-directory (file-name-as-directory (expand-file-name directory))
          drover-prompt--default-directory default-directory)
    (drover-prompt--fetch-defaults)))

;;;; What the prompt runs

(defun drover-prompt--runs-p (model engine)
  "Return non-nil when ENGINE can run MODEL, a `drover-model'.
It can when it has an identifier for it (see `drover-model-id-for')."
  (and (drover-model-id-for model engine) t))

(defun drover-prompt-explicit-options (&optional buffer)
  "Return the options chosen in the prompt BUFFER, as an options plist.
BUFFER defaults to the prompt buffer.  The plist is (:agent AGENT
:model MODEL :variant VARIANT), each nil when not chosen, all of them
without a prompt buffer; see `drover-model'."
  (let ((buffer (or buffer (drover-prompt-buffer))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (list :agent (and drover-prompt-picked-agent
                            (not (string-empty-p drover-prompt-picked-agent))
                            drover-prompt-picked-agent)
                :model drover-prompt-picked-model
                :variant drover-prompt-picked-variant))
      (list :agent nil :model nil :variant nil))))

(defun drover-prompt-options-for (session options &optional warn)
  "Return OPTIONS, an options plist, as SESSION can run them.
A model the engine of SESSION cannot run, having no identifier for it
\(see `drover-model-id-for'), is dropped, and its variant with it, since
a variant belongs to its model; with WARN, that is reported with
`drover-warn'."
  (let ((agent (plist-get options :agent))
        (model (plist-get options :model))
        (variant (plist-get options :variant)))
    (if (and model (not (drover-prompt--runs-p model (drover-session-engine session))))
        (progn
          (when warn
            (drover-warn "%s cannot be applied to %s; keeping its current model"
                         (drover-model-label model) (drover-session-display-title session)))
          (list :agent agent :model nil :variant nil))
      (list :agent agent :model model :variant variant))))

(defun drover-prompt--once (function)
  "Return a function of no argument calling FUNCTION the first time only."
  (let ((called nil))
    (lambda ()
      (unless called
        (setq called t)
        (funcall function)))))

(defun drover-prompt--apply (session options then)
  "Give OPTIONS, already filtered for SESSION, to its backend, then call THEN.
Without :apply-options, THEN is called at once.  THEN is called once,
even when the backend signals, which is reported."
  (let ((apply (drover-backend-property session :apply-options))
        (then (drover-prompt--once then)))
    (if (not (functionp apply))
        (funcall then)
      (condition-case err
          (funcall apply session options then)
        (error
         (drover-warn "Could not apply the options to %s: %s; sending the prompt anyway"
                      (drover-session-display-title session) (drover-error-message err))
         (funcall then))))))

(defun drover-prompt-apply-options (session options then)
  "Apply OPTIONS, an options plist, to the running SESSION, then call THEN.
OPTIONS are filtered for SESSION as `drover-prompt-options-for' does,
with a warning, then given to its backend's :apply-options; without
it, or without SESSION, THEN is called at once."
  (if (null session)
      (funcall then)
    (drover-prompt--apply session (drover-prompt-options-for session options t) then)))

(defun drover-prompt-engine-options (engine agent model variant &optional buffer)
  "Return the resolved options a new session of ENGINE would run.
AGENT, MODEL and VARIANT are the explicit choices; the engine resolves
them with the defaults the prompt BUFFER holds for it, BUFFER
defaulting to the current one.  See `drover-engine-resolve'."
  (with-current-buffer (or buffer (current-buffer))
    (drover-engine-resolve engine (alist-get engine drover-prompt--defaults) agent model variant)))

(defun drover-prompt--display-engine (&optional buffer)
  "Return the engine whose new session the prompt BUFFER describes.
It is the engine that took a prompt last, else
`drover-prompt-default-engine', unless the model chosen cannot run
there: then the first engine that can.  BUFFER defaults to the current
one."
  (with-current-buffer (or buffer (current-buffer))
    (let ((last (or drover-prompt--last-engine drover-prompt-default-engine))
          (engines (and drover-prompt-picked-model
                        (drover-model-engines drover-prompt-picked-model))))
      (if (or (null engines) (memq last engines))
          last
        (car engines)))))

(defun drover-prompt-inherited-options (&optional buffer)
  "Return the resolved options a new session would run without any choice.
The engine is the one that took a prompt last, else
`drover-prompt-default-engine'.  BUFFER is the prompt buffer, the
current one by default.  The pickers' \"Default\" entry names them."
  (with-current-buffer (or buffer (current-buffer))
    (drover-prompt-engine-options (or drover-prompt--last-engine drover-prompt-default-engine)
                                  nil nil nil)))

(defun drover-prompt-effective-options (&optional buffer)
  "Return what the prompt BUFFER shows it runs, as a display plist.
BUFFER defaults to the current one.  The plist holds the resolved
:agent, :model and :variant, whether each was chosen
\(:agent-explicit-p, :model-explicit-p, :variant-explicit-p), the
:directory and whether it was chosen (:directory-explicit-p).  The
resolved values are for display only: only the choices are applied."
  (with-current-buffer (or buffer (current-buffer))
    (let ((resolved (drover-prompt-engine-options (drover-prompt--display-engine)
                                                  drover-prompt-picked-agent
                                                  drover-prompt-picked-model
                                                  drover-prompt-picked-variant)))
      (list :directory (or default-directory drover-prompt--default-directory)
            :directory-explicit-p drover-prompt-directory-picked-p
            :agent (plist-get resolved :agent)
            :agent-explicit-p (and drover-prompt-picked-agent t)
            :model (plist-get resolved :model)
            :model-explicit-p (and drover-prompt-picked-model t)
            :variant (plist-get resolved :variant)
            :variant-explicit-p (and drover-prompt-picked-variant t)))))

(defun drover-prompt-option-display (value explicit-p)
  "Return VALUE as a string in the face of a chosen or an inherited setting.
The face is `opencode-setting-chosen' when EXPLICIT-P, else
`opencode-setting-inherited'."
  (propertize (format "%s" value)
              'face (if explicit-p 'opencode-setting-chosen 'opencode-setting-inherited)))

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

(defun drover-prompt--header-line ()
  "Return the header line of the prompt: its keys and what it runs.
The keys are those of `drover-dispatch', after the key it is bound to;
scheduling shows when the buffer defines it."
  (let* ((options (drover-prompt-effective-options))
         (dispatch (substitute-command-keys "\\[drover-dispatch]"))
         (directory (plist-get options :directory))
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
     "  |  "
     (funcall key #'drover-prompt-directory "cwd" t)
     (drover-prompt--header-option (and directory (abbreviate-file-name directory))
                                   (plist-get options :directory-explicit-p))
     "  " (funcall key #'drover-prompt-agent "agent")
     (drover-prompt--header-option (plist-get options :agent) (plist-get options :agent-explicit-p))
     "  " (funcall key #'drover-prompt-model "model")
     (drover-prompt--header-option (and model (drover-model-label model))
                                   (plist-get options :model-explicit-p))
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
user left the prompt at.  In a list of sessions, the context is the
sessions the region selects, else the one at point, each linked and
quoted with its last reply (see `drover-prompt-quote-sessions').  An
Org heading line is quoted as any other line: `drover-prompt-org-task'
starts a prompt for an Org task.  Signal a `user-error' in the prompt
buffer itself."
  (interactive)
  (when drover-prompt-mode
    (user-error "Already in the prompt"))
  (drover-prompt-add-context))

(defun drover-prompt-add-context ()
  "Open the prompt, or add to it the context at point.
As `drover-prompt-dwim' does, which refuses the prompt buffer first.
The context is the active region, else the line at point, formatted by
`drover-prompt-format-context'.  In a list of sessions, it is the
sessions selected, or the one at point, quoted with their last replies
by `drover-prompt-quote-sessions'.  It goes at point in the prompt's
window, or while the prompt is hidden where the user left point."
  (if-let* (((drover-session-list-p))
            (sessions (drover-selected-sessions t)))
      (drover-prompt-quote-sessions sessions)
    (let* ((source (current-buffer))
           (region (if (use-region-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
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

(defun drover-prompt-quote-sessions (sessions)
  "Add SESSIONS to the prompt, each with a link, its status and its last reply.
The links are made at once, from the current buffer, by
`drover-session-link'; the replies are fetched from the backends of
SESSIONS all at once, without waiting (see `drover-recap'), the echo
area telling how many arrived.  The quote goes into the prompt once
every reply arrived, or after `drover-prompt-quote-timeout' seconds, a
session whose reply did not arrive, or that has none, quoted without
one.  See `drover-prompt-format-sessions' for the quote."
  (let* ((count (length sessions))
         (links (mapcar #'drover-session-link sessions))
         (directory (or (drover-session-directory (car sessions)) default-directory))
         (origin (selected-window))
         (recaps (make-hash-table :test #'equal))
         (finished nil)
         (timer nil))
    (cl-labels ((finish ()
                  (unless finished
                    (setq finished t)
                    (when timer
                      (cancel-timer timer))
                    (let ((late (- count (hash-table-count recaps))))
                      ;; Arriving later, after the user went elsewhere,
                      ;; the prompt shows without taking the window.
                      (drover-prompt--add (drover-prompt-format-sessions sessions links recaps)
                                          directory (not (eq (selected-window) origin)))
                      (message "Quoted %s%s"
                               (if (cdr sessions)
                                   (format "%d sessions" count)
                                 (drover-session-display-title (car sessions)))
                               (if (> late 0)
                                   (format "; %d repl%s did not arrive in time"
                                           late (if (= late 1) "y" "ies"))
                                 "")))))
                (progress ()
                  (unless finished
                    (message "Fetching the last repl%s of %s… %d/%d"
                             (if (cdr sessions) "ies" "y")
                             (if (cdr sessions)
                                 (format "%d sessions" count)
                               (drover-session-display-title (car sessions)))
                             (hash-table-count recaps) count))))
      (setq timer (run-at-time drover-prompt-quote-timeout nil
                               (lambda () (drover-with-reporting "prompt quote" (finish)))))
      (dolist (session sessions)
        (let ((key (drover-session-key session)))
          (drover-recap session
                        (lambda (recap)
                          (unless finished
                            (puthash key recap recaps)
                            (if (= (hash-table-count recaps) count)
                                (finish)
                              (progress)))))))
      (progress))))

(defun drover-prompt--quote-status (session)
  "Return how SESSION stands, as a quote tells an agent."
  (let ((summary (drover-session-summary session)))
    (concat (pcase (drover-session-status session)
              ('waiting "waiting for an answer")
              (status (symbol-name status)))
            (and summary
                 (concat ": " (replace-regexp-in-string "[\n\r\t]+" " " (string-trim summary)))))))

(defun drover-prompt--quote-reply (reply)
  "Return REPLY cut for a quote, as (TEXT . MORE), or nil when blank.
TEXT is at most `drover-prompt-quote-reply-lines' lines and
`drover-prompt-quote-reply-characters' characters of REPLY, a line cut
in the middle ending in an ellipsis; MORE is how many lines of REPLY it
leaves out, a line cut counting, or 0."
  (when (and (stringp reply) (not (string-blank-p reply)))
    (let* ((lines (split-string (string-trim-right (string-trim-left reply "\n+")) "\n"))
           (kept nil)
           (room drover-prompt-quote-reply-characters)
           (left lines))
      (while (and left (< (length kept) drover-prompt-quote-reply-lines) (> room 0))
        (let ((line (pop left)))
          (if (<= (length line) room)
              (progn (push line kept)
                     (setq room (- room (length line) 1)))
            (push (concat (string-trim-right (substring line 0 room)) (truncate-string-ellipsis)) kept)
            ;; The line cut counts among those left out.
            (push line left)
            (setq room 0))))
      (cons (string-join (nreverse kept) "\n") (length left)))))

(defun drover-prompt-format-sessions (sessions links recaps)
  "Return the Org text quoting SESSIONS for an agent to read.
LINKS are Org links to SESSIONS, in their order, nil for a session
without one; RECAPS is a hash table of the recaps of `drover-recap' by
session key.  Each session is an item of a list: its link, else its
title, and how it stands, then its last reply, when it has one, in a
Markdown block, which a sent prompt turns into a fence, cut by
`drover-prompt--quote-reply' with a note telling how much is left out."
  (concat
   (if (cdr sessions) "Agent sessions:\n" "Agent session:\n")
   (string-join
    (cl-mapcar
     (lambda (session link)
       (let* ((title (replace-regexp-in-string "[\n\r\t]+" " "
                                               (format "%s" (drover-session-display-title session))))
              (reply (drover-prompt--quote-reply
                      (plist-get (gethash (drover-session-key session) recaps) :reply))))
         (concat "- " (or link title) " — " (drover-prompt--quote-status session) "\n"
                 (when reply
                   (concat "  Last reply:\n"
                           "  #+begin_src markdown\n"
                           (replace-regexp-in-string "^\\(.\\)" "  \\1"
                                                     (org-escape-code-in-string (car reply)))
                           "\n  #+end_src\n"
                           (when (> (cdr reply) 0)
                             (format "  (The reply goes on for %d more line%s.)\n"
                                     (cdr reply) (if (= (cdr reply) 1) "" "s"))))))))
     sessions links))
   "\n"))

;;;; Text

(defun drover-prompt-format-context (buffer region language)
  "Return the Org context of BUFFER at point: a link, then REGION quoted.
The link names the file and line at point, or the buffer and line.
REGION, a string or nil, goes in a code block of LANGUAGE, escaped so
that no line of it ends the block or opens a heading, without the blank
lines around it, and from a terminal without trailing spaces."
  (with-current-buffer buffer
    (let* ((file (buffer-file-name))
           (line (line-number-at-pos))
           (header (if file
                       (format "From [[file:%s::%d][%s:%d]]" file line (buffer-name) line)
                     (format "From buffer %s line %d" (buffer-name) line)))
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

(defun drover-prompt-content (&optional buffer)
  "Return what the prompt BUFFER would send, or nil when it is blank.
BUFFER defaults to the prompt buffer.  The value is a plist: :org, the
Org text, trimmed; :text, it in Markdown; :start and :end, markers
around it in BUFFER; :region, non-nil when it is the active region
rather than the whole buffer.  Nil too without a prompt buffer."
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
                  :region (and region t))))))))

(defun drover-prompt--close (buffer)
  "Kill the prompt BUFFER and delete the windows that showed it.
A window that is the last of its frame stays."
  (let ((windows (get-buffer-window-list buffer nil t)))
    (kill-buffer buffer)
    (dolist (window windows)
      (when (and (window-live-p window) (eq (window-deletable-p window) t))
        (delete-window window)))))

(defun drover-prompt-consume (content &optional buffer)
  "Remove CONTENT, as `drover-prompt-content' returned it, from the prompt BUFFER.
BUFFER defaults to the prompt buffer.  A region is deleted and the mark
deactivated, the buffer and its window kept, unless only blanks remain;
otherwise the prompt is done with: its buffer is killed and its window
deleted."
  (let ((buffer (or buffer (drover-prompt-buffer))))
    (when (buffer-live-p buffer)
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

(defun drover-prompt--deliver (session buffer text token)
  "Send TEXT with TOKEN through the backend of SESSION.
BUFFER is the live buffer SESSION had when it was chosen, or nil for a
new session; a SESSION whose BUFFER died meanwhile is reported, and so
is a backend failing to send.  This may run from a process filter or a
timer, so it signals nothing."
  (let ((title (drover-session-display-title session))
        (send (drover-backend-property session :send)))
    (cond
     ((and buffer (not (buffer-live-p buffer)))
      (drover-warn "%s died before the prompt was sent; it is in the kill ring" title))
     ((not (functionp send))
      (drover-warn "%s takes no prompt; it is in the kill ring" title))
     (t
      (condition-case err
          (save-current-buffer
            (funcall send session text token))
        (error
         (drover-warn "%s did not take the prompt: %s; it is in the kill ring"
                      title (drover-error-message err))))))))

(defun drover-prompt--send-to-session (session content buffer)
  "Send CONTENT of the prompt BUFFER to the running SESSION.
Its backend's checks run first, for the options about to be applied:
when they cancel, nothing is sent and the prompt stays.  Otherwise the
options are applied, the text sent, and CONTENT leaves the prompt."
  (let* ((text (plist-get content :text))
         (target (drover-session-buffer session))
         (explicit (drover-prompt-explicit-options buffer))
         (check (drover-backend-property session :check))
         (token (and (functionp check)
                     (funcall check session text (drover-prompt-options-for session explicit)))))
    (if (eq token 'cancel)
        (message "Not sent to %s; the prompt is in the kill ring"
                 (drover-session-display-title session))
      (drover-prompt-apply-options session explicit
                                   (lambda () (drover-prompt--deliver session target text token)))
      (drover-prompt-consume content buffer))))

(defun drover-prompt--send-to-new (target content buffer)
  "Start the new session TARGET describes and send it CONTENT of the prompt BUFFER.
TARGET is an entry of `drover-prompt-new-targets'.  Its feature is
loaded; its backend, which must be registered and run its engine,
starts the session in BUFFER's directory with the options chosen there.
CONTENT leaves the prompt, unless the backend refuses at once."
  (let* ((backend (plist-get target :backend))
         (engine (plist-get target :engine))
         (feature (plist-get target :feature))
         (label (or (plist-get target :label)
                    (format "%s session" (drover-engine-label engine)))))
    (when feature
      (require feature))
    (let ((create (drover-backend-property backend :create)))
      (unless (and (memq backend (drover-backends))
                   (functionp create)
                   (memq engine (drover-backend-engines backend)))
        (user-error "%s starts no %s session"
                    (drover-backend-label backend) (drover-engine-label engine)))
      (let ((options (with-current-buffer buffer
                       (append (drover-prompt-explicit-options buffer)
                               (list :title drover-prompt-title
                                     :defaults (alist-get engine drover-prompt--defaults)))))
            (text (plist-get content :text))
            (returned nil)
            (refused nil))
        (funcall create (buffer-local-value 'default-directory buffer) engine options
                 (lambda (session)
                   (drover-prompt--deliver session nil text nil))
                 (lambda (reason)
                   (drover-warn "No new %s: %s; the prompt is in the kill ring" label reason)
                   (unless returned
                     (setq refused t))))
        (setq returned t)
        (unless refused
          (drover-prompt-consume content buffer))))))

(defun drover-prompt-submit ()
  "Send the prompt to a session chosen among the running ones and new ones.
With an active region, only the region is sent.  The text, in Org, is
put in the kill ring, then converted to Markdown.  To a running session,
the options chosen in the prompt are applied first, those its engine
cannot run left out with a warning; to a new session, they are its
start options.  The text sent leaves the prompt, unless nothing was
sent: a check the user cancelled keeps it, region included.  This is
the prompt action `send'."
  (interactive)
  (let* ((buffer (drover-prompt--prompt-buffer))
         (content (or (drover-prompt-content buffer)
                      (user-error "Prompt is empty"))))
    ;; The prompt leaves the buffer before the backend took it.
    (kill-new (plist-get content :org))
    (let ((target (with-current-buffer buffer
                    (drover-prompt-read-target "Send to session: "))))
      (if (drover-session-p target)
          (drover-prompt--send-to-session target content buffer)
        (drover-prompt--send-to-new target content buffer)))))

(defun drover-prompt-erase ()
  "Erase the text of the prompt, region or not.
The buffer, its window and the agent, model, effort and directory
chosen stay, for the next prompt.  This is the prompt action `clear'."
  (interactive)
  (with-current-buffer (drover-prompt--prompt-buffer)
    (deactivate-mark)
    (erase-buffer)))

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
         (directory (plist-get options :directory))
         (model (plist-get options :model))
         (variant (plist-get options :variant))
         (info (concat
                (drover-prompt--minibuffer-option "cwd" (and directory (abbreviate-file-name directory))
                                                  (plist-get options :directory-explicit-p))
                (drover-prompt--minibuffer-option "agent" (plist-get options :agent)
                                                  (plist-get options :agent-explicit-p))
                (when model
                  (concat (propertize " [model: " 'face 'minibuffer-prompt)
                          (drover-prompt-option-display (drover-model-label model)
                                                        (plist-get options :model-explicit-p))
                          (when variant
                            (concat "/" (drover-prompt-option-display
                                         (symbol-name variant) (plist-get options :variant-explicit-p))))
                          (propertize "]" 'face 'minibuffer-prompt))))))
    (unless (string-empty-p info)
      info)))

(defun drover-prompt--remove-minibuffer-overlay ()
  "Delete the overlay of the current minibuffer showing what the prompt runs."
  (drover-with-reporting "prompt"
    (when (overlayp drover-prompt--minibuffer-overlay)
      (delete-overlay drover-prompt--minibuffer-overlay))
    (setq drover-prompt--minibuffer-overlay nil)))

(defun drover-prompt--update-minibuffer (buffer)
  "Show, after the prompt of the current minibuffer, what the prompt BUFFER runs.
An overlay shows it rather than the prompt's text, which completion
interfaces such as Vertico keep positions in."
  (if-let* ((info (and (buffer-live-p buffer) (drover-prompt--minibuffer-info buffer))))
      (progn
        (unless (and (overlayp drover-prompt--minibuffer-overlay)
                     (overlay-buffer drover-prompt--minibuffer-overlay))
          (setq drover-prompt--minibuffer-overlay
                (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end) nil t t)))
        (overlay-put drover-prompt--minibuffer-overlay 'after-string info))
    (drover-prompt--remove-minibuffer-overlay)))

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

(defun drover-prompt-choose-agent ()
  "Choose the agent the prompt runs, among those the engines offer.
With two agents, switch to the other without asking.  This is the
prompt action `agent'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (directory (buffer-local-value 'default-directory buffer))
         (names (drover-agent-names directory))
         (current (plist-get (drover-prompt-effective-options buffer) :agent))
         (other (and (= (length names) 2) (member current names)
                     (car (remove current names))))
         (agent (or other
                    (drover-read-agent (plist-get (drover-prompt-inherited-options buffer) :agent)
                                       directory))))
    (with-current-buffer buffer
      (setq drover-prompt-picked-agent agent))
    (when other
      (message "Agent: %s" other))
    (drover-prompt--refresh-display buffer)))

(defun drover-prompt-choose-model ()
  "Choose the model the prompt runs, among those the engines offer.
A variant chosen before is dropped, as it belongs to its model.  This
is the prompt action `model'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (model (drover-read-model (plist-get (drover-prompt-inherited-options buffer) :model)
                                   (buffer-local-value 'default-directory buffer))))
    (with-current-buffer buffer
      (setq drover-prompt-picked-model model
            drover-prompt-picked-variant nil))
    (drover-prompt--refresh-display buffer)))

(defun drover-prompt-choose-effort ()
  "Choose the variant, the reasoning effort, of the prompt's model.
The model is the one chosen, else the one a new session would run.
This is the prompt action `effort'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (model (or (buffer-local-value 'drover-prompt-picked-model buffer)
                    (plist-get (drover-prompt-inherited-options buffer) :model))))
    (cond
     ((not model) (message "Choose a model first"))
     ((not (drover-model-variants-known model))
      (message "The efforts of %s are unknown until a session runs it" (drover-model-label model)))
     ((not (drover-model-variants model)) (message "No variants for %s" (drover-model-label model)))
     (t
      (let ((variant (drover-read-variant model)))
        (with-current-buffer buffer
          (setq drover-prompt-picked-variant variant))
        (drover-prompt--refresh-display buffer))))))

(defun drover-prompt-choose-directory ()
  "Choose the directory a new session of the prompt starts in.
The engines' defaults are read again for it, as a project's settings
may change any of them.  This is the prompt action `directory'."
  (interactive)
  (let* ((buffer (drover-prompt--options-buffer))
         (directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Working directory: "
                                           (buffer-local-value 'default-directory buffer) nil t)))))
    (with-current-buffer buffer
      (setq default-directory directory
            drover-prompt-directory-picked-p t)
      (drover-prompt--fetch-defaults)
      ;; An open target picker annotates before a scheduled refresh runs.
      (drover-prompt--refresh-volatile))
    (drover-prompt--refresh-display buffer)))

(transient-define-prefix drover-prompt-options-menu ()
  "Choose what the prompt runs: its agent, model, effort and directory."
  [["Prompt runs"
    ("a" "Agent" drover-prompt-choose-agent)
    ("m" "Model" drover-prompt-choose-model)
    ("e" "Effort" drover-prompt-choose-effort)
    ("d" "Directory" drover-prompt-choose-directory)]]
  (interactive)
  (transient-setup 'drover-prompt-options-menu nil nil
                   :display-action drover-menu-display-action))

(defvar-keymap drover-prompt-target-map
  :doc "Keys of the minibuffer choosing where the prompt goes.
The key of `drover-dispatch' opens `drover-prompt-options-menu' there."
  "<remap> <drover-dispatch>" #'drover-prompt-options-menu)

;;;; Choosing where the prompt goes

(defun drover-prompt--target-sessions ()
  "Return the running sessions the prompt may go to, with their buffers.
The value is a list of (SESSION . BUFFER): the sessions whose backend
takes prompts (:send) and that have a live buffer, the current
buffer's first, then in the order of `buffer-list'."
  (let ((ranks (make-hash-table :test #'eq))
        (rank 0)
        (current (current-buffer)))
    (dolist (buffer (buffer-list))
      (puthash buffer (cl-incf rank) ranks))
    (puthash current 0 ranks)
    (sort (delq nil (mapcar (lambda (session)
                              (when-let* (((drover-supports-p session :send))
                                          (buffer (drover-session-buffer session)))
                                (cons session buffer)))
                            (drover-sessions)))
          (lambda (a b)
            (< (gethash (cdr a) ranks most-positive-fixnum)
               (gethash (cdr b) ranks most-positive-fixnum))))))

(defun drover-prompt--new-targets ()
  "Return the entries of `drover-prompt-new-targets' whose feature is installed."
  (seq-filter (lambda (entry)
                (let ((feature (plist-get entry :feature)))
                  (or (null feature) (featurep feature) (locate-library (symbol-name feature)))))
              drover-prompt-new-targets))

(defun drover-prompt--entry-options (target current buffer)
  "Return the model and variant TARGET runs once sent the prompt BUFFER.
TARGET is a running session, whose CURRENT options its backend's
:options told, or an entry of `drover-prompt-new-targets'.  The choices
of BUFFER apply as sending applies them: a model the engine cannot run
drops both model and variant; a new session starts with them, resolved
by its engine; a running session that cannot change its variant keeps
it; one whose engine runs a model's default variant when switched to
it (:model-resets-variant) does so.  The value is an entry plist,
\(:model :model-explicit-p :variant :variant-explicit-p), with the keys
known only; nil for a new session without a prompt buffer."
  (let* ((session (and (drover-session-p target) target))
         (engine (if session (drover-session-engine session) (plist-get target :engine)))
         (live (buffer-live-p buffer))
         (picked (and live (buffer-local-value 'drover-prompt-picked-model buffer)))
         (model (and picked (drover-prompt--runs-p picked engine) picked))
         (variant (and live (or model (not picked))
                       (buffer-local-value 'drover-prompt-picked-variant buffer))))
    (cond
     ((and (not session) live)
      (let ((resolved (drover-prompt-engine-options
                       engine (buffer-local-value 'drover-prompt-picked-agent buffer)
                       model variant buffer)))
        (list :model (plist-get resolved :model) :model-explicit-p (and model t)
              :variant (plist-get resolved :variant) :variant-explicit-p (and variant t))))
     ((not session) nil)
     ((not (memq :variant (if (drover-supports-p session :settable)
                              (drover-backend-call session :settable)
                            '(:agent :model :variant))))
      (if model
          (list :model model :model-explicit-p t :variant (plist-get current :variant))
        current))
     (model
      (list :model model :model-explicit-p t
            :variant (or variant
                         (if (drover-engine-property engine :model-resets-variant)
                             (drover-model-default-variant model)
                           (plist-get current :variant)))
            :variant-explicit-p (and variant t)))
     (variant
      (list :model (plist-get current :model) :variant variant :variant-explicit-p t))
     (t current))))

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
         (label (drover-backend-label backend)))
    (if (or (null engine) (string= (drover-engine-label engine) label))
        label
      (format "%s (%s)" label (drover-engine-label engine)))))

(defun drover-prompt--candidates (sessions targets)
  "Return the candidates of the target picker, as an alist (NAME . ENTRY).
SESSIONS are the running sessions, as (SESSION . BUFFER), named after
their buffer; TARGETS the entries of `drover-prompt-new-targets' to
offer.  ENTRY is a plist: :target, the session or new-target entry;
:current, what a session runs now (see :options); :directory, where it
runs or would start.  Each NAME carries its target as its
`drover-prompt-target' property."
  (let ((taken (make-hash-table :test #'equal))
        (prompt (drover-prompt-buffer))
        candidates)
    (cl-flet ((add (base entry)
                (let ((name base)
                      (counter 1))
                  (while (gethash name taken)
                    (setq counter (1+ counter)
                          name (format "%s <%d>" base counter)))
                  (puthash name t taken)
                  (push (cons (propertize name 'drover-prompt-target (plist-get entry :target)) entry)
                        candidates))))
      (pcase-dolist (`(,session . ,buffer) sessions)
        (add (buffer-name buffer)
             (list :target session
                   :current (and (drover-supports-p session :options)
                                 (drover-with-reporting "session options"
                                   (drover-backend-call session :options)))
                   :directory (or (drover-session-directory session)
                                  (buffer-local-value 'default-directory buffer)))))
      (dolist (target targets)
        (add (format "[New %s…]" (or (plist-get target :label) (plist-get target :engine)))
             (list :target target
                   :directory (and prompt (buffer-local-value 'default-directory prompt))))))
    (nreverse candidates)))

(defun drover-prompt--annotator (candidates)
  "Return the annotation function of the target picker's CANDIDATES.
An annotation shows the status of a session, its backend and engine,
the model and variant it would run with the prompt's choices, computed
anew each time so that choices made in the picker show, its directory
and why it has its status."
  (let ((width (apply #'max 0 (mapcar (lambda (candidate) (string-width (car candidate)))
                                      candidates))))
    (lambda (name)
      (when-let* ((entry (cdr (assoc name candidates))))
        (let* ((target (plist-get entry :target))
               (session (and (drover-session-p target) target))
               (options (drover-prompt--options-label
                         (drover-prompt--entry-options target (plist-get entry :current)
                                                       (drover-prompt-buffer))))
               (directory (plist-get entry :directory))
               (summary (and session (drover-session-summary session))))
          (concat (make-string (max 0 (- width (string-width name))) ?\s)
                  "  " (if session (opencode-session-status-glyph (drover-session-status session)) " ")
                  " " (propertize (format "[%s]" (drover-prompt--target-label target))
                                  'face 'drover-dim)
                  (and options (concat "  " options))
                  (and directory
                       (concat "  " (propertize (abbreviate-file-name directory) 'face 'drover-dim)))
                  (and summary
                       (concat "  " (propertize summary 'face 'completions-annotations)))))))))

(defun drover-prompt--target-predicate (candidates)
  "Return the predicate hiding the new sessions of CANDIDATES that cannot start.
A new session's engine must be able to run the model chosen in the
prompt; running sessions always show, as the user may send to one
anyway, the model left out."
  (lambda (candidate)
    (let* ((name (if (consp candidate) (car candidate) candidate))
           (target (plist-get (cdr (assoc name candidates)) :target))
           (prompt (drover-prompt-buffer))
           (model (and prompt (buffer-local-value 'drover-prompt-picked-model prompt))))
      (or (drover-session-p target)
          (null model)
          (drover-prompt--runs-p model (plist-get target :engine))))))

(defun drover-prompt--setup-picker (keymap)
  "Set up the minibuffer of the target picker, with KEYMAP over its own keys.
KEYMAP is nil for none.  The minibuffer shows what the prompt runs."
  (when keymap
    (use-local-map (make-composed-keymap keymap (current-local-map))))
  (setq-local drover-prompt--picker t)
  (add-hook 'minibuffer-exit-hook #'drover-prompt--remove-minibuffer-overlay nil t)
  (when-let* ((prompt (drover-prompt-buffer)))
    (drover-prompt--update-minibuffer prompt)))

(defun drover-prompt-read-target (prompt &rest keys)
  "Read where to send the prompt, with PROMPT.
The candidates are the running sessions whose backend takes prompts
and that have a live buffer, the current buffer's first, then the most
recently used, each annotated with its backend, what it runs and its
status, then the new sessions of `drover-prompt-new-targets' whose
feature is installed and whose engine can run the model chosen.
Return the `drover-session' chosen, or for a new session its entry of
`drover-prompt-new-targets', a plist (:backend BACKEND :engine ENGINE
...).  KEYS are:

 :existing-only  Non-nil to offer the running sessions only; signal a
                 `user-error' when there is none.
 :options-menu   Non-nil to offer `drover-prompt-options-menu' even
                 without new sessions.

Where new sessions are offered, or with :options-menu, the key of
`drover-dispatch' opens `drover-prompt-options-menu'."
  (let* ((existing-only (plist-get keys :existing-only))
         (sessions (drover-prompt--target-sessions))
         (prompt-buffer (drover-prompt-buffer)))
    (when (and existing-only (null sessions))
      (user-error "No running session takes prompts"))
    ;; New sessions are annotated with the engines' defaults.
    (when (and prompt-buffer (not existing-only))
      (with-current-buffer prompt-buffer
        (drover-prompt--refresh-volatile)))
    (let* ((targets (unless existing-only (drover-prompt--new-targets)))
           (candidates (or (drover-prompt--candidates sessions targets)
                           (user-error "No running session takes prompts, and none can start")))
           (names (mapcar #'car candidates))
           (annotate (drover-prompt--annotator candidates))
           (table (lambda (string predicate action)
                    (if (eq action 'metadata)
                        `(metadata (category . drover-target)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity)
                                   (annotation-function . ,annotate))
                      (complete-with-action action names string predicate))))
           (keymap (and (or targets (plist-get keys :options-menu)) drover-prompt-target-map))
           (choice (minibuffer-with-setup-hook (lambda () (drover-prompt--setup-picker keymap))
                     (completing-read prompt table
                                      (and targets (drover-prompt--target-predicate candidates))
                                      t))))
      (or (plist-get (cdr (assoc choice candidates)) :target)
          (user-error "No session %s" choice)))))

(provide 'drover-prompt)
;;; drover-prompt.el ends here
