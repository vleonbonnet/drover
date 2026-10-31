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
;; (`drover-prompt-dwim'), then sends it to a running session or a new
;; one of any backend and engine (`drover-prompt-submit').  The buffer
;; is in `drover-prompt-mode', which defines drover's prompt actions
;; (see `drover-prompt-actions'): send, clear, attach, copy-block,
;; open-link, and the choice of agent, model, effort and directory the
;; next session runs.
;;
;; The prompt knows no backend: it sends through their prompt
;; operations (:create :check :apply-options :send :options :settable,
;; see `drover-register-backend') and resolves what a new session would
;; run through the engines (see `drover-register-engine').  With an
;; active region, only the region is sent, and the rest stays.
;;
;; Phase 0 of the port: the public names below are frozen, their bodies
;; not written yet.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'drover-core)
(require 'drover-model)
(require 'drover-ui)
(require 'drover-keys)
(require 'drover-claude-code)
(require 'drover-codex)

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
without \"-mode\" or \"-ts-mode\"; see `drover-prompt-language'."
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

(defcustom drover-prompt-org-headings-start-tasks t
  "Whether `drover-prompt-dwim' on an Org heading starts a task prompt.
When non-nil, `drover-prompt-dwim' on a heading line, without an
active region nor a prefix argument, runs `drover-prompt-org-task'."
  :type 'boolean
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
  "Marker at the point the user left the prompt at, while it was hidden.")

;;;; The mode

(defvar-keymap drover-prompt-mode-map
  :doc "Keymap of `drover-prompt-mode', empty: prompt keys go through `drover-dispatch'.")

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
  (error "Not implemented"))

;;;; The buffer

(defun drover-prompt-buffer (&optional create)
  "Return the prompt buffer, or nil when there is none.
With CREATE, create it when missing, in `org-mode' and
`drover-prompt-mode', its header line showing the keys and what a new
session would run."
  (ignore create)
  (error "Not implemented"))

;;;###autoload
(defun drover-prompt-default-buffer ()
  "Return the prompt buffer, creating it undisplayed when missing.
A new one begins a prompt in the current buffer's directory, as
`drover-prompt-toggle' would.  It is the default of
`drover-default-prompt-buffer-function', so the agent, model, effort
and directory actions choose what the next prompt runs from any
buffer."
  (error "Not implemented"))

;;;###autoload
(defun drover-prompt-dwim (&optional plain)
  "Open the prompt, or add to it the context at point.
The first call from a buffer shows the prompt, begun in that buffer's
directory, with a link to the line at point and the active region
quoted in a code block; later calls add such a block at the point the
user left the prompt at.  On an Org heading line, without an active
region, start a prompt for that task instead (see
`drover-prompt-org-task'), when `drover-prompt-org-headings-start-tasks'
says so, unless PLAIN, the prefix argument, asks for the heading line
as plain context.  Signal a `user-error' in the prompt buffer itself."
  (interactive "P")
  (ignore plain)
  (error "Not implemented"))

(defun drover-prompt-add-context ()
  "Open the prompt, or add to it the context at point.
As `drover-prompt-dwim' does, without its Org task case.  The context
is the active region, else the line at point, formatted by
`drover-prompt-format-context'."
  (error "Not implemented"))

;;;###autoload
(defun drover-prompt-toggle ()
  "Show or hide the prompt, adding nothing to it.
A shown prompt, or the current buffer when it is the prompt, is hidden;
otherwise the prompt is shown, created empty when missing.  An empty
prompt begins in the current buffer's directory."
  (interactive)
  (error "Not implemented"))

;;;###autoload
(defun drover-prompt-org-task ()
  "Start a prompt for the Org task at point, the heading point is under.
The prompt replaces the prompt's text, after asking when some is being
written: `drover-prompt-task-format' with an `id:' link to the task.
It picks the agent `drover-prompt-task-agent' and titles the new
session after the heading; picks made on an empty prompt stay."
  (interactive)
  (error "Not implemented"))

;;;; Prompt actions

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
  (error "Not implemented"))

(defun drover-prompt-erase ()
  "Erase the text of the prompt, region or not.
The buffer, its window and the agent, model, effort and directory
chosen stay, for the next prompt.  This is the prompt action `clear'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-insert-attachment ()
  "Insert at point the context of a buffer or of a project file.
Buffers come first, most recently used first, then the files of the
project of the prompt's directory.  A buffer's whole text is quoted as
`drover-prompt-dwim' quotes a region; a file is named by a link, without
its text, which agents read themselves.  An active region is refused,
being prompt text already.  This is the prompt action `attach'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-copy-src-block ()
  "Copy the contents of the code or example block at point to the kill ring.
The block's escaping and common indentation are removed, as
`drover-prompt-to-markdown' removes them.  This is the prompt action
`copy-block'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-choose-agent ()
  "Choose the agent the prompt runs, among those the engines offer.
With two agents, switch to the other without asking.  This is the
prompt action `agent'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-choose-model ()
  "Choose the model the prompt runs, among those the engines offer.
A variant the new model lacks is dropped.  This is the prompt action
`model'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-choose-effort ()
  "Choose the variant, the reasoning effort, of the prompt's model.
This is the prompt action `effort'."
  (interactive)
  (error "Not implemented"))

(defun drover-prompt-choose-directory ()
  "Choose the directory a new session of the prompt starts in.
The engines' defaults are read again for it.  This is the prompt action
`directory'."
  (interactive)
  (error "Not implemented"))

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

;;;; Text

(defun drover-prompt-format-context (buffer region language)
  "Return the Org context of BUFFER at point: a link, then REGION quoted.
The link names the file and line at point, or the buffer and line.
REGION, a string or nil, goes in a code block of LANGUAGE, escaped so
that no line of it ends the block or opens a heading, without the blank
lines around it, and from a terminal without trailing spaces."
  (ignore buffer region language)
  (error "Not implemented"))

(defun drover-prompt-language (mode)
  "Return the language of code blocks quoting a buffer in major MODE.
It is that of `drover-prompt-mode-languages', else MODE without
\"-ts-mode\" or \"-mode\"."
  (ignore mode)
  (error "Not implemented"))

(defun drover-prompt-to-markdown (text)
  "Return Org TEXT with its code and example blocks as Markdown fences.
Block contents are unescaped and lose their common indentation, as an
Org export does, then are indented to the block's column, so a block
in a list item stays in it; a fence is longer than any run of
backticks in its content.  Everything else is kept as it is."
  (ignore text)
  (error "Not implemented"))

(defun drover-prompt-content (&optional buffer)
  "Return what the prompt BUFFER would send, or nil when it is blank.
BUFFER defaults to the prompt buffer.  The value is a plist: :org, the
Org text, trimmed; :text, it in Markdown; :start and :end, markers
around it in BUFFER; :region, non-nil when it is the active region
rather than the whole buffer."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-prompt-consume (content &optional buffer)
  "Remove CONTENT, as `drover-prompt-content' returned it, from the prompt BUFFER.
BUFFER defaults to the prompt buffer.  A region is deleted and the mark
deactivated, the buffer and its window kept, unless only blanks remain;
otherwise the prompt is done with: its buffer is killed and its window
deleted."
  (ignore content buffer)
  (error "Not implemented"))

;;;; Options

(defun drover-prompt-explicit-options (&optional buffer)
  "Return the options chosen in the prompt BUFFER, as an options plist.
BUFFER defaults to the prompt buffer.  The plist is (:agent AGENT
:model MODEL :variant VARIANT), each nil when not chosen; see
`drover-model'."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-prompt-options-for (session options &optional warn)
  "Return OPTIONS, an options plist, as SESSION can run them.
A model the engine of SESSION cannot run (see `drover-model-runs-on-p')
is dropped, and its variant with it; with WARN, that is reported with
`drover-warn'."
  (ignore session options warn)
  (error "Not implemented"))

(defun drover-prompt-apply-options (session options then)
  "Apply OPTIONS, an options plist, to the running SESSION, then call THEN.
OPTIONS are filtered for SESSION as `drover-prompt-options-for' does,
with a warning, then given to its backend's :apply-options; without
it, THEN is called at once."
  (ignore session options then)
  (error "Not implemented"))

(defun drover-prompt-engine-options (engine agent model variant &optional buffer)
  "Return the resolved options a new session of ENGINE would run.
AGENT, MODEL and VARIANT are the explicit choices; the engine resolves
them with the defaults the prompt BUFFER holds for it, BUFFER
defaulting to the current one.  See `drover-engine-resolve'."
  (ignore engine agent model variant buffer)
  (error "Not implemented"))

(defun drover-prompt-inherited-options (&optional buffer)
  "Return the resolved options a new session would run without any choice.
The engine is the one that took a prompt last, else
`drover-prompt-default-engine'.  BUFFER is the prompt buffer, the
current one by default."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-prompt-effective-options (&optional buffer)
  "Return what the prompt BUFFER shows it runs, as a display plist.
BUFFER defaults to the current one.  The plist holds the resolved
:agent, :model and :variant, whether each was chosen
\(:agent-explicit-p, :model-explicit-p, :variant-explicit-p), the
:directory and whether it was chosen (:directory-explicit-p)."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-prompt-option-display (value explicit-p)
  "Return VALUE as a string in the face of a chosen or an inherited setting.
The face is `drover-prompt-picked' when EXPLICIT-P, else
`drover-prompt-inherited'."
  (ignore value explicit-p)
  (error "Not implemented"))

;;;; Choosing where the prompt goes

(defun drover-prompt-read-target (prompt &rest keys)
  "Read where to send the prompt, with PROMPT.
The candidates are the running sessions with a live buffer, the current
buffer's first, then the most recently used, each annotated with its
backend, what it runs and its status, then the new sessions of
`drover-prompt-new-targets' whose engine can run the model chosen.
Return the `drover-session' chosen, or for a new session the plist
\(:backend BACKEND :engine ENGINE).  KEYS are:

 :existing-only  Non-nil to offer the running sessions only; signal a
                 `user-error' when there is none.
 :options-menu   Non-nil to offer `drover-prompt-options-menu' even
                 without new sessions."
  (ignore prompt keys)
  (error "Not implemented"))

(provide 'drover-prompt)
;;; drover-prompt.el ends here
