;;; drover-claude-code.el --- The Claude Code engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: tools

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

;; The `claude-code' engine: what a new Claude Code session starts with,
;; read from the CLI's local state as the CLI itself resolves it (start
;; flags, environment, settings layers and its cached model catalog),
;; its models, when it last took a prompt, and where it keeps the
;; transcripts and identifiers of its sessions.  It needs no front end:
;; `drover-claude-code-ide' and `drover-agent-shell' show its sessions.
;;
;; Phase 0 of the port: the public names below are frozen, their bodies
;; not written yet.

;;; Code:

(require 'drover-core)
(require 'drover-model)

;;;; Options

(defcustom drover-claude-code-home
  (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude"))
  "Claude Code's user configuration directory."
  :type 'directory
  :group 'drover)

(defcustom drover-claude-code-managed-directory
  (pcase system-type
    ('windows-nt "C:/Program Files/ClaudeCode")
    ('darwin "/Library/Application Support/ClaudeCode")
    (_ "/etc/claude-code"))
  "Directory of Claude Code's file-based managed settings."
  :type 'directory
  :group 'drover)

(defconst drover-claude-code-effort-levels '(low medium high xhigh max)
  "Claude Code's effort levels, lowest first.")

(defcustom drover-claude-code-user-effort-models
  '("haiku-3-5" "sonnet-3-5" "sonnet-3-7" "haiku-4-5" "sonnet-4" "sonnet-4-5"
    "sonnet-4-6" "sonnet-5" "opus-4" "opus-4-1" "opus-4-5" "opus-4-6" "opus-4-7"
    "opus-4-8" "opus-5" "fable-5" "fable-5-1" "mythos-5" "mythos-5-1")
  "Keys of the models a top-level `effortLevel' of user settings applies to.
It mirrors Claude Code 2.1.296: later models ignore it, and start at
their own default until a level is saved for them.  Keys are
\"FAMILY-VERSION\", a zero minor dropped, such as \"opus-4-5\"."
  :type '(repeat string)
  :group 'drover)

(defcustom drover-claude-code-alias-families
  '(("best" . "opus") ("opusplan" . "sonnet"))
  "Claude Code model aliases selecting a model of another family.
Each entry is (ALIAS . FAMILY); every other alias names its own family.
`opusplan' runs Sonnet outside plan mode."
  :type '(alist :key-type (string :tag "Alias") :value-type (string :tag "Family"))
  :group 'drover)

(defcustom drover-claude-code-agent-modes '(("plan" . "plan") ("build" . "auto"))
  "Agents Claude Code runs as permission modes, as (AGENT . MODE).
Planning and building are permission modes in Claude Code rather than
agents, and its `--agent' refuses a name no agent definition provides."
  :type '(alist :key-type (string :tag "Agent") :value-type (string :tag "Permission mode"))
  :group 'drover)

(defcustom drover-claude-code-oauth-catalog nil
  "Whether Claude Code's models may come from Anthropic's model list.
When non-nil and no live session offers Claude Code's models, they are
fetched from the Anthropic API with the OAuth token the CLI keeps in
`drover-claude-code-home', once per Emacs session.  Off by default, as
it calls a vendor API with your credential."
  :type 'boolean
  :group 'drover)

(defvar drover-claude-code-start-flags-function nil
  "Function of no argument returning the flags of new Claude Code sessions.
They are a string of command line flags, such as \"--model opus\", or
nil, and take precedence over settings.  `drover-claude-code-ide' sets
it.")

(defvar drover-claude-code-project-directory-function nil
  "Function of a DIRECTORY returning where a session for it starts, or nil.
Claude Code reads project settings there.  Nil starts in DIRECTORY
itself.  `drover-claude-code-ide' sets it.")

;;;; Agents

(defun drover-claude-code-agent-mode (agent)
  "Return the permission mode Claude Code runs AGENT as, or nil.
See `drover-claude-code-agent-modes'."
  (cdr (assoc agent drover-claude-code-agent-modes)))

;;;; Sessions on disk

(defun drover-claude-code-projects-directory ()
  "Return the directory of Claude Code's transcripts, one subdirectory per project."
  (error "Not implemented"))

(defun drover-claude-code-sessions-directory ()
  "Return the directory where Claude Code records its running sessions by PID."
  (error "Not implemented"))

(defun drover-claude-code-encode-directory (dir)
  "Return the name of the subdirectory of transcripts of sessions in DIR.
Claude Code resolves symbolic links, drops the trailing slash and
replaces every character that is not alphanumeric with \"-\"."
  (ignore dir)
  (error "Not implemented"))

(defun drover-claude-code-decode-bucket (encoded)
  "Return the directory ENCODED, a subdirectory name of transcripts, stands for.
Encoding loses characters, so the directory is found by listing the
file system level by level, encoding each name.  Return it with a
trailing slash, or nil."
  (ignore encoded)
  (error "Not implemented"))

(defun drover-claude-code-session-directory (session-id)
  "Return the directory session SESSION-ID ran in, or nil.
It is decoded from the subdirectory of transcripts holding its
transcript."
  (ignore session-id)
  (error "Not implemented"))

(defun drover-claude-code-pid-session-id (pid)
  "Return the identifier of the session the Claude Code process PID runs, or nil.
The CLI records it, by PID, under `drover-claude-code-sessions-directory'."
  (ignore pid)
  (error "Not implemented"))

(defun drover-claude-code-newest-session-id (directory)
  "Return the identifier of the session last written in DIRECTORY, or nil.
It is the newest transcript of DIRECTORY's subdirectory of transcripts."
  (ignore directory)
  (error "Not implemented"))

(defun drover-claude-code-transcript (id dir)
  "Return the transcript file of session ID, started in DIR."
  (ignore id dir)
  (error "Not implemented"))

(defun drover-claude-code-last-turn (file)
  "Return (MODEL-ID . EFFORT) of the newest main turn transcript FILE records.
Only the end of the file is read.  EFFORT is a symbol, nil for a model
without effort.  Nil when no turn is found."
  (ignore file)
  (error "Not implemented"))

;;;; Startup defaults

(defun drover-claude-code-settings-layers (dir)
  "Return Claude Code's settings for a session in DIR, highest precedence first.
Each entry is (SOURCE . SETTINGS), SOURCE being `policy', `local',
`project' or `user'.  In the home directory, the project settings are
the user ones, read once as user settings."
  (ignore dir)
  (error "Not implemented"))

(defun drover-claude-code-catalog ()
  "Return the model catalog Claude Code last cached, or nil.
Its `config' lists every model's name, short name and effort options,
its `state' the default model and the default effort of each model."
  (error "Not implemented"))

(defun drover-claude-code-startup-model-id (layers catalog)
  "Return the identifier of the model a new session starts on, or nil.
Claude Code's precedence: --model in the start flags (see
`drover-claude-code-start-flags-function'), ANTHROPIC_MODEL, the first
of the settings LAYERS naming a `model', ANTHROPIC_DEFAULT_MODEL, then
CATALOG's default."
  (ignore layers catalog)
  (error "Not implemented"))

(defun drover-claude-code-startup-mode (layers)
  "Return the permission mode a new session starts in, or nil.
Claude Code's precedence: --permission-mode in the start flags, then
the first of the settings LAYERS with a `permissions.defaultMode'.
Project and local settings cannot start `auto' nor
`bypassPermissions'."
  (ignore layers)
  (error "Not implemented"))

(defun drover-claude-code-effort (id layers catalog)
  "Return the effort a new session runs model ID at, a string, or nil.
Claude Code's precedence: CLAUDE_CODE_EFFORT_LEVEL (`auto' being the
model's default), --effort in the start flags, the first of the
settings LAYERS with a level for the model, then CATALOG's default for
it.  The level is lowered to any `maxEffortLevel', and to one the model
supports; a model without effort options runs none."
  (ignore id layers catalog)
  (error "Not implemented"))

(defun drover-claude-code-model (id layers catalog)
  "Return the `drover-model' of Claude Code model ID.
CATALOG names it and offers its effort options as variants; its default
variant is the effort LAYERS and CATALOG give it.  Its engines are
those of `drover-model-engines-for' for its identifier."
  (ignore id layers catalog)
  (error "Not implemented"))

(defun drover-claude-code-defaults (dir)
  "Return what a new Claude Code session in DIR starts with, as a plist.
:model is its `drover-model', nil when nothing names one; :agent the
agent of `drover-claude-code-agent-modes' its starting permission mode
stands for, nil when none; :layers and :catalog let
`drover-claude-code-effort' answer for another model.  This is the
engine's :defaults data."
  (ignore dir)
  (error "Not implemented"))

(provide 'drover-claude-code)
;;; drover-claude-code.el ends here
