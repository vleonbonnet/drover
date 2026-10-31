;;; drover-agent-shell.el --- agent-shell sessions in drover  -*- lexical-binding: t; -*-

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

;; The `agent-shell' backend: the shells of agent-shell.el, each running
;; an engine through the Agent Client Protocol: `opencode',
;; `claude-code', `codex' or `gemini-cli', as the shell's configuration
;; names it.  Statuses come from the shells' events.  A live shell also
;; offers the models of its engine.
;;
;; `drover' loads this file once agent-shell.el is loaded.
;;
;; Phase 0 of the port: the public names below are frozen, their bodies
;; not written yet.

;;; Code:

(require 'drover-core)
(require 'drover-model)
(require 'drover-claude-code)
(require 'agent-shell)

(defcustom drover-agent-shell-configs
  '((claude-code . agent-shell-anthropic-make-claude-code-config)
    (opencode . agent-shell-opencode-make-agent-config)
    (codex . agent-shell-openai-make-codex-config)
    (gemini-cli . agent-shell-google-make-gemini-config))
  "Functions making the agent-shell configuration of each engine.
Each entry is (ENGINE . FUNCTION), FUNCTION of no argument returning
the configuration a new shell running ENGINE starts from."
  :type '(alist :key-type (symbol :tag "Engine") :value-type (function :tag "Configuration"))
  :group 'drover)

(defcustom drover-agent-shell-engines '(opencode claude-code codex)
  "Engines whose sessions drover starts in agent-shell.
The prompt offers a new shell of each; `drover-new-session' asks which
when there are several."
  :type '(repeat symbol)
  :group 'drover)

(defun drover-agent-shell-start-config (base-config model-id agent)
  "Return agent-shell configuration BASE-CONFIG set to start on MODEL-ID as AGENT.
MODEL-ID is the identifier the engine takes, or nil; AGENT an agent
name, or nil.  OpenCode takes AGENT as its session mode; Claude Code
too when `drover-claude-code-agent-modes' runs it as a permission mode,
else as a session-start option; other engines ignore it.  BASE-CONFIG
is not modified, and returned as is when there is nothing to set."
  (ignore base-config model-id agent)
  (error "Not implemented"))

(defun drover-agent-shell-send-text (buffer text)
  "Submit TEXT as a prompt to the shell of BUFFER.
A shell still starting gets it once initialized; a busy one queues it;
a ready one takes it at once, without taking the focus."
  (ignore buffer text)
  (error "Not implemented"))

(provide 'drover-agent-shell)
;;; drover-agent-shell.el ends here
