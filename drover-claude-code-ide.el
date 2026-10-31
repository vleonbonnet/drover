;;; drover-claude-code-ide.el --- Claude Code terminals in drover  -*- lexical-binding: t; -*-

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

;; The `claude-code-ide' backend: the Claude Code sessions of
;; claude-code-ide.el, terminals running the CLI, and the terminals it
;; no longer tracks whose buffer names say they are Claude's.  Their
;; statuses come from Ghostel's program status; vterm and eat terminals
;; have none and show idle.  Sessions run the `claude-code' engine.
;;
;; `drover' loads this file once claude-code-ide.el is loaded.
;;
;; Phase 0 of the port: the public names below are frozen, their bodies
;; not written yet.

;;; Code:

(require 'drover-core)
(require 'drover-model)
(require 'drover-claude-code)
(require 'claude-code-ide)

(defvar-local drover-claude-code-ide-session-id nil
  "Identifier of the Claude Code session this terminal started, or nil.
Set when drover starts the session with an explicit --session-id, or
when the session's transcript is found after it started.")

(defcustom drover-claude-code-ide-terminal-modes '(ghostel-mode vterm-mode eat-mode)
  "Major modes of the terminals Claude Code sessions run in."
  :type '(repeat symbol)
  :group 'drover)

(defcustom drover-claude-code-ide-buffer-regexp "\\`\\*claude\\*"
  "Regexp matching the names of the terminal buffers of Claude Code sessions.
A live terminal claude-code-ide.el does not track is a session when its
name matches.  A session's title is its buffer name without the match."
  :type 'regexp
  :group 'drover)

(defcustom drover-claude-code-ide-rename-format "*claude* %s"
  "Format of the buffer name of a renamed session; %s stands for the title."
  :type 'string
  :group 'drover)

(defvar drover-claude-code-ide-input-regexp
  ;; The no-break space is spelled by code point: a literal one would not
  ;; survive `whitespace-cleanup'.
  (concat "^\\(?:❯\\|>" (string #xa0) "\\)")
  "Regexp matching the input caret of Claude Code's interface at a line start.
The CLI draws \">\" and a no-break space, older ones \"❯\"; requiring
the no-break space keeps Markdown quotes of the transcript from
matching.")

(defun drover-claude-code-ide-ready-p (buffer)
  "Return non-nil when the Claude Code interface in BUFFER reads input.
Its input caret (see `drover-claude-code-ide-input-regexp') shows only
once it does."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-claude-code-ide-send-text (buffer text)
  "Submit TEXT as a prompt to the Claude Code session of terminal BUFFER.
TEXT is pasted, bracketed, then submitted.  A session not ready yet
gets it once ready (see `drover-claude-code-ide-ready-p'); after 30
seconds, it is given up, with a message saying the prompt is in the
kill ring."
  (ignore buffer text)
  (error "Not implemented"))

(defun drover-claude-code-ide-buffer-for-directory (dir)
  "Return the live terminal of a Claude Code session for DIR, or nil.
With several, the most recently used."
  (ignore dir)
  (error "Not implemented"))

(defun drover-claude-code-ide-live-session-id (&optional buffer)
  "Return the identifier of the session terminal BUFFER runs, or nil.
BUFFER defaults to the current one.  It is the one the CLI records for
the terminal's process (see `drover-claude-code-pid-session-id'), else
`drover-claude-code-ide-session-id', so that a session resumed,
continued or switched within the CLI is followed; nothing is guessed."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-claude-code-ide-current-session-id (&optional buffer)
  "Return the identifier of the session terminal BUFFER runs, or a guess, or nil.
BUFFER defaults to the current one.  It is
`drover-claude-code-ide-live-session-id', else the newest transcript of
the terminal's directory."
  (ignore buffer)
  (error "Not implemented"))

(defun drover-claude-code-ide-start (directory &optional flags)
  "Start a Claude Code session in DIRECTORY, and return its terminal buffer.
FLAGS is a string of command line flags, by default
`claude-code-ide-cli-extra-flags'; unless they resume, continue or name
a session already, \"--session-id UUID\" is added, and the buffer's
`drover-claude-code-ide-session-id' set to UUID.  It starts a new
instance even where a session runs."
  (ignore directory flags)
  (error "Not implemented"))

(defun drover-claude-code-ide-resume (session-id directory)
  "Resume Claude Code session SESSION-ID in DIRECTORY, and return its terminal.
See `drover-claude-code-ide-start'."
  (ignore session-id directory)
  (error "Not implemented"))

(provide 'drover-claude-code-ide)
;;; drover-claude-code-ide.el ends here
