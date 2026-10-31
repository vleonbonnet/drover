;;; drover-codex.el --- The Codex engine  -*- lexical-binding: t; -*-

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

;; The `codex' engine: what a new Codex session starts with, read from
;; its config.toml and models cache, and when it last took a prompt.
;;
;; Phase 0 of the port: the public names below are frozen, their bodies
;; not written yet.

;;; Code:

(require 'drover-core)
(require 'drover-model)

(defcustom drover-codex-home (expand-file-name (or (getenv "CODEX_HOME") "~/.codex"))
  "Codex's home directory."
  :type 'directory
  :group 'drover)

(defun drover-codex-config ()
  "Return Codex's string settings from config.toml, as (KEY . VALUE) pairs.
The keys of the selected `profile' table come before the top-level
ones, so `assoc' finds the value Codex applies.  Only plain strings are
read."
  (error "Not implemented"))

(defun drover-codex-effort (id config)
  "Return the reasoning effort a new Codex session runs model ID at, or nil.
It is CONFIG's `model_reasoning_effort', else the model's default in
Codex's models cache."
  (ignore id config)
  (error "Not implemented"))

(defun drover-codex-defaults ()
  "Return what a new Codex session starts with, as a plist.
:model is the `drover-model' of config.toml's model, nil when it names
none; :config lets `drover-codex-effort' answer for another model.  This
is the engine's :defaults data."
  (error "Not implemented"))

(provide 'drover-codex)
;;; drover-codex.el ends here
