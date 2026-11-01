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
;; its config.toml and models cache, and when it last took a prompt,
;; from the rollouts it writes.  Its models come from live sessions
;; only (see `drover-agent-shell'): the CLI has no command listing them.
;; Each model takes its name and reasoning levels from the models cache,
;; and from the levels Codex's ACP agent lists for each model
;; (`drover-codex-acp-models').

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'drover-core)
(require 'drover-model)

(defcustom drover-codex-home (expand-file-name (or (getenv "CODEX_HOME") "~/.codex"))
  "Codex's home directory."
  :type 'directory
  :group 'drover)

;;;; Defaults

(defun drover-codex-config ()
  "Return Codex's string settings from config.toml, as (KEY . VALUE) pairs.
The keys of the selected `profile' table come before the top-level
ones, so `assoc' finds the value Codex applies.  Only plain strings are
read."
  (let ((file (expand-file-name "config.toml" drover-codex-home))
        (tables nil)
        (table nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Enough TOML for the keys read: table headers, and keys with a
        ;; string value on one line.  Arrays of tables hold none of them.
        (while (not (eobp))
          (cond
           ((looking-at "[ \t]*\\[\\[")
            (setq table :array))
           ((looking-at "[ \t]*\\[[ \t]*\\([^]\n]+?\\)[ \t]*\\]")
            (setq table (replace-regexp-in-string "[\"']" "" (match-string 1))))
           ((looking-at "[ \t]*\\([A-Za-z0-9_-]+\\)[ \t]*=[ \t]*\\(?:\"\\([^\"\n]*\\)\"\\|'\\([^'\n]*\\)'\\)")
            (push (cons (match-string 1) (or (match-string 2) (match-string 3)))
                  (alist-get table tables nil nil #'equal))))
          (forward-line 1)))
      (let* ((top (alist-get nil tables))
             (profile (cdr (assoc "profile" top))))
        (append (and profile (alist-get (concat "profiles." profile) tables nil nil #'equal))
                top)))))

(defun drover-codex--catalog-model (id)
  "Return the entry Codex's models cache keeps for the model ID, or nil."
  (let ((cache (drover-read-json-cached (expand-file-name "models_cache.json" drover-codex-home))))
    (and (drover-json-object-p cache)
         (seq-find (lambda (model) (equal (alist-get 'slug model) id))
                   (alist-get 'models cache)))))

(defun drover-codex-effort (id config)
  "Return the reasoning effort a new Codex session runs model ID at, or nil.
It is CONFIG's `model_reasoning_effort', else the model's default in
Codex's models cache.  The effort is a symbol."
  (when-let* ((level (or (cdr (assoc "model_reasoning_effort" config))
                         (alist-get 'default_reasoning_level (drover-codex--catalog-model id))))
              ((stringp level)))
    (intern level)))

(defun drover-codex-model (id &optional config name)
  "Return the `drover-model' of Codex model ID.
Codex's models cache names it and gives its reasoning levels as
variants, unknown when the cache does not list it; its default variant
is the effort a new session runs it at with CONFIG, Codex's settings
\(see `drover-codex-effort').  NAME names a model the cache does not."
  (let ((entry (drover-codex--catalog-model id))
        (provider (cdr (assoc "model_provider" config))))
    (drover-model-create
     :provider-name (if (member provider '(nil "openai")) "OpenAI" provider)
     :id id
     :name (or (alist-get 'display_name entry) name)
     :variants (mapcar (lambda (level) (list (intern (alist-get 'effort level))))
                       (seq-filter (lambda (level) (stringp (alist-get 'effort level)))
                                   (alist-get 'supported_reasoning_levels entry)))
     :default-variant (drover-codex-effort id config)
     :variants-known (and entry t)
     :engines (delete-dups (cons 'codex (drover-model-engines-for nil id))))))

(defun drover-codex-acp-model-efforts (values)
  "Return the reasoning levels of each model VALUES name.
The value is an alist (ID . LEVELS).  VALUES are the model identifiers
Codex's ACP agent lists apart from its model option, one per model and
level, as \"ID[LEVEL]\"; LEVELS are symbols, in order.  Other values
are ignored."
  (let ((efforts nil))
    (dolist (value values)
      (when (and (stringp value) (string-match "\\`\\([^[]+\\)\\[\\([^]]+\\)\\]\\'" value))
        (let ((id (match-string 1 value))
              (level (intern (match-string 2 value))))
          (if-let* ((entry (assoc id efforts)))
              (unless (memq level (cdr entry))
                (setcdr entry (append (cdr entry) (list level))))
            (push (list id level) efforts)))))
    (nreverse efforts)))

(defun drover-codex-acp-models (options current levels efforts &optional config)
  "Return the models an ACP session of Codex lists.
OPTIONS are the values of the session's model option, in order, as
\(VALUE . NAME), each VALUE a model identifier.  The model the session
runs, CURRENT, has the variants LEVELS, the reasoning levels the session
offers, a list of symbols; the others those EFFORTS, as
`drover-codex-acp-model-efforts' returns them, give, else Codex's
models cache.  CONFIG is Codex's settings, by default those of its
config.toml, which give default variants."
  (let ((config (or config (drover-codex-config))))
    (drover-model-merge-all
     (mapcar (pcase-lambda (`(,value . ,name))
               (let ((model (drover-codex-model value config name))
                     (known (if (equal value current) levels (alist-get value efforts nil nil #'equal))))
                 (when (or known (equal value current))
                   (setf (drover-model-variants model) (mapcar #'list known)
                         (drover-model-variants-known model) t))
                 model))
             options))))

(defun drover-codex-defaults ()
  "Return what a new Codex session starts with, as a plist.
:model is the `drover-model' of config.toml's model, nil when it names
none; :config lets `drover-codex-effort' answer for another model.  This
is the engine's :defaults data."
  (let* ((config (drover-codex-config))
         (id (cdr (assoc "model" config))))
    (list :model (and id (drover-codex-model id config))
          :config config)))

(defun drover-codex--fetch-defaults (_directory callback)
  "Call CALLBACK with the defaults of a new Codex session.
They are read at once; a failure is reported, and CALLBACK not called."
  (when-let* ((defaults (condition-case err
                            (drover-codex-defaults)
                          (error
                           (drover-warn "Could not read Codex's defaults: %s" (error-message-string err))
                           nil))))
    (funcall callback defaults)))

(defun drover-codex--resolve (defaults agent model variant)
  "Return the options a new Codex session runs, from DEFAULTS.
AGENT, MODEL and VARIANT are the explicit choices, each nil when not
made; DEFAULTS is a plist of `drover-codex-defaults', or nil.  The
model is config.toml's unless chosen, and the variant the effort Codex
gives the model; the agent is the one chosen."
  (let ((model (or model (plist-get defaults :model))))
    (list :agent agent
          :model model
          :variant (or variant
                       (and model
                            (drover-codex-effort (drover-model-id model) (plist-get defaults :config)))))))

(defun drover-codex--last-used ()
  "Return when a Codex session last wrote its rollout, as a float time, or nil.
Rollouts sit under sessions/YEAR/MONTH/DAY of the day their session
started, and a session can outlive its day, so every one is checked."
  (let ((latest nil))
    (dolist (day (ignore-errors
                   (mapcan (lambda (month) (directory-files month t "\\`[0-9]+\\'"))
                           (mapcan (lambda (year) (directory-files year t "\\`[0-9]+\\'"))
                                   (directory-files (expand-file-name "sessions" drover-codex-home)
                                                    t "\\`[0-9]+\\'")))))
      (dolist (entry (ignore-errors
                       (directory-files-and-attributes day nil "\\`rollout-.*\\.jsonl\\'" t)))
        (let ((time (float-time (file-attribute-modification-time (cdr entry)))))
          (when (or (null latest) (> time latest))
            (setq latest time)))))
    latest))

;;;; Registration

(defun drover-codex--register ()
  "Register the `codex' engine."
  (drover-register-engine 'codex
                          :label "Codex"
                          :defaults #'drover-codex--fetch-defaults
                          :volatile t
                          :resolve #'drover-codex--resolve
                          :last-used #'drover-codex--last-used))

(drover-codex--register)

(defun drover-codex-unload-function ()
  "Forget the `codex' engine, and its model sources."
  (drover-unregister-engine 'codex)
  nil)

(provide 'drover-codex)
;;; drover-codex.el ends here
