;;; drover-claude-code-test.el --- Tests of the Claude Code engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

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

;; Every test reads Claude Code's local state from the throwaway homes
;; of `drover-test-with-homes', never the user's.

;;; Code:

(require 'drover-test-helpers)
(require 'drover-claude-code)

(defconst drover-claude-code-test-catalog
  "{\"catalog\": {\"surface\": \"cc\",
    \"config\": {\"models\": [
      {\"id\": \"claude-opus-5-5\", \"name\": \"Opus 5.5\", \"short_name\": \"Opus\",
       \"section\": \"main\", \"thinking\": {\"type\": \"effort\", \"effort_options\": [
         {\"id\": \"low\"}, {\"id\": \"medium\", \"badge\": {\"message\": \"Recommended\"}},
         {\"id\": \"high\"}, {\"id\": \"xhigh\"}, {\"id\": \"max\"}]}},
      {\"id\": \"claude-sonnet-5-5\", \"name\": \"Sonnet 5.5\", \"short_name\": \"Sonnet\",
       \"section\": \"main\", \"thinking\": {\"type\": \"effort\", \"effort_options\": [
         {\"id\": \"low\"}, {\"id\": \"medium\", \"badge\": {\"message\": \"Recommended\"}},
         {\"id\": \"high\"}]}},
      {\"id\": \"claude-opus-5\", \"name\": \"Opus 5\", \"short_name\": \"Opus\",
       \"section\": \"overflow\", \"thinking\": {\"type\": \"effort\", \"effort_options\": [
         {\"id\": \"low\"}, {\"id\": \"high\", \"badge\": {\"message\": \"Recommended\"}}]}},
      {\"id\": \"claude-opus-4-6\", \"name\": \"Opus 4.6\", \"short_name\": \"Opus\",
       \"section\": \"overflow\", \"thinking\": {\"type\": \"effort\", \"effort_options\": [
         {\"id\": \"low\"}, {\"id\": \"medium\"},
         {\"id\": \"high\", \"badge\": {\"message\": \"Recommended\"}}, {\"id\": \"max\"}]}},
      {\"id\": \"claude-haiku-4-5-20251001\", \"name\": \"Haiku 4.5\", \"short_name\": \"Haiku\",
       \"section\": \"overflow\", \"thinking\": {\"type\": \"none\"}}]},
    \"state\": {\"model\": \"claude-opus-5\", \"thinking_by_model\": [
      {\"id\": \"claude-sonnet-5-5\", \"thinking\": {\"type\": \"effort\", \"effort\": \"low\"}}]}}}"
  "A cached Claude Code model catalog.
Two Opus generations in different sections, a default effort the state
serves and a model without effort.")

(defun drover-claude-code-test-write-catalog ()
  "Write `drover-claude-code-test-catalog' where Claude Code caches its catalog."
  (drover-test-write (expand-file-name "cache/model-catalog/org-cc.json" drover-claude-code-home)
                     drover-claude-code-test-catalog))

(defmacro drover-claude-code-test-with-flags (flags &rest body)
  "Run BODY with FLAGS, a string, as the start flags of new sessions."
  (declare (indent 1) (debug t))
  `(let ((drover-claude-code-start-flags-function (lambda () ,flags)))
     ,@body))

;;;; Agents

(ert-deftest drover-claude-code-test-agent-modes ()
  "Plan and build are permission modes; other agents are not."
  (should (equal (drover-claude-code-agent-mode "plan") "plan"))
  (should (equal (drover-claude-code-agent-mode "build") "auto"))
  (should-not (drover-claude-code-agent-mode "reviewer"))
  (should-not (drover-claude-code-agent-mode nil))
  (should (equal (drover-claude-code--agents "/any/") '("plan" "build")))
  (let ((drover-claude-code-agent-modes '(("review" . "default"))))
    (should (equal (drover-claude-code--agents "/any/") '("review")))))

;;;; Startup defaults

(ert-deftest drover-claude-code-test-startup-defaults ()
  "Claude Code's startup model and effort follow its documented precedence."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home)))
      (make-directory project t)
      (drover-claude-code-test-write-catalog)
      (drover-test-write (expand-file-name "settings.json" drover-claude-code-home)
                         "{\"model\": \"opus\", \"effortLevel\": \"high\",
                           \"modelSettings\": {\"claude-opus-5-5\": {\"effortLevel\": \"xhigh\"}}}")
      (cl-flet ((model ()
                  (drover-claude-code-startup-model-id (drover-claude-code-settings-layers project)
                                                       (drover-claude-code-catalog)))
                (effort (id)
                  (drover-claude-code-effort id (drover-claude-code-settings-layers project)
                                             (drover-claude-code-catalog))))
        ;; An alias takes its main-section model, which has a saved level.
        (should (equal (model) "claude-opus-5-5"))
        (should (eq (effort "claude-opus-5-5") 'xhigh))
        ;; The user's top-level level skips newer models, which take the
        ;; default served, and reaches the listed ones.
        (should (eq (effort "claude-sonnet-5-5") 'low))
        (should (eq (effort "claude-opus-4-6") 'high))
        ;; A dated catalog identifier still marks Haiku 4.5 as without effort.
        (should-not (effort "claude-haiku-4-5"))
        ;; The environment wins, lowered to a level the model offers.
        (setenv "CLAUDE_CODE_EFFORT_LEVEL" "xhigh")
        (should (eq (effort "claude-opus-4-6") 'high))
        (setenv "CLAUDE_CODE_EFFORT_LEVEL" "auto")
        (should (eq (effort "claude-opus-5-5") 'medium))
        (setenv "CLAUDE_CODE_EFFORT_LEVEL" nil)
        (drover-claude-code-test-with-flags "--effort low --model=sonnet"
          (should (equal (model) "claude-sonnet-5-5"))
          (should (eq (effort "claude-opus-5-5") 'low)))
        ;; Without a front end, there are no start flags.
        (let ((drover-claude-code-start-flags-function nil))
          (should (equal (model) "claude-opus-5-5")))
        (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" "claude-opus-5")
        (should (equal (model) "claude-opus-5"))
        (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" nil)
        (setenv "ANTHROPIC_MODEL" "opus[1m]")
        (should (equal (model) "claude-opus-5-5[1m]"))
        (should (eq (effort "claude-opus-5-5[1m]") 'xhigh))
        (setenv "ANTHROPIC_MODEL" nil)
        ;; A project's top-level level outranks the user's per-model one,
        ;; and a local alias-keyed level outranks both, under its cap.
        (drover-test-write (expand-file-name ".claude/settings.json" project)
                           "{\"effortLevel\": \"low\"}")
        (should (eq (effort "claude-opus-5-5") 'low))
        (drover-test-write (expand-file-name ".claude/settings.local.json" project)
                           "{\"modelSettings\": {\"opus\": {\"effortLevel\": \"max\"}},
                             \"maxEffortLevel\": \"xhigh\"}")
        (should (eq (effort "claude-opus-5-5") 'xhigh))
        ;; Managed settings outrank every other layer.
        (drover-test-write (expand-file-name "managed-settings.json"
                                             drover-claude-code-managed-directory)
                           "{\"model\": \"sonnet\"}")
        (should (equal (model) "claude-sonnet-5-5"))
        ;; With no model named anywhere, the default served starts.
        (delete-file (expand-file-name "managed-settings.json" drover-claude-code-managed-directory))
        (drover-test-write (expand-file-name "settings.json" drover-claude-code-home) "{}")
        (should (equal (model) "claude-opus-5"))
        (let ((default (plist-get (drover-claude-code-defaults project) :model)))
          (should (equal (drover-model-label default) "Anthropic: Opus 5"))
          (should (equal (drover-model-variants default) '((low) (high))))
          ;; The project's top-level level still applies.
          (should (eq (drover-model-default-variant default) 'low))
          (should (equal (drover-model-engines default) '(claude-code))))))))

(ert-deftest drover-claude-code-test-startup-mode ()
  "Claude Code's starting permission mode follows its precedence and names an agent."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home)))
      (make-directory project t)
      (cl-flet ((mode ()
                  (drover-claude-code-startup-mode (drover-claude-code-settings-layers project)))
                (agent ()
                  (plist-get (drover-claude-code-defaults project) :agent)))
        (should-not (mode))
        (drover-test-write (expand-file-name "settings.json" drover-claude-code-home)
                           "{\"permissions\": {\"defaultMode\": \"plan\"}}")
        (should (equal (agent) "plan"))
        ;; A project's mode outranks the user's, even one no agent stands for.
        (drover-test-write (expand-file-name ".claude/settings.json" project)
                           "{\"permissions\": {\"defaultMode\": \"acceptEdits\"}}")
        (should (equal (mode) "acceptEdits"))
        (should-not (agent))
        ;; Local auto leaves the built-in default, not the next layer's mode.
        (drover-test-write (expand-file-name ".claude/settings.local.json" project)
                           "{\"permissions\": {\"defaultMode\": \"auto\"}}")
        (should-not (mode))
        (drover-test-write (expand-file-name ".claude/settings.local.json" project)
                           "{\"permissions\": {\"defaultMode\": \"bypassPermissions\"}}")
        (should (equal (mode) "default"))
        ;; Managed settings may start auto mode, and the start flag wins.
        (drover-test-write (expand-file-name "managed-settings.json"
                                             drover-claude-code-managed-directory)
                           "{\"permissions\": {\"defaultMode\": \"auto\"}}")
        (should (equal (agent) "build"))
        (drover-claude-code-test-with-flags "--permission-mode plan"
          (should (equal (agent) "plan")))))))

(ert-deftest drover-claude-code-test-settings-layers ()
  "Layers come highest first; the home directory's project settings are the user's."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home)))
      (make-directory project t)
      (should-not (drover-claude-code-settings-layers project))
      (drover-test-write (expand-file-name "settings.json" drover-claude-code-home) "{\"a\": 1}")
      (drover-test-write (expand-file-name ".claude/settings.json" project) "{\"a\": 2}")
      (drover-test-write (expand-file-name ".claude/settings.local.json" project) "{\"a\": 3}")
      (should (equal (drover-claude-code-settings-layers project)
                     '((local (a . 3)) (project (a . 2)) (user (a . 1)))))
      ;; Server-managed settings win over the managed file, whose drop-ins
      ;; merge over it in name order.
      (let ((managed drover-claude-code-managed-directory))
        (drover-test-write (expand-file-name "managed-settings.json" managed)
                           "{\"a\": 4, \"permissions\": {\"defaultMode\": \"plan\", \"ask\": [\"x\"]}}")
        (drover-test-write (expand-file-name "managed-settings.d/20-b.json" managed)
                           "{\"permissions\": {\"defaultMode\": \"auto\"}}")
        (drover-test-write (expand-file-name "managed-settings.d/10-a.json" managed)
                           "{\"permissions\": {\"defaultMode\": \"acceptEdits\"}, \"b\": 1}")
        (drover-test-write (expand-file-name "managed-settings.d/.hidden.json" managed)
                           "{\"c\": 1}")
        (let ((policy (car (drover-claude-code-settings-layers project))))
          (should (eq (car policy) 'policy))
          (should (equal (alist-get 'a (cdr policy)) 4))
          (should (equal (alist-get 'b (cdr policy)) 1))
          (should-not (alist-get 'c (cdr policy)))
          (should (equal (map-nested-elt (cdr policy) '(permissions defaultMode)) "auto"))
          (should (equal (map-nested-elt (cdr policy) '(permissions ask)) '("x"))))
        (drover-test-write (expand-file-name "remote-settings.json" drover-claude-code-home)
                           "{\"a\": 5}")
        (should (equal (car (drover-claude-code-settings-layers project)) '(policy (a . 5)))))
      ;; In the home directory, its .claude is the user's, read once.
      (let ((drover-claude-code-home (expand-file-name ".claude" project)))
        (should (equal (mapcar #'car (drover-claude-code-settings-layers project))
                       '(policy user)))
        (should (equal (cdr (assq 'user (drover-claude-code-settings-layers project)))
                       '((a . 2))))))))

(ert-deftest drover-claude-code-test-registry-policy ()
  "On Windows, the machine's registry policy outranks the managed file."
  (drover-test-with-homes
    (drover-test-write (expand-file-name "managed-settings.json" drover-claude-code-managed-directory)
                       "{\"model\": \"file\"}")
    (let (reads)
      (cl-letf (((symbol-function 'w32-read-registry)
                 (lambda (root key name)
                   (push (list root key name) reads)
                   (pcase root
                     ('HKLM (and (equal key "SOFTWARE\\Policies\\ClaudeCode")
                                 (equal name "Settings")
                                 "{\"model\": \"machine\"}"))
                     ('HKCU "{\"model\": \"user\"}")))))
        (should (equal (drover-claude-code--policy) '((model . "machine"))))
        (should (equal (caar reads) 'HKLM)))
      ;; Invalid JSON in the registry is no policy.
      (cl-letf (((symbol-function 'w32-read-registry)
                 (lambda (root &rest _) (and (eq root 'HKLM) "{not json"))))
        (should (equal (drover-claude-code--policy) '((model . "file")))))
      (delete-file (expand-file-name "managed-settings.json" drover-claude-code-managed-directory))
      (cl-letf (((symbol-function 'w32-read-registry)
                 (lambda (root &rest _) (and (eq root 'HKCU) "{\"model\": \"user\"}"))))
        (should (equal (drover-claude-code--policy) '((model . "user"))))))))

(ert-deftest drover-claude-code-test-flags ()
  "The last value of an option counts, separate or after =; quoting is honored."
  (drover-claude-code-test-with-flags "--model opus --effort=low --model \"claude sonnet\""
    (should (equal (drover-claude-code--flag "model") "claude sonnet"))
    (should (equal (drover-claude-code--flag "effort") "low"))
    (should-not (drover-claude-code--flag "permission-mode")))
  (drover-claude-code-test-with-flags nil
    (should-not (drover-claude-code--flag "model")))
  ;; Unbalanced quotes are no flags rather than an error.
  (drover-claude-code-test-with-flags "--model \"opus"
    (should-not (drover-claude-code--flag "model")))
  (let ((drover-claude-code-start-flags-function nil))
    (should-not (drover-claude-code--flag "model"))))

(ert-deftest drover-claude-code-test-catalog ()
  "The newest cached catalog of the CLI counts; others are ignored."
  (drover-test-with-homes
    (should-not (drover-claude-code-catalog))
    (let ((directory (expand-file-name "cache/model-catalog/" drover-claude-code-home)))
      (drover-claude-code-test-write-catalog)
      (drover-test-write (expand-file-name "old-cc.json" directory)
                         "{\"catalog\": {\"surface\": \"cc\", \"state\": {\"model\": \"old\"}}}")
      (drover-test-write (expand-file-name "org-desktop.json" directory)
                         "{\"catalog\": {\"surface\": \"desktop\", \"state\": {\"model\": \"desk\"}}}")
      (set-file-times (expand-file-name "old-cc.json" directory) (seconds-to-time 1000000))
      (set-file-times (expand-file-name "org-desktop.json" directory) (seconds-to-time 3000000))
      (set-file-times (expand-file-name "org-cc.json" directory) (seconds-to-time 2000000))
      (should (equal (map-nested-elt (drover-claude-code-catalog) '(state model)) "claude-opus-5"))
      (set-file-times (expand-file-name "old-cc.json" directory) (seconds-to-time 2500000))
      (should (equal (map-nested-elt (drover-claude-code-catalog) '(state model)) "old"))
      ;; A catalog of another surface is none.
      (drover-test-write (expand-file-name "old-cc.json" directory)
                         "{\"catalog\": {\"surface\": \"desktop\"}}")
      (set-file-times (expand-file-name "old-cc.json" directory) (seconds-to-time 2600000))
      (should-not (drover-claude-code-catalog)))))

(ert-deftest drover-claude-code-test-model ()
  "A model takes its name, variants and default effort from the catalog."
  (drover-test-with-homes
    (drover-claude-code-test-write-catalog)
    (let* ((catalog (drover-claude-code-catalog))
           (opus (drover-claude-code-model "claude-opus-5-5[1m]" nil catalog))
           (haiku (drover-claude-code-model "claude-haiku-4-5" nil catalog))
           (alias (drover-claude-code-model "opus" nil catalog)))
      (should (equal (drover-model-name opus) "Opus 5.5[1m]"))
      (should (equal (drover-model-id opus) "claude-opus-5-5[1m]"))
      (should (equal (drover-model-provider-name opus) "Anthropic"))
      (should-not (drover-model-provider-id opus))
      (should (equal (drover-model-variants opus) '((low) (medium) (high) (xhigh) (max))))
      (should (eq (drover-model-default-variant opus) 'medium))
      (should (equal (drover-model-engines opus) '(claude-code)))
      (should (equal (drover-model-name haiku) "Haiku 4.5"))
      (should-not (drover-model-variants haiku))
      (should-not (drover-model-default-variant haiku))
      ;; An alias the catalog does not list by identifier stays unnamed,
      ;; and runs on Claude Code all the same.
      (should-not (drover-model-name alias))
      (should (equal (drover-model-label alias) "Anthropic: opus"))
      (should (equal (drover-model-engines alias) '(claude-code))))))

(ert-deftest drover-claude-code-test-catalog-models ()
  "The catalog's models, in its order, each as the catalog describes it."
  (drover-test-with-homes
    (should-not (drover-claude-code-catalog-models (drover-claude-code-catalog)))
    (drover-claude-code-test-write-catalog)
    (let* ((catalog (drover-claude-code-catalog))
           (models (drover-claude-code-catalog-models catalog)))
      (should (equal (mapcar #'drover-model-id models)
                     '("claude-opus-5-5" "claude-sonnet-5-5" "claude-opus-5" "claude-opus-4-6"
                       "claude-haiku-4-5-20251001")))
      (should (equal (mapcar #'drover-model-label models)
                     '("Anthropic: Opus 5.5" "Anthropic: Sonnet 5.5" "Anthropic: Opus 5"
                       "Anthropic: Opus 4.6" "Anthropic: Haiku 4.5")))
      (should (equal (drover-model-variants (nth 1 models)) '((low) (medium) (high))))
      ;; The default effort the catalog's state keeps for Sonnet.
      (should (eq (drover-model-default-variant (nth 1 models)) 'low))
      (should-not (drover-model-variants (nth 4 models)))
      ;; Settings layers decide default efforts when given.
      (should (eq (drover-model-default-variant
                   (car (drover-claude-code-catalog-models
                         catalog '((project (effortLevel . "high"))))))
                  'high)))))

;;;; The catalog as the CLI caches it

(ert-deftest drover-claude-code-test-recorded-catalog-levels ()
  "Each model offers the effort levels the CLI runs it at, and its own default."
  (drover-test-with-homes
    (drover-test-write-claude-catalog)
    (let* ((catalog (drover-claude-code-catalog))
           (models (drover-claude-code-catalog-models catalog)))
      (should (equal (mapcar #'drover-model-id models)
                     '("claude-opus-5-5" "claude-opus-5" "claude-sonnet-5-5" "claude-haiku-4-5-20251001")))
      ;; The runtime's levels, max included where the picker leaves it out.
      (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                     '((low medium high xhigh max) (low medium high xhigh) (low medium high) nil)))
      (should (equal (mapcar #'drover-model-default-variant models) '(medium high medium nil)))
      (should (cl-every #'drover-model-variants-known models))
      ;; The CLI takes max for Opus 5.5: a level asked for is kept.
      (drover-claude-code-test-with-flags "--effort max"
        (should (eq (drover-claude-code-effort "claude-opus-5-5" nil catalog) 'max))
        (should (eq (drover-claude-code-effort "claude-opus-5" nil catalog) 'xhigh)))
      ;; A model the catalog does not list has unknown variants, not none.
      (let ((unlisted (drover-claude-code-model "claude-mythos-5-1" nil catalog)))
        (should-not (drover-model-variants-known unlisted))
        (should-not (drover-model-default-variant unlisted)))
      ;; Without runtime levels, those of the picker.
      (drover-claude-code-test-write-catalog)
      (should (equal (mapcar #'car (drover-model-variants
                                    (drover-claude-code-model "claude-opus-5" nil (drover-claude-code-catalog))))
                     '(low high))))))

(ert-deftest drover-claude-code-test-alias-targets ()
  "Aliases resolve to the targets the catalog gives them, a cloud's its own."
  (drover-test-with-homes
    (drover-test-write-claude-catalog)
    (let ((catalog (drover-claude-code-catalog)))
      (should (equal (drover-claude-code--resolve-model "opus" catalog) "claude-opus-5-5"))
      (should (equal (drover-claude-code--resolve-model "opus[1m]" catalog) "claude-opus-5-5[1m]"))
      (should (equal (drover-claude-code--resolve-model "best" catalog) "claude-opus-5-5"))
      (should (equal (drover-claude-code--resolve-model "haiku" catalog) "claude-haiku-4-5-20251001"))
      (should (equal (drover-claude-code--resolve-model "default" catalog) "claude-sonnet-5-5"))
      (should (equal (drover-claude-code--resolve-model "claude-opus-5" catalog) "claude-opus-5"))
      (setenv "CLAUDE_CODE_USE_BEDROCK" "1")
      (should (equal (drover-claude-code--resolve-model "opus" catalog) "claude-opus-5"))
      ;; Bedrock has no target of its own for Sonnet: the default one.
      (should (equal (drover-claude-code--resolve-model "sonnet" catalog) "claude-sonnet-5-5"))
      (setenv "CLAUDE_CODE_USE_BEDROCK" nil)
      ;; The environment's model for the family wins.
      (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" "claude-opus-5")
      (should (equal (drover-claude-code--resolve-model "opus" catalog) "claude-opus-5"))
      (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" nil)
      ;; An alias the catalog has no target for: its family's main model.
      (should (equal (drover-claude-code--resolve-model "opus" (drover-claude-code-test-catalog-data))
                     "claude-opus-5-5")))))

(defun drover-claude-code-test-catalog-data ()
  "Return `drover-claude-code-test-catalog', which has no alias targets, read."
  (alist-get 'catalog (json-parse-string drover-claude-code-test-catalog
                                         :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)))

(ert-deftest drover-claude-code-test-acp-models ()
  "The models an ACP session lists are the catalog's, each once, named as the session takes them."
  (drover-test-with-homes
    (drover-test-write-claude-catalog)
    (let ((models (drover-claude-code-acp-models drover-test-claude-acp-models "default"
                                                 '(low medium high) 'agent-shell)))
      ;; "default" names Sonnet 5.5, which "sonnet" names too.
      (should (equal (mapcar #'drover-model-id models)
                     '("claude-opus-5-5" "claude-opus-5-5[1m]" "claude-sonnet-5-5"
                       "claude-haiku-4-5-20251001")))
      (should (equal (mapcar #'drover-model-label models)
                     '("Anthropic: Opus 5.5" "Anthropic: Opus 5.5[1m]" "Anthropic: Sonnet 5.5"
                       "Anthropic: Haiku 4.5")))
      (should (equal (mapcar (lambda (model) (drover-model-id-for model 'claude-code 'agent-shell)) models)
                     '("opus" "opus[1m]" "sonnet" "haiku")))
      ;; Other front ends take the identifier.
      (should (equal (drover-model-id-for (car models) 'claude-code 'claude-code-ide) "claude-opus-5-5"))
      (should (equal (drover-model-id-for (car models) 'claude-code) "claude-opus-5-5"))
      ;; Each model's own levels: the session's for the one it runs.
      (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                     '((low medium high xhigh max) (low medium high xhigh max) (low medium high) nil)))
      (should (equal (mapcar #'drover-model-default-variant models) '(medium medium medium nil)))
      (should (cl-every #'drover-model-variants-known models))
      ;; The same model as the catalog's own list gives it, but for its alias.
      (let ((listed (car (drover-claude-code-catalog-models (drover-claude-code-catalog)))))
        (should (equal (drover-model-key listed) (drover-model-key (car models))))
        (setf (drover-model-aliases listed) (drover-model-aliases (car models)))
        (should (equal listed (car models)))))
    ;; The session runs Opus: it, and no other model, takes the session's levels.
    (let ((models (drover-claude-code-acp-models drover-test-claude-acp-models "opus" '(low high)
                                                 'agent-shell)))
      (should (equal (mapcar (lambda (model) (mapcar #'car (drover-model-variants model))) models)
                     '((low high) (low medium high xhigh max) (low medium high) nil))))))

(ert-deftest drover-claude-code-test-acp-models-without-catalog ()
  "Without the catalog, a session's aliases are its models, the one it runs alone known."
  (drover-test-with-homes
    (let ((models (drover-claude-code-acp-models drover-test-claude-acp-models "opus" '(low high)
                                                 'agent-shell)))
      ;; "default" names no model drover knows: the pickers' own default stands for it.
      (should (equal (mapcar #'drover-model-id models) '("opus" "opus[1m]" "sonnet" "haiku")))
      (should (equal (mapcar #'drover-model-name models)
                     '("Opus 5.5" "Opus 5.5 (1M context)" "Sonnet 5.5" "Haiku 4.5")))
      (should-not (cl-some #'drover-model-aliases models))
      (should (equal (drover-model-id-for (car models) 'claude-code 'agent-shell) "opus"))
      (should (equal (mapcar #'drover-model-variants-known models) '(t nil nil nil)))
      (should (equal (drover-model-variants (car models)) '((low) (high))))
      (should-not (drover-model-variants (nth 1 models))))))

(ert-deftest drover-claude-code-test-defaults-start-directory ()
  "Project settings are read where a front end starts the session."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home))
          (sub (expand-file-name "project/sub/" home))
          asked)
      (make-directory sub t)
      (drover-test-write (expand-file-name ".claude/settings.json" project)
                         "{\"permissions\": {\"defaultMode\": \"plan\"}}")
      (should-not (plist-get (drover-claude-code-defaults sub) :agent))
      (let ((drover-claude-code-project-directory-function
             (lambda (directory) (push directory asked) project)))
        (should (equal (plist-get (drover-claude-code-defaults sub) :agent) "plan")))
      (should (equal asked (list sub)))
      ;; A function finding no other directory leaves the one asked for.
      (let ((drover-claude-code-project-directory-function #'ignore))
        (should-not (plist-get (drover-claude-code-defaults sub) :agent))))))

;;;; The engine

(ert-deftest drover-claude-code-test-registered ()
  "Loading the file registers the engine, and no model source of its own."
  (should (memq 'claude-code (drover-engines)))
  (should (equal (drover-engine-label 'claude-code) "Claude Code"))
  (should (drover-engine-property 'claude-code :volatile))
  (should-not (drover-engine-property 'claude-code :model-resets-variant))
  (drover-test-with-engines
    (drover-claude-code--register)
    (should (equal (drover-engines) '(claude-code)))
    (should (eq (drover-engine-property 'claude-code :resolve) #'drover-claude-code--resolve))
    (should (equal (drover-agent-names "/any/") '("plan" "build")))
    (should-not drover--model-sources)
    ;; A front end's source survives the engine's unloading.
    (drover-register-model-source 'claude-code #'ignore -10 t)
    (drover-claude-code-unload-function)
    (should-not (drover-engines))
    (should (equal drover--model-sources '((claude-code -10 ignore t))))))

(ert-deftest drover-claude-code-test-oauth-catalog-obsolete ()
  "Setting the old option of Anthropic's model list is harmless, and warned of."
  (should (get 'drover-claude-code-oauth-catalog 'byte-obsolete-variable))
  (should (string-match-p "live sessions"
                          (car (get 'drover-claude-code-oauth-catalog 'byte-obsolete-variable))))
  (should-not (fboundp 'drover-claude-code--oauth-source))
  (with-suppressed-warnings ((obsolete drover-claude-code-oauth-catalog))
    (let ((drover-claude-code-oauth-catalog t))
      (should drover-claude-code-oauth-catalog))))

(ert-deftest drover-claude-code-test-defaults-callback ()
  "The engine's defaults come at once; a failure is warned about, without callback."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home))
          delivered warnings)
      (make-directory project t)
      (drover-test-write (expand-file-name "settings.json" drover-claude-code-home)
                         "{\"model\": \"claude-opus-5-5\", \"permissions\": {\"defaultMode\": \"plan\"}}")
      (funcall (drover-engine-property 'claude-code :defaults) project
               (lambda (defaults) (push defaults delivered)))
      (should (= (length delivered) 1))
      (should (equal (drover-model-id (plist-get (car delivered) :model)) "claude-opus-5-5"))
      (should (equal (plist-get (car delivered) :agent) "plan"))
      (should (equal (plist-get (car delivered) :layers)
                     '((user (model . "claude-opus-5-5") (permissions (defaultMode . "plan"))))))
      (setq delivered nil)
      (cl-letf (((symbol-function 'drover-claude-code-catalog) (lambda () (error "Broken cache")))
                ((symbol-function 'display-warning)
                 (lambda (type message &rest _) (push (list type message) warnings))))
        (drover-claude-code--fetch-defaults project (lambda (defaults) (push defaults delivered))))
      (should-not delivered)
      (should (equal (caar warnings) 'drover))
      (should (string-match-p "Claude Code.s defaults.*Broken cache" (cadar warnings))))))

(ert-deftest drover-claude-code-test-resolve ()
  "Explicit choices win; else the startup model, mode agent and model effort."
  (let* ((opus (drover-model-create :provider-name "Anthropic" :id "claude-opus-5-5"
                                    :engines '(claude-code)))
         (fable (drover-model-create :provider-name "Anthropic" :id "claude-fable-5-1"
                                     :engines '(claude-code)))
         (defaults (list :model opus :agent "plan" :catalog nil
                         :layers '((user (modelSettings (claude-opus-5-5 (effortLevel . "xhigh"))))))))
    (drover-test-with-homes
      (should (equal (drover-engine-resolve 'claude-code defaults nil nil nil)
                     (list :agent "plan" :model opus :variant 'xhigh)))
      (should (equal (drover-engine-resolve 'claude-code defaults "build" fable 'low)
                     (list :agent "build" :model fable :variant 'low)))
      ;; A model chosen runs the effort settings give it.
      (should (equal (drover-engine-resolve 'claude-code defaults nil fable nil)
                     (list :agent "plan" :model fable :variant nil)))
      (should (equal (drover-engine-resolve 'claude-code (list :model opus :layers nil :catalog nil)
                                            nil nil nil)
                     (list :agent nil :model opus :variant nil)))
      ;; Before the defaults are read, only the choices count.
      (should (equal (drover-engine-resolve 'claude-code nil nil nil nil)
                     (list :agent nil :model nil :variant nil)))
      (drover-claude-code-test-with-flags "--effort high"
        (should (equal (drover-engine-resolve 'claude-code nil nil fable nil)
                       (list :agent nil :model fable :variant 'high)))))))

(ert-deftest drover-claude-code-test-last-used ()
  "History and the transcripts of listed sessions tell when Claude Code ran last."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home))
          (history (expand-file-name "history.jsonl" drover-claude-code-home)))
      (make-directory project t)
      (drover-test-with-backend
          (list (drover-test-session "a" :engine 'claude-code :engine-id "id-a" :directory project)
                (drover-test-session "b" :engine 'codex :engine-id "id-b" :directory project)
                (drover-test-session "c" :engine 'claude-code :directory project))
        (should-not (drover-claude-code--last-used))
        (drover-test-write history "{}\n")
        (set-file-times history (seconds-to-time 1000000))
        (should (= (drover-claude-code--last-used) 1000000.0))
        (let ((transcript (drover-claude-code-transcript "id-a" project))
              (other (drover-claude-code-transcript "id-b" project)))
          (drover-test-write transcript "{}\n")
          (drover-test-write other "{}\n")
          (set-file-times transcript (seconds-to-time 2000000))
          (set-file-times other (seconds-to-time 3000000))
          (should (= (drover-claude-code--last-used) 2000000.0))
          (set-file-times history (seconds-to-time 2500000))
          (should (= (funcall (drover-engine-property 'claude-code :last-used)) 2500000.0)))))))

;;;; Sessions on disk

(ert-deftest drover-claude-code-test-directories ()
  "Transcripts and running sessions live under the home directory."
  (let ((drover-claude-code-home "/claude-home"))
    (should (equal (drover-claude-code-projects-directory) "/claude-home/projects/"))
    (should (equal (drover-claude-code-sessions-directory) "/claude-home/sessions/"))))

(ert-deftest drover-claude-code-test-encode-decode ()
  "A directory encodes as Claude Code names it, and decodes back."
  (drover-test-with-homes
    (let* ((root (file-truename home))
           (project (expand-file-name "my-project/v1.2-x_y/" root))
           ;; Encoded alike, it holds nothing below: probing it goes back.
           (decoy (expand-file-name "my project/" root)))
      (make-directory project t)
      (make-directory decoy t)
      (let ((encoded (drover-claude-code-encode-directory project)))
        (should-not (string-match-p "[^a-zA-Z0-9-]" encoded))
        (should (string-suffix-p "-my-project-v1-2-x-y" encoded))
        (should (equal (drover-claude-code-decode-bucket encoded) project)))
      ;; An empty directory encoded alike may be either.
      (should (member (drover-claude-code-decode-bucket (drover-claude-code-encode-directory decoy))
                      (list decoy (expand-file-name "my-project/" root))))
      ;; Whichever comes first, the project is found.
      (let ((list-directory (symbol-function 'directory-files)))
        (dolist (order (list #'string< #'string>))
          (cl-letf (((symbol-function 'directory-files)
                     (lambda (&rest arguments) (sort (apply list-directory arguments) order))))
            (should (equal (drover-claude-code-decode-bucket
                            (drover-claude-code-encode-directory project))
                           project)))))
      ;; Symbolic links are resolved first.
      (let ((link (expand-file-name "link" root)))
        (make-symbolic-link project link)
        (should (equal (drover-claude-code-encode-directory link)
                       (drover-claude-code-encode-directory project))))
      (should-not (drover-claude-code-decode-bucket
                   (concat (drover-claude-code-encode-directory root) "-missing")))
      (should-not (drover-claude-code-decode-bucket "relative")))))

(ert-deftest drover-claude-code-test-conversation-directory ()
  "The engine's :directory tells the directory of a transcript, or that none holds it."
  (should (eq (drover-engine-property 'claude-code :directory) #'drover-claude-code--directory))
  (let (found failed)
    (cl-letf (((symbol-function 'drover-claude-code-session-directory)
               (lambda (id) (and (equal id "id-a") "/work/a/"))))
      (drover-claude-code--directory "id-a" (lambda (directory) (setq found directory)) #'ignore)
      (drover-claude-code--directory "id-b" #'ignore (lambda (message) (setq failed message))))
    (should (equal found "/work/a/"))
    (should (equal failed "No project directory holds Claude Code session id-b"))))

(ert-deftest drover-claude-code-test-session-directory ()
  "A session's directory is decoded from the bucket holding its transcript."
  (drover-test-with-homes
    (let ((project (file-name-as-directory (file-truename (expand-file-name "project" home)))))
      (make-directory project t)
      (should-not (drover-claude-code-session-directory "id-a"))
      (drover-test-write (drover-claude-code-transcript "id-a" project) "{}\n")
      (should (equal (drover-claude-code-transcript "id-a" project)
                     (expand-file-name (concat (drover-claude-code-encode-directory project) "/id-a.jsonl")
                                       (drover-claude-code-projects-directory))))
      (should (equal (drover-claude-code-session-directory "id-a") project))
      (should-not (drover-claude-code-session-directory "id-b")))))

(ert-deftest drover-claude-code-test-session-files ()
  "A session's files are its transcript and the directory beside it, found by identifier."
  (drover-test-with-homes
    (let* ((id "11111111-1111-4111-8111-111111111111")
           (project (file-name-as-directory (file-truename (expand-file-name "project" home))))
           (other (file-name-as-directory (file-truename (expand-file-name "other" home))))
           (transcript (drover-claude-code-transcript id project))
           (beside (expand-file-name id (file-name-directory transcript)))
           (delete-by-moving-to-trash nil))
      (make-directory project t)
      (make-directory other t)
      ;; Only a session identifier is looked for.
      (dolist (bad '("../../etc/passwd" "id-a" nil ""))
        (should-error (drover-claude-code-session-files bad project) :type 'user-error))
      (should-error (drover-claude-code-session-files id project) :type 'user-error)
      (drover-test-write transcript "{}\n")
      (should (equal (drover-claude-code-session-files id project) (cons transcript nil)))
      ;; Found in its own bucket, else the one holding it.
      (should (equal (drover-claude-code-session-files id other) (cons transcript nil)))
      (should (equal (drover-claude-code-session-files id) (cons transcript nil)))
      (drover-test-write (expand-file-name "subagents/agent-1.jsonl" beside) "{}\n")
      (should (equal (drover-claude-code-session-files id project) (cons transcript beside)))
      ;; Two buckets holding it, and the session's own not one: none.
      (let ((copy (drover-claude-code-transcript id other)))
        (drover-test-write copy "{}\n")
        (should (equal (car (drover-claude-code-session-files id project)) transcript))
        (should-error (drover-claude-code-session-files id) :type 'user-error)
        (delete-file copy))
      (let ((neighbour (drover-claude-code-transcript "22222222-2222-4222-8222-222222222222" project)))
        (drover-test-write neighbour "{}\n")
        (drover-claude-code-delete-session-files (drover-claude-code-session-files id project))
        (should-not (file-exists-p transcript))
        (should-not (file-exists-p beside))
        ;; The other transcripts of the bucket stay.
        (should (file-exists-p neighbour))))))

(ert-deftest drover-claude-code-test-pid-session-id ()
  "A process's session is the one the CLI records for its PID, never a guess."
  (drover-test-with-homes
    (let ((first "11111111-1111-4111-8111-111111111111")
          (second "22222222-2222-4222-8222-222222222222")
          (record (expand-file-name "12345.json" (drover-claude-code-sessions-directory))))
      (should-not (drover-claude-code-pid-session-id 12345))
      (should-not (drover-claude-code-pid-session-id nil))
      ;; A continued or switched session follows the record.
      (dolist (id (list first second))
        (drover-test-write record (json-serialize `((pid . 12345) (sessionId . ,id))))
        (should (equal (drover-claude-code-pid-session-id 12345) id)))
      ;; A record of another process, as after a PID is reused, is none.
      (drover-test-write record (json-serialize `((pid . 99999) (sessionId . ,first))))
      (should-not (drover-claude-code-pid-session-id 12345))
      (drover-test-write record (json-serialize '((pid . 12345) (sessionId . "not-a-uuid"))))
      (should-not (drover-claude-code-pid-session-id 12345))
      (drover-test-write record "[1, 2]")
      (should-not (drover-claude-code-pid-session-id 12345)))))

(ert-deftest drover-claude-code-test-newest-session-id ()
  "The newest transcript of a directory names its last session."
  (drover-test-with-homes
    (let ((project (expand-file-name "project/" home)))
      (make-directory project t)
      (should-not (drover-claude-code-newest-session-id project))
      (let ((a (drover-claude-code-transcript "id-a" project))
            (b (drover-claude-code-transcript "id-b" project)))
        (drover-test-write a "{}\n")
        (drover-test-write b "{}\n")
        (drover-test-write (expand-file-name "notes.txt" (file-name-directory a)) "")
        (set-file-times a (seconds-to-time 2000000))
        (set-file-times b (seconds-to-time 1000000))
        (should (equal (drover-claude-code-newest-session-id project) "id-a"))
        (set-file-times b (seconds-to-time 3000000))
        (should (equal (drover-claude-code-newest-session-id project) "id-b"))))))

(ert-deftest drover-claude-code-test-last-turn ()
  "A terminal's model and effort come from its newest main-thread answer."
  (drover-test-with-homes
    (let ((file (expand-file-name "transcript.jsonl" home)))
      (should-not (drover-claude-code-last-turn file))
      (drover-test-write file "")
      (should-not (drover-claude-code-last-turn file))
      (drover-test-write
       file
       (concat "{\"type\":\"assistant\",\"requestedModel\":\"claude-fable-5-1\",\"effort\":\"max\"}\n"
               "{\"type\":\"assistant\",\"requestedModel\":\"claude-opus-5-5\",\"effort\":\"xhigh\"}\n"
               "{\"type\":\"assistant\",\"isSidechain\":true,"
               "\"requestedModel\":\"claude-haiku-5-5\",\"effort\":\"low\"}\n"
               "{\"type\":\"user\",\"message\":\"mentions \\\"requestedModel\\\"\"}\n"))
      (should (equal (drover-claude-code-last-turn file) '("claude-opus-5-5" . xhigh)))
      ;; A model without effort has none.
      (with-temp-buffer
        (insert "{\"type\":\"assistant\",\"requestedModel\":\"claude-haiku-4-5\",\"effort\":null}\n")
        (append-to-file (point-min) (point-max) file))
      (should (equal (drover-claude-code-last-turn file) '("claude-haiku-4-5"))))))

(ert-deftest drover-claude-code-test-recap ()
  "The recap of a transcript's last turn: its prompt, reply, error and files changed."
  (drover-test-with-homes
    (let ((file (expand-file-name "transcript.jsonl" home)))
      (should (equal (drover-claude-code-recap file) '(:prompt nil :reply nil :changes nil :error nil)))
      (drover-test-write
       file
       (mapconcat
        (lambda (record) (concat (json-encode record) "\n"))
        `(((type . "user") (message (role . "user") (content . "An older prompt")))
          ((type . "assistant") (message (id . "m0") (content . [((type . "text") (text . "Older reply"))])))
          ((type . "user") (message (role . "user") (content . "Fix the build")))
          ((type . "user") (isMeta . t) (message (role . "user") (content . "Caveat: notes")))
          ((type . "assistant") (message (id . "m1") (content . [((type . "text") (text . "Looking."))])))
          ((type . "assistant") (message (id . "m1") (content . [((type . "tool_use") (name . "Edit"))])))
          ((type . "user") (message (content . [((type . "tool_result") (content . "ok"))]))
           (toolUseResult (filePath . "/w/a.el")
                          (structuredPatch . [((lines . ["-old" "+new" "+more" " same"]))])))
          ((type . "user") (message (content . [((type . "tool_result") (content . "ok"))]))
           (toolUseResult (type . "create") (filePath . "/w/b.el") (content . "one\ntwo\n")
                          (structuredPatch . [])))
          ((type . "user") (message (content . [((type . "tool_result") (content . "ok"))]))
           (toolUseResult (filePath . "/w/a.el") (structuredPatch . [((lines . ["-x" "+y"]))])))
          ((type . "assistant") (isSidechain . t)
           (message (id . "s1") (content . [((type . "text") (text . "A subagent's text"))])))
          ((type . "user") (message (role . "user") (content . "<command-name>/model</command-name>")))
          ((type . "assistant") (message (id . "m2") (content . [((type . "text") (text . "Fixed: "))])))
          ((type . "assistant") (message (id . "m2") (content . [((type . "text") (text . "it builds."))])))
          ((type . "assistant") (isApiErrorMessage . t)
           (message (id . "e1") (content . [((type . "text") (text . "API Error: overloaded"))]))))
        ""))
      (should (equal (drover-claude-code-recap file)
                     '(:prompt "Fix the build" :reply "Fixed:\nit builds."
                       :changes (("/w/a.el" 3 2) ("/w/b.el" 2 0)) :error "API Error: overloaded")))
      ;; A turn longer than what is read keeps the prompt recorded apart.
      (let ((drover-claude-code--transcript-tail 200))
        (with-temp-buffer
          (insert (json-encode '((type . "last-prompt") (lastPrompt . "Fix the build"))) "\n"
                  (json-encode '((type . "assistant")
                                 (message (id . "m3") (content . [((type . "text") (text . "Still on it"))]))))
                  "\n")
          (append-to-file (point-min) (point-max) file))
        (should (equal (drover-claude-code-recap file)
                       '(:prompt "Fix the build" :reply "Still on it" :changes nil :error nil)))))))

(ert-deftest drover-claude-code-test-last-turn-reads-the-end ()
  "Only the end of a long transcript is read."
  (drover-test-with-homes
    (let ((file (expand-file-name "transcript.jsonl" home))
          (drover-claude-code--transcript-tail 200))
      (drover-test-write
       file
       (concat "{\"type\":\"assistant\",\"requestedModel\":\"claude-opus-5-5\",\"effort\":\"xhigh\"}\n"
               (make-string 300 ?x) "\n"))
      (should-not (drover-claude-code-last-turn file)))))

(provide 'drover-claude-code-test)
;;; drover-claude-code-test.el ends here
