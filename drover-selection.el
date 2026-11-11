;;; drover-selection.el --- Model choices and destination offerings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Valentin Leon

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sources report concrete offerings, kept by engine.  The prompt picks
;; a model independently of its provider, then resolves that choice for
;; each destination.  Native identifiers, aliases, variants and defaults
;; remain those of the destination's offering, never a union sent to it.
;; A selection is an options plist with :selection t; its :model is a
;; provider-free drover-model carrying the logical identifier.  Existing
;; concrete model options remain provider-pinned.

;;; Code:

(require 'drover-model)

(defcustom drover-selection-model-aliases nil
  "Explicit identities of models whose providers use other identifiers.
Each entry is ((PROVIDER . NATIVE-ID) . MODEL-ID).  Display names are
never used to identify models."
  :type '(alist :key-type (cons string string) :value-type string)
  :group 'drover)

(defun drover-selection--vendor-id (id)
  "Return the canonical vendor identity of a recognized native ID, or nil.
Only vendor naming conventions are normalized.  Claude's decimal and
hyphenated versions agree, as do its family-first and version-first
spellings.  Editions, context suffixes and dated snapshots stay distinct."
  (let* ((case-fold-search nil)
         (bare (replace-regexp-in-string
                "\\`\\(?:openai/\\|anthropic/\\|google/\\|moonshotai/\\|accounts/fireworks/models/\\)" "" id))
         (bedrock (string-match "\\`\\(?:global\\.\\|us\\.\\|eu\\.\\|apac\\.\\)?anthropic\\.\\(claude-.+\\)\\'" bare)))
    (when bedrock
      (setq bare (replace-regexp-in-string "-v1:0\\'" "" (match-string 1 bare))))
    (when (string-match "\\`claude-\\([0-9]+\\(?:[.-][0-9]\\{1,2\\}\\)?\\)-\\([a-z]+\\)\\(.*\\)\\'" bare)
      (setq bare (concat "claude-" (match-string 2 bare) "-" (match-string 1 bare) (match-string 3 bare))))
    (when (string-match "\\`\\(claude-[a-z]+-[0-9]+\\)\\.\\([0-9]+\\)\\(.*\\)\\'" bare)
      (setq bare (concat (match-string 1 bare) "-" (match-string 2 bare) (match-string 3 bare))))
    (and (string-match-p "\\`\\(?:gpt-\\|o[134]-\\|claude-\\|gemini-\\|deepseek-\\|glm-\\|kimi-\\)" bare)
         bare)))

(defun drover-selection-id (model)
  "Return the provider-independent identity of MODEL.
Known vendor identifiers are shared across providers.  Other provider
identifiers stay qualified unless `drover-selection-model-aliases'
maps them explicitly.  Native IDs stay on their source records; only
the identity returned here is canonicalized."
  (let* ((alias (alist-get (drover-model-key model) drover-selection-model-aliases nil nil #'equal))
         (id (or alias (drover-model-id model))))
    (or (drover-selection--vendor-id id) alias
        (if-let* ((provider (drover-model-provider-id model)))
            (concat provider "/" id)
          id))))

(defun drover-selection-provider (engine model)
  "Return the serving provider of ENGINE's MODEL, or nil if unknown.
A native engine may know its provider by name without accepting a
provider-qualified model identifier."
  (or (drover-model-provider-id model)
      (pcase engine
        ('codex (let ((name (drover-model-provider-name model)))
                  (if (member name '(nil "OpenAI")) "openai" name)))
        ('claude-code (pcase (drover-model-provider-name model)
                        ("Anthropic" "anthropic")
                        ("Amazon Bedrock" "amazon-bedrock")
                        ("Google Vertex AI" "google-vertex-anthropic")
                        ("Azure AI Foundry" "azure-anthropic")
                        (name name)))
        ('gemini-cli "google"))))

(defun drover-selection-effort (variant)
  "Return the effort symbol the native VARIANT describes."
  (if-let* ((effort (plist-get (cdr variant) :effort)))
      (intern effort)
    (car variant)))

(defun drover-selection-variant (model effort)
  "Return the native variant of MODEL implementing EFFORT, or nil."
  (car (seq-find (lambda (variant) (eq effort (drover-selection-effort variant)))
                 (drover-model-variants model))))

(defun drover-selection-matches-p (engine model choice provider effort)
  "Return whether ENGINE's MODEL satisfies CHOICE, PROVIDER and EFFORT.
CHOICE is a logical identifier, or nil.  Unknown efforts remain
eligible until the destination can validate them."
  (and (or (null choice) (equal (or (drover-selection--vendor-id choice) choice)
                                (drover-selection-id model)))
       (or (null provider) (equal provider (drover-selection-provider engine model)))
       (or (null effort) (not (drover-model-variants-known model))
           (drover-selection-variant model effort))))

(defun drover-selection-models (catalogs &optional provider effort)
  "Return logical model choices from CATALOGS matching PROVIDER and EFFORT.
CATALOGS is an alist (ENGINE . MODELS).  The choices are provider-free
`drover-model' records.  Their efforts are the union of the offerings;
each destination must resolve them before using them."
  (let ((groups nil))
    (pcase-dolist (`(,engine . ,models) catalogs)
      (dolist (model models)
        (when (drover-selection-matches-p engine model nil provider effort)
          (let* ((id (drover-selection-id model))
                 (entry (assoc id groups))
                 (choice (cdr entry)))
            (unless choice
              (setq choice (drover-model-create :id id :name (or (drover-model-name model) id)
                                                :variants-known nil :remembered (drover-model-remembered model)))
              (push (cons id choice) groups))
            (when (and (drover-model-remembered choice) (not (drover-model-remembered model)))
              (setf (drover-model-name choice) (or (drover-model-name model) id)
                    (drover-model-remembered choice) nil))
            (cl-pushnew engine (drover-model-engines choice))
            (when (drover-model-variants-known model)
              (setf (drover-model-variants-known choice) t))
            (dolist (variant (drover-model-variants model))
              (let ((effort (drover-selection-effort variant)))
                (unless (assq effort (drover-model-variants choice))
                  (setf (drover-model-variants choice)
                        (append (drover-model-variants choice) (list (list effort)))))))))))
    (mapcar #'cdr (nreverse groups))))

(defun drover-selection-providers (catalogs &optional choice effort)
  "Return providers offering CHOICE at EFFORT in CATALOGS, as (LABEL . ID).
CHOICE is a logical model identifier, or nil for any model."
  (let (providers)
    (pcase-dolist (`(,engine . ,models) catalogs)
      (dolist (model models)
        (when (drover-selection-matches-p engine model choice nil effort)
          (when-let* ((id (drover-selection-provider engine model)))
            (unless (rassoc id providers)
              (let ((label (or (drover-model-provider-name model) id)))
                (when (assoc label providers) (setq label (format "%s [%s]" label id)))
                (push (cons label id) providers)))))))
    (nreverse providers)))

(defun drover-selection-resolve (engine options current models &optional settable)
  "Resolve OPTIONS for ENGINE using CURRENT and its native MODELS.
CURRENT is the session's options or a new session's resolved defaults.
SETTABLE is t for a new session, or the options an existing session
can change.  Return (:options OPTIONS :effective EFFECTIVE), or
\(:failure MESSAGE).  Explicit choices must all be honored."
  (let* ((picked (plist-get options :model))
         (logical (plist-get options :selection))
         (old (plist-get current :model))
         (effort (plist-get options :variant))
         (provider (or (plist-get options :provider)
                       (and picked (not logical) (drover-selection-provider engine picked))))
         (id (and (or picked old) (drover-selection-id (or picked old))))
         (available (append models (and old
                                        (not (seq-find (lambda (model) (equal (drover-model-key model)
                                                                              (drover-model-key old))) models))
                                        (list old))
                            (and (null models) picked (not logical)
                                 (drover-model-id-for picked engine) (list picked))))
         (matches (seq-filter (lambda (model)
                                (drover-selection-matches-p engine model id provider
                                                            (and logical effort)))
                              available))
         (chosen (if (or (not (or picked provider effort)) (not (or picked provider old))) old
                   (or (and old (seq-find (lambda (model) (equal (drover-model-key old)
                                                                 (drover-model-key model))) matches))
                       (and old
                            (seq-find (lambda (model)
                                        (equal (drover-selection-provider engine old)
                                               (drover-selection-provider engine model))) matches))
                       (car matches))))
         (native-effort (if (and logical effort chosen)
                            (or (drover-selection-variant chosen effort) effort)
                          effort))
         (switch (and chosen (or picked provider effort)
                      (not (and old (equal (drover-model-key chosen) (drover-model-key old))))))
         (effective-effort (or native-effort
                               (if (and (or switch picked provider) chosen
                                        (drover-engine-property engine :model-resets-variant))
                                   (drover-model-default-variant chosen)
                                 (plist-get current :variant)))))
    (cond
     ((and (or picked provider (and effort old)) (not chosen))
      (list :failure (format "%s offers no matching model, provider and effort" (drover-engine-label engine))))
     ((and switch (not (eq settable t)) (not (memq :model settable)))
      (list :failure "This session cannot switch its model or provider"))
     ((and effort chosen (or models logical) (drover-model-variants-known chosen)
           (not (assq native-effort (drover-model-variants chosen))))
      (list :failure (format "%s offers no effort %s" (drover-model-label chosen) effort)))
     ((and effort (not (eq settable t)) (not (memq :variant settable))
           (not (eq native-effort (plist-get current :variant))))
      (list :failure "This session cannot switch its effort"))
     (t
      (list :options (list :agent (plist-get options :agent)
                           :model (and (or switch picked provider)
                                       (or (eq settable t) (memq :model settable)) chosen)
                           :variant (and (or (eq settable t) (memq :variant settable)) native-effort))
            :effective (list :agent (or (plist-get options :agent) (plist-get current :agent))
                             :model (or chosen old) :variant effective-effort))))))

(provide 'drover-selection)
;;; drover-selection.el ends here
