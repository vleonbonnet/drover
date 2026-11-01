;;; drover-model.el --- Engines, their models and their defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: internal

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

;; An engine is the agent program a session runs, such as OpenCode,
;; Claude Code or Codex, whatever front end, the backend, shows it.
;; Engines own what a new session would run: their models, their
;; startup defaults and when they took a prompt last.  Each registers
;; with `drover-register-engine'; models come from sources registered
;; per engine with `drover-register-model-source', the first source of
;; an engine that answers being its catalog.
;;
;; A model is a `drover-model' record naming the engines able to run it:
;; engines taking provider-qualified ids run any model of a provider
;; (`drover-model-qualified-engines'), the others the models of their
;; own family (`drover-model-families').
;;
;; The prompt passes options around as plists:
;;
;; - options, explicit choices: (:agent AGENT :model MODEL :variant
;;   VARIANT), AGENT a string, MODEL a `drover-model', VARIANT a symbol,
;;   each nil when not chosen; an empty agent is nil.
;; - start options, given to a backend's :create: the options, unfiltered,
;;   with :title, the new session's title or nil, and :defaults, the
;;   engine's defaults data the prompt holds, or nil.
;; - resolved options, what a new session runs: (:agent :model :variant),
;;   as `drover-engine-resolve' returns them.
;; - display options, for the prompt's header and minibuffer: resolved
;;   options with :agent-explicit-p, :model-explicit-p,
;;   :variant-explicit-p, :directory and :directory-explicit-p.
;; - entry options, for picker labels: (:model :model-explicit-p
;;   :variant :variant-explicit-p), each key present only when known.
;;
;; The file also holds the helpers engines share to read their local
;; state: JSON files, settings layers, environment variables.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'drover-core)

;;;; Models

(cl-defstruct (drover-model (:constructor drover-model-create)
                            (:copier nil))
  "One selectable model."
  (provider-id nil :documentation "Upstream provider identifier when the model is provider-qualified, or nil.")
  (provider-name nil :documentation "Display name of the provider, or nil.")
  (id nil :documentation "How its engines address the model, without provider.")
  (name nil :documentation "Display name, or nil.")
  (variants nil :documentation "Variants, as an alist (SYMBOL . PLIST).
PLIST may hold :effort, the effort string the variant sends; a variant
without a known effort has a nil PLIST, printed as (SYMBOL).")
  (default-variant nil :documentation "Symbol of the variant a request naming none runs, or nil.
Nil when the provider decides, or it is unknown.")
  (engines nil :documentation "Engines able to run the model, in order of preference."))

(defcustom drover-model-families
  '((claude-code . "anthropic\\|claude")
    (codex . "openai\\|gpt")
    (gemini-cli . "google\\|gemini"))
  "Engines addressing the models of one family by bare identifier.
Each entry is (ENGINE . REGEXP): ENGINE runs a model whose provider or
model identifier, downcased, matches REGEXP."
  :type '(alist :key-type (symbol :tag "Engine") :value-type (regexp :tag "Family"))
  :group 'drover)

(defcustom drover-model-qualified-engines '(opencode)
  "Engines taking any provider-qualified model, as \"PROVIDER/ID\"."
  :type '(repeat (symbol :tag "Engine"))
  :group 'drover)

(defcustom drover-model-engine-order '(claude-code codex gemini-cli opencode)
  "Engines whose models `drover-models' collects first, in this order.
A model two engines report, the same provider and identifier, is the
one the engine first here reported.  Other engines come after, in the
order of `drover-engines'."
  :type '(repeat (symbol :tag "Engine"))
  :group 'drover)

(defun drover-model-engines-for (provider-id model-id)
  "Return the engines able to run the model MODEL-ID of PROVIDER-ID.
PROVIDER-ID is nil for a model known by its bare identifier.  Engines
of `drover-model-qualified-engines' come first, when PROVIDER-ID is
known, then those of `drover-model-families' whose family the provider
or the model belongs to."
  (append (and provider-id (copy-sequence drover-model-qualified-engines))
          (delq nil (mapcar (pcase-lambda (`(,engine . ,regexp))
                              (and (seq-some (lambda (id) (and id (string-match-p regexp (downcase id))))
                                             (list provider-id model-id))
                                   engine))
                            drover-model-families))))

(defun drover-model-runs-on-p (model engine)
  "Return non-nil when ENGINE can run MODEL, a `drover-model'."
  (and model (memq engine (drover-model-engines model)) t))

(defun drover-model-id-for (model engine)
  "Return the identifier ENGINE takes for MODEL, or nil when it cannot run it.
Engines of `drover-model-qualified-engines' take \"PROVIDER/ID\", and
nothing for a model without provider; the others the bare identifier."
  (when (drover-model-runs-on-p model engine)
    (if (memq engine drover-model-qualified-engines)
        (when-let* ((provider (drover-model-provider-id model)))
          (concat provider "/" (drover-model-id model)))
      (drover-model-id model))))

(defun drover-model-label (model)
  "Return how MODEL reads in pickers and headers, \"PROVIDER: NAME\"."
  (let ((provider (or (drover-model-provider-name model) (drover-model-provider-id model)))
        (name (or (drover-model-name model) (drover-model-id model))))
    (if provider (format "%s: %s" provider name) name)))

(defun drover-model-short-name (name)
  "Return model NAME without the family prefix every model of it shares.
It is \"Opus 5.5\" for \"Claude Opus 5.5\", as opencode.el shows models
in its mode line, so that session lists name a model alike whatever
front end shows the session."
  (and name (replace-regexp-in-string "\\`Claude " "" name)))

(defun drover-model-annotation (model)
  "Return the annotation of MODEL in pickers: its identifier and variants."
  (propertize
   (concat (format "  %s" (drover-model-id model))
           (when-let* ((variants (drover-model-variants model)))
             (format "  {%s}%s"
                     (mapconcat (lambda (variant) (symbol-name (car variant))) variants ",")
                     (if-let* ((default (drover-model-default-variant model)))
                         (format " default:%s" default)
                       " default:provider"))))
   'face 'completions-annotations))

(defun drover-model-candidates (models)
  "Return MODELS as an alist (LABEL . MODEL) for completion, in order.
A label already taken is followed by the model's identifier, which
makes it unique among models with distinct providers and identifiers."
  (let (candidates)
    (dolist (model models)
      (let ((label (drover-model-label model)))
        (when (assoc label candidates)
          (setq label (format "%s [%s]" label (drover-model-id model))))
        (push (cons label model) candidates)))
    (nreverse candidates)))

(defun drover-model-effort-candidates (model variant)
  "Return the effort strings VARIANT of MODEL may stand for, or nil.
They are the effort the variant sends, when MODEL tells it, then the
variant's name.  MODEL may be nil; VARIANT is a symbol, or nil for
none."
  (when variant
    (delq nil (list (plist-get (cdr (assq variant (and model (drover-model-variants model)))) :effort)
                    (symbol-name variant)))))

(defun drover-model-anthropic-key (api-id)
  "Return \"FAMILY-VERSION\" for the Claude model API-ID, or nil.
It accepts family-first identifiers (claude-opus-5-5,
claude-opus-5.5-fast, anthropic.claude-opus-5-5-v1:0) and version-first
ones (claude-3-7-sonnet).  Minors have at most two digits, so that a
date suffix is not read as one."
  (let ((id (replace-regexp-in-string "\\." "-" (downcase api-id))))
    (cond
     ((string-match "claude-\\([a-z]+\\)-\\([0-9]+\\(?:-[0-9]\\{1,2\\}\\)?\\|preview\\)\\(?:[^0-9]\\|\\'\\)" id)
      (concat (match-string 1 id) "-" (match-string 2 id)))
     ((string-match "claude-\\([0-9]+\\(?:-[0-9]\\{1,2\\}\\)?\\)-\\([a-z]+\\)" id)
      (concat (match-string 2 id) "-" (match-string 1 id))))))

;;;; Engines

(defvar drover--model-sources nil
  "Model sources, as a list of (ENGINE DEPTH FUNCTION), in registration order.")

(defvar drover--engines nil
  "Registered engines, as an alist (NAME . PROPERTIES), in registration order.")

(defcustom drover-engine-order '(opencode claude-code codex gemini-cli)
  "Engines in the order `drover-engines' lists them; others come after."
  :type '(repeat (symbol :tag "Engine"))
  :group 'drover)

(defun drover-register-engine (name &rest properties)
  "Register engine NAME, a symbol, with PROPERTIES.
NAME is the agent program sessions run, such as `opencode',
`claude-code', `codex' or `gemini-cli'.  PROPERTIES is a plist of:

 :label      Display name.
 :defaults   Function of a DIRECTORY and a CALLBACK calling CALLBACK,
             at most once, with the engine's defaults data for new
             sessions in DIRECTORY, whatever shape :resolve reads.  It
             never calls CALLBACK on failure, which it reports itself.
 :volatile   Non-nil when :defaults reads cheap local state only, and
             calls CALLBACK before returning: the prompt then reads
             them again whenever it gains context or a picker opens.
             Otherwise when the prompt starts or changes directory.
 :resolve    Function of DEFAULTS, the data of :defaults or nil while
             it is fetched, and the explicit AGENT, MODEL and VARIANT,
             each nil when not chosen, returning the resolved options
             (:agent :model :variant) a new session would run.
 :agents     Function of a DIRECTORY returning the agent names the
             engine offers there, possibly after a bounded wait, or nil.
 :last-used  Function of no argument returning when the engine last
             took a prompt, as a float time, or nil.
 :model-resets-variant
             Non-nil when a session switched to a model without naming
             a variant runs that model's default variant, rather than
             keeping its variant.

Every property is optional.  Registering NAME again replaces its
properties and keeps its place."
  (if-let* ((entry (assq name drover--engines)))
      (setcdr entry properties)
    (setq drover--engines (append drover--engines (list (cons name properties)))))
  name)

(defun drover-unregister-engine (name)
  "Forget engine NAME.
Its model sources stay: several files may give an engine sources, such
as the front ends running it, and each removes its own with
`drover-unregister-model-source'."
  (setq drover--engines (assq-delete-all name drover--engines)))

(defun drover-engines ()
  "Return the names of the registered engines.
Those of `drover-engine-order' come first, in that order, then the
others in the order they registered."
  (let ((names (mapcar #'car drover--engines)))
    (append (seq-filter (lambda (name) (memq name names)) drover-engine-order)
            (seq-remove (lambda (name) (memq name drover-engine-order)) names))))

(defun drover-engine-property (engine property)
  "Return PROPERTY of ENGINE, nil when ENGINE is not registered."
  (plist-get (alist-get engine drover--engines) property))

(defun drover-engine-label (engine)
  "Return the display name of ENGINE: its :label, else its name."
  (or (drover-engine-property engine :label) (symbol-name engine)))

(defun drover-engine-resolve (engine defaults agent model variant)
  "Return the resolved options a new session of ENGINE would run.
DEFAULTS is the engine's defaults data, or nil; AGENT, MODEL and VARIANT
are the explicit choices, nil when not made.  Without a :resolve
function, ENGINE runs the choices: (:agent AGENT :model MODEL :variant
VARIANT)."
  (if-let* ((resolve (drover-engine-property engine :resolve)))
      (funcall resolve defaults agent model variant)
    (list :agent agent :model model :variant variant)))

(defun drover-last-used-engine ()
  "Return the engine that took a prompt last, or nil when none tells.
Engines are asked through their :last-used function; one failing counts
as unused."
  (let (best best-time)
    (dolist (engine (drover-engines))
      (when-let* ((function (drover-engine-property engine :last-used))
                  (time (ignore-errors (funcall function)))
                  ((numberp time))
                  ((or (null best-time) (> time best-time))))
        (setq best engine best-time time)))
    best))

;;;; Model sources

(defun drover-register-model-source (engine function &optional depth)
  "Make FUNCTION a source of the models of ENGINE.
FUNCTION is called with a DIRECTORY and returns `drover-model' records,
or nil when it cannot answer.  DEPTH, 0 by default, orders the sources
of ENGINE: lower first.  `drover-models' takes the first answer.
Registering FUNCTION again for ENGINE changes its depth."
  (drover-unregister-model-source engine function)
  (setq drover--model-sources
        (append drover--model-sources (list (list engine (or depth 0) function)))))

(defun drover-unregister-model-source (engine function)
  "Stop FUNCTION being a source of the models of ENGINE."
  (setq drover--model-sources
        (seq-remove (lambda (source)
                      (and (eq (car source) engine) (equal (nth 2 source) function)))
                    drover--model-sources)))

(defun drover-engine-model-sources (engine)
  "Return the functions registered as model sources of ENGINE, lowest depth first."
  (mapcar (lambda (source) (nth 2 source))
          (sort (seq-filter (lambda (source) (eq (car source) engine)) drover--model-sources)
                (lambda (a b) (< (nth 1 a) (nth 1 b))))))

(defun drover--engine-models (engine directory)
  "Return the models of ENGINE in DIRECTORY, from its first source answering.
A source that fails does not answer."
  (let ((sources (sort (seq-filter (lambda (source) (eq (car source) engine)) drover--model-sources)
                       (lambda (a b) (< (nth 1 a) (nth 1 b))))))
    (seq-some (lambda (source)
                (with-demoted-errors "Drover model source: %S"
                  (funcall (nth 2 source) directory)))
              sources)))

(defun drover-models (&optional directory)
  "Return every model the engines offer in DIRECTORY.
DIRECTORY defaults to `default-directory'.  Each engine answers
through its first source returning models (see
`drover-register-model-source').  Engines come in the order of
`drover-model-engine-order', then of `drover-engines', then any other
with a source; a model reported twice, with the same provider and
identifier, is the first report."
  (let* ((directory (or directory default-directory))
         (with-source (delete-dups (mapcar #'car drover--model-sources)))
         (engines (delete-dups (seq-filter (lambda (engine) (memq engine with-source))
                                           (append drover-model-engine-order (drover-engines)
                                                   with-source))))
         (seen (make-hash-table :test #'equal)))
    (seq-filter (lambda (model)
                  (let ((key (cons (drover-model-provider-id model) (drover-model-id model))))
                    (unless (gethash key seen)
                      (puthash key t seen))))
                (mapcan (lambda (engine) (copy-sequence (drover--engine-models engine directory)))
                        engines))))

;;;; Reading models, variants and agents

(defun drover--completion-table (candidates category &optional annotate)
  "Return a completion table of CANDIDATES, in their order, of CATEGORY.
CANDIDATES is a list of strings or an alist keyed by strings; ANNOTATE,
a function of a candidate returning its annotation, or nil."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   ,@(and annotate `((annotation-function . ,annotate))))
      (complete-with-action action candidates string predicate))))

(defun drover-read-model (&optional default directory)
  "Read a model among those the engines offer in DIRECTORY, or the default.
The first candidate, \"Default\", stands for no choice and returns nil;
it names DEFAULT, the model a new session would run without one, when
known.  The others come from `drover-models', annotated with
`drover-model-annotation', in the category `drover-model'.  Return the
`drover-model' chosen, or nil."
  (let* ((models (drover-model-candidates (drover-models directory)))
         (default-label (if default
                            (format "Default (%s)" (drover-model-label default))
                          "Default"))
         (candidates (cons (cons default-label nil) models))
         (choice (completing-read "Model: "
                                  (drover--completion-table
                                   candidates 'drover-model
                                   (lambda (label)
                                     (when-let* ((model (cdr (assoc label candidates))))
                                       (drover-model-annotation model))))
                                  nil t nil nil default-label)))
    (cdr (assoc choice candidates))))

(defun drover-read-variant (model)
  "Read a variant of MODEL, a `drover-model', or the default.
The first candidate, \"Default\", stands for no choice and returns nil;
it names MODEL's default variant when known.  Return a variant symbol,
or nil; nil at once when MODEL has no variant."
  (when-let* ((variants (drover-model-variants model)))
    (let* ((default (drover-model-default-variant model))
           (default-label (if default (format "Default (%s)" default) "Default"))
           (candidates (cons (cons default-label nil)
                             (mapcar (lambda (variant) (cons (symbol-name (car variant)) (car variant)))
                                     variants)))
           (choice (completing-read "Variant: " (drover--completion-table candidates 'drover-variant)
                                    nil t nil nil default-label)))
      (cdr (assoc choice candidates)))))

(defun drover-agent-names (directory)
  "Return the names of the agents the engines offer in DIRECTORY.
They are those of each engine's :agents function, engines in the order
of `drover-engines', without duplicates; an engine failing offers
none."
  (delete-dups
   (mapcan (lambda (engine)
             (when-let* ((function (drover-engine-property engine :agents)))
               (copy-sequence (with-demoted-errors "Drover engine agents: %S"
                                (funcall function directory)))))
           (drover-engines))))

(defun drover-read-agent (&optional default directory)
  "Read an agent among those the engines offer in DIRECTORY, or the default.
DIRECTORY defaults to `default-directory'.  The first candidate,
\"Default\", stands for no choice and returns nil; it names DEFAULT,
the agent a new session would run without one, when known.  Return an
agent name, or nil.  Signal a `user-error' when no engine offers an
agent and DEFAULT is nil."
  (let* ((directory (or directory default-directory))
         (names (drover-agent-names directory))
         (default-label (if default (format "Default (%s)" default) "Default"))
         (candidates (cons (cons default-label nil)
                           (mapcar (lambda (name) (cons name name)) names))))
    (unless (or names default)
      (user-error "No agents available in %s" (abbreviate-file-name directory)))
    (cdr (assoc (completing-read "Agent: " (drover--completion-table candidates 'drover-agent)
                                 nil t nil nil default-label)
                candidates))))

;;;; Reading local state

(defvar drover--json-cache (make-hash-table :test #'equal)
  "Files mapped to (MTIME . JSON), for `drover-read-json-cached'.")

(defun drover-getenv (name)
  "Return the environment variable NAME, or nil when it is unset or empty."
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun drover-file-mtime (file)
  "Return the modification time of FILE as a float, or nil when it is missing."
  (when-let* ((attributes (file-attributes file)))
    (float-time (file-attribute-modification-time attributes))))

(defun drover-read-json (file)
  "Return the JSON value of FILE, objects as alists, or nil.
Arrays are lists, null and false nil.  Nil too when FILE is unreadable
or not JSON."
  (when (file-readable-p file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-buffer :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil)))))

(defun drover-read-json-cached (file)
  "Return what `drover-read-json' returns for FILE, kept until FILE changes.
The value is shared between callers, which must not modify it."
  (let ((mtime (drover-file-mtime file))
        (cached (gethash file drover--json-cache)))
    (if (and mtime (equal (car cached) mtime))
        (cdr cached)
      (let ((json (drover-read-json file)))
        (puthash file (cons mtime json) drover--json-cache)
        json))))

(defun drover-json-object-p (value)
  "Return non-nil when VALUE is a JSON object, as an alist keyed by symbols."
  (and (consp value) (consp (car value)) (symbolp (caar value))))

(defun drover-merge-settings (base over)
  "Return settings OVER merged onto BASE, both JSON objects as alists.
Objects merge key by key, recursively; any other value of OVER
replaces that of BASE.  Neither argument is modified."
  (let ((merged (copy-alist base)))
    (dolist (entry over merged)
      (let ((old (alist-get (car entry) merged)))
        (setf (alist-get (car entry) merged)
              (if (and (drover-json-object-p old) (drover-json-object-p (cdr entry)))
                  (drover-merge-settings old (cdr entry))
                (cdr entry)))))))

(provide 'drover-model)
;;; drover-model.el ends here
