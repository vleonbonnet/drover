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
;; an engine that answers being its catalog.  Sources are the live
;; sessions of backends, and OpenCode's service: drover asks no vendor.
;; What a live session offered is remembered on disk
;; (`drover-remember-models'), and answers when no live session does.  A
;; source whose models change or arrive late announces them with
;; `drover-models-changed', and a model picker open meanwhile offers
;; them.
;;
;; A model is a `drover-model' record naming the engines able to run it:
;; engines taking provider-qualified ids run any model of a provider
;; (`drover-model-qualified-engines'), the others the models of their
;; own family (`drover-model-families').  It is identified by its
;; provider and identifier (`drover-model-key'), whichever source
;; reports it; a front end taking another name for it, such as an
;; alias, finds it in its `aliases' (`drover-model-id-for').  Its
;; variants may be unknown (`variants-known'): a live session tells
;; those of the model it runs only, and what is remembered of each
;; model completes what a live session tells (`drover-model-merge').
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

;; Reloaded into an Emacs running an older drover, load its core anew.
(unless (and (boundp 'drover-core-generation) (>= drover-core-generation 1))
  (load "drover-core" nil t))

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
  (variants-known t :documentation "Non-nil when the variants and default variant are known.
Nil when no source told them, such as for a model a live session lists
without running it: its variants are then unknown rather than none, and
pickers offer none.")
  (aliases nil :documentation "Names front ends take for the model, besides its identifier.
An alist ((ENGINE . BACKEND) . NAME): BACKEND, showing sessions of
ENGINE, selects the model by NAME, such as an alias its agent lists in
place of the identifier.  See `drover-model-id-for'.")
  (engines nil :documentation "Engines able to run the model, in order of preference.")
  (remembered nil :documentation "When a live session last offered the model, as a float time.
Nil for a model a live session offers now; see `drover-remember-models'."))

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

(defun drover-model-id-for (model engine &optional backend)
  "Return the identifier ENGINE takes for MODEL, or nil when it cannot run it.
Engines of `drover-model-qualified-engines' take \"PROVIDER/ID\", and
nothing for a model without provider; the others the bare identifier.
BACKEND, the front end the identifier goes to, takes its own name for
MODEL instead, when MODEL has one (see `drover-model-aliases')."
  (when (drover-model-runs-on-p model engine)
    (or (and backend (drover-model-alias model engine backend))
        (if (memq engine drover-model-qualified-engines)
            (when-let* ((provider (drover-model-provider-id model)))
              (concat provider "/" (drover-model-id model)))
          (drover-model-id model)))))

(defun drover-model-alias (model engine backend)
  "Return the name BACKEND takes for MODEL in sessions of ENGINE, or nil.
Nil when BACKEND takes MODEL's identifier."
  (alist-get (cons engine backend) (drover-model-aliases model) nil nil #'equal))

(defun drover-model-key (model)
  "Return what identifies MODEL among the models of its engines.
It is (PROVIDER-ID . ID): sources name a model alike once they resolve
their aliases, so the models they report are merged by it."
  (cons (drover-model-provider-id model) (drover-model-id model)))

(defun drover-model-merge (model older)
  "Return MODEL completed with what OLDER, the same model, tells and it does not.
OLDER's variants and default variant fill MODEL's when MODEL does not
know them, its names fill MODEL's missing ones, and its aliases join
MODEL's, those of MODEL winning.  Neither is modified; the result
carries MODEL's time it was remembered."
  (let ((merged (copy-sequence model)))
    (when (and (not (drover-model-variants-known model)) (drover-model-variants-known older))
      (setf (drover-model-variants merged) (copy-tree (drover-model-variants older))
            (drover-model-default-variant merged) (drover-model-default-variant older)
            (drover-model-variants-known merged) t))
    (unless (drover-model-name merged)
      (setf (drover-model-name merged) (drover-model-name older)))
    (unless (drover-model-provider-name merged)
      (setf (drover-model-provider-name merged) (drover-model-provider-name older)))
    (let ((aliases (copy-alist (drover-model-aliases model))))
      (pcase-dolist (`(,key . ,name) (drover-model-aliases older))
        (unless (assoc key aliases)
          (setq aliases (append aliases (list (cons key name))))))
      (setf (drover-model-aliases merged) aliases))
    merged))

(defun drover-model-merge-all (models)
  "Return MODELS, each model reported more than once merged into its first report.
Models are the same when their `drover-model-key' is; the first report
wins, completed by the later ones with `drover-model-merge'.  Order is
kept."
  (let ((merged nil))
    (dolist (model models)
      (if-let* ((cell (seq-find (lambda (cell) (equal (car cell) (drover-model-key model))) merged)))
          (setcdr cell (drover-model-merge (cdr cell) model))
        (push (cons (drover-model-key model) model) merged)))
    (mapcar #'cdr (nreverse merged))))

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
  "Return the annotation of MODEL in pickers: its identifier and variants.
Variants no source told yet read as unknown.  A model no live session
offers now also tells the day it was last offered."
  (propertize
   (concat (format "  %s" (drover-model-id model))
           (cond
            ((not (drover-model-variants-known model)) "  {effort unknown}")
            ((drover-model-variants model)
             (format "  {%s}%s"
                     (mapconcat (lambda (variant) (symbol-name (car variant)))
                                (drover-model-variants model) ",")
                     (if-let* ((default (drover-model-default-variant model)))
                         (format " default:%s" default)
                       " default:provider"))))
           (when-let* ((time (drover-model-remembered model)))
             (format "  remembered from %s" (format-time-string "%Y-%m-%d" time))))
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
  "Model sources, as a list of (ENGINE DEPTH FUNCTION LIVE), in registration order.")

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
 :directory  Function of a conversation's ID, THEN and ELSE, finding
             the directory the conversation ran in, from what the
             engine recorded: it calls THEN with the directory, or ELSE
             with a message when it cannot, at most one of them once,
             possibly before returning.  Org links name the
             conversations of the engines that have it (see
             `drover-links'), since front ends resume a conversation in
             its directory.

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

(defcustom drover-remember-models t
  "Whether drover remembers the models live sessions offer.
When non-nil, the models a live session last offered for an engine are
kept in \"models.eld\" of `drover-state-directory', with the day they
were offered, and offered when no live session of that engine answers:
the model picker tells them by that day.  A live answer always wins,
and replaces what was remembered, but for what it does not tell of a
model, such as the variants of a model its session does not run, which
the memory keeps.  When nil, that is kept for this Emacs session only,
and nothing is offered once live sessions are gone."
  :type 'boolean
  :group 'drover)

(defconst drover-model-memory-depth 100
  "Depth at which the remembered models of an engine answer.
Sources of a lower depth, live sessions and OpenCode's catalog, answer
first; deeper ones only when nothing is remembered.")

(defun drover-register-model-source (engine function &optional depth live)
  "Make FUNCTION a source of the models of ENGINE.
FUNCTION is called with a DIRECTORY and returns `drover-model' records,
or nil when it cannot answer.  DEPTH, 0 by default, orders the sources
of ENGINE: lower first, the models remembered answering at
`drover-model-memory-depth'.  `drover-models' takes the first answer.
LIVE non-nil says FUNCTION answers for live sessions: what it answers
is remembered (see `drover-remember-models').  Registering FUNCTION
again for ENGINE changes its depth and whether it is live.

FUNCTION must answer at once.  A source whose models take time to come,
or change, returns those it has, nil at first, and calls
`drover-models-changed' when they arrive."
  (drover-unregister-model-source engine function)
  (setq drover--model-sources
        (append drover--model-sources (list (list engine (or depth 0) function (and live t))))))

(defun drover-unregister-model-source (engine function)
  "Stop FUNCTION being a source of the models of ENGINE."
  (setq drover--model-sources
        (seq-remove (lambda (source)
                      (and (eq (car source) engine) (equal (nth 2 source) function)))
                    drover--model-sources)))

(defvar drover-models-changed-hook nil
  "Hook run when the models an engine's sources offer changed.
Its functions take the ENGINE.  `drover-models-changed' runs it, and
`drover-read-model' offers from it the models that arrive while it
reads.")

(defun drover-models-changed (engine)
  "Note that the models the sources of ENGINE offer changed.
A source calls it when its models arrive or change, such as when a
live session starts.  What a live source of ENGINE answers now is
remembered, then `drover-models-changed-hook' runs.  A function of the
hook failing is reported, and stops neither the others nor the
caller."
  (drover-with-reporting "models changed"
    (drover--engine-models engine default-directory))
  (run-hook-wrapped 'drover-models-changed-hook
                    (lambda (function)
                      (drover-with-reporting "models changed"
                        (funcall function engine))
                      nil)))

(defun drover-engine-model-sources (engine)
  "Return the functions registered as model sources of ENGINE, lowest depth first."
  (mapcar (lambda (source) (nth 2 source))
          (sort (seq-filter (lambda (source) (eq (car source) engine)) drover--model-sources)
                (lambda (a b) (< (nth 1 a) (nth 1 b))))))

(defun drover--engine-models (engine directory)
  "Return the models of ENGINE in DIRECTORY, from its first source answering.
A source that fails does not answer.  The models remembered answer at
`drover-model-memory-depth', after sources registered at the same
depth.  What a live source answers is remembered, and completed by
what was remembered of each model (see `drover--remember-models')."
  (let ((sources (sort (append (seq-filter (lambda (source) (eq (car source) engine))
                                           drover--model-sources)
                               ;; The memory, which has no function.
                               (list (list engine drover-model-memory-depth nil nil)))
                       (lambda (a b) (< (nth 1 a) (nth 1 b))))))
    (catch 'answer
      (dolist (source sources)
        (when-let* ((models (drover-with-reporting "model source"
                              (if (nth 2 source)
                                  (funcall (nth 2 source) directory)
                                (drover-remembered-models engine)))))
          (throw 'answer (if (nth 3 source)
                             (drover--remember-models engine models)
                           models)))))))

(defun drover-models (&optional directory)
  "Return every model the engines offer in DIRECTORY.
DIRECTORY defaults to `default-directory'.  Each engine answers
through its first source returning models (see
`drover-register-model-source'), or the models remembered of it.
Engines come in the order of `drover-model-engine-order', then of
`drover-engines', then any other with a source or remembered models; a
model reported twice, with the same provider and identifier, is the
first report."
  (let* ((directory (or directory default-directory))
         (with-source (delete-dups (append (mapcar #'car drover--model-sources)
                                           (drover--remembered-engines))))
         (engines (delete-dups (seq-filter (lambda (engine) (memq engine with-source))
                                           (append drover-model-engine-order (drover-engines)
                                                   with-source))))
         (seen (make-hash-table :test #'equal)))
    (seq-filter (lambda (model)
                  (let ((key (drover-model-key model)))
                    (unless (gethash key seen)
                      (puthash key t seen))))
                (mapcan (lambda (engine) (copy-sequence (drover--engine-models engine directory)))
                        engines))))

;;;; Remembered models

;; The file is Lisp data: a version, then per engine the day its models
;; were offered and each model as a plist of the slots pickers show.
;; It is read again whenever another Emacs wrote it, and written whole,
;; through a temporary file renamed over it.
;;
;; A live session tells the variants of the model it runs, often not
;; those of the others it lists, so the models remembered are those of
;; the last live answer, each completed by what was remembered of it:
;; variants accumulate per model as sessions switch models.

(defconst drover--model-memory-version 2
  "Version of the format of the file of remembered models.
Version 1 gave every model the variants of the model a session ran:
they are read as unknown.")

(defvar drover--model-memory nil
  "The models remembered, as an alist (ENGINE . (:time TIME :models MODELS)).
TIME is when a live session offered MODELS, a list of plists of
`drover--model-data'.  Without `drover-remember-models', what this
Emacs session learnt, which no file backs.")

(defvar drover--model-memory-stamp 'unread
  "Modification time of the file of remembered models when last read or written.
`unread' before it is read, nil when it does not exist.")

(defvar drover--model-memory-warned nil
  "Non-nil once a problem with the file of remembered models was reported.
A file that cannot be read or written is reported once per Emacs
session, as models are asked for often.")

(defun drover-model-memory-file ()
  "Return the file where drover remembers models, in `drover-state-directory'."
  (expand-file-name "models.eld" drover-state-directory))

(defun drover--model-memory-warn (format-string &rest args)
  "Report a problem with the file of remembered models, once per session.
FORMAT-STRING and ARGS are as for `format-message'."
  (unless drover--model-memory-warned
    (setq drover--model-memory-warned t)
    (apply #'drover-warn format-string args)))

(defun drover--model-data (model)
  "Return what is remembered of MODEL, a plist of the slots pickers show."
  (list :provider-id (drover-model-provider-id model)
        :provider-name (drover-model-provider-name model)
        :id (drover-model-id model)
        :name (drover-model-name model)
        :variants (drover-model-variants model)
        :default-variant (drover-model-default-variant model)
        :variants-known (and (drover-model-variants-known model) t)
        :aliases (drover-model-aliases model)
        :engines (drover-model-engines model)))

(defun drover--model-from-data (data &optional time)
  "Return the `drover-model' DATA, a plist of `drover--model-data', describes.
TIME is when a live session offered it, or nil."
  (drover-model-create :provider-id (plist-get data :provider-id)
                       :provider-name (plist-get data :provider-name)
                       :id (plist-get data :id)
                       :name (plist-get data :name)
                       :variants (copy-tree (plist-get data :variants))
                       :default-variant (plist-get data :default-variant)
                       :variants-known (plist-get data :variants-known)
                       :aliases (copy-tree (plist-get data :aliases))
                       :engines (copy-sequence (plist-get data :engines))
                       :remembered time))

(defun drover--model-data-p (data)
  "Return non-nil when DATA is a model as `drover--model-data' gives it."
  (and (plistp data)
       (stringp (plist-get data :id))
       (seq-every-p (lambda (key) (let ((value (plist-get data key)))
                                    (or (null value) (stringp value))))
                    '(:provider-id :provider-name :name))
       (proper-list-p (plist-get data :variants))
       (seq-every-p (lambda (variant)
                      (and (consp variant) (symbolp (car variant)) (plistp (cdr variant))))
                    (plist-get data :variants))
       (symbolp (plist-get data :default-variant))
       (proper-list-p (plist-get data :aliases))
       (seq-every-p (lambda (alias)
                      (and (consp alias) (consp (car alias))
                           (symbolp (caar alias)) (symbolp (cdar alias))
                           (stringp (cdr alias))))
                    (plist-get data :aliases))
       (proper-list-p (plist-get data :engines))
       (seq-every-p #'symbolp (plist-get data :engines))))

(defun drover--model-data-upgrade (data version)
  "Return model DATA of the file's format VERSION in the current format.
Version 1 knew no aliases, and copied the variants of the model a
session ran to every model it listed: they are dropped, unknown."
  (if (eql version 1)
      (list :provider-id (plist-get data :provider-id)
            :provider-name (plist-get data :provider-name)
            :id (plist-get data :id)
            :name (plist-get data :name)
            :variants nil
            :default-variant nil
            :variants-known nil
            :aliases nil
            :engines (plist-get data :engines))
    data))

(defun drover--model-memory-parse (form)
  "Return the alist of remembered models FORM holds, or signal an error.
FORM is what the file holds, read.  Models of an older format are read
as `drover--model-data-upgrade' upgrades them."
  (let ((version (and (plistp form) (plist-get form :version))))
    (unless (memql version (list 1 drover--model-memory-version))
      (error "Unknown format"))
    (let ((engines (plist-get form :engines)))
      (unless (and (proper-list-p engines)
                   (seq-every-p (lambda (entry)
                                  (and (consp entry)
                                       (symbolp (car entry))
                                       (plistp (cdr entry))
                                       (numberp (plist-get (cdr entry) :time))
                                       (proper-list-p (plist-get (cdr entry) :models))
                                       (seq-every-p #'drover--model-data-p (plist-get (cdr entry) :models))))
                                engines))
        (error "Malformed entries"))
      (mapcar (lambda (entry)
                (cons (car entry)
                      (list :time (float (plist-get (cdr entry) :time))
                            :models (mapcar (lambda (data) (drover--model-data-upgrade data version))
                                            (plist-get (cdr entry) :models)))))
              engines))))

(defun drover--model-memory-read ()
  "Return the alist of models the file remembers, nil when there is none.
A file that is not one of remembered models, or of an unknown version,
is reported once and ignored: the next live answer replaces it."
  (let ((file (drover-model-memory-file)))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (drover--model-memory-parse (read (current-buffer))))
        (error
         (drover--model-memory-warn "Ignoring the models remembered in %s: %s"
                                    (abbreviate-file-name file) (drover-error-message err))
         nil)))))

(defun drover--model-memory ()
  "Return the alist of models remembered, reading the file when it changed.
Without `drover-remember-models', no file is read: what this Emacs
session learnt."
  (when drover-remember-models
    (let ((stamp (drover-file-mtime (drover-model-memory-file))))
      (unless (equal stamp drover--model-memory-stamp)
        (setq drover--model-memory (drover--model-memory-read)
              drover--model-memory-stamp stamp))))
  drover--model-memory)

(defun drover--model-memory-write (memory)
  "Write MEMORY, an alist of remembered models, to the file, atomically.
See `drover-write-data'.  A failure is reported once."
  (let ((file (drover-model-memory-file)))
    (condition-case err
        (drover-write-data file "Models drover remembers" drover--model-memory-version :engines memory)
      (error
       (drover--model-memory-warn "Could not remember models in %s: %s"
                                  (abbreviate-file-name file) (drover-error-message err))))
    (setq drover--model-memory-stamp (drover-file-mtime file))))

(defun drover--complete-models (models remembered)
  "Return MODELS, a live answer, each completed by what REMEMBERED tells of it.
REMEMBERED are the models remembered of the engine.  A model of MODELS
reported twice is merged first.  Each takes from the remembered model
of the same key what it does not know (see `drover-model-merge'), but
an alias another model of MODELS takes now: an alias, such as \"opus\",
moves to a newer model."
  (let* ((models (drover-model-merge-all models))
         (claimed (mapcan (lambda (model) (copy-sequence (drover-model-aliases model))) models)))
    (mapcar (lambda (model)
              (if-let* ((older (seq-find (lambda (old) (equal (drover-model-key old) (drover-model-key model)))
                                         remembered)))
                  (let ((older (copy-sequence older))
                        (own (drover-model-aliases model)))
                    (setf (drover-model-aliases older)
                          (seq-remove (lambda (alias)
                                        (and (not (member alias own)) (member alias claimed)))
                                      (drover-model-aliases older)))
                    (drover-model-merge model older))
                model))
            models)))

(defun drover--remember-models (engine models)
  "Remember MODELS, which a live session of ENGINE offers now, and return them.
They are returned completed by what was remembered of each (see
`drover--complete-models'), and remembered so, in place of the models
remembered of ENGINE.  The file is written only when they differ from
those remembered, or were remembered on another day, so that the day
the picker shows stays true without a write at every answer.  Without
`drover-remember-models', they are completed and remembered for this
Emacs session only."
  (if (null models)
      models
    (let* ((memory (copy-alist (drover--model-memory)))
           (entry (alist-get engine memory))
           (completed (drover--complete-models
                       models (mapcar #'drover--model-from-data (plist-get entry :models))))
           (data (mapcar #'drover--model-data completed))
           (now (float-time)))
      (unless (and (equal (plist-get entry :models) data)
                   (equal (format-time-string "%F" (plist-get entry :time))
                          (format-time-string "%F" now)))
        (setf (alist-get engine memory) (list :time now :models data))
        (setq drover--model-memory memory)
        (when drover-remember-models
          (drover--model-memory-write memory)))
      completed)))

(defun drover-remembered-models (engine)
  "Return the models remembered of ENGINE, each telling when it was offered.
Nil when `drover-remember-models' is off."
  (when drover-remember-models
    (when-let* ((entry (alist-get engine (drover--model-memory))))
      (let ((time (plist-get entry :time)))
        (mapcar (lambda (data) (drover--model-from-data data time))
                (plist-get entry :models))))))

(defun drover-learnt-model (engine key)
  "Return what drover learnt of the model of ENGINE whose key is KEY, or nil.
KEY is a value of `drover-model-key'.  It is what live sessions of
ENGINE last told of it, completed by what they told before (see
`drover--remember-models'): its variants and default variant even
while no live session tells them.  Without `drover-remember-models',
what this Emacs session learnt."
  (when-let* ((entry (alist-get engine (drover--model-memory)))
              (data (seq-find (lambda (data)
                                (equal (cons (plist-get data :provider-id) (plist-get data :id)) key))
                              (plist-get entry :models))))
    (drover--model-from-data data (and drover-remember-models (plist-get entry :time)))))

(defun drover--remembered-engines ()
  "Return the engines whose models are remembered."
  (and drover-remember-models (mapcar #'car (drover--model-memory))))

(defun drover-forget-models ()
  "Forget the models drover remembers, and delete their file.
Pickers then offer only what live sessions offer, until they offer
models again."
  (interactive)
  (let ((file (drover-model-memory-file)))
    (when (file-exists-p file)
      (delete-file file))
    (setq drover--model-memory nil
          drover--model-memory-stamp nil)
    (when (called-interactively-p 'interactive)
      (message "Drover forgot the models it remembered"))))

;;;; Reading models, variants and agents

(defun drover--completion-table (candidates category &optional annotate)
  "Return a completion table of CANDIDATES, in their order, of CATEGORY.
CANDIDATES is a list of strings or an alist keyed by strings, or a
function of no argument returning one, called whenever the table is,
so that candidates arriving late are offered; ANNOTATE, a function of
a candidate returning its annotation, or nil."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   ,@(and annotate `((annotation-function . ,annotate))))
      (complete-with-action action (if (functionp candidates) (funcall candidates) candidates)
                            string predicate))))

(defvar vertico--input)
(declare-function vertico--exhibit "ext:vertico" ())
(declare-function icomplete-exhibit "icomplete" ())

(defun drover--refresh-completions-list ()
  "Show the current minibuffer's candidates again in *Completions*, when shown."
  (when (get-buffer-window "*Completions*" 0)
    (minibuffer-completion-help)))

(defun drover--refresh-icomplete ()
  "Show the current minibuffer's candidates again, when Icomplete shows them."
  (when (and (bound-and-true-p icomplete-mode) (fboundp 'icomplete-exhibit))
    (icomplete-exhibit)))

(defun drover--refresh-vertico ()
  "Show the current minibuffer's candidates again, when Vertico shows them."
  ;; Vertico computes candidates again only for new input, which a
  ;; non-string `vertico--input' stands for, as Consult does.
  (when (and (bound-and-true-p vertico--input) (fboundp 'vertico--exhibit))
    (setq vertico--input t)
    (vertico--exhibit)))

(defvar drover-completion-refresh-functions
  (list #'drover--refresh-completions-list #'drover--refresh-icomplete #'drover--refresh-vertico)
  "Functions showing again the candidates of the current minibuffer.
A picker calls each, without argument, in its minibuffer when its
candidates changed while it reads, such as models that arrived.  The
defaults update the *Completions* list, Icomplete (and Fido) and
Vertico; another completion UI adds its own.")

(defun drover--refresh-completion (minibuffer)
  "Show the candidates of MINIBUFFER, a minibuffer buffer, again.
They are computed anew, then shown by every function of
`drover-completion-refresh-functions'.  Nothing happens once
MINIBUFFER is gone."
  (when (buffer-live-p minibuffer)
    (with-current-buffer minibuffer
      ;; The sorted completions Icomplete and `minibuffer-force-complete'
      ;; keep are flushed only by editing the input.
      (setq completion-all-sorted-completions nil)
      (dolist (function drover-completion-refresh-functions)
        (drover-with-reporting "completion refresh"
          (funcall function))))))

(defun drover-read-model (&optional default directory)
  "Read a model among those the engines offer in DIRECTORY, or the default.
The first candidate, \"Default\", stands for no choice and returns nil;
it names DEFAULT, the model a new session would run without one, when
known.  The others come from `drover-models', annotated with
`drover-model-annotation', in the category `drover-model'.  Models
arriving while it reads (see `drover-models-changed') are offered as
they come.  Return the `drover-model' chosen, or nil."
  (let* ((directory (or directory default-directory))
         (default-label (if default
                            (format "Default (%s)" (drover-model-label default))
                          "Default"))
         (candidates nil)
         (collect (lambda ()
                    (setq candidates (cons (cons default-label nil)
                                           (drover-model-candidates (drover-models directory))))))
         (minibuffer nil)
         (refresh (lambda (_engine)
                    (funcall collect)
                    (drover--refresh-completion minibuffer))))
    (funcall collect)
    (add-hook 'drover-models-changed-hook refresh)
    (unwind-protect
        (let ((choice (minibuffer-with-setup-hook (lambda () (setq minibuffer (current-buffer)))
                        (completing-read "Model: "
                                         (drover--completion-table
                                          (lambda () candidates) 'drover-model
                                          (lambda (label)
                                            (when-let* ((model (cdr (assoc label candidates))))
                                              (drover-model-annotation model))))
                                         nil t nil nil default-label))))
          (cdr (assoc choice candidates)))
      (remove-hook 'drover-models-changed-hook refresh))))

(defun drover-read-variant (model)
  "Read a variant of MODEL, a `drover-model', or the default.
The first candidate, \"Default\", stands for no choice and returns nil;
it names MODEL's default variant when known.  Return a variant symbol,
or nil; nil at once when MODEL has no variant, or none known: only the
variants a model offers are offered."
  (when-let* (((drover-model-variants-known model))
              (variants (drover-model-variants model)))
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
               (copy-sequence (drover-with-reporting "engine agents"
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
